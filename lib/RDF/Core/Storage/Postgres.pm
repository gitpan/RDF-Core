# 
# The contents of this file are subject to the Mozilla Public
# License Version 1.1 (the "License"); you may not use this file
# except in compliance with the License. You may obtain a copy of
# the License at http://www.mozilla.org/MPL/
# 
# Software distributed under the License is distributed on an "AS
# IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
# implied. See the License for the specific language governing
# rights and limitations under the License.
# 
# The Original Code is the RDF::Core module
# 
# The Initial Developer of the Original Code is Ginger Alliance Ltd.
# Portions created by Ginger Alliance are 
# Copyright (C) 2001 Ginger Alliance Ltd.
# All Rights Reserved.
# 
# Contributor(s):
# 
# Alternatively, the contents of this file may be used under the
# terms of the GNU General Public License Version 2 or later (the
# "GPL"), in which case the provisions of the GPL are applicable 
# instead of those above.  If you wish to allow use of your 
# version of this file only under the terms of the GPL and not to
# allow others to use your version of this file under the MPL,
# indicate your decision by deleting the provisions above and
# replace them with the notice and other provisions required by
# the GPL.  If you do not delete the provisions above, a recipient
# may use your version of this file under either the MPL or the
# GPL.
# 

package RDF::Core::Storage::Postgres;

use strict;
require Exporter;

our $VERSION = '0.02';
our @ISA = qw(RDF::Core::Storage);

use Carp;
use DBI qw(:sql_types);
use RDF::Core::Statement;
use RDF::Core::Enumerator::Postgres;

############################################################
# constants

use constant RDF_CORE_DB_DONT_CREATE => 0;
use constant RDF_CORE_DB_CREATE => 1;

use constant RDF_CORE_SELECT_OBJECT_RES => 1;
use constant RDF_CORE_SELECT_OBJECT_LIT => 2;

use constant RDF_CORE_SELECT_DATA => 1;
use constant RDF_CORE_EXISTS_ONLY => 2;

############################################################
# constructor

sub new {
    my ($class, %params) = @_;
    $class = ref $class || $class;
    my $self = { 
		dbconn => $params{ConnectStr},
		dbuser => $params{DBUser},
		dbpasswd => $params{DBPassword},
		model => $params{Model}, 
	       };
    bless $self, $class;
    return $self;
}

############################################################
# internal private methods

sub _getDBHandle {
    my $self = shift;
    unless ($self->{dbh}) {
	$self->{dbh} = DBI->connect($self->{dbconn}, $self->{dbuser},
				    $self->{dbpasswd}, {RaiseError => 1});
	my $sth = $self->{dbh}->prepare('set client_encoding to unicode');
	$sth->execute();
    }
    return $self->{dbh};
}

sub _getModelId {
    my ($self) = @_;
    unless ($self->{model_id}) {
	my $sth = $self->_getDBHandle()->prepare('select rdf_model_get(?,?)');
	$sth->bind_param(1,$self->{model});
	$sth->bind_param(2,1);
	$sth->execute;
	my @row = $sth->fetchrow_array;
	$self->{model_id} = $row[0];    
    };
    return $self->{model_id};
}

sub _getStmt {
    my ($self, $stmt, $create) = @_;
    my $rval;
    my $isLiteral = $stmt->getObject()->isLiteral(); 
    my $proc = $isLiteral ? 
      'select rdf_stmt_get(?,?,?,?,?,?,?)' : 
	'select rdf_stmt_get(?,?,?,?,?,?,?,?)';
    my $sth = $self->_getDBHandle()->prepare($proc);
    my $i = 1;
    $sth->bind_param($i++, $self->_getModelId);
    $sth->bind_param($i++, $stmt->getSubject()->getNamespace());
    $sth->bind_param($i++,  $stmt->getSubject()->getLocalValue());
    $sth->bind_param($i++, $stmt->getPredicate()->getNamespace());
    $sth->bind_param($i++, $stmt->getPredicate()->getLocalValue());
    if ($isLiteral) {
	$sth->bind_param($i++, $stmt->getObject()->getValue());
    } else {
	$sth->bind_param($i++,$stmt->getObject()->getNamespace());
	$sth->bind_param($i++,$stmt->getObject()->getLocalValue());
    };
    $sth->bind_param($i++, $create);
    $sth->execute();		
    my @row = $sth->fetchrow_array;      
    return $row[0];
}

sub _buildSelect {
    my ($self, $subj, $pred, $obj) = @_; 
    my $rval;
    if ($obj) {
	$rval = $self->_buildSelect2($subj, $pred, $obj, 0);
    } else {
	my $rval1 = $self->_buildSelect2($subj, $pred, $obj,
					 RDF_CORE_SELECT_OBJECT_RES);
	my $rval2 = $self->_buildSelect2($subj, $pred, $obj,
					 RDF_CORE_SELECT_OBJECT_LIT);
	$rval2 =~ s/'1'/'4'/;	
	$rval2 =~ s/'2'/'5'/;
	$rval2 =~ s/'3'/'6'/;
	$rval = $rval1."\nunion\n".$rval2;
    }
    return $rval;
};

sub _buildSelect2 {
    my ($self, $subj, $pred, $obj, $switch) = @_;
    
    my $select_subj = 'n1.namespace, r1.local_name';
    my $select_pred = 'n2.namespace, r2.local_name';
    my $select_obj_res = 'n3.namespace, r3.local_name, null';
    my $select_obj_lit = 'null, null, s.object_lit';
    
    my $from_subj = 'rdf_resource r1, rdf_namespace n1';
    my $from_pred = 'rdf_resource r2, rdf_namespace n2';
    my $from_obj_res = 'rdf_resource r3, rdf_namespace n3';
    
    my $where_subj_join = 'r1.res_id = s.subject and r1.ns_id = n1.ns_id';
    my $where_pred_join = 'r2.res_id = s.predicate and r2.ns_id = n2.ns_id';
    my $where_obj_res_join = 'r3.res_id = s.object_res and r3.ns_id = n3.ns_id';
    
    my $where_subj_param = 'n1.namespace = ? and r1.local_name = ?';
    my $where_pred_param = 'n2.namespace = ? and r2.local_name = ?';
    my $where_obj_res_param = 'n3.namespace = ? and r3.local_name = ?';
    my $where_obj_lit_param = 's.object_lit = ?';
    
    # construction of 'select' part
    my $select_obj = $obj ? 
      ($obj->isLiteral() ? $select_obj_lit : $select_obj_res) :
	($switch == RDF_CORE_SELECT_OBJECT_RES ? $select_obj_res : $select_obj_lit);
    my $s = 'select '.join (', ', ($select_subj, $select_pred, $select_obj))."\n";
    
    # construction of 'from' part
    $s = $s . 'from '.join (', ', ('rdf_statement s', $from_subj, $from_pred));
    if ((!$obj && ($switch == RDF_CORE_SELECT_OBJECT_RES)) || 
	($obj && !$obj->isLiteral())) {
	$s = join (', ', ($s, $from_obj_res));
    };
    $s = $s . "\n";
    
    # construction of 'where' part
    $s = $s . "where s.model_id = ?\n";
    # subject
    $s = join (' and ', ($s, $subj ? ($where_subj_param, $where_subj_join) : $where_subj_join)) . "\n";
    # predicate
    $s = join (' and ', ($s, $pred ? ($where_pred_param, $where_pred_join) : $where_pred_join)) . "\n";
    # object
    my $where_obj = $obj ? 
      ($obj->isLiteral() ? $where_obj_lit_param : join (' and ', ($where_obj_res_param, $where_obj_res_join))) : 
	($switch == RDF_CORE_SELECT_OBJECT_RES ? $where_obj_res_join : ' s.object_lit is not null');
    $s = join (' and ', ($s, $where_obj));
    return $s;
};



sub _getStmts {
    my ($self, $subject, $predicate, $object, $switch) = @_;

    $switch = $switch ? $switch : RDF_CORE_SELECT_DATA;

    my $s = $self->_buildSelect($subject, $predicate, $object);
    if ($switch == RDF_CORE_EXISTS_ONLY) {
	if ($subject || $predicate || $object) {
	    $s = 'select exists ( ' . $s . ' )';
	} else {
	    # optimise for 'is there a statement at all?' question
	    $s = 'select exists ' .
	      '( select * from rdf_statement where model_id = ? )';
	}
    }
    my $sth = $self->_getDBHandle()->prepare($s);
    
    my $i=1;
    $sth->bind_param($i++,$self->_getModelId); #model

    if (($switch == RDF_CORE_SELECT_DATA) ||
	(($switch == RDF_CORE_EXISTS_ONLY) && 
	 ($subject || $predicate || $object))) {

	if ($subject) { 
	    $sth->bind_param($i++,$subject->getNamespace()); 
	    $sth->bind_param($i++,$subject->getLocalValue()); 
	}
	if ($predicate) { 
	    $sth->bind_param($i++,$predicate->getNamespace()); 
	    $sth->bind_param($i++,$predicate->getLocalValue()); 
	}
	if ($object) { 
	    if ($object->isLiteral()) {
		$sth->bind_param($i++,$object->getValue()); 
	    } else {
		$sth->bind_param($i++,$object->getNamespace()); 
		$sth->bind_param($i++,$object->getLocalValue()); 
	    }
	}    
	unless ($object) {
	    $sth->bind_param($i++,$self->_getModelId); #model
	    if ($subject) { 
		$sth->bind_param($i++,$subject->getNamespace()); 
		$sth->bind_param($i++,$subject->getLocalValue()); 
	    }
	    if ($predicate) { 
		$sth->bind_param($i++,$predicate->getNamespace()); 
		$sth->bind_param($i++,$predicate->getLocalValue()); 
	    }
	    if ($object) { 
		if ($object->isLiteral()) {
		    $sth->bind_param($i++,$object->getValue()); 
		} else {
		    $sth->bind_param($i++,$object->getNamespace()); 
		    $sth->bind_param($i++,$object->getLocalValue()); 
		}
	    }    
	}
    }

    $sth->execute();
    return $sth;
};


############################################################
# methods

sub addStmt {
    my ($self, $stmt) = @_;
    return _getStmt ($self, $stmt, RDF_CORE_DB_CREATE);
}
sub removeStmt {
    my ($self, $stmt) = @_;
    my $rval;
    my $isLiteral = $stmt->getObject()->isLiteral(); 
    my $proc = $isLiteral ? 
      'select rdf_stmt_del(?,?,?,?,?,?)' : 
	'select rdf_stmt_del(?,?,?,?,?,?,?)';
    my $sth = $self->_getDBHandle()->prepare($proc);
    my $i = 1;
    $sth->bind_param($i++, $self->_getModelId);
    $sth->bind_param($i++, $stmt->getSubject()->getNamespace());
    $sth->bind_param($i++,  $stmt->getSubject()->getLocalValue());
    $sth->bind_param($i++, $stmt->getPredicate()->getNamespace());
    $sth->bind_param($i++, $stmt->getPredicate()->getLocalValue());
    if ($isLiteral) {
	$sth->bind_param($i++, $stmt->getObject()->getValue());
    } else {
	$sth->bind_param($i++,$stmt->getObject()->getNamespace());
	$sth->bind_param($i++,$stmt->getObject()->getLocalValue());
    };
    $sth->execute();		
    my @row = $sth->fetchrow_array;      
    return $row[0];
}

sub existsStmt {
#    my ($self, $stmt) = @_;
#    return (_getStmt ($self, $stmt, RDF_CORE_DB_DONT_CREATE) > 0);
    my ($self, $subject, $predicate, $object) = @_;    
    my $sth = $self->_getStmts($subject, $predicate, $object, 
			       RDF_CORE_EXISTS_ONLY);
    my @row = $sth->fetchrow_array;      
    return $row[0];
}

sub getStmts {
    my ($self, $subject, $predicate, $object) = @_;
    my $sth = $self->_getStmts($subject, $predicate, $object);
    return new RDF::Core::Enumerator::Postgres( (Cursor  => $sth) );
}

sub countStmts {
    my $self = shift;
    my $sql = 'select count(*) from rdf_statement where model_id = ?';
    my $sth = $self->_getDBHandle()->prepare($sql);
    $sth->bind_param(1, $self->_getModelId);
    $sth->execute();		
    my @row = $sth->fetchrow_array;      
    return $row[0];
}

sub getNewResourceId {
    my $self = shift;
    my $sth = $self->_getDBHandle()->prepare('select rdf_res_new_id()');
    $sth->execute();		
    my @row = $sth->fetchrow_array;      
    return $row[0];    
}

sub DESTROY {
    my $self = shift;
    $self->{dbh}->disconnect() if ($self->{dbh});
};

1;
__END__

=head1 NAME

RDF::Core::Storage::Postgres - PostgreSQL implementation of RDF::Core::Storage

=head1 SYNOPSIS

  use RDF::Core::Storage::Postgres;
  my $storage = new RDF::Core::Storage::Postgres((
						  ConnectStr=>'dbi:Pg:dbname=rdf',
						  DBUser=>'username',
						  Model=>'1',
						 ));
  my $model = new RDF::Core::Model (Storage => $storage);

=head1 DESCRIPTION

The storage is based on PostgreSQL database.

=head2 Interface

=over 4

=item * new(%options)

Available options are:

=over 4

=item * ConnectStr

Connect string (see PostgreSQL documentation)

=item * DBUser, DBPassword

Database username and pasword.

=item * Model

More then one model can be stored in one database, use Model to distinct between them.

=back


The rest of the interface is described in RDF::Core::Storage.

=back

=head2 INSTALLATION

You need to have PostgreSQL database installed. Then run scripts in dbmodel/pgsql/rdf-pgsql.sql and database will be created and ready for use.



=head1 LICENSE

This package is subject to the MPL (or the GPL alternatively).

=head1 AUTHOR

Ginger Alliance, rdf@gingerall.cz

=head1 SEE ALSO

RDF::Core::Storage, RDF::Core::Model


=cut

