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

package RDF::Core::Function;

use strict;
require Exporter;

use RDF::Core::Query;
use Carp;


use constant RDF_NS => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
use constant RDFS_NS => 'http://www.w3.org/2000/01/rdf-schema#';

sub new {
    my ($pkg, %options) = @_;
    $pkg = ref $pkg || $pkg;
    my $self = {};
    $self->{_options} = \%options;
    $self->{_functions} = {self=>\&self,
			  subclass=>\&subclass,
			  subproperty=>\&subproperty,
			 };
    bless $self, $pkg;
}
sub getOptions {
    my $self = shift;
    return $self->{_options};
}
sub getFunctions {
    my $self = shift;
    return $self->{_functions};
}
############################################################

sub self {
    my ($self, $subject, $params) = @_;
    my $retVal= [];

    return $retVal = defined $subject ? [$subject] : [$params->[0]];
}
    
sub subclass {
    my ($self, $subject, $params) = @_;
    my $retVal= [];
    
    croak "Function subclass expects one parameter."
      unless @$params == 1;
    croak "What does [".$subject->getLabel.
      "].subclass([".$params->[0]->getLabel."]) mean?" if $subject;
    my @subClasses;
    my $pred = $self->getOptions->{Factory}->newResource(RDFS_NS.'subClassOf');
    my $enum = $self->getOptions->{Schema}->getStmts(undef,$pred,$params->[0]);
    while (my $st = $enum->getNext) {
	push @subClasses, $st->getSubject;
	push @subClasses, @{$self->subclass(undef, [$st->getSubject])};
    }
    $enum->close;
    $retVal = \@subClasses;
    
    return $retVal
}

sub subproperty {
    my ($self, $subject, $params) = @_;
    my $retVal= [];
    
    croak "Function subproperty expects one parameter."
      unless @$params == 1;
    my @subProperties;
    my $pred = $self->getOptions->{Factory}->newResource(RDFS_NS.'subPropertyOf');
    my $enum = $self->getOptions->{Schema}->getStmts(undef,$pred,$params->[0]);
    while (my $st = $enum->getNext) {
	push @subProperties, $st->getSubject;
	push @subProperties, @{$self->subproperty(undef, [$st->getSubject])};
    }
    $enum->close;
    if ($subject) {
	unless ($subject->isLiteral) {
	    foreach my $property (@subProperties) {
		my $enum = $self->getOptions->{Data}->getStmts($subject, 
							       $property,
							       undef);
		while (my $st = $enum->getNext) {
		    push @$retVal, $st->getObject;
		}
		$enum->close;
	    }
	}
    } else {
	$retVal = \@subProperties;
    }
    
    return $retVal
}




1;
__END__

=head1 NAME

RDF::Core::Function - a package of functions for query language.

=head1 DESCRIPTION

When there is a function found while evaluating query, its parameters are evaluated and passed to RDF::Core::Function apropriate piece of code. The code reference is obtained in a hash returned by getFunctions() call. Each function accepts RDF::Core::Literal or RDF::Core::Resource objects as paramaters and returns an array reference to objects of the same type. 

There is a special parameter - a subject parameter, which says that a function is at position of property. For example:

  subproperty(schema:SomeProperty)

has no subject parameter defined and returns property B<names> that are subproperties of given schema:SomeProperty.

  data:SomeObject.subproperty(schema:SomeProperty)

has subject parameter data:SomeObject and return B<values> of subproperties for subject.



=head2 Interface

=over 4

=item * new(%options)

Available options are:

=over 4

=item * Data

RDF::Core::Model object that contains data to be queried.

=item * Schema

RDF::Core::Model object that contains RDF schema.

=item * Factory

RDF::Core::NodeFactory object, that produces resource and literal objects.

=back

=item * getFunctions

Returns a hash reference where each key is a name of a functions and value is a reference to an implementation code.

=back

=head2 Functions implemented

=over 4

=item * subclass(X)

Not defined subject parameter:

Find all subclasses of X in Schema and return them if they have an instance in Data.

Defined subject parameter:

Result is not defined, dies.

=item * subproperty(X)

Not defined subject parameter:

Find all subproperties of X in Schema and return them if they occur in Data.

Defined subject parameter:

Find all subproperties of X in Schema and return their values for subject, if found.

=back

=head1 LICENSE

This package is subject to the MPL (or the GPL alternatively).

=head1 AUTHOR

Ginger Alliance, rdf@gingerall.cz

=head1 SEE ALSO

RDF::Core::Query

=cut


