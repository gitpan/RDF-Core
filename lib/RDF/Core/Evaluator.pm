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

package RDF::Core::Evaluator;

use strict;
require Exporter;

use RDF::Core::Query;
use RDF::Core::Function;
use Carp;

#query syntax elements

use constant Q_QUERY        => RDF::Core::Query::Q_QUERY;
use constant Q_RESULTSET    => RDF::Core::Query::Q_RESULTSET;
use constant Q_SOURCE       => RDF::Core::Query::Q_SOURCE;
use constant Q_SOURCEPATH   => RDF::Core::Query::Q_SOURCEPATH;
use constant Q_HASTARGET    => RDF::Core::Query::Q_HASTARGET;
use constant Q_TARGET       => RDF::Core::Query::Q_TARGET;
use constant Q_CONDITION    => RDF::Core::Query::Q_CONDITION;
use constant Q_MATCH        => RDF::Core::Query::Q_MATCH;
use constant Q_PATH         => RDF::Core::Query::Q_PATH;
use constant Q_CLASSPATH    => RDF::Core::Query::Q_CLASSPATH;
use constant Q_ELEMENTS     => RDF::Core::Query::Q_ELEMENTS;
use constant Q_ELEMENTPATH  => RDF::Core::Query::Q_ELEMENTPATH;
use constant Q_ELEMENT      => RDF::Core::Query::Q_ELEMENT;
use constant Q_FUNCTION     => RDF::Core::Query::Q_FUNCTION;
use constant Q_NODE         => RDF::Core::Query::Q_NODE;
use constant Q_VARIABLE     => RDF::Core::Query::Q_VARIABLE;
use constant Q_URIDEF       => RDF::Core::Query::Q_URIDEF;
use constant Q_NAME         => RDF::Core::Query::Q_NAME;
use constant Q_EXPRESSION   => RDF::Core::Query::Q_EXPRESSION;
use constant Q_CONNECTION   => RDF::Core::Query::Q_CONNECTION;
use constant Q_RELATION     => RDF::Core::Query::Q_RELATION;
use constant Q_OPERATION    => RDF::Core::Query::Q_OPERATION;
use constant Q_LITERAL      => RDF::Core::Query::Q_LITERAL;
use constant Q_URI          => RDF::Core::Query::Q_URI;

use constant RDF_NS => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';

sub new {
    my ($pkg,%options) = @_;
    $pkg = ref $pkg || $pkg;
    my $self = {};
    $self->{_options} = \%options;
#     unless ($options{Functions}) {
#  	$self->{_options}->{Functions} = new RDF::Core::Function;
#     }
    bless $self, $pkg;
}
sub getOptions {
    my $self = shift;
    return $self->{_options};
}

############################################################
# Evaluator part
############################################################
# This one wants to be elementary, even naive approach, using
# existing model interface only

sub evaluate {
    my ($self, $query) = @_;
    my $rs;
    my $descr;
    ($rs,$descr) = $self->_prepareResultSet($query);
    $self->_applyConditions($rs, $descr, $query);
    $self->_formatResult($rs, $descr, $query);
    return $rs;
    
}

############################################################
# Result set setup

sub _prepareResultSet {
    #get result set and it's description
    my ($self, $query) = @_;
    
    my $resultSets = [];
    my $descriptions = [];
    my $idx = 0;
    my $description = $descriptions->[0] = {};
    my $rs = $resultSets->[0] = [[undef]];
    my $wantNewSet;
    foreach (@{$query->{+Q_SOURCE}->[0]->{+Q_SOURCEPATH}}) {
	$wantNewSet = @$rs > 100 ? 1 : 0;
	if ($wantNewSet) {
	    $idx++;
	    $description = $descriptions->[$idx] = {};
	    $rs = $resultSets->[$idx] = [[undef]];
	}
	my $subject = $self->_extractElement($_->{+Q_ELEMENT}->[0],
					     $_->{+Q_VARIABLE}->[0]->
					     {+Q_NAME}->[0]);
	if ($subject->[0]{type} eq Q_FUNCTION) {
	    $self->_funcParams($rs,$description,undef,$_->{+Q_ELEMENT}->[0]);
	}
	my $element;
	if (exists $_->{+Q_ELEMENT}->[1] ) {
	    $element = $self->_extractElement($_->{+Q_ELEMENT}->[1],
					      $_->{+Q_VARIABLE}->[1]->
					      {+Q_NAME}->[0]);
	    $self->_funcParams($rs,$description,$subject,$_->{+Q_ELEMENT}->[1])
	      if $element->[0]{type} eq Q_FUNCTION;
	    if ($_->{+Q_HASTARGET} && @{$_->{+Q_ELEMENT}} == 2) {
		my $target;
		if ($_->{+Q_TARGET}[0]{+Q_EXPRESSION}) {
		    $target = $self->_evalExpression(undef,undef,
						     $_->{+Q_TARGET}[0]);
		    $target = [{object=>$target}];
		} else {
		    $target =  $self->_extractElement
		      ($_->{+Q_TARGET}[0],undef);
		    $self->_funcParams($rs,$description,$subject,
				       $_->{+Q_TARGET}->[0])
		      if $target->[0]{type} eq Q_FUNCTION;
		}
		$self->_expandResult($rs, $description, $subject, $element, 
				     $target);
	    } else {
		$self->_expandResult($rs, $description, $subject, $element);
	    }
	    
	} else {
	    $self->_singularResult($rs, $description, $subject);
	}
	for (my $i = 2; $i < @{$_->{+Q_ELEMENT}} ; $i++) {
	    #iterate through sourcepath elements
	    #make "step" over the element

	    $element = $self->_extractElement($_->{+Q_ELEMENT}->[$i],
					      $_->{+Q_VARIABLE}->[$i]->
					      {+Q_NAME}->[0]);
	    $self->_funcParams($rs,$description,undef,$_->{+Q_ELEMENT}->[$i])
	      if $element->[0]{type} eq Q_FUNCTION;

	    if ($_->{+Q_HASTARGET} && @{$_->{+Q_ELEMENT}} == $i+1) {
		my $target =  $self->
		  _extractElement($_->{+Q_TARGET}->[0], undef);
		
		$self->_funcParams($rs,$description,$subject,
				   $_->{+Q_TARGET}->[0])
		  if $target->[0]{type} eq Q_FUNCTION;
		
		$self->_expandResult($rs, $description, undef, $element, 
				     $target);
	    } else {
		$self->_expandResult($rs,$description, undef, $element);
	    }
	}
	
    }
    return $self->_joinResults($resultSets, $descriptions);
}

sub _expandResult {
    my ($self, $rs, $description, $roots, $elements, $targets) = @_;
    my @newRS;
    my %newDescr = %$description;
    my $targets = $targets || [{object=>undef}];
    #iterate through partial result's rows
    for (my $i = 0; $i < @$rs; $i++) {
	my $subjects = $roots || [{object=>$rs->[$i][0]}];
	foreach my $root (@$subjects) {
	    next unless $self->_bindingPreCheck($rs->[$i],
						$description,$root);
	    my $pushSubject;
	    if ($roots && $root->{type} eq Q_VARIABLE) {
		$newDescr{$root->{name}} = @{$rs->[$i]};
		$pushSubject = 1;
	    } 
	    if ($roots && $root->{binding}) {
		$newDescr{$root->{binding}} = @{$rs->[$i]};
		$pushSubject = 1;
	    }


	    foreach my $element (@$elements) {
		my $pushPredicate;
		if ($element->{type} eq Q_VARIABLE && 
		    !($description->{$element->{name}})) {
		    $newDescr{$element->{name}} = @{$rs->[$i]} + $pushSubject;
		    $pushPredicate = 1;
		}
		my $pushObject;
		if ($element->{binding} && 
		    !($description->{$element->{binding}})) {
		    $newDescr{$element->{binding}} = @{$rs->[$i]} + 
		      $pushSubject + $pushPredicate;
		    $pushObject = 1;
		}
		foreach my $target (@$targets) {
		    if ($target->{type} eq Q_VARIABLE && 
			!($description->{$target->{name}})) {
			$newDescr{$target->{name}} = @{$rs->[$i]} + 
			  $pushSubject + $pushPredicate;
			$pushObject = 1;
		    }
		    if ($target->{binding} && 
			!($description->{$target->{binding}})) {
			$newDescr{$target->{binding}} = @{$rs->[$i]} + 
			  $pushSubject + $pushPredicate;
			$pushObject = 1;
		    }
		    my $found = $self->_getStmts($rs->[$i], $description, 
						 $root, $element, $target);
		    foreach my $enum (@{$found}) {
			while (my $st = $enum->getNext) {
			    next unless $self->_bindingCheck($rs->[$i], 
							     $description, 
							     $root, 
							     $st->getSubject);
			    next unless $self->_bindingCheck($rs->[$i], 
							     $description, 
							     $element, 
							     $st->getObject);
			    my @row = @{$rs->[$i]};
			    push @row, $st->getSubject if $pushSubject;
			    push @row, $st->getPredicate if $pushPredicate;
			    push @row, $st->getObject if $pushObject;

			    @row[0] = $st->getObject;
			    push @newRS, \@row;
			}
			$enum->close;
		    }
		}
	    }
	    
	}
    }
    %$description = %newDescr;
    return @$rs = @newRS;
}

sub _singularResult {
    #get all nodes from the model
    my ($self, $rs, $description, $elements) = @_;
    my %newDescr = %$description;
    my @newRS;
    
    for (my $i = 0; $i < @$rs; $i++) {
	my %res;
	my %lit;
	foreach my $element (@$elements) {
	    next unless $self->_bindingPreCheck($rs->[$i],
						$description,$element);
	    unless (defined $element->{object} && 
		    $element->{object}->isLiteral) {
		my $found = $self->_getStmts($rs->[$i], $description, 
					     $element, undef, undef);
		foreach my $enum (@{$found}) {
		    while (my $st = $enum->getNext) {
			next unless $self->_bindingCheck
			  ($rs->[$i], $description,$element, $st->getSubject);
			my @row = @{$rs->[$i]};
			unless ($res{$st->getSubject->getURI}) {
			    if ($element->{type} eq Q_VARIABLE ||
				defined $element->{binding}) {
				push @row, $st->getSubject;
			    }
			    $res{$st->getSubject->getURI} = 1;
			    push @newRS, \@row;
			}
		    }
		    $enum->close;
		}
		
		$found = $self->_getStmts($rs->[$i], $description, 
					  undef, $element, undef);
		foreach my $enum (@{$found}) {
		    while (my $st = $enum->getNext) {
			next unless $self->_bindingCheck
			  ($rs->[$i],$description,$element,$st->getPredicate);
			my @row = @{$rs->[$i]};
			unless ($res{$st->getPredicate->getURI}) {
			    if ($element->{type} eq Q_VARIABLE ||
				defined $element->{binding}) {
				push @row, $st->getPredicate;
			    }
			    $res{$st->getPredicate->getURI} = 1;
			    push @newRS, \@row;
			}
		    }
		    $enum->close;
		}
	    }
	    
	    my $found = $self->_getStmts($rs->[$i], $description, 
					 undef, undef, $element);
	    foreach my $enum (@{$found}) {
		while (my $st = $enum->getNext) {
		    next unless $self->_bindingCheck
		      ($rs->[$i],$description,$element,$st->getObject);
		    my @row = @{$rs->[$i]};
		    if ($st->getObject->isLiteral) {
			unless ($lit{$st->getObject->getValue}) {
			    if ($element->{type} eq Q_VARIABLE ||
				defined $element->{binding}) {
				push @row, $st->getObject;
			    }
			    $lit{$st->getObject->getValue} = 1;
			    push @newRS, \@row;
			}
		    } else {
			unless ($res{$st->getObject->getURI}) {
			    if ($element->{type} eq Q_VARIABLE ||
				defined $element->{binding}) {
				push @row, $st->getObject;
			    }
			    $res{$st->getObject->getURI} = 1;
			    push @newRS, \@row;
			}
		    }
		}
		$enum->close;
	    }
	    
	    my $lastIndex;
	    if (defined $newRS[0]) {
		$lastIndex = @{$newRS[0]} - 1;
	    } else {
		$lastIndex = 0;
	    }
	    $newDescr{$element->{name}} = $lastIndex
	      if $element->{type} eq Q_VARIABLE;
	    $newDescr{$element->{binding}} = $lastIndex
	      if defined $element->{binding};
	}
    }
    %$description = %newDescr;
    return @$rs = @newRS;
}
sub _extractElement {
    my ($self,$node, $binding) = @_;
    my $elements = [];
    my $element = {};
    
    ($element->{type}) = keys %$node;
    if ($element->{type} eq Q_VARIABLE) {
	$element->{name} = $node->{+Q_VARIABLE}->[0]->{+Q_NAME}->[0];
	$element->{object} = undef;
	$element->{binding} = $binding if defined $binding;
	push @$elements, $element;
    } elsif ($element->{type} eq Q_NODE) {
	if ($node->{+Q_NODE}->[0]->{+Q_URI}) {
	    $element->{name} = $node->{+Q_NODE}->[0]->{+Q_URI}->[0];
	    $element->{object} = $self->getOptions->{Factory}->
	      newResource($element->{name});
	} elsif (my $name = $node->{+Q_NODE}->[0]->{+Q_NAME}) {
	    if (@$name > 1) {
		$element->{name} = $self->getOptions->{Namespaces}->
		  {$name->[0]}.$name->[1];
		$element->{object} =  $self->getOptions->{Factory}->
		  newResource($element->{name});
	    } else {
		$element->{name} = $self->getOptions->{Namespaces}->
		  {Default}.$name->[0];
		$element->{object} =  $self->getOptions->{Factory}->
		  newResource($element->{name});
	    }
	}
	$element->{binding} = $binding if defined $binding;
	push @$elements, $element;
    } elsif ($element->{type} eq Q_FUNCTION) {
	$element->{name} = $node->{+Q_FUNCTION}->[0]->{+Q_NAME}->[0];
	$element->{elementpath} = $node->{+Q_FUNCTION}->[0]->
	  {+Q_ELEMENTPATH};
	$element->{binding} = $binding if defined $binding;
	push @$elements, $element;
    } elsif ($element->{type} eq Q_ELEMENT) {
	foreach (@{$node->{+Q_ELEMENT}}) {
	    my $subEls = $self->_extractElement($_, $binding);
	    push @$elements , @$subEls;
	}
    }
    
    
    return $elements;
}
sub _bindingPreCheck {
    my ($self, $row, $descr, $element) = @_;
    my $retVal = 1;
    if ($element->{binding}) {
	my $bound = $self->_evalVar($row, $descr,
				    $element->{binding}, 'RELAX');
	if ($bound) {
	    if ($element->{type} eq Q_VARIABLE) {
		my $val = $self->_evalVar($row, $descr,
					  $element->{name}, 'RELAX');
		$retVal = 0 unless !$val ||
		  $bound->isLiteral == $val->isLiteral &&
		    $bound->getLabel eq $val->getLabel;
	    } elsif ($element->{type} eq Q_FUNCTION) {
		#do nothing here, check bindings after _getStmts
	    } else {
		my $val =$element->{object};
		$retVal = 0 unless !defined $val ||
		  $bound->isLiteral == $val->isLiteral &&
		    $bound->getLabel eq $val->getLabel;
	    }
	}
    }  
    return $retVal;
}
sub _bindingCheck {
    my ($self, $row, $descr, $element, $result) = @_;
    my $retVal = 1;
    if ($element->{binding}) {
	my $bound = $self->_evalVar($row, $descr, 
				    $element->{binding}, 'RELAX');
	$retVal = 0 unless !$bound ||
	  $result->isLiteral==$bound->isLiteral &&
	    $result->getLabel eq $bound->getLabel;
    }
    return $retVal;
}

sub _funcParams {
    #Find variables resolved not yet in function subject and parameters
    #and resolve them.
    my ($self, $rs, $descr, $subjects, $prmNode) = @_;
    my @vars;
    if (defined $subjects) {
	foreach (@$subjects) {
	    push @vars, $_ unless defined $descr->{$_->{name}};
	}
    }
    $self->_findVars($prmNode, \@vars);
    foreach (@vars) {
	$self->_singularResult($rs, $descr, [$_])
	  unless defined $descr->{$_->{name}};
    }
    
}

sub _joinResults {
    my ($self, $resultSets, $descriptions) = @_;
    my $toJoin ;
    my $joined = $resultSets->[0];
    my $joinedDescr = $descriptions->[0];
    for (my $i = 1; $i < @$resultSets; $i++) {
	my @attachements;
	my @extensions;
	$toJoin = $joined;
	undef $joined;
	#descriptions
	my $lastIndex;
	if (defined $toJoin->[0]) {
	    $lastIndex = @{$toJoin->[0]};
	} else {
	    $lastIndex = 0;
	}
	foreach (keys %{$descriptions->[$i]}) {
	    if (exists $joinedDescr->{$_}) {
		push @attachements, [$joinedDescr->{$_},
				     $descriptions->[$i]->{$_} ];
	    } else {
		push @extensions, $descriptions->[$i]->{$_};
		$joinedDescr->{$_} = $lastIndex++;
	    }
	}

	#data
	foreach my $rowTo (@$toJoin) {
	    foreach my $rowFrom (@{$resultSets->[$i]}) {
		my $fit = 1;
		foreach (@attachements) {
		    $fit = 0 unless $rowTo->[$_->[0]]->getLabel eq 
		      $rowFrom->[$_->[1]]->getLabel;
		}
		if ($fit) {
		    my @newRow = @$rowTo;
		    foreach (@extensions) {
			push @newRow, $rowFrom->[$_];
		    }
		    push @$joined, \@newRow;
		}
	    }
	}
    }
    return ($joined, $joinedDescr);
}

############################################################
# Result formatting
    
sub _formatResult {
    my ($self, $rs, $descr, $query) = @_;
    my $newResult = [];
    my @varlist;
    #just extract all variables for now
    foreach my $rsRow (@$rs) {
	my $rows;
	$rows = $self->_evalRow($rsRow, $descr,$query->
				{+Q_RESULTSET}->[0]->{+Q_ELEMENTPATH});
	push @$newResult, @$rows;
    }
    @$rs = @$newResult;
    return $rs
}

sub _evalRow {
    # Apply element path expressions to a row (a set of variables) 
    # It may return one or more rows
    my ($self, $row, $descr, $elementPath) = @_;
    my $result = [[]];
    if (defined $elementPath) {
	foreach (@$elementPath) {
	    my @newResult;
	    my $values = $self->_evalPath($row, $descr, $_);
	    foreach my $rw (@$result) {
		foreach my $value (@$values) {
		    push @newResult, [@$rw, $value];
		}
		push (@newResult,[@$rw, undef]) unless @$values;
	    }
	    $result = \@newResult;
	}
    }
    return $result;
}

############################################################
# Narrowing result according to conditions
sub _applyConditions {
    my ($self, $rs, $descr, $query) = @_;
    my @newResult;
    my $condition = $query->{+Q_CONDITION}->[0];
    return $rs unless $condition;
    foreach my $row (@$rs) {
	if ($self->_evalCondition($row, $descr, $condition)) {
	    push @newResult, $row;
	}
    }
    @$rs = @newResult;
    return $rs;
}

sub _evalCondition {
    my ($self, $row, $descr, $condition) = @_;
    my $fit;
    my @disjunctions;
    if (exists $condition->{+Q_MATCH}) {
	$fit = $self->_evalMatch($row, $descr, $condition->{+Q_MATCH}->[0]);
    } else {
	for (my $i = 0; $i < @{$condition->{+Q_CONDITION}}; $i++) {
	    if (defined ( $condition->{+Q_CONNECTION}->[$i]) && 
		$condition->{+Q_CONNECTION}->[$i] =~ /^or$/i) {
		$fit  = (defined $fit ? $fit : 1) &&
		  $self->_evalCondition($row, $descr, 
				     $condition->{+Q_CONDITION}->[$i]);
		last if $fit;
		undef $fit;
	    } else {
		$fit  = (defined $fit ? $fit : 1) &&
		  $self->_evalCondition($row, $descr, 
				     $condition->{+Q_CONDITION}->[$i]);
	    }
	}
    }
    return $fit;
}

sub _evalMatch {
    my ($self, $row, $descr, $match) = @_;
    my $fit;
    my $set1 = $self->_evalPath($row, $descr, $match->{+Q_PATH}->[0]);
    if (exists $match->{+Q_PATH}->[1]) {
	my $set2 = $self->_evalPath($row, $descr, $match->{+Q_PATH}->[1]);
	foreach my $val1 (@$set1) {
	    foreach my $val2 (@$set2) {
		if (_relation($val1, $val2,$match->{+Q_RELATION}->[0] )) {
		    $fit = 1;
		    last; #in $set2
		    last; #in $set1
		}
	    }
	}
    } else {
	$fit = scalar @$set1;
    }
    return $fit || 0;
}

sub _evalPath {
    my ($self, $row, $descr, $path) = @_;
    my @values;
    
    if (exists $path->{+Q_EXPRESSION}) {
	my $value = $self->_evalExpression($row, $descr, 
					   $path->{+Q_EXPRESSION}->[0]);
	return [$value];
    }
    my $roots;
    my $skipFirstElement;
    if (exists $path->{+Q_CLASSPATH}) {
	$roots = $self->_extractElement($path->{+Q_CLASSPATH}[0]
					{+Q_ELEMENTS}[0]);
    } elsif (exists $path->{+Q_ELEMENT}) {
	$roots = $self->_extractElement($path->{+Q_ELEMENT}[0]);
	$skipFirstElement = 1;
	
    }
    my $newRoots = [];
    foreach my $inst (@$roots) {
	if ($inst->{type} eq Q_VARIABLE) {
	    $inst->{object} = $self->_evalVar($row, $descr, 
					      $inst->{name});
	    $inst->{type} = Q_NODE;
	    push @$newRoots, $inst;
	} elsif ($inst->{type} eq Q_FUNCTION) {
	    my $val = $self->_evalFun($row, $descr,$inst->{name},
				      undef, $inst->{elementpath});
	    foreach (@$val) {
		push @$newRoots, {object=>$_}
	    }
	} else {
	    push @$newRoots, $inst;
	}
	$roots = $newRoots;
    }
    if (exists $path->{+Q_CLASSPATH} && 
	exists $path->{+Q_CLASSPATH}->[0]->{+Q_ELEMENTS}->[1]) {
	# Check classpath (rdf:type) 
	my $passed = 0;
	my $classpath = $path->{+Q_CLASSPATH}->[0];
	my $type = $self->getOptions->{Factory}->
	  newResource(RDF_NS, 'type');
	foreach my $inst (@$roots) {
	    my $classes = $self->_extractElement($classpath->
						 {
						  +Q_ELEMENTS}->[1]);
	    foreach my $class (@$classes) {
		if ($inst->{type} eq Q_VARIABLE) {
		    $inst->{object} = $self->_evalVar($row, $descr, 
						      $inst->{name});
		    $inst->{type} = Q_NODE;
		}
		my $found = $self->_getStmts($row, $descr, 
					     $inst,{object=>$type}, $class);
		foreach my $enum (@$found) {
		    while ($enum->getNext) {
			$passed = 1;
			last;
		    }
		    $enum->close;
		}
	    }
	}
	return [] unless $passed;
    }
    my $node;
    if (($node = $path->{+Q_ELEMENTS}) || ($node = $path->{+Q_ELEMENT})) {
	foreach (@$node) {
	    if ($skipFirstElement) {
		$skipFirstElement = 0;
		next;
	    }
	    my $newRoots = [];
	    my $element = $self->_extractElement($_);
	    foreach my $subj (@$roots) {
		foreach my $pred (@$element) {
		    my $found = $self->_getStmts($row, $descr, 
						 $subj,$pred,undef);
		    foreach my $enum (@{$found}) {
			while (my $st = $enum->getNext) {
			    my $root = {};
			    $root->{object} = $st->getObject;
			    push (@$newRoots, $root);
			}
			$enum->close;
		    }
		}
	    }
	    $roots = $newRoots;
	}
    } 
    my %lit;
    my %res;
    foreach (@$roots) {
	if ($_->{object}->isLiteral) {
	    next if $lit{$_->{object}->getValue};
	    $lit{$_->{object}->getValue} = 1;
	} else {
	    next if $res{$_->{object}->getURI};
	    $res{$_->{object}->getURI} = 1;
	}
	push @values, $_->{object};
    }
    return \@values;
}

sub _getStmts {
    my ($self, $row, $descr, $s, $p, $o) = @_;
    #Returns hash pointer with keys description, context and data. Description and row has it's usual content, description can bind variables to resulting data considering statement to be a (s,p,o) list appended to row. Description and row contain only new variables found while resolving _getStmts. Data is an array of enumerators.
    #Example: {description=>{?x=>0},row=>[resource1], data=>[enum1,enum2,...]}
    my @subjects;
    my @predicates;
    my @objects;
    my @retValEnum;
    #Evaluate variables if you can
    foreach ($s,$p,$o) {
	$_ = {object=>undef, type=>Q_NODE} unless defined $_;
	$_->{object} = $self->_evalVar($row,$descr,$_->{name},'RELAX')
	  if $_->{type} eq Q_VARIABLE;
    }
    if ($s->{type} eq Q_FUNCTION) {
	my $val = $self->_evalFun($row, $descr, $s->{name}, undef,
				  $s->{elementpath});
	push @subjects, @{$val};
    } else {
	push @subjects, $s->{object};
    }
    if ($o->{type} eq Q_FUNCTION) {
	my $val = $self->_evalFun($row, $descr, $o->{name}, undef, 
				  $o->{elementpath});
	push @objects, @{$val};
    } else {
	push @objects, $o->{object};
    }
    if ($p->{type} eq Q_FUNCTION && defined $s->{object}) {
	my $fakePredicate = new RDF::Core::Resource($p->{name});
	my @retValArray;
	foreach my $subject (@subjects) {
	    my $val = $self->_evalFun($row, $descr, $p->{name}, $subject,
				      $p->{elementpath});
	    foreach my $object (@objects) {
		foreach (@{$val}) {
		    if (!defined $object || 
			($_->isLiteral == $object->isLiteral &&
			 $_->getLabel eq $object->getLabel)) {
			push @retValArray , new RDF::Core::Statement
			  ($subject,$fakePredicate,$_);
		    }
		}
	    }
	}
	push @retValEnum, new RDF::Core::Enumerator::Memory(\@retValArray)
    } else {
	if ($p->{type} eq Q_FUNCTION) {
	    my $val = $self->_evalFun($row, $descr, $p->{name}, undef, 
				      $p->{elementpath});
	    push @predicates, @{$val};
	} else {
	    push @predicates, $p->{object};
	}
	
	foreach my $subject (@subjects) {
	    next if defined $subject && $subject->isLiteral;
	    foreach my $predicate (@predicates) {
		next if defined $predicate && $predicate->isLiteral;
		foreach my $object (@objects) {
		    push @retValEnum, $self->getOptions->{Model}->
		      getStmts($subject, $predicate, $object);
		}
	    }
	}
    }

    return \@retValEnum;
}
sub _evalVar {
    my ($self, $row, $descr, $varName, $relax) = @_;

    my $retVal = $row->[$descr->{$varName}] if defined $descr->{$varName};
    croak "Variable not found: $varName\n"
      unless defined $retVal || $relax;
    return $retVal;
}

sub _evalFun {
    #TODO: Functions should be defined in a separate module
    my ($self, $row, $descr, $name, $subject, $elementPath) = @_;
    my $retVal= [];
    my $params;
    my $functions = $self->getOptions->{Functions};
    $params = $self->_evalRow($row, $descr, $elementPath);
    foreach (@$params) {
	my $fun = $functions->getFunctions->{$name};
	croak "Unknown function: $name" unless $fun;
	my $ret = &{$fun}($functions,$subject,$_);
	push @$retVal, @{$ret};
    }
    

    return $retVal;
}

sub _evalExpression {
    my ($self, $row, $descr, $expression) = @_;
    my $retVal;
    if (exists $expression->{+Q_LITERAL}) {
	$retVal = $self->getOptions->{Factory}->
	  newLiteral($expression->{+Q_LITERAL}->[0]);
    } else {
	$retVal = $self->_evalExpression($row, $descr,
					 $expression->{+Q_EXPRESSION}->[0]);
	for (my $i=1; $i < @{$expression->{+Q_EXPRESSION}}; $i++) {
	    #TODO: Operator precedence may be needed if more operators
	    # are implemented
	    my $newVal = $self->_evalExpression($row, $descr, $expression->
						{+Q_EXPRESSION}->[$i]);
	    $retVal = $self->_operation ($retVal,$newVal,$expression->
				  {+Q_OPERATION}->[$i-1]);
	    
	}
	
    } 
    return $retVal;
}

############################################################
# Utils

sub _relation {
    #Compare two nodes in graph
    my ($lval, $rval, $rel) = @_;
    my $retVal = 0;

    return undef 
      unless defined $lval && defined $rval;

    if ($rel eq '=') {
	$retVal = 1 if $lval->isLiteral == $rval->isLiteral &&
	  $lval->getLabel eq $rval->getLabel;
    } elsif ($rel eq '<=') {
	$retVal = 1 if $lval->isLiteral && $rval->isLiteral &&
	  $lval->getLabel le $rval->getLabel;
    } elsif ($rel eq '<') {
	$retVal = 1 if $lval->isLiteral && $rval->isLiteral &&
	  $lval->getLabel lt $rval->getLabel;
    } elsif ($rel eq '>=') {
	$retVal = 1 if $lval->isLiteral && $rval->isLiteral &&
	  $lval->getLabel ge $rval->getLabel;
    } elsif ($rel eq '>') {
	$retVal = 1 if $lval->isLiteral && $rval->isLiteral &&
	  $lval->getLabel gt $rval->getLabel;
    } else {
	croak "Relation not defined: $rel\n";
    }
    return $retVal;
}

sub _operation {
    my ($self, $lval, $rval, $op) = @_;
    my $retVal;

    return undef 
      unless defined $lval && defined $rval;
    if ($op eq '|') {
	if ($lval->isLiteral && $rval->isLiteral) {
	    $retVal = $self->getOptions->{Factory}->
	      newLiteral($lval->getValue.$rval->getValue);
	}
    } else {
	croak "Operation not defined: $op\n";
    }
    return $retVal
}
sub _findVars {
    # Return variable elements found in given subtree of query
    my ($self, $node, $vars) = @_;
    if (ref $node eq 'ARRAY') {
	foreach (@$node) {
	    $self->_findVars($_, $vars) if ref $_;
	}
    } elsif (ref $node eq 'HASH') {
	if ($node->{+Q_ELEMENT}) {
	    for (my $i = 0; $i < @{$node->{+Q_ELEMENT}}; $i++) {
		my $binding;
		$binding = $_->{+Q_VARIABLE}->[$i]->{+Q_NAME}->[0]
		  if $_->{+Q_VARIABLE};
		my $element = $self->_extractElement($_->{+Q_ELEMENT}->[$i],
						     $binding);
		foreach (@$element) {
		    push @$vars, $_ if $_->{type} eq Q_VARIABLE;
		}
	    }
	} else {
	    foreach (values %$node) {
		$self->_findVars($_, $vars) if ref $_;
	    }
	}
    } else {
	croak "Can't find vars in subtree - $node is not a tree node\n";
    }
}

1;
__END__

=head1 NAME

RDF::Core::Evaluator - gets a query object that RDF::Core::Query parsed and evaluates the query. 

=head1 SYNOPSIS

  my %namespaces = (Default => 'http://myApp.gingerall.org/ns#',
                    ns     => 'http://myApp.gingerall.org/ns#',
		   );
  my $evaluator = new RDF::Core::Evaluator
    (Model => $model,            #an instance of RDF::Core::Model
     Factory => $factory,        #an instance of RDF::Core::NodeFactory
     Namespaces => \%namespaces,
    );
  my $query = new RDF::Core::Query(Evaluator=> $evaluator);

=head1 DESCRIPTION

The evaluator is just to be created and passed to query object, that uses it to evaluate the query.

=head2 Interface

=over 4

=item * new(%options)

Available options are:

=over 4

=item * Model

RDF::Core::Model object that contains data to be queried.

=item * Functions

RDF::Core::Function object is a functions library.

=item * Factory

RDF::Core::NodeFactory object, that produces resource and literal objects.

=item * Namespaces

A hash containing namespace prefixes as keys and URIs as values. See more in paragraph B<Names and URIs> in RDF::Core::Query, 

=back

=back

=head1 LICENSE

This package is subject to the MPL (or the GPL alternatively).

=head1 AUTHOR

Ginger Alliance, rdf@gingerall.cz

=head1 SEE ALSO

RDF::Core::Query

=cut


