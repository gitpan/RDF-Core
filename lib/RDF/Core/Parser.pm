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

package RDF::Core::Parser;

use strict;
use Data::Dumper;

require URI;
require XML::Parser;

########################################
# constants

# XML
use constant XML_NS  => 'http://www.w3.org/XML/1998/namespace';
use constant XMLA_LANG => XML_NS . 'lang';

# RDF

use constant RDF_NS => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
use constant RDF_RDF         => RDF_NS . 'RDF';
use constant RDF_DESCRIPTION => RDF_NS . 'Description';
use constant RDF_BAG         => RDF_NS . 'Bag';
use constant RDF_ALT         => RDF_NS . 'Alt';
use constant RDF_SEQ         => RDF_NS . 'Seq';
use constant RDF_LI          => RDF_NS . 'li';
use constant RDF_TYPE        => RDF_NS . 'type';
use constant RDF_OBJECT      => RDF_NS . 'object';
use constant RDF_SUBJECT     => RDF_NS . 'subject';
use constant RDF_PREDICATE   => RDF_NS . 'predicate';
use constant RDF_STATEMENT   => RDF_NS . 'Statement';

use constant RDFA_ABOUT      => RDF_NS . 'about';
use constant RDFA_ABOUTEACH  => RDF_NS . 'aboutEach';
use constant RDFA_ID         => RDF_NS . 'ID';
use constant RDFA_BAGID      => RDF_NS . 'bagID';
use constant RDFA_RESOURCE   => RDF_NS . 'resource';
use constant RDFA_PARSETYPE  => RDF_NS . 'parseType';
use constant RDFA_TYPE       => RDF_NS . 'type';

use constant PARSE_LITERAL   => "Literal";
use constant PARSE_RESOURCE  => "Resource";

use constant F_IDLE       => 0;
use constant F_PARSING    => 1;
use constant F_HASROOT    => 2;

########################################
# rdf types
use constant RDFT_UNKNOWN => 0;

use constant RDFT_BAG        => 1;
use constant RDFT_SEQ        => 2;
use constant RDFT_ALT        => 4;
use constant RDFT_CONTAINER  => RDFT_BAG | RDFT_SEQ | RDFT_ALT;

my %RDF_TYPES = (RDF_BAG()     => RDFT_BAG,
		 RDF_SEQ()     => RDFT_SEQ,
		 RDF_ALT()     => RDFT_ALT,
		);

########################################
# RDF node types (according to spec.)
use constant NODE_UNKNOWN => 0;

use constant NODE_RDF => 1;

use constant NODE_DESCRIPTION => 2;

use constant NODE_BAG   => 4;
use constant NODE_SEQ   => 8;
use constant NODE_ALT   => 16;
use constant NODE_CONTAINER  => NODE_BAG | NODE_SEQ | NODE_ALT;

use constant NODE_PROPERTY   => 32;
use constant NODE_PROPERTY_1 => 64;  #simple w/ value 
use constant NODE_PROPERTY_2 => 128; #simple literal
use constant NODE_PROPERTY_3 => 256; #parse resource
use constant NODE_PROPERTY_4 => 512; #with id/resource
use constant NODE_PROPERTY_MASK => 
  NODE_PROPERTY_1 | NODE_PROPERTY_2 | NODE_PROPERTY_3 | NODE_PROPERTY_4;

use constant NODE_TYPED => 1024;
use constant NODE_MEMBER => 2048;

use constant NODE_OBJ => NODE_DESCRIPTION | NODE_CONTAINER | NODE_TYPED;

our %RDF_TYPECONST = (Bag      => NODE_BAG,
		      Seq      => NODE_SEQ,
		      Alt      => NODE_ALT,
		     );

our %RDF_TYPENAMES = reverse %RDF_TYPECONST;

########################################
# ordinary methods
sub new {
    my ($class, %params) = @_;
    $class = ref $class || $class;
    my $self = { 
		assert => $params{Assert},
		baseuri => $params{BaseURI},
		strict => $params{STRICT},
		inlineuri => $params{InlineURI},
	       };
    bless $self, $class;
    return $self;
}

sub parse {
    my ($self, $what) = @_;
    #be careful about the circular reference
    my $expatParser = $self->_createExpatParser;
    $self->_setFlag(F_PARSING);
    $expatParser->parse($what);
    $self->_clearFlag(F_PARSING);
}

sub parseFile {
    my ($self, $filename) = @_;
    my $expatParser = $self->_createExpatParser;
    $self->_setFlag(F_PARSING);
    $expatParser->parsefile($filename);
    $self->_clearFlag(F_PARSING);
}

########################################
# tools
# attribure processing
sub _getNamespaces {
    my ($self, $attrs) = @_;
    my %ret;
    foreach my $name (keys %$attrs) {
	if ($name =~ /xmlns:?(.*)/) {
	    my $abbr = $1 || '';
	    $ret{$abbr} = $$attrs{$name};
	    delete $$attrs{$name};
	}
    }
    return \%ret;
}

sub _expandAttributes {
    my ($self, $element, %attrs) = @_;
    my $ret = {};
    foreach my $name (keys %attrs) {
	#expand name and store
	$name =~ /((.*):|^)(.*)/;
	my ($ns, $local) = ($2, $3);
	$ns = $ns ? $self->_findNS($ns) : $element->{ns};
	$$ret{$ns . $local} = {
			       name => $local, 
			       ns => $ns, #
			       value => $attrs{$name},
			      };
    }
    return $ret;
}

sub _getAbout {
    my ($self, $attrs) = @_;
    if (exists $$attrs{+RDFA_ABOUT}) {
	return delete $$attrs{+RDFA_ABOUT};
    } else {
	return undef;
# 	return {
# 		name => 'about',
# 		ns => RDF_NS,
# 		value => $self->_getImplicitURI,
# 	       };
    }
}

sub _getResource {
    my ($self, $attrs) = @_;
    if (exists $$attrs{+RDFA_RESOURCE}) {
	return delete $$attrs{+RDFA_RESOURCE};
    } else {
	return undef;
    }
}

sub _getElementResource {
    my ($self, $element) = @_;
    undef my $ret;

    if ($element->{resource}) {
	$ret = $element->{resource};
    }
    elsif ($element->{rnode}) {
	$ret = $self->_uri($element->{rnode});
    }

    return $ret;
}

# miscellaneous
sub _localName {
    my ($self, $in) = @_;
    $in =~ /((.*):|^)(.*)/;
    return $3;
}

sub _nsAbbr {
    my ($self, $in) = @_;
    $in =~ /((.*):|^)(.*)/;
    return $2 || '';
}

sub _uri {
    my ($self, $element) = @_;
    return $element->{uri} || ($element->{uri} = $self->_getImplicitURI);
}

sub _validFirstLevel {
    my ($self, $element) = @_;
    return $element->{qname} eq RDF_DESCRIPTION;
}

sub _doAssert {
    my ($self, $subject, $params, $stmt) = @_;
    my %params = %$params;
    #ordinary assertion
    unless ($subject->{abouteach}) {
	&{$self->{assert}}(%params);
    } 
    #about each caching
    else {
	my $slot = ${$self->{abouteach}}{$subject->{abouturi}};
	$slot = ${$self->{abouteach}}{$subject->{abouturi}} = [] 
	  unless $slot;
	my %foo = map {($_, $params{$_})} grep {$_ !~ /^subject/} keys %params;
	push @$slot, {%foo};
    }
    #reification
    if ($subject->{bagid} || $stmt) {
	my $suri = $stmt || $self->_getImplicitURI;
	my $stmt = {
		    statement_uri => $suri,
		    %params,
		   };
	$self->_assertReification($stmt);
	if ($subject->{bagid}) {
	    my $buri = $subject->{baguri};
	    $self->{urimembers}{$buri} = [] 
	      unless exists $self->{urimembers}{$buri};
	    push @{$self->{urimembers}{$buri}}, $suri;
	}
    }
}

use Carp;
sub _assertReification {
    my ($self, $stmt) = @_;
    #type
    my $params = {subject_uri => $stmt->{statement_uri},
		  predicate_ns => RDF_NS,
		  predicate_name => 'type',
		  predicate_uri => RDF_TYPE,
		  object_ns => RDF_NS,
		  object_name => 'Statement',
		  object_uri => RDF_STATEMENT,
		 };
    $self->_doAssert({}, $params);
    #subject
    $params = {subject_uri => $stmt->{statement_uri},
	       predicate_ns => RDF_NS,
	       predicate_name => 'subject',
	       predicate_uri => RDF_SUBJECT,
	       object_uri => $stmt->{subject_uri},
	      };
    $params->{object_ns} = $stmt->{subject_ns} if $stmt->{subject_ns};
    $params->{object_name} = $stmt->{subject_name} if $stmt->{subject_name};
    $self->_doAssert({}, $params);
    #predicate
    $params = {subject_uri => $stmt->{statement_uri},
	       predicate_ns => RDF_NS,
	       predicate_name => 'predicate',
	       predicate_uri => RDF_PREDICATE,
	       object_uri => $stmt->{predicate_uri},
	      };
    $params->{object_ns} = $stmt->{predicate_ns} if $stmt->{predicate_ns};
    $params->{object_name} = $stmt->{predicate_name} if $stmt->{predicate_name};
    $self->_doAssert({}, $params);
    #object
    $params = {subject_uri => $stmt->{statement_uri},
	       predicate_ns => RDF_NS,
	       predicate_name => 'object',
	       predicate_uri => RDF_OBJECT,
	      };
    if ($stmt->{object_uri}) {
	$params->{object_uri} = $stmt->{object_uri};
	$params->{object_ns} = $stmt->{object_ns} if $stmt->{object_ns};
	$params->{object_name} = $stmt->{object_name} if $stmt->{object_name};
    } else {
	$params->{object_literal} = $stmt->{object_literal};
    }
    $self->_doAssert({}, $params);
}

sub _assertAttributes {
    my ($self, $subject, $attrs) = @_;
    #foreach my $attr (grep {$_->{ns} ne RDF_NS} values %$attrs) {
    foreach my $attr (values %$attrs) {
	my $qname = $attr->{ns} . $attr->{name};
	#test member for ebaoutEach on ID
	my $re = '^' . RDF_NS . '_\d+$';
	if ($qname =~ /$re/) {
	    #it seems there is nothing to catch for ID, because
	    #this member is literal and has no resource
	}
	#assert
	my %params = (
		      #subject_ns => $subject->{ns},
		      #subject_name => $subject->{name},
		      subject_uri => $self->_uri($subject),
		      predicate_ns => $attr->{ns},
		      predicate_name => $attr->{name},
		      predicate_uri => $qname,
		      object_literal => 
		      defined $attr->{value} ? $attr->{value} : "",
		     );
	$self->_doAssert($subject, \%params);
    }
}

sub _assertPropAttrs {
    my ($self, $element, $attrs) = @_;
    my $type = $element->{type};
    #if (($type == NODE_DESCRIPTION) || ($type == NODE_TYPED)) {
    if ($type & NODE_OBJ) {
	my $subject = ${$self->{subjects}}[-1];
	$self->_assertAttributes($subject, $attrs);
    }
    elsif ($type == NODE_PROPERTY_4) {
	my $subject = {uri => $self->_getElementResource($element)};
	$self->_assertAttributes($subject, $attrs);
    }
}

sub _assertRDFAttrs {
    my ($self, $element, $attrs) = @_;
    my $type = $element->{type};
    if ($type == NODE_TYPED) {
	$self->_assertRDFTypeElement($element, $element);
    }
    elsif ($type & NODE_CONTAINER) {
	$self->_assertRDFType($element, $type);
    } 
    elsif ($element->{rdftype}) {
	$self->_assertRDFTypeString($element, $element->{rdftype});
	my $ctype = $RDF_TYPES{$element->{rdftype}};
	$element->{containertype} = $ctype if $ctype and RDFT_CONTAINER;
    }
}

sub _assertElement {
    my ($self, $expat, $subject, $element) = @_;

    my $uri = $self->_getElementResource($element);
    if ($element->{resource} && __trim($element->{text})) {
	$expat->xpcroak("predicate has both of resource and literal");
    }

    my %object;
    if ($uri) {
	%object = (object_uri => $uri);
    } else {
	%object = (object_literal => 
		   defined $element->{text} ? $element->{text} : "");
    }

    my %params = (
		#  subject_ns => $subject->{ns},
		#  subject_name => $subject->{name},
		  subject_uri => $self->_uri($subject),
		  predicate_ns => $element->{ns},
		  predicate_name => $element->{name},
		  predicate_uri => $element->{qname},
		  %object,
		 );

    $self->_doAssert($subject, \%params, $element->{uri});
}

sub _assertRDFType {
    my ($self, $subject, $type) = @_;
    my %params = (
		  #subject_ns => $subject->{ns},
		  #subject_name => $subject->{name},
		  subject_uri => $self->_uri($subject),
		  predicate_ns => RDF_NS,
		  predicate_name => 'type',
		  predicate_uri => RDF_TYPE,
		  object_ns => RDF_NS,
		  object_name => $RDF_TYPENAMES{$type},
		  object_uri => RDF_NS . $RDF_TYPENAMES{$type},
		 );
    $self->_doAssert($subject, \%params);
}

sub _assertRDFTypeString {
    my ($self, $subject, $string) = @_;
    my %params = (
		  #subject_ns => $subject->{ns},
		  #subject_name => $subject->{name},
		  subject_uri => $self->_uri($subject),
		  predicate_ns => RDF_NS,
		  predicate_name => 'type',
		  predicate_uri => RDF_TYPE,
		  object_uri => $string,
		 );
    $self->_doAssert($subject, \%params);
}

sub _assertRDFTypeElement {
    my ($self, $subject, $element) = @_;
    my %params = (
		  #subject_ns => $subject->{ns},
		  #subject_name => $subject->{name},
		  subject_uri => $self->_uri($subject),
		  predicate_ns => RDF_NS,
		  predicate_name => 'type',
		  predicate_uri => RDF_TYPE,
		  object_ns => $element->{ns},
		  object_name => $element->{name},
		  object_uri => $element->{qname},
		 );
    $self->_doAssert($subject, \%params);
}

sub _assertAboutEach {
    my $self = shift;
    for my $m (keys %{$self->{urimembers}}) {
	my $members = $self->{urimembers}{$m};
	my $abouts = $self->{abouteach}{$m};
	next unless $abouts;
	for my $a (@$abouts) {
	    my %aparams = %$a;
	    for my $s (@$members) {
		my %params = %aparams;
		$params{subject_uri} = $s;
		#assert w/ empty subject
		$self->_doAssert({}, \%params);
	    }
	}
    }
}

sub _getLIURI {
    my ($self, $subject) = @_;
    my $id = "_" . ++$subject->{bag_unique};
    return (RDF_NS, $id, RDF_NS . $id);
}

sub __trim {
    my $val = shift;
    $val =~ s/^\s*$//sg;
    return $val;
}

sub __checkParseType {
    my $element = shift;
    return unless $element->{parsetype};
    my $re = PARSE_LITERAL . "|" . PARSE_RESOURCE;
    $element->{parsetype} = PARSE_LITERAL
      unless $element->{parsetype} =~ /$re/;
}

sub _updateElement {
    my ($self, $expat, $element, $attrs) = @_;
    #rdf attributes
    my $about = delete $$attrs{+RDFA_ABOUT};
    my $abouteach = delete $$attrs{+RDFA_ABOUTEACH};
    my $id = delete $$attrs{+RDFA_ID};
    my $bagid = delete $$attrs{+RDFA_BAGID};
    my $parsetype = delete $$attrs{+RDFA_PARSETYPE};
    my $rdftype = delete $$attrs{+RDFA_TYPE};
    my $resource = delete $$attrs{+RDFA_RESOURCE};
    my $xmllang = delete $$attrs{+XMLA_LANG};
    $element->{about} = $about ? $about->{value} : undef;
    $element->{abouteach} = $abouteach ? $abouteach->{value} : undef;
    $element->{id} = $id ? $id->{value} : undef;
    $element->{bagid} = $bagid ? $bagid->{value} : undef;
    $element->{parsetype} = $parsetype ? $parsetype->{value} : undef;
    __checkParseType($element);
    $element->{rdftype} = $rdftype ? $rdftype->{value} : undef;
    $element->{resource} = $resource ? $resource->{value} : undef;
    $element->{xmllang} = $xmllang ? $xmllang->{value} : undef;

    #create uri/about-uri (from about or id)
    if ($element->{about}) {
	my $u = new URI($element->{about});
	$u = $u->abs($self->{baseuri});
	$element->{uri} = $u->as_string;
    } elsif ($element->{id}) {
	my $u = new URI($self->{baseuri});
	$u->fragment($element->{id});
	$element->{uri} = $u->as_string;
    } elsif ($element->{abouteach}) {
	my $u = new URI($self->{baseuri});
	#$u->fragment($element->{abouteach}); _fixme_
	$element->{abouturi} = $self->{baseuri} . $element->{abouteach};
    } 
    if ($element->{resource}) {
	my $u = new URI($element->{resource});
	$element->{resource} = $u->abs($self->{baseuri});
    }

    #create bagid uri
    if ($element->{bagid}) {
	my $u = new URI($self->{baseuri});
	$u->fragment($element->{bagid});
	$element->{baguri} = $u->as_string;
    }

    #rename element if it is the rdf:li (I hope it is correct)
    if ($element->{qname} eq RDF_LI) {
	#$expat->xpcroak("deprecated element") if $self->{strict};
	my ($ns, $name, $uri) = $self->_getLIURI(${$self->{subjects}}[-1]);
	#we rename the whole element, hopefuly it doesn't matter
	$element->{ns} = $ns;
	$element->{name} = $name;
	$element->{qname} = $uri;
    }
}

sub _analyzePath {
    my ($self, $expat, $attrs) = @_;
    my $path = $self->{path};

    #guess the node type
    my $ce = $$path[-1];
    my $ct; #current type
    my $re = '^' . RDF_NS . '_(\d+)$';
  SWITCH:
    for ($ce->{qname}) {
	$_ eq RDF_RDF && do {$ct = NODE_RDF; last SWITCH;};
	$_ eq RDF_DESCRIPTION && do {$ct = NODE_DESCRIPTION; last SWITCH;};
	$_ eq RDF_BAG && do {$ct = NODE_BAG; 
			     $ce->{containertype} = RDFT_BAG;
			     last SWITCH;};
	$_ eq RDF_SEQ && do {$ct = NODE_SEQ; 
			     $ce->{containertype} = RDFT_SEQ;
			     last SWITCH;};
	$_ eq RDF_ALT && do {$ct = NODE_ALT; 
			     $ce->{containertype} = RDFT_ALT;
			     last SWITCH;};
	#$_ eq RDF_LI && do {$ct = NODE_MEMBER; last SWITCH;}; #never may happen
	#/$re/ && do {$ct = NODE_MEMBER; last SWITCH;};
	#deafult
	$ct = NODE_UNKNOWN; #for now - property or typed object
    }

#     $expat->xpcroak("deprecated element") 
#       if $self->{strict} && ($ct & NODE_CONTAINER);

    #check validity in the context of the parent node
    #and optionally fix the node type for NODE_UNKNOWN
    my $pe = $$path[-2];
    my $pt = $pe ? $pe->{type} : undef;

    if (! defined $pt) {
	#the very beginning
	$expat->xpcroak("bad root element") 
	  unless ($ct & NODE_OBJ) || $ct ==NODE_RDF;
	if ($ct == NODE_UNKNOWN) {
	    $ct = NODE_TYPED;
	}
    } 
    elsif ($pt == NODE_RDF) {
	$ct = NODE_TYPED if $ct == NODE_UNKNOWN;
	$expat->xpcroak("invlaid first level element") unless $ct & NODE_OBJ;
    }
    elsif ($pt == NODE_DESCRIPTION || $pt == NODE_TYPED) {
	$expat->xpcroak("invalid node in the Description element") 
	  unless $ct == NODE_UNKNOWN;
	#force type to property - checked later
	$ct = NODE_PROPERTY;
    } 
    elsif ($pt & NODE_CONTAINER) {
	$expat->xpcroak("invalid node in container") 
	  unless $ct == NODE_UNKNOWN;
	$ct = NODE_PROPERTY;
    } 
    elsif ($pt == NODE_MEMBER || ($pt & NODE_PROPERTY_MASK)) {
	$expat->xpcroak("invalid node in the memeber element") unless
	  $ct == NODE_UNKNOWN || ($ct & NODE_OBJ);
	if ($ct == NODE_UNKNOWN) {
	    if ($pt == NODE_PROPERTY_3) {
		$ct = NODE_PROPERTY;
	    } else {
		$ct = NODE_TYPED;
	    }
	}
    } 
    else {
	$expat->xpcroak("unknown parent node type: $pt");
    }

    #if we found, that we're NODE_PROPERTY, we'' try to determine the subtype
    my $ruri = $self->_getElementResource($ce);
    if ($ct == NODE_PROPERTY) {
	if ($ruri || %$attrs) {
	    $ct = NODE_PROPERTY_4;
	    $ce->{resource} = $self->_getImplicitURI unless $ce->{resource};
	} elsif ($ce->{parsetype} eq PARSE_RESOURCE) {
	    $ct = NODE_PROPERTY_3;
	} elsif ($ce->{parsetype} eq PARSE_LITERAL) {
	    $ct = NODE_PROPERTY_2;
	} else {
	    $ct = NODE_PROPERTY_1;
	}
    }

    #set node type
    $ce->{type} = $ct;
}

sub __slice {
    my ($element, $keys) = @_;
    my (%set, $count);
    foreach (@$keys) {
	$set{$_} = $element->{$_};
	$count++ if defined $set{$_};
    }
    return wantarray ? %set : $count;
}

sub _checkAttributes {
    my ($self, $expat, $element, $attrs) = @_;

    my $allset = [qw(about abouteach id bagid parsetype rdftype resource)];
    my $aboutset = [qw(about abouteach id)];
    my $memberset = [qw(parsetype resource)];
    #all except about and bag
    my $inverseset1 = [qw(parsetype resource)];
    #2 - all except id
    my $inverseset2 = [qw(about abouteach bagid parsetype rdftype resource)];
    #3 - all except id and parsetype
    my $inverseset3 = [qw(about abouteach bagid rdftype resource)];
    #4 - all except resource, id and bag
    my $inverseset4 = [qw(about abouteach parsetype rdftype)];

    my $et = $element->{type};

    my $hasabout = scalar __slice($element, $aboutset);

    #check xml attributes (shouldn't be any)
    if (grep {$_->{ns} eq XML_NS} values %$attrs) {
	$expat->xpcroak("invalid xml attribute");
    }

    if ($et == NODE_RDF) {
	$expat->xpcroak("invalid attribute") 
	  if scalar __slice($element, $allset) || %$attrs;
    } 
    elsif ($et == NODE_DESCRIPTION || $et == NODE_TYPED) {
	$expat->xpcroak("invalid attribute")
	  if scalar __slice($element, $inverseset1);
    } 
    elsif ($et & NODE_CONTAINER) {
	$expat->xpcroak("invalid attribute") 
	  if scalar __slice($element, $inverseset1);
	$element->{hasmembers} = 1 if %$attrs;
    } 
    elsif ($et == NODE_MEMBER) {
	$expat->xpcroak("invalid attribute")
	  if scalar __slice($element, $memberset) > 1;
    }
    elsif ($et == NODE_PROPERTY_1) {
	$expat->xpcroak("invalid attribute") 
	  if scalar __slice($element, $inverseset2) || %$attrs;
    }
    elsif ($et == NODE_PROPERTY_2 || $et == NODE_PROPERTY_3) {
	$expat->xpcroak("invalid attribute") 
	  if scalar __slice($element, $inverseset3) || %$attrs;
    }
    elsif ($et == NODE_PROPERTY_4) {
	$expat->xpcroak("invalid attribute") 
	  if scalar __slice($element, $inverseset4);
	$element->{hasprops} = 1 if %$attrs;
    }
}

sub _checkNoResource {
    my ($self, $expat, $element) = @_;
    $expat->xpcroak("element contain both of rdf:resource and nested node")
      if $element->{resource};
}

#creates the 'current' subject
sub _createSubject {
    my ($self, $expat, $element) = @_;
    my $type = $element->{type};
    if ($type & NODE_OBJ) {
	push @{$self->{subjects}}, $element;
	$element->{subject} = 1;
	my $parent = ${$self->{path}}[-2];
	if ($parent && $parent->{type} != NODE_RDF) {
	    $self->_checkNoResource($expat, $parent);
	    $parent->{rnode} = $element;
	}
    }
    elsif ($type == NODE_PROPERTY_3) {
	my $subject = {uri => $self->_getImplicitURI};
	push @{$self->{subjects}}, $subject;
	$element->{presubject} = 1;
	$self->_checkNoResource($element);
	$element->{rnode} = $subject;
    }
}

########################################
# handlers
sub init {
    my ($self, $expat) = @_;
#    print "---> init\n";
    $self->{path} = [];
    $self->{subjects} = [];
    $self->{status} = F_IDLE;
    $self->{unique} = 0;
    $self->{urimembers} = {};
    $self->{abouteach} = {};
}

sub final {
    my ($self, $expat) = @_;
#    print "---> final\n";
    $self->_assertAboutEach;
#    print "subjects: ", Dumper($self->{subjects});
#    print "urimembers: ", Dumper($self->{urimembers});
#    print "abouteachs: ", Dumper($self->{abouteach});
}

sub start {
    my ($self, $expat, $name, %attrs) = @_;
    my $element;
    my $subject;

    # extract namespace declarations and create element
    push @{$self->{path}}, 
      $element = { name => $self->_localName($name),
		   nslist => $self->_getNamespaces(\%attrs), 
		   members => [],
		 };
    $element->{ns} = $self->_findNS($self->_nsAbbr($name));
    $element->{qname} = $element->{ns} . $element->{name};

    #expand attributes (must follow namespaces handling)
    my $attrs = $self->_expandAttributes($element, %attrs);

    #update element (rename, read red attributes)
    $self->_updateElement($expat, $element, $attrs);

    #now we have all (almost) information to decide on node type
    #we must check the validity and update element status
    $self->_analyzePath($expat, $attrs);

    #check whether attributes match the node type
    $self->_checkAttributes($expat, $element, $attrs);

    # tool variables
    #c'on baby...
    $self->_createSubject($expat, $element);
    
    #spit out attributes
    $self->_assertPropAttrs($element, $attrs);
    $self->_assertRDFAttrs($element, $attrs);

    #switch to the literal mode if needed
    if ($element->{type} == NODE_PROPERTY_2) {
	$expat->setHandlers(%{$self->_getHandlersLiteral($name)});
    }
}

sub end {
    my ($self, $expat, $name) = @_;
    my $element = pop @{$self->{path}};
    
    pop @{$self->{subjects}} if $element->{presubject};

    #remember current subject
    my $subject = $self->{subjects}[-1];

    if (($element->{type} & NODE_PROPERTY_MASK)) {
	$self->_assertElement($expat, ${$self->{subjects}}[-1], $element);
	#update parent type (usefull for containers)
	if ($element->{qname} eq RDF_TYPE) {
	    my $ctype = $RDF_TYPES{$self->_getElementResource($element)};
	    if ($ctype & RDFT_CONTAINER) {
		$subject->{containertype} = $ctype;
	    }
	}
	#catch memebers for aboutEach on ID
	my $re = '^' . RDF_NS . '_\d+$';
	if ($element->{qname} =~ /$re/) {
	    my $uri = $self->_getElementResource($element);
	    push @{$subject->{members}}, $uri if $uri;
	}
    }

    #remember aboutEach stuff
    if ($element->{subject} && $element->{containertype}) {
	$self->{urimembers}{$element->{uri}} = $element->{members};
    }

    pop @{$self->{subjects}} if $element->{subject};
}

sub char {
    my ($self, $expat, $string) = @_;
    my $elt = ${$self->{path}}[-1];
    if ($string !~ /^\s*$/s && $elt->{type} != NODE_PROPERTY_1) {
	$expat->xpcroak("Element '" . $elt->{name} .
			"' can not contain a literal value");
    }
    $elt->{text} .= $string;
}

sub start_literal {
    my ($self, $expat, $name, %attrs) = @_;
    ${$self->{path}}[-1]{text} .= $expat->recognized_string;
}

sub end_literal {
    my ($self, $fname, $expat, $name) = @_;
    if ($name eq $fname) {
	$expat->setHandlers(%{$self->_getHandlersRegular});
	$self->end($expat, $name);
    } else {
	${$self->{path}}[-1]{text} .= $expat->recognized_string;
    }
}

sub char_literal {
    my ($self, $expat, $string) = @_;
    ${$self->{path}}[-1]{text} .= $string;
}

########################################
# private methods
sub _findNS {
    my ($self, $abbr) = @_;
    return XML_NS if $abbr eq 'xml';
    foreach my $element (reverse @{$self->{path}}) {
	return $element->{nslist}{$abbr} if exists $element->{nslist}{$abbr};
    }
}

sub _getHandlers {
    my $self = shift;
    my %handlers = (
		    Init      => sub {$self->init(@_)},
		    Final     => sub {$self->final(@_)},
		    Start     => sub {$self->start(@_)},
		    End       => sub {$self->end(@_)},
		    Char      => sub {$self->char(@_)},
		   );
    return \%handlers;
}

sub _getHandlersLiteral {
    my ($self, $name) = @_;
    my %handlers = (
		    Start     => sub {$self->start_literal(@_)},
		    End       => sub {$self->end_literal($name, @_)},
		    Char      => sub {$self->char_literal(@_)},
		   );
    return \%handlers;
}

sub _getHandlersRegular {
    my ($self, $name) = @_;
    my %handlers = (
		    Start     => sub {$self->start(@_)},
		    End       => sub {$self->end(@_)},
		    Char      => sub {$self->char(@_)},
		   );
    return \%handlers;
}

sub _createExpatParser {
    my $self = shift;
    my $expat = new XML::Parser(
				Handlers => $self->_getHandlers,
			       );
    return $expat;
}

sub _setFlag {
    my ($self, $flag) = @_;
    $self->{status} |= $flag;
}

sub _hasFlag {
    my ($self, $flag) = @_;
    return $self->{status} & $flag;
}

sub _clearFlag {
    my ($self, $flag) = @_;
    $self->{status} &= ~$flag;
}

sub _getImplicitURI {
    my $self = shift;
    my $ret = $self->{inlineuri} || "_:a";
    $ret .=  ++ $self->{unique};
}

1;

__END__


#     # Description, containers, li
#     if ($rdftype = $self->_elementType($element)) {
# 	#all rdf elements are subjects (???)
# 	$element->{subject} = 1;

# 	#create the subject item
# 	$subject = {
# 		    uri => $element->{about} || $self->_getImplicitURI(),
# 		    id => $element->{id},
# 		    bagid => $element->{bagid},
# 		   };
# 	push @{$self->{subjects}}, $subject;

# 	#the parent element might be predicate and need
# 	#update its resourcse reference
# 	my $parent = ${$self->{path}}[-2];
# 	$parent->{resource} = $subject->{uri} unless $parent->{subject};

# 	# create statements from non-RDF attributes
# 	$self->_assertAttributes($subject, $attrs);

# 	if ($rdftype & RDFT_CONTAINER) {
# 	    $self->_assertRDFType($subject, $rdftype);
# 	}
#     }
#     elsif ($element->{qname} eq RDF_LI) {
# 	my ($ns, $name, $uri) = $self->_getLIURI(${$self->{subjects}}[-1]);
# 	#we rename the whole element, hopefuly it doesn't matter
# 	$element->{ns} = $ns;
# 	$element->{name} = $name;
# 	$element->{qname} = $uri;

# 	#my $resource = $self->_getResource($attrs);
# 	#$element->{resource} = $resource->{value} if $resource;
# 	$element->{assert} = 1;
#     }
#     # other namespaces
#     else {
# 	#$element->{foreign} = 1;

# 	#my $resource = $self->_getResource($attrs);
# 	#$element->{resource} = $resource->{value} if $resource;
# 	$element->{assert} = 1;

# 	#in the case our parent is not a subject
# 	#we're probably parsing abbr. syntax 3. we have to
# 	#create a fake subject, and bind it to the parent element
# 	#we have to spit out the rdf:type as well
# 	my $parent = ${$self->{path}}[-2];
# 	unless ($parent->{subject}) {
# 	    $subject = {
# 			uri => $parent->{about} || $self->_getImplicitURI,
# 			id => $parent->{id},
# 			bagid => $parent->{bagid},
# 		       };
# 	    push @{$self->{subjects}}, $subject;
# 	    $parent->{subject} = 1;
# 	    $parent->{early_subject} = 1;
# 	    $parent->{resource} = $subject->{uri};
# 	    $self->_assertRDFTypeElement($subject, $parent);
# 	}
	
# 	# assert attributes; abbr. syntax 2
# 	if ($element->{resource}) {
# 	    my $_subject = {
# 			    uri => $element->{resource},
# 			    id => $element->{id},
# 			    bagid => $element->{bagid},
# 			   };
# 	    $self->_assertAttributes($_subject, $attrs);
# 	}
#     }

=head1 NAME

RDF::Core::Parser - RDF Parser

=head1 DESCRIPTION

A module for parsing XML documents containing RDF data. It's based on XML::Parser. Parser goes through XML and calls what is referenced in Assert option for each statement found.

=head1 SYNOPSIS

  use RDF::Core::Parser;

  my %options = (Assert => \&handleAssert,
                 BaseURI => "http://www.foo.com/",
                 InlineURI => "http://www.bar.com/"
                );
  my $parser = new RDF::Core::Parser(%options);
  $parser->parseFile('./rdfFile.xml');
  #or
  $parser->parse($rdfString);

=head2 Interface

=over 4

=item * new(%options)

Available options are

=over 4

=item * Assert

A reference to a subroutine, that is called for every assertion that parser generates.

=item * BaseURI

A base URI of parsed document. It will be used to resolve relative URI references.

=item * InlineURI

Inline URI is used to generate URIs for anonymous (inline) resources.

=back

=item * parse($string)

=item * parseFile($fileName)

=back

=head2 Assert handler

Assert handler is called with key value pairs in a parameters array.

Keys are:

=over 4

=item * subject_ns, subject_name, subject_uri

namespace, local value and URI of subject

=item * predicate_ns, predicate_name, predicate_uri

namespace, local value and URI of predicate

=item * object_ns, object_name, object_uri

namespace, local value and URI of object, if the object is a resource

or

=item * object_literal

object value for literal

=back

=head1 LICENSE

This package is subject to the MPL (or the GPL alternatively).

=head1 AUTHOR

Ginger Alliance, rdf@gingerall.cz

=head1 SEE ALSO

RDF::Core::Model::Parser

=cut
