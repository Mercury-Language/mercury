%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_to_xml.m.
% Main author: maclarty.
% Stability: low.
% 
% This module provides two mechanisms for converting Mercury terms
% to XML documents.
%
% Method 1
% --------
% The first method requires a type to be an instance of the xmlable typeclass
% before values of the type can be written as XML.
% Members of the xmlable typeclass must implement a to_xml method which
% maps values of the type to XML elements.
% The XML elements may contain arbitrary children, comments and data.
%
% Method 2
% --------
% The second method is less flexible than the first, but it allows for the
% automatic generation of a DTD.
% Each functor in a term is given a corresponding well-formed element name in
% the XML document according to a mapping.  Some predefined mappings are
% provided, but user defined mappings may also be used.
%
% Method 1 vs. Method 2
% ---------------------
%
% Method 1 allows values of a specific type to be mapped to arbitrary XML
% elements with arbitrary children and arbitrary attributes.
% In method 2 each functor in a term can be mapped to only one XML element.
% Method 2 also only allows a selected set of attributes.
% In method 2 a DTD can be automatically generated.  In method 1 DTDs cannot
% be automatically generated.
%
% Method 1 is useful for mapping a specific type to XML,
% for example mapping terms which represent mathematical expressions to
% MathML.
% Method 2 is useful for mapping arbitrary terms of any type to XML.
% 
% In both methods the XML document can be annotated with a stylesheet
% reference.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module term_to_xml.
:- interface.

:- import_module deconstruct.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module std_util.
:- import_module type_desc.

%-----------------------------------------------------------------------------%
%
% Method 1 interface
%

	% Instances of this typeclass can be converted to XML.
	%
:- typeclass xmlable(T) where [
	func to_xml(T::in) = (xml::out(xml_doc)) is det
].

	% Values of this type represent an XML document or a portion of
	% an XML document.
	%
:- type xml
			%
			% An XML element with a name, list of attributes
			% and a list of children.
			%
	--->	elem(
			element_name	:: string, 
			attributes	:: list(attr), 
			children	:: list(xml)
		)
		
			% Textual data.  `<', `>', `&', `'' and `"' characters
			% will be replaced by `&lt;', `&gt;', `&amp;', `&apos;'
			% and `&quot;' respectively.
	;	data(string)

			% Data to be enclosed in `<![CDATA[' and `]]>' tags.
			% Any occurrences of `]]>' in the data will be
			% converted to `]]&gt;'.
	;	cdata(string)

			% An XML comment.  The comment should not
			% include the `<!--' and `-->'.  Any occurrences of
			% `--' will be replaced by ` - '.
	;	comment(string)

			% An entity reference.  The string will
			% have `&' prepended and `;' appended before being
			% output.
	;	entity(string)

			% Raw XML data.  The data will be written out verbatim.
	;	raw(string).

	% An XML document must have an element at the top-level.
	%
:- inst xml_doc
	--->	elem(
			ground,	% element_name
			ground,	% attributes
			ground	% children
		).

	% An element attribute, mapping a name to a value.
	%
:- type attr ---> attr(string, string).

	% Values of this type specify the DOCTYPE of an XML document when
	% the DOCTYPE is defined by an external DTD.
	%
:- type doctype
	--->	public(string)			% FPI
	;	public(string, string)		% FPI, URL
	;	system(string).			% URL

	% Values of this type specify whether a DTD should be included in
	% a generated XML document and if so how.
	%
:- type maybe_dtd
			% Generate and embed the entire DTD in the document
			% (only available for method 2).
	--->	embed
			% Included a reference to an external DTD.
	;	external(doctype)
			% Do not include any DOCTYPE information.
	;	no_dtd.

:- inst non_embedded_dtd
	--->	external(ground)
	;	no_dtd.

	% Values of this type indicate whether a stylesheet reference should be
	% included in a generated XML document.
	%
:- type maybe_stylesheet
	--->	with_stylesheet(
			stylesheet_type	:: string, % For example "text/xsl"
			stylesheet_href	:: string
		)
	;	no_stylesheet.

	% write_xml_doc(Term, !IO).
	% Output Term as an XML document to the current output stream.
	% Term must be an instance of the xmlable typeclass.
	%
:- pred write_xml_doc(T::in, io::di, io::uo) is det <= xmlable(T).

	% write_xml_doc(Stream, Term, !IO).
	% Same as write_xml_doc/3, but use the given output stream.
	%
:- pred write_xml_doc(io.output_stream::in, T::in, io::di, io::uo) is det
	<= xmlable(T).

	% write_xml_doc(Term, MaybeStyleSheet, MaybeDTD, !IO).
	% Write Term to the current output stream as an XML document.
	% MaybeStyleSheet and MaybeDTD specify whether or not a stylesheet
	% reference and/or a DTD should be included.  
	% Using this predicate only external DTDs can be included, i.e.
	% a DTD cannot be automatically generated and embedded
	% (that feature is available only for method 2 -- see below).
	%
:- pred write_xml_doc(T::in, maybe_stylesheet::in, 
	maybe_dtd::in(non_embedded_dtd), io::di, io::uo) is det <= xmlable(T).

	% write_xml_doc(Stream, Term, MaybeStyleSheet, MaybeDTD, !IO).
	% Same as write_xml_doc/5, but write output to the given output
	% stream.
	%
:- pred write_xml_doc(io.output_stream::in, T::in, maybe_stylesheet::in,
	maybe_dtd::in(non_embedded_dtd), io::di, io::uo) is det <= xmlable(T).

	% write_xml_element(Indent, Term, !IO).
	% Write Term out as XML to the current output stream, 
	% using indentation level Indent (each indentation level is one
	% tab character).
	% No `<?xml ... ?>' header will be written.
	% This is useful for generating large XML documents in pieces.
	%
:- pred write_xml_element(int::in, T::in, io::di, io::uo) is det <= xmlable(T).

	% write_xml_element(Stream, Indent, Term, !IO).
	% Same as write_xml_element/4, but use the given output stream.
	%
:- pred write_xml_element(io.output_stream::in, int::in, T::in, io::di, io::uo) 
	is det <= xmlable(T).

	% write_xml_header(MaybeEncoding, !IO).
	% Write an XML header (i.e. `<?xml version="1.0"?>) to the 
	% current output stream.
	% If MaybeEncoding is yes(Encoding), then include `encoding="Encoding"'
	% in the header.
	%
:- pred write_xml_header(maybe(string)::in, io::di, io::uo) is det.

	% Same as write_xml_header/3, but use the given output stream.
	%
:- pred write_xml_header(io.output_stream::in, maybe(string)::in, io::di, 
	io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Method 2 interface
%

	% Values of this type specify which mapping from functors to elements
	% to use when generating XML.  The role of a mapping is twofold:
	%	1. To map functors to elements, and
	%	2. To map functors to a set of attributes that should be
	%	set for the corresponding element.
	%
	% We provide two predefined mappings:
	%
	%	1. simple: The functors `[]', `[|]' and `{}' are mapped to the
	%	elements `List', `Nil' and `Tuple' respectively.  Arrays are
	%	assigned the `Array' element.  The builtin types are assigned
	% 	the elements `Int', `String', `Float' and `Char'.  All other
	%	functors are assigned elements with the same name as the
	%	functor provided the functor name is well formed and does
	% 	not start with a capital letter.  Otherwise a mangled
	%	version of the functor name is used.  
	%
	%	All elements except `Int', `String', `Float' and `Char'
	%	will have their `functor', `arity', `type' and `field' (if
	%	there is a field name) attributes set.  `Int', `String', 
	%	`Float' and `Char' elements will just have their `type' and
	%	possibly their `field' attributes set.
	%
	%	The `simple' mapping is designed to be easy to read and use,
	%	but may result in the same element being assigned to different
	%	functors. 
	%
	%	2. unique: Here we use the same mapping as `simple' except
	%	we append the functor arity for discriminated unions and
	%	a mangled version of the type name for every element.  The same
	%	attributes as the `simple' scheme are provided.  The advantage
	%	of this scheme is that it maps each functor to a unique 
	%	element.  This means that it will always be possible to 
	%	generate a DTD using this mapping so long as there is only
	%	one top level functor and no unsupported types can appear in
	%	terms of the type.
	%
	% A custom mapping can be provided using the `custom' functor.  See the
	% documentation for the element_pred type below for more information.
	%	
:- type element_mapping
	--->	simple
	;	unique
	;	custom(element_pred).

:- inst element_mapping 
	--->	simple
	;	unique
	;	custom(element_pred).

	% Deterministic procedures with the following signature can be used as
	% custom functor to element mappings.  The inputs to the procedure are
	% a type and some information about a functor for that type
	% if the type is a discriminated union.  The output should be a well
	% formed XML element name and a list of attributes that should be set
	% for that element.  See the types `maybe_functor_info' and 
	% `attr_from_source' below.
	%
:- type element_pred == (pred(type_desc.type_desc, maybe_functor_info, string, 
	list(attr_from_source))).

:- inst element_pred == (pred(in, in, out, out) is det).

	% Values of this type are passed to custom functor-to-element
	% mapping predicates to tell the predicate which functor to generate
	% an element name for if the type is a discriminated union.  If the
	% type is not a discriminated union, then none_du is passed to
	% the predicate when requesting an element for the type.
	%
:- type maybe_functor_info
			% The functor's name and arity.
	--->	du_functor(
			functor_name	:: string, 
			functor_arity	:: int
		)
			% The type is not a discriminated union.
	;	none_du.

	% Values of this type specify attributes that should be set from
	% a particular source.  The attribute_name field specifies the name
	% of the attribute in the generated XML and the attribute_source
	% field indicates where the attribute's value should come from.
	%
:- type attr_from_source
	--->	attr_from_source(
			attr_name	:: string, 
			attr_source	:: attr_source
		).

	% Possible attribute sources.
	%
:- type attr_source
			% The original functor name as returned by
			% deconstruct.deconstruct/5.
	--->	functor
			% The field name if the functor appears in a 
			% named field (If the field is not named then this
			% attribute is omitted).
	;	field_name
			% The fully qualified type name the functor is for.
	;	type_name
			% The arity of the functor as returned by
			% deconstruct.deconstruct/5.
	;	arity.

	% To support third parties generating XML which is compatible with the
	% XML generated using method 2, a DTD for a Mercury type can also be
	% generated.  A DTD for a given type and functor-to-element mapping may
	% be generated provided the following conditions hold:
	%
	%	1. If the type is a discriminated union then there must be only
	%	one top-level functor for the type.  This is because the top
	%	level functor will be used to generate the document type name.
	%
	%	2. The functor to element mapping must map each functor to a
	%	unique element name for every functor that could appear in
	%	terms of the type.
	%
	%	3. Only types whose terms consist of discriminated unions,
	%	arrays and the builtin types `int', `string', `character' and
	%	`float' can be used to automatically generate DTDs.  
	%	Existential types are also not supported.
	%
	% The generated DTD is also a good reference when creating a stylesheet
	% as it contains comments describing the mapping from functors to
	% elements.
	%
	% Values of the following type indicate whether a DTD was successfully
	% generated or not.  
	%
:- type dtd_generation_result
	--->	ok
			% The root type is a discriminated union with 
			% multiple functors.
			%
	;	multiple_functors_for_root

			% The functor-to-element mapping maps different
			% functors to the same element.  The duplicate element
			% and a list of types whose functors map to that
			% element is given.
			%
	;	duplicate_elements(
			duplicate_element	:: string,
			duplicate_types		:: list(type_desc.type_desc)
		)
	;
			% At the moment we only support generation of DTDs for
			% types made up of discriminated unions, arrays,
			% strings, ints, characters and floats.  If a type is
			% not supported, then it is returned as the argument
			% of this functor.
			%
		unsupported_dtd_type(type_desc.type_desc)
	;
			% If one of the arguments of a functor is existentially
			% typed, then the pseudo_type_desc for the
			% existentially quantified argument is returned as the
			% argument of this functor.  Since the values of
			% existentially typed arguments can be of any type
			% (provided any typeclass constraints are satisfied) it
			% is not generally possible to generate DTD rules for
			% functors with existentially typed arguments.  
			%
		type_not_ground(pseudo_type_desc).

	% write_xml_doc(Term, ElementMapping, MaybeStyleSheet, MaybeDTD,
	% 	DTDResult, !IO).
	% Write Term to the current output stream as an XML document using
	% ElementMapping as the scheme to map functors to elements.
	% MaybeStyleSheet and MaybeDTD specify whether or not a stylesheet
	% reference and/or a DTD should be included.  Any non-canonical terms
	% will be canonicalized.  If an embedded DTD is requested, but it is
	% not possible to generate a DTD for Term using ElementMapping, then a
	% value other than `ok' is returned in DTDResult and nothing is written
	% out.  See the dtd_generation_result type for a list of the other
	% possible values of DTDResult and their meanings.
	%
:- pred write_xml_doc(T::in, element_mapping::in(element_mapping),
	maybe_stylesheet::in, maybe_dtd::in, 
	dtd_generation_result::out, io::di, io::uo) is det.

	% write_xml_doc(Stream, Term, ElementMapping, MaybeStyleSheet, 
	%	MaybeDTD, DTDResult, !IO).
	% Same as write_xml_doc/7 except write the XML doc to the given 
	% output stream.
	%
:- pred write_xml_doc(io.output_stream::in, T::in, 
	element_mapping::in(element_mapping), maybe_stylesheet::in, 
	maybe_dtd::in, dtd_generation_result::out, io::di, io::uo) is det.

	% write_xml_doc_cc(Term, ElementMapping, MaybeStyleSheet, MaybeDTD, 
	%	DTDResult, !IO).
	% Write Term to the current output stream as an XML document using
	% ElementMapping as the scheme to map functors to elements.
	% MaybeStyleSheet and MaybeDTD specify whether or not a stylesheet
	% reference and/or a DTD should be included.  Any non-canonical terms
	% will be be written out in full.  If an embedded DTD is requested, but
	% it is not possible to generate a DTD for Term using ElementMapping,
	% then a value other than `ok' is returned in DTDResult and nothing is
	% written out.  See the dtd_generation_result type for a list of the
	% other possible values of DTDResult and their meanings.
	%
:- pred write_xml_doc_cc(T::in, element_mapping::in(element_mapping), 
	maybe_stylesheet::in, maybe_dtd::in, dtd_generation_result::out,
	io::di, io::uo) is cc_multi.

	% write_xml_doc_cc(Stream, Term, ElementMapping, MaybeStyleSheet,
	%	 MaybeDTD, DTDResult, !IO).
	% Same as write_xml_doc/7 except write the XML doc to the given 
	% output stream.
	%
:- pred write_xml_doc_cc(io.output_stream::in, T::in, 
	element_mapping::in(element_mapping), maybe_stylesheet::in,
	maybe_dtd::in, dtd_generation_result::out, io::di, io::uo) is cc_multi.

	% can_generate_dtd(ElementMapping, Type) = Result.
	% Check if a DTD can be generated for the given Type using the
	% functor-to-element mapping scheme ElementMapping.  Return `ok' if it
	% is possible to generate a DTD.  See the documentation of the
	% dtd_generation_result type for the meaning of the return value when
	% it is not `ok'.
	%
:- func can_generate_dtd(element_mapping::in(element_mapping), 
	type_desc.type_desc::in) = (dtd_generation_result::out) is det.

	% write_dtd(Term, ElementMapping, DTDResult, !IO).
	% Write a DTD for the given term to the current output stream using
	% ElementMapping to map functors to elements.  If a DTD
	% cannot be generated for Term using ElementMapping then a value
	% other than `ok' is returned in DTDResult and nothing is written.
	% See the dtd_generation_result type for a list of the other
	% possible values of DTDResult and their meanings.
	%
:- pred write_dtd(T::unused, element_mapping::in(element_mapping), 
	dtd_generation_result::out, io::di, io::uo) is det.

	% write_dtd(Stream, Term, ElementMapping, DTDResult, !IO).
	% Same as write_dtd/5 except the DTD will be written to the given
	% output stream.
	%
:- pred write_dtd(io.output_stream::in, T::unused, 
	element_mapping::in(element_mapping), dtd_generation_result::out, 
	io::di, io::uo) is det.

	% write_dtd_for_type(Type, ElementMapping, DTDResult, !IO).
	% Write a DTD for the given type to the current output stream. If a DTD
	% cannot be generated for Type using ElementMapping then a value
	% other than `ok' is returned in DTDResult and nothing is written.
	% See the dtd_generation_result type for a list of the other
	% possible values of DTDResult and their meanings.
	%
:- pred write_dtd_from_type(type_desc.type_desc::in, 
	element_mapping::in(element_mapping), dtd_generation_result::out, 
	io::di, io::uo) is det.

	% write_dtd_for_type(Stream, Type, ElementMapping, DTDResult, !IO).
	% Same as write_dtd_for_type/5 except the DTD will be written to the
	% given output stream.
	%
:- pred write_dtd_from_type(io.output_stream::in, type_desc.type_desc::in, 
	element_mapping::in(element_mapping), dtd_generation_result::out,
	io::di, io::uo) is det.

	% write_xml_element(NonCanon, MakeElement, IndentLevel, Term, !IO).
	% Write XML elements for the given term and all its descendents, 
	% using IndentLevel as the initial indentation level (each 
	% indentation level is one tab character) and using the MakeElement
	% predicate to map functors to elements.  No <?xml ... ?>
	% header will be written.  Non-canonical terms will be handled
	% according to the value of NonCanon.  See the deconstruct
	% module in the standard library for more information on this argument.
	%
:- pred write_xml_element(deconstruct.noncanon_handling, 
	element_mapping, int, T, io, io).
:- mode write_xml_element(in(do_not_allow), in(element_mapping), in, in, di, uo)
	is det.
:- mode write_xml_element(in(canonicalize), in(element_mapping), in, in,  
	di, uo) is det.
:- mode write_xml_element(in(include_details_cc), in(element_mapping), in, in, 
	di, uo) is cc_multi.
:- mode write_xml_element(in, in(element_mapping), in, in, di, uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module construct.
:- import_module exception.
:- import_module map.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

write_xml_doc(Term, !IO) :-
	write_xml_doc(Term, no_stylesheet, no_dtd, !IO).

write_xml_doc(Stream, Term, !IO) :-
	write_xml_doc(Stream, Term, no_stylesheet, no_dtd, !IO).

write_xml_doc(Term, MaybeStyleSheet, MaybeDTD, !IO) :-
	write_xml_header(no, !IO),
	write_stylesheet_ref(MaybeStyleSheet, !IO),
	Root = to_xml(Term),
	Root = elem(RootName, _, Children),
	(
		MaybeDTD = no_dtd
	;
		MaybeDTD = external(DocType),
		write_external_doctype(RootName, DocType, !IO)
	),
	( if contains_noformat_xml(Children) then
		ChildrenFormat = no_format
	else
		ChildrenFormat = format
	),
	write_xml_element_format(ChildrenFormat, 0, Root, !IO).

write_xml_doc(Stream, Term, MaybeStyleSheet, MaybeDTD, !IO) :-
	io.set_output_stream(Stream, OrigStream, !IO),
	write_xml_doc(Term, MaybeStyleSheet, MaybeDTD, !IO),
	io.set_output_stream(OrigStream, _, !IO).

write_xml_element(Indent, Term, !IO) :-
	Root = to_xml(Term),
	Root = elem(_, _, Children),
	( if contains_noformat_xml(Children) then
		ChildrenFormat = no_format
	else
		ChildrenFormat = format
	),
	write_xml_element_format(ChildrenFormat, Indent, Root, !IO).

write_xml_element(Stream, Indent, Term, !IO) :-
	io.set_output_stream(Stream, OrigStream, !IO),
	write_xml_element(Indent, Term, !IO),
	io.set_output_stream(OrigStream, _, !IO).

write_xml_doc(Term, ElementMapping, MaybeStyleSheet, MaybeDTD, DTDResult, !IO)
		:-
	DTDResult = can_generate_dtd(MaybeDTD, ElementMapping,
		type_desc.type_of(Term)),
	(
		DTDResult = ok
	->
		write_xml_header(no, !IO),
		write_stylesheet_ref(MaybeStyleSheet, !IO),
		write_doctype(canonicalize, Term, ElementMapping, MaybeDTD, _,
			!IO),
		write_xml_element(canonicalize, ElementMapping, 0, Term, !IO)
	;
		true
	).

write_xml_doc(Stream, Term, ElementMapping, MaybeStyleSheet, MaybeDTD, 
		DTDResult, !IO) :-
	io.set_output_stream(Stream, OrigStream, !IO),
	write_xml_doc(Term, ElementMapping, MaybeStyleSheet, MaybeDTD,
		DTDResult, !IO),
	io.set_output_stream(OrigStream, _, !IO).

write_xml_doc_cc(Term, ElementMapping, MaybeStyleSheet, MaybeDTD, DTDResult, 
		!IO) :-
	DTDResult = can_generate_dtd(MaybeDTD, ElementMapping,
		type_desc.type_of(Term)),
	(
		DTDResult = ok
	->
		write_xml_header(no, !IO),
		write_stylesheet_ref(MaybeStyleSheet, !IO),
		write_doctype(include_details_cc, Term, ElementMapping,
			MaybeDTD, _, !IO),
		write_xml_element(include_details_cc, ElementMapping, 
			0, Term, !IO)
	;
		true
	).

write_xml_doc_cc(Stream, Term, ElementMapping, MaybeStyleSheet, MaybeDTD, 
		DTDResult, !IO) :-
	io.set_output_stream(Stream, OrigStream, !IO),
	write_xml_doc_cc(Term, ElementMapping, MaybeStyleSheet, MaybeDTD, 
		DTDResult, !IO),
	io.set_output_stream(OrigStream, _, !IO).

write_xml_element(NonCanon, ElementMapping, IndentLevel, Term, !IO) :-
	type_to_univ(Term, Univ),
	get_element_pred(ElementMapping, MakeElement),
	write_xml_element_univ(NonCanon, MakeElement, IndentLevel, Univ, [], _,
		!IO).

write_dtd(Term, ElementMapping, DTDResult, !IO) :-
	type_desc.type_of(Term) = TypeDesc,
	write_dtd_from_type(TypeDesc, ElementMapping, DTDResult, !IO).

write_dtd(Stream, Term, ElementMapping, DTDResult, !IO) :-
	io.set_output_stream(Stream, OrigStream, !IO),
	write_dtd(Term, ElementMapping, DTDResult, !IO),
	io.set_output_stream(OrigStream, _, !IO).

write_dtd_from_type(Stream, TypeDesc, ElementMapping, DTDResult, !IO) :-
	io.set_output_stream(Stream, OrigStream, !IO),
	write_dtd_from_type(TypeDesc, ElementMapping, DTDResult, !IO),
	io.set_output_stream(OrigStream, _, !IO).

write_xml_header(MaybeEncoding, !IO) :-
	io.write_string("<?xml version=""1.0""", !IO),
	(
		MaybeEncoding = yes(Encoding),
		io.write_string(" encoding=""", !IO),
		io.write_string(Encoding, !IO),
		io.write_string("""?>\n", !IO)
	;
		MaybeEncoding = no,
		io.write_string("?>\n", !IO)
	).

write_xml_header(Stream, MaybeEncoding, !IO) :-
	io.set_output_stream(Stream, OrigStream, !IO),
	write_xml_header(MaybeEncoding, !IO),
	io.set_output_stream(OrigStream, _, !IO).

:- pred write_stylesheet_ref(maybe_stylesheet::in, io::di, io::uo) is det.

write_stylesheet_ref(no_stylesheet, !IO).
write_stylesheet_ref(with_stylesheet(Type, Href), !IO) :-
	io.write_string("<?xml-stylesheet type=""", !IO),
	io.write_string(Type, !IO),
	io.write_string(""" href=""", !IO),
	io.write_string(Href, !IO),
	io.write_string("""?>\n", !IO).

:- pred write_doctype(deconstruct.noncanon_handling, T, element_mapping,
	maybe_dtd, dtd_generation_result, io, io).
:- mode write_doctype(in(canonicalize), in, in(element_mapping), in, out, 
	di, uo) is det.
:- mode write_doctype(in(do_not_allow), in, in(element_mapping), in, out, 
	di, uo) is det.
:- mode write_doctype(in(include_details_cc), in, in(element_mapping), in, out, 
	di, uo) is cc_multi.
:- mode write_doctype(in, in, in(element_mapping), in, out, 
	di, uo) is cc_multi.

write_doctype(_, _, _, no_dtd, ok, !IO).
write_doctype(_, T, ElementMapping, embed, DTDResult, !IO) :-
	write_dtd(T, ElementMapping, DTDResult, !IO),
	io.nl(!IO).
write_doctype(NonCanon, T, ElementMapping, external(DocType), ok, !IO) :-
	get_element_pred(ElementMapping, MakeElement),
	deconstruct.deconstruct(T, NonCanon, Functor, Arity, _),
	(
		is_discriminated_union(type_desc.type_of(T), _)
	->
		Request = du_functor(Functor, Arity)
	;
		Request = none_du
	),
	MakeElement(type_desc.type_of(T), Request, Root, _),
	write_external_doctype(Root, DocType, !IO).

:- pred write_external_doctype(string::in, doctype::in, io::di, io::uo) 
	is det.

write_external_doctype(Root, DocType, !IO) :-
	io.write_string("<!DOCTYPE ", !IO),
	io.write_string(Root, !IO),
	(
		DocType = public(PUBLIC),
		io.write_string(" PUBLIC """, !IO),
		io.write_string(PUBLIC, !IO)
	;
		DocType = public(PUBLIC, SYSTEM),
		io.write_string(" PUBLIC """, !IO),
		io.write_string(PUBLIC, !IO),
		io.write_string(""" """, !IO),
		io.write_string(SYSTEM, !IO)
	;
		DocType = system(SYSTEM),
		io.write_string(" SYSTEM """, !IO),
		io.write_string(SYSTEM, !IO)
	),
	io.write_string(""">\n", !IO).

	% Implementation of the `unique' predefined mapping scheme.
	%
:- pred make_unique_element(type_desc.type_desc::in, maybe_functor_info::in,
	string::out, list(attr_from_source)::out) is det.

% XXX This should be uncommented once memoing can be switched off for grades
% which don't support it.
% :- pragma memo(make_unique_element/4).

make_unique_element(TypeDesc, du_functor(Functor, Arity), Element,
		all_attr_sources) :-
	(
		common_mercury_functor(Functor, ReservedElement)
	->
		MangledElement = ReservedElement
	;
		MangledElement = mangle(Functor)
	),
	Element = MangledElement ++ "--" ++ string.int_to_string(Arity) ++ 
		"--" ++ mangle(type_desc.type_name(TypeDesc)).
make_unique_element(TypeDesc, none_du, Element, AttrFromSources) :-
	(
		is_primitive_type(TypeDesc, PrimitiveElement)
	->
		Element = PrimitiveElement,
		AttrFromSources = [attr_from_source("type", type_name),
			attr_from_source("field", field_name)]
	;
		is_array(TypeDesc, _)
	->
		Element = array_element ++ "--" ++
			mangle(type_desc.type_name(TypeDesc)),
		AttrFromSources = all_attr_sources
	;
		Element = mangle(type_desc.type_name(TypeDesc)),
		AttrFromSources = all_attr_sources
	).

	% Implementation of the `simple' mapping scheme.
	%
:- pred make_simple_element(type_desc.type_desc::in, maybe_functor_info::in,
	string::out, list(attr_from_source)::out) is det.

% XXX This should be uncommented once memoing can be switched off for grades
% which don't support it.
% :- pragma memo(make_simple_element/4).

make_simple_element(_, du_functor(Functor, _), Element, all_attr_sources) :-
	(
		common_mercury_functor(Functor, ReservedElement)
	->
		Element = ReservedElement
	;
		Element = mangle(Functor)
	).
make_simple_element(TypeDesc, none_du, Element, AttrFromSources) :-
	(
		is_primitive_type(TypeDesc, PrimitiveElement)
	->
		Element = PrimitiveElement,
		AttrFromSources = [attr_from_source("type", type_name),
			attr_from_source("field", field_name)]
	;
		is_array(TypeDesc, _)
	->
		Element = array_element,
		AttrFromSources = all_attr_sources
	;
		Element = "Unknown",
		AttrFromSources = all_attr_sources
	).

:- func all_attr_sources = list(attr_from_source).

all_attr_sources = [
		attr_from_source("functor", functor),
		attr_from_source("field", field_name),
		attr_from_source("type", type_name),
		attr_from_source("arity", arity)
	].

:- pred get_element_pred(element_mapping::in(element_mapping),
	element_pred::out(element_pred)) is det.

get_element_pred(simple, make_simple_element).
get_element_pred(unique, make_unique_element).
get_element_pred(custom(P), P).

%-----------------------------------------------------------------------------%
%
% Some reserved element names for the predefined mapping schemes.  Reserved
% element names all start with a capital letter so as not to conflict with a
% mangled element name.
%

	% A prefix for functors that start with a capital letter or
	% a non-letter.
	%
:- func reserved_prefix = string.

reserved_prefix = "Tag_".

:- pred common_mercury_functor(string::in, string::out) is semidet.

	% These should all start with a capital letter so as not to 
	% conflict with a mangled name.
	%
common_mercury_functor("[|]", "List").
common_mercury_functor("[]", "Nil").
common_mercury_functor("{}", "Tuple").

:- func array_element = string.

array_element = "Array".

:- pred is_primitive_type(type_desc.type_desc::in, string::out) is semidet.

is_primitive_type(TypeDesc, Element) :-
	(
		type_desc.type_of("") = TypeDesc
	->
		Element = "String"
	;
		type_desc.type_of('c') = TypeDesc
	->
		Element = "Char"
	;
		type_desc.type_of(1) = TypeDesc
	->
		Element = "Int"
	;
		type_desc.type_of(1.0) = TypeDesc,
		Element = "Float"
	).

%-----------------------------------------------------------------------------%
% 
% Mangling functions.  
%
% We use the following mangling scheme to create well formed element names
% that do not begin with a capital letter (capitals are used for reserved
% elements).
%
% If the string to be mangled begins with a capital letter then we prefix it
% with another string reserved for this purpose.  Then we replace all 
% characters which aren't alpha numeric or underscores with '-' followed by
% the character code.
%
% So for example "my-functor!" would become "my-45functor-33" while
% "MyFunctor" would become "Tag_MyFunctor", presuming we were using
% "Tag_" as the prefix for strings that started with capital letters.
%

:- func mangle(string) = string.

mangle(Functor) = Element :-
	string.split(Functor, 1, Head, Tail),
	(
		string.is_alpha(Head),
		string.to_lower(Head) = Head
	->
		First = Head,
		Rest = Tail
	;
		First = reserved_prefix,
		Rest = Head ++ Tail
	),
	string.foldl(mangle_char, Rest, [], ElementChrs),
	Element = First ++ string.from_char_list(ElementChrs).

:- pred mangle_char(char::in, list(char)::in, list(char)::out)
	is det.

	% XXX This is system dependent since char.to_int is system dependent.
	%
mangle_char(Chr, PrevChrs, list.append(PrevChrs, Chrs)) :-
	(
		char.is_alnum_or_underscore(Chr)
	->
		Chrs = [Chr]
	;
		Chrs = ['-' | string.to_char_list(string.int_to_string(
			char.to_int(Chr)))]
	).

%-----------------------------------------------------------------------------%

	% Return a list of elements, functors and arities
	% (if the type is a discriminated union), argument types and 
	% attributes for all the functors for the type. Only one element
	% will be in each list if the type is not a discriminated union.
	%
:- pred get_elements_and_args(element_pred::in(element_pred),
	type_desc.type_desc::in, list(string)::out, 
	list(maybe(string))::out, 
	list(maybe(int))::out, list(list(type_desc.pseudo_type_desc))::out, 
	list(list(attr_from_source))::out) is det.

% XXX This should be uncommented once memoing can be switched off for grades
% which don't support it.
% :- pragma memo(get_elements_and_args/7).

get_elements_and_args(MakeElement, TypeDesc, Elements, MaybeFunctors, 
		MaybeArities, ArgTypeLists, AttributeLists) :-
	(
		is_discriminated_union(TypeDesc, NumFunctors)
	->
		FunctorNums = 0 `..` (NumFunctors - 1),
		(
			list.map3(construct.get_functor(TypeDesc), FunctorNums, 
				Functors, Arities, ArgTypeLists0)
		->
			MaybeFunctors = list.map((func(X) = yes(X)), Functors),
			MaybeArities = list.map((func(X) = yes(X)), Arities),
			ArgTypeLists = ArgTypeLists0,
			Requests = list.map_corresponding(make_du_functor, 
				Functors, Arities),
			P = (pred(A::in, B::out, C::out) is det :-
				MakeElement(TypeDesc, A, B, C)),
			list.map2(P, Requests, Elements, AttributeLists)
		;
			throw(software_error(
				"term_to_xml.get_elements_and_args: " ++
				"get_functor failed for discriminated union"))
		)
	;
		MakeElement(TypeDesc, none_du, Element, AttrFromSources),
		Elements = [Element],
		AttributeLists = [AttrFromSources],
		MaybeFunctors = [no],
		MaybeArities = [no],
		(
			is_array(TypeDesc, ArgType)
		->
			ArgTypeLists = [[ArgType]]
		;
			ArgTypeLists = [[]]
		)
	).

:- func make_du_functor(string, int) = maybe_functor_info.

make_du_functor(Functor, Arity) = du_functor(Functor, Arity).

:- pred primitive_value(univ::in, string::out) is semidet.

primitive_value(Univ, PrimValue) :-
	(
		univ_to_type(Univ, String)
	->
		PrimValue = String`with_type`string
	;
		univ_to_type(Univ, Char)
	->
		PrimValue = char_to_string(Char)
	;
		univ_to_type(Univ, Int)
	->
		PrimValue = int_to_string(Int)
	;
		univ_to_type(Univ, Float),
		PrimValue = float_to_string(Float)
	).

%-----------------------------------------------------------------------------%

	% The following type is used to decide if an entity should be
	% formatted (i.e. be indented and have a newline at the end).
	% We do not format an entity if any of its siblings are anything
	% besides an element, a CDATA entity or a comment, since then
	% whitespaces are more likely to be significant.
	% (Although technically spaces are always significant, they are
	% usually interpreted as only formatting when they are between 
	% markup).
	%
:- type maybe_format
	--->	format
	;	no_format.

:- pred write_xml_element_format(maybe_format::in, int::in, xml::in, 
	io::di, io::uo) is det.

write_xml_element_format(Format, IndentLevel, elem(Name, Attrs, Children), !IO)
		:-
	maybe_indent(Format, IndentLevel, !IO),
	(
		Children = [],
		write_empty_element(Name, Attrs, !IO),
		maybe_nl(Format, !IO)
	;
		Children = [_ | _],
		write_element_start(Name, Attrs, !IO), 
		( if contains_noformat_xml(Children) then
			ChildrenFormat = no_format
		else
			ChildrenFormat = format,
			io.nl(!IO)
		),
		list.foldl(write_xml_element_format(ChildrenFormat, 
			IndentLevel + 1), Children, !IO), 
		maybe_indent(ChildrenFormat, IndentLevel, !IO),
		write_element_end(Name, !IO),
		maybe_nl(Format, !IO)
	).
write_xml_element_format(_, _, data(Data), !IO) :-
	write_xml_escaped_string(Data, !IO).
write_xml_element_format(Format, IndentLevel, cdata(CData), !IO) :-
	maybe_indent(Format, IndentLevel, !IO),
	io.write_string("<![CDATA[", !IO),
	% CData may not contain "]]>", so replace with "]]&gt;".
	string.replace_all(CData, "]]>", "]]&gt;", EscapedCData),
	io.write_string(EscapedCData, !IO),
	io.write_string("]]>", !IO),
	maybe_nl(Format, !IO).
write_xml_element_format(Format, IndentLevel, comment(Comment), !IO) :-
	maybe_indent(Format, IndentLevel, !IO),
	io.write_string("<!-- ", !IO),
	% Comments may not contain "--", so replace with " - ".
	string.replace_all(Comment, "--", " - ", EscapedComment),
	io.write_string(EscapedComment, !IO),
	io.write_string(" -->", !IO),
	maybe_nl(Format, !IO).
write_xml_element_format(_, _, entity(EntityName), !IO) :-
	io.write_char('&', !IO),
	io.write_string(EntityName, !IO),
	io.write_char(';', !IO).
write_xml_element_format(_, _, raw(RawString), !IO) :-
	io.write_string(RawString, !IO).

:- func can_format_siblings(xml) = bool.

can_format_siblings(elem(_, _, _)) = yes.
can_format_siblings(data(_)) = no.
can_format_siblings(cdata(_)) = yes.
can_format_siblings(comment(_)) = yes.
can_format_siblings(raw(_)) = no.
can_format_siblings(entity(_)) = no.

:- pred contains_noformat_xml(list(xml)::in) is semidet.

contains_noformat_xml([XML | Rest]) :- 
	(
		can_format_siblings(XML) = no
	;
		contains_noformat_xml(Rest)
	).

:- pred maybe_nl(maybe_format::in, io::di, io::uo) is det.

maybe_nl(no_format, !IO).
maybe_nl(format, !IO) :- io.nl(!IO).

:- pred maybe_indent(maybe_format::in, int::in, io::di, io::uo) is det.

maybe_indent(Format, Indent, !IO) :-
	(
		Format = format,
		indent(Indent, !IO)
	;
		Format = no_format
	).

%-----------------------------------------------------------------------------%

:- pred write_xml_element_univ(deconstruct.noncanon_handling, 
	element_pred, int, univ, list(maybe(string)), 
	list(maybe(string)), io, io).
:- mode write_xml_element_univ(in(do_not_allow), in(element_pred), in, in, 
	in, out, di, uo) is det.
:- mode write_xml_element_univ(in(canonicalize), in(element_pred), in, in, 
	in, out, di, uo) is det.
:- mode write_xml_element_univ(in(include_details_cc), in(element_pred), in, 
	in, in, out, di, uo) is cc_multi.
:- mode write_xml_element_univ(in, in(element_pred), in, in, in, out, di, uo) 
	is cc_multi.

	% Write an element and all its descendents to the current output
	% stream.  If MaybeFields isn't empty then its head is used for the
	% `field' attribute and the Tail is returned in
	% RemainingMaybeFieldNames.  This is so it can be called using foldl2.
	%
write_xml_element_univ(NonCanon, MakeElement, IndentLevel, Univ, 
		MaybeFieldNames, RemainingMaybeFieldNames, !IO) :-
	(
		MaybeFieldNames = [MaybeFieldName | RemainingMaybeFieldNames]
	;
		MaybeFieldNames = [],
		RemainingMaybeFieldNames = [],
		MaybeFieldName = no
	),
	deconstruct.deconstruct(Term, NonCanon, Functor, Arity, Args),
	Term = univ_value(Univ),
	TypeDesc = type_desc.type_of(Term),
	(
		is_discriminated_union(TypeDesc, _)
	->
		Request = du_functor(Functor, Arity)
	;
		Request = none_du
	),
	MakeElement(TypeDesc, Request, Element, AttrFromSources),
	(
		primitive_value(Univ, PrimValue)
	->
		indent(IndentLevel, !IO),
		write_primitive_element_with_attr_from_source(Element, 
			AttrFromSources, PrimValue, MaybeFieldName, TypeDesc, 
			!IO)
	; 
		(
			Args = [],
			indent(IndentLevel, !IO),
			write_empty_element_with_attr_from_source(Element, 
				AttrFromSources, yes(Functor), 
				yes(Arity), MaybeFieldName, TypeDesc, !IO)
		;
			Args = [_ | _],
			ChildMaybeFieldNames = get_field_names(TypeDesc,
			 	Functor, Arity),
			indent(IndentLevel, !IO),
			write_element_start_with_attr_from_source(Element, 
				AttrFromSources, yes(Functor), 
				yes(Arity), MaybeFieldName, 
				TypeDesc, !IO), 
			write_child_xml_elements(NonCanon, MakeElement,
				IndentLevel + 1,
				Args, ChildMaybeFieldNames, !IO),
			indent(IndentLevel, !IO),
			write_element_end(Element, !IO),
			io.nl(!IO)
		)
	).

:- pred is_discriminated_union(type_desc.type_desc::in, int::out) is semidet.

is_discriminated_union(TypeDesc, NumFunctors) :- 
	NumFunctors = std_util.num_functors(TypeDesc),
	NumFunctors > -1.

:- pred is_array(type_desc.type_desc::in, type_desc.pseudo_type_desc::out)
	is semidet.

is_array(TypeDesc, ArgPseudoType) :-
	PseudoTypeDesc = type_desc_to_pseudo_type_desc(TypeDesc),
	type_desc.pseudo_type_ctor_and_args(PseudoTypeDesc, TypeCtor, 
		ArgPseudoTypes),
	ArgPseudoTypes = [ArgPseudoType],
	type_desc.type_ctor_name(TypeCtor) = "array",
	type_desc.type_ctor_module_name(TypeCtor) = "array".

:- func get_field_names(type_desc.type_desc, string, int)
	= list(maybe(string)).

% XXX This should be uncommented once memoing can be switched off for grades
% which don't support it.
% :- pragma memo(get_field_names/3).

get_field_names(TypeDesc, Functor, Arity) = MaybeFields :-
	(
		is_discriminated_union(TypeDesc, NumFunctors)
	->
		FunctorNums = 0`..`(NumFunctors - 1),
		(
			find_field_names(TypeDesc, FunctorNums, Functor,
				Arity, FoundMaybeFields)
		->	
			MaybeFields = FoundMaybeFields
		;
			MaybeFields = []
		)
	;
		MaybeFields = []
	).

:- pred find_field_names(type_desc.type_desc::in, list(int)::in, string::in,
	int::in, list(maybe(string))::out) is semidet.

find_field_names(TypeDesc, [FunctorNum | FunctorNums], Functor, Arity, 
		MaybeFieldNames) :-
	(
		construct.get_functor_with_names(TypeDesc, FunctorNum, 
			Functor, Arity, _, FoundFieldNames)
	->
		MaybeFieldNames = FoundFieldNames
	;
		find_field_names(TypeDesc, FunctorNums, Functor, Arity,
			MaybeFieldNames)
	).

%-----------------------------------------------------------------------------%
%
% XXX The following is done to get around an unimplemented feature where higher
% order terms with more than one mode can't be passed around (so we can't just
% pass write_xml_element_univ to foldl).
%

:- pred write_child_xml_elements(deconstruct.noncanon_handling, 
	element_pred, int, list(univ), list(maybe(string)), io, io).
:- mode write_child_xml_elements(in(do_not_allow), in(element_pred), in, in, 
	in, di, uo) is det.
:- mode write_child_xml_elements(in(canonicalize), in(element_pred), in, in, 
	in, di, uo) is det.
:- mode write_child_xml_elements(in(include_details_cc), in(element_pred), 
	in, in, in,  di, uo) is cc_multi.
:- mode write_child_xml_elements(in, in(element_pred), in, in, in, di, uo) 
	is cc_multi.

write_child_xml_elements(NonCanon, MakeElement, IndentLevel, Args,
		MaybeFieldNames, !IO) :-
	(
		NonCanon = do_not_allow,
		list.foldl2(
			write_xml_element_univ_do_not_allow(
				MakeElement, IndentLevel), Args, 
			MaybeFieldNames, _, !IO)
	;	
		NonCanon = canonicalize,
		list.foldl2(
			write_xml_element_univ_canonicalize(
				MakeElement, IndentLevel), Args, 
			MaybeFieldNames, _, !IO)
	;
		NonCanon = include_details_cc,
		list.foldl2(
			write_xml_element_univ_include_details_cc(
				MakeElement, IndentLevel), Args, 
			MaybeFieldNames, _, !IO)
	).

:- pred write_xml_element_univ_do_not_allow(element_pred::in(element_pred), 
	int::in, univ::in, list(maybe(string))::in, list(maybe(string))::out,
	io::di, io::uo) is det.

write_xml_element_univ_do_not_allow(MakeElement, IndentLevel, Univ, 
		MaybeFieldNames0, MaybeFieldNames, !IO) :-
	write_xml_element_univ(do_not_allow, MakeElement, IndentLevel, 
		Univ, MaybeFieldNames0, MaybeFieldNames, !IO).

:- pred write_xml_element_univ_canonicalize(element_pred::in(element_pred), 
	int::in, univ::in, list(maybe(string))::in, list(maybe(string))::out,
	io::di, io::uo) is det.

write_xml_element_univ_canonicalize(MakeElement, IndentLevel, Univ, 
		MaybeFieldNames0, MaybeFieldNames, !IO) :-
	write_xml_element_univ(canonicalize, MakeElement, IndentLevel, 
		Univ, MaybeFieldNames0, MaybeFieldNames, !IO).

:- pred write_xml_element_univ_include_details_cc(
	element_pred::in(element_pred), int::in, univ::in, 
	list(maybe(string))::in, list(maybe(string))::out, io::di, io::uo)
	is cc_multi.

write_xml_element_univ_include_details_cc(MakeElement, IndentLevel, Univ, 
		MaybeFieldNames0, MaybeFieldNames, !IO) :-
	write_xml_element_univ(include_details_cc, MakeElement,
		IndentLevel, Univ, MaybeFieldNames0, MaybeFieldNames, !IO).

%-----------------------------------------------------------------------------%
%
% Predicates for writing elements
%

:- pred indent(int::in, io::di, io::uo) is det.

indent(IndentLevel, !IO) :- 
	(
		IndentLevel > 0
	->
		io.write_char('\t', !IO),
		indent(IndentLevel - 1, !IO) 
	;
		true
	).

:- pred write_primitive_element_with_attr_from_source(
	string::in, list(attr_from_source)::in, 
	string::in, maybe(string)::in, type_desc.type_desc::in, io::di, io::uo)
	is det.

write_primitive_element_with_attr_from_source(Element, AttrFromSources, Value, 
		MaybeField, TypeDesc, !IO) :-
	io.write_string("<", !IO),
	io.write_string(Element, !IO),
	Attrs = make_attrs_from_sources(no, no, 
		TypeDesc, MaybeField, AttrFromSources),
	list.foldl(write_attribute, Attrs, !IO), 
	io.write_string(">", !IO),
	write_xml_escaped_string(Value, !IO),
	io.write_string("</", !IO),
	io.write_string(Element, !IO),
	io.write_string(">\n", !IO).

:- pred write_element_start_with_attr_from_source(string::in, 
	list(attr_from_source)::in, 
	maybe(string)::in, maybe(int)::in, maybe(string)::in,
	type_desc.type_desc::in, io::di, io::uo) is det.

write_element_start_with_attr_from_source(Element, AttrFromSources, 
		MaybeFunctor, MaybeArity, MaybeField, TypeDesc, !IO) :-
	Attrs = make_attrs_from_sources(MaybeFunctor, MaybeArity, 
		TypeDesc, MaybeField, AttrFromSources),
	write_element_start(Element, Attrs, !IO),
	io.nl(!IO).

:- pred write_element_start(string::in, list(attr)::in, io::di, io::uo) is det.

write_element_start(Element, Attributes, !IO) :-
	io.write_string("<", !IO),
	io.write_string(Element, !IO),
	list.foldl(write_attribute, Attributes, !IO), 
	io.write_string(">", !IO).

:- pred write_empty_element_with_attr_from_source(string::in, 
	list(attr_from_source)::in, 
	maybe(string)::in, maybe(int)::in, maybe(string)::in,
	type_desc.type_desc::in, io::di, io::uo) is det.

write_empty_element_with_attr_from_source(Element, AttrFromSources,
		MaybeFunctor, MaybeArity, MaybeField, TypeDesc, !IO) :-
	Attrs = make_attrs_from_sources(MaybeFunctor, MaybeArity, 
		TypeDesc, MaybeField, AttrFromSources),
	write_empty_element(Element, Attrs, !IO),
	io.nl(!IO).

:- pred write_empty_element(string::in, list(attr)::in, io::di, io::uo) is det.

write_empty_element(Element, Attributes, !IO) :-
	io.write_string("<", !IO),
	io.write_string(Element, !IO),
	list.foldl(write_attribute, Attributes, !IO), 
	io.write_string(" />", !IO).

:- pred write_element_end(string::in, io::di, io::uo) is det.

write_element_end(Element, !IO) :-
	io.write_string("</", !IO),
	io.write_string(Element, !IO),
	io.write_string(">", !IO).

:- func attr_from_source_to_maybe_attr(maybe(string), 
	maybe(int), type_desc.type_desc, maybe(string), attr_from_source) 
	= maybe(attr).

attr_from_source_to_maybe_attr(MaybeFunctor, MaybeArity, TypeDesc,
		MaybeFieldName, attr_from_source(Name, Source)) = MaybeAttr :-
	(
		Source = functor,
		(
			MaybeFunctor = yes(Functor),
			MaybeAttr = yes(attr(Name, Functor))
		;
			MaybeFunctor = no,
			MaybeAttr = no
		)
	;
		Source = arity,
		(
			MaybeArity = yes(Arity),
			MaybeAttr = yes(attr(Name, 
				string.int_to_string(Arity)))
		;
			MaybeArity = no,
			MaybeAttr = no
		)
	;
		Source = type_name,
		MaybeAttr = yes(attr(Name, type_desc.type_name(TypeDesc)))
	;
		Source = field_name,
		(
			MaybeFieldName = yes(FieldName),
			MaybeAttr = yes(attr(Name, FieldName))
		;
			MaybeFieldName = no,
			MaybeAttr = no
		)
	).

:- func make_attrs_from_sources(maybe(string), maybe(int), type_desc.type_desc,
	maybe(string), list(attr_from_source)) = list(attr).

make_attrs_from_sources(MaybeFunctor, MaybeArity, TypeDesc, MaybeField,
		AttrFromSources) = Attrs :-
	MaybeAttrs = list.map(attr_from_source_to_maybe_attr(MaybeFunctor, 
		MaybeArity, TypeDesc, MaybeField), AttrFromSources),
	list.filter_map(is_maybe_yes, MaybeAttrs, Attrs).

:- pred is_maybe_yes(maybe(T)::in, T::out) is semidet.

is_maybe_yes(yes(X), X).

:- pred write_attribute(attr::in, io::di, io::uo) is det.

write_attribute(attr(Name, Value), !IO) :-
	io.write_string(" ", !IO),
	io.write_string(Name, !IO),
	io.write_string("=""", !IO),
	write_xml_escaped_string(Value, !IO),
	io.write_string("""", !IO).

:- pred write_xml_escaped_string(string::in, io::di, io::uo) is det.

write_xml_escaped_string(Str, !IO) :-
	string.foldl(write_xml_escaped_char, Str, !IO).

:- pred write_xml_escaped_char(char::in, io::di, io::uo) is det.

write_xml_escaped_char(Chr, !IO) :-
	(
		xml_predefined_entity(Chr, Str)
	->
		io.write_string(Str, !IO)
	;
		io.write_char(Chr, !IO)
	).

:- pred xml_predefined_entity(char::in, string::out) is semidet.

xml_predefined_entity(('<'), "&lt;").
xml_predefined_entity(('>'), "&gt;").
xml_predefined_entity(('&'), "&amp;").
xml_predefined_entity(('\''), "&apos;").
xml_predefined_entity(('\"'), "&quot;").

%-----------------------------------------------------------------------------%
%
% Predicates to write the DTD for a type.
%

write_dtd_from_type(TypeDesc, ElementMapping, DTDResult, !IO) :-
	DTDResult = can_generate_dtd(ElementMapping, TypeDesc),
	(
		DTDResult = ok
	->
		get_element_pred(ElementMapping, MakeElement),
		(
			get_elements_and_args(MakeElement, TypeDesc,
				[RootElement], [_], [_], [PseudoArgTypes], _)
		->
			ArgTypes = list.map(
				ground_pseudo_type_desc_to_type_desc_det,
				PseudoArgTypes),
			io.write_string("<!DOCTYPE ", !IO),
			io.write_string(RootElement, !IO),
			io.write_string(" [\n\n", 
				!IO),
			write_dtd_types(MakeElement, [TypeDesc | ArgTypes], 
				map.init, !IO),
			io.write_string("\n]>", !IO),
			DTDResult = ok
		;
			throw(software_error("term_to_xml.write_dtd_from_type"
				++ ": not ok to generate DTD"))
		)
	;
		true
	).

can_generate_dtd(ElementMapping, TypeDesc) =  Result :-
	get_element_pred(ElementMapping, MakeElement),
	(
		get_elements_and_args(MakeElement, TypeDesc, [_], [_], [_], 
			[_], [_])
	->
		PseudoTypeDesc = type_desc_to_pseudo_type_desc(TypeDesc),
		Result = can_generate_dtd_for_types(MakeElement, 
			[PseudoTypeDesc], map.init, map.init)
	;
		Result = multiple_functors_for_root
	).

:- func can_generate_dtd(maybe_dtd::in, element_mapping::in(element_mapping), 
	type_desc.type_desc::in) = (dtd_generation_result::out) is det.  

can_generate_dtd(no_dtd, _, _) = ok.
can_generate_dtd(external(_), _, _) = ok.
can_generate_dtd(embed, ElementMapping, TypeDesc) 
	= can_generate_dtd(ElementMapping, TypeDesc).

	% Check that we can reliably generate a DTD for the types in the list.
	% At the moment this means each type (and all the types of the
	% arguments of functors of the type if it is a discriminated union)
	% must be either a discriminated union, an array, an int, a
	% character, a float or a string and must not be existentially
	% quantified.  
	%
:- func can_generate_dtd_for_types(element_pred::in(element_pred), 
	list(type_desc.pseudo_type_desc)::in, 
	map(type_desc.type_desc, unit)::in, 
	map(string, type_desc.type_desc)::in) = 
	(dtd_generation_result::out) is det.
	
can_generate_dtd_for_types(_, [], _, _) = ok.
can_generate_dtd_for_types(MakeElement, [PseudoTypeDesc | PseudoTypeDescs], 
		Done, ElementsSoFar) = Result :-
	(
		TypeDesc = ground_pseudo_type_desc_to_type_desc(
			PseudoTypeDesc)
	->
		(
			(
				is_discriminated_union(TypeDesc, _)
			;
				is_array(TypeDesc, _)
			;
				is_primitive_type(TypeDesc, _)
			)
		->
			
			(
				map.contains(Done, TypeDesc)
			->
				Result = can_generate_dtd_for_types(
					MakeElement, PseudoTypeDescs, 
					Done, ElementsSoFar)
			;
				get_elements_and_args(MakeElement, 
					TypeDesc, Elements, _, _,
					ArgLists, _),
				list.filter(map.contains(ElementsSoFar),
					Elements, DupElements),
				(
					DupElements = [DupElement | _],
					map.lookup(ElementsSoFar, DupElement, 
						DupTypeDesc),
					DupTypes = [TypeDesc, DupTypeDesc],
					Result = duplicate_elements(DupElement, 
						DupTypes)
				;
					DupElements = [],
					list.merge_and_remove_dups(
						list.condense(ArgLists), 
						PseudoTypeDescs, 
						NewPseudoTypeDescs),
					list.duplicate(length(Elements),
						TypeDesc, 
						TypeDescList),
				map.det_insert_from_corresponding_lists(
						ElementsSoFar, Elements,
						TypeDescList,
						NewElementsSoFar),
					map.det_insert(Done, TypeDesc,
						unit, NewDone),
					Result = can_generate_dtd_for_types(
						MakeElement, 
						NewPseudoTypeDescs,
						NewDone, 
						NewElementsSoFar)
				)
			)
		;
			Result = unsupported_dtd_type(TypeDesc)
		)
	;
		Result = type_not_ground(PseudoTypeDesc)
	).

	% Write out the DTD entries for all the given types and add the written
	% types to AlreadyDone.  Children types found along the way are added
	% to the first argument.  We stop when all the types have had their DTD
	% entry written.
	%
:- pred write_dtd_types(element_pred::in(element_pred), 
	list(type_desc.type_desc)::in, map(type_desc.type_desc, unit)::in,
	io::di, io::uo) is det.

write_dtd_types(_, [], _, !IO).
write_dtd_types(MakeElement, [TypeDesc | TypeDescs], AlreadyDone, !IO) :-
	(
		map.search(AlreadyDone, TypeDesc, _)
	->
		write_dtd_types(MakeElement, TypeDescs, AlreadyDone, !IO)
	;
		write_dtd_type_elements(MakeElement, TypeDesc, ChildArgTypes, 
			!IO),
		map.set(AlreadyDone, TypeDesc, unit, NewAlreadyDone),
		write_dtd_types(MakeElement, append(ChildArgTypes, TypeDescs),
			NewAlreadyDone, !IO)
	).

	% Write the IMPLIED, FIXED or REQUIRED part of the ATTLIST entry.
	%
:- pred write_attribute_source_kind(attr_source::in, maybe(string)::in,
	io::di, io::uo) is det. 

write_attribute_source_kind(functor, no, !IO) :-
	io.write_string("#IMPLIED", !IO).
write_attribute_source_kind(functor, yes(Value), !IO) :-
	io.write_string("#FIXED """, !IO),
	write_xml_escaped_string(Value, !IO),
	io.write_string("""", !IO).
write_attribute_source_kind(field_name, _, !IO) :-
	io.write_string("#IMPLIED", !IO).
write_attribute_source_kind(type_name, no, !IO) :-
	io.write_string("#REQUIRED", !IO).
write_attribute_source_kind(type_name, yes(Value), !IO) :-
	io.write_string("#FIXED """, !IO),
	write_xml_escaped_string(Value, !IO),
	io.write_string("""", !IO).
write_attribute_source_kind(arity, no, !IO) :-
	io.write_string("#IMPLIED", !IO).
write_attribute_source_kind(arity, yes(Value), !IO) :-
	io.write_string("#FIXED """, !IO),
	write_xml_escaped_string(Value, !IO),
	io.write_string("""", !IO).

	% Write an ATTLIST entry for the given attribute.
	%
:- pred write_dtd_attlist(string::in, maybe(string)::in, maybe(int)::in,
	type_desc.type_desc::in, attr_from_source::in, io::di, io::uo) is det.

write_dtd_attlist(Element, MaybeFunctor, MaybeArity, TypeDesc, 	
		attr_from_source(Name, Source), !IO) :-
	(
		Source = functor,
		MaybeValue = MaybeFunctor
	;
		Source = arity,
		(
			MaybeArity = yes(Arity),
			MaybeValue = yes(string.int_to_string(Arity))
		;
			MaybeArity = no,
			MaybeValue = no
		)
	;
		Source = type_name,
		MaybeValue = yes(type_desc.type_name(TypeDesc))
	;
		Source = field_name,
		MaybeValue = no
	),
	io.write_string("<!ATTLIST ", !IO),
	io.write_string(Element, !IO),
	io.write_string(" ", !IO),
	io.write_string(Name, !IO),
	io.write_string(" CDATA ", !IO),
	write_attribute_source_kind(Source, MaybeValue, !IO),
	io.write_string(">\n", !IO).

:- pred write_dtd_attlists(string::in, list(attr_from_source)::in, 
	maybe(string)::in, maybe(int)::in, type_desc.type_desc::in, 
	io::di, io::uo) is det.

write_dtd_attlists(Element, AttrFromSources, MaybeFunctor, MaybeArity,
		TypeDesc, !IO) :-
	list.foldl(write_dtd_attlist(Element, MaybeFunctor, MaybeArity, 
		TypeDesc), AttrFromSources, !IO).

	% Write DTD entries for all the functors for a type.
	%
:- pred write_dtd_type_elements(element_pred::in(element_pred),
	type_desc.type_desc::in, list(type_desc.type_desc)::out,
	io::di, io::uo) is det.

write_dtd_type_elements(MakeElement, TypeDesc, ChildArgTypes, !IO) :-
	get_elements_and_args(MakeElement, TypeDesc, Elements, 
		MaybeFunctors, MaybeArities, ArgPseudoTypeLists, 
		AttributeLists),
	ArgTypeLists = list.map(list.map(
		ground_pseudo_type_desc_to_type_desc_det), ArgPseudoTypeLists),
	list.condense(ArgTypeLists, ChildArgTypes),
	io.write_string("<!-- Elements for functors of type """, !IO),
	write_xml_escaped_string(type_desc.type_name(TypeDesc), !IO),
	io.write_string(""" -->\n\n", !IO),
	write_dtd_entries(MakeElement, TypeDesc, Elements, MaybeFunctors,
		MaybeArities, ArgTypeLists, AttributeLists, !IO).

:- pred write_dtd_entries(element_pred::in(element_pred),
	type_desc.type_desc::in, list(string)::in, list(maybe(string))::in,
	list(maybe(int))::in, list(list(type_desc.type_desc))::in,
	list(list(attr_from_source))::in, io::di, io::uo) is det.

	% Write all the given DTD entries.
	%
write_dtd_entries(_, _, [], _, _, _, _, !IO). 
write_dtd_entries(MakeElement, TypeDesc, [Element | Elements], 
		MaybeFunctorList, MaybeArityList, ArgTypeListList,
		AttributeListList, !IO) :-

	MaybeFunctor = list.det_head(MaybeFunctorList),
	MaybeFunctors = list.det_tail(MaybeFunctorList),
	MaybeArity = list.det_head(MaybeArityList),
	MaybeArities = list.det_tail(MaybeArityList),
	ArgTypeList = list.det_head(ArgTypeListList),
	ArgTypeLists = list.det_tail(ArgTypeListList),
	AttributeList = list.det_head(AttributeListList),
	AttributeLists = list.det_tail(AttributeListList),

	io.write_string("<!ELEMENT ", !IO),
	io.write_string(Element, !IO),
	io.write_string(" ", !IO),
	(
		is_primitive_type(TypeDesc, _)
	->
		io.write_string("(#PCDATA)>\n", !IO)
	;
		(
			ArgTypeList = [],
			io.write_string("EMPTY>\n", !IO)
		;
			ArgTypeList = [Head | Tail],
			(
				Tail = [_ | _],
				Braces = yes
			;
				Tail = [],
				(
					std_util.num_functors(Head) > 1
				->
					Braces = no
				;
					Braces = yes
				)
			),

			% Put extra braces for arrays for the * at the end.
			( is_array(TypeDesc, _) -> 
				io.write_string("(", !IO) 
			; 
				true 
			),
			
			( 
				Braces = yes, 
				io.write_string("(", !IO) 
			; 
				Braces = no 
			),
			
			io.write_list(ArgTypeList, ",", 
				write_dtd_allowed_functors_regex(MakeElement), 
					!IO),
			
			( 
				Braces = yes, 
				io.write_string(")", !IO) 
			; 
				Braces = no 
			),
			
			( is_array(TypeDesc, _) -> 
				io.write_string("*)", !IO) 
			; 
				true 
			),

			io.write_string(">\n", !IO)
		)
	),
	write_dtd_attlists(Element, AttributeList, MaybeFunctor, MaybeArity,
		TypeDesc, !IO),
	io.nl(!IO),
	write_dtd_entries(MakeElement, TypeDesc, Elements, MaybeFunctors, 
		MaybeArities, ArgTypeLists, AttributeLists, !IO).

	% Write the allowed functors for the type as a DTD rule regular 
	% expression.
	%
:- pred write_dtd_allowed_functors_regex(element_pred::in(element_pred),
	type_desc.type_desc::in, io::di, io::uo) is det.

write_dtd_allowed_functors_regex(MakeElement, TypeDesc, !IO) :-
	get_elements_and_args(MakeElement, TypeDesc, Elements, _, _, _, _),
	(
		length(Elements) > 1
	->
		io.write_string("(", !IO),
		io.write_list(Elements, "|", io.write_string, !IO),
		io.write_string(")", !IO)
	;
		io.write_list(Elements, "|", io.write_string, !IO)
	).

%-----------------------------------------------------------------------------%
:- end_module term_to_xml.
%-----------------------------------------------------------------------------%
