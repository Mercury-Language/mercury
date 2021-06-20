%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2007, 2010-2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
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
% Each functor in a term is given a corresponding well-formed element name
% in the XML document according to a mapping. Some predefined mappings are
% provided, but user defined mappings may also be used.
%
% Method 1 vs. Method 2
% ---------------------
%
% Method 2 can automatically generate DTDs, while method 1 cannot.
%
% Method 1 allows values of a specific type to be mapped to arbitrary XML
% elements with arbitrary children and arbitrary attributes.
% With method 2, each functor in a term can be mapped to only one XML element.
% Method 2 also only allows a selected set of attributes.
%
% Method 1 is useful for mapping a specific type to XML, for example
% mapping terms which represent mathematical expressions to MathML.
% Method 2 is useful for mapping terms of *any* type to XML.
%
% In both methods, the XML document can be annotated with a stylesheet
% reference.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_to_xml.
:- interface.

:- import_module deconstruct.
:- import_module list.
:- import_module maybe.
:- import_module stream.
:- import_module type_desc.

%---------------------------------------------------------------------------%
%
% Method 1 interface.
%

    % Instances of this typeclass can be converted to XML.
    %
:- typeclass xmlable(T) where [
    func to_xml(T::in) = (xml::out(xml_doc)) is det
].

    % Values of this type represent either a full XML document
    % or a portion of one.
    %
:- type xml
    --->    elem(
                % An XML element with a name, list of attributes
                % and a list of children.
                element_name    :: string,
                attributes      :: list(attr),
                children        :: list(xml)
            )

    ;       data(string)
            % Textual data. `<', `>', `&', `'' and `"' characters
            % will be replaced by `&lt;', `&gt;', `&amp;', `&apos;'
            % and `&quot;' respectively.

    ;       cdata(string)
            % Data to be enclosed in `<![CDATA[' and `]]>' tags.
            % The string may not contain "]]>" as a substring.
            % If it does, then the generated XML will be invalid.

    ;       comment(string)
            % An XML comment. The comment should not include
            % the `<!--' and `-->'. Any occurrences of the substring "--"
            % will be replaced by " - ", since "--" is not allowed
            % in XML comments.

    ;       entity(string)
            % An entity reference. The string will have `&' prepended
            % and `;' appended before being output.

    ;       raw(string).
            % Raw XML data. The data will be written out verbatim.

    % An XML document must have an element at the top level.
    % The following inst is used to enforce this restriction.
    %
:- inst xml_doc for xml/0
    --->    elem(
                ground, % element_name
                ground, % attributes
                ground  % children
            ).

    % An element attribute, mapping a name to a value.
    %
:- type attr
    --->    attr(string, string).

    % Values of this type specify the DOCTYPE of an XML document when
    % the DOCTYPE is defined by an external DTD.
    %
:- type doctype
    --->    public(string)                  % Formal Public Identifier (FPI)
    ;       public_system(string, string)   % FPI, URL
    ;       system(string).                 % URL

    % Values of this type specify whether a DTD should be included in
    % a generated XML document, and if so, how.
    %
:- type maybe_dtd
    --->    embed_dtd
            % Generate and embed the entire DTD in the document
            % (only available for method 2).

    ;       external_dtd(doctype)
            % Included a reference to an external DTD.

    ;       no_dtd.
            % Do not include any DOCTYPE information.

:- inst non_embedded_dtd for maybe_dtd/0
    --->    external_dtd(ground)
    ;       no_dtd.

    % Values of this type indicate whether a stylesheet reference should be
    % included in a generated XML document.
    %
:- type maybe_stylesheet
    --->    with_stylesheet(
                stylesheet_type :: string, % For example "text/xsl"
                stylesheet_href :: string
            )
    ;       no_stylesheet.

    % write_xml_doc(Stream, Term, !State):
    %
    % Output Term as an XML document to the given stream.
    % Term must be an instance of the xmlable typeclass.
    %
:- pred write_xml_doc(Stream::in, T::in, State::di, State::uo)
    is det <= (xmlable(T), stream.writer(Stream, string, State)).

    % write_xml_doc_style_dtd(Stream, Term, MaybeStyleSheet, MaybeDTD, !State):
    %
    % Write Term to the given stream as an XML document.
    % MaybeStyleSheet and MaybeDTD specify whether or not a stylesheet
    % reference and/or a DTD should be included.
    % Using this predicate, only external DTDs can be included, i.e.
    % a DTD cannot be automatically generated and embedded
    % (that feature is available only for method 2 -- see below).
    %
:- pred write_xml_doc_style_dtd(Stream::in, T::in, maybe_stylesheet::in,
    maybe_dtd::in(non_embedded_dtd), State::di, State::uo) is det
    <= (xmlable(T), stream.writer(Stream, string, State)).

    % write_xml_header(Stream, MaybeEncoding, !State):
    %
    % Write an XML header (i.e. `<?xml version="1.0"?>) to the
    % current file output stream.
    % If MaybeEncoding is yes(Encoding), then include `encoding="Encoding"'
    % in the header.
    %
:- pred write_xml_header(Stream::in, maybe(string)::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

    % write_xml_element(Stream, Indent, Term, !State):
    %
    % Write Term out as XML to the given stream, using Indent as the
    % indentation level (each indentation level is one tab character).
    % No `<?xml ... ?>' header will be written.
    % This is useful for generating large XML documents piecemeal.
    %
:- pred write_xml_element(Stream::in, int::in, T::in,
    State::di, State::uo) is det
    <= (xmlable(T), stream.writer(Stream, string, State)).

%---------------------------------------------------------------------------%
%
% Method 2 interface.
%

    % Values of this type specify which mapping from functors to elements
    % to use when generating XML. The role of a mapping is twofold:
    %   1. To map functors to elements, and
    %   2. To map functors to a set of attributes that should be
    %      generated for the corresponding element.
    %
    % We provide two predefined mappings:
    %
    %   1. simple: The functors `[]', `[|]' and `{}' are mapped to the elements
    %   `List', `Nil' and `Tuple' respectively. Arrays are assigned the
    %   `Array' element. The builtin types are assigned the elements `Int',
    %   `Int8', `Int16', `Int32' `Int64', `UInt', `UInt8', `UInt16, `UInt32',
    %   `UInt64', `String', `Float' and `Char'. All other functors are assigned
    %   elements with the same name as the functor provided the functor name is
    %   well formed and does not start with a capital letter. Otherwise, a
    %   mangled version of the functor name is used.
    %
    %   All elements except those corresponding to builtin types will have
    %   their `functor', `arity', `type' and `field' (if there is a field name)
    %   attributes set. Elements corresponding to builtin types will just have
    %   their `type' and possibly their `field' attributes set.
    %
    %   The `simple' mapping is designed to be easy to read and use, but
    %   may result in the same element being assigned to different functors.
    %
    %   2. unique: Here we use the same mapping as `simple' except we append
    %   the functor arity for discriminated unions and a mangled version
    %   of the type name for every element. The same attributes as the
    %   `simple' scheme are provided. The advantage of this scheme is that
    %   it maps each functor to a unique element. This means that it will
    %   always be possible to generate a DTD using this mapping so long as
    %   there is only one top level functor and no unsupported types
    %   can appear in terms of the type.
    %
    % A custom mapping can be provided using the `custom' functor. See the
    % documentation for the element_pred type below for more information.
    %
:- type element_mapping
    --->    simple
    ;       unique
    ;       custom(element_pred).

:- inst element_mapping for element_mapping/0
    --->    simple
    ;       unique
    ;       custom(element_pred).

    % Deterministic procedures with the following signature can be used as
    % custom functor to element mappings. The inputs to the procedure are
    % a type and some information about a functor for that type if the type
    % is a discriminated union. The output should be a well formed XML element
    % name and a list of attributes that should be set for that element.
    % See the types `maybe_functor_info' and `attr_from_source' below.
    %
:- type element_pred == (pred(type_desc, maybe_functor_info, string,
    list(attr_from_source))).

:- inst element_pred == (pred(in, in, out, out) is det).

    % Values of this type are passed to custom functor-to-element mapping
    % predicates to tell the predicate which functor to generate
    % an element name for if the type is a discriminated union.
    % If the type is not a discriminated union, then non_du is passed
    % to the predicate when requesting an element for the type.
    %
:- type maybe_functor_info
    --->    du_functor(
                % The functor's name and arity.
                functor_name    :: string,
                functor_arity   :: int
            )

    ;       non_du.
            % The type is not a discriminated union.

    % Values of this type specify attributes that should be set from
    % a particular source. The attribute_name field specifies the name
    % of the attribute in the generated XML and the attribute_source
    % field indicates where the attribute's value should come from.
    %
:- type attr_from_source
    --->    attr_from_source(
                attr_name   :: string,
                attr_source :: attr_source
            ).

    % Possible attribute sources.
    %
:- type attr_source
    --->    functor
            % The original functor name as returned by
            % deconstruct.deconstruct/5.

    ;       field_name
            % The field name, if the functor appears in a named field.
            % (If the field is not named, this attribute is omitted.)

    ;       type_name
            % The fully qualified type name the functor is for.

    ;       arity.
            % The arity of the functor as returned by
            % deconstruct.deconstruct/5.

    % To support third parties generating XML which is compatible with the
    % XML generated using method 2, a DTD for a Mercury type can also be
    % generated. A DTD for a given type and functor-to-element mapping may
    % be generated provided the following conditions hold:
    %
    %   1. If the type is a discriminated union, then there must be only one
    %   top-level functor for the type. This is because the top level functor
    %   will be used to generate the document type name.
    %
    %   2. The functor-to-element mapping must map each functor to a
    %   unique element name for every functor that could appear in
    %   terms of the type.
    %
    %   3. Only types whose terms consist of discriminated unions,
    %   arrays and the builtin types `int', `string', `character' and
    %   `float' can be used to automatically generate DTDs.
    %   Existential types are not supported either.
    %
    % The generated DTD is also a good reference when creating a stylesheet
    % as it contains comments describing the mapping from functors to elements.
    %
    % Values of the following type indicate whether a DTD was successfully
    % generated or not.
    %
:- type dtd_generation_result
    --->    ok

    ;       multiple_functors_for_root
            % The root type is a discriminated union with multiple functors.

    ;       duplicate_elements(
                % The functor-to-element mapping maps different functors
                % to the same element. The arguments identify the duplicate
                % element and a list of the types whose functors map
                % to that element.
                duplicate_element   :: string,
                duplicate_types     :: list(type_desc)
            )

    ;       unsupported_dtd_type(type_desc)
            % At the moment we only support generation of DTDs for types
            % made up of discriminated unions, arrays, strings, ints,
            % characters and floats. If a component type is not supported,
            % then it is returned as the argument of this functor.

    ;       type_not_ground(pseudo_type_desc).
            % If one of the arguments of a functor is existentially typed,
            % then the pseudo_type_desc for the existentially quantified
            % argument is returned as the argument of this functor.
            % Since the values of existentially typed arguments can be of
            % any type (provided any typeclass constraints are satisfied),
            % it is not generally possible to generate DTD rules for functors
            % with existentially typed arguments.

    % write_xml_doc_general(Stream, Term, ElementMapping,
    %   MaybeStyleSheet, MaybeDTD, DTDResult, !State):
    %
    % Write Term to the given stream as an XML document using ElementMapping
    % as the scheme to map functors to elements. MaybeStyleSheet and MaybeDTD
    % specify whether or not a stylesheet reference and/or a DTD should be
    % included. Any non-canonical terms will be canonicalized. If an embedded
    % DTD is requested, but it is not possible to generate a DTD for Term
    % using ElementMapping, then a value other than `ok' is returned in
    % DTDResult and nothing is written out. See the dtd_generation_result type
    % for a list of the other possible values of DTDResult and their meanings.
    %
:- pred write_xml_doc_general(Stream::in, T::in,
    element_mapping::in(element_mapping), maybe_stylesheet::in,
    maybe_dtd::in, dtd_generation_result::out, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

    % write_xml_doc_general_cc(Stream, Term, ElementMapping, MaybeStyleSheet,
    %    MaybeDTD, DTDResult, !State):
    %
    % Write Term to the current file output stream as an XML document using
    % ElementMapping as the scheme to map functors to elements.
    % MaybeStyleSheet and MaybeDTD specify whether or not a stylesheet
    % reference and/or a DTD should be included. Any non-canonical terms
    % will be written out in full. If an embedded DTD is requested, but
    % it is not possible to generate a DTD for Term using ElementMapping,
    % then a value other than `ok' is returned in DTDResult and nothing is
    % written out. See the dtd_generation_result type for a list of the
    % other possible values of DTDResult and their meanings.
    %
:- pred write_xml_doc_general_cc(Stream::in, T::in,
    element_mapping::in(element_mapping), maybe_stylesheet::in,
    maybe_dtd::in, dtd_generation_result::out, State::di, State::uo)
    is cc_multi <= stream.writer(Stream, string, State).

    % write_xml_element_general(Stream, NonCanon, MakeElement, IndentLevel,
    %   Term, !State):
    %
    % Write XML elements for the given term and all its descendents, using
    % IndentLevel as the initial indentation level (each indentation level
    % is one tab character) and using the MakeElement predicate to map
    % functors to elements. No <?xml ... ?> header will be written.
    % Non-canonical terms will be handled according to the value of NonCanon.
    % See the deconstruct module in the standard library for more information
    % on this argument.
    %
:- pred write_xml_element_general(Stream, deconstruct.noncanon_handling,
    element_mapping, int, T, State, State)
    <= stream.writer(Stream, string, State).
:- mode write_xml_element_general(in, in(do_not_allow), in(element_mapping),
    in, in, di, uo) is det.
:- mode write_xml_element_general(in, in(canonicalize), in(element_mapping),
    in, in, di, uo) is det.
:- mode write_xml_element_general(in, in(include_details_cc),
    in(element_mapping), in, in, di, uo) is cc_multi.
:- mode write_xml_element_general(in, in, in(element_mapping),
    in, in, di, uo) is cc_multi.

%---------------------------------------------------------------------------%

    % can_generate_dtd(ElementMapping, Type) = Result:
    %
    % Check if a DTD can be generated for the given Type using the
    % functor-to-element mapping scheme ElementMapping. Return `ok' if it
    % is possible to generate a DTD. See the documentation of the
    % dtd_generation_result type for the meaning of the return value when
    % it is not `ok'.
    %
:- func can_generate_dtd(element_mapping::in(element_mapping),
    type_desc::in) = (dtd_generation_result::out) is det.

    % write_dtd(Stream, Term, ElementMapping, DTDResult, !State):
    %
    % Write a DTD for the given term to the current file output stream using
    % ElementMapping to map functors to elements. If a DTD cannot be generated
    % for Term using ElementMapping, then a value other than `ok' is returned
    % in DTDResult and nothing is written. See the dtd_generation_result type
    % for a list of the other possible values of DTDResult and their meanings.
    %
:- pred write_dtd(Stream::in, T::unused,
    element_mapping::in(element_mapping), dtd_generation_result::out,
    State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

    % write_dtd_for_type(Stream, Type, ElementMapping, DTDResult, !State):
    %
    % Write a DTD for the given type to the given stream. If a DTD cannot
    % be generated for Type using ElementMapping then a value other than `ok'
    % is returned in DTDResult and nothing is written. See the
    % dtd_generation_result type for a list of the other possible values
    % of DTDResult and their meanings.
    %
:- pred write_dtd_from_type(Stream::in, type_desc::in,
    element_mapping::in(element_mapping), dtd_generation_result::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module construct.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module unit.
:- import_module univ.

%---------------------------------------------------------------------------%

write_xml_doc(Stream, Term, !State) :-
    write_xml_doc_style_dtd(Stream, Term, no_stylesheet, no_dtd, !State).

write_xml_doc_style_dtd(Stream, Term, MaybeStyleSheet, MaybeDTD, !State) :-
    write_xml_header(Stream, no, !State),
    write_stylesheet_ref(Stream, MaybeStyleSheet, !State),
    Root = to_xml(Term),
    Root = elem(RootName, _, Children),
    (
        MaybeDTD = no_dtd
    ;
        MaybeDTD = external_dtd(DocType),
        write_external_doctype(Stream, RootName, DocType, !State)
    ),
    ( if contains_noformat_xml(Children) then
        ChildrenFormat = no_format
    else
        ChildrenFormat = format
    ),
    write_xml_element_format(Stream, ChildrenFormat, 0, Root, !State).

write_xml_header(Stream, MaybeEncoding, !State) :-
    put(Stream, "<?xml version=""1.0""", !State),
    (
        MaybeEncoding = yes(Encoding),
        put(Stream, " encoding=""", !State),
        put(Stream, Encoding, !State),
        put(Stream, """?>\n", !State)
    ;
        MaybeEncoding = no,
        put(Stream, "?>\n", !State)
    ).

%---------------------------------------------------------------------------%

write_xml_element(Stream, Indent, Term, !State) :-
    Root = to_xml(Term),
    Root = elem(_, _, Children),
    ( if contains_noformat_xml(Children) then
        ChildrenFormat = no_format
    else
        ChildrenFormat = format
    ),
    write_xml_element_format(Stream, ChildrenFormat, Indent, Root, !State).

write_xml_doc_general(Stream, Term, ElementMapping, MaybeStyleSheet, MaybeDTD,
        DTDResult, !State) :-
    DTDResult = can_generate_dtd_2(MaybeDTD, ElementMapping, type_of(Term)),
    (
        DTDResult = ok,
        write_xml_header(Stream, no, !State),
        write_stylesheet_ref(Stream, MaybeStyleSheet, !State),
        write_doctype(Stream, canonicalize, Term, ElementMapping, MaybeDTD, _,
            !State),
        write_xml_element_general(Stream, canonicalize, ElementMapping, 0,
            Term, !State)
    ;
        ( DTDResult = multiple_functors_for_root
        ; DTDResult = duplicate_elements(_, _)
        ; DTDResult = unsupported_dtd_type(_)
        ; DTDResult = type_not_ground(_)
        )
    ).

write_xml_doc_general_cc(Stream, Term, ElementMapping, MaybeStyleSheet,
        MaybeDTD, DTDResult, !State) :-
    DTDResult = can_generate_dtd_2(MaybeDTD, ElementMapping, type_of(Term)),
    (
        DTDResult = ok,
        write_xml_header(Stream, no, !State),
        write_stylesheet_ref(Stream, MaybeStyleSheet, !State),
        write_doctype(Stream, include_details_cc, Term, ElementMapping,
            MaybeDTD, _, !State),
        write_xml_element_general(Stream, include_details_cc, ElementMapping,
            0, Term, !State)
    ;
        ( DTDResult = multiple_functors_for_root
        ; DTDResult = duplicate_elements(_, _)
        ; DTDResult = unsupported_dtd_type(_)
        ; DTDResult = type_not_ground(_)
        )
    ).

write_xml_element_general(Stream, NonCanon, ElementMapping, IndentLevel, Term,
        !State) :-
    type_to_univ(Term, Univ),
    get_element_pred(ElementMapping, MakeElement),
    write_xml_element_univ(Stream, NonCanon, MakeElement, IndentLevel, Univ,
        [], _, !State).

%---------------------------------------------------------------------------%

:- pred write_stylesheet_ref(Stream::in, maybe_stylesheet::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_stylesheet_ref(Stream, MaybeStyleSheet, !State) :-
    (
        MaybeStyleSheet = no_stylesheet
    ;
        MaybeStyleSheet = with_stylesheet(Type, Href),
        put(Stream, "<?xml-stylesheet type=""", !State),
        put(Stream, Type, !State),
        put(Stream, """ href=""", !State),
        put(Stream, Href, !State),
        put(Stream, """?>\n", !State)
    ).

:- pred write_doctype(Stream, deconstruct.noncanon_handling, T,
    element_mapping, maybe_dtd, dtd_generation_result, State, State)
    <= stream.writer(Stream, string, State).
:- mode write_doctype(in, in(canonicalize), in, in(element_mapping), in, out,
    di, uo) is det.
:- mode write_doctype(in, in(do_not_allow), in, in(element_mapping), in, out,
    di, uo) is det.
:- mode write_doctype(in, in(include_details_cc), in, in(element_mapping),
    in, out, di, uo) is cc_multi.
:- mode write_doctype(in, in, in, in(element_mapping), in, out,
    di, uo) is cc_multi.

write_doctype(Stream, NonCanon, Term, ElementMapping, MaybeDTD, DTDResult,
        !State) :-
    (
        MaybeDTD = no_dtd,
        DTDResult = ok
    ;
        MaybeDTD = embed_dtd,
        write_dtd(Stream, Term, ElementMapping, DTDResult, !State),
        put(Stream, "\n", !State)
    ;
        MaybeDTD = external_dtd(DocType),
        get_element_pred(ElementMapping, MakeElement),
        deconstruct.deconstruct(Term, NonCanon, Functor, Arity, _),
        TypeOfTerm = type_of(Term),
        ( if is_discriminated_union(TypeOfTerm, _) then
            Request = du_functor(Functor, Arity)
        else
            Request = non_du
        ),
        MakeElement(TypeOfTerm, Request, Root, _),
        write_external_doctype(Stream, Root, DocType, !State),
        DTDResult = ok
    ).

:- pred write_external_doctype(Stream::in, string::in, doctype::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_external_doctype(Stream, Root, DocType, !State) :-
    put(Stream, "<!DOCTYPE ", !State),
    put(Stream, Root, !State),
    (
        DocType = public(PUBLIC),
        put(Stream, " PUBLIC """, !State),
        put(Stream, PUBLIC, !State)
    ;
        DocType = public_system(PUBLIC, SYSTEM),
        put(Stream, " PUBLIC """, !State),
        put(Stream, PUBLIC, !State),
        put(Stream, """ """, !State),
        put(Stream, SYSTEM, !State)
    ;
        DocType = system(SYSTEM),
        put(Stream, " SYSTEM """, !State),
        put(Stream, SYSTEM, !State)
    ),
    put(Stream, """>\n", !State).

    % Implementation of the `unique' predefined mapping scheme.
    %
:- pred make_unique_element(type_desc::in, maybe_functor_info::in,
    string::out, list(attr_from_source)::out) is det.

make_unique_element(TypeDesc, MaybeFunctorInfo, Element, AttrFromSources) :-
    (
        MaybeFunctorInfo = du_functor(Functor, Arity),
        MangledElement = maybe_mangle_uncommon_functor(Functor),
        Element = MangledElement ++ "--" ++ string.int_to_string(Arity) ++
            "--" ++ mangle(type_name(TypeDesc)),
        AttrFromSources = all_attr_sources
    ;
        MaybeFunctorInfo = non_du,
        ( if is_primitive_type(TypeDesc, PrimitiveElement) then
            Element = PrimitiveElement,
            AttrFromSources = [attr_from_source("type", type_name),
                attr_from_source("field", field_name)]
        else if is_array(TypeDesc, _) then
            Element = array_element ++ "--" ++
                mangle(type_name(TypeDesc)),
            AttrFromSources = all_attr_sources
        else
            Element = mangle(type_name(TypeDesc)),
            AttrFromSources = all_attr_sources
        )
    ).

    % Implementation of the `simple' mapping scheme.
    %
:- pred make_simple_element(type_desc::in, maybe_functor_info::in,
    string::out, list(attr_from_source)::out) is det.

make_simple_element(TypeDesc, MaybeFunctorInfo, Element, AttrFromSources) :-
    (
        MaybeFunctorInfo = du_functor(Functor, _),
        Element = maybe_mangle_uncommon_functor(Functor),
        AttrFromSources = all_attr_sources
    ;
        MaybeFunctorInfo = non_du,
        ( if is_primitive_type(TypeDesc, PrimitiveElement) then
            Element = PrimitiveElement,
            AttrFromSources = [attr_from_source("type", type_name),
                attr_from_source("field", field_name)]
        else if is_array(TypeDesc, _) then
            Element = array_element,
            AttrFromSources = all_attr_sources
        else
            Element = "Unknown",
            AttrFromSources = all_attr_sources
        )
    ).

:- func all_attr_sources = list(attr_from_source).

all_attr_sources =
    [
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

%---------------------------------------------------------------------------%
%
% Some reserved element names for the predefined mapping schemes. Reserved
% element names all start with a capital letter so as not to conflict with a
% mangled element name.
%

    % A prefix for functors that start with a capital letter or a non-letter.
    %
:- func reserved_prefix = string.

reserved_prefix = "Tag_".

:- pred common_mercury_functor(string::in, string::out) is semidet.

    % These should all start with a capital letter so as not to conflict
    % with a mangled name.
    %
common_mercury_functor("[|]", "List").
common_mercury_functor("[]", "Nil").
common_mercury_functor("{}", "Tuple").

:- func maybe_mangle_uncommon_functor(string) = string.

maybe_mangle_uncommon_functor(Functor) = Element :-
    ( if common_mercury_functor(Functor, ReservedElement) then
        Element = ReservedElement
    else
        Element = mangle(Functor)
    ).

:- func array_element = string.

array_element = "Array".

:- pred is_primitive_type(type_desc::in, string::out) is semidet.

is_primitive_type(TypeDesc, Element) :-
    ( if TypeDesc = type_of(_ : string) then
        Element = "String"
    else if TypeDesc = type_of(_ : character) then
        Element = "Char"
    else if TypeDesc = type_of(_ : int) then
        Element = "Int"
    else if TypeDesc = type_of(_ : float) then
        Element = "Float"
    else if TypeDesc = type_of(_ : uint) then
        Element = "UInt"
    else if TypeDesc = type_of(_ : int8) then
        Element = "Int8"
    else if TypeDesc = type_of(_ : int16) then
        Element = "Int16"
    else if TypeDesc = type_of(_ : int32) then
        Element = "Int32"
    else if TypeDesc = type_of(_ : int64) then
        Element = "Int64"
    else if TypeDesc = type_of(_ : uint8) then
        Element = "UInt8"
    else if TypeDesc = type_of(_ : uint16) then
        Element = "UInt16"
    else if TypeDesc = type_of(_ : uint32) then
        Element = "UInt32"
    else if TypeDesc = type_of(_ : uint64) then
        Element = "UInt64"
    else
        fail
    ).

%---------------------------------------------------------------------------%
%
% Mangling functions.
%
% We use the following mangling scheme to create well formed element names
% that do not begin with a capital letter (capitals are used for reserved
% elements).
%
% If the string to be mangled begins with a capital letter then we prefix it
% with another string reserved for this purpose. Then we replace all
% characters which aren't alphanumeric or underscores with '-' followed by
% the character code.
%
% For example "my-functor!" would become "my-45functor-33".
% If we were using "Tag_" as the prefix for strings that start with
% capital letters, then "MyFunctor" would become "Tag_MyFunctor".
%

:- func mangle(string) = string.

mangle(Functor) = Element :-
    string.split(Functor, 1, Head, Tail),
    ( if
        string.is_all_alpha(Head),
        LowerHead = string.to_lower(Head),
        Head = LowerHead
    then
        First = Head,
        Rest = Tail
    else
        First = reserved_prefix,
        Rest = Head ++ Tail
    ),
    string.foldl(mangle_char, Rest, [], ElementChrs),
    Element = First ++ string.from_char_list(ElementChrs).

:- pred mangle_char(char::in, list(char)::in, list(char)::out) is det.

mangle_char(Chr, PrevChrs, list.append(PrevChrs, Chrs)) :-
    % XXX This is system dependent since char.to_int is system dependent.
    ( if char.is_alnum_or_underscore(Chr) then
        Chrs = [Chr]
    else
        Chrs = ['-' |
            string.to_char_list(string.int_to_string(char.to_int(Chr)))]
    ).

%---------------------------------------------------------------------------%

    % Return a list of elements, functors and arities (if the type is
    % a discriminated union), argument types and attributes for
    % all the functors for the type. Only one element will be in each list
    % if the type is not a discriminated union.
    %
:- pred get_elements_and_args(element_pred::in(element_pred),
    type_desc::in, list(string)::out, list(maybe(string))::out,
    list(maybe(int))::out, list(list(pseudo_type_desc))::out,
    list(list(attr_from_source))::out) is det.

get_elements_and_args(MakeElement, TypeDesc, Elements, MaybeFunctors,
        MaybeArities, ArgTypeLists, AttributeLists) :-
    ( if is_discriminated_union(TypeDesc, NumFunctors) then
        FunctorNums = 0 .. (NumFunctors - 1),
        ( if
            list.map3(construct.get_functor(TypeDesc), FunctorNums,
                Functors, Arities, ArgTypeLists0)
        then
            MaybeFunctors = list.map((func(X) = yes(X)), Functors),
            MaybeArities = list.map((func(X) = yes(X)), Arities),
            ArgTypeLists = ArgTypeLists0,
            Requests = list.map_corresponding(make_du_functor,
                Functors, Arities),
            P = (pred(A::in, B::out, C::out) is det :-
                MakeElement(TypeDesc, A, B, C)),
            list.map2(P, Requests, Elements, AttributeLists)
        else
            unexpected($pred, "get_functor failed for discriminated union")
        )
    else
        MakeElement(TypeDesc, non_du, Element, AttrFromSources),
        Elements = [Element],
        AttributeLists = [AttrFromSources],
        MaybeFunctors = [no],
        MaybeArities = [no],
        ( if is_array(TypeDesc, ArgType) then
            ArgTypeLists = [[ArgType]]
        else
            ArgTypeLists = [[]]
        )
    ).

:- func make_du_functor(string, int) = maybe_functor_info.

make_du_functor(Functor, Arity) = du_functor(Functor, Arity).

:- pred primitive_value(univ::in, string::out) is semidet.

primitive_value(Univ, PrimValue) :-
    ( if univ_to_type(Univ, String) then
        PrimValue = String : string
    else if univ_to_type(Univ, Char) then
        PrimValue = char_to_string(Char)
    else if univ_to_type(Univ, Int) then
        PrimValue = int_to_string(Int)
    else if univ_to_type(Univ, Float) then
        PrimValue = float_to_string(Float)
    else if univ_to_type(Univ, UInt) then
        PrimValue = uint_to_string(UInt)
    else if univ_to_type(Univ, Int8) then
        PrimValue = int8_to_string(Int8)
    else if univ_to_type(Univ, Int16) then
        PrimValue = int16_to_string(Int16)
    else if univ_to_type(Univ, Int32) then
        PrimValue = int32_to_string(Int32)
    else if univ_to_type(Univ, Int64) then
        PrimValue = int64_to_string(Int64)
    else if univ_to_type(Univ, UInt8) then
        PrimValue = uint8_to_string(UInt8)
    else if univ_to_type(Univ, UInt16) then
        PrimValue = uint16_to_string(UInt16)
    else if univ_to_type(Univ, UInt32) then
        PrimValue = uint32_to_string(UInt32)
    else
        univ_to_type(Univ, UInt64),
        PrimValue = uint64_to_string(UInt64)
    ).

%---------------------------------------------------------------------------%

    % The following type is used to decide if an entity should be formatted
    % (i.e. be indented and have a newline at the end). We do not format
    % an entity if any of its siblings are anything besides an element,
    % a CDATA entity or a comment, since then whitespaces are more likely
    % to be significant. (Although technically spaces are always significant,
    % they are usually interpreted as only formatting when they are between
    % markup).
    %
:- type maybe_format
    --->    format
    ;       no_format.

:- pred write_xml_element_format(Stream::in, maybe_format::in, int::in,
    xml::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_xml_element_format(Stream, Format, IndentLevel, XML, !State) :-
    (
        XML = elem(Name, Attrs, Children),
        maybe_indent(Stream, Format, IndentLevel, !State),
        (
            Children = [],
            write_empty_element(Stream, Name, Attrs, !State),
            maybe_nl(Stream, Format, !State)
        ;
            Children = [_ | _],
            write_element_start(Stream, Name, Attrs, !State),
            ( if contains_noformat_xml(Children) then
                ChildrenFormat = no_format
            else
                ChildrenFormat = format,
                put(Stream, "\n", !State)
            ),
            list.foldl(write_xml_element_format(Stream, ChildrenFormat,
                IndentLevel + 1), Children, !State),
            maybe_indent(Stream, ChildrenFormat, IndentLevel, !State),
            write_element_end(Stream, Name, !State),
            maybe_nl(Stream, Format, !State)
        )
    ;
        XML = data(Data),
        write_xml_escaped_string(Stream, Data, !State)
    ;
        XML = cdata(CData),
        maybe_indent(Stream, Format, IndentLevel, !State),
        put(Stream, "<![CDATA[", !State),
        put(Stream, CData, !State),
        put(Stream, "]]>", !State),
        maybe_nl(Stream, Format, !State)
    ;
        XML = comment(Comment),
        maybe_indent(Stream, Format, IndentLevel, !State),
        put(Stream, "<!-- ", !State),
        % Comments may not contain "--", so replace with " - ".
        string.replace_all(Comment, "--", " - ", EscapedComment),
        put(Stream, EscapedComment, !State),
        put(Stream, " -->", !State),
        maybe_nl(Stream, Format, !State)
    ;
        XML = entity(EntityName),
        put(Stream, "&", !State),
        put(Stream, EntityName ++ ";", !State)
    ;
        XML = raw(RawString),
        put(Stream, RawString, !State)
    ).

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

:- pred maybe_nl(Stream::in, maybe_format::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

maybe_nl(_Stream, no_format, !State).
maybe_nl(Stream, format, !State) :-
    put(Stream, "\n", !State).

:- pred maybe_indent(Stream::in, maybe_format::in, int::in,
    State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

maybe_indent(Stream, Format, Indent, !State) :-
    (
        Format = format,
        indent(Stream, Indent, !State)
    ;
        Format = no_format
    ).

%---------------------------------------------------------------------------%

    % write_xml_element_univ(Stream, NonCanon, MakeElement, IndentLevel, Univ,
    %   MaybeFieldNames, RemainingMaybeFieldNames, !State):
    %
    % Write an element and all its descendents to the current output stream.
    % If MaybeFields isn't empty, then its head is used for the `field'
    % attribute and the Tail is returned in RemainingMaybeFieldNames.
    % This is so it can be called using foldl2.
    %
:- pred write_xml_element_univ(Stream, deconstruct.noncanon_handling,
    element_pred, int, univ, list(maybe(string)),
    list(maybe(string)), State, State) <= stream.writer(Stream, string, State).
:- mode write_xml_element_univ(in, in(do_not_allow), in(element_pred), in, in,
    in, out, di, uo) is det.
:- mode write_xml_element_univ(in, in(canonicalize), in(element_pred), in, in,
    in, out, di, uo) is det.
:- mode write_xml_element_univ(in, in(include_details_cc), in(element_pred),
    in, in, in, out, di, uo) is cc_multi.
:- mode write_xml_element_univ(in, in, in(element_pred), in, in, in, out, di,
    uo) is cc_multi.

write_xml_element_univ(Stream, NonCanon, MakeElement, IndentLevel, Univ,
        MaybeFieldNames, RemainingMaybeFieldNames, !State) :-
    (
        MaybeFieldNames = [MaybeFieldName | RemainingMaybeFieldNames]
    ;
        MaybeFieldNames = [],
        RemainingMaybeFieldNames = [],
        MaybeFieldName = no
    ),
    deconstruct.deconstruct(Term, NonCanon, Functor, Arity, Args),
    Term = univ_value(Univ),
    TypeDesc = type_of(Term),
    ( if is_discriminated_union(TypeDesc, _) then
        Request = du_functor(Functor, Arity)
    else
        Request = non_du
    ),
    MakeElement(TypeDesc, Request, Element, AttrFromSources),
    ( if primitive_value(Univ, PrimValue) then
        indent(Stream, IndentLevel, !State),
        write_primitive_element_with_attr_from_source(Stream, Element,
            AttrFromSources, PrimValue, MaybeFieldName, TypeDesc, !State)
    else
        (
            Args = [],
            indent(Stream, IndentLevel, !State),
            write_empty_element_with_attr_from_source(Stream, Element,
                AttrFromSources, yes(Functor), yes(Arity), MaybeFieldName,
                TypeDesc, !State)
        ;
            Args = [_ | _],
            ChildMaybeFieldNames = get_field_names(TypeDesc, Functor, Arity),
            indent(Stream, IndentLevel, !State),
            write_element_start_with_attr_from_source(Stream, Element,
                AttrFromSources, yes(Functor), yes(Arity), MaybeFieldName,
                TypeDesc, !State),
            write_child_xml_elements(Stream, NonCanon, MakeElement,
                IndentLevel + 1, Args, ChildMaybeFieldNames, !State),
            indent(Stream, IndentLevel, !State),
            write_element_end(Stream, Element, !State),
            put(Stream, "\n", !State)
        )
    ).

:- pred is_discriminated_union(type_desc::in, int::out) is semidet.

is_discriminated_union(TypeDesc, NumFunctors) :-
    NumFunctors = num_functors(TypeDesc),
    NumFunctors > -1.

:- pred is_array(type_desc::in, pseudo_type_desc::out) is semidet.

is_array(TypeDesc, ArgPseudoType) :-
    PseudoTypeDesc = type_desc_to_pseudo_type_desc(TypeDesc),
    pseudo_type_ctor_and_args(PseudoTypeDesc, TypeCtor, ArgPseudoTypes),
    ArgPseudoTypes = [ArgPseudoType],
    type_ctor_name(TypeCtor) = "array",
    type_ctor_module_name(TypeCtor) = "array".

:- func get_field_names(type_desc, string, int) = list(maybe(string)).

get_field_names(TypeDesc, Functor, Arity) = MaybeFields :-
    ( if is_discriminated_union(TypeDesc, NumFunctors) then
        FunctorNums = 0 .. (NumFunctors - 1),
        ( if
            find_field_names(TypeDesc, FunctorNums, Functor, Arity,
                FoundMaybeFields)
        then
            MaybeFields = FoundMaybeFields
        else
            MaybeFields = []
        )
    else
        MaybeFields = []
    ).

:- pred find_field_names(type_desc::in, list(int)::in, string::in,
    int::in, list(maybe(string))::out) is semidet.

find_field_names(TypeDesc, [FunctorNum | FunctorNums], Functor, Arity,
        MaybeFieldNames) :-
    ( if
        construct.get_functor_with_names(TypeDesc, FunctorNum,
            Functor, Arity, _, FoundFieldNames)
    then
        MaybeFieldNames = FoundFieldNames
    else
        find_field_names(TypeDesc, FunctorNums, Functor, Arity,
            MaybeFieldNames)
    ).

%---------------------------------------------------------------------------%

:- pred write_child_xml_elements(Stream, deconstruct.noncanon_handling,
    element_pred, int, list(univ), list(maybe(string)), State, State)
    <= stream.writer(Stream, string, State).
:- mode write_child_xml_elements(in, in(do_not_allow), in(element_pred), in,
    in, in, di, uo) is det.
:- mode write_child_xml_elements(in, in(canonicalize), in(element_pred), in,
    in, in, di, uo) is det.
:- mode write_child_xml_elements(in, in(include_details_cc), in(element_pred),
    in, in, in,  di, uo) is cc_multi.
:- mode write_child_xml_elements(in, in, in(element_pred), in, in, in, di, uo)
    is cc_multi.

write_child_xml_elements(Stream, NonCanon, MakeElement, IndentLevel, Args,
        MaybeFieldNames, !State) :-
    % Higher order terms with more than one mode can't be passed as arguments,
    % so we can't pass write_xml_element_univ to foldl2. Hence we need
    % this switch.
    (
        NonCanon = do_not_allow,
        list.foldl2(
            write_xml_element_univ_do_not_allow(Stream, MakeElement,
                IndentLevel), Args,
            MaybeFieldNames, _, !State)
    ;
        NonCanon = canonicalize,
        list.foldl2(
            write_xml_element_univ_canonicalize(Stream, MakeElement,
                IndentLevel), Args,
            MaybeFieldNames, _, !State)
    ;
        NonCanon = include_details_cc,
        list.foldl2(
            write_xml_element_univ_include_details_cc(Stream, MakeElement,
                IndentLevel), Args,
            MaybeFieldNames, _, !State)
    ).

:- pred write_xml_element_univ_do_not_allow(Stream::in,
    element_pred::in(element_pred),
    int::in, univ::in, list(maybe(string))::in, list(maybe(string))::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_xml_element_univ_do_not_allow(Stream, MakeElement, IndentLevel, Univ,
        MaybeFieldNames0, MaybeFieldNames, !State) :-
    write_xml_element_univ(Stream, do_not_allow, MakeElement, IndentLevel,
        Univ, MaybeFieldNames0, MaybeFieldNames, !State).

:- pred write_xml_element_univ_canonicalize(Stream::in,
    element_pred::in(element_pred),
    int::in, univ::in, list(maybe(string))::in, list(maybe(string))::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_xml_element_univ_canonicalize(Stream, MakeElement, IndentLevel, Univ,
        MaybeFieldNames0, MaybeFieldNames, !State) :-
    write_xml_element_univ(Stream, canonicalize, MakeElement, IndentLevel,
        Univ, MaybeFieldNames0, MaybeFieldNames, !State).

:- pred write_xml_element_univ_include_details_cc(Stream::in,
    element_pred::in(element_pred), int::in, univ::in,
    list(maybe(string))::in, list(maybe(string))::out, State::di, State::uo)
    is cc_multi <= stream.writer(Stream, string, State).

write_xml_element_univ_include_details_cc(Stream, MakeElement, IndentLevel,
        Univ, MaybeFieldNames0, MaybeFieldNames, !State) :-
    write_xml_element_univ(Stream, include_details_cc, MakeElement,
        IndentLevel, Univ, MaybeFieldNames0, MaybeFieldNames, !State).

%---------------------------------------------------------------------------%
%
% Predicates for writing elements.
%

:- pred indent(Stream::in, int::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

indent(Stream, IndentLevel, !State) :-
    ( if IndentLevel > 0 then
        put(Stream, "\t", !State),
        indent(Stream, IndentLevel - 1, !State)
    else
        true
    ).

:- pred write_primitive_element_with_attr_from_source(Stream::in, string::in,
   list(attr_from_source)::in, string::in, maybe(string)::in,
   type_desc::in, State::di, State::uo) is det
   <= stream.writer(Stream, string, State).

write_primitive_element_with_attr_from_source(Stream, Element,
        AttrFromSources, Value, MaybeField, TypeDesc, !State) :-
    put(Stream, "<", !State),
    put(Stream, Element, !State),
    Attrs = make_attrs_from_sources(no, no,
        TypeDesc, MaybeField, AttrFromSources),
    list.foldl(write_attribute(Stream), Attrs, !State),
    put(Stream, ">", !State),
    write_xml_escaped_string(Stream, Value, !State),
    put(Stream, "</", !State),
    put(Stream, Element, !State),
    put(Stream, ">\n", !State).

:- pred write_element_start_with_attr_from_source(Stream::in, string::in,
    list(attr_from_source)::in,
    maybe(string)::in, maybe(int)::in, maybe(string)::in,
    type_desc::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_element_start_with_attr_from_source(Stream, Element, AttrFromSources,
        MaybeFunctor, MaybeArity, MaybeField, TypeDesc, !State) :-
    Attrs = make_attrs_from_sources(MaybeFunctor, MaybeArity,
        TypeDesc, MaybeField, AttrFromSources),
    write_element_start(Stream, Element, Attrs, !State),
    put(Stream, "\n", !State).

:- pred write_element_start(Stream::in, string::in, list(attr)::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_element_start(Stream, Element, Attributes, !State) :-
    put(Stream, "<", !State),
    put(Stream, Element, !State),
    list.foldl(write_attribute(Stream), Attributes, !State),
    put(Stream, ">", !State).

:- pred write_empty_element_with_attr_from_source(Stream::in, string::in,
    list(attr_from_source)::in, maybe(string)::in, maybe(int)::in,
    maybe(string)::in, type_desc::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_empty_element_with_attr_from_source(Stream, Element, AttrFromSources,
        MaybeFunctor, MaybeArity, MaybeField, TypeDesc, !State) :-
    Attrs = make_attrs_from_sources(MaybeFunctor, MaybeArity,
        TypeDesc, MaybeField, AttrFromSources),
    write_empty_element(Stream, Element, Attrs, !State),
    put(Stream, "\n", !State).

:- pred write_empty_element(Stream::in, string::in, list(attr)::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_empty_element(Stream, Element, Attributes, !State) :-
    put(Stream, "<", !State),
    put(Stream, Element, !State),
    list.foldl(write_attribute(Stream), Attributes, !State),
    put(Stream, " />", !State).

:- pred write_element_end(Stream::in, string::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_element_end(Stream, Element, !State) :-
    put(Stream, "</", !State),
    put(Stream, Element, !State),
    put(Stream, ">", !State).

:- func attr_from_source_to_maybe_attr(maybe(string), maybe(int), type_desc,
    maybe(string), attr_from_source) = maybe(attr).

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
        MaybeAttr = yes(attr(Name, type_name(TypeDesc)))
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

:- func make_attrs_from_sources(maybe(string), maybe(int), type_desc,
    maybe(string), list(attr_from_source)) = list(attr).

make_attrs_from_sources(MaybeFunctor, MaybeArity, TypeDesc, MaybeField,
        AttrFromSources) = Attrs :-
    MaybeAttrs = list.map(attr_from_source_to_maybe_attr(MaybeFunctor,
        MaybeArity, TypeDesc, MaybeField), AttrFromSources),
    list.filter_map(is_maybe_yes, MaybeAttrs, Attrs).

:- pred is_maybe_yes(maybe(T)::in, T::out) is semidet.

is_maybe_yes(yes(X), X).

:- pred write_attribute(Stream::in, attr::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_attribute(Stream, attr(Name, Value), !State) :-
    put(Stream, " ", !State),
    put(Stream, Name, !State),
    put(Stream, "=""", !State),
    write_xml_escaped_string(Stream, Value, !State),
    put(Stream, """", !State).

:- pred write_xml_escaped_string(Stream::in, string::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_xml_escaped_string(Stream, Str, !State) :-
    string.foldl(write_xml_escaped_char(Stream), Str, !State).

:- pred write_xml_escaped_char(Stream::in, char::in, State::di, State::uo)
    is det <= stream.writer(Stream, string, State).

write_xml_escaped_char(Stream, Chr, !State) :-
    ( if xml_predefined_entity(Chr, Str) then
        put(Stream, Str, !State)
    else
        put(Stream, string.from_char(Chr), !State)
    ).

:- pred xml_predefined_entity(char::in, string::out) is semidet.

xml_predefined_entity(('<'), "&lt;").
xml_predefined_entity(('>'), "&gt;").
xml_predefined_entity(('&'), "&amp;").
xml_predefined_entity(('\''), "&apos;").
xml_predefined_entity(('\"'), "&quot;").

%---------------------------------------------------------------------------%

can_generate_dtd(ElementMapping, TypeDesc) =  Result :-
    get_element_pred(ElementMapping, MakeElement),
    ( if
        get_elements_and_args(MakeElement, TypeDesc, [_], [_], [_], [_], [_])
    then
        PseudoTypeDesc = type_desc_to_pseudo_type_desc(TypeDesc),
        Result = can_generate_dtd_for_types(MakeElement, [PseudoTypeDesc],
            map.init, map.init)
    else
        Result = multiple_functors_for_root
    ).

:- func can_generate_dtd_2(maybe_dtd::in, element_mapping::in(element_mapping),
    type_desc::in) = (dtd_generation_result::out) is det.

can_generate_dtd_2(no_dtd, _, _) = ok.
can_generate_dtd_2(external_dtd(_), _, _) = ok.
can_generate_dtd_2(embed_dtd, ElementMapping, TypeDesc)
    = can_generate_dtd(ElementMapping, TypeDesc).

    % Check that we can reliably generate a DTD for the types in the list.
    % At the moment this means each type (and all the types of the
    % arguments of functors of the type if it is a discriminated union)
    % must be either a discriminated union, an array, an int, a character,
    % a float or a string, and must not be existentially quantified.
    %
:- func can_generate_dtd_for_types(element_pred::in(element_pred),
    list(pseudo_type_desc)::in,
    map(type_desc, unit)::in, map(string, type_desc)::in) =
    (dtd_generation_result::out) is det.

can_generate_dtd_for_types(_, [], _, _) = ok.
can_generate_dtd_for_types(MakeElement, [PseudoTypeDesc | PseudoTypeDescs],
        Done, ElementsSoFar) = Result :-
    ( if TypeDesc = ground_pseudo_type_desc_to_type_desc(PseudoTypeDesc) then
        ( if
            ( is_discriminated_union(TypeDesc, _)
            ; is_array(TypeDesc, _)
            ; is_primitive_type(TypeDesc, _)
            )
        then
            ( if map.contains(Done, TypeDesc) then
                Result = can_generate_dtd_for_types(MakeElement,
                    PseudoTypeDescs, Done, ElementsSoFar)
            else
                get_elements_and_args(MakeElement, TypeDesc, Elements, _, _,
                    ArgLists, _),
                list.filter(map.contains(ElementsSoFar), Elements,
                    DupElements),
                (
                    DupElements = [DupElement | _],
                    map.lookup(ElementsSoFar, DupElement, DupTypeDesc),
                    DupTypes = [TypeDesc, DupTypeDesc],
                    Result = duplicate_elements(DupElement, DupTypes)
                ;
                    DupElements = [],
                    list.merge_and_remove_dups(list.condense(ArgLists),
                        PseudoTypeDescs, NewPseudoTypeDescs),
                    list.duplicate(length(Elements), TypeDesc, TypeDescList),
                    map.det_insert_from_corresponding_lists(Elements,
                        TypeDescList, ElementsSoFar, NewElementsSoFar),
                    map.det_insert(TypeDesc, unit, Done, NewDone),
                    Result = can_generate_dtd_for_types(MakeElement,
                        NewPseudoTypeDescs, NewDone, NewElementsSoFar)
                )
            )
        else
            Result = unsupported_dtd_type(TypeDesc)
        )
    else
        Result = type_not_ground(PseudoTypeDesc)
    ).

%---------------------------------------------------------------------------%
%
% Predicates to write the DTD for a type.
%

write_dtd(Stream, Term, ElementMapping, DTDResult, !State) :-
    type_of(Term) = TypeDesc,
    write_dtd_from_type(Stream, TypeDesc, ElementMapping, DTDResult, !State).

write_dtd_from_type(Stream, TypeDesc, ElementMapping, DTDResult, !State) :-
    DTDResult = can_generate_dtd(ElementMapping, TypeDesc),
    (
        DTDResult = ok,
        get_element_pred(ElementMapping, MakeElement),
        ( if
            get_elements_and_args(MakeElement, TypeDesc,
                [RootElement], [_], [_], [PseudoArgTypes], _)
        then
            ArgTypes = list.map(det_ground_pseudo_type_desc_to_type_desc,
                PseudoArgTypes),
            put(Stream, "<!DOCTYPE ", !State),
            put(Stream, RootElement, !State),
            put(Stream, " [\n\n", !State),
            write_dtd_types(Stream, MakeElement, [TypeDesc | ArgTypes],
                map.init, !State),
            put(Stream, "\n]>", !State),
            DTDResult = ok
        else
            unexpected($pred, "not ok to generate DTD")
        )
    ;
        ( DTDResult = multiple_functors_for_root
        ; DTDResult = duplicate_elements(_, _)
        ; DTDResult = unsupported_dtd_type(_)
        ; DTDResult = type_not_ground(_)
        )
    ).

    % Write out the DTD entries for all the given types and add the written
    % types to AlreadyDone. Children types found along the way are added
    % to the first argument. We stop when all the types have had their DTD
    % entry written.
    %
:- pred write_dtd_types(Stream::in, element_pred::in(element_pred),
    list(type_desc)::in, map(type_desc, unit)::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_dtd_types(_, _, [], _, !State).
write_dtd_types(Stream, MakeElement, [TypeDesc | TypeDescs], AlreadyDone,
        !State) :-
    ( if map.search(AlreadyDone, TypeDesc, _) then
        write_dtd_types(Stream, MakeElement, TypeDescs, AlreadyDone, !State)
    else
        write_dtd_type_elements(Stream, MakeElement, TypeDesc, ChildArgTypes,
            !State),
        map.set(TypeDesc, unit, AlreadyDone, NewAlreadyDone),
        write_dtd_types(Stream, MakeElement, append(ChildArgTypes, TypeDescs),
            NewAlreadyDone, !State)
    ).

    % Write the IMPLIED, FIXED or REQUIRED part of the ATTLIST entry.
    %
:- pred write_attribute_source_kind(Stream::in, attr_source::in,
    maybe(string)::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_attribute_source_kind(Stream, functor, no, !State) :-
    put(Stream, "#IMPLIED", !State).
write_attribute_source_kind(Stream, functor, yes(Value), !State) :-
    put(Stream, "#FIXED """, !State),
    write_xml_escaped_string(Stream, Value, !State),
    put(Stream, """", !State).
write_attribute_source_kind(Stream, field_name, _, !State) :-
    put(Stream, "#IMPLIED", !State).
write_attribute_source_kind(Stream, type_name, no, !State) :-
    put(Stream, "#REQUIRED", !State).
write_attribute_source_kind(Stream, type_name, yes(Value), !State) :-
    put(Stream, "#FIXED """, !State),
    write_xml_escaped_string(Stream, Value, !State),
    put(Stream, """", !State).
write_attribute_source_kind(Stream, arity, no, !State) :-
    put(Stream, "#IMPLIED", !State).
write_attribute_source_kind(Stream, arity, yes(Value), !State) :-
    put(Stream, "#FIXED """, !State),
    write_xml_escaped_string(Stream, Value, !State),
    put(Stream, """", !State).

    % Write an ATTLIST entry for the given attribute.
    %
:- pred write_dtd_attlist(Stream::in, string::in, maybe(string)::in,
    maybe(int)::in, type_desc::in, attr_from_source::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_dtd_attlist(Stream, Element, MaybeFunctor, MaybeArity, TypeDesc,
        attr_from_source(Name, Source), !State) :-
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
        MaybeValue = yes(type_name(TypeDesc))
    ;
        Source = field_name,
        MaybeValue = no
    ),
    put(Stream, "<!ATTLIST ", !State),
    put(Stream, Element, !State),
    put(Stream, " ", !State),
    put(Stream, Name, !State),
    put(Stream, " CDATA ", !State),
    write_attribute_source_kind(Stream, Source, MaybeValue, !State),
    put(Stream, ">\n", !State).

:- pred write_dtd_attlists(Stream::in, string::in, list(attr_from_source)::in,
    maybe(string)::in, maybe(int)::in, type_desc::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_dtd_attlists(Stream, Element, AttrFromSources, MaybeFunctor, MaybeArity,
        TypeDesc, !State) :-
    list.foldl(
        write_dtd_attlist(Stream, Element, MaybeFunctor, MaybeArity, TypeDesc),
        AttrFromSources, !State).

    % Write DTD entries for all the functors for a type.
    %
:- pred write_dtd_type_elements(Stream::in, element_pred::in(element_pred),
    type_desc::in, list(type_desc)::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_dtd_type_elements(Stream, MakeElement, TypeDesc, ChildArgTypes,
        !State) :-
    get_elements_and_args(MakeElement, TypeDesc, Elements,
        MaybeFunctors, MaybeArities, ArgPseudoTypeLists, AttributeLists),
    ArgTypeLists = list.map(list.map(
        det_ground_pseudo_type_desc_to_type_desc), ArgPseudoTypeLists),
    list.condense(ArgTypeLists, ChildArgTypes),
    put(Stream, "<!-- Elements for functors of type """, !State),
    write_xml_escaped_string(Stream, type_name(TypeDesc), !State),
    put(Stream, """ -->\n\n", !State),
    write_dtd_entries(Stream, MakeElement, TypeDesc, Elements, MaybeFunctors,
        MaybeArities, ArgTypeLists, AttributeLists, !State).

    % Write all the given DTD entries.
    %
:- pred write_dtd_entries(Stream::in, element_pred::in(element_pred),
    type_desc::in, list(string)::in, list(maybe(string))::in,
    list(maybe(int))::in, list(list(type_desc))::in,
    list(list(attr_from_source))::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_dtd_entries(_, _, _, [], _, _, _, _, !State).
write_dtd_entries(Stream, MakeElement, TypeDesc, [Element | Elements],
        MaybeFunctorList, MaybeArityList, ArgTypeListList, AttributeListList,
        !State) :-
    MaybeFunctor = list.det_head(MaybeFunctorList),
    MaybeFunctors = list.det_tail(MaybeFunctorList),
    MaybeArity = list.det_head(MaybeArityList),
    MaybeArities = list.det_tail(MaybeArityList),
    ArgTypeList = list.det_head(ArgTypeListList),
    ArgTypeLists = list.det_tail(ArgTypeListList),
    AttributeList = list.det_head(AttributeListList),
    AttributeLists = list.det_tail(AttributeListList),

    put(Stream, "<!ELEMENT ", !State),
    put(Stream, Element, !State),
    put(Stream, " ", !State),
    ( if is_primitive_type(TypeDesc, _) then
        put(Stream, "(#PCDATA)", !State)
    else
        (
            ArgTypeList = [],
            put(Stream, "EMPTY", !State)
        ;
            ArgTypeList = [Head | Tail],
            (
                Tail = [_ | _],
                Braces = yes
            ;
                Tail = [],
                ( if num_functors(Head) > 1 then
                    Braces = no
                else
                    Braces = yes
                )
            ),

            % Put extra braces for arrays for the * at the end.
            ( if is_array(TypeDesc, _) then
                put(Stream, "(", !State)
            else
                true
            ),
            (
                Braces = yes,
                put(Stream, "(", !State)
            ;
                Braces = no
            ),
            AllowedFunctorsRegexs = list.map(
                dtd_allowed_functors_regex(MakeElement), ArgTypeList),
            AllowedFunctorsRegex =
                string.join_list(",", AllowedFunctorsRegexs),
            put(Stream, AllowedFunctorsRegex, !State),
            (
                Braces = yes,
                put(Stream, ")", !State)
            ;
                Braces = no
            ),
            ( if is_array(TypeDesc, _) then
                put(Stream, "*)", !State)
            else
                true
            )
        )
    ),
    put(Stream, ">\n", !State),

    write_dtd_attlists(Stream, Element, AttributeList, MaybeFunctor,
        MaybeArity, TypeDesc, !State),
    put(Stream, "\n", !State),
    write_dtd_entries(Stream, MakeElement, TypeDesc, Elements, MaybeFunctors,
        MaybeArities, ArgTypeLists, AttributeLists, !State).

    % Return the allowed functors for the type as a DTD rule regular
    % expression.
    %
:- func dtd_allowed_functors_regex(element_pred::in(element_pred),
    type_desc::in) = (string::out) is det.

dtd_allowed_functors_regex(MakeElement, TypeDesc) = Regex :-
    get_elements_and_args(MakeElement, TypeDesc, Elements, _, _, _, _),
    ElementsStr = string.join_list("|", Elements),
    ( if length(Elements) > 1 then
        Regex = "(" ++ ElementsStr ++ ")"
    else
        Regex = ElementsStr
    ).

%---------------------------------------------------------------------------%
:- end_module term_to_xml.
%---------------------------------------------------------------------------%
