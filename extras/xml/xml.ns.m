%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2001, 2004-2006, 2011 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: xml.ns.m
% Main author: conway@cs.mu.oz.au, inch@students.cs.mu.oz.au
%
% This module provides predicates to turn an XML document into a namespace
% aware XML document. A normal XML document containing multiple elements
% and attributes may encounter problems of recognition and collision, i.e.
% same element type or attribute name may have different scope. A namespace
% aware XML document solves this problem by using URI references to identify
% elements and attributes.
%
% Reference:
% <http://www.w3.org/TR-REC-xml-names>
%
%---------------------------------------------------------------------------%

:- module xml.ns.
:- interface.

:- import_module xml.doc.

:- import_module array.
:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

:- type ns_document
    --->    ns_doc(
                % Array index pointing to prestuff.
                prestuff            :: list(ref(ns_content)),

                % Root of the document tree.
                root                :: ref(ns_content),

                % Array index pointing to poststuff.
                poststuff           :: list(ref(ns_content)),

                % Array storing all document content.
                content             :: array(ns_content)
            ).

:- type ns_content
    --->    ns_element(ns_element)  % element or attribute
    ;       pi(string, string)      % processing instruction
    ;       comment(string)         % comment
    ;       data(string).           % data

:- type ns_element
    --->    ns_element(
                % Qualified name.
                elt_name            :: q_name,

                % List of attributes.
                elt_attrs           :: list(ns_attribute),

                % List of index pointing to children in the document tree.
                elt_content         :: list(ref(content)),

                % List of (Prefix - URI).
                elt_namespaces      :: ns_list
            ).

:- type ns_attribute
    --->    ns_attribute(
                attr_name           :: q_name,  % qualified name
                attr_value          :: string   % attribute value
            ).

:- type q_name
    --->    q_name(
                localName           :: string,  % local name without prefix
                ns_uri              :: ns_uri   % URI reference
            ).

:- type ns_list == list(pair(string, ns_uri)).

:- type ns_uri == string.

    % ns_translate() takes in a normal XML document and returns a namespace
    % aware XML document.
    %
:- pred ns_translate(xml.doc.document::in, ns_document::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type namespaces == map(string, ns_uri).

ns_translate(Doc, NsDoc) :-
    traverse(Doc, [], NsDocContent),
    NsDoc = ns_doc(Doc ^ prestuff, Doc ^ root, Doc ^ poststuff,
        array(NsDocContent)).

    % traverse takes in a normal XML document and an accumulator, creates an
    % empty tree, traverses and translates the document tree, and gives back a
    % namespace aware document.
    %
:- pred traverse(document::in, list(ns_content)::in, list(ns_content)::out)
    is det.

traverse(Doc, !Acc) :-
    traverse(Doc ^ content, map.init, "", Doc ^ root, !Acc).

    % Carries out the actual traverse and transformation. If the content is an
    % element, change it to a namespace aware element and visit its siblings,
    % otherwise, convert the type.
    %
:- pred traverse(array(content)::in, namespaces::in, string::in,
    ref(content)::in, list(ns_content)::in, list(ns_content)::out) is det.

traverse(ContentArray, Namespaces0, Default0, ContentRef, !Acc) :-
    array.lookup(ContentArray, ContentRef, Content),
    (
        Content = element(Elem),
        % Examine the attributes to find any default namespaces.
        ( if default_namespace(Elem ^ elt_attrs, Default1, Attrs0) then
            Default = Default1,
            Attrs1 = Attrs0
        else
            Default = Default0,
            Attrs1 = Elem ^ elt_attrs
        ),

        % Extract any namespace declaration and insert into tree.
        extract_namespace_decls(Attrs1, NSList, _Attrs2),
        list.foldl(
            ( pred((Pref - URI)::in, NSs1::in,
                NSs2::out) is det :-
                map.set(Pref, URI, NSs1, NSs2)
            ), NSList, Namespaces0, Namespaces),

        % Change element and attributes to namespace aware.
        namespaceize_elt_name(Namespaces, Default, Elem ^ elt_name, Name),
        list.map(
            ( pred(Attr0::in, Attr::out) is det :-
                Attr0 = attribute(AttrName0, Value),
                namespaceize_elt_name(Namespaces, Default,
                    AttrName0, AttrName),
                Attr = ns_attribute(AttrName, Value)
            ), Elem ^ elt_attrs, Attrs),

        % Visit its siblings.
        Kids = Elem ^ elt_content,
        list.reverse(Kids, Kids0),
        NsElem = ns_element(
            ns_element(Name, Attrs, Elem ^ elt_content, NSList)),
        !:Acc = [NsElem | !.Acc],
        xml.ns.foldl(traverse, ContentArray, Namespaces, Default, Kids0, !Acc)
    ;
        Content = comment(_),
        !:Acc = [convert_type(Content) | !.Acc]
    ;
        Content = data(_),
        !:Acc = [convert_type(Content) | !.Acc]
    ;
        Content = pi(_,_),
        !:Acc = [convert_type(Content) | !.Acc]
    ).

    % Searches for any default namespaces.
    %
:- pred default_namespace(list(attribute)::in, string::out,
    list(attribute)::out) is semidet.

default_namespace([], _, _) :- fail.
default_namespace([Attr | Attrs], Default, NewAttrs) :-
    ( if
        % If a default namespace is found, return the namespace
        % and the list of attributes without the default namespace
        is_xmlns(Attr ^ attr_name)
    then
        Default = Attr ^ attr_value,
        NewAttrs = Attrs
    else
        % Otherwise keep searching
        Default = Default0,
        NewAttrs = NewAttrs0,
        default_namespace(Attrs, Default0, NewAttrs0)
    ).


    % Searches the list of attributes and extract any namespace declarations.
    %
:- pred extract_namespace_decls(list(attribute)::in, ns_list::out,
    list(attribute)::out) is det.

extract_namespace_decls([], [], []).
extract_namespace_decls([Attr | Attrs], NSList, NewAttrs) :-
    split_on_colon(Attr ^ attr_name, Prefix, Suffix),
    ( if
        % for case like < book xmlns:isbn="someURI" >
        % Prefix = xmlns
        % Suffix = isbn
        is_xmlns(Prefix)
    then
        NSList = [(Suffix - Attr ^ attr_value) | NSList0],
        NewAttrs = NewAttrs0
    else
        NSList = NSList0,
        NewAttrs = [Attr | NewAttrs0]
    ),
    extract_namespace_decls(Attrs, NSList0, NewAttrs0).

    % Change Name to QName by matching Name with the Namespaces list.
    % If fails, applies default namespace.
    %
:- pred namespaceize_elt_name(namespaces::in, string::in, string::in,
    q_name::out) is det.

namespaceize_elt_name(Namespaces, Default, Name, QName) :-
    split_on_colon(Name, Prefix, Suffix),
    ( if
        % for case when element name = prefix:suffix
        map.search(Namespaces, Prefix, URI)
    then
        QName = q_name(Suffix, URI)
    else if
        % for case when attribute name = xmlns:suffix
        is_xmlns(Prefix),
        map.search(Namespaces, Suffix, URI)
    then
        QName = q_name(Suffix, URI)
    else
        % for case when element name has no prefix
        QName = q_name(Suffix, Default)
    ).

    % Split a name into prefix and suffix.
    %
:- pred split_on_colon(string::in, string::out, string::out) is det.

split_on_colon(Name, Prefix, Suffix) :-
    ( if string.sub_string_search(Name, ":", Index) then
        string.length(Name, Length),
        string.right(Name, Length - (Index + 1), Suffix),
        string.left(Name, Index, Prefix)
    else
        Suffix = Name,
        Prefix = ""
    ).

    % According to the namespaces specification `Namespaces in XML'
    % <http://www.w3.org/TR-REC-xml-names>, a namespace is declared
    % as an attribute with `xmlns' as a prefix.
    %
:- pred is_xmlns(string::in) is semidet.

is_xmlns("xmlns").

:- func convert_type(content) = ns_content.

convert_type(comment(S)) = comment(S).
convert_type(data(S)) = data(S).
convert_type(pi(S,S0)) = pi(S,S0).
convert_type(element(_)) = _ :-
    require.error("Convert element failed.").

    % Traverse children in the document tree.
    %
:- pred foldl(
    pred(array(content), namespaces, string, ref(content), T, T)::
        in(pred(in, in, in, in, in, out) is det),
    array(content)::in, namespaces::in, string::in, list(ref(content))::in,
    T::in, T::out) is det.

foldl(_Pred, _, _, _, [], !Acc).
foldl(Pred, Content, NameSpaces, Default, [Ref | Refs], !Acc) :-
    Pred(Content, NameSpaces, Default, Ref, !Acc),
    foldl(Pred, Content, NameSpaces, Default, Refs, !Acc).
