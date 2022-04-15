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
% and attributes may encounter problems of recognition and collision, ie.
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
:- import_module string.

%---------------------------------------------------------------------------%

:- type nsDocument
    --->    nsDoc(
                prestuff :: list(ref(nsContent)),
                % array index pointing to prestuff

                root :: ref(nsContent),
                % root of the document tree

                poststuff :: list(ref(nsContent)),
                % array index pointing to poststuff

                content :: array(nsContent)
                % array storing all document content
            ).

:- type nsContent
    --->    nsElement(nsElement)    % element or attribute
    ;       pi(string, string)      % processing instruction
    ;       comment(string)         % comment
    ;       data(string).           % data

:- type nsElement
    --->    nsElement(
                eName :: qName,
                % qualified name

                eAttrs :: list(nsAttribute),
                % list of attributes

                eContent    :: list(ref(content)),
                % list of index pointing to children in the document tree

                eNamespaces :: nsList
                % list of (Prefix - URI)
            ).

:- type nsAttribute
    --->    nsAttribute(
                aName  :: qName,   % qualified name
                aValue :: string   % attribute value
            ).

:- type qName
    --->    qName(
                localName :: string,  % local name without prefix
                nsURI     :: nsURI    % URI reference
            ).

:- type nsList == list(pair(string, nsURI)).

:- type nsURI == string.

    % nsTranslate() takes in a normal XML document and returns a namespace
    % aware XML document.
    %
:- pred nsTranslate(xml.doc.document::in, nsDocument::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module xml.parse.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module require.

%---------------------------------------------------------------------------%

:- type namespaces == map(string, nsURI).

nsTranslate(Doc, NsDoc) :-
    traverse(Doc, [], NsDocContent),
    NsDoc = nsDoc(Doc ^ prestuff, Doc ^ root, Doc ^ poststuff,
              array(NsDocContent)).

    % traverse takes in a normal XML document and an accumulator, creates an
    % empty tree, traverses and translates the document tree, and gives back a
    % namespace aware document.
    %
:- pred traverse(document::in, list(nsContent)::in, list(nsContent)::out)
    is det.

traverse(Doc, !Acc) :-
    traverse(Doc ^ content, map.init, "", Doc ^ root, !Acc).

    % Carries out the actual traverse and transformation. If the content is an
    % element, change it to a namespace aware element and visit its siblings,
    % otherwise, convert the type.
    %
:- pred traverse(array(content)::in, namespaces::in, string::in,
    ref(content)::in, list(nsContent)::in, list(nsContent)::out) is det.

traverse(ContentArray, Namespaces0, Default0, ContentRef, !Acc) :-
    lookup(ContentArray, ContentRef, Content),
    (
        Content = element(Elem),
        % Examine the attributes to find any default namespaces.
        ( if
            defaultNamespace(Elem^eAttrs, Default1, Attrs0)
        then
            Default = Default1,
            Attrs1 = Attrs0
        else
            Default = Default0,
            Attrs1 = Elem ^ eAttrs
        ),

        % Extract any namespace declaration and insert into tree.
        extractNamespaceDecls(Attrs1, NSList, _Attrs2),
        list.foldl((pred((Pref - URI)::in, NSs1::in,
            NSs2::out) is det :-
            map.set(Pref, URI, NSs1, NSs2)
        ), NSList, Namespaces0, Namespaces),

        % Change element and attributes to namespace aware.
        namespaceizeName(Namespaces, Default, Elem ^ eName, Name),
        map((pred(Attr0::in, Attr::out) is det :-
            Attr0 = attribute(AttrName0, Value),
            namespaceizeName(Namespaces, Default,
                    AttrName0, AttrName),
                Attr = nsAttribute(AttrName, Value)
        ), Elem ^ eAttrs, Attrs),

        % Visit its siblings.
        Kids = Elem ^ eContent,
        list.reverse(Kids, Kids0),
        NsElem = nsElement(nsElement(Name, Attrs, Elem ^ eContent, NSList)),
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
:- pred defaultNamespace(list(attribute)::in, string::out,
    list(attribute)::out) is semidet.

defaultNamespace([], _, _) :- fail.
defaultNamespace([Attr | Attrs], Default, NewAttrs) :-
    ( if
        % If a default namespace is found, return the namespace
        % and the list of attributes without the default namespace
        is_xmlns(Attr ^ aName)
    then
        Default = Attr ^ aValue,
        NewAttrs = Attrs
    else
        % Otherwise keep searching
        Default = Default0,
        NewAttrs = NewAttrs0,
        defaultNamespace(Attrs, Default0, NewAttrs0)
    ).


    % Searches the list of attributes and extract any namespace declarations.
    %
:- pred extractNamespaceDecls(list(attribute)::in, nsList::out,
    list(attribute)::out) is det.

extractNamespaceDecls([], [], []).
extractNamespaceDecls([Attr | Attrs], NSList, NewAttrs) :-
    split_on_colon(Attr^aName, Prefix, Suffix),
    ( if
        % for case like < book xmlns:isbn="someURI" >
        % Prefix = xmlns
        % Suffix = isbn
        is_xmlns(Prefix)
    then
        NSList = [(Suffix - Attr ^ aValue) | NSList0],
        NewAttrs = NewAttrs0
    else
        NSList = NSList0,
        NewAttrs = [Attr | NewAttrs0]
    ),
    extractNamespaceDecls(Attrs, NSList0, NewAttrs0).

    % Change Name to QName by matching Name with the Namespaces list.
    % If fails, applies default namespace.
:- pred namespaceizeName(namespaces::in, string::in, string::in, qName::out)
    is det.

namespaceizeName(Namespaces, Default, Name, QName) :-
    split_on_colon(Name, Prefix, Suffix),
    ( if
        % for case when element name = prefix:suffix
        map.search(Namespaces, Prefix, URI)
    then
        QName = qName(Suffix, URI)
    else if
        % for case when attribute name = xmlns:suffix
        is_xmlns(Prefix),
        map.search(Namespaces, Suffix, URI)
    then
        QName = qName(Suffix, URI)
    else
        % for case when element name has no prefix
        QName = qName(Suffix, Default)
    ).

    % Split a name into prefix and suffix.
    %
:- pred split_on_colon(string::in, string::out, string::out) is det.

split_on_colon(Name, Prefix, Suffix) :-
    ( if
        string.sub_string_search(Name, ":", Index)
    then
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

:- func convert_type(content) = nsContent.

convert_type(comment(S)) = comment(S).
convert_type(data(S)) = data(S).
convert_type(pi(S,S0)) = pi(S,S0).
convert_type(element(_)) = _ :- require.error("Convert element failed.").

    % Traverse children in the document tree.
:- pred xml.ns.foldl(pred(array(content), namespaces, string, ref(content),
    T, T), array(content), namespaces, string, list(ref(content)), T, T).
:- mode xml.ns.foldl(pred(in, in, in, in, in, out) is det, in, in, in, in,
    in, out) is det.

xml.ns.foldl(_Pred, _, _, _, [], !Acc).
xml.ns.foldl(Pred, Content, NameSpaces, Default, [Ref | Refs], !Acc) :-
    Pred(Content, NameSpaces, Default, Ref, !Acc),
    foldl(Pred, Content, NameSpaces, Default, Refs, !Acc).
