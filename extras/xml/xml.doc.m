%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2011 The University of Melbourne.
% Copyright (C) 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%

:- module xml.doc.
:- interface.

:- import_module array.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type document
    --->    doc(
                prestuff        :: list(ref(content)),
                root            :: ref(content),
                poststuff       :: list(ref(content)),
                content         :: array(content)
            ).

:- type content
    --->    element(element)
    ;       pi(string, string)
    ;       comment(string)
    ;       data(string).

:- type content_store
    --->    content(
                e_next          :: ref(content),
                e_map           :: map(ref(content), content)
            ).

:- type element
    --->    element(
                elt_name        :: string,
                elt_attrs       :: list(attribute),
                elt_content     :: list(ref(content))
            ).

:- type attribute
    --->    attribute(
                attr_name       :: string,
                attr_value      :: string
            ).

:- type ref(T) == int.

:- func ref(content_store, ref(content)) = content.

:- pred add(content::in, ref(content)::out,
    content_store::in, content_store::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

ref(Elems, Ref) = Elem :-
    map.lookup(Elems ^ e_map, Ref, Elem).

add(Elem, Ref, !Elems) :-
    Ref = !.Elems ^ e_next,
    !Elems ^ e_next := Ref + 1,

    Map0 = !.Elems ^ e_map,
    map.set(Ref, Elem, Map0, Map),
    !Elems ^ e_map := Map.
