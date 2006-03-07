%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: sveqvclass.m.
% Author: zs.
% Stability: high.
%
% This file provides an interface to the 'eqvclass' ADT that is conducive
% to the use of state variable notation. The predicates here do the same thing
% as their counterparts in the eqvclass module; the only difference is the
% order of the arguments.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module sveqvclass.

:- interface.

:- import_module eqvclass.

    % Make an element known to the equivalence class.
    % The element may already be known to the class;
    % if it isn't, it is created without any equivalence relationships.
    %
:- pred sveqvclass.ensure_element(T::in, eqvclass(T)::in, eqvclass(T)::out)
    is det.

    % Make an element known to the equivalence class.
    % The element must not already be known to the class;
    % it is created without any equivalence relationships.
    %
:- pred sveqvclass.new_element(T::in, eqvclass(T)::in, eqvclass(T)::out)
    is det.

    % Make two elements of the equivalence class equivalent.
    % It is ok if they already are.
    %
:- pred sveqvclass.ensure_equivalence(T::in, T::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

    % Make two elements of the equivalence class equivalent.
    % It is an error if they are already equivalent.
    %
:- pred sveqvclass.new_equivalence(T::in, T::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

    % Remove the given element and all other elements equivalent to it
    % from the given equivalence class.
    %
:- pred sveqvclass.remove_equivalent_elements(T::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

sveqvclass.ensure_element(Element, EqvClass0, EqvClass) :-
    eqvclass.ensure_element(EqvClass0, Element, EqvClass).

sveqvclass.new_element(Element, EqvClass0, EqvClass) :-
    eqvclass.new_element(EqvClass0, Element, EqvClass).

sveqvclass.ensure_equivalence(Element1, Element2, EqvClass0, EqvClass) :-
    eqvclass.ensure_equivalence(EqvClass0, Element1, Element2, EqvClass).

sveqvclass.new_equivalence(Element1, Element2, EqvClass0, EqvClass) :-
    eqvclass.new_equivalence(EqvClass0, Element1, Element2, EqvClass).

sveqvclass.remove_equivalent_elements(X, EqvClass0, EqvClass) :-
    EqvClass = eqvclass.remove_equivalent_elements(EqvClass0, X).
