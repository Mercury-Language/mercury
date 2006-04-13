%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%

% File: set.m.
% Authors: zs.
% Stability: high.

% This file provides an interface to the 'relation' ADT that is conducive to
% the use of state variable notation. The predicates here do the same thing as
% their counterparts in the relation module; the only difference is the order
% of the arguments.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module svrelation.
:- interface.

:- import_module assoc_list.
:- import_module relation.

    % svrelation.add_element adds an element to the domain of a
    % relation.  Return the old relation_key if one already exists.
    %
:- pred svrelation.add_element(T::in, relation_key::out,
    relation(T)::in, relation(T)::out) is det.

    % svrelation.add adds an element to the relation.
    %
:- pred svrelation.add(relation_key::in, relation_key::in,
    relation(T)::in, relation(T)::out) is det.

    % svrelation.add_values adds an pair of values to the relation's
    % domain and adds an element to the relation.
    %
    % svrelation.add_values(X, Y, !R) :-
    %    svrelation.add_element(X, XKey, !R),
    %    svrelation.add_element(Y, YKey, !R),
    %    svrelation.add(XKey, YKey, !R).
    %
:- pred svrelation.add_values(T::in, T::in, relation(T)::in, relation(T)::out)
    is det.

    % svrelation.add_assoc_list adds a list of elements to a
    % relation.
    %
:- pred svrelation.add_assoc_list(assoc_list(relation_key, relation_key)::in,
    relation(T)::in, relation(T)::out) is det.

    % svrelation.remove removes an element from the relation.
    %
:- pred svrelation.remove(relation_key::in, relation_key::in,
    relation(T)::in, relation(T)::out) is det.

    % svrelation.remove_assoc_list removes a list of elements
    % from a relation.
    %
:- pred svrelation.remove_assoc_list(
    assoc_list(relation_key, relation_key)::in,
    relation(T)::in, relation(T)::out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

svrelation.add_element(Item, Key, !Rel) :-
    relation.add_element(!.Rel, Item, Key, !:Rel).

svrelation.add(Key1, Key2, !Rel) :-
    relation.add(!.Rel, Key1, Key2, !:Rel).

svrelation.add_values(Item1, Item2, !Rel) :-
    relation.add_values(!.Rel, Item1, Item2, !:Rel).

svrelation.add_assoc_list(AssocList, !Rel) :-
    relation.add_assoc_list(!.Rel, AssocList, !:Rel).

svrelation.remove(Key1, Key2, !Rel) :-
    relation.remove(!.Rel, Key1, Key2, !:Rel).

svrelation.remove_assoc_list(AssocList, !Rel) :-
    relation.remove_assoc_list(!.Rel, AssocList, !:Rel).
