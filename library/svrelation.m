%---------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
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

:- module svrelation.

:- interface.

:- import_module relation, assoc_list.

	% svrelation__add_element adds an element to the domain of a
	% relation.  Return the old relation_key if one already exists.
	%
:- pred svrelation__add_element(T::in, relation_key::out,
	relation(T)::in, relation(T)::out) is det.

	% svrelation__add adds an element to the relation.
	%
:- pred svrelation__add(relation_key::in, relation_key::in,
	relation(T)::in, relation(T)::out) is det.

	% svrelation__add_values adds an pair of values to the relation's
	% domain and adds an element to the relation.
	%
	% svrelation__add_values(X, Y, !R) :-
	%	 svrelation__add_element(X, XKey, !R),
	%	 svrelation__add_element(Y, YKey, !R),
	%	 svrelation__add(XKey, YKey, !R).
	%
:- pred svrelation__add_values(T::in, T::in, relation(T)::in, relation(T)::out)
	is det.

	% svrelation__add_assoc_list adds a list of elements to a
	% relation.
	%
:- pred svrelation__add_assoc_list(assoc_list(relation_key, relation_key)::in,
	relation(T)::in, relation(T)::out) is det.

	% svrelation__remove removes an element from the relation.
	%
:- pred svrelation__remove(relation_key::in, relation_key::in,
	relation(T)::in, relation(T)::out) is det.

	% svrelation__remove_assoc_list removes a list of elements
	% from a relation.
	%
:- pred svrelation__remove_assoc_list(
	assoc_list(relation_key, relation_key)::in,
	relation(T)::in, relation(T)::out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

svrelation__add_element(Item, Key, !Rel) :-
	relation__add_element(!.Rel, Item, Key, !:Rel).

svrelation__add(Key1, Key2, !Rel) :-
	relation__add(!.Rel, Key1, Key2, !:Rel).

svrelation__add_values(Item1, Item2, !Rel) :-
	relation__add_values(!.Rel, Item1, Item2, !:Rel).

svrelation__add_assoc_list(AssocList, !Rel) :-
	relation__add_assoc_list(!.Rel, AssocList, !:Rel).

svrelation__remove(Key1, Key2, !Rel) :-
	relation__remove(!.Rel, Key1, Key2, !:Rel).

svrelation__remove_assoc_list(AssocList, !Rel) :-
	relation__remove_assoc_list(!.Rel, AssocList, !:Rel).
