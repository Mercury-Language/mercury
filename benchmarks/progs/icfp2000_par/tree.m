%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: conway, fjh.
%
% This file provides a 'tree' data type.
% The code generater uses this to build a tree of instructions and
% then flatten them into a list.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module tree.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module list.
:- type tree(T)
    --->	empty
    ;	    node(T)
    ;	    tree(tree(T), tree(T)).

:- pred tree.flatten(tree(T)::in, list(T)::out) is det.

:- pred tree.is_empty(tree(T)::in) is semidet.

:- pred tree.tree_of_lists_is_empty(tree(list(T))::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

tree.flatten(T, L) :-
	tree.flatten_2(T, [], L).

	% flatten_2(T, L0, L) is true iff L is the list that results from
	% traversing T left-to-right depth-first, and then appending L0.
    %
:- pred tree.flatten_2(tree(T)::in, list(T)::in, list(T)::out) is det.

tree.flatten_2(empty, L, L).
tree.flatten_2(node(T), L, [T | L]).
tree.flatten_2(tree(T1, T2), L0, L) :-
	tree.flatten_2(T2, L0, L1),
	tree.flatten_2(T1, L1, L).

%-----------------------------------------------------------------------------%

tree.is_empty(empty).
tree.is_empty(tree(L, R)) :-
	tree.is_empty(L),
	tree.is_empty(R).

%-----------------------------------------------------------------------------%

tree.tree_of_lists_is_empty(empty).
tree.tree_of_lists_is_empty(node([])).
tree.tree_of_lists_is_empty(tree(L, R)) :-
	tree.tree_of_lists_is_empty(L),
	tree.tree_of_lists_is_empty(R).

%-----------------------------------------------------------------------------%
