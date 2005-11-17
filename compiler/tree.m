%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2001, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: conway, fjh, zs.
%
% This file provides a 'tree' data type.
% The code generater uses this to build a tree of instructions and
% then flatten them into a list.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module libs.tree.

%-----------------------------------------------------------------------------%

:- interface.

:- import_module bool.
:- import_module list.

:- type tree(T)
    --->    empty
    ;       node(T)
    ;       tree(tree(T), tree(T))
    ;       tree_list(list(tree(T))).

:- func tree.flatten(tree(T)) =  list(T).
:- pred tree.flatten(tree(T)::in, list(T)::out) is det.

:- func tree.is_empty(tree(T)) = bool.
:- pred tree.is_empty(tree(T)::in) is semidet.

:- func tree.tree_of_lists_is_empty(tree(list(T))) = bool.
:- pred tree.tree_of_lists_is_empty(tree(list(T))::in) is semidet.

:- pred tree.foldl(pred(T, A, A)::in(pred(in, in, out) is det), tree(T)::in,
    A::in, A::out) is det.

:- func tree.map(func(T) = U, tree(T)) = tree(U).
:- pred tree.map(pred(T, U)::in(pred(in, out) is det),
    tree(T)::in, tree(U)::out) is det.

:- pred tree.map_foldl(pred(T, U, A, A)::in(pred(in, out, in, out) is det),
    tree(T)::in, tree(U)::out, A::in, A::out) is det.

:- pred tree.map_foldl2(
    pred(T, U, A, A, B, B)::in(pred(in, out, in, out, in, out) is det),
    tree(T)::in, tree(U)::out, A::in, A::out, B::in, B::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

tree.flatten(T) = L :-
    tree.flatten(T, L).

tree.flatten(T, L) :-
    tree.flatten_2(T, [], L).

    % flatten_2(T, !Flat) is true iff !:Flat is the list that results from
    % traversing T left-to-right depth-first, and then appending !.Flat.
    %
:- pred tree.flatten_2(tree(T)::in, list(T)::in, list(T)::out) is det.

tree.flatten_2(empty, !Flat).
tree.flatten_2(node(Item), !Flat) :-
    !:Flat = [Item | !.Flat].
tree.flatten_2(tree(T1, T2), !Flat) :-
    tree.flatten_2(T2, !Flat),
    tree.flatten_2(T1, !Flat).
tree.flatten_2(tree_list(List), !Flat) :-
    tree.flatten_list(List, !Flat).

    % flatten_list(List, !Flat) is true iff !:Flat is the list that results
    % from traversing List left-to-right depth-first, and then appending
    % !.Flat.
    %
:- pred tree.flatten_list(list(tree(T))::in, list(T)::in, list(T)::out) is det.

tree.flatten_list([], !Flat).
tree.flatten_list([Head | Tail], !Flat) :-
    tree.flatten_list(Tail, !Flat),
    tree.flatten_2(Head, !Flat).

%-----------------------------------------------------------------------------%

tree.is_empty(T) :-
    tree.is_empty(T) = yes.

tree.is_empty(empty) = yes.
tree.is_empty(node(_)) = no.
tree.is_empty(tree(Left, Right)) =
    ( tree.is_empty(Left) = no ->
        no
    ;
        tree.is_empty(Right)
    ).
tree.is_empty(tree_list(List)) = tree.list_is_empty(List).

:- func tree.list_is_empty(list(tree(T))) = bool.

tree.list_is_empty([]) = yes.
tree.list_is_empty([Head | Tail]) =
    ( tree.is_empty(Head) = no ->
        no
    ;
        tree.list_is_empty(Tail)
    ).

%-----------------------------------------------------------------------------%

% Unfortunately, we can't factor out the common code between tree.is_empty
% and tree.tree_of_lists_is_empty because their signatures are different,
% and the signatures of their helpers must therefore be different too.

tree.tree_of_lists_is_empty(T) :-
    tree.tree_of_lists_is_empty(T) = yes.

tree.tree_of_lists_is_empty(empty) = yes.
tree.tree_of_lists_is_empty(node([])) = yes.
tree.tree_of_lists_is_empty(node([_ | _])) = no.
tree.tree_of_lists_is_empty(tree(Left, Right)) =
    ( tree.tree_of_lists_is_empty(Left) = no ->
        no
    ;
        tree.is_empty(Right)
    ).
tree.tree_of_lists_is_empty(tree_list(List)) =
    tree.list_tree_of_lists_is_empty(List).

:- func tree.list_tree_of_lists_is_empty(list(tree(list(T)))) = bool.

tree.list_tree_of_lists_is_empty([]) = yes.
tree.list_tree_of_lists_is_empty([Head | Tail]) =
    ( tree.tree_of_lists_is_empty(Head) = no ->
        no
    ;
        tree.list_tree_of_lists_is_empty(Tail)
    ).

%-----------------------------------------------------------------------------%

tree.foldl(_P, empty, !A).
tree.foldl(P, node(Node), !A) :-
    P(Node, !A).
tree.foldl(P, tree(Left, Right), !A) :-
    tree.foldl(P, Left, !A),
    tree.foldl(P, Right, !A).
tree.foldl(P, tree_list(List), !A) :-
    list.foldl(tree.foldl(P), List, !A).

tree.map(_F, empty) = empty.
tree.map(F, node(T)) = node(F(T)).
tree.map(F, tree(L, R)) = tree(tree.map(F, L), tree.map(F, R)).
tree.map(F, tree_list(L)) = tree_list(list.map(tree.map(F), L)).

tree.map(_P, empty, empty).
tree.map(P, node(Node0), node(Node)) :-
    P(Node0, Node).
tree.map(P, tree(Left0, Right0), tree(Left, Right)) :-
    tree.map(P, Left0, Left),
    tree.map(P, Right0, Right).
tree.map(P, tree_list(List0), tree_list(List)) :-
    list.map(tree.map(P), List0, List).

tree.map_foldl(_P, empty, empty, !A).
tree.map_foldl(P, node(Node0), node(Node), !A) :-
    P(Node0, Node, !A).
tree.map_foldl(P, tree(Left0, Right0), tree(Left, Right), !A) :-
    tree.map_foldl(P, Left0, Left, !A),
    tree.map_foldl(P, Right0, Right, !A).
tree.map_foldl(P, tree_list(List0), tree_list(List), !A) :-
    list.map_foldl(tree.map_foldl(P), List0, List, !A).

tree.map_foldl2(_P, empty, empty, !A, !B).
tree.map_foldl2(P, node(Node0), node(Node), !A, !B) :-
    P(Node0, Node, !A, !B).
tree.map_foldl2(P, tree(Left0, Right0), tree(Left, Right), !A, !B) :-
    tree.map_foldl2(P, Left0, Left, !A, !B),
    tree.map_foldl2(P, Right0, Right, !A, !B).
tree.map_foldl2(P, tree_list(List0), tree_list(List), !A, !B) :-
    list.map_foldl2(tree.map_foldl2(P), List0, List, !A, !B).

%-----------------------------------------------------------------------------%
:- end_module tree.
%-----------------------------------------------------------------------------%
