:- module det_errors_deep.

:- interface.

:- import_module std_util.

:- type t ---> a ; b ; c ; d.
:- type tree ---> leaf ; node(tree, pair(t, int), tree).

:- pred p1(tree::in, t::in, int::out) is det.
:- pred p2(t::in, tree::in, int::out) is det.
:- pred p3(t::in, tree::in, int::out) is det.
:- pred p4(tree::in, t::in, int::out) is det.

:- implementation.
:- import_module int.

p1(leaf, _, 1).
p1(node(_L, X - Y, _R), X, Y).

p2(_, leaf, 1).
p2(X, node(_L, X - Y, _R), Y).

p3(X, node(_L, X - Y, _R), Y).

p4(leaf, _, 1).
p4(node(_L, X - Y, leaf), X, Y).
p4(node(_L, X - Y, node(_RL, X - _, _RR)), X, Y).
