%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constr_inst_syntax.
:- interface.

:- typeclass foo(T) where [].
:- typeclass bar(T) where [].
:- instance bar(int).

:- type list(T)
    --->    []
    ;       [T | list(T)].
:- inst list_skel(I)
    --->    []
    ;       [I | list_skel(I)].

    % The example from the reference manual.
    %
:- pred append(list(T), list(T), list(T)).
:- (mode append(in(list_skel(I)), in(list_skel(I)),
    out(list_skel(I))) is det) <= I =< ground.

    % Check that inst constraints can be combined with
    % typeclass constraints in predmode decls.
    %
:- pred predmode_1(T::in(I), T::in(I), T::out(I))
    is det <= (I =< any, foo(T)).

    % A more complicated example using inst and type class
    % constraints.
    %
:- some [U]
   (impure pred predmode_2(T::in(I), U::out, T::out(I)) is det => bar(U))
   <= (foo(T), I =< any).

    % Multiple inst constraints.
    %
:- pred multi_inst_constrs(T, T, U, U).
:- (mode multi_inst_constrs(in(I), out(I), in(J), out(J)) is det)
    <= (I =< ground, J =< any).

    % Inst constraints and functions.
    %
:- func id(T) = T.
:- (mode id(in(I)) = out(I) is det) <= I =< ground.

:- func id2(T::in(I), U::in(J)) = (T::out(I)) is det
    <= (foo(T), I =< ground, J =< any).

:- implementation.

:- instance bar(int) where [].

append([], Zs, Zs).
append([ X | Xs ], Ys, [ X | Zs ]) :-
    append(Xs, Ys, Zs).

predmode_1(X, _, X).

multi_inst_constrs(A, A, B, B).

predmode_2(X, 3, X) :-
    impure impure_true.

id(X) = X.

id2(X, _) = X.
