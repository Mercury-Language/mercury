%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2010 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Some test cases for extras/trailed_update/var.m.
%
% author: fjh
%
%-----------------------------------------------------------------------------%

:- module var_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module unsafe.
:- import_module var.

:- import_module int.
:- import_module require.
:- import_module std_util.

:- pragma require_feature_set([trailing]).

%-----------------------------------------------------------------------------%

main(!IO) :-
    print("test_delaying_1: ", !IO),
    ( test_delaying_1 ->
        print("yes\n", !IO)
    ;
        print("no\n", !IO)
    ),

    print("test_delaying_2: ", !IO),
    ( test_delaying_2 ->
        print("yes\n", !IO)
    ;
        print("no\n", !IO)
    ),

    ( create_solvable_delayed_goal(X, Y) ->
        print("X = ", !IO), output_var(X, !IO), nl(!IO),
        print("Y = ", !IO), output_var(Y, !IO), nl(!IO)
    ;
        print("Oops.\n", !IO)
    ),

    print("test_delaying_1: ", !IO),
    ( test_delaying_1 ->
        print("yes\n", !IO)
    ;
        print("no\n", !IO)
    ),

    print("test_delaying_2: ", !IO),
    ( test_delaying_2 ->
        print("yes\n", !IO)
    ;
        print("no\n", !IO)
    ),

    print("test_delaying_3: ", !IO),
    ( create_solvable_delayed_goal(X3, Y3) ->
        print("yes: X = ", !IO), output_var(X3, !IO),
        print(", Y = ", !IO), output_var(Y3, !IO), nl(!IO)
    ;
        print("no\n", !IO)
    ),

    print("test_delaying_4: ", !IO),
    ( create_unsolvable_delayed_goal(X4) ->
        print("yes: X = ", !IO), output_var(X4, !IO), nl(!IO)
    ;
        print("no\n", !IO)
    ),

    print("test_ground:\n", !IO),
    Z = var(42),
    print("Z = ", !IO), output_var(Z, !IO), nl(!IO),
    ( var.init(Z2), var.init(Z3), Z2 = Z3, Z3 = Z ->
        print("Z2 = ", !IO), output_var(Z2, !IO), nl(!IO)
    ;
        print("oops\n", !IO)
    ),
    print("test_alias_twice:\n", !IO),
    ( A == B, A = B ->
        print("A = ", !IO), output_var(A, !IO), nl(!IO),
        print("B = ", !IO), output_var(B, !IO), nl(!IO)
    ;
        print("oops\n", !IO)
    ),
    print("test_dup_call_bug:\n", !IO), 
    ( var.init(A1), var.init(A2), A1 = var(42) ->
        print("A1 = ", !IO), output_var(A1, !IO), nl(!IO),
        print("A2 = ", !IO), output_var(A2, !IO), nl(!IO)
    ;
        print("oops\n", !IO)
    ),
    print("Done.\n", !IO).

:- mode output_var(in(any), di, uo) is cc_multi.

output_var(Var, !IO) :-
    dump_var(Var, !IO),
    var.is_ground(Var, MaybeVal),
    print(" [ground: ", !IO), write(MaybeVal, !IO), print("]", !IO).

test_delaying_1 :-
    create_solvable_delayed_goal(X, Y),
    wake_and_fail(X, Y).

test_delaying_2 :-
    create_solvable_delayed_goal(X, Y),
    wake_and_succeed(X, Y).

create_solvable_delayed_goal(X, Y) :-
    % debug_freeze("add_one",
    freeze(X, (pred(XVal::in, YVal::out) is det :- YVal = XVal + 1), Y).

wake_and_succeed(var(0), var(1)).  % 1 = 0 + 1 succeeds
%   unsafe_perform_io(print("Y = ")),
%   unsafe_perform_io(output_var(Y)),
%   unsafe_perform_io(nl).

wake_and_fail(var(0), var(42)). % 42 = 0 + 1 fails.

create_unsolvable_delayed_goal(X) :-
    init(X),
    % debug_freeze("always_fail",
    freeze(X, (pred(_::in) is semidet :- fail)).

% :- mode test_modes_1.  (not yet supported)
test_modes_1 :-
    % test auto-initialize (implied free -> any)
    p(_),
    p2(_).

:- mode test_modes_2(in(any)).
test_modes_2(X) :-
    % test implied mode
    q(X).

:- mode p(ia) is semidet.
p(_) :- semidet_succeed.

:- mode p2(any >> ground) is failure.

p2(_) :- fail.

:- mode q(oa) is det.

q(X) :- init(X).

%-----------------------------------------------------------------------------%
:- end_module var_test.
%-----------------------------------------------------------------------------%
