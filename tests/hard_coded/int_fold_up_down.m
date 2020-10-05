%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% int_fold_up_down.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Jan 27 14:15:39 EST 2004
%---------------------------------------------------------------------------%

:- module int_fold_up_down.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.
:- import_module list.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string(int.fold_up(f, 1, 5, "")   ++ "\n", !IO),
    io.write_string(int.fold_down(f, 1, 5, "") ++ "\n", !IO),

    int.fold_up(p, 1, 5, "", FoldUpRes),
    io.write_string(FoldUpRes ++ "\n", !IO),
    int.fold_down(p, 1, 5, "", FoldDownRes),
    io.write_string(FoldDownRes ++ "\n", !IO),

    int.fold_up2(p2, 1, 5, "", FoldUp2Acc1, "", FoldUp2Acc2),
    io.write_string(FoldUp2Acc1 ++ FoldUp2Acc2 ++ "\n", !IO),
    int.fold_down2(p2, 1, 5, "", FoldDown2Acc1, "", FoldDown2Acc2),
    io.write_string(FoldDown2Acc1 ++ FoldDown2Acc2 ++ "\n", !IO),

    int.fold_up(io.write_int, 1, 5, !IO),
    io.nl(!IO),
    int.fold_down(io.write_int, 1, 5, !IO),
    io.nl(!IO),

    % This should produce some solutions.
    UpNondetPred =
        ( pred(Res::out) is nondet :-
            int.fold_up(p_nondet, 1, 5, "", Res)
        ),
    solutions(UpNondetPred, NondetSolnsUp),
    io.write_list(NondetSolnsUp, "\n", io.write_string, !IO),
    io.nl(!IO),

    % This will produce no solutions.
    DownNondetPred =
        ( pred(Res::out) is nondet :-
            int.fold_down(p_nondet, 1, 6, "", Res)
        ),
    solutions(DownNondetPred, NondetSolnsDown),
    io.write_list(NondetSolnsDown, "\n", io.write_string, !IO),
    io.nl(!IO),

    % This should succeed.
    ( if int.fold_up(p_semi, 1, 5, "", SemidetUpResS)
    then
        io.write_string(SemidetUpResS ++ "\n", !IO)
    else
        io.write_string("int.fold_up/5 failed\n", !IO)
    ),

    % This should fail.
    ( if int.fold_up(p_semi, 1, 6, "", SemidetUpResF) then
        io.write_string(SemidetUpResF ++ "\n", !IO)
    else
        io.write_string("int.fold_up/5 failed\n", !IO)
    ),

    % This should succeed.
    ( if int.fold_down(p_semi, 1, 5, "", SemidetDownResS) then
        io.write_string(SemidetDownResS ++ "\n", !IO)
    else
        io.write_string("int.fold_down/5 failed\n", !IO)
    ),

    % This should fail.
    ( if int.fold_down(p_semi, 1, 6, "", SemidetDownResF) then
        io.write_string(SemidetDownResF ++ "\n", !IO)
    else
        io.write_string("int.fold_down/5 failed\n", !IO)
    ).

:- func f(int, string) = string.

f(X, S) = string.format("(%d%s)", [i(X), s(S)]).

:- pred p(int::in, string::in, string::out) is det.

p(X, S, string.format("(%d%s)", [i(X), s(S)])).

:- pred p2(int::in, string::in, string::out, string::in, string::out) is det.

p2(X, S, string.format("(%d%s)", [i(X), s(S)]), T,
        string.format("(%d%s)", [i(X), s(T)])).

:- pred p_nondet(int::in, string::in, string::out) is nondet.

p_nondet(1, S, string.format("(1-1%s)", [s(S)])).
p_nondet(1, S, string.format("(1-2%s)", [s(S)])).
p_nondet(2, S, string.format("(2-1%s)", [s(S)])).
p_nondet(2, S, string.format("(2-2%s)", [s(S)])).
p_nondet(3, S, string.format("(3-1%s)", [s(S)])).
p_nondet(3, S, string.format("(3-2%s)", [s(S)])).
p_nondet(4, S, string.format("(4-1%s)", [s(S)])).
p_nondet(4, S, string.format("(4-2%s)", [s(S)])).
p_nondet(5, S, string.format("(5-1%s)", [s(S)])).
p_nondet(5, S, string.format("(5-2%s)", [s(S)])).

:- pred p_semi(int::in, string::in, string::out) is semidet.

p_semi(1, S, string.format("(1%s)", [s(S)])).
p_semi(2, S, string.format("(2%s)", [s(S)])).
p_semi(3, S, string.format("(3%s)", [s(S)])).
p_semi(4, S, string.format("(4%s)", [s(S)])).
p_semi(5, S, string.format("(5%s)", [s(S)])).

%---------------------------------------------------------------------------%
