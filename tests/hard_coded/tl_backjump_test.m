%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test for thread-local backjumping.
%
%---------------------------------------------------------------------------%

:- module tl_backjump_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module backjump.
:- import_module list.
:- import_module solutions.
:- import_module string.
:- import_module thread.

main(!IO) :-
    ( can_spawn ->
        thread.spawn(run_problem(2), !IO),
        thread.spawn(run_problem(3), !IO)
    ;
        io.write_string("spawn/3 not supported in this grade", !IO)
    ).

:- type thread_id == int.

:- pred run_problem(thread_id::in, io::di, io::uo) is cc_multi.

run_problem(TId0, !IO) :-
    cc_multi_equal(TId0, TId),  % Make sure we are cc_multi.
    solutions(problem(TId), Sols),
    (
        Sols = [],
        io.format("(TID: #%d) No solutions!\n",
            [i(TId)], !IO)
    ;
        Sols = [_ | _],
        io.format("(TID: #%d) Solutions:\n", [i(TId)], !IO),
        WriteSoln = (pred(Sol::in, !.IO::di, !:IO::uo) is det :-
            io.format("(TID: #%d) ", [i(TId)], !IO),
            io.write(Sol, !IO)
        ),
        io.write_list(Sols, ",\n", WriteSoln, !IO),
        io.nl(!IO)
    ).

:- pred problem(thread_id::in, {int, int, int}::out) is nondet.

problem(TId, {A, B, C}) :-
    promise_pure (
        impure label(TId, "A", [1, 2], A, PA),
        impure label(TId, "B", [1, 2], B, PB),
        impure label(TId, "C", [1, 2, 3], C, PC),
        impure check(TId, A, B, C, PA, PB, PC)
    ).

:- impure pred label(thread_id::in, string::in, list(int)::in, int::out,
    choice_id::out) is nondet.

label(TId, Name, [N | _], N, P) :-
    impure get_choice_id(P),
    trace [io(!IO)] (
        io.format("(TID: #%d) label %s = %d, (%d)\n",
            [i(TId), s(Name), i(N), i(to_int(P))], !IO),
        true
    ).
label(TId, Name, [_ | Ns], N, P) :-
    impure label(TId, Name, Ns, N, P).

:- impure pred check(thread_id::in, int::in, int::in, int::in, choice_id::in,
    choice_id::in, choice_id::in) is semidet.

check(TId, A, B, C, PA, PB, PC) :-
    ( is_nogood(A, B, C, PA, PB, PC, P) ->
        trace [io(!IO)] (
            io.format("(TID: #%d) backjump (%d)\n", [i(TId), i(to_int(P))],
                !IO)
        ),
        impure backjump(P)
    ;
        is_solution(A, B, C),
        trace [io(!IO)] (
            io.format("(TID: #%d) solution %d, %d, %d\n",
                [i(TId), i(A), i(B), i(C)], !IO)
        )
    ).

:- pred is_nogood(int::in, int::in, int::in, choice_id::in, choice_id::in,
    choice_id::in, choice_id::out) is semidet.

is_nogood(1, 1, 2, _, _, P, P).
is_nogood(1, 2, 1, P, _, _, P).
is_nogood(2, 1, 2, _, P, _, P).

:- pred is_solution(int::in, int::in, int::in) is semidet.

is_solution(1, 1, 3).
is_solution(2, 1, 1).
is_solution(2, 2, 2).
