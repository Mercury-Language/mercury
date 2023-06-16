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
:- import_module thread.semaphore.

main(!IO) :-
    ( if can_spawn then
        semaphore.init(1, Sem, !IO),
        thread.spawn(run_problem(Sem, 2), !IO),
        thread.spawn(run_problem(Sem, 3), !IO)
    else
        io.write_string("spawn/3 not supported in this grade", !IO)
    ).

:- type thread_id == int.

:- pred run_problem(semaphore::in, thread_id::in, io::di, io::uo) is cc_multi.

run_problem(Sem, TId0, !IO) :-
    cc_multi_equal(TId0, TId),  % Make sure we are cc_multi.
    solutions(problem(Sem, TId), Sols),
    (
        Sols = [],
        locked_write_string(Sem,
            format("(TID: #%d) No solutions!\n", [i(TId)]),
            !IO)
    ;
        Sols = [_ | _],
        wait(Sem, !IO),
        io.format("(TID: #%d) Solutions:\n", [i(TId)], !IO),
        WriteSoln =
            ( pred(Sol::in, !.IO::di, !:IO::uo) is det :-
                io.format("(TID: #%d) ", [i(TId)], !IO),
                io.write(Sol, !IO)
            ),
        io.write_list(Sols, ",\n", WriteSoln, !IO),
        io.nl(!IO),
        signal(Sem, !IO)
    ).

:- pred problem(semaphore::in, thread_id::in, {int, int, int}::out) is nondet.

problem(Sem, TId, {A, B, C}) :-
    promise_pure (
        impure label(Sem, TId, "A", [1, 2], A, PA),
        impure label(Sem, TId, "B", [1, 2], B, PB),
        impure label(Sem, TId, "C", [1, 2, 3], C, PC),
        impure check(Sem, TId, A, B, C, PA, PB, PC)
    ).

:- impure pred label(semaphore::in, thread_id::in, string::in,
    list(int)::in, int::out, choice_id::out) is nondet.

label(Sem, TId, Name, [N | _], N, P) :-
    impure get_choice_id(P),
    trace [io(!IO)] (
        locked_write_string(Sem, format("(TID: #%d) label %s = %d, (%d)\n",
            [i(TId), s(Name), i(N), i(to_int(P))]), !IO),
        true
    ).
label(Sem, TId, Name, [_ | Ns], N, P) :-
    impure label(Sem, TId, Name, Ns, N, P).

:- impure pred check(semaphore::in, thread_id::in, int::in, int::in, int::in,
    choice_id::in, choice_id::in, choice_id::in) is semidet.

check(Sem, TId, A, B, C, PA, PB, PC) :-
    ( if is_nogood(A, B, C, PA, PB, PC, P) then
        trace [io(!IO)] (
            locked_write_string(Sem,
                format("(TID: #%d) backjump (%d)\n", [i(TId), i(to_int(P))]),
                !IO)
        ),
        impure backjump(P)
    else
        is_solution(A, B, C),
        trace [io(!IO)] (
            locked_write_string(Sem, format("(TID: #%d) solution %d, %d, %d\n",
                [i(TId), i(A), i(B), i(C)]), !IO)
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

:- pred locked_write_string(semaphore::in, string::in, io::di, io::uo) is det.

locked_write_string(Sem, String, !IO) :-
    wait(Sem, !IO),
    write_string(String, !IO),
    signal(Sem, !IO).
