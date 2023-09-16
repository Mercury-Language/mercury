%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Test format_call pragams when applied to multi-moded predicates.
%-----------------------------------------------------------------------------%

:- module format_call_multi.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module format_call_multi_helper_1.

:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    Msg = "I/O state",
    log_msg(2, "Test 1: %s.", [s(Msg)], !IO),  % Ok.
    log_msg(2, "Test 2: %d.", [s(Msg)], !IO),  % Warning.
    solve_all(solve_pred, Result, !IO),
    io.print_line(Result, !IO).

:- pred solve_pred(int::out, ss::mdi, ss::muo) is nondet.

solve_pred(N, !SS) :-
    Msg = "Search state",
    log_msg(2, "Test 3: %s.", [s(Msg)], !SS),  % Ok.
    log_msg(2, "Test 4: %d.", [s(Msg)], !SS),  % Warning.
    invoke(prop_pred, I, !SS),
    ( N = 1 + I
    ; N = 2 + I
    ; N = 3 + I
    ; N = 4 + I
    ).

:- pred prop_pred(int::out, ps::di, ps::uo) is semidet.

prop_pred(4, !PS) :-
    Msg = "Propagation state",
    log_msg(2, "Test 5: %s.", [s(Msg)], !PS),  % Ok.
    log_msg(2, "Test 6: %d.", [s(Msg)], !PS),  % Warning.
    semidet_true.

%-----------------------------------------------------------------------------%
:- end_module format_call_multi.
%-----------------------------------------------------------------------------%
