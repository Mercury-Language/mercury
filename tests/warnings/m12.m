%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% "mini"-G12 -- a simplified version of the main G12 runtime.
%-----------------------------------------------------------------------------%

:- module m12.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

:- typeclass m12.state(S).
:- instance m12.state(io).

:- pred solve_first(solve(T), maybe(T), io, io).
:- mode solve_first(in(solve_semidet), out, di, uo) is cc_multi.
:- mode solve_first(in(solve_nondet), out, di, uo) is cc_multi.

:- pred solve_all(solve(T)::in(solve_nondet), list(T)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type ss.
:- instance m12.state(ss).

:- type solve(T)        == pred(T, ss, ss).
:- inst solve_det       == (pred(out, mdi, muo) is det).
:- inst solve_semidet   == (pred(out, mdi, muo) is semidet).
:- inst solve_multi     == (pred(out, mdi, muo) is multi).
:- inst solve_nondet    == (pred(out, mdi, muo) is nondet).
:- inst solve_erroneous == (pred(out, mdi, muo) is erroneous).

%-----------------------------------------------------------------------------%

:- type ps.
:- instance m12.state(ps).

:- type propagate(T)        == pred(T, ps, ps).
:- inst propagate_det       == (pred(out, di, uo) is det).
:- inst propagate_semidet   == (pred(out, di, uo) is semidet).
:- inst propagate_erroneous == (pred(out, di, uo) is erroneous).

:- pred invoke(propagate(T)) : solve(T).
:- mode invoke(in(propagate_det)) `with_inst` solve_semidet.
:- mode invoke(in(propagate_semidet)) `with_inst` solve_semidet.

%-----------------------------------------------------------------------------%

:- pred log_msg(int, string, list(poly_type), S, S) <= m12.state(S).
:- mode log_msg(in, in, in, mdi, muo) is det.
:- mode log_msg(in, in, in,  di,  uo) is det.
:- pragma format_call(pred(log_msg/5),  format_string_values(2, 3)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- typeclass m12.state(S) where [].

:- instance m12.state(io) where [].

:- type ss
    --->    dummy_solver_state.

:- instance m12.state(ss) where [].

solve_first(Pred, MaybeSolution, !IO) :-
    ( if unsafe_solve(Pred, Solution) then
        cc_multi_equal(yes(Solution), MaybeSolution)
    else
        MaybeSolution = no
    ).

solve_all(Pred, Solutions, !IO) :-
    SPred = (pred(T::out) is nondet :- unsafe_solve(Pred, T)),
    solutions(SPred, Solutions).

:- pred unsafe_solve(solve(T), T).
:- mode unsafe_solve(in(solve_det), out) is det.
:- mode unsafe_solve(in(solve_semidet), out) is semidet.
:- mode unsafe_solve(in(solve_nondet), out) is nondet.

unsafe_solve(Pred, Solution) :-
    Pred(Solution, dummy_solver_state, _).

%-----------------------------------------------------------------------------%

:- type ps
    --->    dummy_propagation_state.

:- instance m12.state(ps) where [].

invoke(Pred, Output, !SS) :-
    some [!PS] (
        enter_invoke(!.SS, !:PS),
        Pred(Output, !PS),
        semidet_true,
        leave_invoke(!.PS, !:SS)
    ).

%-----------------------------------------------------------------------------%

:- pred enter_invoke(ss::mdi, ps::uo) is det.

enter_invoke(!.SS, !:PS) :-
    unsafe_switch_to_propagation(!.SS, !:PS).

:- pred leave_invoke(ps::di, ss::muo) is det.

leave_invoke(!.PS, !:SS) :-
    unsafe_switch_to_search(!.PS, !:SS).

:- pred unsafe_switch_to_propagation(ss::mdi, ps::uo) is det.

unsafe_switch_to_propagation(_, dummy_propagation_state).

:- pred unsafe_switch_to_search(ps::di, ss::muo) is det.

unsafe_switch_to_search(_, dummy_solver_state).

%-----------------------------------------------------------------------------%

log_msg(L, S, P, !T) :-
    trace [io(!IO)] (
        do_format_log_msg(L, S, P, !IO)
    ).

:- pred do_format_log_msg(int::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

do_format_log_msg(L, FormatString, PolyList, !IO) :-
    K = get_log_level(!.IO),
    ( if K >= L then
        io.stderr_stream(LogFile, !IO),
        io.format(LogFile, FormatString, PolyList, !IO),
        io.flush_output(LogFile, !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- mutable(log_level_var, int, 0, ground, [attach_to_io_state, untrailed]).

:- pred set_log_level_io(int::in, io::di, io::uo) is det.

set_log_level_io(K, !IO) :-
    set_log_level_var(K, !IO).

:- func get_log_level(S) = int <= m12.state(S).
:- mode get_log_level(mui) = out is det.
:- mode get_log_level(ui) = out is det.

get_log_level(_S) = K :-
    promise_pure (
        semipure get_log_level_var(K)
    ).

:- pred get_log_level(int, S, S) <= m12.state(S).
:- mode get_log_level(out, mdi, muo) is det.
:- mode get_log_level(out, di, uo) is det.

get_log_level(K, !S) :-
    promise_pure (
        semipure get_log_level_var(K)
    ).

%-----------------------------------------------------------------------------%
:- end_module m12.
%-----------------------------------------------------------------------------%
