%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Many versions of the compiler prior to
% nov 11, 1999 generated incorrect code for the check_interval predicate.
% The bug was that frameopt recognized that check_interval did not need a
% stack frame, and deleted the frame setup code, but did not redirect the
% failure branches inside the inlined model_semi pragma_c instructions,
% so that they still led to code that deallocated the (now nonexistent)
% stack frame. This meant that calls to check_interval violated the
% invariant which says that calls leave MR_sp unchanged.
%

:- module frameopt_pragma_redirect.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

main(!IO) :-
    add_interval_list([1 - 2, 3 - 4, 5 - 6, 7 - 8, 9 - 10], I),
    io.write(I, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- type interval == pair(int).

:- pred add_interval_list(list(interval)::in, interval::out) is det.

add_interval_list([], (0 - 0)).
add_interval_list([I | Is], SumI) :-
    check_interval(I),
    add_interval_list(Is, SumI0),
    add_intervals(I, SumI0, SumI).

:- pred add_intervals(interval::in, interval::in, interval::out) is det.

add_intervals(S1 - E1, S2 - E2, S - E) :-
    S = S1 + S2,
    E = E1 + E2.

:- pred check_interval(interval::in) is det.
:- pragma no_inline(check_interval/1).

check_interval(S - E) :-
    ( if is_invalid(S) then
        error("Found an invalid interval 1!")
    else if is_invalid(E) then
        error("Found an invalid interval 2!")
    else
        true
    ).

:- pred is_invalid(int::in) is semidet.
:- pragma inline(is_invalid/1).

:- pragma foreign_proc("C",
    is_invalid(X :: in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = X > 50;
").

is_invalid(X) :- X > 50.
