%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. There was no out-of-line definition of
% private_builtin.store_at_ref which is required for
% --optimise-constructor-last-call and --no-inline-builtins to work together.

%---------------------------------------------------------------------------%

:- module lco_no_inline.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

main(!IO) :-
    AL = ["one" - 1, "two" - 2],
    to_list_2(AL, L),
    io.write_line(L, !IO).

:- pred to_list_2(list(pair(string, int))::in, list(string)::out) is det.

to_list_2([], []).
to_list_2([X - Int | Xs], Out) :-
    ( if Int =< 0 then
        to_list_2(Xs, Out)
    else
        NewInt = Int - 1,
        to_list_2([X - NewInt | Xs], Out0),
        Out = [X | Out0]
    ).

%---------------------------------------------------------------------------%
