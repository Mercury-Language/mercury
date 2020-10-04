%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test constraint propagation.

:- module constraint.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    benchmark([1, 16, 100, 15, 20], Found),
    io.write_string(Found, !IO).

:- pred benchmark(list(int), string).
:- mode benchmark(in, out) is det.
    % Disable unrolling of the loop.
:- pragma no_inline(benchmark/2).

benchmark(Data, Out) :-
    ( if mymember(X, Data), test(X) then
        Out = "found"
    else
        Out = "not_found"
    ).

:- pred mymember(int, list(int)).
:- mode mymember(out, in) is nondet.

mymember(X, [X | _]).
mymember(X, [_ | Xs]) :- mymember(X, Xs).

:- pred test(int).
:- mode test(in) is semidet.

test(15).
