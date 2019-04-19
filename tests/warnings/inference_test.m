%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case isn't really testing warnings,
% it is actually testing that the compiler
% prints out the right type/mode inference message.
%
%---------------------------------------------------------------------------%

:- module inference_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    io.print(len_func([42]), !IO),
    io.nl(!IO).

len_func(List) = Len :-
    len(List, Len).

len([], zero).
len([_ | Xs], N) :-
    len(Xs, N - 1).

:- typeclass null(T) where [
    func zero = T
].

:- instance null(int) where [
    func(zero/0) is int_zero
].

int_zero = 0.

unused_pred([X], [_]) :-
    X = zero.
