%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multi_moded.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions.

:- typeclass thisclass(T) where [
    pred c(T, T),
    mode c(in, in) is semidet,
    mode c(out, in) is semidet,
    mode c(in, out) is nondet
].

:- instance thisclass(int) where [
    pred(c/2) is p
].

:- pred p(int, int).
:- mode p(in, in) is semidet.
:- mode p(out, in) is semidet.
:- mode p(in, out) is nondet.

p(1, 1).
p(1, 2).
p(1, 3).
p(2, 4).

:- pred mypred(T::in, T::out) is nondet <= thisclass(T).

mypred(A, B) :-
    c(A, B).

main(!IO) :-
    solutions(mypred(1), Solns),
    io.write_line(Solns, !IO).
