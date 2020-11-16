%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_typeclass_bug2.

:- interface.

:- import_module char.

:- typeclass c(T) where [
    pred p(T, int),
    mode p(in, out) is det,
    mode p(out, in) is semidet,

    func q(T) = int,
    mode q(in) = out is semidet,
    mode q(out) = in is semidet
].

:- instance c(char).

:- implementation.

:- instance c(char) where [
    func(q/1) is bar,
    pred(p/2) is foo
].

:- pred foo(char, int).
:- mode foo(in, out) is det.
:- mode foo(out, in) is semidet.

foo(Char, Int) :-
    char.to_int(Char, Int).

:- func bar(char) = int.
:- mode bar(in) = out is semidet.
:- mode bar(out) = in is semidet.

bar(Char) = Int :-
    char.lower_upper(Char, Char1),
    char.to_int(Char1, Int).
