%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that the compiler gives a half decent error message when it is
% unable to resolve predicate overloading in this module.
%

:- module unresolved_overloading.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module unresolved_overloading.sub.

%---------------------------------------------------------------------------%

main(!IO) :-
    loud_and_annoying(gibbon, !IO).

%---------------------------------------------------------------------------%

:- typeclass gibbon(T) where [].

:- type gibbon
    --->    gibbon.
:- instance gibbon(gibbon) where [].

:- pred loud_and_annoying(T::in, io::di, io::uo) is det <= gibbon(T).

loud_and_annoying(_, !IO).

%---------------------------------------------------------------------------%

:- module unresolved_overloading.sub.
:- interface.

:- import_module io.

:- typeclass howler_monkey(T) where [].

:- pred loud_and_annoying(T::in, io::di, io::uo) is det <= howler_monkey(T).

:- implementation.

loud_and_annoying(_, !IO).

:- end_module unresolved_overloading.sub.
