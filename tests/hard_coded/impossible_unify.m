%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impossible_unify.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool.

main(!IO) :-
    not bar(42),
    print("ok", !IO),
    nl(!IO).

:- pred bar(int::in) is failure.

bar(X) :-
    foo(X, yes).

:- pred foo(int, bool).
:- mode foo(in, out(bound(no))) is det.

foo(_, no).
