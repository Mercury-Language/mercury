%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% From orig/stm-compiler/test5
%
% Tests that an exception thrown from within an atomic scope propogates
% outside it.
%

:- module exception_propogate.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module stm_builtin.
:- import_module univ.
:- import_module int.

:- pred call_err is det.
call_err :-
    throw(123).

:- pred trans(int::out, io::di, io::uo) is det.
trans(X, IO0, IO) :-
    atomic [outer(IO0, IO), inner(STM, STM)]
    (
        call_err,
        X = 28
    ).

main(!IO) :-
    try_io(trans, Result, !IO),
    print(Result, !IO), nl(!IO).
