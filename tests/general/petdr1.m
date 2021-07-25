%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test - previous versions of the compiler
% got an internal compiler error when compiling this file.
% NOTE: do NOT fix the determinism warnings in this file -
% one version of the compiler aborted only when printing the warning.

:- module petdr1.
:- interface.
:- import_module require.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- pred interpreter(int::out) is nondet.    % don't fix the determinism

:- pred which(int::out) is nondet.          % don't fix the determinism

:- pred write(int::in) is det.

:- implementation.

main(!IO).

interpreter(Out) :-
    ( if
        which(X),
        write(X),
        fail
    then
        error("unreachable")
    else
        Out = 42
    ).

which(1).
which(2).

write(_).
