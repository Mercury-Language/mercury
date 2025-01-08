%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test - previous versions of the compiler
% got an internal compiler error when compiling this file.
% NOTE: do NOT fix the determinism warnings in this file -
% one version of the compiler aborted only when printing the warning.
%

:- module petdr1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module require.

main(!IO).

:- pred interpreter(int::out) is nondet.    % don't fix the determinism

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

:- pred which(int::out) is nondet.          % don't fix the determinism

which(1).
which(2).

:- pred write(int::in) is det.

write(_).
