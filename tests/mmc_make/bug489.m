%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that mmc --make can handle a module kept in a source file
% that could be confused for a standard library module
% (or other module that whose source file is not available).

:- module bug489.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- include_module lexer.
:- include_module other.
:- import_module bug489.lexer.

main(!IO) :-
    io.write(tok1, !IO),
    io.nl(!IO).
