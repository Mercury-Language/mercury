%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Ensure that the foreign_decl is placed into the .opt file for procedures
% which are defined by both a foreign proc and a mercury clause.
%

:- module intermod_pragma_clause.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_pragma_clause_sub.

main(!IO) :-
    f(X),
    io.write_int(X, !IO),
    io.nl(!IO),
    g(Y),
    io.write_int(Y, !IO),
    io.nl(!IO).
