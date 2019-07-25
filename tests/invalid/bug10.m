%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test for bug #10: the source_file pragma was being ignored
% when reporting the unused module in the interface.
%---------------------------------------------------------------------------%

:- pragma source_file("x").
:- module bug10.
:- interface.

:- import_module list.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    foo(!IO),
    io.nl(!IO).
