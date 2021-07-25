%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module duplicate_label.
:- interface.
:- import_module io.

% An old version of the Mercury compiler
% generated incorrect C code for this module
% (there was a duplicate definition for a static constant).

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
    X = "foo",
    Y = [X, X],
    Z = [Y, Y],
    list.condense(Z, L),
    io.write_strings(L, !IO).
