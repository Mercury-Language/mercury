%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. It tests whether overload resolution will pick
% a match *with* typeclass constraints over a match *without* such constraints.
%

:- module overload_resolution_preference.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module enum.
:- import_module list.
:- import_module string.
:- import_module uint.

main(!IO) :-
    % This call to from_int matches both enum.from_int/2 and uint.from_int/2.
    % Having overload resolution pick enum.from_int/2 leads to a compiler
    % abort, due to the class constraints on that predicate not being found.
    ( if from_int(42, U) then
        io.format("U = %u\n", [u(U)], !IO)
    else
        io.write_string("from_int failed.\n", !IO)
    ).
