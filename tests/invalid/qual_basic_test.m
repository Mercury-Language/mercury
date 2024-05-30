%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test to ensure a qualified predicate name is parsed correctly.
%

:- module qual_basic_test.
:- interface.

:- import_module io.

:- pred qual_basic_test.main(io, io).
:- mode qual_basic_test.main(di, uo) is det.

:- implementation.

qual_basic_test.main -->
    io.io__write_string("Gotcha!\n").
