%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test to ensure parsing of the functor `is/2' is done correctly.

:- module qual_is_test.

:- interface.

:- import_module qual_is_test_helper_1.

:- import_module io.
:- import_module list.
:- import_module string.

:- pred qual_is_test.main(io.state::di, io.state::uo) is det.

:- implementation.

qual_is_test.main(!IO) :-
    io.write_string(W, !IO),
    is("Hi!.\n", W).
