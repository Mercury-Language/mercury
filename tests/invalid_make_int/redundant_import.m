%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module redundant_import.
:- interface.

:- import_module io.

% This error is here just to make this test case fail.
% The purpose of this test case is not to test this diagnostic;
% it is to test the *absence* of a diagnostic for the redundant imports below.
:- pred main(undef_type::in, io::di, io::uo) is det.

% This should get a warning when generating code for this module,
% but this warning should be suppressed when making the .int file.
:- import_module io.

:- implementation.

% This should get a warning when generating code for this module,
% but this warning should be suppressed when making the .int file.
:- import_module io.

main(!IO) :-
    io.write_string("Hello, world\n", !IO).
