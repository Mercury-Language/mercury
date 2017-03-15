%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for the error messages generated for syntax errror in 'foreign_export'
% pragmas.

:- module bad_foreign_export.
:- interface.

:- func foo(int, int) = int.

:- implementation.
:- import_module int.

foo(X, Y) = X + Y.

    % Incorrect number of arguments.
    %
:- pragma foreign_export("C").

    % Second arg is not pred-and-modes.
    %
:- pragma foreign_export("C", 11111, "Test2").

    % Third arg is not a string.
    %
:- pragma foreign_export("C", foo(in, in) = out, 22222).

    % Invalid foreign laguage.
    %
:- pragma foreign_export("InvalidLanguage", foo(in, in) = out, "Test4").

    % Check that the contexts error messages for each argument are correct.
    %
:- pragma foreign_export(
    "InvalidLanguage",
    3333,
    4444
).

    % Third arg is the empty string.
    %
:- pragma foreign_export("C", foo(in, in) = out, "").
