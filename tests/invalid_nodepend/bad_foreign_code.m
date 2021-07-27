%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for error messages produced by syntax errors in 'foreign_code' pragmas.
%

:- module bad_foreign_code.
:- interface.

:- type foo ---> foo.

:- implementation.

    % Too few arguments.
    %
:- pragma foreign_code("C").

    % Too many arguments.
    %
:- pragma foreign_code("C", "", "").

    % Invalid foreign language.
    %
:- pragma foreign_code("InvalidLanguage", "").

    % Code argument is not a string or include_file.
    %
:- pragma foreign_code("C", 2222).

    % Argument of include_file is not a string.
    %
:- pragma foreign_code("C", include_file(3333)).

    % Argument of include_file is the empty string.
    %
:- pragma foreign_code("C", include_file("")).

    % Check that the correct contexts are associated with errors
    % for each argument.
    %
:- pragma foreign_code(
    "InvalidLanguage",
    6666
).

:- pragma foreign_code("Erlang", include_file("")).
