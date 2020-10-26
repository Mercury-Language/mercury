%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for error messages produced by syntax errors in 'foreign_type' pragmas.
%

:- module bad_foreign_type.
:- interface.

:- type foo.

:- implementation.

    % Too few arguments.
    %
:- pragma foreign_type("C").

    % Too many arguments.
    %
:- pragma foreign_type("C", foo, "int", [can_pass_as_mercury_type], "").

    % Invalid foreign language.
    %
:- pragma foreign_type("InvalidLanguage", foo, "int").

    % Second arg is not an atom.
    %
:- pragma foreign_type("C", 1111, "int").

    % Third arg is not a string.
    %
:- pragma foreign_type("C", foo, 2222).

    % Fourth argument is not list of assertions.
    %
:- pragma foreign_type("C", foo, "int", 5555).

    % Unrecognised assertion.
    %
:- pragma foreign_type("C", foo, "int", [not_an_assertion]).

    % Assertion is not an atom.
    %
:- pragma foreign_type("C", foo, "int", [3333, "I'm a string"]).

    % Bad where clause.
    %
:- pragma foreign_type("C", foo, "int")
    where 7777.

    % Another bad where clause.
    %
:- pragma foreign_type("C", foo, "int")
    where foo is bar.

    % Check contexts on different arguments are correct.
    %
:- pragma foreign_type(
    "InvalidLanguage",
    9999,
    [not_an_assertion]
)
    where
         bar
    is
         baaz.

    % No corresponding `:- type' declaration present.
    %
:- pragma foreign_type("C", bar, "int").

    % Second argument is a type variable.
    %
:- pragma foreign_type("C", T, "int").

    % Empty foreign type descriptor (C, C# , Java).
    %
:- type quux.
:- pragma foreign_type("C", quux, "").
:- pragma foreign_type("C#", quux, "").
:- pragma foreign_type("Java", quux, "").

    % Repeated foreign type assertion.
    %
:- pragma foreign_type("C", foo, "int",
    [can_pass_as_mercury_type, can_pass_as_mercury_type]).

:- type quux.
