%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for error messages produced by syntax errors in 'foreign_import_module'
% pragmas.
%

:- module bad_foreign_import_module.
:- interface.

:- type foo ---> foo.

:- implementation.

    % Incorrect number of arguments.
    %
:- pragma foreign_import_module("C").

    % Invalid foreign language.
    %
:- pragma foreign_import_module("InvalidForeignLang", int).

    % Check that contexts for the language and import term are correct.
    %
:- pragma foreign_import_module(
     "InvalidForeignLang",
      int(int)
).

:- pragma foreign_import_module("Erlang", int).
