%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for error messages produced by syntax errors in 'foreign_decl' pragmas.
%

:- module bad_foreign_decl.
:- interface.

:- type foo ---> foo.

:- implementation.

    % Too few arguments.
    %
:- pragma foreign_decl("C").

    % Too many arguments.
    %
:- pragma foreign_decl("C", local, "", "").

    % Invalid foreign language.
    %
:- pragma foreign_decl("InvalidLanguage", "").

    % DeclCode argument is not a string or include_file.
    %
:- pragma foreign_decl("C", 1234).

    % 'local' is misspelled.
    %
:- pragma foreign_decl("C", lcoal, "").

    % 'local' is an integer.
    %
:- pragma foreign_decl("C", 4444, "").

    % Argument of include file is not a string.
    %
:- pragma foreign_decl("C", include_file(5555)).

    % Argument of include_file is an empty string.
    % XXX The compiler currently accepts this.
:- pragma foreign_decl("C", include_file("")).

    % Check that the correct contexts are associated with errors
    % for each argument.  (XXX they're currently not!)
    %
:- pragma foreign_decl(
    "InvalidLanguage",
    lcoal,
    7777
).

:- pragma foreign_decl("Erlang", include_file("")).
