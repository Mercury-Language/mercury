%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Regression test: rotd-2016-05-16 and before incorrectly reported the errors
% in the foreign_enum pragmas as occurring in foreign_export_enum pragmas.

:- module bad_foreign_enum.
:- interface.

:- type fruit
    --->   orange
    ;      lemon
    ;      apple.

:- type more_fruit
    --->   pear
    ;      grapefruit.

:- implementation.

    % Wrong number of arguments.
    %
:- pragma foreign_enum("C", fruit/0, [], []).

    % Invalid foreign language.
    %
:- pragma foreign_enum("UnsupportedLanguage", more_fruit/0, []).
