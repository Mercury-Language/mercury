%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for various syntax errors in 'foreign_enum' pragmas.
% This is also a regression test for a problem in rotd-2016-05-16 and before
% where errors occurring in 'foreign_enum' pragmas were incorrectly reported
% as occurring in 'foreign_export_enum' pragmas.

:- module bad_foreign_enum.
:- interface.

:- type fruit
    --->   orange
    ;      lemon
    ;      apple.

:- type more_fruit
    --->   pear
    ;      grapefruit.

:- type vegetable
    --->   carrot
    ;      potato
    ;      turnip.

:- type meat
    --->    beef
    ;       lamb
    ;       pork.

:- type poultry
    --->    chicken
    ;       duck.

:- type more_poultry
    --->    goose
    ;       turkey.

:- implementation.

    % Wrong number of arguments.
    %
:- pragma foreign_enum("C", fruit/0, [], []).

    % Invalid foreign language.
    %
:- pragma foreign_enum("InvalidLanguage", more_fruit/0, []).

    % Second arg is not name / arity.
    %
:- pragma foreign_enum("C", vegetable,
    [carrot - "1", potato - "2", turnip - "3"]).

    % Check that multiple errors are reported and that the correct
    % contexts are attached to them.
    %
:- pragma foreign_enum(
    "InvalidLanguage",
    meat,
    []
).

    % Check for invalid elements in the mapping list.
    %
:- pragma foreign_enum("C", poultry/0, [
     chicken,
     duck - "2"
]).

    % Ditto.
    %
:- pragma foreign_enum("C", more_poultry/0, [
     "goose" - "1",
     turkey - "561"
]).

    % Ditto.
    %
:- pragma foreign_enum("C", more_poultry/0, [
     "goose",
     turkey,
     X
]).

:- pragma foreign_enum(
    "Erlang",
    meat,
    []
).

% XXX TODO: we should generate more specific error messages in cases
% like the following:
%:- pragma foreign_enum("C", more_poultry/0, [
%     goose - "",
%     turkey - 562
%]).
