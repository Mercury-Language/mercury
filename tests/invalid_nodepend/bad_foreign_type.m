%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for error messages produced by syntax errors in 'foreign_type' pragmas.
%

:- module bad_foreign_type.
:- interface.

% Originally, this test case had several foreign_type definitions
% for foo, with all those definitions having errors of one kind or another.
% All those errors are now diagnosed when creating .int files, so they are
% now tested in tests/invalid_make_int/bad_foreign_type_int.m.
%
% Any bad foreign_type declarations in a test in *this* directory
% would cause bootcheck in C# and Java grades (which use mmc --make)
% to stop after generating bad_foreign_type.int, not proceeding to
% the errors we want to test here, which are the ones discovered
% during compilation to target language code.
:- type foo.

:- implementation.

    % No corresponding `:- type' declaration present.
    %
:- pragma foreign_type("C", bar, "int").

    % Same issue as with foo above, but in the implementation section.
:- type quux.

:- type quux.
