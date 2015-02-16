%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_type_missing.

:- interface.

:- type ft2
        --->    f(foreign).

    % This test is always run using `--grade il'.
:- type foreign.
:- pragma foreign_type("C", foreign, "int").
