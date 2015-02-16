%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module g12_fe_bug.solvers.
:- interface.

:- include_module bounds.

:- type trailing_mode
    --->    no_trailing
    ;       trailing.

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_enum("C", trailing_mode/0, [
    no_trailing - "0",
    trailing    - "1"
]).

%---------------------------------------------------------------------------%
