%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module g12_fe_bug.g12_fe_bug_helper_2.
:- interface.

:- include_module g12_fe_bug_helper_3.

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
