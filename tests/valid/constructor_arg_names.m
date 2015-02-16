%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constructor_arg_names.

:- interface.

:- type test
    --->    f(field1:: int, field2:: string)
    ;       g(field3:: string, float).
