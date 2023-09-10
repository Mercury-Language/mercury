%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called nested_mod_type_bug.
%

:- module type_exported_to_submods.
:- interface.

    :- module type_exported_to_submods_helper_1.
    :- interface.
    :- type blah == int.
    :- end_module type_exported_to_submods_helper_1.

:- type foo(T).

:- implementation.

:- type foo(T)
    --->    f(T).
