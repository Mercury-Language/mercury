%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_arity.
:- interface.

:- import_module assoc_list.

:- type t1
    --->    t1_1f
    ;       t1_2f
    ;       t1_3f(t2(int)).

:- type t2(K, V) == assoc_list(K, V).
