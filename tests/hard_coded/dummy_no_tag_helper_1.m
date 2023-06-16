%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dummy_no_tag_helper_1.
:- interface.

:- type no_tag_dummy.

:- func fun = no_tag_dummy.

:- type no_tag_dummy_eqv.
:- type no_tag_dummy_eqv2.

:- func fun_eqv = no_tag_dummy_eqv.
:- func fun_eqv2 = no_tag_dummy_eqv2.

:- implementation.

:- type no_tag_dummy ---> no_tag_dummy(simple_dummy).

:- type no_tag_dummy_eqv == no_tag_dummy.
:- type no_tag_dummy_eqv2 == no_tag_dummy_eqv.

:- type simple_dummy ---> simple_dummy.

% check circular types
:- type not_dummy_a ---> not_dummy_a(not_dummy_b).
:- type not_dummy_b ---> not_dummy_b(not_dummy_a).

fun = no_tag_dummy(simple_dummy).

fun_eqv = fun.

fun_eqv2 = fun.
