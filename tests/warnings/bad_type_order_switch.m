%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_type_order_switch.
:- interface.

:- type t
    --->    f01
    ;       f02
    ;       f03
    ;       f04
    ;       f05
    ;       f06
    ;       f07
    ;       f08
    ;       f09
    ;       f10
    ;       f11
    ;       f12
    ;       f13
    ;       f14
    ;       f15
    ;       f16
    ;       f17
    ;       f18
    ;       f19
    ;       f20.

:- pred p(t::in, int::out) is det.

:- implementation.

:- pragma type_order_switch(pred(p/2)).

p(T, N) :-
    ( T = f14, N = 42
    ; T = f01, N = 42
    ; T = f02, N = 42
    ; T = f03, N = 42
    ; T = f04, N = 42
    ; T = f06, N = 42
    ; T = f07, N = 42
    ; T = f08, N = 42
    ; T = f09, N = 42
    ; T = f10, N = 42
    ; T = f11, N = 42
    ; T = f12, N = 42
    ; T = f13, N = 42
    ; T = f15, N = 42
    ; T = f16, N = 42
    ; T = f05, N = 42
    ; T = f17, N = 42
    ; T = f18, N = 42
    ; T = f19, N = 42
    ; T = f20, N = 42
    ).
