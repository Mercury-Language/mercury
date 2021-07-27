%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module where_direct_arg2.
:- interface.

:- type bad_example
    --->    zero
    ;       two(int, int)
    ;       string(string)
    ;       int(int)
    ;       struct(struct)
    ;       eqv(eqv_struct)
    ;       tuple({int, int})
    ;       enum(enum)
    where   direct_arg is [string/1, int/1, struct/1, eqv/1, tuple/1, enum/1].

:- type struct
    --->    struct(int, int).
:- type eqv_struct == struct.

:- type enum
    --->    enum1
    ;       enum2
    ;       enum3.

%---------------------------------------------------------------------------%

:- implementation.

:- type good_example
    --->    nil
    ;       struct(struct)
    where direct_arg is [struct/1].

:- type foreign ---> foreign.
:- pragma foreign_type("C", foreign, "int") where direct_arg is [].
