% vim: ts=4 sw=4 ft=mercury

:- module repeated_field_name.

:- interface.

:- type t
    --->    t1(
                f1 :: int,
                f2 :: int,
                f2 :: int,
                f2 :: int
            )
    ;       t2(
                f1 :: int
            ).

:- type u
    --->    u1(
                f1 :: int,
                f1 :: int,
                f2 :: int
            ).
