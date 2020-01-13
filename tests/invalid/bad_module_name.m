% vim: ts=4 sw=4 et ft=mercury

:- module bad_module_name.
:- interface.

:- type t
    --->    f1
    ;       f2.

:- include_module sub.
