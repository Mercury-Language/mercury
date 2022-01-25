%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module change_type_nr_3.

:- interface.

:- import_module change_type_nr_2.

:- type foo
    --->    a
    ;       b(foo2).

:- pred init(foo::out) is det.

:- implementation.

init(a).
