%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_enum_invalid.
:- interface.

:- type incomplete
    --->    foo
    ;       bar
    ;       baz.

:- type incomplete2
    --->    foo2
    ;       bar2
    ;       baz2.

:- type not_a_bijection
    --->    a
    ;       b
    ;       c.

:- type in_int
    --->    in_int.

:- pragma foreign_enum("C", in_int/0, [in_int - "300"]).

:- type dup_foreign_enum ---> dup_foreign_enum.

:- implementation.

:- pragma foreign_enum("C", incomplete/0, [
    foo - "3",
    bar - "4"
]).

:- pragma foreign_enum("C", incomplete2/0, []).

:- pragma foreign_enum("C", not_a_bijection/0, [
    a - "30",
    a - "40",
    b - "60",
    c - "60"
]).

:- pragma foreign_enum("C", dup_foreign_enum/0, [dup_foreign_enum - "400"]).
:- pragma foreign_enum("C", dup_foreign_enum/0, [dup_foreign_enum - "500"]).
