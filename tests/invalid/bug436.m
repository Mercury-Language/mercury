%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that all of the symbols in the third argument of a foreign_enum pragma
% are data constructors of the type referred to by the foreign_enum pragma.
%
%---------------------------------------------------------------------------%

:- module bug436.
:- interface.

:- type foo
    --->    foo1
    ;       foo2.

:- type bar
    --->    bar1
    ;       bar2.

:- implementation.

:- pragma foreign_enum("C", foo/0, [
    foo1 - "1",
    foo2 - "2",
    bar  - "3"
]).

:- pragma foreign_enum("C", bar/0, [
    bar1  - "1",
    bar2  - "2",
    baaz1 - "3",
    baaz2 - "4"
]).
