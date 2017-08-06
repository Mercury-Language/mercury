%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_inst_for_type.

:- interface.

:- import_module char.
:- import_module list.

:- type t1
    --->    f1a(int)
    ;       f1b(float).

:- type t2(T)
    --->    f2a(T, int)
    ;       f2b(char).

:- inst i1a for t1/0
    --->    f1a(ground).

:- inst i1b for t1/0
    --->    f1a(ground)
    ;       f2b(ground).

:- inst i1c for t1/0
    --->    f1a(ground, ground)
    ;       f1b(ground).

:- inst i1d for t1/0 == unique((f1a(ground) ; f1b(ground))).

:- inst i2a(I) for t2/1
    --->    f2a(I, ground).

:- inst i2b for t2/1
    --->    f1a(ground)
    ;       f2b(ground).

:- inst i2c for t2/1
    --->    f2a(ground, ground)
    ;       f2b(ground, free)
    ;       f2c(ground, free).

:- inst c1 for char/0
    --->    a
    ;       b.

:- inst c2 for character/0
    --->    a
    ;       b.

:- inst c3 for character/0
    --->    a
    ;       bb.

:- inst ls(I) for list/1
    --->    []
    ;       [I | ls(I)].
:- inst li(I) == ls(I).

:- inst el for list/1
    --->    [].

:- inst nel(I) for list/1
    --->    [ground | ground].

:- inst x1 for t1/0 == ground.

:- type t3
    --->    f3(int, int, int)
    ;       f3(int, int, int, int, int).

:- inst i3 for t3/0
    --->    f3(ground, ground, ground, ground).

:- type t4
    --->    yes(int).

:- inst i4a for t4/0
    --->    yes(ground).

:- inst i4b for t4/0
    --->    maybe.yes(ground).
