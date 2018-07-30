%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module local_args.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.

:- type thing
    --->    thing(
                t1 :: bool,
                t2 :: bool,
                t3 :: int8,
                t4 :: bool,
                t5 :: bool,
                t6 :: bool
            ).

:- type tree
    --->    three(tree, tree, tree)
    ;       one(thing).

main(!IO) :-
    A = thing(no, no, 42i8, yes, yes, yes),
    io.write_line(A, !IO),
    update_test(no, A, B),
    io.write_line(B, !IO),

    Tree = three(
        three(
            one(thing(no, no, 42i8, yes, yes, yes)),
            one(thing(no, yes, 43i8, no, yes, yes)),
            one(thing(no, yes, 43i8, no, yes, no))
        ),
        three(
            one(thing(no, no, 42i8, yes, yes, yes)),
            one(thing(no, yes, 43i8, no, yes, yes)),
            one(thing(no, yes, 43i8, no, yes, no))
        ),
        three(
            one(thing(no, no, 42i8, yes, yes, yes)),
            one(thing(no, yes, 43i8, no, yes, yes)),
            one(thing(no, yes, 43i8, no, yes, no))
        )
    ),
    io.write_line(Tree, !IO).

:- pred update_test(bool::in, thing::in, thing::out) is det.
:- pragma no_inline(update_test/3).

update_test(N, A, B) :-
    B = A ^ t6 := N.
