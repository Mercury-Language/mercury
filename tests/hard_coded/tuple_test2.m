%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Unify/compare of tuples did not maintain deep profiler invariants.

:- module tuple_test2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type list
    --->    nil
    ;       node({list, fruit}).

:- type fruit
    --->    fruit(string).

main(!IO) :-
    A = node({nil, fruit("apple")}),
    B = node({nil, fruit("peach")}),
    Cases = [{A, A}, {A, B}, {B, A}, {B, B}],

    io.write_string("unify:\n", !IO),
    list.foldl(test_unify, Cases, !IO),

    io.write_string("\ncompare:\n", !IO),
    list.foldl(test_compare, Cases, !IO).

:- pred test_unify({T, T}::in, io::di, io::uo) is det.

test_unify({A, B}, !IO) :-
    io.write(A, !IO),
    ( if unify(A, B) then
        io.write_string(" =  ", !IO)
    else
        io.write_string(" \\= ", !IO)
    ),
    io.write(B, !IO),
    io.nl(!IO).

:- pred test_compare({T, T}::in, io::di, io::uo) is det.

test_compare({A, B}, !IO) :-
    compare(R, A, B),
    io.write(A, !IO),
    (
        R = (=),
        io.write_string(" = ", !IO)
    ;
        R = (<),
        io.write_string(" < ", !IO)
    ;
        R = (>),
        io.write_string(" > ", !IO)
    ),
    io.write_line(B, !IO).
