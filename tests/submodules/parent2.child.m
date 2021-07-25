%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Some test cases to test nested modules.

:- module parent2.child.
:- interface.

% module `io' is imported in parent2

:- type t1 == foo.
:- type t2 == parent2.foo.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.
:- import_module type_desc.

:- type t3 == foo.
:- type t4 == parent2.foo.

:- func has_type_t1 = t1.
:- func has_type_t2 = t2.
:- func has_type_t3 = t3.
:- func has_type_t4 = t4.

has_type_t1 = bar.
has_type_t2 = parent2.bar.
has_type_t3 = baz(42).
has_type_t4 = parent2.baz(42).

main(!IO) :-
    parent2.hello(!IO),
    hello(!IO),

    io.print("t1 = ", !IO), io.print_line(type_of(has_type_t1), !IO),
    io.print("t2 = ", !IO), io.print_line(type_of(has_type_t2), !IO),
    io.print("t3 = ", !IO), io.print_line(type_of(has_type_t3), !IO),
    io.print("t4 = ", !IO), io.print_line(type_of(has_type_t4), !IO),

    io.print("has_type_t1 = ", !IO), io.print_line(has_type_t1, !IO),
    io.print("has_type_t2 = ", !IO), io.print_line(has_type_t2, !IO),
    io.print("has_type_t3 = ", !IO), io.print_line(has_type_t3, !IO),
    io.print("has_type_t4 = ", !IO), io.print_line(has_type_t4, !IO).
