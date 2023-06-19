%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% "Hello World" in Mercury, using nested modules.
%

:- module parent_t1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- include_module parent_t1_helper_1, parent_t1_helper_2.

:- import_module parent_t1.parent_t1_helper_1.
:- use_module parent_t1.parent_t1_helper_2.
:- import_module require.
:- import_module std_util.
:- import_module type_desc.

:- type t1 == parent_t1.parent_t1_helper_1.foo.
:- type t2 == parent_t1_helper_1.foo.
:- type t3 == foo.
:- type t4 == parent_t1.parent_t1_helper_2.foo.
:- type t5 == parent_t1.parent_t1_helper_2.foo.

main(!IO) :-
    parent_t1.parent_t1_helper_1.hello(!IO),
    parent_t1_helper_1.hello(!IO),
    hello(!IO),
    parent_t1.parent_t1_helper_2.hello(!IO),
    parent_t1_helper_2.hello(!IO),

    io.print("t1 = ", !IO), io.print_line(type_of(has_type_t1), !IO),
    io.print("t2 = ", !IO), io.print_line(type_of(has_type_t2), !IO),
    io.print("t3 = ", !IO), io.print_line(type_of(has_type_t3), !IO),
    io.print("t4 = ", !IO), io.print_line(type_of(has_type_t4), !IO),
    io.print("t5 = ", !IO), io.print_line(type_of(has_type_t5), !IO),

    io.print("has_type_t1 = ", !IO), io.print_line(has_type_t1, !IO),
    io.print("has_type_t2 = ", !IO), io.print_line(has_type_t2, !IO),
    io.print("has_type_t3 = ", !IO), io.print_line(has_type_t3, !IO),
    io.print("has_type_t4 = ", !IO), io.print_line(has_type_t4, !IO),
    io.print("has_type_t5 = ", !IO), io.print_line(has_type_t5, !IO).

:- func has_type_t1 = t1.
:- func has_type_t2 = t2.
:- func has_type_t3 = t3.
:- func has_type_t4 = t4.
:- func has_type_t5 = t5.

has_type_t1 = parent_t1.parent_t1_helper_1.bar.
has_type_t2 = parent_t1_helper_1.bar.
has_type_t3 = bar.
has_type_t4 = parent_t1.parent_t1_helper_2.bar.
has_type_t5 = parent_t1_helper_2.bar.
