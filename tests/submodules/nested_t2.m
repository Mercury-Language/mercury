%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Another test case to test nested modules.

:- module nested_t2.
:- interface.
:- import_module io.

:- pred hello(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    :- module nested_t2.child.
    :- interface.

    % module `io' is imported in nested_t2

    :- type t1 == foo.
    :- type t2 == nested_t2.foo.

    :- pred main(io::di, io::uo) is det.

    :- end_module nested_t2.child.

%---------------------------------------------------------------------------%

:- implementation.

    :- module nested_t2.child.
    :- implementation.
    :- import_module std_util.
    :- import_module type_desc.

    :- type t3 == foo.
    :- type t4 == nested_t2.foo.

    :- func has_type_t1 = t1.
    :- func has_type_t2 = t2.
    :- func has_type_t3 = t3.
    :- func has_type_t4 = t4.

    has_type_t1 = bar.
    has_type_t2 = nested_t2.bar.
    has_type_t3 = baz(42).
    has_type_t4 = nested_t2.baz(42).

    main(!IO) :-
        nested_t2.hello(!IO),
        hello(!IO),

        io.print("t1 = ", !IO), io.print_line(type_of(has_type_t1), !IO),
        io.print("t2 = ", !IO), io.print_line(type_of(has_type_t2), !IO),
        io.print("t3 = ", !IO), io.print_line(type_of(has_type_t3), !IO),
        io.print("t4 = ", !IO), io.print_line(type_of(has_type_t4), !IO),

        io.print("has_type_t1 = ", !IO), io.print_line(has_type_t1, !IO),
        io.print("has_type_t2 = ", !IO), io.print_line(has_type_t2, !IO),
        io.print("has_type_t3 = ", !IO), io.print_line(has_type_t3, !IO),
        io.print("has_type_t4 = ", !IO), io.print_line(has_type_t4, !IO).

    :- end_module nested_t2.child.

:- type foo
    --->    bar
    ;       baz(int).

hello(!IO) :-
    io.print("nested_t2.hello\n", !IO).

:- end_module nested_t2.
