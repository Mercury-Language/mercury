%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% "Hello World" in Mercury, using nested modules.

:- module nested_t1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

    :- module nested_t1.child.
    :- interface.
    :- import_module io.

    :- type foo
        --->    bar
        ;       baz(int).

    :- pred hello(io::di, io::uo) is det.

    :- implementation.

    hello(!IO) :-
        io.write_string("nested_t1.child.hello\n", !IO).

    :- end_module nested_t1.child.

%---------------------------------------------------------------------------%

    :- module nested_t1.child2.
    :- interface.
    :- import_module io.

    :- type foo
        --->    bar
        ;       baz(int).

    :- pred hello(io::di, io::uo) is det.

    :- implementation.

    hello(!IO) :-
        io.write_string("nested_t1.child2.hello\n", !IO).

    :- end_module nested_t1.child2.

%---------------------------------------------------------------------------%

% Now we are back in the parent module.

:- import_module nested_t1.child.
:- use_module nested_t1.child2.
:- import_module require.
:- import_module std_util.
:- import_module type_desc.

:- type t1 == nested_t1.child.foo.
:- type t2 == child.foo.
:- type t3 == foo.
:- type t4 == nested_t1.child2.foo.
:- type t5 == nested_t1.child2.foo.

main(!IO) :-
    nested_t1.child.hello(!IO),
    child.hello(!IO),
    hello(!IO),
    nested_t1.child2.hello(!IO),
    child2.hello(!IO),

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

has_type_t1 = nested_t1.child.bar.
has_type_t2 = child.bar.
has_type_t3 = bar.
has_type_t4 = nested_t1.child2.bar.
has_type_t5 = child2.bar.

:- end_module nested_t1.
