%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the use of module names that are C/C++/Java
% keywords, namely `int', `char', and `class'.  These might cause
% problems for back-ends targetting C/Java.

:- module class.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- module class.char.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("class.char.hello\n", !IO).

:- end_module class.char.

%---------------------------------------------------------------------------%

:- module class.int.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("class.int.hello\n", !IO).

:- end_module class.int.

%---------------------------------------------------------------------------%

% Now we are back in the parent module.

:- import_module class.char.
:- use_module class.int.
:- import_module type_desc.
:- import_module std_util.
:- import_module require.

:- type t1 == class.char.foo.
:- type t2 == char.foo.
:- type t3 == foo.
:- type t4 == class.int.foo.
:- type t5 == class.int.foo.    % was int.foo, but that is not fully qualified

main(!IO) :-
    class.char.hello(!IO),
    char.hello(!IO),
    hello(!IO),
    class.int.hello(!IO),
    int.hello(!IO),

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

has_type_t1 = class.char.bar.
has_type_t2 = char.bar.
has_type_t3 = bar.
has_type_t4 = class.int.bar.
has_type_t5 = int.bar.

:- end_module class.
