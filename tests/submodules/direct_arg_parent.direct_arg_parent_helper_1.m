%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_parent.direct_arg_parent_helper_1.
:- interface.

:- import_module io.

:- type foo
    --->    foo(int, int).

:- pred write_maybe_foo(maybe_foo::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma no_inline(write_maybe_foo/3).

write_maybe_foo(M, !IO) :-
    (
        M = no,
        io.write_string("no\n", !IO)
    ;
        M = not_possible(Foo),
        io.write_string("not_possible(", !IO),
        io.write(Foo, !IO),
        io.write_string(")\n", !IO)
    ;
        M = forced(Foo),
        io.write_string("forced(", !IO),
        io.write(Foo, !IO),
        io.write_string(")\n", !IO)
    ).
