%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test potential problems with direct argument type representation and
% submodules.

:- module direct_arg_parent.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module direct_arg_parent.direct_arg_sub.
:- import_module direct_arg_parent.direct_arg_sub.

:- type maybe_foo
    --->    no
    ;       not_possible(foo)
    ;       forced(foo)
    where direct_arg is [forced/1].

%---------------------------------------------------------------------------%

main(!IO) :-
    M1 = not_possible(foo(one, 1)),
    M2 = forced(foo(one, 2)),
    direct_arg_sub.write_maybe_foo(M1, !IO),
    direct_arg_sub.write_maybe_foo(M2, !IO).

:- func one = int.
:- pragma no_inline(one/0).

one = 1.
