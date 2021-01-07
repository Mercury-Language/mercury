%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test.
%
% The Mercury compiler of 27/10/2000 failed this test due to overeager
% specialization of unifications involving no-tag types with user-defined
% equality.

:- module user_defined_equality2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module std_util.

:- type foo ---> foo(int)
    where equality is foo_equal.

:- pred foo_equal(foo::in, foo::in) is semidet.

foo_equal(_, _) :-
    semidet_succeed.

main(!IO) :-
    ( if unify_no_tag(foo(1), foo(2)) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred unify_no_tag(T::in, T::in) is semidet.
:- pragma type_spec(unify_no_tag/2, T = foo).

unify_no_tag(T, T).
