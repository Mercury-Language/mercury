% This is a regression test.
%
% The Mercury compiler of 27/10/2000 failed this test
% due to overeager specialization of unifications
% involving no-tag types with user-defined equality.

:- module user_defined_equality2.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module std_util.

:- type foo ---> foo(int)
	where equality is foo_equal.

:- pred foo_equal(foo::in, foo::in) is semidet.

foo_equal(_, _) :-
	semidet_succeed.

main -->
	( { unify_no_tag(foo(1), foo(2)) } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred unify_no_tag(T::in, T::in) is semidet.
:- pragma type_spec(unify_no_tag/2, T = foo).

unify_no_tag(T, T).
