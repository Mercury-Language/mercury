% Various checks that impurity declarations are treated properly.

:- module purity.
:- interface.
:- import_module io.
:- impure pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, list, string.

main -->
	impure test1,
	impure test2,
	impure test3,
	impure test1_inline,
	impure test2_inline,
	impure test3_inline.


:- impure pred test1(io__state::di, io__state::uo) is det.
:- impure pred test2(io__state::di, io__state::uo) is det.
:- impure pred test3(io__state::di, io__state::uo) is det.

:- impure pred test1_inline(io__state::di, io__state::uo) is det.
:- impure pred test2_inline(io__state::di, io__state::uo) is det.
:- impure pred test3_inline(io__state::di, io__state::uo) is det.


:- impure pred set_x(int::in) is det.
:- pragma c_code(set_x(X::in), will_not_call_mercury, "x=X;" ).
:- pragma no_inline(set_x/1).

:- impure pred incr_x is det.
:- pragma c_code(incr_x, will_not_call_mercury, "++x;" ).
:- pragma no_inline(incr_x/0).

:- semipure pred get_x(int::out) is det.
:- pragma c_code(get_x(X::out), will_not_call_mercury, "X=x;").
:- pragma no_inline(get_x/1).


:- impure pred set_x_inline(int::in) is det.
:- pragma c_code(set_x_inline(X::in), will_not_call_mercury, "x=X;" ).
:- pragma inline(set_x_inline/1).

:- impure pred incr_x_inline is det.
:- pragma c_code(incr_x_inline, will_not_call_mercury, "++x;" ).
:- pragma inline(incr_x_inline/0).

:- semipure pred get_x_inline(int::out) is det.
:- pragma c_code(get_x_inline(X::out), will_not_call_mercury, "X=x;").
:- pragma inline(get_x_inline/1).


:- pragma c_header_code("int x = 0;").


% tempt compiler to optimize away duplicate semipure goals.
test1 -->
	{ semipure get_x(X) },
	io__format("%d\n", [i(X)]),
	{ impure set_x(X+1) },
	{ semipure get_x(Y) },
	io__format("%d\n", [i(Y)]).


% tempt compiler to optimize away duplicate impure goals, or to compile away
% det goals with no outputs.
test2 -->
	{ impure incr_x },
	{ impure incr_x },
	{ semipure get_x(Y) },
	io__format("%d\n", [i(Y)]).

% tempt compiler to optimize away impure goal in branch that cannot succeed.
test3 -->
	(   { impure incr_x },
	    { fail }
	;   { semipure get_x(Y) },
	    io__format("%d\n", [i(Y)])
	).

%  Now do it all again with inlining requested

test1_inline -->
	{ semipure get_x_inline(X) },
	io__format("%d\n", [i(X)]),
	{ impure set_x_inline(X+1) },
	{ semipure get_x_inline(Y) },
	io__format("%d\n", [i(Y)]).


% tempt compiler to optimize away duplicate impure goals, or to compile away
% det goals with no outputs.
test2_inline -->
	{ impure incr_x_inline },
	{ impure incr_x_inline },
	{ semipure get_x_inline(Y) },
	io__format("%d\n", [i(Y)]).

% tempt compiler to optimize away impure goal in branch that cannot succeed.
test3_inline -->
	(   { impure incr_x_inline },
	    { fail }
	;   { semipure get_x_inline(Y) },
	    io__format("%d\n", [i(Y)])
	).

