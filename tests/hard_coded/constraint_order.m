% Test that constraint propagation maintains unique mode correctness.
% The calls to q/2 and test/1 in p/2 must not be reordered.
:- module constraint_order.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	( { p(3, Y) } ->
		io__write_string("succeeded: "),
		io__write_int(Y),
		io__nl
	;
		io__write_string("failed\n")
	).

:- pred p(int::di, int::out) is semidet.

p(X, Y) :- q(X, Y), test(X).

:- pred q(int::ui, int::out) is det.
:- pragma promise_pure(q/2).
:- pragma no_inline(q/2).
:- pragma terminates(q/2).

q(_, 1) :- impure unsafe_write_string("call to q\n").

:- pred test(int::di) is semidet.
:- pragma promise_pure(test/1).
:- pragma no_inline(test/1).
:- pragma terminates(test/1).

test(3) :- impure unsafe_write_string("call to test\n").

:- impure pred unsafe_write_string(string::in) is det.

:- pragma c_code(unsafe_write_string(Str::in), "printf(Str);").
	
