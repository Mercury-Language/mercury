
% Subverting the Mercury purity system.

% This should not be possible.

:- module impure_pred_t2.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require, list.

main -->
	{ Y = get_counter },
	{ Y(4, Z) },
	print("X = "), 
	print(Z), 
	nl.

:- impure pred get_counter(int::in, int::out) is det.

:- pragma foreign_decl("C", "extern MR_Integer counter;").
:- pragma foreign_code("C", "MR_Integer counter = 0;").
:- pragma foreign_proc("C",
	get_counter(Y::in, X::out),
	[will_not_call_mercury],
"
	X = counter + Y;
").
get_counter(X, X).
