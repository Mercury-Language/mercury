:- module foreign_purity_mismatch.
:- interface.

:- import_module string.

:- pred pure_with_impure(string::in) is det.
:- pred pure_with_semipure(string::in) is det.

:- semipure pred semipure_with_impure(string::in) is det.
:- semipure pred semipure_with_pure(string::in) is det.

	% This one was particularly bad since the compiler was
	% optimising away the foreign_proc goal(!).
	%
:- impure pred impure_with_pure(string::in) is det.
:- impure pred impure_with_semipure(string::in) is det.

:- implementation.

:- pragma foreign_proc("C",
	pure_with_impure(S::in),
	[will_not_call_mercury],
"
	/* S */
").

:- pragma foreign_proc("C",
	pure_with_semipure(S::in),
	[will_not_call_mercury, promise_semipure],
"
	/* S */
").

:- pragma foreign_proc("C",
	semipure_with_impure(S::in),
	[will_not_call_mercury],
"
	/* S */
").

:- pragma foreign_proc("C",
	semipure_with_pure(S::in),
	[will_not_call_mercury, promise_pure],
"
	/* S */
").

:- pragma foreign_proc("C",
	impure_with_pure(S::in),
	[will_not_call_mercury, promise_pure],
"
	/* S */
").

:- pragma foreign_proc("C",
	impure_with_semipure(S::in),
	[will_not_call_mercury, promise_semipure],
"
	/* S */
").
