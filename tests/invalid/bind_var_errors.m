:- module bind_var_errors.
:- interface.

:- import_module int.

:- pred bind_var_in_negation is semidet.

:- pred bind_var_in_ite_cond(int :: in) is semidet.

:- pred bind_var_in_lambda is semidet.

% :- pred share_var_in_lambda(T :: di) is det.

:- pred clobber_var_in_lambda(T :: di) is det.

:- implementation.

:- pragma no_inline(consume/1).
:- pred consume(T :: in) is det.
consume(_).

:- pragma no_inline(destroy/1).
:- pred destroy(T :: di) is det.
destroy(_).

:- pragma no_inline(share/1).
:- pred share(T :: in) is det.
share(_).

bind_var_in_negation :-
	\+ (X = 42),
	consume(X).

bind_var_in_ite_cond(X) :-
	(
		X = 42,
		Y = 42
	->
		true
	;
		true
	),
	consume(Y).

bind_var_in_lambda :-
	call((pred) is det :- Y = 42),
	consume(Y).

	% The compiler currently does not pass this test.  It should
	% report an error but doesn't.
% share_var_in_lambda(X) :-
% 	call((pred) is det :- share(X)),
% 	destroy(X).

clobber_var_in_lambda(X) :-
	call((pred) is det :- destroy(X)),
	destroy(X).

