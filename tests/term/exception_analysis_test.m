	% This module performs a basic test of the compiler's
	% exception analysis.
	%
:- module exception_analysis_test.

:- interface.

	% Uses user-defined equality that may throw an exception
	% but will not throw an exception itself.
:- pred test1(T::in) is det.
	
	% Uses user-defined equality that throws an exception
	% and throws a (type) exception.
:- pred test2(T::in, T::in) is semidet.

	% Conditional.
	%
:- pred test3(T::in, T::in) is semidet.

	% Throws a user exception.
	%
:- pred test4(T::in, T::in) is semidet.

	%  This will be conditional...if we had
	% a more precise analysis we could work out 
	% that it is only conditional if you enter
	% the SCC via the predicate mutual_test1.  
	% If we enter via mutual_test2 then an
	% exception will never be thrown.
:- pred mutual_test1(T::in, T::in) is semidet.

:- pred mutual_test2(int::in, int::in) is semidet.

:- implementation.

:- import_module require.

:- type wrap(T) ---> wrap(T) where equality is wrap_equals.

:- pred wrap_equals(wrap(T)::in, wrap(T)::in) is semidet.

wrap_equals(_, _) :- error("Type exception.").
 
test1(T) :- test1(wrap(T)).

test2(X, Y) :-
	( X = Y ->
		true
	;
		test2(wrap(X), wrap(Y))
	).

test3(X, Y) :-
	( X = Y ->
		true
	;
		test3(X, Y)
	).

test4(X, Y) :-
	( X = Y ->
		error("User exception.")
	;
		test4(X, Y)
	).

mutual_test1(X::in, Y::in) :-
	( X = Y ->
		mutual_test2(3, 4)
	;
		mutual_test1("hello", "world")
	).

mutual_test2(X::in, Y::in) :-
	( X = Y ->
		mutual_test1(500, 400)
	;
		mutual_test2(60, 20)
	).

:- end_module exception_analysis_test.
