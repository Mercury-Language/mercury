%-----------------------------------------------------------------------------%
%
% This is an example of how to use the `lazy' module to define
% a recursive lazy data type, in this case lazy lists.
% It also defines a small number of functions and predicates
% that operate on lazy lists.
%
% See also lazy_list_test.m, which is an example program using this module.
%
% This source file is hereby placed in the public domain.  -fjh (the author).

:- module lazy_list.
:- interface.
:- import_module lazy, int, list.

%-----------------------------------------------------------------------------%

	% The definition of the type `lazy_list(T)':
	%	A lazy lazy_list is either an empty lazy_list, denoted `[]',
	%	or an element `Head' of type `T' followed by a lazily
	%	evaluated tail `Tail', of type `lazy(lazy_list(T))',
	%	denoted `[Head | Tail]'.

:- type lazy_list(T) ---> [] ; [T | lazy(lazy_list(T))].

:- inst lazy_list(I) ---> [] ; [I | lazy(lazy_list(I))].
:- inst lazy_list == lazy_list(ground).

:- inst nonempty_lazy_list(I) ---> [I | lazy(lazy_list(I))].
:- inst nonempty_lazy_list == nonempty_lazy_list(ground).

%-----------------------------------------------------------------------------%

	% force evaluation of (the top level of) a lazy list
:- func force_list(lazy(lazy_list(T))) = lazy_list(T).
:- mode force_list(in(lazy(lazy_list))) = out(lazy_list) is det.

%-----------------------------------------------------------------------------%

	% Convert a lazy_list to an ordinary list.
:- func to_list(lazy_list(T)) = list(T).
:- mode to_list(in(lazy_list)) = out is det.

	% Convert an ordinary list to a lazy_list.
:- func from_list(list(T)) = lazy_list(T).
:- mode from_list(in) = out(lazy_list) is det.

%-----------------------------------------------------------------------------%

	% A lazy_list function version of the usual append predicate:
	% append(Start, End) = List is true iff
	% `List' is the result of concatenating `Start' and `End'.
	%
:- func append(lazy_list(T), lazy(lazy_list(T))) = lazy_list(T).
:- mode append(in(lazy_list), in(lazy(lazy_list))) = out(lazy_list)
	is det.

	% member(Elem, List) :
	%	True iff `List' contains `Elem'.
:- pred member(T, lazy_list(T)).
:- mode member(in, in(lazy_list)) is semidet.
:- mode member(out, in(nonempty_lazy_list)) is multi.
:- mode member(out, in(lazy_list)) is nondet.

%-----------------------------------------------------------------------------%

	% iterate(F, X0) = [X0, F(X0), F(F(X0)), F(F(F(X0))), ...]
:- func iterate(func(T) = T, T) = lazy_list(T).
:- mode iterate(func(in) = out is det, in) = out(lazy_list) is det.

	% take(N, L) returns the first N elements of L
:- func take(int, lazy_list(T)) = lazy_list(T).
:- mode take(in, in(lazy_list)) = out(lazy_list) is det.

	% map(F, [X0, X1, X2, ...]) = [F(X0), F(X1), F(X2), ...].
:- func map(func(X) = Y, lazy_list(X)) = lazy_list(Y).
:- mode map(func(in) = out is det, in(lazy_list)) = out(lazy_list) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

force_list(Xs) = list_inst_cast(force(Xs)).

% Because the Mercury mode system is not properly polymorphic,
% it doesn't always infer the right inst.  We sometimes need
% to use inst casts (which can be implemented using `pragma c_code').
% :-(

:- func list_inst_cast(lazy_list(T)) = lazy_list(T).
:- mode list_inst_cast(in) = out(lazy_list) is det.

:- pragma c_code(list_inst_cast(F::in) = (F2::out(lazy_list)),
	[will_not_call_mercury, thread_safe], "F2 = F;").

%-----------------------------------------------------------------------------%

to_list([]) = [].
to_list([X | Xs]) = [X | to_list(force_list(Xs))].

from_list([]) = [].
from_list([X | Xs]) =
	[X | delay((func) = R :- R = from_list(Xs))].

%-----------------------------------------------------------------------------%

append([], Ys) = force_list(Ys).
append([X | Xs], Ys) =
	[X | delay((func) = R :- R = append(force_list(Xs), Ys))].

member(X, [X | _]).
member(X, [_ | Xs]) :-
	member(X, force_list(Xs)).

%-----------------------------------------------------------------------------%

map(_, []) = [].
map(F, [H|T]) = [F(H) | delay((func) = R :- R = map(F, force_list(T)))].

iterate(F, X0) = [X0 | delay((func) = R :- R = iterate(F, F(X0)))].

take(_, []) = [].
take(N, [X|Xs]) =
	(if N > 0 then
		[X | delay((func) = R :- R = take(N-1, force_list(Xs)))]
	else
		[]
	).

%-----------------------------------------------------------------------------%
