%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module term.

% Main author: fjh.

% This file provides a type `term' used to represent Prolog terms.
% This file also provides the following predicates:
%	term__vars(Term, Vars)
%		Vars is the list of variables contained in Term, in the order 
%		obtained by traversing the term depth first, left-to-right.
%	term__contains_var(Term, Var)
%		True if Term contains Var. (On backtracking returns all the 
%		variables contained in Term.)
%	term__unify(Term1, Term2, Binding0, Binding)
%		unify (with occur check) two terms with respect to a set
%	 	of bindings and possibly update the set of bindings
%	term__substitute(Term0, Var, Replacement, Term) :
%		replace all occurrences of Var in Term0 with Replacement,
%		and return the result in Term.

%		
%-----------------------------------------------------------------------------%

:- import_module integer, string, float.
:- export_pred  term__vars, term__contains_var,
		term__context_line, term__context_init.
:- export_type term, const, variable.

:- type term		--->	term_functor(const,list(term), term__context)
			;	term_variable(variable).
:- type const 		--->	term_atom(string)
			;	term_integer(integer)
			;	term_string(string)
			;	term_float(float).
:- type variable	==	integer.

%-----------------------------------------------------------------------------%
	% term__vars(Term, Vars) is true if Vars is the list of variables
	% contained in Term obtained by depth-first left-to-right traversal.

:- pred term__vars(term, list(var_id)).
term__vars(Term, Vars) :-
	term__vars_2(Term, [], Vars).

:- pred term__vars_2(term, list(var_id), list(var_id)).
term__vars_2(term_variable(V), Vs, V.Vs).
term__vars_2(term_functor(_,Args,_), Vs0, Vs) :-
	term__vars_2_list(Args, Vs0, Vs).

:- pred term__vars_2_list(list(term), list(var_id), list(var_id)).
term__vars_2_list([], Vs, Vs).
term__vars_2_list(T.Ts, Vs0, Vs) :-
	term__vars_2(T, Vs0, Vs1),
	term__vars_2_list(Ts, Vs1, Vs).

%-----------------------------------------------------------------------------%

	% term__contains_var(Term, Var) is true if Var occurs in Term.

:- pred term__contains_var(term, var_id).
term__contains_var(term_variable(V), V).
term__contains_var(term_functor(_, Args, _), V) :-
	term__contains_var_list(Args, V).

:- pred term__contains_var_list(list(term), var_id).
term__contains_var_list(T._, V) :-
	term__contains_var(T, V).
term__contains_var_list(_.Ts, V) :-
	term__contains_var_list(Ts, V).

%-----------------------------------------------------------------------------%

	% term__contains_functor(Term, Functor, Args):
	%	term_functor(Functor, Args, _) is a subterm of Term.

:- pred term__contains_functor(term, const, list(term)).
:- mode term__contains_functor(input, input, input).
:- mode term__contains_functor(input, output, output).
term__contains_functor(term_functor(Functor, Args, _), Functor, Args).
term__contains_functor(term_functor(_, Args, _), SubFunctor, SubArgs) :-
 	member(SubTerm, Args),
 	term__contains_functor(SubTerm, SubFunctor, SubArgs).

:- pred term__subterm(term, term).
:- mode term__subterm(input, output).
:- mode term__subterm(input, input).
term__subterm(Term, Term).
term__subterm(term_functor(_, Args, _), SubTerm) :-
	member(Term, Args),
	term__subterm(Term, SubTerm).

%-----------------------------------------------------------------------------%

	% Access predicates for the term context data structure.
	% At the moment, the only context we store is the line
	% number (and even that is only the line number at the
	% end of the entire read-term).

:- type term__context == int.

	% Given a term context, return the source line number.

:- pred term__context_line(term__context, int).
:- mode term__context_line(input, output).
term__context_line(LineNumber, LineNumber).

	% Used by io.nl to initialize the term context when reading
	% in a term.

:- pred term__context_init(int, term__context).
:- mode term__context_init(input, output).
term__context_init(LineNumber, LineNumber).

%-----------------------------------------------------------------------------%

:- pred term__get_int(term, int).
:- mode term__get_int(input, output).
term__get_int(term_functor(term_integer(Int), _, _), Int).

:- pred term__get_string(term, string).
:- mode term__get_string(input, output).
term__get_string(term_functor(term_string(String), _, _), String).

%-----------------------------------------------------------------------------%

	% Unify two terms (with occurs check), updating the bindings of
	% the variables in the terms.  

:- type substitution == map(variable, term).

:- term__unify(X, Y, _, _) when X and Y.		% NU-Prolog indexing

:- pred term__unify(term, term, substitution, substitution).
:- mode term__unify(input, input, input, output).

term__unify(term_variable(X), term_variable(Y), Bindings0, Bindings) :-
	(if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	then
		(if some [BindingOfY]
			map__search(Bindings0, Y, BindingOfY)
		then
			% both X and Y already have bindings - just
			% unify the terms they are bound to
			term__unify(BindingOfX, BindingOfY, Bindings0, Bindings)
		else
			% Y is a variable which hasn't been bound yet
			not term__occurs(BindingOfX, Y, Bindings0),
			map__set(Bindings0, Y, BindingOfX, Bindings)
		)
	else
		(if some [BindingOfY2]
			map__search(Bindings0, Y, BindingOfY2)
		then
			% X is a variable which hasn't been bound yet
			not term__occurs(BindingOfY2, X, Bindings0),
			map__set(Bindings0, X, BindingOfY2, Bindings)
		else
			% both X and Y are unbound variables -
			% bind one to the other
			(if X = Y
			then
				true
			else
				map__set(Bindings0, X, term_variable(Y),
					Bindings)
			)
		)
	).

term__unify(term_variable(X), term_functor(F, As, C), Bindings0, Bindings) :-
	(if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	then
		term__unify(BindingOfX, term_functor(F, As, C), Bindings0,
			Bindings)
	else
		not term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

term__unify(term_functor(F, As, C), term_variable(X), Bindings0, Bindings) :-
	(if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	then
		not term__occurs_list(As, X, Bindings0),
		term__unify(term_functor(F, As, C), BindingOfX, Bindings0,
			Bindings)
	else
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

term__unify(term_functor(F, AsX, _), term_functor(F, AsY, _)) -->
	term__unify_list(AsX, AsY).

:- pred term__unify_list(list(term), list(term), substitution, substitution).
:- mode term__unify_list(input, input, input, output).

term__unify_list([], []) --> [].
term__unify_list([X | Xs], [Y | Ys]) -->
	term__unify(X, Y),
	term__unify_list(Xs, Ys).

%-----------------------------------------------------------------------------%

	% succeeds if term contains variable (perhaps indirectly, via
	% the substitution).  (The variable must not be mapped by the
	% substitution.)

:- pred term__occurs(term, variable, substitution).
:- mode term__occurs(input, input, input).

term__occurs(term_variable(X), Y, Bindings) :-
	(if
		X = Y
	then
		true
	else
		map__search(Bindings, X, BindingOfX),
		term__occurs(BindingOfX, Y, Bindings)
	).
term__occurs(term_functor(_F, As, _), Y, Bindings) :-
	term__occurs_list(As, Y, Bindings).

term__occurs_list([Term | Terms], Y, Bindings) :-
	(if
		term__occurs(Term, Y, Bindings)
	then
		true
	else
		term__occurs_list(Terms, Y, Bindings)
	).

%-----------------------------------------------------------------------------%

	% term__substitute(Term0, Var, Replacement, Term) :
	%	replace all occurrences of Var in Term0 with Replacement,
	%	and return the result in Term.

:- pred term__substitute(term, variable, term, term).
:- mode term__substitute(input, input, input, output).

term__substitute(term_variable(Var), SearchVar, Replacement, Term) :-
	(if 
		Var = SearchVar
	then
		Term = Replacement
	else
		Term = term_variable(Var)
	).
term__substitute(term_functor(Name, Args0, Context), Var, Replacement,
		 term_functor(Name, Args, Context)) :-
	term__substitute_list(Args0, Var, Replacement, Args).

:- pred term__substitute_list(list(term), variable, term, list(term)).
:- mode term__substitute_list(input, input, input, output).

term__substitute_list([], _Var, _Replacement, []).
term__substitute_list([Term0 | Terms0], Var, Replacement, [Term | Terms]) :-
	term__substitute(Term0, Var, Replacement, Term),
	term__substitute_list(Terms0, Var, Replacement, Terms).

:- pred term__substitute_corresponding(list(variable), list(term), term, term).
:- mode term__substitute_corresponding(input, input, input, output).

term__substitute_corresponding([], [], Term, Term).
term__substitute_corresponding([S | Ss], [R | Rs], Term0, Term) :-
	term__substitute(Term0, S, R, Term1),
	term__substitute_corresponding(Ss, Rs, Term1, Term).

%-----------------------------------------------------------------------------%
