%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: term.nl.
% Main author: fjh.

% This file provides a type `term' used to represent Prolog terms,
% and various predicates to manipulate terms and substitutions.

%-----------------------------------------------------------------------------%

:- module term.
:- interface.
:- import_module int, string, float, list, map.

%-----------------------------------------------------------------------------%

:- type term		--->	term__functor(const, list(term), term__context)
			;	term__variable(var).
:- type const 		--->	term__atom(string)
			;	term__integer(int)
			;	term__string(string)
			;	term__float(float).
:- type comparison	--->	(>)
			;	(<)
			;	(=).

:- type var.
:- type var_supply.
:- type term__context.

%-----------------------------------------------------------------------------%

:- pred term__vars(term, list(var)).
:- mode term__vars(in, out) is det.
%	term__vars(Term, Vars)
%		Vars is the list of variables contained in Term, in the order 
%		obtained by traversing the term depth first, left-to-right.

:- pred term__vars_2(term, list(var), list(var)).
:- mode term__vars_2(in, in, out) is det.
%		As above, but with an accumulator.

:- pred term__vars_list(list(term), list(var)).
:- mode term__vars_list(in, out) is det.
%	term__vars_list(TermList, Vars)
%		Vars is the list of variables contained in TermList, in the
%		order obtained by traversing the list of terms depth-first,
%		left-to-right.

:- pred term__contains_var(term, var).
:- mode term__contains_var(in, in) is semidet.
:- mode term__contains_var(in, out) is nondet.
%	term__contains_var(Term, Var)
%		True if Term contains Var. (On backtracking returns all the 
%		variables contained in Term.)

:- pred term__contains_var_list(list(term), var).
:- mode term__contains_var_list(in, in) is semidet.
:- mode term__contains_var_list(in, out) is nondet.
%	term__contains_var_list(TermList, Var)
%		True if TermList contains Var. (On backtracking returns all the 
%		variables contained in Term.)

:- type substitution == map(var, term).

:- pred term__unify(term, term, substitution, substitution).
:- mode term__unify(in, in, in, out).
%	term__unify(Term1, Term2, Bindings0, Bindings)
%		unify (with occur check) two terms with respect to a set
%	 	of bindings and possibly update the set of bindings

:- pred term__substitute(term, var, term, term).
:- mode term__substitute(in, in, in, out).
%	term__substitute(Term0, Var, Replacement, Term) :
%		replace all occurrences of Var in Term0 with Replacement,
%		and return the result in Term.

:- pred term__substitute_list(list(term), var, term, list(term)).
:- mode term__substitute_list(in, in, in, out).
%		as above, except for a list of terms rather than a single term

:- pred term__substitute_corresponding(list(var), list(term), term, term).
:- mode term__substitute_corresponding(in, in, in, out).
%       term__substitute_corresponding(Vars, Repls, Term0, Term).
%		replace all occurrences of variables in Vars with
%		the corresponding term in Repls (which should be the
%		same length as Vars!), and return the result in Term.

:- pred term__apply_rec_substitution(term, substitution, term).
:- mode term__apply_rec_substitution(in, in, out) is det.
%	term__apply_rec_substitution(Term0, Substitution, Term) :
%		recursively apply substitution to Term0 until
%		no more substitions can be applied, and then
%		return the result in Term.

:- pred term__apply_substitution(term, substitution, term).
:- mode term__apply_substitution(in, in, out) is det.
%	term__apply_substitution(Term0, Substitution, Term) :
%		apply substitution to Term0 and return the result in Term.

:- pred term__apply_substitution_to_list(list(term), substitution, list(term)).
:- mode term__apply_substitution_to_list(in, in, out).
%	term__apply_substitution_to_list(TermList0, Substitution, TermList) :
%		as above, except for a list of terms rather than a single term


:- pred term__occurs(term, var, substitution).
:- mode term__occurs(in, in, in).
%	term__occurs(Term0, Var, Substitution) :
%		true iff Var occurs in the term resulting after
%		applying Substitution to Term0.

:- pred term__occurs_list(list(term), var, substitution).
:- mode term__occurs_list(in, in, in).
%		as above, except for a list of terms rather than a single term

:- pred term__relabel_variable(term, var, var, term).
:- mode term__relabel_variable(in, in, in, out) is det.
%	term__relabel_variable(Term0, OldVar, NewVar, Term) :
%		replace all occurences of OldVar in Term0 with
%		NewVar and put the result in Term.

:- pred term__relabel_variables(list(term), var, var, list(term)).
:- mode term__relabel_variables(in, in, in, out) is det.
%	term__relabel_variables(Terms0, OldVar, NewVar, Terms) :
%		same as term__relabel_variable but for a list of terms.


:- pred term__is_ground(term, substitution).
:- mode term__is_ground(in, in).
%	term__is_ground(Term, Bindings) is true iff no variables contained
%		in Term are non-ground in Bindings.

:- pred term__compare(comparison, term, term, substitution).
:- mode term__compare(out, in, in, in).
%	term__compare(Comparison, Term1, Term2, Bindings) is true iff
%		there is a binding of Comparison to <, =, or > such
%		that the binding holds for the two ground terms Term1
%		and Term2 with respect to the bindings in Bindings.

%-----------------------------------------------------------------------------%

	% To manage a supply of variables, use the following 2 predicates.
	% (We might want to give these a unique mode later.)

:- pred term__init_var_supply(var_supply).
:- mode term__init_var_supply(out) is det.
%	term__init_var_supply(VarSupply) :
%		returns a fresh var_supply for producing fresh variables.

:- pred term__create_var(var_supply, var, var_supply).
:- mode term__create_var(in, out, out) is det.
%	term__create_var(VarSupply0, Variable, VarSupply) :
%		create a fresh variable (var) and return the
%		updated var_supply.

:- pred term__var_to_int(var, int).
:- mode term__var_to_int(in, out).
%		Convert a variable to an int.
%		Different variables map to different ints.
%		Other than that, the mapping is unspecified.
	
%-----------------------------------------------------------------------------%

	% Given a term context, return the source line number.

:- pred term__context_line(term__context, int).
:- mode term__context_line(in, out).

	% Given a term context, return the source file.

:- pred term__context_file(term__context, string).
:- mode term__context_file(in, out).

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.

:- pred term__context_init(int, term__context).
:- mode term__context_init(in, out).

:- pred term__context_init(string, int, term__context).
:- mode term__context_init(in, in, out).

	% Convert a list of terms which are all vars into a list
	% of vars (or vice versa).

:- pred term_list_to_var_list(list(term), list(var)).
:- mode term_list_to_var_list(in, out) is semidet.
:- mode term_list_to_var_list(out, in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- type var		==	int.
:- type var_supply	==	int.

%-----------------------------------------------------------------------------%

	% term__vars(Term, Vars) is true if Vars is the list of variables
	% contained in Term obtained by depth-first left-to-right traversal
	% in that order.

term__vars(Term, Vars) :-
	term__vars_2(Term, [], Vars).

term__vars_list(Terms, Vars) :-
	term__vars_2_list(Terms, [], Vars).

term__vars_2(term__variable(V), Vs, [V|Vs]).
term__vars_2(term__functor(_,Args,_), Vs0, Vs) :-
	term__vars_2_list(Args, Vs0, Vs).

:- pred term__vars_2_list(list(term), list(var), list(var)).
:- mode term__vars_2_list(in, in, out) is det.

term__vars_2_list([], Vs, Vs).
term__vars_2_list([T|Ts], Vs0, Vs) :-
	term__vars_2_list(Ts, Vs0, Vs1),
	term__vars_2(T, Vs1, Vs).

%-----------------------------------------------------------------------------%

	% term__contains_var(Term, Var) is true if Var occurs in Term.

term__contains_var(term__variable(V), V).
term__contains_var(term__functor(_, Args, _), V) :-
	term__contains_var_list(Args, V).

term__contains_var_list(T._, V) :-
	term__contains_var(T, V).
term__contains_var_list(_.Ts, V) :-
	term__contains_var_list(Ts, V).

%-----------------------------------------------------------------------------%

	% term__contains_functor(Term, Functor, Args):
	%	term__functor(Functor, Args, _) is a subterm of Term.
	%
	% CURRENTLY NOT USED.

:- pred term__contains_functor(term, const, list(term)).
:- mode term__contains_functor(in, in, in) is semidet.
:- mode term__contains_functor(in, out, out) is nondet.

term__contains_functor(term__functor(Functor, Args, _), Functor, Args).
term__contains_functor(term__functor(_, Args, _), SubFunctor, SubArgs) :-
 	list__member(SubTerm, Args),
 	term__contains_functor(SubTerm, SubFunctor, SubArgs).

%-----------------------------------------------------------------------------%

	% term__subterm(Term, SubTerm):
	%	SubTerm is a subterm of Term.
	%
	% CURRENTLY NOT USED.

:- pred term__subterm(term, term).
:- mode term__subterm(in, in) is semidet.
:- mode term__subterm(in, out) is nondet.

term__subterm(Term, Term).
term__subterm(term__functor(_, Args, _), SubTerm) :-
	list__member(Term, Args),
	term__subterm(Term, SubTerm).

%-----------------------------------------------------------------------------%

	% Access predicates for the term__context data structure.
	% At the moment, the only context we store is the line
	% number (and even that is only the line number at the
	% end of the entire read-term).

:- type term__context	--->	term__context(string, int).
				% file, line number.

	% Given a term context, return the source line number.

term__context_line(term__context(_, LineNumber), LineNumber).

	% Given a term context, return the source file.

term__context_file(term__context(FileName, _), FileName).

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.
	% term__context_init/3 is for old code; use
	% term__context_init/4 if possible.
	% XXX should this be /2?

term__context_init(LineNumber, term__context("", LineNumber)).

term__context_init(File, LineNumber, term__context(File, LineNumber)).

%-----------------------------------------------------------------------------%

:- interface.

:- pred term__get_int(term, int).
:- mode term__get_int(in, out) is semidet.

:- pred term__get_string(term, string).
:- mode term__get_string(in, out) is semidet.

:- implementation.

term__get_int(term__functor(term__integer(Int), _, _), Int).

term__get_string(term__functor(term__string(String), _, _), String).

%-----------------------------------------------------------------------------%

	% Unify two terms (with occurs check), updating the bindings of
	% the variables in the terms.  

:- term__unify(X, Y, _, _) when X and Y.		% NU-Prolog indexing

term__unify(term__variable(X), term__variable(Y), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		( %%% if some [BindingOfY]
			map__search(Bindings0, Y, BindingOfY)
		->
			% both X and Y already have bindings - just
			% unify the terms they are bound to
			term__unify(BindingOfX, BindingOfY, Bindings0, Bindings)
		;
			% Y is a variable which hasn't been bound yet
			term__apply_rec_substitution(BindingOfX, Bindings0,
				SubstBindingOfX),
			( SubstBindingOfX = term__variable(Y) ->
			 	Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfX, Y, Bindings0),
				map__set(Bindings0, Y, SubstBindingOfX,
					Bindings)
			)
		)
	;
		( %%% if some [BindingOfY2]
			map__search(Bindings0, Y, BindingOfY2)
		->
			% X is a variable which hasn't been bound yet
			term__apply_rec_substitution(BindingOfY2, Bindings0,
				SubstBindingOfY2),
			( SubstBindingOfY2 = term__variable(X) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfY2, X, Bindings0),
				map__set(Bindings0, X, SubstBindingOfY2,
					Bindings)
			)
		;
			% both X and Y are unbound variables -
			% bind one to the other
			( X = Y ->
				Bindings = Bindings0
			;
				map__set(Bindings0, X, term__variable(Y),
					Bindings)
			)
		)
	).

term__unify(term__variable(X), term__functor(F, As, C), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		term__unify(BindingOfX, term__functor(F, As, C), Bindings0,
			Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term__functor(F, As, C), Bindings)
	).

term__unify(term__functor(F, As, C), term__variable(X), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		term__unify(term__functor(F, As, C), BindingOfX, Bindings0,
			Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term__functor(F, As, C), Bindings)
	).

term__unify(term__functor(F, AsX, _), term__functor(F, AsY, _)) -->
	term__unify_list(AsX, AsY).

:- pred term__unify_list(list(term), list(term), substitution, substitution).
:- mode term__unify_list(in, in, in, out).

term__unify_list([], []) --> [].
term__unify_list([X | Xs], [Y | Ys]) -->
	term__unify(X, Y),
	term__unify_list(Xs, Ys).

%-----------------------------------------------------------------------------%

	% term__occurs(Term, Var, Subst) succeeds if Term contains Var,
	% perhaps indirectly via the substitution.  (The variable must
	% not be mapped by the substitution.)

term__occurs(term__variable(X), Y, Bindings) :-
	(
		X = Y
	->
		true
	;
		map__search(Bindings, X, BindingOfX),
		term__occurs(BindingOfX, Y, Bindings)
	).
term__occurs(term__functor(_F, As, _), Y, Bindings) :-
	term__occurs_list(As, Y, Bindings).

term__occurs_list([Term | Terms], Y, Bindings) :-
	(
		term__occurs(Term, Y, Bindings)
	->
		true
	;
		term__occurs_list(Terms, Y, Bindings)
	).

%-----------------------------------------------------------------------------%

	% term__substitute(Term0, Var, Replacement, Term) :
	%	replace all occurrences of Var in Term0 with Replacement,
	%	and return the result in Term.

term__substitute(term__variable(Var), SearchVar, Replacement, Term) :-
	(
		Var = SearchVar
	->
		Term = Replacement
	;
		Term = term__variable(Var)
	).
term__substitute(term__functor(Name, Args0, Context), Var, Replacement,
		 term__functor(Name, Args, Context)) :-
	term__substitute_list(Args0, Var, Replacement, Args).

term__substitute_list([], _Var, _Replacement, []).
term__substitute_list([Term0 | Terms0], Var, Replacement, [Term | Terms]) :-
	term__substitute(Term0, Var, Replacement, Term),
	term__substitute_list(Terms0, Var, Replacement, Terms).

term__substitute_corresponding(Ss, Rs, Term0, Term) :-
	map__init(Subst0),
	term__substitute_corresponding_2(Ss, Rs, Subst0, Subst),
	term__apply_substitution(Term0, Subst, Term).

:- pred term__substitute_corresponding_2(list(var), list(term),
					substitution, substitution).
:- mode term__substitute_corresponding_2(in, in, in, out).

term__substitute_corresponding_2([], [], Subst, Subst).
term__substitute_corresponding_2([S | Ss], [R | Rs], Subst0, Subst) :-
	map__insert(Subst0, S, R, Subst1),
	term__substitute_corresponding_2(Ss, Rs, Subst1, Subst).

%-----------------------------------------------------------------------------%

term__apply_rec_substitution(term__variable(Var), Substitution, Term) :-
	(
		%some [Replacement]
		map__search(Substitution, Var, Replacement)
	->
		% recursively apply the substition to the replacement
		term__apply_rec_substitution(Replacement, Substitution, Term)
	;
		Term = term__variable(Var)
	).
term__apply_rec_substitution(term__functor(Name, Args0, Context), Substitution,
		 term__functor(Name, Args, Context)) :-
	term__apply_rec_substitution_to_list(Args0, Substitution, Args).

:- pred term__apply_rec_substitution_to_list(list(term), substitution,
						list(term)).
:- mode term__apply_rec_substitution_to_list(in, in, out).

term__apply_rec_substitution_to_list([], _Substitution, []).
term__apply_rec_substitution_to_list([Term0 | Terms0], Substitution,
		[Term | Terms]) :-
	term__apply_rec_substitution(Term0, Substitution, Term),
	term__apply_rec_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

term__apply_substitution(term__variable(Var), Substitution, Term) :-
	(
		%some [Replacement]
		map__search(Substitution, Var, Replacement)
	->
		Term = Replacement
	;
		Term = term__variable(Var)
	).
term__apply_substitution(term__functor(Name, Args0, Context), Substitution,
		 term__functor(Name, Args, Context)) :-
	term__apply_substitution_to_list(Args0, Substitution, Args).

term__apply_substitution_to_list([], _Substitution, []).
term__apply_substitution_to_list([Term0 | Terms0], Substitution,
		[Term | Terms]) :-
	term__apply_substitution(Term0, Substitution, Term),
	term__apply_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

	% create a new supply of variables
term__init_var_supply(0).

	% create a fresh [unique] variable.
term__create_var(VarSupply0, VarSupply0, VarSupply) :-
	VarSupply is VarSupply0 + 1.

%-----------------------------------------------------------------------------%

	% variables are represented as ints, so there's nothing to do.

term__var_to_int(Var, Var).

%-----------------------------------------------------------------------------%

	% substitute a variable name in a term.
term__relabel_variable(term__functor(Const, Terms0, Cont), OldVar, NewVar,
				term__functor(Const, Terms, Cont)) :-
	term__relabel_variables(Terms0, OldVar, NewVar, Terms).
term__relabel_variable(term__variable(Var0), OldVar, NewVar,
				term__variable(Var)) :-
	(
		Var0 = OldVar
	->
		Var = NewVar
	;
		Var = Var0
	).

term__relabel_variables([], _, _, []).
term__relabel_variables([Term0|Terms0], OldVar, NewVar, [Term|Terms]):-
	term__relabel_variable(Term0, OldVar, NewVar, Term),
	term__relabel_variables(Terms0, OldVar, NewVar, Terms).

%-----------------------------------------------------------------------------%

:- term_list_to_var_list(Terms, Vars) when Terms or Vars. % Indexing

term_list_to_var_list([], []).
term_list_to_var_list([term__variable(Var) | Terms], [Var | Vars]) :-
	term_list_to_var_list(Terms, Vars).

%-----------------------------------------------------------------------------%

term__is_ground(term__variable(V), Bindings) :-
	map__search(Bindings, V, Binding),
	term__is_ground(Binding, Bindings).
term__is_ground(term__functor(_, Args, _), Bindings) :-
	term__is_ground_2(Args, Bindings).

:- pred term__is_ground_2(list(term), substitution).
:- mode term__is_ground_2(in, in).

term__is_ground_2([], _Bindings).
term__is_ground_2([Term|Terms], Bindings) :-
	term__is_ground(Term, Bindings),
	term__is_ground_2(Terms, Bindings).
	
%-----------------------------------------------------------------------------%

term__compare(Cmp, Term1, Term2, Bindings) :-
	term__apply_rec_substitution(Term1, Bindings, TermA),
	term__is_ground(TermA, Bindings),
	term__apply_rec_substitution(Term2, Bindings, TermB),
	term__is_ground(TermB, Bindings),
	compare(Cmp0, TermA, TermB),
	(
		Cmp0 = (=)
	->
		Cmp = (=)
	;
		Cmp0 = (<)
	->
		Cmp = (<)
	;
		Cmp = (>)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
