%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: term.m.
% Main author: fjh.
% Stability: medium.

% This file provides a type `term' used to represent Prolog terms,
% and various predicates to manipulate terms and substitutions.

%-----------------------------------------------------------------------------%

:- module term.
:- interface.
:- import_module int, string, float, list, map.

%-----------------------------------------------------------------------------%

% The term type is actually defined in mercury_builtin.m.

/*
:- type term		--->	term_functor(const, list(term), term_context)
			;	term_variable(var).
:- type const		--->	term_atom(string)
			;	term_integer(int)
			;	term_string(string)
			;	term_float(float).
*/

:- type var.
:- type var_supply.

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
:- mode term__unify(in, in, in, out) is semidet.
%	term__unify(Term1, Term2, Bindings0, Bindings)
%		unify (with occur check) two terms with respect to a set
%	 	of bindings and possibly update the set of bindings

:- pred term__substitute(term, var, term, term).
:- mode term__substitute(in, in, in, out) is det.
%	term__substitute(Term0, Var, Replacement, Term) :
%		replace all occurrences of Var in Term0 with Replacement,
%		and return the result in Term.

:- pred term__substitute_list(list(term), var, term, list(term)).
:- mode term__substitute_list(in, in, in, out) is det.
%		as above, except for a list of terms rather than a single term

:- pred term__substitute_corresponding(list(var), list(term), term, term).
:- mode term__substitute_corresponding(in, in, in, out) is det.
%       term__substitute_corresponding(Vars, Repls, Term0, Term).
%		replace all occurrences of variables in Vars with
%		the corresponding term in Repls, and return the result in Term.
%		If Vars contains duplicates, or if Vars is not the same
%	        length as Repls, the behaviour is undefined and probably
%		harmful.

:- pred term__substitute_corresponding_list(list(var), list(term), list(term),
						list(term)).
:- mode term__substitute_corresponding_list(in, in, in, out) is det.
%       term__substitute_corresponding_list(Vars, Repls, TermList0, TermList).
%		As above, except applies to a list of terms rather than a
%		single term.

:- pred term__apply_rec_substitution(term, substitution, term).
:- mode term__apply_rec_substitution(in, in, out) is det.
%	term__apply_rec_substitution(Term0, Substitution, Term) :
%		recursively apply substitution to Term0 until
%		no more substitions can be applied, and then
%		return the result in Term.

:- pred term__apply_rec_substitution_to_list(list(term), substitution,
						list(term)).
:- mode term__apply_rec_substitution_to_list(in, in, out) is det.

:- pred term__apply_substitution(term, substitution, term).
:- mode term__apply_substitution(in, in, out) is det.
%	term__apply_substitution(Term0, Substitution, Term) :
%		apply substitution to Term0 and return the result in Term.

:- pred term__apply_substitution_to_list(list(term), substitution, list(term)).
:- mode term__apply_substitution_to_list(in, in, out) is det.
%	term__apply_substitution_to_list(TermList0, Substitution, TermList) :
%		as above, except for a list of terms rather than a single term


:- pred term__occurs(term, var, substitution).
:- mode term__occurs(in, in, in) is semidet.
%	term__occurs(Term0, Var, Substitution) :
%		true iff Var occurs in the term resulting after
%		applying Substitution to Term0.

:- pred term__occurs_list(list(term), var, substitution).
:- mode term__occurs_list(in, in, in) is semidet.
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
:- mode term__is_ground(in, in) is semidet.
%	term__is_ground(Term, Bindings) is true iff no variables contained
%		in Term are non-ground in Bindings.

:- pred term__is_ground(term).
:- mode term__is_ground(in) is semidet.
%	term__is_ground(Term, Bindings) is true iff Term contains no
%		variables.

:- pred term__compare(comparison_result, term, term, substitution).
:- mode term__compare(out, in, in, in) is semidet.
%	term__compare(Comparison, Term1, Term2, Bindings) is true iff
%		there is a binding of Comparison to <, =, or > such
%		that the binding holds for the two ground terms Term1
%		and Term2 with respect to the bindings in Bindings.
%		Fails if Term1 or Term2 is not ground (with respect to
%		the bindings in Bindings).

%-----------------------------------------------------------------------------%

	% To manage a supply of variables, use the following 2 predicates.
	% (We might want to give these a unique mode later.)

:- pred term__init_var_supply(var_supply).
:- mode term__init_var_supply(out) is det.
:- mode term__init_var_supply(in) is semidet. % implied
%	term__init_var_supply(VarSupply) :
%		returns a fresh var_supply for producing fresh variables.

:- pred term__create_var(var_supply, var, var_supply).
:- mode term__create_var(in, out, out) is det.
%	term__create_var(VarSupply0, Variable, VarSupply) :
%		create a fresh variable (var) and return the
%		updated var_supply.

:- pred term__var_to_int(var, int).
:- mode term__var_to_int(in, out) is det.
%		Convert a variable to an int.
%		Different variables map to different ints.
%		Other than that, the mapping is unspecified.
	
%-----------------------------------------------------------------------------%

	% Given a term context, return the source line number.

:- pred term_context_line(term_context, int).
:- mode term_context_line(in, out) is det.

	% Given a term context, return the source file.

:- pred term_context_file(term_context, string).
:- mode term_context_file(in, out) is det.

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.
	% Unify_proc__generate_du_type_to_term_clauses
	% requires the use of an initialized term_context. It
	% directly constructs an initialized term_context
	% without calling term_context_init to avoid the
	% prob of including the term module in everything.

% :- pred term_context_init(term_context).
% :- mode term_context_init(out) is det.

:- pred term_context_init(string, int, term_context).
:- mode term_context_init(in, in, out) is det.

	% Convert a list of terms which are all vars into a list
	% of vars.  Abort (call error/1) if the list contains
	% any non-variables.

:- pred term__term_list_to_var_list(list(term), list(var)).
:- mode term__term_list_to_var_list(in, out) is det.

	% Convert a list of terms which are all vars into a list
	% of vars (or vice versa).

:- pred term__var_list_to_term_list(list(var), list(term)).
:- mode term__var_list_to_term_list(in, out) is det.
:- mode term__var_list_to_term_list(out, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, require.
:- import_module random.

%-----------------------------------------------------------------------------%

:- type var_supply	==	int.
:- type var		==	int.

%-----------------------------------------------------------------------------%

	% term__vars(Term, Vars) is true if Vars is the list of variables
	% contained in Term obtained by depth-first left-to-right traversal
	% in that order.

term__vars(Term, Vars) :-
	term__vars_2(Term, [], Vars).

term__vars_list(Terms, Vars) :-
	term__vars_2_list(Terms, [], Vars).

term__vars_2(term_variable(V), Vs, [V|Vs]).
term__vars_2(term_functor(_,Args,_), Vs0, Vs) :-
	term__vars_2_list(Args, Vs0, Vs).

:- pred term__vars_2_list(list(term), list(var), list(var)).
:- mode term__vars_2_list(in, in, out) is det.

term__vars_2_list([], Vs, Vs).
term__vars_2_list([T|Ts], Vs0, Vs) :-
	term__vars_2_list(Ts, Vs0, Vs1),
	term__vars_2(T, Vs1, Vs).

%-----------------------------------------------------------------------------%

	% term__contains_var(Term, Var) is true if Var occurs in Term.

term__contains_var(term_variable(V), V).
term__contains_var(term_functor(_, Args, _), V) :-
	term__contains_var_list(Args, V).

term__contains_var_list([T|_], V) :-
	term__contains_var(T, V).
term__contains_var_list([_|Ts], V) :-
	term__contains_var_list(Ts, V).

%-----------------------------------------------------------------------------%

	% term__contains_functor(Term, Functor, Args):
	%	term_functor(Functor, Args, _) is a subterm of Term.
	%
	% CURRENTLY NOT USED.

:- pred term__contains_functor(term, const, list(term)).
% :- mode term__contains_functor(in, in, in) is semidet.
:- mode term__contains_functor(in, out, out) is nondet.

term__contains_functor(term_functor(Functor, Args, _), Functor, Args).
term__contains_functor(term_functor(_, Args, _), SubFunctor, SubArgs) :-
 	list__member(SubTerm, Args),
 	term__contains_functor(SubTerm, SubFunctor, SubArgs).

%-----------------------------------------------------------------------------%

	% term__subterm(Term, SubTerm):
	%	SubTerm is a subterm of Term.
	%
	% CURRENTLY NOT USED.

:- pred term__subterm(term, term).
:- mode term__subterm(in, in) is semidet.
:- mode term__subterm(in, out) is multidet.

term__subterm(Term, Term).
term__subterm(term_functor(_, Args, _), SubTerm) :-
	list__member(Term, Args),
	term__subterm(Term, SubTerm).

%-----------------------------------------------------------------------------%

	% Access predicates for the term_context data structure.

	% Given a term context, return the source line number.

term_context_line(term_context(_, LineNumber), LineNumber).

	% Given a term context, return the source file.

term_context_file(term_context(FileName, _), FileName).

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.

% term_context_init(term_context("", 0)).

term_context_init(File, LineNumber, term_context(File, LineNumber)).

%-----------------------------------------------------------------------------%

	% Unify two terms (with occurs check), updating the bindings of
	% the variables in the terms.  

:- term__unify(X, Y, _, _) when X and Y.		% NU-Prolog indexing

term__unify(term_variable(X), term_variable(Y), Bindings0, Bindings) :-
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
			( SubstBindingOfX = term_variable(Y) ->
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
			( SubstBindingOfY2 = term_variable(X) ->
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
				map__set(Bindings0, X, term_variable(Y),
					Bindings)
			)
		)
	).

term__unify(term_variable(X), term_functor(F, As, C), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		term__unify(BindingOfX, term_functor(F, As, C), Bindings0,
			Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

term__unify(term_functor(F, As, C), term_variable(X), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		term__unify(term_functor(F, As, C), BindingOfX, Bindings0,
			Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

term__unify(term_functor(F, AsX, _), term_functor(F, AsY, _)) -->
	term__unify_list(AsX, AsY).

:- pred term__unify_list(list(term), list(term), substitution, substitution).
:- mode term__unify_list(in, in, in, out) is semidet.

term__unify_list([], []) --> [].
term__unify_list([X | Xs], [Y | Ys]) -->
	term__unify(X, Y),
	term__unify_list(Xs, Ys).

%-----------------------------------------------------------------------------%

	% term__occurs(Term, Var, Subst) succeeds if Term contains Var,
	% perhaps indirectly via the substitution.  (The variable must
	% not be mapped by the substitution.)

term__occurs(term_variable(X), Y, Bindings) :-
	(
		X = Y
	->
		true
	;
		map__search(Bindings, X, BindingOfX),
		term__occurs(BindingOfX, Y, Bindings)
	).
term__occurs(term_functor(_F, As, _), Y, Bindings) :-
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

term__substitute(term_variable(Var), SearchVar, Replacement, Term) :-
	(
		Var = SearchVar
	->
		Term = Replacement
	;
		Term = term_variable(Var)
	).
term__substitute(term_functor(Name, Args0, Context), Var, Replacement,
		 term_functor(Name, Args, Context)) :-
	term__substitute_list(Args0, Var, Replacement, Args).

term__substitute_list([], _Var, _Replacement, []).
term__substitute_list([Term0 | Terms0], Var, Replacement, [Term | Terms]) :-
	term__substitute(Term0, Var, Replacement, Term),
	term__substitute_list(Terms0, Var, Replacement, Terms).

term__substitute_corresponding(Ss, Rs, Term0, Term) :-
	map__init(Subst0),
	( term__substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
		term__apply_substitution(Term0, Subst, Term)
	;
		error("term__substitute_corresponding: different length lists")
	).

term__substitute_corresponding_list(Ss, Rs, TermList0, TermList) :-
	map__init(Subst0),
	( term__substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
		term__apply_substitution_to_list(TermList0, Subst, TermList)
	;
		error(
		  "term__substitute_corresponding_list: different length lists"
		)
	).

:- pred term__substitute_corresponding_2(list(var), list(term),
					substitution, substitution).
:- mode term__substitute_corresponding_2(in, in, in, out) is semidet.

term__substitute_corresponding_2([], [], Subst, Subst).
term__substitute_corresponding_2([S | Ss], [R | Rs], Subst0, Subst) :-
	map__set(Subst0, S, R, Subst1),
	term__substitute_corresponding_2(Ss, Rs, Subst1, Subst).

%-----------------------------------------------------------------------------%

term__apply_rec_substitution(term_variable(Var), Substitution, Term) :-
	(
		%some [Replacement]
		map__search(Substitution, Var, Replacement)
	->
		% recursively apply the substition to the replacement
		term__apply_rec_substitution(Replacement, Substitution, Term)
	;
		Term = term_variable(Var)
	).
term__apply_rec_substitution(term_functor(Name, Args0, Context), Substitution,
		 term_functor(Name, Args, Context)) :-
	term__apply_rec_substitution_to_list(Args0, Substitution, Args).

term__apply_rec_substitution_to_list([], _Substitution, []).
term__apply_rec_substitution_to_list([Term0 | Terms0], Substitution,
		[Term | Terms]) :-
	term__apply_rec_substitution(Term0, Substitution, Term),
	term__apply_rec_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

term__apply_substitution(term_variable(Var), Substitution, Term) :-
	(
		%some [Replacement]
		map__search(Substitution, Var, Replacement)
	->
		Term = Replacement
	;
		Term = term_variable(Var)
	).
term__apply_substitution(term_functor(Name, Args0, Context), Substitution,
		 term_functor(Name, Args, Context)) :-
	term__apply_substitution_to_list(Args0, Substitution, Args).

term__apply_substitution_to_list([], _Substitution, []).
term__apply_substitution_to_list([Term0 | Terms0], Substitution,
		[Term | Terms]) :-
	term__apply_substitution(Term0, Substitution, Term),
	term__apply_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

	% create a new supply of variables
term__init_var_supply(0).

	% We number variables using bit-reversed sequential numbers,
	% to ensure that our trees remain perfectly balanced.
	% Hopefully the overhead of bit-reversal isn't too high.

term__create_var(VarSupply0, VarSupply, VarSupply) :-
	VarSupply is VarSupply0 + 1.

%-----------------------------------------------------------------------------%

	% To convert a variable to an int, we want to undo the bit-reversal.

term__var_to_int(Var, Var).

%-----------------------------------------------------------------------------%

	% substitute a variable name in a term.
term__relabel_variable(term_functor(Const, Terms0, Cont), OldVar, NewVar,
				term_functor(Const, Terms, Cont)) :-
	term__relabel_variables(Terms0, OldVar, NewVar, Terms).
term__relabel_variable(term_variable(Var0), OldVar, NewVar,
				term_variable(Var)) :-
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

:- term__term_list_to_var_list(Terms, Vars) when Terms or Vars. % Indexing

term__term_list_to_var_list(Terms, Vars) :-
	( term__var_list_to_term_list(Vars0, Terms) ->
		Vars = Vars0
	;
		error("term__term_list_to_var_list")
	).

:- term__var_list_to_term_list(Terms, Vars) when Terms or Vars. % Indexing

term__var_list_to_term_list([], []).
term__var_list_to_term_list([Var | Vars], [term_variable(Var) | Terms]) :-
	term__var_list_to_term_list(Vars, Terms).

%-----------------------------------------------------------------------------%

term__is_ground(term_variable(V), Bindings) :-
	map__search(Bindings, V, Binding),
	term__is_ground(Binding, Bindings).
term__is_ground(term_functor(_, Args, _), Bindings) :-
	term__is_ground_2(Args, Bindings).

:- pred term__is_ground_2(list(term), substitution).
:- mode term__is_ground_2(in, in) is semidet.

term__is_ground_2([], _Bindings).
term__is_ground_2([Term|Terms], Bindings) :-
	term__is_ground(Term, Bindings),
	term__is_ground_2(Terms, Bindings).

%-----------------------------------------------------------------------------%

term__is_ground(term_functor(_, Args, _)) :-
	term__is_ground_2(Args).

:- pred term__is_ground_2(list(term)).
:- mode term__is_ground_2(in) is semidet.

term__is_ground_2([]).
term__is_ground_2([Term|Terms]) :-
	term__is_ground(Term),
	term__is_ground_2(Terms).

%-----------------------------------------------------------------------------%

term__compare(Cmp, Term1, Term2, Bindings) :-
	term__apply_rec_substitution(Term1, Bindings, TermA),
	term__is_ground(TermA, Bindings),
	term__apply_rec_substitution(Term2, Bindings, TermB),
	term__is_ground(TermB, Bindings),
	compare(Cmp, TermA, TermB).

%-----------------------------------------------------------------------------%
%
% The following group of predicates are useful for converting data
% structures to terms.
%
%-----------------------------------------------------------------------------%

	% term__construct_term(Functor, ArgClosures, Term) takes the string
	% Functor and a list of closures ArgClosures which yield the arguments
	% of the term, and it returns a Term with the functor Functor and
	% the arguments derived from the closures. The context is set to
	% the result of term_context_init/1.
:- pred term__construct_term(string, list(pred(term)), term).
:- mode term__construct_term( in, list_skel_in(pred(out) is det), out) is det.

	% term__construct_const(Functor, Term) makes a zero arity term Term
	% with the functor Functor.
:- pred term__construct_const(string, term).
:- mode term__construct_const(in, out) is det.

	% term_string_to_term(String, Term) makes a 'string' term Term,
	% corresponding to String.
:- pred term_string_to_term(string, term).
:- mode term_string_to_term(in, out) is det.

	% term__int_to_term(Int, Term) makes a 'int' term Term,
	% corresponding to Int.
:- pred term__int_to_term(int, term).
:- mode term__int_to_term(in, out) is det.

	% term__from_list(Converter, List, Term) creates Term corresponding
	% to the list List, using Converter to convert the elements of the
	% list to terms.
:- pred term__from_list(pred(X, term), list(X), term).
:- mode term__from_list(pred(in, out) is det, in, out) is det.

:- implementation.

term__from_list(_, [], T) :-
	term__construct_const("[]", T).
term__from_list(P, [H|L], T) :-
	term__construct_term(".", [
		lambda([T0::out] is det, call(P, H, T0)),
		term__from_list(P, L)
	], T).

term__construct_const(Functor, term_functor(term_atom(Functor), [], C)) :-
	term_context_init(C).

term__construct_term(S, L0, term_functor(term_atom(S), L, C)) :-
	list__apply(L0, L),
	term_context_init(C).

term_string_to_term(S, term_functor(term_string(S), [], C)) :-
	term_context_init(C).

term__int_to_term(I, term_functor(term_integer(I), [], C)) :-
	term_context_init(C).

%-----------------------------------------------------------------------------%
