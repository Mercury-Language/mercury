%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module term.

% Main author: fjh.

% This file provides a type `term' used to represent Prolog terms.
% This file also provides the following predicates:
%	term_vars(Term,Vars)
%		Vars is the list of variables contained in Term, in the order 
%		obtained by traversing the term depth first, left-to-right.
%	term_contains_var(Term,Var)
%		True if Term contains Var. (On backtracking returns all the 
%		variables contained in Term.)

%-----------------------------------------------------------------------------%

:- import_module integer, string, float, varset.
:- export_pred  term_vars, term_contains_var,
		term__context_line, term__context_init.
:- export_type term, const, variable.

:- type term		--->	term_functor(const,list(term), term__context)
			;	term_variable(variable).
:- type const 		--->	term_atom(string)
			;	term_integer(integer)
			;	term_string(string)
			;	term_float(float).
:- type variable	==	var_id.

%-----------------------------------------------------------------------------%
	% term_vars(Term, Vars) is true if Vars is the list of variables
	% contained in Term obtained by depth-first left-to-right traversal.

:- pred term_vars(term, list(var_id)).
term_vars(Term, Vars) :-
	term_vars_2(Term, [], Vars).

:- pred term_vars_2(term, list(var_id), list(var_id)).
term_vars_2(term_variable(V), Vs, V.Vs).
term_vars_2(term_functor(_,Args,_), Vs0, Vs) :-
	term_vars_2_list(Args, Vs0, Vs).

:- pred term_vars_2_list(list(term), list(var_id), list(var_id)).
term_vars_2_list([], Vs, Vs).
term_vars_2_list(T.Ts, Vs0, Vs) :-
	term_vars_2(T, Vs0, Vs1),
	term_vars_2_list(Ts, Vs1, Vs).

%-----------------------------------------------------------------------------%

	% term_contains_var(Term, Var) is true if Var occurs in Term.

:- pred term_contains_var(term, var_id).
term_contains_var(term_variable(V), V).
term_contains_var(term_functor(_, Args, _), V) :-
	term_contains_var_list(Args, V).

:- pred term_contains_var_list(list(term), var_id).
term_contains_var_list(T._, V) :-
	term_contains_var(T, V).
term_contains_var_list(_.Ts, V) :-
	term_contains_var_list(Ts, V).

%-----------------------------------------------------------------------------%

	% term_contains_functor(Term, Functor, Args):
	%	term_functor(Functor, Args, _) is a subterm of Term.

:- pred term_contains_functor(term, const, list(term)).
term_contains_functor(Term, Functor, Args) :-
	term_subterm(Term, term_functor(Functor, Args, _)).

:- pred term_subterm(term, term).
term_subterm(Term, Term).
term_subterm(term_functor(_, Args, _), SubTerm) :-
	member(SubTerm, Args).

%% term_contains_functor(term_functor(Functor, Args, _), Functor, Args).
%% term_contains_functor(term_functor(_, Args, _), SubFunctor, SubArgs) :-
%% 	member(SubTerm, Args),
%% 	term_contains_functor(SubTerm, SubFunctor, SubArgs).

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
