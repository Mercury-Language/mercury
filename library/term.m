%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: term.m.
% Main author: fjh.
% Stability: medium.

% This file provides a type `term' used to represent Prolog terms,
% and various predicates to manipulate terms and substitutions.
% Terms are polymorphic so that terms representing different kinds of
% thing can be made to be of different types so they don't get mixed up.

%-----------------------------------------------------------------------------%

:- module term.
:- interface.
:- import_module enum, list, map, std_util.

%-----------------------------------------------------------------------------%

:- type term(T)
	--->	term__functor(
			const,
			list(term(T)),
			term__context
		)
	;	term__variable(
			var(T)
		).

:- type const
	--->	term__atom(string)
	;	term__integer(int)
	;	term__string(string)
	;	term__float(float).

:- type term__context
	--->    term__context(string, int).
				% file name, line number.

:- type var(T).
:- type var_supply(T).

:- type generic
	--->	generic.

:- type term	==	term(generic).
:- type var	==	var(generic).

%-----------------------------------------------------------------------------%

	% The following predicates can convert values of (almost)
	% any type to the type `term' and back again.

:- type term_to_type_result(T, U)
	--->	ok(T)
	;	error(term_to_type_error(U)).

:- type term_to_type_result(T) == term_to_type_result(T, generic).

	% term__try_term_to_type(Term, Result):
	% Try to convert the given term to a ground value of type T.
	% If successful, return `ok(X)' where X is the converted value.
	% If Term is not ground, return `mode_error(Var, Context)',
	% where Var is a variable occurring in Term.
	% If Term is not a valid term of the specified type, return
	% `type_error(SubTerm, ExpectedType, Context, ArgContexts)',
	% where SubTerm is a sub-term of Term and ExpectedType is
	% the type expected for that part of Term.
	% Context specifies the file and line number where the
	% offending part of the term was read in from, if available.
	% ArgContexts specifies the path from the root of the term
	% to the offending subterm.
:- func term__try_term_to_type(term(U)) = term_to_type_result(T, U).
:- pred term__try_term_to_type(term(U)::in, term_to_type_result(T, U)::out)
	is det.

:- type term_to_type_error(T)
	--->	type_error(
			term(T),
			type_desc,
			term__context,
			term_to_type_context
		)
	;	mode_error(
			var(T),
			term_to_type_context
		).

:- type term_to_type_context == list(term_to_type_arg_context).

:- type term_to_type_arg_context
	--->	arg_context(
			const,		% functor
			int,		% argument number (starting from 1)
			term__context	% filename & line number
		).

	% term_to_type(Term, Type) :- try_term_to_type(Term, ok(Type)).
:- pred term__term_to_type(term(U)::in, T::out) is semidet.

	% like term_to_type, but calls error/1 rather than failing.
:- func term__det_term_to_type(term(_)) = T.
:- pred term__det_term_to_type(term(_)::in, T::out) is det.

	% converts a value to a term representation of that value
:- func term__type_to_term(T) = term(_).
:- pred term__type_to_term(T::in, term(_)::out) is det.

	% calls term__type_to_term on the value stored in the univ
	% (as distinct from the univ itself).
:- func term__univ_to_term(univ) = term(_).
:- pred term__univ_to_term(univ::in, term(_)::out) is det.

%-----------------------------------------------------------------------------%

	% term__vars(Term, Vars)
	% Vars is the list of variables contained in Term, in the order
	% obtained by traversing the term depth first, left-to-right.
:- func term__vars(term(T)) = list(var(T)).
:- pred term__vars(term(T)::in, list(var(T))::out) is det.

	% As above, but with an accumulator.
:- func term__vars_2(term(T), list(var(T))) = list(var(T)).
:- pred term__vars_2(term(T)::in, list(var(T))::in, list(var(T))::out) is det.

	% term__vars_list(TermList, Vars)
	% Vars is the list of variables contained in TermList, in the order
	% obtained by traversing the list of terms depth-first, left-to-right.
:- func term__vars_list(list(term(T))) = list(var(T)).
:- pred term__vars_list(list(term(T))::in, list(var(T))::out) is det.

	% term__contains_var(Term, Var)
	% True if Term contains Var. On backtracking returns all the
	% variables contained in Term.
:- pred term__contains_var(term(T), var(T)).
:- mode term__contains_var(in, in) is semidet.
:- mode term__contains_var(in, out) is nondet.

	% term__contains_var_list(TermList, Var)
	% True if TermList contains Var. On backtracking returns all the
	% variables contained in Term.
:- pred term__contains_var_list(list(term(T)), var(T)).
:- mode term__contains_var_list(in, in) is semidet.
:- mode term__contains_var_list(in, out) is nondet.

:- type substitution(T) == map(var(T), term(T)).
:- type substitution	== substitution(generic).

	% term__unify(Term1, Term2, Bindings0, Bindings):
	% Unify (with occur check) two terms with respect to a set of bindings
	% and possibly update the set of bindings.
:- pred term__unify(term(T)::in, term(T)::in, substitution(T)::in,
	substitution(T)::out) is semidet.

	% term__substitute(Term0, Var, Replacement, Term):
	% Replace all occurrences of Var in Term0 with Replacement,
	% and return the result in Term.
:- func term__substitute(term(T), var(T), term(T)) = term(T).
:- pred term__substitute(term(T)::in, var(T)::in, term(T)::in, term(T)::out)
	is det.

	% As above, except for a list of terms rather than a single term.
:- func term__substitute_list(list(term(T)), var(T), term(T)) = list(term(T)).
:- pred term__substitute_list(list(term(T))::in, var(T)::in, term(T)::in,
	list(term(T))::out) is det.

	% term__substitute_corresponding(Vars, Repls, Term0, Term).
	% Replace all occurrences of variables in Vars with the corresponding
	% term in Repls, and return the result in Term. If Vars contains
	% duplicates, or if Vars is not the same length as Repls, the
	% behaviour is undefined and probably harmful.
:- func term__substitute_corresponding(list(var(T)), list(term(T)),
	term(T)) = term(T).
:- pred term__substitute_corresponding(list(var(T))::in, list(term(T))::in,
	term(T)::in, term(T)::out) is det.

	% As above, except applies to a list of terms rather than a
	% single term.
:- func term__substitute_corresponding_list(list(var(T)),
	list(term(T)), list(term(T))) = list(term(T)).
:- pred term__substitute_corresponding_list(list(var(T))::in,
	list(term(T))::in, list(term(T))::in, list(term(T))::out) is det.

	% term__apply_rec_substitution(Term0, Substitution, Term):
	% Recursively apply substitution to Term0 until no more substitions
	% can be applied, and then return the result in Term.
:- func term__apply_rec_substitution(term(T), substitution(T)) = term(T).
:- pred term__apply_rec_substitution(term(T)::in, substitution(T)::in,
	term(T)::out) is det.

	% As above, except applies to a list of terms rather than a
	% single term.
:- func term__apply_rec_substitution_to_list(list(term(T)),
	substitution(T)) = list(term(T)).
:- pred term__apply_rec_substitution_to_list(list(term(T))::in,
	substitution(T)::in, list(term(T))::out) is det.

	% term__apply_substitution(Term0, Substitution, Term):
	% Apply substitution to Term0 and return the result in Term.
:- func term__apply_substitution(term(T), substitution(T)) = term(T).
:- pred term__apply_substitution(term(T)::in, substitution(T)::in,
	term(T)::out) is det.

	% As above, except applies to a list of terms rather than a
	% single term.
:- func term__apply_substitution_to_list(list(term(T)),
	substitution(T)) = list(term(T)).
:- pred term__apply_substitution_to_list(list(term(T))::in,
	substitution(T)::in, list(term(T))::out) is det.

	% term__occurs(Term0, Var, Substitution):
	% True iff Var occurs in the term resulting after applying
	% Substitution to Term0.
:- pred term__occurs(term(T)::in, var(T)::in, substitution(T)::in) is semidet.

	% As above, except for a list of terms rather than a single term.
:- pred term__occurs_list(list(term(T))::in, var(T)::in, substitution(T)::in)
	is semidet.

	% term__relabel_variable(Term0, OldVar, NewVar, Term):
	% Replace all occurences of OldVar in Term0 with NewVar and
	% put the result in Term.
:- func term__relabel_variable(term(T), var(T), var(T)) = term(T).
:- pred term__relabel_variable(term(T)::in, var(T)::in, var(T)::in,
	term(T)::out) is det.

	% As above, except applies to a list of terms rather than a
	% single term.
	% XXX the name of the predicate is misleading.
:- func term__relabel_variables(list(term(T)), var(T), var(T)) = list(term(T)).
:- pred term__relabel_variables(list(term(T))::in, var(T)::in, var(T)::in,
	list(term(T))::out) is det.

	% Same as term__relabel_variable, except relabels multiple variables.
	% If a variable is not in the map, it is not replaced.
:- func term__apply_variable_renaming(term(T), map(var(T), var(T))) = term(T).
:- pred term__apply_variable_renaming(term(T)::in, map(var(T), var(T))::in,
	term(T)::out) is det.

	% Applies term__apply_variable_renaming to a list of terms.
:- func term__apply_variable_renaming_to_list(list(term(T)),
	map(var(T), var(T))) = list(term(T)).
:- pred term__apply_variable_renaming_to_list(list(term(T))::in,
	map(var(T), var(T))::in, list(term(T))::out) is det.

	% Applies term__apply_variable_renaming to a var.
:- func term__apply_variable_renaming_to_var(map(var(T), var(T)),
	var(T)) = var(T).
:- pred term__apply_variable_renaming_to_var(map(var(T), var(T))::in,
	var(T)::in, var(T)::out) is det.

	% Applies term__apply_variable_renaming to a list of vars.
:- func term__apply_variable_renaming_to_vars(map(var(T), var(T)),
	list(var(T))) = list(var(T)).
:- pred term__apply_variable_renaming_to_vars(map(var(T), var(T))::in,
	list(var(T))::in, list(var(T))::out) is det.

	% term__is_ground(Term, Bindings) is true iff no variables contained
	% in Term are non-ground in Bindings.
:- pred term__is_ground(term(T)::in, substitution(T)::in) is semidet.

	% term__is_ground(Term) is true iff Term contains no variables.
:- pred term__is_ground(term(T)::in) is semidet.

%-----------------------------------------------------------------------------%

	% To manage a supply of variables, use the following 2 predicates.
	% (We might want to give these a unique mode later.)

	% term__init_var_supply(VarSupply):
	% Returns a fresh var_supply for producing fresh variables.
:- func term__init_var_supply = var_supply(T).
:- pred term__init_var_supply(var_supply(T)).
:- mode term__init_var_supply(out) is det.
:- mode term__init_var_supply(in) is semidet. % implied

	% term__create_var(VarSupply0, Variable, VarSupply):
	% Create a fresh variable (var) and return the updated var_supply.
:- pred term__create_var(var_supply(T), var(T), var_supply(T)).
:- mode term__create_var(in, out, out) is det.

	% term__var_id(Variable):
	% Returns a unique number associated with this variable w.r.t.
	% its originating var_supply.
:- func term__var_id(var(T)) = int.

%-----------------------------------------------------------------------------%

	% from_int/1 should only be applied to integers returned
	% by to_int/1. This instance declaration is needed to
	% allow sets of variables to be represented using
	% sparse_bitset.m.
:- instance enum(var(_)).

	% Convert a variable to an int.
	% Different variables map to different ints.
	% Other than that, the mapping is unspecified.
:- func term__var_to_int(var(T)) = int.
:- pred term__var_to_int(var(T)::in, int::out) is det.

%-----------------------------------------------------------------------------%

	% Given a term context, return the source line number.
:- pred term__context_line(term__context::in, int::out) is det.
:- func term__context_line(term__context) = int.

	% Given a term context, return the source file.
:- pred term__context_file(term__context::in, string::out) is det.
:- func term__context_file(term__context) = string.

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.
:- pred term__context_init(term__context::out) is det.
:- func term__context_init = term__context.
:- pred term__context_init(string::in, int::in, term__context::out) is det.
:- func term__context_init(string, int) = term__context.

	% Convert a list of terms which are all vars into a list
	% of vars.  Abort (call error/1) if the list contains
	% any non-variables.

:- pred term__term_list_to_var_list(list(term(T))::in, list(var(T))::out)
	is det.
:- func term__term_list_to_var_list(list(term(T))) = list(var(T)).

	% Convert a list of terms which are all vars into a list
	% of vars (or vice versa).

:- pred term__var_list_to_term_list(list(var(T)), list(term(T))).
:- mode term__var_list_to_term_list(in, out) is det.
:- mode term__var_list_to_term_list(out, in) is semidet.

:- func term__var_list_to_term_list(list(var(T))) = list(term(T)).

%-----------------------------------------------------------------------------%

	% term__generic_term(Term) is true iff `Term' is a term of type
	% `term' ie `term(generic)'.
	% It is useful because in some instances it doesn't matter what
	% the type of a term is, and passing it to this predicate will
	% ground the type avoiding unbound type variable warnings.
:- pred term__generic_term(term::in) is det.

	% Coerce a term of type `T' into a term of type `U'.
:- pred term__coerce(term(T)::in, term(U)::out) is det.
:- func term__coerce(term(T)) = term(U).

	% Coerce a var of type `T' into a var of type `U'.
:- pred term__coerce_var(var(T)::in, var(U)::out) is det.
:- func term__coerce_var(var(T)) = var(U).

	% Coerce a var_supply of type `T' into a var_supply of type `U'.
:- pred term__coerce_var_supply(var_supply(T)::in, var_supply(U)::out) is det.
:- func term__coerce_var_supply(var_supply(T)) = var_supply(U).

%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%-----------------------------------------------------------------------------%

:- interface.

% This is the same as term_to_type, except that an integer
% is allowed where a character is expected. This is needed by
% extras/aditi/aditi.m because Aditi does not have a builtin
% character type. This also allows an integer where a float
% is expected.

:- pred term__term_to_type_with_int_instead_of_char(term(U)::in, T::out)
	is semidet.

	% term__compare(Comparison, Term1, Term2, Bindings) is true iff
	% there is a binding of Comparison to <, =, or > such
	% that the binding holds for the two ground terms Term1
	% and Term2 with respect to the bindings in Bindings.
	% Fails if Term1 or Term2 is not ground (with respect to
	% the bindings in Bindings).
	%
	% BEWARE: This predicate is being phased out, because the comparison
	% does *not* ignore the term__contexts in the terms.
:- pragma obsolete(term__compare/4).
:- pred term__compare(comparison_result::out, term(T)::in, term(T)::in,
	substitution(T)::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, char, float, std_util, require, array, int, string.

%-----------------------------------------------------------------------------%

:- type var_supply(T)
	--->	var_supply(int).

:- type var(T)
	--->	var(int).

%-----------------------------------------------------------------------------%

term__term_to_type(Term, Val) :-
	term__try_term_to_type(Term, ok(Val)).

term__term_to_type_with_int_instead_of_char(Term, Val) :-
	IsAditiTuple = yes,
	term__try_term_to_type(IsAditiTuple, Term, ok(Val)).

term__try_term_to_type(Term, Result) :-
	IsAditiTuple = no,
	term__try_term_to_type(IsAditiTuple, Term, Result).

:- pred term__try_term_to_type(bool::in, term(U)::in,
	term_to_type_result(T, U)::out) is det.

term__try_term_to_type(IsAditiTuple, Term, Result) :-
	term__try_term_to_univ(IsAditiTuple, Term,
		type_of(ValTypedVar), UnivResult),
	(
		UnivResult = ok(Univ),
		det_univ_to_type(Univ, Val),
		same_type(Val, ValTypedVar),
		Result = ok(Val)
	;
		UnivResult = error(Error),
		Result = error(Error)
	).

:- pred term__try_term_to_univ(bool::in, term(T)::in, type_desc::in,
	term_to_type_result(univ, T)::out) is det.

term__try_term_to_univ(IsAditiTuple, Term, Type, Result) :-
	term__try_term_to_univ_2(IsAditiTuple, Term, Type, [], Result).

:- pred term__try_term_to_univ_2(bool::in, term(T)::in, type_desc::in,
	term_to_type_context::in, term_to_type_result(univ, T)::out) is det.

term__try_term_to_univ_2(_, term__variable(Var), _Type, Context,
		error(mode_error(Var, Context))).
term__try_term_to_univ_2(IsAditiTuple, Term, Type, Context, Result) :-
	Term = term__functor(Functor, ArgTerms, TermContext),
	(
		type_ctor_and_args(Type, TypeCtor, TypeArgs),
		term__term_to_univ_special_case(IsAditiTuple,
			type_ctor_module_name(TypeCtor),
			type_ctor_name(TypeCtor),
			TypeArgs, Term, Type, Context, SpecialCaseResult)
	->
		Result = SpecialCaseResult
	;
		Functor = term__atom(FunctorName),
		list__length(ArgTerms, Arity),
		find_functor(Type, FunctorName, Arity, FunctorNumber, ArgTypes),
		term__term_list_to_univ_list(IsAditiTuple, ArgTerms,
			ArgTypes, Functor, 1, Context, TermContext, ArgsResult)
	->
		(
			ArgsResult = ok(ArgValues),
			( Value = construct(Type, FunctorNumber, ArgValues) ->
				Result = ok(Value)
			;
				error("term_to_type: construct/3 failed")
			)
		;
			ArgsResult = error(Error),
			Result = error(Error)
		)
	;
		% the arg contexts are built up in reverse order,
		% so we need to reverse them here
		list__reverse(Context, RevContext),
		Result = error(type_error(Term, Type, TermContext, RevContext))
	).

:- pred term__term_to_univ_special_case(bool::in, string::in, string::in,
	list(type_desc)::in,
	term(T)::in(bound(term__functor(ground, ground, ground))),
	type_desc::in, term_to_type_context::in,
	term_to_type_result(univ, T)::out) is semidet.

term__term_to_univ_special_case(IsAditiTuple, "builtin", "character", [],
		Term, _, _, ok(Univ)) :-
	(
		IsAditiTuple = no,
		Term = term__functor(term__atom(FunctorName), [], _),
		string__first_char(FunctorName, Char, "")
	;
		IsAditiTuple = yes,
		Term = term__functor(term__integer(Int), [], _),
		char__to_int(Char, Int)
	),
	type_to_univ(Char, Univ).
term__term_to_univ_special_case(_, "builtin", "int", [],
		Term, _, _, ok(Univ)) :-
	Term = term__functor(term__integer(Int), [], _),
	type_to_univ(Int, Univ).
term__term_to_univ_special_case(_, "builtin", "string", [],
		Term, _, _, ok(Univ)) :-
	Term = term__functor(term__string(String), [], _),
	type_to_univ(String, Univ).
term__term_to_univ_special_case(IsAditiTuple, "builtin", "float", [],
		Term, _, _, ok(Univ)) :-
	( Term = term__functor(term__float(Float), [], _) ->
		type_to_univ(Float, Univ)
	;
		IsAditiTuple = yes,
		Term = term__functor(term__integer(Int), [], _),
		Float = float__float(Int),
		type_to_univ(Float, Univ)
	).
term__term_to_univ_special_case(IsAditiTuple, "array", "array", [ElemType],
		Term, _Type, PrevContext, Result) :-
	%
	% arrays are represented as terms of the form
	%	array([elem1, elem2, ...])
	%
	Term = term__functor(term__atom("array"), [ArgList], TermContext),

	% To convert such terms back to arrays, we first
	% convert the term representing the list of elements back to a list,
	% and then (if successful) we just call the array/1 function.
	%
	has_type(Elem, ElemType),
	ListType = type_of([Elem]),
	ArgContext = arg_context(term__atom("array"), 1, TermContext),
	NewContext = [ArgContext | PrevContext],
	term__try_term_to_univ_2(IsAditiTuple, ArgList, ListType, NewContext,
		ArgResult),
	(
		ArgResult = ok(ListUniv),
		has_type(Elem2, ElemType),
		same_type(List, [Elem2]),
		det_univ_to_type(ListUniv, List),
		Array = array(List),
		Result = ok(univ(Array))
	;
		ArgResult = error(Error),
		Result = error(Error)
	).
term__term_to_univ_special_case(_, "builtin", "c_pointer", _, _, _, _, _) :-
	fail.
term__term_to_univ_special_case(_, "std_util", "univ", [],
		Term, _, _, Result) :-
	% Implementing this properly would require keeping a
	% global table mapping from type names to type_infos
	% for all of the types in the program...
	% so for the moment, we only allow it for basic types.
	Term = term__functor(term__atom("univ"), [Arg], _),
	Arg = term__functor(term__atom(":"), [Value, Type], _),
	(
		Type = term__functor(term__atom("int"), [], _),
		Value = term__functor(term__integer(Int), [], _),
		Univ = univ(Int)
	;
		Type = term__functor(term__atom("string"), [], _),
		Value = term__functor(term__string(String), [], _),
		Univ = univ(String)
	;
		Type = term__functor(term__atom("float"), [], _),
		Value = term__functor(term__float(Float), [], _),
		Univ = univ(Float)
	),
	% The result is a `univ', but it is also wrapped in a `univ'
	% like all the other results returned from this procedure.
	Result = ok(univ(Univ)).

term__term_to_univ_special_case(_, "std_util", "type_info", _, _, _, _, _) :-
	% ditto
	fail.

:- pred term__term_list_to_univ_list(bool::in, list(term(T))::in,
	list(type_desc)::in, term__const::in, int::in,
	term_to_type_context::in, term__context::in,
	term_to_type_result(list(univ), T)::out) is semidet.

term__term_list_to_univ_list(_, [], [], _, _, _, _, ok([])).
term__term_list_to_univ_list(IsAditiTuple, [ArgTerm|ArgTerms],
		[Type|Types], Functor, ArgNum, PrevContext,
		TermContext, Result) :-
	ArgContext = arg_context(Functor, ArgNum, TermContext),
	NewContext = [ArgContext | PrevContext],
	term__try_term_to_univ_2(IsAditiTuple, ArgTerm, Type, NewContext,
		ArgResult),
	(
		ArgResult = ok(Arg),
		term__term_list_to_univ_list(IsAditiTuple, ArgTerms, Types,
			Functor, ArgNum + 1, PrevContext,
			TermContext, RestResult),
		(
			RestResult = ok(Rest),
			Result = ok([Arg | Rest])
		;
			RestResult = error(Error),
			Result = error(Error)
		)
	;
		ArgResult = error(Error),
		Result = error(Error)
	).

:- pred term__find_functor(type_desc::in, string::in, int::in, int::out,
	list(type_desc)::out) is semidet.

term__find_functor(Type, Functor, Arity, FunctorNumber, ArgTypes) :-
	N = num_functors(Type),
	term__find_functor_2(Type, Functor, Arity, N, FunctorNumber, ArgTypes).

:- pred term__find_functor_2(type_desc::in, string::in, int::in, int::in,
	int::out, list(type_desc)::out) is semidet.

term__find_functor_2(TypeInfo, Functor, Arity, Num, FunctorNumber, ArgTypes) :-
	Num >= 0,
	Num1 = Num - 1,
	( get_functor(TypeInfo, Num1, Functor, Arity, ArgTypes1) ->
		ArgTypes = ArgTypes1,
		FunctorNumber = Num1
	;
		term__find_functor_2(TypeInfo, Functor, Arity, Num1,
			FunctorNumber, ArgTypes)
	).

term__det_term_to_type(Term, X) :-
	( term__term_to_type(Term, X1) ->
		X = X1
	; \+ term__is_ground(Term) ->
		error("term__det_term_to_type failed, " ++
			"because the term wasn't ground")
	;
		string__append_list([
			"term__det_term_to_type failed, due to a type error:\n",
			"the term wasn't a valid term for type `",
			type_name(type_of(X)),
			"'"], Message),
		error(Message)
	).

%-----------------------------------------------------------------------------%

term__type_to_term(Val, Term) :- type_to_univ(Val, Univ),
	term__univ_to_term(Univ, Term).

	% convert the value stored in the univ (as distinct from
	% the univ itself) to a term.
term__univ_to_term(Univ, Term) :-
	term__context_init(Context),
	Type = univ_type(Univ),
	% NU-Prolog barfs on `num_functors(Type) < 0'
	( num_functors(Type) = N, N < 0 ->
		(
			type_ctor_and_args(Type, TypeCtor, TypeArgs),
			TypeName = type_ctor_name(TypeCtor),
			ModuleName = type_ctor_module_name(TypeCtor),
			term__univ_to_term_special_case(ModuleName, TypeName,
				TypeArgs, Univ, Context, SpecialCaseTerm)
		->
			Term = SpecialCaseTerm
		;
			string__append_list(
				["term__type_to_term: unknown type `",
				type_name(univ_type(Univ)),
				"'"],
				Message),
			error(Message)
		)
	;
		deconstruct(univ_value(Univ), FunctorString, _FunctorArity,
			FunctorArgs),
		term__univ_list_to_term_list(FunctorArgs, TermArgs),
		Term = term__functor(term__atom(FunctorString), TermArgs,
			Context)
	).

:- pred term__univ_to_term_special_case(string::in, string::in,
	list(type_desc)::in, univ::in, term__context::in,
	term(T)::out) is semidet.

term__univ_to_term_special_case("builtin", "int", [], Univ, Context,
		term__functor(term__integer(Int), [], Context)) :-
	det_univ_to_type(Univ, Int).
term__univ_to_term_special_case("builtin", "float", [], Univ, Context,
		term__functor(term__float(Float), [], Context)) :-
	det_univ_to_type(Univ, Float).
term__univ_to_term_special_case("builtin", "character", [], Univ,
		Context, term__functor(term__atom(CharName), [], Context)) :-
	det_univ_to_type(Univ, Character),
	string__char_to_string(Character, CharName).
term__univ_to_term_special_case("builtin", "string", [], Univ, Context,
		term__functor(term__string(String), [], Context)) :-
	det_univ_to_type(Univ, String).
term__univ_to_term_special_case("std_util", "type_info", [], Univ, Context,
		term__functor(term__atom("type_info"), [Term], Context)) :-
	det_univ_to_type(Univ, TypeInfo),
	type_info_to_term(Context, TypeInfo, Term).
term__univ_to_term_special_case("std_util", "univ", [], Univ, Context, Term) :-
	det_univ_to_type(Univ, NestedUniv),
	Term = term__functor(term__atom("univ"),
		% XXX what operator should we use for type
		% qualification?
		[term__functor(term__atom(":"),	 % TYPE_QUAL_OP
			[ValueTerm, TypeTerm], Context)], Context),
	type_info_to_term(Context, univ_type(NestedUniv), TypeTerm),
	NestedUnivValue = univ_value(NestedUniv),
	term__type_to_term(NestedUnivValue, ValueTerm).

term__univ_to_term_special_case("array", "array", [ElemType], Univ, Context,
		Term) :-
	Term = term__functor(term__atom("array"), [ArgsTerm], Context),
	has_type(Elem, ElemType),
	same_type(List, [Elem]),
	det_univ_to_type(Univ, Array),
	array__to_list(Array, List),
	term__type_to_term(List, ArgsTerm).

:- pred same_type(T::unused, T::unused) is det.

same_type(_, _).

:- pred term__univ_list_to_term_list(list(univ)::in,
	list(term(T))::out) is det.

term__univ_list_to_term_list([], []).
term__univ_list_to_term_list([Value|Values], [Term|Terms]) :-
	term__univ_to_term(Value, Term),
	term__univ_list_to_term_list(Values, Terms).

% given a type_info, return a term that represents the name of that type.
:- pred type_info_to_term(term__context::in, type_desc::in, term(T)::out)
	is det.

type_info_to_term(Context, TypeInfo, Term) :-
	type_ctor_and_args(TypeInfo, TypeCtor, ArgTypes),
	TypeName = type_ctor_name(TypeCtor),
	ModuleName = type_ctor_name(TypeCtor),
	list__map(type_info_to_term(Context), ArgTypes, ArgTerms),

	( ModuleName = "builtin" ->
		Term = term__functor(term__atom(TypeName), ArgTerms, Context)
	;
		Arg1 = term__functor(term__atom(ModuleName), [], Context),
		Arg2 = term__functor(term__atom(TypeName), ArgTerms, Context),
		Term = term__functor(term__atom(":"), [Arg1, Arg2], Context)
	).

%-----------------------------------------------------------------------------%

	% term__vars(Term, Vars) is true if Vars is the list of variables
	% contained in Term obtained by depth-first left-to-right traversal
	% in that order.

term__vars(Term, Vars) :-
	term__vars_2(Term, [], Vars).

term__vars_list(Terms, Vars) :-
	term__vars_2_list(Terms, [], Vars).

term__vars_2(term__variable(Var), !Vars) :-
	!:Vars = [Var | !.Vars].
term__vars_2(term__functor(_, Args, _), !Vars) :-
	term__vars_2_list(Args, !Vars).

:- pred term__vars_2_list(list(term(T))::in, list(var(T))::in,
	list(var(T))::out) is det.

term__vars_2_list([], !Vars).
term__vars_2_list([Term | Terms], !Vars) :-
	term__vars_2_list(Terms, !Vars),
	term__vars_2(Term, !Vars).

%-----------------------------------------------------------------------------%

	% term__contains_var(Term, Var) is true if Var occurs in Term.

term__contains_var(term__variable(Var), Var).
term__contains_var(term__functor(_, Args, _), Var) :-
	term__contains_var_list(Args, Var).

term__contains_var_list([Term | _], Var) :-
	term__contains_var(Term, Var).
term__contains_var_list([_ | Terms], Var) :-
	term__contains_var_list(Terms, Var).

%-----------------------------------------------------------------------------%

term__context_line(term__context(_, LineNumber), LineNumber).
term__context_file(term__context(FileName, _), FileName).
term__context_init(term__context("", 0)).
term__context_init(File, LineNumber, term__context(File, LineNumber)).

%-----------------------------------------------------------------------------%

	% Unify two terms (with occurs check), updating the bindings of
	% the variables in the terms.

term__unify(term__variable(X), term__variable(Y), !Bindings) :-
	( map__search(!.Bindings, X, BindingOfX) ->
		( map__search(!.Bindings, Y, BindingOfY) ->
			% both X and Y already have bindings - just
			% unify the terms they are bound to
			term__unify(BindingOfX, BindingOfY, !Bindings)
		;
			% Y is a variable which hasn't been bound yet
			term__apply_rec_substitution(BindingOfX, !.Bindings,
				SubstBindingOfX),
			( SubstBindingOfX = term__variable(Y) ->
			 	true
			;
				\+ term__occurs(SubstBindingOfX, Y,
					!.Bindings),
				map__set(!.Bindings, Y, SubstBindingOfX,
					!:Bindings)
			)
		)
	;
		( map__search(!.Bindings, Y, BindingOfY) ->
			% X is a variable which hasn't been bound yet
			term__apply_rec_substitution(BindingOfY, !.Bindings,
				SubstBindingOfY),
			( SubstBindingOfY = term__variable(X) ->
				true
			;
				\+ term__occurs(SubstBindingOfY, X,
					!.Bindings),
				map__set(!.Bindings, X, SubstBindingOfY,
					!:Bindings)
			)
		;
			% both X and Y are unbound variables -
			% bind one to the other
			( X = Y ->
				true
			;
				map__set(!.Bindings, X, term__variable(Y),
					!:Bindings)
			)
		)
	).

term__unify(term__variable(X), term__functor(F, As, C), !Bindings) :-
	( map__search(!.Bindings, X, BindingOfX) ->
		term__unify(BindingOfX, term__functor(F, As, C), !Bindings)
	;
		\+ term__occurs_list(As, X, !.Bindings),
		map__set(!.Bindings, X, term__functor(F, As, C), !:Bindings)
	).

term__unify(term__functor(F, As, C), term__variable(X), !Bindings) :-
	( map__search(!.Bindings, X, BindingOfX) ->
		term__unify(term__functor(F, As, C), BindingOfX, !Bindings)
	;
		\+ term__occurs_list(As, X, !.Bindings),
		map__set(!.Bindings, X, term__functor(F, As, C), !:Bindings)
	).

term__unify(term__functor(F, AsX, _), term__functor(F, AsY, _), !Bindings) :-
	term__unify_list(AsX, AsY, !Bindings).

:- pred term__unify_list(list(term(T))::in, list(term(T))::in,
	substitution(T)::in, substitution(T)::out) is semidet.

term__unify_list([], [], !Bindings).
term__unify_list([X | Xs], [Y | Ys], !Bindings) :-
	term__unify(X, Y, !Bindings),
	term__unify_list(Xs, Ys, !Bindings).

%-----------------------------------------------------------------------------%

	% term__occurs(Term, Var, Subst) succeeds if Term contains Var,
	% perhaps indirectly via the substitution.  (The variable must
	% not be mapped by the substitution.)

term__occurs(term__variable(X), Y, Bindings) :-
	( X = Y ->
		true
	;
		map__search(Bindings, X, BindingOfX),
		term__occurs(BindingOfX, Y, Bindings)
	).
term__occurs(term__functor(_F, As, _), Y, Bindings) :-
	term__occurs_list(As, Y, Bindings).

term__occurs_list([Term | Terms], Y, Bindings) :-
	( term__occurs(Term, Y, Bindings) ->
		true
	;
		term__occurs_list(Terms, Y, Bindings)
	).

%-----------------------------------------------------------------------------%

	% term__substitute(Term0, Var, Replacement, Term) :
	%	replace all occurrences of Var in Term0 with Replacement,
	%	and return the result in Term.

term__substitute(term__variable(Var), SearchVar, Replacement, Term) :-
	( Var = SearchVar ->
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
		error("term__substitute_corresponding_list: " ++
			"different length lists")
	).

:- pred term__substitute_corresponding_2(list(var(T))::in, list(term(T))::in,
	substitution(T)::in, substitution(T)::out) is semidet.

term__substitute_corresponding_2([], [], !Subst).
term__substitute_corresponding_2([S | Ss], [R | Rs], !Subst) :-
	map__set(!.Subst, S, R, !:Subst),
	term__substitute_corresponding_2(Ss, Rs, !Subst).

%-----------------------------------------------------------------------------%

term__apply_rec_substitution(term__variable(Var), Substitution, Term) :-
	( map__search(Substitution, Var, Replacement) ->
		% recursively apply the substition to the replacement
		term__apply_rec_substitution(Replacement, Substitution, Term)
	;
		Term = term__variable(Var)
	).
term__apply_rec_substitution(term__functor(Name, Args0, Context), Substitution,
		 term__functor(Name, Args, Context)) :-
	term__apply_rec_substitution_to_list(Args0, Substitution, Args).

term__apply_rec_substitution_to_list([], _Substitution, []).
term__apply_rec_substitution_to_list([Term0 | Terms0], Substitution,
		[Term | Terms]) :-
	term__apply_rec_substitution(Term0, Substitution, Term),
	term__apply_rec_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

term__apply_substitution(term__variable(Var), Substitution, Term) :-
	( map__search(Substitution, Var, Replacement) ->
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
term__init_var_supply(var_supply(0)).

	% We number variables using sequential numbers,

term__create_var(var_supply(V0), var(V), var_supply(V)) :-
	V = V0 + 1.

%------------------------------------------------------------------------------%

term__var_id(var(V)) = V.

%-----------------------------------------------------------------------------%

:- instance enum(var(_)) where [
	to_int(X) = term__var_to_int(X),
	from_int(X) = term__unsafe_int_to_var(X)
].

term__var_to_int(var(Var), Var).

	% Cast an integer to a var(T), subverting the type-checking.
:- func unsafe_int_to_var(int) = var(T).

term__unsafe_int_to_var(Var) = var(Var).

%-----------------------------------------------------------------------------%

	% substitute a variable name in a term.
term__relabel_variable(term__functor(Const, Terms0, Cont), OldVar, NewVar,
		term__functor(Const, Terms, Cont)) :-
	term__relabel_variables(Terms0, OldVar, NewVar, Terms).
term__relabel_variable(term__variable(Var0), OldVar, NewVar,
		term__variable(Var)) :-
	( Var0 = OldVar ->
		Var = NewVar
	;
		Var = Var0
	).

term__relabel_variables([], _, _, []).
term__relabel_variables([Term0|Terms0], OldVar, NewVar, [Term|Terms]):-
	term__relabel_variable(Term0, OldVar, NewVar, Term),
	term__relabel_variables(Terms0, OldVar, NewVar, Terms).

term__apply_variable_renaming(term__functor(Const, Args0, Cont), Renaming,
		term__functor(Const, Args, Cont)) :-
	term__apply_variable_renaming_to_list(Args0, Renaming, Args).
term__apply_variable_renaming(term__variable(Var0), Renaming,
		term__variable(Var)) :-
	term__apply_variable_renaming_to_var(Renaming, Var0, Var).

term__apply_variable_renaming_to_list([], _, []).
term__apply_variable_renaming_to_list([Term0|Terms0], Renaming, [Term|Terms]) :-
	term__apply_variable_renaming(Term0, Renaming, Term),
	term__apply_variable_renaming_to_list(Terms0, Renaming, Terms).

term__apply_variable_renaming_to_var(Renaming, Var0, Var) :-
	( map__search(Renaming, Var0, NewVar) ->
		Var = NewVar
	;
		Var = Var0
	).

term__apply_variable_renaming_to_vars(_Renaming, [], []).
term__apply_variable_renaming_to_vars(Renaming, [Var0 | Vars0],
		[Var | Vars]) :-
	term__apply_variable_renaming_to_var(Renaming, Var0, Var),
	term__apply_variable_renaming_to_vars(Renaming, Vars0, Vars).

%-----------------------------------------------------------------------------%

term__term_list_to_var_list(Terms, Vars) :-
	( term__var_list_to_term_list(Vars0, Terms) ->
		Vars = Vars0
	;
		error("term__term_list_to_var_list")
	).

term__var_list_to_term_list([], []).
term__var_list_to_term_list([Var | Vars], [term__variable(Var) | Terms]) :-
	term__var_list_to_term_list(Vars, Terms).

%-----------------------------------------------------------------------------%

term__is_ground(term__variable(V), Bindings) :-
	map__search(Bindings, V, Binding),
	term__is_ground(Binding, Bindings).
term__is_ground(term__functor(_, Args, _), Bindings) :-
	term__is_ground_2(Args, Bindings).

:- pred term__is_ground_2(list(term(T))::in, substitution(T)::in) is semidet.

term__is_ground_2([], _Bindings).
term__is_ground_2([Term | Terms], Bindings) :-
	term__is_ground(Term, Bindings),
	term__is_ground_2(Terms, Bindings).

%-----------------------------------------------------------------------------%

term__is_ground(term__functor(_, Args, _)) :-
	term__is_ground_2(Args).

:- pred term__is_ground_2(list(term(T))::in) is semidet.

term__is_ground_2([]).
term__is_ground_2([Term | Terms]) :-
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

term__generic_term(_).

%-----------------------------------------------------------------------------%

term__coerce(A, B) :-
	% Normally calls to this predicate should only be
	% generated by the compiler, but type coercion by
	% copying was taking about 3% of the compiler's runtime.
	private_builtin__unsafe_type_cast(A, B).

term__coerce_var(var(V), var(V)).

term__coerce_var_supply(var_supply(Supply), var_supply(Supply)).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
% 	Function forms added.

term__context_init = C :-
	term__context_init(C).

term__init_var_supply = VS :-
	term__init_var_supply(VS).

term__try_term_to_type(T) = TTTR :-
	term__try_term_to_type(T, TTTR).

term__det_term_to_type(T1) = T2 :-
	term__det_term_to_type(T1, T2).

term__type_to_term(T1) = T2 :-
	term__type_to_term(T1, T2).

term__univ_to_term(U) = T :-
	term__univ_to_term(U, T).

term__vars(T) = Vs :-
	term__vars(T, Vs).

term__vars_2(T, Vs1) = Vs2 :-
	term__vars_2(T, Vs1, Vs2).

term__vars_list(Ts) = Vs :-
	term__vars_list(Ts, Vs).

term__substitute(T1, V, T2) = T3 :-
	term__substitute(T1, V, T2, T3).

term__substitute_list(Ts1, V, T) = Ts2 :-
	term__substitute_list(Ts1, V, T, Ts2).

term__substitute_corresponding(Vs, T1s, T) = T2 :-
	term__substitute_corresponding(Vs, T1s, T, T2).

term__substitute_corresponding_list(Vs, Ts1, Ts2) = Ts3 :-
	term__substitute_corresponding_list(Vs, Ts1, Ts2, Ts3).

term__apply_rec_substitution(T1, S) = T2 :-
	term__apply_rec_substitution(T1, S, T2).

term__apply_rec_substitution_to_list(Ts1, S) = Ts2 :-
	term__apply_rec_substitution_to_list(Ts1, S, Ts2).

term__apply_substitution(T1, S) = T2 :-
	term__apply_substitution(T1, S, T2).

term__apply_substitution_to_list(Ts1, S) = Ts2 :-
	term__apply_substitution_to_list(Ts1, S, Ts2).

term__relabel_variable(T1, V1, V2) = T2 :-
	term__relabel_variable(T1, V1, V2, T2).

term__relabel_variables(Ts1, V1, V2) = Ts2 :-
	term__relabel_variables(Ts1, V1, V2, Ts2).

term__apply_variable_renaming(T1, M) = T2 :-
	term__apply_variable_renaming(T1, M, T2).

term__apply_variable_renaming_to_list(Ts1, M) = Ts2 :-
	term__apply_variable_renaming_to_list(Ts1, M, Ts2).

term__apply_variable_renaming_to_vars(M, Vs0) = Vs :-
	term__apply_variable_renaming_to_vars(M, Vs0, Vs).

term__apply_variable_renaming_to_var(M, V0) = V :-
	term__apply_variable_renaming_to_var(M, V0, V).

term__var_to_int(V) = N :-
	term__var_to_int(V, N).

term__context_line(C) = N :-
	term__context_line(C, N).

term__context_file(C) = S :-
	term__context_file(C, S).

term__context_init(S, N) = C :-
	term__context_init(S, N, C).

term__term_list_to_var_list(Ts) = Vs :-
	term__term_list_to_var_list(Ts, Vs).

term__var_list_to_term_list(Vs) = Ts :-
	term__var_list_to_term_list(Vs, Ts).

term__coerce(T1) = T2 :-
	term__coerce(T1, T2).

term__coerce_var(V1) = V2 :-
	term__coerce_var(V1, V2).

term__coerce_var_supply(VS1) = VS2 :-
	term__coerce_var_supply(VS1, VS2).
