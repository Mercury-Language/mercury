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
:- import_module list, map.

%-----------------------------------------------------------------------------%

:- type term		--->	term__functor(const, list(term), term__context)
			;	term__variable(var).
:- type const		--->	term__atom(string)
			;	term__integer(int)
			;	term__string(string)
			;	term__float(float).

:- type term__context   --->    term__context(string, int).
				% file name, line number.

:- type var.
:- type var_supply.

%-----------------------------------------------------------------------------%

	% The following predicates can convert values of (almost)
	% any type to the type `term' and back again.

:- type term_to_type_result(T)
	--->	ok(T)
	;	error(term_to_type_error).

:- pred term__try_term_to_type(term, term_to_type_result(T)).
:- mode term__try_term_to_type(in, out) is det.
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

:- type term_to_type_error
	--->	type_error(term, type_info, term__context,
			term_to_type_context)
	;	mode_error(var, term_to_type_context).

:- type term_to_type_context == list(term_to_type_arg_context).

:- type term_to_type_arg_context
	--->	arg_context(
			const,		% functor
			int,		% argument number (starting from 1)
			term__context	% filename & line number
		).

:- pred term__term_to_type(term, T).
:- mode term__term_to_type(in, out) is semidet.
	% term_to_type(Term, Type) :- try_term_to_type(Term, ok(Type)).

:- pred term__det_term_to_type(term, T).
:- mode term__det_term_to_type(in, out) is det.
	% like term_to_type, but calls error/1 rather than failing.

:- pred term__type_to_term(T, term).
:- mode term__type_to_term(in, out) is det.
	% converts a value to a term representation of that value

:- pred term__univ_to_term(univ, term).
:- mode term__univ_to_term(in, out) is det.
	% calls term__type_to_term on the value stored in the univ
	% (as distinct from the univ itself).

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

:- pred term__apply_variable_renaming(term, map(var, var), term).
:- mode term__apply_variable_renaming(in, in, out) is det.
% 		same as term__relabel_variable, except relabels
% 		multiple variables. If a variable is not in the
% 		map, it is not replaced.

:- pred term__apply_variable_renaming_to_list(list(term), map(var, var),
							 list(term)).
:- mode term__apply_variable_renaming_to_list(in, in, out) is det.
%		applies term__apply_variable_renaming to a list of terms.
		

:- pred term__is_ground(term, substitution).
:- mode term__is_ground(in, in) is semidet.
%	term__is_ground(Term, Bindings) is true iff no variables contained
%		in Term are non-ground in Bindings.

:- pred term__is_ground(term).
:- mode term__is_ground(in) is semidet.
%	term__is_ground(Term) is true iff Term contains no variables.

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

:- pred term__context_line(term__context, int).
:- mode term__context_line(in, out) is det.

	% Given a term context, return the source file.

:- pred term__context_file(term__context, string).
:- mode term__context_file(in, out) is det.

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.

:- pred term__context_init(term__context).
:- mode term__context_init(out) is det.

:- pred term__context_init(string, int, term__context).
:- mode term__context_init(in, in, out) is det.

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
:- import_module std_util, require, uniq_array, int, string.

%-----------------------------------------------------------------------------%

:- type var_supply	==	int.
:- type var		==	int.

%-----------------------------------------------------------------------------%

term__term_to_type(Term, Val) :-
	term__try_term_to_type(Term, ok(Val)).

term__try_term_to_type(Term, Result) :-
	term__try_term_to_univ(Term, type_of(ValTypedVar), UnivResult),
	(
		UnivResult = ok(Univ),
		det_univ_to_type(Univ, Val),
		same_type(Val, ValTypedVar),
		Result = ok(Val)
	;
		UnivResult = error(Error),
		Result = error(Error)
	).

:- pred term__try_term_to_univ(term::in, type_info::in,
		term_to_type_result(univ)::out) is det.

term__try_term_to_univ(Term, Type, Result) :-
	term__try_term_to_univ_2(Term, Type, [], Result).
	
:- pred term__try_term_to_univ_2(term::in, type_info::in,
		term_to_type_context::in,
		term_to_type_result(univ)::out) is det.

term__try_term_to_univ_2(term__variable(Var), _Type, Context,
		error(mode_error(Var, Context))).
term__try_term_to_univ_2(Term, Type, Context, Result) :-
	Term = term__functor(Functor, ArgTerms, TermContext),
	(
		type_ctor_and_args(Type, TypeCtor, TypeArgs),
		term__term_to_univ_special_case(
			type_ctor_name(TypeCtor), TypeArgs,
			Term, Type, Context, SpecialCaseResult)
	->
		Result = SpecialCaseResult
	;
		Functor = term__atom(FunctorName),
		list__length(ArgTerms, Arity),
		find_functor(Type, FunctorName, Arity, FunctorNumber, ArgTypes),
		term__term_list_to_univ_list(ArgTerms, ArgTypes,
			Functor, 1, Context, TermContext, ArgsResult)
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

:- pred term__term_to_univ_special_case(string::in, list(type_info)::in,
		term::in(bound(term__functor(ground, ground, ground))),
		type_info::in, term_to_type_context::in,
		term_to_type_result(univ)::out) is semidet.
term__term_to_univ_special_case("character", [], Term, _, _, ok(Univ)) :-
	Term = term__functor(term__atom(FunctorName), [], _),
	string__first_char(FunctorName, Char, ""),
	type_to_univ(Char, Univ).
term__term_to_univ_special_case("int", [], Term, _, _, ok(Univ)) :-
	Term = term__functor(term__integer(Int), [], _),
	type_to_univ(Int, Univ).
term__term_to_univ_special_case("string", [], Term, _, _, ok(Univ)) :-
	Term = term__functor(term__string(String), [], _),
	type_to_univ(String, Univ).
term__term_to_univ_special_case("float", [], Term, _, _, ok(Univ)) :-
	Term = term__functor(term__float(Float), [], _),
	type_to_univ(Float, Univ).
term__term_to_univ_special_case("uniq_array", [ElemType], Term, _Type,
		PrevContext, Result) :-
	%
	% uniq_arrays are represented as terms of the form
	%	uniq_array([elem1, elem2, ...])
	%
	Term = term__functor(term__atom("uniq_array"), [ArgList], TermContext),

	% To convert such terms back to uniq_arrays, we first
	% convert the term representing the list of elements back to a list,
	% and then (if successful) we just call the uniq_array/1 function.
	%
	ListTypeCtor = type_ctor(type_of([0])),
	ListType = det_make_type(ListTypeCtor, [ElemType]),
	ArgContext = arg_context(term__atom("uniq_array"), 1, TermContext),
	NewContext = [ArgContext | PrevContext],
	term__try_term_to_univ_2(ArgList, ListType, NewContext, ArgResult),
	(
		ArgResult = ok(ListUniv),
/***************
% XXX existential types not yet implemented...
		% :- some [T] pred has_type(T::unused, type_info::in) is det.
		has_type(List, ListType),
		det_univ_to_type(ListUniv, List),
		Array = uniq_array(List),
		Result = ok(univ(Array))
****************/
		% since we don't have existential types, we have to use
		% some unsafe casts...
		require_equal(univ_type(ListUniv), ListType),
		list_of_any(List),   % explicit type qualification
				     % to avoid unbound type variables
		List = unsafe_cast(univ_value_as_type_any(ListUniv)),
		Array = uniq_array(List),
		ArrayTypeCtor = type_ctor(type_of(Array)),
		ArrayType = det_make_type(ArrayTypeCtor, [ElemType]),
		Result = ok(unsafe_any_to_univ(ArrayType, unsafe_cast(Array)))
	;
		ArgResult = error(Error),
		Result = error(Error)
	).
term__term_to_univ_special_case("c_pointer", _, _, _, _, _) :-
	fail.
term__term_to_univ_special_case("univ", _, _, _, _, _) :-
	% Implementing this properly would require keeping a
	% global table mapping from type names to type_infos
	% for all of the types in the program...
	% so for the moment, we don't allow it.
	fail.
term__term_to_univ_special_case("type_info", _, _, _, _, _) :-
	% ditto
	fail.

:- pred same_type(T::unused, T::unused) is det.
same_type(_, _).

:- pred term__term_list_to_univ_list(list(term)::in, list(type_info)::in,
		term__const::in, int::in, term_to_type_context::in,
		term__context::in, term_to_type_result(list(univ))::out)
		is semidet.
term__term_list_to_univ_list([], [], _, _, _, _, ok([])).
term__term_list_to_univ_list([ArgTerm|ArgTerms], [Type|Types],
		Functor, ArgNum, PrevContext, TermContext, Result) :-
	ArgContext = arg_context(Functor, ArgNum, TermContext),
	NewContext = [ArgContext | PrevContext],
	term__try_term_to_univ_2(ArgTerm, Type, NewContext, ArgResult),
	(
		ArgResult = ok(Arg),
		term__term_list_to_univ_list(ArgTerms, Types, Functor,
			ArgNum + 1, PrevContext, TermContext, RestResult),
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

:- pred term__find_functor(type_info::in, string::in, int::in, int::out,
		list(type_info)::out) is semidet.
term__find_functor(Type, Functor, Arity, FunctorNumber, ArgTypes) :-
	N = num_functors(Type),
	term__find_functor_2(Type, Functor, Arity, N, FunctorNumber, ArgTypes).
        
:- pred term__find_functor_2(type_info::in, string::in, int::in, int::in, 
	int::out, list(type_info)::out) is semidet.
term__find_functor_2(TypeInfo, Functor, Arity, Num, FunctorNumber, ArgTypes) :-
	Num >= 0,
	Num1 = Num - 1,
	(
		get_functor(TypeInfo, Num1, Functor, Arity, ArgTypes1)
	->
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
		error("term__det_term_to_type failed, because the term wasn't ground")
	;
		string__append_list([
			"term__det_term_to_type failed, due to a type error:\n",
			"the term wasn't a valid term for type `",
			type_name(type_of(X)),
			"'"], Message),
		error(Message)
	).

%-----------------------------------------------------------------------------%

:- pred det_univ_to_type(univ::in, T::out) is det.
det_univ_to_type(Univ, Value) :-
	( univ_to_type(Univ, Value1) ->
		Value = Value1
	;
		error("det_univ_to_type: univ_to_type failed")
	).

%-----------------------------------------------------------------------------%

/**********
XXX existential types not yet implemented
:- some [T] pred has_type(T::unused, type_info::in) is det.
:- pragma c_code(has_type(Arg::unused, TypeInfo::in), will_not_call_mercury,
	"TypeInfo_for_T = TypeInfo;"
).
**********/

:- func unsafe_cast(T1::in) = (T2::out) is det.
:- pragma c_code(unsafe_cast(VarIn::in) = (VarOut::out), will_not_call_mercury,
	"VarOut = VarIn;").

:- type any == c_pointer.

:- pred array_of_any(uniq_array(any)::unused) is det.
array_of_any(_).

:- pred list_of_any(list(any)::unused) is det.
list_of_any(_).

:- func univ_value_as_type_any(univ) = any.
:- pragma c_code(univ_value_as_type_any(Univ::in) = (Val::out),
		will_not_call_mercury,
"
	Val = field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA);
").

:- func unsafe_any_to_univ(type_info, any) = univ.
	% Forward mode - convert from type to univ.
	% Allocate heap space, set the first field to contain the address
	% of the type_info for this type, and then store the input argument
	% in the second field.
:- pragma c_code(unsafe_any_to_univ(TypeInfo::in, Value::in) = (Univ::out),
		will_not_call_mercury,
"
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = (Word) TypeInfo;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = (Word) Value;
").

%-----------------------------------------------------------------------------%

term__type_to_term(Val, Term) :-
	type_to_univ(Val, Univ),
	term__univ_to_term(Univ, Term).

term__univ_to_term(Univ, Term) :-
	term__context_init(Context),
	Type = univ_type(Univ),
	% NU-Prolog barfs on `num_functors(Type) < 0'
	( num_functors(Type) = N, N < 0 ->
		(
			type_ctor_and_args(Type, TypeCtor, TypeArgs),
			TypeName = type_ctor_name(TypeCtor),
			term__univ_to_term_special_case(TypeName, TypeArgs,
				Univ, Context, SpecialCaseTerm)
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
		deconstruct(Univ, FunctorString, _FunctorArity, FunctorArgs),
		term__univ_list_to_term_list(FunctorArgs, TermArgs),
		Term = term__functor(term__atom(FunctorString), TermArgs,
			Context)
	).

:- pred term__univ_to_term_special_case(string::in, list(type_info)::in,
		univ::in, term__context::in, term::out) is semidet.

term__univ_to_term_special_case("int", [], Univ, Context,
		term__functor(term__integer(Int), [], Context)) :-
	det_univ_to_type(Univ, Int).
term__univ_to_term_special_case("float", [], Univ, Context,
		term__functor(term__float(Float), [], Context)) :-
	det_univ_to_type(Univ, Float).
term__univ_to_term_special_case("character", [], Univ, Context,
		term__functor(term__atom(CharName), [], Context)) :-
	det_univ_to_type(Univ, Character),
	string__char_to_string(Character, CharName).
term__univ_to_term_special_case("string", [], Univ, Context,
		term__functor(term__string(String), [], Context)) :-
	det_univ_to_type(Univ, String).
term__univ_to_term_special_case("type_info", [], Univ, Context,
		term__functor(term__atom("type_info"), [Term], Context)) :-
	det_univ_to_type(Univ, TypeInfo),
	type_info_to_term(Context, TypeInfo, Term).
term__univ_to_term_special_case("univ", [], Univ, Context, Term) :-
	Term = term__functor(term__atom("univ"),
			% XXX what operator should we use for type
			% qualification?
			[term__functor(term__atom(":"),	 % TYPE_QUAL_OP
				[ValueTerm, TypeTerm],
				Context)], Context),
/****
XXX existential types not implemented
	det_univ_to_type(univ_value(Univ), UnivValue),
****/
	% instead, we use some unsafe casts...
	require_equal(univ_type(Univ), type_of(UnivValue)),
	UnivValue = unsafe_cast(univ_value_as_type_any(Univ)),

	type_info_to_term(Context, univ_type(UnivValue), TypeTerm),
	term__univ_to_term(UnivValue, ValueTerm).

term__univ_to_term_special_case("uniq_array", [ElemType], Univ, Context,
		Term) :-
	Term = term__functor(term__atom("uniq_array"), [ArgsTerm], Context),
	ListTypeCtor = type_ctor(type_of([0])),
	ListType = det_make_type(ListTypeCtor, [ElemType]),
/***
XXX existential types not yet implemented
	has_type(List, ListType),
	det__univ_to_type(Univ, Array),	
	uniq_array__to_list(Array, List),
	term__type_to_term(List, ArgsTerm).
***/
	% instead, we need to use some unsafe casts...
	array_of_any(Array), % explicit type qualification
			     % to avoid unbound type variables
	Array = unsafe_cast(univ_value_as_type_any(Univ)),	
	uniq_array__to_list(Array, List),
	ListUniv = unsafe_any_to_univ(ListType, unsafe_cast(List)),
	term__univ_to_term(ListUniv, ArgsTerm).

:- pred term__univ_list_to_term_list(list(univ)::in,
				list(term)::out) is det.

term__univ_list_to_term_list([], []).
term__univ_list_to_term_list([Value|Values], [Term|Terms]) :-
	term__univ_to_term(Value, Term),
	term__univ_list_to_term_list(Values, Terms).

% given a type_info, return a term that represents the name of that type.
:- pred type_info_to_term(term__context::in, type_info::in, term::out) is det.
type_info_to_term(Context, TypeInfo, Term) :-
	type_ctor_and_args(TypeInfo, TypeCtor, ArgTypes),
	TypeName = type_ctor_name(TypeCtor),
	list__map(type_info_to_term(Context), ArgTypes, ArgTerms),
	Term = term__functor(term__atom(TypeName), ArgTerms, Context).

:- pred require_equal(T::in, T::in) is det.
require_equal(X, Y) :-
	( X = Y ->
		true
	;
		error("require_equal failed")
	).

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

term__contains_var_list([T|_], V) :-
	term__contains_var(T, V).
term__contains_var_list([_|Ts], V) :-
	term__contains_var_list(Ts, V).

%-----------------------------------------------------------------------------%

	% term__contains_functor(Term, Functor, Args):
	%	term__functor(Functor, Args, _) is a subterm of Term.
	%
	% CURRENTLY NOT USED.

:- pred term__contains_functor(term, const, list(term)).
% :- mode term__contains_functor(in, in, in) is semidet.
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
:- mode term__subterm(in, out) is multidet.

term__subterm(Term, Term).
term__subterm(term__functor(_, Args, _), SubTerm) :-
	list__member(Term, Args),
	term__subterm(Term, SubTerm).

%-----------------------------------------------------------------------------%

	% Access predicates for the term__context data structure.

	% Given a term context, return the source line number.

term__context_line(term__context(_, LineNumber), LineNumber).

	% Given a term context, return the source file.

term__context_file(term__context(FileName, _), FileName).

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.

term__context_init(term__context("", 0)).

term__context_init(File, LineNumber, term__context(File, LineNumber)).

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
:- mode term__unify_list(in, in, in, out) is semidet.

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

	% We number variables using sequential numbers,

term__create_var(VarSupply0, VarSupply, VarSupply) :-
	VarSupply is VarSupply0 + 1.

%-----------------------------------------------------------------------------%

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


term__apply_variable_renaming(term__functor(Const, Args0, Cont), Renaming,
				 term__functor(Const, Args, Cont)) :-
	term__apply_variable_renaming_to_list(Args0, Renaming, Args).
term__apply_variable_renaming(term__variable(Var0), Renaming,
				 term__variable(Var)) :-
	(
		map__search(Renaming, Var0, NewVar)
	->
		Var = NewVar
	;
		Var = Var0
	).	

term__apply_variable_renaming_to_list([], _, []).
term__apply_variable_renaming_to_list([Term0|Terms0], Renaming, [Term|Terms]) :-
	term__apply_variable_renaming(Term0, Renaming, Term),
	term__apply_variable_renaming_to_list(Terms0, Renaming, Terms).

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
term__var_list_to_term_list([Var | Vars], [term__variable(Var) | Terms]) :-
	term__var_list_to_term_list(Vars, Terms).

%-----------------------------------------------------------------------------%

term__is_ground(term__variable(V), Bindings) :-
	map__search(Bindings, V, Binding),
	term__is_ground(Binding, Bindings).
term__is_ground(term__functor(_, Args, _), Bindings) :-
	term__is_ground_2(Args, Bindings).

:- pred term__is_ground_2(list(term), substitution).
:- mode term__is_ground_2(in, in) is semidet.

term__is_ground_2([], _Bindings).
term__is_ground_2([Term|Terms], Bindings) :-
	term__is_ground(Term, Bindings),
	term__is_ground_2(Terms, Bindings).

%-----------------------------------------------------------------------------%

term__is_ground(term__functor(_, Args, _)) :-
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
