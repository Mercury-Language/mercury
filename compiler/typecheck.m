/******************************************************************************

% Main author: fjh.

This file contains a type-checker which I started writing a fair while
ago.  It still needs quite a bit of work to integrate it with
the rest of the stuff in this directory and to get it to actually work.

******************************************************************************/

:- import_module io, bag.

%-----------------------------------------------------------------------------%
	io__write_string("Type-checking...\n"),
%-----------------------------------------------------------------------------%
%
% There are three sorts of types:
%
% 1) discriminated unions:
%	:- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2) undiscriminated unions (if rhs has a single type then the two
%	types have same structure but *different* name):
%	Basically just syntactic sugar for discriminated unions,
%	except that it also works for builtin types like int.
%
%	:- type t1 ---> a ; b.
%	:- type t2 ---> c ; d.
%
%		% same as type t3 ---> a ; b ; c ; d.
%	:- type t3 = t1 + t2.
%
%	:- type number = int + float.
%
% 3) equivalent types (treated identically, ie, same name.  Any number
%	of types can be equivalent; the *canonical* one is the one
%	which is not defined using ==):
%	:- type real == float.
%
% 4) pred declarations:
%	:- pred app(list(T), list(T), list(T)).
%
% builtin types: (these have special syntax)
%	char, int, float
%	pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ...
% system types:
%	module <system>: array(T), list(T), string.
%	module <io>: io__state.
%	module <lowlevel>: byte, word, ...
% generally useful types:
%	list(T) ---> [] | T.list(T).
%	string == list(char).
%	integer(Min,Max) == integer where (X : Min <= X, X <= Max).
%	real == float.
%	func(Dom,Ran) == pred(Dom,Ran)
%		where (Func : all [X] some [Y] Func(X,Y),
%			all [X,Y1,Y2] (Func(X,Y1),Func(X,Y2) => Y1 = Y2)).
%
%-----------------------------------------------------------------------------%

type_check_clause(clause(VarSet,PredName,Args,Body), GlobalTypes, Types) :-
	types_init(VarSet, GlobalTypes, Types0),
	type_check_clause_2(PredName, Args, Body, Types0, Types).

type_check_clause_2(PredName, Args, Body) -->
	type_check_call_pred(PredName,Args),
	type_check_goal(Body).

type_check_goal(Goal - _GoalInfo) -->
	type_check_goal_2(Goal).

type_check_goal_2(true) --> [].
type_check_goal_2(fail) --> [].
type_check_goal_2((A,B)) -->
	type_check_goal(A),
	type_check_goal(B).
type_check_goal_2((A;B)) -->
	type_check_goal(A),
	type_check_goal(B).
type_check_goal_2(if_then_else(_Vs,A,B,C)) -->
	type_check_goal(A),
	type_check_goal(B),
	type_check_goal(C).
type_check_goal_2(not(_Vs,A)) -->
	type_check_goal(A).
type_check_goal_2(some(_Vs,G)) -->
	type_check_goal(G).
type_check_goal_2(all(_Vs,G)) -->
	type_check_goal(G).
type_check_goal_2(call(PredName,_Mode,Args,_Builtin)) -->
	type_check_call_pred(PredName,Args).
type_check_goal_2(unify(A,B,_Info)) -->
	type_check_unify(A,B).

%-----------------------------------------------------------------------------%

typecheck(Module) -->
	io__write_string("Checking for undefined types...\n"),
	check_undefined_types(Module),
	check_circular_eqv_types(Module),
	io__write_string("Type-checking clauses...\n"),
	check_pred_types(Module).

%-----------------------------------------------------------------------------%

check_pred_types([], _) --> [].
check_pred_types(clause(VarSet,Head,Body).Clauses, Types) -->
	{ check_clause(Types, VarSet, Head, Body, Result) },
	check_pred_types_2(Result),
	check_pred_types(Clauses, Types).

check_pred_types_2(ok) --> [].
check_pred_types_2(error(Error)) --> print_error(Error).

check_clause(Types, VarSet, Head, Body, Result) :-
	(if some [Error] check_clause_2(Types, VarSet, Head, Body, Error) then
		Result = error(Error)
	else
		Result = ok
	).

%-----------------------------------------------------------------------------%

	% XXX - At the moment we don't check for circular eqv types.
check_circular_eqv_types(_Module) --> [].

%-----------------------------------------------------------------------------%

	% Check for any possible undefined types.

check_undefined_types(Module) -->
	{ solutions(UndefType, undefined_type(Types, UndefType), List) },
	write_undefined_types(List).

:- pred write_undefined_types(list(type_id), io__state, io__state).
:- mode write_undefined_types(input, di, uo).

write_undefined_types([]) --> [].
write_undefined_types((F/N).Ts) -->
	io__write_string("Undefined type "),
	io__write_string(F),
	io__write_string("/"),
	io__write_int(N),
	io__write_string(".\n"),
	write_undefined_types(Ts).

	% true if Type/Arity is an undefined type
:- type func_arity ---> string / integer.
:- pred undefined_types(types, func_arity).
undefined_type(Types, Type/Arity) :-
	types_type_template(Types, _Name, _VarSet, _TypeArgs, Template, _Cond),
	undefined_type_2(Template, Types, Type, Arity).

	% true if the type template contains a type which is not defined.
undefined_type_2(functor(Term), Types, Type, Arity) :-
	term_contains_functor(Term, term_atom(Type), Args),
	Term ~= term_functor(term_atom(Type), Args),
	length(Args, Arity),
	not (some [ArgTemplate,X1,X2,X3] (
		length(ArgTemplate, Arity),
		types_type_template(Types, Type, X1, ArgTemplate, X2, X3)
	)).
undefined_type_2(type(Term), Types, Type, Arity) :-
	term_contains_functor(Term, term_atom(Type), Args),
	length(Args, Arity),
	not (some [ArgTemplate,X1,X2,X3] (
		length(ArgTemplate, Arity),
		types_type_template(Types, Type, X1, ArgTemplate, X2, X3)
	)).

%-----------------------------------------------------------------------------%

:- type ok_type_decl 	--->	type(varset, type_defn, condition)
			;	pred(varset, term, condition).
:- type types = bag(ok_type_decl).

:- pred types_init(types).
types_init(Types) :-
	bag_init(Types).

	% given a type name, let Args be the type parameters for that type
	% and let Term be a valid type template for that type.
:- type type_template	--->	functor(term)
			;	type(term).
:- pred types_type_template(types, string, varset, list(variable),
				type_template, condition).
types_type_template(Types, Name, VarSet, Args, Template, Cond) :-
	bag_contains(Types, type(VarSet, TypeDecl, Cond)),
	types_type_template_2(TypeDecl, Name, Args, Template).
types_type_template(_, Name, VarSet, [], Template, true) :-
	builtin_type(Const, Name),
	Template = functor(term_functor(Const,[])), 
	varset_init(VarSet).

:- pred types_type_template_2(type_defn, string, list(variable), type_template).
types_type_template_2(du_type(Name, Args, RHS), Name, Args, functor(Term)) :-
	member(Term, RHS).
types_type_template_2(uu_type(Name, Args, RHS), Name, Args, type(Term)) :-
	member(Term, RHS).
types_type_template_2(eqv_type(Name, Args, Term), Name, Args, type(Term)).

	% Given a predicate name, look up the types of its arguments
:- pred types_pred_type(types, string, varset, list(term), condition).
types_pred_type(Types, Functor, VarSet, ArgTypes, Cond) :-
	bag_contains(Types, pred(VarSet, term_functor(Functor,ArgTypes), Cond)).

%-----------------------------------------------------------------------------%

    % builtin types
:- mode types_lookup_type(input, output, output, output, input, output).
types_lookup_type(_, BuiltInType, VarSet, [], term_functor(Const,[]), true) :-
	builtin_type(Const, BuiltInType),
	varset_init(VarSet).
    % user-defined types
types_lookup_type(Types, Functor, VarSet, Args, term_functor(F1,As1), Cond) :-
	bag_contains(Types, type(VarSet, du_type(Functor, Args, RHS), Cond)),
	length(As1,Arity),
	length(As2,Arity),
	member(term_functor(F1, As2), RHS).

%-----------------------------------------------------------------------------%

	% builtin_type(Term, Type)
	%	is true iff 'Term' is a constant of the builtin type 'Type'.

:- pred builtin_type(const, string).
:- mode builtin_type(input, output).

builtin_type(term_integer(_), "integer").
builtin_type(term_float(_), "float").
builtin_type(term_atom(String), "char") :-
	char_to_string(_, String).

%-----------------------------------------------------------------------------%

/* This version of member uses first-argument indexing to avoid creating
   a choice point when a the item matches with the _last_ element in the list.
   So this version will sometimes create fewer choice points than the
   original.
*/
:- pred my_member(T, list(T)).
:- mode my_member(output, input).
my_member(X, H.T) :-
	my_member_2(T, H, X).

:- pred my_member_2(list(T), T, T).
:- mode my_member_2(input, input, output).

my_member_2([], X, X).
my_member_2(_._, X, X).
my_member_2(H.T, _, X) :-
	my_member_2(T, H, X).

%-----------------------------------------------------------------------------%
