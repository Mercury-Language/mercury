/******************************************************************************

This file contains a type-checker which I started writing a fair while
ago.  It still needs quite a bit of work to integrate it with
the rest of the stuff in this directory and to get it to actually work.

******************************************************************************/

:- import_module io, bag.

%-----------------------------------------------------------------------------%

	% A program is a list of type declarations, a list of other
	% declarations (to be passed on to the underlying implementation), and
	% a list of clauses.
:- type program		--->	program(list(decl), list(type_decl),
					list(clause)).

%-----------------------------------------------------------------------------%
	% A declaration other than a type declaration.
	% We just store these as terms and pass them on to the underlying
	% implementation.
:- type decl		--->	decl(varset, term).
	
%-----------------------------------------------------------------------------%
	% Terms. We use a ground representation.

:- type term		--->	term_functor(const,list(term))
			;	term_variable(variable).
:- type const 		--->	term_atom(string)
			;	term_integer(integer)
			;	term_float(float).
:- type variable	==	var_id.

%-----------------------------------------------------------------------------%
	% Here's a non-ground representation for terms.
	% Not used at the moment.
:- type term(T)		--->	term_functor(const, list(term(T)))
			;	term_variable(T).
%-----------------------------------------------------------------------------%

	% Here's how clauses and goals are represented.
	% Constructs like "=>", "<=", "<=>", and "all" are translated
	% into their equivalents in the simplified structure which
	% follows.

:- type clause		--->	clause(varset, string, list(term), goal).
			%	clause(VarSet, PredName, HeadArgs, ClauseBody)

:- type goal		--->	(goal,goal)
			;	fail	
					% could use conj(goals) instead 
			;	(goal;goal)
			;	true	
					% could use disj(goals) instead
			;	not(goal)
			;	some(variable,goal)
					% could use some(vars,goal)
			;	if_then_else(vars,goal,goal,goal)
					% redundant? we could use
					% if_then_else(V,A,B,C) ==>
					% 	(A,B);not(some(V,A)),C
					% except that this duplicates A
			;	call(string,list(term)).

:- type goals		==	list(goal).
:- type vars		==	list(variable).

%-----------------------------------------------------------------------------%

	% When actually reading in type declarations, we need to
	% check for errors.

:- type maybe_functor	--->	error(string)
			;	ok(const, list(term)).
:- type maybe_type_defn	--->	error(string)
			;	type_defn(type_defn).
:- type type_decl 	--->	type(type_defn)
			;	error(string).

%-----------------------------------------------------------------------------%

	% OK, this is how types are represented.

:- type type_defn	--->    is_type(varset, type_head, type, condition).
:- type type_head	--->	string - list(type_param).
:- type type_param	--->	param(variable)
			;	val(id, type_body).
:- type type		--->	du_type(list(constructor))
			;	uu_type(list(type_body))
			;	eqv_type(type_body)
			;	pred_type(list(type_body)).
:- type constructor	--->	string - list(type_body).
:- type type_body	--->	param(variable)
			;	string - list(type_body).
:- type condition	--->	true ; where(goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Validate command line arguments

main_predicate([]) --> usage.
main_predicate([_]) --> usage.
main_predicate([_,File]) --> process_file(File).
main_predicate([_,_,_|_]) --> usage.

	% Display usage message
:- pred usage(io_state, io_state).
usage -->
	{ progname(Progname) },
 	io__write_string("Fergus' type-checker version 0.1\n"),
 	io__write_string("Usage: "),
	io__write_string(Progname),
	io__write_string(" filename\n").

%-----------------------------------------------------------------------------%

	% Add appropriate operatator definitions,
	% open the file, and process it.

:- pred process_file(string, io_state, io_state).
process_file(File) -->
	io__write_string("Reading program...\n"),
	read_program(File, Program, Messages, Result),

	process_file_2(

	read_program(Program, TypeDecls, OtherDecls),
	io__write_string("Writing declarations...\n"),
	write_decls(OtherDecls),
	io__write_string("Type-checking...\n"),
	typecheck(Program, TypeDecls).


%-----------------------------------------------------------------------------%
%
% 1) descriminated unions:
%	?- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2) undescriminated unions (if rhs has a single type then the two
%	types have same structure but *different* name):
%	?- type number = int + float.
%
% 3) equivalent types (treated identically, ie, same name.  Any number
%	of types can be equivalent; the *canonical* one is the one
%	which is not defined using ==):
%	?- type real == float.
%
% 4) pred declarations:
%	?- pred app(list, list, list).
%
% builtin types: (these have special syntax)
%	char, int, float
%	pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ...
% system types:
%	module <system>: array(T), list(T), string.
%	module <io>: io_state.
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

type_check_goal(true) --> [].
type_check_goal(fail) --> [].
type_check_goal((A,B)) -->
	type_check_goal(A),
	type_check_goal(B).
type_check_goal((A;B),T0,T) -->
	type_check_goal(A,T0,T1),
	type_check_goal(B,T0,T2),
	type_join(T0,T1,T).
type_check_goal(not(A)) -->
	type_check_goal(A).
type_check_goal(some(Vs,G)) -->
	type_new_var(Vs),
	type_check_goal(G).
type_check_goal(call(PredName,Args)) -->
	type_check_call_pred(PredName,Args).

%-----------------------------------------------------------------------------%

typecheck(Program, TypeDecls) -->
	{ types_init(Types0) },
	io__write_string("Processing type declarations...\n"),
	process_types(TypeDecls, Types0, Types),
	io__write_string("Checking for undefined types...\n"),
	check_undefined_types(Types),
	check_circular_eqv_types(Types),
	io__write_string("Type-checking clauses...\n"),
	check_pred_types(Program, Types).

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

check_clause_2(_, _, term_variable(_), _, "clause head is free variable").
/****
check_clause_2(Types, VarSet, term_functor(F,Args), Body, error(X)) :-
	length(Args, Arity),
	length(ArgTypes, Arity),
	types_pred_type(Types, F, _VarSet, ArgTypes, _Cond),
	check_has_type(
*******/

%-----------------------------------------------------------------------------%

	% !! At the moment we don't check for circular eqv types.
check_circular_eqv_types(_Types) --> [].

%-----------------------------------------------------------------------------%

	% Check for any possible undefined types.

check_undefined_types(Types) -->
	{ solutions(UndefType, undefined_type(Types, UndefType), List) },
	write_undefined_types(List).

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
types_lookup_type(_, BuiltInType, VarSet, [], term_functor(Const,[]), true) :-
	builtin_type(Const, BuiltInType),
	varset_init(VarSet).
    % user-defined types
types_lookup_type(Types, Functor, VarSet, Args, term_functor(F1,As1), Cond) :-
	bag_contains(Types, type(VarSet, du_type(Functor, Args, RHS), Cond)),
	length(As1,Arity),
	length(As2,Arity),
	member(term_functor(F1, As2), RHS).

	% builtin_type(Term, Type)
	%	is true iff 'Term' is a constant of the builtin type 'Type'.
:- pred builtin_type(const, string).
builtin_type(term_integer(_),"integer").
builtin_type(term_float(_),"float").
builtin_type(term_atom(String),"char") :-
	char_to_string(_,String).

%-----------------------------------------------------------------------------%

	% Given a list of type declarations and errors,
	% write the errors to stdout and build our type data structure
	% from the type declarations.
	% At the moment our type data is very simple, just a bag (list)
	% of all the declarations, but we want need to change this to
	% improve efficiency later.

process_types([], Types, Types) --> [].
process_types(D.Ds, Types0, Types) -->
	process_types_2(D, Types0, Types1),
	process_types(Ds, Types1, Types).

process_types_2(error(Error), Types, Types) -->
	print_error(Error).
process_types_2(type(VarSet,TypeDecl,Cond), Types0, Types) -->
	{ bag_insert(Types0, type(VarSet, TypeDecl, Cond), Types) }.
process_types_2(pred(VarSet,PredDecl,Cond), Types0, Types) -->
	{ bag_insert(Types0, pred(VarSet, PredDecl, Cond), Types) }.

%-----------------------------------------------------------------------------%

% member(X, X._).
% member(X, _.Xs) :- member(X, Xs).

/* This version uses first-argument indexing to avoid creating a choice
   point when a the item matches with the _last_ element in the list.
   So this version will sometimes create fewer choice points than the
   original.
*/
member(X, H.T) :-
	member2(T, H, X).

member2([], X, X).
member2(_._, X, X).
member2(H.T, _, X) :-
	member2(T, H, X).

%-----------------------------------------------------------------------------%
