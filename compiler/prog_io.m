%-----------------------------------------------------------------------------%

:- module prog_io.
:- import_module string, list, varset, term, io.
:- interface.

:- type message_list	==	list(string).
:- type maybe_program	--->	ok(message_list, program)
			;	error(message_list).
:- type program		==	list(item).
:- type item		--->	clause(varset, string, list(term), goal)
			; 	type_defn(varset, type_defn, goal)
			; 	module_defn(varset, module_defn)
			; 	pred(varset, string, list(type_body), goal)
			; 	rule(varset, string, list(type_body), goal)
			; 	error.

	% Here's how clauses and goals are represented.
	% (Constructs like "=>", "<=", and "<=>" are considered to be
	% just higher-order predicates, and so aren't represented
	% specially here.)

:- type clause		--->	clause(varset, string, list(term), goal).
			%	clause(VarSet, PredName, HeadArgs, ClauseBody)

:- type goal		--->	(goal,goal)
			;	fail	
					% could use conj(goals) instead 
			;	{goal;goal}	% {...} quotes ';'/2.
			;	true	
					% could use disj(goals) instead
			;	not(vars,goal)
			;	some(vars,goal)
			;	all(vars,goal)
			;	if_then(vars,goal,goal)
			;	if_then_else(vars,goal,goal,goal)
			;	call(term)
			;	error.

:- type goals		==	list(goal).
:- type vars		==	list(variable).

:- type int		==	integer.

%-----------------------------------------------------------------------------%

	% OK, this is how types are represented.

			% one day I'll allow value parameters as well
			% as type parameters.
:- type type_defn	--->	du_type(string, list(term), list(constructor))
			;	uu_type(string, list(term), list(type_body))
			;	eqv_type(string, list(term), type_body).
:- type constructor	==	term.
:- type type_head	==	term.
:- type type_body	==	term.
:- type (type)		==	term.

/** JUNK
:- type constructor	--->	string - list(type_body).
:- type type_head	--->	string - list(type_param).
:- type type_param	==	variable.
:- type type_body	--->	param(variable)
 			;	string - list(type_body).
**/


:- type condition	--->	true
			;	where(term).

%-----------------------------------------------------------------------------%

:- type module_defn	--->	module(module_name)
			;	interface
			;	implementation
			;	end_module(module_name)
			;	export(sym_list)
			;	import(sym_list)
			;	use(sym_list).
:- type sym_list	--->	sym(list(sym_specifier))
			;	pred(list(pred_specifier))
			;	cons(list(pred_specifier))
			;	op(list(op_specifier))
			;	adt(list(sym_name_specifier))
	 		;	type(list(sym_name_specifier))
	 		;	module(list(module_specifier)).
:- type sym_specifier	--->	sym(sym_name_specifier)
			;	typed_sym(typed_cons_specifier)
			;	pred(pred_specifier)
			;	cons(cons_specifier)
			;	op(op_specifier)
			;	adt(sym_name_specifier)
	 		;	type(sym_name_specifier)
	 		;	module(module_specifier).
:- type pred_specifier	--->	sym(sym_name_specifier)
			;	name_args(sym_name, list(type)).
:- type cons_specifier	--->	sym(sym_name_specifier)
			;	typed(typed_cons_specifier).
:- type typed_cons_specifier --->	
				name_args(sym_name, list(type))
			;	name_res(sym_name_specifier, type)
			;	name_args_res(sym_name,
						list(type), type).
:- type op_specifier	--->	sym(sym_name_specifier)
			;	fixity(sym_name_specifier, fixity).
:- type fixity		--->	infix ; prefix ; postfix.
:- type sym_name_specifier ---> name(sym_name)
			;	name_arity(sym_name, integer).
:- type sym_name 	--->	unqualified(string)
			;	qualified(module_specifier, string).

:- type module_specifier == string.
:- type module_name == string.

%-----------------------------------------------------------------------------%

% This module exports the following predicates:

:- pred prog_io__read_program(string, maybe_program, io__state, io__state).

% 	read_program(FileName, Result)
%	- reads and parses file 'FileName'. Result is either
%	  ok(Warnings,Program) or error(Messages) where Warnings
%	  is a list of warning messages, Program is the parse tree,
%	  and Messages is a list of warning/error messages.

%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

	% When actually reading in type declarations, we need to
	% check for errors.

:- type maybe(T)	--->	error(string, term)
			;	ok(T).
:- type maybe_functor	--->	error(string)
			;	ok(string, list(term)).
:- type maybe_type_defn	--->	error(string)
			;	type_defn(type_defn).
:- type maybe_module_defn =	maybe(module_defn).
:- type maybe_decl	=	maybe(item).

% This implementation uses io__read_term to read in the program
% term at a time, and then converts those terms into clauses and
% declarations, checking for errors as it goes.

prog_io__read_program(FileName, Result) -->
	io__op(1199, fx, "rule"),
	io__op(1179, xfy, "--->"),
	io__see(FileName, R),
	read_program_2(R, FileName, Result).

	% check that the file was opened succesfully
	% (this is a separate prediacte, because
	% you can't use if-then-else directly in DCGs
	% due to a NU-Prolog bug)

:- pred read_program_2(res, string, maybe_program, io__state, io__state).
read_program_2(ok, _, Program) -->
	read_program_3(Messages, Items, no, Error),
	{if Error = yes then
		Program = error(Messages)
	else
		Program = ok(Items, Messages)
	},
	io__seen.
read_program_2(error, FileName, error([Message])) -->
	{ io__progname(Progname),
	  % Message = Progname ++ ": can't open file '" ++ FileName ++ "'.\n"
	  append(Progname, ": can't open file '", Message1),
	  append(Message1, FileName, Message2),
	  append(Message2, "'.\n", Message)
	}.

%-----------------------------------------------------------------------------%
 	% Read a source file from standard input, first reading in
	% the input term by term and then parsing those terms and producing
	% a high-level representation.
	% Parsing is actually a 3-stage process instead of the
	% normal two-stage process:
	%	lexical analysis (chars -> tokens),
	% 	parsing stage 1 (tokens -> terms),
	%	parsing stage 2 (terms -> items).
	% The final stage produces a list of program items, each of
	% which may be a declaration or a rule.

:- type yes_or_no ---> yes ; no.
:- pred read_program_3(message_list, program, yes_or_no, yes_or_no,
			io__state, io__state).
read_program_3(Messages, Items, Error0, Error) -->
	io__read_term(Result),
	read_program_4(Result, Messages, Items, Error0, Error).

:- pred read_program_4(read_term,message_list,program,yes_or_no,yes_or_no,
			io__state,io__state).
read_program_4(eof, [], [], Error, Error) --> []. 
read_program_4(term(VarSet, Term), Messages, Items, Error0, Error) -->
	{ read_program_5(VarSet, Term, Messages, Items, Error0,
				 Messages1, Items1, Error1) },
 	read_program_3(Messages1, Items1, Error1, Error).



:- pred read_program_5(varset, term, message_list, program, yes_or_no,
		       message_list, program, yes_or_no).
read_program_5(VarSet, Term, Msgs, Items, Error0, Msgs1, Items1, Error) :-
	Items = Item.Items1,
 	(if some [Decl]
		Term = term_functor(term_atom(":-"), [Decl])
	then
		parse_decl(VarSet, Decl, Item, Msgs, Msgs1, Error1),
		join_error(Error0, Error1, Error)
	else
			% OK, it's not a declaration. Is it a fact, or a rule?
		(if some [H,B]
			Term = term_functor(term_atom(":-"), [H,B])
		then		% it's a fact
			Head = H,
			Body = B
		else		% it's a rule
			Head = Term,
			Body = term_functor(term_atom("true"), [])
		),
		parse_goal(Body, Body2),
		(if some [Name, Args]
			Head = term_functor(term_atom(Name),Args)
		then
			Item = clause(VarSet, Name, Args, Body2),
			Error = Error0,
			Msgs1 = Msgs
		else
			Item = error,
			Error = yes,
			add_error("atom expected in clause head", Msgs, Msgs1)
		)
	).

:- pred join_error(yes_or_no, yes_or_no, yes_or_no).
join_error(yes, _, yes).
join_error(no, Error, Error).

%-----------------------------------------------------------------------------%

	% parse a goal

:- pred parse_goal(term, goal).
parse_goal(Term, Goal) :-
	(if some Goal2 parse_goal_2(Term, Goal2) then
		Goal = Goal2
	else
		Goal = call(Term)
	).

:- pred parse_goal_2(term, goal).
parse_goal_2(term_functor(term_atom("true"),[]), true).
parse_goal_2(term_functor(term_atom("fail"),[]), fail).
parse_goal_2(term_functor(term_atom(","),[A0,B0]), (A,B)) :-
	parse_goal(A0, A),
	parse_goal(B0, B).
parse_goal_2(term_functor(term_atom(";"),[A0,B0]), (A;B)) :-
	parse_goal(A0, A),
	parse_goal(B0, B).
parse_goal_2(term_functor(term_atom("if"),
		[term_functor(term_atom("then"),[A0,B0])]),
		if_then_else(Vars,A,B,true)) :-
	parse_some_vars_goal(A0, Vars, A),
	parse_goal(B0, B).
parse_goal_2( term_functor(term_atom("else"),[
		    term_functor(term_atom("if"),[
			term_functor(term_atom("then"),[A0,B0])
		    ]),
		    C0
		]),
		if_then_else(Vars,A,B,C)) :-
	parse_some_vars_goal(A0, Vars, A),
	parse_goal(B0, B),
	parse_goal(C0, C).
parse_goal_2( term_functor(term_atom("not"), [A0]), not([],A) ) :-
	parse_goal(A0, A).
parse_goal_2( term_functor(term_atom("all"),[Vars0,A0]),all(Vars,A) ):-
	term_vars(Vars0, Vars),
	parse_goal(A0, A).

:- pred parse_some_vars_goal(term, vars, goal).
parse_some_vars_goal(A0, Vars, A) :-
	(if some [Vars0, A1]
		A0 = term_functor(term_atom("some"), [Vars0,A1])
	then
		term_vars(Vars0, Vars),
		parse_goal(A1, A)
	else
		Vars = [],
		parse_goal(A0, A)
	).

%-----------------------------------------------------------------------------%
:- pred parse_decl(varset, term, item, message_list, message_list, yes_or_no).
parse_decl(VarSet, F, ParsedDecl, Msgs, Msgs1, Error) :-
	(if some [Atom, As]
		F = term_functor(term_atom(Atom), As)
	then
		(if some [Result]
			process_decl(VarSet, Atom, As, Result)
		then
			parse_decl_2(Result, ParsedDecl, Msgs, Msgs1, Error)
		else
			ParsedDecl = error,
			Error = yes,
			add_error("unrecognized declaration", Msgs, Msgs1)
		)
	else
		ParsedDecl = error,
		Error = yes,
		add_error("atom expected after `:-'", Msgs, Msgs1)
	).

add_warning(Warning, Msg.Msgs, Msgs) :-
	string__append("warning: ", Warning, Msg).
add_error(Error, Msg.Msgs, Msgs) :-
	string__append("error: ", Error, Msg).

:- pred parse_decl_2(maybe_decl, item, message_list, message_list, yes_or_no).
parse_decl_2(error(ErrorMsg,_), error, ErrorMsg.Msgs1, Msgs1, yes).
parse_decl_2(ok(ParsedDecl), ParsedDecl, Msgs1, Msgs1, no).

	% process_decl(VarSet, Atom, Args, Result) succeeds if Atom(Args)
	% is a declaration and binds Result to a representation of that
	% declaration.
:- pred process_decl(varset, string, list(term), maybe_decl).
process_decl(VarSet, "type", [TypeDecl], Result) :-
	parse_type_decl(VarSet, TypeDecl, Result).

process_decl(VarSet, "pred", [PredDecl], Result) :-
	parse_type_decl_pred(VarSet, PredDecl, Result).

process_decl(VarSet, "rule", [RuleDecl], Result) :-
	parse_type_decl_rule(VarSet, RuleDecl, Result).

process_decl(VarSet, "import_module", [ModuleSpec], Result) :-
	parse_import_module_decl(VarSet, ModuleSpec, Result).

process_decl(VarSet, "use_module", [ModuleSpec], Result) :-
	parse_use_module_decl(VarSet, ModuleSpec, Result).

process_decl(VarSet, "export_module", [ModuleSpec], Result) :-
	parse_export_module_decl(VarSet, ModuleSpec, Result).

process_decl(VarSet, "import_pred", [PredSpec], Result) :-
	parse_import_pred_decl(VarSet, PredSpec, Result).

process_decl(VarSet, "use_pred", [PredSpec], Result) :-
	parse_use_pred_decl(VarSet, PredSpec, Result).

process_decl(VarSet, "import_pred", [PredSpec], Result) :-
	parse_export_pred_decl(VarSet, PredSpec, Result).

process_decl(VarSet, "export_pred", [PredSpec], Result) :-
	parse_export_pred_decl(VarSet, PredSpec, Result).

process_decl(VarSet, "import_type", [TypeSpec], Result) :-
	parse_import_type_decl(VarSet, TypeSpec, Result).

process_decl(VarSet, "use_type", [TypeSpec], Result) :-
	parse_use_type_decl(VarSet, TypeSpec, Result).

process_decl(VarSet, "export_type", [TypeSpec], Result) :-
	parse_export_type_decl(VarSet, TypeSpec, Result).

process_decl(VarSet, "interface", [], ok(module_defn(VarSet, interface))).
process_decl(VarSet, "implementation", [],
				ok(module_defn(VarSet, implementation))).

:- pred type_decl(varset, term, maybe_decl).
parse_type_decl(VarSet, TypeDecl, Result) :-
    (if some [R, Cond]
	parse_type_decl_type(TypeDecl, Cond, R) 
    then
	parse_type_decl_2(R, VarSet, Cond, Result)
    else
	Result = error("Invalid type declaration (need =, == or --->)", TypeDecl)
    ).

:- pred parse_type_decl_2(maybe_type_defn, varset, condition, maybe_decl).
parse_type_decl_2(error(Error, Term), _, _, error(Error, Term)).
parse_type_decl_2(ok(TypeDefn), VarSet, Cond,
					ok(type_defn(VarSet, TypeDefn, Cond))).
		% !! need to check condition for errs
		%    (don't bother at the moment, since we ignore
		%     conditions anyhow :-)
%-----------------------------------------------------------------------------%
	% parse_type_decl_type(Term, Condition, Result) succeeds
	% if Term is a "type" type declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.

:- pred parse_type_decl_type(term, condition, maybe_type_defn).

parse_type_decl_type(term_functor(term_atom("--->"),[H,B]), Condition, R) :-
	get_condition(B, Body, Condition),
	process_du_type(H, Body, R).

parse_type_decl_type(term_functor(term_atom("="),[H,B]), Condition, R) :-
	get_condition(B, Body, Condition),
	process_uu_type(H, Body, R).
	
parse_type_decl_type(term_functor(term_atom("=="),[H,B]), Condition, R) :-
	get_condition(B, Body, Condition),
	process_eqv_type(H, Body, R).

%-----------------------------------------------------------------------------%
	% parse_type_decl_pred(Pred, Condition, Result) succeeds
	% if Pred is a "pred" type declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.
:- pred parse_type_decl_pred(varset, term, maybe_decl).
parse_type_decl_pred(VarSet, Pred, R) :-
	get_condition(Pred, Body, Condition),
	process_pred(VarSet, Body, Condition, R).

%-----------------------------------------------------------------------------%
	% parse_type_decl_rule(VarSet, Rule, Result) succeeds
	% if Rule is a "rule" type declaration, and binds Result to
	% a representation of the declaration.
	% ("rule" here means DCG predicate, not horn clause.)
:- pred parse_type_decl_rule(varset, term, maybe_decl).
parse_type_decl_rule(VarSet, Rule, R) :-
	get_condition(Rule, Body, Condition),
	process_rule(VarSet, Body, Condition, R).

%-----------------------------------------------------------------------------%
	% get_condition(Term0, Term, Condition) binds Condition
	% to a representation of the 'where' condition of Term0, if any,
	% and binds Term to the other part of Term0. If Term0 does not
	% contain a condition, then Condition is bound to true.
:- pred get_condition(term, term, condition).
get_condition(B, Body, Condition) :-
	(if some [Body1, Condition1]
		B = term_functor(term_atom("where"), [Body1, Condition1])
	then
		Body = Body1,
		Condition = where(Condition1)
	else
		Body = B,
		Condition = true
	).

%-----------------------------------------------------------------------------%

	% This is for "Head = Body" (undiscriminated union) definitions.
:- pred process_uu_type(type_head, term, maybe_type_defn).
process_uu_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_uu_type_2(Result0, Body, Result).

:- pred process_uu_type_2(maybe_functor, term, maybe_type_defn).
process_uu_type_2(error(Error, Term), _, error(Error, Term)).
process_uu_type_2(ok(Name, Args), Body, ok(uu_type(Name,Args,List))) :-
		sum_to_list(Body, List).

%-----------------------------------------------------------------------------%

	% This is for "Head == Body" (equivalence) definitions.
:- pred process_eqv_type(type_head, term, maybe_type_defn).
process_eqv_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_eqv_type_2(Result0, Body, Result).

:- pred process_eqv_type_2(maybe_functor, term, maybe_type_defn).
process_eqv_type_2(error(Error, Term), _, error(Error, Term)).
process_eqv_type_2(ok(Name, Args), Body, ok(eqv_type(Name,Args,Body))).

%-----------------------------------------------------------------------------%

	% process_du_type(TypeHead, TypeBody, Result)
	% checks that its arguments are well formed, and if they are,
	% binds Result to a representation of the type information about the
	% TypeHead.
	% This is for "Head ---> Body" (constructor) definitions.
:- pred process_du_type(type_head, term, maybe_type_defn).
process_du_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_du_type_2(Result0, Body, Result).

:- pred process_du_type_2(maybe_functor, term, maybe_type_defn).
process_du_type_2(error(Error, Term), _, error(Error, Term)).
process_du_type_2(ok(Functor,Args), Body, Result) :-
	% check that body is a disjunction of constructors
	(if some [Constrs] 
		convert_constructors(Body, Constrs)
	then
		Result = ok(du_type(Functor, Args, Constrs))
	else
		Result = error("Invalid RHS of type definition", Body)
	).

%-----------------------------------------------------------------------------%
	
:- pred check_for_errors(term, term, maybe_functor).
check_for_errors(term_variable(V), _, 
		error("variable on LHS of type definition", term_variable(V))).
check_for_errors(term_functor(Functor,Args), Body, Result) :-
	check_for_errors_2(Functor, Args, Body, Term, Result).

:- pred check_for_errors_2(const, list(term), term, term, maybe_functor).
check_for_errors_2(term_float(_), _, _, Term, 
		error("type name can't be a floating point number", Term)).
check_for_errors_2(term_integer(_), _, _, Term,
		error("type name can't be an integer", Term)).
check_for_errors_2(term_string(_), _, _, Term,
		error("type name can't be a string", Term)).
check_for_errors_2(term_atom(Name), Args, Body, Term, Result) :-
	check_for_errors_3(Name, Args, Body, Term, Result).

:- pred check_for_errors_3(string, list(term), term, term, maybe_functor).
check_for_errors_3(Name, Args, Body, Term, Result) :-
	% check that all the head args are variables
	(if	some [Arg] (
			member(Arg, Args),
			all [Var] Arg ~= term_variable(Var)
		)
	then
		Result = error("Type parameters must be variables", Arg)
	else
	% check that all the head arg variables are distinct
	if	some [Arg2, OtherArgs] (
			member(Arg2, Args, Arg2.OtherArgs),
			member(Arg2, OtherArgs)
		)
	then
		Result = error("Repeated type parameters in LHS of type defn", Term)
	else
	% check that all the variables in the body occur in the head
	if	some [Var2] (
			term_contains_var(Body, Var2),
			not term_contains_var_list(Args, Var2)
		)
	then
		Result = error("Free type parameter in RHS of type definition",
				Var2)
	else
		Result = ok(Name, Args)
	).

%-----------------------------------------------------------------------------%

:- pred convert_constructors(term, list(term)).
convert_constructors(Body,Constrs) :-
	disjunction_to_list(Body, List),
	convert_constructors_2(List, Constrs).

:- pred convert_constructors_2(list(term), list(term)).
convert_constructors_2([], []).
convert_constructors_2(Term.Terms, Constr.Constrs) :-
	convert_constructor(Term, Constr),
	convert_constructors_2(Terms, Constrs).

	% true if argument is a valid constructor
	% (used to mean a functor all of whose arguments are variables,
	%  but now we allow any terms as arguments).
:- pred convert_constructor(term, term).
convert_constructor(term_functor(Functor,Args), term_functor(Functor,Args)).
	% all [Arg] member(Arg,Args) => some Var Arg = term_variable(Var).

%-----------------------------------------------------------------------------%

	% convert an disjunction to a list

:- pred disjunction_to_list(term, list(term)).
disjunction_to_list(Term, List) :-
	disjunction_to_list_2(Term, [], List).

:- pred disjunction_to_list_2(term, list(term), list(term)).
disjunction_to_list_2(Term, List0, List) :-
	(if some [L, R]
		Term = term_functor(term_atom(";"), [L, R])
	then
		disjunction_to_list_2(L, List0, List1),
		disjunction_to_list_2(R, List1, List)
	else
		List = Term.List0
	).

%-----------------------------------------------------------------------------%

	% convert a sum to a list

:- pred sum_to_list(term, list(term)).
sum_to_list(Term, List) :-
	sum_to_list_2(Term, [], List).

:- pred sum_to_list_2(term, list(term), list(term)).
sum_to_list_2(Term, List0, List) :-
	(if some [L, R]
		Term = term_functor(term_atom("+"), [L, R])
	then
		sum_to_list_2(L, List0, List1),
		sum_to_list_2(R, List1, List)
	else
		List = Term.List0
	).

%-----------------------------------------------------------------------------%

:- pred process_pred(varset, term, condition, maybe_decl).
process_pred(VarSet, PredType, Cond, Result) :-
	(if some [F,As]
		PredType = term_functor(term_atom(F), As)
	then
		Result = ok(pred(VarSet, F, As, Cond))
	else
		Result = error("atom expected after `:- pred' declaration",
				PredType)
	).

	% A rule declaration is just the same as a pred declaration,
	% except that there are two hidden arguments. 
:- pred process_rule(varset, term, condition, maybe_decl).
process_rule(VarSet, RuleType, Cond, Result) :-
	(if some [F,As]
		RuleType = term_functor(term_atom(F), As)
	then
		Result = ok(rule(VarSet, F, As, Cond))
	else
		Result = error("atom expected after `:- rule' declaration",
				RuleType)
	).

/** JUNK
process_rule(VarSet, RuleType, Cond, Result) :-
	varset__new_var(VarSet, Var, VarSet1),
	RuleType = term_functor(F, RuleArgs),
	append(RuleArgs, [Var, Var], PredArgs),
	PredType = term_functor(F, PredArgs),
	process_pred(VarSet1, PredType, Cond, Result).
**/

%-----------------------------------------------------------------------------%

% parse import, use, and export module declarations

:- pred parse_import_module_decl(varset, term, maybe_decl).
parse_import_module_decl(VarSet, ModuleSpec, Result) :-
	parse_module_spec_list(ModuleSpec, R),
	process_import(R, Result).

:- pred parse_use_module_decl(varset, term, maybe_decl).
parse_use_module_decl(VarSet, ModuleSpec, Result) :-
	parse_module_spec_list(ModuleSpec, R),
	process_use(R, Result).

:- pred parse_export_module_decl(varset, term, maybe_decl).
parse_export_module_decl(VarSet, ModuleSpec, Result) :-
	parse_module_spec_list(ModuleSpec, R),
	process_export(R, Result).

% import, use, and export predicate

:- pred import_module_decl(varset, term, maybe_decl).
parse_import_pred_decl(VarSet, ModuleSpec, Result) :-
	parse_pred_spec_list(ModuleSpec, R),
	process_import(R, Result).

:- pred parse_use_pred_decl(varset, term, maybe_decl).
parse_use_pred_decl(VarSet, ModuleSpec, Result) :-
	parse_pred_spec_list(ModuleSpec, R),
	process_use(R, Result).

:- pred parse_export_pred_decl(varset, term, maybe_decl).
parse_export_pred_decl(VarSet, ModuleSpec, Result) :-
	parse_pred_spec_list(ModuleSpec, R),
	process_export(R, VarSet, Result).

% import, use, and export type

:- pred parse_import_type_decl(varset, term, maybe_decl).
parse_import_type_decl(VarSet, ModuleSpec, Result) :-
	parse_type_spec_list(ModuleSpec, R),
	process_import(R, Result).

:- pred parse_use_type_decl(varset, term, maybe_decl).
parse_use_type_decl(VarSet, ModuleSpec, Result) :-
	parse_type_spec_list(ModuleSpec, R),
	process_use(R, Result).

:- pred parse_export_type_decl(varset, term, maybe_decl).
parse_export_type_decl(VarSet, ModuleSpec, Result) :-
	parse_type_spec_list(ModuleSpec, R),
	process_export(R, VarSet, Result).

%-----------------------------------------------------------------------------%

	% Parse a comma-separated list (misleading described as
	% a "conjunction") of type specifiers.

:- pred parse_type_spec_list(term, maybe(list(sym_name_specifier))).
:- mode parse_type_spec_list(input, output).
parse_type_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_type_spec_list_2(List, Result).

:- pred parse_type_spec_list_2(list(term), maybe(list(sym_name_specifier))).
:- mode parse_type_spec_list_2(input, output).
parse_type_spec_list_2([], ok([])).
parse_type_spec_list_2(X.Xs, Result) :-
	parse_type_specifier(X, X_Result),
	parse_type_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

	% If a list of things contains multiple errors, then we only
	% report the first one.

:- pred combine_list_results(maybe(T), maybe(list(T)), maybe(list(T))).
:- mode combine_list_results(input, input, output).
combine_list_results(error(Msg, Term), _, error(Msg, Term)).
combine_list_results(ok(_), error(Msg, Term), error(Msg, Term)).
combine_list_results(ok(X), ok(Xs), ok([X|Xs])).

%-----------------------------------------------------------------------------%
%	A symbol specifier is one of
%		SymbolNameSpecifier
%			Matches any symbol matched by the SymbolNameSpecifier.
%		TypedConstructorSpecifier
%			Matches any constructors matched by the
%			TypedConstructorSpecifier.
%		cons(ConstructorSpecifier)
%			Matches only constructors.
%		pred(PredSpecifier)
%			Matches only predicates, ie. constructors of type
%			`pred'.
%		adt(SymbolNameSpecifier)
%			Matches only type names.
%		type(SymbolNameSpecifier)
%			Matches type names matched by the SymbolNameSpecifier,
%			and also matches any constructors for the matched type
%			names.
%		op(SymbolNameSpecifier)
%			Matches only operators.
%		module(Module)
%			Matches all symbols in the specified module.

:- pred parse_symbol_specifier(term, maybe(sym_specifier)).
parse_symbol_specifier(Term, Result) :-
	(if some [ConsSpecTerm]
	    Term = term_functor(term_atom("cons"), [ConsSpecTerm])
	then
	    parse_constructor_specifier(ConsSpecTerm, ConsSpecResult),
	    process_cons_symbol_specifier(ConsSpecResult, Result)
	else if some [PredSpecTerm]
	    Term = term_functor(term_atom("pred"), [PredSpecTerm])
	then
	    parse_predicate_specifier(PredSpecTerm, PredSpecResult),
	    process_pred_symbol_specifier(PredSpecResult, Result)
	else if some [TypeSpecTerm]
	    Term = term_functor(term_atom("type"), [TypeSpecTerm])
	then
	    parse_type_specifier(TypeSpecTerm, TypeSpecResult),
	    process_type_symbol_specifier(TypeSpecResult, Result)
	else if some [AdtSpecTerm]
	    Term = term_functor(term_atom("adt"), [AdtSpecTerm])
	then
	    parse_adt_specifier(AdtSpecTerm, AdtSpecResult),
	    process_adt_symbol_specifier(AdtSpecResult, Result)
	else if some [OpSpecTerm]
	    Term = term_functor(term_atom("op"), [OpSpecTerm])
	then
	    parse_op_specifier(OpSpecTerm, OpSpecResult),
	    process_op_symbol_specifier(OpSpecResult, Result)
	else if some [ModuleSpecTerm]
	    Term = term_functor(term_atom("module"), [ModuleSpecTerm])
	then
	    parse_module_specifier(ModuleSpecTerm, ModuleSpecResult),
	    process_module_symbol_specifier(ModuleSpecResult, Result)
	else
	    parse_constructor_specifier(Term, TermResult),
	    process_any_symbol_specifier(TermResult, Result)
	).

%	A ConstructorSpecifier is one of
%		SymbolNameSpecifier
%		TypedConstructorSpecifier
%
%	A TypedConstructorSpecifier is one of
%		SymbolNameSpecifier::Type
%			Matches only constructors with the specified result
%			type.
%		SymbolName(ArgType1, ..., ArgTypeN)
%			Matches only constructors with the specified argument
%			types.
%		SymbolName(ArgType1, ..., ArgTypeN)::Type
%			Matches only constructors with the specified argument
%			and result types.

:- pred parse_constructor_specifier(term, maybe(cons_specifier)).
parse_constructor_specifier(Term, Result) :-
    (if some [NameArgsTerm, TypeTerm]
	Term = term_functor(term_atom("::"), NameArgsTerm, TypeTerm)
    then
	parse_arg_types_specifier(NameArgsTerm, NameArgsResult),
	parse_type(TypeTerm, TypeResult),
	process_typed_constructor_specifier(NameArgsResult, TypeResult, Result)
    else
	parse_arg_types_specifier(Term, TermResult),
	process_untyped_constructor_specifier(TermResult, Result)
    ).

%	A PredicateSpecifier is one of
%		SymbolName(ArgType1, ..., ArgTypeN)
%			Matches only predicates with the specified argument
%			types.
%		SymbolNameSpecifier

:- pred parse_predicate_specifier(term, maybe(pred_specifier)).
parse_predicate_specifier(Term, Result) :-
    (if Term = term_functor(term_atom("/"), [_,_]) then
	parse_symbol_name_specifier(Term, TermResult),
        process_arity_predicate_specifier(TermResult, Result)
    else
	parse_symbol_name_args(Term, TermResult),
	process_typed_predicate_specifier(TermResult, Result)
    ).
    
:- pred process_typed_predicate_specifier(maybe(pair(sym_name_specifier, list(term))), maybe(pred_specifier)).
process_typed_predicate_specifier(ok(pair(Name, Args)), ok(Result)) :-
    (if Args = [] then
	Result = sym(name(Name))
    else
	Result = name_args(Name, Args)
    ).
process_typed_predicate_specifier(error(Msg, Term), error(Msg, Term)).

:- pred process_arity_predicate_specifier(maybe(sym_name_specifier),
		maybe(pred_specifier)).
process_arity_predicate_specifier(ok(Result), ok(sym(Result))).
process_arity_predicate_specifier(error(Msg, Term), error(Msg, Term)).

%	A SymbolNameSpecifier is one of
%		SymbolName
%		SymbolName/Arity
%			Matches only symbols of the specified arity.
%	

:- pred parse_symbol_name_specifier(term, maybe(sym_name_specifier)).
parse_symbol_name_specifier(Term, Result) :-
    (if some [NameTerm, ArityTerm]
       	Term = term_functor(term_atom("/"), [NameTerm, ArityTerm])
    then
        (if some [Arity]
            ArityTerm = term_functor(term_integer(Arity),[])
	then
            (if Arity >= 0 then
		parse_symbol_name(NameTerm, NameResult),
		process_name_arity_specifier(NameResult, Arity, Result)
	    else
		Result = error("Arity in symbol name specifier must be a non-negative integer", Term)
	    )
        else
	    Result = error("Arity in symbol name specifier must be an integer", Term)
        )
    else
	parse_symbol_name(Term, SymbolNameResult),
	process_name_specifier(SymbolNameResult, Result)
    ).

:- pred process_name_arity_specifier(maybe(sym_name), int,
		maybe(sym_name_specifier)).
process_name_arity_specifier(ok(Name), Arity, ok(name_arity(Name, Arity))).
process_name_arity_specifier(error(Error, Term), _, error(Error, Term)).

:- pred process_name_specifier(maybe(sym_name), maybe(sym_name_specifier)).
process_name_specifier(ok(Name), ok(name(Name))).
process_name_specifier(error(Error, Term), error(Error, Term)).

%	A SymbolName is one of
%		Name
%			Matches symbols with the specified name in the
%			current namespace.
%		Module:Name
%			Matches symbols with the specified name exported
%			by the specified module.

:- pred parse_symbol_name_args(term, maybe(pair(sym_name,list(term)))).
parse_symbol_name_args(Term, Result) :-
    (if some [ModuleTerm, NameArgsTerm]
       	Term = term_functor(term_atom(":"), [ModuleTerm, NameArgsTerm])
    then
        (if some [Name, Args]
            NameArgsTerm = term_functor(term_atom(Name),Args)
        then
            (if some [Module]
                ModuleTerm = term_functor(term_atom(Module),[])
	    then
		Result = ok(pair(qualified(Module, Name), Args))
	    else
		Result = error("module name identifier expected before ':' in qualified symbol name", Term)
            )
        else
            Result = error("identifier expected after ':' in qualified symbol name", Term)
	)
    else
        (if some [Name2, Args2]
            Term = term_functor(term_atom(Name2), Args2)
        then
            Result = ok(pair(unqualified(Name2), Args2))
        else
            Result = error("symbol name specifier expected", Term)
        )
    ).

%	A SymbolName is one of
%		Name
%			Matches symbols with the specified name in the
%			current namespace.
%		Module:Name
%			Matches symbols with the specified name exported
%			by the specified module.

:- pred parse_symbol_name(term, maybe(sym_name)).
parse_symbol_name(Term, Result) :-
    (if some [ModuleTerm, NameTerm]
       	Term = term_functor(term_atom(":"), [ModuleTerm, NameTerm])
    then
        (if some [Name]
            NameTerm = term_functor(term_atom(Name),[])
        then
            (if some [Module]
                ModuleTerm = term_functor(term_atom(Module),[])
	    then
		Result = ok(qualified(Module, Name))
	    else
		Result = error("module name identifier expected before ':' in qualified symbol name", Term)
            )
        else
            Result = error("identifier expected after ':' in qualified symbol name", Term)
	)
    else
        (if some [Name2]
            Term = term_functor(term_atom(Name2),[])
        then
            Result = ok(unqualified(Name2))
        else
            Result = error("symbol name specifier expected", Term)
        )
    ).

%-----------------------------------------------------------------------------%

% convert a module definition to a program item,
% propagating errors upwards

:- pred process_import(maybe_module_defn, varset, maybe_decl).
process_import(ok(X), VarSet, ok(module_defn(VarSet, import(X)))).
process_import(error(X), _, error(X)).

:- pred process_use(maybe_module_defn, varset, maybe_decl).
process_use(ok(X), VarSet, ok(module_defn(VarSet, use(X)))).
process_use(error(X), _, error(X)).

:- pred process_export(maybe_module_defn, varset, maybe_decl).
process_export(ok(X), VarSet, ok(module_defn(VarSet, export(X)))).
process_export(error(X), _, error(X)).

%-----------------------------------------------------------------------------%

% print an error message

:- pred print_error(string,io__state,io__state).
print_error(Error) -->
	% io__tell("user_output"),
	io__write_string("Error: "),
	io__write_string(Error),
	io__write_string("\n").
	% io__told.

%-----------------------------------------------------------------------------%
