%-----------------------------------------------------------------------------%

% Main author: fjh.

% This program converts Mercury source code into Goedel.

% TODO:
% (Crucial)
%	handle Mercury's overloaded functors 
%
% (Important)
%	handle Mercury's implicit quantification;
%	implement the Mercury IO library in Goedel;
%	implement various NU-Prolog predicates in Goedel; 
%	handle undiscriminated union types;
%
% (Wish list)
%	indent `ELSE IF' properly
% 	translate mode declarations into delay declarations;
%	translate Mercury's module system declarations into Goedel;
%	add a command-line option to output line number comments
%	preserve the comments in the original source code.

%-----------------------------------------------------------------------------%

:- module mercury_to_goedel.
:- interface.

:- import_module string, list, prog_io, io.

:- pred convert_to_goedel(string, list(item_and_context), io__state, io__state).
:- mode convert_to_goedel(input, input, di, uo).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module prog_io, prog_out, prog_util.

%-----------------------------------------------------------------------------%

	% XXX - These predicates should be command-line options.

:- pred option_write_line_numbers.
option_write_line_numbers :- fail.

	% The following is a hard-coded hack.

:- pred option_handle_functor_overloading(string).
:- mode option_handle_functor_overloading(input).

option_handle_functor_overloading("character").

%-----------------------------------------------------------------------------%

convert_to_goedel(ProgName, Items) -->
	io__stderr_stream(StdErr),
	io__write_string(StdErr, "% Expanding equivalence types..."),
	io__flush_output(StdErr),
	{ goedel_expand_eqv_types(Items, Items3) },
	{ goedel_replace_int_integer(Items3, Items4) },
	io__write_string(StdErr, " done\n"),
	{ convert_functor_name(ProgName, GoedelName) },
	{ string__append(GoedelName, ".loc", OutputFileName) },
	io__tell(OutputFileName, Res),
	( { Res = ok } ->
		io__write_string(StdErr, "% Writing output to "),
		io__write_string(StdErr, OutputFileName),
		io__write_string(StdErr, "...\n"),
		io__write_string("MODULE       "),
		io__write_string(GoedelName),
		io__write_string(".\n"),
		io__write_string("IMPORT       MercuryCompat.\n"),
		io__write_string("\n"),
		goedel_output_item_list(Items4),
		io__write_string(StdErr, "% done\n"),
		io__told
	;
		io__write_string(StdErr, "Error: couldn't open file `"),
		io__write_string(StdErr, OutputFileName),
		io__write_string(StdErr, "' for output.\n")
	).

%-----------------------------------------------------------------------------%

	% Mercury uses "int" as the integer type, whereas Goedel uses
	% "Integer", so we need to replace all occurrences of int with integer.

:- pred goedel_replace_int_integer(list(item_and_context),
					list(item_and_context)).
:- mode goedel_replace_int_integer(input, output).

goedel_replace_int_integer(Items0, Items) :-
	varset__init(VarSet),
	term__context_init(0, Context),
	goedel_replace_eqv_type_list(Items0, VarSet, "int", [],
		term_functor(term_atom("integer"), [], Context), Items).
	
%-----------------------------------------------------------------------------%

	% add the declarations one by one to the module

:- pred goedel_output_item_list(list(item_and_context), io__state, io__state).
:- mode goedel_output_item_list(input, di, uo).

goedel_output_item_list([]) --> [].
goedel_output_item_list([Item - Context | Items]) -->
	( goedel_output_item(Item, Context) ->
		[]
	;
		% goedel_output_item should always succeed
		% if it fails, report an internal error

		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("mercury_to_goedel internal error.\n"),
		io__write_string("Failed to process the following item:\n"),
		io__write_anything(Item),
		io__write_string("\n"),
		io__set_output_stream(OldStream, _)
	),
	goedel_output_item_list(Items).

%-----------------------------------------------------------------------------%

:- pred goedel_output_item(item, term__context, io__state, io__state).
:- mode goedel_output_item(input, input, di, uo).

	% dispatch on the different types of items

goedel_output_item(type_defn(VarSet, TypeDefn, _Cond), Context) -->
	io__write_string("\n"),
	goedel_output_type_defn(VarSet, TypeDefn, Context).

goedel_output_item(inst_defn(VarSet, InstDefn, _Cond), Context) -->
	goedel_output_inst_defn(VarSet, InstDefn, Context).

goedel_output_item(mode_defn(VarSet, ModeDefn, _Cond), Context) -->
	goedel_output_mode_defn(VarSet, ModeDefn, Context).

goedel_output_item(pred(VarSet, PredName, TypesAndModes, _Det, _Cond), Context)
		-->
	( { option_write_line_numbers } ->
		io__write_string("\n"),
		prog_out__write_context(Context)
	),
	io__write_string("\n"),
	goedel_output_pred(VarSet, PredName, TypesAndModes, Context).

goedel_output_item(mode(VarSet, PredName, Modes, _Det, _Cond), Context) -->
	goedel_output_mode(VarSet, PredName, Modes, Context).

goedel_output_item(module_defn(_VarSet, _ModuleDefn), _Context) -->
	% io__write_string("warning: module declarations not yet supported.\n").
	[].

goedel_output_item(clause(VarSet, PredName, Args, Body), Context) -->
	( { option_write_line_numbers } ->
		prog_out__write_context(Context),
		io__write_string("\n")
	),
	goedel_output_clause(VarSet, PredName, Args, Body, Context).

goedel_output_item(nothing, _) --> [].

%-----------------------------------------------------------------------------%

:- pred goedel_output_inst_defn(varset, inst_defn, term__context,
			io__state, io__state).
:- mode goedel_output_inst_defn(input, input, input, di, uo).

goedel_output_inst_defn(_VarSet, _InstDefn, _Context) -->
	% io__write_string("% inst definitions not supported\n"),
	[].

:- pred goedel_output_mode_defn(varset, mode_defn, term__context,
			io__state, io__state).
:- mode goedel_output_mode_defn(input, input, input, di, uo).

goedel_output_mode_defn(_VarSet, _ModeDefn, _Context) -->
	% io__write_string("% mode definitions not supported\n"),
	[].

%-----------------------------------------------------------------------------%

:- pred goedel_output_type_defn(varset, type_defn, term__context,
			io__state, io__state).
:- mode goedel_output_type_defn(input, input, input, di, uo).

goedel_output_type_defn(VarSet, TypeDefn, Context) -->
	goedel_output_type_defn_2(TypeDefn, VarSet, Context).

:- pred goedel_output_type_defn_2(type_defn, varset, term__context,
			io__state, io__state).
:- mode goedel_output_type_defn_2(input, input, input, di, uo).

goedel_output_type_defn_2(uu_type(_Name, _Args, _Body), _VarSet, Context) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string("warning: undiscriminated union types not yet supported.\n"),
	io__set_output_stream(OldStream, _).

goedel_output_type_defn_2(abstract_type(_Name, _Args), _VarSet, Context) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string("warning: abstract type definition ignored.\n"),
	io__set_output_stream(OldStream, _).

goedel_output_type_defn_2(eqv_type(_Name, _Args, _Body), _VarSet, Context) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string("mercury_to_goedel internal error:\n"),
	io__write_string("equivalence type unexpected.\n"),
	io__set_output_stream(OldStream, _).

goedel_output_type_defn_2(du_type(Name, Args, Ctors), VarSet, Context) -->
	{ unqualify_name(Name, Name2) },
	{ convert_functor_name(Name2, Name3) },
	( { option_handle_functor_overloading(Name2) } ->
		{ string__append("Type__", Name3, TypeModule) },
		{ string__append(TypeModule, ".exp", TypeModuleExport) },
		{ string__append(TypeModule, ".loc", TypeModuleLocal) },
		io__output_stream(OldStream),
		io__tell(TypeModuleExport, _Res1),	% XXX handle errors
		io__write_string("EXPORT       "),
		io__write_string(TypeModule),
		io__write_string(".\n"),
		io__write_string("IMPORT       MercuryCompat.\n"),
		io__write_string("\n"),
		goedel_output_type_defn_3(Name2, Name3, Args, Ctors, VarSet,
				Context),
		io__told,
		io__tell(TypeModuleLocal, _Res2),	% XXX handle errors
		io__write_string("LOCAL       "),
		io__write_string(TypeModule),
		io__write_string(".\n"),
		io__told,
		io__set_output_stream(OldStream, _),
		io__write_string("IMPORT       "),
		io__write_string(TypeModule),
		io__write_string(".\n")
	;
		goedel_output_type_defn_3(Name2, Name3, Args, Ctors, VarSet,
				Context)
	).

:- pred goedel_output_type_defn_3(string, string, list(term), list(constructor),
			varset, term__context, io__state, io__state).
:- mode goedel_output_type_defn_3(input, input, input, input, input, input,
			di, uo).

goedel_output_type_defn_3(Name2, Name3, Args, Ctors, VarSet, Context) -->
	( { option_write_line_numbers } ->
		prog_out__write_context(Context),
		io__write_string("\n")
	),
	{ length(Args, Arity) },
	(if
		{ Arity = 0 }
	then
		io__write_string("BASE         "),
		io__write_string(Name3)
	else
		io__write_string("CONSTRUCTOR  "),
		io__write_string(Name3),
		io__write_string("/"),
		io__write_int(Arity)
	),
	io__write_string(".\n"),
	goedel_output_ctors(Ctors,
		term_functor(term_atom(Name2), Args, Context), VarSet).

:- pred goedel_output_ctors(list(constructor), type, varset,
				io__state, io__state).
:- mode goedel_output_ctors(input, input, input, di, uo).

goedel_output_ctors([], _, _) --> [].
goedel_output_ctors([Name - ArgTypes | Ctors], Type, VarSet) -->
	{ unqualify_name(Name, Name2),
	  convert_functor_name(Name2, Name3) },
	(if %%% some [Type | Rest]
		{ ArgTypes = [ArgType | Rest] }
	then
		io__write_string("FUNCTION     "),
		io__write_string(Name3),
		{ length(ArgTypes, Arity) },
		(if
			{ Arity = 2, goedel_infix_op(Name2) }
		then
			io__write_string(" : xFx(100)")
		else if
			{ Arity = 1, goedel_unary_prefix_op(Name2) }
		then
			io__write_string(" : Fx(100)")
		else if
			{ Arity = 1, goedel_unary_postfix_op(Name2) }
		then
			io__write_string(" : xF(100)")
		else
			[]
		),
		io__write_string(" : "),
		goedel_output_term(ArgType, VarSet),
		goedel_output_remaining_types(Rest, VarSet),
		io__write_string(" -> ")
	else
		io__write_string("CONSTANT     "),
		io__write_string(Name3),
		io__write_string(" : ")
	),
	goedel_output_term(Type, VarSet),
	io__write_string(".\n"),
	goedel_output_ctors(Ctors, Type, VarSet).

%-----------------------------------------------------------------------------%

:- pred goedel_output_pred(varset, sym_name, list(type_and_mode),
		term__context, io__state, io__state).
:- mode goedel_output_pred(input, input, input, input, di, uo).

goedel_output_pred(VarSet, PredName, TypesAndModes, Context) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	goedel_output_pred_type(VarSet, PredName, Types, Context),
	(if %%% some [Modes]
		{ MaybeModes = yes(Modes) }
	then
		goedel_output_mode(VarSet, PredName, Modes, Context)
	else
		[]
	).

:- pred goedel_output_pred_type(varset, sym_name, list(type),
		term__context, io__state, io__state).
:- mode goedel_output_pred_type(input, input, input, input, di, uo).

goedel_output_pred_type(VarSet, PredName, Types, _Context) -->
	{ unqualify_name(PredName, PredName2),
	  convert_functor_name(PredName2, PredName3) },
	(if %%% some [Type | Rest]
		{ Types = [Type | Rest] }
	then
		io__write_string("PREDICATE    "),
		io__write_string(PredName3),
		{ length(Types, Arity) },
		(if
			{ Arity = 2, goedel_infix_pred(PredName2) }
		then
			io__write_string(" : zPz")
		else
			[]
		),
		io__write_string(" : "),
		goedel_output_type(Type, VarSet),
		goedel_output_remaining_types(Rest, VarSet)
	else
		io__write_string("PROPOSITION  "),
		io__write_string(PredName3)
	),
	io__write_string(".\n").

:- pred goedel_output_remaining_types(list(type), varset, io__state, io__state).
:- mode goedel_output_remaining_types(input, input, di, uo).

goedel_output_remaining_types([], _VarSet) --> [].
goedel_output_remaining_types([Type | Types], VarSet) -->
	io__write_string(" * "),
	goedel_output_type(Type, VarSet),
	goedel_output_remaining_types(Types, VarSet).

%-----------------------------------------------------------------------------%

	% Output a mode declaration for a predicate.

:- pred goedel_output_mode(varset, sym_name, list(mode), term__context,
			io__state, io__state).
:- mode goedel_output_mode(input, input, input, input, di, uo).

goedel_output_mode(_VarSet, _PredName, _Modes, _Context) -->
	% io__write_string("% warning: mode declarations not supported.\n"),
	[].

%-----------------------------------------------------------------------------%

	% Output a clause.

:- pred goedel_output_clause(varset, sym_name, list(term), goal, term__context,
			io__state, io__state).
:- mode goedel_output_clause(input, input, input, input, input, di, uo).

goedel_output_clause(VarSet, PredName, Args, Body, Context) -->
	{ unqualify_name(PredName, PredName2) },
	goedel_output_term(term_functor(term_atom(PredName2), Args, Context),
			VarSet),
	(if 
		{ Body = true }
	then
		[]
	else
		io__write_string(" <-\n\t"),
		goedel_output_goal(Body, VarSet, 1)
	),
	io__write_string(".\n").

:- pred goedel_output_goal(goal, varset, int, io__state, io__state).
:- mode goedel_output_goal(input, input, input, di, uo).

goedel_output_goal(fail, _, _) -->
	io__write_string("False").

goedel_output_goal(true, _, _) -->
	io__write_string("True").

goedel_output_goal(some(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		goedel_output_goal(Goal, VarSet, Indent)
	;
		io__write_string("(SOME ["),
		goedel_output_vars(Vars, VarSet),
		io__write_string("] "),
		{ Indent1 is Indent + 1 },
		goedel_output_newline(Indent1),
		goedel_output_goal(Goal, VarSet, Indent1),
		goedel_output_newline(Indent),
		io__write_string(")")
	).

goedel_output_goal(all(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		goedel_output_goal(Goal, VarSet, Indent)
	;
		io__write_string("(ALL ["),
		goedel_output_vars(Vars, VarSet),
		io__write_string("] "),
		{ Indent1 is Indent + 1 },
		goedel_output_newline(Indent1),
		goedel_output_goal(Goal, VarSet, Indent1),
		goedel_output_newline(Indent),
		io__write_string(")")
	).

goedel_output_goal(if_then_else(Vars, A, B, C), VarSet, Indent) -->
	io__write_string("(IF"),
	goedel_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	goedel_output_goal(A, VarSet, Indent1),
	goedel_output_newline(Indent),
	io__write_string("THEN"),
	goedel_output_newline(Indent1),
	goedel_output_goal(B, VarSet, Indent1),
	goedel_output_newline(Indent),
	io__write_string("ELSE"),
	goedel_output_newline(Indent1),
	goedel_output_goal(C, VarSet, Indent1),
	goedel_output_newline(Indent),
	io__write_string(")").

goedel_output_goal(if_then(Vars, A, B), VarSet, Indent) -->
	io__write_string("(IF"),
	goedel_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	goedel_output_goal(A, VarSet, Indent1),
	goedel_output_newline(Indent),
	io__write_string("THEN"),
	goedel_output_newline(Indent1),
	goedel_output_goal(B, VarSet, Indent1),
	goedel_output_newline(Indent),
	io__write_string(")").

goedel_output_goal(not(Vars, Goal), VarSet, Indent) -->
	io__write_string("(~"),
	goedel_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	goedel_output_goal(Goal, VarSet, Indent),
	goedel_output_newline(Indent),
	io__write_string(")").

goedel_output_goal((A,B), VarSet, Indent) -->
	goedel_output_goal(A, VarSet, Indent),
	io__write_string(" &"),
	goedel_output_newline(Indent),
	goedel_output_goal(B, VarSet, Indent).

goedel_output_goal((A;B), VarSet, Indent) -->
	io__write_string("("),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	goedel_output_goal(A, VarSet, Indent1),
	goedel_output_disj(B, VarSet, Indent),
	goedel_output_newline(Indent),
	io__write_string(")").

goedel_output_goal(call(Term), VarSet, Indent) -->
	goedel_output_call(Term, VarSet, Indent).

goedel_output_goal(unify(A, B), VarSet, _Indent) -->
	goedel_output_term(A, VarSet),
	io__write_string(" = "),
	goedel_output_term(B, VarSet).

:- pred goedel_output_call(term, varset, int, io__state, io__state).
:- mode goedel_output_call(input, input, input, di, uo).

goedel_output_call(term_variable(Var), VarSet, _Indent) -->
	goedel_output_var(Var, VarSet).
goedel_output_call(term_functor(Functor, Args, Context), VarSet, Indent) -->
	(if %%% some [Cond, Message]
		{ Functor = term_atom("require"),
		  Args = [Cond, Message]
		}
	then
		% output `if Cond then true else error(Msg)'
		{ parse_goal(Cond, CondGoal) },
		goedel_output_goal(if_then_else([], CondGoal, true,
			call(term_functor(term_atom("error"), [Message],
			Context))), VarSet, Indent)
	else
	if %%% some [Arg1, Arg2, PredName]
		{ Args = [Arg1, Arg2],
		  Functor = term_atom("is")
		}
	then
		{ goedel_convert_expression(Arg2, GoedelArg2) },
		goedel_output_term(Arg1, VarSet),
		io__write_string(" Is "),
		goedel_output_term(GoedelArg2, VarSet)
	else
	if %%% some [Arg1, Arg2, PredName]
		{ Args = [Arg1, Arg2],
		  Functor = term_atom(PredName),
		  goedel_infix_pred(PredName)
		}
	then
		goedel_output_term(Arg1, VarSet),
		io__write_string(" "),
		{ convert_functor_name(PredName, PredName1) },
		io__write_string(PredName1),
		io__write_string(" "),
		goedel_output_term(Arg2, VarSet)
	else
		goedel_output_constant(Functor),
		(if %%% some [X,Xs]		% NU-Prolog inconsistency
			{ Args = [X | Xs] }
		then
			io__write_string("("),
			goedel_output_term(X, VarSet),
			goedel_output_term_args(Xs, VarSet),
			io__write_string(")")
		else
			[]
		)
	).

:- pred goedel_output_disj(goal, varset, int, io__state, io__state).
:- mode goedel_output_disj(input, input, input, di, uo).

goedel_output_disj(Goal, VarSet, Indent) -->
	goedel_output_newline(Indent),
	io__write_string("\\/"),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	(if %%% some [A, B]	% NU-Prolog DCG inconsistency
		{ Goal = (A;B) }
	then
		goedel_output_goal(A, VarSet, Indent1),
		goedel_output_disj(B, VarSet, Indent)
	else
		goedel_output_goal(Goal, VarSet, Indent1)
	).

:- pred goedel_output_some(list(var), varset, io__state, io__state).
:- mode goedel_output_some(input, input, di, uo).

goedel_output_some(Vars, VarSet) -->
	(if
		{ Vars = [] }
	then
		[]
	else
		io__write_string(" SOME ["),
		goedel_output_vars(Vars, VarSet),
		io__write_string("]")
	).

:- pred goedel_convert_expression(term, term).
:- mode goedel_convert_expression(input, output).

goedel_convert_expression(Term0, Term) :-
	( Term0 = term_functor(term_atom(F0), [X0, Y0], Context) ->
		goedel_convert_expression(X0, X),
		goedel_convert_expression(Y0, Y),
		( F0 = "//" ->
			F = "div"
		;
			F = F0
		),
		Term = term_functor(term_atom(F), [X, Y], Context)
	; 
		Term = Term0
	).

%-----------------------------------------------------------------------------%

:- pred goedel_output_newline(int, io__state, io__state).
:- mode goedel_output_newline(input, di, uo).

goedel_output_newline(Indent) -->
	io__write_string("\n"),
	goedel_output_tabs(Indent).

:- pred goedel_output_tabs(int, io__state, io__state).
:- mode goedel_output_tabs(input, di, uo).

goedel_output_tabs(Indent) -->
	(if 
		{ Indent = 0 }
	then
		[]
	else
		io__write_string("\t"),
		{ Indent1 is Indent - 1 },
		goedel_output_tabs(Indent1)
	).

%-----------------------------------------------------------------------------%

:- pred goedel_output_list_args(term, varset, io__state, io__state).
:- mode goedel_output_list_args(input, input, di, uo).

goedel_output_list_args(Term, VarSet) -->
	(
	    	{ Term = term_functor(term_atom("."), Args, _),
		  Args = [X, Xs]
	    	}
	->
		io__write_string(", "),
		goedel_output_term(X, VarSet),
		goedel_output_list_args(Xs, VarSet)
	;
		{ Term = term_functor(term_atom("[]"), [], _) }
	->
		[]
	;
		io__write_string(" | "),
		goedel_output_term(Term, VarSet)
	).

	% write a term to standard output.

:- pred goedel_output_term(term, varset, io__state, io__state).
:- mode goedel_output_term(input, input, di, uo).

goedel_output_term(term_variable(Var), VarSet) -->
	goedel_output_var(Var, VarSet).
goedel_output_term(term_functor(Functor, Args, _), VarSet) -->
	(if %%% some [X, Xs]
	    	{ Functor = term_atom("."),
		  Args = [X, Xs]
	    	}
	then
		io__write_string("["),
		goedel_output_term(X, VarSet),
		goedel_output_list_args(Xs, VarSet),
		io__write_string("]")
	else
	if %%% some [PrefixArg]	% NU-Prolog inconsistency
		{ Args = [PrefixArg],
		  Functor = term_atom(FunctorName),
		  goedel_unary_prefix_op(FunctorName)
	    	}
	then
		io__write_string("("),
		goedel_output_constant(Functor),
		io__write_string(" "),
		goedel_output_term(PrefixArg, VarSet),
		io__write_string(")")
	else
	if %%% some [PostfixArg]
		{ Args = [PostfixArg],
		  Functor = term_atom(FunctorName),
		  goedel_unary_postfix_op(FunctorName)
	    	}
	then
		io__write_string("("),
		goedel_output_term(PostfixArg, VarSet),
		io__write_string(" "),
		goedel_output_constant(Functor),
		io__write_string(")")
	else
	if %%% some [Arg1, Arg2]
		{ Args = [Arg1, Arg2],
		  Functor = term_atom(FunctorName),
		  goedel_infix_op(FunctorName)
		}
	then
		io__write_string("("),
		goedel_output_term(Arg1, VarSet),
		io__write_string(" "),
		goedel_output_constant(Functor),
		io__write_string(" "),
		goedel_output_term(Arg2, VarSet),
		io__write_string(")")
	else
		goedel_output_constant(Functor),
		(if %%% some [Y, Ys]		% NU-Prolog inconsistency
			{ Args = [Y | Ys] }
		then
			io__write_string("("),
			goedel_output_term(Y, VarSet),
			goedel_output_term_args(Ys, VarSet),
			io__write_string(")")
		else
			[]
		)
	).

	% output the remaining arguments

:- pred goedel_output_term_args(list(term), varset, io__state, io__state).
:- mode goedel_output_term_args(input, input, di, uo).

goedel_output_term_args([], _VarSet) --> [].
goedel_output_term_args([X | Xs], VarSet) -->
	io__write_string(", "),
	goedel_output_term(X, VarSet),
	goedel_output_term_args(Xs, VarSet).

	% output the functor

:- pred goedel_output_constant(const, io__state, io__state).
:- mode goedel_output_constant(input, di, uo).

goedel_output_constant(term_integer(I)) -->
	io__write_int(I).
goedel_output_constant(term_float(F)) -->
	io__write_float(F).
goedel_output_constant(term_atom(Name)) -->
	{ convert_functor_name(Name, GoedelName) },
	io__write_string(GoedelName).
goedel_output_constant(term_string(S)) -->
	io__write_string("\""),
	goedel_quote_string(S), 
	io__write_string("\"").

:- pred goedel_quote_string(string, io__state, io__state).
:- mode goedel_quote_string(input, di, uo).

goedel_quote_string(S0) -->
	( { string__first_char(S0, Char, S1) } ->
		( { goedel_quote_char(Char, QuoteChar) } ->
			io__write_char('\\'),
			io__write_char(QuoteChar)
		;
			io__write_char(Char)
		),
		goedel_quote_string(S1)
	;
		[]
	).

:- pred goedel_quote_char(character, character).
:- mode goedel_quote_char(input, output).

goedel_quote_char('\"', '"').
goedel_quote_char('\\', '\\').
goedel_quote_char('\n', 'n').
goedel_quote_char('\t', 't').
goedel_quote_char('\b', 'b').

	% output a comma-separated list of variables

:- pred goedel_output_vars(list(var), varset, io__state, io__state).
:- mode goedel_output_vars(input, input, di, uo).

goedel_output_vars([], _VarSet) --> [].
goedel_output_vars([Var | Vars], VarSet) -->
	goedel_output_var(Var, VarSet),
	goedel_output_vars_2(Vars, VarSet).

:- pred goedel_output_vars_2(list(var), varset, io__state, io__state).
:- mode goedel_output_vars_2(input, input, di, uo).

goedel_output_vars_2([], _VarSet) --> [].
goedel_output_vars_2([Var | Vars], VarSet) -->
	io__write_string(", "),
	goedel_output_var(Var, VarSet),
	goedel_output_vars_2(Vars, VarSet).

	% Output a single variable.
	% Variables that didn't have names are given the name "v_<n>"
	% where <n> is there variable id.
	% Variables whose name originally started with `v_' have their
	% name changed to start with `v__' to avoid name clashes.

:- pred goedel_output_var(var, varset, io__state, io__state).
:- mode goedel_output_var(input, input, di, uo).

goedel_output_var(Var, VarSet) -->
	(
		{ varset__lookup_name(VarSet, Var, Name) }
	->
		{ convert_var_name(Name, GoedelName) },
		io__write_string(GoedelName)
	;
		{ term__var_to_int(Var, Id),
		  string__int_to_string(Id, Num),
		  string__append("v_", Num, VarName)
		},
		io__write_string(VarName)
	).

%-----------------------------------------------------------------------------%

	% write a type to standard output.

:- pred goedel_output_type(term, varset, io__state, io__state).
:- mode goedel_output_type(input, input, di, uo).

goedel_output_type(term_variable(Var), VarSet) -->
	goedel_output_var(Var, VarSet).
goedel_output_type(term_functor(Functor, Args, _), VarSet) -->
	goedel_output_constant(Functor),
	(if %%% some [X,Xs]		% NU-Prolog inconsistency
		{ Args = [X | Xs] }
	then
		io__write_string("("),
		goedel_output_type(X, VarSet),
		goedel_output_type_args(Xs, VarSet),
		io__write_string(")")
	else
		[]
	).

	% output the remaining arguments

:- pred goedel_output_type_args(list(type), varset, io__state, io__state).
:- mode goedel_output_type_args(input, input, di, uo).

goedel_output_type_args([], _VarSet) --> [].
goedel_output_type_args([X | Xs], VarSet) -->
	io__write_string(", "),
	goedel_output_type(X, VarSet),
	goedel_output_type_args(Xs, VarSet).

%-----------------------------------------------------------------------------%

	% Predicates to test whether a functor is a Goedel operator
	% (an operator defined in one of the Goedel system modules).

:- pred goedel_infix_op(string).
:- mode goedel_infix_op(input).

goedel_infix_op("+").
goedel_infix_op("-").
goedel_infix_op("*").
goedel_infix_op("/").
goedel_infix_op("^").
goedel_infix_op("++").
goedel_infix_op("\\").
goedel_infix_op("//").
goedel_infix_op("div").
goedel_infix_op("mod").

:- pred goedel_unary_prefix_op(string).
:- mode goedel_unary_prefix_op(input).

goedel_unary_prefix_op("-").

:- pred goedel_unary_postfix_op(string).
:- mode goedel_unary_postfix_op(input).

goedel_unary_postfix_op(_) :- fail.

:- pred goedel_infix_pred(string).
:- mode goedel_infix_pred(input).

goedel_infix_pred("<").
goedel_infix_pred(">").
goedel_infix_pred("=<").
goedel_infix_pred(">=").
goedel_infix_pred("in").
goedel_infix_pred("subset").
goedel_infix_pred("strictSubset").
goedel_infix_pred("is").

%-----------------------------------------------------------------------------%

	% Convert a Mercury functor name into a Goedel functor name.

	% XXX handle non-alphanumeric functors

:- pred convert_functor_name(string, string).
:- mode convert_functor_name(input, output).

convert_functor_name(Name, GoedelName) :-
	(
		string__first_char(Name, Char, Rest),
		is_lower(Char),
		valid_functor_tail(Rest)
	->
		string__capitalize_first(Name, GoedelName0),
		(
			string__append("F_", Suffix, GoedelName0)
		->
			string__append("F__", Suffix, GoedelName)
		;
			GoedelName = GoedelName0
		)
	;
		convert_to_valid_functor_name(Name, GoedelName)
	).

:- pred valid_functor_tail(string).
:- mode valid_functor_tail(input).

valid_functor_tail(String) :-
	( string__first_char(String, Char, Rest) ->
		some [] (
			is_alpha(Char) ;
			is_digit(Char) ;
			Char = '_'
		),
		valid_functor_tail(Rest)
	;
		true
	).

:- pred convert_to_valid_functor_name(string, string).
:- mode convert_to_valid_functor_name(input, output).

convert_to_valid_functor_name(String, Name) :-	
	(
		string__first_char(String, Char, ""),
		is_upper(Char)
	->
		string__append("F_", String, Name)
	;
		conversion_table(String, Name0)
	->
		Name = Name0
	;
		convert_to_valid_functor_name_2(String, Name0),
		string__append("F", Name0, Name)
	).

	% A table used to convert Mercury functors into
	% Goedel functors.  Feel free to add any new translations you want.
	% The Goedel functor names should start with "F_" if
	% they are alphanumeric, to avoid introducing name clashes.
	% If the functor name is not found in the table, then
	% we use a fall-back method which produces ugly names.

:- pred conversion_table(string, string).
:- mode conversion_table(input, output).

conversion_table("[]", "[]").
conversion_table("\=", "~=").
conversion_table(">=", ">=").
conversion_table("=<", "=<").
conversion_table("=", "=").
conversion_table("<", "<").
conversion_table(">", ">").
conversion_table("-", "-").
conversion_table("+", "+").
conversion_table("*", "*").
conversion_table("/", "/").
conversion_table(",", "F_Comma").
conversion_table(";", "F_Semicolon").
conversion_table("0", "F_Digit_0").
conversion_table("1", "F_Digit_1").
conversion_table("2", "F_Digit_2").
conversion_table("3", "F_Digit_3").
conversion_table("4", "F_Digit_4").
conversion_table("5", "F_Digit_5").
conversion_table("6", "F_Digit_6").
conversion_table("7", "F_Digit_7").
conversion_table("8", "F_Digit_8").
conversion_table("9", "F_Digit_9").

	% This is the fall-back method.
	% Given a string, produce the tail of a functor name
	% for that string by concatenating the decimal
	% expansions of the character codes in the string,
	% separated by underlines.
	% The functor name will start with "F_"; this predicate
	% constructs everything except the initial "F".
	%
	% For example, given the input "\n\t" we return "_10_8".

:- pred convert_to_valid_functor_name_2(string, string).
:- mode convert_to_valid_functor_name_2(input, output).

convert_to_valid_functor_name_2(String, Name) :-	
	(
		string__first_char(String, Char, Rest)
	->
		char_to_int(Char, Code),
		string__int_to_string(Code, CodeString),
		string__append("_", CodeString, ThisCharString),
		convert_to_valid_functor_name_2(Rest, Name0),
		string__append(ThisCharString, Name0, Name)
	;
		% String is the empty string
		Name = String
	).

%-----------------------------------------------------------------------------%

	% Convert a Mercury variable name into a Goedel variable name.
	% We can't use Goedel variable names starting with an underscore,
	% because these have special semantics.
	% We have to be careful that every possible Mercury name
	% (including variables which only have numbers, not names!)
	% is mapped to a distinct Goedel name.  The following table
	% shows how this is done:
	%
	%	Goedel name	Mercury Name
	%	-----------	------------
	%	v_[0-9]*	none
	%	v__.*		_.*
	%	v_V_.*		V_.*
	%	v[^_].*		V[^_].*
	%	[^v].*		[^V_].*
	%
	% If the Mercury variable name starts with an underline, or with
	% "V_", then we insert "v_" at the start; otherwise we just change
	% the first letter to lower-case.  Goedel names starting with "v_" and
	% a sequence of digits are reserved for variables which didn't
	% have any Mercury name (eg. implicit DCG arguments).

:- pred convert_var_name(string, string).
:- mode convert_var_name(input, output).

convert_var_name(Name, GoedelName) :-
	( string__prefix(Name, "_") ->
		string__append("v_", Name, GoedelName)
	; string__prefix(Name, "V_") ->
		string__append("v_", Name, GoedelName)
	;
		string__uncapitalize_first(Name, GoedelName)
	).

%-----------------------------------------------------------------------------%
