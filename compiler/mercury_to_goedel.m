%----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mercury_to_goedel.m.
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
%	handle (possibly overloaded) predicates with module qualifiers;
%
% (Wish list)
%	indent `ELSE IF' properly
% 	translate mode declarations into delay declarations;
%	translate Mercury's module system declarations into Goedel;
%	preserve the comments in the original source code.

%-----------------------------------------------------------------------------%

:- module mercury_to_goedel.
:- interface.

:- import_module list, io.
:- import_module prog_data.

:- pred convert_to_goedel(string, list(item_and_context), io__state, io__state).
:- mode convert_to_goedel(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, int, char, std_util, varset, term, require, string.
:- import_module prog_io, prog_out, prog_util, equiv_type.
:- import_module globals, options.
%-----------------------------------------------------------------------------%

	% The following is a hard-coded hack.
	% Mercury allows functor overloading, whereas Goedel
	% only allows it if the functors are defined in different
	% modules.  As a work-around, we can define types for
	% which this is a problem in their own module.
	% Currently we only do this for type "character".
	% XXX This should be a command-line option.
	% (Or we should work out a better solution.)

:- pred option_handle_functor_overloading(string).
:- mode option_handle_functor_overloading(in) is semidet.

option_handle_functor_overloading("character").

%-----------------------------------------------------------------------------%

convert_to_goedel(ProgName, Items0) -->
	io__stderr_stream(StdErr),
	io__write_string(StdErr, "% Expanding equivalence types..."),
	io__flush_output(StdErr),
	{ goedel_replace_int_integer(IntEquivTypeDefn) },
	equiv_type__expand_eqv_types([IntEquivTypeDefn | Items0],
					Items, Error,  _),
	io__write_string(StdErr, " done\n"),
	( { Error = no } ->
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
			goedel_output_item_list(Items),
			io__write_string(StdErr, "% done\n"),
			io__told
		;
			io__write_string(StdErr, "Error: couldn't open file `"),
			io__write_string(StdErr, OutputFileName),
			io__write_string(StdErr, "' for output.\n")
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% Mercury uses "int" as the integer type, whereas Goedel uses
	% "Integer", so we need to replace all occurrences of int with integer.
	% We do this by inserting a type declaration
	%	:- type int == integer.
	% at the start of the item list.

:- pred goedel_replace_int_integer(item_and_context).
:- mode goedel_replace_int_integer(out) is det.

goedel_replace_int_integer(Item - Context) :-
	varset__init(VarSet),
	term__context_init(Context),
	Item = type_defn(VarSet, TypeDefn, true),
	TypeDefn = eqv_type(unqualified("int"), [],
		term__functor(term__atom("integer"), [], Context)).

%-----------------------------------------------------------------------------%

	% add the declarations one by one to the module

:- pred goedel_output_item_list(list(item_and_context), io__state, io__state).
:- mode goedel_output_item_list(in, di, uo) is det.

goedel_output_item_list([]) --> [].
goedel_output_item_list([Item - Context | Items]) -->
	goedel_output_item(Item, Context),
	goedel_output_item_list(Items).

%-----------------------------------------------------------------------------%

:- pred goedel_output_item(item, term__context, io__state, io__state).
:- mode goedel_output_item(in, in, di, uo) is det.

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
	io__write_string("\n"),
	maybe_write_line_number(Context),
	goedel_output_pred(VarSet, PredName, TypesAndModes, Context).

goedel_output_item(func(VarSet, PredName, TypesAndModes, RetTypeAndMode, _Det,
		_Cond), Context) -->
	io__write_string("\n"),
	maybe_write_line_number(Context),
	goedel_output_func(VarSet, PredName, TypesAndModes, RetTypeAndMode,
		Context).

goedel_output_item(pred_mode(VarSet, PredName, Modes, _Det, _Cond), Context) -->
	goedel_output_pred_mode(VarSet, PredName, Modes, Context).
goedel_output_item(func_mode(VarSet, PredName, ArgModes, RetMode, _Det, _Cond),
		Context) -->
	goedel_output_func_mode(VarSet, PredName, ArgModes, RetMode, Context).

goedel_output_item(module_defn(_VarSet, _ModuleDefn), _Context) -->
	% io__write_string("warning: module declarations not yet supported.\n").
	[].

goedel_output_item(pred_clause(VarSet, PredName, Args, Body), Context) -->
	maybe_write_line_number(Context),
	goedel_output_pred_clause(VarSet, PredName, Args, Body, Context).

goedel_output_item(func_clause(VarSet, PredName, Args, Result, Body), Context)
		-->
	maybe_write_line_number(Context),
	goedel_output_func_clause(VarSet, PredName, Args, Result, Body,
		Context).

% Give a warning but ignore pragma declarations.
goedel_output_item(pragma(_Pragma), _Context) -->
	io__stderr_stream(Stderr),
	io__write_string(Stderr, 
			"warning: C header declarations not allowed. Ignoring\n").

goedel_output_item(nothing, _) --> [].

%-----------------------------------------------------------------------------%

:- pred goedel_output_inst_defn(varset, inst_defn, term__context,
			io__state, io__state).
:- mode goedel_output_inst_defn(in, in, in, di, uo) is det.

goedel_output_inst_defn(_VarSet, _InstDefn, _Context) -->
	% io__write_string("% inst definitions not supported\n"),
	[].

:- pred goedel_output_mode_defn(varset, mode_defn, term__context,
			io__state, io__state).
:- mode goedel_output_mode_defn(in, in, in, di, uo) is det.

goedel_output_mode_defn(_VarSet, _ModeDefn, _Context) -->
	% io__write_string("% mode definitions not supported\n"),
	[].

%-----------------------------------------------------------------------------%

:- pred goedel_output_type_defn(varset, type_defn, term__context,
			io__state, io__state).
:- mode goedel_output_type_defn(in, in, in, di, uo) is det.

goedel_output_type_defn(VarSet, TypeDefn, Context) -->
	goedel_output_type_defn_2(TypeDefn, VarSet, Context).

:- pred goedel_output_type_defn_2(type_defn, varset, term__context,
			io__state, io__state).
:- mode goedel_output_type_defn_2(in, in, in, di, uo) is det.

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
:- mode goedel_output_type_defn_3(in, in, in, in, in, in, di, uo) is det.

goedel_output_type_defn_3(Name2, Name3, Args, Ctors, VarSet, Context) -->
	maybe_write_line_number(Context),
	{ list__length(Args, Arity) },
	(
		{ Arity = 0 }
	->
		io__write_string("BASE         "),
		io__write_string(Name3)
	;
		io__write_string("CONSTRUCTOR  "),
		io__write_string(Name3),
		io__write_string("/"),
		io__write_int(Arity)
	),
	io__write_string(".\n"),
	goedel_output_ctors(Ctors,
		term__functor(term__atom(Name2), Args, Context), VarSet).

:- pred goedel_output_ctors(list(constructor), type, varset,
				io__state, io__state).
:- mode goedel_output_ctors(in, in, in, di, uo) is det.

goedel_output_ctors([], _, _) --> [].
goedel_output_ctors([Name - Args | Ctors], Type, VarSet) -->
	{ unqualify_name(Name, Name2),
	  convert_functor_name(Name2, Name3) },
	(
		{ Args = [_ArgName - ArgType | Rest] }
	->
		io__write_string("FUNCTION     "),
		io__write_string(Name3),
		{ list__length(Args, Arity) },
		(
			{ Arity = 2, goedel_infix_op(Name2) }
		->
			io__write_string(" : xFx(100)")
		;
			{ Arity = 1, goedel_unary_prefix_op(Name2) }
		->
			io__write_string(" : Fx(100)")
		;
			{ Arity = 1, goedel_unary_postfix_op(Name2) }
		->
			io__write_string(" : xF(100)")
		;
			[]
		),
		io__write_string(" : "),
		goedel_output_term(ArgType, VarSet),
		goedel_output_remaining_ctor_args(Rest, VarSet),
		io__write_string(" -> ")
	;
		io__write_string("CONSTANT     "),
		io__write_string(Name3),
		io__write_string(" : ")
	),
	goedel_output_term(Type, VarSet),
	io__write_string(".\n"),
	goedel_output_ctors(Ctors, Type, VarSet).

:- pred goedel_output_remaining_ctor_args(list(constructor_arg), varset,
		io__state, io__state).
:- mode goedel_output_remaining_ctor_args(in, in, di, uo) is det.

goedel_output_remaining_ctor_args([], _VarSet) --> [].
goedel_output_remaining_ctor_args([_Name - Type | Args], VarSet) -->
	io__write_string(" * "),
	goedel_output_type(Type, VarSet),
	goedel_output_remaining_ctor_args(Args, VarSet).

%-----------------------------------------------------------------------------%

:- pred goedel_output_pred(varset, sym_name, list(type_and_mode),
		term__context, io__state, io__state).
:- mode goedel_output_pred(in, in, in, in, di, uo) is det.

goedel_output_pred(VarSet, PredName, TypesAndModes, Context) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	goedel_output_pred_type(VarSet, PredName, Types, Context),
	(
		{ MaybeModes = yes(Modes) }
	->
		goedel_output_pred_mode(VarSet, PredName, Modes, Context)
	;
		[]
	).

:- pred goedel_output_pred_type(varset, sym_name, list(type),
		term__context, io__state, io__state).
:- mode goedel_output_pred_type(in, in, in, in, di, uo) is det.

goedel_output_pred_type(VarSet, PredName, Types, _Context) -->
	{ unqualify_name(PredName, PredName2),
	  convert_functor_name(PredName2, PredName3) },
	(
		{ Types = [Type | Rest] }
	->
		io__write_string("PREDICATE    "),
		io__write_string(PredName3),
		{ list__length(Types, Arity) },
		(
			{ Arity = 2, goedel_infix_pred(PredName2) }
		->
			io__write_string(" : zPz")
		;
			[]
		),
		io__write_string(" : "),
		goedel_output_type(Type, VarSet),
		goedel_output_remaining_types(Rest, VarSet)
	;
		io__write_string("PROPOSITION  "),
		io__write_string(PredName3)
	),
	io__write_string(".\n").

:- pred goedel_output_remaining_types(list(type), varset, io__state, io__state).
:- mode goedel_output_remaining_types(in, in, di, uo) is det.

goedel_output_remaining_types([], _VarSet) --> [].
goedel_output_remaining_types([Type | Types], VarSet) -->
	io__write_string(" * "),
	goedel_output_type(Type, VarSet),
	goedel_output_remaining_types(Types, VarSet).

%-----------------------------------------------------------------------------%

:- pred goedel_output_func(varset, sym_name, list(type_and_mode), type_and_mode,
		term__context, io__state, io__state).
:- mode goedel_output_func(in, in, in, in, in, di, uo) is det.

goedel_output_func(VarSet, PredName, TypesAndModes, RetTypeAndMode, Context) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	{ split_type_and_mode(RetTypeAndMode, RetType, MaybeRetMode) },
	goedel_output_func_type(VarSet, PredName, Types, RetType, Context),
	(
		{ MaybeModes = yes(Modes) },
		{ MaybeRetMode = yes(RetMode) }
	->
		goedel_output_func_mode(VarSet, PredName, Modes, RetMode,
			Context)
	;
		[]
	).

:- pred goedel_output_func_type(varset, sym_name, list(type), type,
		term__context, io__state, io__state).
:- mode goedel_output_func_type(in, in, in, in, in, di, uo) is det.

goedel_output_func_type(VarSet, FuncName, Types, RetType, _Context) -->
	{ list__map(lambda([Type::in, Arg::out] is det, (Arg = "" - Type)),
		Types, Args) },
	goedel_output_ctors([FuncName - Args], RetType, VarSet).

%-----------------------------------------------------------------------------%

	% Output a mode declaration for a predicate.

:- pred goedel_output_pred_mode(varset, sym_name, list(mode), term__context,
			io__state, io__state).
:- mode goedel_output_pred_mode(in, in, in, in, di, uo) is det.

goedel_output_pred_mode(_VarSet, _PredName, _Modes, _Context) -->
	% io__write_string("% warning: mode declarations not supported.\n"),
	[].

%-----------------------------------------------------------------------------%

	% Output a mode declaration for a function.

:- pred goedel_output_func_mode(varset, sym_name, list(mode), mode,
			term__context, io__state, io__state).
:- mode goedel_output_func_mode(in, in, in, in, in, di, uo) is det.

goedel_output_func_mode(_VarSet, _PredName, _Modes, _RetMode, _Context) -->
	% io__write_string("% warning: mode declarations not supported.\n"),
	[].

%-----------------------------------------------------------------------------%

	% Output a clause.

:- pred goedel_output_pred_clause(varset, sym_name, list(term), goal,
			term__context, io__state, io__state).
:- mode goedel_output_pred_clause(in, in, in, in, in, di, uo) is det.

goedel_output_pred_clause(VarSet, PredName, Args, Body, Context) -->
	{ unqualify_name(PredName, PredName2) },
	goedel_output_term(term__functor(term__atom(PredName2), Args, Context),
			VarSet),
	(
		{ Body = true - Context }
	->
		[]
	;
		io__write_string(" <-\n\t"),
		goedel_output_goal(Body, VarSet, 1)
	),
	io__write_string(".\n").

	% Output an equation.

:- pred goedel_output_func_clause(varset, sym_name, list(term), term, goal,
			term__context, io__state, io__state).
:- mode goedel_output_func_clause(in, in, in, in, in, in, di, uo) is det.

goedel_output_func_clause(VarSet, PredName, Args, Result, Body, Context) -->
	{ unqualify_name(PredName, PredName2) },
	goedel_output_term(
		term__functor(term__atom("="), [
			term__functor(term__atom(PredName2), Args, Context),
			Result], Context),
		VarSet),
	(
		{ Body = true - Context }
	->
		[]
	;
		io__write_string(" <-\n\t"),
		goedel_output_goal(Body, VarSet, 1)
	),
	io__write_string(".\n").

:- pred goedel_output_goal(goal, varset, int, io__state, io__state).
:- mode goedel_output_goal(in, in, in, di, uo) is det.

goedel_output_goal(Goal - _Context, VarSet, Indent) -->
	goedel_output_goal_2(Goal, VarSet, Indent).

:- pred goedel_output_goal_2(goal_expr, varset, int, io__state, io__state).
:- mode goedel_output_goal_2(in, in, in, di, uo) is det.

goedel_output_goal_2(fail, _, _) -->
	io__write_string("False").

goedel_output_goal_2(true, _, _) -->
	io__write_string("True").

	% Implication and equivalence should have been transformed out
	% by now
goedel_output_goal_2(implies(_G1,_G2), _VarSet, _Indent) -->
	{ error("mercury_to_goedel: implies/2 in goedel_output_goal")}.

goedel_output_goal_2(equivalent(_G1,_G2), _VarSet, _Indent) -->
	{ error("mercury_to_goedel: equivalent/2 in goedel_output_goal")}.

goedel_output_goal_2(some(Vars, Goal), VarSet, Indent) -->
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

goedel_output_goal_2(all(Vars, Goal), VarSet, Indent) -->
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

goedel_output_goal_2(if_then_else(Vars, A, B, C), VarSet, Indent) -->
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

goedel_output_goal_2(if_then(Vars, A, B), VarSet, Indent) -->
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

goedel_output_goal_2(not(Goal), VarSet, Indent) -->
	io__write_string("(~"),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	goedel_output_goal(Goal, VarSet, Indent),
	goedel_output_newline(Indent),
	io__write_string(")").

goedel_output_goal_2((A,B), VarSet, Indent) -->
	goedel_output_goal(A, VarSet, Indent),
	io__write_string(" &"),
	goedel_output_newline(Indent),
	goedel_output_goal(B, VarSet, Indent).

goedel_output_goal_2((A;B), VarSet, Indent) -->
	io__write_string("("),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	goedel_output_goal(A, VarSet, Indent1),
	goedel_output_disj(B, VarSet, Indent),
	goedel_output_newline(Indent),
	io__write_string(")").

% XXX should preserve some of the qualification information?
goedel_output_goal_2(call(Name, Term), VarSet, Indent) -->
	{ unqualify_name(Name, Name0) },
	{ term__context_init(Context0) },
	goedel_output_call(term__functor(term__atom(Name0), Term, Context0), VarSet, Indent).

goedel_output_goal_2(unify(A, B), VarSet, _Indent) -->
	goedel_output_term(A, VarSet),
	io__write_string(" = "),
	goedel_output_term(B, VarSet).


:- pred goedel_output_call(term, varset, int, io__state, io__state).
:- mode goedel_output_call(in, in, in, di, uo) is det.

goedel_output_call(term__variable(Var), VarSet, _Indent) -->
	goedel_output_var(Var, VarSet).

goedel_output_call(term__functor(Functor, Args, _Context), VarSet, _Indent) -->
	(
		{ Args = [Arg1, Arg2],
		  Functor = term__atom("is")
		}
	->
		{ goedel_convert_expression(Arg2, GoedelArg2) },
		goedel_output_term(Arg1, VarSet),
		io__write_string(" Is "),
		goedel_output_term(GoedelArg2, VarSet)
	;
		{ Args = [Arg1, Arg2],
		  Functor = term__atom(PredName),
		  goedel_infix_pred(PredName)
		}
	->
		goedel_output_term(Arg1, VarSet),
		io__write_string(" "),
		{ convert_functor_name(PredName, PredName1) },
		io__write_string(PredName1),
		io__write_string(" "),
		goedel_output_term(Arg2, VarSet)
	;
		goedel_output_constant(Functor),
		(
			{ Args = [X | Xs] }
		->
			io__write_string("("),
			goedel_output_term(X, VarSet),
			goedel_output_term_args(Xs, VarSet),
			io__write_string(")")
		;
			[]
		)
	).

:- pred goedel_output_disj(goal, varset, int, io__state, io__state).
:- mode goedel_output_disj(in, in, in, di, uo) is det.

goedel_output_disj(Goal, VarSet, Indent) -->
	goedel_output_newline(Indent),
	io__write_string("\\/"),
	{ Indent1 is Indent + 1 },
	goedel_output_newline(Indent1),
	(
		{ Goal = (A;B) - _Context }
	->
		goedel_output_goal(A, VarSet, Indent1),
		goedel_output_disj(B, VarSet, Indent)
	;
		goedel_output_goal(Goal, VarSet, Indent1)
	).

:- pred goedel_output_some(list(var), varset, io__state, io__state).
:- mode goedel_output_some(in, in, di, uo) is det.

goedel_output_some(Vars, VarSet) -->
	(
		{ Vars = [] }
	->
		[]
	;
		io__write_string(" SOME ["),
		goedel_output_vars(Vars, VarSet),
		io__write_string("]")
	).

:- pred goedel_convert_expression(term, term).
:- mode goedel_convert_expression(in, out) is det.

goedel_convert_expression(Term0, Term) :-
	( Term0 = term__functor(term__atom(F0), [X0, Y0], Context) ->
		goedel_convert_expression(X0, X),
		goedel_convert_expression(Y0, Y),
		( F0 = "//" ->
			F = "div"
		;
			F = F0
		),
		Term = term__functor(term__atom(F), [X, Y], Context)
	; 
		Term = Term0
	).

%-----------------------------------------------------------------------------%

:- pred goedel_output_newline(int, io__state, io__state).
:- mode goedel_output_newline(in, di, uo) is det.

goedel_output_newline(Indent) -->
	io__write_string("\n"),
	goedel_output_tabs(Indent).

:- pred goedel_output_tabs(int, io__state, io__state).
:- mode goedel_output_tabs(in, di, uo) is det.

goedel_output_tabs(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_string("\t"),
		{ Indent1 is Indent - 1 },
		goedel_output_tabs(Indent1)
	).

%-----------------------------------------------------------------------------%

:- pred goedel_output_list_args(term, varset, io__state, io__state).
:- mode goedel_output_list_args(in, in, di, uo) is det.

goedel_output_list_args(Term, VarSet) -->
	(
	    	{ Term = term__functor(term__atom("."), Args, _),
		  Args = [X, Xs]
	    	}
	->
		io__write_string(", "),
		goedel_output_term(X, VarSet),
		goedel_output_list_args(Xs, VarSet)
	;
		{ Term = term__functor(term__atom("[]"), [], _) }
	->
		[]
	;
		io__write_string(" | "),
		goedel_output_term(Term, VarSet)
	).

	% write a term to standard output.

:- pred goedel_output_term(term, varset, io__state, io__state).
:- mode goedel_output_term(in, in, di, uo) is det.

goedel_output_term(term__variable(Var), VarSet) -->
	goedel_output_var(Var, VarSet).
goedel_output_term(term__functor(Functor, Args, _), VarSet) -->
	(
	    	{ Functor = term__atom("."),
		  Args = [X, Xs]
	    	}
	->
		io__write_string("["),
		goedel_output_term(X, VarSet),
		goedel_output_list_args(Xs, VarSet),
		io__write_string("]")
	;
		{ Args = [PrefixArg],
		  Functor = term__atom(FunctorName),
		  goedel_unary_prefix_op(FunctorName)
	    	}
	->
		io__write_string("("),
		goedel_output_constant(Functor),
		io__write_string(" "),
		goedel_output_term(PrefixArg, VarSet),
		io__write_string(")")
	;
		{ Args = [PostfixArg],
		  Functor = term__atom(FunctorName),
		  goedel_unary_postfix_op(FunctorName)
	    	}
	->
		io__write_string("("),
		goedel_output_term(PostfixArg, VarSet),
		io__write_string(" "),
		goedel_output_constant(Functor),
		io__write_string(")")
	;
		{ Args = [Arg1, Arg2],
		  Functor = term__atom(FunctorName),
		  goedel_infix_op(FunctorName)
		}
	->
		io__write_string("("),
		goedel_output_term(Arg1, VarSet),
		io__write_string(" "),
		goedel_output_constant(Functor),
		io__write_string(" "),
		goedel_output_term(Arg2, VarSet),
		io__write_string(")")
	;
		goedel_output_constant(Functor),
		(
			{ Args = [Y | Ys] }
		->
			io__write_string("("),
			goedel_output_term(Y, VarSet),
			goedel_output_term_args(Ys, VarSet),
			io__write_string(")")
		;
			[]
		)
	).

	% output the remaining arguments

:- pred goedel_output_term_args(list(term), varset, io__state, io__state).
:- mode goedel_output_term_args(in, in, di, uo) is det.

goedel_output_term_args([], _VarSet) --> [].
goedel_output_term_args([X | Xs], VarSet) -->
	io__write_string(", "),
	goedel_output_term(X, VarSet),
	goedel_output_term_args(Xs, VarSet).

	% output the functor

:- pred goedel_output_constant(const, io__state, io__state).
:- mode goedel_output_constant(in, di, uo) is det.

goedel_output_constant(term__integer(I)) -->
	io__write_int(I).
goedel_output_constant(term__float(F)) -->
	io__write_float(F).
goedel_output_constant(term__atom(Name)) -->
	{ convert_functor_name(Name, GoedelName) },
	io__write_string(GoedelName).
goedel_output_constant(term__string(S)) -->
	io__write_string(""""),
	goedel_quote_string(S), 
	io__write_string("""").

:- pred goedel_quote_string(string, io__state, io__state).
:- mode goedel_quote_string(in, di, uo) is det.

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

:- pred goedel_quote_char(char, char).
:- mode goedel_quote_char(in, out) is semidet.

goedel_quote_char('"', '"').
goedel_quote_char('\\', '\\').
goedel_quote_char('\n', 'n').
goedel_quote_char('\t', 't').
goedel_quote_char('\b', 'b').

	% output a comma-separated list of variables

:- pred goedel_output_vars(list(var), varset, io__state, io__state).
:- mode goedel_output_vars(in, in, di, uo) is det.

goedel_output_vars([], _VarSet) --> [].
goedel_output_vars([Var | Vars], VarSet) -->
	goedel_output_var(Var, VarSet),
	goedel_output_vars_2(Vars, VarSet).

:- pred goedel_output_vars_2(list(var), varset, io__state, io__state).
:- mode goedel_output_vars_2(in, in, di, uo) is det.

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
:- mode goedel_output_var(in, in, di, uo) is det.

goedel_output_var(Var, VarSet) -->
	(
		{ varset__search_name(VarSet, Var, Name) }
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
:- mode goedel_output_type(in, in, di, uo) is det.

goedel_output_type(term__variable(Var), VarSet) -->
	goedel_output_var(Var, VarSet).
goedel_output_type(term__functor(Functor, Args, _), VarSet) -->
	goedel_output_constant(Functor),
	(
		{ Args = [X | Xs] }
	->
		io__write_string("("),
		goedel_output_type(X, VarSet),
		goedel_output_type_args(Xs, VarSet),
		io__write_string(")")
	;
		[]
	).

	% output the remaining arguments

:- pred goedel_output_type_args(list(type), varset, io__state, io__state).
:- mode goedel_output_type_args(in, in, di, uo) is det.

goedel_output_type_args([], _VarSet) --> [].
goedel_output_type_args([X | Xs], VarSet) -->
	io__write_string(", "),
	goedel_output_type(X, VarSet),
	goedel_output_type_args(Xs, VarSet).

%-----------------------------------------------------------------------------%

	% Predicates to test whether a functor is a Goedel operator
	% (an operator defined in one of the Goedel system modules).

:- pred goedel_infix_op(string).
:- mode goedel_infix_op(in) is semidet.

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
:- mode goedel_unary_prefix_op(in) is semidet.

goedel_unary_prefix_op("-").

:- pred goedel_unary_postfix_op(string).
:- mode goedel_unary_postfix_op(in) is semidet.

goedel_unary_postfix_op(_) :- semidet_fail.

:- pred goedel_infix_pred(string).
:- mode goedel_infix_pred(in) is semidet.

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

:- pred convert_functor_name(string, string).
:- mode convert_functor_name(in, out) is det.

convert_functor_name(Name, GoedelName) :-
	(
		string__first_char(Name, Char, Rest),
		char__is_lower(Char),
		string__is_alnum_or_underscore(Rest)
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

:- pred convert_to_valid_functor_name(string, string).
:- mode convert_to_valid_functor_name(in, out) is det.

convert_to_valid_functor_name(String, Name) :-	
	(
		string__first_char(String, Char, ""),
		char__is_upper(Char)
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
:- mode conversion_table(in, out) is semidet.

conversion_table("[]", "[]").
conversion_table("\\=", "~=").
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
:- mode convert_to_valid_functor_name_2(in, out) is det.

convert_to_valid_functor_name_2(String, Name) :-	
	(
		string__first_char(String, Char, Rest)
	->
		char__to_int(Char, Code),
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
:- mode convert_var_name(in, out) is det.

convert_var_name(Name, GoedelName) :-
	( string__prefix(Name, "_") ->
		string__append("v_", Name, GoedelName)
	; string__prefix(Name, "V_") ->
		string__append("v_", Name, GoedelName)
	;
		string__uncapitalize_first(Name, GoedelName)
	).

%-----------------------------------------------------------------------------%

:- pred maybe_write_line_number(term__context, io__state, io__state).
:- mode maybe_write_line_number(in, di, uo) is det.

maybe_write_line_number(Context) -->
	globals__io_lookup_bool_option(line_numbers, LineNumbers),
	( { LineNumbers = yes } ->
		io__write_string("\t% "),
		prog_out__write_context(Context),
		io__write_string("\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%
