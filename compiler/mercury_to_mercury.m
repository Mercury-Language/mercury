%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: fjh.

% This program converts the parse tree structure provided by prog_io
% back into Mercury source text.

%-----------------------------------------------------------------------------%

:- module mercury_to_mercury.
:- interface.

:- import_module hlds_goal, hlds_data, hlds_pred, prog_data, (inst).
:- import_module list, io, varset, term.

:- type expand_inst_alias --->
		dont_expand		% alias(IK)
	;	expand_silently		% Expansion
	;	expand_noisily.		% alias(IK, Expansion)

%	convert_to_mercury(ProgName, OutputFileName, Items)
:- pred convert_to_mercury(string, string, list(item_and_context),
				io__state, io__state).
:- mode convert_to_mercury(in, in, in, di, uo) is det.

:- pred mercury_output_pred_type(varset, sym_name, list(type),
		maybe(determinism), term__context, io__state, io__state).
:- mode mercury_output_pred_type(in, in, in, in, in, di, uo) is det.

:- pred mercury_output_func_type(varset, sym_name, list(type), type,
		maybe(determinism), term__context, io__state, io__state).
:- mode mercury_output_func_type(in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_pred_mode_decl(varset, sym_name, list(mode),
		maybe(determinism), term__context, inst_table,
		io__state, io__state).
:- mode mercury_output_pred_mode_decl(in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_func_mode_decl(varset, sym_name, list(mode), mode,
		maybe(determinism), term__context, inst_table,
		io__state, io__state).
:- mode mercury_output_func_mode_decl(in, in, in, in, in, in, in,
		di, uo) is det.

:- pred mercury_output_mode_subdecl(pred_or_func, varset, sym_name,
		list(mode), maybe(determinism), term__context,
		inst_table, io__state, io__state).
:- mode mercury_output_mode_subdecl(in, in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_pred_mode_subdecl(varset, sym_name, list(mode),
		maybe(determinism), term__context, inst_table,
		io__state, io__state).
:- mode mercury_output_pred_mode_subdecl(in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_func_mode_subdecl(varset, sym_name, list(mode), mode,
		maybe(determinism), term__context, inst_table,
		io__state, io__state).
:- mode mercury_output_func_mode_subdecl(in, in, in, in, in, in, in,
		di, uo) is det.

:- pred mercury_output_pragma_decl(sym_name, int, pred_or_func, string,
		io__state, io__state).
:- mode mercury_output_pragma_decl(in, in, in, in, di, uo) is det.

:- pred mercury_output_pragma_c_code(may_call_mercury, sym_name, pred_or_func,
		list(pragma_var), maybe(pair(list(string))),
		varset, string, inst_table, io__state, io__state).
:- mode mercury_output_pragma_c_code(in, in, in, in, in, in, in, in,
		di, uo) is det.

:- pred mercury_output_pragma_unused_args(pred_or_func, sym_name,
		int, proc_id, list(int), io__state, io__state) is det.
:- mode mercury_output_pragma_unused_args(in, in, in, in, in, di, uo) is det.

	% Output the given c_header_code declaration
:- pred mercury_output_pragma_c_header(string, io__state, io__state).
:- mode mercury_output_pragma_c_header(in, di, uo) is det.

:- pred mercury_output_type_defn(varset, type_defn, term__context,
			io__state, io__state).
:- mode mercury_output_type_defn(in, in, in, di, uo) is det.

:- pred mercury_output_ctor_arg(varset, constructor_arg, io__state, io__state).
:- mode mercury_output_ctor_arg(in, in, di, uo) is det.

:- pred mercury_output_remaining_ctor_args(varset, list(constructor_arg),
				io__state, io__state).
:- mode mercury_output_remaining_ctor_args(in, in, di, uo) is det.

:- pred mercury_output_inst_defn(varset, inst_defn, term__context,
			inst_table, io__state, io__state).
:- mode mercury_output_inst_defn(in, in, in, in, di, uo) is det.

:- pred mercury_output_mode_defn(varset, mode_defn, term__context,
			inst_table, io__state, io__state).
:- mode mercury_output_mode_defn(in, in, in, in, di, uo) is det.

:- pred mercury_output_structured_inst_list(expand_inst_alias, list(inst), int,
		varset, inst_table, io__state, io__state).
:- mode mercury_output_structured_inst_list(in, in, in, in, in, di, uo) is det.

:- pred mercury_output_inst_list(expand_inst_alias, list(inst), varset,
		inst_table, io__state, io__state).
:- mode mercury_output_inst_list(in, in, in, in, di, uo) is det.

:- pred mercury_output_structured_inst(expand_inst_alias, inst, int, varset,
		inst_table, io__state, io__state).
:- mode mercury_output_structured_inst(in, in, in, in, in, di, uo) is det.

:- pred mercury_output_inst(expand_inst_alias, inst, varset, inst_table,
		io__state, io__state).
:- mode mercury_output_inst(in, in, in, in, di, uo) is det.

:- pred mercury_output_mode(mode, varset, inst_table,
		io__state, io__state).
:- mode mercury_output_mode(in, in, in, di, uo) is det.

:- pred mercury_output_cons_id(cons_id, bool, io__state, io__state).
:- mode mercury_output_cons_id(in, in, di, uo) is det.

:- pred mercury_output_mode_list(list(mode), varset, inst_table,
		io__state, io__state).
:- mode mercury_output_mode_list(in, in, in, di, uo) is det.

:- pred mercury_output_uni_mode(uni_mode, varset, inst_table,
		io__state, io__state).
:- mode mercury_output_uni_mode(in, in, in, di, uo) is det.

:- pred mercury_output_uni_mode_list(list(uni_mode), varset, inst_table,
		io__state, io__state).
:- mode mercury_output_uni_mode_list(in, in, in, di, uo) is det.

:- pred mercury_output_det(determinism, io__state, io__state).
:- mode mercury_output_det(in, di, uo) is det.

	% Output a comma-separated list of variables, making sure that
	% the variable number appears in the variable name if the boolean
	% argument is set to `yes'.

:- pred mercury_output_vars(list(var), varset, bool, io__state, io__state).
:- mode mercury_output_vars(in, in, in, di, uo) is det.

	% Output a variable, making sure that the variable number appears
	% in the variable name if the boolean argument is set to `yes'.

:- pred mercury_output_var(var, varset, bool, io__state, io__state).
:- mode mercury_output_var(in, in, in, di, uo) is det.

	% Output a term, making sure that the variable number appears
	% in variable names if the boolean argument is set to `yes'.

:- pred mercury_output_term(term, varset, bool, io__state, io__state).
:- mode mercury_output_term(in, in, in, di, uo) is det.

:- pred mercury_output_newline(int, io__state, io__state).
:- mode mercury_output_newline(in, di, uo) is det.

:- pred mercury_output_bracketed_constant(const, io__state, io__state).
:- mode mercury_output_bracketed_constant(in, di, uo) is det.

:- pred mercury_output_bracketed_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_bracketed_sym_name(in, di, uo) is det.

:- pred mercury_convert_var_name(string, string).
:- mode mercury_convert_var_name(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_out, prog_util, hlds_pred, hlds_out, (inst).
:- import_module globals, options, termination.
:- import_module bool, int, string, set, term_io, lexer, std_util, require.

%-----------------------------------------------------------------------------%

convert_to_mercury(ProgName, OutputFileName, Items) -->
	io__stderr_stream(StdErr),
	io__tell(OutputFileName, Res),
	( { Res = ok } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		( { Verbose = yes } ->
			io__write_string(StdErr, "% Writing output to "),
			io__write_string(StdErr, OutputFileName),
			io__write_string(StdErr, "..."),
			io__flush_output(StdErr)
		;
			[]
		),
		io__write_string(":- module "),
		mercury_output_bracketed_constant(term__atom(ProgName)),
		io__write_string(".\n"),
		{ inst_table_init(InstTable) },
		mercury_output_item_list(Items, InstTable),
		( { Verbose = yes } ->
			io__write_string(StdErr, " done\n")
		;
			[]
		),
		io__told
	;
		io__write_string(StdErr, "Error: couldn't open file `"),
		io__write_string(StdErr, OutputFileName),
		io__write_string(StdErr, "' for output.\n")
	).

%-----------------------------------------------------------------------------%

	% output the declarations one by one 

:- pred mercury_output_item_list(list(item_and_context), inst_table,
		io__state, io__state).
:- mode mercury_output_item_list(in, in, di, uo) is det.

mercury_output_item_list([], _) --> [].
mercury_output_item_list([Item - Context | Items], InstTable) -->
	mercury_output_item(Item, Context, InstTable),
	mercury_output_item_list(Items, InstTable).

%-----------------------------------------------------------------------------%

:- pred mercury_output_item(item, term__context, inst_table,
		io__state, io__state).
:- mode mercury_output_item(in, in, in, di, uo) is det.

	% dispatch on the different types of items

mercury_output_item(type_defn(VarSet, TypeDefn, _Cond), Context, _) -->
	maybe_output_line_number(Context),
	mercury_output_type_defn(VarSet, TypeDefn, Context).

mercury_output_item(inst_defn(VarSet, InstDefn, _Cond), Context,
		InstTable) -->
	maybe_output_line_number(Context),
	mercury_output_inst_defn(VarSet, InstDefn, Context, InstTable).

mercury_output_item(mode_defn(VarSet, ModeDefn, _Cond), Context,
		InstTable) -->
	maybe_output_line_number(Context),
	mercury_output_mode_defn(VarSet, ModeDefn, Context, InstTable).

mercury_output_item(pred(VarSet, PredName, TypesAndModes, Det, _Cond), Context,
		InstTable) -->
	maybe_output_line_number(Context),
	mercury_output_pred_decl(VarSet, PredName, TypesAndModes, Det, Context,
		InstTable).

mercury_output_item(func(VarSet, PredName, TypesAndModes, RetTypeAndMode, Det,
		_Cond), Context, InstTable) -->
	maybe_output_line_number(Context),
	mercury_output_func_decl(VarSet, PredName, TypesAndModes,
			RetTypeAndMode, Det, Context, InstTable).

mercury_output_item(pred_mode(VarSet, PredName, Modes, MaybeDet, _Cond),
			Context, InstTable) -->
	maybe_output_line_number(Context),
	mercury_output_pred_mode_decl(VarSet, PredName, Modes, MaybeDet,
			Context, InstTable).

mercury_output_item(func_mode(VarSet, PredName, Modes, RetMode, MaybeDet,
		_Cond), Context, InstTable) -->
	maybe_output_line_number(Context),
	mercury_output_func_mode_decl(VarSet, PredName, Modes, RetMode,
			MaybeDet, Context, InstTable).

mercury_output_item(module_defn(VarSet, ModuleDefn), Context, _) -->
	maybe_output_line_number(Context),
	mercury_output_module_defn(VarSet, ModuleDefn, Context).

mercury_output_item(pred_clause(VarSet, PredName, Args, Body), Context, _) -->
	maybe_output_line_number(Context),
	mercury_output_pred_clause(VarSet, PredName, Args, Body, Context).

mercury_output_item(func_clause(VarSet, FuncName, Args, Result, Body),
		Context, _) -->
	maybe_output_line_number(Context),
	mercury_output_func_clause(VarSet, FuncName, Args, Result, Body,
		Context).

mercury_output_item(pragma(Pragma), Context, InstTable) -->
	maybe_output_line_number(Context),
	(
		{ Pragma = source_file(SourceFile) },
		mercury_output_pragma_source_file(SourceFile)
	;
		{ Pragma = c_header_code(C_HeaderString) },
		mercury_output_pragma_c_header(C_HeaderString)
	;
		{ Pragma = c_code(Code) }, 
		mercury_output_pragma_c_body_code(Code)
	;
		{ Pragma = c_code(MayCallMercury, Pred, PredOrFunc, Vars,
			VarSet, C_CodeString) }, 
		mercury_output_pragma_c_code(MayCallMercury, Pred, PredOrFunc, 
			Vars, no, VarSet, C_CodeString, InstTable)
	;
		{ Pragma = c_code(MayCallMercury, Pred, PredOrFunc, Vars,
			SavedVars, LabelNames, VarSet, C_CodeString) }, 
		mercury_output_pragma_c_code(MayCallMercury, Pred, PredOrFunc, 
			Vars, yes(SavedVars - LabelNames), VarSet, C_CodeString,
			InstTable)
	;
		{ Pragma = export(Pred, PredOrFunc, ModeList, C_Function) },
		mercury_output_pragma_export(Pred, PredOrFunc, ModeList,
			C_Function, InstTable)
	;
		{ Pragma = obsolete(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "obsolete")
	;
		{ Pragma = memo(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "memo")
	;
		{ Pragma = inline(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "inline")
	;
		{ Pragma = no_inline(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "no_inline")
	;
		{ Pragma = unused_args(PredOrFunc, PredName,
			Arity, ProcId, UnusedArgs) },
		mercury_output_pragma_unused_args(PredOrFunc,
			PredName, Arity, ProcId, UnusedArgs)
	;
		{ Pragma = fact_table(Pred, Arity, FileName) },
		mercury_output_pragma_fact_table(Pred, Arity, FileName)
	;
		{ Pragma = termination_info(PredOrFunc, PredName, 
			ModeList, Termination) },
		termination__output_pragma_termination_info(PredOrFunc,
			PredName, argument_modes(InstTable, ModeList),
			Termination, Context)
	;
		{ Pragma = terminates(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "terminates")
	;
		{ Pragma = does_not_terminate(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate,
			"does_not_terminate")
	;
		{ Pragma = check_termination(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate,
			"check_termination")
	).

mercury_output_item(nothing, _, _) --> [].

%-----------------------------------------------------------------------------%

mercury_output_pragma_unused_args(PredOrFunc, SymName,
		Arity, ProcId, UnusedArgs) -->
	io__write_string(":- pragma unused_args("),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(", "),
	mercury_output_bracketed_sym_name(SymName),
	io__write_string(", "),
	io__write_int(Arity),
	io__write_string(", "),
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_int(ProcInt),
	io__write_string(", ["),
	mercury_output_int_list(UnusedArgs),
	io__write_string("]).\n").

:- pred mercury_output_int_list(list(int)::in,
		io__state::di, io__state::uo) is det.

mercury_output_int_list([]) --> [].
mercury_output_int_list([First | Rest]) -->
	io__write_int(First),
	mercury_output_int_list_2(Rest).

:- pred mercury_output_int_list_2(list(int)::in,
		io__state::di, io__state::uo) is det.

mercury_output_int_list_2([]) --> [].
mercury_output_int_list_2([First | Rest]) -->
	io__write_string(", "),
	io__write_int(First),
	mercury_output_int_list_2(Rest).

:- pred mercury_output_module_defn(varset, module_defn, term__context,
			io__state, io__state).
:- mode mercury_output_module_defn(in, in, in, di, uo) is det.

mercury_output_module_defn(_VarSet, Module, _Context) -->
	( { Module = import(module(ImportedModules)) } ->
		io__write_string(":- import_module "),
		mercury_write_module_spec_list(ImportedModules),
		io__write_string(".\n")
	; { Module = use(module(UsedModules)) } ->
		io__write_string(":- use_module "),
		mercury_write_module_spec_list(UsedModules),
		io__write_string(".\n")
	; { Module = interface } ->
		io__write_string(":- interface.\n")
	; { Module = implementation } ->
		io__write_string(":- implementation.\n")
	;
		% XXX unimplemented
		io__write_string("% unimplemented module declaration\n")
	).

:- pred mercury_write_module_spec_list(list(module_specifier),
					io__state, io__state).
:- mode mercury_write_module_spec_list(in, di, uo) is det.

mercury_write_module_spec_list([]) --> [].
mercury_write_module_spec_list([ModuleName | ModuleNames]) -->
	mercury_output_bracketed_constant(term__atom(ModuleName)),
	( { ModuleNames = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_write_module_spec_list(ModuleNames)
	).

mercury_output_inst_defn(VarSet, abstract_inst(Name, Args), Context, _) -->
	io__write_string(":- inst ("),
	{ construct_qualified_term(Name, Args, Context, InstTerm) },
	mercury_output_term(InstTerm, VarSet, no),
	io__write_string(").\n").
mercury_output_inst_defn(VarSet, eqv_inst(Name, Args, Body), Context,
		InstTable) -->
	io__write_string(":- inst ("),
	{ construct_qualified_term(Name, Args, Context, InstTerm) },
	mercury_output_term(InstTerm, VarSet, no),
	io__write_string(") = "),
	mercury_output_inst(expand_noisily, Body, VarSet, InstTable),
	io__write_string(".\n").

mercury_output_structured_inst_list(_, [], _, _, _) --> [].
mercury_output_structured_inst_list(Expand, [Inst | Insts], Indent0, VarSet,
		InstTable) -->
	mercury_output_structured_inst(Expand, Inst, Indent0, VarSet,
		InstTable),
	mercury_output_structured_inst_list(Expand, Insts, Indent0, VarSet,
		InstTable).

mercury_output_inst_list(_, [], _, _) --> [].
mercury_output_inst_list(Expand, [Inst | Insts], VarSet, InstTable) -->
	mercury_output_inst(Expand, Inst, VarSet, InstTable),
	( { Insts = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_inst_list(Expand, Insts, VarSet, InstTable)
	).
  
mercury_output_structured_inst(_, any(Uniq), Indent, _, _) -->
	mercury_output_tabs(Indent),
	mercury_output_any_uniqueness(Uniq),
	io__write_string("\n").
mercury_output_structured_inst(Expand, alias(Key), Indent, VarSet,
		InstTable) -->
	mercury_output_tabs(Indent),
	{ inst_table_get_inst_key_table(InstTable, IKT) },
	( { Expand = expand_noisily },
		io__write_string("alias("),
		inst_key_table_write_inst_key(IKT, Key),
		io__write_string(",\n"),
		{ inst_key_table_lookup(IKT, Key, Inst) },
		{ Indent1 is Indent + 1 },
		mercury_output_structured_inst(expand_noisily, Inst, Indent1,
			VarSet, InstTable),
		mercury_output_tabs(Indent),
		io__write_string(")\n")
	; { Expand = dont_expand },
		io__write_string("alias("),
		inst_key_table_write_inst_key(IKT, Key),
		mercury_output_tabs(Indent),
		io__write_string(")\n")
	; { Expand = expand_silently },
		{ inst_key_table_lookup(IKT, Key, Inst) },
		mercury_output_inst(Expand, Inst, VarSet, InstTable)
	).
mercury_output_structured_inst(_, free, Indent, _, _) -->
	mercury_output_tabs(Indent),
	io__write_string("free\n").
mercury_output_structured_inst(_, free(_T), Indent, _, _) -->
	mercury_output_tabs(Indent),
	io__write_string("free(with some type)\n").
mercury_output_structured_inst(Expand, bound(Uniq, BoundInsts), Indent,
		VarSet, InstTable) -->
	mercury_output_tabs(Indent),
	mercury_output_uniqueness(Uniq, "bound"),
	io__write_string("(\n"),
	mercury_output_structured_bound_insts(Expand, BoundInsts, Indent,
		VarSet, InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst(_Expand, ground(Uniq, MaybePredInfo), Indent,
		VarSet, _InstTable) -->
	mercury_output_tabs(Indent),
	(
		{ MaybePredInfo = yes(pred_inst_info(PredOrFunc, Modes, Det)) }
	->
		( { Uniq = shared } ->
			[]
		;
			io__write_string("/* "),
			mercury_output_uniqueness(Uniq, "ground"),
			io__write_string(" */")
		),
		{ Modes = argument_modes(ArgInstTable, ArgModes) },
		(
			{ PredOrFunc = predicate },
			( { ArgModes = [] } ->
				io__write_string("(pred) is "),
				mercury_output_det(Det),
				io__write_string(")\n")
			;
				io__write_string("(pred("),
				mercury_output_mode_list(ArgModes, VarSet,
					ArgInstTable),
				io__write_string(") is "),
				mercury_output_det(Det),
				io__write_string(")\n")
			)
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(ArgModes, FuncArgModes,
				RetMode) },
			( { ArgModes = [] } ->
				io__write_string("((func) = ")
			;
				io__write_string("(func("),
				mercury_output_mode_list(FuncArgModes, VarSet,
					ArgInstTable),
				io__write_string(") = ")
			),
			mercury_output_mode(RetMode, VarSet, ArgInstTable),
			io__write_string(" is "),
			mercury_output_det(Det),
			io__write_string(")\n")
		)
	;
		mercury_output_uniqueness(Uniq, "ground"),
		io__write_string("\n")
	).
mercury_output_structured_inst(_, inst_var(Var), Indent, VarSet, _) -->
	mercury_output_tabs(Indent),
	mercury_output_var(Var, VarSet, no),
	io__write_string("\n").
mercury_output_structured_inst(Expand, abstract_inst(Name, Args), Indent,
		VarSet, InstTable) -->
	mercury_output_structured_inst_name(Expand, user_inst(Name, Args),
		Indent, VarSet, InstTable).
mercury_output_structured_inst(Expand, defined_inst(InstName), Indent, VarSet,
		InstTable) -->
	mercury_output_structured_inst_name(Expand, InstName, Indent, VarSet,
		InstTable).
mercury_output_structured_inst(_, not_reached, Indent, _, _) -->
	mercury_output_tabs(Indent),
	io__write_string("not_reached\n").

mercury_output_inst(_, any(Uniq), _, _) -->
	mercury_output_any_uniqueness(Uniq).
mercury_output_inst(Expand, alias(Key), VarSet, InstTable) -->
	{ inst_table_get_inst_key_table(InstTable, IKT) },
	( { Expand = expand_noisily },
		io__write_string("alias("),
		inst_key_table_write_inst_key(IKT, Key),
		io__write_string(", "),
		{ inst_key_table_lookup(IKT, Key, Inst) },
		mercury_output_inst(Expand, Inst, VarSet, InstTable),
		io__write_string(")")
	; { Expand = dont_expand },
		io__write_string("alias("),
		inst_key_table_write_inst_key(IKT, Key),
		io__write_string(")")
	; { Expand = expand_silently },
		% expand_silently
		{ inst_key_table_lookup(IKT, Key, Inst) },
		mercury_output_inst(Expand, Inst, VarSet, InstTable)
	).
mercury_output_inst(_, free, _, _) -->
	io__write_string("free").
mercury_output_inst(_, free(_T), _, _) -->
	io__write_string("free(with some type)").
mercury_output_inst(Expand, bound(Uniq, BoundInsts), VarSet, InstTable) -->
	mercury_output_uniqueness(Uniq, "bound"),
	io__write_string("("),
	mercury_output_bound_insts(Expand, BoundInsts, VarSet, InstTable),
	io__write_string(")").
mercury_output_inst(_, ground(Uniq, MaybePredInfo), VarSet,
		_InstTable) -->
	(	
		{ MaybePredInfo = yes(pred_inst_info(PredOrFunc, Modes, Det)) }
	->
		( { Uniq = shared } ->
			[]
		;
			io__write_string("/* "),
			mercury_output_uniqueness(Uniq, "ground"),
			io__write_string(" */")
		),
		{ Modes = argument_modes(ArgInstTable, ArgModes) },
		(
			{ PredOrFunc = predicate },
			( { ArgModes = [] } ->
				io__write_string("((pred) is "),
				mercury_output_det(Det),
				io__write_string(")")
			;
				io__write_string("(pred("),
				mercury_output_mode_list(ArgModes, VarSet,
					ArgInstTable),
				io__write_string(") is "),
				mercury_output_det(Det),
				io__write_string(")")
			)
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(ArgModes, FuncArgModes,
				RetMode) },
			( { ArgModes = [] } ->
				io__write_string("((func)")
			;
				io__write_string("(func("),
				mercury_output_mode_list(FuncArgModes, VarSet,
					ArgInstTable),
				io__write_string(")")
			),
			io__write_string(" = "),
			mercury_output_mode(RetMode, VarSet, ArgInstTable),
			io__write_string(" is "),
			mercury_output_det(Det),
			io__write_string(")")
		)
	;
		mercury_output_uniqueness(Uniq, "ground")
	).
mercury_output_inst(_, inst_var(Var), VarSet, _) -->
	mercury_output_var(Var, VarSet, no).
mercury_output_inst(_, abstract_inst(Name, Args), VarSet, InstTable) -->
	mercury_output_inst_name(user_inst(Name, Args), VarSet, InstTable).
mercury_output_inst(_, defined_inst(InstName), VarSet, InstTable) -->
	mercury_output_inst_name(InstName, VarSet, InstTable).
mercury_output_inst(_, not_reached, _, _) -->
	io__write_string("not_reached").

:- pred mercury_output_structured_inst_name(expand_inst_alias, inst_name, int,
	varset, inst_table, io__state, io__state).
:- mode mercury_output_structured_inst_name(in, in, in, in, in, di, uo) is det.

mercury_output_structured_inst_name(Expand, user_inst(Name, Args), Indent,
		VarSet, InstTable) -->
	( { Args = [] } ->
		mercury_output_tabs(Indent),
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_tabs(Indent),
		mercury_output_sym_name(Name),
		io__write_string("(\n"),
		{ Indent1 is Indent + 1 },
		mercury_output_structured_inst_list(Expand, Args, Indent1,
			VarSet, InstTable),
		mercury_output_tabs(Indent),
		io__write_string(")\n")
	).
mercury_output_structured_inst_name(Expand, merge_inst(InstA, InstB), Indent,
		VarSet, InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$merge_inst(\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_list(Expand, [InstA, InstB], Indent1,
		VarSet, InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(Expand, shared_inst(InstName), Indent,
		VarSet, InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$shared_inst(\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(Expand, InstName, Indent1, VarSet,
		InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(Expand, mostly_uniq_inst(InstName),
		Indent, VarSet, InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$mostly_uniq_inst(\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(Expand, InstName, Indent1, VarSet,
		InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(Expand,
		unify_inst(Liveness, InstA, InstB, Real), Indent, VarSet,
		InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$unify("),
	( { Liveness = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	( { Real = real_unify } ->
		io__write_string("real,\n")
	;
		io__write_string("fake,\n")
	),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_list(Expand, [InstA, InstB], Indent1,
		VarSet, InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(Expand,
		ground_inst(InstName, IsLive, Uniq, Real),
		Indent, VarSet, InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$ground("),
	( { IsLive = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	( { Real = real_unify } ->
		io__write_string("real, ")
	;
		io__write_string("fake, ")
	),
	mercury_output_uniqueness(Uniq, "shared"),
	io__write_string(",\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(Expand, InstName, Indent1, VarSet,
		InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(Expand,
		any_inst(InstName, IsLive, Uniq, Real),
		Indent, VarSet, InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$any("),
	( { IsLive = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	( { Real = real_unify } ->
		io__write_string("real, ")
	;
		io__write_string("fake, ")
	),
	mercury_output_uniqueness(Uniq, "shared"),
	io__write_string(",\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(Expand, InstName, Indent1, VarSet,
		InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(_Expand, typed_ground(Uniqueness, Type),
		Indent, _VarSet, _InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$typed_ground("),
	mercury_output_uniqueness(Uniqueness, "shared"),
	io__write_string(", "),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(")\n").
mercury_output_structured_inst_name(Expand, typed_inst(Type, InstName),
		Indent, VarSet, InstTable) -->
	mercury_output_tabs(Indent),
	io__write_string("$typed_inst("),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(",\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(Expand, InstName, Indent1, VarSet,
		InstTable),
	mercury_output_tabs(Indent),
	io__write_string(")\n").

:- pred mercury_output_inst_name(inst_name, varset, inst_table,
		io__state, io__state).
:- mode mercury_output_inst_name(in, in, in, di, uo) is det.

mercury_output_inst_name(user_inst(Name, Args), VarSet, InstTable) -->
	( { Args = [] } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_inst_list(expand_noisily, Args, VarSet,
			InstTable),
		io__write_string(")")
	).
mercury_output_inst_name(merge_inst(InstA, InstB), VarSet, InstTable) -->
	io__write_string("$merge_inst("),
	mercury_output_inst_list(expand_noisily, [InstA, InstB], VarSet,
		InstTable),
	io__write_string(")").
mercury_output_inst_name(shared_inst(InstName), VarSet, InstTable) -->
	io__write_string("$shared_inst("),
	mercury_output_inst_name(InstName, VarSet, InstTable),
	io__write_string(")").
mercury_output_inst_name(mostly_uniq_inst(InstName), VarSet, InstTable) -->
	io__write_string("$mostly_uniq_inst("),
	mercury_output_inst_name(InstName, VarSet, InstTable),
	io__write_string(")").
mercury_output_inst_name(unify_inst(Liveness, InstA, InstB, Real), VarSet,
		InstTable) -->
	io__write_string("$unify("),
	( { Liveness = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	mercury_output_inst_list(expand_noisily, [InstA, InstB], VarSet,
		InstTable),
	( { Real = real_unify } ->
		io__write_string(", real")
	;
		io__write_string(", fake")
	),
	io__write_string(")").
mercury_output_inst_name(ground_inst(InstName, IsLive, Uniq, Real), VarSet,
		InstTable) -->
	io__write_string("$ground("),
	mercury_output_inst_name(InstName, VarSet, InstTable),
	io__write_string(", "),
	( { IsLive = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	mercury_output_uniqueness(Uniq, "shared"),
	( { Real = real_unify } ->
		io__write_string(", real")
	;
		io__write_string(", fake")
	),
	io__write_string(")").
mercury_output_inst_name(any_inst(InstName, IsLive, Uniq, Real), VarSet,
		InstTable) -->
	io__write_string("$any("),
	mercury_output_inst_name(InstName, VarSet, InstTable),
	io__write_string(", "),
	( { IsLive = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	mercury_output_uniqueness(Uniq, "shared"),
	( { Real = real_unify } ->
		io__write_string(", real")
	;
		io__write_string(", fake")
	),
	io__write_string(")").
mercury_output_inst_name(typed_ground(Uniqueness, Type), _VarSet, _) -->
	io__write_string("$typed_ground("),
	mercury_output_uniqueness(Uniqueness, "shared"),
	io__write_string(", "),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(")").
mercury_output_inst_name(typed_inst(Type, InstName), VarSet, InstTable) -->
	io__write_string("$typed_inst("),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(", "),
	mercury_output_inst_name(InstName, VarSet, InstTable),
	io__write_string(")").

:- pred mercury_output_uniqueness(uniqueness, string, io__state, io__state).
:- mode mercury_output_uniqueness(in, in, di, uo) is det.

mercury_output_uniqueness(shared, SharedString) -->
	io__write_string(SharedString).
mercury_output_uniqueness(unique, _) -->
	io__write_string("unique").
mercury_output_uniqueness(mostly_unique, _) -->
	io__write_string("mostly_unique").
mercury_output_uniqueness(clobbered, _) -->
	io__write_string("clobbered").
mercury_output_uniqueness(mostly_clobbered, _) -->
	io__write_string("mostly_clobbered").

:- pred mercury_output_any_uniqueness(uniqueness, io__state, io__state).
:- mode mercury_output_any_uniqueness(in, di, uo) is det.

mercury_output_any_uniqueness(shared) -->
	io__write_string("any").
mercury_output_any_uniqueness(unique) -->
	io__write_string("unique_any").
mercury_output_any_uniqueness(mostly_unique) -->
	io__write_string("mostly_unique_any").
mercury_output_any_uniqueness(clobbered) -->
	io__write_string("clobbered_any").
mercury_output_any_uniqueness(mostly_clobbered) -->
	io__write_string("mostly_clobbered_any").

:- pred mercury_output_structured_bound_insts(expand_inst_alias,
		list(bound_inst), int, varset, inst_table,
		io__state, io__state).
:- mode mercury_output_structured_bound_insts(in, in, in, in, in,
		di, uo) is det.

mercury_output_structured_bound_insts(_, [], _, _, _) --> [].
mercury_output_structured_bound_insts(Expand,
		[functor(ConsId, Args) | BoundInsts], Indent0, VarSet,
		InstTable) -->
	{ Indent1 is Indent0 + 1 },
	{ Indent2 is Indent1 + 1 },
	( { Args = [] } ->
		mercury_output_tabs(Indent1),
		mercury_output_cons_id(ConsId, yes),
		io__write_string("\n")
	;
		mercury_output_tabs(Indent1),
		mercury_output_cons_id(ConsId, no),
		io__write_string("(\n"),
		mercury_output_structured_inst_list(Expand, Args, Indent2,
			VarSet, InstTable),
		mercury_output_tabs(Indent1),
		io__write_string(")\n")
	),
	( { BoundInsts = [] } ->
		[]
	;
		mercury_output_tabs(Indent0),
		io__write_string(";\n"),
		mercury_output_structured_bound_insts(Expand, BoundInsts,
			Indent0, VarSet, InstTable)
	).

:- pred mercury_output_bound_insts(expand_inst_alias, list(bound_inst), varset,
		inst_table, io__state, io__state).
:- mode mercury_output_bound_insts(in, in, in, in, di, uo) is det.

mercury_output_bound_insts(_, [], _, _) --> [].
mercury_output_bound_insts(Expand, [functor(ConsId, Args) | BoundInsts], VarSet,
		InstTable) -->
	( { Args = [] } ->
		mercury_output_cons_id(ConsId, yes)
	;
		mercury_output_cons_id(ConsId, no),
		io__write_string("("),
		mercury_output_inst_list(Expand, Args, VarSet, InstTable),
		io__write_string(")")
	),
	( { BoundInsts = [] } ->
		[]
	;
		io__write_string(" ; "),
		mercury_output_bound_insts(Expand, BoundInsts, VarSet,
			InstTable)
	).

mercury_output_cons_id(cons(Name, _), Bracketed) -->
	( { Bracketed = yes } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name)
	).
mercury_output_cons_id(int_const(X), _) -->
	io__write_int(X).
mercury_output_cons_id(float_const(X), _) -->
	io__write_float(X).
mercury_output_cons_id(string_const(X), _) -->
	io__write_strings(["""", X, """"]).
mercury_output_cons_id(pred_const(PredId, ProcId), _) -->
	% XXX Sufficient, but probably should print this out in
	%     name/arity form.

	{ pred_id_to_int(PredId, PredInt) },
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_string("<pred_const("),
	io__write_int(PredInt),
	io__write_string(", "),
	io__write_int(ProcInt),
	io__write_string(")>").
mercury_output_cons_id(code_addr_const(PredId, ProcId), _) -->
	% XXX Sufficient, but probably should print this out in
	%     name/arity form.

	{ pred_id_to_int(PredId, PredInt) },
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_string("<code_addr_const("),
	io__write_int(PredInt),
	io__write_string(", "),
	io__write_int(ProcInt),
	io__write_string(")>").
mercury_output_cons_id(base_type_info_const(Module, Type, Arity), _) -->
	{ string__int_to_string(Arity, ArityString) },
	io__write_strings(["<base_type_info for ", Module, ":", Type, "/",
		ArityString, ">"]).

mercury_output_mode_defn(VarSet, eqv_mode(Name, Args, Mode), Context,
		InstTable) -->
	io__write_string(":- mode ("),
	{ construct_qualified_term(Name, Args, Context, ModeTerm) },
	mercury_output_term(ModeTerm, VarSet, no),
	io__write_string(") :: "),
	mercury_output_mode(Mode, VarSet, InstTable),
	io__write_string(".\n").

mercury_output_mode_list([], _VarSet, _InstTable) --> [].
mercury_output_mode_list([Mode | Modes], VarSet, InstTable) -->
	mercury_output_mode(Mode, VarSet, InstTable),
	( { Modes = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_mode_list(Modes, VarSet, InstTable)
	).

mercury_output_uni_mode_list([], _VarSet, _InstTable) --> [].
mercury_output_uni_mode_list([Mode | Modes], VarSet, InstTable) -->
	mercury_output_uni_mode(Mode, VarSet, InstTable),
	( { Modes = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_uni_mode_list(Modes, VarSet, InstTable)
	).

mercury_output_uni_mode((InstA1 - InstB1 -> InstA2 - InstB2), VarSet,
		InstTable) -->
	mercury_output_mode((InstA1 -> InstA2), VarSet, InstTable),
	io__write_string(" = "),
	mercury_output_mode((InstB1 -> InstB2), VarSet, InstTable).

mercury_output_mode((InstA -> InstB), VarSet, InstTable) -->
	( 
	    %
	    % check for higher-order pred or func modes, and output them
	    % in a nice format
	    %
	    { InstA = ground(_Uniq,
			yes(pred_inst_info(_PredOrFunc, _Modes, _Det))) },
	    { InstB = InstA }
	->
	    mercury_output_inst(expand_noisily, InstA, VarSet, InstTable)
	;
	    io__write_string("("),
	    mercury_output_inst(expand_noisily, InstA, VarSet, InstTable),
	    io__write_string(" -> "),
	    mercury_output_inst(expand_noisily, InstB, VarSet, InstTable),
	    io__write_string(")")
	).
mercury_output_mode(user_defined_mode(Name, Args), VarSet, InstTable) -->
	( { Args = [] } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_inst_list(expand_noisily, Args, VarSet,
			InstTable),
		io__write_string(")")
	).

%-----------------------------------------------------------------------------%

mercury_output_type_defn(VarSet, TypeDefn, Context) -->
	mercury_output_type_defn_2(TypeDefn, VarSet, Context).

:- pred mercury_output_type_defn_2(type_defn, varset, term__context,
			io__state, io__state).
:- mode mercury_output_type_defn_2(in, in, in, di, uo) is det.

mercury_output_type_defn_2(uu_type(_Name, _Args, _Body), _VarSet, Context) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string("warning: undiscriminated union types not yet supported.\n"),
	io__set_output_stream(OldStream, _).

mercury_output_type_defn_2(abstract_type(Name, Args), VarSet, Context) -->
	io__write_string(":- type "),
	{ construct_qualified_term(Name, Args, Context, TypeTerm) },
	mercury_output_term(TypeTerm, VarSet, no),
	io__write_string(".\n").

mercury_output_type_defn_2(eqv_type(Name, Args, Body), VarSet, Context) -->
	io__write_string(":- type "),
	{ construct_qualified_term(Name, Args, Context, TypeTerm) },
	mercury_output_term(TypeTerm, VarSet, no),
	io__write_string(" == "),
	mercury_output_term(Body, VarSet, no),
	io__write_string(".\n").

mercury_output_type_defn_2(du_type(Name, Args, Ctors, MaybeEqualityPred),
		VarSet, Context) -->
	io__write_string(":- type "),
	{ construct_qualified_term(Name, Args, Context, TypeTerm) },
	mercury_output_term(TypeTerm, VarSet, no),
	io__write_string("\n\t--->\t"),
	mercury_output_ctors(Ctors, VarSet),
	( { MaybeEqualityPred = yes(EqualityPredName) } ->
		io__write_string("\n\twhere equality is "),
		mercury_output_bracketed_sym_name(EqualityPredName)
	;
		[]
	),
	io__write_string(".\n").

:- pred mercury_output_ctors(list(constructor), varset,
				io__state, io__state).
:- mode mercury_output_ctors(in, in, di, uo) is det.

mercury_output_ctors([], _) --> [].
mercury_output_ctors([Name - Args | Ctors], VarSet) -->
	% we need to quote ';'/2 and '{}'/2
	{ list__length(Args, Arity) },
	(
		{ Arity = 2 },
		{ Name = unqualified(";") ; Name = unqualified("{}") }
	->
		io__write_string("{ ")
	;
		[]
	),
	(
		{ Args = [Arg | Rest] }
	->
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_ctor_arg(VarSet, Arg),
		mercury_output_remaining_ctor_args(VarSet, Rest),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(Name)
	),
	(
		{ Arity = 2 },
		{ Name = unqualified(";") ; Name = unqualified("{}") }
	->
		io__write_string(" }")
	;
		[]
	),
	( { Ctors \= [] } ->
		io__write_string("\n\t;\t")
	;	
		[]
	),
	mercury_output_ctors(Ctors, VarSet).

mercury_output_ctor_arg(Varset, N - T) -->
	mercury_output_ctor_arg_name_prefix(N),
	mercury_output_term(T, Varset, no).

mercury_output_remaining_ctor_args(_Varset, []) --> [].
mercury_output_remaining_ctor_args(Varset, [N - T | As]) -->
	io__write_string(", "),
	mercury_output_ctor_arg_name_prefix(N),
	mercury_output_term(T, Varset, no),
	mercury_output_remaining_ctor_args(Varset, As).

:- pred mercury_output_ctor_arg_name_prefix(string,
				io__state, io__state).
:- mode mercury_output_ctor_arg_name_prefix(in, di, uo) is det.

mercury_output_ctor_arg_name_prefix(Name) -->
	( { Name = "" } ->
		[]
	;
		io__write_string(Name),
		io__write_string(": ")
	).

%-----------------------------------------------------------------------------%

:- pred mercury_output_pred_decl(varset, sym_name, list(type_and_mode),
		maybe(determinism), term__context, inst_table,
		io__state, io__state).
:- mode mercury_output_pred_decl(in, in, in, in, in, in, di, uo) is det.

mercury_output_pred_decl(VarSet, PredName, TypesAndModes, MaybeDet, Context,
		InstTable) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	mercury_output_pred_type(VarSet, PredName, Types, MaybeDet, Context),
	(
		{ MaybeModes = yes(Modes) },
		{ Modes \= [] }
	->
		mercury_output_pred_mode_decl(VarSet, PredName, Modes,
				MaybeDet, Context, InstTable)
	;
		[]
	).

mercury_output_pred_type(VarSet, PredName, Types, MaybeDet, _Context) -->
	io__write_string(":- pred "),
	(
		{ Types = [Type | Rest] }
	->
		mercury_output_sym_name(PredName),
		io__write_string("("),
		mercury_output_term(Type, VarSet, no),
		mercury_output_remaining_terms(Rest, VarSet, no),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(PredName),
		mercury_output_det_annotation(MaybeDet)
	),

	% We need to handle is/2 specially, because it's used for
	% determinism annotations (`... is det'), and so the compiler
	% will misinterpret a bare `:- pred is(int, int_expr)' as
	% `:- pred int is int_expr' and then report some very confusing
	% error message.  Thus you _have_ to give a determinism
	% annotation in the pred declaration for is/2, eg.
	% `:- pred is(int, int_expr) is det.'
	% (Yes, this made me puke too.)
	%
	% The alternative is a term traversal in compiler/prog_io.m 
	% get_determinism/3.  The alternative is more `nice', but less
	% efficient.

	(
		{ unqualify_name(PredName, "is") },
		{ list__length(Types, 2) }
	->
		mercury_output_det_annotation(MaybeDet)
	;
		[]
	),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%

:- pred mercury_output_func_decl(varset, sym_name, list(type_and_mode),
		type_and_mode, maybe(determinism), term__context,
		inst_table, io__state, io__state).
:- mode mercury_output_func_decl(in, in, in, in, in, in, in, di, uo) is det.

mercury_output_func_decl(VarSet, FuncName, TypesAndModes, RetTypeAndMode,
		MaybeDet, Context, InstTable) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	{ split_type_and_mode(RetTypeAndMode, RetType, MaybeRetMode) },
	(
		{ MaybeModes = yes(Modes) },
		{ MaybeRetMode = yes(RetMode) }
	->
		mercury_output_func_type(VarSet, FuncName, Types, RetType,
				no, Context),
		mercury_output_func_mode_decl(VarSet, FuncName, Modes, RetMode,
				MaybeDet, Context, InstTable)
	;
		mercury_output_func_type(VarSet, FuncName, Types, RetType,
				MaybeDet, Context)
	).

mercury_output_func_type(VarSet, FuncName, Types, RetType, MaybeDet, _Context)
		-->
	io__write_string(":- func "),
	(
		{ Types = [Type | Rest] }
	->
		mercury_output_sym_name(FuncName),
		io__write_string("("),
		mercury_output_term(Type, VarSet, no),
		mercury_output_remaining_terms(Rest, VarSet, no),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(FuncName)
	),
	io__write_string(" = "),
	mercury_output_term(RetType, VarSet, no),
	mercury_output_det_annotation(MaybeDet),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%

	% Output a mode declaration for a predicate or function.

mercury_output_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes, MaybeDet,
		Context, InstTable) -->
	(	{ PredOrFunc = predicate },
		mercury_output_pred_mode_subdecl(InstVarSet, Name, Modes,
				MaybeDet, Context, InstTable)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
		mercury_output_func_mode_subdecl(InstVarSet, Name, ArgModes,
				RetMode, MaybeDet, Context, InstTable)
	).

	% Output a mode declaration for a predicate.

mercury_output_pred_mode_decl(VarSet, PredName, Modes, MaybeDet, Context,
		InstTable) -->
	io__write_string(":- mode "),
	mercury_output_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		Context, InstTable),
	io__write_string(".\n").

mercury_output_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		_Context, InstTable) -->
	(
		{ Modes \= [] }
	->
		mercury_output_sym_name(PredName),
		io__write_string("("),
		mercury_output_mode_list(Modes, VarSet, InstTable),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(PredName)
	),
	mercury_output_det_annotation(MaybeDet).

	% Output a mode declaration for a function.

mercury_output_func_mode_decl(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context, InstTable) -->
	io__write_string(":- mode "),
	mercury_output_func_mode_subdecl(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context, InstTable),
	io__write_string(".\n").

mercury_output_func_mode_subdecl(VarSet, FuncName, Modes, RetMode, MaybeDet,
		_Context, InstTable) -->
	(
		{ Modes \= [] }
	->
		mercury_output_sym_name(FuncName),
		io__write_string("("),
		mercury_output_mode_list(Modes, VarSet, InstTable),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(FuncName)
	),
	io__write_string(" = "),
	mercury_output_mode(RetMode, VarSet, InstTable),
	mercury_output_det_annotation(MaybeDet).

:- pred mercury_output_det_annotation(maybe(determinism), io__state, io__state).
:- mode mercury_output_det_annotation(in, di, uo) is det.

mercury_output_det_annotation(MaybeDet) -->
	(
		{ MaybeDet = no },
		[]
	;
		{ MaybeDet = yes(Det) },
		io__write_string(" is "),
		mercury_output_det(Det)
	).

mercury_output_det(det) -->
	io__write_string("det").
mercury_output_det(semidet) -->
	io__write_string("semidet").
mercury_output_det(nondet) -->
	io__write_string("nondet").
mercury_output_det(multidet) -->
	io__write_string("multi").
mercury_output_det(cc_multidet) -->
	io__write_string("cc_multi").
mercury_output_det(cc_nondet) -->
	io__write_string("cc_nondet").
mercury_output_det(failure) -->
	io__write_string("failure").
mercury_output_det(erroneous) -->
	io__write_string("erroneous").

	%
	% Use mercury_output_bracketed_sym_name/3 when the sym_name has
	% no arguments, otherwise use mercury_output_sym_name/3.
	%

mercury_output_bracketed_sym_name(Name) -->
	(	{ Name = qualified(ModuleName, Name2) },
		mercury_output_bracketed_constant(term__atom(ModuleName)),
		io__write_char(':')
	;
		{ Name = unqualified(Name2) }
	),
	mercury_output_bracketed_constant(term__atom(Name2)).

:- pred mercury_output_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_sym_name(in, di, uo) is det.

mercury_output_sym_name(Name) -->
	(	{ Name = qualified(ModuleName, PredName) },
		mercury_output_bracketed_constant(term__atom(ModuleName)),
		io__write_char(':'),
		mercury_quote_qualified_atom(PredName)
	;
		{ Name = unqualified(PredName) },
		term_io__quote_atom(PredName)
	).

:- pred mercury_quote_qualified_atom(string, io__state, io__state).
:- mode mercury_quote_qualified_atom(in, di, uo) is det.

mercury_quote_qualified_atom(Name) -->
	%
	% If the symname is composed of only graphic token chars,
	% then term_io__quote_atom will not quote it; but since
	% ':' is a graphic token char, it needs to be quoted,
	% otherwise the ':' would be considered part of the
	% symbol name (e.g. "int:<" tokenizes as ["int", ":<"].)
	%
	(
		{ string__to_char_list(Name, Chars) },
		{ \+ (  list__member(Char, Chars),
			\+ lexer__graphic_token_char(Char)) }
	->
		io__write_string("'"),
		term_io__write_escaped_string(Name),
		io__write_string("'")
	;
		term_io__quote_atom(Name)
	).

%-----------------------------------------------------------------------------%

	% Output a clause.

:- pred mercury_output_pred_clause(varset, sym_name, list(term), goal,
		term__context, io__state, io__state).
:- mode mercury_output_pred_clause(in, in, in, in, in, di, uo) is det.

mercury_output_pred_clause(VarSet, PredName, Args, Body, _Context) -->
	mercury_output_sym_name(PredName),
	(
		{ Args = [Arg | Args0] }
	->
		io__write_string("("),
		mercury_output_term(Arg, VarSet, no),
		mercury_output_remaining_terms(Args0, VarSet, no),
		io__write_string(")")
	;
		[]
	),
	(
		{ Body = true - _Context0 }
	->
		[]
	;
		io__write_string(" :-\n\t"),
		mercury_output_goal(Body, VarSet, 1)
	),
	io__write_string(".\n").

	% Output an equation.

:- pred mercury_output_func_clause(varset, sym_name, list(term), term, goal,
		term__context, io__state, io__state).
:- mode mercury_output_func_clause(in, in, in, in, in, in, di, uo) is det.

mercury_output_func_clause(VarSet, PredName, Args, Result, Body, _Context) -->
	mercury_output_sym_name(PredName),
	(
		{ Args = [Arg | Args0] }
	->
		io__write_string("("),
		mercury_output_term(Arg, VarSet, no),
		mercury_output_remaining_terms(Args0, VarSet, no),
		io__write_string(")")
	;
		[]
	),
	io__write_string(" = "),
	mercury_output_term(Result, VarSet, no),
	(
		{ Body = true - _Context0 }
	->
		[]
	;
		io__write_string(" :-\n\t"),
		mercury_output_goal(Body, VarSet, 1)
	),
	io__write_string(".\n").

:- pred mercury_output_goal(goal, varset, int, io__state, io__state).
:- mode mercury_output_goal(in, in, in, di, uo) is det.

mercury_output_goal(Goal - _Context, VarSet, Indent) -->
	mercury_output_goal_2(Goal, VarSet, Indent).

:- pred mercury_output_goal_2(goal_expr, varset, int, io__state, io__state).
:- mode mercury_output_goal_2(in, in, in, di, uo) is det.

mercury_output_goal_2(fail, _, _) -->
	io__write_string("fail").

mercury_output_goal_2(true, _, _) -->
	io__write_string("true").

	% Implication and equivalence should have been transformed out
	% by now
mercury_output_goal_2(implies(_G1,_G2), _VarSet, _Indent) -->
	{ error("mercury_to_mercury: implies/2 in mercury_output_goal")}.

mercury_output_goal_2(equivalent(_G1,_G2), _VarSet, _Indent) -->
	{ error("mercury_to_mercury: equivalent/2 in mercury_output_goal")}.

mercury_output_goal_2(some(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		mercury_output_goal(Goal, VarSet, Indent)
	;
		io__write_string("some ["),
		mercury_output_vars(Vars, VarSet, no),
		io__write_string("] ("),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_goal(Goal, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

mercury_output_goal_2(all(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		mercury_output_goal(Goal, VarSet, Indent)
	;
		io__write_string("all ["),
		mercury_output_vars(Vars, VarSet, no),
		io__write_string("] ("),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_goal(Goal, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

mercury_output_goal_2(if_then_else(Vars, A, B, C), VarSet, Indent) -->
	io__write_string("(if"),
	mercury_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("then"),
	mercury_output_newline(Indent1),
	mercury_output_goal(B, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("else"),
	mercury_output_newline(Indent1),
	mercury_output_goal(C, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal_2(if_then(Vars, A, B), VarSet, Indent) -->
	io__write_string("(if"),
	mercury_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("then"),
	mercury_output_newline(Indent1),
	mercury_output_goal(B, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal_2(not(Goal), VarSet, Indent) -->
	io__write_string("\\+ ("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(Goal, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal_2((A,B), VarSet, Indent) -->
	mercury_output_goal(A, VarSet, Indent),
	io__write_string(","),
	mercury_output_newline(Indent),
	mercury_output_goal(B, VarSet, Indent).

mercury_output_goal_2((A;B), VarSet, Indent) -->
	io__write_string("("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_disj(B, VarSet, Indent),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal_2(call(Name, Term), VarSet, Indent) -->
	mercury_output_call(Name, Term, VarSet, Indent).

mercury_output_goal_2(unify(A, B), VarSet, _Indent) -->
	mercury_output_term(A, VarSet, no),
	io__write_string(" = "),
	mercury_output_term(B, VarSet, no).

:- pred mercury_output_call(sym_name, list(term), varset, int,
	io__state, io__state).
:- mode mercury_output_call(in, in, in, in, di, uo) is det.

mercury_output_call(Name, Term, VarSet, _Indent) -->
	(	
		{ Name = qualified(ModuleName, PredName) },
		io__write_string(ModuleName),
		io__write_string(":")
	;
		{ Name = unqualified(PredName) }
	),
	{ term__context_init(Context0) },
	mercury_output_term(term__functor(term__atom(PredName), Term, Context0),
		VarSet, no).

:- pred mercury_output_disj(goal, varset, int, io__state, io__state).
:- mode mercury_output_disj(in, in, in, di, uo) is det.

mercury_output_disj(Goal, VarSet, Indent) -->
	mercury_output_newline(Indent),
	io__write_string(";"),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	(
		{ Goal = (A;B) - _Context }
	->
		mercury_output_goal(A, VarSet, Indent1),
		mercury_output_disj(B, VarSet, Indent)
	;
		mercury_output_goal(Goal, VarSet, Indent1)
	).

:- pred mercury_output_some(list(var), varset, io__state, io__state).
:- mode mercury_output_some(in, in, di, uo) is det.

mercury_output_some(Vars, VarSet) -->
	(
		{ Vars = [] }
	->
		[]
	;
		io__write_string(" some ["),
		mercury_output_vars(Vars, VarSet, no),
		io__write_string("]")
	).

%-----------------------------------------------------------------------------%

mercury_output_pragma_c_header(C_HeaderString) -->
	io__write_string(":- pragma c_header_code("),
	term_io__quote_string(C_HeaderString),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

	% Output the given pragma source_file declaration
:- pred mercury_output_pragma_source_file(string, io__state, io__state).
:- mode mercury_output_pragma_source_file(in, di, uo) is det.

mercury_output_pragma_source_file(SourceFileString) -->
	io__write_string(":- pragma source_file("),
	term_io__quote_string(SourceFileString),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

	% Output the given c_body_code declaration
:- pred mercury_output_pragma_c_body_code(string, io__state, io__state).
:- mode mercury_output_pragma_c_body_code(in, di, uo) is det.

mercury_output_pragma_c_body_code(C_CodeString) -->
	io__write_string(":- pragma c_code("),
	term_io__quote_string(C_CodeString),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

	% Output the given pragma c_code declaration
mercury_output_pragma_c_code(MayCallMercury, PredName, PredOrFunc, Vars0,
		MaybeExtraInfo, VarSet, C_CodeString, InstTable) -->
	io__write_string(":- pragma c_code("),
	mercury_output_sym_name(PredName),
	{
		PredOrFunc = predicate,
		Vars = Vars0,
		ResultVars = []
	;
		PredOrFunc = function,
		pred_args_to_func_args(Vars0, Vars, ResultVar),
		ResultVars = [ResultVar]
	},
	( { Vars = [] } ->
		[]
	;
		io__write_string("("),
		mercury_output_pragma_c_code_vars(Vars, VarSet, InstTable),
		io__write_string(")")
	),
	(
		{ PredOrFunc = predicate }
	;
		{ PredOrFunc = function },
		io__write_string(" = ("),
		mercury_output_pragma_c_code_vars(ResultVars, VarSet,
			InstTable),
		io__write_string(")")
	),
	(
		{ MayCallMercury = may_call_mercury },
		io__write_string(", may_call_mercury, ")
	; 
		{ MayCallMercury = will_not_call_mercury },
		io__write_string(", will_not_call_mercury, ")
	),
	(
		{ MaybeExtraInfo = no }
	;
		{ MaybeExtraInfo = yes(SavedVars - LabelNames) },
		mercury_output_c_ident_list(SavedVars),
		io__write_string(", "),
		mercury_output_c_ident_list(LabelNames),
		io__write_string(", ")
	),
	term_io__quote_string(C_CodeString),
	io__write_string(").\n").

:- pred mercury_output_c_ident_list(list(string), io__state, io__state).
:- mode mercury_output_c_ident_list(in, di, uo) is det.

mercury_output_c_ident_list([]) -->
	io__write_string("[]").
mercury_output_c_ident_list([First | Rest]) -->
	io__write_string("["),
	io__write_string(First),
	mercury_output_c_ident_list_2(Rest),
	io__write_string("]").

:- pred mercury_output_c_ident_list_2(list(string), io__state, io__state).
:- mode mercury_output_c_ident_list_2(in, di, uo) is det.

mercury_output_c_ident_list_2([]) --> [].
mercury_output_c_ident_list_2([First | Rest]) -->
	io__write_string(", "),
	io__write_string(First),
	mercury_output_c_ident_list_2(Rest).

%-----------------------------------------------------------------------------%

	% Output the varnames of the pragma vars
:- pred mercury_output_pragma_c_code_vars(list(pragma_var), varset,
		inst_table, io__state, io__state).
:- mode mercury_output_pragma_c_code_vars(in, in, in, di, uo) is det.

mercury_output_pragma_c_code_vars([], _, _) --> [].
mercury_output_pragma_c_code_vars([V|Vars], VarSet, InstTable) -->
	{ V = pragma_var(_Var, VarName, Mode) },
	io__write_string(VarName),
	io__write_string(" :: "),
	mercury_output_mode(Mode, VarSet, InstTable),
	(	{ Vars = [] }
	->
		[]
	;
		io__write_string(", ")
	),
	mercury_output_pragma_c_code_vars(Vars, VarSet, InstTable).


%-----------------------------------------------------------------------------%

mercury_output_pragma_decl(PredName, Arity, PredOrFunc, PragmaName) -->
	{ PredOrFunc = predicate,
		DeclaredArity = Arity
	; PredOrFunc = function,
		DeclaredArity is Arity - 1
	},
	io__write_string(":- pragma "),
	io__write_string(PragmaName),
	io__write_string("(("),
	mercury_output_bracketed_sym_name(PredName),
	io__write_string(")/"),
	io__write_int(DeclaredArity),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

:- pred mercury_output_pragma_export(sym_name, pred_or_func, list(mode),
	string, inst_table, io__state, io__state).
:- mode mercury_output_pragma_export(in, in, in, in, in, di, uo) is det.

mercury_output_pragma_export(Name, PredOrFunc, ModeList, C_Function,
		InstTable) -->
	{ varset__init(Varset) }, % the varset isn't really used.
	io__write_string(":- pragma export("),
	mercury_output_sym_name(Name),
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ModeList, ArgModes, RetMode) },
		io__write_string("("),
		mercury_output_mode_list(ArgModes, Varset, InstTable),
		io__write_string(") = "),
		mercury_output_mode(RetMode, Varset, InstTable)
	;
		{ PredOrFunc = predicate },
		io__write_string("("),
		mercury_output_mode_list(ModeList, Varset, InstTable),
		io__write_string("), ")
	),
	io__write_string(C_Function),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

:- pred mercury_output_pragma_fact_table(sym_name, arity, string,
		io__state, io__state).
:- mode mercury_output_pragma_fact_table(in, in, in, di, uo) is det.

mercury_output_pragma_fact_table(Pred, Arity, FileName) -->
	io__write_string(":- pragma fact_table("),
	mercury_output_sym_name(Pred),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(", "),
	term_io__quote_string(FileName),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

mercury_output_newline(Indent) -->
	io__write_char('\n'),
	mercury_output_tabs(Indent).

:- pred mercury_output_tabs(int, io__state, io__state).
:- mode mercury_output_tabs(in, di, uo) is det.

mercury_output_tabs(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_char('\t'),
		{ Indent1 is Indent - 1 },
		mercury_output_tabs(Indent1)
	).

%-----------------------------------------------------------------------------%

	% write a term to standard output.

mercury_output_term(term__variable(Var), VarSet, AppendVarnums) -->
	mercury_output_var(Var, VarSet, AppendVarnums).
mercury_output_term(term__functor(Functor, Args, _), VarSet, AppendVarnums) -->
	(
	    	{ Functor = term__atom(".") },
		{ Args = [X, Xs] }
	->
		io__write_string("["),
		mercury_output_term(X, VarSet, AppendVarnums),
		mercury_output_list_args(Xs, VarSet, AppendVarnums),
		io__write_string("]")
	;
		{ Args = [PrefixArg] },
		{ Functor = term__atom(FunctorName) },
		{ mercury_unary_prefix_op(FunctorName) }
	->
		io__write_string("("),
		io__write_string(FunctorName),
		io__write_string(" "),
		mercury_output_term(PrefixArg, VarSet, AppendVarnums),
		io__write_string(")")
	;
		{ Args = [PostfixArg] },
		{ Functor = term__atom(FunctorName) },
		{ mercury_unary_postfix_op(FunctorName) }
	->
		io__write_string("("),
		mercury_output_term(PostfixArg, VarSet, AppendVarnums),
		io__write_string(" "),
		io__write_string(FunctorName),
		io__write_string(")")
	;
		{ Args = [Arg1, Arg2] },
		{ Functor = term__atom(FunctorName) },
		{ mercury_infix_op(FunctorName) }
	->
		io__write_string("("),
		mercury_output_term(Arg1, VarSet, AppendVarnums),
		( { FunctorName = ":" } ->
			io__write_string(":")
		;
			io__write_string(" "),
			io__write_string(FunctorName),
			io__write_string(" ")
		),
		mercury_output_term(Arg2, VarSet, AppendVarnums),
		io__write_string(")")
	;
		{ Args = [Y | Ys] }
	->
		term_io__write_constant(Functor),
		io__write_string("("),
		mercury_output_term(Y, VarSet, AppendVarnums),
		mercury_output_remaining_terms(Ys, VarSet, AppendVarnums),
		io__write_string(")")
	;
		mercury_output_bracketed_constant(Functor)
	).

:- pred mercury_output_list_args(term, varset, bool, io__state, io__state).
:- mode mercury_output_list_args(in, in, in, di, uo) is det.

mercury_output_list_args(Term, VarSet, AppendVarnums) -->
	(
	    	{ Term = term__functor(term__atom("."), Args, _) },
		{ Args = [X, Xs] }
	->
		io__write_string(", "),
		mercury_output_term(X, VarSet, AppendVarnums),
		mercury_output_list_args(Xs, VarSet, AppendVarnums)
	;
		{ Term = term__functor(term__atom("[]"), [], _) }
	->
		[]
	;
		io__write_string(" | "),
		mercury_output_term(Term, VarSet, AppendVarnums)
	).

:- pred mercury_output_remaining_terms(list(term), varset, bool,
	io__state, io__state).
:- mode mercury_output_remaining_terms(in, in, in, di, uo) is det.

mercury_output_remaining_terms([], _VarSet, _AppendVarnums) --> [].
mercury_output_remaining_terms([Term | Terms], VarSet, AppendVarnums) -->
	io__write_string(", "),
	mercury_output_term(Term, VarSet, AppendVarnums),
	mercury_output_remaining_terms(Terms, VarSet, AppendVarnums).

	% output a comma-separated list of variables

mercury_output_vars([], _VarSet, _AppendVarnum) --> [].
mercury_output_vars([Var | Vars], VarSet, AppendVarnum) -->
	mercury_output_var(Var, VarSet, AppendVarnum),
	mercury_output_vars_2(Vars, VarSet, AppendVarnum).

:- pred mercury_output_vars_2(list(var), varset, bool, io__state, io__state).
:- mode mercury_output_vars_2(in, in, in, di, uo) is det.

mercury_output_vars_2([], _VarSet, _AppendVarnum) --> [].
mercury_output_vars_2([Var | Vars], VarSet, AppendVarnum) -->
	io__write_string(", "),
	mercury_output_var(Var, VarSet, AppendVarnum),
	mercury_output_vars_2(Vars, VarSet, AppendVarnum).

	% Output a single variable.
	% Variables that didn't have names are given the name "V_<n>"
	% where <n> is there variable id.
	% Variables whose name originally started with `V_' have their
	% name changed to start with `V__' to avoid name clashes.

mercury_output_var(Var, VarSet, AppendVarnum) -->
	(
		{ varset__search_name(VarSet, Var, Name) }
	->
		{ mercury_convert_var_name(Name, ConvertedName) },
		io__write_string(ConvertedName),
		(
			{ AppendVarnum = yes },
			{ term__var_to_int(Var, VarNum) },
			io__write_string("_"),
			io__write_int(VarNum)
		;
			{ AppendVarnum = no }
		)
	;
		{ term__var_to_int(Var, Id) },
		{ string__int_to_string(Id, Num) },
		{ string__append("V_", Num, VarName) },
		io__write_string(VarName)
	).

mercury_output_bracketed_constant(Const) -->
	( { Const = term__atom(Op), mercury_op(Op) } ->
		io__write_string("("),
		term_io__write_constant(Const),
		io__write_string(")")
	;
		term_io__write_constant(Const)
	).

%-----------------------------------------------------------------------------%

	% Predicates to test whether a functor is a Mercury operator

:- pred mercury_op(string).
:- mode mercury_op(in) is semidet.

mercury_op(Op) :-
	(
	    (
		mercury_infix_op(Op)
	    ;
		mercury_binary_prefix_op(Op)
	    ;
		mercury_unary_prefix_op(Op)
	    ;
		mercury_unary_postfix_op(Op)
	    )
	->
		true
	;
		fail
	).

:- pred mercury_binary_prefix_op(string).
:- mode mercury_binary_prefix_op(in) is semidet.

mercury_binary_prefix_op("some").
mercury_binary_prefix_op("all").
mercury_binary_prefix_op("gSome").	/* NU-Prolog */
mercury_binary_prefix_op("gAll").	/* NU-Prolog */
mercury_binary_prefix_op("lambda").

:- pred mercury_infix_op(string).
:- mode mercury_infix_op(in) is semidet.

mercury_infix_op("--->").
mercury_infix_op("-->").
mercury_infix_op(":-").
mercury_infix_op("::").
mercury_infix_op("where").
mercury_infix_op("sorted").	/* NU-Prolog */
mercury_infix_op("else").
mercury_infix_op("then").
mercury_infix_op(";").
mercury_infix_op("->").
mercury_infix_op(",").
mercury_infix_op("&").
mercury_infix_op("to").		/* NU-Prolog */
mercury_infix_op("<=").
mercury_infix_op("<=>").
mercury_infix_op("=>").
mercury_infix_op("when").	/* NU-Prolog */
mercury_infix_op("or").		/* NU-Prolog */
mercury_infix_op("and").	/* NU-Prolog */
mercury_infix_op("=").
mercury_infix_op("=..").
mercury_infix_op("=:=").	/* Prolog */
mercury_infix_op("==").		/* Prolog (also for constraints, in cfloat.m) */
mercury_infix_op("\\=").	/* Prolog */
mercury_infix_op("\\==").	/* Prolog */
mercury_infix_op("=\\=").	/* Prolog */
mercury_infix_op(">").
mercury_infix_op(">=").
mercury_infix_op("<").
mercury_infix_op("=<").
mercury_infix_op("@<").		/* Prolog */
mercury_infix_op("@=<").	/* Prolog */
mercury_infix_op("@>").		/* Prolog */
mercury_infix_op("@>=").	/* Prolog */
mercury_infix_op("~=").		/* NU-Prolog */
mercury_infix_op("is").		
mercury_infix_op(".").		
mercury_infix_op(":").		
mercury_infix_op("+").
mercury_infix_op("-").
mercury_infix_op("/\\").
mercury_infix_op("\\/").
mercury_infix_op("*").
mercury_infix_op("/").
mercury_infix_op("//").
mercury_infix_op(">>").
mercury_infix_op("<<").
mercury_infix_op("**").
mercury_infix_op("div").
mercury_infix_op("mod").
mercury_infix_op("rem").
mercury_infix_op("^").

:- pred mercury_unary_prefix_op(string).
:- mode mercury_unary_prefix_op(in) is semidet.

mercury_unary_prefix_op("+").
mercury_unary_prefix_op("-").
mercury_unary_prefix_op(":-").
mercury_unary_prefix_op("::").
mercury_unary_prefix_op("?-").
mercury_unary_prefix_op("\\").
mercury_unary_prefix_op("\\+").
mercury_unary_prefix_op("delete").
mercury_unary_prefix_op("dynamic").
mercury_unary_prefix_op("end_module").
mercury_unary_prefix_op("func").
mercury_unary_prefix_op("if").
mercury_unary_prefix_op("import_module").
mercury_unary_prefix_op("insert").
mercury_unary_prefix_op("inst").
mercury_unary_prefix_op("lib").
mercury_unary_prefix_op("listing").
mercury_unary_prefix_op("man").
mercury_unary_prefix_op("mode").
mercury_unary_prefix_op("module").
mercury_unary_prefix_op("nospy").
mercury_unary_prefix_op("not").
mercury_unary_prefix_op("once").
mercury_unary_prefix_op("pragma").
mercury_unary_prefix_op("pred").
mercury_unary_prefix_op("pure").
mercury_unary_prefix_op("rule").	/* NU-Prolog */
mercury_unary_prefix_op("sorted").
mercury_unary_prefix_op("spy").
mercury_unary_prefix_op("type").
mercury_unary_prefix_op("update").
mercury_unary_prefix_op("useIf").
mercury_unary_prefix_op("wait").
mercury_unary_prefix_op("~").

:- pred mercury_unary_postfix_op(string).
:- mode mercury_unary_postfix_op(in) is semidet.

mercury_unary_postfix_op("sorted").

%-----------------------------------------------------------------------------%

	% Convert a Mercury variable into a Mercury variable name.  
	% This is tricky because the compiler may introduce new variables
	% who either don't have names at all, or whose names end in
	% some sequence of primes (eg. Var''').
	% We have to be careful that every possible variable
	% is mapped to a distinct name.  Variables without names are
	% given names starting with `V_' followed by a sequence of digits
	% corresponding to their variable id.
	% To ensure that this doesn't clash with any existing names,
	% any variables whose name originally started with `V_' get
	% another `V_' inserted at the start of their name.

	% Compiler's internal name	Converted name
	% ------------------------	--------------
	% none				V_[0-9]*
	% .*'+				V_[0-9]*_.*
	% V_.*				V_V_.*
	% anthing else			same as original name

mercury_convert_var_name(Name, ConvertedName) :-
	( string__remove_suffix(Name, "'", _) ->
		strip_trailing_primes(Name, StrippedName, NumPrimes),
		string__append("V_", StrippedName, Tmp1),
		string__int_to_string(NumPrimes, NumString),
		string__append(Tmp1, "_", Tmp2),
		string__append(Tmp2, NumString, ConvertedName)
	; string__prefix(Name, "V_") ->
		string__append("V_", Name, ConvertedName)
	;
		ConvertedName = Name
	).

:- pred strip_trailing_primes(string, string, int).
:- mode strip_trailing_primes(in, out, out) is det.

	% XXX This implementation is O(N*N), but it ought to be O(N)

strip_trailing_primes(Name0, Name, Num) :-
	( string__remove_suffix(Name0, "'", Name1) ->
		strip_trailing_primes(Name1, Name, Num0),
		Num is Num0 + 1
	;
		Num = 0,
		Name = Name0
	).

%-----------------------------------------------------------------------------%

:- pred maybe_output_line_number(term__context, io__state, io__state).
:- mode maybe_output_line_number(in, di, uo) is det.

maybe_output_line_number(Context) -->
	globals__io_lookup_bool_option(line_numbers, LineNumbers),
	( { LineNumbers = yes } ->
		io__write_string("\t% "),
		prog_out__write_context(Context),
		io__write_string("\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%
