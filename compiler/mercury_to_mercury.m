%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: fjh.

% This program converts the parse tree structure provided by prog_io
% back into Mercury source text.

%-----------------------------------------------------------------------------%

:- module mercury_to_mercury.
:- interface.

:- type needs_brackets
	--->	needs_brackets		% needs brackets, if it is an op
	;	does_not_need_brackets.	% doesn't need brackets

:- type needs_quotes
	--->	next_to_graphic_token		% needs quotes, if it
						% is another graphic token
	;	not_next_to_graphic_token.	% doesn't need quotes

:- import_module hlds_goal, hlds_data, prog_data, (inst).
:- import_module bool, std_util, list, io, varset, term.

%	convert_to_mercury(ModuleName, OutputFileName, Items)
:- pred convert_to_mercury(module_name, string, list(item_and_context),
				io__state, io__state).
:- mode convert_to_mercury(in, in, in, di, uo) is det.

%	mercury_output_item(Item, Context)
%		output the specified item, followed by ".\n"
:- pred mercury_output_item(item, prog_context, io__state, io__state).
:- mode mercury_output_item(in, in, di, uo) is det.

	% Output a `:- pred' declaration, making sure that the variable
	% number appears in variable names if the boolean argument
	% is set to `yes'.
:- pred mercury_output_pred_type(tvarset, existq_tvars, sym_name, list(type),
		maybe(determinism), purity, class_constraints,
		prog_context, bool, io__state, io__state).
:- mode mercury_output_pred_type(in, in, in, in, in, in, in, in, in,
		di, uo) is det.

	% Output a `:- func' declaration, making sure that the variable
	% number appears in variable names if the boolean argument
	% is set to `yes'.
:- pred mercury_output_func_type(tvarset, existq_tvars, sym_name,
		list(type), type,
		maybe(determinism), purity, class_constraints,
		prog_context, bool, io__state, io__state).
:- mode mercury_output_func_type(in, in, in, in, in, in, in, in, in, in,
		di, uo) is det.

:- pred mercury_output_pred_mode_decl(inst_varset, sym_name, list(mode),
		maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_pred_mode_decl(in, in, in, in, in, di, uo) is det.

:- pred mercury_output_func_mode_decl(inst_varset, sym_name, list(mode), mode,
		maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_func_mode_decl(in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_mode_subdecl(pred_or_func, inst_varset, sym_name,
		list(mode), maybe(determinism), prog_context,
		io__state, io__state).
:- mode mercury_output_mode_subdecl(in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_pred_mode_subdecl(inst_varset, sym_name, list(mode),
		maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_pred_mode_subdecl(in, in, in, in, in, di, uo) is det.

:- pred mercury_output_func_mode_subdecl(inst_varset, sym_name, list(mode),
		mode, maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_func_mode_subdecl(in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_pragma_decl(sym_name, int, pred_or_func, string,
		io__state, io__state).
:- mode mercury_output_pragma_decl(in, in, in, in, di, uo) is det.

:- pred mercury_output_pragma_c_code(pragma_foreign_code_attributes, sym_name,
		pred_or_func, list(pragma_var), prog_varset,
		pragma_foreign_code_impl, io__state, io__state).
:- mode mercury_output_pragma_c_code(in, in, in, in, in, in, di, uo) is det.

:- pred mercury_output_pragma_unused_args(pred_or_func, sym_name,
		int, mode_num, list(int), io__state, io__state) is det.
:- mode mercury_output_pragma_unused_args(in, in, in, in, in, di, uo) is det.

	% Write an Aditi index specifier.
:- pred mercury_output_index_spec(index_spec, io__state, io__state). 
:- mode mercury_output_index_spec(in, di, uo) is det.

	% Output the given c_header_code declaration
:- pred mercury_output_pragma_c_header(string, io__state, io__state).
:- mode mercury_output_pragma_c_header(in, di, uo) is det.

:- pred mercury_output_type_defn(tvarset, type_defn, prog_context,
		io__state, io__state).
:- mode mercury_output_type_defn(in, in, in, di, uo) is det.

:- pred mercury_output_ctor(constructor, tvarset, io__state, io__state).
:- mode mercury_output_ctor(in, in, di, uo) is det.

:- pred mercury_output_remaining_ctor_args(tvarset, list(constructor_arg),
				io__state, io__state).
:- mode mercury_output_remaining_ctor_args(in, in, di, uo) is det.

:- pred mercury_output_inst_defn(inst_varset, inst_defn, prog_context,
			io__state, io__state).
:- mode mercury_output_inst_defn(in, in, in, di, uo) is det.

:- pred mercury_output_mode_defn(inst_varset, mode_defn, prog_context,
			io__state, io__state).
:- mode mercury_output_mode_defn(in, in, in, di, uo) is det.

	% Output a list of insts in a format that makes them easy to read
	% but may not be valid Mercury.

:- pred mercury_output_structured_inst_list(list(inst), int, inst_varset,
			io__state, io__state).
:- mode mercury_output_structured_inst_list(in, in, in, di, uo) is det.

:- pred mercury_output_inst_list(list(inst), inst_varset, io__state, io__state).
:- mode mercury_output_inst_list(in, in, di, uo) is det.

	% Output an inst in a format that makes it easy to read
	% but may not be valid Mercury.

:- pred mercury_output_structured_inst(inst, int, inst_varset,
		io__state, io__state).
:- mode mercury_output_structured_inst(in, in, in, di, uo) is det.

:- pred mercury_output_inst(inst, inst_varset, io__state, io__state).
:- mode mercury_output_inst(in, in, di, uo) is det.

:- pred mercury_output_cons_id(cons_id, needs_brackets, io__state, io__state).
:- mode mercury_output_cons_id(in, in, di, uo) is det.

:- pred mercury_output_mode(mode, inst_varset, io__state, io__state).
:- mode mercury_output_mode(in, in, di, uo) is det.

:- pred mercury_output_mode_list(list(mode), inst_varset, io__state, io__state).
:- mode mercury_output_mode_list(in, in, di, uo) is det.

:- pred mercury_output_uni_mode(uni_mode, inst_varset, io__state, io__state).
:- mode mercury_output_uni_mode(in, in, di, uo) is det.

:- pred mercury_output_uni_mode_list(list(uni_mode), inst_varset,
					io__state, io__state).
:- mode mercury_output_uni_mode_list(in, in, di, uo) is det.

:- pred mercury_output_det(determinism, io__state, io__state).
:- mode mercury_output_det(in, di, uo) is det.

	% Output a comma-separated list of variables, making sure that
	% the variable number appears in the variable name if the boolean
	% argument is set to `yes'.

:- pred mercury_output_vars(list(var(T)), varset(T), bool,
		io__state, io__state).
:- mode mercury_output_vars(in, in, in, di, uo) is det.

	% Output a variable, making sure that the variable number appears
	% in the variable name if the boolean argument is set to `yes'.

:- pred mercury_output_var(var(T), varset(T), bool, io__state, io__state).
:- mode mercury_output_var(in, in, in, di, uo) is det.

	% Output a term, making sure that the variable number appears
	% in variable names if the boolean argument is set to `yes'.

:- pred mercury_output_term(term(T), varset(T), bool, io__state, io__state).
:- mode mercury_output_term(in, in, in, di, uo) is det.

:- pred mercury_output_term(term(T), varset(T), bool, needs_quotes,
				io__state, io__state).
:- mode mercury_output_term(in, in, in, in, di, uo) is det.

:- pred mercury_type_to_string(tvarset, type, string).
:- mode mercury_type_to_string(in, in, out) is det.

:- pred mercury_type_list_to_string(tvarset, list(type), string).
:- mode mercury_type_list_to_string(in, in, out) is det.

:- pred mercury_output_newline(int, io__state, io__state).
:- mode mercury_output_newline(in, di, uo) is det.

:- pred mercury_output_bracketed_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_bracketed_sym_name(in, di, uo) is det.

:- pred mercury_convert_var_name(string, string).
:- mode mercury_convert_var_name(in, out) is det.

	% Output a constraint, making sure that the variable number appears
	% in variable names if the boolean argument is set to `yes'.
:- pred mercury_output_constraint(tvarset, bool, class_constraint,
		io__state, io__state).
:- mode mercury_output_constraint(in, in, in, di, uo) is det.

:- pred mercury_constraint_to_string(tvarset, class_constraint, 
		string).
:- mode mercury_constraint_to_string(in, in, out) is det.

	% Output an existential quantifier, making sure that the variable
	% number appears in variable names if the boolean argument
	% is set to `yes'.
:- pred mercury_output_quantifier(tvarset, bool, existq_tvars,
		io__state, io__state).
:- mode mercury_output_quantifier(in, in, in, di, uo) is det.

:- pred mercury_output_instance_methods(instance_methods, io__state,
	io__state).
:- mode mercury_output_instance_methods(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_out, prog_util, hlds_pred, hlds_out, instmap.
:- import_module purity, term_util.
:- import_module globals, options, termination.

:- import_module assoc_list, char, int, string, set, lexer, require.
:- import_module term, term_io, varset.

%-----------------------------------------------------------------------------%

convert_to_mercury(ModuleName, OutputFileName, Items) -->
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
		mercury_output_bracketed_sym_name(ModuleName),
		io__write_string(".\n"),
		mercury_output_item_list(Items),
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

:- pred mercury_output_item_list(list(item_and_context), io__state, io__state).
:- mode mercury_output_item_list(in, di, uo) is det.

mercury_output_item_list([]) --> [].
mercury_output_item_list([Item - Context | Items]) -->
	mercury_output_item(Item, Context),
	mercury_output_item_list(Items).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

mercury_output_item(type_defn(VarSet, TypeDefn, _Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_type_defn(VarSet, TypeDefn, Context).

mercury_output_item(inst_defn(VarSet, InstDefn, _Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_inst_defn(VarSet, InstDefn, Context).

mercury_output_item(mode_defn(VarSet, ModeDefn, _Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_mode_defn(VarSet, ModeDefn, Context).

mercury_output_item(pred(TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, Det, _Cond, Purity, ClassContext), Context) -->
	maybe_output_line_number(Context),
	mercury_output_pred_decl(TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, Det, Purity, ClassContext, Context,
		":- ", ".\n", ".\n").

mercury_output_item(func(TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, RetTypeAndMode, Det, _Cond, Purity,
		ClassContext), Context) -->
	maybe_output_line_number(Context),
	mercury_output_func_decl(TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, RetTypeAndMode, Det, Purity, ClassContext,
		Context, ":- ", ".\n", ".\n").

mercury_output_item(pred_mode(VarSet, PredName, Modes, MaybeDet, _Cond),
			Context) -->
	maybe_output_line_number(Context),
	mercury_output_pred_mode_decl(VarSet, PredName, Modes, MaybeDet,
			Context).

mercury_output_item(func_mode(VarSet, PredName, Modes, RetMode, MaybeDet,
		_Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_func_mode_decl(VarSet, PredName, Modes, RetMode,
			MaybeDet, Context).

mercury_output_item(module_defn(VarSet, ModuleDefn), Context) -->
	maybe_output_line_number(Context),
	mercury_output_module_defn(VarSet, ModuleDefn, Context).

mercury_output_item(pred_clause(VarSet, PredName, Args, Body), Context) -->
	maybe_output_line_number(Context),
	mercury_output_pred_clause(VarSet, PredName, Args, Body, Context),
	io__write_string(".\n").

mercury_output_item(func_clause(VarSet, FuncName, Args, Result, Body),
		Context) -->
	maybe_output_line_number(Context),
	mercury_output_func_clause(VarSet, FuncName, Args, Result, Body,
		Context),
	io__write_string(".\n").

mercury_output_item(pragma(Pragma), Context) -->
	maybe_output_line_number(Context),
	(
		{ Pragma = source_file(SourceFile) },
		mercury_output_pragma_source_file(SourceFile)
	;
		{ Pragma = c_header_code(C_HeaderString) },
		mercury_output_pragma_c_header(C_HeaderString)
	;
		{ Pragma = foreign(_Lang, Code) }, 
		% XXX if it is C code only
		mercury_output_pragma_c_body_code(Code)
	;
		{ Pragma = foreign(_Lang, Attributes, Pred, PredOrFunc, Vars,
			VarSet, PragmaCode) }, 
		% XXX if it is C code only
		mercury_output_pragma_c_code(Attributes, Pred, PredOrFunc, 
			Vars, VarSet, PragmaCode)
	;
		{ Pragma = import(Pred, PredOrFunc, ModeList, Attributes,
			C_Function) },
		mercury_output_pragma_import(Pred, PredOrFunc, ModeList,
			Attributes, C_Function)
	;
		{ Pragma = export(Pred, PredOrFunc, ModeList, C_Function) },
		mercury_output_pragma_export(Pred, PredOrFunc, ModeList,
			C_Function)
	;
		{ Pragma = obsolete(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "obsolete")
	;
		{ Pragma = tabled(Type, Pred, Arity, _PredOrFunc, _Mode) },
		{ eval_method_to_string(Type, TypeS) },
		mercury_output_pragma_decl(Pred, Arity, predicate, TypeS)
	;
		{ Pragma = type_spec(PredName, SymName, Arity,
			MaybePredOrFunc, MaybeModes, Subst, VarSet) },
		mercury_output_pragma_type_spec(PredName, SymName, Arity,
			MaybePredOrFunc, MaybeModes, Subst, VarSet)
	;
		{ Pragma = inline(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "inline")
	;
		{ Pragma = no_inline(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "no_inline")
	;
		{ Pragma = unused_args(PredOrFunc, PredName,
			Arity, ModeNum, UnusedArgs) },
		mercury_output_pragma_unused_args(PredOrFunc,
			PredName, Arity, ModeNum, UnusedArgs)
	;
		{ Pragma = fact_table(Pred, Arity, FileName) },
		mercury_output_pragma_fact_table(Pred, Arity, FileName)
	;
		{ Pragma = aditi(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "aditi")
	;
		{ Pragma = base_relation(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, 
			predicate, "base_relation")
	;
		{ Pragma = aditi_index(Pred, Arity, Index) },
		mercury_output_pragma_index(Pred, Arity, Index)
	;
		{ Pragma = aditi_memo(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity,
			predicate, "aditi_memo")
	;
		{ Pragma = aditi_no_memo(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity,
			predicate, "aditi_no_memo")
	;
		{ Pragma = supp_magic(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, 
			predicate, "supp_magic")
	;
		{ Pragma = context(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, 
			predicate, "context")
	;
		{ Pragma = owner(Pred, Arity, Owner) },
		mercury_output_pragma_owner(Pred, Arity, Owner)
	;
		{ Pragma = naive(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "naive")
	;
		{ Pragma = psn(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "psn")
	;
		{ Pragma = promise_pure(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate,
					   "promise_pure")
	;
		{ Pragma = termination_info(PredOrFunc, PredName, 
			ModeList, MaybePragmaArgSizeInfo,
			MaybePragmaTerminationInfo) },
		{ add_context_to_arg_size_info(MaybePragmaArgSizeInfo, Context,
			MaybeArgSizeInfo) },
		{ add_context_to_termination_info(MaybePragmaTerminationInfo,
			Context, MaybeTerminationInfo) },
		termination__write_pragma_termination_info(PredOrFunc,
			PredName, ModeList, Context,
			MaybeArgSizeInfo, MaybeTerminationInfo)
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

mercury_output_item(assertion(Goal, VarSet), _) -->
	io__write_string(":- promise "),
	{ Indent = 1 },
	mercury_output_newline(Indent),
	mercury_output_goal(Goal, VarSet, Indent),
	io__write_string(".\n").

mercury_output_item(nothing, _) --> [].
mercury_output_item(typeclass(Constraints, ClassName, Vars, Methods, 
		VarSet), _) --> 
	io__write_string(":- typeclass "),

		% We put an extra set of brackets around the class name in
		% case the name is an operator
	mercury_output_sym_name(ClassName),
	io__write_char('('),
	io__write_list(Vars, ", ", 
			lambda([V::in, IO0::di, IO::uo] is det,
				(
				varset__lookup_name(VarSet, V, VarName),
				io__write_string(VarName, IO0, IO)
				)
			)
		),
	io__write_char(')'),

	{ AppendVarnums = no },
	mercury_output_class_constraint_list(Constraints, VarSet, "<=",
		AppendVarnums),

	io__write_string(" where [\n"),

	output_class_methods(Methods),
	
	io__write_string("\n].\n").
mercury_output_item(instance(Constraints, ClassName, Types, Body, 
		VarSet), _) --> 
	io__write_string(":- instance "),

		% We put an extra set of brackets around the class name in
		% case the name is an operator
	io__write_char('('),
	mercury_output_sym_name(ClassName),
	io__write_char('('),
	io__write_list(Types, ", ", term_io__write_term(VarSet)),
	io__write_char(')'),
	io__write_char(')'),
	
	{ AppendVarnums = no },
	mercury_output_class_constraint_list(Constraints, VarSet, "<=",
		AppendVarnums),

	(
		{ Body = abstract }
	;
		{ Body = concrete(Methods) },
		io__write_string(" where [\n"),
		mercury_output_instance_methods(Methods),
		io__write_string("\n]")
	),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%

:- pred output_class_methods(list(class_method), io__state, io__state).
:- mode output_class_methods(in, di, uo) is det.

output_class_methods(Methods) -->
	io__write_list(Methods, ",\n", output_class_method).

:- pred output_class_method(class_method, io__state, io__state).
:- mode output_class_method(in, di, uo) is det.

output_class_method(Method) -->
	io__write_string("\t"),
	(
		{ Method = pred(TypeVarSet, InstVarSet, ExistQVars, Name,
			TypesAndModes, Detism, _Condition, Purity, ClassContext,
			Context) },
		mercury_output_pred_decl(TypeVarSet, InstVarSet, ExistQVars,
			Name, TypesAndModes, Detism, Purity, ClassContext,
			Context, "", ",\n\t", "")
	;
		{ Method = func(TypeVarSet, InstVarSet, ExistQVars, Name,
			TypesAndModes, TypeAndMode, Detism, _Condition,
			Purity, ClassContext, Context) },
		mercury_output_func_decl(TypeVarSet, InstVarSet, ExistQVars,
			Name, TypesAndModes, TypeAndMode, Detism, Purity,
			ClassContext, Context, "", ",\n\t", "")
	;
		{ Method = pred_mode(VarSet, Name, Modes, Detism, 
			_Condition, Context) },
		mercury_output_pred_mode_decl_2(VarSet, Name, Modes, Detism,
			Context, "", "")
	;
		{ Method = func_mode(VarSet, Name, Modes, Mode, 
			Detism, _Condition, Context) },
		mercury_output_func_mode_decl_2(VarSet, Name, Modes, 
			Mode, Detism, Context, "", "")
	).

mercury_output_instance_methods(Methods) -->
	io__write_list(Methods, ",\n", output_instance_method).

:- pred output_instance_method(instance_method, io__state, io__state).
:- mode output_instance_method(in, di, uo) is det.

output_instance_method(Method) -->
	{ Method = instance_method(PredOrFunc, Name1, Defn, Arity, Context) },
	(
		{ Defn = name(Name2) },
		io__write_char('\t'),
		(
			{ PredOrFunc = function },
			io__write_string("func(")
		;
			{ PredOrFunc = predicate },
			io__write_string("pred(")
		),
		mercury_output_bracketed_sym_name(Name1, next_to_graphic_token),
		io__write_string("/"),
		io__write_int(Arity),
		io__write_string(") is "),
		mercury_output_bracketed_sym_name(Name2)
	;
		{ Defn = clauses(ItemList) },
		% XXX should we output the term contexts?
		io__write_string("\t("),
		(	
			{ PredOrFunc = predicate },
			{ WriteOneItem = (pred(Item::in, di, uo) is det -->
				(
					{ Item = pred_clause(VarSet, _PredName,
						HeadTerms, Body) }
				->
					mercury_output_pred_clause(VarSet,
						Name1, HeadTerms, Body,
						Context)
				;
					{ error("invalid instance item") }
				)) },
			io__write_list(ItemList, "),\n\t(", WriteOneItem)
		;
			{ PredOrFunc = function },
			{ WriteOneItem = (pred(Item::in, di, uo) is det -->
				(
					{ Item = func_clause(VarSet, _PredName,
						ArgTerms, ResultTerm, Body) }
				->
					mercury_output_func_clause(VarSet,
						Name1, ArgTerms, ResultTerm,
						Body, Context)
				;
					{ error("invalid instance item") }
				)) },
			io__write_list(ItemList, "),\n\t(", WriteOneItem)
		),
		io__write_string(")")
	).

%-----------------------------------------------------------------------------%

:- pred mercury_output_module_defn(prog_varset, module_defn, term__context,
			io__state, io__state).
:- mode mercury_output_module_defn(in, in, in, di, uo) is det.

mercury_output_module_defn(_VarSet, ModuleDefn, _Context) -->
	( { ModuleDefn = import(module(ImportedModules)) } ->
		io__write_string(":- import_module "),
		mercury_write_module_spec_list(ImportedModules),
		io__write_string(".\n")
	; { ModuleDefn = use(module(UsedModules)) } ->
		io__write_string(":- use_module "),
		mercury_write_module_spec_list(UsedModules),
		io__write_string(".\n")
	; { ModuleDefn = interface } ->
		io__write_string(":- interface.\n")
	; { ModuleDefn = implementation } ->
		io__write_string(":- implementation.\n")
	; { ModuleDefn = include_module(IncludedModules) } ->
		io__write_string(":- include_module "),
		mercury_write_module_spec_list(IncludedModules),
		io__write_string(".\n")
	; { ModuleDefn = module(Module) } ->
		io__write_string(":- module "),
		mercury_output_bracketed_sym_name(Module),
		io__write_string(".\n")
	; { ModuleDefn = end_module(Module) } ->
		io__write_string(":- end_module "),
		mercury_output_bracketed_sym_name(Module),
		io__write_string(".\n")
	;
		% XXX unimplemented
		io__write_string("% unimplemented module declaration\n")
	).

:- pred mercury_write_module_spec_list(list(module_specifier),
					io__state, io__state).
:- mode mercury_write_module_spec_list(in, di, uo) is det.

mercury_write_module_spec_list([]) --> [].
mercury_write_module_spec_list([ModuleName | ModuleNames]) -->
	mercury_output_bracketed_sym_name(ModuleName),
	( { ModuleNames = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_write_module_spec_list(ModuleNames)
	).

mercury_output_inst_defn(VarSet, abstract_inst(Name, Args), Context) -->
	io__write_string(":- inst ("),
	{ construct_qualified_term(Name, Args, Context, InstTerm) },
	mercury_output_term(InstTerm, VarSet, no),
	io__write_string(").\n").
mercury_output_inst_defn(VarSet, eqv_inst(Name, Args, Body), Context) -->
	io__write_string(":- inst ("),
	{ construct_qualified_term(Name, Args, Context, InstTerm) },
	mercury_output_term(InstTerm, VarSet, no),
	io__write_string(") = "),
	mercury_output_inst(Body, VarSet),
	io__write_string(".\n").

mercury_output_structured_inst_list([], _, _) --> [].
mercury_output_structured_inst_list([Inst | Insts], Indent0, VarSet) -->
	mercury_output_structured_inst(Inst, Indent0, VarSet),
	mercury_output_structured_inst_list(Insts, Indent0, VarSet).

mercury_output_inst_list([], _) --> [].
mercury_output_inst_list([Inst | Insts], VarSet) -->
	mercury_output_inst(Inst, VarSet),
	( { Insts = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_inst_list(Insts, VarSet)
	).

mercury_output_structured_inst(any(Uniq), Indent, _) -->
	mercury_output_tabs(Indent),
	mercury_output_any_uniqueness(Uniq),
	io__write_string("\n").
mercury_output_structured_inst(free, Indent, _) -->
	mercury_output_tabs(Indent),
	io__write_string("free\n").
mercury_output_structured_inst(free(_T), Indent, _) -->
	mercury_output_tabs(Indent),
	io__write_string("free(with some type)\n").
mercury_output_structured_inst(bound(Uniq, BoundInsts), Indent, VarSet) -->
	mercury_output_tabs(Indent),
	mercury_output_uniqueness(Uniq, "bound"),
	io__write_string("(\n"),
	mercury_output_structured_bound_insts(BoundInsts, Indent, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst(ground(Uniq, MaybePredInfo), Indent, VarSet)
		-->
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
		(
			{ PredOrFunc = predicate },
			( { Modes = [] } ->
				io__write_string("(pred) is "),
				mercury_output_det(Det),
				io__write_string(")\n")
			;
				io__write_string("(pred("),
				mercury_output_mode_list(Modes, VarSet),
				io__write_string(") is "),
				mercury_output_det(Det),
				io__write_string(")\n")
			)
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
			( { Modes = [] } ->
				io__write_string("((func) = ")
			;
				io__write_string("(func("),
				mercury_output_mode_list(ArgModes, VarSet),
				io__write_string(") = ")
			),
			mercury_output_mode(RetMode, VarSet),
			io__write_string(" is "),
			mercury_output_det(Det),
			io__write_string(")\n")
		)
	;
		mercury_output_uniqueness(Uniq, "ground"),
		io__write_string("\n")
	).
mercury_output_structured_inst(inst_var(Var), Indent, VarSet) -->
	mercury_output_tabs(Indent),
	mercury_output_var(Var, VarSet, no),
	io__write_string("\n").
mercury_output_structured_inst(abstract_inst(Name, Args), Indent, VarSet) -->
	mercury_output_structured_inst_name(user_inst(Name, Args), Indent,
		VarSet).
mercury_output_structured_inst(defined_inst(InstName), Indent, VarSet) -->
	mercury_output_structured_inst_name(InstName, Indent, VarSet).
mercury_output_structured_inst(not_reached, Indent, _) -->
	mercury_output_tabs(Indent),
	io__write_string("not_reached\n").

mercury_output_inst(any(Uniq), _) -->
	mercury_output_any_uniqueness(Uniq).
mercury_output_inst(free, _) -->
	io__write_string("free").
mercury_output_inst(free(_T), _) -->
	io__write_string("free(with some type)").
mercury_output_inst(bound(Uniq, BoundInsts), VarSet) -->
	mercury_output_uniqueness(Uniq, "bound"),
	io__write_string("("),
	mercury_output_bound_insts(BoundInsts, VarSet),
	io__write_string(")").
mercury_output_inst(ground(Uniq, MaybePredInfo), VarSet) -->
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
		(
			{ PredOrFunc = predicate },
			( { Modes = [] } ->
				io__write_string("((pred) is "),
				mercury_output_det(Det),
				io__write_string(")")
			;
				io__write_string("(pred("),
				mercury_output_mode_list(Modes, VarSet),
				io__write_string(") is "),
				mercury_output_det(Det),
				io__write_string(")")
			)
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
			( { ArgModes = [] } ->
				io__write_string("((func)")
			;
				io__write_string("(func("),
				mercury_output_mode_list(ArgModes, VarSet),
				io__write_string(")")
			),
			io__write_string(" = "),
			mercury_output_mode(RetMode, VarSet),
			io__write_string(" is "),
			mercury_output_det(Det),
			io__write_string(")")
		)
	;
		mercury_output_uniqueness(Uniq, "ground")
	).
mercury_output_inst(inst_var(Var), VarSet) -->
	mercury_output_var(Var, VarSet, no).
mercury_output_inst(abstract_inst(Name, Args), VarSet) -->
	mercury_output_inst_name(user_inst(Name, Args), VarSet).
mercury_output_inst(defined_inst(InstName), VarSet) -->
	mercury_output_inst_name(InstName, VarSet).
mercury_output_inst(not_reached, _) -->
	io__write_string("not_reached").

:- pred mercury_output_structured_inst_name(inst_name, int, inst_varset,
	io__state, io__state).
:- mode mercury_output_structured_inst_name(in, in, in, di, uo) is det.

mercury_output_structured_inst_name(user_inst(Name, Args), Indent, VarSet) -->
	( { Args = [] } ->
		mercury_output_tabs(Indent),
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_tabs(Indent),
		mercury_output_sym_name(Name),
		io__write_string("(\n"),
		{ Indent1 is Indent + 1 },
		mercury_output_structured_inst_list(Args, Indent1, VarSet),
		mercury_output_tabs(Indent),
		io__write_string(")\n")
	).
mercury_output_structured_inst_name(merge_inst(InstA, InstB), Indent, VarSet)
		-->
	mercury_output_tabs(Indent),
	io__write_string("$merge_inst(\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_list([InstA, InstB], Indent1, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(shared_inst(InstName), Indent, VarSet) -->
	mercury_output_tabs(Indent),
	io__write_string("$shared_inst(\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(InstName, Indent1, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(mostly_uniq_inst(InstName), Indent, VarSet)
		-->
	mercury_output_tabs(Indent),
	io__write_string("$mostly_uniq_inst(\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(InstName, Indent1, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(unify_inst(Liveness, InstA, InstB, Real),
		Indent, VarSet) -->
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
	mercury_output_structured_inst_list([InstA, InstB], Indent1, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(ground_inst(InstName, IsLive, Uniq, Real),
		Indent, VarSet) -->
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
	mercury_output_structured_inst_name(InstName, Indent1, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(any_inst(InstName, IsLive, Uniq, Real),
		Indent, VarSet) -->
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
	mercury_output_structured_inst_name(InstName, Indent1, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").
mercury_output_structured_inst_name(typed_ground(Uniqueness, Type),
		Indent, _VarSet) -->
	mercury_output_tabs(Indent),
	io__write_string("$typed_ground("),
	mercury_output_uniqueness(Uniqueness, "shared"),
	io__write_string(", "),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(")\n").
mercury_output_structured_inst_name(typed_inst(Type, InstName),
		Indent, VarSet) -->
	mercury_output_tabs(Indent),
	io__write_string("$typed_inst("),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(",\n"),
	{ Indent1 is Indent + 1 },
	mercury_output_structured_inst_name(InstName, Indent1, VarSet),
	mercury_output_tabs(Indent),
	io__write_string(")\n").

:- pred mercury_output_inst_name(inst_name, inst_varset, io__state, io__state).
:- mode mercury_output_inst_name(in, in, di, uo) is det.

mercury_output_inst_name(user_inst(Name, Args), VarSet) -->
	( { Args = [] } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_inst_list(Args, VarSet),
		io__write_string(")")
	).
mercury_output_inst_name(merge_inst(InstA, InstB), VarSet) -->
	io__write_string("$merge_inst("),
	mercury_output_inst_list([InstA, InstB], VarSet),
	io__write_string(")").
mercury_output_inst_name(shared_inst(InstName), VarSet) -->
	io__write_string("$shared_inst("),
	mercury_output_inst_name(InstName, VarSet),
	io__write_string(")").
mercury_output_inst_name(mostly_uniq_inst(InstName), VarSet) -->
	io__write_string("$mostly_uniq_inst("),
	mercury_output_inst_name(InstName, VarSet),
	io__write_string(")").
mercury_output_inst_name(unify_inst(Liveness, InstA, InstB, Real), VarSet) -->
	io__write_string("$unify("),
	( { Liveness = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	mercury_output_inst_list([InstA, InstB], VarSet),
	( { Real = real_unify } ->
		io__write_string(", real")
	;
		io__write_string(", fake")
	),
	io__write_string(")").
mercury_output_inst_name(ground_inst(InstName, IsLive, Uniq, Real), VarSet) -->
	io__write_string("$ground("),
	mercury_output_inst_name(InstName, VarSet),
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
mercury_output_inst_name(any_inst(InstName, IsLive, Uniq, Real), VarSet) -->
	io__write_string("$any("),
	mercury_output_inst_name(InstName, VarSet),
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
mercury_output_inst_name(typed_ground(Uniqueness, Type), _VarSet) -->
	io__write_string("$typed_ground("),
	mercury_output_uniqueness(Uniqueness, "shared"),
	io__write_string(", "),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(")").
mercury_output_inst_name(typed_inst(Type, InstName), VarSet) -->
	io__write_string("$typed_inst("),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(", "),
	mercury_output_inst_name(InstName, VarSet),
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

:- pred mercury_output_structured_bound_insts(list(bound_inst), int,
		inst_varset, io__state, io__state).
:- mode mercury_output_structured_bound_insts(in, in, in, di, uo) is det.

mercury_output_structured_bound_insts([], _, _) --> [].
mercury_output_structured_bound_insts([functor(ConsId, Args) | BoundInsts],
		Indent0, VarSet) -->
	{ Indent1 is Indent0 + 1 },
	{ Indent2 is Indent1 + 1 },
	( { Args = [] } ->
		mercury_output_tabs(Indent1),
		mercury_output_cons_id(ConsId, needs_brackets),
		io__write_string("\n")
	;
		mercury_output_tabs(Indent1),
		mercury_output_cons_id(ConsId, does_not_need_brackets),
		io__write_string("(\n"),
		mercury_output_structured_inst_list(Args, Indent2, VarSet),
		mercury_output_tabs(Indent1),
		io__write_string(")\n")
	),
	( { BoundInsts = [] } ->
		[]
	;
		mercury_output_tabs(Indent0),
		io__write_string(";\n"),
		mercury_output_structured_bound_insts(BoundInsts, Indent0,
			VarSet)
	).

:- pred mercury_output_bound_insts(list(bound_inst), inst_varset, io__state,
		io__state).
:- mode mercury_output_bound_insts(in, in, di, uo) is det.

mercury_output_bound_insts([], _) --> [].
mercury_output_bound_insts([functor(ConsId, Args) | BoundInsts], VarSet) -->
	( { Args = [] } ->
		mercury_output_cons_id(ConsId, needs_brackets)
	;
		mercury_output_cons_id(ConsId, does_not_need_brackets),
		io__write_string("("),
		mercury_output_inst_list(Args, VarSet),
		io__write_string(")")
	),
	( { BoundInsts = [] } ->
		[]
	;
		io__write_string(" ; "),
		mercury_output_bound_insts(BoundInsts, VarSet)
	).

mercury_output_cons_id(cons(Name, _), NeedsBrackets) -->
	( { NeedsBrackets = needs_brackets } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name)
	).
mercury_output_cons_id(int_const(X), _) -->
	io__write_int(X).
mercury_output_cons_id(float_const(X), _) -->
	io__write_float(X).
mercury_output_cons_id(string_const(X), _) -->
	term_io__quote_string(X).
mercury_output_cons_id(pred_const(PredId, ProcId, EvalMethod), _) -->
	% XXX Sufficient, but probably should print this out in
	%     name/arity form.

	{ pred_id_to_int(PredId, PredInt) },
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_string("<pred_const("),
	io__write_int(PredInt),
	io__write_string(", "),
	io__write_int(ProcInt),
	io__write_string(", "),
	io__write(EvalMethod),
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
mercury_output_cons_id(type_ctor_info_const(Module, Type, Arity), _) -->
	{ prog_out__sym_name_to_string(Module, ModuleString) },
	{ string__int_to_string(Arity, ArityString) },
	io__write_strings(["<type_ctor_info for ",
		ModuleString, ":", Type, "/", ArityString, ">"]).
mercury_output_cons_id(base_typeclass_info_const(Module, Class, InstanceNum,
		InstanceString), _) -->
	{ prog_out__sym_name_to_string(Module, ModuleString) },
	io__write_string("<base_typeclass_info for "),
	io__write(Class),
	( { ModuleString \= "some bogus module name" } ->
		io__write_strings([" from module ", ModuleString])
	;
		[]
	),
	io__format(", instance number %d (%s)>",
		[i(InstanceNum), s(InstanceString)]).
mercury_output_cons_id(tabling_pointer_const(_, _), _) -->
	io__write_string("<tabling pointer>").

mercury_output_mode_defn(VarSet, eqv_mode(Name, Args, Mode), Context) -->
	io__write_string(":- mode ("),
	{ construct_qualified_term(Name, Args, Context, ModeTerm) },
	mercury_output_term(ModeTerm, VarSet, no),
	io__write_string(") :: "),
	mercury_output_mode(Mode, VarSet),
	io__write_string(".\n").

mercury_output_mode_list([], _VarSet) --> [].
mercury_output_mode_list([Mode | Modes], VarSet) -->
	mercury_output_mode(Mode, VarSet),
	( { Modes = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_mode_list(Modes, VarSet)
	).

mercury_output_uni_mode_list([], _VarSet) --> [].
mercury_output_uni_mode_list([Mode | Modes], VarSet) -->
	mercury_output_uni_mode(Mode, VarSet),
	( { Modes = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_uni_mode_list(Modes, VarSet)
	).

mercury_output_uni_mode((InstA1 - InstB1 -> InstA2 - InstB2), VarSet) -->
	mercury_output_mode((InstA1 -> InstA2), VarSet),
	io__write_string(" = "),
	mercury_output_mode((InstB1 -> InstB2), VarSet).

mercury_output_mode((InstA -> InstB), VarSet) -->
	( 
	    %
	    % check for higher-order pred or func modes, and output them
	    % in a nice format
	    %
	    { InstA = ground(_Uniq,
			yes(pred_inst_info(_PredOrFunc, _Modes, _Det))) },
	    { InstB = InstA }
	->
	    mercury_output_inst(InstA, VarSet)
	;
	    io__write_string("("),
	    mercury_output_inst(InstA, VarSet),
	    io__write_string(" -> "),
	    mercury_output_inst(InstB, VarSet),
	    io__write_string(")")
	).
mercury_output_mode(user_defined_mode(Name, Args), VarSet) -->
	( { Args = [] } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_inst_list(Args, VarSet),
		io__write_string(")")
	).

%-----------------------------------------------------------------------------%

mercury_output_type_defn(VarSet, TypeDefn, Context) -->
	mercury_output_type_defn_2(TypeDefn, VarSet, Context).

:- pred mercury_output_type_defn_2(type_defn, tvarset, prog_context,
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
	mercury_output_term(TypeTerm, VarSet, no, next_to_graphic_token),
	io__write_string(".\n").

mercury_output_type_defn_2(eqv_type(Name, Args, Body), VarSet, Context) -->
	io__write_string(":- type "),
	{ construct_qualified_term(Name, Args, Context, TypeTerm) },
	mercury_output_term(TypeTerm, VarSet, no),
	io__write_string(" == "),
	mercury_output_term(Body, VarSet, no, next_to_graphic_token),
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

:- pred mercury_output_ctors(list(constructor), tvarset,
				io__state, io__state).
:- mode mercury_output_ctors(in, in, di, uo) is det.

mercury_output_ctors([], _) --> [].
mercury_output_ctors([Ctor | Ctors], VarSet) -->
	mercury_output_ctor(Ctor, VarSet),
	( { Ctors \= [] } ->
		io__write_string("\n\t;\t")
	;	
		[]
	),
	mercury_output_ctors(Ctors, VarSet).

mercury_output_ctor(Ctor, VarSet) -->
	{ Ctor = ctor(ExistQVars, Constraints, Name, Args) },

	{ AppendVarnums = no },
	mercury_output_quantifier(VarSet, AppendVarnums, ExistQVars),

	(
		{ ExistQVars = [] }
	->
		[]
	;
		io__write_string("(")
	),

	% we need to quote ';'/2, '{}'/2, '=>'/2, and 'some'/2
	{ list__length(Args, Arity) },
	(
		{ Arity = 2 },
		{ Name = unqualified(";")
		; Name = unqualified("{}")
		; Name = unqualified("some")
		; Name = unqualified("=>")
		}
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
		{ Name = unqualified(";")
		; Name = unqualified("{}")
		; Name = unqualified("some")
		; Name = unqualified("=>")
		}
	->
		io__write_string(" }")
	;
		[]
	),

	{ AppendVarnums = no },
	mercury_output_class_constraint_list(Constraints, VarSet, "=>",
		AppendVarnums),
	(
		{ ExistQVars = [] }
	->
		[]
	;
		io__write_string(")")
	).

:- pred mercury_output_ctor_arg(tvarset, constructor_arg, io__state, io__state).
:- mode mercury_output_ctor_arg(in, in, di, uo) is det.

mercury_output_ctor_arg(Varset, N - T) -->
	mercury_output_ctor_arg_name_prefix(N),
	mercury_output_term(T, Varset, no).

mercury_output_remaining_ctor_args(_Varset, []) --> [].
mercury_output_remaining_ctor_args(Varset, [N - T | As]) -->
	io__write_string(", "),
	mercury_output_ctor_arg_name_prefix(N),
	mercury_output_term(T, Varset, no),
        mercury_output_remaining_ctor_args(Varset, As).

:- pred mercury_output_ctor_arg_name_prefix(maybe(ctor_field_name),
		io__state, io__state).
:- mode mercury_output_ctor_arg_name_prefix(in, di, uo) is det.

mercury_output_ctor_arg_name_prefix(no) --> [].
mercury_output_ctor_arg_name_prefix(yes(Name)) -->
	mercury_output_bracketed_sym_name(Name),
	io__write_string(" :: ").

%-----------------------------------------------------------------------------%

:- pred mercury_output_pred_decl(tvarset, inst_varset, existq_tvars,
		sym_name, list(type_and_mode),
		maybe(determinism), purity, class_constraints,
		prog_context, string, string, string, io__state, io__state).
:- mode mercury_output_pred_decl(in, in, in, in, in, in, in, in, in, in, in, in,
		di, uo) is det.

mercury_output_pred_decl(TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, MaybeDet, Purity, ClassContext, Context,
		StartString, Separator, Terminator) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	(
		{ MaybeModes = yes(Modes) },
		{ Modes \= [] }
	->
		{ AppendVarnums = no },
		mercury_output_pred_type_2(TypeVarSet, ExistQVars, PredName,
			Types, MaybeDet, Purity, ClassContext, Context,
			AppendVarnums, StartString, Separator),
		mercury_output_pred_mode_decl_2(InstVarSet, PredName, Modes,
				MaybeDet, Context, StartString, Terminator)
	;
		{ AppendVarnums = no },
		mercury_output_pred_type_2(TypeVarSet, ExistQVars, PredName,
			Types, MaybeDet, Purity, ClassContext, Context,
			AppendVarnums, StartString, Terminator)
	).

mercury_output_pred_type(VarSet, ExistQVars, PredName, Types, MaybeDet, Purity,
		ClassContext, Context, AppendVarnums) -->
	mercury_output_pred_type_2(VarSet, ExistQVars, PredName, Types,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums,
		":- ", ".\n").


:- pred mercury_output_pred_type_2(tvarset, existq_tvars, sym_name, list(type),
		maybe(determinism), purity, class_constraints,
		prog_context, bool, string, string, io__state, io__state).
:- mode mercury_output_pred_type_2(in, in, in, in, in, in, in, in, in, in, in,
		di, uo) is det.

mercury_output_pred_type_2(VarSet, ExistQVars, PredName, Types, MaybeDet,
		Purity, ClassContext, _Context, AppendVarnums,
		StartString, Separator) -->
	io__write_string(StartString),
	mercury_output_quantifier(VarSet, AppendVarnums, ExistQVars),
	( { ExistQVars = [], ClassContext = constraints(_, []) } -> 
		[] 
	; 
		io__write_string("(")
	),
	write_purity_prefix(Purity),
	io__write_string("pred "),
	(
		{ Types = [Type | Rest] }
	->
		mercury_output_sym_name(PredName),
		io__write_string("("),
		mercury_output_term(Type, VarSet, AppendVarnums),
		mercury_output_remaining_terms(Rest, VarSet, AppendVarnums),
		io__write_string(")"),
		mercury_output_class_context(ClassContext, ExistQVars, VarSet,
			AppendVarnums)
	;
		mercury_output_bracketed_sym_name(PredName),
		mercury_output_class_context(ClassContext, ExistQVars, VarSet,
			AppendVarnums),
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
	io__write_string(Separator).


%-----------------------------------------------------------------------------%

:- pred mercury_output_func_decl(tvarset, inst_varset, existq_tvars, sym_name,
		list(type_and_mode), type_and_mode, maybe(determinism), 
		purity, class_constraints, prog_context, string, string,
		string, io__state, io__state).
:- mode mercury_output_func_decl(in, in, in, in, in, in, in, in, in, in, in, in,
		in, di, uo) is det.

mercury_output_func_decl(TypeVarSet, InstVarSet, ExistQVars, FuncName,
		TypesAndModes, RetTypeAndMode, MaybeDet, Purity, ClassContext,
		Context, StartString, Separator, Terminator) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	{ split_type_and_mode(RetTypeAndMode, RetType, MaybeRetMode) },
	(
		{ MaybeModes = yes(Modes) },
		{ MaybeRetMode = yes(RetMode) }
	->
		{ AppendVarnums = no },
		mercury_output_func_type_2(TypeVarSet, ExistQVars, FuncName,
			Types, RetType, no, Purity, ClassContext,
			Context, AppendVarnums, StartString, Separator),
		mercury_output_func_mode_decl_2(InstVarSet, FuncName, Modes,
				RetMode, MaybeDet, Context, 
				StartString, Terminator)
	;
		{ AppendVarnums = no },
		mercury_output_func_type_2(TypeVarSet, ExistQVars, FuncName,
			Types, RetType, MaybeDet, Purity, ClassContext,
			Context, AppendVarnums, StartString, Terminator)
	).

mercury_output_func_type(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums) -->
	mercury_output_func_type_2(VarSet, ExistQVars, FuncName, Types,
		RetType, MaybeDet, Purity, ClassContext, Context,
		AppendVarnums, ":- ", ".\n").

:- pred mercury_output_func_type_2(tvarset, existq_tvars, sym_name,
		list(type), type, maybe(determinism), purity, class_constraints,
		prog_context, bool, string, string, io__state, io__state).
:- mode mercury_output_func_type_2(in, in, in, in, in, in, in, in, in, in, in, 
		in, di, uo) is det.

mercury_output_func_type_2(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, _Context, AppendVarnums,
		StartString, Separator) -->
	io__write_string(StartString),
	mercury_output_quantifier(VarSet, AppendVarnums, ExistQVars),
	( { ExistQVars = [], ClassContext = constraints(_, []) } -> 
		[] 
	; 
		io__write_string("(")
	),
	write_purity_prefix(Purity),
	io__write_string("func "),
	(
		{ Types = [Type | Rest] }
	->
		mercury_output_sym_name(FuncName),
		io__write_string("("),
		mercury_output_term(Type, VarSet, AppendVarnums),
		mercury_output_remaining_terms(Rest, VarSet, AppendVarnums),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(FuncName)
	),
	io__write_string(" = "),
	mercury_output_term(RetType, VarSet, AppendVarnums,
		next_to_graphic_token),
	mercury_output_class_context(ClassContext, ExistQVars, VarSet,
		AppendVarnums),
	mercury_output_det_annotation(MaybeDet),
	io__write_string(Separator).

%-----------------------------------------------------------------------------%

mercury_output_quantifier(VarSet, AppendVarNums, ExistQVars) -->
	( { ExistQVars = [] } ->
		[]
	;
		io__write_string("some ["),
		mercury_output_vars(ExistQVars, VarSet, AppendVarNums),
		io__write_string("] ")
	).

%-----------------------------------------------------------------------------%

:- pred mercury_output_class_context(class_constraints, existq_tvars, tvarset, 
	bool, io__state, io__state).
:- mode mercury_output_class_context(in, in, in, in, di, uo) is det.

mercury_output_class_context(ClassContext, ExistQVars, VarSet,
		AppendVarnums) -->
	{ ClassContext = constraints(UnivCs, ExistCs) },
	mercury_output_class_constraint_list(ExistCs, VarSet, "=>",
		AppendVarnums),
	( { ExistQVars = [], ExistCs = [] } -> 
		[] 
	; 
		io__write_string(")")
	),
	mercury_output_class_constraint_list(UnivCs, VarSet, "<=",
		AppendVarnums).

:- pred mercury_output_class_constraint_list(list(class_constraint), tvarset, 
	string, bool, io__state, io__state).
:- mode mercury_output_class_constraint_list(in, in, in, in, di, uo) is det.
	
mercury_output_class_constraint_list(Constraints, VarSet, Operator,
		AppendVarnums) -->
	(
		{ Constraints = [] }
	;
		{ Constraints = [_|_] },
		io__write_strings([" ", Operator, " ("]),
		io__write_list(Constraints, ", ",
			mercury_output_constraint(VarSet, AppendVarnums)),
		io__write_char(')')
	).

	% This code could be written in terms of mercury_constraint_to_string
	% and io__write_string, but for efficiency's sake it's probably not
	% worth doing as it would mean building an intermediate string every
	% time you print a constraint. (eg. when generating interface files).
mercury_output_constraint(VarSet, AppendVarnums, constraint(Name, Types)) -->
	mercury_output_sym_name(Name),
	io__write_char('('),
	io__write_list(Types, ", ", output_type(VarSet, AppendVarnums)),
	io__write_char(')').

mercury_constraint_to_string(VarSet, constraint(Name, Types), String) :-
	prog_out__sym_name_to_string(Name, NameString),
	mercury_type_list_to_string(VarSet, Types, TypeString),
	string__append_list([NameString, "(", TypeString, ")"], String).

mercury_type_list_to_string(_, [], "").
mercury_type_list_to_string(VarSet, [T|Ts], String) :-
	mercury_type_to_string(VarSet, T, String0),
	type_list_to_string_2(VarSet, Ts, String1),
	string__append(String0, String1, String).

:- pred type_list_to_string_2(tvarset, list(type), string).
:- mode type_list_to_string_2(in, in, out) is det.

type_list_to_string_2(_, [], "").
type_list_to_string_2(VarSet, [T|Ts], String) :-
	mercury_type_to_string(VarSet, T, String0),
	type_list_to_string_2(VarSet, Ts, String1),
	string__append_list([", ", String0, String1], String).

	% XXX this should probably be a little cleverer, like
	% mercury_output_term. 
mercury_type_to_string(VarSet, term__variable(Var), String) :-
	varset__lookup_name(VarSet, Var, String).
mercury_type_to_string(VarSet, term__functor(Functor, Args, _), String) :-
	(
		Functor = term__atom(":"),
		Args = [Arg1, Arg2]
	->
		mercury_type_to_string(VarSet, Arg1, String1),
		mercury_type_to_string(VarSet, Arg2, String2),
		string__append_list([String1, ":", String2], String)
	;
		(
			Functor = term__atom(String0)
		;
			Functor = term__string(String0)
		;
			Functor = term__integer(Int),
			string__int_to_string(Int, String0)
		;
			Functor = term__float(Float),
			string__float_to_string(Float, String0)
		),
		(
			Args = []
		->
			String = String0
		;
			mercury_type_list_to_string(VarSet, Args, ArgsString),
			string__append_list([String0, "(", ArgsString, ")"],
				String)
		)
	).

:- pred output_type(tvarset, bool, (type), io__state, io__state).
:- mode output_type(in, in, in, di, uo) is det.

output_type(VarSet, AppendVarnums, Type) -->
	mercury_output_term(Type, VarSet, AppendVarnums).

%-----------------------------------------------------------------------------%

	% Output a mode declaration for a predicate or function.

mercury_output_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes, MaybeDet,
		Context) -->
	(	{ PredOrFunc = predicate },
		mercury_output_pred_mode_subdecl(InstVarSet, Name, Modes,
				MaybeDet, Context)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
		mercury_output_func_mode_subdecl(InstVarSet, Name, ArgModes,
				RetMode, MaybeDet, Context)
	).

	% Output a mode declaration for a predicate.

mercury_output_pred_mode_decl(VarSet, PredName, Modes, MaybeDet, Context) -->
	mercury_output_pred_mode_decl_2(VarSet, PredName, Modes, MaybeDet,
		Context, ":- ", ".\n").

:- pred mercury_output_pred_mode_decl_2(inst_varset, sym_name, list(mode),
		maybe(determinism), prog_context, string, string, 
		io__state, io__state).
:- mode mercury_output_pred_mode_decl_2(in, in, in, in, in, in, in, 
		di, uo) is det.

mercury_output_pred_mode_decl_2(VarSet, PredName, Modes, MaybeDet, Context,
		StartString, Separator) -->
	io__write_string(StartString),
	io__write_string("mode "),
	mercury_output_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		Context),
	io__write_string(Separator).

mercury_output_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		_Context) -->
	(
		{ Modes \= [] }
	->
		mercury_output_sym_name(PredName),
		io__write_string("("),
		mercury_output_mode_list(Modes, VarSet),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(PredName)
	),
	mercury_output_det_annotation(MaybeDet).

	% Output a mode declaration for a function.

mercury_output_func_mode_decl(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context) -->
	mercury_output_func_mode_decl_2(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context, ":- ", ".\n").

:- pred mercury_output_func_mode_decl_2(inst_varset, sym_name, list(mode), mode,
		maybe(determinism), prog_context, string, string,
		io__state, io__state).
:- mode mercury_output_func_mode_decl_2(in, in, in, in, in, in, in, in,
	di, uo) is det.

mercury_output_func_mode_decl_2(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context, StartString, Separator) -->
	io__write_string(StartString),
	io__write_string("mode "),
	mercury_output_func_mode_subdecl(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context),
	io__write_string(Separator).

mercury_output_func_mode_subdecl(VarSet, FuncName, Modes, RetMode, MaybeDet,
		_Context) -->
	(
		{ Modes \= [] }
	->
		mercury_output_sym_name(FuncName),
		io__write_string("("),
		mercury_output_mode_list(Modes, VarSet),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(FuncName)
	),
	io__write_string(" = "),
	mercury_output_mode(RetMode, VarSet),
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

%-----------------------------------------------------------------------------%

	% Output a clause.

:- pred mercury_output_pred_clause(prog_varset, sym_name, list(prog_term), goal,
		prog_context, io__state, io__state).
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
	).

	% Output an equation.

:- pred mercury_output_func_clause(prog_varset, sym_name, list(prog_term),
		prog_term, goal, prog_context, io__state, io__state).
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
	(
		{ Body = true - _Context0 }
	->
		mercury_output_term(Result, VarSet, no, next_to_graphic_token)
	;
		mercury_output_term(Result, VarSet, no),
		io__write_string(" :-\n\t"),
		mercury_output_goal(Body, VarSet, 1)
	).

:- pred mercury_output_goal(goal, prog_varset, int, io__state, io__state).
:- mode mercury_output_goal(in, in, in, di, uo) is det.

mercury_output_goal(Goal - _Context, VarSet, Indent) -->
	mercury_output_goal_2(Goal, VarSet, Indent).

:- pred mercury_output_goal_2(goal_expr, prog_varset, int,
		io__state, io__state).
:- mode mercury_output_goal_2(in, in, in, di, uo) is det.

mercury_output_goal_2(fail, _, _) -->
	io__write_string("fail").

mercury_output_goal_2(true, _, _) -->
	io__write_string("true").

mercury_output_goal_2(implies(G1,G2), VarSet, Indent) -->
	{ Indent1 is Indent + 1 },
	io__write_string("("),
	mercury_output_newline(Indent1),
	mercury_output_goal(G1, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("=>"),
	mercury_output_newline(Indent1),
	mercury_output_goal(G2, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal_2(equivalent(G1,G2), VarSet, Indent) -->
	{ Indent1 is Indent + 1 },
	io__write_string("("),
	mercury_output_newline(Indent1),
	mercury_output_goal(G1, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("<=>"),
	mercury_output_newline(Indent1),
	mercury_output_goal(G2, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

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

mercury_output_goal_2((A & B), VarSet, Indent) -->
	io__write_string("("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_par_conj(B, VarSet, Indent),
	mercury_output_newline(Indent),
	io__write_string(")").


mercury_output_goal_2((A;B), VarSet, Indent) -->
	io__write_string("("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_disj(B, VarSet, Indent),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal_2(call(Name, Term, Purity), VarSet, Indent) -->
	write_purity_prefix(Purity),
	mercury_output_call(Name, Term, VarSet, Indent).

mercury_output_goal_2(unify(A, B, Purity), VarSet, _Indent) -->
	write_purity_prefix(Purity),
	mercury_output_term(A, VarSet, no),
	io__write_string(" = "),
	mercury_output_term(B, VarSet, no, next_to_graphic_token).


:- pred mercury_output_call(sym_name, list(prog_term), prog_varset, int,
	io__state, io__state).
:- mode mercury_output_call(in, in, in, in, di, uo) is det.

mercury_output_call(Name, Term, VarSet, _Indent) -->
	(	
		{ Name = qualified(ModuleName, PredName) },
		mercury_output_bracketed_sym_name(ModuleName,
			next_to_graphic_token),
		io__write_string(":"),
		{ term__context_init(Context0) },
		mercury_output_term(term__functor(term__atom(PredName),
			Term, Context0), VarSet, no, next_to_graphic_token)
	;
		{ Name = unqualified(PredName) },
		{ term__context_init(Context0) },
		mercury_output_term(term__functor(term__atom(PredName),
			Term, Context0), VarSet, no, next_to_graphic_token)
	).

:- pred mercury_output_disj(goal, prog_varset, int, io__state, io__state).
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

:- pred mercury_output_par_conj(goal, prog_varset, int,
		io__state, io__state).
:- mode mercury_output_par_conj(in, in, in, di, uo) is det.

mercury_output_par_conj(Goal, VarSet, Indent) -->
	mercury_output_newline(Indent),
	io__write_string("&"),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	(
		{ Goal = (A & B) - _Context }
	->
		mercury_output_goal(A, VarSet, Indent1),
		mercury_output_par_conj(B, VarSet, Indent)
	;
		mercury_output_goal(Goal, VarSet, Indent1)
	).

:- pred mercury_output_some(list(var(T)), varset(T), io__state, io__state).
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
	mercury_output_c_code_string(C_HeaderString),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

% The code here is similar to the code for term_io__quote_string,
% but \n and \t are output directly, rather than escaped.
% Any changes here may require corresponding changes to term_io and vice versa.

:- pred mercury_output_c_code_string(string::in, io__state::di, io__state::uo)
	is det.
mercury_output_c_code_string(S) -->
	io__write_char('"'),
	mercury_write_escaped_string(S),
	io__write_char('"').

:- pred mercury_write_escaped_string(string::in, io__state::di, io__state::uo)
	is det.
mercury_write_escaped_string(String) -->
	string__foldl(mercury_write_escaped_char, String).

:- pred mercury_write_escaped_char(char::in, io__state::di, io__state::uo)
	is det.
mercury_write_escaped_char(Char) -->
	( { escape_special_char(Char, QuoteChar) } ->
		io__write_char('\\'),
		io__write_char(QuoteChar)
	; { mercury_is_source_char(Char) } ->
		io__write_char(Char)
	;
		{ mercury_escape_char(Char, String) },
		io__write_string(String)
	).

:- pred mercury_escape_char(char, string).
:- mode mercury_escape_char(in, out) is det.

	% Convert a character to the corresponding octal escape code.

	% XXX Note that we use C-style octal escapes rather than ISO-Prolog
	% octal escapes.  This is for backwards compatibility with
	% NU-Prolog and (old versions of?) SICStus Prolog.
	% The Mercury lexer accepts either, so this should work
	% ok so long as you don't have two escaped characters
	% in a row :-(

mercury_escape_char(Char, EscapeCode) :-
	char__to_int(Char, Int),
	string__int_to_base_string(Int, 8, OctalString0),
	string__pad_left(OctalString0, '0', 3, OctalString),
	string__first_char(EscapeCode, '\\', OctalString).

:- pred mercury_is_source_char(char).
:- mode mercury_is_source_char(in) is semidet.

	% Succeed if Char is a character which is allowed in
	% Mercury string and character literals.

mercury_is_source_char(Char) :-
	( char__is_alnum(Char)
	; is_mercury_punctuation_char(Char)
	; Char = '\n'
	; Char = '\t'
	).

	% Currently we only allow the following characters.
	% XXX should we just use is_printable(Char) instead?

:- pred is_mercury_punctuation_char(char).
:- mode is_mercury_punctuation_char(in) is semidet.

is_mercury_punctuation_char(' ').
is_mercury_punctuation_char('!').
is_mercury_punctuation_char('@').
is_mercury_punctuation_char('#').
is_mercury_punctuation_char('$').
is_mercury_punctuation_char('%').
is_mercury_punctuation_char('^').
is_mercury_punctuation_char('&').
is_mercury_punctuation_char('*').
is_mercury_punctuation_char('(').
is_mercury_punctuation_char(')').
is_mercury_punctuation_char('-').
is_mercury_punctuation_char('_').
is_mercury_punctuation_char('+').
is_mercury_punctuation_char('=').
is_mercury_punctuation_char('`').
is_mercury_punctuation_char('~').
is_mercury_punctuation_char('{').
is_mercury_punctuation_char('}').
is_mercury_punctuation_char('[').
is_mercury_punctuation_char(']').
is_mercury_punctuation_char(';').
is_mercury_punctuation_char(':').
is_mercury_punctuation_char('''').
is_mercury_punctuation_char('"').
is_mercury_punctuation_char('<').
is_mercury_punctuation_char('>').
is_mercury_punctuation_char('.').
is_mercury_punctuation_char(',').
is_mercury_punctuation_char('/').
is_mercury_punctuation_char('?').
is_mercury_punctuation_char('\\').
is_mercury_punctuation_char('|').

%-----------------------------------------------------------------------------%

	% escape_special_char(Char, EscapeChar)
	% is true iff Char is character for which there is a special
	% backslash-escape character EscapeChar that can be used
	% after a backslash in Mercury c_code string literals represent Char.

:- pred escape_special_char(char, char).
:- mode escape_special_char(in, out) is semidet.

escape_special_char('''', '''').
escape_special_char('"', '"').
escape_special_char('\\', '\\').
escape_special_char('\b', 'b').

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
	mercury_output_c_code_string(C_CodeString),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

	% Output the given pragma c_code declaration
mercury_output_pragma_c_code(Attributes, PredName, PredOrFunc, Vars0,
		VarSet, PragmaCode) -->
	(
		{ PragmaCode = import(_, _, _, _) }
	->
		io__write_string(":- pragma import(")
	;
		io__write_string(":- pragma c_code(")
	),
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
		mercury_output_pragma_c_code_vars(Vars, VarSet),
		io__write_string(")")
	),
	(
		{ PredOrFunc = predicate }
	;
		{ PredOrFunc = function },
		io__write_string(" = ("),
		mercury_output_pragma_c_code_vars(ResultVars, VarSet),
		io__write_string(")")
	),
	io__write_string(", "),
	mercury_output_pragma_c_attributes(Attributes),
	io__write_string(", "),
	(
		{ PragmaCode = ordinary(C_Code, _) },
		mercury_output_c_code_string(C_Code)
	;
		{ PragmaCode = nondet(Fields, _, First, _,
			Later, _, Treat, Shared, _) },
		io__write_string("local_vars("),
		mercury_output_c_code_string(Fields),
		io__write_string("), "),
		io__write_string("first_code("),
		mercury_output_c_code_string(First),
		io__write_string("), "),
		io__write_string("retry_code("),
		mercury_output_c_code_string(Later),
		io__write_string("), "),
		(
			{ Treat = share },
			io__write_string("shared_code(")
		;
			{ Treat = duplicate },
			io__write_string("duplicated_code(")
		;
			{ Treat = automatic },
			io__write_string("common_code(")
		),
		mercury_output_c_code_string(Shared),
		io__write_string(")")
	;
		{ PragmaCode = import(Name, _, _, _) },
		io__write_string(""""),
		io__write_string(Name),
		io__write_string("""")
	),
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
:- pred mercury_output_pragma_c_code_vars(list(pragma_var), prog_varset,
		io__state, io__state).
:- mode mercury_output_pragma_c_code_vars(in, in, di, uo) is det.

mercury_output_pragma_c_code_vars([], _) --> [].
mercury_output_pragma_c_code_vars([V|Vars], VarSet) -->
	{ V = pragma_var(_Var, VarName, Mode) },
	io__write_string(VarName),
	io__write_string(" :: "),
		% XXX Fake the inst varset
	{ varset__init(InstVarSet) },
	mercury_output_mode(Mode, InstVarSet),
	(	{ Vars = [] }
	->
		[]
	;
		io__write_string(", ")
	),
	mercury_output_pragma_c_code_vars(Vars, VarSet).

%-----------------------------------------------------------------------------%

:- pred mercury_output_pragma_type_spec(sym_name, sym_name, arity,
		maybe(pred_or_func), maybe(list(mode)), assoc_list(tvar, type),
		tvarset, io__state, io__state).
:- mode mercury_output_pragma_type_spec(in, in, in, in, in,
		in, in, di, uo) is det.

mercury_output_pragma_type_spec(PredName, SpecName, Arity,
		MaybePredOrFunc, MaybeModes, Subst, VarSet) -->
	io__write_string(":- pragma type_spec("),
	( { MaybeModes = yes(Modes) } ->
		{ MaybePredOrFunc = yes(PredOrFunc0) ->
			PredOrFunc = PredOrFunc0
		;
			error("pragma type_spec: no pred_or_func")
		},
		(
			{ PredOrFunc = function },
			{ pred_args_to_func_args(Modes, FuncModes, RetMode) },
			mercury_output_sym_name(PredName),
			io__write_string("("),
			{ varset__init(InstVarSet) },
			mercury_output_mode_list(FuncModes, InstVarSet),
			io__write_string(") = "),
			mercury_output_mode(RetMode, InstVarSet)
		;
			{ PredOrFunc = predicate },
			mercury_output_sym_name(PredName),
			io__write_string("("),
			{ varset__init(InstVarSet) },
			mercury_output_mode_list(Modes, InstVarSet),
			io__write_string(")")
		)
	;
		mercury_output_bracketed_sym_name(PredName,
			next_to_graphic_token),
		io__write_string("/"),
		io__write_int(Arity)
	),

	io__write_string(", ("),
	io__write_list(Subst, ", ", mercury_output_type_subst(VarSet)),
	io__write_string("), "),
	mercury_output_bracketed_sym_name(SpecName, not_next_to_graphic_token),
	io__write_string(").\n").
	
:- pred mercury_output_type_subst(tvarset, pair(tvar, type),	
		io__state, io__state).
:- mode mercury_output_type_subst(in, in, di, uo) is det.

mercury_output_type_subst(VarSet, Var - Type) -->
	mercury_output_var(Var, VarSet, no),
	io__write_string(" = "),
	mercury_output_term(Type, VarSet, no).

%-----------------------------------------------------------------------------%

mercury_output_pragma_unused_args(PredOrFunc, SymName,
		Arity, ModeNum, UnusedArgs) -->
	io__write_string(":- pragma unused_args("),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(", "),
	mercury_output_bracketed_sym_name(SymName),
	io__write_string(", "),
	io__write_int(Arity),
	io__write_string(", "),
	io__write_int(ModeNum),
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

%-----------------------------------------------------------------------------%

mercury_output_pragma_decl(PredName, Arity, PredOrFunc, PragmaName) -->
	{ PredOrFunc = predicate,
		DeclaredArity = Arity
	; PredOrFunc = function,
		DeclaredArity is Arity - 1
	},
	io__write_string(":- pragma "),
	io__write_string(PragmaName),
	io__write_string("("),
	mercury_output_bracketed_sym_name(PredName, next_to_graphic_token),
	io__write_string("/"),
	io__write_int(DeclaredArity),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

:- pred mercury_output_pragma_import(sym_name, pred_or_func, list(mode),
	pragma_foreign_code_attributes, string, io__state, io__state).
:- mode mercury_output_pragma_import(in, in, in, in, in, di, uo) is det.

mercury_output_pragma_import(Name, PredOrFunc, ModeList, Attributes,
		C_Function) -->
	{ varset__init(Varset) }, % the varset isn't really used.
	io__write_string(":- pragma import("),
	mercury_output_sym_name(Name),
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ModeList, ArgModes, RetMode) },
		io__write_string("("),
		mercury_output_mode_list(ArgModes, Varset),
		io__write_string(") = "),
		mercury_output_mode(RetMode, Varset)
	;
		{ PredOrFunc = predicate },
		io__write_string("("),
		mercury_output_mode_list(ModeList, Varset),
		io__write_string(")")
	),
	io__write_string(", "),
	mercury_output_pragma_c_attributes(Attributes),
	io__write_string(", "),
	io__write_string(C_Function),
	io__write_string(").\n").

:- pred mercury_output_pragma_export(sym_name, pred_or_func, list(mode),
	string, io__state, io__state).
:- mode mercury_output_pragma_export(in, in, in, in, di, uo) is det.

mercury_output_pragma_export(Name, PredOrFunc, ModeList, C_Function) -->
	{ varset__init(Varset) }, % the varset isn't really used.
	io__write_string(":- pragma export("),
	mercury_output_sym_name(Name),
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ModeList, ArgModes, RetMode) },
		io__write_string("("),
		mercury_output_mode_list(ArgModes, Varset),
		io__write_string(") = "),
		mercury_output_mode(RetMode, Varset)
	;
		{ PredOrFunc = predicate },
		io__write_string("("),
		mercury_output_mode_list(ModeList, Varset),
		io__write_string(")")
	),
	io__write_string(", "),
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

:- pred mercury_output_pragma_owner(sym_name, arity, string, 
		io__state, io__state).
:- mode mercury_output_pragma_owner(in, in, in, di, uo) is det.

mercury_output_pragma_owner(Pred, Arity, Owner) -->
	io__write_string(":- pragma owner("),
	mercury_output_sym_name(Pred),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(", "),
	term_io__quote_string(Owner),
	io__write_string(").\n").

:- pred mercury_output_pragma_index(sym_name, arity, index_spec,
		io__state, io__state). 
:- mode mercury_output_pragma_index(in, in, in, di, uo) is det.

mercury_output_pragma_index(PredName, Arity, IndexSpec) -->
	io__write_string(":- pragma aditi_index("),
	mercury_output_bracketed_sym_name(PredName, next_to_graphic_token),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(", "),
	mercury_output_index_spec(IndexSpec),
	io__write_string(").\n").

mercury_output_index_spec(IndexSpec) -->
	{ IndexSpec = index_spec(IndexType, Attrs) },
	io__write(IndexType),
	io__write_string(", ["),
	mercury_output_int_list(Attrs),
	io__write_string("]").

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

:- pred mercury_output_pragma_c_attributes(pragma_foreign_code_attributes,
		io__state, io__state).
:- mode mercury_output_pragma_c_attributes(in, di, uo) is det.

mercury_output_pragma_c_attributes(Attributes) -->
	io__write_string("["),
	{ may_call_mercury(Attributes, MayCallMercury) },
	(
		{ MayCallMercury = may_call_mercury },
		io__write_string("may_call_mercury, ")
	;
		{ MayCallMercury = will_not_call_mercury },
		io__write_string("will_not_call_mercury, ")
	),
	{ thread_safe(Attributes, ThreadSafe) },
	(
		{ ThreadSafe = not_thread_safe },
		io__write_string("not_thread_safe")
	;
		{ ThreadSafe = thread_safe },
		io__write_string("thread_safe")
	),
	io__write_string("]").

%-----------------------------------------------------------------------------%

	% write a term to standard output.

mercury_output_term(Term, VarSet, AppendVarnums) -->
	mercury_output_term(Term, VarSet, AppendVarnums,
		not_next_to_graphic_token).

mercury_output_term(term__variable(Var), VarSet, AppendVarnums, _) -->
	mercury_output_var(Var, VarSet, AppendVarnums).
mercury_output_term(term__functor(Functor, Args, _), VarSet, AppendVarnums,
		NextToGraphicToken) -->
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
		( { FunctorName = ":" } ->
			mercury_output_term(Arg1, VarSet, AppendVarnums,
				next_to_graphic_token),
			io__write_string(":"),
			mercury_output_term(Arg2, VarSet, AppendVarnums,
				next_to_graphic_token)
		;
			mercury_output_term(Arg1, VarSet, AppendVarnums,
				not_next_to_graphic_token),
			io__write_string(" "),
			io__write_string(FunctorName),
			io__write_string(" "),
			mercury_output_term(Arg2, VarSet, AppendVarnums,
				not_next_to_graphic_token)
		),
		io__write_string(")")
	;
		{ Args = [Y | Ys] }
	->
		mercury_output_constant(Functor, NextToGraphicToken),
		io__write_string("("),
		mercury_output_term(Y, VarSet, AppendVarnums),
		mercury_output_remaining_terms(Ys, VarSet, AppendVarnums),
		io__write_string(")")
	;
		mercury_output_bracketed_constant(Functor, NextToGraphicToken)
	).

:- pred mercury_output_list_args(term(T), varset(T), bool,
		io__state, io__state).
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

:- pred mercury_output_remaining_terms(list(term(T)), varset(T), bool,
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

:- pred mercury_output_vars_2(list(var(T)), varset(T), bool,
		io__state, io__state).
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

:- pred mercury_output_bracketed_constant(const, io__state, io__state).
:- mode mercury_output_bracketed_constant(in, di, uo) is det.

mercury_output_bracketed_constant(Const) -->
	mercury_output_bracketed_constant(Const, not_next_to_graphic_token).

:- pred mercury_output_bracketed_constant(const, needs_quotes,
					io__state, io__state).
:- mode mercury_output_bracketed_constant(in, in, di, uo) is det.

mercury_output_bracketed_constant(Const, NextToGraphicToken) -->
	( { Const = term__atom(Op), mercury_op(Op) } ->
		io__write_string("("),
		term_io__quote_atom(Op),
		io__write_string(")")
	;
		mercury_output_constant(Const, NextToGraphicToken)
	).

:- pred mercury_output_constant(const, needs_quotes, io__state, io__state).
:- mode mercury_output_constant(in, in, di, uo) is det.

mercury_output_constant(Const, NextToGraphicToken) -->
	( { Const = term__atom(Atom) } ->
		mercury_quote_atom(Atom, NextToGraphicToken)
	;
		term_io__write_constant(Const)
	).

:- pred mercury_output_bracketed_atom(string, needs_quotes,
				io__state, io__state).
:- mode mercury_output_bracketed_atom(in, in, di, uo) is det.

mercury_output_bracketed_atom(Name, NextToGraphicToken) -->
	( { mercury_op(Name) } ->
		io__write_string("("),
		term_io__quote_atom(Name),
		io__write_string(")")
	;
		mercury_quote_atom(Name, NextToGraphicToken)
	).

	%
	% Use mercury_output_bracketed_sym_name when the sym_name has
	% no arguments, otherwise use mercury_output_sym_name.
	%

:- pred mercury_output_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_sym_name(in, di, uo) is det.

mercury_output_sym_name(SymName) -->
	mercury_output_sym_name(SymName, not_next_to_graphic_token).

mercury_output_bracketed_sym_name(SymName) -->
	mercury_output_bracketed_sym_name(SymName, not_next_to_graphic_token).

:- pred mercury_output_bracketed_sym_name(sym_name, needs_quotes,
					io__state, io__state).
:- mode mercury_output_bracketed_sym_name(in, in, di, uo) is det.

mercury_output_bracketed_sym_name(Name, NextToGraphicToken) -->
	(	{ Name = qualified(ModuleName, Name2) },
		io__write_char('('),
		mercury_output_bracketed_sym_name(ModuleName,
			next_to_graphic_token),
		io__write_char(':'),
		mercury_output_bracketed_atom(Name2, next_to_graphic_token),
		io__write_char(')')
	;
		{ Name = unqualified(Name2) },
		mercury_output_bracketed_atom(Name2, NextToGraphicToken)
	).

:- pred mercury_output_sym_name(sym_name, needs_quotes, io__state, io__state).
:- mode mercury_output_sym_name(in, in, di, uo) is det.

mercury_output_sym_name(Name, NextToGraphicToken) -->
	(	{ Name = qualified(ModuleName, PredName) },
		mercury_output_bracketed_sym_name(ModuleName,
			next_to_graphic_token),
		io__write_char(':'),
		mercury_quote_atom(PredName,
			next_to_graphic_token)
	;
		{ Name = unqualified(PredName) },
		mercury_quote_atom(PredName, NextToGraphicToken)
	).

:- pred mercury_quote_atom(string, needs_quotes, io__state, io__state).
:- mode mercury_quote_atom(in, in, di, uo) is det.

mercury_quote_atom(Name, NextToGraphicToken) -->
	%
	% If the symname is composed of only graphic token chars,
	% then term_io__quote_atom will not quote it; but if
	% it is next another graphic token, it needs to be quoted,
	% otherwise the two would be considered part of one
	% symbol name (e.g. In "int:<", the ":<" parses as one token,
	% so when writing out the "<" after the ":" we need to quote it.
	%
	(
		{ NextToGraphicToken = next_to_graphic_token },
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
mercury_infix_op(":=").
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
mercury_infix_op("==>").
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
mercury_infix_op("=^").
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
mercury_infix_op("++").
mercury_infix_op("-").
mercury_infix_op("--").
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
mercury_unary_prefix_op("aditi_bottom_up").
mercury_unary_prefix_op("aditi_top_down").
mercury_unary_prefix_op("delete").
mercury_unary_prefix_op("dynamic").
mercury_unary_prefix_op("end_module").
mercury_unary_prefix_op("func").
mercury_unary_prefix_op("if").
mercury_unary_prefix_op("import_module").
mercury_unary_prefix_op("include_module").
mercury_unary_prefix_op("impure").
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
mercury_unary_prefix_op("promise").
mercury_unary_prefix_op("pure").
mercury_unary_prefix_op("rule").	/* NU-Prolog */
mercury_unary_prefix_op("semipure").
mercury_unary_prefix_op("sorted").
mercury_unary_prefix_op("spy").
mercury_unary_prefix_op("type").
mercury_unary_prefix_op("update").
mercury_unary_prefix_op("useIf").
mercury_unary_prefix_op("wait").
mercury_unary_prefix_op("~").
mercury_unary_prefix_op("^").

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

:- pred maybe_output_line_number(prog_context, io__state, io__state).
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
