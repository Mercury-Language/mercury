%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: fjh.

% This program converts the parse tree structure provided by prog_io
% back into Mercury source text.
%
% Many (though not all) of the procedures in this module come in
% groups of three, where the three follow the pattern:
%
%	:- pred mercury_output_xyz(..., io__state::di, io__state::uo) is det.
%	:- func mercury_xyz_to_string(...) = string.
%	:- pred mercury_format_xyz(..., U::di, U::uo) is det <= output(U).
%
% The first two simply forward all the work to the third. This is possible
% because both io__state and string are members of the required typeclass,
% which is defined at the end of this module.
%
% For the mercury_output_xyz versions, going through a typeclass interface is
% (for now) a slight slowdown, but the time cost is still small compared to
% the cost of I/O itself.
%
% For the mercury_xyz_to_string versions, the cost is acceptable because
% (for now) we only create relatively small strings this way, e.g. strings that
% go into error messages. The typeclass instance for strings has a quadratic
% complexity in the number of strings being appended but a reasonably low
% constant factor. If we ever want to use these functions to create long
% strings (longer than a few lines), then we should use a typeclass
% instance implementation that represents the entity being converted to string
% as a list of strings that must be concatenated together at the end using
% string__append_list (probably after being un-reversed, so that you can
% represent appending to the string by consing onto the front of the list).
% The complexity of an implementation like that can be linear in the size
% of the string being built, although it will have a higher constant factor.

%-----------------------------------------------------------------------------%

:- module mercury_to_mercury.
:- interface.

:- import_module prog_data, (inst).
:- import_module hlds_goal, hlds_data.

:- import_module bool, std_util, list, io, varset, term.

:- type needs_brackets
	--->	needs_brackets		% needs brackets, if it is an op
	;	does_not_need_brackets.	% doesn't need brackets

:- type needs_quotes
	--->	next_to_graphic_token		% needs quotes, if it
						% is another graphic token
	;	not_next_to_graphic_token.	% doesn't need quotes

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

:- func mercury_pred_type_to_string(tvarset, existq_tvars, sym_name,
	list(type), maybe(determinism), purity, class_constraints,
	prog_context, bool) = string.

	% Output a `:- func' declaration, making sure that the variable
	% number appears in variable names if the boolean argument
	% is set to `yes'.
:- pred mercury_output_func_type(tvarset, existq_tvars, sym_name,
		list(type), type,
		maybe(determinism), purity, class_constraints,
		prog_context, bool, io__state, io__state).
:- mode mercury_output_func_type(in, in, in, in, in, in, in, in, in, in,
		di, uo) is det.

:- func mercury_func_type_to_string(tvarset, existq_tvars, sym_name,
	list(type), type, maybe(determinism), purity, class_constraints,
	prog_context, bool) = string.

:- pred mercury_output_pred_mode_decl(inst_varset, sym_name, list(mode),
		maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_pred_mode_decl(in, in, in, in, in, di, uo) is det.

:- func mercury_pred_mode_decl_to_string(inst_varset, sym_name, list(mode),
	maybe(determinism), prog_context) = string.

:- pred mercury_output_func_mode_decl(inst_varset, sym_name, list(mode), mode,
		maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_func_mode_decl(in, in, in, in, in, in, di, uo) is det.

:- func mercury_func_mode_decl_to_string(inst_varset, sym_name, list(mode),
	mode, maybe(determinism), prog_context) = string.

:- pred mercury_output_mode_subdecl(pred_or_func, inst_varset, sym_name,
		list(mode), maybe(determinism), prog_context,
		io__state, io__state).
:- mode mercury_output_mode_subdecl(in, in, in, in, in, in, di, uo) is det.

:- func mercury_mode_subdecl_to_string(pred_or_func, inst_varset, sym_name,
	list(mode), maybe(determinism), prog_context) = string.

:- pred mercury_output_pred_mode_subdecl(inst_varset, sym_name, list(mode),
		maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_pred_mode_subdecl(in, in, in, in, in, di, uo) is det.

:- func mercury_pred_mode_subdecl_to_string(inst_varset, sym_name, list(mode),
	maybe(determinism), prog_context) = string.

:- pred mercury_output_func_mode_subdecl(inst_varset, sym_name, list(mode),
		mode, maybe(determinism), prog_context, io__state, io__state).
:- mode mercury_output_func_mode_subdecl(in, in, in, in, in, in, di, uo) is det.

:- func mercury_func_mode_subdecl_to_string(inst_varset, sym_name, list(mode),
	mode, maybe(determinism), prog_context) = string.

:- pred mercury_output_pragma_decl(sym_name, int, pred_or_func, string,
		io__state, io__state).
:- mode mercury_output_pragma_decl(in, in, in, in, di, uo) is det.

:- func mercury_pragma_decl_to_string(sym_name, int, pred_or_func, string)
	= string.

:- pred mercury_output_foreign_language_string(foreign_language,
		io__state, io__state).
:- mode mercury_output_foreign_language_string(in, di, uo) is det.

:- func mercury_foreign_language_to_string(foreign_language) = string.

:- pred mercury_output_pragma_foreign_code(
		pragma_foreign_proc_attributes, sym_name,
		pred_or_func, list(pragma_var), prog_varset,
		pragma_foreign_code_impl, io__state, io__state).
:- mode mercury_output_pragma_foreign_code(
		in, in, in, in, in, in, di, uo) is det.

:- func mercury_pragma_foreign_code_to_string(pragma_foreign_proc_attributes,
	sym_name, pred_or_func, list(pragma_var), prog_varset,
	pragma_foreign_code_impl) = string.

:- inst type_spec == bound(type_spec(ground, ground, ground, ground,
			ground, ground, ground, ground)).

	% mercury_output_pragma_type_spec(Pragma, AppendVarnums).
:- pred mercury_output_pragma_type_spec((pragma_type), bool,
		io__state, io__state).
:- mode mercury_output_pragma_type_spec(in(type_spec), in, di, uo) is det.

:- pred mercury_output_pragma_unused_args(pred_or_func, sym_name,
		int, mode_num, list(int), io__state, io__state).
:- mode mercury_output_pragma_unused_args(in, in, in, in, in, di, uo) is det.

	% Write an Aditi index specifier.
:- pred mercury_output_index_spec(index_spec, io__state, io__state). 
:- mode mercury_output_index_spec(in, di, uo) is det.

	% Output the given foreign_decl declaration
:- pred mercury_output_pragma_foreign_decl(foreign_language, string,
		io__state, io__state).
:- mode mercury_output_pragma_foreign_decl(in, in, di, uo) is det.

:- func mercury_pragma_foreign_decl_to_string(foreign_language, string)
	= string.

:- pred mercury_output_pragma_foreign_import_module(foreign_language,
		module_name, io__state, io__state).
:- mode mercury_output_pragma_foreign_import_module(in, in, di, uo) is det.

:- pred mercury_output_ctor(constructor, tvarset, io__state, io__state).
:- mode mercury_output_ctor(in, in, di, uo) is det.

:- pred mercury_output_remaining_ctor_args(tvarset, list(constructor_arg),
		io__state, io__state).
:- mode mercury_output_remaining_ctor_args(in, in, di, uo) is det.

	% Output a list of insts in a format that makes them easy to read
	% but may not be valid Mercury.

:- pred mercury_output_structured_inst_list(list(inst), int, inst_varset,
			io__state, io__state).
:- mode mercury_output_structured_inst_list(in, in, in, di, uo) is det.

:- func mercury_structured_inst_list_to_string(list(inst), int, inst_varset)
	= string.

:- pred mercury_output_inst_list(list(inst), inst_varset, io__state, io__state).
:- mode mercury_output_inst_list(in, in, di, uo) is det.

:- func mercury_inst_list_to_string(list(inst), inst_varset) = string.

	% Output an inst in a format that makes it easy to read
	% but may not be valid Mercury.

:- pred mercury_output_structured_inst(inst, int, inst_varset,
		io__state, io__state).
:- mode mercury_output_structured_inst(in, in, in, di, uo) is det.

:- func mercury_structured_inst_to_string(inst, int, inst_varset) = string.

:- pred mercury_output_inst(inst, inst_varset, io__state, io__state).
:- mode mercury_output_inst(in, in, di, uo) is det.

:- func mercury_inst_to_string(inst, inst_varset) = string.

:- pred mercury_output_cons_id(cons_id, needs_brackets, io__state, io__state).
:- mode mercury_output_cons_id(in, in, di, uo) is det.

:- func mercury_cons_id_to_string(cons_id, needs_brackets) = string.

:- pred mercury_output_mode(mode, inst_varset, io__state, io__state).
:- mode mercury_output_mode(in, in, di, uo) is det.

:- func mercury_mode_to_string(mode, inst_varset) = string.

:- pred mercury_output_mode_list(list(mode), inst_varset, io__state, io__state).
:- mode mercury_output_mode_list(in, in, di, uo) is det.

:- func mercury_mode_list_to_string(list(mode), inst_varset) = string.

:- pred mercury_output_uni_mode(uni_mode, inst_varset, io__state, io__state).
:- mode mercury_output_uni_mode(in, in, di, uo) is det.

:- func mercury_uni_mode_to_string(uni_mode, inst_varset) = string.

:- pred mercury_output_uni_mode_list(list(uni_mode), inst_varset,
		io__state, io__state).
:- mode mercury_output_uni_mode_list(in, in, di, uo) is det.

:- func mercury_uni_mode_list_to_string(list(uni_mode), inst_varset) = string.

:- pred mercury_output_det(determinism, io__state, io__state).
:- mode mercury_output_det(in, di, uo) is det.

:- func mercury_det_to_string(determinism) = string.

	% Output a comma-separated list of variables, making sure that
	% the variable number appears in the variable name if the boolean
	% argument is set to `yes'.

:- pred mercury_output_vars(list(var(T)), varset(T), bool,
		io__state, io__state).
:- mode mercury_output_vars(in, in, in, di, uo) is det.

:- func mercury_vars_to_string(list(var(T)), varset(T), bool) = string.

	% Output a variable, making sure that the variable number appears
	% in the variable name if the boolean argument is set to `yes'.

:- pred mercury_output_var(var(T), varset(T), bool, io__state, io__state).
:- mode mercury_output_var(in, in, in, di, uo) is det.

:- func mercury_var_to_string(var(T), varset(T), bool) = string.

	% Output a term, making sure that the variable number appears
	% in variable names if the boolean argument is set to `yes'.

:- pred mercury_output_term(term(T), varset(T), bool, io__state, io__state).
:- mode mercury_output_term(in, in, in, di, uo) is det.

:- func mercury_term_to_string(term(T), varset(T), bool) = string.

:- pred mercury_output_term(term(T), varset(T), bool, needs_quotes,
		io__state, io__state).
:- mode mercury_output_term(in, in, in, in, di, uo) is det.

:- func mercury_term_to_string(term(T), varset(T), bool, needs_quotes)
	= string.

	% XXX Even though types are defined to be terms, these two functions 
	% format types not as mercury_term_to_string does but in a simplified
	% manner.
:- func mercury_type_to_string(tvarset, type) = string.
:- func mercury_type_list_to_string(tvarset, list(type)) = string.

:- pred mercury_output_newline(int, io__state, io__state).
:- mode mercury_output_newline(in, di, uo) is det.

:- pred mercury_output_bracketed_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_bracketed_sym_name(in, di, uo) is det.

:- pred mercury_output_bracketed_sym_name(sym_name, needs_quotes,
		io__state, io__state).
:- mode mercury_output_bracketed_sym_name(in, in, di, uo) is det.

:- pred mercury_convert_var_name(string, string).
:- mode mercury_convert_var_name(in, out) is det.

	% Output a constraint, making sure that the variable number appears
	% in variable names if the boolean argument is set to `yes'.
:- pred mercury_output_constraint(tvarset, bool, class_constraint,
		io__state, io__state).
:- mode mercury_output_constraint(in, in, in, di, uo) is det.

:- func mercury_constraint_to_string(tvarset, class_constraint) = string.

	% Output an existential quantifier, making sure that the variable
	% number appears in variable names if the boolean argument
	% is set to `yes'.
:- pred mercury_output_quantifier(tvarset, bool, existq_tvars,
		io__state, io__state).
:- mode mercury_output_quantifier(in, in, in, di, uo) is det.

:- func mercury_quantifier_to_string(tvarset, bool, existq_tvars) = string.

:- pred mercury_output_instance_methods(instance_methods, io__state,
	io__state).
:- mode mercury_output_instance_methods(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_out, prog_util, hlds_pred, hlds_out, instmap.
:- import_module recompilation_version, purity, term_util.
:- import_module globals, options, termination, foreign.

:- import_module assoc_list, char, int, string, set, lexer, ops, require.
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

		% Module qualifiers on items are redundant after the
		% declaration above.
		{ UnqualifiedItemNames = yes },
		mercury_output_item_list(UnqualifiedItemNames, Items),
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

:- pred mercury_output_item_list(bool, list(item_and_context),
		io__state, io__state).
:- mode mercury_output_item_list(in, in, di, uo) is det.

mercury_output_item_list(_, []) --> [].
mercury_output_item_list(UnqualifiedItemNames, [Item - Context | Items]) -->
	mercury_output_item(UnqualifiedItemNames, Item, Context),
	mercury_output_item_list(UnqualifiedItemNames, Items).

%-----------------------------------------------------------------------------%

mercury_output_item(Item, Context) -->
	{ UnqualifiedItemNames = no },
	mercury_output_item(UnqualifiedItemNames, Item, Context).

:- pred mercury_output_item(bool, item, prog_context, io__state, io__state).
:- mode mercury_output_item(in, in, in, di, uo) is det.

	% dispatch on the different types of items

mercury_output_item(UnqualifiedItemNames,
		type_defn(VarSet, Name0, Args, TypeDefn, _Cond),
		Context) -->
	{ maybe_unqualify_sym_name(UnqualifiedItemNames, Name0, Name) },
	maybe_output_line_number(Context),
	mercury_output_type_defn(VarSet, Name, Args, TypeDefn, Context).

mercury_output_item(UnqualifiedItemNames,
		inst_defn(VarSet, Name0, Args, InstDefn, _Cond),
		Context) -->
	{ maybe_unqualify_sym_name(UnqualifiedItemNames, Name0, Name) },
	maybe_output_line_number(Context),
	mercury_output_inst_defn(VarSet, Name, Args, InstDefn, Context).

mercury_output_item(UnqualifiedItemNames,
		mode_defn(VarSet, Name0, Args, ModeDefn, _Cond),
		Context) -->
	{ maybe_unqualify_sym_name(UnqualifiedItemNames, Name0, Name) },
	maybe_output_line_number(Context),
	mercury_format_mode_defn(VarSet, Name, Args, ModeDefn, Context).

mercury_output_item(UnqualifiedItemNames,
		pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
			PredOrFunc, PredName0, TypesAndModes, Det,
			_Cond, Purity, ClassContext),
		Context) -->
	{ maybe_unqualify_sym_name(UnqualifiedItemNames, PredName0, PredName) },
	maybe_output_line_number(Context),
	(
		{ PredOrFunc = predicate },
		mercury_format_pred_decl(TypeVarSet, InstVarSet, ExistQVars,
			PredName, TypesAndModes, Det, Purity,
			ClassContext, Context,
			":- ", ".\n", ".\n")
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(TypesAndModes, FuncTypesAndModes,
				RetTypeAndMode) },
		mercury_format_func_decl(TypeVarSet, InstVarSet, ExistQVars,
			PredName, FuncTypesAndModes, RetTypeAndMode,
			Det, Purity, ClassContext, Context,
			":- ", ".\n", ".\n")
	).

mercury_output_item(UnqualifiedItemNames,
		pred_or_func_mode(VarSet, PredOrFunc, PredName0,
			Modes, MaybeDet, _Cond),
		Context) -->
	{ maybe_unqualify_sym_name(UnqualifiedItemNames, PredName0, PredName) },
	maybe_output_line_number(Context),
	(
		{ PredOrFunc = predicate },
		mercury_output_pred_mode_decl(VarSet, PredName, Modes,
			MaybeDet, Context)
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, FuncModes, RetMode) },
		mercury_output_func_mode_decl(VarSet, PredName,
			FuncModes, RetMode, MaybeDet, Context)
	).

mercury_output_item(_, module_defn(VarSet, ModuleDefn), Context) -->
	maybe_output_line_number(Context),
	mercury_output_module_defn(VarSet, ModuleDefn, Context).

mercury_output_item(UnqualifiedItemNames,
		clause(VarSet, PredOrFunc, PredName0, Args, Body),
		Context) -->
	{ maybe_unqualify_sym_name(UnqualifiedItemNames, PredName0, PredName) },
	maybe_output_line_number(Context),
	(
		{ PredOrFunc = predicate },
		mercury_output_pred_clause(VarSet, PredName,
			Args, Body, Context)
	;
		{ PredOrFunc = function },	
		{ pred_args_to_func_args(Args, FuncArgs, Result) },
		mercury_output_func_clause(VarSet, PredName, FuncArgs, Result,
			Body, Context)
	),
	io__write_string(".\n").

mercury_output_item(_UnqualifiedItemNames, pragma(Pragma), Context) -->
	maybe_output_line_number(Context),
	(
		{ Pragma = source_file(SourceFile) },
		mercury_output_pragma_source_file(SourceFile)
	;
		{ Pragma = foreign_decl(Lang, ForeignHeaderString) },
		mercury_output_pragma_foreign_decl(Lang, ForeignHeaderString)
	;
		{ Pragma = foreign_import_module(Lang, ModuleName) },
		mercury_output_pragma_foreign_import_module(Lang, ModuleName)
	;
		{ Pragma = foreign_code(Lang, Code) }, 
		mercury_output_pragma_foreign_body_code(Lang, Code)
	;
		{ Pragma = foreign_proc(Attributes, Pred, PredOrFunc, Vars,
			VarSet, PragmaCode) }, 
		mercury_output_pragma_foreign_code(Attributes, Pred,
			PredOrFunc, Vars, VarSet, PragmaCode)
	;
		{ Pragma = foreign_type(ForeignType, _MercuryType,
				MercuryTypeSymName) },
		{ ForeignType = il(RefOrVal, ForeignLocStr, ForeignTypeName) },

		io__write_string(":- pragma foreign_type("),
		io__write_string("il, "),
		mercury_output_sym_name(MercuryTypeSymName),
		io__write_string(", "),
		( { RefOrVal = reference },
			io__write_string("\"class [")
		; { RefOrVal = value },
			io__write_string("\"valuetype [")
		),
		io__write_string(ForeignLocStr),
		io__write_string("]"),
		{ sym_name_to_string(ForeignTypeName, ".", ForeignTypeStr) },
		io__write_string(ForeignTypeStr),
		io__write_string("\").\n")
	;
		{ Pragma = import(Pred, PredOrFunc, ModeList, Attributes,
			C_Function) },
		mercury_format_pragma_import(Pred, PredOrFunc, ModeList,
			Attributes, C_Function)
	;
		{ Pragma = export(Pred, PredOrFunc, ModeList, C_Function) },
		mercury_format_pragma_export(Pred, PredOrFunc, ModeList,
			C_Function)
	;
		{ Pragma = obsolete(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "obsolete")
	;
		{ Pragma = tabled(Type, Pred, Arity, _PredOrFunc, _Mode) },
		{ eval_method_to_string(Type, TypeS) },
		mercury_output_pragma_decl(Pred, Arity, predicate, TypeS)
	;
		{ Pragma = type_spec(_, _, _, _, _, _, _, _) },
		{ AppendVarnums = no },
		mercury_output_pragma_type_spec(Pragma, AppendVarnums)
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
		mercury_format_pragma_fact_table(Pred, Arity, FileName)
	;
		{ Pragma = aditi(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate, "aditi")
	;
		{ Pragma = base_relation(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, 
			predicate, "base_relation")
	;
		{ Pragma = aditi_index(Pred, Arity, Index) },
		mercury_format_pragma_index(Pred, Arity, Index)
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
		mercury_format_pragma_owner(Pred, Arity, Owner)
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
		{ Pragma = promise_semipure(Pred, Arity) },
		mercury_output_pragma_decl(Pred, Arity, predicate,
					   "promise_semipure")
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

mercury_output_item(_, assertion(Goal, VarSet), _) -->
	io__write_string(":- promise "),
	{ Indent = 1 },
	mercury_output_newline(Indent),
	mercury_output_goal(Goal, VarSet, Indent),
	io__write_string(".\n").

mercury_output_item(_, nothing(_), _) --> [].
mercury_output_item(UnqualifiedItemNames,
		typeclass(Constraints, ClassName0, Vars, Interface, VarSet),
		_) --> 
	{ maybe_unqualify_sym_name(UnqualifiedItemNames,
		ClassName0, ClassName) },
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
	mercury_format_class_constraint_list(Constraints, VarSet, "<=",
		AppendVarnums),

	(
		{ Interface = abstract },
		io__write_string(".\n")
	;
		{ Interface = concrete(Methods) },
		io__write_string(" where [\n"),
		output_class_methods(Methods),
		io__write_string("\n].\n")
	).
mercury_output_item(_, instance(Constraints, ClassName, Types, Body, 
		VarSet, _InstanceModuleName), _) --> 
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
	mercury_format_class_constraint_list(Constraints, VarSet, "<=",
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
		{ Method = pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
			PredOrFunc, Name0, TypesAndModes, Detism, _Condition,
			Purity, ClassContext, Context) },

		% The module name is implied by the qualifier of the
		% `:- typeclass declaration'.
		{ unqualify_name(Name0, Name) },
		(
			{ PredOrFunc = predicate },
			mercury_format_pred_decl(TypeVarSet, InstVarSet,
				ExistQVars, unqualified(Name), TypesAndModes,
				Detism, Purity, ClassContext, Context,
				"", ",\n\t", "")
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(TypesAndModes,
				FuncTypesAndModes, RetTypeAndMode) },
			mercury_format_func_decl(TypeVarSet, InstVarSet,
				ExistQVars, unqualified(Name),
				FuncTypesAndModes, RetTypeAndMode, Detism,
				Purity, ClassContext, Context, "", ",\n\t", "")
		)
	;
		{ Method = pred_or_func_mode(VarSet, PredOrFunc,
			Name0, Modes, Detism, _Condition, Context) },

		% The module name is implied by the qualifier of the
		% `:- typeclass declaration'.
		{ unqualify_name(Name0, Name) },
		(
			{ PredOrFunc = predicate },
			mercury_format_pred_mode_decl_2(VarSet,
				unqualified(Name), Modes,
				Detism, Context, "", "")
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(Modes, FuncModes, RetMode) },
			mercury_format_func_mode_decl_2(VarSet,
				unqualified(Name), FuncModes, RetMode,
				Detism, Context, "", "")
		)
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
		{ WriteOneItem = (pred(Item::in, di, uo) is det -->
		    (
			{ Item = clause(VarSet, PredOrFunc, _PredName,
				HeadTerms, Body) }
		    ->
			(
				{ PredOrFunc = predicate },
				mercury_output_pred_clause(VarSet,
					Name1, HeadTerms, Body, Context)
			;
				{ PredOrFunc = function },
				{ pred_args_to_func_args(HeadTerms, ArgTerms,
					ResultTerm) },
				mercury_output_func_clause(VarSet,
					Name1, ArgTerms, ResultTerm,
					Body, Context)
			)
		    ;
			{ error("invalid instance method item") }
		    )) },
		io__write_list(ItemList, "),\n\t(", WriteOneItem),
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
	; { ModuleDefn = version_numbers(Module, VersionNumbers) } ->
		io__write_string(":- version_numbers("),
		io__write_int(version_numbers_version_number),
		io__write_string(", "),
		mercury_output_bracketed_sym_name(Module),
		io__write_string(",\n"),
		recompilation_version__write_version_numbers(VersionNumbers),
		io__write_string(").\n")
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

:- pred mercury_output_inst_defn(inst_varset, sym_name, list(inst_var),
		inst_defn, prog_context, io__state, io__state).
:- mode mercury_output_inst_defn(in, in, in, in, in, di, uo) is det.

mercury_output_inst_defn(VarSet, Name, Args, abstract_inst, Context) -->
	io__write_string(":- inst ("),
	{ list__map(pred(V::in, variable(V)::out) is det, Args, ArgTerms) },
	{ construct_qualified_term(Name, ArgTerms, Context, InstTerm) },
	mercury_output_term(InstTerm, VarSet, no),
	io__write_string(").\n").
mercury_output_inst_defn(VarSet, Name, Args, eqv_inst(Body), Context) -->
	io__write_string(":- inst ("),
	{ list__map(pred(V::in, variable(V)::out) is det, Args, ArgTerms) },
	{ construct_qualified_term(Name, ArgTerms, Context, InstTerm) },
	mercury_output_term(InstTerm, VarSet, no),
	io__write_string(") = "),
	mercury_output_inst(Body, VarSet),
	io__write_string(".\n").

mercury_output_structured_inst_list(Insts, Indent, VarSet) -->
	mercury_format_structured_inst_list(Insts, Indent, VarSet).

mercury_structured_inst_list_to_string(Insts, Indent, VarSet) = String :-
	mercury_format_structured_inst_list(Insts, Indent, VarSet, "", String).

:- pred mercury_format_structured_inst_list(list(inst)::in, int::in,
	inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_inst_list([], _, _) --> [].
mercury_format_structured_inst_list([Inst | Insts], Indent0, VarSet) -->
	mercury_format_structured_inst(Inst, Indent0, VarSet),
	mercury_format_structured_inst_list(Insts, Indent0, VarSet).

mercury_output_inst_list(Insts, VarSet) -->
	mercury_format_inst_list(Insts, VarSet).

mercury_inst_list_to_string(Insts, VarSet) = String :-
	mercury_format_inst_list(Insts, VarSet, "", String).

:- pred mercury_format_inst_list(list(inst)::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_inst_list([], _) --> [].
mercury_format_inst_list([Inst | Insts], VarSet) -->
	mercury_format_inst(Inst, VarSet),
	( { Insts = [] } ->
		[]
	;
		add_string(", "),
		mercury_format_inst_list(Insts, VarSet)
	).

mercury_output_structured_inst(Inst, Indent, VarSet) -->
	mercury_format_structured_inst(Inst, Indent, VarSet).

mercury_structured_inst_to_string(Inst, Indent, VarSet) = String :-
	mercury_format_structured_inst(Inst, Indent, VarSet, "", String).

:- pred mercury_format_structured_inst((inst)::in, int::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_structured_inst(any(Uniq), Indent, _) -->
	mercury_format_tabs(Indent),
	mercury_format_any_uniqueness(Uniq),
	add_string("\n").
mercury_format_structured_inst(free, Indent, _) -->
	mercury_format_tabs(Indent),
	add_string("free\n").
mercury_format_structured_inst(free(_T), Indent, _) -->
	mercury_format_tabs(Indent),
	add_string("free(with some type)\n").
mercury_format_structured_inst(bound(Uniq, BoundInsts), Indent, VarSet) -->
	mercury_format_tabs(Indent),
	mercury_format_uniqueness(Uniq, "bound"),
	add_string("(\n"),
	mercury_format_structured_bound_insts(BoundInsts, Indent, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").
mercury_format_structured_inst(ground(Uniq, GroundInstInfo), Indent, VarSet)
		-->
	mercury_format_tabs(Indent),
	(	
		{ GroundInstInfo = higher_order(pred_inst_info(PredOrFunc,
				Modes, Det)) },
		( { Uniq = shared } ->
			[]
		;
			add_string("/* "),
			mercury_format_uniqueness(Uniq, "ground"),
			add_string(" */")
		),
		(
			{ PredOrFunc = predicate },
			( { Modes = [] } ->
				add_string("(pred) is "),
				mercury_format_det(Det),
				add_string(")\n")
			;
				add_string("(pred("),
				mercury_format_mode_list(Modes, VarSet),
				add_string(") is "),
				mercury_format_det(Det),
				add_string(")\n")
			)
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
			( { Modes = [] } ->
				add_string("((func) = ")
			;
				add_string("(func("),
				mercury_format_mode_list(ArgModes, VarSet),
				add_string(") = ")
			),
			mercury_format_mode(RetMode, VarSet),
			add_string(" is "),
			mercury_format_det(Det),
			add_string(")\n")
		)
	;
		{ GroundInstInfo = constrained_inst_var(Var) },
		mercury_format_tabs(Indent),
		mercury_format_var(Var, VarSet, no),
		add_string("\n")
	;
		{ GroundInstInfo = none},
		mercury_format_uniqueness(Uniq, "ground"),
		add_string("\n")
	).
mercury_format_structured_inst(inst_var(Var), Indent, VarSet) -->
	mercury_format_tabs(Indent),
	mercury_format_var(Var, VarSet, no),
	add_string("\n").
mercury_format_structured_inst(abstract_inst(Name, Args), Indent, VarSet) -->
	mercury_format_structured_inst_name(user_inst(Name, Args), Indent,
		VarSet).
mercury_format_structured_inst(defined_inst(InstName), Indent, VarSet) -->
	mercury_format_structured_inst_name(InstName, Indent, VarSet).
mercury_format_structured_inst(not_reached, Indent, _) -->
	mercury_format_tabs(Indent),
	add_string("not_reached\n").

mercury_output_inst(Inst, VarSet) -->
	mercury_format_inst(Inst, VarSet).

mercury_inst_to_string(Inst, VarSet) = String :-
	mercury_format_inst(Inst, VarSet, "", String).

:- pred mercury_format_inst((inst)::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_inst(any(Uniq), _) -->
	mercury_format_any_uniqueness(Uniq).
mercury_format_inst(free, _) -->
	add_string("free").
mercury_format_inst(free(_T), _) -->
	add_string("free(with some type)").
mercury_format_inst(bound(Uniq, BoundInsts), VarSet) -->
	mercury_format_uniqueness(Uniq, "bound"),
	add_string("("),
	mercury_format_bound_insts(BoundInsts, VarSet),
	add_string(")").
mercury_format_inst(ground(Uniq, GroundInstInfo), VarSet) -->
	(	
		{ GroundInstInfo = higher_order(pred_inst_info(PredOrFunc,
				Modes, Det)) },
		( { Uniq = shared } ->
			[]
		;
			add_string("/* "),
			mercury_format_uniqueness(Uniq, "ground"),
			add_string(" */")
		),
		(
			{ PredOrFunc = predicate },
			( { Modes = [] } ->
				add_string("((pred) is "),
				mercury_format_det(Det),
				add_string(")")
			;
				add_string("(pred("),
				mercury_format_mode_list(Modes, VarSet),
				add_string(") is "),
				mercury_format_det(Det),
				add_string(")")
			)
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
			( { ArgModes = [] } ->
				add_string("((func)")
			;
				add_string("(func("),
				mercury_format_mode_list(ArgModes, VarSet),
				add_string(")")
			),
			add_string(" = "),
			mercury_format_mode(RetMode, VarSet),
			add_string(" is "),
			mercury_format_det(Det),
			add_string(")")
		)
	;
		{ GroundInstInfo = constrained_inst_var(Var) },
		mercury_format_var(Var, VarSet, no)
	;
		{ GroundInstInfo = none },
		mercury_format_uniqueness(Uniq, "ground")
	).
mercury_format_inst(inst_var(Var), VarSet) -->
	mercury_format_var(Var, VarSet, no).
mercury_format_inst(abstract_inst(Name, Args), VarSet) -->
	mercury_format_inst_name(user_inst(Name, Args), VarSet).
mercury_format_inst(defined_inst(InstName), VarSet) -->
	mercury_format_inst_name(InstName, VarSet).
mercury_format_inst(not_reached, _) -->
	add_string("not_reached").

:- pred mercury_format_structured_inst_name(inst_name::in, int::in,
	inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_inst_name(user_inst(Name, Args), Indent, VarSet) -->
	( { Args = [] } ->
		mercury_format_tabs(Indent),
		mercury_format_bracketed_sym_name(Name)
	;
		mercury_format_tabs(Indent),
		mercury_format_sym_name(Name),
		add_string("(\n"),
		{ Indent1 = Indent + 1 },
		mercury_format_structured_inst_list(Args, Indent1, VarSet),
		mercury_format_tabs(Indent),
		add_string(")\n")
	).
mercury_format_structured_inst_name(merge_inst(InstA, InstB), Indent, VarSet)
		-->
	mercury_format_tabs(Indent),
	add_string("$merge_inst(\n"),
	{ Indent1 = Indent + 1 },
	mercury_format_structured_inst_list([InstA, InstB], Indent1, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").
mercury_format_structured_inst_name(shared_inst(InstName), Indent, VarSet) -->
	mercury_format_tabs(Indent),
	add_string("$shared_inst(\n"),
	{ Indent1 = Indent + 1 },
	mercury_format_structured_inst_name(InstName, Indent1, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").
mercury_format_structured_inst_name(mostly_uniq_inst(InstName), Indent, VarSet)
		-->
	mercury_format_tabs(Indent),
	add_string("$mostly_uniq_inst(\n"),
	{ Indent1 = Indent + 1 },
	mercury_format_structured_inst_name(InstName, Indent1, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").
mercury_format_structured_inst_name(unify_inst(Liveness, InstA, InstB, Real),
		Indent, VarSet) -->
	mercury_format_tabs(Indent),
	add_string("$unify("),
	( { Liveness = live } ->
		add_string("live, ")
	;
		add_string("dead, ")
	),
	( { Real = real_unify } ->
		add_string("real,\n")
	;
		add_string("fake,\n")
	),
	{ Indent1 = Indent + 1 },
	mercury_format_structured_inst_list([InstA, InstB], Indent1, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").
mercury_format_structured_inst_name(ground_inst(InstName, IsLive, Uniq, Real),
		Indent, VarSet) -->
	mercury_format_tabs(Indent),
	add_string("$ground("),
	( { IsLive = live } ->
		add_string("live, ")
	;
		add_string("dead, ")
	),
	( { Real = real_unify } ->
		add_string("real, ")
	;
		add_string("fake, ")
	),
	mercury_format_uniqueness(Uniq, "shared"),
	add_string(",\n"),
	{ Indent1 = Indent + 1 },
	mercury_format_structured_inst_name(InstName, Indent1, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").
mercury_format_structured_inst_name(any_inst(InstName, IsLive, Uniq, Real),
		Indent, VarSet) -->
	mercury_format_tabs(Indent),
	add_string("$any("),
	( { IsLive = live } ->
		add_string("live, ")
	;
		add_string("dead, ")
	),
	( { Real = real_unify } ->
		add_string("real, ")
	;
		add_string("fake, ")
	),
	mercury_format_uniqueness(Uniq, "shared"),
	add_string(",\n"),
	{ Indent1 = Indent + 1 },
	mercury_format_structured_inst_name(InstName, Indent1, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").
mercury_format_structured_inst_name(typed_ground(Uniqueness, Type),
		Indent, _VarSet) -->
	mercury_format_tabs(Indent),
	add_string("$typed_ground("),
	mercury_format_uniqueness(Uniqueness, "shared"),
	add_string(", "),
	{ varset__init(TypeVarSet) },
	mercury_format_term(Type, TypeVarSet, no),
	add_string(")\n").
mercury_format_structured_inst_name(typed_inst(Type, InstName),
		Indent, VarSet) -->
	mercury_format_tabs(Indent),
	add_string("$typed_inst("),
	{ varset__init(TypeVarSet) },
	mercury_format_term(Type, TypeVarSet, no),
	add_string(",\n"),
	{ Indent1 = Indent + 1 },
	mercury_format_structured_inst_name(InstName, Indent1, VarSet),
	mercury_format_tabs(Indent),
	add_string(")\n").

:- pred mercury_format_inst_name(inst_name::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_inst_name(user_inst(Name, Args), VarSet) -->
	( { Args = [] } ->
		mercury_format_bracketed_sym_name(Name)
	;
		mercury_format_sym_name(Name),
		add_string("("),
		mercury_format_inst_list(Args, VarSet),
		add_string(")")
	).
mercury_format_inst_name(merge_inst(InstA, InstB), VarSet) -->
	add_string("$merge_inst("),
	mercury_format_inst_list([InstA, InstB], VarSet),
	add_string(")").
mercury_format_inst_name(shared_inst(InstName), VarSet) -->
	add_string("$shared_inst("),
	mercury_format_inst_name(InstName, VarSet),
	add_string(")").
mercury_format_inst_name(mostly_uniq_inst(InstName), VarSet) -->
	add_string("$mostly_uniq_inst("),
	mercury_format_inst_name(InstName, VarSet),
	add_string(")").
mercury_format_inst_name(unify_inst(Liveness, InstA, InstB, Real), VarSet) -->
	add_string("$unify("),
	( { Liveness = live } ->
		add_string("live, ")
	;
		add_string("dead, ")
	),
	mercury_format_inst_list([InstA, InstB], VarSet),
	( { Real = real_unify } ->
		add_string(", real")
	;
		add_string(", fake")
	),
	add_string(")").
mercury_format_inst_name(ground_inst(InstName, IsLive, Uniq, Real), VarSet) -->
	add_string("$ground("),
	mercury_format_inst_name(InstName, VarSet),
	add_string(", "),
	( { IsLive = live } ->
		add_string("live, ")
	;
		add_string("dead, ")
	),
	mercury_format_uniqueness(Uniq, "shared"),
	( { Real = real_unify } ->
		add_string(", real")
	;
		add_string(", fake")
	),
	add_string(")").
mercury_format_inst_name(any_inst(InstName, IsLive, Uniq, Real), VarSet) -->
	add_string("$any("),
	mercury_format_inst_name(InstName, VarSet),
	add_string(", "),
	( { IsLive = live } ->
		add_string("live, ")
	;
		add_string("dead, ")
	),
	mercury_format_uniqueness(Uniq, "shared"),
	( { Real = real_unify } ->
		add_string(", real")
	;
		add_string(", fake")
	),
	add_string(")").
mercury_format_inst_name(typed_ground(Uniqueness, Type), _VarSet) -->
	add_string("$typed_ground("),
	mercury_format_uniqueness(Uniqueness, "shared"),
	add_string(", "),
	{ varset__init(TypeVarSet) },
	mercury_format_term(Type, TypeVarSet, no),
	add_string(")").
mercury_format_inst_name(typed_inst(Type, InstName), VarSet) -->
	add_string("$typed_inst("),
	{ varset__init(TypeVarSet) },
	mercury_format_term(Type, TypeVarSet, no),
	add_string(", "),
	mercury_format_inst_name(InstName, VarSet),
	add_string(")").

:- pred mercury_format_uniqueness(uniqueness::in, string::in,
	U::di, U::uo) is det <= output(U).

mercury_format_uniqueness(shared, SharedString) -->
	add_string(SharedString).
mercury_format_uniqueness(unique, _) -->
	add_string("unique").
mercury_format_uniqueness(mostly_unique, _) -->
	add_string("mostly_unique").
mercury_format_uniqueness(clobbered, _) -->
	add_string("clobbered").
mercury_format_uniqueness(mostly_clobbered, _) -->
	add_string("mostly_clobbered").

:- pred mercury_format_any_uniqueness(uniqueness::in,
	U::di, U::uo) is det <= output(U).

mercury_format_any_uniqueness(shared) -->
	add_string("any").
mercury_format_any_uniqueness(unique) -->
	add_string("unique_any").
mercury_format_any_uniqueness(mostly_unique) -->
	add_string("mostly_unique_any").
mercury_format_any_uniqueness(clobbered) -->
	add_string("clobbered_any").
mercury_format_any_uniqueness(mostly_clobbered) -->
	add_string("mostly_clobbered_any").

:- pred mercury_format_structured_bound_insts(list(bound_inst)::in, int::in,
	inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_bound_insts([], _, _) --> [].
mercury_format_structured_bound_insts([functor(ConsId, Args) | BoundInsts],
		Indent0, VarSet) -->
	{ Indent1 = Indent0 + 1 },
	{ Indent2 = Indent1 + 1 },
	( { Args = [] } ->
		mercury_format_tabs(Indent1),
		mercury_format_cons_id(ConsId, needs_brackets),
		add_string("\n")
	;
		mercury_format_tabs(Indent1),
		mercury_format_cons_id(ConsId, does_not_need_brackets),
		add_string("(\n"),
		mercury_format_structured_inst_list(Args, Indent2, VarSet),
		mercury_format_tabs(Indent1),
		add_string(")\n")
	),
	( { BoundInsts = [] } ->
		[]
	;
		mercury_format_tabs(Indent0),
		add_string(";\n"),
		mercury_format_structured_bound_insts(BoundInsts, Indent0,
			VarSet)
	).

:- pred mercury_format_bound_insts(list(bound_inst)::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_bound_insts([], _) --> [].
mercury_format_bound_insts([functor(ConsId, Args) | BoundInsts], VarSet) -->
	( { Args = [] } ->
		mercury_format_cons_id(ConsId, needs_brackets)
	;
		mercury_format_cons_id(ConsId, does_not_need_brackets),
		add_string("("),
		mercury_format_inst_list(Args, VarSet),
		add_string(")")
	),
	( { BoundInsts = [] } ->
		[]
	;
		add_string(" ; "),
		mercury_format_bound_insts(BoundInsts, VarSet)
	).

mercury_output_cons_id(ConsId, NeedsBrackets) -->
	mercury_format_cons_id(ConsId, NeedsBrackets).

mercury_cons_id_to_string(ConsId, NeedsBrackets) = String :-
	mercury_format_cons_id(ConsId, NeedsBrackets, "", String).

:- pred mercury_format_cons_id(cons_id::in, needs_brackets::in,
	U::di, U::uo) is det <= output(U).

mercury_format_cons_id(cons(Name, _), NeedsBrackets) -->
	( { NeedsBrackets = needs_brackets } ->
		mercury_format_bracketed_sym_name(Name)
	;
		mercury_format_sym_name(Name)
	).
mercury_format_cons_id(int_const(X), _) -->
	add_int(X).
mercury_format_cons_id(float_const(X), _) -->
	add_float(X).
mercury_format_cons_id(string_const(X), _) -->
	add_quoted_string(X).
mercury_format_cons_id(pred_const(PredId, ProcId, EvalMethod), _) -->
	% XXX Sufficient, but probably should print this out in
	%     name/arity form.

	{ pred_id_to_int(PredId, PredInt) },
	{ proc_id_to_int(ProcId, ProcInt) },
	add_string("<pred_const("),
	add_int(PredInt),
	add_string(", "),
	add_int(ProcInt),
	add_string(", "),
	add_lambda_eval_method(EvalMethod),
	add_string(")>").
mercury_format_cons_id(code_addr_const(PredId, ProcId), _) -->
	% XXX Sufficient, but probably should print this out in
	%     name/arity form.

	{ pred_id_to_int(PredId, PredInt) },
	{ proc_id_to_int(ProcId, ProcInt) },
	add_string("<code_addr_const("),
	add_int(PredInt),
	add_string(", "),
	add_int(ProcInt),
	add_string(")>").
mercury_format_cons_id(type_ctor_info_const(Module, Type, Arity), _) -->
	{ prog_out__sym_name_to_string(Module, ModuleString) },
	{ string__int_to_string(Arity, ArityString) },
	add_strings(["<type_ctor_info for ",
		ModuleString, ":", Type, "/", ArityString, ">"]).
mercury_format_cons_id(base_typeclass_info_const(Module, Class, InstanceNum,
		InstanceString), _) -->
	{ prog_out__sym_name_to_string(Module, ModuleString) },
	add_string("<base_typeclass_info for "),
	add_class_id(Class),
	( { ModuleString \= "some bogus module name" } ->
		add_strings([" from module ", ModuleString])
	;
		[]
	),
	add_format(", instance number %d (%s)>",
		[i(InstanceNum), s(InstanceString)]).
mercury_format_cons_id(tabling_pointer_const(_, _), _) -->
	add_string("<tabling pointer>").
mercury_format_cons_id(deep_profiling_proc_static(_), _) -->
	add_string("<deep_profiling_proc_static>").

:- pred mercury_format_mode_defn(inst_varset::in, sym_name::in,
	list(inst_var)::in, mode_defn::in, prog_context::in,
	U::di, U::uo) is det <= output(U).

mercury_format_mode_defn(VarSet, Name, Args, eqv_mode(Mode), Context) -->
	add_string(":- mode ("),
	{ list__map(pred(V::in, variable(V)::out) is det, Args, ArgTerms) },
	{ construct_qualified_term(Name, ArgTerms, Context, ModeTerm) },
	mercury_format_term(ModeTerm, VarSet, no),
	add_string(") :: "),
	mercury_format_mode(Mode, VarSet),
	add_string(".\n").

mercury_output_mode_list(Modes, VarSet) -->
	mercury_format_mode_list(Modes, VarSet).

mercury_mode_list_to_string(Modes, VarSet) = String :-
	mercury_format_mode_list(Modes, VarSet, "", String).

:- pred mercury_format_mode_list(list(mode)::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_mode_list([], _VarSet) --> [].
mercury_format_mode_list([Mode | Modes], VarSet) -->
	mercury_format_mode(Mode, VarSet),
	( { Modes = [] } ->
		[]
	;
		add_string(", "),
		mercury_format_mode_list(Modes, VarSet)
	).

mercury_output_uni_mode_list(UniModes, VarSet) -->
	mercury_format_uni_mode_list(UniModes, VarSet).

mercury_uni_mode_list_to_string(UniModes, VarSet) = String :-
	mercury_format_uni_mode_list(UniModes, VarSet, "", String).

:- pred mercury_format_uni_mode_list(list(uni_mode)::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_uni_mode_list([], _VarSet) --> [].
mercury_format_uni_mode_list([Mode | Modes], VarSet) -->
	mercury_format_uni_mode(Mode, VarSet),
	( { Modes = [] } ->
		[]
	;
		add_string(", "),
		mercury_format_uni_mode_list(Modes, VarSet)
	).

mercury_output_uni_mode(UniMode, VarSet) -->
	mercury_format_uni_mode(UniMode, VarSet).

mercury_uni_mode_to_string(UniMode, VarSet) = String :-
	mercury_format_uni_mode(UniMode, VarSet, "", String).

:- pred mercury_format_uni_mode(uni_mode::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_uni_mode((InstA1 - InstB1 -> InstA2 - InstB2), VarSet) -->
	mercury_format_mode((InstA1 -> InstA2), VarSet),
	add_string(" = "),
	mercury_format_mode((InstB1 -> InstB2), VarSet).

mercury_output_mode(Mode, VarSet) -->
	mercury_format_mode(Mode, VarSet).

mercury_mode_to_string(Mode, VarSet) = String :-
	mercury_format_mode(Mode, VarSet, "", String).

:- pred mercury_format_mode((mode)::in, inst_varset::in,
	U::di, U::uo) is det <= output(U).

mercury_format_mode((InstA -> InstB), VarSet) -->
	( 
		%
		% check for higher-order pred or func modes, and output them
		% in a nice format
		%
		{ InstA = ground(_Uniq, higher_order(pred_inst_info(_PredOrFunc,
				_Modes, _Det))) },
		{ InstB = InstA }
	->
		mercury_format_inst(InstA, VarSet)
	;
		add_string("("),
		mercury_format_inst(InstA, VarSet),
		add_string(" -> "),
		mercury_format_inst(InstB, VarSet),
		add_string(")")
	).
mercury_format_mode(user_defined_mode(Name, Args), VarSet) -->
	( { Args = [] } ->
		mercury_format_bracketed_sym_name(Name)
	;
		mercury_format_sym_name(Name),
		add_string("("),
		mercury_format_inst_list(Args, VarSet),
		add_string(")")
	).

%-----------------------------------------------------------------------------%

:- pred mercury_output_type_defn(tvarset, sym_name, list(type_param),
		type_defn, prog_context, io__state, io__state).
:- mode mercury_output_type_defn(in, in, in, in, in, di, uo) is det.

mercury_output_type_defn(_VarSet, _Name, _Args, uu_type(_Body), Context) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string("warning: undiscriminated union types not yet supported.\n"),
	io__set_output_stream(OldStream, _).

mercury_output_type_defn(VarSet, Name, Args, abstract_type, Context) -->
	io__write_string(":- type "),
	{ construct_qualified_term(Name, Args, Context, TypeTerm) },
	mercury_output_term(TypeTerm, VarSet, no, next_to_graphic_token),
	io__write_string(".\n").

mercury_output_type_defn(VarSet, Name, Args, eqv_type(Body), Context) -->
	io__write_string(":- type "),
	{ construct_qualified_term(Name, Args, Context, TypeTerm) },
	mercury_output_term(TypeTerm, VarSet, no),
	io__write_string(" == "),
	mercury_output_term(Body, VarSet, no, next_to_graphic_token),
	io__write_string(".\n").

mercury_output_type_defn(VarSet, Name, Args,
		du_type(Ctors, MaybeEqualityPred), Context) -->
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
	io__write_string("\n\t.\n").

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
	{ Ctor = ctor(ExistQVars, Constraints, SymName, Args) },

	% We'll have attached the module name to the type definition,
	% so there's no point adding it to the constructor as well.
	{ unqualify_name(SymName, Name) },

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
		{ Name = ";"
		; Name = "{}"
		; Name = "some"
		; Name = "=>"
		}
	->
		io__write_string("{ ")
	;
		[]
	),
	(
		{ Args = [Arg | Rest] }
	->
		mercury_output_sym_name(unqualified(Name)),
		io__write_string("("),
		mercury_output_ctor_arg(VarSet, Arg),
		mercury_output_remaining_ctor_args(VarSet, Rest),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(unqualified(Name))
	),
	(
		{ Arity = 2 },
		{ Name = ";"
		; Name = "{}"
		; Name = "some"
		; Name = "=>"
		}
	->
		io__write_string(" }")
	;
		[]
	),

	{ AppendVarnums = no },
	mercury_format_class_constraint_list(Constraints, VarSet, "=>",
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

:- pred mercury_format_pred_decl(tvarset::in, inst_varset::in, existq_tvars::in,
	sym_name::in, list(type_and_mode)::in, maybe(determinism)::in,
	purity::in, class_constraints::in, prog_context::in,
	string::in, string::in, string::in, U::di, U::uo) is det <= output(U).

mercury_format_pred_decl(TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, MaybeDet, Purity, ClassContext, Context,
		StartString, Separator, Terminator) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	(
		{ MaybeModes = yes(Modes) },
		{ Modes \= [] }
	->
		{ AppendVarnums = no },
		mercury_format_pred_type_2(TypeVarSet, ExistQVars, PredName,
			Types, MaybeDet, Purity, ClassContext, Context,
			AppendVarnums, StartString, Separator),
		mercury_format_pred_mode_decl_2(InstVarSet, PredName, Modes,
			MaybeDet, Context, StartString, Terminator)
	;
		{ AppendVarnums = no },
		mercury_format_pred_type_2(TypeVarSet, ExistQVars, PredName,
			Types, MaybeDet, Purity, ClassContext, Context,
			AppendVarnums, StartString, Terminator)
	).

mercury_output_pred_type(VarSet, ExistQVars, PredName, Types, MaybeDet, Purity,
		ClassContext, Context, AppendVarnums) -->
	mercury_format_pred_type(VarSet, ExistQVars, PredName, Types, MaybeDet,
		Purity, ClassContext, Context, AppendVarnums).

mercury_pred_type_to_string(VarSet, ExistQVars, PredName, Types, MaybeDet,
		Purity, ClassContext, Context, AppendVarnums) = String :-
	mercury_format_pred_type(VarSet, ExistQVars, PredName, Types, MaybeDet,
		Purity, ClassContext, Context, AppendVarnums, "", String).

:- pred mercury_format_pred_type(tvarset::in, existq_tvars::in, sym_name::in,
	list(type)::in, maybe(determinism)::in, purity::in,
	class_constraints::in, prog_context::in, bool::in, U::di, U::uo)
	is det <= output(U).

mercury_format_pred_type(VarSet, ExistQVars, PredName, Types, MaybeDet, Purity,
		ClassContext, Context, AppendVarnums) -->
	mercury_format_pred_type_2(VarSet, ExistQVars, PredName, Types,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums,
		":- ", ".\n").

:- pred mercury_format_pred_type_2(tvarset::in, existq_tvars::in, sym_name::in,
	list(type)::in, maybe(determinism)::in, purity::in,
	class_constraints::in, prog_context::in, bool::in,
	string::in, string::in, U::di, U::uo) is det <= output(U).

mercury_format_pred_type_2(VarSet, ExistQVars, PredName, Types, MaybeDet,
		Purity, ClassContext, _Context, AppendVarnums,
		StartString, Separator) -->
	add_string(StartString),
	mercury_format_quantifier(VarSet, AppendVarnums, ExistQVars),
	( { ExistQVars = [], ClassContext = constraints(_, []) } -> 
		[] 
	; 
		add_string("(")
	),
	add_purity_prefix(Purity),
	add_string("pred "),
	(
		{ Types = [Type | Rest] }
	->
		mercury_format_sym_name(PredName),
		add_string("("),
		mercury_format_term(Type, VarSet, AppendVarnums),
		mercury_format_remaining_terms(Rest, VarSet, AppendVarnums),
		add_string(")"),
		mercury_format_class_context(ClassContext, ExistQVars, VarSet,
			AppendVarnums)
	;
		mercury_format_bracketed_sym_name(PredName),
		mercury_format_class_context(ClassContext, ExistQVars, VarSet,
			AppendVarnums),
		mercury_format_det_annotation(MaybeDet)
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
		mercury_format_det_annotation(MaybeDet)
	;
		[]
	),
	add_string(Separator).

%-----------------------------------------------------------------------------%

:- pred mercury_format_func_decl(tvarset::in, inst_varset::in,
	existq_tvars::in, sym_name::in, list(type_and_mode)::in,
	type_and_mode::in, maybe(determinism)::in, purity::in,
	class_constraints::in, prog_context::in, string::in, string::in,
	string::in, U::di, U::uo) is det <= output(U).

mercury_format_func_decl(TypeVarSet, InstVarSet, ExistQVars, FuncName,
		TypesAndModes, RetTypeAndMode, MaybeDet, Purity, ClassContext,
		Context, StartString, Separator, Terminator) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	{ split_type_and_mode(RetTypeAndMode, RetType, MaybeRetMode) },
	(
		{ MaybeModes = yes(Modes) },
		{ MaybeRetMode = yes(RetMode) }
	->
		{ AppendVarnums = no },
		mercury_format_func_type_2(TypeVarSet, ExistQVars, FuncName,
			Types, RetType, no, Purity, ClassContext,
			Context, AppendVarnums, StartString, Separator),
		mercury_format_func_mode_decl_2(InstVarSet, FuncName, Modes,
			RetMode, MaybeDet, Context, StartString, Terminator)
	;
		{ AppendVarnums = no },
		mercury_format_func_type_2(TypeVarSet, ExistQVars, FuncName,
			Types, RetType, MaybeDet, Purity, ClassContext,
			Context, AppendVarnums, StartString, Terminator)
	).

mercury_output_func_type(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums) -->
	mercury_format_func_type(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums).

mercury_func_type_to_string(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums)
		= String :-
	mercury_format_func_type(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums,
		"", String).

:- pred mercury_format_func_type(tvarset::in, existq_tvars::in, sym_name::in,
	list(type)::in, (type)::in, maybe(determinism)::in, purity::in,
	class_constraints::in, prog_context::in, bool::in, U::di, U::uo)
	is det <= output(U).

mercury_format_func_type(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, Context, AppendVarnums) -->
	mercury_format_func_type_2(VarSet, ExistQVars, FuncName, Types,
		RetType, MaybeDet, Purity, ClassContext, Context,
		AppendVarnums, ":- ", ".\n").

:- pred mercury_format_func_type_2(tvarset::in, existq_tvars::in, sym_name::in,
	list(type)::in, (type)::in, maybe(determinism)::in,
	purity::in, class_constraints::in, prog_context::in, bool::in,
	string::in, string::in, U::di, U::uo) is det <= output(U).

mercury_format_func_type_2(VarSet, ExistQVars, FuncName, Types, RetType,
		MaybeDet, Purity, ClassContext, _Context, AppendVarnums,
		StartString, Separator) -->
	add_string(StartString),
	mercury_format_quantifier(VarSet, AppendVarnums, ExistQVars),
	( { ExistQVars = [], ClassContext = constraints(_, []) } -> 
		[] 
	; 
		add_string("(")
	),
	add_purity_prefix(Purity),
	add_string("func "),
	(
		{ Types = [Type | Rest] }
	->
		mercury_format_sym_name(FuncName),
		add_string("("),
		mercury_format_term(Type, VarSet, AppendVarnums),
		mercury_format_remaining_terms(Rest, VarSet, AppendVarnums),
		add_string(")")
	;
		mercury_format_bracketed_sym_name(FuncName)
	),
	add_string(" = "),
	mercury_format_term(RetType, VarSet, AppendVarnums,
		next_to_graphic_token),
	mercury_format_class_context(ClassContext, ExistQVars, VarSet,
		AppendVarnums),
	mercury_format_det_annotation(MaybeDet),
	add_string(Separator).

%-----------------------------------------------------------------------------%

mercury_output_quantifier(VarSet, AppendVarNums, ExistQVars) -->
	mercury_format_quantifier(VarSet, AppendVarNums, ExistQVars).

mercury_quantifier_to_string(VarSet, AppendVarNums, ExistQVars) = String :-
	mercury_format_quantifier(VarSet, AppendVarNums, ExistQVars,
		"", String).

:- pred mercury_format_quantifier(tvarset::in, bool::in, existq_tvars::in,
	U::di, U::uo) is det <= output(U).

mercury_format_quantifier(VarSet, AppendVarNums, ExistQVars) -->
	( { ExistQVars = [] } ->
		[]
	;
		add_string("some ["),
		mercury_format_vars(ExistQVars, VarSet, AppendVarNums),
		add_string("] ")
	).

%-----------------------------------------------------------------------------%

:- pred mercury_output_class_context(class_constraints, existq_tvars, tvarset, 
	bool, io__state, io__state).
:- mode mercury_output_class_context(in, in, in, in, di, uo) is det.

mercury_output_class_context(ClassContext, ExistQVars, VarSet,
		AppendVarnums) -->
	mercury_format_class_context(ClassContext, ExistQVars, VarSet,
		AppendVarnums).

:- pred mercury_format_class_context(class_constraints::in, existq_tvars::in,
	tvarset::in, bool::in, U::di, U::uo) is det <= output(U).

mercury_format_class_context(ClassContext, ExistQVars, VarSet,
		AppendVarnums) -->
	{ ClassContext = constraints(UnivCs, ExistCs) },
	mercury_format_class_constraint_list(ExistCs, VarSet, "=>",
		AppendVarnums),
	( { ExistQVars = [], ExistCs = [] } -> 
		[] 
	; 
		add_string(")")
	),
	mercury_format_class_constraint_list(UnivCs, VarSet, "<=",
		AppendVarnums).

:- pred mercury_format_class_constraint_list(list(class_constraint)::in,
	tvarset::in, string::in, bool::in, U::di, U::uo) is det <= output(U).
	
mercury_format_class_constraint_list(Constraints, VarSet, Operator,
		AppendVarnums) -->
	(
		{ Constraints = [] }
	;
		{ Constraints = [_|_] },
		add_strings([" ", Operator, " ("]),
		add_list(Constraints, ", ",
			mercury_format_constraint(VarSet, AppendVarnums)),
		add_string(")")
	).

mercury_output_constraint(VarSet, AppendVarnums, constraint(Name, Types)) -->
	mercury_format_constraint(VarSet, AppendVarnums,
		constraint(Name, Types)).

mercury_constraint_to_string(VarSet, constraint(Name, Types)) = String :-
	mercury_format_constraint(VarSet, no, constraint(Name, Types),
		"", String).

:- pred mercury_format_constraint(tvarset::in, bool::in, class_constraint::in,
	U::di, U::uo) is det <= output(U).

mercury_format_constraint(VarSet, AppendVarnums, constraint(Name, Types)) -->
	mercury_format_sym_name(Name),
	add_string("("),
	add_list(Types, ", ", format_type(VarSet, AppendVarnums)),
	add_string(")").

:- pred format_type(tvarset::in, bool::in, (type)::in, U::di, U::uo) is det
	<= output(U).

format_type(VarSet, AppendVarnums, Type) -->
	mercury_format_term(Type, VarSet, AppendVarnums).

mercury_type_list_to_string(_, []) = "".
mercury_type_list_to_string(VarSet, [T | Ts]) = String :-
	String0 = mercury_type_to_string(VarSet, T),
	String1 = mercury_type_list_to_string_2(VarSet, Ts),
	string__append(String0, String1, String).

:- func mercury_type_list_to_string_2(tvarset, list(type)) = string.

mercury_type_list_to_string_2(_, []) = "".
mercury_type_list_to_string_2(VarSet, [T | Ts]) = String :-
	String0 = mercury_type_to_string(VarSet, T),
	String1 = mercury_type_list_to_string_2(VarSet, Ts),
	string__append_list([", ", String0, String1], String).

	% XXX this should probably be a little cleverer, like
	% mercury_output_term. 
mercury_type_to_string(VarSet, term__variable(Var)) = String :-
	varset__lookup_name(VarSet, Var, String).
mercury_type_to_string(VarSet, term__functor(Functor, Args, _)) = String :-
	(
		Functor = term__atom(":"),
		Args = [Arg1, Arg2]
	->
		String1 = mercury_type_to_string(VarSet, Arg1),
		String2 = mercury_type_to_string(VarSet, Arg2),
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
			ArgsString = mercury_type_list_to_string(VarSet, Args),
			string__append_list([String0, "(", ArgsString, ")"],
				String)
		)
	).

%-----------------------------------------------------------------------------%

	% Output a mode declaration for a predicate or function.

mercury_output_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes, MaybeDet,
		Context) -->
	mercury_format_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes,
		MaybeDet, Context).

mercury_mode_subdecl_to_string(PredOrFunc, InstVarSet, Name, Modes, MaybeDet,
		Context) = String :-
	mercury_format_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes,
		MaybeDet, Context, "", String).

:- pred mercury_format_mode_subdecl(pred_or_func::in, inst_varset::in,
	sym_name::in, list(mode)::in, maybe(determinism)::in, prog_context::in,
	U::di, U::uo) is det <= output(U).

mercury_format_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes,
		MaybeDet, Context) -->
	(
		{ PredOrFunc = predicate },
		mercury_format_pred_mode_subdecl(InstVarSet, Name, Modes,
			MaybeDet, Context)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
		mercury_format_func_mode_subdecl(InstVarSet, Name, ArgModes,
			RetMode, MaybeDet, Context)
	).

	% Output a mode declaration for a predicate.

mercury_output_pred_mode_decl(VarSet, PredName, Modes, MaybeDet, Context) -->
	mercury_format_pred_mode_decl_2(VarSet, PredName, Modes, MaybeDet,
		Context, ":- ", ".\n").

mercury_pred_mode_decl_to_string(VarSet, PredName, Modes, MaybeDet, Context)
		= String :-
	mercury_format_pred_mode_decl_2(VarSet, PredName, Modes, MaybeDet,
		Context, ":- ", ".\n", "", String).

:- pred mercury_format_pred_mode_decl_2(inst_varset::in, sym_name::in,
	list(mode)::in, maybe(determinism)::in, prog_context::in,
	string::in, string::in, U::di, U::uo) is det <= output(U).

mercury_format_pred_mode_decl_2(VarSet, PredName, Modes, MaybeDet, Context,
		StartString, Separator) -->
	add_string(StartString),
	add_string("mode "),
	mercury_format_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		Context),
	add_string(Separator).

mercury_output_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		Context) -->
	mercury_format_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		Context).

mercury_pred_mode_subdecl_to_string(VarSet, PredName, Modes, MaybeDet, Context)
		= String :-
	mercury_format_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		Context, "", String).

:- pred mercury_format_pred_mode_subdecl(inst_varset::in, sym_name::in,
	list(mode)::in, maybe(determinism)::in, prog_context::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pred_mode_subdecl(VarSet, PredName, Modes, MaybeDet,
		_Context) -->
	(
		{ Modes = [_|_] },
		mercury_format_sym_name(PredName),
		add_string("("),
		mercury_format_mode_list(Modes, VarSet),
		add_string(")")
	;
		{ Modes = [] },
		mercury_format_bracketed_sym_name(PredName)
	),
	mercury_format_det_annotation(MaybeDet).

	% Output a mode declaration for a function.

mercury_output_func_mode_decl(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context) -->
	mercury_format_func_mode_decl_2(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context, ":- ", ".\n").

mercury_func_mode_decl_to_string(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context) = String :-
	mercury_format_func_mode_decl_2(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context, ":- ", ".\n", "", String).

:- pred mercury_format_func_mode_decl_2(inst_varset::in, sym_name::in,
	list(mode)::in, (mode)::in, maybe(determinism)::in, prog_context::in,
	string::in, string::in, U::di, U::uo) is det <= output(U).

mercury_format_func_mode_decl_2(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context, StartString, Separator) -->
	add_string(StartString),
	add_string("mode "),
	mercury_format_func_mode_subdecl(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context),
	add_string(Separator).

mercury_output_func_mode_subdecl(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context) -->
	mercury_format_func_mode_subdecl(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context).

mercury_func_mode_subdecl_to_string(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Context) = String :-
	mercury_format_func_mode_subdecl(VarSet, FuncName, Modes, RetMode,
		MaybeDet, Context, "", String).

:- pred mercury_format_func_mode_subdecl(inst_varset::in, sym_name::in,
	list(mode)::in, (mode)::in, maybe(determinism)::in, prog_context::in,
	U::di, U::uo) is det <= output(U).

mercury_format_func_mode_subdecl(VarSet, FuncName, Modes, RetMode, MaybeDet,
		_Context) -->
	(
		{ Modes = [_|_] },
		mercury_format_sym_name(FuncName),
		add_string("("),
		mercury_format_mode_list(Modes, VarSet),
		add_string(")")
	;
		{ Modes = [] },
		mercury_format_bracketed_sym_name(FuncName)
	),
	add_string(" = "),
	mercury_format_mode(RetMode, VarSet),
	mercury_format_det_annotation(MaybeDet).

:- pred mercury_format_det_annotation(maybe(determinism)::in, U::di, U::uo)
	is det <= output(U).

mercury_format_det_annotation(MaybeDet) -->
	(
		{ MaybeDet = no },
		[]
	;
		{ MaybeDet = yes(Det) },
		add_string(" is "),
		add_string(mercury_det_to_string(Det))
	).

mercury_output_det(Detism) -->
	mercury_format_det(Detism).

mercury_det_to_string(det) = "det".
mercury_det_to_string(semidet) = "semidet".
mercury_det_to_string(nondet) = "nondet".
mercury_det_to_string(multidet) = "multi".
mercury_det_to_string(cc_multidet) = "cc_multi".
mercury_det_to_string(cc_nondet) = "cc_nondet".
mercury_det_to_string(failure) = "failure".
mercury_det_to_string(erroneous) = "erroneous".

:- pred mercury_format_det(determinism::in,
	U::di, U::uo) is det <= output(U).

mercury_format_det(Detism) -->
	add_string(mercury_det_to_string(Detism)).

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
		mercury_format_term(Arg, VarSet, no),
		mercury_format_remaining_terms(Args0, VarSet, no),
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
		mercury_format_term(Arg, VarSet, no),
		mercury_format_remaining_terms(Args0, VarSet, no),
		io__write_string(")")
	;
		[]
	),
	io__write_string(" = "),
	(
		{ Body = true - _Context0 }
	->
		mercury_format_term(Result, VarSet, no, next_to_graphic_token)
	;
		mercury_format_term(Result, VarSet, no),
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
	{ Indent1 = Indent + 1 },
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
	{ Indent1 = Indent + 1 },
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
		{ Indent1 = Indent + 1 },
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
		{ Indent1 = Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_goal(Goal, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

mercury_output_goal_2(if_then_else(Vars, A, B, C), VarSet, Indent) -->
	io__write_string("(if"),
	mercury_output_some(Vars, VarSet),
	{ Indent1 = Indent + 1 },
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
	{ Indent1 = Indent + 1 },
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
	{ Indent1 = Indent + 1 },
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
	{ Indent1 = Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_par_conj(B, VarSet, Indent),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal_2((A;B), VarSet, Indent) -->
	io__write_string("("),
	{ Indent1 = Indent + 1 },
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
	{ Indent1 = Indent + 1 },
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
	{ Indent1 = Indent + 1 },
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

mercury_output_pragma_foreign_decl(Lang, ForeignDeclString) -->
	mercury_format_pragma_foreign_decl(Lang, ForeignDeclString).

mercury_pragma_foreign_decl_to_string(Lang, ForeignDeclString) = String :-
	mercury_format_pragma_foreign_decl(Lang, ForeignDeclString,
		"", String).

:- pred mercury_format_pragma_foreign_decl(foreign_language::in, string::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_decl(Lang, ForeignDeclString) -->
	add_string(":- pragma foreign_decl("),
	mercury_format_foreign_language_string(Lang),
	add_string(", "),
	mercury_format_foreign_code_string(ForeignDeclString),
	add_string(").\n").

mercury_output_foreign_language_string(Lang) -->
	mercury_format_foreign_language_string(Lang).

mercury_foreign_language_to_string(Lang) = String :- 
	mercury_format_foreign_language_string(Lang, "", String).

:- pred mercury_format_foreign_language_string(foreign_language::in,
	U::di, U::uo) is det <= output(U).

mercury_format_foreign_language_string(Lang) -->
	add_string("""" ++ foreign_language_string(Lang) ++ """").

mercury_output_pragma_foreign_import_module(Lang, ModuleName) -->
	io__write_string(":- pragma foreign_import_module("),
	mercury_format_foreign_language_string(Lang),
	io__write_string(", "),
	mercury_output_bracketed_sym_name(ModuleName,
		not_next_to_graphic_token),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

% The code here is similar to the code for term_io__quote_string,
% but \n and \t are output directly, rather than escaped.
% Any changes here may require corresponding changes to term_io and vice versa.

:- pred mercury_format_foreign_code_string(string::in, 
	U::di, U::uo) is det <= output(U).

mercury_format_foreign_code_string(S) -->
	add_string(""""),
	mercury_format_escaped_string(S),
	add_string("""").

:- pred mercury_format_escaped_string(string::in,
	U::di, U::uo) is det <= output(U).

mercury_format_escaped_string(String) -->
	string__foldl(mercury_format_escaped_char, String).

:- pred mercury_format_escaped_char(char::in,
	U::di, U::uo) is det <= output(U).

mercury_format_escaped_char(Char) -->
	( { escape_special_char(Char, QuoteChar) } ->
		add_char('\\'),
		add_char(QuoteChar)
	; { mercury_is_source_char(Char) } ->
		add_char(Char)
	;
		{ mercury_escape_char(Char, String) },
		add_string(String)
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
	% after a backslash in Mercury foreign_code string literals 
	% represent Char.

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

	% Output the given foreign_body_code declaration
:- pred mercury_output_pragma_foreign_body_code(foreign_language, 
		string, io__state, io__state).
:- mode mercury_output_pragma_foreign_body_code(in, in, di, uo) is det.

mercury_output_pragma_foreign_body_code(Lang, ForeignCodeString) -->
	io__write_string(":- pragma foreign_code("),
	mercury_format_foreign_language_string(Lang),
	io__write_string(", "),
	mercury_format_foreign_code_string(ForeignCodeString),
	io__write_string(").\n").

%-----------------------------------------------------------------------------%

mercury_output_pragma_foreign_code(Attributes, PredName, PredOrFunc, Vars0,
		VarSet, PragmaCode) -->
	mercury_format_pragma_foreign_code(Attributes, PredName, PredOrFunc,
		Vars0, VarSet, PragmaCode).

mercury_pragma_foreign_code_to_string(Attributes, PredName, PredOrFunc, Vars0,
		VarSet, PragmaCode) = String :-
	mercury_format_pragma_foreign_code(Attributes, PredName, PredOrFunc,
		Vars0, VarSet, PragmaCode, "", String).

:- pred mercury_format_pragma_foreign_code(pragma_foreign_proc_attributes::in,
	sym_name::in, pred_or_func::in, list(pragma_var)::in, prog_varset::in,
	pragma_foreign_code_impl::in, U::di, U::uo) is det <= output(U).

	% Output the given pragma foreign_code declaration
mercury_format_pragma_foreign_code(Attributes, PredName, PredOrFunc, Vars0,
		VarSet, PragmaCode) -->
	(
		{ PragmaCode = import(C_Function, _, _, _) },
		% The predicate or function arguments in a `:- pragma import'
		% declaration are not named.
		{ ImportModes = list__map(
			(func(pragma_var(_, _, ImportMode)) = ImportMode),
			Vars0) },

		mercury_format_pragma_import(PredName, PredOrFunc,
			ImportModes, Attributes, C_Function)
	;
		{ PragmaCode = ordinary(_, _) },
		mercury_format_pragma_foreign_code_2(Attributes, PredName,
			PredOrFunc, Vars0, VarSet, PragmaCode)
	;
		{ PragmaCode = nondet(_, _, _, _, _, _, _, _, _) },
		mercury_format_pragma_foreign_code_2(Attributes, PredName,
			PredOrFunc, Vars0, VarSet, PragmaCode)
	).

:- pred mercury_format_pragma_foreign_code_2(
	pragma_foreign_proc_attributes::in, sym_name::in, pred_or_func::in,
	list(pragma_var)::in, prog_varset::in, pragma_foreign_code_impl::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_code_2(Attributes, PredName, PredOrFunc, Vars0,
		VarSet, PragmaCode) -->
	add_string(":- pragma foreign_proc("),
	{ foreign_language(Attributes, Lang) },
	mercury_format_foreign_language_string(Lang),
	add_string(", "),
	mercury_format_sym_name(PredName),
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
		add_string("("),
		mercury_format_pragma_foreign_code_vars(Vars, VarSet),
		add_string(")")
	),
	(
		{ PredOrFunc = predicate }
	;
		{ PredOrFunc = function },
		add_string(" = ("),
		mercury_format_pragma_foreign_code_vars(ResultVars, VarSet),
		add_string(")")
	),
	add_string(", "),
	mercury_format_pragma_foreign_attributes(Attributes),
	add_string(", "),
	(
		{ PragmaCode = ordinary(C_Code, _) },
		mercury_format_foreign_code_string(C_Code)
	;
		{ PragmaCode = nondet(Fields, _, First, _,
			Later, _, Treat, Shared, _) },
		add_string("local_vars("),
		mercury_format_foreign_code_string(Fields),
		add_string("), "),
		add_string("first_code("),
		mercury_format_foreign_code_string(First),
		add_string("), "),
		add_string("retry_code("),
		mercury_format_foreign_code_string(Later),
		add_string("), "),
		(
			{ Treat = share },
			add_string("shared_code(")
		;
			{ Treat = duplicate },
			add_string("duplicated_code(")
		;
			{ Treat = automatic },
			add_string("common_code(")
		),
		mercury_format_foreign_code_string(Shared),
		add_string(")")
	;
		{ PragmaCode = import(_, _, _, _) },
		% This should be handle in mercury_output_pragma_foreign_code.
		{ error("mercury_output_pragma_foreign_code_2") }
	),
	add_string(").\n").

%-----------------------------------------------------------------------------%

	% Output the varnames of the pragma vars
:- pred mercury_format_pragma_foreign_code_vars(list(pragma_var)::in,
	prog_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_code_vars([], _) --> [].
mercury_format_pragma_foreign_code_vars([V|Vars], VarSet) -->
	{ V = pragma_var(_Var, VarName, Mode) },
	add_string(VarName),
	add_string(" :: "),
		% XXX Fake the inst varset
	{ varset__init(InstVarSet) },
	mercury_format_mode(Mode, InstVarSet),
	( { Vars = [] } ->
		[]
	;
		add_string(", ")
	),
	mercury_format_pragma_foreign_code_vars(Vars, VarSet).

%-----------------------------------------------------------------------------%

mercury_output_pragma_type_spec(Pragma, AppendVarnums) -->
	{ Pragma = type_spec(PredName, SpecName, Arity,
		MaybePredOrFunc, MaybeModes, Subst, VarSet, _) },
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
	io__write_list(Subst, ", ",
		mercury_output_type_subst(VarSet, AppendVarnums)),
	io__write_string("), "),
	mercury_output_bracketed_sym_name(SpecName, not_next_to_graphic_token),
	io__write_string(").\n").
	
:- pred mercury_output_type_subst(tvarset, bool, pair(tvar, type),	
		io__state, io__state).
:- mode mercury_output_type_subst(in, in, in, di, uo) is det.

mercury_output_type_subst(VarSet, AppendVarnums, Var - Type) -->
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string(" = "),
	mercury_output_term(Type, VarSet, AppendVarnums).

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
	mercury_format_int_list(UnusedArgs),
	io__write_string("]).\n").

:- pred mercury_format_int_list(list(int)::in,
	U::di, U::uo) is det <= output(U).

mercury_format_int_list([]) --> [].
mercury_format_int_list([First | Rest]) -->
	add_int(First),
	mercury_format_int_list_2(Rest).

:- pred mercury_format_int_list_2(list(int)::in,
	U::di, U::uo) is det <= output(U).

mercury_format_int_list_2([]) --> [].
mercury_format_int_list_2([First | Rest]) -->
	add_string(", "),
	add_int(First),
	mercury_format_int_list_2(Rest).

%-----------------------------------------------------------------------------%

mercury_output_pragma_decl(PredName, Arity, PredOrFunc, PragmaName) -->
	mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName).

mercury_pragma_decl_to_string(PredName, Arity, PredOrFunc, PragmaName)
		= String :-
	mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName,
		"", String).

:- pred mercury_format_pragma_decl(sym_name::in, int::in, pred_or_func::in,
	string::in, U::di, U::uo) is det <= output(U).

mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName) -->
	{ PredOrFunc = predicate,
		DeclaredArity = Arity
	; PredOrFunc = function,
		DeclaredArity is Arity - 1
	},
	add_string(":- pragma "),
	add_string(PragmaName),
	add_string("("),
	mercury_format_bracketed_sym_name(PredName, next_to_graphic_token),
	add_string("/"),
	add_int(DeclaredArity),
	add_string(").\n").

%-----------------------------------------------------------------------------%

:- pred mercury_format_pragma_import(sym_name::in, pred_or_func::in,
	list(mode)::in, pragma_foreign_proc_attributes::in, string::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_import(Name, PredOrFunc, ModeList, Attributes,
		C_Function) -->
	{ varset__init(Varset) }, % the varset isn't really used.
	add_string(":- pragma import("),
	mercury_format_sym_name(Name),
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ModeList, ArgModes, RetMode) },
		add_string("("),
		mercury_format_mode_list(ArgModes, Varset),
		add_string(") = "),
		mercury_format_mode(RetMode, Varset)
	;
		{ PredOrFunc = predicate },
		add_string("("),
		mercury_format_mode_list(ModeList, Varset),
		add_string(")")
	),
	add_string(", "),
	mercury_format_pragma_foreign_attributes(Attributes),
	add_string(", """),
	add_string(C_Function),
	add_string(""").\n").

:- pred mercury_format_pragma_export(sym_name::in, pred_or_func::in,
	list(mode)::in, string::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_export(Name, PredOrFunc, ModeList, C_Function) -->
	{ varset__init(Varset) }, % the varset isn't really used.
	add_string(":- pragma export("),
	mercury_format_sym_name(Name),
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ModeList, ArgModes, RetMode) },
		add_string("("),
		mercury_format_mode_list(ArgModes, Varset),
		add_string(") = "),
		mercury_format_mode(RetMode, Varset)
	;
		{ PredOrFunc = predicate },
		add_string("("),
		mercury_format_mode_list(ModeList, Varset),
		add_string(")")
	),
	add_string(", "),
	add_string(C_Function),
	add_string(").\n").

%-----------------------------------------------------------------------------%

:- pred mercury_format_pragma_fact_table(sym_name::in, arity::in, string::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_fact_table(Pred, Arity, FileName) -->
	add_string(":- pragma fact_table("),
	mercury_format_bracketed_sym_name(Pred, next_to_graphic_token),
	add_string("/"),
	add_int(Arity),
	add_string(", "),
	add_quoted_string(FileName),
	add_string(").\n").

%-----------------------------------------------------------------------------%

:- pred mercury_format_pragma_owner(sym_name::in, arity::in, string::in, 
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_owner(Pred, Arity, Owner) -->
	add_string(":- pragma owner("),
	mercury_format_bracketed_sym_name(Pred, next_to_graphic_token),
	add_string("/"),
	add_int(Arity),
	add_string(", "),
	add_quoted_atom(Owner),
	add_string(").\n").

:- pred mercury_format_pragma_index(sym_name::in, arity::in, index_spec::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_index(PredName, Arity, IndexSpec) -->
	add_string(":- pragma aditi_index("),
	mercury_format_bracketed_sym_name(PredName, next_to_graphic_token),
	add_string("/"),
	add_int(Arity),
	add_string(", "),
	mercury_format_index_spec(IndexSpec),
	add_string(").\n").

mercury_output_index_spec(IndexSpec) -->
	mercury_format_index_spec(IndexSpec).

:- pred mercury_format_index_spec(index_spec::in,
	U::di, U::uo) is det <= output(U).

mercury_format_index_spec(IndexSpec) -->
	{ IndexSpec = index_spec(IndexType, Attrs) },
	add_index_type(IndexType),
	add_string(", ["),
	mercury_format_int_list(Attrs),
	add_string("]").

%-----------------------------------------------------------------------------%

mercury_output_newline(Indent) -->
	io__write_char('\n'),
	mercury_format_tabs(Indent).

:- pred mercury_format_tabs(int::in,
	U::di, U::uo) is det <= output(U).

mercury_format_tabs(Indent) -->
	( { Indent = 0 } ->
		[]
	;
		add_string("\t"),
		{ Indent1 = Indent - 1 },
		mercury_format_tabs(Indent1)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_format_pragma_foreign_attributes(
	pragma_foreign_proc_attributes::in,
	U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_attributes(Attributes) -->
	% This is one case where it is a bad idea to use field
	% accessors.  
	{ attributes_to_strings(Attributes, AttrStrings) },
	add_string("["),
	add_list(AttrStrings, ", ", add_string),
	add_string("]").

%-----------------------------------------------------------------------------%

	% write a term to standard output.

mercury_output_term(Term, VarSet, AppendVarnums) -->
	mercury_output_term(Term, VarSet, AppendVarnums,
		not_next_to_graphic_token).

mercury_output_term(Term, VarSet, AppendVarnums, NextToGraphicToken) -->
	mercury_format_term(Term, VarSet, AppendVarnums, NextToGraphicToken).

mercury_term_to_string(Term, VarSet, AppendVarnums) =
	mercury_term_to_string(Term, VarSet, AppendVarnums,
		not_next_to_graphic_token).

mercury_term_to_string(Term, VarSet, AppendVarnums, NextToGraphicToken)
		= String :-
	mercury_format_term(Term, VarSet, AppendVarnums, NextToGraphicToken,
		"", String).

:- pred mercury_format_term(term(T)::in, varset(T)::in, bool::in,
	U::di, U::uo) is det <= output(U).

mercury_format_term(Term, VarSet, AppendVarnums) -->
	mercury_format_term(Term, VarSet, AppendVarnums,
		not_next_to_graphic_token).

:- pred mercury_format_term(term(T)::in, varset(T)::in, bool::in,
	needs_quotes::in, U::di, U::uo) is det <= output(U).

mercury_format_term(term__variable(Var), VarSet, AppendVarnums, _) -->
	mercury_format_var(Var, VarSet, AppendVarnums).
mercury_format_term(term__functor(Functor, Args, _), VarSet, AppendVarnums,
		NextToGraphicToken) -->
	(
	    	{ Functor = term__atom("") },
		{ Args = [F, X | Xs] }
	->
		mercury_format_term(F, VarSet, AppendVarnums,
			NextToGraphicToken),
		add_string("("),
		mercury_format_term(X, VarSet, AppendVarnums),
		mercury_format_remaining_terms(Xs, VarSet, AppendVarnums),
		add_string(")")
	;
	    	{ Functor = term__atom("[|]") },
		{ Args = [X, Xs] }
	->
		add_string("["),
		mercury_format_term(X, VarSet, AppendVarnums),
		mercury_format_list_args(Xs, VarSet, AppendVarnums),
		add_string("]")
	;
		{ Functor = term__atom("{}") },
		{ Args = [X] }
	->
		% A unary tuple is usually a DCG escape,
		% so add some extra space.
		add_string("{ "),
		mercury_format_term(X, VarSet, AppendVarnums),
		add_string(" }")
	;
		{ Functor = term__atom("{}") },
		{ Args = [X | Xs] }
	->
		add_string("{"),
		mercury_format_term(X, VarSet, AppendVarnums),
		mercury_format_remaining_terms(Xs, VarSet, AppendVarnums),
		add_string("}")
	;
		{ Args = [PrefixArg] },
		{ Functor = term__atom(FunctorName) },
		{ mercury_unary_prefix_op(FunctorName) }
	->
		add_string("("),
		add_string(FunctorName),
		add_string(" "),
		mercury_format_term(PrefixArg, VarSet, AppendVarnums),
		add_string(")")
	;
		{ Args = [PostfixArg] },
		{ Functor = term__atom(FunctorName) },
		{ mercury_unary_postfix_op(FunctorName) }
	->
		add_string("("),
		mercury_format_term(PostfixArg, VarSet, AppendVarnums),
		add_string(" "),
		add_string(FunctorName),
		add_string(")")
	;
		{ Args = [Arg1, Arg2] },
		{ Functor = term__atom(FunctorName) },
		{ mercury_infix_op(FunctorName) }
	->
		add_string("("),
		( { FunctorName = ":" } ->
			mercury_format_term(Arg1, VarSet, AppendVarnums,
				next_to_graphic_token),
			add_string(":"),
			mercury_format_term(Arg2, VarSet, AppendVarnums,
				next_to_graphic_token)
		;
			mercury_format_term(Arg1, VarSet, AppendVarnums,
				not_next_to_graphic_token),
			add_string(" "),
			add_string(FunctorName),
			add_string(" "),
			mercury_format_term(Arg2, VarSet, AppendVarnums,
				not_next_to_graphic_token)
		),
		add_string(")")
	;
		{ Args = [Y | Ys] }
	->
		mercury_format_constant(Functor, NextToGraphicToken),
		add_string("("),
		mercury_format_term(Y, VarSet, AppendVarnums),
		mercury_format_remaining_terms(Ys, VarSet, AppendVarnums),
		add_string(")")
	;
		mercury_format_bracketed_constant(Functor, NextToGraphicToken)
	).

:- pred mercury_format_list_args(term(T)::in, varset(T)::in, bool::in,
	U::di, U::uo) is det <= output(U).

mercury_format_list_args(Term, VarSet, AppendVarnums) -->
	(
	    	{ Term = term__functor(term__atom("[|]"), Args, _) },
		{ Args = [X, Xs] }
	->
		add_string(", "),
		mercury_format_term(X, VarSet, AppendVarnums),
		mercury_format_list_args(Xs, VarSet, AppendVarnums)
	;
		{ Term = term__functor(term__atom("[]"), [], _) }
	->
		[]
	;
		add_string(" | "),
		mercury_format_term(Term, VarSet, AppendVarnums)
	).

:- pred mercury_format_remaining_terms(list(term(T))::in, varset(T)::in,
	bool::in, U::di, U::uo) is det <= output(U).

mercury_format_remaining_terms([], _VarSet, _AppendVarnums) --> [].
mercury_format_remaining_terms([Term | Terms], VarSet, AppendVarnums) -->
	add_string(", "),
	mercury_format_term(Term, VarSet, AppendVarnums),
	mercury_format_remaining_terms(Terms, VarSet, AppendVarnums).

	% output a comma-separated list of variables

mercury_output_vars(Vars, VarSet, AppendVarnum) -->
	mercury_format_vars(Vars, VarSet, AppendVarnum).

mercury_vars_to_string(Vars, VarSet, AppendVarnum) = String :-
	mercury_format_vars(Vars, VarSet, AppendVarnum, "", String).

:- pred mercury_format_vars(list(var(T))::in, varset(T)::in,
	bool::in, U::di, U::uo) is det <= output(U).

mercury_format_vars([], _VarSet, _AppendVarnum) --> [].
mercury_format_vars([Var | Vars], VarSet, AppendVarnum) -->
	mercury_format_var(Var, VarSet, AppendVarnum),
	mercury_format_vars_2(Vars, VarSet, AppendVarnum).

:- pred mercury_format_vars_2(list(var(T))::in, varset(T)::in,
	bool::in, U::di, U::uo) is det <= output(U).

mercury_format_vars_2([], _VarSet, _AppendVarnum) --> [].
mercury_format_vars_2([Var | Vars], VarSet, AppendVarnum) -->
	add_string(", "),
	mercury_format_var(Var, VarSet, AppendVarnum),
	mercury_format_vars_2(Vars, VarSet, AppendVarnum).

	% Output a single variable.
	% Variables that didn't have names are given the name "V_<n>"
	% where <n> is there variable id.
	% Variables whose name originally started with `V_' have their
	% name changed to start with `V__' to avoid name clashes.

mercury_output_var(Var, VarSet, AppendVarnum) -->
	mercury_format_var(Var, VarSet, AppendVarnum).

mercury_var_to_string(Var, VarSet, AppendVarnum) = String :-
	mercury_format_var(Var, VarSet, AppendVarnum, "", String).

:- pred mercury_format_var(var(T)::in, varset(T)::in,
	bool::in, U::di, U::uo) is det <= output(U).

mercury_format_var(Var, VarSet, AppendVarnum) -->
	(
		{ varset__search_name(VarSet, Var, Name) }
	->
		{ mercury_convert_var_name(Name, ConvertedName) },
		add_string(ConvertedName),
		(
			{ AppendVarnum = yes },
			{ term__var_to_int(Var, VarNum) },
			add_string("_"),
			add_int(VarNum)
		;
			{ AppendVarnum = no }
		)
	;
		{ term__var_to_int(Var, Id) },
		{ string__int_to_string(Id, Num) },
		{ string__append("V_", Num, VarName) },
		add_string(VarName)
	).

:- pred mercury_format_bracketed_constant(const::in, U::di, U::uo) is det
	<= output(U).

mercury_format_bracketed_constant(Const) -->
	mercury_format_bracketed_constant(Const, not_next_to_graphic_token).

:- pred mercury_format_bracketed_constant(const::in, needs_quotes::in,
	U::di, U::uo) is det <= output(U).

mercury_format_bracketed_constant(Const, NextToGraphicToken) -->
	( { Const = term__atom(Op), mercury_op(Op) } ->
		add_string("("),
		add_quoted_atom(Op),
		add_string(")")
	;
		mercury_format_constant(Const, NextToGraphicToken)
	).

:- pred mercury_format_constant(const::in, needs_quotes::in,
	U::di, U::uo) is det <= output(U).

mercury_format_constant(Const, NextToGraphicToken) -->
	( { Const = term__atom(Atom) } ->
		mercury_format_quoted_atom(Atom, NextToGraphicToken)
	;
		add_constant(Const)
	).

:- pred mercury_format_bracketed_atom(string::in, needs_quotes::in,
	U::di, U::uo) is det <= output(U).

mercury_format_bracketed_atom(Name, NextToGraphicToken) -->
	( { mercury_op(Name) } ->
		add_string("("),
		add_quoted_atom(Name),
		add_string(")")
	;
		mercury_format_quoted_atom(Name, NextToGraphicToken)
	).

	%
	% Use mercury_output_bracketed_sym_name when the sym_name has
	% no arguments, otherwise use mercury_output_sym_name.
	%

:- pred mercury_output_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_sym_name(in, di, uo) is det.

mercury_output_sym_name(SymName) -->
	mercury_output_sym_name(SymName, not_next_to_graphic_token).

:- pred mercury_output_sym_name(sym_name, needs_quotes, io__state, io__state).
:- mode mercury_output_sym_name(in, in, di, uo) is det.

mercury_output_sym_name(Name, NextToGraphicToken) -->
	mercury_format_sym_name(Name, NextToGraphicToken).

mercury_output_bracketed_sym_name(SymName) -->
	mercury_output_bracketed_sym_name(SymName, not_next_to_graphic_token).

mercury_output_bracketed_sym_name(Name, NextToGraphicToken) -->
	mercury_format_bracketed_sym_name(Name, NextToGraphicToken).

:- pred mercury_format_bracketed_sym_name(sym_name::in,
	U::di, U::uo) is det <= output(U).

mercury_format_bracketed_sym_name(Name) -->
	mercury_format_bracketed_sym_name(Name, not_next_to_graphic_token).

:- pred mercury_format_bracketed_sym_name(sym_name::in, needs_quotes::in,
	U::di, U::uo) is det <= output(U).

mercury_format_bracketed_sym_name(Name, NextToGraphicToken) -->
	(
		{ Name = qualified(ModuleName, Name2) },
		add_string("("),
		mercury_format_bracketed_sym_name(ModuleName,
			next_to_graphic_token),
		add_string(":"),
		mercury_format_bracketed_atom(Name2, next_to_graphic_token),
		add_string(")")
	;
		{ Name = unqualified(Name2) },
		mercury_format_bracketed_atom(Name2, NextToGraphicToken)
	).

:- pred mercury_format_sym_name(sym_name::in, U::di, U::uo)
	is det <= output(U).

mercury_format_sym_name(SymName) -->
	mercury_format_sym_name(SymName, not_next_to_graphic_token).

:- pred mercury_format_sym_name(sym_name::in, needs_quotes::in, U::di, U::uo)
	is det <= output(U).

mercury_format_sym_name(Name, NextToGraphicToken) -->
	(
		{ Name = qualified(ModuleName, PredName) },
		mercury_format_bracketed_sym_name(ModuleName,
			next_to_graphic_token),
		add_string(":"),
		mercury_format_quoted_atom(PredName, next_to_graphic_token)
	;
		{ Name = unqualified(PredName) },
		mercury_format_quoted_atom(PredName, NextToGraphicToken)
	).

:- pred mercury_quote_atom(string, needs_quotes, io__state, io__state).
:- mode mercury_quote_atom(in, in, di, uo) is det.

mercury_quote_atom(Name, NextToGraphicToken) -->
	mercury_format_quoted_atom(Name, NextToGraphicToken).

:- func mercury_quoted_atom_to_string(string, needs_quotes) = string.

mercury_quoted_atom_to_string(Name, NextToGraphicToken) = String :-
	mercury_format_quoted_atom(Name, NextToGraphicToken, "", String).

:- pred mercury_format_quoted_atom(string::in, needs_quotes::in, U::di, U::uo)
	is det <= output(U).

mercury_format_quoted_atom(Name, NextToGraphicToken) -->
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
		add_string("'"),
		add_escaped_string(Name),
		add_string("'")
	;
		add_quoted_atom(Name)
	).

%-----------------------------------------------------------------------------%

	% Predicates to test whether a functor is a Mercury operator

:- pred mercury_op(string).
:- mode mercury_op(in) is semidet.

mercury_op(Op) :-
	ops__lookup_op(ops__init_mercury_op_table, Op).

:- pred mercury_binary_prefix_op(string).
:- mode mercury_binary_prefix_op(in) is semidet.

mercury_binary_prefix_op(Op) :-
	ops__lookup_binary_prefix_op(ops__init_mercury_op_table, Op, _, _, _).

:- pred mercury_infix_op(string).
:- mode mercury_infix_op(in) is semidet.

mercury_infix_op(Op) :-
	ops__lookup_infix_op(ops__init_mercury_op_table, Op, _, _, _).

:- pred mercury_unary_prefix_op(string).
:- mode mercury_unary_prefix_op(in) is semidet.

mercury_unary_prefix_op(Op) :-
	ops__lookup_prefix_op(ops__init_mercury_op_table, Op, _, _).

:- pred mercury_unary_postfix_op(string).
:- mode mercury_unary_postfix_op(in) is semidet.

mercury_unary_postfix_op(Op) :-
	ops__lookup_postfix_op(ops__init_mercury_op_table, Op, _, _).

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

:- pred maybe_unqualify_sym_name(bool, sym_name, sym_name).
:- mode maybe_unqualify_sym_name(in, in, out) is det.

maybe_unqualify_sym_name(no, Name, Name).
maybe_unqualify_sym_name(yes, Name0, unqualified(Name)) :-
	unqualify_name(Name0, Name).

%-----------------------------------------------------------------------------%

% This is the typeclass mentioned in the long comment at the top of the module.

:- typeclass output(U) where [
	pred add_string(string::in, U::di, U::uo) is det,
	pred add_strings(list(string)::in, U::di, U::uo) is det,
	pred add_char(char::in, U::di, U::uo) is det,
	pred add_int(int::in, U::di, U::uo) is det,
	pred add_float(float::in, U::di, U::uo) is det,
	pred add_purity_prefix(purity::in, U::di, U::uo) is det,
	pred add_quoted_atom(string::in, U::di, U::uo) is det,
	pred add_quoted_string(string::in, U::di, U::uo) is det,
	pred add_constant(const::in, U::di, U::uo) is det,
	pred add_class_id(class_id::in, U::di, U::uo) is det,
	pred add_eval_method(eval_method::in, U::di, U::uo) is det,
	pred add_lambda_eval_method(lambda_eval_method::in, U::di, U::uo)
		is det,
	pred add_index_type(index_type::in, U::di, U::uo) is det,
	pred add_escaped_string(string::in, U::di, U::uo) is det,
	pred add_format(string::in, list(io__poly_type)::in,
		U::di, U::uo) is det,
	pred add_list(list(T)::in, string::in,
		pred(T, U, U)::pred(in, di, uo) is det,
		U::di, U::uo) is det
].

:- instance output(io__state) where [
	pred(add_string/3) is io__write_string,
	pred(add_strings/3) is io__write_strings,
	pred(add_char/3) is io__write_char,
	pred(add_int/3) is io__write_int,
	pred(add_float/3) is io__write_float,
	pred(add_purity_prefix/3) is write_purity_prefix,
	pred(add_quoted_atom/3) is term_io__quote_atom,
	pred(add_quoted_string/3) is term_io__quote_string,
	pred(add_constant/3) is term_io__write_constant,
	pred(add_class_id/3) is io__write,
	pred(add_eval_method/3) is io__write,
	pred(add_lambda_eval_method/3) is io__write,
	pred(add_index_type/3) is io__write,
	pred(add_escaped_string/3) is term_io__write_escaped_string,
	pred(add_format/4) is io__format,
	pred(add_list/5) is io__write_list
].

:- instance output(string) where [
	pred(add_string/3) is output_string,
	pred(add_strings/3) is output_strings,
	pred(add_char/3) is output_char,
	pred(add_int/3) is output_int,
	pred(add_float/3) is output_float,
	pred(add_purity_prefix/3) is output_purity_prefix,
	pred(add_quoted_atom/3) is output_quoted_atom,
	pred(add_quoted_string/3) is output_quoted_string,
	pred(add_constant/3) is output_constant,
	pred(add_class_id/3) is output_class_id,
	pred(add_eval_method/3) is output_eval_method,
	pred(add_lambda_eval_method/3) is output_lambda_eval_method,
	pred(add_index_type/3) is output_index_type,
	pred(add_escaped_string/3) is output_escaped_string,
	pred(add_format/4) is output_format,
	pred(add_list/5) is output_list
].

:- pred output_string(string::in, string::di, string::uo) is det.

output_string(S, Str0, Str) :-
	string__append(Str0, S, Str).

:- pred output_strings(list(string)::in, string::di, string::uo) is det.

output_strings(Strs, Str0, Str) :-
	string__append_list([Str0 | Strs], Str).

:- pred output_char(char::in, string::di, string::uo) is det.

output_char(C, Str0, Str) :-
	string__char_to_string(C, S),
	string__append(Str0, S, Str).

:- pred output_int(int::in, string::di, string::uo) is det.

output_int(I, Str0, Str) :-
	string__int_to_string(I, S),
	string__append(Str0, S, Str).

:- pred output_float(float::in, string::di, string::uo) is det.

output_float(F, Str0, Str) :-
	string__float_to_string(F, S),
	string__append(Str0, S, Str).

:- pred output_purity_prefix(purity::in, string::di, string::uo) is det.

output_purity_prefix(P, Str0, Str) :-
	S = purity_prefix_to_string(P),
	string__append(Str0, S, Str).

:- pred output_quoted_atom(string::in, string::di, string::uo) is det.

output_quoted_atom(A, Str0, Str) :-
	QA = term_io__quoted_atom(A),
	string__append(Str0, QA, Str).

:- pred output_quoted_string(string::in, string::di, string::uo) is det.

output_quoted_string(A, Str0, Str) :-
	QA = term_io__quoted_string(A),
	string__append(Str0, QA, Str).

:- pred output_constant(const::in, string::di, string::uo) is det.

output_constant(C, Str0, Str) :-
	CS = term_io__format_constant(C),
	string__append(Str0, CS, Str).

:- pred output_escaped_string(string::in, string::di, string::uo) is det.

output_escaped_string(S, Str0, Str) :-
	ES = term_io__escaped_string(S),
	string__append(Str0, ES, Str).

:- pred output_class_id(class_id::in, string::di, string::uo) is det.

output_class_id(class_id(Name, Arity)) -->
	output_string("class_id("),
	mercury_format_sym_name(Name),
	output_string(", "),
	output_int(Arity),
	output_string(")").

:- pred output_eval_method(eval_method::in, string::di, string::uo) is det.

output_eval_method(eval_normal) -->
	output_string("eval_normal").
output_eval_method(eval_loop_check) -->
	output_string("eval_loop_check").
output_eval_method(eval_memo) -->
	output_string("eval_memo").
output_eval_method(eval_table_io) -->
	output_string("eval_table_io").
output_eval_method(eval_minimal) -->
	output_string("eval_minimal").

:- pred output_lambda_eval_method(lambda_eval_method::in,
	string::di, string::uo) is det.

output_lambda_eval_method(normal) -->
	output_string("normal").
output_lambda_eval_method(aditi_top_down) -->
	output_string("aditi_top_down").
output_lambda_eval_method(aditi_bottom_up) -->
	output_string("aditi_bottom_up").

:- pred output_index_type(index_type::in, string::di, string::uo) is det.

output_index_type(unique_B_tree) -->
	output_string("unique_B_tree").
output_index_type(non_unique_B_tree) -->
	output_string("non_unique_B_tree").

:- pred output_format(string::in, list(io__poly_type)::in,
	string::di, string::uo) is det.

output_format(Format, Items, Str0, Str) :-
	S = string__format(Format, Items),
	string__append(Str0, S, Str).

:- pred output_list(list(T)::in, string::in,
	pred(T, string, string)::pred(in, di, uo) is det,
	string::di, string::uo) is det.

output_list([], _, _, Str, Str).
output_list([Item | Items], Sep, Pred, Str0, Str) :-
	Pred(Item, Str0, Str1),
	(
		Items = [],
		Str = Str1
	;
		Items = [_|_],
		output_string(Sep, Str1, Str2),
		output_list(Items, Sep, Pred, Str2, Str)
	).

%-----------------------------------------------------------------------------%
