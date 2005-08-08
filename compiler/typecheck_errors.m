%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck_errors.m.
% Main author: fjh.
%
% This file contains predicates to report errors and debugging messages for
% typechecking.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck_errors.

:- interface.

:- import_module check_hlds.typecheck_info.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type cons_error
	--->	foreign_type_constructor(type_ctor, hlds_type_defn)
	;	abstract_imported_type
	;	invalid_field_update(ctor_field_name, hlds_ctor_field_defn,
			tvarset, list(tvar))
	;	new_on_non_existential_type(type_ctor).

%-----------------------------------------------------------------------------%

:- pred report_pred_call_error(simple_call_id::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

:- pred report_no_clauses(string::in, pred_id::in, pred_info::in,
	module_info::in, io::di, io::uo) is det.

:- pred report_warning_too_much_overloading(typecheck_info::in,
	io::di, io::uo) is det.

:- pred report_error_unif_var_var(typecheck_info::in,
	prog_var::in, prog_var::in, type_assign_set::in, io::di, io::uo)
	is det.

:- pred report_error_lambda_var(typecheck_info::in, pred_or_func::in,
	lambda_eval_method::in, prog_var::in, list(prog_var)::in,
	type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_functor_type(typecheck_info::in,
	prog_var::in, list(cons_type_info)::in, cons_id::in, int::in,
	type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_functor_arg_types(typecheck_info::in, prog_var::in,
	list(cons_type_info)::in, cons_id::in, list(prog_var)::in,
	args_type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_var(typecheck_info::in, prog_var::in, (type)::in,
	type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_arg_var(typecheck_info::in, prog_var::in,
	args_type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_undef_cons(typecheck_info::in, list(cons_error)::in,
	cons_id::in, int::in, io::di, io::uo) is det.

:- pred report_ambiguity_error(typecheck_info::in,
	type_assign::in, type_assign::in, io::di, io::uo) is det.

:- pred report_unsatisfiable_constraints(type_assign_set::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

	% Write out the inferred `pred' or `func' declarations
	% for a list of predicates.  Don't write out the inferred types
	% for assertions.
	%
:- pred write_inference_messages(list(pred_id)::in, module_info::in,
	io::di, io::uo) is det.

	% Used for debugging typechecking.
	%
:- pred checkpoint(string::in, typecheck_info::in, typecheck_info::out,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

report_pred_call_error(PredCallId, !Info, !IO) :-
	PredCallId = PredOrFunc0 - SymName/_Arity,
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		predicate_table_search_pf_sym(PredicateTable,
			calls_are_fully_qualified(!.Info ^ pred_markers),
			PredOrFunc0, SymName, OtherIds),
		predicate_table_get_preds(PredicateTable, Preds),
		OtherIds \= []
	->
		typecheck_find_arities(Preds, OtherIds, Arities),
		report_error_pred_num_args(!.Info, PredCallId, Arities, !IO)
	;
		( PredOrFunc0 = predicate, PredOrFunc = function
		; PredOrFunc0 = function, PredOrFunc = predicate
		),
		predicate_table_search_pf_sym(PredicateTable,
			calls_are_fully_qualified(!.Info ^ pred_markers),
			PredOrFunc, SymName, OtherIds),
		OtherIds \= []
	->
		report_error_func_instead_of_pred(!.Info, PredOrFunc,
			PredCallId, !IO)
	;
		report_error_undef_pred(!.Info, PredCallId, !IO)
	),
	typecheck_info_set_found_error(yes, !Info).

:- pred typecheck_find_arities(pred_table::in, list(pred_id)::in,
	list(int)::out) is det.

typecheck_find_arities(_, [], []).
typecheck_find_arities(Preds, [PredId | PredIds], [Arity | Arities]) :-
	map__lookup(Preds, PredId, PredInfo),
	Arity = pred_info_orig_arity(PredInfo),
	typecheck_find_arities(Preds, PredIds, Arities).

:- pred report_error_pred_num_args(typecheck_info::in, simple_call_id::in,
	list(int)::in, io::di, io::uo) is det.

report_error_pred_num_args(Info, PredOrFunc - SymName/Arity, Arities, !IO) :-
	write_context_and_pred_id(Info, !IO),
	typecheck_info_get_context(Info, Context),
	prog_out__write_context(Context, !IO),
	io__write_string("  error: ", !IO),
	report_error_num_args(yes(PredOrFunc), Arity, Arities, !IO),
	io__nl(!IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  in call to ", !IO),
	prog_out__write_pred_or_func(PredOrFunc, !IO),
	io__write_string(" `", !IO),
	prog_out__write_sym_name(SymName, !IO),
	io__write_string("'.\n", !IO).

:- pred report_error_func_instead_of_pred(typecheck_info::in, pred_or_func::in,
	simple_call_id::in, io::di, io::uo) is det.

report_error_func_instead_of_pred(Info, PredOrFunc, PredCallId, !IO) :-
	report_error_undef_pred(Info, PredCallId, !IO),
	typecheck_info_get_context(Info, Context),
	PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
	(
		PredOrFunc = function,
		Pieces = [words("(There is a *" ++ PredOrFuncStr ++ "*"),
			words("with that name, however."), nl,
			words("Perhaps you forgot to add"),
			fixed("` = ...'?)")]
	;
		PredOrFunc = predicate,
		Pieces = [words("(There is a *" ++ PredOrFuncStr ++ "*"),
			words("with that name, however.)")]
	),
	write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- pred report_error_undef_pred(typecheck_info::in, simple_call_id::in,
	io::di, io::uo) is det.

report_error_undef_pred(Info, PredOrFunc - PredCallId, !IO) :-
	PredCallId = PredName/Arity,
	typecheck_info_get_context(Info, Context),
	write_typecheck_info_context(Info, !IO),
	(
		PredName = unqualified("->"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: `->' without `;'.\n", !IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		(
			VerboseErrors = yes,
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Note: the else part is not optional.\n",
				!IO),
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Every if-then must have an else.\n", !IO)
		;
			VerboseErrors = no
		)
	;
		PredName = unqualified("else"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: unmatched `else'.\n", !IO)
	;
		PredName = unqualified("if"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: `if' without `then' or `else'.\n",
			!IO)
	;
		PredName = unqualified("then"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: `then' without `if' or `else'.\n",
			!IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		(
			VerboseErrors = yes,
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Note: the `else' part is not optional.\n",
				!IO),
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Every if-then must have an `else'.\n", !IO)
		;
			VerboseErrors = no
		)
	;
		PredName = unqualified("apply"),
		Arity >= 1
	->
		report_error_apply_instead_of_pred(Info, !IO)
	;
		PredName = unqualified(PurityString),
		Arity = 1,
		( PurityString = "impure" ; PurityString = "semipure" )
	->
		io__write_string("  error: `", !IO),
		io__write_string(PurityString, !IO),
		io__write_string("' marker in an inappropriate place.\n", !IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		(
			VerboseErrors = yes,
			prog_out__write_context(Context, !IO),
			io__write_string("  Such markers only belong " ++
				"before predicate calls.\n", !IO)
		;
			VerboseErrors = no
		)
	;
		PredName = unqualified("some"),
		Arity = 2
	->
		io__write_string("  syntax error in existential " ++
			"quantification: first\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  argument of `some' should be " ++
			"a list of variables.\n", !IO)
	;
		io__write_string("  error: undefined ", !IO),
		write_simple_call_id(PredOrFunc - PredCallId, !IO),
		( PredName = qualified(ModuleQualifier, _) ->
			maybe_report_missing_import(Info, ModuleQualifier, !IO)
		;
			io__write_string(".\n", !IO)
		)
	).

:- pred report_error_apply_instead_of_pred(typecheck_info::in, io::di, io::uo)
	is det.

report_error_apply_instead_of_pred(Info, !IO) :-
	typecheck_info_get_context(Info, Context),
	Pieces1 = [words("error: the language construct `apply'"),
		words("should be used as an expression, not as a goal."), nl],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		Pieces2 = [words("Perhaps you forgot to add"),
			fixed("` = ...'?)"), nl,
			words("If you're trying to invoke"),
			words("a higher-order predicate,"),
			words("use `call', not `apply'."), nl,
			words("If you're trying to curry"),
			words("a higher-order function,"),
			words("use a forwarding function:"), nl,
			words("e.g. instead of "),
			fixed("`NewFunc = apply(OldFunc, X)'"),
			words("use"),
			fixed("`NewFunc = my_apply(OldFunc, X)'"), 
			words("where `my_apply' is defined"),
			words("with the appropriate arity, e.g."),
			fixed("`my_apply(Func, X, Y) :- apply(Func, X, Y).'")]
	;
		VerboseErrors = no,
		Pieces2 = []
	),
	write_error_pieces_not_first_line(Context, 0, Pieces1 ++ Pieces2, !IO).

%-----------------------------------------------------------------------------%

report_no_clauses(MessageKind, PredId, PredInfo, ModuleInfo, !IO) :-
	pred_info_context(PredInfo, Context),
	PredPieces = describe_one_pred_name(ModuleInfo,
		should_not_module_qualify, PredId),
	ErrorMsg = [words(MessageKind ++ ": no clauses for ") | PredPieces] ++
		[suffix(".")],
	error_util__write_error_pieces(Context, 0, ErrorMsg, !IO).

%-----------------------------------------------------------------------------%

report_warning_too_much_overloading(Info, !IO) :-
	typecheck_info_get_context(Info, Context),
	make_pred_id_preamble(Info, Preamble),
	SmallWarning = [fixed(Preamble),
		words("warning: highly ambiguous overloading.") ],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		VerboseWarning = [
			words("This may cause type-checking to be very"),
			words("slow. It may also make your code"),
			words("difficult to understand.") ],
		list__append(SmallWarning, VerboseWarning, Warning)
	;
		VerboseErrors = no,
		Warning = SmallWarning
	),
	error_util__report_warning(Context, 0, Warning, !IO).

%-----------------------------------------------------------------------------%

report_error_unif_var_var(Info, X, Y, TypeAssignSet, !IO) :-
	typecheck_info_get_context(Info, Context),
	typecheck_info_get_varset(Info, VarSet),
	typecheck_info_get_unify_context(Info, UnifyContext),

	write_context_and_pred_id(Info, !IO),
	hlds_out__write_unify_context(UnifyContext, Context, !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  type error in unification of variable `", !IO),
	mercury_output_var(X, VarSet, no, !IO),
	io__write_string("'\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  and variable `", !IO),
	mercury_output_var(Y, VarSet, no, !IO),
	io__write_string("'.\n", !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  `", !IO),
	mercury_output_var(X, VarSet, no, !IO),
	io__write_string("'", !IO),
	write_type_of_var(Info, Context, TypeAssignSet, X, !IO),
	io__write_string(",\n", !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  `", !IO),
	mercury_output_var(Y, VarSet, no, !IO),
	io__write_string("'", !IO),
	write_type_of_var(Info, Context, TypeAssignSet, Y, !IO),
	io__write_string(".\n", !IO),

	write_type_assign_set_msg(TypeAssignSet, VarSet, !IO).

report_error_lambda_var(Info, PredOrFunc, EvalMethod, Var, ArgVars,
		TypeAssignSet, !IO) :-
	typecheck_info_get_context(Info, Context),
	typecheck_info_get_varset(Info, VarSet),
	typecheck_info_get_unify_context(Info, UnifyContext),

	write_context_and_pred_id(Info, !IO),
	hlds_out__write_unify_context(UnifyContext, Context, !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  type error in unification of ", !IO),
	write_argument_name(VarSet, Var, !IO),
	io__write_string("\n", !IO),
	prog_out__write_context(Context, !IO),

	( EvalMethod = normal, EvalStr = ""
	; EvalMethod = (aditi_bottom_up), EvalStr = "aditi_bottom_up "
	),

	(
		PredOrFunc = predicate,
		io__write_string("  and `", !IO),
		io__write_string(EvalStr, !IO),
		io__write_string("pred(", !IO),
		mercury_output_vars(ArgVars, VarSet, no, !IO),
		io__write_string(") :- ...':\n", !IO)
	;
		PredOrFunc = function,
		pred_args_to_func_args(ArgVars, FuncArgs, RetVar),
		io__write_string("  and `", !IO),
		io__write_string(EvalStr, !IO),
		io__write_string("func(", !IO),
		mercury_output_vars(FuncArgs, VarSet, no, !IO),
		io__write_string(") = ", !IO),
		mercury_output_var(RetVar, VarSet, no, !IO),
		io__write_string(" :- ...':\n", !IO)
	),

	prog_out__write_context(Context, !IO),
	io__write_string("  ", !IO),
	write_argument_name(VarSet, Var, !IO),
	write_type_of_var(Info, Context, TypeAssignSet, Var, !IO),
	io__write_string(",\n", !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  lambda expression has type `", !IO),
	(
		PredOrFunc = predicate,
		io__write_string("pred", !IO),
		( ArgVars = [] ->
			true
		;
			io__write_string("(_", !IO),
			list__length(ArgVars, NumArgVars),
			NumArgVars1 = NumArgVars - 1,
			list__duplicate(NumArgVars1, ", _", Strings),
			io__write_strings(Strings, !IO),
			io__write_string(")", !IO)
		)
	;
		PredOrFunc = function,
		io__write_string("func", !IO),
		pred_args_to_func_args(ArgVars, FuncArgs2, _),
		( FuncArgs2 = [] ->
			true
		;
			io__write_string("(_", !IO),
			list__length(FuncArgs2, NumArgVars),
			NumArgVars1 = NumArgVars - 1,
			list__duplicate(NumArgVars1, ", _", Strings),
			io__write_strings(Strings, !IO),
			io__write_string(")", !IO)
		),
		io__write_string(" = _", !IO)
	),
	io__write_string("'.\n", !IO),
	write_type_assign_set_msg(TypeAssignSet, VarSet, !IO).

report_error_functor_type(Info, Var, ConsDefnList, Functor, Arity,
		TypeAssignSet, !IO) :-
	typecheck_info_get_context(Info, Context),
	typecheck_info_get_varset(Info, VarSet),
	typecheck_info_get_unify_context(Info, UnifyContext),

	write_context_and_pred_id(Info, !IO),
	hlds_out__write_unify_context(UnifyContext, Context, !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  type error in unification of ", !IO),
	write_argument_name(VarSet, Var, !IO),
	io__write_string("\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  and ", !IO),
	write_functor_name(Functor, Arity, !IO),
	io__write_string(".\n", !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  ", !IO),
	write_argument_name(VarSet, Var, !IO),
	write_type_of_var(Info, Context, TypeAssignSet, Var, !IO),
	io__write_string(",\n", !IO),

	prog_out__write_context(Context, !IO),
	io__write_string("  ", !IO),
	write_functor_name(Functor, Arity, !IO),
	write_type_of_functor(Functor, Arity, Context, ConsDefnList, !IO),
	io__write_string(".\n", !IO),

	write_type_assign_set_msg(TypeAssignSet, VarSet, !IO).

report_error_functor_arg_types(Info, Var, ConsDefnList, Functor, Args,
		ArgsTypeAssignSet) -->
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_varset(Info, VarSet) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	{ typecheck_info_get_module_info(Info, ModuleInfo) },
	{ list__length(Args, Arity) },

	write_context_and_pred_id(Info),
	hlds_out__write_unify_context(UnifyContext, Context),

	prog_out__write_context(Context),
	io__write_string("  in unification of "),
	write_argument_name(VarSet, Var),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  and term `"),
	{ strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor) },
	hlds_out__write_functor_cons_id(StrippedFunctor, Args, VarSet,
		ModuleInfo, no),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  type error in argument(s) of "),
	write_functor_name(StrippedFunctor, Arity),
	io__write_string(".\n"),

	{ ConsArgTypesSet = list__map(get_callee_arg_types,
		ArgsTypeAssignSet) },

	% If we know the type of the function symbol, and each argument
	% also has at most one possible type, then we prefer to print an
	% error message that mentions the actual and expected types of the
	% arguments only for the arguments in which the two types differ.
	(
		{ list__all_same(ConsArgTypesSet) },
		{ ConsArgTypesSet = [ConsArgTypes | _] },
		{ assoc_list__from_corresponding_lists(Args, ConsArgTypes,
			ArgExpTypes) },
		{ TypeAssigns = list__map(get_caller_arg_assign,
			ArgsTypeAssignSet) },
		{ find_mismatched_args(ArgExpTypes, TypeAssigns, 1,
			SimpleMismatches, ComplexMismatches, AllMismatches) },
		{ require(list__is_not_empty(AllMismatches),
			"report_error_functor_arg_types: no mismatches") },
		{ ComplexMismatches = [] }
	->
		report_mismatched_args(SimpleMismatches, yes, VarSet, Functor,
			Context)
	;
		% XXX If we can compute AllMismatches, then we should use it
		% to report which arguments are OK, and which are suspect.

		{ convert_args_type_assign_set(ArgsTypeAssignSet,
			TypeAssignSet) },

		%
		% For polymorphic data structures,
		% the type of `Var' (the functor's result type)
		% can affect the valid types for the arguments.
		%
		(
			% could the type of the functor be polymorphic?
			{ list__member(ConsDefn, ConsDefnList) },
			{ ConsDefn = cons_type_info(_, _, _, ConsArgTypes, _) },
			{ ConsArgTypes \= [] }
		->
			% if so, print out the type of `Var'
			prog_out__write_context(Context),
			io__write_string("  "),
			write_argument_name(VarSet, Var),
			write_type_of_var(Info, Context, TypeAssignSet, Var),
			io__write_string(",\n")
		;
			[]
		),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_functor_name(Functor, Arity),
		write_type_of_functor(Functor, Arity, Context, ConsDefnList),

		write_types_of_vars(Args, VarSet, Context, Info,
			TypeAssignSet),

		write_type_assign_set_msg(TypeAssignSet, VarSet)
	).

:- type mismatch_info
	--->	mismatch_info(
			int,		% argument number, starting from 1
			prog_var,	% variable in that position
			list(type_mismatch)
					% list of possible type mismatches
		).

:- type type_mismatch
	--->	type_mismatch(
			type,		% actual type of that variable
			type,		% expected type of that variable
			tvarset,	% the type vars in the expected
					% and expected types
			head_type_params % existentially quantified type vars
		).

:- pred find_mismatched_args(assoc_list(prog_var, type)::in,
	type_assign_set::in, int::in, list(mismatch_info)::out,
	list(mismatch_info)::out, list(mismatch_info)::out) is det.

find_mismatched_args([], _, _, [], [], []).
find_mismatched_args([Arg - ExpType | ArgExpTypes], TypeAssignSet, ArgNum0,
		SimpleMismatches, ComplexMismatches, AllMismatches) :-
	ArgNum1 = ArgNum0 + 1,
	find_mismatched_args(ArgExpTypes, TypeAssignSet, ArgNum1,
		SimpleMismatchesTail, ComplexMismatchesTail,
		AllMismatchesTail),
	get_type_stuff(TypeAssignSet, Arg, TypeStuffList),
	list__filter_map(substitute_types_check_match(ExpType), TypeStuffList,
		TypeMismatches0),
	list__sort_and_remove_dups(TypeMismatches0, TypeMismatches),
	(
		TypeMismatches = [],
		SimpleMismatches = SimpleMismatchesTail,
		ComplexMismatches = ComplexMismatchesTail,
		AllMismatches = AllMismatchesTail
	;
		TypeMismatches = [_],
		Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
		SimpleMismatches = [Mismatch | SimpleMismatchesTail],
		ComplexMismatches = ComplexMismatchesTail,
		AllMismatches = [Mismatch | AllMismatchesTail]
	;
		TypeMismatches = [_, _ | _],
		Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
		SimpleMismatches = SimpleMismatchesTail,
		ComplexMismatches = [Mismatch | ComplexMismatchesTail],
		AllMismatches = [Mismatch | AllMismatchesTail]
	).

:- pred substitute_types_check_match((type)::in, type_stuff::in,
	type_mismatch::out) is semidet.

substitute_types_check_match(ExpType, TypeStuff, TypeMismatch) :-
	TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, HeadTypeParams),
	term__apply_rec_substitution(ArgType, TypeBindings, FullArgType),
	term__apply_rec_substitution(ExpType, TypeBindings, FullExpType),
	(
		(
			% there is no mismatch if the actual type of the
			% argument is the same as the expected type
			identical_types(FullArgType, FullExpType)
		;
			% there is no mismatch if the actual type of the
			% argument has no constraints on it
			FullArgType = term__functor(term__atom("<any>"), [], _)
		)
	->
		fail
	;
		TypeMismatch = type_mismatch(FullArgType, FullExpType,
			TVarSet, HeadTypeParams)
	).

:- pred report_mismatched_args(list(mismatch_info)::in, bool::in,
	prog_varset::in, cons_id::in, prog_context::in, io::di, io::uo) is det.

report_mismatched_args([], _, _, _, _, !IO).
report_mismatched_args([Mismatch | Mismatches], First, VarSet, Functor,
		Context, !IO) :-
	Mismatch = mismatch_info(ArgNum, Var, TypeMismatches),
	( TypeMismatches = [TypeMismatch] ->
		TypeMismatch = type_mismatch(ActType, ExpType, TVarSet,
			HeadTypeParams)
	;
		error("report_mismatched_args: more than one type mismatch")
	),
	prog_out__write_context(Context, !IO),
	(
		% Handle higher-order syntax such as ''(F, A) specially:
		% output
		%	Functor (F) has type ...;
		%	argument 1 (A) has type ...
		% instead of
		%	Argument 1 (F) has type ...;
		%	argument 2 (A) has type ...
		Functor = cons(unqualified(""), Arity),
		Arity > 0
	->
		(
			First = yes,
			io__write_string("  Functor", !IO)
		;
			First = no,
			io__write_string("  argument ", !IO),
			io__write_int(ArgNum - 1, !IO)
		)
	;
		(
			First = yes,
			io__write_string("  Argument ", !IO)
		;
			First = no,
			io__write_string("  argument ", !IO)
		),
		io__write_int(ArgNum, !IO)
	),
	( varset__search_name(VarSet, Var, _) ->
		io__write_string(" (", !IO),
		mercury_output_var(Var, VarSet, no, !IO),
		io__write_string(")", !IO)
	;
		true
	),
	io__write_string(" has type `", !IO),
	output_type(ActType, TVarSet, HeadTypeParams, !IO),
	io__write_string("',\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  expected type was `", !IO),
	output_type(ExpType, TVarSet, HeadTypeParams, !IO),
	(
		Mismatches = [],
		io__write_string("'.\n", !IO)
	;
		Mismatches = [_ | _],
		io__write_string("';\n", !IO),
		report_mismatched_args(Mismatches, no, VarSet, Functor,
			Context, !IO)
	).

%-----------------------------------------------------------------------------%

report_error_var(Info, Var, Type, TypeAssignSet0) -->
	{ typecheck_info_get_pred_markers(Info, PredMarkers) },
	{ typecheck_info_get_called_predid(Info, CalledPredId) },
	{ typecheck_info_get_arg_num(Info, ArgNum) },
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	{ get_type_stuff(TypeAssignSet0, Var, TypeStuffList) },
	{ typecheck_info_get_varset(Info, VarSet) },
	write_context_and_pred_id(Info),
	write_call_context(Context, PredMarkers,
		CalledPredId, ArgNum, UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: "),
	( { TypeStuffList = [SingleTypeStuff] } ->
		write_argument_name(VarSet, Var),
		{ SingleTypeStuff = type_stuff(VType, TVarSet, TBinding,
			HeadTypeParams) },
		io__write_string(" has type `"),
		write_type_b(VType, TVarSet, TBinding, HeadTypeParams),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected type was `"),
		write_type_b(Type, TVarSet, TBinding, HeadTypeParams),
		io__write_string("'.\n")
	;
		io__write_string("type of "),
		write_argument_name(VarSet, Var),
		io__write_string(" does not match its expected type;\n"),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_argument_name(VarSet, Var),
		io__write_string(" has overloaded actual/expected types {\n"),

		write_var_type_stuff_list(Context, TypeStuffList, Type),
		io__write_string("\n"),

		prog_out__write_context(Context),
		io__write_string("  }.\n")
	),
	write_type_assign_set_msg(TypeAssignSet0, VarSet).

report_error_arg_var(Info, Var, ArgTypeAssignSet0) -->
	{ typecheck_info_get_pred_markers(Info, PredMarkers) },
	{ typecheck_info_get_called_predid(Info, CalledPredId) },
	{ typecheck_info_get_arg_num(Info, ArgNum) },
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	{ get_arg_type_stuff(ArgTypeAssignSet0, Var, ArgTypeStuffList) },
	{ typecheck_info_get_varset(Info, VarSet) },
	write_context_and_pred_id(Info),
	write_call_context(Context, PredMarkers, CalledPredId, ArgNum,
		UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: "),
	( { ArgTypeStuffList = [SingleArgTypeStuff] } ->
		write_argument_name(VarSet, Var),
		{ SingleArgTypeStuff = arg_type_stuff(Type0, VType0, TVarSet,
			HeadTypeParams) },
		io__write_string(" has type `"),
		output_type(VType0, TVarSet, HeadTypeParams),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected type was `"),
		output_type(Type0, TVarSet, HeadTypeParams),
		io__write_string("'.\n")
	;
		io__write_string("type of "),
		write_argument_name(VarSet, Var),
		io__write_string(" does not match its expected type;\n"),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_argument_name(VarSet, Var),
		io__write_string(" has overloaded actual/expected types {\n"),

		write_arg_type_stuff_list(Context, ArgTypeStuffList),
		io__write_string("\n"),

		prog_out__write_context(Context),
		io__write_string("  }.\n")
	),
	write_args_type_assign_set_msg(ArgTypeAssignSet0, VarSet).

%-----------------------------------------------------------------------------%

report_error_undef_cons(Info, ConsErrors, Functor, Arity) -->
	{ typecheck_info_get_pred_markers(Info, PredMarkers) },
	{ typecheck_info_get_called_predid(Info, CalledPredId) },
	{ typecheck_info_get_arg_num(Info, ArgNum) },
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	write_context_and_pred_id(Info),
	write_call_context(Context, PredMarkers, CalledPredId, ArgNum,
		UnifyContext),
	prog_out__write_context(Context),
	%
	% Check for some special cases, so that we can give
	% clearer error messages.
	%
	(
		{ Functor = cons(unqualified(Name), _) },
		{ language_builtin(Name, Arity) }
	->
		io__write_string("  error: the language construct "),
		hlds_out__write_cons_id(Functor),
		io__write_string(" should be\n"),
		prog_out__write_context(Context),
		io__write_string("  used as a goal, not as an expression.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
		"  If you are trying to use a goal as a boolean function,\n"),
			prog_out__write_context(Context),
			io__write_string(
		"  you should write `if <goal> then yes else no' instead.\n"),
			( { Name = "call" } ->
				prog_out__write_context(Context),
				io__write_string(
		"  If you are trying to invoke a higher-order\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  function, you should use `apply', not `call'.\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  If you're trying to curry a higher-order predicate,\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  see the ""Creating higher-order terms"" section of the\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  Mercury Language Reference Manual.\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  If you really are trying to use `call' as an expression\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  and not as an application of the language builtin\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  call/N, make sure that you have the arity correct, and\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  that the functor `call' is actually defined (if it is\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  defined in a separate module, check that the module is\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  correctly imported).\n")
			;
				[]
			)
		;
			[]
		)
	; { Functor = cons(unqualified("else"), 2) } ->
		io__write_string("  error: unmatched `else'.\n")
	; { Functor = cons(unqualified("if"), 2) } ->
		io__write_string("  error: `if' without `then' or `else'.\n")
	; { Functor = cons(unqualified("then"), 2) } ->
		io__write_string("  error: `then' without `if' or `else'.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
				"  Note: the `else' part is not optional.\n"),
			prog_out__write_context(Context),
			io__write_string(
				"  Every if-then must have an `else'.\n")
		;
			[]
		)
	; { Functor = cons(unqualified("->"), 2) } ->
		io__write_string("  error: `->' without `;'.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
				"  Note: the else part is not optional.\n"),
			prog_out__write_context(Context),
			io__write_string(
				"  Every if-then must have an else.\n")
		;
			[]
		)
	; { Functor = cons(unqualified("^"), 2) } ->
		io__write_string("  error: invalid use of field selection " ++
			"operator (`^').\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string("  This is probably some kind " ++
				"of syntax error.\n"),
			prog_out__write_context(Context),
			io__write_string("  The field name must be an " ++
				"atom, not a variable or other term.\n")
		;
			[]
		)
	; { Functor = cons(unqualified(":="), 2) } ->
		io__write_string("  error: invalid use of field update " ++
			"operator (`:=').\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string("  This is probably some kind " ++
				"of syntax error.\n")
		;
			[]
		)
	; { Functor = cons(unqualified(":-"), 2) } ->
		io__write_string("  syntax error in lambda expression " ++
			"(`:-').\n")
	; { Functor = cons(unqualified("-->"), 2) } ->
		io__write_string("  syntax error in DCG lambda expression " ++
			"(`-->').\n")
	; { Functor = cons(unqualified("."), 2) } ->
		io__write_string("  error: the list constructor is " ++
			"now `[|]/2', not `./2'.\n")
	; { Functor = cons(unqualified("!"), 1) } ->
		io__write_string("  error: invalid use of `!' " ++
			"state variable operator.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string("  You probably meant to use " ++
				"`!.' or `!:'.\n")
		;
			[]
		)
	;
		(
			{ Functor = cons(Constructor, Arity) },
			{ typecheck_info_get_ctors(Info, ConsTable) },
			{ solutions(
				(pred(N::out) is nondet :-
					map__member(ConsTable,
						cons(Constructor, N),
						_),
				N \= Arity
			), ActualArities) },
			{ ActualArities \= [] }
		->
			report_wrong_arity_constructor(Constructor, Arity,
				ActualArities, Context)
		;
			io__write_string("  error: undefined symbol `"),
			{ strip_builtin_qualifier_from_cons_id(Functor,
				StrippedFunctor) },
			hlds_out__write_cons_id(StrippedFunctor),
			io__write_string("'"),
			(
				{ Functor = cons(Constructor, _) },
				{ Constructor = qualified(ModQual, _) }
			->
				maybe_report_missing_import(Info, ModQual)
			;
				{ Functor = cons(unqualified("[|]"), 2) }
			->
				maybe_report_missing_import(Info,
					unqualified("list"))
			;
				io__write_string(".\n")
			)
		),
		(
			{ ConsErrors = [_ | _] },
			list__foldl(report_cons_error(Context), ConsErrors)
		;
			{ ConsErrors = [] }
		)
	).

	% language_builtin(Name, Arity) is true iff Name/Arity is the name
	% of a builtin language construct that should be used as a goal,
	% not as an expression.
	%
:- pred language_builtin(string::in, arity::in) is semidet.

language_builtin("=", 2).
language_builtin("\\=", 2).
language_builtin(",", 2).
language_builtin(";", 2).
language_builtin("\\+", 1).
language_builtin("not", 1).
language_builtin("<=>", 2).
language_builtin("=>", 2).
language_builtin("<=", 2).
language_builtin("call", _).
language_builtin("impure", 1).
language_builtin("semipure", 1).
language_builtin("all", 2).
language_builtin("some", 2).
language_builtin("aditi_insert", 3).
language_builtin("aditi_delete", 3).
language_builtin("aditi_bulk_insert", 3).
language_builtin("aditi_bulk_insert", 4).
language_builtin("aditi_bulk_delete", 3).
language_builtin("aditi_bulk_delete", 4).
language_builtin("aditi_bulk_modify", 3).
language_builtin("aditi_bulk_modify", 4).

:- pred report_wrong_arity_constructor(sym_name::in, arity::in, list(int)::in,
	prog_context::in, io::di, io::uo) is det.

report_wrong_arity_constructor(Name, Arity, ActualArities, Context) -->
	io__write_string("  error: "),
	{ MaybePredOrFunc = no },
	report_error_num_args(MaybePredOrFunc, Arity, ActualArities),
	io__nl,
	prog_out__write_context(Context),
	io__write_string("  in use of constructor `"),
	prog_out__write_sym_name(Name),
	io__write_string("'.\n").

:- pred report_cons_error(prog_context::in, cons_error::in, io::di, io::uo)
	is det.

report_cons_error(Context, ConsError, !IO) :-
	(
		ConsError = foreign_type_constructor(TypeName - TypeArity, _),
		Pieces = [words("There are"),
			fixed("`:- pragma foreign_type'"),
			words("declarations for type"),
			sym_name_and_arity(TypeName / TypeArity),
			suffix(","),
			words("so it is treated as an abstract type"),
			words("in all predicates and functions"),
			words("which are not implemented"),
			words("for those foreign types.")],
		write_error_pieces_not_first_line(Context, 0, Pieces, !IO)
	;
		ConsError = abstract_imported_type
		% For `abstract_imported_type' errors, the "undefined symbol"
		% error written by `report_error_undef_cons' is sufficient so
		% we do not print an additional error message here.
	;
		ConsError = invalid_field_update(FieldName, FieldDefn,
			TVarSet, TVars),
		FieldDefn = hlds_ctor_field_defn(DefnContext, _, _, ConsId, _),
		Pieces1 = [words("Field"), sym_name(FieldName),
			words("cannot be updated because"),
			words("the existentially quantified type")],
		(
			TVars = [],
			unexpected(this_file, "report_invalid_field_update:"
				++ " no type variables")
		;
			TVars = [TVar],
			TVarsStr = mercury_var_to_string(TVar, TVarSet, no),
			Pieces2 = [words("variable"),
				words("`" ++ TVarsStr ++ "'"),
				words("occurs")]
		;
			TVars = [_, _ | _],
			TVarsStr = mercury_vars_to_string(TVars, TVarSet, no),
			Pieces2 = [words("variables"),
				words("`" ++ TVarsStr ++ "'"),
				words("occur")]
		),
		ConsIdStr = cons_id_to_string(ConsId),
		Pieces3 = [words("in the types of field"),
			sym_name(FieldName),
			words("and some other field"),
			words("in definition of constructor"),
			fixed("`" ++ ConsIdStr ++ "'.")],
		Pieces = Pieces1 ++ Pieces2 ++ Pieces3,
		write_error_pieces_not_first_line(DefnContext, 0, Pieces, !IO)
	;
		ConsError = new_on_non_existential_type(TypeCtor),
		TypeCtor = TypeName - TypeArity,
		Pieces = [words("Invalid use of `new'"),
			words("on a constructor of type"),
			sym_name_and_arity(TypeName / TypeArity),
			words("which is not existentially typed.")],
		write_error_pieces_not_first_line(Context, 0, Pieces, !IO)
	).

%-----------------------------------------------------------------------------%

report_ambiguity_error(Info, TypeAssign1, TypeAssign2, !IO) :-
	write_typecheck_info_context(Info, !IO),
	io__write_string(
		"  error: ambiguous overloading causes type ambiguity.\n", !IO),
	typecheck_info_get_varset(Info, VarSet),
	type_assign_get_var_types(TypeAssign1, VarTypes1),
	map__keys(VarTypes1, Vars1),
	report_ambiguity_error_2(Vars1, VarSet, Info, TypeAssign1, TypeAssign2,
		no, Found, !IO),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	typecheck_info_get_context(Info, Context),
	( Found = no ->
		prog_out__write_context(Context, !IO),
		io__write_string("  One or more of the predicates or " ++
			"functions called\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  is declared in more than one module.\n",
			!IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  Try adding explicit module qualifiers.\n",
			!IO)
	; VerboseErrors = yes ->
		io__write_strings([
"\tYou will need to add an explicit type qualification to resolve the\n",
"\ttype ambiguity.\n",
"\tThe way to add an explicit type qualification is to use ""with_type"".\n",
"\tFor details see the ""Explicit type qualification"" sub-section\n",
"\tof the ""Data-terms"" section of the ""Syntax"" chapter\n",
"\tof the Mercury langauge reference manual.\n"
		], !IO)
	;
		true
	).

:- pred report_ambiguity_error_2(list(prog_var)::in, prog_varset::in,
	typecheck_info::in, type_assign::in, type_assign::in,
	bool::in, bool::out, io::di, io::uo) is det.

report_ambiguity_error_2([], _VarSet, _, _TypeAssign1, _TypeAssign2,
		!Found, !IO).
report_ambiguity_error_2([V | Vs], VarSet, Info, TypeAssign1,
		TypeAssign2, !Found, !IO) :-
	type_assign_get_var_types(TypeAssign1, VarTypes1),
	type_assign_get_var_types(TypeAssign2, VarTypes2),
	type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
	type_assign_get_type_bindings(TypeAssign2, TypeBindings2),
	type_assign_get_head_type_params(TypeAssign1, HeadTypeParams1),
	type_assign_get_head_type_params(TypeAssign2, HeadTypeParams2),
	(
		map__search(VarTypes1, V, Type1),
		map__search(VarTypes2, V, Type2),
		term__apply_rec_substitution(Type1, TypeBindings1, T1),
		term__apply_rec_substitution(Type2, TypeBindings2, T2),
		\+ identical_types(T1, T2)
	->
		typecheck_info_get_context(Info, Context),
		(
			!.Found = no,
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Possible type assignments include:\n", !IO)
		;
			!.Found = yes
		),
		!:Found = yes,
		prog_out__write_context(Context, !IO),
		mercury_output_var(V, VarSet, no, !IO),
		io__write_string(": ", !IO),
		type_assign_get_typevarset(TypeAssign1, TVarSet1),
		output_type(T1, TVarSet1, HeadTypeParams1, !IO),
		io__write_string(" or ", !IO),
		type_assign_get_typevarset(TypeAssign2, TVarSet2),
		output_type(T2, TVarSet2, HeadTypeParams2, !IO),
		io__write_string("\n", !IO)
	;
		true
	),
	report_ambiguity_error_2(Vs, VarSet, Info, TypeAssign1, TypeAssign2,
		!Found, !IO).

%-----------------------------------------------------------------------------%

report_unsatisfiable_constraints(TypeAssignSet, !Info, !IO) :-
	typecheck_info_get_context(!.Info, Context),
	write_context_and_pred_id(!.Info, !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  unsatisfiable typeclass constraint(s):\n", !IO),

		% XXX this won't be very pretty when there are
		% XXX multiple type_assigns.
	io__write_list(TypeAssignSet, "\n", write_constraints(Context), !IO),
	typecheck_info_set_found_error(yes, !Info).

:- pred write_constraints(prog_context::in, type_assign::in, io::di, io::uo)
	is det.

write_constraints(Context, TypeAssign, !IO) :-
	type_assign_get_typeclass_constraints(TypeAssign, Constraints),
	UnprovenConstraints = Constraints ^ unproven,
	retrieve_prog_constraint_list(UnprovenConstraints,
		UnprovenProgConstraints0),

	type_assign_get_typevarset(TypeAssign, VarSet),
	type_assign_get_type_bindings(TypeAssign, Bindings),
	apply_rec_subst_to_prog_constraint_list(Bindings,
		UnprovenProgConstraints0, UnprovenProgConstraints1),
	list__sort_and_remove_dups(UnprovenProgConstraints1,
		UnprovenProgConstraints),
	prog_out__write_context(Context, !IO),
	io__write_string("  `", !IO),
	AppendVarnums = no,
	io__write_list(UnprovenProgConstraints, "', `",
		mercury_output_constraint(VarSet, AppendVarnums), !IO),
	io__write_string("'.\n", !IO).

%-----------------------------------------------------------------------------%

:- pred write_types_of_vars(list(prog_var)::in, prog_varset::in,
	prog_context::in, typecheck_info::in, type_assign_set::in,
	io::di, io::uo) is det.

write_types_of_vars([], _, _, _, _, !IO) :-
	io__write_string(".\n", !IO).
write_types_of_vars([Var | Vars], VarSet, Context, Info, TypeAssignSet, !IO) :-
	io__write_string(",\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  ", !IO),
	write_argument_name(VarSet, Var, !IO),
	write_type_of_var(Info, Context, TypeAssignSet, Var, !IO),
	write_types_of_vars(Vars, VarSet, Context, Info, TypeAssignSet, !IO).

:- pred write_argument_name(prog_varset::in, prog_var::in, io::di, io::uo)
	is det.

write_argument_name(VarSet, Var, !IO) :-
	( varset__search_name(VarSet, Var, _) ->
		io__write_string("variable `", !IO),
		mercury_output_var(Var, VarSet, no, !IO),
		io__write_string("'", !IO)
	;
		io__write_string("argument", !IO)
	).

:- pred write_functor_name(cons_id::in, int::in, io::di, io::uo) is det.

write_functor_name(Functor, Arity, !IO) :-
	strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
	( Arity = 0 ->
		io__write_string("constant `", !IO),
		( Functor = cons(Name, _) ->
			prog_out__write_sym_name(Name, !IO)
		;
			hlds_out__write_cons_id(StrippedFunctor, !IO)
		),
		io__write_string("'", !IO)
	; Functor = cons(unqualified(""), _) ->
		io__write_string("higher-order term (with arity ", !IO),
		io__write_int(Arity - 1, !IO),
		io__write_string(")", !IO)
	;
		io__write_string("functor `", !IO),
		hlds_out__write_cons_id(StrippedFunctor, !IO),
		io__write_string("'", !IO)
	).

:- pred write_type_of_var(typecheck_info::in, prog_context::in,
	type_assign_set::in, prog_var::in, io::di, io::uo) is det.

write_type_of_var(_Info, Context, TypeAssignSet, Var, !IO) :-
	get_type_stuff(TypeAssignSet, Var, TypeStuffList),
	TypeStrs0 = list__map(typestuff_to_typestr, TypeStuffList),
	list__sort_and_remove_dups(TypeStrs0, TypeStrs),
	( TypeStrs = [TypeStr] ->
		io__write_string(" has type `", !IO),
		io__write_string(TypeStr, !IO),
		io__write_string("'", !IO)
	;
		io__write_string(" has overloaded type {\n", !IO),
		write_types_list(Context, TypeStrs, !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  }", !IO)
	).

:- pred write_type_of_functor(cons_id::in, int::in, prog_context::in,
	list(cons_type_info)::in, io::di, io::uo) is det.

write_type_of_functor(Functor, Arity, Context, ConsDefnList, !IO) :-
	( ConsDefnList = [SingleDefn] ->
		io__write_string(" has type ", !IO),
		( Arity \= 0 ->
			io__write_string("\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  `", !IO)
		;
			io__write_string("`", !IO)
		),
		write_cons_type(SingleDefn, Functor, Context, !IO),
		io__write_string("'", !IO)
	;
		io__write_string(" has overloaded type\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  { ", !IO),
		write_cons_type_list(ConsDefnList, Functor, Arity, Context,
			!IO),
		io__write_string(" }", !IO)
	).

:- pred write_cons_type(cons_type_info::in, cons_id::in, prog_context::in,
	io::di, io::uo) is det.

	% XXX Should we mention the context here?
write_cons_type(cons_type_info(TVarSet, ExistQVars, ConsType, ArgTypes, _),
		Functor, _, !IO) :-
	(
		ArgTypes = [_ | _],
		( cons_id_and_args_to_term(Functor, ArgTypes, Term) ->
			output_type(Term, TVarSet, ExistQVars, !IO)
		;
			error("typecheck.write_cons_type: invalid cons_id")
		),
		io__write_string(": ", !IO)
	;
		ArgTypes = []
	),
	output_type(ConsType, TVarSet, ExistQVars, !IO).

:- pred write_cons_type_list(list(cons_type_info)::in, cons_id::in, int::in,
	prog_context::in, io::di, io::uo) is det.

write_cons_type_list([], _, _, _, !IO).
write_cons_type_list([ConsDefn | ConsDefns], Functor, Arity, Context, !IO) :-
	write_cons_type(ConsDefn, Functor, Context, !IO),
	(
		ConsDefns = []
	;
		ConsDefns = [_ | _],
		( Arity = 0 ->
			io__write_string(", ", !IO)
		;
			io__write_string(",\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  ", !IO)
		),
		write_cons_type_list(ConsDefns, Functor, Arity, Context, !IO)
	).

:- pred write_type_assign_set_msg(type_assign_set::in, prog_varset::in,
	io::di, io::uo) is det.

write_type_assign_set_msg(TypeAssignSet, VarSet, !IO) :-
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		( TypeAssignSet = [_] ->
			io__write_string(
				"\tThe partial type assignment was:\n", !IO)
		;
			io__write_string("\tThe possible partial type " ++
				"assignments were:\n", !IO)
		),
		write_type_assign_set(TypeAssignSet, VarSet, !IO)
	;
		VerboseErrors = no
	).

:- pred write_args_type_assign_set_msg(args_type_assign_set::in,
	prog_varset::in, io::di, io::uo) is det.

write_args_type_assign_set_msg(ArgTypeAssignSet, VarSet, !IO) :-
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		( ArgTypeAssignSet = [_] ->
			io__write_string(
				"\tThe partial type assignment was:\n", !IO)
		;
			io__write_string("\tThe possible partial type " ++
				"assignments were:\n", !IO)
		),
		write_args_type_assign_set(ArgTypeAssignSet, VarSet, !IO)
	;
		VerboseErrors = no
	).

:- pred output_type((type)::in, tvarset::in, head_type_params::in,
	io::di, io::uo) is det.

output_type(Type0, TVarSet, HeadTypeParams, !IO) :-
	strip_builtin_qualifiers_from_type(Type0, Type1),
	Type = maybe_add_existential_quantifier(HeadTypeParams, Type1),
	mercury_output_term(Type, TVarSet, no, !IO).

:- pred write_types_list(prog_context::in, list(string)::in,
	io::di, io::uo) is det.

write_types_list(_Context, [], !IO).
write_types_list(Context, [Type | Types], !IO) :-
	prog_out__write_context(Context, !IO),
	io__write_string("    ", !IO),
	io__write_string(Type, !IO),
	(
		Types = [],
		io__write_string("\n", !IO)
	;
		Types = [_ | _],
		io__write_string(",\n", !IO),
		write_types_list(Context, Types, !IO)
	).

:- pred write_type_stuff(type_stuff::in, io::di, io::uo) is det.

write_type_stuff(type_stuff(Type, TVarSet, TypeBinding, HeadTypeParams),
		!IO) :-
	write_type_b(Type, TVarSet, TypeBinding, HeadTypeParams, !IO).

:- pred write_var_type_stuff_list(prog_context::in, list(type_stuff)::in,
	(type)::in, io::di, io::uo) is det.

write_var_type_stuff_list(Context, TypeStuffs, Type, !IO) :-
	io__write_list(TypeStuffs, ",\n", write_var_type_stuff(Context, Type),
		!IO).

:- pred write_var_type_stuff(prog_context::in, (type)::in, type_stuff::in,
	io::di, io::uo) is det.

write_var_type_stuff(Context, Type, VarTypeStuff, !IO) :-
	VarTypeStuff = type_stuff(VarType, TVarSet, TypeBinding,
		HeadTypeParams),
	prog_out__write_context(Context, !IO),
	io__write_string("    (inferred) ", !IO),
	write_type_b(VarType, TVarSet, TypeBinding, HeadTypeParams, !IO),
	io__write_string(",\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("    (expected) ", !IO),
	write_type_b(Type, TVarSet, TypeBinding, HeadTypeParams, !IO).

:- pred write_type_b((type)::in, tvarset::in, tsubst::in, head_type_params::in,
	io::di, io::uo) is det.

write_type_b(Type0, TypeVarSet, TypeBindings, HeadTypeParams, !IO) :-
	Type = maybe_add_existential_quantifier(HeadTypeParams, Type0),
	write_type_with_bindings(Type, TypeVarSet, TypeBindings, !IO).

:- pred write_arg_type_stuff_list(prog_context::in, list(arg_type_stuff)::in,
	io::di, io::uo) is det.

write_arg_type_stuff_list(Context, TypeStuffs, !IO) :-
	io__write_list(TypeStuffs, ",\n", write_arg_type_stuff(Context), !IO).

:- pred write_arg_type_stuff(prog_context::in, arg_type_stuff::in,
	io::di, io::uo) is det.

write_arg_type_stuff(Context, ArgTypeStuff, !IO) :-
	ArgTypeStuff = arg_type_stuff(Type, VarType, TVarSet, HeadTypeParams),
	prog_out__write_context(Context, !IO),
	io__write_string("    (inferred) ", !IO),
	output_type(VarType, TVarSet, HeadTypeParams, !IO),
	io__write_string(",\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("    (expected) ", !IO),
	output_type(Type, TVarSet, HeadTypeParams, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_report_missing_import(typecheck_info::in, module_specifier::in,
	io::di, io::uo) is det.

maybe_report_missing_import(Info, ModuleQualifier, !IO) :-
	typecheck_info_get_context(Info, Context),
	%
	% First check if this module wasn't imported.
	%
	typecheck_info_get_module_info(Info, ModuleInfo),
	(
		% If the module qualifier couldn't match any of the visible
		% modules, then we report that the module has not been
		% imported.
		\+ (
			visible_module(VisibleModule, ModuleInfo),
			match_sym_name(ModuleQualifier, VisibleModule)
		)
	->
		io__write_string("\n", !IO),
		error_util__write_error_pieces(Context, 2,
			[words("(the module "),
			fixed(error_util__describe_sym_name(ModuleQualifier)),
			words("has not been imported).")], !IO)
	;
		% The module qualifier matches one or more of the
		% visible modules.  But maybe the user forgot to
		% import the parent module(s) of that module...
		solutions(get_unimported_parent(ModuleQualifier,
			ModuleInfo), UnimportedParents),
		UnimportedParents \= []
	->
		io__write_string("\n", !IO),
		report_unimported_parents(Context, UnimportedParents, !IO)
	;
		io__write_string(".\n", !IO)
	).

	% Nondeterministically return all the possible parent
	% modules which could be parent modules of the given
	% module qualifier, and which are not imported.
	%
:- pred get_unimported_parent(module_name::in, module_info::in,
	module_name::out) is nondet.

get_unimported_parent(ModuleQualifier, ModuleInfo, UnimportedParent) :-
	visible_module(ModuleName, ModuleInfo),
	match_sym_name(ModuleQualifier, ModuleName),
	ParentModules = get_ancestors(ModuleName),
	list__member(UnimportedParent, ParentModules),
	\+ visible_module(UnimportedParent, ModuleInfo).

:- pred report_unimported_parents(prog_context::in, list(module_name)::in,
	io::di, io::uo) is det.

report_unimported_parents(Context, UnimportedParents, !IO) :-
	UnimportedParentDescs = list__map(error_util__describe_sym_name,
		UnimportedParents),
	AllUnimportedParents = list_to_pieces(UnimportedParentDescs),
	error_util__write_error_pieces(Context, 2,
		( AllUnimportedParents = [_] ->
			[words("(the possible parent module ")]
			++ AllUnimportedParents
			++ [words("has not been imported).")]
		;
			[words("(the possible parent modules ")]
			++ AllUnimportedParents
			++ [words("have not been imported).")]
		), !IO).

%-----------------------------------------------------------------------------%

:- pred write_call_context(prog_context::in, pred_markers::in,
	call_id::in, int::in, unify_context::in, io::di, io::uo) is det.

write_call_context(Context, PredMarkers, CallId, ArgNum, UnifyContext) -->
	( { ArgNum = 0 } ->
		hlds_out__write_unify_context(UnifyContext, Context)
	;
		prog_out__write_context(Context),
		io__write_string("  in "),
		hlds_out__write_call_arg_id(CallId, ArgNum, PredMarkers),
		io__write_string(":\n")
	).

:- pred write_typecheck_info_context(typecheck_info::in, io::di, io::uo)
	is det.

write_typecheck_info_context(Info, !IO) :-
	write_context_and_pred_id(Info, !IO),
	typecheck_info_get_context(Info, Context),
	prog_out__write_context(Context, !IO).

:- pred write_context_and_pred_id(typecheck_info::in, io::di, io::uo) is det.

write_context_and_pred_id(Info, !IO) :-
	typecheck_info_get_module_info(Info, ModuleInfo),
	typecheck_info_get_context(Info, Context),
	typecheck_info_get_predid(Info, PredId),
	prog_out__write_context(Context, !IO),
	io__write_string("In clause for ", !IO),
	hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
	io__write_string(":\n", !IO).

	% This is intended to supercede the above predicate - It performs the
	% same action, but instead of just writing to the output straight away
	% the resultant string is passed back to the caller to deal with.
	% This allows `nicer' handling of error messages, since this string
	% can be used by the predicates in error_util.m
	%
	% The string generated by this predicate is of the form:
	% In clause for module.pred/N:
	%
:- pred make_pred_id_preamble(typecheck_info::in, string::out) is det.

make_pred_id_preamble(Info, Preamble) :-
	typecheck_info_get_module_info(Info, Module),
	typecheck_info_get_predid(Info, PredId),
	PredPieces = describe_one_pred_name(Module, should_not_module_qualify,
		PredId),
	PredName = error_pieces_to_string(PredPieces),
	Preamble = "In clause for " ++ PredName ++ ":".

	% Check whether two types are identical ignoring their
	% prog_contexts, i.e. whether they can be unified without
	% binding any type parameters.
	%
:- pred identical_types((type)::in, (type)::in) is semidet.

identical_types(Type1, Type2) :-
	map__init(TypeSubst0),
	type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
	TypeSubst = TypeSubst0.

%-----------------------------------------------------------------------------%

	% Given a type assignment set and a variable,
	% return the list of possible different types for the variable.
	%
:- type type_stuff ---> type_stuff(type, tvarset, tsubst, head_type_params).

:- pred get_type_stuff(type_assign_set::in, prog_var::in,
	list(type_stuff)::out) is det.

get_type_stuff([], _Var, []).
get_type_stuff([TypeAssign | TypeAssigns], Var, TypeStuffs) :-
	get_type_stuff(TypeAssigns, Var, TailTypeStuffs),
	type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TVarSet),
	type_assign_get_var_types(TypeAssign, VarTypes),
	( map__search(VarTypes, Var, Type0) ->
		Type = Type0
	;
		% This shouldn't happen - how can a variable which has
		% not yet been assigned a type variable fail to have
		% the correct type?
		term__context_init(Context),
		Type = term__functor(term__atom("<any>"), [], Context)
	),
	TypeStuff = type_stuff(Type, TVarSet, TypeBindings, HeadTypeParams),
	( list__member(TypeStuff, TailTypeStuffs) ->
		TypeStuffs = TailTypeStuffs
	;
		TypeStuffs = [TypeStuff | TailTypeStuffs]
	).

:- func typestuff_to_typestr(type_stuff) = string.

typestuff_to_typestr(TypeStuff) = TypeStr :-
	TypeStuff = type_stuff(Type0, TypeVarSet, TypeBindings,
		HeadTypeParams),
	term__apply_rec_substitution(Type0, TypeBindings, Type1),
	strip_builtin_qualifiers_from_type(Type1, Type2),
	Type = maybe_add_existential_quantifier(HeadTypeParams, Type2),
	TypeStr = mercury_term_to_string(Type, TypeVarSet, no).

	% Given an arg type assignment set and a variable id,
	% return the list of possible different types for the argument
	% and the variable.
	%
:- type arg_type_stuff --->
	arg_type_stuff(type, type, tvarset, head_type_params).

:- pred get_arg_type_stuff(args_type_assign_set::in, prog_var::in,
	list(arg_type_stuff)::out) is det.

get_arg_type_stuff([], _Var, []).
get_arg_type_stuff([ArgTypeAssign | ArgTypeAssigns], Var, ArgTypeStuffs) :-
	ArgTypeAssign = args(TypeAssign, ArgTypes, _),
	get_arg_type_stuff(ArgTypeAssigns, Var, TailArgTypeStuffs),
	type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TVarSet),
	type_assign_get_var_types(TypeAssign, VarTypes),
	( map__search(VarTypes, Var, VarType0) ->
		VarType = VarType0
	;
		% This shouldn't happen - how can a variable which has
		% not yet been assigned a type variable fail to have
		% the correct type?
		term__context_init(Context),
		VarType = term__functor(term__atom("<any>"), [], Context)
	),
	list__index0_det(ArgTypes, 0, ArgType),
	term__apply_rec_substitution(ArgType, TypeBindings, ArgType2),
	term__apply_rec_substitution(VarType, TypeBindings, VarType2),
	ArgTypeStuff = arg_type_stuff(ArgType2, VarType2, TVarSet,
		HeadTypeParams),
	( list__member(ArgTypeStuff, TailArgTypeStuffs) ->
		ArgTypeStuffs = TailArgTypeStuffs
	;
		ArgTypeStuffs = [ArgTypeStuff | TailArgTypeStuffs]
	).

	% Check if any of the type variables in the type are existentially
	% quantified (occur in HeadTypeParams), and if so, add an
	% appropriate existential quantifier at the front of the type.
	%
:- func maybe_add_existential_quantifier(head_type_params, (type)) = (type).

maybe_add_existential_quantifier(HeadTypeParams, Type0) = Type :-
	prog_type__vars(Type0, TVars),
	ExistQuantTVars = set__to_sorted_list(set__intersect(
		set__list_to_set(HeadTypeParams), set__list_to_set(TVars))),
	(
		ExistQuantTVars = [],
		Type = Type0
	;
		ExistQuantTVars = [_ | _],
		Type = term__functor(term__atom("some"),
			[make_list_term(ExistQuantTVars), Type0],
			term__context_init)
	).

:- func make_list_term(list(tvar)) = (type).

make_list_term([]) = term__functor(term__atom("[]"), [], term__context_init).
make_list_term([Var | Vars]) = term__functor(term__atom("[|]"),
	[term__variable(Var), make_list_term(Vars)], term__context_init).

%-----------------------------------------------------------------------------%

write_inference_messages([], _, !IO).
write_inference_messages([PredId | PredIds], ModuleInfo, !IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	(
		check_marker(Markers, infer_type),
		module_info_predids(ModuleInfo, ValidPredIds),
		list__member(PredId, ValidPredIds),
		\+ pred_info_get_goal_type(PredInfo, promise(_))
	->
		write_inference_message(PredInfo, !IO)
	;
		true
	),
	write_inference_messages(PredIds, ModuleInfo, !IO).

	% Write out the inferred `pred' or `func' declaration
	% for a single predicate.
	%
:- pred write_inference_message(pred_info::in, io::di, io::uo) is det.

write_inference_message(PredInfo, !IO) :-
	PredName = pred_info_name(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	Name = unqualified(PredName),
	pred_info_context(PredInfo, Context),
	pred_info_arg_types(PredInfo, VarSet, ExistQVars, Types0),
	strip_builtin_qualifiers_from_type_list(Types0, Types),
	pred_info_get_class_context(PredInfo, ClassContext),
	pred_info_get_purity(PredInfo, Purity),
	MaybeDet = no,
	prog_out__write_context(Context, !IO),
	io__write_string("Inferred ", !IO),
	AppendVarNums = no,
	(
		PredOrFunc = predicate,
		mercury_output_pred_type(VarSet, ExistQVars, Name, Types,
			MaybeDet, Purity, ClassContext, Context, AppendVarNums,
			!IO)
	;
		PredOrFunc = function,
		pred_args_to_func_args(Types, ArgTypes, RetType),
		mercury_output_func_type(VarSet, ExistQVars, Name, ArgTypes,
			RetType, MaybeDet, Purity, ClassContext, Context,
			AppendVarNums, !IO)
	).

checkpoint(Msg, !Info, !IO) :-
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, debug_types, DoCheckPoint),
	(
		DoCheckPoint = yes,
		checkpoint_2(Msg, !.Info, !IO)
	;
		DoCheckPoint = no
	).

:- pred checkpoint_2(string::in, typecheck_info::in, io::di, io::uo) is det.

checkpoint_2(Msg, T0, !IO) :-
	io__write_string("At ", !IO),
	io__write_string(Msg, !IO),
	io__write_string(": ", !IO),
	globals__io_lookup_bool_option(statistics, Statistics, !IO),
	maybe_report_stats(Statistics, !IO),
	io__write_string("\n", !IO),
	typecheck_info_get_type_assign_set(T0, TypeAssignSet),
	(
		Statistics = yes,
		TypeAssignSet = [TypeAssign | _]
	->
		type_assign_get_var_types(TypeAssign, VarTypes),
		checkpoint_tree_stats("\t`var -> type' map", VarTypes, !IO),
		type_assign_get_type_bindings(TypeAssign, TypeBindings),
		checkpoint_tree_stats("\t`type var -> type' map", TypeBindings,
			!IO)
	;
		true
	),
	typecheck_info_get_varset(T0, VarSet),
	write_type_assign_set(TypeAssignSet, VarSet, !IO).

:- pred checkpoint_tree_stats(string::in, map(_K, _V)::in, io::di, io::uo)
	is det.

checkpoint_tree_stats(Description, Tree, !IO) :-
	map__count(Tree, Count),
	io__write_string(Description, !IO),
	io__write_string(": count = ", !IO),
	io__write_int(Count, !IO),
	io__write_string("\n", !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "typecheck_errors.m".

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_errors.
%-----------------------------------------------------------------------------%

