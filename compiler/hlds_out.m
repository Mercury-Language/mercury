
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% hlds_out.m

% Main authors: conway, fjh.

% There is quite a bit of overlap between the following modules:
%
%	hlds_out.m
%	mercury_to_mercury.m
%	term_io.m
%
% mercury_to_mercury.m prints the parse tree data structure defined
% in prog_data.m.  hlds_out.m does a similar task, but for the data
% structure defined in hlds.m.  term_io.m prints terms.

% There are two different ways of printing variables.
% One way uses the names Var', Var'', etc. which are generated
% by the compiler.  The other way converts all names back into
% a format allowed as source code.  Currently this module calls
% mercury_to_mercury.m, which uses the second method, rather
% than term_io.m, which uses the first method.  We should
% think about using an option to specify which method is used.

%-----------------------------------------------------------------------------%

:- module hlds_out.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data.
:- import_module prog_data, llds, instmap.
:- import_module io, bool, term, map, list, varset.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_type_id(type_id, io__state, io__state).
:- mode hlds_out__write_type_id(in, di, uo) is det.

:- pred hlds_out__write_cons_id(cons_id, io__state, io__state).
:- mode hlds_out__write_cons_id(in, di, uo) is det.

:- pred hlds_out__cons_id_to_string(cons_id, string).
:- mode hlds_out__cons_id_to_string(in, out) is det.

	% hlds_out__write_pred_id/4 writes out a message such as
	% 	predicate `foo:bar/3'
	% or	function `foo:myfoo/5'
	% unless the predicate name begins with a double underscore "__",
	% in which case mercury_output_term is used to print out the
	% predicate's (or function's) name and argument types (since for
	% `__Unify__' predicates, the module, name and arity are not
	% sufficient to indentify the predicate).

:- pred hlds_out__write_pred_id(module_info, pred_id, io__state, io__state).
:- mode hlds_out__write_pred_id(in, in, di, uo) is det.

:- pred hlds_out__write_pred_proc_id(module_info, pred_id, proc_id,
	io__state, io__state).
:- mode hlds_out__write_pred_proc_id(in, in, in, di, uo) is det.

:- pred hlds_out__write_call_id(pred_or_func, pred_call_id,
	io__state, io__state).
:- mode hlds_out__write_call_id(in, in, di, uo) is det.

:- pred hlds_out__write_pred_call_id(pred_call_id, io__state, io__state).
:- mode hlds_out__write_pred_call_id(in, di, uo) is det.

:- pred hlds_out__write_pred_or_func(pred_or_func, io__state, io__state).
:- mode hlds_out__write_pred_or_func(in, di, uo) is det.

:- pred hlds_out__pred_or_func_to_str(pred_or_func, string).
:- mode hlds_out__pred_or_func_to_str(in, out) is det.

	% hlds_out__write_unify_context/5 writes out a message such as
	%	foo.m:123:   in argument 3 of functor `foo/5':
	% 	foo.m:123:   in unification of `X' and `blah':
	% based on the unify_context and term__context.
	%
:- pred hlds_out__write_unify_context(unify_context, term__context,
	io__state, io__state).
:- mode hlds_out__write_unify_context(in, in, di, uo) is det.

	% hlds_out__write_unify_context_first/6 is the
	% same as above, except that it also takes and returns a bool
	% which specifies whether this is the start of a sentence.
	% If the first argument is `yes', then
	% it means this is the first line of an error message, so
	% the message starts with a capital letter, e.g.
	%	foo.m:123:   In argument 3 of functor `foo/5':
	% 	foo.m:123:   in unification of `X' and `blah':
	% The bool returned as the fourth argument will be `no' unless nothing
	% was printed out, in which case it will be the same as the first arg.
	%
:- pred hlds_out__write_unify_context(bool, unify_context, term__context,
	bool, io__state, io__state).
:- mode hlds_out__write_unify_context(in, in, in, out, di, uo) is det.

:- pred hlds_out__write_determinism(determinism, io__state, io__state).
:- mode hlds_out__write_determinism(in, di, uo) is det.

:- pred hlds_out__write_can_fail(can_fail, io__state, io__state).
:- mode hlds_out__write_can_fail(in, di, uo) is det.

:- pred hlds_out__write_code_model(code_model, io__state, io__state).
:- mode hlds_out__write_code_model(in, di, uo) is det.

:- pred hlds_out__write_import_status(import_status, io__state, io__state).
:- mode hlds_out__write_import_status(in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_proc(int, bool, module_info, pred_id, proc_id,
	import_status, proc_info, io__state, io__state).
:- mode hlds_out__write_proc(in, in, in, in, in, in, in, di, uo) is det.

	% print out an entire hlds structure.

:- pred hlds_out__write_hlds(int, module_info, io__state, io__state).
:- mode hlds_out__write_hlds(in, in, di, uo) is det.

:- pred hlds_out__write_clauses(int, inst_table, module_info, pred_id,
	varset, bool, list(var), pred_or_func, list(clause), vartypes,
	io__state, io__state).
:- mode hlds_out__write_clauses(in, in, in, in, in, in, in, in, in, in, di, uo)
	is det.

	% print out an hlds goal.

:- pred hlds_out__write_goal(hlds_goal, inst_table, module_info, varset,
	bool, int, string, io__state, io__state).
:- mode hlds_out__write_goal(in, in, in, in, in, in, in, di, uo) is det.

	% print out a functor and its arguments

:- pred hlds_out__write_functor(const, list(var), varset, bool,
	io__state, io__state).
:- mode hlds_out__write_functor(in, in, in, in, di, uo) is det.

	% print out a cons_id and arguments

:- pred hlds_out__write_functor_cons_id(cons_id, list(var), varset, bool,
	io__state, io__state).
:- mode hlds_out__write_functor_cons_id(in, in, in, in, di, uo) is det.

	% print out the right-hand-side of a unification

:- pred hlds_out__write_unify_rhs(unify_rhs, inst_table, module_info,
	varset, bool, int, io__state, io__state).
:- mode hlds_out__write_unify_rhs(in, in, in, in, in, in, di, uo) is det.

	% print out a list of variables their corresponding modes
	% (e.g. for a lambda expressions)

:- pred hlds_out__write_var_modes(list(var), list(mode), varset, bool,
	inst_table, io__state, io__state).
:- mode hlds_out__write_var_modes(in, in, in, in, in, di, uo) is det.

:- pred hlds_out__write_instmap(instmap, varset, bool, int, inst_table,
	io__state, io__state).
:- mode hlds_out__write_instmap(in, in, in, in, in, di, uo) is det.

	% find the name of a marker

:- pred hlds_out__marker_name(marker, string).
:- mode hlds_out__marker_name(in, out) is det.

	% print out the name of a marker

:- pred hlds_out__write_marker(marker, io__state, io__state).
:- mode hlds_out__write_marker(in, di, uo) is det.

:- type vartypes --->
		yes(tvarset, map(var, type))
	;	no.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_to_mercury, globals, options, purity, special_pred.
:- import_module llds_out, prog_out, prog_util, (inst), instmap, trace.
:- import_module termination, term_errors.

:- import_module int, string, set, std_util, assoc_list, multi_map.
:- import_module term_io, require, getopt.


hlds_out__write_type_id(Name - Arity) -->
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity).

hlds_out__cons_id_to_string(cons(SymName, Arity), String) :-
	prog_out__sym_name_to_string(SymName, SymNameString0),
	( string__contains_char(SymNameString0, '*') ->
		% We need to protect against the * appearing next to a /
		Stuff = lambda([Char::in, Str0::in, Str::out] is det, (
			( Char = ('*') ->
				string__append(Str0, "star", Str)
			;
				string__char_to_string(Char, CharStr),
				string__append(Str0, CharStr, Str)
			)
		)),
		string__foldl(Stuff, SymNameString0, "", SymNameString)
	;
		SymNameString = SymNameString0
	),
	string__int_to_string(Arity, ArityString),
	string__append_list([SymNameString, "/", ArityString], String).
hlds_out__cons_id_to_string(int_const(Int), String) :-
	string__int_to_string(Int, String).
hlds_out__cons_id_to_string(string_const(String), S) :-
	string__append_list(["""", String, """"], S).
hlds_out__cons_id_to_string(float_const(_), "<float>").
hlds_out__cons_id_to_string(pred_const(_, _), "<pred>").
hlds_out__cons_id_to_string(code_addr_const(_, _), "<code_addr>").
hlds_out__cons_id_to_string(base_type_info_const(_, _, _), "<base_type_info>").
hlds_out__cons_id_to_string(base_typeclass_info_const(_, _, _), 
	"<base_typeclass_info>").

hlds_out__write_cons_id(cons(SymName, Arity)) -->
	prog_out__write_sym_name(SymName),
	io__write_string("/"),
	io__write_int(Arity).
hlds_out__write_cons_id(int_const(Int)) -->
	io__write_int(Int).
hlds_out__write_cons_id(string_const(String)) -->
	io__write_char('"'),
	io__write_string(String),
	io__write_char('"').
hlds_out__write_cons_id(float_const(Float)) -->
	io__write_float(Float).
hlds_out__write_cons_id(pred_const(_PredId, _ProcId)) -->
	io__write_string("<pred>").
hlds_out__write_cons_id(code_addr_const(_PredId, _ProcId)) -->
	io__write_string("<code_addr>").
hlds_out__write_cons_id(base_type_info_const(_, _, _)) -->
	io__write_string("<base_type_info>").
hlds_out__write_cons_id(base_typeclass_info_const(_, _, _)) -->
	io__write_string("<base_typeclass_info>").

	% The code of this predicate duplicates the functionality of
	% term_errors__describe_one_pred_name. Changes here should be made
	% there as well.

hlds_out__write_pred_id(ModuleInfo, PredId) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	( { special_pred_name_arity(Kind, _, Name, Arity) } ->	
		{ special_pred_description(Kind, Descr) },
		io__write_string(Descr),
		io__write_string(" for type "),
		{ pred_info_arg_types(PredInfo, TVarSet, _ExistQVars,
			ArgTypes) },
		( { special_pred_get_type(Name, ArgTypes, Type) } ->
			mercury_output_term(Type, TVarSet, no)
		;
			{ error("special_pred_get_type failed!") }
		)
	; { string__prefix(Name, "Introduced predicate for ") } ->
		io__write_string("type class method implementation")
	;
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(" `"),
		prog_out__write_sym_name(Module),
		io__write_string(":"),
		{ PredOrFunc = function ->
			OrigArity is Arity - 1
		;
			OrigArity = Arity
		},
		io__write_string(Name),
		io__write_string("/"),
		io__write_int(OrigArity),
		io__write_string("'")
	).

hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId) -->
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" mode "),
	{ proc_id_to_int(ProcId, ModeNum) },
	io__write_int(ModeNum).

hlds_out__write_call_id(PredOrFunc, Name/Arity) -->
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	{ PredOrFunc = function ->
		OrigArity is Arity - 1
	;
		OrigArity = Arity
	},
	hlds_out__write_pred_call_id(Name/OrigArity),
	io__write_string("'").

hlds_out__write_pred_call_id(Name / Arity) -->
	prog_out__write_sym_name(Name),
	io__write_char('/'),
	io__write_int(Arity).

hlds_out__write_pred_or_func(predicate) -->
	io__write_string("predicate").
hlds_out__write_pred_or_func(function) -->
	io__write_string("function").

hlds_out__pred_or_func_to_str(predicate, "pred").
hlds_out__pred_or_func_to_str(function, "func").

%-----------------------------------------------------------------------------%

hlds_out__write_unify_context(UnifyContext, Context) -->
	hlds_out__write_unify_context(no, UnifyContext, Context, _).

hlds_out__write_unify_context(First0,
		unify_context(MainContext, RevSubContexts), Context, First) -->
	hlds_out__write_unify_main_context(First0, MainContext, Context,
			First1),
	{ list__reverse(RevSubContexts, SubContexts) },
	hlds_out__write_unify_sub_contexts(First1, SubContexts, Context, First).

:- pred hlds_out__write_unify_main_context(bool, unify_main_context,
	term__context, bool, io__state, io__state).
:- mode hlds_out__write_unify_main_context(in, in, in, out, di, uo) is det.

hlds_out__write_unify_main_context(First, explicit, _, First) -->
	[].
hlds_out__write_unify_main_context(First, head(ArgNum), Context, no) -->
	hlds_out__write_in_argument(First, ArgNum, Context),
	io__write_string(" of clause head:\n").
hlds_out__write_unify_main_context(First, call(PredId, ArgNum), Context, no) -->
	hlds_out__write_in_argument(First, ArgNum, Context),
	io__write_string(" of call to predicate `"),
	hlds_out__write_pred_call_id(PredId),
	io__write_string("':\n").

:- pred hlds_out__write_unify_sub_contexts(bool, unify_sub_contexts,
	term__context, bool, io__state, io__state).
:- mode hlds_out__write_unify_sub_contexts(in, in, in, out, di, uo) is det.

hlds_out__write_unify_sub_contexts(First, [], _, First) --> [].
hlds_out__write_unify_sub_contexts(First0, [ConsId - ArgNum | SubContexts],
		Context, First) -->
	hlds_out__write_in_argument(First0, ArgNum, Context),
	io__write_string(" of functor `"),
	hlds_out__write_cons_id(ConsId),
	io__write_string("':\n"),
	hlds_out__write_unify_sub_contexts(no, SubContexts, Context, First).

:- pred hlds_out__write_in_argument(bool, int, term__context,
					io__state, io__state).
:- mode hlds_out__write_in_argument(in, in, in, di, uo) is det.

hlds_out__write_in_argument(First, ArgNum, Context) -->
	prog_out__write_context(Context),
	( { First = yes } ->
		io__write_string("  In argument ")
	;
		io__write_string("  in argument ")
	),
	io__write_int(ArgNum).

%-----------------------------------------------------------------------------%

hlds_out__write_hlds(Indent, Module) -->
	{
		module_info_preds(Module, PredTable),
		module_info_types(Module, TypeTable),
		module_info_user_insts(Module, UserInstTable),
		module_info_modes(Module, ModeTable),
		module_info_classes(Module, ClassTable),
		module_info_superclasses(Module, SuperClassTable),
		module_info_instances(Module, InstanceTable)
	},
	io__write_string("\n"),
	hlds_out__write_header(Indent, Module),
	io__write_string("\n"),
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'T') } ->
		hlds_out__write_types(Indent, TypeTable),
		io__write_string("\n"),
		hlds_out__write_classes(Indent, ClassTable),
		io__write_string("\n"),
		hlds_out__write_superclasses(Indent, SuperClassTable),
		io__write_string("\n"),
		hlds_out__write_instances(Indent, InstanceTable),
		io__write_string("\n")
	;
		[]
	),
	( { string__contains_char(Verbose, 'M') } ->
		hlds_out__write_user_insts(Indent, UserInstTable),
		io__write_string("\n"),
		hlds_out__write_modes(Indent, ModeTable),
		io__write_string("\n")
	;
		io__write_string("\n")  
	),
	hlds_out__write_preds(Indent, Module, PredTable),
	io__write_string("\n"),
	hlds_out__write_footer(Indent, Module).

:- pred hlds_out__write_header(int, module_info, io__state, io__state).
:- mode hlds_out__write_header(in, in, di, uo) is det.

hlds_out__write_header(Indent, Module) -->
	{ module_info_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(":- module "),
	prog_out__write_sym_name(Name),
	io__write_string(".\n").

:- pred hlds_out__write_footer(int, module_info, io__state, io__state).
:- mode hlds_out__write_footer(in, in, di, uo) is det.

hlds_out__write_footer(Indent, Module) -->
	{ module_info_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(":- end_module "),
	prog_out__write_sym_name(Name),
	io__write_string(".\n").

:- pred hlds_out__write_preds(int, module_info, pred_table,
	io__state, io__state).
:- mode hlds_out__write_preds(in, in, in, di, uo) is det.

hlds_out__write_preds(Indent, ModuleInfo, PredTable) -->
	io__write_string("%-------- Predicates --------\n\n"),
	hlds_out__write_indent(Indent),
	{ map__keys(PredTable, PredIds) },
	hlds_out__write_preds_2(Indent, ModuleInfo, PredIds, PredTable).

:- pred hlds_out__write_preds_2(int, module_info, list(pred_id), pred_table,
	io__state, io__state).
:- mode hlds_out__write_preds_2(in, in, in, in, di, uo) is det.

hlds_out__write_preds_2(Indent, ModuleInfo, PredIds0, PredTable) -->
        globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	(
		{ PredIds0 = [PredId|PredIds] }
	->
		{ map__lookup(PredTable, PredId, PredInfo) },
		( 	
			{ pred_info_is_imported(PredInfo) } 
		->
			[]
		;
			% for pseudo-imported predicates (i.e. unification
			% preds), only print them if we are using a local
			% mode for them
			{ pred_info_is_pseudo_imported(PredInfo) },
			{ pred_info_procids(PredInfo, ProcIds) },
			{ hlds_pred__in_in_unification_proc_id(ProcId) },
			{ ProcIds = [ProcId] }
		->
			[]
		;
			% We dump unification predicates if suboption 
			% 'U' is on. We don't really need that 
			% information to understand how the program has 
			% been transformed.
			{ \+ string__contains_char(Verbose, 'U') },
			{ pred_info_arity(PredInfo, Arity) },
			{ Arity = 2 },
			{ pred_info_name(PredInfo, PredName) },
			{ PredName =  "__Unify__" }
		->
			[]
		;
			hlds_out__write_pred(Indent, ModuleInfo, PredId,
				PredInfo)
		),
		hlds_out__write_preds_2(Indent, ModuleInfo, PredIds, PredTable)
	;
		[]
	).

:- pred hlds_out__write_pred(int, module_info, pred_id, pred_info,
	io__state, io__state).
:- mode hlds_out__write_pred(in, in, in, in, di, uo) is det.

hlds_out__write_pred(Indent, ModuleInfo, PredId, PredInfo) -->
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_arg_types(PredInfo, ArgTypes) },
	{ pred_info_get_exist_quant_tvars(PredInfo, ExistQVars) },
	{ pred_info_typevarset(PredInfo, TVarSet) },
	{ pred_info_clauses_info(PredInfo, ClausesInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_import_status(PredInfo, ImportStatus) },
	{ pred_info_get_markers(PredInfo, Markers) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_get_class_context(PredInfo, ClassContext) },
	{ pred_info_get_constraint_proofs(PredInfo, Proofs) },
	{ pred_info_get_purity(PredInfo, Purity) },
	{ pred_info_get_head_type_params(PredInfo, HeadTypeParams) },
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'C') } ->
		% Information about predicates is dumped if 'C' 
		% suboption is on.
		mercury_output_pred_type(TVarSet, ExistQVars,
				qualified(Module, PredName), 
				ArgTypes, no, Purity, ClassContext, Context)
	;
		[]
	),
	{ ClausesInfo = clauses_info(VarSet, _, VarTypes, HeadVars, Clauses) },
	( { string__contains_char(Verbose, 'C') } ->
		hlds_out__write_indent(Indent),
		io__write_string("% pred id: "),
		{ pred_id_to_int(PredId, PredInt) },
		io__write_int(PredInt),
		io__write_string(", category: "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(", status: "),
		hlds_out__write_import_status(ImportStatus),
		io__write_string("\n"),
		{ markers_to_marker_list(Markers, MarkerList) },
		( { MarkerList = [] } ->
			[]
		;
			io__write_string("% markers:"),
			hlds_out__write_marker_list(MarkerList),
			io__write_string("\n")
		),
		( { map__is_empty(Proofs) } ->
			[]
		;
			hlds_out__write_constraint_proofs(Indent, VarSet,
				Proofs),
			io__write_string("\n")
		)
	;
		[]
	),

	( { string__contains_char(Verbose, 'v') } ->
		{ AppendVarnums = yes }
	;
		{ AppendVarnums = no }
	),
	( { string__contains_char(Verbose, 'C') } ->
		( { HeadTypeParams \= [] } ->
			io__write_string(
				"% head_type_params:\n"),
			io__write_string("% "),
			mercury_output_vars(HeadTypeParams, TVarSet,
					AppendVarnums),
			io__write_string("\n")
		;
			[]
		),
		hlds_out__write_var_types(Indent, VarSet, AppendVarnums,
			VarTypes, TVarSet),

		% Never write the clauses out verbosely -
		% disable the verbose_dump_hlds option before writing them,
		% and restore its initial value afterwards
		globals__io_set_option(verbose_dump_hlds, string("")),
		{ inst_table_init(InstTable) },	% YYY
		hlds_out__write_clauses(Indent, InstTable, ModuleInfo, PredId,
			VarSet, AppendVarnums, HeadVars, PredOrFunc, Clauses,
			no),
		globals__io_set_option(verbose_dump_hlds, string(Verbose))
	;
		[]
	),
	hlds_out__write_procs(Indent, AppendVarnums, ModuleInfo, PredId,
		ImportStatus, ProcTable),
	io__write_string("\n").

:- pred hlds_out__write_marker_list(list(marker), io__state, io__state).
:- mode hlds_out__write_marker_list(in, di, uo) is det.

hlds_out__write_marker_list(Markers) -->
	io__write_list(Markers, ", ", hlds_out__write_marker).

hlds_out__marker_name(infer_type, "infer_type").
hlds_out__marker_name(infer_modes, "infer_modes").
hlds_out__marker_name(inline, "inline").
hlds_out__marker_name(no_inline, "no_inline").
hlds_out__marker_name(dnf, "dnf").
hlds_out__marker_name(magic, "magic").
hlds_out__marker_name(obsolete, "obsolete").
hlds_out__marker_name(class_method, "class_method").
hlds_out__marker_name((impure), "impure").
hlds_out__marker_name((semipure), "semipure").
hlds_out__marker_name(promised_pure, "promise_pure").
hlds_out__marker_name(terminates, "terminates").
hlds_out__marker_name(check_termination, "check_termination").
hlds_out__marker_name(does_not_terminate, "does_not_terminate").

hlds_out__write_marker(Marker) -->
	{ hlds_out__marker_name(Marker, Name) },
	io__write_string(Name).

hlds_out__write_clauses(Indent, InstTable, ModuleInfo, PredId, VarSet,
		AppendVarnums, HeadVars, PredOrFunc, Clauses0, TypeQual) -->
	(
		{ Clauses0 = [Clause|Clauses] }
	->
		hlds_out__write_clause(Indent, InstTable, ModuleInfo, PredId,
			VarSet, AppendVarnums, HeadVars, PredOrFunc, Clause,
			TypeQual),
		hlds_out__write_clauses(Indent, InstTable, ModuleInfo, PredId,
			VarSet, AppendVarnums, HeadVars, PredOrFunc, Clauses,
			TypeQual)
	;
		[]
	).

:- pred hlds_out__write_clause(int, inst_table, module_info, pred_id,
	varset, bool, list(var), pred_or_func, clause, vartypes,
	io__state, io__state).
:- mode hlds_out__write_clause(in, in, in, in, in, in, in, in, in, in, di, uo)
	is det.

hlds_out__write_clause(Indent, InstTable, ModuleInfo, PredId, VarSet,
		AppendVarnums, HeadVars, PredOrFunc, Clause, TypeQual) -->
	{
		Clause = clause(
			Modes,
			Goal,
			_Context
		),
		Indent1 is Indent + 1
	},
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'm') } ->
		hlds_out__write_indent(Indent),
		io__write_string("% Modes for which this clause applies: "),
		{ list__map(lambda([Mode :: in, ModeInt :: out] is det,
			proc_id_to_int(Mode, ModeInt)), Modes, ModeInts) },
		hlds_out__write_intlist(ModeInts),
		io__write_string("\n")
	;
		[]
	),
	hlds_out__write_clause_head(ModuleInfo, PredId, VarSet, AppendVarnums,
		HeadVars, PredOrFunc),
	( { Goal = conj([]) - _GoalInfo } ->
		io__write_string(".\n")
	;
		io__write_string(" :-\n"),
		hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, ".", TypeQual)
	).

:- pred hlds_out__write_intlist(list(int), io__state, io__state).
:- mode hlds_out__write_intlist(in, di, uo) is det.

hlds_out__write_intlist(IntList) -->
	(
		{ IntList = [] }
	->
		io__write_string("[]")
	;
		io__write_string("[ "),
		hlds_out__write_intlist_2(IntList),
		io__write_string("]")
	).

:- pred hlds_out__write_intlist_2(list(int), io__state, io__state).
:- mode hlds_out__write_intlist_2(in, di, uo) is det.

hlds_out__write_intlist_2(Ns0) -->
	(
		{ Ns0 = [N] }
	->
		io__write_int(N)
	;
		{ Ns0 = [N|Ns] }
	->
		io__write_int(N),
		io__write_string(", "),
		hlds_out__write_intlist_2(Ns)
	;
		{ error("This should be unreachable.") }
	).

:- pred hlds_out__write_clause_head(module_info, pred_id, varset, bool,
	list(var), pred_or_func, io__state, io__state).
:- mode hlds_out__write_clause_head(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_clause_head(ModuleInfo, PredId, VarSet, AppendVarnums,
			HeadVars, PredOrFunc) -->
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(HeadVars, FuncArgs, RetVal) },
		hlds_out__write_qualified_functor(ModuleName,
			term__atom(PredName), FuncArgs, VarSet, AppendVarnums),
		io__write_string(" = "),
		mercury_output_term(term__variable(RetVal), VarSet,
			AppendVarnums)
	;
		{ PredOrFunc = predicate },
		hlds_out__write_qualified_functor(ModuleName,
			term__atom(PredName), HeadVars, VarSet, AppendVarnums)
	).

hlds_out__write_goal(Goal, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow) -->
		% don't type qualify everything
	hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, no).

	% TypeQual is yes(TVarset, VarTypes) if all constructors should
	% be module qualified.
:- pred hlds_out__write_goal_a(hlds_goal, inst_table, module_info, varset,
	bool, int, string, vartypes, io__state, io__state).
:- mode hlds_out__write_goal_a(in, in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_goal_a(Goal - GoalInfo, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'c') } ->
		{ goal_info_get_context(GoalInfo, Context) },
		{ term__context_file(Context, FileName) },
		{ term__context_line(Context, LineNumber) },
		( { FileName \= "" } ->
			hlds_out__write_indent(Indent),
			io__write_string("% context: file `"),
			io__write_string(FileName),
			io__write_string("', line "),
			io__write_int(LineNumber),
			io__write_string("\n")
		;
			[]
		)
	;
		[]
	),
	( { string__contains_char(Verbose, 'P') } ->
		{ goal_info_get_goal_path(GoalInfo, Path) },
		( { Path \= [] } ->
			{ trace__path_to_string(Path, PathStr) },
			hlds_out__write_indent(Indent),
			io__write_string("% goal path: "),
			io__write_string(PathStr),
			io__write_string("\n")
		;
			[]
		)
	;
		[]
	),
	( { string__contains_char(Verbose, 'n') } ->
		{ goal_info_get_nonlocals(GoalInfo, NonLocalsSet) },
		{ set__to_sorted_list(NonLocalsSet, NonLocalsList) },
		( { NonLocalsList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% nonlocals: "),
			mercury_output_vars(NonLocalsList, VarSet,
				AppendVarnums),
			io__write_string("\n")
		;
			[]
		)
	;
		[]
	),
	( { string__contains_char(Verbose, 'p') } ->
		{ goal_info_get_pre_deaths(GoalInfo, PreDeaths) },
		{ set__to_sorted_list(PreDeaths, PreDeathList) },
		( { PreDeathList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% pre-deaths: "),
			mercury_output_vars(PreDeathList, VarSet,
				AppendVarnums),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_pre_births(GoalInfo, PreBirths) },
		{ set__to_sorted_list(PreBirths, PreBirthList) },
		( { PreBirthList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% pre-births: "),
			mercury_output_vars(PreBirthList, VarSet,
				AppendVarnums),
			io__write_string("\n")
		;
			[]
		)
	;
		[]
	),
	( { string__contains_char(Verbose, 'f') } ->
		{ goal_info_get_follow_vars(GoalInfo, MaybeFollowVars) },
		(
			{ MaybeFollowVars = yes(FollowVars) }
		->
			{ map__to_assoc_list(FollowVars, FVlist) },
			hlds_out__write_indent(Indent),
			io__write_string("% follow vars:\n"),
			hlds_out__write_var_to_lvals(FVlist,
				VarSet, AppendVarnums, Indent)
		;
			[]
		)
	;
		[]
	),
	( { string__contains_char(Verbose, 'd') } ->
		hlds_out__write_indent(Indent),
		io__write_string("% determinism: "),
		{ goal_info_get_determinism(GoalInfo, Determinism) },
		hlds_out__write_determinism(Determinism),
		io__write_string("\n")
	;
		[]
	),
	hlds_out__write_goal_2(Goal, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual),
	( { string__contains_char(Verbose, 'i') } ->
		{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
		(
			{ instmap_delta_is_reachable(InstMapDelta) },
			{ semidet_fail }
			% { instmap_delta_changed_vars(InstMapDelta, Vars) },
			% { set__empty(Vars) }
		->
			[]
		;
			hlds_out__write_indent(Indent),
			io__write_string("% new insts: "),
			hlds_out__write_instmap_delta(InstMapDelta, VarSet,
				AppendVarnums, Indent, InstTable),
			io__write_string("\n")
		)
	;
		[]
	),
	( { string__contains_char(Verbose, 'p') } ->
		{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
		{ set__to_sorted_list(PostDeaths, PostDeathList) },
		( { PostDeathList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% post-deaths: "),
			mercury_output_vars(PostDeathList, VarSet,
				AppendVarnums),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_post_births(GoalInfo, PostBirths) },
		{ set__to_sorted_list(PostBirths, PostBirthList) },
		( { PostBirthList \= [] } ->
			hlds_out__write_indent(Indent),
			io__write_string("% post-births: "),
			mercury_output_vars(PostBirthList, VarSet,
				AppendVarnums),
			io__write_string("\n")
		;
			[]
		),
		{ goal_info_get_refs(GoalInfo, Refs) },
		{ set__to_sorted_list(Refs, RefList) },
		(
			{ RefList = [] }
		->
			[]
		;
			hlds_out__write_indent(Indent),
			io__write_string("% refs: "),
			mercury_output_vars(RefList, VarSet,
				AppendVarnums),
			io__write_string("\n")
		)
	;
		[]
	),
	( { string__contains_char(Verbose, 'r') } ->
		{ goal_info_get_resume_point(GoalInfo, Resume) },
		(
			{ Resume = no_resume_point }
		;
			{ Resume = resume_point(ResumeVars, Locs) },
			{ set__to_sorted_list(ResumeVars, ResumeVarList) },
			hlds_out__write_indent(Indent),
			io__write_string("% resume point "),
			(
				{ Locs = orig_only },
				io__write_string("orig only ")
			;
				{ Locs = stack_only },
				io__write_string("stack only ")
			;
				{ Locs = orig_and_stack },
				io__write_string("orig and stack ")
			;
				{ Locs = stack_and_orig },
				io__write_string("stack and orig ")
			),
			mercury_output_vars(ResumeVarList, VarSet,
				AppendVarnums),
			io__write_string("\n")
		)
	;
		[]
	),
	(
		{ string__contains_char(Verbose, 's') },
		( { Goal = disj(_, SM) }
		; { Goal = switch(_, _, _, SM) }
		; { Goal = if_then_else(_, _, _, _, SM) }
		),
		{ map__to_assoc_list(SM, SMlist) },
		{ SMlist \= [] }
	->
		hlds_out__write_indent(Indent),
		io__write_string("% store map:\n"),
		hlds_out__write_var_to_lvals(SMlist, VarSet, AppendVarnums,
			Indent)
	;
		[]
	),
	( { string__contains_char(Verbose, 'g') }
	->	
		{ goal_info_get_features(GoalInfo, Features) },
		{ set__to_sorted_list(Features, Flist) },
		(   { Flist = [] } ->
			[]
		;   
			hlds_out__write_indent(Indent),
			io__write_string("% Goal features:  "),
			io__write(Flist),
			io__write_string("\n")
		)
	;
		[]
	).

:- pred hlds_out__write_goal_2(hlds_goal_expr, inst_table, module_info,
	varset, bool, int, string, vartypes, io__state, io__state).
:- mode hlds_out__write_goal_2(in, in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_goal_2(switch(Var, CanFail, CasesList, _), InstTable, ModuleInfo,
		VarSet, AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("( % "),
	hlds_out__write_can_fail(CanFail),
	io__write_string(" switch on `"),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string("'\n"),
	{ Indent1 is Indent + 1 },
	( { CasesList = [Case | Cases] } ->
		hlds_out__write_case(Case, Var, InstTable, ModuleInfo,
			VarSet, AppendVarnums, Indent1, TypeQual),
		hlds_out__write_cases(Cases, Var, InstTable, ModuleInfo,
			VarSet, AppendVarnums, Indent, TypeQual)
	;
		hlds_out__write_indent(Indent1),
		io__write_string("fail\n")
	),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(some(Vars, Goal), InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("some ["),
	mercury_output_vars(Vars, VarSet, AppendVarnums),
	io__write_string("] (\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(if_then_else(Vars, Cond, Then, Else, _), InstTable, ModuleInfo,
		VarSet, AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("(if"),
	hlds_out__write_some(Vars, VarSet),
	io__write_string("\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Cond, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string("then\n"),
	hlds_out__write_goal_a(Then, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string("else\n"),
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	(
		{ Verbose \= "" },
		{ Else = if_then_else(_, _, _, _, _) - _ }
	->
		hlds_out__write_goal_a(Else, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent, "", TypeQual)
	;
		hlds_out__write_goal_a(Else, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, "", TypeQual)
	),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(not(Goal), InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("\\+ (\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(conj(List), InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	( { List = [Goal | Goals] } ->
		globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
		( { Verbose \= "" } ->
			{ Indent1 is Indent + 1 },
			hlds_out__write_indent(Indent),
			io__write_string("( % conjunction\n"),
			hlds_out__write_conj(Goal, Goals, InstTable, ModuleInfo,
				VarSet, AppendVarnums, Indent1, "", Verbose,
				",\n", TypeQual),
			hlds_out__write_indent(Indent),
			io__write_string(")"),
			io__write_string(Follow),
			io__write_string("\n")
		;
			hlds_out__write_conj(Goal, Goals, InstTable, ModuleInfo,
				VarSet, AppendVarnums, Indent, Follow, Verbose,
				",\n", TypeQual)
		)
	;
		hlds_out__write_indent(Indent),
		io__write_string("true"),
		io__write_string(Follow),
		io__write_string("\n")
	).

hlds_out__write_goal_2(par_conj(List, _), InstTable, ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	( { List = [Goal | Goals] } ->
		io__write_string("( % parallel conjunction\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, "", TypeQual),
			% See comments at hlds_out__write_goal_list.
		hlds_out__write_goal_list(Goals, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent, "&", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")"),
		io__write_string(Follow),
		io__write_string("\n")
	;
		io__write_string("/* parallel */ true"),
		io__write_string(Follow),
		io__write_string("\n")
	).


hlds_out__write_goal_2(disj(List, _), InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	( { List = [Goal | Goals] } ->
		io__write_string("( % disjunction\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, "", TypeQual),
		hlds_out__write_goal_list(Goals, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent, ";", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")"),
		io__write_string(Follow),
		io__write_string("\n")
	;
		io__write_string("fail"),
		io__write_string(Follow),
		io__write_string("\n")
	).

hlds_out__write_goal_2(higher_order_call(PredVar, ArgVars, _, _, _, PredOrFunc),
		_InstTable, _ModuleInfo, VarSet, AppendVarnums, Indent,
		Follow, _) -->
		% XXX we should print more info here
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	hlds_out__write_indent(Indent),
	(	{ PredOrFunc = predicate },
		( { string__contains_char(Verbose, 'l') } ->
			io__write_string("% higher-order predicate call\n"),
			hlds_out__write_indent(Indent)
		;
			[]
		),
		hlds_out__write_functor(term__atom("call"), [PredVar|ArgVars],
			VarSet, AppendVarnums)
	;
		{ PredOrFunc = function },
		( { string__contains_char(Verbose, 'l') } ->
			io__write_string(
				"% higher-order function application\n"),
			hlds_out__write_indent(Indent)
		;
			[]
		),
		{ pred_args_to_func_args([PredVar | ArgVars],
			FuncArgVars, FuncRetVar) },
		mercury_output_var(FuncRetVar, VarSet, AppendVarnums),
		io__write_string(" = "),
		hlds_out__write_functor(term__atom("apply"), FuncArgVars,
			VarSet, AppendVarnums)
	),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(class_method_call(TCInfoVar, _, ArgVars, _, _, _),
		_InstTable, _ModuleInfo, VarSet, AppendVarnums, Indent,
		Follow, _) -->
		% XXX we should print more info here too
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	hlds_out__write_indent(Indent),
	( { string__contains_char(Verbose, 'l') } ->
		io__write_string("% class method call"),
		hlds_out__write_indent(Indent)
	;
		[]
	),
	hlds_out__write_functor(term__atom("class_method_call"),
		[TCInfoVar|ArgVars], VarSet, AppendVarnums),
	io__write_string(Follow),
	io__write_string("\n").

hlds_out__write_goal_2(call(PredId, ProcId, ArgVars, Builtin,
			MaybeUnifyContext, PredName),
		InstTable, ModuleInfo, VarSet, AppendVarnums, Indent, Follow,
		TypeQual) -->
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'b') } ->
		(
			{ Builtin = inline_builtin },
			hlds_out__write_indent(Indent),
			io__write_string("% inline builtin\n")
		;
			{ Builtin = out_of_line_builtin },
			hlds_out__write_indent(Indent),
			io__write_string("% out of line builtin\n")
		;
			{ Builtin = not_builtin }
		)
	;
		[]
	),
	hlds_out__write_indent(Indent),
	( { invalid_pred_id(PredId)} ->
		[]
	;
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ pred_info_get_purity(PredInfo, Purity) },
		write_purity_prefix(Purity)
	),
	(
		{ PredName = qualified(ModuleName, Name) },
		hlds_out__write_qualified_functor(ModuleName, term__atom(Name),
			ArgVars, VarSet, AppendVarnums)
	;
		{ PredName = unqualified(Name) },
		hlds_out__write_functor(term__atom(Name), ArgVars, VarSet,
			AppendVarnums)
	),
	io__write_string(Follow),
	io__write_string("\n"),
	( { string__contains_char(Verbose, 'l') } ->
		{ pred_id_to_int(PredId, PredNum) },
		{ proc_id_to_int(ProcId, ProcNum) },
		hlds_out__write_indent(Indent),
		io__write_string("% pred id: "),
		io__write_int(PredNum),
		io__write_string(", proc id: "),
		io__write_int(ProcNum),
		io__write_strings([Follow, "\n"]),
		( { MaybeUnifyContext = yes(CallUnifyContext) } ->
			{ TypeQual = yes(_, VarTypes) ->
				map__lookup(VarTypes, Var, UniType),
				VarType = yes(UniType)
			;
				VarType = no
			},
			{ CallUnifyContext = call_unify_context(Var,
					RHS, _UnifyContext) },
			hlds_out__write_indent(Indent),
			io__write_string("% unify context: "),
			mercury_output_var(Var, VarSet, AppendVarnums),
			io__write_string(" = "),
			hlds_out__write_unify_rhs_2(RHS, InstTable, ModuleInfo,
				VarSet, AppendVarnums, Indent, Follow, VarType,
				TypeQual)
		;
			[]
		)
	;
		[]
	).

hlds_out__write_goal_2(unify(A, B, _, Unification, _), InstTable, ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	mercury_output_var(A, VarSet, AppendVarnums),
	io__write_string(" = "),
	{ TypeQual = yes(_, VarTypes) ->
		map__lookup(VarTypes, A, UniType),
		VarType = yes(UniType)
	;
		VarType = no
	},
	hlds_out__write_unify_rhs_2(B, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, VarType, TypeQual),
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'u') } ->
		(
			% don't output bogus info if we haven't been through
			% mode analysis yet
			{ Unification = complicated_unify(Mode, CanFail) },
			{ CanFail = can_fail },
			{ Mode = (free(unique) - free(unique) -> 
					free(unique) - free(unique)) }
		->
			hlds_out__write_indent(Indent),
			io__write_string("% Not yet classified\n")
		;
			hlds_out__write_unification(Unification, InstTable,
				ModuleInfo, VarSet, AppendVarnums, Indent)
		)
	;
		[]
	).

hlds_out__write_goal_2(pragma_c_code(_, _, _, ArgVars, ArgNames, _,
			PragmaCode), _, _, _, _, Indent, Follow, _) -->
	hlds_out__write_indent(Indent),
	io__write_string("$pragma_c_code(["),
	hlds_out__write_varnum_list(ArgVars),
	io__write_string("], ["),
	{ get_pragma_c_var_names(ArgNames, Names) },
	hlds_out__write_string_list(Names),
	io__write_string("], "),
	(
		{ PragmaCode = ordinary(C_Code, _) },
		io__write_string(""""),
		io__write_string(C_Code),
		io__write_string("""")
	;
		{ PragmaCode = nondet(Fields, _FieldsContext,
			First, _FirstContext,
			Later, _LaterContext,
			Treat, Shared, _SharedContext) },
		io__write_string("local_vars("""),
		io__write_string(Fields),
		io__write_string("""), "),
		io__write_string("first_code("""),
		io__write_string(First),
		io__write_string("""), "),
		io__write_string("retry_code("""),
		io__write_string(Later),
		io__write_string("""), "),
		(
			{ Treat = share },
			io__write_string("shared_code(""")
		;
			{ Treat = duplicate },
			io__write_string("duplicated_code(""")
		;
			{ Treat = automatic },
			io__write_string("common_code(""")
		),
		io__write_string(Shared),
		io__write_string(""")")
	),
	io__write_string(")"),
	io__write_string(Follow),
	io__write_string("\n").

:- pred hlds_out__write_varnum_list(list(var), io__state, io__state).
:- mode hlds_out__write_varnum_list(in, di, uo) is det.

hlds_out__write_varnum_list([]) --> [].
hlds_out__write_varnum_list([Var]) -->
	hlds_out__write_varnum(Var).
hlds_out__write_varnum_list([Var1, Var2 | Vars]) -->
	hlds_out__write_varnum(Var1),
	io__write_string(", "),
	hlds_out__write_varnum_list([Var2 | Vars]).

:- pred hlds_out__write_varnum(var, io__state, io__state).
:- mode hlds_out__write_varnum(in, di, uo) is det.

hlds_out__write_varnum(Var) -->
	{ term__var_to_int(Var, VarNum) },
	io__write_int(VarNum).

:- pred hlds_out__write_var_name_list(list(pair(var, string)),
	io__state, io__state).
:- mode hlds_out__write_var_name_list(in, di, uo) is det.

hlds_out__write_var_name_list([]) --> [].
hlds_out__write_var_name_list([Var - Name]) -->
	hlds_out__write_varnum(Var),
	io__write_string(" - "),
	io__write_string(Name).
hlds_out__write_var_name_list([Var1 - Name1, VarName2 | Vars]) -->
	hlds_out__write_varnum(Var1),
	io__write_string(" - "),
	io__write_string(Name1),
	io__write_string(", "),
	hlds_out__write_var_name_list([VarName2 | Vars]).

:- pred hlds_out__write_string_list(list(string), io__state, io__state).
:- mode hlds_out__write_string_list(in, di, uo) is det.

hlds_out__write_string_list([]) --> [].
hlds_out__write_string_list([Name]) -->
	io__write_string(Name).
hlds_out__write_string_list([Name1, Name2 | Names]) -->
	io__write_string(Name1),
	io__write_string(", "),
	hlds_out__write_string_list([Name2 | Names]).

:- pred hlds_out__write_unification(unification, inst_table, module_info,
	varset, bool, int, io__state, io__state).
:- mode hlds_out__write_unification(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_unification(assign(X, Y), _, _, VarSet, AppendVarnums,
		Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(X, VarSet, AppendVarnums),
	io__write_string(" := "),
	mercury_output_var(Y, VarSet, AppendVarnums),
	io__write_string("\n").
hlds_out__write_unification(simple_test(X, Y), _, _, VarSet, AppendVarnums,
		Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(X, VarSet, AppendVarnums),
	io__write_string(" == "),
	mercury_output_var(Y, VarSet, AppendVarnums),
	io__write_string("\n").
hlds_out__write_unification(construct(Var, ConsId, ArgVars, ArgModes),
		InstTable, ModuleInfo, VarSet, AppendVarnums, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string(" := "),
	hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes,
		InstTable, ModuleInfo, VarSet, AppendVarnums, Indent).
hlds_out__write_unification(deconstruct(Var, ConsId, ArgVars, ArgModes,
		CanFail), InstTable, ModuleInfo, VarSet, AppendVarnums, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet, AppendVarnums),
	( { CanFail = can_fail },
		io__write_string(" ?= ")
	; { CanFail = cannot_fail },
		io__write_string(" => ")
	),
	!,
	hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes,
		InstTable, ModuleInfo, VarSet, AppendVarnums, Indent).
hlds_out__write_unification(complicated_unify(Mode, CanFail),
		InstTable, _ModuleInfo, VarSet, _, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	( { CanFail = can_fail },
		io__write_string("can_fail, ")
	; { CanFail = cannot_fail },
		io__write_string("cannot_fail, ")
	),
	!,
	io__write_string("mode: "),
	mercury_output_uni_mode(Mode, VarSet, InstTable),
	io__write_string("\n").

:- pred hlds_out_write_functor_and_submodes(cons_id, list(var), list(uni_mode),
	inst_table, module_info, varset, bool, int, io__state, io__state).
:- mode hlds_out_write_functor_and_submodes(in, in, in, in, in, in, in, in,
	di, uo) is det.

hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes, InstTable, _ModuleInfo,
		VarSet, AppendVarnums, Indent) -->
	hlds_out__write_cons_id(ConsId),
	( { ArgVars = [] } ->
		io__write_string("\n")
	;
		io__write_string(" ("),
		mercury_output_vars(ArgVars, VarSet, AppendVarnums),
		io__write_string(")\n"),
		globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
		( { string__contains_char(Verbose, 'a') } ->
			hlds_out__write_indent(Indent),
			io__write_string("% arg-modes "),
			mercury_output_uni_mode_list(ArgModes, VarSet, InstTable),
			io__write_string("\n")
		;
			[]
		)
	).

hlds_out__write_unify_rhs(Rhs, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent) -->
	hlds_out__write_unify_rhs_3(Rhs, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, no, no).

:- pred hlds_out__write_unify_rhs_2(unify_rhs, inst_table, module_info,
	varset, bool, int, string, maybe(type), vartypes, io__state, io__state).
:- mode hlds_out__write_unify_rhs_2(in, in, in, in, in, in, in, in, in, di, uo)
	is det.

hlds_out__write_unify_rhs_2(Rhs, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, MaybeType, TypeQual) -->
	hlds_out__write_unify_rhs_3(Rhs, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, MaybeType, TypeQual),
	io__write_string(Follow),
	io__write_string("\n").

:- pred hlds_out__write_unify_rhs_3(unify_rhs, inst_table, module_info,
	varset, bool, int, maybe(type), vartypes, io__state, io__state).
:- mode hlds_out__write_unify_rhs_3(in, in, in, in, in, in, in, in,
	di, uo) is det.

hlds_out__write_unify_rhs_3(var(Var), _, _, VarSet, AppendVarnums, _, _, _) -->
	mercury_output_var(Var, VarSet, AppendVarnums).
hlds_out__write_unify_rhs_3(functor(ConsId, ArgVars), _, _, VarSet,
		AppendVarnums, _Indent, MaybeType, TypeQual) -->
	hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet, AppendVarnums),
	( { MaybeType = yes(Type), TypeQual = yes(TVarSet, _) } ->
		io__write_string(" TYPE_QUAL_OP "),
		mercury_output_term(Type, TVarSet, no)
	;
		[]
	).
hlds_out__write_unify_rhs_3(
		lambda_goal(PredOrFunc, NonLocals, Vars, Modes, Det,
				_IMDelta, Goal),
		InstTable, ModuleInfo, VarSet, AppendVarnums, Indent,
		MaybeType, TypeQual) -->
	{ Modes = argument_modes(ArgInstTable, ArgModes) },
	{ Indent1 is Indent + 1 },
	(
		{ PredOrFunc = predicate },
		io__write_string("(pred("),
		hlds_out__write_var_modes(Vars, ArgModes, VarSet, AppendVarnums,
			ArgInstTable),
		io__write_string(") is "),
		mercury_output_det(Det),
		io__write_string(" :-\n"),
		hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, "", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")")
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ArgModes, ParamModes, RetMode) },
		{ pred_args_to_func_args(Vars, ParamVars, RetVar) },
		io__write_string("(func("),
		hlds_out__write_var_modes(ParamVars, ParamModes, VarSet,
			AppendVarnums, ArgInstTable),
		io__write_string(") = ("),
		hlds_out__write_var_mode(RetVar, RetMode, VarSet,
			AppendVarnums, ArgInstTable),
		io__write_string(") is "),
		mercury_output_det(Det),
		io__write_string(" :-\n"),
		hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, "", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")")
	),
	( { MaybeType = yes(Type), TypeQual = yes(TVarSet, _) } ->
		io__write_string(" TYPE_QUAL_OP "),
		mercury_output_term(Type, TVarSet, AppendVarnums)
	;
		[]
	),
        globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'n') } ->
		( { NonLocals \= [] } ->
			hlds_out__write_indent(Indent1),
			io__write_string("% lambda nonlocals: "),
			mercury_output_vars(NonLocals, VarSet, AppendVarnums)
		;
			[]

		)
	;
		[]
	).

hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums) -->
	{ term__context_init(Context) },
	{ term__var_list_to_term_list(ArgVars, ArgTerms) },
	{ Term = term__functor(Functor, ArgTerms, Context) },
	mercury_output_term(Term, VarSet, AppendVarnums).

:- pred hlds_out__write_qualified_functor(module_name, const, list(var),
		varset, bool, io__state, io__state).
:- mode hlds_out__write_qualified_functor(in, in, in, in, in, di, uo) is det.

hlds_out__write_qualified_functor(ModuleName, Functor, ArgVars, VarSet,
		AppendVarnums) -->
	mercury_output_bracketed_sym_name(ModuleName),
	io__write_string(":"),
	hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums).

hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet, AppendVarnums) -->
	(
		{ ConsId = cons(SymName, _) },
		(
			{ SymName = qualified(Module, Name) },
			hlds_out__write_qualified_functor(Module,
				term__atom(Name), ArgVars, VarSet,
				AppendVarnums)
		;
			{ SymName = unqualified(Name) },
			hlds_out__write_functor(term__atom(Name),
				ArgVars, VarSet, AppendVarnums)
		)
	;
		{ ConsId = int_const(Int) },
		hlds_out__write_functor(term__integer(Int), ArgVars, VarSet,
			AppendVarnums)
	;
		{ ConsId = float_const(Float) },
		hlds_out__write_functor(term__float(Float), ArgVars, VarSet,
			AppendVarnums)
	;
		{ ConsId = string_const(Str) },
		hlds_out__write_functor(term__string(Str), ArgVars, VarSet,
			AppendVarnums)
	;
		{ ConsId = pred_const(_, _) },
		{ error("hlds_out__write_functor_cons_id: pred_const") }
	;
		{ ConsId = code_addr_const(_, _) },
		{ error("hlds_out__write_functor_cons_id: code_addr_const") }
	;
		{ ConsId = base_type_info_const(Module, Name, Arity) },
		io__write_string("base_type_info("""),
		prog_out__write_sym_name(Module),
		io__write_string(""", """),
		io__write_string(Name),
		io__write_string(""", "),
		io__write_int(Arity),
		io__write_string(")")
	;
		{ ConsId = base_typeclass_info_const(Module,
			class_id(Name, Arity), Instance) },
		io__write_string("base_typeclass_info("""),
		prog_out__write_sym_name(Module),
		io__write_string(""", """),
		io__write_string("class_id("),
		prog_out__write_sym_name(Name),
		io__write_string(", "),
		io__write_int(Arity),
		io__write_string("), "),
		io__write_string(Instance),
		io__write_string(")")
	).

hlds_out__write_var_modes([], [], _, _, _) --> [].
hlds_out__write_var_modes([Var|Vars], [Mode|Modes], VarSet, AppendVarnums,
		InstTable) -->
	hlds_out__write_var_mode(Var, Mode, VarSet, AppendVarnums,
			InstTable),
	( { Vars \= [] } ->
		io__write_string(", ")
	;
		[]
	),
	hlds_out__write_var_modes(Vars, Modes, VarSet, AppendVarnums,
		InstTable).
hlds_out__write_var_modes([], [_|_], _, _, _) -->
	{ error("hlds_out__write_var_modes: length mis-match") }.
hlds_out__write_var_modes([_|_], [], _, _, _) -->
	{ error("hlds_out__write_var_modes: length mis-match") }.

:- pred hlds_out__write_var_mode(var, mode, varset, bool, inst_table,
		io__state, io__state).
:- mode hlds_out__write_var_mode(in, in, in, in, in, di, uo) is det.

hlds_out__write_var_mode(Var, Mode, VarSet, AppendVarnums, InstTable) -->
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string("::"),
	mercury_output_mode(Mode, VarSet, InstTable).

:- pred hlds_out__write_conj(hlds_goal, list(hlds_goal), inst_table,
	module_info, varset, bool, int, string, string, string, vartypes,
	io__state, io__state).
:- mode hlds_out__write_conj(in, in, in, in, in, in, in, in, in, in, in,
	di, uo) is det.

hlds_out__write_conj(Goal1, Goals1, InstTable, ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, Verbose, Separator, TypeQual) -->
	(
		{ Goals1 = [Goal2 | Goals2] }
	->
		( { Verbose \= "" } ->
			% when generating verbose dumps,
			% we want the comma on its own line,
			% since that way it visually separates
			% the lines after one goal
			% and the lines before the next
			hlds_out__write_goal_a(Goal1, InstTable, ModuleInfo, VarSet,
				AppendVarnums, Indent, "", TypeQual),
			hlds_out__write_indent(Indent),
			io__write_string(Separator)
		;
			hlds_out__write_goal_a(Goal1, InstTable, ModuleInfo, VarSet,
				AppendVarnums, Indent, ",", TypeQual)
		),
		hlds_out__write_conj(Goal2, Goals2, InstTable, ModuleInfo,
			VarSet, AppendVarnums, Indent, Follow, Verbose,
			Separator, TypeQual)
	;
		hlds_out__write_goal_a(Goal1, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent, Follow, TypeQual)
	).

 	% hlds_out__write_goal_list is used to write both disjunctions and
 	% parallel conjunctions.

:- pred hlds_out__write_goal_list(list(hlds_goal), inst_table, module_info,
	varset, bool, int, string, vartypes, io__state, io__state).
:- mode hlds_out__write_goal_list(in, in, in, in, in, in, in, in, di, uo)
	is det.

hlds_out__write_goal_list(GoalList, InstTable, ModuleInfo, VarSet,
		AppendVarnums, Indent, Separator, TypeQual) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		hlds_out__write_indent(Indent),
		io__write_string(Separator),
		io__write_string("\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, "", TypeQual),
		hlds_out__write_goal_list(Goals, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent, Separator, TypeQual)
	;
		[]
	).

:- pred hlds_out__write_case(case, var, inst_table, module_info, varset,
	bool, int, vartypes, io__state, io__state).
:- mode hlds_out__write_case(in, in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_case(case(ConsId, IMDelta, Goal), Var, InstTable, ModuleInfo,
		VarSet, AppendVarnums, Indent, VarTypes) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string(" has functor "),
	hlds_out__write_cons_id(ConsId),
	io__write_string("\n"),

	% Print out the new insts after the implicit case unification.
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'i') } ->
		(
			{ instmap_delta_is_reachable(IMDelta) },
			{ semidet_fail }
			% { instmap_delta_changed_vars(IMDelta, Vars) },
			% { set__empty(Vars) }
		->
			[]
		;
			hlds_out__write_indent(Indent),
			io__write_string("% new insts: "),
			hlds_out__write_instmap_delta(IMDelta, VarSet,
				AppendVarnums, Indent, InstTable),
			io__write_string("\n")
		)
	;
		[]
	),

	% XXX if the output of this is to be used, e.g. in
	% inter-module optimization, output a unification to bind the
	% Var to the functor, since simplify.m and unused_args.m remove
	% the unification. At the moment this is not a problem, since
	% intermod.m works on the unoptimized clauses.
	hlds_out__write_goal_a(Goal, InstTable, ModuleInfo, VarSet,
		AppendVarnums, Indent, "", VarTypes).

:- pred hlds_out__write_cases(list(case), var, inst_table, module_info,
	varset, bool, int, vartypes, io__state, io__state).
:- mode hlds_out__write_cases(in, in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_cases(CasesList, Var, InstTable, ModuleInfo, VarSet, AppendVarnums,
		Indent, VarTypes) -->
	(
		{ CasesList = [Case | Cases] }
	->
		hlds_out__write_indent(Indent),
		io__write_string(";\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_case(Case, Var, InstTable, ModuleInfo,
			VarSet, AppendVarnums, Indent1, VarTypes),
		hlds_out__write_cases(Cases, Var, InstTable, ModuleInfo,
			VarSet, AppendVarnums, Indent, VarTypes)
	;
		[]
	).

:- pred hlds_out__write_some(list(var), varset, io__state, io__state).
:- mode hlds_out__write_some(in, in, di, uo) is det.

	% quantification is all implicit by the time we get to the hlds.

hlds_out__write_some(_Vars, _VarSet) --> [].

hlds_out__write_instmap(InstMap, VarSet, AppendVarnums, Indent,
		InstTable) -->
	( { instmap__is_unreachable(InstMap) } ->
		io__write_string("unreachable")
	;
		{ instmap__to_assoc_list(InstMap, AssocList) },
		hlds_out__write_instmap_2(AssocList, VarSet, AppendVarnums,
			Indent, InstMap, InstTable)
	).

:- pred hlds_out__write_instmap_2(assoc_list(var, inst), varset, bool, int,
	instmap, inst_table, io__state, io__state).
:- mode hlds_out__write_instmap_2(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_instmap_2([], _, _, _, _, _) --> [].
hlds_out__write_instmap_2([Var - Inst | Rest], VarSet, AppendVarnums, Indent,
		InstMap, InstTable) -->
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string(" -> "),
	{ varset__init(InstVarSet) },
	mercury_output_structured_inst(expand_noisily, Inst, Indent,
		InstVarSet, InstMap, InstTable),
	( { Rest = [] } ->
		[]
	;
		mercury_output_newline(Indent),
		io__write_string("%            "),
		hlds_out__write_instmap_2(Rest, VarSet, AppendVarnums, Indent,
			InstMap, InstTable)
	).

:- pred hlds_out__write_instmap_delta(instmap_delta, varset, bool, int,
		inst_table, io__state, io__state).
:- mode hlds_out__write_instmap_delta(in, in, in, in, in, di, uo) is det.

hlds_out__write_instmap_delta(InstMapDelta, VarSet, AppendVarnums, Indent,
		InstTable) -->
	( { instmap_delta_is_unreachable(InstMapDelta) } ->
		io__write_string("unreachable")
	;
		{ instmap_delta_to_assoc_list(InstMapDelta, AssocList) },
		{ instmap__init_reachable(InstMap) },	% YYY
		hlds_out__write_instmap_2(AssocList, VarSet, AppendVarnums,
			Indent, InstMap, InstTable)
	).

hlds_out__write_import_status(local) -->
	io__write_string("local").
hlds_out__write_import_status(exported) -->
	io__write_string("exported").
hlds_out__write_import_status(abstract_exported) -->
	io__write_string("abstract_exported").
hlds_out__write_import_status(pseudo_exported) -->
	io__write_string("pseudo_exported").
hlds_out__write_import_status(imported) -->
	io__write_string("imported").
hlds_out__write_import_status(abstract_imported) -->
	io__write_string("abstract_imported").
hlds_out__write_import_status(opt_imported) -->
	io__write_string("opt_imported").
hlds_out__write_import_status(pseudo_imported) -->
	io__write_string("pseudo_imported").

:- pred hlds_out__write_var_types(int, varset, bool, map(var, type), varset,
	io__state, io__state).
:- mode hlds_out__write_var_types(in, in, in, in, in, di, uo) is det.

hlds_out__write_var_types(Indent, VarSet, AppendVarnums, VarTypes, TVarSet) -->
	{ map__keys(VarTypes, Vars) },
	hlds_out__write_var_types_2(Vars, Indent, VarSet, AppendVarnums,
		VarTypes, TVarSet).

:- pred hlds_out__write_var_types_2(list(var), int, varset, bool,
	map(var, type), varset, io__state, io__state).
:- mode hlds_out__write_var_types_2(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_var_types_2([], _, _, _, _, _) --> [].
hlds_out__write_var_types_2([Var | Vars], Indent, VarSet, AppendVarnums,
		VarTypes, TypeVarSet) -->
	{ map__lookup(VarTypes, Var, Type) },
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string(" (number "),
	{ term__var_to_int(Var, VarNum) },
	io__write_int(VarNum),
	io__write_string(")"),
	io__write_string(" :: "),
	mercury_output_term(Type, TypeVarSet, AppendVarnums),
	io__write_string("\n"),
	hlds_out__write_var_types_2(Vars, Indent, VarSet, AppendVarnums,
		VarTypes, TypeVarSet).

:- pred hlds_out__write_typeinfo_varmap(int, bool, map(tvar, type_info_locn),
	varset, tvarset, io__state, io__state).
:- mode hlds_out__write_typeinfo_varmap(in, in, in, in, in, di, uo) is det.

hlds_out__write_typeinfo_varmap(Indent, AppendVarnums, TypeInfoMap, VarSet,
		TVarSet) -->
	hlds_out__write_indent(Indent),
	io__write_string("% type_info varmap:\n"),
	{ map__keys(TypeInfoMap, TypeVars) },
	hlds_out__write_typeinfo_varmap_2(TypeVars, Indent, AppendVarnums, 
		TypeInfoMap, VarSet, TVarSet).

:- pred hlds_out__write_typeinfo_varmap_2(list(tvar), int, bool, 
	map(tvar, type_info_locn), varset, tvarset, io__state, io__state).
:- mode hlds_out__write_typeinfo_varmap_2(in, in, in, in, in, in, di, uo) 
	is det.

hlds_out__write_typeinfo_varmap_2([], _, _, _, _, _) --> [].
hlds_out__write_typeinfo_varmap_2([TVar | TVars], Indent, AppendVarnums,
		TypeInfoMap, VarSet, TVarSet) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),

	mercury_output_var(TVar, TVarSet, AppendVarnums),
	io__write_string(" (number "),
	{ term__var_to_int(TVar, TVarNum) },
	io__write_int(TVarNum),
	io__write_string(")"),

	io__write_string(" -> "),
	{ map__lookup(TypeInfoMap, TVar, Locn) },
	(
		{ Locn = type_info(Var) },
		io__write_string("type_info("),
		mercury_output_var(Var, VarSet, AppendVarnums),
		io__write_string(") ")
	;
		{ Locn = typeclass_info(Var, Index) },
		io__write_string("typeclass_info("),
		mercury_output_var(Var, VarSet, AppendVarnums),
		io__write_string(", "),
		io__write_int(Index),
		io__write_string(") ")
	),
	io__write_string(" (number "),
	{ term__var_to_int(Var, VarNum) },
	io__write_int(VarNum),
	io__write_string(")"),
	io__write_string("\n"),

	hlds_out__write_typeinfo_varmap_2(TVars, Indent, AppendVarnums,
		TypeInfoMap, VarSet, TVarSet).

:- pred hlds_out__write_stack_slots(int, stack_slots, varset, bool,
	io__state, io__state).
:- mode hlds_out__write_stack_slots(in, in, in, in, di, uo) is det.

hlds_out__write_stack_slots(Indent, StackSlots, VarSet, AppendVarnums) -->
	{ map__to_assoc_list(StackSlots, VarSlotList) },
	{ list__map(lambda([X::in, Y::out] is det, 
			( X = V - L, Y = V - store_info(val, L) )),
		VarSlotList, StoreInfoList) },
	hlds_out__write_var_to_lvals(StoreInfoList, VarSet, AppendVarnums,
		Indent).

:- pred hlds_out__write_var_to_lvals(assoc_list(var, store_info), varset,
	bool, int, io__state, io__state).
:- mode hlds_out__write_var_to_lvals(in, in, in, in, di, uo) is det.

	hlds_out__write_var_to_lvals([], _, _, _) --> [].
hlds_out__write_var_to_lvals([Var - store_info(ValOrRef, Loc) | VarLocs],
		VarSet, AppendVarnums, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("%\t"),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string("\t-> "),
	( { ValOrRef = ref },
		io__write_string("*")
	; { ValOrRef = val }
	),
	{ llds_out__lval_to_string(Loc, LocStrPrime) ->
		LocStr = LocStrPrime
	;
		LocStr = "unknown location"
	},
	io__write_string(LocStr),
	io__write_string("\n"),
	hlds_out__write_var_to_lvals(VarLocs, VarSet, AppendVarnums, Indent).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_types(int, type_table, io__state, io__state).
:- mode hlds_out__write_types(in, in, di, uo) is det.

hlds_out__write_types(Indent, TypeTable) -->
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Types --------\n"),
	{ map__to_assoc_list(TypeTable, TypeAL) },
	hlds_out__write_types_2(Indent, TypeAL).

:- pred hlds_out__write_types_2(int, assoc_list(type_id, hlds_type_defn),
	io__state, io__state).
:- mode hlds_out__write_types_2(in, in, di, uo) is det.

hlds_out__write_types_2(_Indent, []) --> [].
hlds_out__write_types_2(Indent, [TypeId - TypeDefn | Types]) -->
	{ hlds_data__get_type_defn_tvarset(TypeDefn, TVarSet) },
	{ hlds_data__get_type_defn_tparams(TypeDefn, TypeParams) },
	{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
	{ hlds_data__get_type_defn_status(TypeDefn, Status) },
	{ hlds_data__get_type_defn_context(TypeDefn, Context) },

	% Write the context

	io__write_char('\n'),
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 'c') } ->
		{ term__context_file(Context, FileName) },
		{ term__context_line(Context, LineNumber) },
		( { FileName \= "" } ->
			hlds_out__write_indent(Indent),
			io__write_string("% context: file `"),
			io__write_string(FileName),
			io__write_string("', line "),
			io__write_int(LineNumber),
			io__write_string(", status "),
			hlds_out__write_import_status(Status),
			io__write_char('\n')
		;
			[]
		)
	;
		[]
	),

	hlds_out__write_indent(Indent),
	io__write_string(":- type "),
	hlds_out__write_type_name(TypeId),
	hlds_out__write_type_params(TVarSet, TypeParams),
	{ Indent1 is Indent + 1 },
	hlds_out__write_type_body(Indent1, TVarSet, TypeBody),
	hlds_out__write_types_2(Indent, Types).

:- pred hlds_out__write_type_name(type_id, io__state, io__state).
:- mode hlds_out__write_type_name(in, di, uo) is det.

hlds_out__write_type_name(Name - _Arity) -->
	prog_out__write_sym_name(Name).

:- pred hlds_out__write_type_params(tvarset, list(type_param),
	io__state, io__state).
:- mode hlds_out__write_type_params(in, in, di, uo) is det.

hlds_out__write_type_params(_Tvarset, []) --> [].
hlds_out__write_type_params(Tvarset, [P]) -->
	io__write_string("("),
	term_io__write_term(Tvarset, P),
	io__write_string(")").
hlds_out__write_type_params(Tvarset, [P | Ps]) -->
	{ Ps = [_ | _] },
	io__write_string("("),
	term_io__write_term(Tvarset, P),
	hlds_out__write_type_params_2(Tvarset, Ps).

:- pred hlds_out__write_type_params_2(tvarset, list(type_param),
	io__state, io__state).
:- mode hlds_out__write_type_params_2(in, in, di, uo) is det.

hlds_out__write_type_params_2(_Tvarset, []) -->
	io__write_string(")").
hlds_out__write_type_params_2(Tvarset, [P | Ps]) -->
	io__write_string(", "),
	term_io__write_term(Tvarset, P),
	hlds_out__write_type_params_2(Tvarset, Ps).

:- pred hlds_out__write_type_body(int, tvarset, hlds_type_body,
	io__state, io__state).
:- mode hlds_out__write_type_body(in, in, in, di, uo) is det.

hlds_out__write_type_body(Indent, Tvarset, du_type(Ctors, _Tags, _Enum,
		MaybeEqualityPred)) -->
	io__write_string(" --->\n"),
	hlds_out__write_constructors(Indent, Tvarset, Ctors),
	( { MaybeEqualityPred = yes(PredName) } ->
		io__write_string("\n\twhere equality is "),
		prog_out__write_sym_name(PredName)
	;
		[]
	),
	io__write_string(".\n").
hlds_out__write_type_body(_Indent, _Tvarset, uu_type(_)) -->
	{ error("hlds_out__write_type_body: undiscriminated union found") }.
hlds_out__write_type_body(_Indent, Tvarset, eqv_type(Type)) -->
	io__write_string(" == "),
	term_io__write_term(Tvarset, Type),
	io__write_string(".\n").
hlds_out__write_type_body(_Indent, _Tvarset, abstract_type) -->
	io__write_string(".\n").

:- pred hlds_out__write_constructors(int, tvarset, list(constructor),
	io__state, io__state).
:- mode hlds_out__write_constructors(in, in, in, di, uo) is det.

hlds_out__write_constructors(_Indent, _Tvarset, []) -->
	{ error("hlds_out__write_constructors: empty constructor list?") }.
hlds_out__write_constructors(Indent, Tvarset, [C]) -->
	hlds_out__write_indent(Indent),
	io__write_char('\t'),
	mercury_output_ctor(C, Tvarset).
hlds_out__write_constructors(Indent, Tvarset, [C | Cs]) -->
	{ Cs = [_ | _] },
	hlds_out__write_indent(Indent),
	io__write_char('\t'),
	mercury_output_ctor(C, Tvarset),
	io__write_string("\n"),
	hlds_out__write_constructors_2(Indent, Tvarset, Cs).

:- pred hlds_out__write_constructors_2(int, tvarset, list(constructor),
	io__state, io__state).
:- mode hlds_out__write_constructors_2(in, in, in, di, uo) is det.

hlds_out__write_constructors_2(_Indent, _Tvarset, []) --> [].
hlds_out__write_constructors_2(Indent, Tvarset, [C | Cs]) -->
	hlds_out__write_indent(Indent),
	io__write_string(";\t"),
	mercury_output_ctor(C, Tvarset),
	( { Cs = [] } ->
		io__write_string(".\n")
	;
		io__write_string("\n"),
		hlds_out__write_constructors_2(Indent, Tvarset, Cs)
	).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_classes(int, class_table, io__state, io__state).
:- mode hlds_out__write_classes(in, in, di, uo) is det.

hlds_out__write_classes(Indent, ClassTable) -->
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Classes --------\n"),
	{ map__to_assoc_list(ClassTable, ClassTableList) },
	io__write_list(ClassTableList, "\n",
		hlds_out__write_class_defn(Indent)),
	io__nl.

:- pred hlds_out__write_class_defn(int, pair(class_id, hlds_class_defn), 
			io__state, io__state).
:- mode hlds_out__write_class_defn(in, in, di, uo) is det.

hlds_out__write_class_defn(Indent, ClassId - ClassDefn) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),

	{ ClassId = class_id(SymName, Arity) },
	prog_out__write_sym_name(SymName),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(":\n"),

	{ ClassDefn = hlds_class_defn(Constraints, Vars, Interface, VarSet,
				Context) },

	{ term__context_file(Context, FileName) },
	{ term__context_line(Context, LineNumber) },
	( { FileName \= "" } ->
		hlds_out__write_indent(Indent),
		io__write_string("% context: file `"),
		io__write_string(FileName),
		io__write_string("', line "),
		io__write_int(LineNumber),
		io__write_string("\n")
	;
		[]
	),

		% curry the varset for term_io__write_variable/4
	{ PrintVar = lambda([VarName::in, IO0::di, IO::uo] is det,
			term_io__write_variable(VarName, VarSet, IO0, IO)
		) },
	hlds_out__write_indent(Indent),
	io__write_string("% Vars: "),
	io__write_list(Vars, ", ", PrintVar),
	io__nl,

	hlds_out__write_indent(Indent),
	io__write_string("% Constraints: "),
	io__write_list(Constraints, ", ",  mercury_output_constraint(VarSet)),
	io__nl,

	hlds_out__write_indent(Indent),
	io__write_string("% Class Methods: "),
	io__write_list(Interface, ", ", hlds_out__write_class_proc),
	io__nl.

	% Just output the class methods as pred_ids and proc_ids because
	% its probably not that useful to have the names. If that information
	% is needed, it shouldn't be a very difficult fix.
:- pred hlds_out__write_class_proc(hlds_class_proc, io__state, io__state).
:- mode hlds_out__write_class_proc(in, di, uo) is det.

hlds_out__write_class_proc(hlds_class_proc(PredId, ProcId)) -->
	io__write_string("hlds_class_proc(pred_id:"),
	{ pred_id_to_int(PredId, PredInt) },
	io__write_int(PredInt),
	io__write_string(", proc_id:"),
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_int(ProcInt),
	io__write_char(')').

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_superclasses(int, superclass_table, 
	io__state, io__state).
:- mode hlds_out__write_superclasses(in, in, di, uo) is det.

hlds_out__write_superclasses(Indent, SuperClassTable) -->
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Super Classes --------\n"),
	{ multi_map__to_assoc_list(SuperClassTable, SuperClassTableList) },
	io__write_list(SuperClassTableList, "\n\n",
		hlds_out__write_superclass(Indent)),
	io__nl.

:- pred hlds_out__write_superclass(int, pair(class_id, list(subclass_details)), 
			io__state, io__state).
:- mode hlds_out__write_superclass(in, in, di, uo) is det.

hlds_out__write_superclass(Indent, ClassId - SubClassDetailsList) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),

	{ ClassId = class_id(SymName, Arity) },
	prog_out__write_sym_name(SymName),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(":\n"),

	io__write_list(SubClassDetailsList, "\n",
		hlds_out__write_subclass_details(Indent, ClassId)).

:- pred hlds_out__write_subclass_details(int, class_id, subclass_details, 
			io__state, io__state).
:- mode hlds_out__write_subclass_details(in, in, in, di, uo) is det.

hlds_out__write_subclass_details(Indent, SuperClassId, SubClassDetails) -->
	{ SubClassDetails = subclass_details(SuperClassVars, SubClassId,
		SubClassVars, VarSet) },

		% curry the varset for term_io__write_variable/4
	{ PrintVar = lambda([VarName::in, IO0::di, IO::uo] is det,
			term_io__write_variable(VarName, VarSet, IO0, IO)
		) },
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	{ SubClassId = class_id(SubSymName, _SubArity) },
	prog_out__write_sym_name(SubSymName),
	io__write_char('('),
	io__write_list(SubClassVars, ", ", PrintVar),
	io__write_string(") <= "),

	{ SuperClassId = class_id(SuperSymName, _SuperArity) },
	prog_out__write_sym_name(SuperSymName),
	io__write_char('('),
	io__write_list(SuperClassVars, ", ", PrintVar),
	io__write_char(')').

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_instances(int, instance_table, io__state, io__state).
:- mode hlds_out__write_instances(in, in, di, uo) is det.

hlds_out__write_instances(Indent, InstanceTable) -->
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Instances --------\n"),
	{ map__to_assoc_list(InstanceTable, InstanceTableList) },
	io__write_list(InstanceTableList, "\n\n",
		hlds_out__write_instance_defns(Indent)),
	io__nl.

:- pred hlds_out__write_instance_defns(int, 
	pair(class_id, list(hlds_instance_defn)), io__state, io__state).
:- mode hlds_out__write_instance_defns(in, in, di, uo) is det.

hlds_out__write_instance_defns(Indent, ClassId - InstanceDefns) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),

	{ ClassId = class_id(SymName, Arity) },
	prog_out__write_sym_name(SymName),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(":\n"),

	io__write_list(InstanceDefns, "\n",
		hlds_out__write_instance_defn(Indent)).

:- pred hlds_out__write_instance_defn(int, hlds_instance_defn, 
			io__state, io__state).
:- mode hlds_out__write_instance_defn(in, in, di, uo) is det.

hlds_out__write_instance_defn(Indent, InstanceDefn) -->

	{ InstanceDefn = hlds_instance_defn(_, Constraints, Types, Interface,
		_MaybeClassInterface, VarSet, Proofs) },

	/*
	{ term__context_file(Context, FileName) },
	{ term__context_line(Context, LineNumber) },
	( { FileName \= "" } ->
		hlds_out__write_indent(Indent),
		io__write_string("% context: file `"),
		io__write_string(FileName),
		io__write_string("', line "),
		io__write_int(LineNumber),
		io__write_string("\n")
	;
		[]
	),
	*/

		% curry the varset for term_io__write_variable/4
	{ PrintTerm = lambda([TypeName::in, IO0::di, IO::uo] is det,
			term_io__write_term(VarSet, TypeName, IO0, IO)
		) },
	hlds_out__write_indent(Indent),
	io__write_string("% Types: "),
	io__write_list(Types, ", ", PrintTerm),
	io__nl,

	hlds_out__write_indent(Indent),
	io__write_string("% Constraints: "),
	io__write_list(Constraints, ", ",  mercury_output_constraint(VarSet)),
	io__nl,

	hlds_out__write_indent(Indent),
	io__write_string("% Instance Methods: "),
	mercury_output_instance_methods(Interface),
	io__nl,

	hlds_out__write_constraint_proofs(Indent, VarSet, Proofs),
	io__nl.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_user_insts(int, user_inst_table, io__state, io__state).
:- mode hlds_out__write_user_insts(in, in, di, uo) is det.

hlds_out__write_user_insts(Indent, _UserInstTable) -->
		% XXX fix this up.
	hlds_out__write_indent(Indent),
	io__write_string("%-------- User insts --------\n"),
	hlds_out__write_indent(Indent),
	io__write_string("%%% Not yet implemented, sorry.\n").
	% io__write_string("% ").
	% io__print(InstTable),
	% io__nl.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_modes(int, mode_table, io__state, io__state).
:- mode hlds_out__write_modes(in, in, di, uo) is det.

hlds_out__write_modes(Indent, _ModeTable) -->
		% XXX fix this up.
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Modes --------\n"),
	hlds_out__write_indent(Indent),
	io__write_string("%%% Not yet implemented, sorry.\n").
	% io__write_string("% "),
	% io__print(ModeTable),
	% io__nl.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_procs(int, bool, module_info, pred_id, import_status,
	proc_table, io__state, io__state).
:- mode hlds_out__write_procs(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_procs(Indent, AppendVarnums, ModuleInfo, PredId,
		ImportStatus, ProcTable) -->
	{ map__keys(ProcTable, ProcIds) },
	hlds_out__write_procs_2(ProcIds, AppendVarnums, ModuleInfo, Indent,
		PredId, ImportStatus, ProcTable).

:- pred hlds_out__write_procs_2(list(proc_id), bool, module_info, int, pred_id,
	import_status, proc_table, io__state, io__state).
:- mode hlds_out__write_procs_2(in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_procs_2([], _, _ModuleInfo, _Indent, _PredId, _, _ProcTable) -->
	[].
hlds_out__write_procs_2([ProcId | ProcIds], AppendVarnums, ModuleInfo, Indent,
		PredId, ImportStatus, ProcTable) -->
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	hlds_out__write_proc(Indent, AppendVarnums, ModuleInfo, PredId, ProcId, ImportStatus, ProcInfo),
	hlds_out__write_procs_2(ProcIds, AppendVarnums, ModuleInfo, Indent,
		PredId, ImportStatus, ProcTable).

hlds_out__write_proc(Indent, AppendVarnums, ModuleInfo, PredId, ProcId,
		ImportStatus, Proc) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_typevarset(PredInfo, TVarSet) },
	{ proc_info_vartypes(Proc, VarTypes) },
	{ proc_info_declared_determinism(Proc, DeclaredDeterminism) },
	{ proc_info_inferred_determinism(Proc, InferredDeterminism) },
	{ proc_info_varset(Proc, VarSet) },
	{ proc_info_headvars(Proc, HeadVars) },
	{ proc_info_argmodes(Proc, argument_modes(HeadInstTable, HeadModes)) },
	{ proc_info_maybe_arglives(Proc, MaybeArgLives) },
	{ proc_info_goal(Proc, Goal) },
	{ proc_info_context(Proc, ModeContext) },
	{ proc_info_get_maybe_arg_size_info(Proc, MaybeArgSize) },
	{ proc_info_get_maybe_termination_info(Proc, MaybeTermination) },
	{ proc_info_typeinfo_varmap(Proc, TypeInfoMap) },
	{ proc_info_inst_table(Proc, InstTable) },
	{ proc_info_args_method(Proc, ArgsMethod) },
	{ Indent1 is Indent + 1 },

	hlds_out__write_indent(Indent1),
	io__write_string("% mode number "),
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_int(ProcInt),
	io__write_string(" of "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" ("),
	hlds_out__write_determinism(InferredDeterminism),
	io__write_string("):\n"),
	hlds_out__write_indent(Indent),

	
	globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
	( { string__contains_char(Verbose, 't') } ->
		hlds_out__write_indent(Indent),
		io__write_string("% Arg size properties: "),
		termination__write_maybe_arg_size_info(MaybeArgSize, yes),
		io__nl,
		hlds_out__write_indent(Indent),
		io__write_string("% Termination properties: "),
		termination__write_maybe_termination_info(MaybeTermination,
			yes),
		io__nl
	;
		[]
	),

	hlds_out__write_indent(Indent),
	hlds_out__write_var_types(Indent, VarSet, AppendVarnums,
		VarTypes, TVarSet),
	hlds_out__write_typeinfo_varmap(Indent, AppendVarnums, TypeInfoMap,
		VarSet, TVarSet),

	io__write_string("% args method: "),
	hlds_out__write_args_method(ArgsMethod),
	io__nl,

	hlds_out__write_indent(Indent),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ varset__init(ModeVarSet) },
	mercury_output_pred_mode_decl(ModeVarSet, unqualified(PredName),
		HeadModes, DeclaredDeterminism, ModeContext, HeadInstTable),

	( { MaybeArgLives = yes(ArgLives) } ->
		hlds_out__write_indent(Indent),
		io__write_string("% arg lives: "),
		io__print(ArgLives),
		io__nl
	;
		[]
	),

	(
		{ ImportStatus = pseudo_imported },
		{ hlds_pred__in_in_unification_proc_id(ProcId) }
	->
		[]
	;
		{ proc_info_stack_slots(Proc, StackSlots) },
		hlds_out__write_indent(Indent),
		hlds_out__write_stack_slots(Indent, StackSlots, VarSet,
			AppendVarnums),
		hlds_out__write_indent(Indent),
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadVars, PredOrFunc),
		io__write_string(" :-\n"),
		hlds_out__write_goal(Goal, InstTable, ModuleInfo, VarSet,
			AppendVarnums, Indent1, ".")
	).

% :- pred hlds_out__write_varnames(int, map(var, string), io__state, io__state).
% :- mode hlds_out__write_varnames(in, in, di, uo) is det.
% 
% hlds_out__write_varnames(Indent, VarNames) -->
% 	{ map__to_assoc_list(VarNames, VarNameList) },
% 	(
% 		{ VarNameList = [] }
% 	->
% 		hlds_out__write_indent(Indent),
% 		io__write_string("[]\n")
% 	;
% 		hlds_out__write_indent(Indent),
% 		io__write_string("[\n"),
% 		{Indent1 is Indent + 1},
% 		hlds_out__write_varnames_2(Indent1, VarNameList),
% 		hlds_out__write_indent(Indent),
% 		io__write_string("]\n")
% 	).
% 
% :- pred hlds_out__write_varnames_2(int, list(pair(var, string)),
% 							io__state, io__state).
% :- mode hlds_out__write_varnames_2(in, in, di, uo) is det.
% 
% hlds_out__write_varnames_2(Indent, VarNameList0) -->
% 	(
% 		{ VarNameList0 = [VarId - Name|VarNameList] }
% 	->
% 		{ Indent1 is Indent + 1 },
% 		hlds_out__write_indent(Indent1),
% 		{ term__var_to_int(VarId, VarNum) },
% 		io__write_int(VarNum),
% 		io__write_string(" - "),
% 		io__write_string(Name),
% 		io__write_string("\n"),
% 		( { VarNameList = [] } ->
% 			[]
% 		;
% 			io__write_string(",\n"),
% 			hlds_out__write_varnames_2(Indent, VarNameList)
% 		)
% 	;
% 		{ error("This cannot happen") }
% 	).

:- pred hlds_out__write_vartypes(int, map(var, type), io__state, io__state).
:- mode hlds_out__write_vartypes(in, in, di, uo) is det.

hlds_out__write_vartypes(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write(X),
	io__write_string("\n").

hlds_out__write_determinism(det) -->
	io__write_string("det").
hlds_out__write_determinism(semidet) -->
	io__write_string("semidet").
hlds_out__write_determinism(nondet) -->
	io__write_string("nondet").
hlds_out__write_determinism(multidet) -->
	io__write_string("multi").
hlds_out__write_determinism(cc_nondet) -->
	io__write_string("cc_nondet").
hlds_out__write_determinism(cc_multidet) -->
	io__write_string("cc_multi").
hlds_out__write_determinism(erroneous) -->
	io__write_string("erroneous").
hlds_out__write_determinism(failure) -->
	io__write_string("failure").

hlds_out__write_can_fail(can_fail) -->
	io__write_string("can_fail").
hlds_out__write_can_fail(cannot_fail) -->
	io__write_string("cannot_fail").

hlds_out__write_code_model(model_det) -->
	io__write_string("model_det").
hlds_out__write_code_model(model_semi) -->
	io__write_string("model_semi").
hlds_out__write_code_model(model_non) -->
	io__write_string("model_non").

:- pred hlds_out__write_args_method(args_method, io__state, io__state).
:- mode hlds_out__write_args_method(in, di, uo) is det.

hlds_out__write_args_method(simple) -->
	io__write_string("simple").
hlds_out__write_args_method(compact) -->
	io__write_string("compact").

:- pred hlds_out__write_indent(int, io__state, io__state).
:- mode hlds_out__write_indent(in, di, uo) is det.

hlds_out__write_indent(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_char('\t'),
		{ Indent1 is Indent - 1 },
		hlds_out__write_indent(Indent1)
	).

%-----------------------------------------------------------------------------%
:- pred hlds_out__write_constraint_proofs(int, varset,
	map(class_constraint, constraint_proof), io__state, io__state).
:- mode hlds_out__write_constraint_proofs(in, in, in, di, uo) is det.

hlds_out__write_constraint_proofs(Indent, VarSet, Proofs) -->
	hlds_out__write_indent(Indent),
	io__write_string("% Proofs: \n"),
	{ map__to_assoc_list(Proofs, ProofsList) },
	io__write_list(ProofsList, "\n", 
		hlds_out__write_constraint_proof(Indent, VarSet)).

:- pred hlds_out__write_constraint_proof(int, varset,
	pair(class_constraint, constraint_proof), io__state, io__state).
:- mode hlds_out__write_constraint_proof(in, in, in, di, uo) is det.

hlds_out__write_constraint_proof(Indent, VarSet, Constraint - Proof) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_constraint(VarSet, Constraint),
	io__write_string(": "),
	(
		{ Proof = apply_instance(_, Num) },
		io__write_string("apply instance decl #"),
		io__write_int(Num)
	;
		{ Proof = superclass(Super) },
		io__write_string("super class of "),
		mercury_output_constraint(VarSet, Super)
	).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
