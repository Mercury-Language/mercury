%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
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

% Parse tree modules
:- import_module prog_data, (inst).
% HLDS modules
:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, instmap.

:- import_module io, bool, list, term.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_type_id(type_id, io__state, io__state).
:- mode hlds_out__write_type_id(in, di, uo) is det.

:- pred hlds_out__write_class_id(class_id, io__state, io__state).
:- mode hlds_out__write_class_id(in, di, uo) is det.

:- pred hlds_out__write_cons_id(cons_id, io__state, io__state).
:- mode hlds_out__write_cons_id(in, di, uo) is det.

:- pred hlds_out__cons_id_to_string(cons_id, string).
:- mode hlds_out__cons_id_to_string(in, out) is det.

:- pred hlds_out__aditi_builtin_name(aditi_builtin, string).
:- mode hlds_out__aditi_builtin_name(in, out) is det.

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

:- pred hlds_out__write_call_id(call_id, io__state, io__state).
:- mode hlds_out__write_call_id(in, di, uo) is det.

:- pred hlds_out__write_simple_call_id(simple_call_id, io__state, io__state).
:- mode hlds_out__write_simple_call_id(in, di, uo) is det.

:- pred hlds_out__write_simple_call_id(pred_or_func, sym_name_and_arity,
	io__state, io__state).
:- mode hlds_out__write_simple_call_id(in, in, di, uo) is det.

:- pred hlds_out__write_simple_call_id(pred_or_func, sym_name, arity,
	io__state, io__state).
:- mode hlds_out__write_simple_call_id(in, in, in, di, uo) is det.

:- pred hlds_out__simple_call_id_to_string(simple_call_id, string).
:- mode hlds_out__simple_call_id_to_string(in, out) is det.

	% Write "argument %i of call to pred_or_func `foo/n'".
	% The pred_markers argument is used to tell if the calling
	% predicate is a type class method implementation; if so,
	% we omit the "call to" part, since the user didn't write
	% any explicit call.
:- pred hlds_out__write_call_arg_id(call_id, int, pred_markers,
		io__state, io__state).
:- mode hlds_out__write_call_arg_id(in, in, in, di, uo) is det.

:- pred hlds_out__write_pred_or_func(pred_or_func, io__state, io__state).
:- mode hlds_out__write_pred_or_func(in, di, uo) is det.

:- pred hlds_out__pred_or_func_to_str(pred_or_func, string).
:- mode hlds_out__pred_or_func_to_str(in, out) is det.

	% hlds_out__write_unify_context/5 writes out a message such as
	%	foo.m:123:   in argument 3 of functor `foo/5':
	% 	foo.m:123:   in unification of `X' and `blah':
	% based on the unify_context and prog_context.
	%
:- pred hlds_out__write_unify_context(unify_context, prog_context,
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
:- pred hlds_out__write_unify_context(bool, unify_context, prog_context,
	bool, io__state, io__state).
:- mode hlds_out__write_unify_context(in, in, in, out, di, uo) is det.

:- pred hlds_out__write_determinism(determinism, io__state, io__state).
:- mode hlds_out__write_determinism(in, di, uo) is det.

:- pred hlds_out__write_can_fail(can_fail, io__state, io__state).
:- mode hlds_out__write_can_fail(in, di, uo) is det.

:- pred hlds_out__write_eval_method(eval_method, io__state, io__state).
:- mode hlds_out__write_eval_method(in, di, uo) is det.

:- pred hlds_out__write_import_status(import_status, io__state, io__state).
:- mode hlds_out__write_import_status(in, di, uo) is det.

%-----------------------------------------------------------------------------%

	% Print out an entire hlds structure.

:- pred hlds_out__write_hlds(int, module_info, io__state, io__state).
:- mode hlds_out__write_hlds(in, in, di, uo) is det.

	% hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
	%	AppendVarNums, HeadVars, PredOrFunc, Clauses, MaybeVarTypes).
:- pred hlds_out__write_clauses(int, module_info, pred_id, prog_varset, bool,
		list(prog_var), pred_or_func, list(clause), maybe_vartypes,
		io__state, io__state).
:- mode hlds_out__write_clauses(in, in, in, in, in, in, in, in, in, di, uo)
	is det.

	% hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
	%	AppendVarNums, HeadTerms, PredOrFunc, Clause,
	%	UseDeclaredModes, MaybeVarTypes).
:- pred hlds_out__write_clause(int, module_info, pred_id, prog_varset, bool,
		list(prog_term), pred_or_func, clause, bool, maybe_vartypes,
		io__state, io__state).
:- mode hlds_out__write_clause(in, in, in, in, in, in, in, in, in, in, di, uo)
	is det.

:- pred hlds_out__write_assertion(int, module_info, pred_id, prog_varset, bool,
		list(prog_var), pred_or_func, clause, maybe_vartypes,
		io__state, io__state).
:- mode hlds_out__write_assertion(in, in, in, in, in, in, in, in, in, di, uo)
	is det.

	% Print out an HLDS goal. The module_info and prog_varset give
	% the context of the goal. The boolean says whether variables should
	% have their numbers appended to them. The integer gives the level
	% of indentation to be used within the goal. The string says what
	% should end the line containing the goal; it should include a newline
	% character, but may also contain other characters before that.

:- pred hlds_out__write_goal(hlds_goal, module_info, prog_varset, bool, int,
		string, io__state, io__state).
:- mode hlds_out__write_goal(in, in, in, in, in, in, di, uo) is det.

	% hlds_out__write_goal_list is used to write both disjunctions
	% and parallel conjunctions. The module_info, prog_varset and
	% maybe_vartypes give the context of the goal. The boolean
	% says whether variables should have their numbers appended to
	% them. The integer gives the level of indentation to be used
	% within the goal. The string says what should be on the line
	% between each goal; it should include a newline character,
	% but may also contain other characters before that.

:- pred hlds_out__write_goal_list(list(hlds_goal), module_info, prog_varset,
		bool, int, string, maybe_vartypes, io__state, io__state).
:- mode hlds_out__write_goal_list(in, in, in, in, in, in, in, di, uo) is det.

	% Print out a functor and its arguments. The prog_varset gives
	% the context. The boolean says whether variables should have their
	% numbers appended to them.

:- pred hlds_out__write_functor(const, list(prog_var), prog_varset, bool,
		io__state, io__state).
:- mode hlds_out__write_functor(in, in, in, in, di, uo) is det.

	% Print out a cons_id and arguments. The module_info and prog_varset
	% give the context. The boolean says whether variables should have
	% their numbers appended to them.

:- pred hlds_out__write_functor_cons_id(cons_id, list(prog_var), prog_varset,
		module_info, bool, io__state, io__state).
:- mode hlds_out__write_functor_cons_id(in, in, in, in, in, di, uo) is det.

	% Print out the right-hand-side of a unification. The module_info and
	% the varsets give the context of the rhs. The boolean says whether
	% variables should have their numbers appended to them. The integer
	% gives the level of indentation to be used within the rhs.

:- pred hlds_out__write_unify_rhs(unify_rhs, module_info, prog_varset,
		inst_varset, bool, int, io__state, io__state).
:- mode hlds_out__write_unify_rhs(in, in, in, in, in, in, di, uo) is det.

	% Print out a list of variables and their corresponding modes
	% (e.g. for a lambda expressions). The varsets gives the context.
	% The boolean says whether variables should have their numbers
	% appended to them.

:- pred hlds_out__write_var_modes(list(prog_var), list(mode), prog_varset,
		inst_varset, bool, io__state, io__state).
:- mode hlds_out__write_var_modes(in, in, in, in, in, di, uo) is det.

:- pred hlds_out__write_instmap(instmap, prog_varset, bool, int,
	io__state, io__state).
:- mode hlds_out__write_instmap(in, in, in, in, di, uo) is det.

	% Find the name of a marker.

:- pred hlds_out__marker_name(marker, string).
:- mode hlds_out__marker_name(in, out) is det.

	% Print out the name of a marker.

:- pred hlds_out__write_marker(marker, io__state, io__state).
:- mode hlds_out__write_marker(in, di, uo) is det.

:- type maybe_vartypes
	--->	yes(tvarset, vartypes)
	;	no.

	% Convert a mode or inst to a term representation.
:- func mode_to_term(mode) = prog_term.
:- func inst_to_term(inst) = prog_term.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Parse tree modules.
:- import_module prog_out, prog_util, (inst).

% HLDS modules.
:- import_module mercury_to_mercury, purity, special_pred, instmap.
:- import_module termination, term_errors, check_typeclass, rtti.

% RL back-end modules (XXX should avoid using those here).
:- import_module rl.

% LLDS back-end modules (XXX should avoid using those here).
:- import_module code_util, llds, llds_out, trace.

% Misc
:- import_module globals, options, foreign.

% Standard library modules
:- import_module int, string, set, assoc_list, map, multi_map.
:- import_module require, getopt, std_util, term_io, varset.


hlds_out__write_type_id(Name - Arity) -->
	prog_out__write_sym_name_and_arity(Name / Arity).

hlds_out__write_class_id(class_id(Name, Arity)) -->
	prog_out__write_sym_name_and_arity(Name / Arity).

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
hlds_out__cons_id_to_string(pred_const(_, _, _), "<pred>").
hlds_out__cons_id_to_string(code_addr_const(_, _), "<code_addr>").
hlds_out__cons_id_to_string(type_ctor_info_const(_, _, _), "<type_ctor_info>").
hlds_out__cons_id_to_string(base_typeclass_info_const(_, _, _, _),
	"<base_typeclass_info>").
hlds_out__cons_id_to_string(tabling_pointer_const(_, _),
	"<tabling_pointer>").
hlds_out__cons_id_to_string(deep_profiling_proc_static(_),
	"<deep_profiling_proc_static>").

hlds_out__write_cons_id(cons(SymName, Arity)) -->
	prog_out__write_sym_name_and_arity(SymName / Arity).
hlds_out__write_cons_id(int_const(Int)) -->
	io__write_int(Int).
hlds_out__write_cons_id(string_const(String)) -->
	term_io__quote_string(String).
hlds_out__write_cons_id(float_const(Float)) -->
	io__write_float(Float).
hlds_out__write_cons_id(pred_const(_PredId, _ProcId, _)) -->
	io__write_string("<pred>").
hlds_out__write_cons_id(code_addr_const(_PredId, _ProcId)) -->
	io__write_string("<code_addr>").
hlds_out__write_cons_id(type_ctor_info_const(_, _, _)) -->
	io__write_string("<type_ctor_info>").
hlds_out__write_cons_id(base_typeclass_info_const(_, _, _, _)) -->
	io__write_string("<base_typeclass_info>").
hlds_out__write_cons_id(tabling_pointer_const(_, _)) -->
	io__write_string("<tabling_pointer>").
hlds_out__write_cons_id(deep_profiling_proc_static(_)) -->
	io__write_string("<deep_profiling_proc_static>").

	% The code of this predicate duplicates the functionality of
	% error_util__describe_one_pred_name. Changes here should be made
	% there as well.

hlds_out__write_pred_id(ModuleInfo, PredId) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	( 
		{ special_pred_name_arity(Kind, _, Name, Arity) } 
	->
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
	; 
		{ pred_info_get_markers(PredInfo, Markers) },
		{ check_marker(Markers, class_instance_method) }
	->
		io__write_string("type class method implementation")
	;
		{ pred_info_get_goal_type(PredInfo, assertion) }
	->
		io__write_string("promise")
	;
		hlds_out__write_simple_call_id(PredOrFunc,
			qualified(Module, Name), Arity)
	).

hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId) -->
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" mode "),
	{ proc_id_to_int(ProcId, ModeNum) },
	io__write_int(ModeNum).

hlds_out__write_simple_call_id(PredOrFunc - Name/Arity) -->
	hlds_out__write_simple_call_id(PredOrFunc, Name, Arity).

hlds_out__write_simple_call_id(PredOrFunc, Name/Arity) -->
	hlds_out__write_simple_call_id(PredOrFunc, Name, Arity).

hlds_out__write_simple_call_id(PredOrFunc, Name, Arity) -->
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	{ hlds_out__simple_call_id_to_sym_name_and_arity(
		PredOrFunc - Name/Arity, SymArity) },
	prog_out__write_sym_name_and_arity(SymArity),
	io__write_string("'").

:- pred hlds_out__simple_call_id_to_sym_name_and_arity(simple_call_id,
		sym_name_and_arity).
:- mode hlds_out__simple_call_id_to_sym_name_and_arity(in, out) is det.

hlds_out__simple_call_id_to_sym_name_and_arity(PredOrFunc - SymName/Arity,
		SymName/OrigArity) :-
	adjust_func_arity(PredOrFunc, OrigArity, Arity).

hlds_out__simple_call_id_to_string(CallId, String) :-
	hlds_out__simple_call_id_to_sym_name_and_arity(CallId, NameArity),
	CallId = PredOrFunc - _,
	( PredOrFunc = predicate, PorFString = "predicate"
	; PredOrFunc = function, PorFString = "function"
	),
	prog_out__sym_name_and_arity_to_string(NameArity, NameArityString),
	string__append_list([PorFString, " `", NameArityString, "'"], String).

hlds_out__write_call_id(call(PredCallId)) -->
	hlds_out__write_simple_call_id(PredCallId).
hlds_out__write_call_id(generic_call(GenericCallId)) -->
	hlds_out__write_generic_call_id(GenericCallId).

:- pred hlds_out__write_generic_call_id(generic_call_id, io__state, io__state).
:- mode hlds_out__write_generic_call_id(in, di, uo) is det.

hlds_out__write_generic_call_id(higher_order(PredOrFunc, _)) -->
	io__write_string("higher-order "),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" call").

hlds_out__write_generic_call_id(class_method(_ClassId, MethodId)) -->
	hlds_out__write_simple_call_id(MethodId).

hlds_out__write_generic_call_id(
		aditi_builtin(AditiBuiltin, CallId)) -->
	{ hlds_out__aditi_builtin_name(AditiBuiltin, Name) },
	io__write_strings(["`", Name, "' of "]),
	hlds_out__write_simple_call_id(CallId).

hlds_out__write_call_arg_id(CallId, ArgNum, PredMarkers) -->
	( { ArgNum =< 0 } ->
		% Argument numbers that are less than or equal to zero
		% are used for the type_info and typeclass_info arguments
		% that are introduced by polymorphism.m.
		% I think argument number equal to zero might also be used
		% in some other cases when we just don't have any information
		% about which argument it is.
		% For both of these, we just say "in call to"
		% rather than "in argument N of call to".
		[]
	;
		hlds_out__write_arg_number(CallId, ArgNum),
		io__write_string(" of ")
	),	
	(
		(
			% The text printed for generic calls other than
			% `aditi_call' and `class__method' does not need
			% the "call to" prefix ("in call to higher-order
			% call" is redundant, it's much better to just say
			% "in higher-order call").
			{ CallId = generic_call(GenericCall) },
			\+ { GenericCall = class_method(_, _) },
			\+ { GenericCall = aditi_builtin(aditi_call(_, _,
				_, _), _) }
		;
			% For calls from type class instance implementations
			% that were defined using the named syntax rather
			% than the clause syntax, we also omit the "call to",
			% since in that case there was no explicit call in
			% the user's source code.
			{ check_marker(PredMarkers,
				named_class_instance_method) }
		)
	->
		[]
	;
		io__write_string("call to ")
	),
	hlds_out__write_call_id(CallId).
	
:- pred hlds_out__write_arg_number(call_id, int, io__state, io__state).
:- mode hlds_out__write_arg_number(in, in, di, uo) is det.

hlds_out__write_arg_number(call(PredOrFunc - _/Arity), ArgNum) -->
	( { PredOrFunc = function, Arity = ArgNum } ->
		io__write_string("the return value")
	;
		io__write_string("argument "),
		io__write_int(ArgNum)
	).
hlds_out__write_arg_number(generic_call(higher_order(PredOrFunc, Arity)),
		ArgNum) -->
	( { PredOrFunc = function, ArgNum = Arity } ->
		io__write_string("the return value")
	;
		io__write_string("argument "),
		io__write_int(ArgNum),

		% Make error messages for higher-order calls
		% such as `P(A, B)' clearer.
		io__write_string(" (i.e. "),
		( { ArgNum = 1 } ->
			io__write_string("the "),
			hlds_out__write_pred_or_func(PredOrFunc),
			io__write_string(" term")
		;
			io__write_string("argument "),
			{ ArgNum1 is ArgNum - 1 },
			io__write_int(ArgNum1),
			io__write_string(" of the called "),
			hlds_out__write_pred_or_func(PredOrFunc)
		)
	),
	io__write_string(")").

hlds_out__write_arg_number(generic_call(class_method(_, _)), ArgNum) -->
	io__write_string("argument "),
	io__write_int(ArgNum).

hlds_out__write_arg_number(generic_call(aditi_builtin(Builtin, CallId)),
		ArgNum) -->
	hlds_out__write_aditi_builtin_arg_number(Builtin, CallId, ArgNum).

:- pred hlds_out__write_aditi_builtin_arg_number(aditi_builtin, simple_call_id,
		int, io__state, io__state).
:- mode hlds_out__write_aditi_builtin_arg_number(in, in, in, di, uo) is det.

hlds_out__write_aditi_builtin_arg_number(aditi_call(_, _, _, _), _, ArgNum) -->
	io__write_string("argument "),
	io__write_int(ArgNum).

hlds_out__write_aditi_builtin_arg_number(
		aditi_tuple_insert_delete(InsertDelete, _),
		_ - _/Arity, ArgNum) -->
	io__write_string("argument "),
	( { ArgNum =< Arity } ->
		io__write_int(ArgNum),
		io__write_string(" of the "),
		{ InsertDelete = insert, Str = "inserted"
		; InsertDelete = delete, Str = "deleted"
		},
		io__write_string(Str),
		io__write_string(" tuple")
	;
		io__write_int(ArgNum - Arity + 1)
	).

hlds_out__write_aditi_builtin_arg_number(
		aditi_insert_delete_modify(_, _, pred_term),
		_, ArgNum) -->
	io__write_string("argument "),
	io__write_int(ArgNum).

hlds_out__write_aditi_builtin_arg_number(
		aditi_insert_delete_modify(_, _, sym_name_and_closure),
		_, ArgNum) -->
	% The original goal had a sym_name/arity
	% at the front of the argument list.
	io__write_string("argument "),
	io__write_int(ArgNum + 1).

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
	prog_context, bool, io__state, io__state).
:- mode hlds_out__write_unify_main_context(in, in, in, out, di, uo) is det.

hlds_out__write_unify_main_context(First, explicit, _, First) --> [].
hlds_out__write_unify_main_context(First, head(ArgNum), Context, no) -->
	hlds_out__write_in_argument(First, ArgNum, Context),
	io__write_string(" of clause head:\n").

hlds_out__write_unify_main_context(First, head_result, Context, no) -->
	hlds_out__start_in_message(First, Context),
	io__write_string("function result term of clause head:\n").

hlds_out__write_unify_main_context(First, call(CallId, ArgNum), Context, no) -->
	hlds_out__start_in_message(First, Context),
	% The markers argument below is used only for type class method
	% implementations defined using the named syntax rather than
	% the clause syntax, and the bodies of such procedures should
	% only contain a single call, so we shouldn't get unifications
	% nested inside calls.  Hence we can safely initialize the
	% markers to empty here.  (Anyway the worst possible consequence
	% is slightly sub-optimal text for an error message.)
	{ init_markers(Markers) },
	hlds_out__write_call_arg_id(CallId, ArgNum, Markers),
	io__write_string(":\n").

:- pred hlds_out__write_unify_sub_contexts(bool, unify_sub_contexts,
	prog_context, bool, io__state, io__state).
:- mode hlds_out__write_unify_sub_contexts(in, in, in, out, di, uo) is det.

hlds_out__write_unify_sub_contexts(First, [], _, First) --> [].
hlds_out__write_unify_sub_contexts(First0, [ConsId - ArgNum | SubContexts],
		Context, First) -->
	hlds_out__write_in_argument(First0, ArgNum, Context),
	io__write_string(" of functor `"),
	hlds_out__write_cons_id(ConsId),
	io__write_string("':\n"),
	hlds_out__write_unify_sub_contexts(no, SubContexts, Context, First).

:- pred hlds_out__write_in_argument(bool, int, prog_context,
					io__state, io__state).
:- mode hlds_out__write_in_argument(in, in, in, di, uo) is det.

hlds_out__write_in_argument(First, ArgNum, Context) -->
	hlds_out__start_in_message(First, Context),
	io__write_string("argument "),
	io__write_int(ArgNum).

:- pred hlds_out__start_in_message(bool, prog_context, io__state, io__state).
:- mode hlds_out__start_in_message(in, in, di, uo) is det.

hlds_out__start_in_message(First, Context) -->
	prog_out__write_context(Context),
	( { First = yes } ->
		io__write_string("  In ")
	;
		io__write_string("  in ")
	).

%-----------------------------------------------------------------------------%

hlds_out__write_hlds(Indent, Module) -->
	{
		module_info_preds(Module, PredTable),
		module_info_types(Module, TypeTable),
		module_info_insts(Module, InstTable),
		module_info_modes(Module, ModeTable),
		module_info_classes(Module, ClassTable),
		module_info_superclasses(Module, SuperClassTable),
		module_info_instances(Module, InstanceTable)
	},
	hlds_out__write_header(Indent, Module),
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
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
		hlds_out__write_insts(Indent, InstTable),
		io__write_string("\n"),
		hlds_out__write_modes(Indent, ModeTable),
		io__write_string("\n")
	;
		[]
	),
	hlds_out__write_preds(Indent, Module, PredTable),
	hlds_out__write_footer(Indent, Module).

:- pred hlds_out__write_header(int, module_info, io__state, io__state).
:- mode hlds_out__write_header(in, in, di, uo) is det.

hlds_out__write_header(Indent, Module) -->
	{ module_info_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(":- module "),
	prog_out__write_sym_name(Name),
	io__write_string(".\n\n").

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
        globals__io_lookup_string_option(dump_hlds_options, Verbose),
	(
		{ PredIds0 = [PredId|PredIds] }
	->
		{ map__lookup(PredTable, PredId, PredInfo) },
		(
			{ \+ string__contains_char(Verbose, 'I') },
			{ pred_info_is_imported(PredInfo) }
		->
			[]
		;
			% for pseudo-imported predicates (i.e. unification
			% preds), only print them if we are using a local
			% mode for them
			{ \+ string__contains_char(Verbose, 'I') },
			{ pred_info_is_pseudo_imported(PredInfo) },
			{ pred_info_procids(PredInfo, ProcIds) },
			{ hlds_pred__in_in_unification_proc_id(ProcId) },
			{ ProcIds = [ProcId] }
		->
			[]
		;
			% We dump unification and other compiler-generated
			% special predicates if suboption 'U' is on. We don't
			% need that information to understand how the program
			% has been transformed.
			{ \+ string__contains_char(Verbose, 'U') },
			{ code_util__compiler_generated(PredInfo) }
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
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_import_status(PredInfo, ImportStatus) },
	{ pred_info_get_markers(PredInfo, Markers) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_get_class_context(PredInfo, ClassContext) },
	{ pred_info_get_constraint_proofs(PredInfo, Proofs) },
	{ pred_info_get_purity(PredInfo, Purity) },
	{ pred_info_get_head_type_params(PredInfo, HeadTypeParams) },
	{ pred_info_get_indexes(PredInfo, Indexes) },
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
	( { string__contains_char(Verbose, 'v') } ->
		{ AppendVarnums = yes }
	;
		{ AppendVarnums = no }
	),
	( { string__contains_char(Verbose, 'C') } ->
		% Information about predicates is dumped if 'C'
		% suboption is on.
		(
			{ PredOrFunc = predicate },
			mercury_output_pred_type(TVarSet, ExistQVars,
				qualified(Module, PredName),
				ArgTypes, no, Purity, ClassContext, Context,
				AppendVarnums)
		;
			{ PredOrFunc = function },
			{ pred_args_to_func_args(ArgTypes, FuncArgTypes,
				FuncRetType) },
			mercury_output_func_type(TVarSet, ExistQVars,
				qualified(Module, PredName), FuncArgTypes,
				FuncRetType, no, Purity, ClassContext,
				Context, AppendVarnums)
		)
	;
		[]
	),
	{ ClausesInfo = clauses_info(VarSet, _, _, VarTypes, HeadVars, Clauses,
		TypeInfoMap, TypeClassInfoMap, _) },
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
			io__write_string("% markers: "),
			hlds_out__write_marker_list(MarkerList),
			io__write_string("\n")
		),
		hlds_out__write_typeinfo_varmap(Indent, AppendVarnums,
			TypeInfoMap, VarSet, TVarSet),
		hlds_out__write_typeclass_info_varmap(Indent, AppendVarnums,
			TypeClassInfoMap, VarSet, TVarSet),
		( { map__is_empty(Proofs) } ->
			[]
		;
			hlds_out__write_constraint_proofs(Indent, TVarSet,
				Proofs, AppendVarnums),
			io__write_string("\n")
		),

		% XXX The indexes are not part of the clauses_info,
		% so why is this code inside this if-then-else
		% with the condition `string__contains_char(Verbose, 'C')'?
		% Shouldn't it be dependent on a different letter?

		( { Indexes = [] } ->
			[]
		;
			io__write_string("% indexes: "),
			io__write_list(Indexes, ", ",
				mercury_output_index_spec),
			io__nl
		),

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

		( { Clauses \= [] } ->
			% Never write the clauses out verbosely -
			% disable the dump_hlds_options option before writing
			% them, and restore its initial value afterwards
			globals__io_set_option(dump_hlds_options, string("")),
			hlds_out__write_clauses(Indent, ModuleInfo, PredId,
				VarSet, AppendVarnums, HeadVars, PredOrFunc,
				Clauses, no),
			globals__io_set_option(dump_hlds_options,
				string(Verbose))
		;
			[]
		)
	;
		[]
	),
	hlds_out__write_procs(Indent, AppendVarnums, ModuleInfo, PredId,
		ImportStatus, PredInfo),
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
hlds_out__marker_name(obsolete, "obsolete").
hlds_out__marker_name(class_method, "class_method").
hlds_out__marker_name(class_instance_method, "class_instance_method").
hlds_out__marker_name(named_class_instance_method,
		"named_class_instance_method").
hlds_out__marker_name((impure), "impure").
hlds_out__marker_name((semipure), "semipure").
hlds_out__marker_name(promised_pure, "promise_pure").
hlds_out__marker_name(promised_semipure, "promise_semipure").
hlds_out__marker_name(terminates, "terminates").
hlds_out__marker_name(check_termination, "check_termination").
hlds_out__marker_name(does_not_terminate, "does_not_terminate").
hlds_out__marker_name(aditi, "aditi").
hlds_out__marker_name((aditi_top_down), "aditi_top_down").
hlds_out__marker_name(base_relation, "base_relation").
hlds_out__marker_name(generate_inline, "generate_inline").
hlds_out__marker_name(aditi_memo, "aditi_memo").
hlds_out__marker_name(aditi_no_memo, "aditi_no_memo").
hlds_out__marker_name(naive, "naive").
hlds_out__marker_name(psn, "psn").
hlds_out__marker_name(supp_magic, "supp_magic").
hlds_out__marker_name(context, "context").

hlds_out__write_marker(Marker) -->
	{ hlds_out__marker_name(Marker, Name) },
	io__write_string(Name).

hlds_out__write_assertion(Indent, ModuleInfo, _PredId, VarSet, AppendVarnums,
		HeadVars, _PredOrFunc, Clause, TypeQual) -->

		% curry the varset for term_io__write_variable/4
	{ PrintVar = lambda([VarName::in, IO0::di, IO::uo] is det,
			term_io__write_variable(VarName, VarSet, IO0, IO)
		) },

	hlds_out__write_indent(Indent),
	io__write_string(":- promise all["),
	io__write_list(HeadVars, ", ", PrintVar),
	io__write_string("] (\n"),

	{ Clause = clause(_Modes, Goal, _Lang, _Context) },
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent+1, ").\n", TypeQual).



hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet, AppendVarnums,
		HeadVars, PredOrFunc, Clauses0, TypeQual) -->
	(
		{ Clauses0 = [Clause|Clauses] }
	->
		{ term__var_list_to_term_list(HeadVars, HeadTerms) },
		{ UseDeclaredModes = no },
		hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadTerms, PredOrFunc,
			Clause, UseDeclaredModes, TypeQual),
		hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadVars, PredOrFunc, Clauses, TypeQual)
	;
		[]
	).

hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
		AppendVarnums, HeadTerms, PredOrFunc, Clause,
		UseDeclaredModes, TypeQual) -->
	{
		Clause = clause(
			Modes,
			Goal,
			Lang,
			Context
		),
		Indent1 is Indent + 1
	},
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
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
	(
		{ Lang = mercury }
	;
		{ Lang = foreign_language(ForeignLang) },
		io__write_string("% Language of implementation: "),
		io__write(ForeignLang),
		io__nl
	),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_procids(PredInfo, ProcIds) },
	( { Modes = [] ; Modes = ProcIds } ->
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadTerms, PredOrFunc)
	;
		% If Modes contains more than one mode, the output will have
		% multiple clause heads. This won't be pretty and it won't be 
		% syntactically valid, but it is more useful for debugging
		% than a compiler abort during the dumping process.
		hlds_out__write_annotated_clause_heads(ModuleInfo, Context,
			PredId, Modes, VarSet, AppendVarnums, HeadTerms,
			PredOrFunc, UseDeclaredModes)
	),
	( { Goal = conj([]) - _GoalInfo } ->
		io__write_string(".\n")
	;
		io__write_string(" :-\n"),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, ".\n", TypeQual)
	).

:- pred hlds_out__write_annotated_clause_heads(module_info::in,
	term__context::in, pred_id::in, list(proc_id)::in, prog_varset::in,
	bool::in, list(prog_term)::in, pred_or_func::in, bool::in,
	io__state::di, io__state::uo) is det.

hlds_out__write_annotated_clause_heads(_, _, _, [], _, _, _, _, _) --> [].
hlds_out__write_annotated_clause_heads(ModuleInfo, Context, PredId,
		[ProcId | ProcIds], VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes) -->
	hlds_out__write_annotated_clause_head(ModuleInfo, Context, PredId,
		ProcId, VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes),
	hlds_out__write_annotated_clause_heads(ModuleInfo, Context, PredId,
		ProcIds, VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes).

:- pred hlds_out__write_annotated_clause_head(module_info::in,
	term__context::in, pred_id::in, proc_id::in, prog_varset::in,
	bool::in, list(prog_term)::in, pred_or_func::in, bool::in,
	io__state::di, io__state::uo) is det.

hlds_out__write_annotated_clause_head(ModuleInfo, Context, PredId, ProcId,
		VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procedures) },
	( { map__search(Procedures, ProcId, ProcInfo) } ->
		%
		% When writing `.opt' files, use the declared
		% argument modes so that the modes are guaranteed
		% to be syntactically identical to those in the
		% original program. The test in make_hlds.m to
		% check whether a clause matches a procedure 
		% tests for syntactic identity (roughly).
		% The modes returned by proc_info_argmodes may have
		% been slightly expanded by propagate_types_into_modes.
		%
		% We can't use the declared argument modes when writing
		% HLDS dumps because the modes of the type-infos will
		% not have been added, so the call to
		% assoc_list__from_corresponding_lists below
		% will abort. `.opt' files are written before
		% the polymorphism pass.
		%
		{ UseDeclaredModes = yes ->
			proc_info_declared_argmodes(ProcInfo, ArgModes)
		;
			proc_info_argmodes(ProcInfo, ArgModes)
		},
		{ assoc_list__from_corresponding_lists(HeadTerms, ArgModes, 
			AnnotatedPairs) },
		{ AnnotatedHeadTerms = list__map(add_mode_qualifier(Context),
			AnnotatedPairs) },
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			AppendVarnums, AnnotatedHeadTerms, PredOrFunc)
	;
		% This procedure, even though it existed in the past, has been
		% eliminated.
		[]
	).

:- pred hlds_out__write_clause_head(module_info, pred_id, prog_varset, bool,
		list(prog_term), pred_or_func, io__state, io__state).
:- mode hlds_out__write_clause_head(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_clause_head(ModuleInfo, PredId, VarSet, AppendVarnums,
			HeadTerms, PredOrFunc) -->
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(HeadTerms, FuncArgs, RetVal) },
		hlds_out__write_qualified_functor_with_term_args(
			ModuleName, term__atom(PredName), FuncArgs, VarSet,
			AppendVarnums),
		io__write_string(" = "),
		mercury_output_term(RetVal, VarSet,
			AppendVarnums, next_to_graphic_token)
	;
		{ PredOrFunc = predicate },
		hlds_out__write_qualified_functor_with_term_args(
			ModuleName, term__atom(PredName), HeadTerms, VarSet,
			AppendVarnums)
	).

hlds_out__write_goal(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow) -->
		% don't type qualify everything
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, no).

	% TypeQual is yes(TVarset, VarTypes) if all constructors should
	% be module qualified.
:- pred hlds_out__write_goal_a(hlds_goal, module_info, prog_varset, bool, int,
	string, maybe_vartypes, io__state, io__state).
:- mode hlds_out__write_goal_a(in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_goal_a(Goal - GoalInfo, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
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
			% XXX should avoid dependency on trace.m here
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
			{ FollowVars = follow_vars(FollowVarsMap, NextReg) },
			{ map__to_assoc_list(FollowVarsMap, FVlist) },
			hlds_out__write_indent(Indent),
			io__write_string("% follow vars: "),
			io__write_int(NextReg),
			io__write_string("\n"),
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
	hlds_out__write_goal_2(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual),
	( { string__contains_char(Verbose, 'i') } ->
		{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
		(
			{ instmap_delta_is_reachable(InstMapDelta) },
			{ instmap_delta_changed_vars(InstMapDelta, Vars) },
			{ set__empty(Vars) }
		->
			[]
		;
			hlds_out__write_indent(Indent),
			io__write_string("% new insts: "),
			hlds_out__write_instmap_delta(InstMapDelta, VarSet,
				AppendVarnums, Indent),
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
	( { string__contains_char(Verbose, 'g') } ->
		{ goal_info_get_features(GoalInfo, Features) },
		{ set__to_sorted_list(Features, Flist) },
		( { Flist = [] } ->
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

:- pred hlds_out__write_goal_2(hlds_goal_expr, module_info, prog_varset, bool,
	int, string, maybe_vartypes, io__state, io__state).
:- mode hlds_out__write_goal_2(in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_goal_2(switch(Var, CanFail, CasesList, _), ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("( % "),
	hlds_out__write_can_fail(CanFail),
	io__write_string(" switch on `"),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string("'\n"),
	{ Indent1 is Indent + 1 },
	( { CasesList = [Case | Cases] } ->
		hlds_out__write_case(Case, Var, ModuleInfo,
			VarSet, AppendVarnums, Indent1, TypeQual),
		hlds_out__write_cases(Cases, Var, ModuleInfo,
			VarSet, AppendVarnums, Indent, TypeQual)
	;
		hlds_out__write_indent(Indent1),
		io__write_string("fail\n")
	),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow).

hlds_out__write_goal_2(some(Vars, CanRemove, Goal), ModuleInfo,
		VarSet, AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("some ["),
	mercury_output_vars(Vars, VarSet, AppendVarnums),
	io__write_string("] ("),
	( { CanRemove = cannot_remove } ->
		io__write_string(" % (cannot remove)")
	;
		[]
	),
	io__nl,
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow).

hlds_out__write_goal_2(if_then_else(Vars, Cond, Then, Else, _), ModuleInfo,
		VarSet, AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("(if"),
	hlds_out__write_some(Vars, VarSet),
	io__write_string("\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Cond, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string("then\n"),
	hlds_out__write_goal_a(Then, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string("else\n"),
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
	(
		{ Verbose \= "" },
		{ Else = if_then_else(_, _, _, _, _) - _ }
	->
		hlds_out__write_goal_a(Else, ModuleInfo, VarSet, AppendVarnums,
			Indent, "\n", TypeQual)
	;
		hlds_out__write_goal_a(Else, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual)
	),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow).

hlds_out__write_goal_2(not(Goal), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("\\+ (\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow).

hlds_out__write_goal_2(conj(List), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	( { List = [Goal | Goals] } ->
		globals__io_lookup_string_option(dump_hlds_options, Verbose),
		( { Verbose \= "" } ->
			{ Indent1 is Indent + 1 },
			hlds_out__write_indent(Indent),
			io__write_string("( % conjunction\n"),
			hlds_out__write_conj(Goal, Goals, ModuleInfo, VarSet,
				AppendVarnums, Indent1, "\n", Verbose, ",\n",
				TypeQual),
			hlds_out__write_indent(Indent),
			io__write_string(")"),
			io__write_string(Follow)
		;
			hlds_out__write_conj(Goal, Goals, ModuleInfo, VarSet,
				AppendVarnums, Indent, Follow, Verbose, ",\n",
				TypeQual)
		)
	;
		hlds_out__write_indent(Indent),
		io__write_string("true"),
		io__write_string(Follow)
	).

hlds_out__write_goal_2(par_conj(List, _), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	( { List = [Goal | Goals] } ->
		io__write_string("( % parallel conjunction\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual),
			% See comments at hlds_out__write_goal_list.
		hlds_out__write_goal_list(Goals, ModuleInfo, VarSet,
			AppendVarnums, Indent, "&\n", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")"),
		io__write_string(Follow)
	;
		io__write_string("/* parallel */ true"),
		io__write_string(Follow)
	).

hlds_out__write_goal_2(disj(List, _), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	( { List = [Goal | Goals] } ->
		io__write_string("( % disjunction\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual),
		hlds_out__write_goal_list(Goals, ModuleInfo, VarSet,
			AppendVarnums, Indent, ";\n", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")"),
		io__write_string(Follow)
	;
		io__write_string("fail"),
		io__write_string(Follow)
	).

hlds_out__write_goal_2(generic_call(GenericCall, ArgVars, _, _),
		ModuleInfo, VarSet, AppendVarnums, Indent, Follow, _) -->
		% XXX we should print more info here
    ( 
	{ GenericCall = higher_order(PredVar, PredOrFunc, _) },
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
	hlds_out__write_indent(Indent),
	(
		{ PredOrFunc = predicate },
		( { string__contains_char(Verbose, 'l') } ->
			io__write_string("% higher-order predicate call\n"),
			hlds_out__write_indent(Indent)
		;
			[]
		),
		hlds_out__write_functor(term__atom("call"),
				[PredVar|ArgVars], VarSet, AppendVarnums)
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
	io__write_string(Follow)
    ; 
	{ GenericCall = class_method(TCInfoVar, MethodNum,
		_ClassId, _MethodId) },
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
	hlds_out__write_indent(Indent),
	( { string__contains_char(Verbose, 'l') } ->
		io__write_string("% class method call\n"),
		hlds_out__write_indent(Indent)
	;
		[]
	),
	{ term__context_init(Context) },
	{ Functor = term__atom("class_method_call") },
	{ TCInfoTerm = term__variable(TCInfoVar) },
	{ MethodNumTerm = term__functor(term__integer(MethodNum), [],
			Context) },
	{ term__var_list_to_term_list(ArgVars, ArgTerms) },
	{ Term = term__functor(Functor, [TCInfoTerm, MethodNumTerm | ArgTerms],
			Context) },
	mercury_output_term(Term, VarSet, AppendVarnums),
	io__write_string(Follow)
    ;
	{ GenericCall = aditi_builtin(AditiBuiltin, CallId) },
	hlds_out__write_aditi_builtin(ModuleInfo, AditiBuiltin, CallId,
		ArgVars, VarSet, AppendVarnums, Indent, Follow)
    ).

hlds_out__write_goal_2(call(PredId, ProcId, ArgVars, Builtin,
			MaybeUnifyContext, PredName),
		ModuleInfo, VarSet, AppendVarnums, Indent, Follow, TypeQual) -->
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
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
	( { invalid_pred_id(PredId) } ->
			% If we don't know then the call must be treated
			% as a predicate.
		{ PredOrFunc = predicate }
	;
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ pred_info_get_purity(PredInfo, Purity) },
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		write_purity_prefix(Purity)
	),
	(
		{ PredOrFunc = predicate },
		{ NewArgVars = ArgVars }
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ArgVars, NewArgVars, LHSVar) },
		mercury_output_var(LHSVar, VarSet, AppendVarnums),
		io__write_string(" = ")
	),
	hlds_out__write_sym_name_and_args(PredName, NewArgVars, VarSet,
			AppendVarnums),
	io__write_string(Follow),
	( { string__contains_char(Verbose, 'l') } ->
		{ pred_id_to_int(PredId, PredNum) },
		{ proc_id_to_int(ProcId, ProcNum) },
		hlds_out__write_indent(Indent),
		io__write_string("% pred id: "),
		io__write_int(PredNum),
		io__write_string(", proc id: "),
		io__write_int(ProcNum),
		io__write_string(Follow),
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
				% XXX Fake the inst varset
			{ varset__init(InstVarSet) },
			hlds_out__write_unify_rhs_2(RHS, ModuleInfo, VarSet,
				InstVarSet, AppendVarnums, Indent, Follow,
				VarType, TypeQual)
		;
			[]
		)
	;
		[]
	).

hlds_out__write_goal_2(unify(A, B, _, Unification, _), ModuleInfo, VarSet,
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
		% XXX Fake the inst varset
	{ varset__init(InstVarSet) },
	hlds_out__write_unify_rhs_2(B, ModuleInfo, VarSet, InstVarSet,
		AppendVarnums, Indent, Follow, VarType, TypeQual),
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
	(
		{
			string__contains_char(Verbose, 'u') 
		;
			string__contains_char(Verbose, 'p')
		}
	->
		(
			% don't output bogus info if we haven't been through
			% mode analysis yet
			{ Unification = complicated_unify(Mode, CanFail,
					TypeInfoVars) },
			{ CanFail = can_fail },
			{ Mode = (free - free -> free - free) },
			{ TypeInfoVars = [] }
		->
			[]
		;
			hlds_out__write_unification(Unification, ModuleInfo,
				VarSet, InstVarSet, AppendVarnums, Indent)
		)
	;
		[]
	).

hlds_out__write_goal_2(foreign_proc(Attributes, _, _, ArgVars,
		ArgNames, _, PragmaCode), _, _, _, Indent, Follow, _) -->
	{ foreign_language(Attributes, ForeignLang) },
	hlds_out__write_indent(Indent),
	io__write_string("$pragma_foreign_proc( /* "),
	io__write_string(foreign_language_string(ForeignLang)),
	io__write_string(" */ ["),
	hlds_out__write_varnum_list(ArgVars),
	io__write_string("], ["),
	{ get_pragma_foreign_var_names(ArgNames, Names) },
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
	;
		{ PragmaCode = import(Name, _, _, _Context) },
		io__write_string(""""),
		io__write_string(Name),
		io__write_string("""")
	),
	io__write_string(")"),
	io__write_string(Follow).

hlds_out__write_goal_2(shorthand(ShortHandGoal), ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_goal_2_shorthand(ShortHandGoal, ModuleInfo,
		VarSet, AppendVarnums, Indent, Follow, TypeQual).


:- pred hlds_out__write_goal_2_shorthand(shorthand_goal_expr, module_info,
	prog_varset, bool, int, string, maybe_vartypes, io__state, io__state).
:- mode hlds_out__write_goal_2_shorthand(in, in, in, in, in, in, in, di, uo)
	is det.

hlds_out__write_goal_2_shorthand(bi_implication(LHS, RHS), ModuleInfo, 
		VarSet,	AppendVarnums, Indent, Follow, TypeQual) -->
	hlds_out__write_indent(Indent),
	io__write_string("( % bi-implication\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_goal_a(LHS, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string("<=>\n"),
	hlds_out__write_goal_a(RHS, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual),
	hlds_out__write_indent(Indent),
	io__write_string(")"),
	io__write_string(Follow).



:- pred hlds_out__write_varnum_list(list(prog_var), io__state, io__state).
:- mode hlds_out__write_varnum_list(in, di, uo) is det.

hlds_out__write_varnum_list([]) --> [].
hlds_out__write_varnum_list([Var]) -->
	hlds_out__write_varnum(Var).
hlds_out__write_varnum_list([Var1, Var2 | Vars]) -->
	hlds_out__write_varnum(Var1),
	io__write_string(", "),
	hlds_out__write_varnum_list([Var2 | Vars]).

:- pred hlds_out__write_varnum(var(T), io__state, io__state).
:- mode hlds_out__write_varnum(in, di, uo) is det.

hlds_out__write_varnum(Var) -->
	{ term__var_to_int(Var, VarNum) },
	io__write_int(VarNum).

:- pred hlds_out__write_var_name_list(list(pair(var(T), string)),
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

:- pred hlds_out__write_aditi_builtin(module_info, aditi_builtin,
	simple_call_id, list(prog_var), prog_varset, bool, int, string,
	io__state, io__state).
:- mode hlds_out__write_aditi_builtin(in, in, in, in, in, in, in, in,
	di, uo) is det.

hlds_out__write_aditi_builtin(ModuleInfo,
		aditi_call(PredProcId, _NumInputs, _InputTypes, _NumOutputs),
		_CallId, ArgVars, VarSet, AppendVarnums,
		Indent, Follow) -->
	hlds_out__write_indent(Indent),	
	io__write_string("aditi_call "),
	% XXX should avoid dependency on rl.m here
	{ rl__get_entry_proc_name(ModuleInfo, PredProcId, ProcName) },
	io__write(ProcName),
	io__write_string("("),
	mercury_output_vars(ArgVars, VarSet, AppendVarnums),
	io__write_string(")"),
	io__write_string(Follow),
	io__nl.

hlds_out__write_aditi_builtin(_ModuleInfo,
		aditi_tuple_insert_delete(InsertDelete, PredId), CallId,
		ArgVars, VarSet, AppendVarnums, Indent, Follow) -->
	% make_hlds.m checks the arity so this cannot fail. 
	{ get_state_args_det(ArgVars, Args, State0Var, StateVar) },
	hlds_out__write_indent(Indent),	

	( { InsertDelete = insert }, io__write_string("aditi_insert(")
	; { InsertDelete = delete }, io__write_string("aditi_delete(")
	),

	{ CallId = PredOrFunc - SymName/_ },
	(
		{ PredOrFunc = predicate },
		hlds_out__write_sym_name_and_args(SymName, Args,
			VarSet, AppendVarnums)
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(Args, FuncArgs, RetArg) },
		io__write_string("("),
		hlds_out__write_sym_name_and_args(SymName, FuncArgs,
			VarSet, AppendVarnums),	
		io__write_string(" = "),
		mercury_output_var(RetArg, VarSet, AppendVarnums),
		io__write_string(")")
	),
	io__write_string(", "),
	mercury_output_var(State0Var, VarSet, AppendVarnums),
	io__write_string(", "),
	mercury_output_var(StateVar, VarSet, AppendVarnums),
	io__write_string(")"),
	io__write_string(Follow),
	io__nl,
	hlds_out__write_aditi_builtin_pred_id(Indent, PredId).

hlds_out__write_aditi_builtin(_ModuleInfo, Builtin, CallId,
		ArgVars, VarSet, AppendVarnums, Indent, Follow) -->
	{ Builtin = aditi_insert_delete_modify(_, PredId, _Syntax) },
	hlds_out__write_indent(Indent),	
	{ hlds_out__aditi_builtin_name(Builtin, UpdateName) },
	io__write_string(UpdateName),
	io__write_string("("),
	{ CallId = PredOrFunc - _ },
	{ hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr) },
	io__write_string(PredOrFuncStr),
	io__write_string(" "),
	{ hlds_out__simple_call_id_to_sym_name_and_arity(CallId, SymArity) },
	prog_out__write_sym_name_and_arity(SymArity),
	io__write_string(", "),
	mercury_output_vars(ArgVars, VarSet, AppendVarnums),	
	io__write_string(")"),
	io__write_string(Follow),
	io__nl,
	hlds_out__write_aditi_builtin_pred_id(Indent, PredId).

:- pred hlds_out__write_aditi_builtin_pred_id(int, pred_id,
		io__state, io__state).
:- mode hlds_out__write_aditi_builtin_pred_id(in, in, di, uo) is det.

hlds_out__write_aditi_builtin_pred_id(Indent, PredId) -->
	hlds_out__write_indent(Indent),
	io__write_string("% Update of pred_id: "),
	{ pred_id_to_int(PredId, PredInt) },
	io__write_int(PredInt),
	io__write_string(".\n").

hlds_out__aditi_builtin_name(aditi_call(_, _, _, _), "aditi_call").
hlds_out__aditi_builtin_name(aditi_tuple_insert_delete(_, _), "aditi_insert").
hlds_out__aditi_builtin_name(aditi_insert_delete_modify(InsertDelMod, _, _),
		Name) :-
	hlds_out__aditi_insert_delete_modify_name(InsertDelMod, Name).

:- pred hlds_out__aditi_insert_delete_modify_name(aditi_insert_delete_modify,
		string).
:- mode hlds_out__aditi_insert_delete_modify_name(in, out) is det.

hlds_out__aditi_insert_delete_modify_name(bulk_insert, "aditi_bulk_insert").
hlds_out__aditi_insert_delete_modify_name(delete(bulk), "aditi_bulk_delete").
hlds_out__aditi_insert_delete_modify_name(delete(filter), "aditi_delete").
hlds_out__aditi_insert_delete_modify_name(modify(bulk), "aditi_bulk_modify").
hlds_out__aditi_insert_delete_modify_name(modify(filter), "aditi_modify").

:- pred hlds_out__write_unification(unification, module_info, prog_varset,
		inst_varset, bool, int, io__state, io__state).
:- mode hlds_out__write_unification(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_unification(assign(X, Y), _, ProgVarSet, _InstVarSet,
		AppendVarnums, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(X, ProgVarSet, AppendVarnums),
	io__write_string(" := "),
	mercury_output_var(Y, ProgVarSet, AppendVarnums),
	io__write_string("\n").

hlds_out__write_unification(simple_test(X, Y), _, ProgVarSet, _, AppendVarnums,
		Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(X, ProgVarSet, AppendVarnums),
	io__write_string(" == "),
	mercury_output_var(Y, ProgVarSet, AppendVarnums),
	io__write_string("\n").

hlds_out__write_unification(construct(Var, ConsId, ArgVars, ArgModes, _, _, _),
		ModuleInfo, ProgVarSet, InstVarSet, AppendVarnums, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, ProgVarSet, AppendVarnums),
	io__write_string(" := "),
	hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes,
		ModuleInfo, ProgVarSet, InstVarSet, AppendVarnums, Indent).

hlds_out__write_unification(deconstruct(Var, ConsId, ArgVars, ArgModes,
		CanFail, CanCGC),
		ModuleInfo, ProgVarSet, InstVarSet, AppendVarnums, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% Compile time garbage collect: "),
	io__write(CanCGC),
	io__nl,
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, ProgVarSet, AppendVarnums),
	( { CanFail = can_fail },
		io__write_string(" ?= ")
	; { CanFail = cannot_fail },
		io__write_string(" => ")
	),
	hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes,
		ModuleInfo, ProgVarSet, InstVarSet, AppendVarnums, Indent).

hlds_out__write_unification(complicated_unify(Mode, CanFail, TypeInfoVars),
		_ModuleInfo, ProgVarSet, InstVarSet, AppendVarNums, Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	( { CanFail = can_fail },
		io__write_string("can_fail, ")
	; { CanFail = cannot_fail },
		io__write_string("cannot_fail, ")
	),
	io__write_string("mode: "),
	mercury_output_uni_mode(Mode, InstVarSet),
	io__write_string("\n"),
	hlds_out__write_indent(Indent),
	io__write_string("% type-info vars: "),
	mercury_output_vars(TypeInfoVars, ProgVarSet, AppendVarNums),
	io__write_string("\n").


:- pred hlds_out_write_functor_and_submodes(cons_id, list(prog_var),
		list(uni_mode), module_info, prog_varset, inst_varset,
		bool, int, io__state, io__state).
:- mode hlds_out_write_functor_and_submodes(in, in, in, in, in, in, in, in,
		di, uo) is det.

hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes, _ModuleInfo,
		ProgVarSet, InstVarSet, AppendVarnums, Indent) -->
	hlds_out__write_cons_id(ConsId),
	( { ArgVars = [] } ->
		io__write_string("\n")
	;
		io__write_string(" ("),
		mercury_output_vars(ArgVars, ProgVarSet, AppendVarnums),
		io__write_string(")\n"),
		globals__io_lookup_string_option(dump_hlds_options, Verbose),
		( { string__contains_char(Verbose, 'a') } ->
			hlds_out__write_indent(Indent),
			io__write_string("% arg-modes "),
			mercury_output_uni_mode_list(ArgModes, InstVarSet),
			io__write_string("\n")
		;
			[]
		)
	).

hlds_out__write_unify_rhs(Rhs, ModuleInfo, VarSet, InstVarSet, AppendVarnums,
		Indent) -->
	hlds_out__write_unify_rhs_3(Rhs, ModuleInfo, VarSet, InstVarSet,
		AppendVarnums, Indent, no, no).

:- pred hlds_out__write_unify_rhs_2(unify_rhs, module_info, prog_varset,
		inst_varset, bool, int, string, maybe(type), maybe_vartypes,
		io__state, io__state).
:- mode hlds_out__write_unify_rhs_2(in, in, in, in, in, in, in, in, in, di, uo)
	is det.

hlds_out__write_unify_rhs_2(Rhs, ModuleInfo, VarSet, InstVarSet, AppendVarnums,
		Indent, Follow, MaybeType, TypeQual) -->
	hlds_out__write_unify_rhs_3(Rhs, ModuleInfo, VarSet, InstVarSet,
		AppendVarnums, Indent, MaybeType, TypeQual),
	io__write_string(Follow).

:- pred hlds_out__write_unify_rhs_3(unify_rhs, module_info, prog_varset,
	inst_varset, bool, int, maybe(type), maybe_vartypes, io__state, io__state).
:- mode hlds_out__write_unify_rhs_3(in, in, in, in, in, in, in, in,
	di, uo) is det.

hlds_out__write_unify_rhs_3(var(Var), _, VarSet, _, AppendVarnums, _, _, _) -->
	mercury_output_var(Var, VarSet, AppendVarnums).
hlds_out__write_unify_rhs_3(functor(ConsId, ArgVars), ModuleInfo, VarSet, _,
		AppendVarnums, _Indent, MaybeType, TypeQual) -->
	hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet, ModuleInfo,
		AppendVarnums),
	( { MaybeType = yes(Type), TypeQual = yes(TVarSet, _) } ->
		io__write_string(" `with_type` "),
		mercury_output_term(Type, TVarSet, no, next_to_graphic_token)
	;
		[]
	).

hlds_out__write_unify_rhs_3(
		lambda_goal(PredOrFunc, EvalMethod, _, NonLocals, Vars, Modes,
		Det, Goal),
		ModuleInfo, VarSet, InstVarSet, AppendVarnums, Indent,
		MaybeType, TypeQual)
		-->
	{ Indent1 is Indent + 1 },
	{
		EvalMethod = normal,
		EvalStr = ""
	;
		EvalMethod = (aditi_bottom_up),
		EvalStr = "aditi_bottom_up "
	;
		EvalMethod = (aditi_top_down),
		EvalStr = "aditi_top_down "
	},
	(
		{ PredOrFunc = predicate },
		io__write_string("("),
		io__write_string(EvalStr),
		( { Vars = [] } ->
			io__write_string("(pred)")
		;
			io__write_string("pred("),
			hlds_out__write_var_modes(Vars, Modes, VarSet,
				InstVarSet, AppendVarnums),
			io__write_string(")")
		),
		io__write_string(" is "),
		mercury_output_det(Det),
		io__write_string(" :-\n"),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")")
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
		{ pred_args_to_func_args(Vars, ArgVars, RetVar) },
		io__write_string("("),
		io__write_string(EvalStr),
		( { ArgVars = [] } ->
			io__write_string("(func)")
		;
			io__write_string("func("),
			hlds_out__write_var_modes(ArgVars, ArgModes, VarSet,
				InstVarSet, AppendVarnums),
			io__write_string(")")
		),
		io__write_string(" = ("),
		hlds_out__write_var_mode(RetVar, RetMode, VarSet,
			InstVarSet, AppendVarnums),
		io__write_string(") is "),
		mercury_output_det(Det),
		io__write_string(" :-\n"),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual),
		hlds_out__write_indent(Indent),
		io__write_string(")")
	),
	( { MaybeType = yes(Type), TypeQual = yes(TVarSet, _) } ->
		io__write_string(" `with_type` "),
		mercury_output_term(Type, TVarSet, AppendVarnums,
			next_to_graphic_token)
	;
		[]
	),
        globals__io_lookup_string_option(dump_hlds_options, Verbose),
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

:- pred hlds_out__write_sym_name_and_args(sym_name, list(prog_var),
		prog_varset, bool, io__state, io__state).
:- mode hlds_out__write_sym_name_and_args(in, in, in, in, di, uo) is det.

hlds_out__write_sym_name_and_args(PredName, ArgVars, VarSet, AppendVarnums) -->
	(
		{ PredName = qualified(ModuleName, Name) },
		hlds_out__write_qualified_functor(ModuleName,
			term__atom(Name),
			ArgVars, VarSet, AppendVarnums)
	;
		{ PredName = unqualified(Name) },
		hlds_out__write_functor(term__atom(Name),
			ArgVars, VarSet, AppendVarnums, next_to_graphic_token)
	).

hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums) -->
	hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums,
		not_next_to_graphic_token).

:- pred hlds_out__write_functor(const, list(prog_var), prog_varset, bool,
		needs_quotes,
	io__state, io__state).
:- mode hlds_out__write_functor(in, in, in, in, in, di, uo) is det.

hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums, 
		NextToGraphicToken) -->
	{ term__context_init(Context) },
	{ term__var_list_to_term_list(ArgVars, ArgTerms) },
	{ Term = term__functor(Functor, ArgTerms, Context) },
	mercury_output_term(Term, VarSet, AppendVarnums, NextToGraphicToken).


:- pred hlds_out__write_qualified_functor(module_name, const, list(prog_var),
		prog_varset, bool, io__state, io__state).
:- mode hlds_out__write_qualified_functor(in, in, in, in, in, di, uo) is det.

hlds_out__write_qualified_functor(ModuleName, Functor, ArgVars, VarSet,
		AppendVarnums) -->
	mercury_output_bracketed_sym_name(ModuleName),
	io__write_string(":"),
	hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums,
		next_to_graphic_token).

:- pred hlds_out__write_qualified_functor_with_term_args(module_name, const,
		list(prog_term), prog_varset, bool, io__state, io__state).
:- mode hlds_out__write_qualified_functor_with_term_args(in, in, in,
		in, in, di, uo) is det.

hlds_out__write_qualified_functor_with_term_args(ModuleName, Functor,
		ArgTerms, VarSet, AppendVarNums) -->
	mercury_output_bracketed_sym_name(ModuleName),
	io__write_string(":"),
	{ term__context_init(Context) },
	mercury_output_term(term__functor(Functor, ArgTerms, Context), VarSet,
		AppendVarNums, next_to_graphic_token).

hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet, ModuleInfo,
		AppendVarnums) -->
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
				ArgVars, VarSet, AppendVarnums,
				next_to_graphic_token)
		)
	;
		{ ConsId = int_const(Int) },
		hlds_out__write_functor(term__integer(Int), ArgVars,
			VarSet, AppendVarnums)
	;
		{ ConsId = float_const(Float) },
		hlds_out__write_functor(term__float(Float), ArgVars,
			VarSet, AppendVarnums)
	;
		{ ConsId = string_const(Str) },
		hlds_out__write_functor(term__string(Str), ArgVars,
			VarSet, AppendVarnums)
	;
		{ ConsId = pred_const(PredId, _, _) },
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ pred_info_module(PredInfo, PredModule) },
		{ pred_info_name(PredInfo, PredName) },
		hlds_out__write_functor_cons_id(
			cons(qualified(PredModule, PredName),
				list__length(ArgVars)),
			ArgVars, VarSet, ModuleInfo, AppendVarnums)
	;
		{ ConsId = code_addr_const(PredId, ProcId) },
		io__write_string("code_addr_const("),
		hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId),
		io__write_string(")")
	;
		{ ConsId = type_ctor_info_const(Module, Name, Arity) },
		io__write_string("type_ctor_info("""),
		prog_out__write_sym_name(Module),
		io__write_string(""", """),
		io__write_string(Name),
		io__write_string(""", "),
		io__write_int(Arity),
		io__write_string(")")
	;
		{ ConsId = base_typeclass_info_const(Module,
			class_id(Name, Arity), _, Instance) },
		io__write_string("base_typeclass_info("""),
		prog_out__write_sym_name(Module),
		io__write_string(""", "),
		io__write_string("class_id("),
		prog_out__write_sym_name(Name),
		io__write_string(", "),
		io__write_int(Arity),
		io__write_string("), "),
		io__write_string(Instance),
		io__write_string(")")
	;
		{ ConsId = tabling_pointer_const(PredId, ProcId) },
		io__write_string("tabling_pointer_const("),
		hlds_out__write_pred_id(ModuleInfo, PredId),
		io__write_string(", "),
		{ proc_id_to_int(ProcId, ProcIdInt) },
		io__write_int(ProcIdInt),
		io__write_string(")")
	;
		{ ConsId = deep_profiling_proc_static(RttiProcLabel) },
		{ rtti__proc_label_pred_proc_id(RttiProcLabel,
			PredId, ProcId) },
		io__write_string("deep_profiling_proc_static("),
		hlds_out__write_pred_id(ModuleInfo, PredId),
		{ proc_id_to_int(ProcId, ProcIdInt) },
		io__write_string(" (mode "),
		io__write_int(ProcIdInt),
		io__write_string("))")
	).

hlds_out__write_var_modes([], [], _, _, _) --> [].
hlds_out__write_var_modes([Var|Vars], [Mode|Modes], VarSet, InstVarSet,
		AppendVarnums) -->
	hlds_out__write_var_mode(Var, Mode, VarSet, InstVarSet, AppendVarnums),
	( { Vars \= [] } ->
		io__write_string(", ")
	;
		[]
	),
	hlds_out__write_var_modes(Vars, Modes, VarSet, InstVarSet,
		AppendVarnums).

hlds_out__write_var_modes([], [_|_], _, _, _) -->
	{ error("hlds_out__write_var_modes: length mis-match") }.
hlds_out__write_var_modes([_|_], [], _, _, _) -->
	{ error("hlds_out__write_var_modes: length mis-match") }.

:- pred hlds_out__write_var_mode(prog_var, mode, prog_varset, inst_varset, bool,
		io__state, io__state).
:- mode hlds_out__write_var_mode(in, in, in, in, in, di, uo) is det.

hlds_out__write_var_mode(Var, Mode, VarSet, InstVarSet, AppendVarnums) -->
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string("::"),
	mercury_output_mode(Mode, InstVarSet).

:- pred hlds_out__write_conj(hlds_goal, list(hlds_goal), module_info,
		prog_varset, bool, int, string, string, string, maybe_vartypes,
		io__state, io__state).
:- mode hlds_out__write_conj(in, in, in, in, in, in, in, in, in, in,
	di, uo) is det.

hlds_out__write_conj(Goal1, Goals1, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, Verbose, Separator, TypeQual) -->
	(
		{ Goals1 = [Goal2 | Goals2] }
	->
		( { Verbose \= "" } ->
			% when generating verbose dumps,
			% we want the comma on its own line,
			% since that way it visually separates
			% the lines after one goal
			% and the lines before the next
			hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
				AppendVarnums, Indent, "\n", TypeQual),
			hlds_out__write_indent(Indent),
			io__write_string(Separator)
		;
			hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
				AppendVarnums, Indent, Separator, TypeQual)
		),
		hlds_out__write_conj(Goal2, Goals2, ModuleInfo, VarSet,
			AppendVarnums, Indent, Follow, Verbose, Separator,
			TypeQual)
	;
		hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
			AppendVarnums, Indent, Follow, TypeQual)
	).

hlds_out__write_goal_list(GoalList, ModuleInfo, VarSet, AppendVarnums, Indent,
		Separator, TypeQual) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		hlds_out__write_indent(Indent),
		io__write_string(Separator),
		{ Indent1 is Indent + 1 },
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet,
			AppendVarnums, Indent1, "\n", TypeQual),
		hlds_out__write_goal_list(Goals, ModuleInfo, VarSet,
			AppendVarnums, Indent, Separator, TypeQual)
	;
		[]
	).

:- pred hlds_out__write_case(case, prog_var, module_info, prog_varset, bool,
		int, maybe_vartypes, io__state, io__state).
:- mode hlds_out__write_case(in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_case(case(ConsId, Goal), Var, ModuleInfo, VarSet,
		AppendVarnums, Indent, VarTypes) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string(" has functor "),
	hlds_out__write_cons_id(ConsId),
	io__write_string("\n"),
	% XXX if the output of this is to be used, e.g. in
	% inter-module optimization, output a unification to bind the
	% Var to the functor, since simplify.m and unused_args.m remove
	% the unification. At the moment this is not a problem, since
	% intermod.m works on the unoptimized clauses.
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent, "\n", VarTypes).

:- pred hlds_out__write_cases(list(case), prog_var, module_info, prog_varset,
		bool, int, maybe_vartypes, io__state, io__state).
:- mode hlds_out__write_cases(in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_cases(CasesList, Var, ModuleInfo, VarSet, AppendVarnums,
		Indent, VarTypes) -->
	(
		{ CasesList = [Case | Cases] }
	->
		hlds_out__write_indent(Indent),
		io__write_string(";\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_case(Case, Var, ModuleInfo,
			VarSet, AppendVarnums, Indent1, VarTypes),
		hlds_out__write_cases(Cases, Var, ModuleInfo,
			VarSet, AppendVarnums, Indent, VarTypes)
	;
		[]
	).

:- pred hlds_out__write_some(list(prog_var), prog_varset, io__state, io__state).
:- mode hlds_out__write_some(in, in, di, uo) is det.

	% quantification is all implicit by the time we get to the hlds.

hlds_out__write_some(_Vars, _VarSet) --> [].

hlds_out__write_instmap(InstMap, VarSet, AppendVarnums, Indent) -->
	( { instmap__is_unreachable(InstMap) } ->
		io__write_string("unreachable")
	;
		{ instmap__to_assoc_list(InstMap, AssocList) },
		hlds_out__write_instmap_2(AssocList, VarSet, AppendVarnums,
			Indent)
	).

:- pred hlds_out__write_instmap_2(assoc_list(prog_var, inst), prog_varset, bool,
		int, io__state, io__state).
:- mode hlds_out__write_instmap_2(in, in, in, in, di, uo) is det.

hlds_out__write_instmap_2([], _, _, _) --> [].
hlds_out__write_instmap_2([Var - Inst | Rest], VarSet, AppendVarnums, Indent)
		-->
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string(" -> "),
	{ varset__init(InstVarSet) },
	mercury_output_inst(Inst, InstVarSet),
	( { Rest = [] } ->
		[]
	;
		mercury_output_newline(Indent),
		io__write_string("%            "),
		hlds_out__write_instmap_2(Rest, VarSet, AppendVarnums, Indent)
	).

:- pred hlds_out__write_instmap_delta(instmap_delta, prog_varset, bool, int,
		io__state, io__state).
:- mode hlds_out__write_instmap_delta(in, in, in, in, di, uo) is det.

hlds_out__write_instmap_delta(InstMapDelta, VarSet, AppendVarnums, Indent) -->
	( { instmap_delta_is_unreachable(InstMapDelta) } ->
		io__write_string("unreachable")
	;
		{ instmap_delta_to_assoc_list(InstMapDelta, AssocList) },
		hlds_out__write_instmap_2(AssocList, VarSet, AppendVarnums,
			Indent)
	).

hlds_out__write_import_status(local) -->
	io__write_string("local").
hlds_out__write_import_status(exported) -->
	io__write_string("exported").
hlds_out__write_import_status(opt_exported) -->
	io__write_string("opt_exported").
hlds_out__write_import_status(abstract_exported) -->
	io__write_string("abstract_exported").
hlds_out__write_import_status(pseudo_exported) -->
	io__write_string("pseudo_exported").
hlds_out__write_import_status(imported(interface)) -->
	io__write_string("imported in the interface").
hlds_out__write_import_status(imported(implementation)) -->
	io__write_string("imported in the implementation").
hlds_out__write_import_status(external(interface)) -->
	io__write_string("external (and exported)").
hlds_out__write_import_status(external(implementation)) -->
	io__write_string("external (and local)").
hlds_out__write_import_status(abstract_imported) -->
	io__write_string("abstract_imported").
hlds_out__write_import_status(opt_imported) -->
	io__write_string("opt_imported").
hlds_out__write_import_status(pseudo_imported) -->
	io__write_string("pseudo_imported").
hlds_out__write_import_status(exported_to_submodules) -->
	io__write_string("exported_to_submodules").

:- pred hlds_out__write_var_types(int, prog_varset, bool, vartypes,
		tvarset, io__state, io__state).
:- mode hlds_out__write_var_types(in, in, in, in, in, di, uo) is det.

hlds_out__write_var_types(Indent, VarSet, AppendVarnums, VarTypes, TVarSet) -->
	hlds_out__write_indent(Indent),
	io__write_string("% variable types map:\n"),
	{ map__keys(VarTypes, Vars) },
	hlds_out__write_var_types_2(Vars, Indent, VarSet, AppendVarnums,
		VarTypes, TVarSet).

:- pred hlds_out__write_var_types_2(list(prog_var), int, prog_varset, bool,
	vartypes, tvarset, io__state, io__state).
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
		prog_varset, tvarset, io__state, io__state).
:- mode hlds_out__write_typeinfo_varmap(in, in, in, in, in, di, uo) is det.

hlds_out__write_typeinfo_varmap(Indent, AppendVarnums, TypeInfoMap, VarSet,
		TVarSet) -->
	hlds_out__write_indent(Indent),
	io__write_string("% type_info varmap:\n"),
	{ map__keys(TypeInfoMap, TypeVars) },
	hlds_out__write_typeinfo_varmap_2(TypeVars, Indent, AppendVarnums,
		TypeInfoMap, VarSet, TVarSet).

:- pred hlds_out__write_typeinfo_varmap_2(list(tvar), int, bool,
		map(tvar, type_info_locn), prog_varset, tvarset,
		io__state, io__state).
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

:- pred hlds_out__write_typeclass_info_varmap(int, bool,
		map(class_constraint, prog_var), prog_varset, tvarset,
		io__state, io__state).
:- mode hlds_out__write_typeclass_info_varmap(in, in,
	in, in, in, di, uo) is det.

hlds_out__write_typeclass_info_varmap(Indent, AppendVarnums,
		TypeClassInfoVarMap, VarSet, TVarSet) -->
	hlds_out__write_indent(Indent),
	io__write_string("% typeclass_info varmap:\n"),
	map__foldl(hlds_out__write_typeclass_info_varmap_2(Indent,
		AppendVarnums, VarSet, TVarSet), TypeClassInfoVarMap).

:- pred hlds_out__write_typeclass_info_varmap_2(int, bool,
		prog_varset, tvarset, class_constraint, prog_var,
		io__state, io__state).
:- mode hlds_out__write_typeclass_info_varmap_2(in, in,
		in, in, in, in, di, uo) is det.

hlds_out__write_typeclass_info_varmap_2(Indent, AppendVarnums, VarSet, TVarSet,
		Constraint, Var) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_constraint(TVarSet, AppendVarnums, Constraint),
	io__write_string(" -> "),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__nl.

:- pred hlds_out__write_stack_slots(int, stack_slots, prog_varset, bool,
		io__state, io__state).
:- mode hlds_out__write_stack_slots(in, in, in, in, di, uo) is det.

hlds_out__write_stack_slots(Indent, StackSlots, VarSet, AppendVarnums) -->
	{ map__to_assoc_list(StackSlots, VarSlotList) },
	hlds_out__write_var_to_lvals(VarSlotList, VarSet, AppendVarnums,
		Indent).

:- pred hlds_out__write_var_to_lvals(assoc_list(prog_var, lval), prog_varset,
		bool, int, io__state, io__state).
:- mode hlds_out__write_var_to_lvals(in, in, in, in, di, uo) is det.

hlds_out__write_var_to_lvals([], _, _, _) --> [].
hlds_out__write_var_to_lvals([Var - Loc | VarLocs], VarSet, AppendVarnums,
		Indent) -->
	hlds_out__write_indent(Indent),
	io__write_string("%\t"),
	mercury_output_var(Var, VarSet, AppendVarnums),
	io__write_string("\t-> "),
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
	globals__io_lookup_string_option(dump_hlds_options, Verbose),
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
		io__write_string("\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_indent(Indent1),
		io__write_string("where equality is "),
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
		[]
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

	hlds_out__write_class_id(ClassId),
	io__write_string(":\n"),

	{ ClassDefn = hlds_class_defn(_, Constraints, Vars, _, Interface,
				VarSet, Context) },

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

	globals__io_lookup_string_option(dump_hlds_options, Verbose),
	( { string__contains_char(Verbose, 'v') } ->
		{ AppendVarnums = yes }
	;
		{ AppendVarnums = no }
	),

	hlds_out__write_indent(Indent),
	io__write_string("% Vars: "),
	mercury_output_vars(Vars, VarSet, AppendVarnums),
	io__nl,

	hlds_out__write_indent(Indent),
	io__write_string("% Constraints: "),
	io__write_list(Constraints, ", ",
		mercury_output_constraint(VarSet, AppendVarnums)),
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
	hlds_out__write_class_id(ClassId),
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
	hlds_out__write_class_id(ClassId),
	io__write_string(":\n"),
	io__write_list(InstanceDefns, "\n",
		hlds_out__write_instance_defn(Indent)).

:- pred hlds_out__write_instance_defn(int, hlds_instance_defn,
			io__state, io__state).
:- mode hlds_out__write_instance_defn(in, in, di, uo) is det.

hlds_out__write_instance_defn(Indent, InstanceDefn) -->

	{ InstanceDefn = hlds_instance_defn(_InstanceModule, _Status,
		Context, Constraints, Types, Body,
		MaybePredProcIds, VarSet, Proofs) },

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

	globals__io_lookup_string_option(dump_hlds_options, Verbose),
	( { string__contains_char(Verbose, 'v') } ->
		{ AppendVarnums = yes }
	;
		{ AppendVarnums = no }
	),

		% curry the varset for term_io__write_variable/4
	{ PrintTerm = lambda([TypeName::in, IO0::di, IO::uo] is det,
			mercury_output_term(TypeName, VarSet,
				AppendVarnums, IO0, IO)
		) },
	hlds_out__write_indent(Indent),
	io__write_string("% Types: "),
	io__write_list(Types, ", ", PrintTerm),
	io__nl,
		
	hlds_out__write_indent(Indent),
	io__write_string("% Constraints: "),
	io__write_list(Constraints, ", ", 
		mercury_output_constraint(VarSet, AppendVarnums)),
	io__nl,

	hlds_out__write_indent(Indent),
	(	{ Body = abstract },
		io__write_string("% abstract")
	;	{ Body = concrete(Methods) },
		io__write_string("% Instance Methods: "),
		mercury_output_instance_methods(Methods)
	),
	io__nl,

	( { MaybePredProcIds = yes(PredProcIds) } ->
		hlds_out__write_indent(Indent),
		io__write_string("% procedures: "),
		io__write(PredProcIds),
		io__nl
	;
		[]
	),

	hlds_out__write_constraint_proofs(Indent,
		VarSet, Proofs, AppendVarnums),
	io__nl.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_insts(int, inst_table, io__state, io__state).
:- mode hlds_out__write_insts(in, in, di, uo) is det.

hlds_out__write_insts(Indent, _InstTable) -->
		% XXX fix this up.
	hlds_out__write_indent(Indent),
	io__write_string("%-------- Insts --------\n"),
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
	pred_info, io__state, io__state).
:- mode hlds_out__write_procs(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_procs(Indent, AppendVarnums, ModuleInfo, PredId,
		ImportStatus, PredInfo) -->
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ pred_info_procids(PredInfo, ProcIds) },
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
	hlds_out__write_proc(Indent, AppendVarnums, ModuleInfo, PredId, ProcId,
		ImportStatus, ProcInfo),
	hlds_out__write_procs_2(ProcIds, AppendVarnums, ModuleInfo, Indent,
		PredId, ImportStatus, ProcTable).

:- pred hlds_out__write_proc(int, bool, module_info, pred_id, proc_id,
	import_status, proc_info, io__state, io__state).
:- mode hlds_out__write_proc(in, in, in, in, in, in, in, di, uo) is det.

hlds_out__write_proc(Indent, AppendVarnums, ModuleInfo, PredId, ProcId,
		ImportStatus, Proc) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_typevarset(PredInfo, TVarSet) },
	{ proc_info_vartypes(Proc, VarTypes) },
	{ proc_info_declared_determinism(Proc, DeclaredDeterminism) },
	{ proc_info_inferred_determinism(Proc, InferredDeterminism) },
	{ proc_info_varset(Proc, VarSet) },
	{ proc_info_headvars(Proc, HeadVars) },
	{ proc_info_argmodes(Proc, HeadModes) },
	{ proc_info_maybe_arglives(Proc, MaybeArgLives) },
	{ proc_info_goal(Proc, Goal) },
	{ proc_info_context(Proc, ModeContext) },
	{ proc_info_get_maybe_arg_size_info(Proc, MaybeArgSize) },
	{ proc_info_get_maybe_termination_info(Proc, MaybeTermination) },
	{ proc_info_typeinfo_varmap(Proc, TypeInfoMap) },
	{ proc_info_typeclass_info_varmap(Proc, TypeClassInfoMap) },
	{ proc_info_eval_method(Proc, EvalMethod) },
	{ proc_info_is_address_taken(Proc, IsAddressTaken) },
	{ proc_info_get_call_table_tip(Proc, MaybeCallTableTip) },
	{ proc_info_get_maybe_deep_profile_info(Proc, MaybeDeepProfileInfo) },
	{ Indent1 is Indent + 1 },

	hlds_out__write_indent(Indent1),
	io__write_string("% pred id "),
	{ pred_id_to_int(PredId, PredInt) },
	io__write_int(PredInt),
	io__nl,
	hlds_out__write_indent(Indent1),
	io__write_string("% mode number "),
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_int(ProcInt),
	io__write_string(" of "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" ("),
	hlds_out__write_determinism(InferredDeterminism),
	io__write_string("):\n"),

	globals__io_lookup_string_option(dump_hlds_options, Verbose),
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
	hlds_out__write_typeclass_info_varmap(Indent, AppendVarnums,
		TypeClassInfoMap, VarSet, TVarSet),

	( { IsAddressTaken = address_is_taken } ->
		io__write_string("% address is taken\n")
	;
		io__write_string("% address is not taken\n")
	),

	( { EvalMethod = eval_normal } ->
		[]
	;
		io__write_string("% eval method: "),
		hlds_out__write_eval_method(EvalMethod),
		io__write_string("\n")
	),

	( { MaybeCallTableTip = yes(CallTableTip) } ->
		io__write_string("% call table tip: "),
		mercury_output_var(CallTableTip, VarSet, AppendVarnums),
		io__write_string("\n")
	;
		[]
	),

	( { MaybeDeepProfileInfo = yes(DeepProfileInfo) } ->
		{ DeepProfileInfo = deep_profile_proc_info(Role, _SCC) },
		io__write_string("% deep profile info: "),
		(
			{ Role = inner_proc(DeepPredProcId) },
			io__write_string("inner, outer is ")
		;
			{ Role = outer_proc(DeepPredProcId) },
			io__write_string("outer, inner is ")
		),
		{ DeepPredProcId = proc(DeepPredId, DeepProcId) },
		{ pred_id_to_int(DeepPredId, DeepPredInt) },
		{ proc_id_to_int(DeepProcId, DeepProcInt) },
		io__write_int(DeepPredInt),
		io__write_string("/"),
		io__write_int(DeepProcInt),
		io__write_string("\n")
	;
		[]
	),

	hlds_out__write_indent(Indent),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ varset__init(ModeVarSet) },
	( 
		{ PredOrFunc = predicate },
		mercury_output_pred_mode_decl(ModeVarSet,
			unqualified(PredName), HeadModes,
			DeclaredDeterminism, ModeContext)
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(HeadModes, FuncHeadModes,
			RetHeadMode) },
		mercury_output_func_mode_decl(ModeVarSet,
			unqualified(PredName), FuncHeadModes, RetHeadMode,
			DeclaredDeterminism, ModeContext)
	),

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
		{ term__var_list_to_term_list(HeadVars, HeadTerms) },
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadTerms, PredOrFunc),
		io__write_string(" :-\n"),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, ".\n")
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

:- pred hlds_out__write_vartypes(int, vartypes, io__state, io__state).
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

hlds_out__write_eval_method(eval_normal) -->
	io__write_string("normal").
hlds_out__write_eval_method(eval_loop_check) -->
	io__write_string("loop_check").
hlds_out__write_eval_method(eval_memo) -->
	io__write_string("memo").
hlds_out__write_eval_method(eval_minimal) -->
	io__write_string("minimal").
hlds_out__write_eval_method(eval_table_io) -->
	io__write_string("table_io").

%-----------------------------------------------------------------------------%

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

:- pred hlds_out__write_intlist(list(int), io__state, io__state).
:- mode hlds_out__write_intlist(in, di, uo) is det.

hlds_out__write_intlist(IntList) -->
	(
		{ IntList = [] },
		io__write_string("[]")
	;
		{ IntList = [H | T] },
		io__write_string("[ "),
		hlds_out__write_intlist_2(H, T),
		io__write_string("]")
	).

:- pred hlds_out__write_intlist_2(int, list(int), io__state, io__state).
:- mode hlds_out__write_intlist_2(in, in, di, uo) is det.

hlds_out__write_intlist_2(H, T) -->
	io__write_int(H),
	(
		{ T = [TH | TT] },
		io__write_string(", "),
		hlds_out__write_intlist_2(TH, TT)
	;
		{ T = [] }
	).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_constraint_proofs(int, tvarset,
	map(class_constraint, constraint_proof), bool, io__state, io__state).
:- mode hlds_out__write_constraint_proofs(in, in, in, in, di, uo) is det.

hlds_out__write_constraint_proofs(Indent, VarSet, Proofs, AppendVarnums) -->
	hlds_out__write_indent(Indent),
	io__write_string("% Proofs: \n"),
	{ map__to_assoc_list(Proofs, ProofsList) },
	io__write_list(ProofsList, "\n",
		hlds_out__write_constraint_proof(Indent,
			VarSet, AppendVarnums)).

:- pred hlds_out__write_constraint_proof(int, tvarset, bool,
	pair(class_constraint, constraint_proof), io__state, io__state).
:- mode hlds_out__write_constraint_proof(in, in, in, in, di, uo) is det.

hlds_out__write_constraint_proof(Indent, VarSet, AppendVarnums,
		Constraint - Proof) -->
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_constraint(VarSet, AppendVarnums, Constraint),
	io__write_string(": "),
	(
		{ Proof = apply_instance(Num) },
		io__write_string("apply instance decl #"),
		io__write_int(Num)
	;
		{ Proof = superclass(Super) },
		io__write_string("super class of "),
		mercury_output_constraint(VarSet, AppendVarnums, Super)
	).

%-----------------------------------------------------------------------------%

:- func add_mode_qualifier(prog_context, pair(prog_term, mode)) = prog_term.
add_mode_qualifier(Context, HeadTerm - Mode) = AnnotatedTerm :-
	construct_qualified_term(unqualified("::"),
		[HeadTerm, mode_to_term(Context, Mode)],
		Context, AnnotatedTerm).

mode_to_term(Mode) = mode_to_term(term__context_init, Mode).

:- func mode_to_term(term__context, mode) = prog_term.
mode_to_term(Context, (InstA -> InstB)) = Term :-
	( 
		%
		% check for higher-order pred or func modes, and output them
		% in a nice format
		%
		InstA = ground(_Uniq, higher_order(_)),
		InstB = InstA
	->
		Term = inst_to_term(InstA, Context)
	;
		construct_qualified_term(unqualified(">>"),
			[inst_to_term(InstA, Context),
			 inst_to_term(InstB, Context)],
			Context, Term)
	).
mode_to_term(Context, user_defined_mode(Name, Args)) = Term :-
	construct_qualified_term(Name,
		list__map(map_inst_to_term(Context), Args),
		Context, Term).

:- func make_atom(string, prog_context) = prog_term.
make_atom(Name, Context) =
	term__functor(term__atom(Name), [], Context).

:- func map_inst_to_term(prog_context, inst) = prog_term.
map_inst_to_term(Context, Inst) = inst_to_term(Inst, Context).

inst_to_term(Inst) = inst_to_term(Inst, term__context_init).

:- func inst_to_term(inst, prog_context) = prog_term.
inst_to_term(any(Uniq), Context) =
	make_atom(any_inst_uniqueness(Uniq), Context).
inst_to_term(free, Context) =
	make_atom("free", Context).
inst_to_term(free(Type), Context) =
	term__functor(term__atom("free"), [term__coerce(Type)], Context).
inst_to_term(bound(Uniq, BoundInsts), Context) = Term :-
	construct_qualified_term(unqualified(inst_uniqueness(Uniq, "bound")),
		[bound_insts_to_term(BoundInsts, Context)], Context, Term).
inst_to_term(ground(Uniq, GroundInstInfo), Context) = Term :-
	(	
		GroundInstInfo = higher_order(pred_inst_info(PredOrFunc,
				Modes, Det)),
		/* XXX we ignore Uniq */
		(
			PredOrFunc = predicate,
			construct_qualified_term(unqualified("pred"),
				list__map(mode_to_term(Context), Modes),
				Context, ModesTerm)
		;
			PredOrFunc = function,
			pred_args_to_func_args(Modes, ArgModes, RetMode),
			construct_qualified_term(unqualified("func"),
				list__map(mode_to_term(Context), ArgModes),
				Context, ArgModesTerm),
			construct_qualified_term(unqualified("="),
				[ArgModesTerm, mode_to_term(Context, RetMode)],
				Context, ModesTerm)
		),
		construct_qualified_term(unqualified("is"), [
			ModesTerm, det_to_term(Det, Context)], Context, Term)
	;
		GroundInstInfo = constrained_inst_var(Var),
		Term = term__coerce(term__variable(Var))
	;
		GroundInstInfo = none,
		Term = make_atom(inst_uniqueness(Uniq, "ground"), Context)
	).
inst_to_term(inst_var(Var), _) =
	term__coerce(term__variable(Var)).
inst_to_term(abstract_inst(Name, Args), Context) =
	inst_name_to_term(user_inst(Name, Args), Context).
inst_to_term(defined_inst(InstName), Context) =
	inst_name_to_term(InstName, Context).
inst_to_term(not_reached, Context) =
	make_atom("not_reached", Context).

:- func inst_name_to_term(inst_name, prog_context) = prog_term.

inst_name_to_term(user_inst(Name, Args), Context) = Term :-
	construct_qualified_term(Name,
		list__map(map_inst_to_term(Context), Args),
		Context, Term).
inst_name_to_term(merge_inst(InstA, InstB), Context) = Term :-
	construct_qualified_term(unqualified("$merge_inst"),
		list__map(map_inst_to_term(Context), [InstA, InstB]),
		Context, Term).
inst_name_to_term(shared_inst(InstName), Context) = Term :-
	construct_qualified_term(unqualified("$shared_inst"),
		[inst_name_to_term(InstName, Context)],
		Context, Term).
inst_name_to_term(mostly_uniq_inst(InstName), Context) = Term :-
	construct_qualified_term(unqualified("$mostly_uniq_inst"),
		[inst_name_to_term(InstName, Context)],
		Context, Term).
inst_name_to_term(unify_inst(Liveness, InstA, InstB, Real), Context) = Term :-
	construct_qualified_term(unqualified("$unify"),
		[make_atom((Liveness = live -> "live" ; "dead"), Context)] ++
		 list__map(map_inst_to_term(Context), [InstA, InstB]) ++
		[make_atom((Real = real_unify -> "real" ; "fake"), Context)],
		Context, Term).
inst_name_to_term(ground_inst(InstName, IsLive, Uniq, Real), Context) = Term :-
	construct_qualified_term(unqualified("$ground"),
		[inst_name_to_term(InstName, Context),
		 make_atom((IsLive = live -> "live" ; "dead"), Context),
		 make_atom(inst_uniqueness(Uniq, "shared"), Context),
		 make_atom((Real = real_unify -> "real" ; "fake"), Context)],
		Context, Term).
inst_name_to_term(any_inst(InstName, IsLive, Uniq, Real), Context) = Term :-
	construct_qualified_term(unqualified("$any"),
		[inst_name_to_term(InstName, Context),
		 make_atom((IsLive = live -> "live" ; "dead"), Context),
		 make_atom(inst_uniqueness(Uniq, "shared"), Context),
		 make_atom((Real = real_unify -> "real" ; "fake"), Context)],
		Context, Term).
inst_name_to_term(typed_ground(Uniq, Type), Context) = Term :-
	construct_qualified_term(unqualified("$typed_ground"),
		[make_atom(inst_uniqueness(Uniq, "shared"), Context),
		 term__coerce(Type)],
		Context, Term).
inst_name_to_term(typed_inst(Type, InstName), Context) = Term :-
	construct_qualified_term(unqualified("$typed_inst"),
		[term__coerce(Type),
		 inst_name_to_term(InstName, Context)],
		Context, Term).

:- func any_inst_uniqueness(uniqueness) = string.
any_inst_uniqueness(shared) = "any".
any_inst_uniqueness(unique) = "unique_any".
any_inst_uniqueness(mostly_unique) = "mostly_unique_any".
any_inst_uniqueness(clobbered) = "clobbered_any".
any_inst_uniqueness(mostly_clobbered) = "mostly_clobbered_any".

:- func inst_uniqueness(uniqueness, string) = string.
inst_uniqueness(shared, SharedName) = SharedName.
inst_uniqueness(unique, _) = "unique".
inst_uniqueness(mostly_unique, _) = "mostly_unique".
inst_uniqueness(clobbered, _) = "clobbered".
inst_uniqueness(mostly_clobbered, _) = "mostly_clobbered".

:- func bound_insts_to_term(list(bound_inst), prog_context) = prog_term.
bound_insts_to_term([], _) = _ :-
	error("bound_insts_to_term([])").
bound_insts_to_term([functor(ConsId, Args) | BoundInsts], Context) = Term :-
	( cons_id_and_args_to_term(ConsId,
		list__map(map_inst_to_term(Context), Args), FirstTerm)
	->
		( BoundInsts = [] ->
			Term = FirstTerm
		;
			construct_qualified_term(unqualified(";"),
				[FirstTerm,
				 bound_insts_to_term(BoundInsts, Context)],
				Context, Term)
		)
	;
		error("bound_insts_to_term: cons_id_and_args_to_term failed")
	).

:- func det_to_term(determinism, prog_context) = prog_term.
det_to_term(Det, Context) = make_atom(det_to_string(Det), Context).

:- func det_to_string(determinism) = string.
det_to_string(erroneous) = "erroneous".
det_to_string(failure) = "failure".
det_to_string(det) = "det".
det_to_string(semidet) = "semidet".
det_to_string(cc_multidet) = "cc_multi".
det_to_string(cc_nondet) = "cc_nondet".
det_to_string(multidet) = "multi".
det_to_string(nondet) = "nondet".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
