%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2004 The University of Melbourne.
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

:- module hlds__hlds_out.

:- interface.

% Parse tree modules
:- import_module parse_tree__inst.
:- import_module parse_tree__prog_data.
% HLDS modules
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.

:- import_module io, bool, list, term.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_type_ctor(type_ctor::in, io::di, io::uo) is det.

:- pred hlds_out__write_class_id(class_id::in, io::di, io::uo) is det.

:- pred hlds_out__write_cons_id(cons_id::in, io::di, io::uo) is det.

:- pred hlds_out__cons_id_to_string(cons_id::in, string::out) is det.

:- pred hlds_out__aditi_builtin_name(aditi_builtin::in, string::out) is det.

	% hlds_out__write_pred_id/4 writes out a message such as
	%	predicate `foo:bar/3'
	% or	function `foo:myfoo/5'
	% unless the predicate is a special (unify, compare or index)
	% predicate, in which case mercury_output_term is used to print out
	% the predicate's name and argument types (since for such predicates,
	% the module, name and arity are not sufficient to identify the
	% predicate).

:- pred hlds_out__write_pred_id(module_info::in, pred_id::in, io::di, io::uo)
	is det.

:- pred hlds_out__write_pred_proc_id(module_info::in, pred_id::in, proc_id::in,
	io::di, io::uo) is det.

:- pred hlds_out__write_call_id(call_id::in, io::di, io::uo) is det.

:- pred hlds_out__write_simple_call_id(simple_call_id::in, io::di, io::uo)
	is det.

:- pred hlds_out__write_simple_call_id(pred_or_func::in,
	sym_name_and_arity::in, io::di, io::uo) is det.

:- pred hlds_out__write_simple_call_id(pred_or_func::in, sym_name::in,
	arity::in, io::di, io::uo) is det.

:- pred hlds_out__simple_call_id_to_string(simple_call_id::in, string::out)
	is det.

	% Write "argument %i of call to pred_or_func `foo/n'".
	% The pred_markers argument is used to tell if the calling
	% predicate is a type class method implementation; if so,
	% we omit the "call to" part, since the user didn't write
	% any explicit call.
:- pred hlds_out__write_call_arg_id(call_id::in, int::in, pred_markers::in,
	io::di, io::uo) is det.

	% Print "predicate" or "function" depending on the given value.
:- pred hlds_out__write_pred_or_func(pred_or_func::in, io::di, io::uo) is det.

	% Return "predicate" or "function" depending on the given value.
:- pred hlds_out__pred_or_func_to_full_str(pred_or_func::in, string::out)
	is det.

	% Return "pred" or "func" depending on the given value.
:- pred hlds_out__pred_or_func_to_str(pred_or_func::in, string::out) is det.

	% hlds_out__write_unify_context/5 writes out a message such as
	%	foo.m:123:   in argument 3 of functor `foo/5':
	%	foo.m:123:   in unification of `X' and `blah':
	% based on the unify_context and prog_context.
	%
:- pred hlds_out__write_unify_context(unify_context::in, prog_context::in,
	io::di, io::uo) is det.

	% hlds_out__write_unify_context_first/6 is the
	% same as above, except that it also takes and returns a bool
	% which specifies whether this is the start of a sentence.
	% If the first argument is `yes', then
	% it means this is the first line of an error message, so
	% the message starts with a capital letter, e.g.
	%	foo.m:123:   In argument 3 of functor `foo/5':
	%	foo.m:123:   in unification of `X' and `blah':
	% The bool returned as the fourth argument will be `no' unless nothing
	% was printed out, in which case it will be the same as the first arg.
	%
:- pred hlds_out__write_unify_context(bool::in, unify_context::in,
	prog_context::in, bool::out, io::di, io::uo) is det.

:- pred hlds_out__write_determinism(determinism::in, io::di, io::uo) is det.

:- pred hlds_out__write_can_fail(can_fail::in, io::di, io::uo) is det.

:- pred hlds_out__write_eval_method(eval_method::in, io::di, io::uo) is det.

:- pred hlds_out__write_import_status(import_status::in, io::di, io::uo)
	is det.

%-----------------------------------------------------------------------------%

	% Print out an entire hlds structure.

:- pred hlds_out__write_hlds(int::in, module_info::in, io::di, io::uo) is det.

	% hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
	%	AppendVarNums, HeadVars, PredOrFunc, Clauses, MaybeVarTypes).
:- pred hlds_out__write_clauses(int::in, module_info::in, pred_id::in,
	prog_varset::in, bool::in, list(prog_var)::in, pred_or_func::in,
	list(clause)::in, maybe_vartypes::in, io::di, io::uo) is det.

	% hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
	%	AppendVarNums, HeadTerms, PredOrFunc, Clause,
	%	UseDeclaredModes, MaybeVarTypes).
:- pred hlds_out__write_clause(int::in, module_info::in, pred_id::in,
	prog_varset::in, bool::in, list(prog_term)::in, pred_or_func::in,
	clause::in, bool::in, maybe_vartypes::in, io::di, io::uo) is det.

:- pred hlds_out__write_promise(promise_type::in, int::in, module_info::in,
	pred_id::in, prog_varset::in, bool::in, list(prog_var)::in,
	pred_or_func::in, clause::in, maybe_vartypes::in, io::di, io::uo)
	is det.

	% Print out an HLDS goal. The module_info and prog_varset give
	% the context of the goal. The boolean says whether variables should
	% have their numbers appended to them. The integer gives the level
	% of indentation to be used within the goal. The string says what
	% should end the line containing the goal; it should include a newline
	% character, but may also contain other characters before that.

:- pred hlds_out__write_goal(hlds_goal::in, module_info::in, prog_varset::in,
	bool::in, int::in, string::in, io::di, io::uo) is det.

	% hlds_out__write_goal_list is used to write both disjunctions
	% and parallel conjunctions. The module_info, prog_varset and
	% maybe_vartypes give the context of the goal. The boolean
	% says whether variables should have their numbers appended to
	% them. The integer gives the level of indentation to be used
	% within the goal. The string says what should be on the line
	% between each goal; it should include a newline character,
	% but may also contain other characters before that.

:- pred hlds_out__write_goal_list(list(hlds_goal)::in, module_info::in,
	prog_varset::in, bool::in, int::in, string::in, maybe_vartypes::in,
	io::di, io::uo) is det.

	% Print out a functor and its arguments. The prog_varset gives
	% the context. The boolean says whether variables should have their
	% numbers appended to them.

:- pred hlds_out__write_functor(const::in, list(prog_var)::in, prog_varset::in,
	bool::in, io::di, io::uo) is det.

	% Print out a cons_id and arguments. The module_info and prog_varset
	% give the context. The boolean says whether variables should have
	% their numbers appended to them.

:- pred hlds_out__write_functor_cons_id(cons_id::in, list(prog_var)::in,
	prog_varset::in, module_info::in, bool::in, io::di, io::uo) is det.

	% Print out the right-hand-side of a unification. The module_info and
	% the varsets give the context of the rhs. The boolean says whether
	% variables should have their numbers appended to them. The integer
	% gives the level of indentation to be used within the rhs.

:- pred hlds_out__write_unify_rhs(unify_rhs::in, module_info::in,
	prog_varset::in, inst_varset::in, bool::in, int::in, io::di, io::uo)
	is det.

	% Print out a list of variables and their corresponding modes
	% (e.g. for a lambda expressions). The varsets gives the context.
	% The boolean says whether variables should have their numbers
	% appended to them.

:- pred hlds_out__write_var_modes(list(prog_var)::in, list(mode)::in,
	prog_varset::in, inst_varset::in, bool::in, io::di, io::uo) is det.

:- pred hlds_out__write_instmap(instmap::in, prog_varset::in, bool::in,
	int::in, io::di, io::uo) is det.

	% Find the name of a marker.

:- pred hlds_out__marker_name(marker::in, string::out) is det.

	% Print out the name of a marker.

:- pred hlds_out__write_marker(marker::in, io::di, io::uo) is det.

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
:- import_module parse_tree__inst.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

% HLDS modules.
:- import_module check_hlds__check_typeclass.
:- import_module check_hlds__purity.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_llds.
:- import_module hlds__instmap.
:- import_module hlds__special_pred.
:- import_module transform_hlds__term_errors.
:- import_module transform_hlds__termination.

% RL back-end modules (XXX should avoid using those here).
:- import_module aditi_backend__rl.

% LLDS back-end modules (XXX should avoid using those here).
:- import_module ll_backend.
:- import_module ll_backend__code_util.
:- import_module ll_backend__llds.
:- import_module ll_backend__llds_out.

% Misc
:- import_module backend_libs__foreign.
:- import_module backend_libs__rtti.
:- import_module libs__globals.
:- import_module libs__options.

% Standard library modules
:- import_module int, string, set, assoc_list, map, multi_map.
:- import_module require, getopt, std_util, term_io, varset.

hlds_out__write_type_ctor(Name - Arity, !IO) :-
	prog_out__write_sym_name_and_arity(Name / Arity, !IO).

hlds_out__write_class_id(class_id(Name, Arity), !IO) :-
	prog_out__write_sym_name_and_arity(Name / Arity, !IO).

hlds_out__cons_id_to_string(cons(SymName, Arity), String) :-
	prog_out__sym_name_to_string(SymName, SymNameString0),
	( string__contains_char(SymNameString0, '*') ->
		% We need to protect against the * appearing next to a /
		Stuff = (pred(Char::in, Str0::in, Str::out) is det :-
			( Char = ('*') ->
				string__append(Str0, "star", Str)
			;
				string__char_to_string(Char, CharStr),
				string__append(Str0, CharStr, Str)
			)
		),
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
hlds_out__cons_id_to_string(type_ctor_info_const(_, _, _), "<type_ctor_info>").
hlds_out__cons_id_to_string(base_typeclass_info_const(_, _, _, _),
	"<base_typeclass_info>").
hlds_out__cons_id_to_string(type_info_cell_constructor(_),
	"<type_info_cell_constructor>").
hlds_out__cons_id_to_string(typeclass_info_cell_constructor,
	"<typeclass_info_cell_constructor>").
hlds_out__cons_id_to_string(tabling_pointer_const(_, _),
	"<tabling_pointer>").
hlds_out__cons_id_to_string(deep_profiling_proc_static(_),
	"<deep_profiling_proc_static>").
hlds_out__cons_id_to_string(table_io_decl(_), "<table_io_decl>").

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
hlds_out__write_cons_id(type_ctor_info_const(_, _, _)) -->
	io__write_string("<type_ctor_info>").
hlds_out__write_cons_id(base_typeclass_info_const(_, _, _, _)) -->
	io__write_string("<base_typeclass_info>").
hlds_out__write_cons_id(type_info_cell_constructor(_)) -->
	io__write_string("<type_info_cell_constructor>").
hlds_out__write_cons_id(typeclass_info_cell_constructor) -->
	io__write_string("<typeclass_info_cell_constructor>").
hlds_out__write_cons_id(tabling_pointer_const(_, _)) -->
	io__write_string("<tabling_pointer>").
hlds_out__write_cons_id(deep_profiling_proc_static(_)) -->
	io__write_string("<deep_profiling_proc_static>").
hlds_out__write_cons_id(table_io_decl(_)) -->
	io__write_string("<table_io_decl>").

	% The code of this predicate duplicates the functionality of
	% error_util__describe_one_pred_name. Changes here should be made
	% there as well.

hlds_out__write_pred_id(ModuleInfo, PredId, !IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	Module = pred_info_module(PredInfo),
	Name = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_get_maybe_special_pred(PredInfo, MaybeSpecial),
	(
		MaybeSpecial = yes(SpecialId - TypeCtor)
	->
		special_pred_description(SpecialId, Descr),
		io__write_string(Descr, !IO),
		TypeCtor = _TypeSymName - TypeArity,
		( TypeArity = 0 ->
			io__write_string(" for type ", !IO)
		;
			io__write_string(" for type constructor ", !IO)
		),
		hlds_out__write_type_name(TypeCtor, !IO)
	;
		pred_info_get_markers(PredInfo, Markers),
		check_marker(Markers, class_instance_method)
	->
		io__write_string("type class method implementation", !IO)
	;
		pred_info_get_goal_type(PredInfo, promise(PromiseType))
	->
		io__write_string("`" ++ prog_out__promise_to_string(PromiseType)
					++ "' declaration", !IO)
	;
		hlds_out__write_simple_call_id(PredOrFunc,
			qualified(Module, Name), Arity, !IO)
	).

hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId, !IO) :-
	hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
	io__write_string(" mode ", !IO),
	proc_id_to_int(ProcId, ModeNum),
	io__write_int(ModeNum, !IO).

hlds_out__write_simple_call_id(PredOrFunc - Name/Arity, !IO) :-
	hlds_out__write_simple_call_id(PredOrFunc, Name, Arity, !IO).

hlds_out__write_simple_call_id(PredOrFunc, Name/Arity, !IO) :-
	hlds_out__write_simple_call_id(PredOrFunc, Name, Arity, !IO).

hlds_out__write_simple_call_id(PredOrFunc, Name, Arity, !IO) :-
		% XXX when printed, promises are differentiated from
		%     predicates or functions by module name, so the module
		%     names `promise', `promise_exclusive', etc. should be
		%     reserved, and their dummy predicates should have more
		%     unusual module names
	(
		Name = unqualified(StrName)
	;
		Name = qualified(_, StrName)
	),
		% is it really a promise?
	( string__prefix(StrName, "promise__") ->
		Promise = promise(true)
	; string__prefix(StrName, "promise_exclusive__") ->
		Promise = promise(exclusive)
	; string__prefix(StrName, "promise_exhaustive__") ->
		Promise = promise(exhaustive)
	; string__prefix(StrName, "promise_exclusive_exhaustive__") ->
		Promise = promise(exclusive_exhaustive)
	;
		Promise = none	% no, it is really a pred or func
	),

	(
		Promise = promise(PromiseType)
	->
		io__write_string("`", !IO),
		prog_out__write_promise_type(PromiseType, !IO),
		io__write_string("' declaration", !IO)
	;
		hlds_out__write_pred_or_func(PredOrFunc, !IO),
		io__write_string(" `", !IO),
		hlds_out__simple_call_id_to_sym_name_and_arity(
			PredOrFunc - Name/Arity, SymArity),
		prog_out__write_sym_name_and_arity(SymArity, !IO),
		io__write_string("'", !IO)
	).

:- pred hlds_out__simple_call_id_to_sym_name_and_arity(simple_call_id::in,
	sym_name_and_arity::out) is det.

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

hlds_out__write_call_id(call(PredCallId), !IO) :-
	hlds_out__write_simple_call_id(PredCallId, !IO).
hlds_out__write_call_id(generic_call(GenericCallId), !IO) :-
	hlds_out__write_generic_call_id(GenericCallId, !IO).

:- pred hlds_out__write_generic_call_id(generic_call_id::in,
	io::di, io::uo) is det.

hlds_out__write_generic_call_id(higher_order(Purity, PredOrFunc, _), !IO) :-
	write_purity_prefix(Purity, !IO),
	io__write_string("higher-order ", !IO),
	hlds_out__write_pred_or_func(PredOrFunc, !IO),
	io__write_string(" call", !IO).

hlds_out__write_generic_call_id(class_method(_ClassId, MethodId), !IO) :-
	hlds_out__write_simple_call_id(MethodId, !IO).

hlds_out__write_generic_call_id(unsafe_cast, !IO) :-
	io__write_string("unsafe_cast", !IO).

hlds_out__write_generic_call_id(aditi_builtin(AditiBuiltin, CallId), !IO) :-
	hlds_out__aditi_builtin_name(AditiBuiltin, Name),
	io__write_strings(["`", Name, "' of "], !IO),
	hlds_out__write_simple_call_id(CallId, !IO).

hlds_out__write_call_arg_id(CallId, ArgNum, PredMarkers, !IO) :-
	( ArgNum =< 0 ->
		% Argument numbers that are less than or equal to zero
		% are used for the type_info and typeclass_info arguments
		% that are introduced by polymorphism.m.
		% I think argument number equal to zero might also be used
		% in some other cases when we just don't have any information
		% about which argument it is.
		% For both of these, we just say "in call to"
		% rather than "in argument N of call to".
		true
	;
		hlds_out__write_arg_number(CallId, ArgNum, !IO),
		io__write_string(" of ", !IO)
	),
	(
		(
			% The text printed for generic calls other than
			% `class__method' does not need the "call to"
			% prefix ("in call to higher-order call" is redundant,
			% it's much better to just say "in higher-order call").
			CallId = generic_call(GenericCall),
			\+ GenericCall = class_method(_, _)
		;
			% For calls from type class instance implementations
			% that were defined using the named syntax rather
			% than the clause syntax, we also omit the "call to",
			% since in that case there was no explicit call in
			% the user's source code.
			check_marker(PredMarkers, named_class_instance_method)
		)
	->
		true
	;
		io__write_string("call to ", !IO)
	),
	hlds_out__write_call_id(CallId, !IO).

:- pred hlds_out__write_arg_number(call_id::in, int::in, io::di, io::uo)
	is det.

hlds_out__write_arg_number(call(PredOrFunc - _/Arity), ArgNum, !IO) :-
	(
		PredOrFunc = function,
		Arity = ArgNum
	->
		io__write_string("the return value", !IO)
	;
		io__write_string("argument ", !IO),
		io__write_int(ArgNum, !IO)
	).
hlds_out__write_arg_number(generic_call(
		higher_order(_Purity, PredOrFunc, Arity)), ArgNum, !IO) :-
	(
		PredOrFunc = function,
		ArgNum = Arity
	->
		io__write_string("the return value", !IO)
	;
		io__write_string("argument ", !IO),
		io__write_int(ArgNum, !IO),

		% Make error messages for higher-order calls
		% such as `P(A, B)' clearer.
		io__write_string(" (i.e. ", !IO),
		( ArgNum = 1 ->
			io__write_string("the ", !IO),
			hlds_out__write_pred_or_func(PredOrFunc, !IO),
			io__write_string(" term", !IO)
		;
			io__write_string("argument ", !IO),
			io__write_int(ArgNum - 1, !IO),
			io__write_string(" of the called ", !IO),
			hlds_out__write_pred_or_func(PredOrFunc, !IO)
		)
	),
	io__write_string(")", !IO).

hlds_out__write_arg_number(generic_call(class_method(_, _)), ArgNum, !IO) :-
	io__write_string("argument ", !IO),
	io__write_int(ArgNum, !IO).

hlds_out__write_arg_number(generic_call(unsafe_cast), ArgNum, !IO) :-
	io__write_string("argument ", !IO),
	io__write_int(ArgNum, !IO).

hlds_out__write_arg_number(generic_call(aditi_builtin(Builtin, CallId)),
		ArgNum, !IO) :-
	hlds_out__write_aditi_builtin_arg_number(Builtin, CallId, ArgNum, !IO).

:- pred hlds_out__write_aditi_builtin_arg_number(aditi_builtin::in,
	simple_call_id::in, int::in, io::di, io::uo) is det.

hlds_out__write_aditi_builtin_arg_number(aditi_tuple_update(InsertDelete, _),
		_ - _/Arity, ArgNum, !IO) :-
	io__write_string("argument ", !IO),
	( ArgNum =< Arity ->
		io__write_int(ArgNum, !IO),
		io__write_string(" of the ", !IO),
		( InsertDelete = insert, Str = "inserted"
		; InsertDelete = delete, Str = "deleted"
		),
		io__write_string(Str, !IO),
		io__write_string(" tuple", !IO)
	;
		io__write_int(ArgNum - Arity + 1, !IO)
	).

hlds_out__write_aditi_builtin_arg_number(aditi_bulk_update(_, _, pred_term),
		_, ArgNum, !IO) :-
	io__write_string("argument ", !IO),
	io__write_int(ArgNum, !IO).

hlds_out__write_aditi_builtin_arg_number(
		aditi_bulk_update(_, _, sym_name_and_closure),
		_, ArgNum, !IO) :-
	% The original goal had a sym_name/arity
	% at the front of the argument list.
	io__write_string("argument ", !IO),
	io__write_int(ArgNum + 1, !IO).

hlds_out__write_pred_or_func(predicate, !IO) :-
	io__write_string("predicate", !IO).

hlds_out__write_pred_or_func(function, !IO) :-
	io__write_string("function", !IO).

hlds_out__pred_or_func_to_full_str(predicate, "predicate").
hlds_out__pred_or_func_to_full_str(function, "function").

hlds_out__pred_or_func_to_str(predicate, "pred").
hlds_out__pred_or_func_to_str(function, "func").

%-----------------------------------------------------------------------------%

hlds_out__write_unify_context(UnifyContext, Context, !IO) :-
	hlds_out__write_unify_context(no, UnifyContext, Context, _, !IO).

hlds_out__write_unify_context(First0,
		unify_context(MainContext, RevSubContexts), Context, First,
		!IO) :-
	hlds_out__write_unify_main_context(First0, MainContext, Context,
		First1, !IO),
	list__reverse(RevSubContexts, SubContexts),
	hlds_out__write_unify_sub_contexts(First1, SubContexts, Context,
		First, !IO).

:- pred hlds_out__write_unify_main_context(bool::in, unify_main_context::in,
	prog_context::in, bool::out, io::di, io::uo) is det.

hlds_out__write_unify_main_context(First, explicit, _, First, !IO).
hlds_out__write_unify_main_context(First, head(ArgNum), Context, no, !IO) :-
	hlds_out__write_in_argument(First, ArgNum, Context, !IO),
	io__write_string(" of clause head:\n", !IO).

hlds_out__write_unify_main_context(First, head_result, Context, no, !IO) :-
	hlds_out__start_in_message(First, Context, !IO),
	io__write_string("function result term of clause head:\n", !IO).

hlds_out__write_unify_main_context(First, call(CallId, ArgNum), Context, no,
		!IO) :-
	hlds_out__start_in_message(First, Context, !IO),
	% The markers argument below is used only for type class method
	% implementations defined using the named syntax rather than
	% the clause syntax, and the bodies of such procedures should
	% only contain a single call, so we shouldn't get unifications
	% nested inside calls.  Hence we can safely initialize the
	% markers to empty here.  (Anyway the worst possible consequence
	% is slightly sub-optimal text for an error message.)
	init_markers(Markers),
	hlds_out__write_call_arg_id(CallId, ArgNum, Markers, !IO),
	io__write_string(":\n", !IO).

hlds_out__write_unify_main_context(First, implicit(Source), Context, First,
		!IO) :-
	hlds_out__start_in_message(First, Context, !IO),
	io__format("implicit %s unification:\n", [s(Source)], !IO).

:- pred hlds_out__write_unify_sub_contexts(bool::in, unify_sub_contexts::in,
	prog_context::in, bool::out, io::di, io::uo) is det.

hlds_out__write_unify_sub_contexts(First, [], _, First, !IO).
hlds_out__write_unify_sub_contexts(First0, [ConsId - ArgNum | SubContexts],
		Context, First, !IO) :-
	hlds_out__write_in_argument(First0, ArgNum, Context, !IO),
	io__write_string(" of functor `", !IO),
	hlds_out__write_cons_id(ConsId, !IO),
	io__write_string("':\n", !IO),
	hlds_out__write_unify_sub_contexts(no, SubContexts, Context, First,
		!IO).

:- pred hlds_out__write_in_argument(bool::in, int::in, prog_context::in,
	io::di, io::uo) is det.

hlds_out__write_in_argument(First, ArgNum, Context, !IO) :-
	hlds_out__start_in_message(First, Context, !IO),
	io__write_string("argument ", !IO),
	io__write_int(ArgNum, !IO).

:- pred hlds_out__start_in_message(bool::in, prog_context::in,
	io::di, io::uo) is det.

hlds_out__start_in_message(First, Context, !IO) :-
	prog_out__write_context(Context, !IO),
	( First = yes ->
		io__write_string("  In ", !IO)
	;
		io__write_string("  in ", !IO)
	).

%-----------------------------------------------------------------------------%

hlds_out__write_hlds(Indent, Module, !IO) :-
	module_info_get_imported_module_specifiers(Module, Imports),
	module_info_preds(Module, PredTable),
	module_info_types(Module, TypeTable),
	module_info_insts(Module, InstTable),
	module_info_modes(Module, ModeTable),
	module_info_classes(Module, ClassTable),
	module_info_superclasses(Module, SuperClassTable),
	module_info_instances(Module, InstanceTable),
	hlds_out__write_header(Indent, Module, !IO),
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'I') ->
		hlds_out__write_imports(Indent, Imports, !IO)
	;
		true
	),
	( string__contains_char(Verbose, 'T') ->
		hlds_out__write_types(Indent, TypeTable, !IO),
		io__write_string("\n", !IO),
		hlds_out__write_classes(Indent, ClassTable, !IO),
		io__write_string("\n", !IO),
		hlds_out__write_superclasses(Indent, SuperClassTable, !IO),
		io__write_string("\n", !IO),
		hlds_out__write_instances(Indent, InstanceTable, !IO),
		io__write_string("\n", !IO)
	;
		true
	),
	( string__contains_char(Verbose, 'M') ->
		hlds_out__write_insts(Indent, InstTable, !IO),
		io__write_string("\n", !IO),
		hlds_out__write_modes(Indent, ModeTable, !IO),
		io__write_string("\n", !IO)
	;
		true
	),
	hlds_out__write_preds(Indent, Module, PredTable, !IO),
	hlds_out__write_footer(Indent, Module, !IO).

:- pred hlds_out__write_header(int::in, module_info::in, io::di, io::uo)
	is det.

hlds_out__write_header(Indent, Module, !IO) :-
	module_info_name(Module, Name),
	hlds_out__write_indent(Indent, !IO),
	io__write_string(":- module ", !IO),
	prog_out__write_sym_name(Name, !IO),
	io__write_string(".\n\n", !IO).

:- pred hlds_out__write_imports(int::in, set(module_specifier)::in,
	io::di, io::uo) is det.

hlds_out__write_imports(Indent, ImportSet, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string(":- import_module ", !IO),
	io__write_list(set__to_sorted_list(ImportSet), ", ",
		prog_out__write_sym_name, !IO),
	io__write_string(".\n\n", !IO).

:- pred hlds_out__write_footer(int::in, module_info::in, io::di, io::uo)
	is det.

hlds_out__write_footer(Indent, Module, !IO) :-
	module_info_name(Module, Name),
	hlds_out__write_indent(Indent, !IO),
	io__write_string(":- end_module ", !IO),
	prog_out__write_sym_name(Name, !IO),
	io__write_string(".\n", !IO).

:- pred hlds_out__write_preds(int::in, module_info::in, pred_table::in,
	io::di, io::uo) is det.

hlds_out__write_preds(Indent, ModuleInfo, PredTable, !IO) :-
	io__write_string("%-------- Predicates --------\n\n", !IO),
	hlds_out__write_indent(Indent, !IO),
	map__keys(PredTable, PredIds),
	list__foldl(hlds_out__maybe_write_pred(Indent, ModuleInfo, PredTable),
		PredIds, !IO).

:- pred hlds_out__maybe_write_pred(int::in, module_info::in, pred_table::in,
	pred_id::in, io__state::di, io__state::uo) is det.

hlds_out__maybe_write_pred(Indent, ModuleInfo, PredTable, PredId, !IO) :-
        globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	globals__io_lookup_int_option(dump_hlds_pred_id, DumpPredId, !IO),
	pred_id_to_int(PredId, PredIdInt),
	map__lookup(PredTable, PredId, PredInfo),
	(
		% If the user requested one predicate/function to be dumped,
		% we dump it even if the condition of the nested if-then-else
		% says it shouldn't be dumped, and we don't dump anything else.
		DumpPredId >= 0
	->
		( PredIdInt = DumpPredId ->
			hlds_out__write_pred(Indent, ModuleInfo, PredId,
				PredInfo, !IO)
		;
			true
		)
	;
		(
			\+ string__contains_char(Verbose, 'I'),
			pred_info_is_imported(PredInfo)
		;
			% For pseudo-imported predicates (i.e. unification
			% preds), only print them if we are using a local
			% mode for them.
			\+ string__contains_char(Verbose, 'I'),
			pred_info_is_pseudo_imported(PredInfo),
			ProcIds = pred_info_procids(PredInfo),
			hlds_pred__in_in_unification_proc_id(ProcId),
			ProcIds = [ProcId]
		;
			% We dump unification and other compiler-generated
			% special predicates if suboption 'U' is on. We don't
			% need that information to understand how the program
			% has been transformed.
			\+ string__contains_char(Verbose, 'U'),
			is_unify_or_compare_pred(PredInfo)
		)
	->
		true
	;
		hlds_out__write_pred(Indent, ModuleInfo, PredId, PredInfo, !IO)
	).

:- pred hlds_out__write_pred(int::in, module_info::in, pred_id::in,
	pred_info::in, io::di, io::uo) is det.

hlds_out__write_pred(Indent, ModuleInfo, PredId, PredInfo, !IO) :-
	Module = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_arg_types(PredInfo, ArgTypes),
	pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
	pred_info_typevarset(PredInfo, TVarSet),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	pred_info_context(PredInfo, Context),
	pred_info_import_status(PredInfo, ImportStatus),
	pred_info_get_markers(PredInfo, Markers),
	pred_info_get_class_context(PredInfo, ClassContext),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	pred_info_get_purity(PredInfo, Purity),
	pred_info_get_head_type_params(PredInfo, HeadTypeParams),
	pred_info_get_indexes(PredInfo, Indexes),
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'v') ->
		AppendVarnums = yes
	;
		AppendVarnums = no
	),
	( string__contains_char(Verbose, 'C') ->
		% Information about predicates is dumped if 'C'
		% suboption is on.
		(
			PredOrFunc = predicate,
			mercury_output_pred_type(TVarSet, ExistQVars,
				qualified(Module, PredName),
				ArgTypes, no, Purity, ClassContext, Context,
				AppendVarnums, !IO)
		;
			PredOrFunc = function,
			pred_args_to_func_args(ArgTypes, FuncArgTypes,
				FuncRetType),
			mercury_output_func_type(TVarSet, ExistQVars,
				qualified(Module, PredName), FuncArgTypes,
				FuncRetType, no, Purity, ClassContext,
				Context, AppendVarnums, !IO)
		)
	;
		true
	),
	ClausesInfo = clauses_info(VarSet, _, _, VarTypes, HeadVars, Clauses,
		TypeInfoMap, TypeClassInfoMap, _),
	( string__contains_char(Verbose, 'C') ->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% pred id: ", !IO),
		pred_id_to_int(PredId, PredInt),
		io__write_int(PredInt, !IO),
		io__write_string(", category: ", !IO),
		hlds_out__write_pred_or_func(PredOrFunc, !IO),
		io__write_string(", status: ", !IO),
		hlds_out__write_import_status(ImportStatus, !IO),
		io__write_string("\n", !IO),
		io__write_string("% goal_type: ", !IO),
		pred_info_get_goal_type(PredInfo, GoalType),
		io__write(GoalType, !IO),
		io__write_string("\n", !IO),
		markers_to_marker_list(Markers, MarkerList),
		( MarkerList = [] ->
			true
		;
			io__write_string("% markers: ", !IO),
			hlds_out__write_marker_list(MarkerList, !IO),
			io__write_string("\n", !IO)
		),
		hlds_out__write_typeinfo_varmap(Indent, AppendVarnums,
			TypeInfoMap, VarSet, TVarSet, !IO),
		hlds_out__write_typeclass_info_varmap(Indent, AppendVarnums,
			TypeClassInfoMap, VarSet, TVarSet, !IO),
		( map__is_empty(Proofs) ->
			true
		;
			hlds_out__write_constraint_proofs(Indent, TVarSet,
				Proofs, AppendVarnums, !IO),
			io__write_string("\n", !IO)
		),

		% XXX The indexes are not part of the clauses_info,
		% so why is this code inside this if-then-else
		% with the condition `string__contains_char(Verbose, 'C')'?
		% Shouldn't it be dependent on a different letter?

		( Indexes = [] ->
			true
		;
			io__write_string("% indexes: ", !IO),
			io__write_list(Indexes, ", ",
				mercury_output_index_spec, !IO),
			io__nl(!IO)
		),

		( HeadTypeParams \= [] ->
			io__write_string("% head_type_params:\n", !IO),
			io__write_string("% ", !IO),
			mercury_output_vars(HeadTypeParams, TVarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		;
			true
		),
		hlds_out__write_var_types(Indent, VarSet, AppendVarnums,
			VarTypes, TVarSet, !IO),

		( Clauses \= [] ->
			% XXX FIXME Never write the clauses out verbosely -
			% disable the dump_hlds_options option before writing
			% them, and restore its initial value afterwards
			% globals__io_set_option(dump_hlds_options, string("")),
			hlds_out__write_clauses(Indent, ModuleInfo, PredId,
				VarSet, AppendVarnums, HeadVars, PredOrFunc,
				Clauses, no, !IO)
			% globals__io_set_option(dump_hlds_options,
			%	string(Verbose), !IO)
		;
			true
		),

		pred_info_get_maybe_instance_method_constraints(PredInfo,
			MaybeCs),
		( MaybeCs = yes(MethodConstraints) ->
			MethodConstraints = instance_method_constraints(
				ClassId, InstanceTypes, InstanceConstraints,
				ClassMethodConstraints),
			io__write_string("% instance method constraints:\n",
				!IO),
			ClassId = class_id(ClassName, _),
			mercury_output_constraint(TVarSet, AppendVarnums,
				constraint(ClassName, InstanceTypes), !IO),
			io__nl(!IO),
			io__write_string("instance constraints: ", !IO),
			io__write_list(InstanceConstraints, ", ",
				mercury_output_constraint(TVarSet,
					AppendVarnums), !IO),
			io__nl(!IO),

			ClassMethodConstraints = constraints(
				MethodUnivConstraints,
				MethodExistConstraints),
			io__write_string("method univ constraints: ", !IO),
			io__write_list(MethodUnivConstraints, ", ",
				mercury_output_constraint(TVarSet,
					AppendVarnums), !IO),
			io__nl(!IO),
			io__write_string("method exist constraints: ", !IO),
			io__write_list(MethodExistConstraints, ", ",
				mercury_output_constraint(TVarSet,
					AppendVarnums), !IO),
			io__nl(!IO)

		;
			true
		)
	;
		true
	),
	hlds_out__write_procs(Indent, AppendVarnums, ModuleInfo, PredId,
		ImportStatus, PredInfo, !IO),
	io__write_string("\n", !IO).

:- pred hlds_out__write_marker_list(list(marker)::in, io::di, io::uo) is det.

hlds_out__write_marker_list(Markers, !IO) :-
	io__write_list(Markers, ", ", hlds_out__write_marker, !IO).

hlds_out__marker_name(stub, "stub").
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
hlds_out__marker_name(base_relation, "base_relation").
hlds_out__marker_name(generate_inline, "generate_inline").
hlds_out__marker_name(aditi_memo, "aditi_memo").
hlds_out__marker_name(aditi_no_memo, "aditi_no_memo").
hlds_out__marker_name(naive, "naive").
hlds_out__marker_name(psn, "psn").
hlds_out__marker_name(supp_magic, "supp_magic").
hlds_out__marker_name(context, "context").
hlds_out__marker_name(calls_are_fully_qualified, "calls_are_fully_qualified").

hlds_out__write_marker(Marker, !IO) :-
	hlds_out__marker_name(Marker, Name),
	io__write_string(Name, !IO).

hlds_out__write_promise(PromiseType, Indent, ModuleInfo, _PredId, VarSet,
		AppendVarnums, HeadVars, _PredOrFunc, Clause, TypeQual, !IO) :-

		% curry the varset for term_io__write_variable/4
	PrintVar = (pred(VarName::in, IOState0::di, IOState::uo) is det :-
		term_io__write_variable(VarName, VarSet, IOState0, IOState)
	),

	hlds_out__write_indent(Indent, !IO),

		% print initial formatting differently for assertions
	( PromiseType = true ->
		io__write_string(":- promise all [", !IO),
		io__write_list(HeadVars, ", ", PrintVar, !IO),
		io__write_string("] (\n", !IO)
	;
		io__write_string(":- all [", !IO),
		io__write_list(HeadVars, ", ", PrintVar, !IO),
		io__write_string("]", !IO),
		mercury_output_newline(Indent, !IO),
		prog_out__write_promise_type(PromiseType, !IO),
		mercury_output_newline(Indent, !IO),
		io__write_string("(\n", !IO)
	),

	Clause = clause(_Modes, Goal, _Lang, _Context),
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent+1, ").\n", TypeQual, !IO).

hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet, AppendVarnums,
		HeadVars, PredOrFunc, Clauses0, TypeQual, !IO) :-
	(
		Clauses0 = [Clause | Clauses]
	->
		term__var_list_to_term_list(HeadVars, HeadTerms),
		UseDeclaredModes = no,
		hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadTerms, PredOrFunc,
			Clause, UseDeclaredModes, TypeQual, !IO),
		hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadVars, PredOrFunc, Clauses, TypeQual,
			!IO)
	;
		true
	).

hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
		AppendVarnums, HeadTerms, PredOrFunc, Clause,
		UseDeclaredModes, TypeQual, !IO) :-
	Clause = clause(Modes, Goal, Lang, Context),
	Indent1 = Indent + 1,
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'm') ->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% Modes for which this clause applies: ",
			!IO),
		list__map((pred(Mode :: in, ModeInt :: out) is det :-
				proc_id_to_int(Mode, ModeInt)
			), Modes, ModeInts),
		hlds_out__write_intlist(ModeInts, !IO),
		io__write_string("\n", !IO)
	;
		true
	),
	(
		Lang = mercury
	;
		Lang = foreign_language(ForeignLang),
		io__write_string("% Language of implementation: ", !IO),
		io__write(ForeignLang, !IO),
		io__nl(!IO)
	),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	ProcIds = pred_info_procids(PredInfo),
	(
		( Modes = []
		; Modes = ProcIds
		)
	->
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadTerms, PredOrFunc, !IO)
	;
		% If Modes contains more than one mode, the output will have
		% multiple clause heads. This won't be pretty and it won't be
		% syntactically valid, but it is more useful for debugging
		% than a compiler abort during the dumping process.
		hlds_out__write_annotated_clause_heads(ModuleInfo, Context,
			PredId, Modes, VarSet, AppendVarnums, HeadTerms,
			PredOrFunc, UseDeclaredModes, !IO)
	),
	( Goal = conj([]) - _GoalInfo ->
		io__write_string(".\n", !IO)
	;
		io__write_string(" :-\n", !IO),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, ".\n", TypeQual, !IO)
	).

:- pred hlds_out__write_annotated_clause_heads(module_info::in,
	term__context::in, pred_id::in, list(proc_id)::in, prog_varset::in,
	bool::in, list(prog_term)::in, pred_or_func::in, bool::in,
	io__state::di, io__state::uo) is det.

hlds_out__write_annotated_clause_heads(_, _, _, [], _, _, _, _, _, !IO).
hlds_out__write_annotated_clause_heads(ModuleInfo, Context, PredId,
		[ProcId | ProcIds], VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes, !IO) :-
	hlds_out__write_annotated_clause_head(ModuleInfo, Context, PredId,
		ProcId, VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes, !IO),
	hlds_out__write_annotated_clause_heads(ModuleInfo, Context, PredId,
		ProcIds, VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes, !IO).

:- pred hlds_out__write_annotated_clause_head(module_info::in,
	term__context::in, pred_id::in, proc_id::in, prog_varset::in,
	bool::in, list(prog_term)::in, pred_or_func::in, bool::in,
	io__state::di, io__state::uo) is det.

hlds_out__write_annotated_clause_head(ModuleInfo, Context, PredId, ProcId,
		VarSet, AppendVarnums, HeadTerms,
		PredOrFunc, UseDeclaredModes, !IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procedures),
	( map__search(Procedures, ProcId, ProcInfo) ->
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
		( UseDeclaredModes = yes ->
			proc_info_declared_argmodes(ProcInfo, ArgModes)
		;
			proc_info_argmodes(ProcInfo, ArgModes)
		),
		assoc_list__from_corresponding_lists(HeadTerms, ArgModes,
			AnnotatedPairs),
		AnnotatedHeadTerms = list__map(add_mode_qualifier(Context),
			AnnotatedPairs),
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			AppendVarnums, AnnotatedHeadTerms, PredOrFunc, !IO)
	;
		% This procedure, even though it existed in the past, has been
		% eliminated.
		true
	).

:- pred hlds_out__write_clause_head(module_info::in, pred_id::in,
	prog_varset::in, bool::in, list(prog_term)::in, pred_or_func::in,
	io::di, io::uo) is det.

hlds_out__write_clause_head(ModuleInfo, PredId, VarSet, AppendVarnums,
			HeadTerms, PredOrFunc, !IO) :-
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_module(ModuleInfo, PredId, ModuleName),
	(
		PredOrFunc = function,
		pred_args_to_func_args(HeadTerms, FuncArgs, RetVal),
		hlds_out__write_qualified_functor_with_term_args(
			ModuleName, term__atom(PredName), FuncArgs, VarSet,
			AppendVarnums, !IO),
		io__write_string(" = ", !IO),
		mercury_output_term(RetVal, VarSet, AppendVarnums,
			next_to_graphic_token, !IO)
	;
		PredOrFunc = predicate,
		hlds_out__write_qualified_functor_with_term_args(
			ModuleName, term__atom(PredName), HeadTerms, VarSet,
			AppendVarnums, !IO)
	).

hlds_out__write_goal(Goal, ModuleInfo, VarSet, AppendVarnums, Indent, Follow,
		!IO) :-
		% don't type qualify everything
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, no, !IO).

	% TypeQual is yes(TVarset, VarTypes) if all constructors should
	% be module qualified.
:- pred hlds_out__write_goal_a(hlds_goal::in, module_info::in, prog_varset::in,
	bool::in, int::in, string::in, maybe_vartypes::in, io::di, io::uo)
	is det.

hlds_out__write_goal_a(Goal - GoalInfo, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual, !IO) :-
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'c') ->
		goal_info_get_context(GoalInfo, Context),
		term__context_file(Context, FileName),
		term__context_line(Context, LineNumber),
		( FileName \= "" ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% context: file `", !IO),
			io__write_string(FileName, !IO),
			io__write_string("', line ", !IO),
			io__write_int(LineNumber, !IO),
			io__write_string("\n", !IO)
		;
			true
		)
	;
		true
	),
	( string__contains_char(Verbose, 'P') ->
		goal_info_get_goal_path(GoalInfo, Path),
		( Path \= [] ->
			goal_path_to_string(Path, PathStr),
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% goal path: ", !IO),
			io__write_string(PathStr, !IO),
			io__write_string("\n", !IO)
		;
			true
		)
	;
		true
	),
	( string__contains_char(Verbose, 'n') ->
		goal_info_get_nonlocals(GoalInfo, NonLocalsSet),
		set__to_sorted_list(NonLocalsSet, NonLocalsList),
		( NonLocalsList \= [] ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% nonlocals: ", !IO),
			mercury_output_vars(NonLocalsList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		;
			true
		)
	;
		true
	),
	( string__contains_char(Verbose, 'p') ->
		(
			goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths),
			set__to_sorted_list(PreDeaths, PreDeathList),
			PreDeathList \= []
		->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% pre-deaths: ", !IO),
			mercury_output_vars(PreDeathList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		;
			true
		),
		(
			goal_info_maybe_get_pre_births(GoalInfo, PreBirths),
			set__to_sorted_list(PreBirths, PreBirthList),
			PreBirthList \= []
		->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% pre-births: ", !IO),
			mercury_output_vars(PreBirthList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		;
			true
		)
	;
		true
	),
	( string__contains_char(Verbose, 'd') ->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% determinism: ", !IO),
		goal_info_get_determinism(GoalInfo, Determinism),
		hlds_out__write_determinism(Determinism, !IO),
		io__write_string("\n", !IO)
	;
		true
	),
	hlds_out__write_goal_2(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual, !IO),
	( string__contains_char(Verbose, 'i') ->
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		(
			instmap_delta_is_reachable(InstMapDelta),
			instmap_delta_changed_vars(InstMapDelta, Vars),
			set__empty(Vars)
		->
			true
		;
			hlds_out__write_indent(Indent, !IO),
			( string__contains_char(Verbose, 'D') ->
				io__write_string("% new insts: ", !IO),
				hlds_out__write_instmap_delta(InstMapDelta,
					VarSet, AppendVarnums, Indent, !IO),
				io__write_string("\n", !IO)
			;
				io__write_string("% vars with new insts: ",
					!IO),
				hlds_out__write_instmap_delta_vars(
					InstMapDelta, VarSet, AppendVarnums,
					!IO),
				io__write_string("\n", !IO)
			)
		)
	;
		true
	),
	( string__contains_char(Verbose, 'p') ->
		(
			goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths),
			set__to_sorted_list(PostDeaths, PostDeathList),
			PostDeathList \= []
		->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% post-deaths: ", !IO),
			mercury_output_vars(PostDeathList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		;
			true
		),
		(
			goal_info_maybe_get_post_births(GoalInfo, PostBirths),
			set__to_sorted_list(PostBirths, PostBirthList),
			PostBirthList \= []
		->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% post-births: ", !IO),
			mercury_output_vars(PostBirthList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		;
			true
		)
	;
		true
	),
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	(
		CodeGenInfo = no_code_gen_info
	;
		CodeGenInfo = llds_code_gen_info(_CodeGenDetails),
		hlds_out__write_llds_code_gen_info(GoalInfo, VarSet,
			AppendVarnums, Indent, Verbose, !IO)
	),
	( string__contains_char(Verbose, 'g') ->
		goal_info_get_features(GoalInfo, Features),
		set__to_sorted_list(Features, Flist),
		( Flist = [] ->
			true
		;
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% Goal features:  ", !IO),
			io__write(Flist, !IO),
			io__write_string("\n", !IO)
		)
	;
		true
	).

:- pred hlds_out__write_goal_2(hlds_goal_expr::in, module_info::in,
	prog_varset::in, bool::in, int::in, string::in, maybe_vartypes::in,
	io::di, io::uo) is det.

hlds_out__write_goal_2(switch(Var, CanFail, CasesList), ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("( % ", !IO),
	hlds_out__write_can_fail(CanFail, !IO),
	io__write_string(" switch on `", !IO),
	mercury_output_var(Var, VarSet, AppendVarnums, !IO),
	io__write_string("'\n", !IO),
	Indent1 = Indent + 1,
	( CasesList = [Case | Cases] ->
		hlds_out__write_case(Case, Var, ModuleInfo, VarSet,
			AppendVarnums, Indent1, TypeQual, !IO),
		hlds_out__write_cases(Cases, Var, ModuleInfo, VarSet,
			AppendVarnums, Indent, TypeQual, !IO)
	;
		hlds_out__write_indent(Indent1, !IO),
		io__write_string("fail\n", !IO)
	),
	hlds_out__write_indent(Indent, !IO),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO).

hlds_out__write_goal_2(some(Vars, CanRemove, Goal), ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("some [", !IO),
	mercury_output_vars(Vars, VarSet, AppendVarnums, !IO),
	io__write_string("] (", !IO),
	( CanRemove = cannot_remove ->
		io__write_string(" % (cannot remove)", !IO)
	;
		true
	),
	io__nl(!IO),
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent + 1, "\n", TypeQual, !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO).

hlds_out__write_goal_2(if_then_else(Vars, Cond, Then, Else), ModuleInfo,
		VarSet, AppendVarnums, Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("(if", !IO),
	hlds_out__write_some(Vars, VarSet, !IO),
	io__write_string("\n", !IO),
	Indent1 = Indent + 1,
	hlds_out__write_goal_a(Cond, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual, !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("then\n", !IO),
	hlds_out__write_goal_a(Then, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual, !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("else\n", !IO),
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	(
		Verbose \= "",
		Else = if_then_else(_, _, _, _) - _
	->
		hlds_out__write_goal_a(Else, ModuleInfo, VarSet, AppendVarnums,
			Indent, "\n", TypeQual, !IO)
	;
		hlds_out__write_goal_a(Else, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual, !IO)
	),
	hlds_out__write_indent(Indent, !IO),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO).

hlds_out__write_goal_2(not(Goal), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("\\+ (\n", !IO),
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent + 1, "\n", TypeQual, !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO).

hlds_out__write_goal_2(conj(List), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual, !IO) :-
	( List = [Goal | Goals] ->
		globals__io_lookup_string_option(dump_hlds_options, Verbose,
			!IO),
		( Verbose \= "" ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("( % conjunction\n", !IO),
			hlds_out__write_conj(Goal, Goals, ModuleInfo, VarSet,
				AppendVarnums, Indent + 1, "\n", Verbose,
				",\n", TypeQual, !IO),
			hlds_out__write_indent(Indent, !IO),
			io__write_string(")", !IO),
			io__write_string(Follow, !IO)
		;
			hlds_out__write_conj(Goal, Goals, ModuleInfo, VarSet,
				AppendVarnums, Indent, Follow, Verbose, ",\n",
				TypeQual, !IO)
		)
	;
		hlds_out__write_indent(Indent, !IO),
		io__write_string("true", !IO),
		io__write_string(Follow, !IO)
	).

hlds_out__write_goal_2(par_conj(List), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	( List = [Goal | Goals] ->
		io__write_string("( % parallel conjunction\n", !IO),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent + 1, "\n", TypeQual, !IO),
			% See comments at hlds_out__write_goal_list.
		hlds_out__write_goal_list(Goals, ModuleInfo, VarSet,
			AppendVarnums, Indent, "&\n", TypeQual, !IO),
		hlds_out__write_indent(Indent, !IO),
		io__write_string(")", !IO),
		io__write_string(Follow, !IO)
	;
		io__write_string("/* parallel */ true", !IO),
		io__write_string(Follow, !IO)
	).

hlds_out__write_goal_2(disj(List), ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	( List = [Goal | Goals] ->
		io__write_string("( % disjunction\n", !IO),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent + 1, "\n", TypeQual, !IO),
		hlds_out__write_goal_list(Goals, ModuleInfo, VarSet,
			AppendVarnums, Indent, ";\n", TypeQual, !IO),
		hlds_out__write_indent(Indent, !IO),
		io__write_string(")", !IO),
		io__write_string(Follow, !IO)
	;
		io__write_string("fail", !IO),
		io__write_string(Follow, !IO)
	).

hlds_out__write_goal_2(generic_call(GenericCall, ArgVars, Modes, _),
		ModuleInfo, VarSet, AppendVarnums, Indent, Follow, _, !IO) :-
		% XXX we should print more info here
	(
		GenericCall = higher_order(PredVar, Purity, PredOrFunc, _),
		globals__io_lookup_string_option(dump_hlds_options, Verbose,
			!IO),
		(
			PredOrFunc = predicate,
			( string__contains_char(Verbose, 'l') ->
				hlds_out__write_indent(Indent, !IO),
				io__write_string("% higher-order " ++
					"predicate call\n", !IO)
			;
				true
			),
			hlds_out__write_indent(Indent, !IO),
			write_purity_prefix(Purity, !IO),
			hlds_out__write_functor(term__atom("call"),
				[PredVar | ArgVars], VarSet, AppendVarnums, !IO)
		;
			PredOrFunc = function,
			( string__contains_char(Verbose, 'l') ->
				hlds_out__write_indent(Indent, !IO),
				io__write_string("% higher-order " ++
					"function application\n", !IO)
			;
				true
			),
			pred_args_to_func_args([PredVar | ArgVars],
				FuncArgVars, FuncRetVar),
			hlds_out__write_indent(Indent, !IO),
			write_purity_prefix(Purity, !IO),
			mercury_output_var(FuncRetVar, VarSet, AppendVarnums,
				!IO),
			io__write_string(" = ", !IO),
			hlds_out__write_functor(term__atom("apply"),
				FuncArgVars, VarSet, AppendVarnums, !IO)
		),
		io__write_string(Follow, !IO)
	;
		GenericCall = class_method(TCInfoVar, MethodNum, _ClassId,
			_MethodId),
		globals__io_lookup_string_option(dump_hlds_options, Verbose,
			!IO),
		( string__contains_char(Verbose, 'l') ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% class method call\n", !IO)
		;
			true
		),
		term__context_init(Context),
		Functor = term__atom("class_method_call"),
		TCInfoTerm = term__variable(TCInfoVar),
		MethodNumTerm = term__functor(term__integer(MethodNum), [],
			Context),
		term__var_list_to_term_list(ArgVars, ArgTerms),
		Term = term__functor(Functor,
			[TCInfoTerm, MethodNumTerm | ArgTerms], Context),
		hlds_out__write_indent(Indent, !IO),
		mercury_output_term(Term, VarSet, AppendVarnums, !IO),
		io__write_string(Follow, !IO)
	;
		GenericCall = unsafe_cast,
		globals__io_lookup_string_option(dump_hlds_options, Verbose,
			!IO),
		( string__contains_char(Verbose, 'l') ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% unsafe_cast\n", !IO)
		;
			true
		),
		( string__contains_char(Verbose, 'D') ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% modes: ", !IO),
			varset__init(InstVarSet),
			mercury_output_mode_list(Modes, InstVarSet, !IO),
			io__nl(!IO)
		;
			true
		),
		Functor = term__atom("unsafe_cast"),
		term__var_list_to_term_list(ArgVars, ArgTerms),
		term__context_init(Context),
		Term = term__functor(Functor, ArgTerms, Context),
		hlds_out__write_indent(Indent, !IO),
		mercury_output_term(Term, VarSet, AppendVarnums, !IO),
		io__write_string(Follow, !IO)
	;
		GenericCall = aditi_builtin(AditiBuiltin, CallId),
		hlds_out__write_indent(Indent, !IO),
		hlds_out__write_aditi_builtin(ModuleInfo, AditiBuiltin, CallId,
			ArgVars, VarSet, AppendVarnums, Indent, Follow, !IO)
	).

hlds_out__write_goal_2(call(PredId, ProcId, ArgVars, Builtin,
		MaybeUnifyContext, PredName), ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual, !IO) :-
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'b') ->
		(
			Builtin = inline_builtin,
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% inline builtin\n", !IO)
		;
			Builtin = out_of_line_builtin,
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% out of line builtin\n", !IO)
		;
			Builtin = not_builtin
		)
	;
		true
	),
	hlds_out__write_indent(Indent, !IO),
	( PredId = invalid_pred_id ->
			% If we don't know then the call must be treated
			% as a predicate.
		PredOrFunc = predicate
	;
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_get_purity(PredInfo, Purity),
		PredOrFunc = pred_info_is_pred_or_func(PredInfo),
		write_purity_prefix(Purity, !IO)
	),
	(
		PredOrFunc = predicate,
		NewArgVars = ArgVars
	;
		PredOrFunc = function,
		pred_args_to_func_args(ArgVars, NewArgVars, LHSVar),
		mercury_output_var(LHSVar, VarSet, AppendVarnums, !IO),
		io__write_string(" = ", !IO)
	),
	hlds_out__write_sym_name_and_args(PredName, NewArgVars, VarSet,
		AppendVarnums, !IO),
	io__write_string(Follow, !IO),
	( string__contains_char(Verbose, 'l') ->
		pred_id_to_int(PredId, PredNum),
		proc_id_to_int(ProcId, ProcNum),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% pred id: ", !IO),
		io__write_int(PredNum, !IO),
		io__write_string(", proc id: ", !IO),
		io__write_int(ProcNum, !IO),
		io__write_string(Follow, !IO),
		( MaybeUnifyContext = yes(CallUnifyContext) ->
			( TypeQual = yes(_, VarTypes) ->
				map__lookup(VarTypes, Var, UniType),
				VarType = yes(UniType)
			;
				VarType = no
			),
			CallUnifyContext = call_unify_context(Var,
					RHS, _UnifyContext),
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% unify context: ", !IO),
			mercury_output_var(Var, VarSet, AppendVarnums, !IO),
			io__write_string(" = ", !IO),
				% XXX Fake the inst varset
			varset__init(InstVarSet),
			hlds_out__write_unify_rhs_2(RHS, ModuleInfo, VarSet,
				InstVarSet, AppendVarnums, Indent, Follow,
				VarType, TypeQual, !IO)
		;
			true
		)
	;
		true
	).

hlds_out__write_goal_2(unify(A, B, _, Unification, _), ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	mercury_output_var(A, VarSet, AppendVarnums, !IO),
	io__write_string(" = ", !IO),
	( TypeQual = yes(_, VarTypes) ->
		map__lookup(VarTypes, A, UniType),
		VarType = yes(UniType)
	;
		VarType = no
	),
		% XXX Fake the inst varset
	varset__init(InstVarSet),
	hlds_out__write_unify_rhs_2(B, ModuleInfo, VarSet, InstVarSet,
		AppendVarnums, Indent, Follow, VarType, TypeQual, !IO),
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	(
		(
			string__contains_char(Verbose, 'u')
		;
			string__contains_char(Verbose, 'p')
		)
	->
		(
			% don't output bogus info if we haven't been through
			% mode analysis yet
			Unification = complicated_unify(Mode, CanFail,
				TypeInfoVars),
			CanFail = can_fail,
			Mode = (free - free -> free - free),
			TypeInfoVars = []
		->
			true
		;
			hlds_out__write_unification(Unification, ModuleInfo,
				VarSet, InstVarSet, AppendVarnums, Indent, !IO)
		)
	;
		true
	).

hlds_out__write_goal_2(foreign_proc(Attributes, _PredId, _ProcId, ArgVars,
		ArgNames, OrigArgTypes, PragmaCode),
		_, _, AppendVarNums, Indent, Follow, _, !IO) :-
	ForeignLang = foreign_language(Attributes),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("$pragma_foreign_proc( /* ", !IO),
	io__write_string(foreign_language_string(ForeignLang), !IO),
	io__write_string(" */ [", !IO),
	hlds_out__write_varnum_list(ArgVars, !IO),
	io__write_string("], [", !IO),
	get_pragma_foreign_var_names(ArgNames, Names),
	hlds_out__write_string_list(Names, !IO),
	io__write_string("], [", !IO),
	% XXX We don't have the TypeVarSet available here,
	%     but it's only used for printing out the names of the
	%     type variables, which isn't essential.
	varset__init(TypeVarSet),
	hlds_out__write_type_list(OrigArgTypes, TypeVarSet, AppendVarNums, !IO),
	io__write_string("], ", !IO),
	(
		PragmaCode = ordinary(C_Code, _),
		io__write_string("""", !IO),
		io__write_string(C_Code, !IO),
		io__write_string("""", !IO)
	;
		PragmaCode = nondet(Fields, _FieldsContext,
			First, _FirstContext, Later, _LaterContext,
			Treat, Shared, _SharedContext),
		io__write_string("local_vars(""", !IO),
		io__write_string(Fields, !IO),
		io__write_string("""), ", !IO),
		io__write_string("first_code(""", !IO),
		io__write_string(First, !IO),
		io__write_string("""), ", !IO),
		io__write_string("retry_code(""", !IO),
		io__write_string(Later, !IO),
		io__write_string("""), ", !IO),
		(
			Treat = share,
			io__write_string("shared_code(""", !IO)
		;
			Treat = duplicate,
			io__write_string("duplicated_code(""", !IO)
		;
			Treat = automatic,
			io__write_string("common_code(""", !IO)
		),
		io__write_string(Shared, !IO),
		io__write_string(""")", !IO)
	;
		PragmaCode = import(Name, _, _, _Context),
		io__write_string("""", !IO),
		io__write_string(Name, !IO),
		io__write_string("""", !IO)
	),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO).

hlds_out__write_goal_2(shorthand(ShortHandGoal), ModuleInfo, VarSet,
		AppendVarnums, Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_goal_2_shorthand(ShortHandGoal, ModuleInfo,
		VarSet, AppendVarnums, Indent, Follow, TypeQual, !IO).

:- pred hlds_out__write_goal_2_shorthand(shorthand_goal_expr::in,
	module_info::in, prog_varset::in, bool::in, int::in, string::in,
	maybe_vartypes::in, io::di, io::uo) is det.

hlds_out__write_goal_2_shorthand(bi_implication(LHS, RHS), ModuleInfo,
		VarSet,	AppendVarnums, Indent, Follow, TypeQual, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("( % bi-implication\n", !IO),
	Indent1 = Indent + 1,
	hlds_out__write_goal_a(LHS, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual, !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("<=>\n", !IO),
	hlds_out__write_goal_a(RHS, ModuleInfo, VarSet, AppendVarnums,
		Indent1, "\n", TypeQual, !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO).

:- pred hlds_out__write_llds_code_gen_info(hlds_goal_info::in, prog_varset::in,
	bool::in, int::in, string::in, io__state::di, io__state::uo) is det.

hlds_out__write_llds_code_gen_info(GoalInfo, VarSet, AppendVarnums,
		Indent, Verbose, !IO) :-
	( string__contains_char(Verbose, 'f') ->
		goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
		(
			MaybeFollowVars = yes(FollowVars),
			FollowVars = follow_vars(FollowVarsMap, NextReg),
			map__to_assoc_list(FollowVarsMap, FVlist),
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% follow vars: ", !IO),
			io__write_int(NextReg, !IO),
			io__write_string("\n", !IO),
			hlds_out__write_var_to_lvals(FVlist, VarSet,
				AppendVarnums, Indent, !IO)
		;
			MaybeFollowVars = no
		)
	;
		true
	),
	( string__contains_char(Verbose, 'r') ->
		goal_info_get_resume_point(GoalInfo, Resume),
		(
			Resume = no_resume_point
		;
			Resume = resume_point(ResumeVars, Locs),
			set__to_sorted_list(ResumeVars, ResumeVarList),
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% resume point ", !IO),
			(
				Locs = orig_only,
				io__write_string("orig only ", !IO)
			;
				Locs = stack_only,
				io__write_string("stack only ", !IO)
			;
				Locs = orig_and_stack,
				io__write_string("orig and stack ", !IO)
			;
				Locs = stack_and_orig,
				io__write_string("stack and orig ", !IO)
			),
			mercury_output_vars(ResumeVarList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		)
	;
		true
	),
	(
		string__contains_char(Verbose, 's'),
		goal_info_get_store_map(GoalInfo, StoreMap),
		map__to_assoc_list(StoreMap, StoreMaplist),
		StoreMaplist \= []
	->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% store map:\n", !IO),
		hlds_out__write_var_to_lvals(StoreMaplist, VarSet,
			AppendVarnums, Indent, !IO)
	;
		true
	),
	(
		string__contains_char(Verbose, 's'),
		goal_info_get_maybe_need_across_call(GoalInfo,
			MaybeNeedAcrossCall),
		MaybeNeedAcrossCall = yes(NeedAcrossCall)
	->
		NeedAcrossCall = need_across_call(CallForwardSet,
			CallResumeSet, CallNondetSet),
		set__to_sorted_list(CallForwardSet, CallForwardList),
		set__to_sorted_list(CallResumeSet, CallResumeList),
		set__to_sorted_list(CallNondetSet, CallNondetList),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% need across call forward vars: ", !IO),
		( CallForwardList = [] ->
			io__write_string("none\n", !IO)
		;
			hlds_out__write_vars(CallForwardList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		),

		hlds_out__write_indent(Indent, !IO),
		io__write_string("% need across call resume vars: ", !IO),
		( CallResumeList = [] ->
			io__write_string("none\n", !IO)
		;
			hlds_out__write_vars(CallResumeList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		),

		hlds_out__write_indent(Indent, !IO),
		io__write_string("% need across call nondet vars: ", !IO),
		( CallNondetList = [] ->
			io__write_string("none\n", !IO)
		;
			hlds_out__write_vars(CallNondetList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		)
	;
		true
	),
	(
		string__contains_char(Verbose, 's'),
		goal_info_get_maybe_need_in_resume(GoalInfo,
			MaybeNeedInResume),
		MaybeNeedInResume = yes(NeedInResume)
	->
		NeedInResume = need_in_resume(ResumeOnStack, ResumeResumeSet,
			ResumeNondetSet),
		set__to_sorted_list(ResumeResumeSet, ResumeResumeList),
		set__to_sorted_list(ResumeNondetSet, ResumeNondetList),

		hlds_out__write_indent(Indent, !IO),
		(
			ResumeOnStack = yes,
			io__write_string("% resume point has stack label\n",
				!IO)
		;
			ResumeOnStack = no,
			io__write_string("% resume point has no stack label\n",
				!IO)
		),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% need in resume resume vars: ", !IO),
		( ResumeResumeList = [] ->
			io__write_string("none\n", !IO)
		;
			hlds_out__write_vars(ResumeResumeList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		),

		hlds_out__write_indent(Indent, !IO),
		io__write_string("% need in resume nondet vars: ", !IO),
		( ResumeNondetList = [] ->
			io__write_string("none\n", !IO)
		;
			hlds_out__write_vars(ResumeNondetList, VarSet,
				AppendVarnums, !IO),
			io__write_string("\n", !IO)
		)
	;
		true
	),
	(
		string__contains_char(Verbose, 's'),
		goal_info_get_maybe_need_in_par_conj(GoalInfo,
			MaybeNeedInParConj),
		MaybeNeedInParConj = yes(NeedInParConj)
	->
		NeedInParConj = need_in_par_conj(ParConjSet),
		set__to_sorted_list(ParConjSet, ParConjList),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% need in par_conj vars: ", !IO),
		hlds_out__write_vars(ParConjList, VarSet, AppendVarnums, !IO),
		io__write_string("\n", !IO)
	;
		true
	).

:- pred hlds_out__write_vars(list(prog_var)::in, prog_varset::in, bool::in,
	io__state::di, io__state::uo) is det.

hlds_out__write_vars([], _, _, !IO).
hlds_out__write_vars([Var], VarSet, AppendVarnums, !IO) :-
	mercury_output_var(Var, VarSet, AppendVarnums, !IO).
hlds_out__write_vars([Var1, Var2 | Vars], VarSet, AppendVarnums, !IO) :-
	mercury_output_var(Var1, VarSet, AppendVarnums, !IO),
	io__write_string(", ", !IO),
	hlds_out__write_vars([Var2 | Vars], VarSet, AppendVarnums, !IO).

:- pred hlds_out__write_varnum_list(list(prog_var)::in, io::di, io::uo) is det.

hlds_out__write_varnum_list([], !IO).
hlds_out__write_varnum_list([Var], !IO) :-
	hlds_out__write_varnum(Var, !IO).
hlds_out__write_varnum_list([Var1, Var2 | Vars], !IO) :-
	hlds_out__write_varnum(Var1, !IO),
	io__write_string(", ", !IO),
	hlds_out__write_varnum_list([Var2 | Vars], !IO).

:- pred hlds_out__write_varnum(var(T)::in, io::di, io::uo) is det.

hlds_out__write_varnum(Var, !IO) :-
	term__var_to_int(Var, VarNum),
	io__write_int(VarNum, !IO).

:- pred hlds_out__write_var_name_list(list(pair(var(T), string))::in,
	io::di, io::uo) is det.

hlds_out__write_var_name_list([], !IO).
hlds_out__write_var_name_list([Var - Name], !IO) :-
	hlds_out__write_varnum(Var, !IO),
	io__write_string(" - ", !IO),
	io__write_string(Name, !IO).

hlds_out__write_var_name_list([Var1 - Name1, VarName2 | Vars], !IO) :-
	hlds_out__write_varnum(Var1, !IO),
	io__write_string(" - ", !IO),
	io__write_string(Name1, !IO),
	io__write_string(", ", !IO),
	hlds_out__write_var_name_list([VarName2 | Vars], !IO).

:- pred hlds_out__write_string_list(list(string)::in, io::di, io::uo) is det.

hlds_out__write_string_list([], !IO).
hlds_out__write_string_list([Name], !IO) :-
	io__write_string(Name, !IO).
hlds_out__write_string_list([Name1, Name2 | Names], !IO) :-
	io__write_string(Name1, !IO),
	io__write_string(", ", !IO),
	hlds_out__write_string_list([Name2 | Names], !IO).

:- pred hlds_out__write_aditi_builtin(module_info::in, aditi_builtin::in,
	simple_call_id::in, list(prog_var)::in, prog_varset::in, bool::in,
	int::in, string::in, io::di, io::uo) is det.

hlds_out__write_aditi_builtin(_ModuleInfo,
		aditi_tuple_update(InsertDelete, PredId), CallId,
		ArgVars, VarSet, AppendVarnums, Indent, Follow, !IO) :-
	% make_hlds.m checks the arity so this cannot fail.
	get_state_args_det(ArgVars, Args, State0Var, StateVar),
	hlds_out__write_indent(Indent, !IO),

	(
		InsertDelete = insert,
		io__write_string("aditi_insert(", !IO)
	;
		InsertDelete = delete,
		io__write_string("aditi_delete(", !IO)
	),

	CallId = PredOrFunc - SymName/_,
	(
		PredOrFunc = predicate,
		hlds_out__write_sym_name_and_args(SymName, Args,
			VarSet, AppendVarnums, !IO)
	;
		PredOrFunc = function,
		pred_args_to_func_args(Args, FuncArgs, RetArg),
		io__write_string("(", !IO),
		hlds_out__write_sym_name_and_args(SymName, FuncArgs,
			VarSet, AppendVarnums, !IO),
		io__write_string(" = ", !IO),
		mercury_output_var(RetArg, VarSet, AppendVarnums, !IO),
		io__write_string(")", !IO)
	),
	io__write_string(", ", !IO),
	mercury_output_var(State0Var, VarSet, AppendVarnums, !IO),
	io__write_string(", ", !IO),
	mercury_output_var(StateVar, VarSet, AppendVarnums, !IO),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO),
	io__nl(!IO),
	hlds_out__write_aditi_builtin_pred_id(Indent, PredId, !IO).

hlds_out__write_aditi_builtin(_ModuleInfo, Builtin, CallId,
		ArgVars, VarSet, AppendVarnums, Indent, Follow, !IO) :-
	Builtin = aditi_bulk_update(_, PredId, _Syntax),
	hlds_out__write_indent(Indent, !IO),
	hlds_out__aditi_builtin_name(Builtin, UpdateName),
	io__write_string(UpdateName, !IO),
	io__write_string("(", !IO),
	CallId = PredOrFunc - _,
	hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
	io__write_string(PredOrFuncStr, !IO),
	io__write_string(" ", !IO),
	hlds_out__simple_call_id_to_sym_name_and_arity(CallId, SymArity),
	prog_out__write_sym_name_and_arity(SymArity, !IO),
	io__write_string(", ", !IO),
	mercury_output_vars(ArgVars, VarSet, AppendVarnums, !IO),
	io__write_string(")", !IO),
	io__write_string(Follow, !IO),
	io__nl(!IO),
	hlds_out__write_aditi_builtin_pred_id(Indent, PredId, !IO).

:- pred hlds_out__write_aditi_builtin_pred_id(int::in, pred_id::in,
	io::di, io::uo) is det.

hlds_out__write_aditi_builtin_pred_id(Indent, PredId, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% Update of pred_id: ", !IO),
	pred_id_to_int(PredId, PredInt),
	io__write_int(PredInt, !IO),
	io__write_string(".\n", !IO).

hlds_out__aditi_builtin_name(aditi_tuple_update(_, _), "aditi_insert").
hlds_out__aditi_builtin_name(aditi_bulk_update(Update, _, _), Name) :-
	hlds_out__aditi_bulk_update_name(Update, Name).

:- pred hlds_out__aditi_bulk_update_name(aditi_bulk_update::in, string::out)
	is det.

hlds_out__aditi_bulk_update_name(bulk_insert, "aditi_bulk_insert").
hlds_out__aditi_bulk_update_name(bulk_delete, "aditi_bulk_delete").
hlds_out__aditi_bulk_update_name(bulk_modify, "aditi_bulk_modify").

:- pred hlds_out__write_unification(unification::in, module_info::in,
	prog_varset::in, inst_varset::in, bool::in, int::in, io::di, io::uo)
	is det.

hlds_out__write_unification(assign(X, Y), _, ProgVarSet, _InstVarSet,
		AppendVarnums, Indent, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_var(X, ProgVarSet, AppendVarnums, !IO),
	io__write_string(" := ", !IO),
	mercury_output_var(Y, ProgVarSet, AppendVarnums, !IO),
	io__write_string("\n", !IO).

hlds_out__write_unification(simple_test(X, Y), _, ProgVarSet, _, AppendVarnums,
		Indent, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_var(X, ProgVarSet, AppendVarnums, !IO),
	io__write_string(" == ", !IO),
	mercury_output_var(Y, ProgVarSet, AppendVarnums, !IO),
	io__write_string("\n", !IO).

hlds_out__write_unification(construct(Var, ConsId, ArgVars, ArgModes,
		_ConstructHow, Uniqueness, Size), ModuleInfo, ProgVarSet,
		InstVarSet, AppendVarnums, Indent, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_var(Var, ProgVarSet, AppendVarnums, !IO),
	io__write_string(" := ", !IO),
	hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes,
		ModuleInfo, ProgVarSet, InstVarSet, AppendVarnums, Indent,
		!IO),
	(
		Uniqueness = cell_is_unique,
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% cell_is_unique\n", !IO)
	;
		Uniqueness = cell_is_shared
	),
	(
		Size = yes(SizeSource),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% term size ", !IO),
		(
			SizeSource = known_size(KnownSize),
			io__write_string("const ", !IO),
			io__write_int(KnownSize, !IO),
			io__write_string("\n", !IO)
		;
			SizeSource = dynamic_size(SizeVar),
			io__write_string("var ", !IO),
			mercury_output_var(SizeVar, ProgVarSet, AppendVarnums,
				!IO),
			io__write_string("\n", !IO)
		)
	;
		Size = no
	).

hlds_out__write_unification(deconstruct(Var, ConsId, ArgVars, ArgModes,
		CanFail, CanCGC), ModuleInfo, ProgVarSet, InstVarSet,
		AppendVarnums, Indent, !IO) :-
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'G') ->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% Compile time garbage collect: ", !IO),
		io__write(CanCGC, !IO),
		io__nl(!IO)
	;
		true
	),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_var(Var, ProgVarSet, AppendVarnums, !IO),
	(
		CanFail = can_fail,
		io__write_string(" ?= ", !IO)
	;
		CanFail = cannot_fail,
		io__write_string(" => ", !IO)
	),
	hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes,
		ModuleInfo, ProgVarSet, InstVarSet, AppendVarnums, Indent,
		!IO).

hlds_out__write_unification(complicated_unify(Mode, CanFail, TypeInfoVars),
		_ModuleInfo, ProgVarSet, InstVarSet, AppendVarNums, Indent,
		!IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	(
		CanFail = can_fail,
		io__write_string("can_fail, ", !IO)
	;
		CanFail = cannot_fail,
		io__write_string("cannot_fail, ", !IO)
	),
	io__write_string("mode: ", !IO),
	mercury_output_uni_mode(Mode, InstVarSet, !IO),
	io__write_string("\n", !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% type-info vars: ", !IO),
	mercury_output_vars(TypeInfoVars, ProgVarSet, AppendVarNums, !IO),
	io__write_string("\n", !IO).

:- pred hlds_out_write_functor_and_submodes(cons_id::in, list(prog_var)::in,
	list(uni_mode)::in, module_info::in, prog_varset::in, inst_varset::in,
	bool::in, int::in, io::di, io::uo) is det.

hlds_out_write_functor_and_submodes(ConsId, ArgVars, ArgModes, _ModuleInfo,
		ProgVarSet, InstVarSet, AppendVarnums, Indent, !IO) :-
	hlds_out__write_cons_id(ConsId, !IO),
	( ArgVars = [] ->
		io__write_string("\n", !IO)
	;
		io__write_string(" (", !IO),
		mercury_output_vars(ArgVars, ProgVarSet, AppendVarnums, !IO),
		io__write_string(")\n", !IO),
		globals__io_lookup_string_option(dump_hlds_options, Verbose,
			!IO),
		( string__contains_char(Verbose, 'a') ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% arg-modes ", !IO),
			mercury_output_uni_mode_list(ArgModes, InstVarSet, !IO),
			io__write_string("\n", !IO)
		;
			true
		)
	).

hlds_out__write_unify_rhs(Rhs, ModuleInfo, VarSet, InstVarSet, AppendVarnums,
		Indent, !IO) :-
	hlds_out__write_unify_rhs_3(Rhs, ModuleInfo, VarSet, InstVarSet,
		AppendVarnums, Indent, no, no, !IO).

:- pred hlds_out__write_unify_rhs_2(unify_rhs::in, module_info::in,
	prog_varset::in, inst_varset::in, bool::in, int::in, string::in,
	maybe(type)::in, maybe_vartypes::in, io::di, io::uo) is det.

hlds_out__write_unify_rhs_2(Rhs, ModuleInfo, VarSet, InstVarSet, AppendVarnums,
		Indent, Follow, MaybeType, TypeQual, !IO) :-
	hlds_out__write_unify_rhs_3(Rhs, ModuleInfo, VarSet, InstVarSet,
		AppendVarnums, Indent, MaybeType, TypeQual, !IO),
	io__write_string(Follow, !IO).

:- pred hlds_out__write_unify_rhs_3(unify_rhs::in, module_info::in,
	prog_varset::in, inst_varset::in, bool::in, int::in, maybe(type)::in,
	maybe_vartypes::in, io::di, io::uo) is det.

hlds_out__write_unify_rhs_3(var(Var), _, VarSet, _, AppendVarnums, _, _, _,
		!IO) :-
	mercury_output_var(Var, VarSet, AppendVarnums, !IO).
hlds_out__write_unify_rhs_3(functor(ConsId0, IsExistConstruct, ArgVars),
		ModuleInfo, VarSet, _, AppendVarnums, _Indent,
		MaybeType, TypeQual, !IO) :-
	(
		IsExistConstruct = yes,
		ConsId0 = cons(SymName0, Arity)
	->
		remove_new_prefix(SymName, SymName0),
		ConsId = cons(SymName, Arity)
	;
		ConsId = ConsId0
	),
	hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet, ModuleInfo,
		AppendVarnums, !IO),
	(
		MaybeType = yes(Type),
		TypeQual = yes(TVarSet, _)
	->
		io__write_string(" `with_type` ", !IO),
		mercury_output_term(Type, TVarSet, no, next_to_graphic_token,
			!IO)
	;
		true
	).

hlds_out__write_unify_rhs_3(lambda_goal(Purity, PredOrFunc, EvalMethod, _,
		NonLocals, Vars, Modes, Det, Goal), ModuleInfo, VarSet,
		InstVarSet, AppendVarnums, Indent, MaybeType, TypeQual, !IO) :-
	Indent1 = Indent + 1,
	write_purity_prefix(Purity, !IO),
	(
		EvalMethod = normal,
		EvalStr = ""
	;
		EvalMethod = (aditi_bottom_up),
		EvalStr = "aditi_bottom_up "
	),
	(
		PredOrFunc = predicate,
		io__write_string("(", !IO),
		io__write_string(EvalStr, !IO),
		( Vars = [] ->
			io__write_string("(pred)", !IO)
		;
			io__write_string("pred(", !IO),
			hlds_out__write_var_modes(Vars, Modes, VarSet,
				InstVarSet, AppendVarnums, !IO),
			io__write_string(")", !IO)
		),
		io__write_string(" is ", !IO),
		mercury_output_det(Det, !IO),
		io__write_string(" :-\n", !IO),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual, !IO),
		hlds_out__write_indent(Indent, !IO),
		io__write_string(")", !IO)
	;
		PredOrFunc = function,
		pred_args_to_func_args(Modes, ArgModes, RetMode),
		pred_args_to_func_args(Vars, ArgVars, RetVar),
		io__write_string("(", !IO),
		io__write_string(EvalStr, !IO),
		( ArgVars = [] ->
			io__write_string("(func)", !IO)
		;
			io__write_string("func(", !IO),
			hlds_out__write_var_modes(ArgVars, ArgModes, VarSet,
				InstVarSet, AppendVarnums, !IO),
			io__write_string(")", !IO)
		),
		io__write_string(" = (", !IO),
		hlds_out__write_var_mode(RetVar, RetMode, VarSet,
			InstVarSet, AppendVarnums, !IO),
		io__write_string(") is ", !IO),
		mercury_output_det(Det, !IO),
		io__write_string(" :-\n", !IO),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, "\n", TypeQual, !IO),
		hlds_out__write_indent(Indent, !IO),
		io__write_string(")", !IO)
	),
	(
		MaybeType = yes(Type),
		TypeQual = yes(TVarSet, _)
	->
		io__write_string(" `with_type` ", !IO),
		mercury_output_term(Type, TVarSet, AppendVarnums,
			next_to_graphic_token, !IO)
	;
		true
	),
        globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'n') ->
		( NonLocals \= [] ->
			hlds_out__write_indent(Indent1, !IO),
			io__write_string("% lambda nonlocals: ", !IO),
			mercury_output_vars(NonLocals, VarSet, AppendVarnums,
				!IO)
		;
			true
		)
	;
		true
	).

:- pred hlds_out__write_sym_name_and_args(sym_name::in, list(prog_var)::in,
	prog_varset::in, bool::in, io::di, io::uo) is det.

hlds_out__write_sym_name_and_args(PredName, ArgVars, VarSet, AppendVarnums,
		!IO) :-
	(
		PredName = qualified(ModuleName, Name),
		hlds_out__write_qualified_functor(ModuleName,
			term__atom(Name), ArgVars, VarSet, AppendVarnums, !IO)
	;
		PredName = unqualified(Name),
		hlds_out__write_functor(term__atom(Name), ArgVars, VarSet,
			AppendVarnums, next_to_graphic_token, !IO)
	).

hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums, !IO) :-
	hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums,
		not_next_to_graphic_token, !IO).

:- pred hlds_out__write_functor(const::in, list(prog_var)::in, prog_varset::in,
	bool::in, needs_quotes::in, io::di, io::uo) is det.

hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums,
		NextToGraphicToken, !IO) :-
	term__context_init(Context),
	term__var_list_to_term_list(ArgVars, ArgTerms),
	Term = term__functor(Functor, ArgTerms, Context),
	mercury_output_term(Term, VarSet, AppendVarnums, NextToGraphicToken,
		!IO).

:- pred hlds_out__write_qualified_functor(module_name::in, const::in,
	list(prog_var)::in, prog_varset::in, bool::in, io::di, io::uo) is det.

hlds_out__write_qualified_functor(ModuleName, Functor, ArgVars, VarSet,
		AppendVarnums, !IO) :-
	mercury_output_bracketed_sym_name(ModuleName, !IO),
	io__write_string(".", !IO),
	hlds_out__write_functor(Functor, ArgVars, VarSet, AppendVarnums,
		next_to_graphic_token, !IO).

:- pred hlds_out__write_qualified_functor_with_term_args(module_name::in,
	const::in, list(prog_term)::in, prog_varset::in, bool::in,
	io::di, io::uo) is det.

hlds_out__write_qualified_functor_with_term_args(ModuleName, Functor,
		ArgTerms, VarSet, AppendVarNums, !IO) :-
	mercury_output_bracketed_sym_name(ModuleName, !IO),
	io__write_string(".", !IO),
	term__context_init(Context),
	mercury_output_term(term__functor(Functor, ArgTerms, Context), VarSet,
		AppendVarNums, next_to_graphic_token, !IO).

hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet, ModuleInfo,
		AppendVarnums, !IO) :-
	(
		ConsId = cons(SymName, _),
		(
			SymName = qualified(Module, Name),
			hlds_out__write_qualified_functor(Module,
				term__atom(Name), ArgVars, VarSet,
				AppendVarnums, !IO)
		;
			SymName = unqualified(Name),
			hlds_out__write_functor(term__atom(Name),
				ArgVars, VarSet, AppendVarnums,
				next_to_graphic_token, !IO)
		)
	;
		ConsId = int_const(Int),
		hlds_out__write_functor(term__integer(Int), ArgVars,
			VarSet, AppendVarnums, !IO)
	;
		ConsId = float_const(Float),
		hlds_out__write_functor(term__float(Float), ArgVars,
			VarSet, AppendVarnums, !IO)
	;
		ConsId = string_const(Str),
		hlds_out__write_functor(term__string(Str), ArgVars,
			VarSet, AppendVarnums, !IO)
	;
		ConsId = pred_const(PredId, _, _),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		PredModule = pred_info_module(PredInfo),
		PredName = pred_info_name(PredInfo),
		hlds_out__write_functor_cons_id(
			cons(qualified(PredModule, PredName),
				list__length(ArgVars)),
			ArgVars, VarSet, ModuleInfo, AppendVarnums, !IO)
	;
		ConsId = type_ctor_info_const(Module, Name, Arity),
		io__write_string("type_ctor_info(""", !IO),
		prog_out__write_sym_name(Module, !IO),
		io__write_string(""", """, !IO),
		io__write_string(Name, !IO),
		io__write_string(""", ", !IO),
		io__write_int(Arity, !IO),
		io__write_string(")", !IO)
	;
		ConsId = base_typeclass_info_const(Module,
			class_id(Name, Arity), _, Instance),
		io__write_string("base_typeclass_info(""", !IO),
		prog_out__write_sym_name(Module, !IO),
		io__write_string(""", ", !IO),
		io__write_string("class_id(", !IO),
		prog_out__write_sym_name(Name, !IO),
		io__write_string(", ", !IO),
		io__write_int(Arity, !IO),
		io__write_string("), ", !IO),
		io__write_string(Instance, !IO),
		io__write_string(")", !IO)
	;
		ConsId = type_info_cell_constructor(_),
		hlds_out__write_functor(
			term__atom("type_info_cell_constructor"),
			ArgVars, VarSet, AppendVarnums, next_to_graphic_token,
			!IO)
	;
		ConsId = typeclass_info_cell_constructor,
		hlds_out__write_functor(
			term__atom("typeclass_info_cell_constructor"),
			ArgVars, VarSet, AppendVarnums, next_to_graphic_token,
			!IO)
	;
		ConsId = tabling_pointer_const(PredId, ProcId),
		io__write_string("tabling_pointer_const(", !IO),
		hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
		io__write_string(", ", !IO),
		proc_id_to_int(ProcId, ProcIdInt),
		io__write_int(ProcIdInt, !IO),
		io__write_string(")", !IO)
	;
		ConsId = deep_profiling_proc_static(RttiProcLabel),
		rtti__proc_label_pred_proc_id(RttiProcLabel, PredId, ProcId),
		io__write_string("deep_profiling_proc_static(", !IO),
		hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
		proc_id_to_int(ProcId, ProcIdInt),
		io__write_string(" (mode ", !IO),
		io__write_int(ProcIdInt, !IO),
		io__write_string("))", !IO)
	;
		ConsId = table_io_decl(RttiProcLabel),
		rtti__proc_label_pred_proc_id(RttiProcLabel, PredId, ProcId),
		io__write_string("table_io_decl(", !IO),
		hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
		proc_id_to_int(ProcId, ProcIdInt),
		io__write_string(" (mode ", !IO),
		io__write_int(ProcIdInt, !IO),
		io__write_string("))", !IO)
	).

hlds_out__write_var_modes([], [], _, _, _, !IO).
hlds_out__write_var_modes([Var|Vars], [Mode|Modes], VarSet, InstVarSet,
		AppendVarnums, !IO) :-
	hlds_out__write_var_mode(Var, Mode, VarSet, InstVarSet, AppendVarnums,
		!IO),
	( Vars \= [] ->
		io__write_string(", ", !IO)
	;
		true
	),
	hlds_out__write_var_modes(Vars, Modes, VarSet, InstVarSet,
		AppendVarnums, !IO).

hlds_out__write_var_modes([], [_|_], _, _, _, !IO) :-
	error("hlds_out__write_var_modes: length mis-match").
hlds_out__write_var_modes([_|_], [], _, _, _, !IO) :-
	error("hlds_out__write_var_modes: length mis-match").

:- pred hlds_out__write_var_mode(prog_var::in, (mode)::in, prog_varset::in,
	inst_varset::in, bool::in, io::di, io::uo) is det.

hlds_out__write_var_mode(Var, Mode, VarSet, InstVarSet, AppendVarnums, !IO) :-
	mercury_output_var(Var, VarSet, AppendVarnums, !IO),
	io__write_string("::", !IO),
	mercury_output_mode(Mode, InstVarSet, !IO).

:- pred hlds_out__write_conj(hlds_goal::in, list(hlds_goal)::in,
	module_info::in, prog_varset::in, bool::in, int::in, string::in,
	string::in, string::in, maybe_vartypes::in, io::di, io::uo) is det.

hlds_out__write_conj(Goal1, Goals1, ModuleInfo, VarSet, AppendVarnums,
		Indent, Follow, Verbose, Separator, TypeQual, !IO) :-
	(
		Goals1 = [Goal2 | Goals2],
		( Verbose \= "" ->
			% when generating verbose dumps,
			% we want the comma on its own line,
			% since that way it visually separates
			% the lines after one goal
			% and the lines before the next
			hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
				AppendVarnums, Indent, "\n", TypeQual, !IO),
			hlds_out__write_indent(Indent, !IO),
			io__write_string(Separator, !IO)
		;
			hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
				AppendVarnums, Indent, Separator, TypeQual,
				!IO)
		),
		hlds_out__write_conj(Goal2, Goals2, ModuleInfo, VarSet,
			AppendVarnums, Indent, Follow, Verbose, Separator,
			TypeQual, !IO)
	;
		Goals1 = [],
		hlds_out__write_goal_a(Goal1, ModuleInfo, VarSet,
			AppendVarnums, Indent, Follow, TypeQual, !IO)
	).

hlds_out__write_goal_list(GoalList, ModuleInfo, VarSet, AppendVarnums, Indent,
		Separator, TypeQual, !IO) :-
	(
		GoalList = [Goal | Goals],
		hlds_out__write_indent(Indent, !IO),
		io__write_string(Separator, !IO),
		hlds_out__write_goal_a(Goal, ModuleInfo, VarSet,
			AppendVarnums, Indent + 1, "\n", TypeQual, !IO),
		hlds_out__write_goal_list(Goals, ModuleInfo, VarSet,
			AppendVarnums, Indent, Separator, TypeQual, !IO)
	;
		GoalList = []
	).

:- pred hlds_out__write_case(case::in, prog_var::in, module_info::in,
	prog_varset::in, bool::in, int::in, maybe_vartypes::in,
	io::di, io::uo) is det.

hlds_out__write_case(case(ConsId, Goal), Var, ModuleInfo, VarSet,
		AppendVarnums, Indent, VarTypes, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_var(Var, VarSet, AppendVarnums, !IO),
	io__write_string(" has functor ", !IO),
	hlds_out__write_cons_id(ConsId, !IO),
	io__write_string("\n", !IO),
	% XXX if the output of this is to be used, e.g. in
	% inter-module optimization, output a unification to bind the
	% Var to the functor, since simplify.m and unused_args.m remove
	% the unification. At the moment this is not a problem, since
	% intermod.m works on the unoptimized clauses.
	hlds_out__write_goal_a(Goal, ModuleInfo, VarSet, AppendVarnums,
		Indent, "\n", VarTypes, !IO).

:- pred hlds_out__write_cases(list(case)::in, prog_var::in, module_info::in,
	prog_varset::in, bool::in, int::in, maybe_vartypes::in,
	io::di, io::uo) is det.

hlds_out__write_cases(CasesList, Var, ModuleInfo, VarSet, AppendVarnums,
		Indent, VarTypes, !IO) :-
	(
		CasesList = [Case | Cases],
		hlds_out__write_indent(Indent, !IO),
		io__write_string(";\n", !IO),
		hlds_out__write_case(Case, Var, ModuleInfo,
			VarSet, AppendVarnums, Indent + 1, VarTypes, !IO),
		hlds_out__write_cases(Cases, Var, ModuleInfo,
			VarSet, AppendVarnums, Indent, VarTypes, !IO)
	;
		CasesList = []
	).

:- pred hlds_out__write_some(list(prog_var)::in, prog_varset::in,
	io::di, io::uo) is det.

	% quantification is all implicit by the time we get to the hlds.

hlds_out__write_some(_Vars, _VarSet, !IO).

hlds_out__write_instmap(InstMap, VarSet, AppendVarnums, Indent, !IO) :-
	( instmap__is_unreachable(InstMap) ->
		io__write_string("unreachable", !IO)
	;
		instmap__to_assoc_list(InstMap, AssocList),
		hlds_out__write_instmap_2(AssocList, VarSet, AppendVarnums,
			Indent, !IO)
	).

:- pred hlds_out__write_instmap_2(assoc_list(prog_var, inst)::in,
	prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

hlds_out__write_instmap_2([], _, _, _, !IO).
hlds_out__write_instmap_2([Var - Inst | Rest], VarSet, AppendVarnums, Indent,
		!IO) :-
	mercury_output_var(Var, VarSet, AppendVarnums, !IO),
	io__write_string(" -> ", !IO),
	varset__init(InstVarSet),
	mercury_output_inst(Inst, InstVarSet, !IO),
	( Rest = [] ->
		true
	;
		mercury_output_newline(Indent, !IO),
		io__write_string("%            ", !IO),
		hlds_out__write_instmap_2(Rest, VarSet, AppendVarnums, Indent,
			!IO)
	).

:- pred hlds_out__write_instmap_delta(instmap_delta::in, prog_varset::in,
	bool::in, int::in, io::di, io::uo) is det.

hlds_out__write_instmap_delta(InstMapDelta, VarSet, AppendVarnums, Indent,
		!IO) :-
	( instmap_delta_is_unreachable(InstMapDelta) ->
		io__write_string("unreachable", !IO)
	;
		instmap_delta_to_assoc_list(InstMapDelta, AssocList),
		hlds_out__write_instmap_2(AssocList, VarSet, AppendVarnums,
			Indent, !IO)
	).

:- pred hlds_out__write_instmap_delta_vars(instmap_delta::in, prog_varset::in,
	bool::in, io__state::di, io__state::uo) is det.

hlds_out__write_instmap_delta_vars(InstMapDelta, VarSet, AppendVarnums, !IO) :-
	( instmap_delta_is_unreachable(InstMapDelta) ->
		io__write_string("unreachable", !IO)
	;
		instmap_delta_to_assoc_list(InstMapDelta, AssocList),
		assoc_list__keys(AssocList, Vars),
		hlds_out__write_vars(Vars, VarSet, AppendVarnums, !IO)
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
hlds_out__write_import_status(imported(ancestor_private_interface)) -->
	io__write_string("imported from an ancestor's private interface").
hlds_out__write_import_status(imported(ancestor)) -->
	io__write_string("imported by an ancestor").
hlds_out__write_import_status(external(Status)) -->
	io__write_string("external (and "),
	hlds_out__write_import_status(Status),
	io__write_string(")").
hlds_out__write_import_status(abstract_imported) -->
	io__write_string("abstract_imported").
hlds_out__write_import_status(opt_imported) -->
	io__write_string("opt_imported").
hlds_out__write_import_status(pseudo_imported) -->
	io__write_string("pseudo_imported").
hlds_out__write_import_status(exported_to_submodules) -->
	io__write_string("exported_to_submodules").

:- pred hlds_out__write_type_list(list(type)::in, tvarset::in, bool::in,
		io::di, io::uo) is det.
hlds_out__write_type_list(Types, TypeVarSet, AppendVarNums) -->
	list__foldl((pred(Type::in, di, uo) is det -->
		mercury_output_term(Type, TypeVarSet, AppendVarNums),
		io__write_string(", ")),
		Types).

:- pred hlds_out__write_var_types(int::in, prog_varset::in, bool::in,
	vartypes::in, tvarset::in, io::di, io::uo) is det.

hlds_out__write_var_types(Indent, VarSet, AppendVarnums, VarTypes, TVarSet,
		!IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% variable types map:\n", !IO),
	map__keys(VarTypes, Vars),
	hlds_out__write_var_types_2(Vars, Indent, VarSet, AppendVarnums,
		VarTypes, TVarSet, !IO).

:- pred hlds_out__write_var_types_2(list(prog_var)::in, int::in,
	prog_varset::in, bool::in, vartypes::in, tvarset::in, io::di, io::uo)
	is det.

hlds_out__write_var_types_2([], _, _, _, _, _, !IO).
hlds_out__write_var_types_2([Var | Vars], Indent, VarSet, AppendVarnums,
		VarTypes, TypeVarSet, !IO) :-
	map__lookup(VarTypes, Var, Type),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_var(Var, VarSet, AppendVarnums, !IO),
	io__write_string(" (number ", !IO),
	term__var_to_int(Var, VarNum),
	io__write_int(VarNum, !IO),
	io__write_string(")", !IO),
	io__write_string(": ", !IO),
	mercury_output_term(Type, TypeVarSet, AppendVarnums, !IO),
	io__write_string("\n", !IO),
	hlds_out__write_var_types_2(Vars, Indent, VarSet, AppendVarnums,
		VarTypes, TypeVarSet, !IO).

:- pred hlds_out__write_typeinfo_varmap(int::in, bool::in,
	type_info_varmap::in, prog_varset::in, tvarset::in, io::di, io::uo)
	is det.

hlds_out__write_typeinfo_varmap(Indent, AppendVarnums, TypeInfoMap, VarSet,
		TVarSet, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% type_info varmap:\n", !IO),
	map__keys(TypeInfoMap, TypeVars),
	hlds_out__write_typeinfo_varmap_2(TypeVars, Indent, AppendVarnums,
		TypeInfoMap, VarSet, TVarSet, !IO).

:- pred hlds_out__write_typeinfo_varmap_2(list(tvar)::in, int::in, bool::in,
	type_info_varmap::in, prog_varset::in, tvarset::in,
	io::di, io::uo) is det.

hlds_out__write_typeinfo_varmap_2([], _, _, _, _, _, !IO).
hlds_out__write_typeinfo_varmap_2([TVar | TVars], Indent, AppendVarnums,
		TypeInfoMap, VarSet, TVarSet, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),

	mercury_output_var(TVar, TVarSet, AppendVarnums, !IO),
	io__write_string(" (number ", !IO),
	term__var_to_int(TVar, TVarNum),
	io__write_int(TVarNum, !IO),
	io__write_string(")", !IO),

	io__write_string(" -> ", !IO),
	map__lookup(TypeInfoMap, TVar, Locn),
	(
		Locn = type_info(Var),
		io__write_string("type_info(", !IO),
		mercury_output_var(Var, VarSet, AppendVarnums, !IO),
		io__write_string(") ", !IO)
	;
		Locn = typeclass_info(Var, Index),
		io__write_string("typeclass_info(", !IO),
		mercury_output_var(Var, VarSet, AppendVarnums, !IO),
		io__write_string(", ", !IO),
		io__write_int(Index, !IO),
		io__write_string(") ", !IO)
	),
	io__write_string(" (number ", !IO),
	term__var_to_int(Var, VarNum),
	io__write_int(VarNum, !IO),
	io__write_string(")", !IO),
	io__write_string("\n", !IO),

	hlds_out__write_typeinfo_varmap_2(TVars, Indent, AppendVarnums,
		TypeInfoMap, VarSet, TVarSet, !IO).

:- pred hlds_out__write_typeclass_info_varmap(int::in, bool::in,
	typeclass_info_varmap::in, prog_varset::in, tvarset::in,
	io::di, io::uo) is det.

hlds_out__write_typeclass_info_varmap(Indent, AppendVarnums,
		TypeClassInfoVarMap, VarSet, TVarSet, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% typeclass_info varmap:\n", !IO),
	map__foldl(hlds_out__write_typeclass_info_varmap_2(Indent,
		AppendVarnums, VarSet, TVarSet), TypeClassInfoVarMap, !IO).

:- pred hlds_out__write_typeclass_info_varmap_2(int::in, bool::in,
	prog_varset::in, tvarset::in, class_constraint::in, prog_var::in,
	io::di, io::uo) is det.

hlds_out__write_typeclass_info_varmap_2(Indent, AppendVarnums, VarSet, TVarSet,
		Constraint, Var, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_constraint(TVarSet, AppendVarnums, Constraint, !IO),
	io__write_string(" -> ", !IO),
	mercury_output_var(Var, VarSet, AppendVarnums, !IO),
	io__nl(!IO).

:- pred hlds_out__write_stack_slots(int::in, stack_slots::in, prog_varset::in,
	bool::in, io::di, io::uo) is det.

hlds_out__write_stack_slots(Indent, StackSlots, VarSet, AppendVarnums, !IO) :-
	map__to_assoc_list(StackSlots, VarSlotList),
	hlds_out__write_var_to_lvals(VarSlotList, VarSet, AppendVarnums,
		Indent, !IO).

:- pred hlds_out__write_var_to_lvals(assoc_list(prog_var, lval)::in,
	prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

hlds_out__write_var_to_lvals([], _, _, _, !IO).
hlds_out__write_var_to_lvals([Var - Loc | VarLocs], VarSet, AppendVarnums,
		Indent, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%\t", !IO),
	mercury_output_var(Var, VarSet, AppendVarnums, !IO),
	io__write_string("\t-> ", !IO),
	( llds_out__lval_to_string(Loc, LocStrPrime) ->
		LocStr = LocStrPrime
	;
		LocStr = "unknown location"
	),
	io__write_string(LocStr, !IO),
	io__write_string("\n", !IO),
	hlds_out__write_var_to_lvals(VarLocs, VarSet, AppendVarnums, Indent,
		!IO).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_types(int::in, type_table::in, io::di, io::uo) is det.

hlds_out__write_types(Indent, TypeTable, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%-------- Types --------\n", !IO),
	map__to_assoc_list(TypeTable, TypeAssocList),
	hlds_out__write_types_2(Indent, TypeAssocList, !IO).

:- pred hlds_out__write_types_2(int::in,
	assoc_list(type_ctor, hlds_type_defn)::in,
	io::di, io::uo) is det.

hlds_out__write_types_2(_Indent, [], !IO).
hlds_out__write_types_2(Indent, [TypeCtor - TypeDefn | Types], !IO) :-
	hlds_data__get_type_defn_tvarset(TypeDefn, TVarSet),
	hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	hlds_data__get_type_defn_status(TypeDefn, Status),
	hlds_data__get_type_defn_context(TypeDefn, Context),

	% Write the context

	io__write_char('\n', !IO),
	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'c') ->
		term__context_file(Context, FileName),
		term__context_line(Context, LineNumber),
		( FileName \= "" ->
			hlds_out__write_indent(Indent, !IO),
			io__write_string("% context: file `", !IO),
			io__write_string(FileName, !IO),
			io__write_string("', line ", !IO),
			io__write_int(LineNumber, !IO),
			io__write_string(", status ", !IO),
			hlds_out__write_import_status(Status, !IO),
			io__write_char('\n', !IO)
		;
			true
		)
	;
		true
	),

	hlds_out__write_indent(Indent, !IO),
	(
		( TypeBody ^ du_type_is_solver_type = solver_type
		; TypeBody = abstract_type(solver_type)
		; TypeBody = foreign_type(_, solver_type)
		)
	->
		io__write_string(":- solver type ", !IO)
	;
		io__write_string(":- type ", !IO)
	),
	hlds_out__write_type_name(TypeCtor, !IO),
	hlds_out__write_type_params(TVarSet, TypeParams, !IO),
	hlds_out__write_type_body(Indent + 1, TVarSet, TypeBody, !IO),
	hlds_out__write_types_2(Indent, Types, !IO).

:- pred hlds_out__write_type_name(type_ctor::in, io::di, io::uo) is det.

hlds_out__write_type_name(Name - _Arity, !IO) :-
	prog_out__write_sym_name(Name, !IO).

:- pred hlds_out__write_type_params(tvarset::in, list(type_param)::in,
	io::di, io::uo) is det.

hlds_out__write_type_params(_Tvarset, [], !IO).
hlds_out__write_type_params(Tvarset, [P], !IO) :-
	io__write_string("(", !IO),
	term_io__write_term(Tvarset, P, !IO),
	io__write_string(")", !IO).
hlds_out__write_type_params(Tvarset, [P | Ps], !IO) :-
	Ps = [_ | _],
	io__write_string("(", !IO),
	term_io__write_term(Tvarset, P, !IO),
	hlds_out__write_type_params_2(Tvarset, Ps, !IO).

:- pred hlds_out__write_type_params_2(tvarset::in, list(type_param)::in,
	io::di, io::uo) is det.

hlds_out__write_type_params_2(_Tvarset, [], !IO) :-
	io__write_string(")", !IO).
hlds_out__write_type_params_2(Tvarset, [P | Ps], !IO) :-
	io__write_string(", ", !IO),
	term_io__write_term(Tvarset, P, !IO),
	hlds_out__write_type_params_2(Tvarset, Ps, !IO).

:- pred hlds_out__write_type_body(int::in, tvarset::in, hlds_type_body::in,
	io::di, io::uo) is det.

hlds_out__write_type_body(Indent, Tvarset, du_type(Ctors, Tags, Enum,
		MaybeEqualityPred, ReservedTag, _IsSolverType, Foreign),
		!IO) :-
	io__write_string(" --->\n", !IO),
	(
		Enum = yes,
		hlds_out__write_indent(Indent, !IO),
		io__write_string("/* enumeration */\n", !IO)
	;
		Enum = no
	),
	(
		ReservedTag = yes,
		hlds_out__write_indent(Indent, !IO),
		io__write_string("/* reserved_tag */\n", !IO)
	;
		ReservedTag = no
	),
	hlds_out__write_constructors(Indent, Tvarset, Ctors, Tags, !IO),
	( MaybeEqualityPred = yes(unify_compare(MaybeEq, MaybeCompare)) ->
		io__write_string("\n", !IO),
		hlds_out__write_indent(Indent + 1, !IO),
		io__write_string("where ", !IO),
		(
			MaybeEq = yes(Eq),
			io__write_string("equality is ", !IO),
			prog_out__write_sym_name(Eq, !IO),
			( MaybeCompare = yes(_) ->
				io__write_string(", ", !IO)
			;
				true
			)
		;
			MaybeEq = no
		),
		(
			MaybeCompare = yes(Compare),
			io__write_string("comparison is ", !IO),
			prog_out__write_sym_name(Compare, !IO)
		;
			MaybeCompare = no
		)
	;
		true
	),
	(
		Foreign = yes(_),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("/* has foreign_type */\n", !IO)
	;
		Foreign = no
	),
	io__write_string(".\n", !IO).

hlds_out__write_type_body(_Indent, Tvarset, eqv_type(Type), !IO) :-
	io__write_string(" == ", !IO),
	term_io__write_term(Tvarset, Type, !IO),
	io__write_string(".\n", !IO).

hlds_out__write_type_body(_Indent, _Tvarset, abstract_type(_IsSolverType),
		!IO) :-
	io__write_string(".\n", !IO).

hlds_out__write_type_body(_Indent, _Tvarset, foreign_type(_, _), !IO) :-
	% XXX
	io__write_string(" == $foreign_type.\n", !IO).

:- pred hlds_out__write_constructors(int::in, tvarset::in,
	list(constructor)::in, cons_tag_values::in, io::di, io::uo) is det.

hlds_out__write_constructors(_Indent, _Tvarset, [], _, !IO) :-
	error("hlds_out__write_constructors: empty constructor list?").
hlds_out__write_constructors(Indent, Tvarset, [C], TagValues, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_char('\t', !IO),
	hlds_out__write_ctor(C, Tvarset, TagValues, !IO).
hlds_out__write_constructors(Indent, Tvarset, [C | Cs], TagValues, !IO) :-
	Cs = [_ | _],
	hlds_out__write_indent(Indent, !IO),
	io__write_char('\t', !IO),
	hlds_out__write_ctor(C, Tvarset, TagValues, !IO),
	io__write_string("\n", !IO),
	hlds_out__write_constructors_2(Indent, Tvarset, Cs, TagValues, !IO).

:- pred hlds_out__write_constructors_2(int::in, tvarset::in,
	list(constructor)::in, cons_tag_values::in, io::di, io::uo) is det.

hlds_out__write_constructors_2(_Indent, _Tvarset, [], _, !IO).
hlds_out__write_constructors_2(Indent, Tvarset, [C | Cs], TagValues, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string(";\t", !IO),
	hlds_out__write_ctor(C, Tvarset, TagValues, !IO),
	( Cs = [] ->
		true
	;
		io__write_string("\n", !IO),
		hlds_out__write_constructors_2(Indent, Tvarset, Cs, TagValues,
			!IO)
	).

:- pred hlds_out__write_ctor(constructor::in, tvarset::in,
	cons_tag_values::in, io::di, io::uo) is det.

hlds_out__write_ctor(C, Tvarset, TagValues, !IO) :-
	mercury_output_ctor(C, Tvarset, !IO),
	C = ctor(_, _, Name, Args),
	ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
	( map__search(TagValues, ConsId, TagValue) ->
		io__write_string("\t% tag: ", !IO),
		io__print(TagValue, !IO)
	;
		true
	).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_classes(int::in, class_table::in, io::di, io::uo)
	is det.

hlds_out__write_classes(Indent, ClassTable, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%-------- Classes --------\n", !IO),
	map__to_assoc_list(ClassTable, ClassTableList),
	io__write_list(ClassTableList, "\n",
		hlds_out__write_class_defn(Indent), !IO),
	io__nl(!IO).

:- pred hlds_out__write_class_defn(int::in,
	pair(class_id, hlds_class_defn)::in, io::di, io::uo) is det.

hlds_out__write_class_defn(Indent, ClassId - ClassDefn, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),

	hlds_out__write_class_id(ClassId, !IO),
	io__write_string(":\n", !IO),

	ClassDefn = hlds_class_defn(_, Constraints, Vars, _, Interface,
		VarSet, Context),

	term__context_file(Context, FileName),
	term__context_line(Context, LineNumber),
	( FileName \= "" ->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% context: file `", !IO),
		io__write_string(FileName, !IO),
		io__write_string("', line ", !IO),
		io__write_int(LineNumber, !IO),
		io__write_string("\n", !IO)
	;
		true
	),

	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'v') ->
		AppendVarnums = yes
	;
		AppendVarnums = no
	),

	hlds_out__write_indent(Indent, !IO),
	io__write_string("% Vars: ", !IO),
	mercury_output_vars(Vars, VarSet, AppendVarnums, !IO),
	io__nl(!IO),

	hlds_out__write_indent(Indent, !IO),
	io__write_string("% Constraints: ", !IO),
	io__write_list(Constraints, ", ",
		mercury_output_constraint(VarSet, AppendVarnums), !IO),
	io__nl(!IO),

	hlds_out__write_indent(Indent, !IO),
	io__write_string("% Class Methods: ", !IO),
	io__write_list(Interface, ", ", hlds_out__write_class_proc, !IO),
	io__nl(!IO).

	% Just output the class methods as pred_ids and proc_ids because
	% its probably not that useful to have the names. If that information
	% is needed, it shouldn't be a very difficult fix.

:- pred hlds_out__write_class_proc(hlds_class_proc::in, io::di, io::uo) is det.

hlds_out__write_class_proc(hlds_class_proc(PredId, ProcId), !IO) :-
	io__write_string("hlds_class_proc(pred_id:", !IO),
	pred_id_to_int(PredId, PredInt),
	io__write_int(PredInt, !IO),
	io__write_string(", proc_id:", !IO),
	proc_id_to_int(ProcId, ProcInt),
	io__write_int(ProcInt, !IO),
	io__write_char(')', !IO).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_superclasses(int::in, superclass_table::in,
	io::di, io::uo) is det.

hlds_out__write_superclasses(Indent, SuperClassTable, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%-------- Super Classes --------\n", !IO),
	multi_map__to_assoc_list(SuperClassTable, SuperClassTableList),
	io__write_list(SuperClassTableList, "\n\n",
		hlds_out__write_superclass(Indent), !IO),
	io__nl(!IO).

:- pred hlds_out__write_superclass(int::in,
	pair(class_id, list(subclass_details))::in, io::di, io::uo) is det.

hlds_out__write_superclass(Indent, ClassId - SubClassDetailsList, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	hlds_out__write_class_id(ClassId, !IO),
	io__write_string(":\n", !IO),

	io__write_list(SubClassDetailsList, "\n",
		hlds_out__write_subclass_details(Indent, ClassId), !IO).

:- pred hlds_out__write_subclass_details(int::in, class_id::in,
	subclass_details::in, io::di, io::uo) is det.

hlds_out__write_subclass_details(Indent, SuperClassId, SubClassDetails, !IO) :-
	SubClassDetails = subclass_details(SuperClassVars, SubClassId,
		SubClassVars, VarSet),

		% curry the varset for term_io__write_variable/4
	PrintVar = (pred(VarName::in, IO0::di, IO::uo) is det :-
			term_io__write_variable(VarName, VarSet, IO0, IO)
		),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	SubClassId = class_id(SubSymName, _SubArity),
	prog_out__write_sym_name(SubSymName, !IO),
	io__write_char('(', !IO),
	io__write_list(SubClassVars, ", ", PrintVar, !IO),
	io__write_string(") <= ", !IO),

	SuperClassId = class_id(SuperSymName, _SuperArity),
	prog_out__write_sym_name(SuperSymName, !IO),
	io__write_char('(', !IO),
	io__write_list(SuperClassVars, ", ", term_io__write_term(VarSet), !IO),
	io__write_char(')', !IO).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_instances(int::in, instance_table::in,
	io::di, io::uo) is det.

hlds_out__write_instances(Indent, InstanceTable, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%-------- Instances --------\n", !IO),
	map__to_assoc_list(InstanceTable, InstanceTableList),
	io__write_list(InstanceTableList, "\n\n",
		hlds_out__write_instance_defns(Indent), !IO),
	io__nl(!IO).

:- pred hlds_out__write_instance_defns(int::in,
	pair(class_id, list(hlds_instance_defn))::in, io::di, io::uo) is det.

hlds_out__write_instance_defns(Indent, ClassId - InstanceDefns, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	hlds_out__write_class_id(ClassId, !IO),
	io__write_string(":\n", !IO),
	io__write_list(InstanceDefns, "\n",
		hlds_out__write_instance_defn(Indent), !IO).

:- pred hlds_out__write_instance_defn(int::in, hlds_instance_defn::in,
	io::di, io::uo) is det.

hlds_out__write_instance_defn(Indent, InstanceDefn, !IO) :-
	InstanceDefn = hlds_instance_defn(_InstanceModule, _Status,
		Context, Constraints, Types, Body,
		MaybePredProcIds, VarSet, Proofs),

	term__context_file(Context, FileName),
	term__context_line(Context, LineNumber),
	( FileName \= "" ->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% context: file `", !IO),
		io__write_string(FileName, !IO),
		io__write_string("', line ", !IO),
		io__write_int(LineNumber, !IO),
		io__write_string("\n", !IO)
	;
		true
	),

	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 'v') ->
		AppendVarnums = yes
	;
		AppendVarnums = no
	),

		% curry the varset for term_io__write_variable/4
	PrintTerm = (pred(TypeName::in, IO0::di, IO::uo) is det :-
			mercury_output_term(TypeName, VarSet,
				AppendVarnums, IO0, IO)
		),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% Types: ", !IO),
	io__write_list(Types, ", ", PrintTerm, !IO),
	io__nl(!IO),

	hlds_out__write_indent(Indent, !IO),
	io__write_string("% Constraints: ", !IO),
	io__write_list(Constraints, ", ",
		mercury_output_constraint(VarSet, AppendVarnums), !IO),
	io__nl(!IO),

	hlds_out__write_indent(Indent, !IO),
	(
		Body = abstract,
		io__write_string("% abstract", !IO)
	;
		Body = concrete(Methods),
		io__write_string("% Instance Methods: ", !IO),
		mercury_output_instance_methods(Methods, !IO)
	),
	io__nl(!IO),

	(
		MaybePredProcIds = yes(PredProcIds),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% procedures: ", !IO),
		io__write(PredProcIds, !IO),
		io__nl(!IO)
	;
		MaybePredProcIds = no
	),

	hlds_out__write_constraint_proofs(Indent, VarSet, Proofs,
		AppendVarnums, !IO),
	io__nl(!IO).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_insts(int::in, inst_table::in, io::di, io::uo) is det.

hlds_out__write_insts(Indent, _InstTable, !IO) :-
		% XXX fix this up.
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%-------- Insts --------\n", !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%%% Not yet implemented, sorry.\n", !IO).
	% io__write_string("% ", !IO).
	% io__print(InstTable, !IO),
	% io__nl(!IO).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_modes(int::in, mode_table::in, io::di, io::uo) is det.

hlds_out__write_modes(Indent, _ModeTable, !IO) :-
		% XXX fix this up.
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%-------- Modes --------\n", !IO),
	hlds_out__write_indent(Indent, !IO),
	io__write_string("%%% Not yet implemented, sorry.\n", !IO).
	% io__write_string("% ", !IO),
	% io__print(ModeTable, !IO),
	% io__nl(!IO).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_procs(int::in, bool::in, module_info::in, pred_id::in,
	import_status::in, pred_info::in, io::di, io::uo) is det.

hlds_out__write_procs(Indent, AppendVarnums, ModuleInfo, PredId,
		ImportStatus, PredInfo, !IO) :-
	pred_info_procedures(PredInfo, ProcTable),
	ProcIds = pred_info_procids(PredInfo),
	hlds_out__write_procs_2(ProcIds, AppendVarnums, ModuleInfo, Indent,
		PredId, ImportStatus, ProcTable, !IO).

:- pred hlds_out__write_procs_2(list(proc_id)::in, bool::in, module_info::in,
	int::in, pred_id::in, import_status::in, proc_table::in,
	io::di, io::uo) is det.

hlds_out__write_procs_2([], _, _, _, _, _, _, !IO).
hlds_out__write_procs_2([ProcId | ProcIds], AppendVarnums, ModuleInfo, Indent,
		PredId, ImportStatus, ProcTable, !IO) :-
	map__lookup(ProcTable, ProcId, ProcInfo),
	hlds_out__write_proc(Indent, AppendVarnums, ModuleInfo, PredId, ProcId,
		ImportStatus, ProcInfo, !IO),
	hlds_out__write_procs_2(ProcIds, AppendVarnums, ModuleInfo, Indent,
		PredId, ImportStatus, ProcTable, !IO).

:- pred hlds_out__write_proc(int::in, bool::in, module_info::in, pred_id::in,
	proc_id::in, import_status::in, proc_info::in, io::di, io::uo) is det.

hlds_out__write_proc(Indent, AppendVarnums, ModuleInfo, PredId, ProcId,
		ImportStatus, Proc, !IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_typevarset(PredInfo, TVarSet),
	proc_info_vartypes(Proc, VarTypes),
	proc_info_declared_determinism(Proc, DeclaredDeterminism),
	proc_info_inferred_determinism(Proc, InferredDeterminism),
	proc_info_varset(Proc, VarSet),
	proc_info_headvars(Proc, HeadVars),
	proc_info_argmodes(Proc, HeadModes),
	proc_info_maybe_arglives(Proc, MaybeArgLives),
	proc_info_maybe_arg_info(Proc, MaybeArgInfos),
	proc_info_goal(Proc, Goal),
	proc_info_context(Proc, ModeContext),
	proc_info_get_maybe_arg_size_info(Proc, MaybeArgSize),
	proc_info_get_maybe_termination_info(Proc, MaybeTermination),
	proc_info_typeinfo_varmap(Proc, TypeInfoMap),
	proc_info_typeclass_info_varmap(Proc, TypeClassInfoMap),
	proc_info_eval_method(Proc, EvalMethod),
	proc_info_is_address_taken(Proc, IsAddressTaken),
	proc_info_get_call_table_tip(Proc, MaybeCallTableTip),
	proc_info_get_maybe_deep_profile_info(Proc, MaybeDeepProfileInfo),
	Indent1 = Indent + 1,

	hlds_out__write_indent(Indent1, !IO),
	io__write_string("% pred id ", !IO),
	pred_id_to_int(PredId, PredInt),
	io__write_int(PredInt, !IO),
	io__nl(!IO),
	hlds_out__write_indent(Indent1, !IO),
	io__write_string("% mode number ", !IO),
	proc_id_to_int(ProcId, ProcInt),
	io__write_int(ProcInt, !IO),
	io__write_string(" of ", !IO),
	hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
	io__write_string(" (", !IO),
	hlds_out__write_determinism(InferredDeterminism, !IO),
	io__write_string("):\n", !IO),

	globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
	( string__contains_char(Verbose, 't') ->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% Arg size properties: ", !IO),
		termination__write_maybe_arg_size_info(MaybeArgSize, yes, !IO),
		io__nl(!IO),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% Termination properties: ", !IO),
		termination__write_maybe_termination_info(MaybeTermination,
			yes, !IO),
		io__nl(!IO)
	;
		true
	),

	hlds_out__write_indent(Indent, !IO),
	hlds_out__write_var_types(Indent, VarSet, AppendVarnums,
		VarTypes, TVarSet, !IO),
	hlds_out__write_typeinfo_varmap(Indent, AppendVarnums, TypeInfoMap,
		VarSet, TVarSet, !IO),
	hlds_out__write_typeclass_info_varmap(Indent, AppendVarnums,
		TypeClassInfoMap, VarSet, TVarSet, !IO),

	(
		IsAddressTaken = address_is_taken,
		io__write_string("% address is taken\n", !IO)
	;
		IsAddressTaken = address_is_not_taken,
		io__write_string("% address is not taken\n", !IO)
	),

	( EvalMethod = eval_normal ->
		true
	;
		io__write_string("% eval method: ", !IO),
		hlds_out__write_eval_method(EvalMethod, !IO),
		io__write_string("\n", !IO)
	),

	(
		MaybeCallTableTip = yes(CallTableTip),
		io__write_string("% call table tip: ", !IO),
		mercury_output_var(CallTableTip, VarSet, AppendVarnums, !IO),
		io__write_string("\n", !IO)
	;
		MaybeCallTableTip = no
	),

	(
		MaybeDeepProfileInfo = yes(DeepProfileInfo),
		DeepProfileInfo = deep_profile_proc_info(Role, _SCC),
		io__write_string("% deep profile info: ", !IO),
		(
			Role = inner_proc(DeepPredProcId),
			io__write_string("inner, outer is ", !IO)
		;
			Role = outer_proc(DeepPredProcId),
			io__write_string("outer, inner is ", !IO)
		),
		DeepPredProcId = proc(DeepPredId, DeepProcId),
		pred_id_to_int(DeepPredId, DeepPredInt),
		proc_id_to_int(DeepProcId, DeepProcInt),
		io__write_int(DeepPredInt, !IO),
		io__write_string("/", !IO),
		io__write_int(DeepProcInt, !IO),
		io__write_string("\n", !IO)
	;
		MaybeDeepProfileInfo = no
	),

	hlds_out__write_indent(Indent, !IO),
	predicate_name(ModuleInfo, PredId, PredName),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	varset__init(ModeVarSet),
	(
		PredOrFunc = predicate,
		mercury_output_pred_mode_decl(ModeVarSet,
			unqualified(PredName), HeadModes,
			DeclaredDeterminism, ModeContext, !IO)
	;
		PredOrFunc = function,
		pred_args_to_func_args(HeadModes, FuncHeadModes,
			RetHeadMode),
		mercury_output_func_mode_decl(ModeVarSet,
			unqualified(PredName), FuncHeadModes, RetHeadMode,
			DeclaredDeterminism, ModeContext, !IO)
	),

	(
		MaybeArgLives = yes(ArgLives),
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% arg lives: ", !IO),
		io__print(ArgLives, !IO),
		io__nl(!IO)
	;
		MaybeArgLives = no
	),

	(
		string__contains_char(Verbose, 'A'),
		MaybeArgInfos = yes(ArgInfos)
	->
		hlds_out__write_indent(Indent, !IO),
		io__write_string("% arg_infos: ", !IO),
		io__print(ArgInfos, !IO),
		io__nl(!IO)
	;
		true
	),

	(
		ImportStatus = pseudo_imported,
		hlds_pred__in_in_unification_proc_id(ProcId)
	->
		true
	;
		proc_info_stack_slots(Proc, StackSlots),
		hlds_out__write_indent(Indent, !IO),
		hlds_out__write_stack_slots(Indent, StackSlots, VarSet,
			AppendVarnums, !IO),
		hlds_out__write_indent(Indent, !IO),
		term__var_list_to_term_list(HeadVars, HeadTerms),
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
			AppendVarnums, HeadTerms, PredOrFunc, !IO),
		io__write_string(" :-\n", !IO),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, AppendVarnums,
			Indent1, ".\n", !IO)
	).

% :- pred hlds_out__write_varnames(int, map(var, string), io__state, io__state).
% :- mode hlds_out__write_varnames(in, in, di, uo) is det.
%
% hlds_out__write_varnames(Indent, VarNames) -->
%	{ map__to_assoc_list(VarNames, VarNameList) },
%	(
%		{ VarNameList = [] }
%	->
%		hlds_out__write_indent(Indent),
%		io__write_string("[]\n")
%	;
%		hlds_out__write_indent(Indent),
%		io__write_string("[\n"),
%		{Indent1 = Indent + 1},
%		hlds_out__write_varnames_2(Indent1, VarNameList),
%		hlds_out__write_indent(Indent),
%		io__write_string("]\n")
%	).
%
% :- pred hlds_out__write_varnames_2(int, list(pair(var, string)),
%	io__state, io__state).
% :- mode hlds_out__write_varnames_2(in, in, di, uo) is det.
%
% hlds_out__write_varnames_2(Indent, VarNameList0) -->
%	(
%		{ VarNameList0 = [VarId - Name|VarNameList] }
%	->
%		{ Indent1 = Indent + 1 },
%		hlds_out__write_indent(Indent1),
%		{ term__var_to_int(VarId, VarNum) },
%		io__write_int(VarNum),
%		io__write_string(" - "),
%		io__write_string(Name),
%		io__write_string("\n"),
%		( { VarNameList = [] } ->
%			[]
%		;
%			io__write_string(",\n"),
%			hlds_out__write_varnames_2(Indent, VarNameList)
%		)
%	;
%		{ error("This cannot happen") }
%	).

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
hlds_out__write_eval_method(eval_table_io(IsDecl, IsUnitize)) -->
	io__write_string("table_io("),
	(
		{ IsDecl = table_io_decl },
		io__write_string("decl, ")
	;
		{ IsDecl = table_io_proc },
		io__write_string("proc, ")
	),
	(
		{ IsUnitize = table_io_unitize },
		io__write_string("unitize")
	;
		{ IsUnitize = table_io_alone },
		io__write_string("alone")
	),
	io__write_string(")").

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_indent(int::in, io::di, io::uo) is det.

hlds_out__write_indent(Indent, !IO) :-
	( Indent = 0 ->
		true
	;
		io__write_char('\t', !IO),
		hlds_out__write_indent(Indent - 1, !IO)
	).

:- pred hlds_out__write_intlist(list(int)::in, io::di, io::uo) is det.

hlds_out__write_intlist(IntList, !IO) :-
	(
		IntList = [],
		io__write_string("[]", !IO)
	;
		IntList = [H | T],
		io__write_string("[ ", !IO),
		hlds_out__write_intlist_2(H, T, !IO),
		io__write_string("]", !IO)
	).

:- pred hlds_out__write_intlist_2(int::in, list(int)::in, io::di, io::uo)
	is det.

hlds_out__write_intlist_2(H, T, !IO) :-
	io__write_int(H, !IO),
	(
		T = [TH | TT],
		io__write_string(", ", !IO),
		hlds_out__write_intlist_2(TH, TT, !IO)
	;
		T = []
	).

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_constraint_proofs(int::in, tvarset::in,
	map(class_constraint, constraint_proof)::in, bool::in,
	io::di, io::uo) is det.

hlds_out__write_constraint_proofs(Indent, VarSet, Proofs, AppendVarnums,
		!IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% Proofs: \n", !IO),
	map__to_assoc_list(Proofs, ProofsList),
	io__write_list(ProofsList, "\n",
		hlds_out__write_constraint_proof(Indent,
			VarSet, AppendVarnums), !IO).

:- pred hlds_out__write_constraint_proof(int::in, tvarset::in, bool::in,
	pair(class_constraint, constraint_proof)::in, io::di, io::uo) is det.

hlds_out__write_constraint_proof(Indent, VarSet, AppendVarnums,
		Constraint - Proof, !IO) :-
	hlds_out__write_indent(Indent, !IO),
	io__write_string("% ", !IO),
	mercury_output_constraint(VarSet, AppendVarnums, Constraint, !IO),
	io__write_string(": ", !IO),
	(
		Proof = apply_instance(Num),
		io__write_string("apply instance decl #", !IO),
		io__write_int(Num, !IO)
	;
		Proof = superclass(Super),
		io__write_string("super class of ", !IO),
		mercury_output_constraint(VarSet, AppendVarnums, Super, !IO)
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
		GroundInstInfo = none,
		Term = make_atom(inst_uniqueness(Uniq, "ground"), Context)
	).
inst_to_term(inst_var(Var), _) =
	term__coerce(term__variable(Var)).
inst_to_term(constrained_inst_vars(Vars, Inst), Context) =
	set__fold(func(Var, Term) =
			term__functor(term__atom("=<"),
				[term__coerce(term__variable(Var)), Term],
				Context),
		Vars, inst_to_term(Inst, Context)).
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
