%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Check for any possible undefined types.
	% Should we add a definition for undefined types?

:- module undef_types.
:- interface.
:- import_module hlds, io.

:- pred check_undefined_types(module_info, module_info, io__state, io__state).
:- mode check_undefined_types(in, out, di, uo) is det.

:- implementation.
:- import_module std_util, map, list, term, varset.
:- import_module globals, options.
:- import_module prog_out, prog_util, hlds_out, type_util, mercury_to_mercury.

check_undefined_types(Module, Module) -->
	{ module_info_types(Module, TypeDefns) },
	{ map__keys(TypeDefns, TypeIds) },
	find_undef_type_bodies(TypeIds, TypeDefns),
	globals__lookup_option(statistics, bool(Statistics)),
	maybe_report_stats(Statistics),
	{ module_info_preds(Module, Preds) },
	{ module_info_predids(Module, PredIds) },
	find_undef_pred_types(PredIds, Preds, TypeDefns).

	% Find any undefined types used in `:- pred' declarations.

:- pred find_undef_pred_types(list(pred_id), pred_table, type_table,
				io__state, io__state).
:- mode find_undef_pred_types(in, in, in, di, uo) is det.

find_undef_pred_types([], _Preds, _TypeDefns) --> [].
find_undef_pred_types([PredId | PredIds], Preds, TypeDefns) -->
	{ map__lookup(Preds, PredId, PredDefn) },
	{ pred_info_arg_types(PredDefn, _VarSet, ArgTypes) },
	{ pred_info_context(PredDefn, Context) },
	find_undef_type_list(ArgTypes, pred(PredId) - Context, TypeDefns),
	find_undef_pred_types(PredIds, Preds, TypeDefns).

	% Find any undefined types used in the bodies of other type
	% declarations.

:- pred find_undef_type_bodies(list(type_id), type_table, io__state, io__state).
:- mode find_undef_type_bodies(in, in, di, uo) is det.

find_undef_type_bodies([], _) --> [].
find_undef_type_bodies([TypeId | TypeIds], TypeDefns) -->
	{ map__lookup(TypeDefns, TypeId, HLDS_TypeDefn) },
		% XXX abstract hlds__type_defn/5
	{ HLDS_TypeDefn = hlds__type_defn(_, _, TypeBody, _, Context) },
	find_undef_type_body(TypeBody, type(TypeId) - Context, TypeDefns),
	find_undef_type_bodies(TypeIds, TypeDefns).

	% Find any undefined types used in the given type definition.

:- pred find_undef_type_body(hlds__type_body, error_context, type_table,
				io__state, io__state).
:- mode find_undef_type_body(in, in, in, di, uo) is det.

find_undef_type_body(eqv_type(Type), ErrorContext, TypeDefns) -->
	find_undef_type(Type, ErrorContext, TypeDefns).
find_undef_type_body(uu_type(Types), ErrorContext, TypeDefns) -->
	find_undef_type_list(Types, ErrorContext, TypeDefns).
find_undef_type_body(du_type(Constructors, _, _), ErrorContext, TypeDefns) -->
	find_undef_type_du_body(Constructors, ErrorContext, TypeDefns).
find_undef_type_body(abstract_type, _ErrorContext, _TypeDefns) --> [].

	% Find any undefined types in a list of types.

:- pred find_undef_type_list(list(type), error_context, type_table,
				io__state, io__state).
:- mode find_undef_type_list(in, in, in, di, uo) is det.

find_undef_type_list([], _ErrorContext, _TypeDefns) --> [].
find_undef_type_list([Type|Types], ErrorContext, TypeDefns) -->
	find_undef_type(Type, ErrorContext, TypeDefns),
	find_undef_type_list(Types, ErrorContext, TypeDefns).

	% Find any undefined types in a list of contructors
	% (the constructors for a discrimiated union type).

:- pred find_undef_type_du_body(list(constructor), error_context, type_table,
				io__state, io__state).
:- mode find_undef_type_du_body(in, in, in, di, uo) is det.

find_undef_type_du_body([], _ErrorContext, _TypeDefns) --> [].
find_undef_type_du_body([Constructor | Constructors], ErrorContext,
		TypeDefns) -->
	{ Constructor = _Functor - ArgTypes },
	find_undef_type_list(ArgTypes, ErrorContext, TypeDefns),
	find_undef_type_du_body(Constructors, ErrorContext, TypeDefns).

	% Find any undefined types used in type.
	% The type itself may be undefined, and also
	% any type arguments may also be undefined.
	% (eg. the type `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_type(type, error_context, type_table,
				io__state, io__state).
:- mode find_undef_type(in, in, in, di, uo) is det.

find_undef_type(term__variable(_), _ErrorContext, _TypeDefns) --> [].
find_undef_type(term__functor(F, As, C), ErrorContext, TypeDefns) -->
	{ list__length(As, Arity) },
	(
		{ make_type_id(F, Arity, TypeId) }
	->
		(
			{ is_builtin_atomic_type(TypeId) }
		->
			[]
		;
			{ map__contains(TypeDefns, TypeId) }
		->
			[]
		;
			{ is_builtin_pred_type(TypeId) }
		->
			[]
		;
			report_undef_type(TypeId, ErrorContext)
		)
	;
		report_invalid_type(term__functor(F, As, C), ErrorContext)
	),
	find_undef_type_list(As, ErrorContext, TypeDefns).

%-----------------------------------------------------------------------------%

:- type error_context == pair(error_context_2, term__context).
:- type error_context_2 ---> type(type_id) ; pred(pred_id).

	% Output an error message about an ill-formed type
	% in the specified context.

:- pred report_invalid_type(term, error_context, io__state, io__state).
:- mode report_invalid_type(in, in, di, uo) is det.

report_invalid_type(Type, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In definition of "),
	write_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: ill-formed type `"),
	{ varset__init(VarSet) },
	mercury_output_term(Type, VarSet),
	io__write_string("'.\n").

	% Output an error message about an undefined type
	% in the specified context.

:- pred report_undef_type(type_id, error_context, io__state, io__state).
:- mode report_undef_type(in, in, di, uo) is det.

report_undef_type(TypeId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In definition of "),
	write_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: undefined type "),
	hlds_out__write_type_id(TypeId),
	io__write_string(".\n").

	% Output a description of the context where an undefined type was
	% used.

:- pred write_error_context(error_context_2, io__state, io__state).
:- mode write_error_context(in, di, uo) is det.

write_error_context(pred(_PredId)) -->
	io__write_string("predicate").
	% XXX hlds_out__write_pred_id(PredId).
write_error_context(type(TypeId)) -->
	io__write_string("type "),
	hlds_out__write_type_id(TypeId).

%-----------------------------------------------------------------------------%

	% is_builtin_atomic_type(TypeId)
	%	is true iff 'TypeId' is the type_id of a builtin atomic type

:- pred is_builtin_atomic_type(type_id).
:- mode is_builtin_atomic_type(in) is semidet.

is_builtin_atomic_type(QualifiedName - 0) :-
	unqualify_name(QualifiedName, Name),
	is_builtin_atomic_type_2(Name).

:- pred is_builtin_atomic_type_2(string).
:- mode is_builtin_atomic_type_2(in) is semidet.

:- is_builtin_atomic_type_2([]) when ever.
:- is_builtin_atomic_type_2([X|_]) when X.

is_builtin_atomic_type_2("int").
is_builtin_atomic_type_2("float").
is_builtin_atomic_type_2("string").
is_builtin_atomic_type_2("character").

	% is_builtin_pred_type(TypeId)
	%	is true iff 'TypeId' is the type_id of a builtin higher-order
	%	predicate type.

:- pred is_builtin_pred_type(type_id).
:- mode is_builtin_pred_type(in) is semidet.

is_builtin_pred_type(QualifiedName - _Arity) :-
	unqualify_name(QualifiedName, Name),
	Name = "pred".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
