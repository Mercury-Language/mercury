%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module globals.

% Main author: fjh.

% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the io__state.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module bool, getopt, list.
:- import_module options.

:- type globals.

:- type gc_method
	--->	none
	;	conservative
	;	accurate.

:- type tags_method
	--->	none
	;	low
	;	high.

:- type args_method
	--->	simple
	;	compact.

	% Once upon a time, there were more type_info options than this.

:- type type_info_method
	--->	shared_one_or_two_cell.

:- type prolog_dialect
	--->	default
	;	nu
	;	sicstus.

:- pred convert_gc_method(string::in, gc_method::out) is semidet.

:- pred convert_tags_method(string::in, tags_method::out) is semidet.

:- pred convert_args_method(string::in, args_method::out) is semidet.

:- pred convert_type_info_method(string::in, type_info_method::out) is semidet.

:- pred convert_prolog_dialect(string::in, prolog_dialect::out) is semidet.

%-----------------------------------------------------------------------------%

	% Access predicates for the `globals' structure.

:- pred globals__init(option_table::di, gc_method::di, tags_method::di,
	args_method::di, type_info_method::di, prolog_dialect::di, globals::uo)
	is det.

:- pred globals__get_options(globals::in, option_table::out) is det.
:- pred globals__get_gc_method(globals::in, gc_method::out) is det.
:- pred globals__get_tags_method(globals::in, tags_method::out) is det.
:- pred globals__get_args_method(globals::in, args_method::out) is det.
:- pred globals__get_type_info_method(globals::in, type_info_method::out)
	is det.
:- pred globals__get_prolog_dialect(globals::in, prolog_dialect::out) is det.

:- pred globals__set_options(globals::in, option_table::in, globals::out)
	is det.
:- pred globals__set_gc_method(globals::in, gc_method::in, globals::out)
	is det.
:- pred globals__set_tags_method(globals::in, tags_method::in, globals::out)
	is det.
:- pred globals__set_args_method(globals::in, args_method::in, globals::out)
	is det.
:- pred globals__set_type_info_method(globals::in, type_info_method::in,
	globals::out) is det.
:- pred globals__set_prolog_dialect(globals::in, prolog_dialect::in,
	globals::out) is det.

:- pred globals__lookup_option(globals::in, option::in, option_data::out)
	is det.

:- pred globals__lookup_bool_option(globals, option, bool).
:- mode globals__lookup_bool_option(in, in, out) is det.
:- mode globals__lookup_bool_option(in, in, in) is semidet. % implied
:- pred globals__lookup_int_option(globals::in, option::in, int::out) is det.
:- pred globals__lookup_string_option(globals::in, option::in, string::out)
	is det.
:- pred globals__lookup_accumulating_option(globals::in, option::in,
	list(string)::out) is det.

%-----------------------------------------------------------------------------%

	% More complex options

	% Check if static code addresses are available in the
	% current grade of compilation

:- pred globals__have_static_code_addresses(globals::in, bool::out) is det.

%-----------------------------------------------------------------------------%

	% Access predicates for storing a `globals' structure in the
	% io__state using io__set_globals and io__get_globals.

:- pred globals__io_init(option_table::di, gc_method::in, tags_method::in,
	args_method::in, type_info_method::in, prolog_dialect::in,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_gc_method(gc_method::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_tags_method(tags_method::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_args_method(args_method::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_type_info_method(type_info_method::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_prolog_dialect(prolog_dialect::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_globals(globals::out, io__state::di, io__state::uo)
	is det.

:- pred globals__io_set_globals(globals::di, io__state::di, io__state::uo)
	is det.

:- pred globals__io_lookup_option(option::in, option_data::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_set_option(option::in, option_data::in,
	io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_bool_option(option, bool, io__state, io__state).
:- mode globals__io_lookup_bool_option(in, out, di, uo) is det.
:- mode globals__io_lookup_bool_option(in, in, di, uo) is semidet. % implied

:- pred globals__io_lookup_int_option(option::in, int::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_string_option(option::in, string::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_accumulating_option(option::in, list(string)::out,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exprn_aux.
:- import_module map, std_util, io, require.

%-----------------------------------------------------------------------------%

convert_gc_method("none", none).
convert_gc_method("conservative", conservative).
convert_gc_method("accurate", accurate).

convert_tags_method("none", none).
convert_tags_method("low", low).
convert_tags_method("high", high).

convert_args_method("simple", simple).
convert_args_method("compact", compact).

convert_type_info_method("shared-one-or-two-cell", shared_one_or_two_cell).
convert_type_info_method("default", shared_one_or_two_cell).

convert_prolog_dialect("default", default).
convert_prolog_dialect("nu", nu).
convert_prolog_dialect("NU", nu).
convert_prolog_dialect("nuprolog", nu).
convert_prolog_dialect("NUprolog", nu).
convert_prolog_dialect("nu-prolog", nu).
convert_prolog_dialect("NU-Prolog", nu).
convert_prolog_dialect("sicstus", sicstus).
convert_prolog_dialect("Sicstus", sicstus).
convert_prolog_dialect("SICStus", sicstus).
convert_prolog_dialect("sicstus-prolog", sicstus).
convert_prolog_dialect("Sicstus-Prolog", sicstus).
convert_prolog_dialect("SICStus-Prolog", sicstus).

%-----------------------------------------------------------------------------%

:- type globals
	--->	globals(
			option_table,
			gc_method,
			tags_method,
			args_method,
			type_info_method,
			prolog_dialect
		).

globals__init(Options, GC_Method, TagsMethod, ArgsMethod,
		TypeInfoMethod, PrologDialect,
	globals(Options, GC_Method, TagsMethod, ArgsMethod,
		TypeInfoMethod, PrologDialect)).

globals__get_options(globals(Options, _, _, _, _, _), Options).
globals__get_gc_method(globals(_, GC_Method, _, _, _, _), GC_Method).
globals__get_tags_method(globals(_, _, TagsMethod, _, _, _), TagsMethod).
globals__get_args_method(globals(_, _, _, ArgsMethod, _, _), ArgsMethod).
globals__get_type_info_method(globals(_, _, _, _, TypeInfoMethod, _),
	TypeInfoMethod).
globals__get_prolog_dialect(globals(_, _, _, _, _, PrologDialect),
	PrologDialect).

globals__set_options(globals(_, B, C, D, E, F), Options,
	globals(Options, B, C, D, E, F)).
globals__set_gc_method(globals(A, _, C, D, E, F), GC_Method,
	globals(A, GC_Method, C, D, E, F)).
globals__set_tags_method(globals(A, B, _, D, E, F), TagsMethod,
	globals(A, B, TagsMethod, D, E, F)).
globals__set_args_method(globals(A, B, C, _, E, F), ArgsMethod,
	globals(A, B, C, ArgsMethod, E, F)).
globals__set_type_info_method(globals(A, B, C, D, _, F), TypeInfoMethod,
	globals(A, B, C, D, TypeInfoMethod, F)).
globals__set_prolog_dialect(globals(A, B, C, D, E, _), PrologDialect,
	globals(A, B, C, D, E, PrologDialect)).

globals__lookup_option(Globals, Option, OptionData) :-
	globals__get_options(Globals, OptionTable),
	map__lookup(OptionTable, Option, OptionData).

%-----------------------------------------------------------------------------%

globals__lookup_bool_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = bool(Bool) ->
		Value = Bool
	;
		error("globals__lookup_bool_option: invalid bool option")
	).

globals__lookup_string_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = string(String) ->
		Value = String
	;
		error("globals__lookup_string_option: invalid string option")
	).

globals__lookup_int_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = int(Int) ->
		Value = Int
	;
		error("globals__lookup_int_option: invalid int option")
	).

globals__lookup_accumulating_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = accumulating(Accumulating) ->
		Value = Accumulating
	;
		error("globals__lookup_accumulating_option: invalid accumulating option")
	).

%-----------------------------------------------------------------------------%

globals__have_static_code_addresses(Globals, IsConst) :-
	globals__get_options(Globals, OptionTable),
	globals__have_static_code_addresses_2(OptionTable, IsConst).

:- pred globals__have_static_code_addresses_2(option_table::in, 
	bool::out) is det.

globals__have_static_code_addresses_2(OptionTable, IsConst) :-
	getopt__lookup_bool_option(OptionTable, gcc_non_local_gotos,
		NonLocalGotos),
	getopt__lookup_bool_option(OptionTable, asm_labels, AsmLabels),
	exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

globals__io_init(Options, GC_Method, TagsMethod, ArgsMethod,
		TypeInfoMethod, PrologDialect) -->
	{ copy(GC_Method, GC_Method1) },
	{ copy(TagsMethod, TagsMethod1) },
	{ copy(ArgsMethod, ArgsMethod1) },
	{ copy(TypeInfoMethod, TypeInfoMethod1) },
	{ copy(PrologDialect, PrologDialect1) },
	{ globals__init(Options, GC_Method1, TagsMethod1, ArgsMethod1,
		TypeInfoMethod1, PrologDialect1, Globals) },
	globals__io_set_globals(Globals).

globals__io_get_gc_method(GC_Method) -->
	globals__io_get_globals(Globals),
	{ globals__get_gc_method(Globals, GC_Method) }.

globals__io_get_tags_method(Tags_Method) -->
	globals__io_get_globals(Globals),
	{ globals__get_tags_method(Globals, Tags_Method) }.

globals__io_get_args_method(ArgsMethod) -->
	globals__io_get_globals(Globals),
	{ globals__get_args_method(Globals, ArgsMethod) }.

globals__io_get_type_info_method(TypeInfoMethod) -->
	globals__io_get_globals(Globals),
	{ globals__get_type_info_method(Globals, TypeInfoMethod) }.

globals__io_get_prolog_dialect(PrologDIalect) -->
	globals__io_get_globals(Globals),
	{ globals__get_prolog_dialect(Globals, PrologDIalect) }.

globals__io_get_globals(Globals) -->
	io__get_globals(UnivGlobals),
	{
		univ_to_type(UnivGlobals, Globals0)
	->
		Globals = Globals0
	;
		error("globals__io_get_globals: univ_to_type failed")
	}.

globals__io_set_globals(Globals) -->
	{ type_to_univ(Globals, UnivGlobals) },
	io__set_globals(UnivGlobals).

%-----------------------------------------------------------------------------%

globals__io_lookup_option(Option, OptionData) -->
	globals__io_get_globals(Globals),
	{ globals__get_options(Globals, OptionTable) },
	{ map__lookup(OptionTable, Option, OptionData) }.

globals__io_set_option(Option, OptionData) -->
	globals__io_get_globals(Globals0),
	{ globals__get_options(Globals0, OptionTable0) },
	{ map__set(OptionTable0, Option, OptionData, OptionTable) },
	{ globals__set_options(Globals0, OptionTable, Globals1) },
	{ copy(Globals1, Globals) },	% XXX inefficient!!!
	globals__io_set_globals(Globals).

%-----------------------------------------------------------------------------%

globals__io_lookup_bool_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, Option, Value) }.

globals__io_lookup_int_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_int_option(Globals, Option, Value) }.

globals__io_lookup_string_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_string_option(Globals, Option, Value) }.

globals__io_lookup_accumulating_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_accumulating_option(Globals, Option, Value) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
