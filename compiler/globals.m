%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module globals.

% Main author: fjh.

% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the io__state.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module options, getopt.

:- type globals.

:- type gc_method
	--->	none
	;	conservative
	;	accurate.

:- type tags_method
	--->	none
	;	low
	;	high.

:- pred convert_gc_method(string::in, gc_method::out) is semidet.

:- pred convert_tags_method(string::in, tags_method::out) is semidet.

%-----------------------------------------------------------------------------%

	% Access predicates for the `globals' structure.

:- pred globals__init(option_table::in, gc_method::in, tags_method::in,
			globals::out) is det.

:- pred globals__get_options(globals::in, option_table::out) is det.

:- pred globals__set_options(globals::in, option_table::in, globals::out)
	is det.

:- pred globals__get_gc_method(globals::in, gc_method::out) is det.

:- pred globals__set_gc_method(globals::in, gc_method::in, globals::out)
	is det.

:- pred globals__get_tags_method(globals::in, tags_method::out) is det.

:- pred globals__set_tags_method(globals::in, tags_method::in, globals::out)
	is det.

:- pred globals__lookup_option(globals::in, option::in, option_data::out)
	is det.

:- pred globals__lookup_bool_option(globals::in, option::in, bool::out) is det.
:- pred globals__lookup_int_option(globals::in, option::in, int::out) is det.
:- pred globals__lookup_string_option(globals::in, option::in, string::out)
	is det.
:- pred globals__lookup_accumulating_option(globals::in, option::in,
		list(string)::out) is det.

%-----------------------------------------------------------------------------%

	% Access predicates for storing a `globals' structure in the
	% io__state using io__set_globals and io__get_globals.

:- pred globals__io_init(option_table::in, gc_method::in, tags_method::in,
			io__state::di, io__state::uo) is det.

:- pred globals__io_get_gc_method(gc_method::out,
				io__state::di, io__state::uo) is det.

:- pred globals__io_get_tags_method(tags_method::out,
				io__state::di, io__state::uo) is det.

:- pred globals__io_get_globals(globals::out, io__state::di, io__state::uo)
	is det.

:- pred globals__io_set_globals(globals::in, io__state::di, io__state::uo)
	is det.

:- pred globals__io_lookup_option(option::in, option_data::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_set_option(option::in, option_data::in,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_bool_option(option::in, bool::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_int_option(option::in, int::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_string_option(option::in, string::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_accumulating_option(option::in, list(string)::out,
			io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, std_util, io, require.

%-----------------------------------------------------------------------------%

convert_gc_method("none", none).
convert_gc_method("conservative", conservative).
convert_gc_method("accurate", accurate).

convert_tags_method("none", none).
convert_tags_method("low", low).
convert_tags_method("high", high).

%-----------------------------------------------------------------------------%

:- type globals
	--->	globals(
			option_table,
			gc_method,
			tags_method
		).

globals__init(Options, GC_Method, Tags_Method,
	globals(Options, GC_Method, Tags_Method)).

globals__get_options(globals(Options, _, _), Options).

globals__set_options(globals(_, B, C), Options, globals(Options, B, C)).

globals__get_gc_method(globals(_, GC_Method, _), GC_Method).

globals__set_gc_method(globals(A, _, C), GC_Method, globals(A, GC_Method, C)).

globals__get_tags_method(globals(_, _, Tags_Method), Tags_Method).

globals__set_tags_method(globals(A, B, _), Tags_Method,
	globals(A, B, Tags_Method)).

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
%-----------------------------------------------------------------------------%

globals__io_init(Options, GC_Method, Tags_Method) -->
	{ globals__init(Options, GC_Method, Tags_Method, Globals) },
	globals__io_set_globals(Globals).

globals__io_get_gc_method(GC_Method) -->
	globals__io_get_globals(Globals),
	{ globals__get_gc_method(Globals, GC_Method) }.

globals__io_get_tags_method(Tags_Method) -->
	globals__io_get_globals(Globals),
	{ globals__get_tags_method(Globals, Tags_Method) }.

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
	{ globals__set_options(Globals0, OptionTable, Globals) },
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
