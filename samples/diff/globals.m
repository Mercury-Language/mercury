%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998, 2001, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: globals.m.
% Main author: conway, bromage.
% 
% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the I/O state.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module globals.
:- interface.

:- import_module diff_out.
:- import_module options.

:- import_module bool.
:- import_module getopt.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type globals.

	% Access predicates for the `globals' structure.

:- pred globals__init(option_table::in, globals::out) is det.

:- pred globals__get_options(globals::in, option_table::out) is det.

:- pred globals__set_options(globals::in, option_table::in, globals::out)
		is det.

:- pred globals__get_output_style(globals::in, diff_out__output_style::out)
		is det.

:- pred globals__set_output_style(globals::in, diff_out__output_style::in,
		globals::out) is det.

:- pred globals__lookup_option(globals::in, option::in, option_data::out)
		is det.

:- pred globals__lookup_bool_option(globals, option, bool).
:- mode globals__lookup_bool_option(in, in, out) is det.
:- pred globals__lookup_int_option(globals::in, option::in, int::out) is det.
:- pred globals__lookup_string_option(globals::in, option::in, string::out)
		is det.
:- pred globals__lookup_accumulating_option(globals::in, option::in,
		list(string)::out) is det.

%-----------------------------------------------------------------------------%

	% Access predicates for storing a `globals' structure in the
	% io__state using io__set_globals and io__get_globals.

:- pred globals__io_init(option_table::in,
			io__state::di, io__state::uo) is det.

:- pred globals__io_get_globals(globals::out, io__state::di, io__state::uo)
			is det.

:- pred globals__io_set_globals(globals::in, io__state::di, io__state::uo)
			is det.

:- pred globals__io_get_output_style(diff_out__output_style::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_set_output_style(diff_out__output_style::in,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_option(option::in, option_data::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_set_option(option::in, option_data::in,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_bool_option(option, bool, io__state, io__state).
:- mode globals__io_lookup_bool_option(in, out, di, uo) is det.

:- pred globals__io_lookup_int_option(option::in, int::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_string_option(option::in, string::out,
			io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_accumulating_option(option::in, list(string)::out,
			io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type globals
	--->	globals(
			option_table,		% Current options
			diff_out__output_style	% Current module name
		).

globals__init(Options, globals(Options, OutputType)) :-
	diff_out__default_output_style(OutputType).

globals__get_options(globals(Options, _), Options).

globals__set_options(globals(_, Scanner), Options, globals(Options, Scanner)).

globals__get_output_style(globals(_, Output), Output).

globals__set_output_style(globals(A, _), Output, globals(A, Output)).

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

globals__io_init(Options) -->
	{ globals__init(Options, Globals) },
	globals__io_set_globals(Globals).

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
	{ unsafe_promise_unique(Globals, UniqGlobals) },
	{ type_to_univ(UniqGlobals, UnivGlobals) },
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

globals__io_get_output_style(Output) -->
	globals__io_get_globals(Globals),
	{ globals__get_output_style(Globals, Output) }.

globals__io_set_output_style(Output) -->
	globals__io_get_globals(Globals0),
	{ globals__set_output_style(Globals0, Output, Globals) },
	globals__io_set_globals(Globals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
