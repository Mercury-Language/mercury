%-----------------------------------------------------------------------------%
% Copyright (C) 1995, 1997-1998, 2001, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: fjh.

% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the io.

%-----------------------------------------------------------------------------%

:- module globals.

:- interface.

:- import_module bool.
:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module options.

:- type globals.

%-----------------------------------------------------------------------------%

:- type what_to_profile
	--->	memory_words
	;	memory_cells
	;	user_plus_system_time
	;	user_time
	;	real_time.

:- pred what_to_profile(string, what_to_profile) is semidet.
:- mode what_to_profile(in, out) is semidet.
:- mode what_to_profile(out, in) is det.

	% Access predicates for the `globals' structure.

:- pred globals__init(option_table::in, globals::out) is det.

:- pred globals__get_what_to_profile(globals::in, what_to_profile::out) is det.
:- pred globals__get_options(globals::in, option_table::out) is det.

:- pred globals__set_what_to_profile(what_to_profile::in,
	globals::in, globals::out) is det.
:- pred globals__set_options(option_table::in, globals::in, globals::out)
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
	% io using io__set_globals and io__get_globals.

:- pred globals__io_init(option_table::in, io::di, io::uo) is det.

:- pred globals__io_get_globals(globals::out, io::di, io::uo) is det.

:- pred globals__io_set_globals(globals::in, io::di, io::uo) is det.

:- pred globals__io_lookup_option(option::in, option_data::out,
	io::di, io::uo) is det.

:- pred globals__io_set_option(option::in, option_data::in,
	io::di, io::uo) is det.

:- pred globals__io_lookup_bool_option(option::in, bool::out, io::di, io::uo)
	is det.
:- pred globals__io_lookup_int_option(option::in, int::out,
	io::di, io::uo) is det.
:- pred globals__io_lookup_string_option(option::in, string::out,
	io::di, io::uo) is det.
:- pred globals__io_lookup_accumulating_option(option::in, list(string)::out,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

what_to_profile("memory-words", memory_words).
what_to_profile("memory-cells", memory_cells).
what_to_profile("user-plus-system-time", user_plus_system_time).
what_to_profile("user-time", user_time).
what_to_profile("real-time", real_time).

:- type globals
	--->	globals(
			what_to_profile	:: what_to_profile,
			option_table	:: option_table
		).

globals__init(Options, globals(user_plus_system_time, Options)).

globals__get_what_to_profile(Globals, Globals ^ what_to_profile).
globals__get_options(Globals, Globals ^ option_table).

globals__set_what_to_profile(WhatToProfile,
	Globals, Globals ^ what_to_profile := WhatToProfile).
globals__set_options(Options, Globals, Globals ^ option_table := Options).

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
		error("globals__lookup_accumulating_option: " ++ 
			"invalid accumulating option")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

globals__io_init(Options, !IO) :-
	globals__init(Options, Globals),
	globals__io_set_globals(Globals, !IO).

globals__io_get_globals(Globals, !IO) :-
	io__get_globals(UnivGlobals, !IO),
	( univ_to_type(UnivGlobals, Globals0) ->
		Globals = Globals0
	;
		error("globals.io_get_globals: univ_to_type failed")
	).

globals__io_set_globals(Globals0, !IO) :-
	unsafe_promise_unique(Globals0, Globals),
	type_to_univ(Globals, UnivGlobals),
	io__set_globals(UnivGlobals, !IO).

%-----------------------------------------------------------------------------%

globals__io_lookup_option(Option, OptionData, !IO) :-
	globals__io_get_globals(Globals, !IO),
	globals__get_options(Globals, OptionTable),
	map__lookup(OptionTable, Option, OptionData).

globals__io_set_option(Option, OptionData, !IO) :-
	globals__io_get_globals(Globals0, !IO),
	globals__get_options(Globals0, OptionTable0),
	map__set(OptionTable0, Option, OptionData, OptionTable),
	globals__set_options(OptionTable, Globals0, Globals),
	globals__io_set_globals(Globals, !IO).

%-----------------------------------------------------------------------------%

globals__io_lookup_bool_option(Option, Value, !IO) :-
	globals__io_get_globals(Globals, !IO),
	globals__lookup_bool_option(Globals, Option, Value).

globals__io_lookup_int_option(Option, Value, !IO) :-
	globals__io_get_globals(Globals, !IO),
	globals__lookup_int_option(Globals, Option, Value).

globals__io_lookup_string_option(Option, Value, !IO) :-
	globals__io_get_globals(Globals, !IO),
	globals__lookup_string_option(Globals, Option, Value).

globals__io_lookup_accumulating_option(Option, Value, !IO) :-
	globals__io_get_globals(Globals, !IO),
	globals__lookup_accumulating_option(Globals, Option, Value).

%-----------------------------------------------------------------------------%
:- end_module globals.
%-----------------------------------------------------------------------------%
