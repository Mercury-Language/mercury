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

:- pred globals__init(option_table::in, globals::out) is det.

:- pred globals__get_options(globals::in, option_table::out) is det.

:- pred globals__set_options(globals::in, option_table::in, globals::out)
	is det.

:- pred globals__lookup_option(option::in, option_data::out,
			io__state::di, io__state::uo) is det.

:- pred globals__set_option(option::in, option_data::in,
			io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, std_util, io, require.

%-----------------------------------------------------------------------------%

	% currently the only global data is the option table

:- type globals == option_table.

globals__init(Globals, Globals).

globals__get_options(Globals, Globals).

globals__set_options(_, Globals, Globals).

%-----------------------------------------------------------------------------%

globals__lookup_option(Option, OptionData) -->
	io__get_globals(UnivGlobals),
	{
		univ_to_type(UnivGlobals, Globals),
		globals__get_options(Globals, OptionTable),
		map__search(OptionTable, Option, OptionData0)
	->
		OptionData = OptionData0
	;
		error("globals__lookup_option: failed")
	}.

globals__set_option(Option, OptionData) -->
	io__get_globals(UnivGlobals0),
	{
		univ_to_type(UnivGlobals0, Globals0)
	->
		Globals1 = Globals0
	;
		error("globals__set_option: univ_to_type failed")
	},
	{ globals__get_options(Globals1, OptionTable0) },
	{ map__set(OptionTable0, Option, OptionData, OptionTable) },
	{ globals__set_options(Globals1, OptionTable, Globals) },
	{ type_to_univ(Globals, UnivGlobals) },
	io__set_globals(UnivGlobals).

%-----------------------------------------------------------------------------%
