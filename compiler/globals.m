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

:- pred globals__init(option_table::in, globals::out).

:- pred globals__get_options(globals::in, option_table::out).

:- pred lookup_option(option::in, option_data::out,
			io__state::di, io__state::uo).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, std_util.

	% currently the only global data is the option table

:- type globals == option_table.

globals__init(Globals, Globals).

globals__get_options(Globals, Globals).

%-----------------------------------------------------------------------------%

lookup_option(Option, OptionData) -->
	io__get_globals(UnivGlobals),
	{ univ_to_type(UnivGlobals, Globals) },
	{ globals__get_options(Globals, OptionTable) },
	{ map__search(OptionTable, Option, OptionData) }.

%-----------------------------------------------------------------------------%
