%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998, 2001, 2006, 2011 The University of Melbourne.
% Copyright (C) 2015, 2024 The Mercury team.
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
% This global data is stored in a mutable attached to the I/O state.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module globals.
:- interface.

:- import_module diff_out.
:- import_module options.

:- import_module bool.
:- import_module getopt.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type globals.

    % Access predicates for the `globals' structure.

:- pred init(option_table::in, globals::out) is det.

:- pred get_options(globals::in, option_table::out) is det.

:- pred set_options(option_table::in, globals::in, globals::out) is det.

:- pred get_output_style(globals::in, output_style::out) is det.

:- pred set_output_style(output_style::in, globals::in, globals::out) is det.

:- pred lookup_option(globals::in, option::in, option_data::out) is det.

:- pred lookup_bool_option(globals::in, option::in, bool::out) is det.

:- pred lookup_int_option(globals::in, option::in, int::out) is det.

:- pred lookup_string_option(globals::in, option::in, string::out) is det.

:- pred lookup_accumulating_option(globals::in, option::in, list(string)::out)
    is det.

%-----------------------------------------------------------------------------%

    % Access predicates for storing a `globals' structure in a mutable
    % attached to the I/O state.

:- pred io_init(option_table::in, io::di, io::uo) is det.

:- pred io_get_output_style(output_style::out, io::di, io::uo) is det.

:- pred io_set_output_style(output_style::in, io::di, io::uo) is det.

:- pred io_lookup_option(option::in, option_data::out,
    io::di, io::uo) is det.

:- pred io_set_option(option::in, option_data::in,
    io::di, io::uo) is det.

:- pred io_lookup_bool_option(option::in, bool::out,
    io::di, io::uo) is det.

:- pred io_lookup_int_option(option::in, int::out,
    io::di, io::uo) is det.

:- pred io_lookup_string_option(option::in, string::out,
    io::di, io::uo) is det.

:- pred io_lookup_accumulating_option(option::in, list(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

:- mutable(globals_var, globals, globals(map.init, normal), ground,
    [attach_to_io_state, untrailed]).

%-----------------------------------------------------------------------------%

:- type globals
    --->    globals(
                option_table,  % Current options.
                output_style   % Current output style.
            ).

init(Options, globals(Options, OutputType)) :-
    default_output_style(OutputType).

get_options(globals(Options, _), Options).

set_options(Options, globals(_, Scanner), globals(Options, Scanner)).

get_output_style(globals(_, Output), Output).

set_output_style(Output, globals(A, _), globals(A, Output)).

lookup_option(Globals, Option, OptionData) :-
    globals.get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

%-----------------------------------------------------------------------------%

lookup_bool_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = bool(Bool) then
        Value = Bool
    else
        error("globals.lookup_bool_option: invalid bool option")
    ).

lookup_string_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = string(String) then
        Value = String
    else
        error("globals.lookup_string_option: invalid string option")
    ).

lookup_int_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = int(Int) then
        Value = Int
    else
        error("globals.lookup_int_option: invalid int option")
    ).

lookup_accumulating_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = accumulating(Accumulating) then
        Value = Accumulating
    else
        error("globals.lookup_accumulating_option: invalid accumulating option")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io_init(Options, !IO) :-
    globals.init(Options, Globals),
    set_globals_var(Globals, !IO).

%-----------------------------------------------------------------------------%

io_lookup_option(Option, OptionData, !IO) :-
    get_globals_var(Globals, !IO),
    globals.get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

io_set_option(Option, OptionData, !IO) :-
    get_globals_var(Globals0, !IO),
    globals.get_options(Globals0, OptionTable0),
    map.set(Option, OptionData, OptionTable0, OptionTable),
    globals.set_options(OptionTable, Globals0, Globals),
    set_globals_var(Globals, !IO).

%-----------------------------------------------------------------------------%

io_lookup_bool_option(Option, Value, !IO) :-
    get_globals_var(Globals, !IO),
    globals.lookup_bool_option(Globals, Option, Value).

io_lookup_int_option(Option, Value, !IO) :-
    get_globals_var(Globals, !IO),
    globals.lookup_int_option(Globals, Option, Value).

io_lookup_string_option(Option, Value, !IO) :-
    get_globals_var(Globals, !IO),
    globals.lookup_string_option(Globals, Option, Value).

io_lookup_accumulating_option(Option, Value, !IO) :-
    get_globals_var(Globals, !IO),
    globals.lookup_accumulating_option(Globals, Option, Value).

%-----------------------------------------------------------------------------%

io_get_output_style(Output, !IO) :-
    get_globals_var(Globals, !IO),
    globals.get_output_style(Globals, Output).

io_set_output_style(Output, !IO) :-
    get_globals_var(Globals0, !IO),
    globals.set_output_style(Output, Globals0, Globals),
    set_globals_var(Globals, !IO).

%-----------------------------------------------------------------------------%
:- end_module globals.
%-----------------------------------------------------------------------------%
