%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998, 2001, 2006, 2011 The University of Melbourne.
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
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type globals.

    % Access predicates for the `globals' structure.

:- pred globals.init(option_table::in, globals::out) is det.

:- pred globals.get_options(globals::in, option_table::out) is det.

:- pred globals.set_options(globals::in, option_table::in, globals::out)
    is det.

:- pred globals.get_output_style(globals::in, output_style::out)
        is det.

:- pred globals.set_output_style(globals::in, output_style::in,
    globals::out) is det.

:- pred globals.lookup_option(globals::in, option::in, option_data::out)
    is det.

:- pred globals.lookup_bool_option(globals::in, option::in, bool::out) is det.

:- pred globals.lookup_int_option(globals::in, option::in, int::out) is det.

:- pred globals.lookup_string_option(globals::in, option::in,
    string::out) is det.

:- pred globals.lookup_accumulating_option(globals::in, option::in,
    list(string)::out) is det.

%-----------------------------------------------------------------------------%

    % Access predicates for storing a `globals' structure in the
    % I/O state using io.set_globals/3 and io.get_globals/3.

:- pred globals.io_init(option_table::in, io::di, io::uo) is det.

:- pred globals.io_get_globals(globals::out, io::di, io::uo) is det.

:- pred globals.io_set_globals(globals::in, io::di, io::uo) is det.

:- pred globals.io_get_output_style(output_style::out, io::di, io::uo) is det.

:- pred globals.io_set_output_style(output_style::in, io::di, io::uo) is det.

:- pred globals.io_lookup_option(option::in, option_data::out,
    io::di, io::uo) is det.

:- pred globals.io_set_option(option::in, option_data::in,
    io::di, io::uo) is det.

:- pred globals.io_lookup_bool_option(option::in, bool::out,
    io::di, io::uo) is det.

:- pred globals.io_lookup_int_option(option::in, int::out,
    io::di, io::uo) is det.

:- pred globals.io_lookup_string_option(option::in, string::out,
    io::di, io::uo) is det.

:- pred globals.io_lookup_accumulating_option(option::in, list(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type globals
    --->    globals(
                option_table,  % Current options.
                output_style   % Current module name.
            ).

globals.init(Options, globals(Options, OutputType)) :-
    default_output_style(OutputType).

globals.get_options(globals(Options, _), Options).

globals.set_options(globals(_, Scanner), Options, globals(Options, Scanner)).

globals.get_output_style(globals(_, Output), Output).

globals.set_output_style(globals(A, _), Output, globals(A, Output)).

globals.lookup_option(Globals, Option, OptionData) :-
    globals.get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

%-----------------------------------------------------------------------------%

globals.lookup_bool_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( OptionData = bool(Bool) ->
        Value = Bool
    ;
        error("globals.lookup_bool_option: invalid bool option")
    ).

globals.lookup_string_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( OptionData = string(String) ->
        Value = String
    ;
        error("globals.lookup_string_option: invalid string option")
    ).

globals.lookup_int_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( OptionData = int(Int) ->
        Value = Int
    ;
        error("globals.lookup_int_option: invalid int option")
    ).

globals.lookup_accumulating_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( OptionData = accumulating(Accumulating) ->
        Value = Accumulating
    ;
        error("globals.lookup_accumulating_option: invalid accumulating option")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

globals.io_init(Options, !IO) :-
    globals.init(Options, Globals),
    globals.io_set_globals(Globals, !IO).

globals.io_get_globals(Globals, !IO) :-
    io.get_globals(UnivGlobals, !IO),
    (
        univ_to_type(UnivGlobals, Globals0)
    ->
        Globals = Globals0
    ;
        error("globals.io_get_globals: univ_to_type failed")
    ).

globals.io_set_globals(Globals, !IO) :-
    unsafe_promise_unique(Globals, UniqGlobals),
    type_to_univ(UniqGlobals, UnivGlobals),
    io.set_globals(UnivGlobals, !IO).

%-----------------------------------------------------------------------------%

globals.io_lookup_option(Option, OptionData, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

globals.io_set_option(Option, OptionData, !IO) :-
    globals.io_get_globals(Globals0, !IO),
    globals.get_options(Globals0, OptionTable0),
    map.set(Option, OptionData, OptionTable0, OptionTable),
    globals.set_options(Globals0, OptionTable, Globals),
    globals.io_set_globals(Globals, !IO).

%-----------------------------------------------------------------------------%

globals.io_lookup_bool_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, Option, Value).

globals.io_lookup_int_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_int_option(Globals, Option, Value).

globals.io_lookup_string_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_string_option(Globals, Option, Value).

globals.io_lookup_accumulating_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_accumulating_option(Globals, Option, Value).

%-----------------------------------------------------------------------------%

globals.io_get_output_style(Output, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.get_output_style(Globals, Output).

globals.io_set_output_style(Output, !IO) :-
    globals.io_get_globals(Globals0, !IO),
    globals.set_output_style(Globals0, Output, Globals),
    globals.io_set_globals(Globals, !IO).

%-----------------------------------------------------------------------------%
:- end_module globals.
%-----------------------------------------------------------------------------%
