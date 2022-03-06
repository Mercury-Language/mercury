%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995, 1997-1998, 2001, 2004-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: globals.m.
% Main author: fjh.
%
% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the io state.
%
%---------------------------------------------------------------------------%

:- module globals.
:- interface.

:- import_module options.

:- import_module bool.
:- import_module getopt.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type globals.

:- type what_to_profile
    --->    memory_words
    ;       memory_cells
    ;       memory_snapshots
    ;       user_plus_system_time
    ;       user_time
    ;       real_time.

:- pred what_to_profile(string, what_to_profile).
:- mode what_to_profile(in, out) is semidet.
:- mode what_to_profile(out, in) is det.

%---------------------------------------------------------------------------%
%
% Access predicates for the `globals' structure.
%

:- pred get_what_to_profile(globals::in, what_to_profile::out) is det.
:- pred get_options(globals::in, option_table::out) is det.

:- pred set_what_to_profile(what_to_profile::in,
    globals::in, globals::out) is det.
:- pred set_options(option_table::in, globals::in, globals::out) is det.

:- pred lookup_option(globals::in, option::in, option_data::out) is det.

:- pred lookup_bool_option(globals::in, option::in, bool::out) is det.
:- pred lookup_int_option(globals::in, option::in, int::out) is det.
:- pred lookup_string_option(globals::in, option::in, string::out) is det.
:- pred lookup_accumulating_option(globals::in, option::in,
    list(string)::out) is det.

%---------------------------------------------------------------------------%

    % Access predicates for storing a `globals' structure in the
    % io using io.set_globals and io.get_globals.

:- pred io_init(option_table::in, io::di, io::uo) is det.

:- pred io_get_globals(globals::out, io::di, io::uo) is det.

:- pred io_set_globals(globals::in, io::di, io::uo) is det.

:- pred io_lookup_option(option::in, option_data::out, io::di, io::uo) is det.

:- pred io_set_option(option::in, option_data::in, io::di, io::uo) is det.

:- pred io_lookup_bool_option(option::in, bool::out, io::di, io::uo) is det.
:- pred io_lookup_int_option(option::in, int::out, io::di, io::uo) is det.
:- pred io_lookup_string_option(option::in, string::out,
    io::di, io::uo) is det.
:- pred io_lookup_accumulating_option(option::in, list(string)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

what_to_profile("memory-words", memory_words).
what_to_profile("memory-cells", memory_cells).
what_to_profile("snapshots", memory_snapshots).
what_to_profile("user-plus-system-time", user_plus_system_time).
what_to_profile("user-time", user_time).
what_to_profile("real-time", real_time).

:- type globals
    --->    globals(
                what_to_profile :: what_to_profile,
                option_table    :: option_table
            ).

get_what_to_profile(Globals, Globals ^ what_to_profile).
get_options(Globals, Globals ^ option_table).

set_what_to_profile(WhatToProfile, !Globals) :-
    !Globals ^ what_to_profile := WhatToProfile.
set_options(Options, !Globals) :-
    !Globals ^ option_table := Options.

lookup_option(Globals, Option, OptionData) :-
    globals.get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

%---------------------------------------------------------------------------%

lookup_bool_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = bool(Bool) then
        Value = Bool
    else
        error("globals.lookup_bool_option: invalid bool option")
    ).

lookup_int_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = int(Int) then
        Value = Int
    else
        error("globals.lookup_int_option: invalid int option")
    ).

lookup_string_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = string(String) then
        Value = String
    else
        error("globals.lookup_string_option: invalid string option")
    ).

lookup_accumulating_option(Globals, Option, Value) :-
    globals.lookup_option(Globals, Option, OptionData),
    ( if OptionData = accumulating(Accumulating) then
        Value = Accumulating
    else
        error("globals.lookup_accumulating_option: " ++
            "invalid accumulating option")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- mutable(profiler_globals, globals, init_profiler_globals, ground,
    [untrailed, attach_to_io_state]).

:- func init_profiler_globals = globals.

init_profiler_globals = Globals :-
    Globals = globals(user_plus_system_time, map.init).

%---------------------------------------------------------------------------%

io_init(Options, !IO) :-
    Globals = globals(user_plus_system_time, Options),
    set_profiler_globals(Globals, !IO).

io_get_globals(Globals, !IO) :-
    get_profiler_globals(Globals, !IO).

io_set_globals(Globals, !IO) :-
    set_profiler_globals(Globals, !IO).

%---------------------------------------------------------------------------%

io_lookup_option(Option, OptionData, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

io_set_option(Option, OptionData, !IO) :-
    globals.io_get_globals(Globals0, !IO),
    globals.get_options(Globals0, OptionTable0),
    map.set(Option, OptionData, OptionTable0, OptionTable),
    globals.set_options(OptionTable, Globals0, Globals),
    globals.io_set_globals(Globals, !IO).

%---------------------------------------------------------------------------%

io_lookup_bool_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, Option, Value).

io_lookup_int_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_int_option(Globals, Option, Value).

io_lookup_string_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_string_option(Globals, Option, Value).

io_lookup_accumulating_option(Option, Value, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_accumulating_option(Globals, Option, Value).

%---------------------------------------------------------------------------%
:- end_module globals.
%---------------------------------------------------------------------------%
