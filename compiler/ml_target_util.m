%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_target_util.m.
% Author: zs.
%
% This module contains utility predicates dealing with target languages.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_target_util.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.

%---------------------------------------------------------------------------%

    % Return `yes' iff the target language supports the specified construct.
    %
    % Note that for int switches one of our target languages (Java) does not
    % support switching on 64-bit values, so we treat that case separately.
    %
:- func globals_target_supports_int_switch(globals) = bool.
:- func globals_target_supports_int_type_switch(globals, int_type) = bool.
:- func globals_target_supports_int64_switch(globals) = bool.
:- func globals_target_supports_string_switch(globals) = bool.
:- func globals_target_supports_goto(globals) = bool.
:- func globals_target_supports_computed_goto(globals) = bool.
:- func globals_target_supports_break_and_continue(globals) = bool.

:- func target_supports_int_switch(compilation_target) = bool.
:- func target_supports_int64_switch(compilation_target) = bool.
:- func target_supports_string_switch(compilation_target) = bool.
:- func target_supports_goto(compilation_target) = bool.
:- func target_supports_computed_goto(compilation_target) = bool.
:- func target_supports_break_and_continue(compilation_target) = bool.

    % This should return `yes' iff downcasts are not needed.
    %
:- func target_supports_inheritence(compilation_target) = bool.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

globals_target_supports_int_switch(Globals) = SupportsIntSwitch :-
    globals.get_target(Globals, Target),
    SupportsIntSwitch = target_supports_int_switch(Target).

globals_target_supports_int64_switch(Globals) = SupportsInt64Switch :-
    globals.get_target(Globals, Target),
    SupportsInt64Switch = target_supports_int64_switch(Target).

globals_target_supports_int_type_switch(Globals, IntType)
        = SupportsIntTypeSwitch :-
    (
        ( IntType = int_type_int
        ; IntType = int_type_uint
        ; IntType = int_type_int8
        ; IntType = int_type_uint8
        ; IntType = int_type_int16
        ; IntType = int_type_uint16
        ; IntType = int_type_int32
        ; IntType = int_type_uint32
        ),
        SupportsIntTypeSwitch = globals_target_supports_int_switch(Globals)
    ;
        ( IntType = int_type_int64
        ; IntType = int_type_uint64
        ),
        SupportsIntTypeSwitch = globals_target_supports_int64_switch(Globals)
    ).

globals_target_supports_string_switch(Globals) = SupportsStringSwitch :-
    globals.get_target(Globals, Target),
    SupportsStringSwitch = target_supports_string_switch(Target).

globals_target_supports_goto(Globals) = SupportsGoto :-
    globals.get_target(Globals, Target),
    SupportsGoto = target_supports_goto(Target).

globals_target_supports_computed_goto(Globals) = SupportsComputedGoto :-
    globals.get_target(Globals, Target),
    SupportsComputedGoto = target_supports_computed_goto(Target).

globals_target_supports_break_and_continue(Globals) = SupportsBreakContinue :-
    globals.get_target(Globals, Target),
    SupportsBreakContinue = target_supports_break_and_continue(Target).

%---------------------------------------------------------------------------%

target_supports_int_switch(target_c) = yes.
target_supports_int_switch(target_csharp) = yes.
target_supports_int_switch(target_java) = yes.
target_supports_int_switch(target_erlang) =
    unexpected($pred, "target erlang").

target_supports_int64_switch(target_c) = yes.
target_supports_int64_switch(target_csharp) = yes.
target_supports_int64_switch(target_java) = no.
target_supports_int64_switch(target_erlang) =
    unexpected($pred, "target erlang").

target_supports_string_switch(target_c) = no.
target_supports_string_switch(target_csharp) = yes.
target_supports_string_switch(target_java) = yes.
    % String switches were added in Java 7, and we now require 8.
target_supports_string_switch(target_erlang) =
    unexpected($pred, "target erlang").

target_supports_computed_goto(target_c) = yes.
target_supports_computed_goto(target_csharp) = no.
target_supports_computed_goto(target_java) = no.
target_supports_computed_goto(target_erlang) =
    unexpected($pred, "target erlang").

target_supports_goto(target_c) = yes.
% XXX C# *does* support gotos but mlds_to_cs.m currently aborts if it
% encounters them.
target_supports_goto(target_csharp) = no.
target_supports_goto(target_java) = no.
target_supports_goto(target_erlang) =
    unexpected($pred, "target erlang").

target_supports_break_and_continue(target_c) = yes.
target_supports_break_and_continue(target_csharp) = yes.
target_supports_break_and_continue(target_java) = yes.
target_supports_break_and_continue(target_erlang) = _ :-
    unexpected($pred, "target erlang").

target_supports_inheritence(target_c) = no.
target_supports_inheritence(target_csharp) = yes.
target_supports_inheritence(target_java) = yes.
target_supports_inheritence(target_erlang) =
    unexpected($pred, "target erlang").

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_target_util.
%---------------------------------------------------------------------------%
