%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
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

:- import_module bool.

    % Return `yes' iff the target language supports the specified construct.
    %
:- func globals_target_supports_int_switch(globals) = bool.
:- func globals_target_supports_string_switch(globals) = bool.
:- func globals_target_supports_goto(globals) = bool.
:- func globals_target_supports_computed_goto(globals) = bool.
:- func globals_target_supports_break_and_continue(globals) = bool.

:- func target_supports_int_switch(compilation_target) = bool.
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
    unexpected($module, $pred, "target erlang").

target_supports_string_switch(target_c) = no.
target_supports_string_switch(target_csharp) = yes.
target_supports_string_switch(target_java) = no.
    % String switches were added in Java 7.
target_supports_string_switch(target_erlang) =
    unexpected($module, $pred, "target erlang").

target_supports_computed_goto(target_c) = yes.
target_supports_computed_goto(target_csharp) = no.
target_supports_computed_goto(target_java) = no.
target_supports_computed_goto(target_erlang) =
    unexpected($module, $pred, "target erlang").

target_supports_goto(target_c) = yes.
% XXX C# *does* support gotos but mlds_to_cs.m currently aborts if it
% encounters them.
target_supports_goto(target_csharp) = no.
target_supports_goto(target_java) = no.
target_supports_goto(target_erlang) =
    unexpected($module, $pred, "target erlang").

target_supports_break_and_continue(target_c) = yes.
target_supports_break_and_continue(target_csharp) = yes.
target_supports_break_and_continue(target_java) = yes.
target_supports_break_and_continue(target_erlang) = _ :-
    unexpected($module, $pred, "target erlang").

target_supports_inheritence(target_c) = no.
target_supports_inheritence(target_csharp) = yes.
target_supports_inheritence(target_java) = yes.
target_supports_inheritence(target_erlang) =
    unexpected($module, $pred, "target erlang").

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_target_util.
%---------------------------------------------------------------------------%
