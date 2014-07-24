%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_tasks.m.
%
% This module handles the specification of what tasks the submodules of
% simplify.m should perform.
%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_tasks.
:- interface.

:- import_module libs.
:- import_module libs.globals.

:- import_module bool.
:- import_module list.

    % Each value of this type represents a task, or a group of related tasks,
    % that simplification should perform.
:- type simplification
    --->    simp_warn_simple_code       % --warn-simple-code
    ;       simp_warn_duplicate_calls   % --warn-duplicate-calls
    ;       simp_format_calls           % Invoke format_call.m.
    ;       simp_warn_obsolete          % --warn-obsolete
    ;       simp_do_once                % Run things that should be done once.
    ;       simp_after_front_end        % Run things that should be done
                                        % at the end of the front end.
    ;       simp_excess_assigns         % Remove excess assignment
                                        % unifications.
    ;       simp_elim_removable_scopes  % Remove scopes that do not need
                                        % processing during LLDS code
                                        % generation.
    ;       simp_opt_duplicate_calls    % Optimize duplicate calls.
    ;       simp_constant_prop          % Partially evaluate calls.
    ;       simp_common_struct          % Common structure elimination.
    ;       simp_extra_common_struct    % Do common structure elimination
                                        % even when it might increase stack
                                        % usage (used by deforestation).
    ;       simp_ignore_par_conjs.      % Replace parallel conjunctions with
                                        % plain conjunctions.

    % Each value of this type represents the full set of tasks
    % that simplification should perform. The submodules of simplify.m
    % use it to find out whether they should perform a specific task
    % without having to search a list of simplifications.
    %
    % The definition of this type does not need to be visible to modules
    % outside simplify.m, but avoiding such visibility would cost more
    % in extra complexity than it would gain.
:- type simplifications
    --->    simplifications(
                do_warn_simple_code         :: bool,
                do_warn_duplicate_calls     :: bool,
                do_format_calls             :: bool,
                do_warn_obsolete            :: bool,
                do_do_once                  :: bool,
                do_after_front_end          :: bool,
                do_excess_assign            :: bool,
                do_elim_removable_scopes    :: bool,
                do_opt_duplicate_calls      :: bool,
                do_constant_prop            :: bool,
                do_common_struct            :: bool,
                do_extra_common_struct      :: bool,
                do_ignore_par_conjunctions  :: bool
            ).

:- func simplifications_to_list(simplifications) = list(simplification).
:- func list_to_simplifications(list(simplification)) = simplifications.

    % Find out which simplifications should be run from the options table
    % stored in the globals. The first argument states whether warnings
    % should be issued during this pass of simplification.
    %
:- pred find_simplifications(bool::in, globals::in, simplifications::out)
    is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.

simplifications_to_list(Simplifications) = List :-
    Simplifications = simplifications(WarnSimpleCode, WarnDupCalls,
        DoFormatCalls, WarnObsolete, DoOnce,
        AfterFrontEnd, ExcessAssign, ElimRemovableScopes, OptDuplicateCalls,
        ConstantProp, CommonStruct, ExtraCommonStruct, RemoveParConjunctions),
    List =
        ( WarnSimpleCode = yes -> [simp_warn_simple_code] ; [] ) ++
        ( WarnDupCalls = yes -> [simp_warn_duplicate_calls] ; [] ) ++
        ( DoFormatCalls = yes -> [simp_format_calls] ; [] ) ++
        ( WarnObsolete = yes -> [simp_warn_obsolete] ; [] ) ++
        ( DoOnce = yes -> [simp_do_once] ; [] ) ++
        ( AfterFrontEnd = yes -> [simp_after_front_end] ; [] ) ++
        ( ExcessAssign = yes -> [simp_excess_assigns] ; [] ) ++
        ( ElimRemovableScopes = yes -> [simp_elim_removable_scopes] ; [] ) ++
        ( OptDuplicateCalls = yes -> [simp_opt_duplicate_calls] ; [] ) ++
        ( ConstantProp = yes -> [simp_constant_prop] ; [] ) ++
        ( CommonStruct = yes -> [simp_common_struct] ; [] ) ++
        ( ExtraCommonStruct = yes -> [simp_extra_common_struct] ; [] ) ++
        ( RemoveParConjunctions = yes -> [simp_ignore_par_conjs] ; [] ).

list_to_simplifications(List) =
    simplifications(
        ( list.member(simp_warn_simple_code, List) -> yes ; no ),
        ( list.member(simp_warn_duplicate_calls, List) -> yes ; no ),
        ( list.member(simp_format_calls, List) -> yes ; no ),
        ( list.member(simp_warn_obsolete, List) -> yes ; no ),
        ( list.member(simp_do_once, List) -> yes ; no ),
        ( list.member(simp_after_front_end, List) -> yes ; no ),
        ( list.member(simp_excess_assigns, List) -> yes ; no ),
        ( list.member(simp_elim_removable_scopes, List) -> yes ; no ),
        ( list.member(simp_opt_duplicate_calls, List) -> yes ; no ),
        ( list.member(simp_constant_prop, List) -> yes ; no ),
        ( list.member(simp_common_struct, List) -> yes ; no ),
        ( list.member(simp_extra_common_struct, List) -> yes ; no ),
        ( list.member(simp_ignore_par_conjs, List) -> yes ; no )
    ).

find_simplifications(WarnThisPass, Globals, Simplifications) :-
    globals.lookup_bool_option(Globals, warn_simple_code, WarnSimple),
    globals.lookup_bool_option(Globals, warn_duplicate_calls, WarnDupCalls),
    globals.lookup_bool_option(Globals, warn_known_bad_format_calls,
        WarnKnownBadFormat),
    globals.lookup_bool_option(Globals, warn_unknown_format_calls,
        WarnUnknownFormat),
    globals.lookup_bool_option(Globals, optimize_format_calls,
        OptFormatCalls),
    (
        (
            WarnThisPass = yes,
            ( WarnKnownBadFormat = yes ; WarnUnknownFormat = yes  )
        ;
            OptFormatCalls = yes
        )
    ->
        DoFormatCalls = yes
    ;
        DoFormatCalls = no
    ),
    globals.lookup_bool_option(Globals, warn_obsolete, WarnObsolete),
    globals.lookup_bool_option(Globals, excess_assign, ExcessAssign),
    globals.lookup_bool_option(Globals, common_struct, CommonStruct),
    globals.lookup_bool_option(Globals, optimize_duplicate_calls,
        OptDuplicateCalls),
    globals.lookup_bool_option(Globals, constant_propagation, ConstantProp),
    DoOnce = no,
    AfterFrontEnd = no,
    ElimRemovableScopes = no,
    ExtraCommonStruct = no,
    globals.lookup_bool_option(Globals, ignore_par_conjunctions,
        RemoveParConjunctions),

    Simplifications = simplifications(
        ( WarnSimple = yes, WarnThisPass = yes -> yes ; no),
        ( WarnDupCalls = yes, WarnThisPass = yes -> yes ; no),
        DoFormatCalls,
        ( WarnObsolete = yes, WarnThisPass = yes -> yes ; no),
        DoOnce,
        AfterFrontEnd,
        ExcessAssign,
        ElimRemovableScopes,
        OptDuplicateCalls,
        ConstantProp,
        CommonStruct,
        ExtraCommonStruct,
        RemoveParConjunctions
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_tasks.
%---------------------------------------------------------------------------%
