%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2007, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2019, 2021-2023, 2025-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: term_util.m.
% Main author: crs.
%
% This module defines some types and utility predicates that are either
% - actually used by both our termination analysers, or
% - could potentially be used by both our termination analysers.
%
%---------------------------------------------------------------------------%

:- module termination.term_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc.
:- import_module hlds.pred_proc_id.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

%---------------------------------------------------------------------------%

:- pred get_context_from_scc(module_info::in, scc::in, prog_context::out)
    is det.

%---------------------------------------------------------------------------%

    % Succeeds if the foreign proc attributes imply that a procedure is
    % terminating.
    %
:- pred attributes_imply_termination(foreign_proc_attributes::in)
    is semidet.

%---------------------------------------------------------------------------%

:- type maybe_believe_check_termination
    --->    do_not_believe_check_termination
    ;       do_believe_check_termination.

    % When we process imported predicates, should we believe that
    % the presence of a 'check_termination' pragma, or rather the pred marker
    % indicating the presence of such a pragma, guarantees that (in the absence
    % of an error from that pragma) the predicate will actually terminate?
    %
    % The check_termination pragma will be checked by the compiler
    % when it compiles the source file that the predicate was imported from.
    % However, when we make .opt files, we do not check whether predicates
    % with check_termination pragmas actually terminate, so we cannot assume
    % that they do, since any violations of that assumption will *not* be
    % reported.
    %
:- pred should_we_believe_check_termination_markers(module_info::in,
    maybe_believe_check_termination::out) is det.

%---------------------------------------------------------------------------%

    % Succeed if all arguments of the given procedure of the given predicate
    % are either input or zero size.
    %
:- pred all_args_input_or_zero_size(module_info::in, pred_info::in,
    proc_info::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.mode_test.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module termination.term_norm.

:- import_module list.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

get_context_from_scc(ModuleInfo, SCC, Context) :-
    set.to_sorted_list(SCC, SCCProcs),
    (
        SCCProcs = [proc(PredId, _) | _],
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_context(PredInfo, Context)
    ;
        SCCProcs = [],
        unexpected($pred, "empty SCC")
    ).

%---------------------------------------------------------------------------%

attributes_imply_termination(Attributes) :-
    (
        get_terminates(Attributes) = proc_terminates
    ;
        get_terminates(Attributes) = depends_on_mercury_calls,
        get_may_call_mercury(Attributes) = proc_will_not_call_mercury
    ).

%---------------------------------------------------------------------------%

should_we_believe_check_termination_markers(ModuleInfo, Believe) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_op_mode(Globals, OpMode),
    ( if OpMode = opm_top_args(opma_augment(opmau_make_plain_opt), _) then
        Believe = do_not_believe_check_termination
    else
        Believe = do_believe_check_termination
    ).

%---------------------------------------------------------------------------%

all_args_input_or_zero_size(ModuleInfo, PredInfo, ProcInfo) :-
    pred_info_get_arg_types(PredInfo, TypeList),
    proc_info_get_argmodes(ProcInfo, ModeList),
    all_args_input_or_zero_size_2(TypeList, ModeList, ModuleInfo).

:- pred all_args_input_or_zero_size_2(list(mer_type)::in, list(mer_mode)::in,
    module_info::in) is semidet.

all_args_input_or_zero_size_2([], [], _).
all_args_input_or_zero_size_2([], [_|_], _) :-
    unexpected($pred, "unmatched lists").
all_args_input_or_zero_size_2([_|_], [], _) :-
    unexpected($pred, "unmatched lists").
all_args_input_or_zero_size_2([Type | Types], [Mode | Modes], ModuleInfo) :-
    ( if mode_is_input(ModuleInfo, Mode) then
        % The variable is an input variables, so its size is irrelevant.
        all_args_input_or_zero_size_2(Types, Modes, ModuleInfo)
    else
        term_norm.zero_size_type(ModuleInfo, Type),
        all_args_input_or_zero_size_2(Types, Modes, ModuleInfo)
    ).

%---------------------------------------------------------------------------%
:- end_module termination.term_util.
%---------------------------------------------------------------------------%
