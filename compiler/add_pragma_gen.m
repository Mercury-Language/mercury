%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2023-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Add compiler-generated pragmas (from .trans_opt files) to the HLDS.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma_gen.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------%

:- pred add_gen_pragma_unused_args(gen_pragma_unused_args_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_exceptions(gen_pragma_exceptions_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_trailing(gen_pragma_trailing_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_mm_tabling(gen_pragma_mm_tabling_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_pragma_util.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

add_gen_pragma_unused_args(UnusedArgsInfo, !ModuleInfo, !Specs) :-
    UnusedArgsInfo = gen_pragma_unused_args_info(PredNameArityPFMn, UnusedArgs,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_internal_error, Context, "unused_args",
        PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        module_info_get_unused_arg_info(!.ModuleInfo, UnusedArgInfo0),
        % Convert the mode number to a proc_id.
        proc_id_to_int(ProcId, ModeNum),
        PredProcId = proc(PredId, ProcId),
        map.set(PredProcId, UnusedArgs, UnusedArgInfo0, UnusedArgInfo),
        module_info_set_unused_arg_info(UnusedArgInfo, !ModuleInfo)
    ;
        MaybePredId = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------%

add_gen_pragma_exceptions(Exceptions, !ModuleInfo, !Specs) :-
    Exceptions = gen_pragma_exceptions_info(PredNameArityPFMn, ThrowStatus,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    % XXX We will just ignore errors for the time being -
    % it causes errors with transitive-intermodule optimization.
    % XXX What kinds of errors?
    look_up_pragma_pf_sym_arity_mode_num(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "exceptions",
        PredOrFunc, SymName, UserArity, ModeNum, MaybePredProc),
    (
        MaybePredProc = ok4(PredId, ProcId, PredInfo0, ProcInfo0),
        ProcExceptionInfo = proc_exception_info(ThrowStatus, no),
        proc_info_set_exception_info(yes(ProcExceptionInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredProc = error4(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------%

add_gen_pragma_trailing(Trailing, !ModuleInfo, !Specs) :-
    Trailing = gen_pragma_trailing_info(PredNameArityPFMn, TrailingStatus,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    % XXX We will just ignore errors for the time being -
    % it causes errors with transitive-intermodule optimization.
    % XXX What kinds of errors?
    look_up_pragma_pf_sym_arity_mode_num(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "trailing_info",
        PredOrFunc, SymName, UserArity, ModeNum, MaybePredProc),
    (
        MaybePredProc = ok4(PredId, ProcId, PredInfo0, ProcInfo0),
        ProcTrailingInfo = proc_trailing_info(TrailingStatus, no),
        proc_info_set_trailing_info(yes(ProcTrailingInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredProc = error4(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------%

add_gen_pragma_mm_tabling(MMTabling, !ModuleInfo, !Specs) :-
    MMTabling = gen_pragma_mm_tabling_info(PredNameArityPFMn, TablingStatus,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    % XXX We will just ignore errors for the time being -
    % it causes errors with transitive-intermodule optimization.
    % XXX What kinds of errors?
    look_up_pragma_pf_sym_arity_mode_num(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "mm_tabling_info",
        PredOrFunc, SymName, UserArity, ModeNum, MaybePredProc),
    (
        MaybePredProc = ok4(PredId, ProcId, PredInfo0, ProcInfo0),
        ProcMMTablingInfo = proc_mm_tabling_info(TablingStatus, no),
        proc_info_set_mm_tabling_info(yes(ProcMMTablingInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredProc = error4(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma_gen.
%---------------------------------------------------------------------------%
