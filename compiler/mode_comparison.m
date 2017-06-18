%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mod_comparison.m.
%
% This file contains code compare different modes of a predicate.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_comparison.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Given two modes of a predicate, figure out whether they are
    % indistinguishable; that is, whether any valid call to one mode
    % would also be a valid call to the other. (If so, it is a mode error.)
    % Note that mode declarations which only have different final insts
    % do not count as distinguishable.
    %
:- pred modes_are_indistinguishable(proc_id::in, proc_id::in, pred_info::in,
    module_info::in) is semidet.

    % Given two modes of a predicate, figure out whether they are identical,
    % except that one is cc_nondet/cc_multi and the other is nondet/multi.
    % This is used by determinism analysis to substitute a multi mode
    % for a cc_multi one if the call occurs in a non-cc context.
    %
:- pred modes_are_identical_bar_cc(proc_id::in, proc_id::in, pred_info::in,
    module_info::in) is semidet.

%---------------------------------------------------------------------------%

:- type proc_mode
    --->    proc_mode(proc_id, inst_var_sub, list(mer_mode)).

:- type match
    --->    better
    ;       worse
    ;       same
    ;       incomparable.

    % The algorithm for choose_best_match is supposed to be equivalent
    % to the following specification:
    %
    %   1.  Remove any modes that are strictly less instantiated or
    %       less informative on input than other valid modes. For example,
    %
    %       - we prefer an (in, in, out) mode over an (out, in, out) mode,
    %         but not necessarily over an (out, out, in) mode;
    %       - we prefer a (ground -> ...) mode over a (any -> ...) mode;
    %       - we prefer a (bound(f) -> ...) mode over a (ground -> ...) mode;
    %       - we prefer a (... -> dead) mode over a (... -> not dead) mode.
    %
    %       Also, we prefer a (any -> ...) mode over a (free -> ...) mode,
    %       unless the actual argument is free, in which case we prefer
    %       the (free -> ...) mode.
    %
    %   2.  If neither is prefered over the other by step 1, then prioritize
    %       them by determinism, according to the standard partial order
    %       (best first):
    %
    %                           erroneous
    %                          /       \
    %                       det         failure
    %                     /    \       /
    %                 multi     semidet
    %                    \      /
    %                     nondet
    %
    %   3.  If there are still multiple possibilities, take them in
    %       declaration order.
    %
:- pred choose_best_match(mode_info::in, list(proc_mode)::in, pred_id::in,
    proc_table::in, list(prog_var)::in, proc_id::out, inst_var_sub::out,
    list(mer_mode)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.modecheck_util.
:- import_module hlds.vartypes.
:- import_module parse_tree.prog_detism.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

modes_are_indistinguishable(ProcId, OtherProcId, PredInfo, ModuleInfo) :-
    % The code of this predicate is similar to the code for
    % modes_are_identical/4 and compare_proc/5 below.
    %
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    map.lookup(Procs, OtherProcId, OtherProcInfo),

    % Compare the initial insts of the arguments.
    proc_info_get_argmodes(ProcInfo, ProcArgModes),
    proc_info_get_argmodes(OtherProcInfo, OtherProcArgModes),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    mode_list_get_initial_insts(ModuleInfo, OtherProcArgModes,
        OtherInitialInsts),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    compare_inst_list(ModuleInfo, InitialInsts, OtherInitialInsts, no,
        ArgTypes, CompareInsts),
    CompareInsts = same,

    % Compare the expected livenesses of the arguments.
    get_arg_lives(ModuleInfo, ProcArgModes, ProcArgLives),
    get_arg_lives(ModuleInfo, OtherProcArgModes, OtherProcArgLives),
    compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),
    CompareLives = same,

    % Compare the determinisms; if both are cc_, or if both are not cc_,
    % then they are indistinguishable.
    proc_info_interface_determinism(ProcInfo, Detism),
    proc_info_interface_determinism(OtherProcInfo, OtherDetism),
    determinism_components(Detism, _CanFail, Solns),
    determinism_components(OtherDetism, _OtherCanFail, OtherSolns),
    ( Solns = at_most_many_cc, OtherSolns = at_most_many_cc
    ; Solns \= at_most_many_cc, OtherSolns \= at_most_many_cc
    ).

%---------------------------------------------------------------------------%

modes_are_identical_bar_cc(ProcId, OtherProcId, PredInfo, ModuleInfo) :-
    % The code of this predicate is similar to the code for
    % compare_proc/5 below and modes_are_indistinguishable/4 above.

    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    map.lookup(Procs, OtherProcId, OtherProcInfo),

    % Compare the initial insts of the arguments
    proc_info_get_argmodes(ProcInfo, ProcArgModes),
    proc_info_get_argmodes(OtherProcInfo, OtherProcArgModes),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    mode_list_get_initial_insts(ModuleInfo, OtherProcArgModes,
        OtherInitialInsts),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    compare_inst_list(ModuleInfo, InitialInsts, OtherInitialInsts, no,
        ArgTypes, CompareInitialInsts),
    CompareInitialInsts = same,

    % Compare the final insts of the arguments
    mode_list_get_final_insts(ModuleInfo, ProcArgModes, FinalInsts),
    mode_list_get_final_insts(ModuleInfo, OtherProcArgModes,
        OtherFinalInsts),
    compare_inst_list(ModuleInfo, FinalInsts, OtherFinalInsts, no,
        ArgTypes, CompareFinalInsts),
    CompareFinalInsts = same,

    % Compare the expected livenesses of the arguments
    get_arg_lives(ModuleInfo, ProcArgModes, ProcArgLives),
    get_arg_lives(ModuleInfo, OtherProcArgModes, OtherProcArgLives),
    compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),
    CompareLives = same,

    % Compare the determinisms, ignoring the cc part.
    proc_info_interface_determinism(ProcInfo, Detism),
    proc_info_interface_determinism(OtherProcInfo, OtherDetism),
    determinism_components(Detism, CanFail, Solns),
    determinism_components(OtherDetism, OtherCanFail, OtherSolns),
    CanFail = OtherCanFail,
    ( Solns = OtherSolns
    ; Solns = at_most_many_cc, OtherSolns = at_most_many
    ; Solns = at_most_many, OtherSolns = at_most_many_cc
    ).

%---------------------------------------------------------------------------%

choose_best_match(_, [], _, _, _, _, _, _) :-
    unexpected($module, $pred, "no best match").
choose_best_match(ModeInfo, [ProcMode | ProcModes], PredId,
        Procs, ArgVars, TheProcId, TheInstVarSub, TheArgModes) :-
    ProcMode = proc_mode(ProcId, InstVarSub, ArgModes),
    % ProcMode is best iff there is no other proc_mode which is better.
    ( if
        some [OtherProcId] (
            list.member(proc_mode(OtherProcId, _, _), ProcModes),
            compare_proc(ModeInfo, OtherProcId, ProcId, ArgVars, Procs, better)
        )
    then
        choose_best_match(ModeInfo, ProcModes, PredId, Procs, ArgVars,
            TheProcId, TheInstVarSub, TheArgModes)
    else
        TheProcId = ProcId,
        TheInstVarSub = InstVarSub,
        TheArgModes = ArgModes
    ).

    % Given two modes of a predicate, figure out whether one of them is a
    % better match than the other, for calls which could match either mode.
    %
:- pred compare_proc(mode_info::in, proc_id::in, proc_id::in,
    list(prog_var)::in, proc_table::in, match::out) is det.

compare_proc(ModeInfo, ProcId, OtherProcId, ArgVars, Procs, Compare) :-
    % The code of this predicate is similar to the code for
    % modes_are_indistinguishable/4 and modes_are_identical_bar_cc/4 above.

    map.lookup(Procs, ProcId, ProcInfo),
    map.lookup(Procs, OtherProcId, OtherProcInfo),

    % Compare the initial insts of the arguments.
    proc_info_get_argmodes(ProcInfo, ProcArgModes),
    proc_info_get_argmodes(OtherProcInfo, OtherProcArgModes),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_var_types(ModeInfo, VarTypes),
    lookup_var_types(VarTypes, ArgVars, ArgTypes),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    mode_list_get_initial_insts(ModuleInfo, OtherProcArgModes,
        OtherInitialInsts),
    get_var_insts(ModeInfo, ArgVars, ArgInitialInsts),
    compare_inst_list(ModuleInfo, InitialInsts, OtherInitialInsts,
        yes(ArgInitialInsts), ArgTypes, CompareInsts),

    % Compare the expected livenesses of the arguments.
    get_arg_lives(ModuleInfo, ProcArgModes, ProcArgLives),
    get_arg_lives(ModuleInfo, OtherProcArgModes, OtherProcArgLives),
    compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),

    % Compare the determinisms.
    proc_info_interface_determinism(ProcInfo, Detism),
    proc_info_interface_determinism(OtherProcInfo, OtherDetism),
    determinism_components(Detism, CanFail, SolnCount),
    determinism_components(OtherDetism, OtherCanFail, OtherSolnCount),
    compare_solncounts(SolnCount, OtherSolnCount, CompareSolnCounts),
    (
        CompareSolnCounts = first_tighter_than,
        CompareDet = better
    ;
        CompareSolnCounts = first_same_as,
        compare_canfails(CanFail, OtherCanFail, CompareCanFails),
        (
            CompareCanFails = first_tighter_than,
            CompareDet = better
        ;
            CompareCanFails = first_same_as,
            CompareDet = same
        ;
            CompareCanFails = first_looser_than,
            CompareDet = worse
        )
    ;
        CompareSolnCounts = first_looser_than,
        CompareDet = worse
    ),

    % Combine the results, with the insts & lives comparisons
    % taking priority over the determinism comparison.
    combine_results(CompareInsts, CompareLives, Compare0),
    prioritized_combine_results(Compare0, CompareDet, Compare).

:- pred get_var_insts(mode_info::in, list(prog_var)::in,
    list(mer_inst)::out) is det.

get_var_insts(_, [], []).
get_var_insts(ModeInfo, [Var | Vars], [Inst | Insts]) :-
    get_var_inst(ModeInfo, Var, Inst),
    get_var_insts(ModeInfo, Vars, Insts).

:- pred compare_inst_list(module_info::in,
    list(mer_inst)::in, list(mer_inst)::in,
    maybe(list(mer_inst))::in, list(mer_type)::in, match::out) is det.

compare_inst_list(ModuleInfo, InstsA, InstsB, ArgInsts, Types, Result) :-
    ( if
        compare_inst_list_2(ModuleInfo, InstsA, InstsB, ArgInsts,
            Types, Result0)
    then
        Result = Result0
    else
        unexpected($module, $pred, "length mismatch")
    ).

:- pred compare_inst_list_2(module_info::in,
    list(mer_inst)::in, list(mer_inst)::in,
    maybe(list(mer_inst))::in, list(mer_type)::in, match::out) is semidet.

compare_inst_list_2(_, [], [], _, [], same).
compare_inst_list_2(ModuleInfo, [InstA | InstsA], [InstB | InstsB],
        no, [Type | Types], Result) :-
    compare_inst(ModuleInfo, InstA, InstB, no, Type, Result0),
    compare_inst_list_2(ModuleInfo, InstsA, InstsB, no, Types, Result1),
    combine_results(Result0, Result1, Result).
compare_inst_list_2(ModuleInfo, [InstA | InstsA], [InstB | InstsB],
        yes([ArgInst | ArgInsts]), [Type | Types], Result) :-
    compare_inst(ModuleInfo, InstA, InstB, yes(ArgInst), Type, Result0),
    compare_inst_list_2(ModuleInfo, InstsA, InstsB, yes(ArgInsts), Types,
        Result1),
    combine_results(Result0, Result1, Result).

:- pred compare_liveness_list(list(is_live)::in, list(is_live)::in, match::out)
    is det.

compare_liveness_list([], [], same).
compare_liveness_list([_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
compare_liveness_list([], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
compare_liveness_list([LiveA | LiveAs], [LiveB | LiveBs], Result) :-
    compare_liveness(LiveA, LiveB, Result0),
    compare_liveness_list(LiveAs, LiveBs, Result1),
    combine_results(Result0, Result1, Result).

    % Compare_liveness -- prefer dead to live. If either is a valid match,
    % then the actual argument must be dead, so prefer the mode which can take
    % advantage of that).
    %
:- pred compare_liveness(is_live::in, is_live::in, match::out) is det.

compare_liveness(is_dead, is_dead, same).
compare_liveness(is_dead, is_live, better).
compare_liveness(is_live, is_dead, worse).
compare_liveness(is_live, is_live, same).

    % Combine two results, giving priority to the first one.
    %
:- pred prioritized_combine_results(match::in, match::in, match::out) is det.

prioritized_combine_results(better, _, better).
prioritized_combine_results(worse, _, worse).
prioritized_combine_results(same, Result, Result).
prioritized_combine_results(incomparable, _, incomparable).

    % Combine two results, giving them equal priority.
    %
:- pred combine_results(match::in, match::in, match::out) is det.

combine_results(better, better, better).
combine_results(better, same, better).
combine_results(better, worse, incomparable).
combine_results(better, incomparable, incomparable).
combine_results(worse, worse, worse).
combine_results(worse, same, worse).
combine_results(worse, better, incomparable).
combine_results(worse, incomparable, incomparable).
combine_results(same, Result, Result).
combine_results(incomparable, _, incomparable).

    % Compare two initial insts, to figure out which would be a better match.
    %
    % More information is better:
    %   prefer bound(f) to ground
    %   prefer unique to mostly_unique or ground, and
    %   prefer mostly_unique to ground
    %       (unique > mostly_unique > shared > mostly_dead > dead)
    % More bound is better:
    %       (if both can match, the one which is more bound
    %       is better, because it may be an exact match, whereas
    %       the other one would be an implied mode)
    %   prefer ground to free   (i.e. prefer in to out)
    %   prefer ground to any    (e.g. prefer in to in(any))
    %   prefer any to free  (e.g. prefer any->ground to out)
    %
:- pred compare_inst(module_info::in, mer_inst::in, mer_inst::in,
    maybe(mer_inst)::in, mer_type::in, match::out) is det.

compare_inst(ModuleInfo, InstA, InstB, MaybeArgInst, Type, Result) :-
    % inst_matches_initial(A,B) succeeds iff
    %   A specifies at least as much information
    %   and at least as much binding as B --
    %   with the exception that `any' matches_initial `free'
    %   and perhaps vice versa.
    ( if inst_matches_initial(InstA, InstB, Type, ModuleInfo) then
        A_mi_B = yes
    else
        A_mi_B = no
    ),
    ( if inst_matches_initial(InstB, InstA, Type, ModuleInfo) then
        B_mi_A = yes
    else
        B_mi_A = no
    ),
    ( A_mi_B = yes, B_mi_A = no,  Result = better
    ; A_mi_B = no,  B_mi_A = yes, Result = worse
    ; A_mi_B = no,  B_mi_A = no,  Result = incomparable
    ; A_mi_B = yes, B_mi_A = yes,
        % We need to further disambiguate the cases involving `any' and `free',
        % since `any' matches_initial `free' and vice versa.
        % For these cases, we want to take the actual inst of the argument
        % into account: if the argument is `free', we should prefer `free',
        % but otherwise, we should prefer `any'.
        %
        (
            MaybeArgInst = no,
            Result0 = same
        ;
            MaybeArgInst = yes(ArgInst),
            ( if
                inst_matches_initial_no_implied_modes(ArgInst,
                    InstA, Type, ModuleInfo)
            then
                Arg_mf_A = yes
            else
                Arg_mf_A = no
            ),
            ( if
                inst_matches_initial_no_implied_modes(ArgInst,
                    InstB, Type, ModuleInfo)
            then
                Arg_mf_B = yes
            else
                Arg_mf_B = no
            ),
            ( Arg_mf_A = yes, Arg_mf_B = no,  Result0 = better
            ; Arg_mf_A = no,  Arg_mf_B = yes, Result0 = worse
            ; Arg_mf_A = yes, Arg_mf_B = yes, Result0 = same
            ; Arg_mf_A = no,  Arg_mf_B = no,  Result0 = same
            )
        ),
        (
            Result0 = same,
            % If the actual arg inst is not available, or comparing with
            % the arg inst doesn't help, then compare the two proc insts.
            ( if
                inst_matches_initial_no_implied_modes(InstA,
                    InstB, Type, ModuleInfo)
            then
                A_mf_B = yes
            else
                A_mf_B = no
            ),
            ( if
                inst_matches_initial_no_implied_modes(InstB,
                    InstA, Type, ModuleInfo)
            then
                B_mf_A = yes
            else
                B_mf_A = no
            ),
            ( A_mf_B = yes, B_mf_A = no,  Result = better
            ; A_mf_B = no,  B_mf_A = yes, Result = worse
            ; A_mf_B = no,  B_mf_A = no,  Result = incomparable
            ; A_mf_B = yes, B_mf_A = yes, Result = same
            )
        ;
            ( Result0 = better
            ; Result0 = worse
            ),
            Result = Result0
        )
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_comparison.
%---------------------------------------------------------------------------%
