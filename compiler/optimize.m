%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: optimize.m.
% Main author: zs.
%
% This module contains LLDS to LLDS optimizations.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.optimize.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred optimize_procs(globals::in, module_name::in, global_data::in,
    list(c_procedure)::in, list(c_procedure)::out) is det.

:- pred optimize_proc(globals::in, module_name::in, global_data::in,
    c_procedure::in, c_procedure::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_pred.
:- import_module libs.file_util.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.delay_slot.
:- import_module ll_backend.dupelim.
:- import_module ll_backend.frameopt.
:- import_module ll_backend.jumpopt.
:- import_module ll_backend.labelopt.
:- import_module ll_backend.stdlabel.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.opt_util.
:- import_module ll_backend.peephole.
:- import_module ll_backend.reassign.
:- import_module ll_backend.use_local_vars.
:- import_module ll_backend.wrap_blocks.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module char.
:- import_module counter.
:- import_module dir.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%-----------------------------------------------------------------------------%

optimize_procs(_, _, _, [], []).
optimize_procs(Globals, ModuleName, GlobalData,
        [Proc0 | Procs0], [Proc | Procs]) :-
    optimize_proc(Globals, ModuleName, GlobalData, Proc0, Proc),
    optimize_procs(Globals, ModuleName, GlobalData, Procs0, Procs).

optimize_proc(Globals, ModuleName, GlobalData, CProc0, CProc) :-
    Info = init_llds_opt_info(Globals, ModuleName),
    some [!OptDebugInfo, !LabelNumCounter, !Instrs] (
        CProc0 = c_procedure(Name, Arity, PredProcId, ProcLabel, CodeModel,
            !:Instrs, !:LabelNumCounter, MayAlterRtti, CGlobalVars),
        need_opt_debug_info(Info, Name, Arity, PredProcId, MaybeBaseName),
        (
            MaybeBaseName = no,
            !:OptDebugInfo = no_opt_debug_info
        ;
            MaybeBaseName = yes(BaseName),
            FirstFileName = BaseName ++ ".opt" ++ num_to_str(0),
            trace [io(!IO)] (
                output_first_opt_debug(Info, FirstFileName, ProcLabel,
                    !.Instrs, !.LabelNumCounter, !IO)
            ),
            !:OptDebugInfo = opt_debug_info(BaseName, 0, FirstFileName, 0,
                FirstFileName, !.Instrs)
        ),
        Repeat = Info ^ lopt_opt_repeat,
        ( if
            global_data_maybe_get_proc_layout(GlobalData,
                PredProcId, ProcLayout)
        then
            LabelMap = ProcLayout ^ pli_internal_map,
            map.sorted_keys(LabelMap, LayoutLabelNums),
            LayoutLabels = list.map(
                make_internal_label_for_proc_label(ProcLabel),
                LayoutLabelNums),
            set_tree234.sorted_list_to_set(LayoutLabels, LayoutLabelSet)
        else
            LayoutLabelSet = set_tree234.init
        ),
        Statistics = Info ^ lopt_detailed_statistics,
        optimize_initial(Info, LayoutLabelSet, ProcLabel, CodeModel,
            MayAlterRtti, !LabelNumCounter, !OptDebugInfo, !Instrs),
        optimize_repeat(Info, Repeat, LayoutLabelSet, ProcLabel,
            MayAlterRtti, !LabelNumCounter, !OptDebugInfo, !Instrs),
        trace [io(!IO)] (
            get_opt_progress_output_stream(Info, ProgressStream, !IO),
            maybe_report_stats(ProgressStream, Statistics, !IO)
        ),
        optimize_middle(Info, yes, LayoutLabelSet, ProcLabel, CodeModel,
            MayAlterRtti, !LabelNumCounter, !OptDebugInfo, !Instrs),
        trace [io(!IO)] (
            get_opt_progress_output_stream(Info, ProgressStream, !IO),
            maybe_report_stats(ProgressStream, Statistics, !IO)
        ),
        optimize_last(Info, LayoutLabelSet, ProcLabel,
            !LabelNumCounter, !.OptDebugInfo, !Instrs),
        trace [io(!IO)] (
            get_opt_progress_output_stream(Info, ProgressStream, !IO),
            maybe_report_stats(ProgressStream, Statistics, !IO)
        ),
        CProc = c_procedure(Name, Arity, PredProcId, ProcLabel, CodeModel,
            !.Instrs, !.LabelNumCounter, MayAlterRtti, CGlobalVars)
    ).

:- func make_internal_label_for_proc_label(proc_label, int) = label.

make_internal_label_for_proc_label(ProcLabel, LabelNum)
    = internal_label(LabelNum, ProcLabel).

%-----------------------------------------------------------------------------%

:- type opt_debug_info
    --->    opt_debug_info(
                % Base file name for the dump files.
                odi_dump_file_base_name             :: string,

                % The number of the last dump file written.
                odi_last_dump_num                   :: int,

                % The name of the last dump file written.
                odi_last_dump_file_name             :: string,

                % The number of the last dump file written that has
                % the instruction sequence in it.
                odi_last_instrs_dump_num            :: int,

                % The name of the last dump file written that has
                % the instruction sequence in it.
                odi_last_instrs_dump_file_name      :: string,

                % The instruction sequence at the time the last dump file
                % was written.
                odi_last_dump_instr_seq             :: list(instruction)
            )
    ;       no_opt_debug_info.

:- pred need_opt_debug_info(llds_opt_info::in, string::in, int::in,
    pred_proc_id::in, maybe(string)::out) is det.

need_opt_debug_info(Info, Name, Arity, PredProcId, MaybeBaseName) :-
    DebugOpt = Info ^ lopt_debug_opt,
    DebugOptPredIdStrs = Info ^ lopt_debug_opt_pred_ids,
    DebugOptPredNames = Info ^ lopt_debug_opt_pred_names,
    PredProcId = proc(PredId, ProcId),
    pred_id_to_int(PredId, PredIdInt),
    proc_id_to_int(ProcId, ProcIdInt),
    ( if
        DebugOpt = yes,
        (
            DebugOptPredIdStrs = [_ | _],
            DebugOptPredNames = [_ | _],
            (
                some [DebugOptPredIdStr, DebugOptPredId] (
                    list.member(DebugOptPredIdStr, DebugOptPredIdStrs),
                    string.to_int(DebugOptPredIdStr, DebugOptPredId),
                    DebugOptPredId = PredIdInt
                )
            ;
                list.member(Name, DebugOptPredNames)
            )
        ;
            DebugOptPredIdStrs = [_ | _],
            DebugOptPredNames = [],
            some [DebugOptPredIdStr, DebugOptPredId] (
                list.member(DebugOptPredIdStr, DebugOptPredIdStrs),
                string.to_int(DebugOptPredIdStr, DebugOptPredId),
                DebugOptPredId = PredIdInt
            )
        ;
            DebugOptPredIdStrs = [],
            DebugOptPredNames = [_ | _],
            list.member(Name, DebugOptPredNames)
        ;
            DebugOptPredIdStrs = [],
            DebugOptPredNames = []
        )
    then
        BaseName = opt_subdir_name ++ "/"
            ++ mangle_name_as_filename(Name) ++ "_" ++ int_to_string(Arity)
            ++ ".pred" ++ int_to_string(PredIdInt)
            ++ ".proc" ++ int_to_string(ProcIdInt),
        MaybeBaseName = yes(BaseName)
    else
        MaybeBaseName = no
    ).

:- pred output_first_opt_debug(llds_opt_info::in, string::in, proc_label::in,
    list(instruction)::in, counter::in, io::di, io::uo) is det.

output_first_opt_debug(Info, FileName, ProcLabel, Instrs0, Counter, !IO) :-
    io.call_system("mkdir -p " ++ opt_subdir_name, MkdirRes, !IO),
    ( if MkdirRes = ok(0) then
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            counter.allocate(NextLabel, Counter, _),
            opt_debug.msg(FileStream, yes, NextLabel,
                "before optimization", !IO),
            AutoComments = Info ^ lopt_auto_comments,
            opt_debug.maybe_write_instrs(FileStream, yes, AutoComments,
                yes(ProcLabel), Instrs0, !IO),
            io.close_output(FileStream, !IO)
        ;
            Res = error(_),
            unexpected($pred, "cannot open " ++ FileName)
        )
    else
        unexpected($pred, "cannot make " ++ opt_subdir_name)
    ).

:- func opt_subdir_name = string.

opt_subdir_name = "OptSubdir".

:- func num_to_str(int) = string.

num_to_str(N) =
    ( if N < 10 then
        "0" ++ string.int_to_string(N)
    else
        string.int_to_string(N)
    ).

:- pred maybe_opt_debug(llds_opt_info::in, list(instruction)::in, counter::in,
    string::in, string::in, proc_label::in,
    opt_debug_info::in, opt_debug_info::out) is det.

maybe_opt_debug(Info, Instrs, Counter, Suffix, Msg, ProcLabel,
        !OptDebugInfo) :-
    (
        !.OptDebugInfo = opt_debug_info(BaseName, OptNum0, _OptFileName0,
            PrevNum, PrevFileName, PrevInstrs),
        OptNum = OptNum0 + 1,
        OptFileName = BaseName ++ ".opt" ++ num_to_str(OptNum)
            ++ "." ++ Suffix,
        DiffFileName = BaseName ++ ".diff" ++ num_to_str(OptNum)
            ++ "." ++ Suffix,
        ( if Instrs = PrevInstrs then
            Same = yes,
            !:OptDebugInfo = opt_debug_info(BaseName, OptNum, OptFileName,
                PrevNum, PrevFileName, Instrs)
        else
            Same = no,
            !:OptDebugInfo = opt_debug_info(BaseName, OptNum, OptFileName,
                OptNum, OptFileName, Instrs)
        ),
        trace [io(!IO)] (
            io.open_output(OptFileName, Res, !IO),
            (
                Res = ok(FileStream),
                counter.allocate(NextLabel, Counter, _),
                opt_debug.msg(FileStream, yes, NextLabel, Msg, !IO),
                (
                    Same = yes,
                    io.write_string(FileStream,
                        "same as previous version\n", !IO)
                ;
                    Same = no,
                    AutoComments = Info ^ lopt_auto_comments,
                    opt_debug.maybe_write_instrs(FileStream, yes, AutoComments,
                        yes(ProcLabel), Instrs, !IO)
                ),
                io.close_output(FileStream, !IO)
            ;
                Res = error(_),
                unexpected($pred, "cannot open " ++ OptFileName)
            ),
            (
                Same = yes
            ;
                Same = no,
                % Although the -u is not fully portable, it is available on
                % all the systems we intend to use it on, and the main user
                % of --debug-opt (zs) strongly prefers -u to -c.
                DiffCommand = "diff -u '" ++ PrevFileName ++ "' '" ++
                    OptFileName ++ "' > '" ++ DiffFileName ++ "'",
                io.call_system(DiffCommand, _, !IO)
            )
        )
    ;
        !.OptDebugInfo = no_opt_debug_info
    ).

%-----------------------------------------------------------------------------%

:- pred optimize_initial(llds_opt_info::in, set_tree234(label)::in,
    proc_label::in, code_model::in, may_alter_rtti::in,
    counter::in, counter::out, opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out) is det.

optimize_initial(Info, LayoutLabelSet, ProcLabel, CodeModel, MayAlterRtti,
        !LabelNumCounter, !OptDebugInfo, !Instrs) :-
    LabelStr = opt_util.format_proc_label(ProcLabel),
    OptFrames = Info ^ lopt_opt_frames,
    ( if
        OptFrames = opt_frames,
        MayAlterRtti = may_alter_rtti,
        CodeModel = model_non
    then
        VeryVerbose = Info ^ lopt_very_verbose,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream,
                    "%% Optimizing nondet frames for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        frameopt_keep_nondet_frame(ProcLabel, LayoutLabelSet,
            !LabelNumCounter, !Instrs, _Mod),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter, "ndframeopt",
            "after nondet frame opt", ProcLabel, !OptDebugInfo)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred optimize_repeat(llds_opt_info::in, int::in, set_tree234(label)::in,
    proc_label::in, may_alter_rtti::in, counter::in, counter::out,
    opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out) is det.

optimize_repeat(Info, CurIter, LayoutLabelSet, ProcLabel,
        MayAlterRtti, !LabelNumCounter, !OptDebugInfo, !Instrs) :-
    ( if CurIter > 0 then
        NextIter = CurIter - 1,
        ( if NextIter = 0 then
            Final = yes
        else
            Final = no
        ),
        optimize_repeated(Info, Final, LayoutLabelSet, ProcLabel, MayAlterRtti,
            !LabelNumCounter, !OptDebugInfo, !Instrs, Mod),
        (
            Mod = yes,
            optimize_repeat(Info, NextIter, LayoutLabelSet, ProcLabel,
                MayAlterRtti, !LabelNumCounter, !OptDebugInfo, !Instrs)
        ;
            Mod = no
        )
    else
        true
    ).

    % We short-circuit jump sequences before normal peepholing
    % to create more opportunities for use of the tailcall macro.
    %
:- pred optimize_repeated(llds_opt_info::in, bool::in, set_tree234(label)::in,
    proc_label::in, may_alter_rtti::in, counter::in, counter::out,
    opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out, bool::out) is det.

optimize_repeated(Info, Final, LayoutLabelSet, ProcLabel, MayAlterRtti,
        !LabelNumCounter, !OptDebugInfo, !Instrs, Mod) :-
    InstrsAtStart = !.Instrs,
    LabelStr = opt_util.format_proc_label(ProcLabel),
    VeryVerbose = Info ^ lopt_very_verbose,
    OptJump = Info ^ lopt_opt_jumps,
    OptFullJump = Info ^ lopt_opt_fulljumps,
    PessimizeTailCalls = Info ^ lopt_pes_tailcalls,
    CheckedNondetTailCalls = Info ^ lopt_checked_nondet_tailcalls,
    (
        OptJump = opt_jumps,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing jumps for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        optimize_jumps_in_proc(LayoutLabelSet, MayAlterRtti, ProcLabel,
            OptFullJump, Final, PessimizeTailCalls, CheckedNondetTailCalls,
            !LabelNumCounter, !Instrs, Mod1),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "jump", "after jump opt", ProcLabel, !OptDebugInfo)
    ;
        OptJump = do_not_opt_jumps,
        Mod1 = no
    ),
    Peephole = Info ^ lopt_opt_peep,
    (
        Peephole = opt_peep,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing locally for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        GC_Method = Info ^ lopt_gc_method,
        OptPeepMkword = Info ^ lopt_opt_peep_mkword,
        peephole_optimize(GC_Method, OptPeepMkword, !Instrs, Mod2),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "peep", "after peephole", ProcLabel, !OptDebugInfo)
    ;
        Peephole = do_not_opt_peep,
        Mod2 = no
    ),
    OptLabels = Info ^ lopt_opt_labels,
    (
        OptLabels = opt_labels,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing labels for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        labelopt_main(Final, LayoutLabelSet, !Instrs, Mod3),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "label", "after label opt", ProcLabel, !OptDebugInfo)
    ;
        OptLabels = do_not_opt_labels,
        Mod3 = no
    ),
    DupElim = Info ^ lopt_opt_dups,
    (
        DupElim = opt_dups,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing duplicates for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        dupelim_main(ProcLabel, !LabelNumCounter, !Instrs),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "dup", "after duplicates", ProcLabel, !OptDebugInfo)
    ;
        DupElim = do_not_opt_dups
    ),
    ( if Mod1 = no, Mod2 = no, Mod3 = no, !.Instrs = InstrsAtStart then
        Mod = no
    else
        Mod = yes
    ),
    trace [io(!IO)] (
        get_opt_progress_output_stream(Info, ProgressStream, !IO),
        Statistics = Info ^ lopt_detailed_statistics,
        maybe_report_stats(ProgressStream, Statistics, !IO)
    ).

:- pred optimize_middle(llds_opt_info::in, bool::in, set_tree234(label)::in,
    proc_label::in, code_model::in, may_alter_rtti::in,
    counter::in, counter::out, opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out) is det.

optimize_middle(Info, Final, LayoutLabelSet, ProcLabel, CodeModel,
        MayAlterRtti, !LabelNumCounter, !OptDebugInfo, !Instrs) :-
    VeryVerbose = Info ^ lopt_very_verbose,
    LabelStr = opt_util.format_proc_label(ProcLabel),

    OptFrames = Info ^ lopt_opt_frames,
    (
        OptFrames = opt_frames,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing frames for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        FrameOptComments = Info ^ lopt_frameopt_comments,
        (
            ( CodeModel = model_det
            ; CodeModel = model_semi
            ),
            frameopt_main_det_stack(ProcLabel, !LabelNumCounter, !Instrs,
                FrameOptComments, Mod1)
        ;
            CodeModel = model_non,
            frameopt_main_nondet_stack(ProcLabel, !LabelNumCounter, !Instrs,
                FrameOptComments, Mod1)
        ),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "frame", "after frame opt", ProcLabel, !OptDebugInfo),
        Statistics = Info ^ lopt_detailed_statistics,
        trace [io(!IO)] (
            get_opt_progress_output_stream(Info, ProgressStream, !IO),
            maybe_report_stats(ProgressStream, Statistics, !IO)
        ),

        OptFullJump = Info ^ lopt_opt_fulljumps,
        PessimizeTailCalls = Info ^ lopt_pes_tailcalls,
        CheckedNondetTailCalls = Info ^ lopt_checked_nondet_tailcalls,
        ( if
            ( OptFullJump = opt_fulljumps
            ; Mod1 = yes
            )
        then
            (
                VeryVerbose = yes,
                trace [io(!IO)] (
                    get_opt_progress_output_stream(Info, ProgressStream, !IO),
                    io.format(ProgressStream, "%% Optimizing jumps for %s\n",
                        [s(LabelStr)], !IO)
                )
            ;
                VeryVerbose = no
            ),
            optimize_jumps_in_proc(LayoutLabelSet, MayAlterRtti, ProcLabel,
                OptFullJump, Final, PessimizeTailCalls, CheckedNondetTailCalls,
                !LabelNumCounter, !Instrs, _Mod2),
            maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
                "jump", "after jumps", ProcLabel, !OptDebugInfo)
        else
            true
        ),
        (
            Mod1 = yes,
            (
                VeryVerbose = yes,
                trace [io(!IO)] (
                    get_opt_progress_output_stream(Info, ProgressStream, !IO),
                    io.format(ProgressStream, "%% Optimizing labels for %s\n",
                        [s(LabelStr)], !IO)
                )
            ;
                VeryVerbose = no
            ),
            labelopt_main(Final, LayoutLabelSet, !Instrs, _Mod3),
            maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
                "label", "after labels", ProcLabel, !OptDebugInfo)
        ;
            Mod1 = no
        ),
        (
            Mod1 = yes,
            (
                VeryVerbose = yes,
                trace [io(!IO)] (
                    get_opt_progress_output_stream(Info, ProgressStream, !IO),
                    io.format(ProgressStream, "%% Optimizing locally for %s\n",
                        [s(LabelStr)], !IO)
                )
            ;
                VeryVerbose = no
            ),
            GC_Method = Info ^ lopt_gc_method,
            OptPeepMkword = Info ^ lopt_opt_peep_mkword,
            peephole_optimize(GC_Method, OptPeepMkword, !Instrs, _Mod),
            maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
                "peep", "after peephole", ProcLabel, !OptDebugInfo)
        ;
            Mod1 = no
        )
    ;
        OptFrames = do_not_opt_frames
    ),
    UseLocalVars = Info ^ lopt_use_local_vars,
    (
        UseLocalVars = use_local_vars,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing local vars for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        NumRealRRegs = Info ^ lopt_num_real_r_regs,
        AccessThreshold = Info ^ lopt_local_vars_access_threshold,
        AutoComments = Info ^ lopt_auto_comments,
        use_local_vars_proc(!Instrs, NumRealRRegs, AccessThreshold,
            AutoComments, ProcLabel, !LabelNumCounter),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter, "use_local",
            "after use_local_vars", ProcLabel, !OptDebugInfo)
    ;
        UseLocalVars = do_not_use_local_vars
    ).

:- pred optimize_last(llds_opt_info::in, set_tree234(label)::in,
    proc_label::in, counter::in, counter::out, opt_debug_info::in,
    list(instruction)::in, list(instruction)::out) is det.

optimize_last(Info, LayoutLabelSet, ProcLabel,
        !LabelNumCounter, !.OptDebugInfo, !Instrs) :-
    VeryVerbose = Info ^ lopt_very_verbose,
    LabelStr = opt_util.format_proc_label(ProcLabel),

    Reassign = Info ^ lopt_opt_reassign,
    DelaySlot = Info ^ lopt_opt_delay_slots,
    UseLocalVars = Info ^ lopt_use_local_vars,
    StdLabels = Info ^ lopt_std_labels,
    ( if
        ( Reassign = opt_reassign
        ; DelaySlot = opt_delay_slot
        ; UseLocalVars = use_local_vars
        ; StdLabels = standardize_labels
        )
    then
        % We must get rid of any extra labels added by other passes,
        % since they can confuse reassign, wrap_blocks and delay_slot.
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing labels for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        labelopt_main(no, LayoutLabelSet, !Instrs, _Mod1),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "label", "after label opt", ProcLabel, !OptDebugInfo)
    else
        true
    ),
    (
        Reassign = opt_reassign,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing reassign for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        remove_reassign(!Instrs),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "reassign", "after reassign", ProcLabel, !OptDebugInfo)
    ;
        Reassign = do_not_opt_reassign
    ),
    (
        DelaySlot = opt_delay_slot,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Optimizing delay slot for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        fill_branch_delay_slot(!Instrs),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "delay_slot", "after delay slots", ProcLabel, !OptDebugInfo)
    ;
        DelaySlot = do_not_opt_delay_slot
    ),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            get_opt_progress_output_stream(Info, ProgressStream, !IO),
            io.format(ProgressStream, "%% Optimizing returns for %s\n",
                [s(LabelStr)], !IO)
        )
    ;
        VeryVerbose = no
    ),
    combine_decr_sp(!Instrs),
    maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
        "decr_sp", "after combine decr_sp", ProcLabel, !OptDebugInfo),
    (
        StdLabels = standardize_labels,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Standardizing labels for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        standardize_labels(!Instrs, !LabelNumCounter),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "stdlabel", "after standard labels", ProcLabel, !OptDebugInfo)
    ;
        StdLabels = do_not_standardize_labels
    ),
    (
        UseLocalVars = use_local_vars,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_opt_progress_output_stream(Info, ProgressStream, !IO),
                io.format(ProgressStream, "%% Wrapping blocks for %s\n",
                    [s(LabelStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),
        wrap_blocks(!Instrs),
        maybe_opt_debug(Info, !.Instrs, !.LabelNumCounter,
            "wrapblocks", "after wrap blocks", ProcLabel, !.OptDebugInfo, _)
    ;
        UseLocalVars = do_not_use_local_vars
    ).

%-----------------------------------------------------------------------------%

    % Mangle the given name just sufficiently to make it acceptable as a
    % filename.
    %
:- func mangle_name_as_filename(string) = string.

mangle_name_as_filename(Str0) = Str :-
    string.foldl(escape_dir_char, Str0, "", Str).

:- pred escape_dir_char(char::in, string::in, string::out) is det.

escape_dir_char(Char, !Str) :-
    ( if dir.is_directory_separator(Char) then
        !:Str = !.Str ++ "_slash_"
    else
        !:Str = !.Str ++ char_to_string(Char)
    ).

%-----------------------------------------------------------------------------%

:- type llds_opt_info
    --->    llds_opt_info(
                lopt_debug_opt_pred_ids         :: list(string),
                lopt_debug_opt_pred_names       :: list(string),
                lopt_num_real_r_regs            :: int,
                lopt_local_vars_access_threshold :: int,
                lopt_opt_repeat                 :: int,
                lopt_globals                    :: globals,
                lopt_module_name                :: module_name,

                lopt_gc_method                  :: gc_method,

                lopt_debug_opt                  :: bool,

                lopt_auto_comments              :: maybe_auto_comments,
                lopt_frameopt_comments          :: bool,
                lopt_detailed_statistics        :: bool,
                lopt_very_verbose               :: bool,

                lopt_checked_nondet_tailcalls   ::
                                            maybe_opt_checked_nondet_tailcalls,
                lopt_opt_delay_slots            :: maybe_opt_delay_slot,
                lopt_opt_dups                   :: maybe_opt_dups,
                lopt_opt_frames                 :: maybe_opt_frames,
                lopt_opt_jumps                  :: maybe_opt_jumps,
                lopt_opt_fulljumps              :: maybe_opt_fulljumps,
                lopt_opt_labels                 :: maybe_opt_labels,
                lopt_opt_peep                   :: maybe_opt_peep,
                lopt_opt_peep_mkword            :: maybe_opt_peep_mkword,
                lopt_opt_reassign               :: maybe_opt_reassign,
                lopt_pes_tailcalls              :: maybe_pessimize_tailcalls,
                lopt_std_labels                 :: maybe_standardize_labels,
                lopt_use_local_vars             :: maybe_use_local_vars
            ).

:- func init_llds_opt_info(globals, module_name) = llds_opt_info.

init_llds_opt_info(Globals, ModuleName) = Info :-
    globals.lookup_accumulating_option(Globals, debug_opt_pred_id,
        DebugOptPredIdStrs),
    globals.lookup_accumulating_option(Globals, debug_opt_pred_name,
        DebugOptPredNames),
    globals.lookup_int_option(Globals, num_real_r_regs, NumRealRRegs),
    globals.get_opt_tuple(Globals, OptTuple),
    LocalVarAccessThreshold = OptTuple ^ ot_local_var_access_threshold,
    OptRepeat = OptTuple ^ ot_opt_repeat,

    globals.get_gc_method(Globals, GCMethod),

    globals.lookup_bool_option(Globals, debug_opt, DebugOpt),

    globals.lookup_bool_option(Globals, auto_comments, AutoCommentsOption),
    ( AutoCommentsOption = no, AutoComments = no_auto_comments
    ; AutoCommentsOption = yes, AutoComments = auto_comments
    ),
    globals.lookup_bool_option(Globals, frameopt_comments, FrameOptComments),
    globals.lookup_bool_option(Globals, detailed_statistics,
        DetailedStatistics),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),

    CheckedNondetTailCalls = OptTuple ^ ot_opt_checked_nondet_tailcalls,
    OptDelaySlots = OptTuple ^ ot_opt_delay_slot,
    OptDups = OptTuple ^ ot_opt_dups,
    OptFrames = OptTuple ^ ot_opt_frames,
    OptJumps = OptTuple ^ ot_opt_jumps,
    OptFullJumps = OptTuple ^ ot_opt_fulljumps,
    OptLabels = OptTuple ^ ot_opt_labels,
    OptPeep = OptTuple ^ ot_opt_peep,
    OptPeepMkword = OptTuple ^ ot_opt_peep_mkword,
    OptReassign = OptTuple ^ ot_opt_reassign,
    PessimizeTailCalls = OptTuple ^ ot_pessimize_tailcalls,
    StdLabels = OptTuple ^ ot_standardize_labels,
    UseLocalVars = OptTuple ^ ot_use_local_vars,

    Info = llds_opt_info(DebugOptPredIdStrs, DebugOptPredNames,
        NumRealRRegs, LocalVarAccessThreshold, OptRepeat, Globals, ModuleName,
        GCMethod, DebugOpt, AutoComments, FrameOptComments,
        DetailedStatistics, VeryVerbose,
        CheckedNondetTailCalls, OptDelaySlots, OptDups, OptFrames,
        OptJumps, OptFullJumps, OptLabels, OptPeep, OptPeepMkword, OptReassign,
        PessimizeTailCalls, StdLabels, UseLocalVars).

:- pred get_opt_progress_output_stream(llds_opt_info::in,
    io.text_output_stream::out, io::di, io::uo) is det.

get_opt_progress_output_stream(Info, ProgressStream, !IO) :-
    Globals = Info ^ lopt_globals,
    ModuleName = Info ^ lopt_module_name,
    get_progress_output_stream(Globals, ModuleName, ProgressStream, !IO).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.optimize.
%-----------------------------------------------------------------------------%
