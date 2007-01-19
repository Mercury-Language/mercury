%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2007 The University of Melbourne.
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

:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred optimize_procs(global_data::in,
    list(c_procedure)::in, list(c_procedure)::out, io::di, io::uo) is det.

:- pred optimize_proc(global_data::in, c_procedure::in, c_procedure::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.code_model.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
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
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module char.
:- import_module counter.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

optimize_procs(GlobalData, !Procs, !IO) :-
    list.map_foldl(optimize_proc(GlobalData), !Procs, !IO).

optimize_proc(GlobalData, CProc0, CProc, !IO) :-
    some [!OptDebugInfo, !C, !Instrs] (
        CProc0 = c_procedure(Name, Arity, PredProcId, CodeModel, !:Instrs,
            ProcLabel, !:C, MayAlterRtti, CGlobalVars),
        init_opt_debug_info(Name, Arity, PredProcId, ProcLabel,
            !.Instrs, !.C, !:OptDebugInfo, !IO),
        globals.io_lookup_int_option(optimize_repeat, Repeat, !IO),
        (
            global_data_maybe_get_proc_layout(GlobalData,
                PredProcId, ProcLayout)
        ->
            LabelMap = ProcLayout ^ internal_map,
            map.sorted_keys(LabelMap, LayoutLabelNums),
            LayoutLabels = list.map(
                make_internal_label_for_proc_label(ProcLabel),
                LayoutLabelNums),
            set.sorted_list_to_set(LayoutLabels, LayoutLabelSet)
        ;
            set.init(LayoutLabelSet)
        ),
        optimize_initial(LayoutLabelSet, ProcLabel, CodeModel, MayAlterRtti,
            !C, !OptDebugInfo, !Instrs, !IO),
        optimize_repeat(Repeat, LayoutLabelSet, ProcLabel, MayAlterRtti, !C,
            !OptDebugInfo, !Instrs, !IO),
        optimize_middle(yes, LayoutLabelSet, ProcLabel, CodeModel,
            MayAlterRtti, !C, !OptDebugInfo, !Instrs, !IO),
        optimize_last(LayoutLabelSet, ProcLabel, !C, !.OptDebugInfo, !Instrs,
            !IO),
        CProc = c_procedure(Name, Arity, PredProcId, CodeModel, !.Instrs,
            ProcLabel, !.C, MayAlterRtti, CGlobalVars)
    ).

:- func make_internal_label_for_proc_label(proc_label, int) = label.

make_internal_label_for_proc_label(ProcLabel, LabelNum)
    = internal_label(LabelNum, ProcLabel).

%-----------------------------------------------------------------------------%

:- type opt_debug_info
    --->    opt_debug_info(
                string,         % Base file name for the dump files.
                int,            % The number of the last dump file written.
                string,         % The name of this file.
                int,            % The number of the last dump file written
                                % that has the instruction sequence in it.
                string,         % The name of this file.
                list(instruction)
                                % The instruction sequence at the time the
                                % last dump file was written.
            )
    ;       no_opt_debug_info.

:- pred init_opt_debug_info(string::in, int::in, pred_proc_id::in,
    proc_label::in, list(instruction)::in, counter::in, opt_debug_info::out,
    io::di, io::uo) is det.

init_opt_debug_info(Name, Arity, PredProcId, ProcLabel, Instrs0, Counter,
        OptDebugInfo, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, debug_opt, DebugOpt),
    globals.lookup_accumulating_option(Globals, debug_opt_pred_id,
        DebugOptPredIdStrs),
    globals.lookup_accumulating_option(Globals, debug_opt_pred_name,
        DebugOptPredNames),
    PredProcId = proc(PredId, ProcId),
    pred_id_to_int(PredId, PredIdInt),
    proc_id_to_int(ProcId, ProcIdInt),
    (
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
    ->
        BaseName = opt_subdir_name ++ "/"
            ++ mangle_name_as_filename(Name) ++ "_" ++ int_to_string(Arity)
            ++ ".pred" ++ int_to_string(PredIdInt)
            ++ ".proc" ++ int_to_string(ProcIdInt),

        io.call_system("mkdir -p " ++ opt_subdir_name, MkdirRes, !IO),
        ( MkdirRes = ok(0) ->
            true
        ;
            unexpected(this_file, "cannot make " ++ opt_subdir_name)
        ),
        FileName = BaseName ++ ".opt" ++ num_to_str(0),
        io.open_output(FileName, Res, !IO),
        ( Res = ok(FileStream) ->
            io.set_output_stream(FileStream, OutputStream, !IO),
            counter.allocate(NextLabel, Counter, _),
            opt_debug.msg(yes, NextLabel, "before optimization", !IO),
            opt_debug.maybe_dump_instrs(yes, ProcLabel, Instrs0, !IO),
            io.set_output_stream(OutputStream, _, !IO),
            io.close_output(FileStream, !IO)
        ;
            unexpected(this_file, "cannot open " ++ FileName)
        ),
        OptDebugInfo = opt_debug_info(BaseName, 0, FileName, 0, FileName,
            Instrs0)
    ;
        OptDebugInfo = no_opt_debug_info
    ).

:- func opt_subdir_name = string.

opt_subdir_name = "OptSubdir".

:- func num_to_str(int) = string.

num_to_str(N) =
    ( N < 10 ->
        "0" ++ string.int_to_string(N)
    ;
        string.int_to_string(N)
    ).

:- pred maybe_opt_debug(list(instruction)::in, counter::in,
    string::in, string::in, proc_label::in,
    opt_debug_info::in, opt_debug_info::out, io::di, io::uo) is det.

maybe_opt_debug(Instrs, Counter, Suffix, Msg, ProcLabel, !OptDebugInfo, !IO) :-
    (
        !.OptDebugInfo = opt_debug_info(BaseName, OptNum0, _OptFileName0,
            PrevNum, PrevFileName, PrevInstrs),
        ( Instrs = PrevInstrs ->
            Same = yes
        ;
            Same = no
        ),
        OptNum = OptNum0 + 1,
        OptFileName = BaseName ++ ".opt" ++ num_to_str(OptNum)
            ++ "." ++ Suffix,
        DiffFileName = BaseName ++ ".diff" ++ num_to_str(OptNum)
            ++ "." ++ Suffix,
        io.open_output(OptFileName, Res, !IO),
        ( Res = ok(FileStream) ->
            io.set_output_stream(FileStream, OutputStream, !IO),
            counter.allocate(NextLabel, Counter, _),
            opt_debug.msg(yes, NextLabel, Msg, !IO),
            (
                Same = yes,
                io.write_string("same as previous version\n", !IO)
            ;
                Same = no,
                opt_debug.maybe_dump_instrs(yes, ProcLabel, Instrs, !IO)
            ),
            io.set_output_stream(OutputStream, _, !IO),
            io.close_output(FileStream, !IO)
        ;
            ErrorMsg = "cannot open " ++ OptFileName,
            unexpected(this_file, ErrorMsg)
        ),
        (
            Same = yes,
            !:OptDebugInfo = opt_debug_info(BaseName, OptNum, OptFileName,
                PrevNum, PrevFileName, Instrs)
        ;
            Same = no,
            % Although the -u is not fully portable, it is available
            % on all the systems we intend to use it on, and the main user
            % of --debug-opt (zs) strongly prefers -u to -c.
            DiffCommand = "diff -u '" ++ PrevFileName ++ "' '" ++ OptFileName
                ++ "' > '" ++ DiffFileName ++ "'",
            io.call_system(DiffCommand, _, !IO),
            !:OptDebugInfo = opt_debug_info(BaseName, OptNum, OptFileName,
                OptNum, OptFileName, Instrs)
        )
    ;
        !.OptDebugInfo = no_opt_debug_info
    ).

%-----------------------------------------------------------------------------%

:- pred optimize_initial(set(label)::in, proc_label::in, code_model::in,
    may_alter_rtti::in, counter::in, counter::out,
    opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize_initial(LayoutLabelSet, ProcLabel, CodeModel, MayAlterRtti,
        !C, !OptDebugInfo, !Instrs, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    LabelStr = opt_util.format_proc_label(ProcLabel),

    globals.io_lookup_bool_option(optimize_frames, FrameOpt, !IO),
    (
        FrameOpt = yes,
        MayAlterRtti = may_alter_rtti,
        CodeModel = model_non
    ->
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing nondet frames for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        frameopt_keep_nondet_frame(ProcLabel, LayoutLabelSet,
            !C, !Instrs, _Mod),
        maybe_opt_debug(!.Instrs, !.C, "ndframeopt", "after nondet frame opt",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred optimize_repeat(int::in, set(label)::in, proc_label::in,
    may_alter_rtti::in, counter::in, counter::out,
    opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize_repeat(CurIter, LayoutLabelSet, ProcLabel, MayAlterRtti,
        !C, !OptDebugInfo, !Instrs, !IO) :-
    ( CurIter > 0 ->
        NextIter = CurIter - 1,
        ( NextIter = 0 ->
            Final = yes
        ;
            Final = no
        ),
        optimize_repeated(Final, LayoutLabelSet, ProcLabel, MayAlterRtti,
            !C, !OptDebugInfo, !Instrs, Mod, !IO),
        (
            Mod = yes,
            optimize_repeat(NextIter, LayoutLabelSet, ProcLabel, MayAlterRtti,
                !C, !OptDebugInfo, !Instrs, !IO)
        ;
            Mod = no
        )
    ;
        true
    ).

    % We short-circuit jump sequences before normal peepholing
    % to create more opportunities for use of the tailcall macro.
    %
:- pred optimize_repeated(bool::in, set(label)::in, proc_label::in,
    may_alter_rtti::in, counter::in, counter::out,
    opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out, bool::out,
    io::di, io::uo) is det.

optimize_repeated(Final, LayoutLabelSet, ProcLabel, MayAlterRtti,
        !C, !OptDebugInfo, !Instrs, Mod, !IO) :-
    InstrsAtStart = !.Instrs,
    LabelStr = opt_util.format_proc_label(ProcLabel),
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    globals.io_lookup_bool_option(optimize_jumps, Jumpopt, !IO),
    globals.io_lookup_bool_option(optimize_fulljumps, FullJumpopt, !IO),
    globals.io_lookup_bool_option(pessimize_tailcalls,
        PessimizeTailCalls, !IO),
    globals.io_lookup_bool_option(checked_nondet_tailcalls,
        CheckedNondetTailCalls, !IO),
    (
        Jumpopt = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing jumps for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        optimize_jumps_in_proc(LayoutLabelSet, MayAlterRtti, ProcLabel,
            FullJumpopt, Final, PessimizeTailCalls, CheckedNondetTailCalls,
            !C, !Instrs, Mod1),
        maybe_opt_debug(!.Instrs, !.C, "jump", "after jump opt",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        Jumpopt = no,
        Mod1 = no
    ),
    globals.io_lookup_bool_option(optimize_peep, Peephole, !IO),
    (
        Peephole = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing locally for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        globals.io_get_gc_method(GC_Method, !IO),
        peephole.optimize(GC_Method, !Instrs, Mod2),
        maybe_opt_debug(!.Instrs, !.C, "peep", "after peephole",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        Peephole = no,
        Mod2 = no
    ),
    globals.io_lookup_bool_option(optimize_labels, LabelElim, !IO),
    (
        LabelElim = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing labels for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        labelopt_main(Final, LayoutLabelSet, !Instrs, Mod3),
        maybe_opt_debug(!.Instrs, !.C, "label", "after label opt",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        LabelElim = no,
        Mod3 = no
    ),
    globals.io_lookup_bool_option(optimize_dups, DupElim, !IO),
    (
        DupElim = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing duplicates for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        dupelim_main(ProcLabel, !C, !Instrs),
        maybe_opt_debug(!.Instrs, !.C, "dup", "after duplicates",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        DupElim = no
    ),
    ( Mod1 = no, Mod2 = no, Mod3 = no, !.Instrs = InstrsAtStart ->
        Mod = no
    ;
        Mod = yes
    ),
    globals.io_lookup_bool_option(detailed_statistics, Statistics, !IO),
    maybe_report_stats(Statistics, !IO).

:- pred optimize_middle(bool::in, set(label)::in, proc_label::in,
    code_model::in, may_alter_rtti::in, counter::in, counter::out,
    opt_debug_info::in, opt_debug_info::out,
    list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize_middle(Final, LayoutLabelSet, ProcLabel, CodeModel, MayAlterRtti, !C,
        !OptDebugInfo, !Instrs, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    LabelStr = opt_util.format_proc_label(ProcLabel),

    globals.io_lookup_bool_option(optimize_frames, FrameOpt, !IO),
    (
        FrameOpt = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing frames for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        globals.io_get_globals(Globals, !IO),
        (
            ( CodeModel = model_det
            ; CodeModel = model_semi
            ),
            frameopt_main_det_stack(ProcLabel, !C, !Instrs, Globals, Mod1)
        ;
            CodeModel = model_non,
            frameopt_main_nondet_stack(ProcLabel, !C, !Instrs, Globals, Mod1)
        ),
        maybe_opt_debug(!.Instrs, !.C, "frame", "after frame opt",
            ProcLabel, !OptDebugInfo, !IO),
        globals.io_lookup_bool_option(optimize_fulljumps, FullJumpopt, !IO),
        globals.io_lookup_bool_option(pessimize_tailcalls,
            PessimizeTailCalls, !IO),
        globals.io_lookup_bool_option(checked_nondet_tailcalls,
            CheckedNondetTailCalls, !IO),
        (
            ( FullJumpopt = yes
            ; Mod1 = yes
            )
        ->
            (
                VeryVerbose = yes,
                io.write_string("% Optimizing jumps for ", !IO),
                io.write_string(LabelStr, !IO),
                io.write_string("\n", !IO)
            ;
                VeryVerbose = no
            ),
            optimize_jumps_in_proc(LayoutLabelSet, MayAlterRtti, ProcLabel,
                FullJumpopt, Final, PessimizeTailCalls, CheckedNondetTailCalls,
                !C, !Instrs, _Mod2),
            maybe_opt_debug(!.Instrs, !.C, "jump", "after jumps",
                ProcLabel, !OptDebugInfo, !IO)
        ;
            true
        ),
        (
            Mod1 = yes,
            (
                VeryVerbose = yes,
                io.write_string("% Optimizing labels for ", !IO),
                io.write_string(LabelStr, !IO),
                io.write_string("\n", !IO)
            ;
                VeryVerbose = no
            ),
            labelopt_main(Final, LayoutLabelSet, !Instrs, _Mod3),
            maybe_opt_debug(!.Instrs, !.C, "label", "after labels",
                ProcLabel, !OptDebugInfo, !IO)
        ;
            Mod1 = no
        ),
        (
            Mod1 = yes,
            (
                VeryVerbose = yes,
                io.write_string("% Optimizing locally for ", !IO),
                io.write_string(LabelStr, !IO),
                io.write_string("\n", !IO)
            ;
                VeryVerbose = no
            ),
            globals.io_get_gc_method(GC_Method, !IO),
            peephole.optimize(GC_Method, !Instrs, _Mod),
            maybe_opt_debug(!.Instrs, !.C, "peep", "after peephole",
                ProcLabel, !OptDebugInfo, !IO)
        ;
            Mod1 = no
        )
    ;
        FrameOpt = no
    ),
    globals.io_lookup_bool_option(use_local_vars, UseLocalVars, !IO),
    (
        UseLocalVars = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing local vars for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        globals.io_lookup_int_option(num_real_r_regs, NumRealRRegs, !IO),
        globals.io_lookup_int_option(local_var_access_threshold,
            AccessThreshold, !IO),
        globals.io_lookup_bool_option(auto_comments, AutoComments, !IO),
        use_local_vars_proc(!Instrs, NumRealRRegs, AccessThreshold,
            AutoComments, ProcLabel, !C),
        maybe_opt_debug(!.Instrs, !.C, "use_local", "after use_local_vars",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        UseLocalVars = no
    ).

:- pred optimize_last(set(label)::in, proc_label::in,
    counter::in, counter::out, opt_debug_info::in,
    list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize_last(LayoutLabelSet, ProcLabel, !C, !.OptDebugInfo, !Instrs, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    LabelStr = opt_util.format_proc_label(ProcLabel),

    globals.io_lookup_bool_option(optimize_reassign, Reassign, !IO),
    globals.io_lookup_bool_option(optimize_delay_slot, DelaySlot, !IO),
    globals.io_lookup_bool_option(use_local_vars, UseLocalVars, !IO),
    globals.io_lookup_bool_option(standardize_labels, StdLabels, !IO),
    (
        ( Reassign = yes
        ; DelaySlot = yes
        ; UseLocalVars = yes
        ; StdLabels = yes
        )
    ->
        % We must get rid of any extra labels added by other passes,
        % since they can confuse reassign, wrap_blocks and delay_slot.
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing labels for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        labelopt_main(no, LayoutLabelSet, !Instrs, _Mod1),
        maybe_opt_debug(!.Instrs, !.C, "label", "after label opt",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        true
    ),
    (
        Reassign = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing reassign for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        remove_reassign(!Instrs),
        maybe_opt_debug(!.Instrs, !.C, "reassign", "after reassign",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        Reassign = no
    ),
    (
        DelaySlot = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Optimizing delay slot for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        fill_branch_delay_slot(!Instrs),
        maybe_opt_debug(!.Instrs, !.C, "delay_slot", "after delay slots",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        DelaySlot = no
    ),
    (
        VeryVerbose = yes,
        io.write_string("% Optimizing returns for ", !IO),
        io.write_string(LabelStr, !IO),
        io.write_string("\n", !IO)
    ;
        VeryVerbose = no
    ),
    combine_decr_sp(!Instrs),
    maybe_opt_debug(!.Instrs, !.C, "decr_sp", "after combine decr_sp",
        ProcLabel, !OptDebugInfo, !IO),
    (
        StdLabels = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Standardizing labels for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        standardize_labels(!Instrs, !C),
        maybe_opt_debug(!.Instrs, !.C, "stdlabel", "after standard labels",
            ProcLabel, !OptDebugInfo, !IO)
    ;
        StdLabels = no
    ),
    (
        UseLocalVars = yes,
        (
            VeryVerbose = yes,
            io.write_string("% Wrapping blocks for ", !IO),
            io.write_string(LabelStr, !IO),
            io.write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        wrap_blocks(!Instrs),
        maybe_opt_debug(!.Instrs, !.C, "wrapblocks", "after wrap blocks",
            ProcLabel, !.OptDebugInfo, _OptDebugInfo, !IO)
    ;
        UseLocalVars = no
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
    ( dir.is_directory_separator(Char) ->
        !:Str = !.Str ++ "_slash_"
    ;
        !:Str = !.Str ++ char_to_string(Char)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "optimize".

%-----------------------------------------------------------------------------%
:- end_module optimize.
%-----------------------------------------------------------------------------%
