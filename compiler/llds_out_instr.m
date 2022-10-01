%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: llds_out_instr.m.
% Main authors: conway, fjh, zs.
%
% This module defines the routines for printing out LLDS instructions.
%
%----------------------------------------------------------------------------%

:- module ll_backend.llds_out.llds_out_instr.
:- interface.

:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.llds_out_util.

:- import_module io.
:- import_module list.
:- import_module set_tree234.

%----------------------------------------------------------------------------%

:- pred output_record_instruction_decls(llds_out_info::in,
    io.text_output_stream::in, instruction::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- type label_output_info
    --->    label_output_info(
                % The entry label of the procedure we are in, which is the
                % caller of every call made in the procedure.
                loi_caller_label            :: label,

                % The set of labels that are continuation labels for
                % calls, nondet disjunctions, forks or joins.
                loi_cont_labels             :: set_tree234(label),

                % The set of labels at which we should start a while loop.
                % Some of these should be defined as labels, and some should
                % not, as decided by the following field.
                loi_while_labels            :: set_tree234(label),

                % The set of labels at which we should start a while loop,
                % but which should not be defined, because they are referred
                % to only by control transfers that get turned into continue
                % statements. This set should be a subset of loi_while_labels.
                loi_undef_while_labels      :: set_tree234(label)
            ).

:- type after_layout_label
    --->    not_after_layout_label
    ;       after_layout_label.

:- pred output_instruction_list(llds_out_info::in, io.text_output_stream::in,
    list(instruction)::in, label_output_info::in, after_layout_label::in,
    io::di, io::uo) is det.

    % Output an instruction and the comment.
    % This predicate is provided for debugging use only.
    %
:- pred output_debug_instruction_and_comment(llds_out_info::in,
    io.text_output_stream::in, instr::in, string::in,
    io::di, io::uo) is det.

    % Output an instruction.
    % Same as the above predicate, only without the comment.
    % This predicate is provided for debugging use only.
    %
:- pred output_debug_instruction(llds_out_info::in, io.text_output_stream::in,
    instr::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.export.
:- import_module backend_libs.name_mangle.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module ll_backend.pragma_c_gen.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%----------------------------------------------------------------------------%
%
% Declare the things inside instructions that need to be declared.
%

output_record_instruction_decls(Info, Stream, Instr, !DeclSet, !IO) :-
    Instr = llds_instr(Uinstr, _),
    output_record_instr_decls(Info, Stream, Uinstr, !DeclSet, !IO).

:- pred output_record_instr_decls(llds_out_info::in, io.text_output_stream::in,
    instr::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_instr_decls(Info, Stream, Instr, !DeclSet, !IO) :-
    (
        ( Instr = comment(_)
        ; Instr = livevals(_)
        ; Instr = arbitrary_c_code(_, _, _)
        ; Instr = label(_)
        ; Instr = push_region_frame(_, _)
        ; Instr = use_and_maybe_pop_region_frame(_, _)
        ; Instr = discard_ticket
        ; Instr = prune_ticket
        ; Instr = incr_sp(_, _, _)
        ; Instr = decr_sp(_)
        ; Instr = decr_sp_and_return(_)
        )
    ;
        Instr = block(_TempR, _TempF, Instrs),
        list.foldl2(output_record_instruction_decls(Info, Stream), Instrs,
            !DeclSet, !IO)
    ;
        Instr = assign(Lval, Rval),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO),
        output_record_rval_decls(Info, Stream, Rval, !DeclSet, !IO)
    ;
        Instr = keep_assign(Lval, Rval),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO),
        output_record_rval_decls(Info, Stream, Rval, !DeclSet, !IO)
    ;
        Instr = llcall(Target, ContLabel, _, _, _, _),
        output_record_code_addr_decls(Info, Stream, Target, !DeclSet, !IO),
        output_record_code_addr_decls(Info, Stream, ContLabel, !DeclSet, !IO)
    ;
        Instr = mkframe(_FrameInfo, MaybeFailureContinuation),
        (
            MaybeFailureContinuation = yes(FailureContinuation),
            output_record_code_addr_decls(Info, Stream, FailureContinuation,
                !DeclSet, !IO)
        ;
            MaybeFailureContinuation = no
        )
    ;
        Instr = goto(CodeAddr),
        output_record_code_addr_decls(Info, Stream, CodeAddr, !DeclSet, !IO)
    ;
        Instr = computed_goto(Rval, _MaybeLabels),
        output_record_rval_decls(Info, Stream, Rval, !DeclSet, !IO)
    ;
        Instr = if_val(Rval, Target),
        output_record_rval_decls(Info, Stream, Rval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, Stream, Target, !DeclSet, !IO)
    ;
        Instr = save_maxfr(Lval),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO)
    ;
        Instr = restore_maxfr(Lval),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO)
    ;
        Instr = incr_hp(Lval, _Ptag, _, Rval, _, _, MaybeRegionRval,
            MaybeReuse),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO),
        output_record_rval_decls(Info, Stream, Rval, !DeclSet, !IO),
        (
            MaybeRegionRval = yes(RegionRval),
            output_record_rval_decls(Info, Stream, RegionRval, !DeclSet, !IO)
        ;
            MaybeRegionRval = no
        ),
        (
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            output_record_rval_decls(Info, Stream, ReuseRval, !DeclSet, !IO),
            (
                MaybeFlagLval = yes(FlagLval),
                output_record_lval_decls(Info, Stream, FlagLval, !DeclSet, !IO)
            ;
                MaybeFlagLval = no
            )
        ;
            MaybeReuse = no_llds_reuse
        )
    ;
        ( Instr = mark_hp(Lval)
        ; Instr = store_ticket(Lval)
        ; Instr = mark_ticket_stack(Lval)
        ; Instr = init_sync_term(Lval, _NumBranches, _ConjIdSlotNum)
        ),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO)
    ;
        ( Instr = restore_hp(Rval)
        ; Instr = free_heap(Rval)
        ; Instr = prune_tickets_to(Rval)
        ; Instr = reset_ticket(Rval, _Reason)
        ),
        output_record_rval_decls(Info, Stream, Rval, !DeclSet, !IO)
    ;
        Instr = region_fill_frame(_FillOp, _EmbeddedFrame, IdRval,
            NumLval, AddrLval),
        output_record_rval_decls(Info, Stream, IdRval, !DeclSet, !IO),
        output_record_lval_decls(Info, Stream, NumLval, !DeclSet, !IO),
        output_record_lval_decls(Info, Stream, AddrLval, !DeclSet, !IO)
    ;
        Instr = region_set_fixed_slot(_SetOp, _EmbeddedFrame, ValueRval),
        output_record_rval_decls(Info, Stream, ValueRval, !DeclSet, !IO)
    ;
        Instr = foreign_proc_code(_, Comps, _, _, _, _, _, _, _, _),
        list.foldl2(output_record_foreign_proc_component_decls(Info, Stream),
            Comps, !DeclSet, !IO)
    ;
        Instr = fork_new_child(Lval, Child),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, Stream, code_label(Child),
            !DeclSet, !IO)
    ;
        Instr = join_and_continue(Lval, Label),
        output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, Stream, code_label(Label),
            !DeclSet, !IO)
    ;
        Instr = lc_create_loop_control(_, LCLval),
        output_record_lval_decls(Info, Stream, LCLval, !DeclSet, !IO)
    ;
        Instr = lc_wait_free_slot(LCRval, LCSLval, InternalLabel),
        output_record_rval_decls(Info, Stream, LCRval, !DeclSet, !IO),
        output_record_lval_decls(Info, Stream, LCSLval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, Stream, code_label(InternalLabel),
            !DeclSet, !IO)
    ;
        Instr = lc_spawn_off(LCRval, LCSRval, ChildLabel),
        output_record_rval_decls(Info, Stream, LCRval, !DeclSet, !IO),
        output_record_rval_decls(Info, Stream, LCSRval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, Stream, code_label(ChildLabel),
            !DeclSet, !IO)
    ;
        Instr = lc_join_and_terminate(LCRval, LCSRval),
        output_record_rval_decls(Info, Stream, LCRval, !DeclSet, !IO),
        output_record_rval_decls(Info, Stream, LCSRval, !DeclSet, !IO)
    ).

:- pred output_record_foreign_proc_component_decls(llds_out_info::in,
    io.text_output_stream::in, foreign_proc_component::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_foreign_proc_component_decls(Info, Stream, Component,
        !DeclSet, !IO) :-
    (
        Component = foreign_proc_inputs(Inputs),
        output_record_foreign_proc_input_rval_decls(Info, Stream, Inputs,
            !DeclSet, !IO)
    ;
        Component = foreign_proc_outputs(Outputs),
        output_record_foreign_proc_output_lval_decls(Info, Stream, Outputs,
            !DeclSet, !IO)
    ;
        ( Component = foreign_proc_raw_code(_, _, _, _)
        ; Component = foreign_proc_user_code(_, _, _)
        ; Component = foreign_proc_fail_to(_)
        ; Component = foreign_proc_alloc_id(_)
        ; Component = foreign_proc_noop
        )
    ).

%----------------------------------------------------------------------------%
%
% Output a list of instructions.
%

output_instruction_list(_, _, [], _, _, !IO).
output_instruction_list(Info, Stream, [Instr | Instrs], LabelOutputInfo,
        AfterLayoutLabel0, !IO) :-
    Instr = llds_instr(Uinstr, Comment),
    ( if Uinstr = label(Label) then
        InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
        ( if map.search(InternalLabelToLayoutMap, Label, _) then
            AfterLayoutLabel = after_layout_label
        else
            AfterLayoutLabel = not_after_layout_label
        ),
        ( if
            AfterLayoutLabel0 = after_layout_label,
            AfterLayoutLabel = after_layout_label
        then
            % Make sure that the addresses of the two labels are distinct.
            io.write_string(Stream, "\tMR_dummy_function_call();\n", !IO)
        else
            true
        ),
        WhileLabels = LabelOutputInfo ^ loi_while_labels,
        ( if set_tree234.contains(WhileLabels, Label) then
            UndefWhileLabels = LabelOutputInfo ^ loi_undef_while_labels,
            ( if set_tree234.contains(UndefWhileLabels, Label) then
                true
            else
                output_instruction_and_comment(Info, Stream, Uinstr, Comment,
                    LabelOutputInfo, !IO)
            ),
            io.write_string(Stream, "\twhile (1) {\n", !IO),
            output_instruction_list_while(Info, Stream, Label, Instrs,
                AfterWhileInstrs, LabelOutputInfo, !IO),
            io.write_string(Stream, "\t} /* end while */\n", !IO),
            output_instruction_list(Info, Stream,
                AfterWhileInstrs, LabelOutputInfo, not_after_layout_label, !IO)
        else
            output_instruction_and_comment(Info, Stream, Uinstr, Comment,
                LabelOutputInfo, !IO),
            output_instruction_list(Info, Stream, Instrs, LabelOutputInfo,
                AfterLayoutLabel, !IO)
        )
    else if
        Instrs = [Instr1 | Instrs1],
        Instr1 = llds_instr(Uinstr1, _),
        is_aligned_float_dword_assignment(Uinstr, Uinstr1, Lval, Rval)
    then
        output_float_dword_assignment(Info, Stream, Lval, Rval, !IO),
        AfterLayoutLabel = not_after_layout_label,
        output_instruction_list(Info, Stream, Instrs1, LabelOutputInfo,
            AfterLayoutLabel, !IO)
    else
        output_instruction_and_comment(Info, Stream, Uinstr, Comment,
            LabelOutputInfo, !IO),
        ( if Uinstr = comment(_) then
            AfterLayoutLabel = AfterLayoutLabel0
        else
            AfterLayoutLabel = not_after_layout_label
        ),
        output_instruction_list(Info, Stream, Instrs, LabelOutputInfo,
            AfterLayoutLabel, !IO)
    ).

:- pred output_instruction_list_while(llds_out_info::in,
    io.text_output_stream::in, label::in,
    list(instruction)::in, list(instruction)::out,
    label_output_info::in, io::di, io::uo) is det.

output_instruction_list_while(_, Stream, _, [], [], _, !IO) :-
    io.write_string(Stream, "\tbreak;\n", !IO).
output_instruction_list_while(Info, Stream, Label, [Instr | Instrs],
        AfterWhileInstrs, LabelOutputInfo, !IO) :-
    Instr = llds_instr(Uinstr, Comment),
    ( if Uinstr = label(_) then
        io.write_string(Stream, "\tbreak;\n", !IO),
        AfterWhileInstrs = [Instr | Instrs]
    else if Uinstr = goto(code_label(Label)) then
        io.write_string(Stream, "\t/* continue */\n", !IO),
        AfterWhileInstrs = Instrs
    else if Uinstr = if_val(Rval, code_label(Label)) then
        io.write_string(Stream, "\tif (", !IO),
        output_test_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")\n\t\tcontinue;\n", !IO),
        AutoComments = Info ^ lout_auto_comments,
        ( if
            AutoComments = auto_comments,
            Comment \= ""
        then
            io.format(Stream, "\t\t/* %s */\n", [s(Comment)], !IO)
        else
            true
        ),
        output_instruction_list_while(Info, Stream, Label, Instrs,
            AfterWhileInstrs, LabelOutputInfo, !IO)
    else if Uinstr = block(TempR, TempF, BlockInstrs) then
        output_block_start(Stream, TempR, TempF, !IO),
        output_instruction_list_while_block(Info, Stream, BlockInstrs, Label,
            LabelOutputInfo, !IO),
        output_block_end(Stream, !IO),
        output_instruction_list_while(Info, Stream, Label, Instrs,
            AfterWhileInstrs, LabelOutputInfo, !IO)
    else
        output_instruction_and_comment(Info, Stream, Uinstr, Comment,
            LabelOutputInfo, !IO),
        output_instruction_list_while(Info, Stream, Label, Instrs,
            AfterWhileInstrs, LabelOutputInfo, !IO)
    ).

:- pred output_instruction_list_while_block(llds_out_info::in,
    io.text_output_stream::in, list(instruction)::in,
    label::in, label_output_info::in, io::di, io::uo) is det.

output_instruction_list_while_block(_, _, [], _, _, !IO).
output_instruction_list_while_block(Info, Stream, [Instr | Instrs], Label,
        LabelOutputInfo, !IO) :-
    Instr = llds_instr(Uinstr, Comment),
    ( if Uinstr = label(_) then
        unexpected($pred, "label in block")
    else if Uinstr = goto(code_label(Label)) then
        io.write_string(Stream, "\tcontinue;\n", !IO),
        expect(unify(Instrs, []), $pred, "code after goto")
    else if Uinstr = if_val(Rval, code_label(Label)) then
        io.write_string(Stream, "\tif (", !IO),
        output_test_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")\n\t\tcontinue;\n", !IO),
        AutoComments = Info ^ lout_auto_comments,
        ( if
            AutoComments = auto_comments,
            Comment \= ""
        then
            io.format(Stream, "\t\t/* %s */\n", [s(Comment)], !IO)
        else
            true
        ),
        output_instruction_list_while_block(Info, Stream, Instrs, Label,
            LabelOutputInfo, !IO)
    else if Uinstr = block(_, _, _) then
        unexpected($pred, "block in block")
    else
        output_instruction_and_comment(Info, Stream, Uinstr, Comment,
            LabelOutputInfo, !IO),
        output_instruction_list_while_block(Info, Stream, Instrs, Label,
            LabelOutputInfo, !IO)
    ).

:- pred is_aligned_float_dword_assignment(instr::in, instr::in, lval::out,
    rval::out) is semidet.

is_aligned_float_dword_assignment(InstrA, InstrB, LvalA, Rval) :-
    InstrA = assign(LvalA, RvalA),
    InstrB = assign(LvalB, RvalB),
    RvalA = unop(dword_float_get_word0, Rval),
    RvalB = unop(dword_float_get_word1, Rval),
    LvalA = field(MaybePtag, Address, const(llconst_int(Offset))),
    LvalB = field(MaybePtag, Address, const(llconst_int(Offset + 1))),
    % Only output an aligned memory reference to a double-word,
    % i.e. fields at even word offsets from the start of the cell.
    int.even(Offset).

:- pred output_float_dword_assignment(llds_out_info::in,
    io.text_output_stream::in, lval::in, rval::in, io::di, io::uo) is det.

output_float_dword_assignment(Info, Stream, Lval, Rval, !IO) :-
    % This looks neater than two statements to assign a double precision float,
    % but can only be if the address is aligned for the target architecture.

    io.write_string(Stream, "\t* (MR_Float *) &(", !IO),
    output_lval_for_assign(Info, Stream, Lval, Type, !IO),
    expect(unify(Type, lt_word), $pred, "expected word"),
    io.write_string(Stream, ") = ", !IO),
    output_rval_as_type(Info, Rval, lt_float, Stream, !IO),
    io.write_string(Stream, ";\n", !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.write_string(Stream, "\t\t/* assigning float dword */\n", !IO)
    ;
        AutoComments = no_auto_comments
    ).

%----------------------------------------------------------------------------%
%
% Output an instruction for debugging.
%

    % output_debug_instruction_and_comment/5 is only for debugging.
    % Normally we use output_instruction_and_comment/6.
    %
output_debug_instruction_and_comment(Info, Stream, Instr, Comment, !IO) :-
    DummyModule = unqualified("DEBUG"),
    DummyPredName = "DEBUG",
    proc_id_to_int(hlds_pred.initial_proc_id, InitialProcIdInt),
    ProcLabel = ordinary_proc_label(DummyModule, pf_predicate, DummyModule,
        DummyPredName, 0, InitialProcIdInt),
    CallerLabel = entry_label(entry_label_local, ProcLabel),
    ContLabels = set_tree234.init,
    WhileLabels = set_tree234.init,
    UndefWhileLabels = set_tree234.init,
    LabelOutputInfo = label_output_info(CallerLabel, ContLabels,
        WhileLabels, UndefWhileLabels),
    output_instruction_and_comment(Info, Stream, Instr, Comment,
        LabelOutputInfo, !IO).

    % output_debug_instruction/3 is only for debugging.
    % Normally we use output_instruction/4.
    %
output_debug_instruction(Info, Stream, Instr, !IO) :-
    DummyModule = unqualified("DEBUG"),
    DummyPredName = "DEBUG",
    proc_id_to_int(hlds_pred.initial_proc_id, InitialProcIdInt),
    ProcLabel = ordinary_proc_label(DummyModule, pf_predicate, DummyModule,
        DummyPredName, 0, InitialProcIdInt),
    CallerLabel = entry_label(entry_label_local, ProcLabel),
    ContLabels = set_tree234.init,
    WhileLabels = set_tree234.init,
    UndefWhileLabels = set_tree234.init,
    LabelOutputInfo = label_output_info(CallerLabel, ContLabels,
        WhileLabels, UndefWhileLabels),
    output_instruction(Info, Stream, Instr, LabelOutputInfo, !IO).

%----------------------------------------------------------------------------%
%
% Output an instruction and a comment.
%

:- pred output_instruction_and_comment(llds_out_info::in,
    io.text_output_stream::in, instr::in, string::in, label_output_info::in,
    io::di, io::uo) is det.

output_instruction_and_comment(Info, Stream, Instr, Comment, LabelOutputInfo,
        !IO) :-
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = no_auto_comments,
        ( if
            ( Instr = comment(_)
            ; Instr = livevals(_)
            )
        then
            true
        else
            output_instruction(Info, Stream, Instr, LabelOutputInfo, !IO)
        )
    ;
        AutoComments = auto_comments,
        output_instruction(Info, Stream, Instr, LabelOutputInfo, !IO),
        ( if Comment = "" then
            true
        else
            io.format(Stream, "\t\t/* %s */\n", [s(Comment)], !IO)
        )
    ).

%----------------------------------------------------------------------------%
%
% Output a single instruction.
%

:- pred output_instruction(llds_out_info::in, io.text_output_stream::in,
    instr::in, label_output_info::in, io::di, io::uo) is det.

output_instruction(Info, Stream, Instr, LabelOutputInfo, !IO) :-
    (
        Instr = comment(Comment),
        % Ensure that any comments embedded inside Comment are made safe, i.e.
        % prevent the closing of embedded comments from closing the outer
        % comment. The fact that the code here is not very efficient doesn't
        % matter since we write out comments only with --auto-comments,
        % which we enable only when we want to debug the generated C code.
        io.write_string(Stream, "/* ", !IO),
        string.to_char_list(Comment, CommentChars),
        output_comment_chars(Stream, '*', CommentChars, !IO),
        io.write_string(Stream, " */\n", !IO)
    ;
        Instr = livevals(LiveVals),
        io.write_string(Stream, "/*\n* Live lvalues:\n", !IO),
        set.to_sorted_list(LiveVals, LiveValsList),
        output_livevals(Info, Stream, LiveValsList, !IO),
        io.write_string(Stream, "*/\n", !IO)
    ;
        Instr = block(TempR, TempF, Instrs),
        output_block_start(Stream, TempR, TempF, !IO),
        LabelOutputInfo = label_output_info(CallerLabel, ContLabels, _, _),
        BlockLabelOutputInfo = label_output_info(CallerLabel, ContLabels,
            set_tree234.init, set_tree234.init),
        output_instruction_list(Info, Stream, Instrs, BlockLabelOutputInfo,
            not_after_layout_label, !IO),
        output_block_end(Stream, !IO)
    ;
        (
            Instr = assign(Lval, Rval)
        ;
            Instr = keep_assign(Lval, Rval)
        ),
        io.write_string(Stream, "\t", !IO),
        output_lval_for_assign(Info, Stream, Lval, Type, !IO),
        io.write_string(Stream, " = ", !IO),
        output_rval_as_type(Info, Rval, Type, Stream, !IO),
        io.write_string(Stream, ";\n", !IO)
    ;
        Instr = llcall(Target, ContLabel, LiveVals, _, _, _),
        CallerLabel = LabelOutputInfo ^ loi_caller_label,
        output_call(Info, Stream, Target, ContLabel, CallerLabel, !IO),
        output_gc_livevals(Info, Stream, LiveVals, !IO)
    ;
        Instr = arbitrary_c_code(_, _, C_Code),
        io.write_string(Stream, "\t", !IO),
        io.write_string(Stream, C_Code, !IO)
    ;
        Instr = mkframe(FrameInfo, MaybeFailCont),
        (
            FrameInfo = ordinary_frame(Msg, Num),
            (
                MaybeFailCont = yes(FailCont),
                io.write_string(Stream, "\tMR_mkframe(", !IO),
                output_quoted_string_c(Stream, Msg, !IO),
                io.write_string(Stream, ", ", !IO),
                io.write_int(Stream, Num, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_code_addr(Stream, FailCont, !IO),
                io.write_string(Stream, ");\n", !IO)
            ;
                MaybeFailCont = no,
                io.write_string(Stream, "\tMR_mkframe_no_redoip(", !IO),
                output_quoted_string_c(Stream, Msg, !IO),
                io.write_string(Stream, ", ", !IO),
                io.write_int(Stream, Num, !IO),
                io.write_string(Stream, ");\n", !IO)
            )
        ;
            FrameInfo = temp_frame(Kind),
            (
                Kind = det_stack_proc,
                io.write_string(Stream, "\tMR_mkdettempframe(", !IO),
                (
                    MaybeFailCont = yes(FailCont),
                    output_code_addr(Stream, FailCont, !IO)
                ;
                    MaybeFailCont = no,
                    unexpected($pred, "no failcont")
                ),
                io.write_string(Stream, ");\n", !IO)
            ;
                Kind = nondet_stack_proc,
                io.write_string(Stream, "\tMR_mktempframe(", !IO),
                (
                    MaybeFailCont = yes(FailCont),
                    output_code_addr(Stream, FailCont, !IO)
                ;
                    MaybeFailCont = no,
                    unexpected($pred, "no failcont")
                ),
                io.write_string(Stream, ");\n", !IO)
            )
        )
    ;
        Instr = label(Label),
        output_label_defn(Stream, Label, !IO),
        LocalThreadEngineBase = Info ^ lout_local_thread_engine_base,
        (
            LocalThreadEngineBase = use_local_thread_engine_base,
            io.write_string(Stream,
                "\tMR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE\n", !IO)
        ;
            LocalThreadEngineBase = do_not_use_local_thread_engine_base
        ),
        maybe_output_update_prof_counter(Info, Stream, Label,
            LabelOutputInfo, !IO)
    ;
        Instr = goto(CodeAddr),
        CallerLabel = LabelOutputInfo ^ loi_caller_label,
        io.write_string(Stream, "\t", !IO),
        output_goto(Info, Stream, CodeAddr, CallerLabel, !IO)
    ;
        Instr = computed_goto(Rval, MaybeLabels),
        io.write_string(Stream, "\tMR_COMPUTED_GOTO(", !IO),
        output_rval_as_type(Info, Rval, lt_int(int_type_uint), Stream, !IO),
        io.write_string(Stream, ",\n\t\t", !IO),
        output_label_list_or_not_reached(Stream, MaybeLabels, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = if_val(Rval, Target),
        CallerLabel = LabelOutputInfo ^ loi_caller_label,
        io.write_string(Stream, "\tif (", !IO),
        output_test_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ") {\n\t\t", !IO),
        output_goto(Info, Stream, Target, CallerLabel, !IO),
        io.write_string(Stream, "\t}\n", !IO)
    ;
        Instr = save_maxfr(Lval),
        io.write_string(Stream, "\tMR_save_maxfr(", !IO),
        output_lval(Info, Stream, Lval, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = restore_maxfr(Lval),
        io.write_string(Stream, "\tMR_restore_maxfr(", !IO),
        output_lval(Info, Stream, Lval, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = incr_hp(Lval, MaybePtag, MaybeOffset, SizeRval, MaybeAllocId,
            MayUseAtomicAlloc, MaybeRegionRval, MaybeReuse),
        io.write_string(Stream, "\t", !IO),
        (
            MaybeReuse = no_llds_reuse,
            output_incr_hp_no_reuse(Info, Stream, Lval, MaybePtag, MaybeOffset,
                SizeRval, MaybeAllocId, MayUseAtomicAlloc, MaybeRegionRval,
                LabelOutputInfo, !IO)
        ;
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            (
                MaybePtag = no,
                (
                    MaybeFlagLval = yes(FlagLval),
                    io.write_string(Stream,
                        "MR_reuse_or_alloc_heap_flag(", !IO),
                    output_lval_as_word(Info, Stream, Lval, !IO),
                    io.write_string(Stream, ", ", !IO),
                    output_lval_as_word(Info, Stream, FlagLval, !IO)
                ;
                    MaybeFlagLval = no,
                    io.write_string(Stream, "MR_reuse_or_alloc_heap(", !IO),
                    output_lval_as_word(Info, Stream, Lval, !IO)
                )
            ;
                MaybePtag = yes(Ptag),
                (
                    MaybeFlagLval = yes(FlagLval),
                    io.write_string(Stream,
                        "MR_tag_reuse_or_alloc_heap_flag(", !IO),
                    output_lval_as_word(Info, Stream, Lval, !IO),
                    io.write_string(Stream, ", ", !IO),
                    output_ptag(Stream, Ptag, !IO),
                    io.write_string(Stream, ", ", !IO),
                    output_lval_as_word(Info, Stream, FlagLval, !IO)
                ;
                    MaybeFlagLval = no,
                    io.write_string(Stream, "MR_tag_reuse_or_alloc_heap(", !IO),
                    output_lval_as_word(Info, Stream, Lval, !IO),
                    io.write_string(Stream, ", ", !IO),
                    output_ptag(Stream, Ptag, !IO)
                )
            ),
            io.write_string(Stream, ", ", !IO),
            output_rval(Info, ReuseRval, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            output_incr_hp_no_reuse(Info, Stream, Lval, MaybePtag, MaybeOffset,
                SizeRval, MaybeAllocId, MayUseAtomicAlloc, MaybeRegionRval,
                LabelOutputInfo, !IO),
            io.write_string(Stream, ")", !IO)
        ),
        io.write_string(Stream, ";\n", !IO)
    ;
        Instr = mark_hp(Lval),
        io.write_string(Stream, "\tMR_mark_hp(", !IO),
        output_lval_as_word(Info, Stream, Lval, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = restore_hp(Rval),
        io.write_string(Stream, "\tMR_restore_hp(", !IO),
        output_rval_as_type(Info, Rval, lt_word, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = free_heap(Rval),
        io.write_string(Stream, "\tMR_free_heap(", !IO),
        output_rval_as_type(Info, Rval, lt_data_ptr, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = push_region_frame(StackId, EmbeddedFrame),
        (
            StackId = region_stack_ite,
            io.write_string(Stream, "\tMR_push_region_ite_frame", !IO)
        ;
            StackId = region_stack_disj,
            io.write_string(Stream, "\tMR_push_region_disj_frame", !IO)
        ;
            StackId = region_stack_commit,
            io.write_string(Stream, "\tMR_push_region_commit_frame", !IO)
        ),
        io.write_string(Stream, "(", !IO),
        output_embedded_frame_addr(Info, Stream, EmbeddedFrame, !IO),
        io.write_string(Stream, ");", !IO),

        % The comment is to make the code easier to debug;
        % we can stop printing it out once that has been done.
        EmbeddedFrame = embedded_stack_frame_id(_StackId,
            FirstSlot, LastSlot),
        Comment = " /* " ++ int_to_string(FirstSlot) ++ ".." ++
            int_to_string(LastSlot) ++ " */",
        io.write_string(Stream, Comment, !IO),

        io.write_string(Stream, "\n", !IO)
    ;
        Instr = region_fill_frame(FillOp, EmbeddedFrame, IdRval,
            NumLval, AddrLval),
        (
            FillOp = region_fill_ite_protect,
            io.write_string(Stream, "\tMR_region_fill_ite_protect", !IO)
        ;
            FillOp = region_fill_ite_snapshot(removed_at_start_of_else),
            io.write_string(Stream,
                "\tMR_region_fill_ite_snapshot_removed", !IO)
        ;
            FillOp = region_fill_ite_snapshot(not_removed_at_start_of_else),
            io.write_string(Stream,
                "\tMR_region_fill_ite_snapshot_not_removed", !IO)
        ;
            FillOp = region_fill_semi_disj_protect,
            io.write_string(Stream, "\tMR_region_fill_semi_disj_protect", !IO)
        ;
            FillOp = region_fill_disj_snapshot,
            io.write_string(Stream, "\tMR_region_fill_disj_snapshot", !IO)
        ;
            FillOp = region_fill_commit,
            io.write_string(Stream, "\tMR_region_fill_commit", !IO)
        ),
        io.write_string(Stream, "(", !IO),
        output_embedded_frame_addr(Info, Stream, EmbeddedFrame, !IO),
        io.write_string(Stream, ", ", !IO),
        output_rval(Info, IdRval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        output_lval(Info, Stream, NumLval, !IO),
        io.write_string(Stream, ", ", !IO),
        output_lval(Info, Stream, AddrLval, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = region_set_fixed_slot(SetOp, EmbeddedFrame, ValueRval),
        (
            SetOp = region_set_ite_num_protects,
            io.write_string(Stream, "\tMR_region_set_ite_num_protects", !IO)
        ;
            SetOp = region_set_ite_num_snapshots,
            io.write_string(Stream, "\tMR_region_set_ite_num_snapshots", !IO)
        ;
            SetOp = region_set_disj_num_protects,
            io.write_string(Stream, "\tMR_region_set_disj_num_protects", !IO)
        ;
            SetOp = region_set_disj_num_snapshots,
            io.write_string(Stream, "\tMR_region_set_disj_num_snapshots", !IO)
        ;
            SetOp = region_set_commit_num_entries,
            io.write_string(Stream, "\tMR_region_set_commit_num_entries", !IO)
        ),
        io.write_string(Stream, "(", !IO),
        output_embedded_frame_addr(Info, Stream, EmbeddedFrame, !IO),
        io.write_string(Stream, ", ", !IO),
        output_rval(Info, ValueRval, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = use_and_maybe_pop_region_frame(UseOp, EmbeddedFrame),
        (
            UseOp = region_ite_then(region_ite_semidet_cond),
            io.write_string(Stream, "\tMR_use_region_ite_then_semidet", !IO)
        ;
            UseOp = region_ite_then(region_ite_nondet_cond),
            io.write_string(Stream, "\tMR_use_region_ite_then_nondet", !IO)
        ;
            UseOp = region_ite_else(region_ite_semidet_cond),
            io.write_string(Stream, "\tMR_use_region_ite_else_semidet", !IO)
        ;
            UseOp = region_ite_else(region_ite_nondet_cond),
            io.write_string(Stream, "\tMR_use_region_ite_else_nondet", !IO)
        ;
            UseOp = region_ite_nondet_cond_fail,
            io.write_string(Stream, "\tMR_use_region_ite_nondet_cond_fail",
                !IO)
        ;
            UseOp = region_disj_later,
            io.write_string(Stream, "\tMR_use_region_disj_later", !IO)
        ;
            UseOp = region_disj_last,
            io.write_string(Stream, "\tMR_use_region_disj_last", !IO)
        ;
            UseOp = region_disj_nonlast_semi_commit,
            io.write_string(Stream,
                "\tMR_use_region_disj_nonlast_semi_commit", !IO)
        ;
            UseOp = region_commit_success,
            io.write_string(Stream, "\tMR_use_region_commit_success", !IO)
        ;
            UseOp = region_commit_failure,
            io.write_string(Stream, "\tMR_use_region_commit_failure", !IO)
        ),
        io.write_string(Stream, "(", !IO),
        output_embedded_frame_addr(Info, Stream, EmbeddedFrame, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = store_ticket(Lval),
        io.write_string(Stream, "\tMR_store_ticket(", !IO),
        output_lval_as_word(Info, Stream, Lval, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = reset_ticket(Rval, Reason),
        io.write_string(Stream, "\tMR_reset_ticket(", !IO),
        output_rval_as_type(Info, Rval, lt_word, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        io.write_string(Stream, reset_trail_reason_to_string(Reason), !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = discard_ticket,
        io.write_string(Stream, "\tMR_discard_ticket();\n", !IO)
    ;
        Instr = prune_ticket,
        io.write_string(Stream, "\tMR_prune_ticket();\n", !IO)
    ;
        Instr = mark_ticket_stack(Lval),
        io.write_string(Stream, "\tMR_mark_ticket_stack(", !IO),
        output_lval_as_word(Info, Stream, Lval, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = prune_tickets_to(Rval),
        io.write_string(Stream, "\tMR_prune_tickets_to(", !IO),
        output_rval_as_type(Info, Rval, lt_word, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = incr_sp(N, _Msg, Kind),
        DwordAlignment = Info ^ lout_det_stack_dword_alignment,
        (
            DwordAlignment = yes,
            expect(int.even(N), $pred, "odd sp increment")
        ;
            DwordAlignment = no
        ),
        (
            Kind = stack_incr_leaf,
            ( if N < max_leaf_stack_frame_size then
                IncrSp = "MR_incr_sp_leaf"
            else
                IncrSp = "MR_incr_sp"
            )
        ;
            Kind = stack_incr_nonleaf,
            IncrSp = "MR_incr_sp"
        ),
        io.format(Stream, "\t%s(%d);\n", [s(IncrSp), i(N)], !IO)
        % Use the code below instead of the code above if you want to run
        % tools/framesize on the output of the compiler.
        % io.write_string(Stream, "\tMR_incr_sp_push_msg(", !IO),
        % io.write_int(Stream, N, !IO),
        % io.write_string(Stream, ", """, !IO),
        % c_util.output_quoted_string(Stream, Msg, !IO),
        % io.write_string(Stream, """);\n", !IO)
    ;
        Instr = decr_sp(N),
        io.format(Stream, "\tMR_decr_sp(%d);\n", [i(N)], !IO)
    ;
        Instr = decr_sp_and_return(N),
        io.format(Stream, "\tMR_decr_sp_and_return(%d);\n", [i(N)], !IO)
    ;
        Instr = foreign_proc_code(Decls, Components, _, _, _, _, _,
            MaybeDefLabel, _, _),
        io.write_string(Stream, "\t{\n", !IO),
        output_foreign_proc_decls(Stream, Decls, !IO),
        (
            MaybeDefLabel = no,
            list.foldl(output_foreign_proc_component(Info, Stream),
                Components, !IO)
        ;
            MaybeDefLabel = yes(DefLabel),
            InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
            map.lookup(InternalLabelToLayoutMap, DefLabel, DefLabelLayout),
            io.write_string(Stream, "#define MR_HASH_DEF_LABEL_LAYOUT ", !IO),
            MangledModuleName = Info ^ lout_mangled_module_name,
            output_layout_slot_addr(Stream, use_layout_macro,
                MangledModuleName, DefLabelLayout, !IO),
            io.nl(Stream, !IO),
            list.foldl(output_foreign_proc_component(Info, Stream),
                Components, !IO),
            io.write_string(Stream, "#undef MR_HASH_DEF_LABEL_LAYOUT\n", !IO)
        ),
        io.write_string(Stream, "\t}\n", !IO)
    ;
        Instr = init_sync_term(Lval, NumConjuncts, TSStringIndex),
        io.write_string(Stream, "\tMR_init_sync_term(", !IO),
        output_lval_as_word(Info, Stream, Lval, !IO),
        io.write_string(Stream, ", ", !IO),
        io.write_int(Stream, NumConjuncts, !IO),
        io.write_string(Stream, ", ", !IO),
        io.write_int(Stream, TSStringIndex, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = fork_new_child(Lval, Child),
        io.write_string(Stream, "\tMR_fork_new_child(", !IO),
        output_lval_as_word(Info, Stream, Lval, !IO),
        io.write_string(Stream, ", ", !IO),
        output_label_as_code_addr(Stream, Child, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = join_and_continue(Lval, Label),
        io.write_string(Stream, "\tMR_join_and_continue(", !IO),
        output_lval(Info, Stream, Lval, !IO),
        io.write_string(Stream, ", ", !IO),
        output_label_as_code_addr(Stream, Label, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = lc_create_loop_control(NumSlots, Lval),
        % XXX placeholder for pbone to fill in
        io.write_string(Stream, "\tMR_lc_create_loop_control(", !IO),
        io.write_int(Stream, NumSlots, !IO),
        io.write_string(Stream, ", ", !IO),
        output_lval(Info, Stream, Lval, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = lc_wait_free_slot(LCRval, LCSLval, InternalLabel),
        % XXX placeholder for pbone to fill in
        io.write_string(Stream, "\tMR_lc_wait_free_slot(", !IO),
        output_rval(Info, LCRval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        output_lval(Info, Stream, LCSLval, !IO),
        io.write_string(Stream, ", ", !IO),
        output_label(Stream, InternalLabel, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = lc_spawn_off(LCRval, LCSRval, ChildLabel),
        io.write_string(Stream, "\tMR_lc_spawn_off((MR_LoopControl*)", !IO),
        output_rval(Info, LCRval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        output_rval(Info, LCSRval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        output_label_as_code_addr(Stream, ChildLabel, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Instr = lc_join_and_terminate(LCRval, LCSRval),
        % XXX placeholder for pbone to fill in
        io.write_string(Stream, "\tMR_lc_join_and_terminate(", !IO),
        output_rval(Info, LCRval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        output_rval(Info, LCSRval, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ).

%----------------------------------------------------------------------------%
%
% Code for the output of a comment instruction.
%

:- pred output_comment_chars(io.text_output_stream::in,
    char::in, list(char)::in, io::di, io::uo) is det.

output_comment_chars(_, _PrevChar, [], !IO).
output_comment_chars(Stream, PrevChar, [Char | Chars], !IO) :-
    ( if
        PrevChar = ('/'),
        Char = ('*')
    then
        io.write_string(Stream, " *", !IO)
    else if
        PrevChar = ('*'),
        Char = ('/')
    then
        io.write_string(Stream, " /", !IO)
    else
        io.write_char(Stream, Char, !IO)
    ),
    output_comment_chars(Stream, Char, Chars, !IO).

%----------------------------------------------------------------------------%
%
% Code for the output of a livevals instruction.
%

:- pred output_livevals(llds_out_info::in, io.text_output_stream::in,
    list(lval)::in, io::di, io::uo) is det.

output_livevals(_, _, [], !IO).
output_livevals(Info, Stream, [Lval | Lvals], !IO) :-
    io.write_string(Stream, "*\t", !IO),
    output_lval(Info, Stream, Lval, !IO),
    io.write_string(Stream, "\n", !IO),
    output_livevals(Info, Stream, Lvals, !IO).

%----------------------------------------------------------------------------%
%
% Code for the output of a block instruction.
%

:- pred output_block_start(io.text_output_stream::in, int::in, int::in,
    io::di, io::uo) is det.

output_block_start(Stream, TempR, TempF, !IO) :-
    io.write_string(Stream, "\t{\n", !IO),
    ( if TempR > 0 then
        io.write_string(Stream, "\tMR_Word ", !IO),
        output_temp_decls(Stream, TempR, "r", !IO),
        io.write_string(Stream, ";\n", !IO)
    else
        true
    ),
    ( if TempF > 0 then
        io.write_string(Stream, "\tMR_Float ", !IO),
        output_temp_decls(Stream, TempF, "f", !IO),
        io.write_string(Stream, ";\n", !IO)
    else
        true
    ).

:- pred output_block_end(io.text_output_stream::in, io::di, io::uo) is det.

output_block_end(Stream, !IO) :-
    io.write_string(Stream, "\t}\n", !IO).

:- pred output_temp_decls(io.text_output_stream::in, int::in, string::in,
    io::di, io::uo) is det.

output_temp_decls(Stream, N, Type, !IO) :-
    output_temp_decls_2(Stream, 1, N, Type, !IO).

:- pred output_temp_decls_2(io.text_output_stream::in, int::in, int::in,
    string::in, io::di, io::uo) is det.

output_temp_decls_2(Stream, Next, Max, Type, !IO) :-
    ( if Next =< Max then
        ( if Next > 1 then
            io.write_string(Stream, ", ", !IO)
        else
            true
        ),
        io.format(Stream, "MR_temp%s%d", [s(Type), i(Next)], !IO),
        output_temp_decls_2(Stream, Next + 1, Max, Type, !IO)
    else
        true
    ).

%----------------------------------------------------------------------------%
%
% Code for the output of a call instruction.
%

    % Note that we also do some optimization here by outputting `localcall'
    % rather than `call' for calls to local labels, or `call_localret' for
    % calls which return to local labels (i.e. most of them).
    %
    % We also reduce the size of the output by emitting shorthand forms of
    % the relevant macros when possible, allowing those shorthand macros
    % to apply mercury__ prefixes and possible MR_ENTRY() wrappers.
    %
:- pred output_call(llds_out_info::in, io.text_output_stream::in,
    code_addr::in, code_addr::in, label::in, io::di, io::uo) is det.

output_call(Info, Stream, Target, Continuation, CallerLabel, !IO) :-
    io.write_string(Stream, "\t", !IO),
    % For profiling, we ignore calls to do_call_closure and
    % do_call_class_method, because in general they lead to cycles in the call
    % graph that screw up the profile. By generating a `noprof_call' rather
    % than a `call', we ensure that time spent inside those routines
    % is credited to the caller, rather than to do_call_closure or
    % do_call_class_method itself. But if we do use a noprof_call,
    % we need to set MR_prof_ho_caller_proc, so that the callee knows
    % which proc it has been called from.
    ( if
        ( Target = do_call_closure(_)
        ; Target = do_call_class_method(_)
        )
    then
        ProfileCall = no,
        io.write_string(Stream, "MR_set_prof_ho_caller_proc(", !IO),
        output_label_as_code_addr(Stream, CallerLabel, !IO),
        io.write_string(Stream, ");\n\t", !IO)
    else
        ProfileCall = Info ^ lout_profile_calls
    ),
    ( if
        Target = code_label(Label),
        % We really shouldn't be calling internal labels ...
        label_is_external_to_c_module(Label) = no
    then
        (
            ProfileCall = yes,
            io.write_string(Stream, "MR_localcall(", !IO),
            output_label(Stream, Label, !IO),
            io.write_string(Stream, ",\n\t\t", !IO),
            output_code_addr(Stream, Continuation, !IO)
        ;
            ProfileCall = no,
            code_addr_to_string_base(Continuation, BaseStr,
                NeedsPrefix, Wrapper),
            (
                NeedsPrefix = no,
                io.write_string(Stream, "MR_noprof_localcall(", !IO),
                output_label_no_prefix(Stream, Label, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                io.write_string(Stream, BaseStr, !IO),
                output_code_addr_from_pieces(Stream, BaseStr,
                    NeedsPrefix, Wrapper, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_entry,
                io.write_string(Stream, "MR_np_localcall_ent(", !IO),
                output_label_no_prefix(Stream, Label, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                io.write_string(Stream, BaseStr, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_label,
                io.write_string(Stream, "MR_np_localcall_lab(", !IO),
                output_label_no_prefix(Stream, Label, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                io.write_string(Stream, BaseStr, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_none,
                io.write_string(Stream, "MR_np_localcall(", !IO),
                output_label_no_prefix(Stream, Label, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_code_addr_from_pieces(Stream, BaseStr,
                    NeedsPrefix, Wrapper, !IO)
            )
        )
    else if
        Continuation = code_label(ContLabel),
        label_is_external_to_c_module(ContLabel) = no
    then
        (
            ProfileCall = yes,
            io.write_string(Stream, "MR_call_localret(", !IO),
            output_code_addr(Stream, Target, !IO),
            io.write_string(Stream, ",\n\t\t", !IO),
            output_label(Stream, ContLabel, !IO)
        ;
            ProfileCall = no,
            code_addr_to_string_base(Target, BaseStr, NeedsPrefix, Wrapper),
            (
                NeedsPrefix = no,
                io.write_string(Stream, "MR_noprof_call_localret(", !IO),
                output_code_addr_from_pieces(Stream, BaseStr,
                    NeedsPrefix, Wrapper, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_label(Stream, ContLabel, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_entry,
                io.write_string(Stream, "MR_np_call_localret_ent(", !IO),
                io.write_string(Stream, BaseStr, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_label_no_prefix(Stream, ContLabel, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_label,
                % We should never get here; the conditions that lead here
                % in this switch should have been caught by the first
                % if-then-else condition that tests Target.
                unexpected($pred, "calling label")
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_none,
                io.write_string(Stream, "MR_np_call_localret(", !IO),
                output_code_addr_from_pieces(Stream, BaseStr,
                    NeedsPrefix, Wrapper, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_label_no_prefix(Stream, ContLabel, !IO)
            )
        )
    else
        (
            ProfileCall = yes,
            io.write_string(Stream, "MR_call(", !IO)
        ;
            ProfileCall = no,
            io.write_string(Stream, "MR_noprof_call(", !IO)
        ),
        output_code_addr(Stream, Target, !IO),
        io.write_string(Stream, ",\n\t\t", !IO),
        output_code_addr(Stream, Continuation, !IO)
    ),
    (
        ProfileCall = yes,
        io.write_string(Stream, ",\n\t\t", !IO),
        output_label_as_code_addr(Stream, CallerLabel, !IO)
    ;
        ProfileCall = no
    ),
    io.write_string(Stream, ");\n", !IO).

:- pred output_gc_livevals(llds_out_info::in, io.text_output_stream::in,
    list(liveinfo)::in, io::di, io::uo) is det.

output_gc_livevals(Info, Stream, LiveVals, !IO) :-
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.write_string(Stream, "/*\n", !IO),
        io.write_string(Stream, "* Garbage collection livevals info\n", !IO),
        output_gc_livevals_2(Info, Stream, LiveVals, !IO),
        io.write_string(Stream, "*/\n", !IO)
    ;
        AutoComments = no_auto_comments
    ).

:- pred output_gc_livevals_2(llds_out_info::in, io.text_output_stream::in,
    list(liveinfo)::in, io::di, io::uo) is det.

output_gc_livevals_2(_, _, [], !IO).
output_gc_livevals_2(Info, Stream, [LiveInfo | LiveInfos], !IO) :-
    LiveInfo = live_lvalue(Locn, LiveValueType, TypeParams),
    io.write_string(Stream, "*\t", !IO),
    output_layout_locn(Info, Stream, Locn, !IO),
    io.write_string(Stream, "\t", !IO),
    output_live_value_type(Stream, LiveValueType, !IO),
    io.write_string(Stream, "\t", !IO),
    map.to_assoc_list(TypeParams, TypeParamList),
    output_gc_livevals_params(Info, Stream, TypeParamList, !IO),
    io.write_string(Stream, "\n", !IO),
    output_gc_livevals_2(Info, Stream, LiveInfos, !IO).

:- pred output_gc_livevals_params(llds_out_info::in,
    io.text_output_stream::in, assoc_list(tvar, set(layout_locn))::in,
    io::di, io::uo) is det.

output_gc_livevals_params(_, _, [], !IO).
output_gc_livevals_params(Info, Stream, [Var - LocnSet | VarLocnSets], !IO) :-
    term.var_to_int(Var, VarInt),
    io.write_int(Stream, VarInt, !IO),
    io.write_string(Stream, " - ", !IO),
    set.to_sorted_list(LocnSet, Locns),
    output_layout_locns(Info, Stream, Locns, !IO),
    io.write_string(Stream, "  ", !IO),
    output_gc_livevals_params(Info, Stream, VarLocnSets, !IO).

:- pred output_layout_locns(llds_out_info::in, io.text_output_stream::in,
    list(layout_locn)::in, io::di, io::uo) is det.

output_layout_locns(_, _, [], !IO).
output_layout_locns(Info, Stream, [Locn | Locns], !IO) :-
    output_layout_locn(Info, Stream, Locn, !IO),
    (
        Locns = []
    ;
        Locns = [_ | _],
        io.write_string(Stream, " and ", !IO),
        output_layout_locns(Info, Stream, Locns, !IO)
    ).

:- pred output_layout_locn(llds_out_info::in, io.text_output_stream::in,
    layout_locn::in, io::di, io::uo) is det.

output_layout_locn(Info, Stream, Locn, !IO) :-
    (
        Locn = locn_direct(Lval),
        output_lval(Info, Stream, Lval, !IO)
    ;
        Locn = locn_indirect(Lval, Offset),
        io.write_string(Stream, "offset ", !IO),
        io.write_int(Stream, Offset, !IO),
        io.write_string(Stream, " from ", !IO),
        output_lval(Info, Stream, Lval, !IO)
    ).

:- pred output_live_value_type(io.text_output_stream::in, live_value_type::in,
    io::di, io::uo) is det.

output_live_value_type(Stream, live_value_succip, !IO) :-
    io.write_string(Stream, "type succip", !IO).
output_live_value_type(Stream, live_value_curfr, !IO) :-
    io.write_string(Stream, "type curfr", !IO).
output_live_value_type(Stream, live_value_maxfr, !IO) :-
    io.write_string(Stream, "type maxfr", !IO).
output_live_value_type(Stream, live_value_redofr, !IO) :-
    io.write_string(Stream, "type redofr", !IO).
output_live_value_type(Stream, live_value_redoip, !IO) :-
    io.write_string(Stream, "type redoip", !IO).
output_live_value_type(Stream, live_value_hp, !IO) :-
    io.write_string(Stream, "type hp", !IO).
output_live_value_type(Stream, live_value_trail_ptr, !IO) :-
    io.write_string(Stream, "type trail_ptr", !IO).
output_live_value_type(Stream, live_value_ticket, !IO) :-
    io.write_string(Stream, "type ticket", !IO).
output_live_value_type(Stream, live_value_region_disj, !IO) :-
    io.write_string(Stream, "type region disj", !IO).
output_live_value_type(Stream, live_value_region_commit, !IO) :-
    io.write_string(Stream, "type region commit", !IO).
output_live_value_type(Stream, live_value_region_ite, !IO) :-
    io.write_string(Stream, "type region ite", !IO).
output_live_value_type(Stream, live_value_unwanted, !IO) :-
    io.write_string(Stream, "unwanted", !IO).
output_live_value_type(Stream, live_value_var(Var, Name, Type, _LldsInst),
        !IO) :-
    term.var_to_int(Var, VarInt),
    io.format(Stream, "var(%d, %s, ", [i(VarInt), s(Name)], !IO),
    % XXX Fake type varset.
    varset.init(NewTVarset),
    mercury_output_type(NewTVarset, print_name_only, Type, Stream, !IO),
%   io.write_string(Stream, ", ", !IO),
%   (
%       LldsInst = llds_inst_ground,
%       io.write_string(Stream, "ground", !IO)
%   ;
%       LldsInst = llds_inst_partial(Inst),
%       % XXX Fake inst varset
%       varset.init(NewIVarset),
%       mercury_output_inst(Inst, NewIVarset, !IO)
%   ),
    io.write_string(Stream, ")", !IO).

%----------------------------------------------------------------------------%
%
% Code for the output of a label instruction.
%

:- pred output_label_defn(io.text_output_stream::in, label::in,
    io::di, io::uo) is det.

output_label_defn(Stream, entry_label(entry_label_exported, ProcLabel), !IO) :-
    io.write_string(Stream, "MR_define_entry(", !IO),
    output_label(Stream, entry_label(entry_label_exported, ProcLabel), !IO),
    io.write_string(Stream, ");\n", !IO).
output_label_defn(Stream, entry_label(entry_label_local, ProcLabel), !IO) :-
    io.format(Stream, "MR_def_static(%s)\n",
        [s(proc_label_to_c_string(do_not_add_label_prefix, ProcLabel))], !IO).
output_label_defn(Stream, entry_label(entry_label_c_local, ProcLabel), !IO) :-
    io.format(Stream, "MR_def_local(%s)\n",
        [s(proc_label_to_c_string(do_not_add_label_prefix, ProcLabel))], !IO).
output_label_defn(Stream, internal_label(Num, ProcLabel), !IO) :-
    io.format(Stream, "MR_def_label(%s, %d)\n",
        [s(proc_label_to_c_string(do_not_add_label_prefix, ProcLabel)),
        i(Num)], !IO).

:- pred maybe_output_update_prof_counter(llds_out_info::in,
    io.text_output_stream::in, label::in, label_output_info::in,
    io::di, io::uo) is det.

maybe_output_update_prof_counter(Info, Stream, Label, LabelOutputInfo, !IO) :-
    % If ProfileTime is no, the definition of MR_update_prof_current_proc
    % is empty anyway.
    ProfileTime = Info ^ lout_profile_time,
    ( if
        ProfileTime = yes,
        ContLabelSet = LabelOutputInfo ^ loi_cont_labels,
        set_tree234.contains(ContLabelSet, Label)
    then
        CallerLabel = LabelOutputInfo ^ loi_caller_label,
        io.write_string(Stream,
            "\tMR_update_prof_current_proc(MR_LABEL_AP(", !IO),
        output_label_no_prefix(Stream, CallerLabel, !IO),
        io.write_string(Stream, "));\n", !IO)
    else
        true
    ).

%----------------------------------------------------------------------------%
%
% Code for the output of a goto instruction.
%

:- pred output_goto(llds_out_info::in, io.text_output_stream::in,
    code_addr::in, label::in, io::di, io::uo) is det.

output_goto(Info, Stream, Target, CallerLabel, !IO) :-
    (
        Target = code_label(Label),
        % Note that we do some optimization here: instead of always outputting
        % `MR_GOTO(<label>)', we output different things for each different
        % kind of label.
        ProfileCalls = Info ^ lout_profile_calls,
        (
            Label = entry_label(entry_label_exported, _),
            (
                ProfileCalls = yes,
                io.write_string(Stream, "MR_tailcall(", !IO),
                output_label_as_code_addr(Stream, Label, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_label_as_code_addr(Stream, CallerLabel, !IO),
                io.write_string(Stream, ");\n", !IO)
            ;
                ProfileCalls = no,
                io.write_string(Stream, "MR_np_tailcall_ent(", !IO),
                output_label_no_prefix(Stream, Label, !IO),
                io.write_string(Stream, ");\n", !IO)
            )
        ;
            Label = entry_label(entry_label_local, _),
            (
                ProfileCalls = yes,
                io.write_string(Stream, "MR_tailcall(", !IO),
                output_label_as_code_addr(Stream, Label, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_label_as_code_addr(Stream, CallerLabel, !IO),
                io.write_string(Stream, ");\n", !IO)
            ;
                ProfileCalls = no,
                io.write_string(Stream, "MR_np_tailcall_ent(", !IO),
                output_label_no_prefix(Stream, Label, !IO),
                io.write_string(Stream, ");\n", !IO)
            )
        ;
            Label = entry_label(entry_label_c_local, _),
            (
                ProfileCalls = yes,
                io.write_string(Stream, "MR_localtailcall(", !IO),
                output_label(Stream, Label, !IO),
                io.write_string(Stream, ",\n\t\t", !IO),
                output_label_as_code_addr(Stream, CallerLabel, !IO),
                io.write_string(Stream, ");\n", !IO)
            ;
                ProfileCalls = no,
                io.write_string(Stream, "MR_np_localtailcall(", !IO),
                output_label_no_prefix(Stream, Label, !IO),
                io.write_string(Stream, ");\n", !IO)
            )
        ;
            Label = internal_label(_, _),
            io.write_string(Stream, "MR_GOTO_LAB(", !IO),
            output_label_no_prefix(Stream, Label, !IO),
            io.write_string(Stream, ");\n", !IO)
        )
    ;
        Target = code_imported_proc(ProcLabel),
        ProfileCalls = Info ^ lout_profile_calls,
        (
            ProfileCalls = yes,
            io.write_string(Stream, "MR_tailcall(MR_ENTRY(", !IO),
            io.write_string(Stream,
                proc_label_to_c_string(add_label_prefix,ProcLabel), !IO),
            io.write_string(Stream, "),\n\t\t", !IO),
            output_label_as_code_addr(Stream, CallerLabel, !IO),
            io.write_string(Stream, ");\n", !IO)
        ;
            ProfileCalls = no,
            io.write_string(Stream, "MR_np_tailcall_ent(", !IO),
            io.write_string(Stream,
                proc_label_to_c_string(do_not_add_label_prefix,ProcLabel),
                !IO),
            io.write_string(Stream, ");\n", !IO)
        )
    ;
        Target = code_succip,
        io.write_string(Stream, "MR_proceed();\n", !IO)
    ;
        Target = do_succeed(Last),
        (
            Last = no,
            io.write_string(Stream, "MR_succeed();\n", !IO)
        ;
            Last = yes,
            io.write_string(Stream, "MR_succeed_discard();\n", !IO)
        )
    ;
        Target = do_redo,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = use_macro_for_redo_fail,
            io.write_string(Stream, "MR_redo();\n", !IO)
        ;
            UseMacro = do_not_use_macro_for_redo_fail,
            io.write_string(Stream, "MR_GOTO(MR_ENTRY(MR_do_redo));\n", !IO)
        )
    ;
        Target = do_fail,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = use_macro_for_redo_fail,
            io.write_string(Stream, "MR_fail();\n", !IO)
        ;
            UseMacro = do_not_use_macro_for_redo_fail,
            io.write_string(Stream, "MR_GOTO(MR_ENTRY(MR_do_fail));\n", !IO)
        )
    ;
        Target = do_trace_redo_fail_shallow,
        io.write_string(Stream,
            "MR_GOTO(MR_ENTRY(MR_do_trace_redo_fail_shallow));\n", !IO)
    ;
        Target = do_trace_redo_fail_deep,
        io.write_string(Stream,
            "MR_GOTO(MR_ENTRY(MR_do_trace_redo_fail_deep));\n", !IO)
    ;
        Target = do_call_closure(Variant),
        % see comment in output_call for why we use `noprof_' etc. here
        io.write_string(Stream, "MR_set_prof_ho_caller_proc(", !IO),
        output_label_as_code_addr(Stream, CallerLabel, !IO),
        io.write_string(Stream, ");\n\t", !IO),
        io.write_string(Stream, "MR_np_tailcall_ent(do_call_closure_", !IO),
        io.write_string(Stream, ho_call_variant_to_string(Variant), !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Target = do_call_class_method(Variant),
        % see comment in output_call for why we use `noprof_' etc. here
        io.write_string(Stream, "MR_set_prof_ho_caller_proc(", !IO),
        output_label_as_code_addr(Stream, CallerLabel, !IO),
        io.write_string(Stream, ");\n\t", !IO),
        io.write_string(Stream,
            "MR_np_tailcall_ent(do_call_class_method_", !IO),
        io.write_string(Stream, ho_call_variant_to_string(Variant), !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Target = do_not_reached,
        io.write_string(Stream,
            "MR_tailcall(MR_ENTRY(MR_do_not_reached),\n\t\t", !IO),
        output_label_as_code_addr(Stream, CallerLabel, !IO),
        io.write_string(Stream, ");\n", !IO)
    ).

%----------------------------------------------------------------------------%
%
% Code for the output of a computed_goto instruction.
%

:- pred output_label_list_or_not_reached(io.text_output_stream::in,
    list(maybe(label))::in, io::di, io::uo) is det.

output_label_list_or_not_reached(_, [], !IO).
output_label_list_or_not_reached(Stream, [MaybeLabel | MaybeLabels], !IO) :-
    output_label_or_not_reached(Stream, MaybeLabel, !IO),
    output_label_list_or_not_reached_2(Stream, MaybeLabels, !IO).

:- pred output_label_list_or_not_reached_2(io.text_output_stream::in,
    list(maybe(label))::in, io::di, io::uo) is det.

output_label_list_or_not_reached_2(_, [], !IO).
output_label_list_or_not_reached_2(Stream, [MaybeLabel | MaybeLabels], !IO) :-
    io.write_string(Stream, " MR_AND\n\t\t", !IO),
    output_label_or_not_reached(Stream, MaybeLabel, !IO),
    output_label_list_or_not_reached_2(Stream, MaybeLabels, !IO).

:- pred output_label_or_not_reached(io.text_output_stream::in,
    maybe(label)::in, io::di, io::uo) is det.

output_label_or_not_reached(Stream, MaybeLabel, !IO) :-
    (
        MaybeLabel = yes(Label),
        io.write_string(Stream, "MR_LABEL_AP(", !IO),
        output_label_no_prefix(Stream, Label, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        MaybeLabel = no,
        io.write_string(Stream, "MR_ENTRY(MR_do_not_reached)", !IO)
    ).

%----------------------------------------------------------------------------%
%
% Code for the output of an incr_hp instruction.
%

:- pred output_incr_hp_no_reuse(llds_out_info::in, io.text_output_stream::in,
    lval::in, maybe(ptag)::in, maybe(int)::in, rval::in,
    maybe(alloc_site_id)::in, may_use_atomic_alloc::in, maybe(rval)::in,
    label_output_info::in, io::di, io::uo) is det.

output_incr_hp_no_reuse(Info, Stream, Lval, MaybePtag, MaybeOffset, Rval,
        MaybeAllocId, MayUseAtomicAlloc, MaybeRegionRval,
        _LabelOutputInfo, !IO) :-
    (
        MaybeRegionRval = yes(RegionRval),
        (
            MaybePtag = no,
            io.write_string(Stream, "MR_alloc_in_region(", !IO),
            output_lval_as_word(Info, Stream, Lval, !IO)
        ;
            MaybePtag = yes(Ptag),
            io.write_string(Stream, "MR_tag_alloc_in_region(", !IO),
            output_lval_as_word(Info, Stream, Lval, !IO),
            io.write_string(Stream, ", ", !IO),
            output_ptag(Stream, Ptag, !IO)
        ),
        io.write_string(Stream, ", ", !IO),
        output_rval(Info, RegionRval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        output_rval_as_type(Info, Rval, lt_word, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        MaybeRegionRval = no,
        ProfMem = Info ^ lout_profile_memory,
        (
            ProfMem = yes,
            (
                MaybePtag = no,
                (
                    MayUseAtomicAlloc = may_not_use_atomic_alloc,
                    io.write_string(Stream, "MR_offset_incr_hp_msg(", !IO)
                ;
                    MayUseAtomicAlloc = may_use_atomic_alloc,
                    io.write_string(Stream, "MR_offset_incr_hp_atomic_msg(",
                        !IO)
                ),
                output_lval_as_word(Info, Stream, Lval, !IO)
            ;
                MaybePtag = yes(Ptag),
                (
                    MayUseAtomicAlloc = may_not_use_atomic_alloc,
                    io.write_string(Stream, "MR_tag_offset_incr_hp_msg(", !IO)
                ;
                    MayUseAtomicAlloc = may_use_atomic_alloc,
                    io.write_string(Stream,
                        "MR_tag_offset_incr_hp_atomic_msg(", !IO)
                ),
                output_lval_as_word(Info, Stream, Lval, !IO),
                io.write_string(Stream, ", ", !IO),
                output_ptag(Stream, Ptag, !IO)
            ),
            io.write_string(Stream, ", ", !IO),
            (
                MaybeOffset = no,
                io.write_string(Stream, "0, ", !IO)
            ;
                MaybeOffset = yes(Offset),
                io.write_int(Stream, Offset, !IO),
                io.write_string(Stream, ", ", !IO)
            ),
            output_rval_as_type(Info, Rval, lt_word, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            output_maybe_alloc_site_id(Info, Stream, MaybeAllocId, !IO),
            io.write_string(Stream, ", NULL)", !IO)
        ;
            ProfMem = no,
            (
                MaybePtag = no,
                (
                    MaybeOffset = yes(_),
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string(Stream, "MR_offset_incr_hp(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string(Stream, "MR_offset_incr_hp_atomic(",
                            !IO)
                    )
                ;
                    MaybeOffset = no,
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string(Stream, "MR_alloc_heap(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string(Stream, "MR_alloc_heap_atomic(", !IO)
                    )
                ),
                output_lval_as_word(Info, Stream, Lval, !IO)
            ;
                MaybePtag = yes(Ptag),
                (
                    MaybeOffset = yes(_),
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string(Stream, "MR_tag_offset_incr_hp(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string(Stream,
                            "MR_tag_offset_incr_hp_atomic(", !IO)
                    ),
                    output_lval_as_word(Info, Stream, Lval, !IO),
                    io.write_string(Stream, ", ", !IO),
                    output_ptag(Stream, Ptag, !IO)
                ;
                    MaybeOffset = no,
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string(Stream, "MR_tag_alloc_heap(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string(Stream, "MR_tag_alloc_heap_atomic(",
                            !IO)
                    ),
                    output_lval_as_word(Info, Stream, Lval, !IO),
                    io.write_string(Stream, ", ", !IO),
                    write_ptag(Stream, Ptag, !IO)
                )
            ),
            io.write_string(Stream, ", ", !IO),
            (
                MaybeOffset = yes(Offset),
                io.write_int(Stream, Offset, !IO),
                io.write_string(Stream, ", ", !IO)
            ;
                MaybeOffset = no
            ),
            output_rval_as_type(Info, Rval, lt_word, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ).

:- pred output_maybe_alloc_site_id(llds_out_info::in,
    io.text_output_stream::in, maybe(alloc_site_id)::in,
    io::di, io::uo) is det.

output_maybe_alloc_site_id(Info, Stream, MaybeAllocId, !IO) :-
    (
        MaybeAllocId = yes(AllocId),
        output_alloc_site_id(Info, Stream, AllocId, !IO)
    ;
        MaybeAllocId = no,
        io.write_string(Stream, "NULL", !IO)
    ).

:- pred output_alloc_site_id(llds_out_info::in, io.text_output_stream::in,
    alloc_site_id::in, io::di, io::uo) is det.

output_alloc_site_id(Info, Stream, AllocId, !IO) :-
    MangledModuleName = Info ^ lout_mangled_module_name,
    AllocSiteMap = Info ^ lout_alloc_site_map,
    map.lookup(AllocSiteMap, AllocId, AllocSiteSlotName),
    output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
        AllocSiteSlotName, !IO).

%----------------------------------------------------------------------------%
%
% Code for the output of push_region_frame, region_fill_frame,
% region_set_fixed_slot and use_and_maybe_pop_region_frame instructions.
%

    % Our stacks grow upwards in that new stack frames have higher addresses
    % than old stack frames, but within in each stack frame, we compute the
    % address of stackvar N or framevar N by *subtracting* N from the address
    % of the top of (the non-fixed part of) the stack frame, so that e.g.
    % framevar N+1 is actually stored at a *lower* address than framevar N.
    %
    % The C code we interact with refers to embedded stack frames by the
    % starting (i.e. lowest) address.
    %
:- pred output_embedded_frame_addr(llds_out_info::in,
    io.text_output_stream::in, embedded_stack_frame_id::in,
    io::di, io::uo) is det.

output_embedded_frame_addr(Info, Stream, EmbeddedFrame, !IO) :-
    EmbeddedFrame = embedded_stack_frame_id(MainStackId,
        _FirstSlot, LastSlot),
    FrameStartRval = stack_slot_num_to_lval_ref(MainStackId, LastSlot),
    output_rval_as_type(Info, FrameStartRval, lt_data_ptr, Stream, !IO).

:- func max_leaf_stack_frame_size = int.

% This should be kept in sync with the value of MR_stack_margin_size_words
% in runtime/mercury_wrapper.c. See the documentation there.
max_leaf_stack_frame_size = 32.

%----------------------------------------------------------------------------%
%
% Code for the output of reset_ticket instructions.
%

:- func reset_trail_reason_to_string(reset_trail_reason) = string.

reset_trail_reason_to_string(reset_reason_undo) = "MR_undo".
reset_trail_reason_to_string(reset_reason_commit) = "MR_commit".
reset_trail_reason_to_string(reset_reason_solve) = "MR_solve".
reset_trail_reason_to_string(reset_reason_exception) = "MR_exception".
reset_trail_reason_to_string(reset_reason_retry) = "MR_retry".
reset_trail_reason_to_string(reset_reason_gc) = "MR_gc".

%----------------------------------------------------------------------------%
%
% Code for the output of foreign_proc_code instructions.
%

:- pred output_foreign_proc_component(llds_out_info::in,
    io.text_output_stream::in, foreign_proc_component::in,
    io::di, io::uo) is det.

output_foreign_proc_component(Info, Stream, Component, !IO) :-
    (
        Component = foreign_proc_inputs(Inputs),
        output_foreign_proc_inputs(Info, Stream, Inputs, !IO)
    ;
        Component = foreign_proc_outputs(Outputs),
        output_foreign_proc_outputs(Info, Stream, Outputs, !IO)
    ;
        Component = foreign_proc_user_code(MaybeContext, _, C_Code),
        ( if C_Code = "" then
            true
        else
            % We should start the C_Code on a new line,
            % just in case it starts with a proprocessor directive.
            (
                MaybeContext = yes(Context),
                io.write_string(Stream, "{\n", !IO),
                output_set_line_num(Stream, Info ^ lout_foreign_line_numbers,
                    Context, !IO),
                io.write_string(Stream, C_Code, !IO),
                io.write_string(Stream, ";}\n", !IO),
                output_reset_line_num(Stream, Info ^ lout_foreign_line_numbers,
                    !IO)
            ;
                MaybeContext = no,
                io.format(Stream, "{\n%s;}\n", [s(C_Code)], !IO)
            )
        )
    ;
        Component = foreign_proc_raw_code(_, _, _, C_Code),
        io.write_string(Stream, C_Code, !IO)
    ;
        Component = foreign_proc_fail_to(Label),
        io.write_string(Stream,
            "if (!" ++ foreign_proc_succ_ind_name ++ ") MR_GOTO_LAB(", !IO),
        output_label_no_prefix(Stream, Label, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        Component = foreign_proc_alloc_id(AllocId),
        output_alloc_site_id(Info, Stream, AllocId, !IO)
    ;
        Component = foreign_proc_noop
    ).

    % Output the local variable declarations at the top of the
    % foreign_proc code for C.
    %
:- pred output_foreign_proc_decls(io.text_output_stream::in,
    list(foreign_proc_decl)::in, io::di, io::uo) is det.

output_foreign_proc_decls(_, [], !IO).
output_foreign_proc_decls(Stream, [Decl | Decls], !IO) :-
    % Apart from special cases, the local variables are MR_Words
    Decl = foreign_proc_arg_decl(_Type, TypeString, VarName),
    io.format(Stream, "\t%s\t%s;\n", [s(TypeString), s(VarName)], !IO),
    output_foreign_proc_decls(Stream, Decls, !IO).

    % Output declarations for any rvals used to initialize the inputs.
    %
:- pred output_record_foreign_proc_input_rval_decls(llds_out_info::in,
    io.text_output_stream::in, list(foreign_proc_input)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_foreign_proc_input_rval_decls(_, _, [], !DeclSet, !IO).
output_record_foreign_proc_input_rval_decls(Info, Stream, [Input | Inputs],
        !DeclSet, !IO) :-
    Input = foreign_proc_input(_VarName, _VarType, _IsDummy, _OrigType, Rval,
        _, _),
    output_record_rval_decls_tab(Info, Stream, Rval, !DeclSet, !IO),
    output_record_foreign_proc_input_rval_decls(Info, Stream, Inputs,
        !DeclSet, !IO).

    % Output the input variable assignments at the top of the foreign code
    % for C.
    %
:- pred output_foreign_proc_inputs(llds_out_info::in,
    io.text_output_stream::in, list(foreign_proc_input)::in,
    io::di, io::uo) is det.

output_foreign_proc_inputs(_, _, [], !IO).
output_foreign_proc_inputs(Info, Stream, [Input | Inputs], !IO) :-
    Input = foreign_proc_input(VarName, VarType, IsDummy, _OrigType, _Rval,
        _MaybeForeignTypeInfo, _BoxPolicy),
    (
        IsDummy = is_dummy_type,
        ( if
            % Avoid outputting an assignment for builtin dummy types.
            % For other dummy types we must output an assignment because
            % code in the foreign_proc body may examine the value.
            type_to_ctor_and_args(VarType, VarTypeCtor, []),
            is_type_ctor_a_builtin_dummy(VarTypeCtor) =
                is_builtin_dummy_type_ctor
        then
            true
        else
            io.write_string(Stream, "\t" ++ VarName ++ " = 0;\n", !IO)
        )
    ;
        IsDummy = is_not_dummy_type,
        output_foreign_proc_input(Info, Stream, Input, !IO)
    ),
    output_foreign_proc_inputs(Info, Stream, Inputs, !IO).

    % Output an input variable assignment at the top of the foreign code
    % for C.
    %
:- pred output_foreign_proc_input(llds_out_info::in, io.text_output_stream::in,
    foreign_proc_input::in, io::di, io::uo) is det.

output_foreign_proc_input(Info, Stream, Input, !IO) :-
    Input = foreign_proc_input(VarName, _VarType, _IsDummy, OrigType, Rval,
        MaybeForeignTypeInfo, BoxPolicy),
    io.write_string(Stream, "\t", !IO),
    (
        BoxPolicy = bp_always_boxed,
        io.write_string(Stream, VarName, !IO),
        io.write_string(Stream, " = ", !IO),
        output_rval_as_type(Info, Rval, lt_word, Stream, !IO)
    ;
        BoxPolicy = bp_native_if_possible,
        (
            MaybeForeignTypeInfo = yes(ForeignTypeInfo),
            ForeignTypeInfo = foreign_proc_type(ForeignType, Assertions),
            % For foreign types for which c_type_is_word_sized_int_or_ptr
            % succeeds, the code in the else branch is not only correct,
            % it also generates faster code than would be generated by
            % the then branch, because MR_MAYBE_UNBOX_FOREIGN_TYPE
            % invokes memcpy when given a word-sized type.
            ( if
                (
                    c_type_is_word_sized_int_or_ptr(ForeignType)
                ;
                    asserted_can_pass_as_mercury_type(Assertions)
                )
            then
                % Note that for this cast to be correct the foreign
                % type must be a word sized integer or pointer type.
                io.format(Stream, "%s = (%s) ",
                    [s(VarName), s(ForeignType)], !IO),
                output_rval_as_type(Info, Rval, lt_word, Stream, !IO)
            else
                io.format(Stream, "MR_MAYBE_UNBOX_FOREIGN_TYPE(%s, ",
                    [s(ForeignType)], !IO),
                output_rval_as_type(Info, Rval, lt_word, Stream, !IO),
                io.format(Stream, ", %s)", [s(VarName)], !IO)
            )
        ;
            MaybeForeignTypeInfo = no,
            io.format(Stream, "%s = ", [s(VarName)], !IO),
            ( if OrigType = builtin_type(BuiltinType) then
                (
                    BuiltinType = builtin_type_string,
                    output_llds_type_cast(Stream, lt_string, !IO),
                    output_rval_as_type(Info, Rval, lt_word, Stream, !IO)
                ;
                    BuiltinType = builtin_type_float,
                    output_rval_as_type(Info, Rval, lt_float, Stream, !IO)
                ;
                    BuiltinType = builtin_type_int(int_type_int64),
                    output_rval_as_type(Info, Rval,
                        lt_int(int_type_int64), Stream, !IO)
                ;
                    BuiltinType = builtin_type_int(int_type_uint64),
                    output_rval_as_type(Info, Rval,
                        lt_int(int_type_uint64), Stream, !IO)
                ;
                    ( BuiltinType = builtin_type_char
                    ; BuiltinType = builtin_type_int(int_type_int)
                    ; BuiltinType = builtin_type_int(int_type_uint)
                    ; BuiltinType = builtin_type_int(int_type_int8)
                    ; BuiltinType = builtin_type_int(int_type_uint8)
                    ; BuiltinType = builtin_type_int(int_type_int16)
                    ; BuiltinType = builtin_type_int(int_type_uint16)
                    ; BuiltinType = builtin_type_int(int_type_int32)
                    ; BuiltinType = builtin_type_int(int_type_uint32)
                    ),
                    output_rval_as_type(Info, Rval, lt_word, Stream, !IO)
                )
            else
                output_rval_as_type(Info, Rval, lt_word, Stream, !IO)
            )
        )
    ),
    io.write_string(Stream, ";\n", !IO).

    % Output declarations for any lvals used for the outputs.
    %
:- pred output_record_foreign_proc_output_lval_decls(llds_out_info::in,
    io.text_output_stream::in, list(foreign_proc_output)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_foreign_proc_output_lval_decls(_, _, [], !DeclSet, !IO).
output_record_foreign_proc_output_lval_decls(Info, Stream, [Output | Outputs],
        !DeclSet, !IO) :-
    Output = foreign_proc_output(Lval, _VarType, _IsDummy, _OrigType,
        _VarName, _, _),
    output_record_lval_decls_tab(Info, Stream, Lval, !DeclSet, !IO),
    output_record_foreign_proc_output_lval_decls(Info, Stream, Outputs,
        !DeclSet, !IO).

    % Output the output variable assignments at the bottom of the foreign code
    % for C.
    %
:- pred output_foreign_proc_outputs(llds_out_info::in,
    io.text_output_stream::in, list(foreign_proc_output)::in,
    io::di, io::uo) is det.

output_foreign_proc_outputs(_, _, [], !IO).
output_foreign_proc_outputs(Info, Stream, [Output | Outputs], !IO) :-
    Output = foreign_proc_output(_Lval, _VarType, IsDummy, _OrigType,
        _VarName, _MaybeForeignType, _BoxPolicy),
    (
        IsDummy = is_dummy_type
    ;
        IsDummy = is_not_dummy_type,
        output_foreign_proc_output(Info, Stream, Output, !IO)
    ),
    output_foreign_proc_outputs(Info, Stream, Outputs, !IO).

    % Output a output variable assignment at the bottom of the foreign code
    % for C.
    %
:- pred output_foreign_proc_output(llds_out_info::in,
    io.text_output_stream::in, foreign_proc_output::in,
    io::di, io::uo) is det.

output_foreign_proc_output(Info, Stream, Output, !IO) :-
    Output = foreign_proc_output(Lval, _VarType, _IsDummy, OrigType, VarName,
        MaybeForeignType, BoxPolicy),
    io.write_string(Stream, "\t", !IO),
    (
        BoxPolicy = bp_always_boxed,
        output_lval_as_word(Info, Stream, Lval, !IO),
        io.write_string(Stream, " = ", !IO),
        io.write_string(Stream, VarName, !IO)
    ;
        BoxPolicy = bp_native_if_possible,
        (
            MaybeForeignType = yes(ForeignTypeInfo),
            ForeignTypeInfo = foreign_proc_type(ForeignType, Assertions),
            ( if asserted_can_pass_as_mercury_type(Assertions) then
                output_lval_as_word(Info, Stream, Lval, !IO),
                io.write_string(Stream, " = ", !IO),
                output_llds_type_cast(Stream, lt_word, !IO),
                io.write_string(Stream, VarName, !IO)
            else
                io.format(Stream,
                    "MR_MAYBE_BOX_FOREIGN_TYPE(%s, %s, ",
                    [s(ForeignType), s(VarName)], !IO),
                output_lval_as_word(Info, Stream, Lval, !IO),
                io.write_string(Stream, ")", !IO)
            )
        ;
            MaybeForeignType = no,
            ( if OrigType = builtin_type(BuiltinType) then
                (
                    BuiltinType = builtin_type_string,
                    output_lval_as_word(Info, Stream, Lval, !IO),
                    io.write_string(Stream, " = ", !IO),
                    output_llds_type_cast(Stream, lt_word, !IO),
                    io.write_string(Stream, VarName, !IO)
                ;
                    BuiltinType = builtin_type_float,
                    llds.lval_type(Lval, ActualType),
                    ( if ActualType = lt_float then
                        output_lval(Info, Stream, Lval, !IO),
                        io.format(Stream, " = %s", [s(VarName)], !IO)
                    else
                        output_lval_as_word(Info, Stream, Lval, !IO),
                        io.format(Stream, " = MR_float_to_word(%s)",
                            [s(VarName)], !IO)
                    )
                ;
                    BuiltinType = builtin_type_char,
                    output_lval_as_word(Info, Stream, Lval, !IO),
                    % Characters must be cast to MR_UnsignedChar to
                    % prevent sign-extension.
                    io.format(Stream, " = (MR_UnsignedChar) %s",
                        [s(VarName)], !IO)
                ;
                    BuiltinType = builtin_type_int(IntType),
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
                        output_lval_as_word(Info, Stream, Lval, !IO),
                        io.format(Stream, " = %s", [s(VarName)], !IO)
                    ;
                        IntType = int_type_int64,
                        llds.lval_type(Lval, ActualType),
                        ( if ActualType = lt_int(int_type_int64) then
                            output_lval(Info, Stream, Lval, !IO),
                            io.format(Stream, " = %s", [s(VarName)], !IO)
                        else
                            output_lval_as_word(Info, Stream, Lval, !IO),
                            io.format(Stream, " = MR_int64_to_word(%s)",
                                [s(VarName)], !IO)
                        )
                    ;
                        IntType = int_type_uint64,
                        llds.lval_type(Lval, ActualType),
                        ( if ActualType = lt_int(int_type_uint64) then
                            output_lval(Info, Stream, Lval, !IO),
                            io.format(Stream, " = %s", [s(VarName)], !IO)
                        else
                            output_lval_as_word(Info, Stream, Lval, !IO),
                            io.format(Stream, " = MR_uint64_to_word(%s)",
                                [s(VarName)], !IO)
                        )
                    )
                )
            else
                output_lval_as_word(Info, Stream, Lval, !IO),
                io.format(Stream, " = %s", [s(VarName)], !IO)
            )
        )
    ),
    io.write_string(Stream, ";\n", !IO).

%---------------------------------------------------------------------------%
:- end_module ll_backend.llds_out.llds_out_instr.
%---------------------------------------------------------------------------%
