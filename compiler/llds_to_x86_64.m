%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: llds_to_x86_64.m.
% Main author: fhandoko.
%
% This module implements the LLDS->x86_64 asm code generator.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.llds_to_x86_64.
:- interface.

:- import_module hlds.hlds_module. 
:- import_module ll_backend.llds.
:- import_module ll_backend.x86_64_instrs.

:- import_module list.

%----------------------------------------------------------------------------%
    
:- pred llds_to_x86_64_asm(module_info::in, list(c_procedure)::in, 
    list(x86_64_procedure)::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.name_mangle.
:- import_module libs.compiler_util.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.x86_64_out.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% binary operation type.
%

:- type binop 
    --->    binop_simple_opands(operand, operand)
            % A binary operation with two simple operands, such as X + Y
            % where X and Y are primitive values (ie. integers). 
    ;       binop_simple_and_compound_opands(operand, list(x86_64_instr))
            % A binary operation can consist of one simple operand at the 
            % left hand side and a compound operand at the right hand side.
            % ie. X + Y where X can consist of further operations (such 
            % as Z + W, Z * W, etc) and Y is a primitive value. 
    ;       binop_compound_and_simple_opands(list(x86_64_instr), operand)
            % A binary operation can consist of a compound operand at the 
            % left hand side and a simple operand at the right hand side.
    ;       binop_compound_opands(list(x86_64_instr), list(x86_64_instr)).
            % Both operands in the binary operation are compound operands.

%----------------------------------------------------------------------------%
%
% llds to x86_64. 
%

llds_to_x86_64_asm(_, CProcs, AsmProcs) :-
    transform_c_proc_list(CProcs, AsmProcs).

    % Transform a list of c procedures into a list of x86_64 procedures. 
    %
:- pred transform_c_proc_list(list(c_procedure)::in, 
    list(x86_64_procedure)::out) is det.

transform_c_proc_list([], []).
transform_c_proc_list([CProc | CProcs], [AsmProc | AsmProcs]) :-
    AsmProc0 = ll_backend.x86_64_instrs.init_x86_64_proc(CProc),
    ProcInstr0 = ll_backend.x86_64_instrs.init_x86_64_instruction,
    % 
    % Get the procedure's label and append it as an x86_64 instruction
    % list before the output of the llds->x86_64 procedure transformation.
    % 
    ProcStr = backend_libs.name_mangle.proc_label_to_c_string(
        AsmProc0 ^ x86_64_proc_label, no),
    ProcName = x86_64_directive(x86_64_pseudo_type(ProcStr, "@function")),
    ProcInstr = ProcInstr0 ^ x86_64_inst := [ProcName],
    transform_c_instr_list(CProc ^ cproc_code, AsmInstr0),
    list.append([ProcInstr], AsmInstr0, AsmInstr),
    AsmProc = AsmProc0 ^ x86_64_code := AsmInstr,
    transform_c_proc_list(CProcs, AsmProcs).

    % Transform a list of c instructions into a list of x86_64 instructions. 
    % 
:- pred transform_c_instr_list(list(instruction)::in, 
    list(x86_64_instruction)::out) is det.

transform_c_instr_list([], []).
transform_c_instr_list([CInstr0 | CInstr0s], [AsmInstr | AsmInstrs]) :-
    CInstr0 = llds_instr(CInstr1, Comment),
    instr_to_x86_64(CInstr1, AsmInstrList),
    AsmInstr0 = ll_backend.x86_64_instrs.init_x86_64_instruction,
    AsmInstr1 = AsmInstr0 ^ x86_64_inst := AsmInstrList,
    AsmInstr = AsmInstr1 ^ x86_64_inst_comment := Comment,
    transform_c_instr_list(CInstr0s, AsmInstrs).

    % Transform a block instruction of an llds instruction into a list of 
    % x86_64 instructions.
    %
:- pred transform_block_instr(list(instruction)::in, list(x86_64_instr)::out) 
    is det. 

transform_block_instr(CInstrs, Instrs) :-
    transform_block_instr_list(CInstrs, ListInstrs),
    list.condense(ListInstrs, Instrs).

:- pred transform_block_instr_list(list(instruction)::in, 
    list(list(x86_64_instr))::out) is det. 

transform_block_instr_list([], []).
transform_block_instr_list([CInstr0 | CInstr0s], [Instr0 | Instr0s]) :-
    CInstr0 = llds_instr(CInstr, _),
    instr_to_x86_64(CInstr, Instr0),
    transform_block_instr_list(CInstr0s, Instr0s).

    % Transform livevals of llds instruction into a list of x86_64 instructions.
    %
:- pred transform_livevals(list(lval)::in, list(x86_64_instr)::out) is det.

transform_livevals([], []). 
transform_livevals([Lval | Lvals], [Instr | Instrs]) :-
    transform_lval(Lval, Res0, Res1),
    (
        Res0 = yes(LvalOp),
        Instr = x86_64_instr(mov(operand_label("<<livevals>>"), LvalOp)),
        transform_livevals(Lvals, Instrs)
    ;
        Res0 = no,
        ( 
            Res1 = yes(LvalInstrs),
            ( get_last_instr_opand(LvalInstrs, LastOp) ->
                Instr = x86_64_instr(mov(operand_label("<<livevals>>"), 
                    LastOp)),
                transform_livevals(Lvals, Instrs)
            ;
                unexpected(this_file, "transform_livevals: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res1 = no,
            unexpected(this_file, "transform_livevals: unexpected:"
                ++ " get_last_instr_opand failed")
        )
    ).

    % Given an llds instruction, transform it into equivalent x86_64 
    % instructions. 
    %
:- pred instr_to_x86_64(instr::in, list(x86_64_instr)::out) is det.

instr_to_x86_64(comment(Comment), [x86_64_comment(Comment)]).
instr_to_x86_64(livevals(RegsAndStackLocs), Instrs) :-
    set.to_sorted_list(RegsAndStackLocs, List),
    transform_livevals(List, Instrs).
instr_to_x86_64(block(_, _, CInstrs), Instrs) :-
    transform_block_instr(CInstrs, Instrs).
instr_to_x86_64(assign(Lval, Rval), Instrs) :-
    transform_lval(Lval, Res0, Res1),
    transform_rval(Rval, Res2, Res3),
    (
        Res0 = yes(LvalOp),
        ( 
            Res2 = yes(RvalOp),
            Instrs = [x86_64_instr(mov(RvalOp, LvalOp))]
        ;
            Res2 = no,
            ( 
                Res3 = yes(RvalInstrs),
                ( get_last_instr_opand(RvalInstrs, LastOp) ->
                    LastInstr = x86_64_instr(mov(LastOp, LvalOp)),
                    list.append(RvalInstrs, [LastInstr], Instrs)
                ;
                    unexpected(this_file, "instr_to_x86_64: assign: unexpected:"
                        ++ " get_last_instr_opand failed")
                )
            ;
                Res3 = no,
                unexpected(this_file, "instr_to_x86_64: assign: unexpected:"
                    ++ " Rval")
            )
        )
    ;
        Res0 = no,
        ( 
            Res1 = yes(LvalInstrs),
            ( get_last_instr_opand(LvalInstrs, LvalLastOp) ->
                (
                    Res2 = yes(RvalOp),
                    Instr0 = x86_64_instr(mov(RvalOp, LvalLastOp)),
                    list.append(LvalInstrs, [Instr0], Instrs)
                    
                ;
                    Res2 = no,
                    ( 
                        Res3 = yes(RvalInstrs),
                        ( get_last_instr_opand(RvalInstrs, RvalLastOp) ->
                            Instr0 = x86_64_instr(mov(RvalLastOp, LvalLastOp)),
                            Instrs = LvalInstrs ++ [Instr0] ++ RvalInstrs
                        ;
                            unexpected(this_file, "instr_to_x86_64: assign:" 
                                ++ " unexpected:get_last_instr_opand failed")
                        )
                    ;
                        Res3 = no,
                        unexpected(this_file, "instr_to_x86_64: assign:"
                            ++ " unexpected: Rval")
                    )
                )
            ;
                unexpected(this_file, "instr_to_x86_64: assign: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res1 = no,
            unexpected(this_file, "instr_to_x86_64: assign: unexpected:"
                ++ "Lval")
        )
    ).
instr_to_x86_64(llcall(Target0, Continuation0, _, _, _, _), Instrs) :-
    code_addr_type(Target0, Target1),
    code_addr_type(Continuation0, Continuation1),
    Instr1 = x86_64_instr(mov(operand_label(Continuation1), 
        operand_label("<<succip>>"))), 
    Instr2 = x86_64_instr(jmp(operand_label(Target1))),
    Instrs = [Instr1, Instr2].
instr_to_x86_64(mkframe(_, _), [x86_64_comment("<<mkframe>>")]).
instr_to_x86_64(label(Label), Instrs) :-
    LabelStr = ll_backend.llds_out.label_to_c_string(Label, no),
    Instrs = [x86_64_label(LabelStr)]. 
instr_to_x86_64(goto(CodeAddr), Instrs) :-
    code_addr_type(CodeAddr, Label),
    Instrs = [x86_64_instr(jmp(operand_label(Label)))].
instr_to_x86_64(computed_goto(Rval, Labels), Instrs) :-
    transform_rval(Rval, Res0, Res1),
    (
        Res0 = yes(RvalOp),
        RvalInstrs = []
    ;
        Res0 = no,
        ( 
            Res1 = yes(RvalInstrs),
            ( get_last_instr_opand(RvalInstrs, RvalOp0) ->
                RvalOp = RvalOp0
            ;
                unexpected(this_file, "instr_to_x86_64: computed_goto:" 
                    ++ " unexpected: get_last_instr_opand failed")
            )
        ;
            Res1 = no,
            unexpected(this_file, "instr_to_x86_64: computed_goto: unexpected:"
                ++ " Rval")
        )
    ),
    labels_to_string(Labels, "", LabelStr),
    TempReg = operand_reg(gp_reg(13)),
    Instr0 = x86_64_instr(mov(operand_mem_ref(mem_abs(base_expr(LabelStr))), 
        TempReg)),
    Instr1 = x86_64_instr(add(RvalOp, TempReg)),
    Instr2 = x86_64_instr(jmp(TempReg)),
    Instrs = RvalInstrs ++ [Instr0] ++ [Instr1] ++ [Instr2].
instr_to_x86_64(arbitrary_c_code(_, _, _), Instrs) :-
    Instrs = [x86_64_comment("<<arbitrary_c_code>>")].
instr_to_x86_64(if_val(Rval, CodeAddr), Instrs) :-
    code_addr_type(CodeAddr, Target),
    transform_rval(Rval, Res0, Res1),
    (
        Res0 = yes(RvalOp)
    ;
        Res0 = no,
        ( 
            Res1 = yes(RvalInstrs),
            ( get_last_instr_opand(RvalInstrs, LastOp) ->
                RvalOp = LastOp
            ;
                unexpected(this_file, "instr_to_x86_64: if_val: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res1 = no,
            unexpected(this_file, "instr_to_x86_64: if_val: unexpected:"
                ++ " Rval")
        )
    ),
    ll_backend.x86_64_out.operand_type(RvalOp, RvalStr),
    Instrs = [x86_64_directive(x86_64_pseudo_if(RvalStr)), x86_64_instr(j(
        operand_label(Target), e)), x86_64_directive(endif)].
instr_to_x86_64(save_maxfr(_), [x86_64_comment("<<save_maxfr>>")]).
instr_to_x86_64(restore_maxfr(_), [x86_64_comment("<<restore_maxfr>>")]).
instr_to_x86_64(incr_hp(Lval, Tag0, Words0, Rval, _, _), Instrs) :-
    transform_rval(Rval, Res0, Res1),
    transform_lval(Lval, Res2, Res3),
    (
        Res0 = yes(RvalOp)
    ;
        Res0 = no,
        (  
            Res1 = yes(RvalInstrs),
            ( get_last_instr_opand(RvalInstrs, RvalLastOp) ->
                RvalOp = RvalLastOp
            ;
                unexpected(this_file, "instr_to_x86_64: incr_hp: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res1 = no,
            unexpected(this_file, "instr_to_x86_64: incr_hp: unexpected:"
                ++ " Rval")
        )
    ),
    (
        Res2 = yes(LvalOp)
    ;
        Res2 = no,
        ( 
            Res3 = yes(LvalInstrs),
            ( get_last_instr_opand(LvalInstrs, LvalLastOp) ->
                LvalOp = LvalLastOp    
            ;
                unexpected(this_file, "instr_to_x86_64: incr_hp: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res3 = no,
            unexpected(this_file, "instr_to_x86_64: incr_hp: unexpected:" 
                ++ " Lval")
        ) 
    ),
    (
        Words0 = yes(Words),
        IncrVal = operand_imm(imm32(int32(Words))),
        TempReg = operand_reg(gp_reg(13)),
        ll_backend.x86_64_out.operand_type(RvalOp, RvalStr),
        MemRef = operand_mem_ref(mem_abs(base_expr(RvalStr))),
        LoadAddr = x86_64_instr(lea(MemRef, TempReg)),
        IncrAddInstr = x86_64_instr(add(IncrVal, TempReg)),
        list.append([LoadAddr], [IncrAddInstr], IncrAddrInstrs)
    ;
        Words0 = no,
        IncrAddrInstrs = []
    ),
    ( 
        Tag0 = yes(Tag)
    ;
        Tag0 = no,
        Tag = 0
    ),
    TempReg = operand_reg(gp_reg(13)),
    ImmToReg = x86_64_instr(mov(RvalOp, TempReg)),
    SetTag = x86_64_instr(or(operand_imm(imm32(int32(Tag))), TempReg)),
    Instr0 = x86_64_instr(mov(TempReg, LvalOp)),
    Instrs = IncrAddrInstrs ++ [ImmToReg] ++ [SetTag] ++ [Instr0]. 
instr_to_x86_64(mark_hp(_), [x86_64_comment("<<mark_hp>>")]).
instr_to_x86_64(restore_hp(_), [x86_64_comment("<<restore_hp>>")]).
instr_to_x86_64(free_heap(_), [x86_64_comment("<<free_heap>>")]).
instr_to_x86_64(store_ticket(_), [x86_64_comment("<<store_ticket>>")]).
instr_to_x86_64(reset_ticket(_, _), [x86_64_comment("<<reset_ticket>>")]).
instr_to_x86_64(prune_ticket, [x86_64_comment("<<prune_ticket>>")]).
instr_to_x86_64(discard_ticket, [x86_64_comment("<<discard_ticket>>")]).
instr_to_x86_64(mark_ticket_stack(_), [x86_64_comment("<<mark_ticket_stack>>")]).
instr_to_x86_64(prune_tickets_to(_), [x86_64_comment("<<prune_tickets_to>>")]).
instr_to_x86_64(incr_sp(NumSlots, ProcName, _), Instrs) :-
    Instr1 = x86_64_comment("<<incr_sp>> " ++ ProcName),
    Instr2 = x86_64_instr(enter(uint16(NumSlots), uint8(0))),
    Instrs = [Instr1, Instr2].
instr_to_x86_64(decr_sp(NumSlots), Instrs) :-
    DecrOp = operand_imm(imm32(int32(NumSlots))),
    Instr = x86_64_instr(sub(DecrOp, operand_reg(gp_reg(13)))), 
    list.append([x86_64_comment("<<decr_sp>> ")], [Instr], Instrs).
instr_to_x86_64(decr_sp_and_return(NumSlots), Instrs) :-
    Instrs = [x86_64_comment("<<decr_sp_and_return>> " ++ 
        string.int_to_string(NumSlots))].
instr_to_x86_64(foreign_proc_code(_, _, _, _, _, _, _, _, _), 
    [x86_64_comment("<<foreign_proc_code>>")]).
instr_to_x86_64(init_sync_term(_, _), [x86_64_comment("<<init_sync_term>>")]).
instr_to_x86_64(fork(_), [x86_64_comment("<<fork>>")]).
instr_to_x86_64(join_and_continue(_, _), [x86_64_comment("<<join_and_continue>>")]).


    % Transform lval into either an x86_64 operand or x86_64 instructions.
    %
:- pred transform_lval(lval::in, maybe(operand)::out, 
    maybe(list(x86_64_instr))::out) is det. 

transform_lval(reg(CReg, CRegNum), Op, no) :-
    ( 
        CReg = reg_r,
        Op = yes(operand_reg(gp_reg(CRegNum)))
    ;
        CReg = reg_f,
        Op = no
    ).
transform_lval(succip, yes(operand_label("<<succip>>")), no).
transform_lval(maxfr, yes(operand_label("<<maxfr>>")), no).
transform_lval(curfr, yes(operand_label("<<curfr>>")), no).
transform_lval(hp, yes(operand_label("<<hp>>")), no).
transform_lval(sp, yes(operand_label("<<sp>>")), no).
transform_lval(parent_sp, yes(operand_label("<<parent_sp>>")), no).
transform_lval(temp(CReg, CRegNum), Op, no) :-
    ( 
        CReg = reg_r,
        Op = yes(operand_reg(gp_reg(CRegNum)))
    ;
        CReg = reg_f,
        Op = no
    ).
transform_lval(stackvar(Offset), Op, no) :-
    Op = yes(operand_label(string.int_to_string(Offset) ++ "(<<stackvar>>)")).
transform_lval(parent_stackvar(_), yes(operand_label("<<parent_stackvar>>")), no).
transform_lval(framevar(_), yes(operand_label("<<framevar>>")), no).
transform_lval(succip_slot(Rval), Op, no) :-
    transform_rval(Rval, Op, _).
transform_lval(redoip_slot(Rval), Op, no) :-
    transform_rval(Rval, Op, _).
transform_lval(redofr_slot(Rval), Op, no) :-
    transform_rval(Rval, Op, _).
transform_lval(succfr_slot(Rval), Op, no) :-
    transform_rval(Rval, Op, _).
transform_lval(prevfr_slot(Rval), Op, no) :-
    transform_rval(Rval, Op, _).
transform_lval(mem_ref(Rval), Op, no) :-
    transform_rval(Rval, Op, _).
transform_lval(global_var_ref(env_var_ref(Name)), yes(operand_label(Name)), no).
transform_lval(lvar(_), yes(operand_label("<<lvar>>")), no).
transform_lval(field(Tag0, Rval1, Rval2), no, Instrs) :-
    transform_rval(Rval1, Res0, Res1),
    transform_rval(Rval2, Res2, Res3),
    (
        Res0 = yes(RvalOp1),
        Instrs1 = []
    ;
        Res0 = no,
        ( 
            Res1 = yes(RvalInstrs1),
            ( get_last_instr_opand(RvalInstrs1, LastOp1) ->
                RvalOp1 = LastOp1,
                Instrs1 = RvalInstrs1
            ;
                unexpected(this_file, "lval_instrs: field: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res1 = no,
            unexpected(this_file, "lval_instrs: field: unexpected: Rval1")
        )
    ),
    (
        Res2 = yes(RvalOp2),
        Instrs2 = []
    ;
        Res2 = no,
        (
            Res3 = yes(RvalInstrs2),
            ( get_last_instr_opand(RvalInstrs2, LastOp2) ->
                RvalOp2 = LastOp2,
                Instrs2 = RvalInstrs2
            ;
                unexpected(this_file, "lval_instrs: field: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res3 = no,
            unexpected(this_file, "lval_instrs: field: unexpected: Rval2")
        )
    ),
    TempReg1 = operand_reg(gp_reg(13)),
    ll_backend.x86_64_out.operand_type(RvalOp1, RvalStr1),
    MemRef = operand_mem_ref(mem_abs(base_expr(RvalStr1))),
    LoadAddr = x86_64_instr(lea(MemRef, TempReg1)),
    FieldNum = x86_64_instr(add(RvalOp2, TempReg1)),
    Instrs3 = Instrs1 ++ Instrs2 ++ [x86_64_comment("<<field>>")] ++ [LoadAddr],
    (
        Tag0 = yes(Tag),
        Mrbody = x86_64_instr(sub(operand_imm(imm32(int32(Tag))), TempReg1)),
        Instrs = yes(Instrs3 ++ [Mrbody] ++ [FieldNum])
    ;
        Tag0 = no,
        Instrs = yes(Instrs3 ++ [FieldNum])
    ).

    % Translates rval into its corresponding x86_64 operand. 
    %
:- pred transform_rval(rval::in, maybe(operand)::out, maybe(list(x86_64_instr))
    ::out) is det. 

transform_rval(lval(Lval0), Op, Instrs) :-
    transform_lval(Lval0, Op, Instrs).
transform_rval(var(_), yes(operand_label("<<var>>")), no).
transform_rval(mkword(Tag, Rval), no, Instrs) :-
    transform_rval(Rval, Res0, Res1),
    (
        Res0 = yes(RvalOp),
        list.append([x86_64_comment("<<mkword>>")], [], Instr0)
    ;
        Res0 = no,
        (
            Res1 = yes(RvalInstrs),
            ( get_last_instr_opand(RvalInstrs, LastOp) ->
                RvalOp = LastOp,
                list.append(RvalInstrs, [x86_64_comment("<<mkword>>")], Instr0)
            ;
                unexpected(this_file, "transform_rval: mkword: unexpected:"
                    ++ " get_last_instr_opand failed")
            )
        ;
            Res1 = no,
            unexpected(this_file, "transform_rval: mkword unexpected: Rval")
        )
    ),
    TempReg = operand_reg(gp_reg(13)),
    ll_backend.x86_64_out.operand_type(RvalOp, RvalStr),
    MemRef = operand_mem_ref(mem_abs(base_expr(RvalStr))),
    LoadAddr = x86_64_instr(lea(MemRef, TempReg)),
    SetTag = x86_64_instr(add(operand_imm(imm32(int32(Tag))), TempReg)),
    Instrs = yes(Instr0 ++ [LoadAddr] ++ [SetTag]).
transform_rval(const(llconst_true), yes(operand_label("<<llconst_true>>")), no).
transform_rval(const(llconst_false), yes(operand_label("<<llconst_false>>")), no).
transform_rval(const(llconst_int(Val)), yes(operand_imm(imm32(int32(Val)))), no).
transform_rval(const(llconst_float(_)), yes(operand_label("<<llconst_float>>")), no).
transform_rval(const(llconst_string(String)), no, yes(Op)) :-
    Op = [x86_64_directive(string([String]))].
transform_rval(const(llconst_multi_string(_, _)), Op, no) :-
    Op = yes(operand_label("<<llconst_multi_string>>")).
transform_rval(const(llconst_code_addr(CodeAddr)), Op, no) :-
    code_addr_type(CodeAddr, CodeAddrType),
    Op = yes(operand_label("<<llconst_code_addr>>" ++ CodeAddrType)).
transform_rval(const(llconst_data_addr(_, _)), Op, no) :-
    Op = yes(operand_label("<<llconst_data_addr>>")).
transform_rval(unop(Op, Rval), no, Instrs) :-
    transform_rval(Rval, Res0, Res1),
    (
        Res0 = yes(_),
        unop_instrs(Op, Res0, no, Instrs0),
        Instrs = yes(Instrs0)
    ;
        Res0 = no,
        (  
            Res1 = yes(_),
            unop_instrs(Op, no, Res1, Instrs0),
            Instrs = yes(Instrs0)
        ;
            Res1 = no,
            unexpected(this_file, "transform_rval: unop: unexpected: Rval")
        )
    ).
transform_rval(binop(Op, Rval1, Rval2), no, Instrs) :-
    transform_rval(Rval1, Res1, Res2),
    transform_rval(Rval2, Res3, Res4),
    (
        Res1 = yes(Val1),
        (
            Res3 = yes(Val2),
            binop_instrs(binop_simple_opands(Val1, Val2), Op, Instrs0),
            Instrs = yes(Instrs0)
        ;
            Res3 = no,
            (
                Res4 = yes(RvalInstr2),
                binop_instrs(binop_simple_and_compound_opands(Val1, RvalInstr2),
                    Op, Instrs0),
                Instrs = yes(Instrs0)
            ;
                Res4 = no,
                Instrs = no
            )
        )
    ;
        Res1 = no,
        (
            Res2 = yes(RvalInstr1),
            (
                Res3 = yes(Val2),
                binop_instrs(binop_compound_and_simple_opands(RvalInstr1, Val2),
                    Op, Instrs0),
                Instrs = yes(Instrs0)
            ;
                Res3 = no,
                (
                    Res4 = yes(RvalInstr2),
                    binop_instrs(binop_compound_opands(RvalInstr1, RvalInstr2), 
                        Op, Instrs0),
                    Instrs = yes(Instrs0)
                ;
                    Res4 = no,
                    unexpected(this_file, "rval_instrs: binop: unexpected:" 
                        ++ " Rval2")
                )
            )
        ;
            Res2 = no,
            unexpected(this_file, "rval_instrs: binop: unexpected: Rval1")
        )
    ).
transform_rval(mem_addr(stackvar_ref(Rval)), Op, no) :-
    transform_rval(Rval, Op, _). 
transform_rval(mem_addr(framevar_ref(Rval)), Op, no) :-
    transform_rval(Rval, Op, _). 
transform_rval(mem_addr(heap_ref(Rval1, Tag, Rval2)), no, Instrs) :-
    transform_rval(Rval1, Res0, Res1),
    transform_rval(Rval2, Res2, Res3),
    (
        Res0 = yes(Rval1Op),
        (
            Res2 = yes(Rval2Op),
            Instrs0 = []
        ;
            Res2 = no,
            ( 
                Res3 = yes(Rval2Instr),
                Instrs0 = Rval2Instr,
                ( get_last_instr_opand(Rval2Instr, LastOp) ->
                     Rval2Op = LastOp
                ;
                    unexpected(this_file, "transform_rval: mem_addr(heap_ref):"
                        ++ " unexpected: get_last_instr_opand failed")
                )
            ;
                Res3 = no,
                unexpected(this_file, "transform_rval: mem_addr(heap_ref):"
                    ++ " unexpected: Rval2")
            )
        )
    ;
        Res0 = no,
        (
            Res1 = yes(Rval1Instr),
            ( get_last_instr_opand(Rval1Instr, LastOp) ->
                Rval1Op = LastOp,
                (
                    Res2 = yes(Rval2Op),
                    Instrs0 = Rval1Instr
                ;
                    Res2 = no,
                    ( 
                        Res3 = yes(Rval2Instr),
                        ( get_last_instr_opand(Rval2Instr, LastOp) ->
                            Rval2Op = LastOp,
                            list.append(Rval1Instr, Rval2Instr, Instrs0)
                        ;
                            unexpected(this_file, "transform_rval:"
                                ++ " mem_addr(heap_ref): unexpected:"
                                ++ " get_last_instr_opand failed")
                        )
                    ;
                        Res3 = no,
                        unexpected(this_file, "transform_rval:"
                            ++ " mem_addr(heap_ref): unexpected: Rval2")
                    )
                )
            ;
                unexpected(this_file, "transform_rval: mem_addr(heap_ref):" 
                    ++ " unexpected: get_last_instr_opand failed")
            )
       ;
            Res1 = no, 
            unexpected(this_file, "transform_rval: mem_addr(heap_ref)" 
                ++ " unexpected: Rval1")
       )
    ),
    TempReg = operand_reg(gp_reg(13)),
    ll_backend.x86_64_out.operand_type(Rval1Op, Rval1Str),
    MemRef = operand_mem_ref(mem_abs(base_expr(Rval1Str))),
    LoadAddr = x86_64_instr(lea(MemRef, TempReg)),
    Instr0 = x86_64_instr(sub(Rval2Op, TempReg)),
    Instr1 = x86_64_instr(add(operand_imm(imm32(int32(Tag))), TempReg)),
    Instrs = yes(Instrs0 ++ [LoadAddr] ++ [Instr0] ++ [Instr1]).

    % x86_64 instructions for binary operation with either an operand or an 
    % expression (given as a list of x86_64 instructions) or a combination of 
    % both.
    %
:- pred binop_instrs(binop::in, binary_op::in, list(x86_64_instr)::out) is det. 

binop_instrs(binop_simple_opands(Op1, Op2), Op, Instrs) :-
    binop_instr(Op, Op1, Op2, Instrs).
binop_instrs(binop_simple_and_compound_opands(Op1, InstrsOp), Op, Instrs) :-
    ( get_last_instr_opand(InstrsOp, LastOp) ->
        binop_instr(Op, Op1, LastOp, Instrs0),
        list.append(InstrsOp, Instrs0, Instrs)
    ;
        unexpected(this_file, "binop_instrs: binop_simple_and_compound_opands"
            ++ " unexpected: get_last_instr_opand failed")
    ).
binop_instrs(binop_compound_and_simple_opands(InstrsOp, Op2), Op, Instrs) :-
    ( get_last_instr_opand(InstrsOp, LastOp) ->
        binop_instr(Op, LastOp, Op2, Instrs0),
        list.append(InstrsOp, Instrs0, Instrs)
    ;
        unexpected(this_file, "binop_instrs: binop_compound_and_simple_opands:" 
            ++ " unexpected: get_last_instr_opand failed")
    ).
binop_instrs(binop_compound_opands(InstrsOp1, InstrsOp2), Op, Instrs) :-
    (
        get_last_instr_opand(InstrsOp1, LastOp1),
        get_last_instr_opand(InstrsOp2, LastOp2)
    ->
        binop_instr(Op, LastOp1, LastOp2, Instrs0), 
        Instrs = InstrsOp1 ++ InstrsOp2 ++ Instrs0
    ;
        unexpected(this_file, "binop_instrs: binop_compound_opands: unexpected:"
            ++ " get_last_instr_opand failed")
    ).

    % Equivalent x86_64 instructions for a unary operation. A unary operation
    % may consist of an operand or an expression (as a list of x86_64 
    % instructions). 
    %
:- pred unop_instrs(backend_libs.builtin_ops.unary_op::in, maybe(operand)::in,
    maybe(list(x86_64_instr))::in, list(x86_64_instr)::out) is det. 

unop_instrs(mktag, _, _, [x86_64_comment("<<mktag>>")]).
unop_instrs(tag, _, _, [x86_64_comment("<<tag>>")]).
unop_instrs(unmktag, _, _, [x86_64_comment("<<unmktag>>")]).
unop_instrs(strip_tag, _, _, [x86_64_comment("<<strip_tag>>")]).
unop_instrs(mkbody, _, _, [x86_64_comment("<<mkbody>>")]).
unop_instrs(unmkbody, _, _, [x86_64_comment("<<unmkbody>>")]).
unop_instrs(hash_string, _, _, [x86_64_comment("<<hash_string>>")]).
unop_instrs(bitwise_complement, Op, Instrs0, Instrs) :-
    ( 
        Op = yes(OpRes),
        Instrs = [x86_64_instr(x86_64_instr_not(OpRes))]
    ;
        Op = no,
        (
            Instrs0 = yes(InsRes),
            ( get_last_instr_opand(InsRes, LastOp) ->
                list.append(InsRes, [x86_64_instr(x86_64_instr_not(LastOp))], 
                    Instrs)
            ;
                unexpected(this_file, "unop_instrs: bitwise_complement:" 
                    ++ " unexpected: get_last_instr_opand failed")
            )
        ;
            Instrs0 = no,
            unexpected(this_file, "unop_instrs: bitwise_complement:"
                ++ " unexpected: instruction operand Instrs0")
        )
    ).
unop_instrs(logical_not, _, _, [x86_64_comment("<<logical_not>>")]).

    % Equivalent x86_64 instructions for a binary operation.
    %
:- pred binop_instr(backend_libs.builtin_ops.binary_op::in, operand::in, 
    operand::in, list(x86_64_instr)::out) is det. 

binop_instr(int_add, Op1, Op2, [x86_64_instr(add(Op1, Op2))]). 
binop_instr(int_sub, Op1, Op2, [x86_64_instr(sub(Op1, Op2))]). 
binop_instr(int_mul, Op1, Op2, [x86_64_instr(imul(Op1, yes(Op2), no))]).
binop_instr(int_mod, _, _, [x86_64_comment("<<int_mod>>")]).
binop_instr(int_div, Op1, Op2, Instrs) :-
    LoadDividend = x86_64_instr(mov(Op2, operand_label("<<int_div>>"))),
    list.cons(LoadDividend, [x86_64_instr(idiv(Op1))], Instrs).
binop_instr(unchecked_left_shift, Op1, Op2, [x86_64_instr(shl(Op1, Op2))]). 
binop_instr(unchecked_right_shift, Op1, Op2, [x86_64_instr(shr(Op1, Op2))]). 
binop_instr(bitwise_and, Op1, Op2, [x86_64_instr(and(Op1, Op2))]). 
binop_instr(bitwise_or, _, _, [x86_64_comment("<<bitwise_or>>")]).
binop_instr(bitwise_xor, Op1, Op2, [x86_64_instr(xor(Op1, Op2))]). 
binop_instr(logical_and, _, _, [x86_64_comment("<<logical_and>>")]).
binop_instr(logical_or, _, _, [x86_64_comment("<<logical_or>>")]).
binop_instr(eq, Op1, Op2, [x86_64_instr(cmp(Op1, Op2))]). 
binop_instr(ne, Op1, Op2, [x86_64_instr(cmp(Op1, Op2))]). 
binop_instr(body, _, _, [x86_64_comment("<<body>>")]).
binop_instr(array_index(_), _, _, [x86_64_comment("<<array_index>>")]).
binop_instr(str_eq, _, _, [x86_64_comment("<<str_eq>>")]).
binop_instr(str_ne, _, _, [x86_64_comment("<<str_ne>>")]).
binop_instr(str_lt, _, _, [x86_64_comment("<<str_lt>>")]).
binop_instr(str_gt, _, _, [x86_64_comment("<<str_gt>>")]).
binop_instr(str_le, _, _, [x86_64_comment("<<str_le>>")]).
binop_instr(str_ge, _, _, [x86_64_comment("<<str_ge>>")]).
binop_instr(int_lt, Op1, Op2, [x86_64_instr(cmp(Op1, Op2))]). 
binop_instr(int_gt, Op1, Op2, [x86_64_instr(cmp(Op1, Op2))]). 
binop_instr(int_le, Op1, Op2, [x86_64_instr(cmp(Op1, Op2))]). 
binop_instr(int_ge, Op1, Op2, [x86_64_instr(cmp(Op1, Op2))]). 
binop_instr(unsigned_le, Op1, Op2, [x86_64_instr(cmp(Op1, Op2))]). 
binop_instr(float_plus, _, _, [x86_64_comment("<<float_plus>>")]).
binop_instr(float_minus, _, _, [x86_64_comment("<<float_minus>>")]).
binop_instr(float_times, _, _, [x86_64_comment("<<float_times>>")]).
binop_instr(float_divide, _, _, [x86_64_comment("<<float_divide>>")]).
binop_instr(float_eq, _, _, [x86_64_comment("<<float_eq>>")]).
binop_instr(float_ne, _, _, [x86_64_comment("<<float_ne>>")]).
binop_instr(float_lt, _, _, [x86_64_comment("<<float_lt>>")]).
binop_instr(float_gt, _, _, [x86_64_comment("<<float_gt>>")]).
binop_instr(float_le, _, _, [x86_64_comment("<<float_le>>")]).
binop_instr(float_ge, _, _, [x86_64_comment("<<float_ge>>")]).

    % Get a string representation of code address types. 
    %
:- pred code_addr_type(code_addr::in, string::out) is det.

code_addr_type(code_label(Label), CodeAddr) :-
    CodeAddr = "$" ++  ll_backend.llds_out.label_to_c_string(Label, no).
code_addr_type(code_imported_proc(ProcLabel), CodeAddr) :-
    CodeAddr = "$" ++ 
        backend_libs.name_mangle.proc_label_to_c_string(ProcLabel, no).
code_addr_type(code_succip, CodeAddr) :-
    CodeAddr = "<<code_succip>>".
code_addr_type(do_succeed(_), CodeAddr) :-
    CodeAddr = "<<do_succeed>>".
code_addr_type(do_redo, CodeAddr) :-
    CodeAddr = "<<do_redo>>".
code_addr_type(do_fail, CodeAddr) :-
    CodeAddr = "<<do_fail>>".
code_addr_type(do_trace_redo_fail_shallow, CodeAddr) :-
    CodeAddr = "<<do_trace_redo_fail_shallow>>".
code_addr_type(do_trace_redo_fail_deep, CodeAddr) :-
    CodeAddr = "<<do_trace_redo_fail_deep>>".
code_addr_type(do_call_closure(_), CodeAddr) :-
    CodeAddr = "<<do_call_closure>>".
code_addr_type(do_call_class_method(_), CodeAddr) :-
    CodeAddr = "<<do_call_class_method>>".
code_addr_type(do_not_reached, CodeAddr) :-
    CodeAddr = "<<do_not_reached>>".

    % Given a list of x86_64 instructions, figure out the operand of the last 
    % instruction in the list. 
    %
:- pred get_last_instr_opand(list(x86_64_instr)::in, operand::out) is semidet.

get_last_instr_opand(Instrs, Op) :-
    list.last_det(Instrs, LastInstr),
    (  
       LastInstr = x86_64_comment(Comment),
       Op = operand_label(Comment)
    ;
       LastInstr = x86_64_label(Label),
       Op = operand_label(Label)
    ;
       LastInstr = x86_64_instr(Instr),
       last_instr_dest(Instr, Op)
    ;
       LastInstr = x86_64_directive(_), 
       Op = operand_label("<<directive>>")
     ).

    % Destination operand of an x86_64_instruction. 
    %
:- pred last_instr_dest(x86_64_inst::in, operand::out) is semidet. 

last_instr_dest(adc(_, Dest), Dest).
last_instr_dest(add(_, Dest), Dest).
last_instr_dest(and(_, Dest), Dest).
last_instr_dest(bs(_, Dest, _), Dest).
last_instr_dest(bswap(Dest), Dest).
last_instr_dest(cmov(_, Dest, _), Dest).
last_instr_dest(cmp(_, Dest), Dest).
last_instr_dest(cmpxchg(_, Dest), Dest).
last_instr_dest(cmpxchg8b(Dest), Dest).
last_instr_dest(dec(Dest), Dest).
last_instr_dest(div(Dest), Dest).
last_instr_dest(idiv(Dest), Dest).
last_instr_dest(imul(_, yes(Dest), _), Dest).
last_instr_dest(inc(Dest), Dest).
last_instr_dest(jrcxz(Dest), Dest).
last_instr_dest(jmp(Dest), Dest).
last_instr_dest(lea(_, Dest), Dest).
last_instr_dest(loop(Dest), Dest).
last_instr_dest(loope(Dest), Dest).
last_instr_dest(loopne(Dest), Dest).
last_instr_dest(loopnz(Dest), Dest).
last_instr_dest(loopz(Dest), Dest).
last_instr_dest(mov(_, Dest), Dest).
last_instr_dest(mul(Dest), Dest).
last_instr_dest(x86_64_instr_not(Dest), Dest).
last_instr_dest(or(_, Dest), Dest).
last_instr_dest(pop(Dest), Dest).
last_instr_dest(push(Dest), Dest).
last_instr_dest(rc(_, Dest, _), Dest).
last_instr_dest(ro(_, Dest, _), Dest).
last_instr_dest(sal(_, Dest), Dest).
last_instr_dest(shl(_, Dest), Dest).
last_instr_dest(sar(_, Dest), Dest).
last_instr_dest(sbb(_, Dest), Dest).
last_instr_dest(set(Dest, _), Dest).
last_instr_dest(shr(_, Dest), Dest).
last_instr_dest(sub(_, Dest), Dest).
last_instr_dest(xadd(_, Dest), Dest).
last_instr_dest(xor(_, Dest), Dest).

    % Get a string representation of llds labels. 
    %
:- pred labels_to_string(list(label)::in, string::in, string::out) is det. 

labels_to_string([], Str, Str).
labels_to_string([Label | Labels], Str0, Str) :-
    LabelStr = ll_backend.llds_out.label_to_c_string(Label, no),
    labels_to_string(Labels, Str0 ++ LabelStr, Str).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "llds_to_x86_64.m".

%----------------------------------------------------------------------------%
:- end_module llds_to_x86_64.
%----------------------------------------------------------------------------%
