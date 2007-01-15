%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: x86_64_out.m.
% Main author: fhandoko.
%
% This module defines the routines for printing out x86_64 instructions. 
%
%-----------------------------------------------------------------------------%

:- module ll_backend.x86_64_out.
:- interface.

:- import_module ll_backend.x86_64_instrs.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred output_x86_64_instrs(x86_64_instr::in, io::di, io::uo) is det.

    % XXX This is just for testing purposes.
    %
% :- pred pretend_main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module type_desc.

%-----------------------------------------------------------------------------%

    % XXX This is just for testing purposes.
    %
% pretend_main(!IO) :-
%     Comment1 = comment("This is a comment"),
%     Label1 = label(".L1"),
%     PseudoOps1 = [
%         abort,
%         align(6, no, yes(2)),
%         align(7, yes(12), no),
%         align(8, no, no),
%         ascii([".LI", ".L2"]),
%         comm(".L3", 8, no),
%         section(".data", no, no, no)
%         ],
%     Instrs1 = [
%         x86_64_instr(adc(operand_reg(rax), 
%                      rmro_reg(rbx) 
%                     ), ""),
%         x86_64_instr(add(operand_imm(imm8(int8(0x178))), 
%                      rmro_mem_ref(mem_abs(base_reg(0, r8)))
%                     ), ""),
%         x86_64_instr(and(operand_reg(rdx), 
%                      rmro_mem_ref(mem_abs(base_reg(2, rdx)))
%                     ), ""),
%         x86_64_instr(bsf(rmro_reg(r8), r15), ""),
%         x86_64_instr(bsr(rmro_reg(rcx), rax), ""),
%         x86_64_instr(bswap(r10), ""),
%         x86_64_instr(bt(rmro_mem_ref(mem_rip(rip_expr(".L1"))), 
%             rio_reg(rsi)), ""),
%         x86_64_instr(btc(rmro_reg(rdi), rio_imm(imm16(int16(0x1822)))), ""),
%         x86_64_instr(btr(rmro_reg(rbp), 
%             rio_imm(imm32(int32(0x182199)))), ""),
%         x86_64_instr(call(rmrol_rel_offset(ro8(int8(127)))), "comment"),
%         x86_64_instr(cmovo(rmro_mem_ref(mem_rip(rip_constant(int32(2)))), 
%             r11), ""),
%         x86_64_instr(mov(operand_imm(imm32(int32(10))), rmro_reg(rbx)), ""),
%         x86_64_instr(or(operand_mem_ref(mem_rip(rip_constant(int32(2)))), 
%            rmro_reg(rcx)), ""), 
%         x86_64_instr(push(operand_mem_ref(mem_abs(base_expr(".L2")))), ""),
%          x86_64_instr(rol(crio_reg_cl(rcx), rmro_reg(rax)), ""),
%         x86_64_instr(ror(crio_imm8(int8(20)), rmro_mem_ref(mem_rip(
%             rip_expr(".L2")) )), ""),
%         x86_64_instr(sbb(operand_mem_ref(mem_rip(rip_expr(".L1"))), 
%             rmro_reg(r11)), "")
%         ],
%     
%     Label2 = label(".L2"),
%     PseudoOps2 = [
%         section(".rodata", yes("aMS"), yes("@progbits"), yes(1)),
%         p2align(1,no,yes(7)),
%         type_("Type", "@function")
%         ],
%     Instrs2 = [ 
%         x86_64_instr(loop(ro8(int8(19))), "another comment"),
%         x86_64_instr(xor(operand_imm(imm32(int32(19))), 
%                      rmro_mem_ref(mem_rip(rip_expr(".L4")))
%                     ), "comment"),
%         x86_64_instr(jo(ro8(int8(22))), "comment again"),
%         x86_64_instr(div(rmro_reg(rdx)), "comment div"),
%         x86_64_instr(jmp(rmrol_label(".L2")), "comment jmp"),
%         x86_64_instr(mov(operand_mem_ref(mem_abs(base_expr(".L3"))), 
%             rmro_reg(rbx)), "")
%         ],
% 
%     Instructions = [
%         comment(""), label(""), directives([file("x86_64_out.m")]), 
%           instrs([]),
%         Comment1, Label1, directives(PseudoOps1), instrs(Instrs1), 
%         comment("Comment"), Label2, directives(PseudoOps2), 
%           instrs(Instrs2) 
%         ],
%     list.foldl(output_x86_64_instrs, Instructions, !IO).

%-----------------------------------------------------------------------------%

    % Output x86_64 instructions including comments, labels and pseudo-ops. 
    %
output_x86_64_instrs(comment(Comment), !IO) :-
    ( string.length(Comment) > 0 ->
        io.write_string("# " ++ Comment ++ "\n", !IO)
    ;
        true
    ).
output_x86_64_instrs(label(LabelName), !IO) :-
    ( string.length(LabelName) > 0 ->
        io.write_string(LabelName ++ ":\n", !IO)
    ;
        true
    ).
output_x86_64_instrs(directives(PseudoOps), !IO) :-
    list.foldl(output_x86_64_pseudo_op, PseudoOps, !IO).
output_x86_64_instrs(instrs(Instrs), !IO) :-
    ( list.length(Instrs) > 0 ->
        output_x86_64_instr_and_comment(Instrs, !IO)
    ;   
        true
    ).

%-----------------------------------------------------------------------------%
%
% Output x86_64 pseudo-op.
%

:- pred output_x86_64_pseudo_op(pseudo_op::in, io::di, io::uo) is det.

output_x86_64_pseudo_op(abort, !IO) :-
    io.write_string("\t.abort\n", !IO).
output_x86_64_pseudo_op(align(Bytes, FillVal, SkipBytes), !IO) :-
    output_pseudo_op_with_int_args(".align", Bytes, FillVal, SkipBytes, !IO).
output_x86_64_pseudo_op(ascii(Literals), !IO) :-
    output_pseudo_op_str_args(".ascii", Literals, !IO).
output_x86_64_pseudo_op(asciiz(Literals), !IO) :-
    output_pseudo_op_str_args(".asciiz", Literals, !IO).
output_x86_64_pseudo_op(balign(Bytes, FillVal, SkipBytes), !IO) :-
    output_pseudo_op_with_int_args(".balign", Bytes, FillVal, SkipBytes, !IO).
output_x86_64_pseudo_op(byte(ExprList), !IO) :-
    output_pseudo_op_str_args(".byte", ExprList, !IO).
output_x86_64_pseudo_op(comm(Symbol, Length, Align0), !IO) :-
    io.write_string("\t.comm\t" ++ Symbol ++ ",", !IO),
    io.write_int(Length, !IO),
    ( 
        Align0 = yes(Align1),
        io.write_string(",", !IO),
        io.write_int(Align1, !IO)
    ;
        Align0 = no
    ),
    io.write_string("\n", !IO).
output_x86_64_pseudo_op(data(Subsection0), !IO) :-
    io.write_string("\t.data\t", !IO),
    ( 
        Subsection0 = yes(Subsection1),
        io.write_int(Subsection1, !IO)
    ;
        Subsection0 = no
    ),
    io.write_string("\n", !IO).
output_x86_64_pseudo_op(desc(Symbol, AbsExpr), !IO) :-
    io.write_string("\t.desc\t" ++ Symbol ++ "," ++ AbsExpr ++ "\n", !IO).
output_x86_64_pseudo_op(def(Name), !IO) :-
    io.write_string("\t.def\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(dim, !IO) :-
    io.write_string("\t.dim\n", !IO).
output_x86_64_pseudo_op(double(NumList), !IO) :-
    output_pseudo_op_float_args(".double", NumList, !IO).
output_x86_64_pseudo_op(eject, !IO) :-
    io.write_string("\t.eject\n", !IO).
output_x86_64_pseudo_op(else_, !IO) :-
    io.write_string("\t.else\n", !IO).
output_x86_64_pseudo_op(elseif, !IO) :-
    io.write_string("\t.elseif\n", !IO).
output_x86_64_pseudo_op(end, !IO) :-
    io.write_string("\t.end\n", !IO).
output_x86_64_pseudo_op(endef, !IO) :-
    io.write_string("\t.endef\n", !IO).
output_x86_64_pseudo_op(endfunc, !IO) :-
    io.write_string("\t.endfunc\n", !IO).
output_x86_64_pseudo_op(endif, !IO) :-
    io.write_string("\t.endif\n", !IO).
output_x86_64_pseudo_op(endm, !IO) :-
    io.write_string("\t.endm\n", !IO).
output_x86_64_pseudo_op(equ(Symbol, Expr), !IO) :-
    io.write_string("\t.equ\t" ++ Symbol ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(equiv(Symbol, Expr), !IO) :-
    io.write_string("\t.equiv\t" ++ Symbol ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(err, !IO) :-
    io.write_string("\t.err\n", !IO).
output_x86_64_pseudo_op(exitm, !IO) :-
    io.write_string("\t.exitm\n", !IO).
output_x86_64_pseudo_op(extern, !IO) :-
    io.write_string("\t.extern\n", !IO).
output_x86_64_pseudo_op(fail_(Expr), !IO) :-
    io.write_string("\t.fail\t" ++ Expr++ "\n", !IO).
output_x86_64_pseudo_op(file(Name), !IO) :-
    io.write_string("\t.file\t\"" ++ Name ++ "\"\n", !IO).
output_x86_64_pseudo_op(fill(Repeat, Size, Val), !IO) :-
    output_pseudo_op_with_int_args(".repeat", Repeat, Size, Val, !IO).
output_x86_64_pseudo_op(float(NumList), !IO) :-
    output_pseudo_op_float_args(".float", NumList, !IO).
output_x86_64_pseudo_op(func_(Name, Label), !IO) :-
    io.write_string("\t.func\t" ++ Name ++ "," ++ Label ++ "\n", !IO).
output_x86_64_pseudo_op(global(Symbol), !IO) :-
    io.write_string("\t.global\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(globl(Symbol), !IO) :-
    io.write_string("\t.globl\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(hidden(Name), !IO) :-
    io.write_string("\t.hidden\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(hword(ExprList), !IO) :-
    output_pseudo_op_str_args(".hword", ExprList, !IO).
output_x86_64_pseudo_op(ident, !IO) :-
    io.write_string("\t.ident\n", !IO).
output_x86_64_pseudo_op(if_(Expr), !IO) :-
    io.write_string("\t.if\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(ifdef(Symbol), !IO) :-
    io.write_string("\t.ifdef\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(ifc(Str1, Str2), !IO) :-
    io.write_string("\t.ifc\t" ++ Str1 ++ "," ++ Str2 ++ "\n", !IO).
output_x86_64_pseudo_op(ifeq(Expr), !IO) :-
    io.write_string("\t.ifeq\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(ifge(Expr), !IO) :-
    io.write_string("\t.ifge\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(ifgt(Expr), !IO) :-
    io.write_string("\t.ifgt\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(ifle(Expr), !IO) :-
    io.write_string("\t.ifle\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(iflt(Expr), !IO) :-
    io.write_string("\t.iflt\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(ifnc(Str1, Str2), !IO) :-
    io.write_string("\t.ifnc\t" ++ Str1 ++ "," ++ Str2 ++ "\n", !IO).
output_x86_64_pseudo_op(ifndef(Symbol), !IO) :-
    io.write_string("\t.ifndef\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(ifnotdef(Symbol), !IO) :-
    io.write_string("\t.ifnotdef\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(ifne(Expr), !IO) :-
    io.write_string("\t.ifne\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(ifnes(Str1, Str2), !IO) :-
    io.write_string("\t.ifnes\t" ++ Str1 ++ "," ++ Str2 ++ "\n", !IO).
output_x86_64_pseudo_op(include(File), !IO) :-
    io.write_string("\t.include\t" ++ "\"" ++ File ++ "\"\n", !IO).
output_x86_64_pseudo_op(int(ExprList), !IO) :-
    output_pseudo_op_str_args(".int", ExprList, !IO).
output_x86_64_pseudo_op(internal(Name), !IO) :-
    io.write_string("\t.internal\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(lcomm(Symbol, Length), !IO) :-
    io.write_string("\tlcomm\t" ++ Symbol, !IO),
    io.write_int(Length, !IO),
    io.write_string("\n", !IO).
output_x86_64_pseudo_op(line(LineNum), !IO) :-
    io.write_string("\t.line\t", !IO),
    io.write_int(LineNum, !IO),
    io.write_string("\n", !IO).
output_x86_64_pseudo_op(list, !IO) :-
    io.write_string("\t.list\n", !IO).
output_x86_64_pseudo_op(long(ExprList), !IO) :-
    output_pseudo_op_str_args(".long", ExprList, !IO).
output_x86_64_pseudo_op(macro, !IO) :-
    io.write_string("\t.macro\n", !IO).
output_x86_64_pseudo_op(nolist, !IO) :-
    io.write_string("\t.nolist\n", !IO).
output_x86_64_pseudo_op(p2align(PowBit, FillVal, SkipBytes), !IO) :-
    output_pseudo_op_with_int_args(".p2align", PowBit, FillVal, SkipBytes, !IO).
output_x86_64_pseudo_op(popsection, !IO) :-
    io.write_string("\t.popsection\n", !IO).
output_x86_64_pseudo_op(previous, !IO) :-
    io.write_string("\t.previous\n", !IO).
output_x86_64_pseudo_op(print(Str), !IO) :-
    io.write_string("\t.print\t" ++ Str ++ "\n", !IO).
output_x86_64_pseudo_op(protected(Name), !IO) :-
    io.write_string("\t.protected\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(psize(Lines, Cols), !IO) :-
    output_pseudo_op_with_int_args(".psize", Lines, Cols, no, !IO).
output_x86_64_pseudo_op(purgem(Name), !IO) :-
    io.write_string("\t.purgem\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(pushsection(Name, Subsection), !IO) :-
    io.write_string("\t.pushsection\t" ++ Name, !IO),
    io.write_int(Subsection, !IO),
    io.write_string("\n", !IO).
output_x86_64_pseudo_op(quad(BigNumList), !IO) :-
    output_pseudo_op_str_args("\t.quad\t", BigNumList, !IO).
output_x86_64_pseudo_op(rept(Count), !IO) :-
    io.write_string("\t.rept\t", !IO),
    io.write_int(Count, !IO),
    io.write_string("\n", !IO).
output_x86_64_pseudo_op(sbttl(SubHeading), !IO) :-
    io.write_string("\t.sbttl\t" ++ "\"" ++ SubHeading ++ "\"\n", !IO).
output_x86_64_pseudo_op(scl(Class), !IO) :-
    io.write_string("\t.scl\t" ++ Class ++ "\n", !IO).
output_x86_64_pseudo_op(section(Name, Flags0, Type0, EntSize0), !IO) :-
    io.write_string("\t.section\t" ++ Name, !IO),
    ( 
        Flags0 = yes(Flags1),
        check_section_flags_and_type(Flags1, Type0, Result0),
        ( 
            Result0 = yes,
            Type0 = yes(Type1)
        ->
            io.write_string(",\"" ++ Flags1 ++ "\"", !IO),
            ( check_pseudo_section_type(Type1) ->
                io.write_string("," ++ Type1, !IO)
            ;
                unexpected(this_file, "output_x86_64_pseudo_op: section: 
                    check_section_type unexpected")
            )
        ;
            unexpected(this_file, "output_x86_64_pseudo_op: section: 
                check_section_flags_and_type unexpected")
        )
     ;
        Flags0 = no
     ),
     ( 
        EntSize0 = yes(EntSize1),
        io.write_string(",", !IO),
        io.write_int(EntSize1, !IO)
     ;
        EntSize0 = no
     ),
     io.write_string("\n", !IO).
output_x86_64_pseudo_op(set(Symbol, Expr), !IO) :-
    io.write_string("\t.set\t" ++ Symbol ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(short(ExprList), !IO) :-
    output_pseudo_op_str_args(".short", ExprList, !IO).
output_x86_64_pseudo_op(single(FloatList), !IO) :-
    output_pseudo_op_float_args(".single", FloatList, !IO).
output_x86_64_pseudo_op(size(Name, Expr), !IO) :-
    io.write_string("\t.size\t" ++ Name ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(skip(Size, Val), !IO) :-
    output_pseudo_op_with_int_args(".skip", Size, Val, no, !IO).
output_x86_64_pseudo_op(sleb128(ExprList), !IO) :-
    output_pseudo_op_str_args(".sleb128\t", ExprList, !IO).
output_x86_64_pseudo_op(space(Size, Fill), !IO) :-
    output_pseudo_op_with_int_args(".space", Size, Fill, no, !IO).
output_x86_64_pseudo_op(string(StrList), !IO) :-
    output_pseudo_op_str_args(".string", StrList, !IO).
output_x86_64_pseudo_op(struct(Expr), !IO) :-
    io.write_string("\t.struct\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(subsection(Name), !IO) :-
    io.write_string("\t.subsection\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(symver(Name, Alias), !IO) :-
    io.write_string("\t.symver\t" ++ Name ++ "," ++ Alias ++ "\n", !IO).
output_x86_64_pseudo_op(tag(Name), !IO) :-
    io.write_string("\t.tag\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(text(Subsection0), !IO) :-
    io.write_string("\ttext\t", !IO),
    ( 
        Subsection0 = yes(Subsection1),
        io.write_int(Subsection1, !IO)
    ;
        Subsection0 = no
    ),
    io.write_string("\n", !IO).
output_x86_64_pseudo_op(title(Heading), !IO) :-
    io.write_string("\t.title\t" ++ Heading ++ "\n", !IO).
output_x86_64_pseudo_op(type_(Name, Desc), !IO) :-
    ( check_pseudo_type_desc(Desc) ->
        io.write_string("\t.type\t" ++ Name ++ "," ++ Desc ++ "\n", !IO)
    ;
       unexpected(this_file, "output_x86_64_pseudo_op: type_ unexpected") 
    ).
output_x86_64_pseudo_op(uleb128(ExprList), !IO) :-
    output_pseudo_op_str_args(".uleb128", ExprList, !IO).
output_x86_64_pseudo_op(val(Addr), !IO) :-
    io.write_string("\t.val\t" ++ Addr ++ "\n", !IO).
output_x86_64_pseudo_op(version(Note), !IO) :-
    io.write_string("\t.version\t" ++ Note ++ "\n", !IO).
output_x86_64_pseudo_op(weak(NameList), !IO) :-
    output_pseudo_op_str_args(".weak", NameList, !IO).
output_x86_64_pseudo_op(word(ExprList), !IO) :-
    output_pseudo_op_str_args(".word", ExprList, !IO).

    % Output pseudo-op name with 1, 2 or 3 integer arguments. 
    %
:- pred output_pseudo_op_with_int_args(string::in, int::in, maybe(int)::in, 
    maybe(int)::in, io::di, io::uo) is det.

output_pseudo_op_with_int_args(OpName, Arg1, Arg2, Arg3, !IO) :-
    io.write_string("\t" ++ OpName ++ "\t", !IO),
    io.write_int(Arg1, !IO),
    ( 
        Arg2 = yes(Arg2Out),
        io.write_string(",", !IO),
        io.write_int(Arg2Out, !IO)
    ;
        Arg2 = no
    ),
    ( 
        Arg3 = yes(Arg3Out),
        ( 
            Arg2 = no,
            io.write_string(",,", !IO),
            io.write_int(Arg3Out, !IO)
        ;
            Arg2 = yes(_),
            io.write_string(",", !IO),
            io.write_int(Arg3Out, !IO)
        )
    ;
        Arg3 = no
    ),
    io.write_string("\n", !IO).

    % Output pseudo-op having list of float as its argument.
    %
:- pred output_pseudo_op_float_args(string::in, list(float)::in, 
    io::di, io::uo) is det.

output_pseudo_op_float_args(OpName, FloatArgs, !IO) :-
    io.write_string("\t" ++ OpName ++ "\t", !IO),
    pseudo_op_float_args_while(FloatArgs, !IO),
    io.write_string("\n", !IO).
 
:- pred pseudo_op_float_args_while(list(float)::in, io::di, io::uo) is det.

pseudo_op_float_args_while([], !IO).
pseudo_op_float_args_while([Arg | Args], !IO) :-
    io.write_float(Arg, !IO),
    ( 
        Args = [],
        pseudo_op_float_args_while(Args, !IO)
    ;
        Args = [_|_],
        io.write_string(",", !IO),
        pseudo_op_float_args_while(Args, !IO)
    ).

    % Output pseudo-op having list of string as its argument.
    %
:- pred output_pseudo_op_str_args(string::in, list(string)::in, 
    io::di, io::uo) is det.

output_pseudo_op_str_args(OpName, StrArgs, !IO) :-
    io.write_string("\t" ++ OpName ++ "\t", !IO),
    pseudo_op_str_args_while(StrArgs, !IO),
    io.write_string("\n", !IO).
 
:- pred pseudo_op_str_args_while(list(string)::in, io::di, io::uo) is det.

pseudo_op_str_args_while([], !IO).
pseudo_op_str_args_while([Arg | Args], !IO) :-
    io.write_string(Arg, !IO),
    ( 
        Args = [],
        pseudo_op_str_args_while(Args, !IO)
    ;
        Args = [_|_],
        io.write_string(",", !IO),
        pseudo_op_str_args_while(Args, !IO)
    ).
 
    % Check if FLAGS and TYPE argument of '.section' pseudo-op is valid
    %
:- pred check_section_flags_and_type(string::in, maybe(string)::in, 
    bool::out) is det.

check_section_flags_and_type(Flags, Type0, Result) :-
    (  string.contains_char(Flags, 'M') ->
        (
            Type0 = yes(Type1),
            string.length(Type1) > 0
        ->
            true
        ;
            unexpected(this_file, "check_section_flags_and_type: 
               flag unexpected")
        )
    ;
        true
    ),
    string.to_char_list(Flags, CharList),
    check_pseudo_section_flags(CharList, Result0),
    ( 
        Result0 = yes,
        Result = yes
    ;
        Result0 = no,
        Result = no
    ).

    % Check FLAGS argument of '.section' pseudo-op
    %
:- pred check_pseudo_section_flags(list(char)::in, bool::out) is det. 

check_pseudo_section_flags([], yes).
check_pseudo_section_flags([Char | Chars], Result) :-
    ( string.contains_char(section_pseudo_op_flags, Char) ->
        check_pseudo_section_flags(Chars, Result)
    ;
        Result = no
    ).

    % The optional FLAGS argument of '.section' pseudo-op contains any 
    % combination of:
    % 'a'   - section is allocatable
    % 'w'   - section is writable 
    % 'x'   - section is executable 
    % 'M'   - section is mergeable 
    % 'S'   - section contains zero terminated string
    %
:- func section_pseudo_op_flags = string.

section_pseudo_op_flags = "awxMS".

    % The optional type of '.section' pseudo-op may contain:
    % @progbits     - section contains data
    % @nobits       - section does not contain data
    %
:- pred check_pseudo_section_type(string::in) is semidet.

check_pseudo_section_type("@progbits").
check_pseudo_section_type("@nobits").

    % Two valid values of 'type_desc' field in pseudo-op '.type':
    %   @function
    %   @object
    %
:- pred check_pseudo_type_desc(string::in) is semidet.

check_pseudo_type_desc("@function").
check_pseudo_type_desc("@function").

%-----------------------------------------------------------------------------%
%
% Output x86_64 instructions.
%

:- pred output_x86_64_instr_and_comment(list(x86_64_instruction)::in, io::di, 
    io::uo) is det.

output_x86_64_instr_and_comment([], !IO).
output_x86_64_instr_and_comment([Instr | Instrs], !IO) :-
    Instr = x86_64_instr(InstrName, Comment),
    output_x86_64_instr(InstrName, !IO),
    output_comment(Comment, !IO),
    output_x86_64_instr_and_comment(Instrs, !IO).

    % Output x86_64 instruction and its operands (if any). 
    %
:- pred output_x86_64_instr(x86_64_op::in, io::di, io::uo) is det.

output_x86_64_instr(adc(Src, Dest), !IO) :-
    instr_with_op_and_rmro("adc", Src, Dest, !IO). 
output_x86_64_instr(add(Src, Dest), !IO) :-
    instr_with_op_and_rmro("add", Src, Dest, !IO). 
output_x86_64_instr(and(Src, Dest), !IO) :-
    instr_with_op_and_rmro("and", Src, Dest, !IO). 
output_x86_64_instr(bsf(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("bsf", Src, Dest, !IO).
output_x86_64_instr(bsr(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("bsr", Src, Dest, !IO).
output_x86_64_instr(bswap(Reg), !IO) :-
    io.write_string("\tbswap\t", !IO),
    reg_type(Reg, RegType),
    io.write_string(RegType ++ "\t", !IO).
output_x86_64_instr(bt(Src, Idx), !IO) :-
    instr_with_rmro_and_rio("bt", Src, Idx, !IO).
output_x86_64_instr(btc(Src, Idx), !IO) :-
    instr_with_rmro_and_rio("btc", Src, Idx, !IO).
output_x86_64_instr(btr(Src, Idx), !IO) :-
    instr_with_rmro_and_rio("btr", Src, Idx, !IO).
output_x86_64_instr(bts(Src, Idx), !IO) :-
    instr_with_rmro_and_rio("bts", Src, Idx, !IO).
output_x86_64_instr(call(TargetLocation), !IO) :-
    instr_with_rmrol("call", TargetLocation, !IO).
output_x86_64_instr(cbw, !IO) :-
    io.write_string("\tcbw\t", !IO).
output_x86_64_instr(cwde, !IO) :-
    io.write_string("\tcwde\t", !IO).
output_x86_64_instr(cdqe, !IO) :-
    io.write_string("\tcdqe\t", !IO).
output_x86_64_instr(cwd, !IO) :-
    io.write_string("\tcwd\t", !IO).
output_x86_64_instr(cdq, !IO) :-
    io.write_string("\tcdq\t", !IO).
output_x86_64_instr(cqo, !IO) :-
    io.write_string("\tcqo\t", !IO).
output_x86_64_instr(clc, !IO) :-
    io.write_string("\tclc\t", !IO).
output_x86_64_instr(cld, !IO) :-
    io.write_string("\tcld\t", !IO).
output_x86_64_instr(cmc, !IO) :-
    io.write_string("\tcmc\t", !IO).
output_x86_64_instr(cmovo(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovo", Src, Dest, !IO).
output_x86_64_instr(cmovno(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovno", Src, Dest, !IO).
output_x86_64_instr(cmovb(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovb", Src, Dest, !IO).
output_x86_64_instr(cmovc(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovc", Src, Dest, !IO).
output_x86_64_instr(cmovnae(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnae", Src, Dest, !IO).
output_x86_64_instr(cmovnb(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnb", Src, Dest, !IO).
output_x86_64_instr(cmovnc(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnc", Src, Dest, !IO).
output_x86_64_instr(cmovae(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovae", Src, Dest, !IO).
output_x86_64_instr(cmovz(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovz", Src, Dest, !IO).
output_x86_64_instr(cmove(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmove", Src, Dest, !IO).
output_x86_64_instr(cmovnz(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnz", Src, Dest, !IO).
output_x86_64_instr(cmovne(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovne", Src, Dest, !IO).
output_x86_64_instr(cmovbe(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovbe", Src, Dest, !IO).
output_x86_64_instr(cmovna(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovna", Src, Dest, !IO).
output_x86_64_instr(cmovnbe(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnbe", Src, Dest, !IO).
output_x86_64_instr(cmova(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmova", Src, Dest, !IO).
output_x86_64_instr(cmovs(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovs", Src, Dest, !IO).
output_x86_64_instr(cmovns(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovns", Src, Dest, !IO).
output_x86_64_instr(cmovp(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovp", Src, Dest, !IO).
output_x86_64_instr(cmovpe(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovpe", Src, Dest, !IO).
output_x86_64_instr(cmovnp(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnp", Src, Dest, !IO).
output_x86_64_instr(cmovpo(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovpo", Src, Dest, !IO).
output_x86_64_instr(cmovl(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovl", Src, Dest, !IO).
output_x86_64_instr(cmovnge(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnge", Src, Dest, !IO).
output_x86_64_instr(cmovnl(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnl", Src, Dest, !IO).
output_x86_64_instr(cmovge(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovge", Src, Dest, !IO).
output_x86_64_instr(cmovle(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovle", Src, Dest, !IO).
output_x86_64_instr(cmovng(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovng", Src, Dest, !IO).
output_x86_64_instr(cmovnle(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovnle", Src, Dest, !IO).
output_x86_64_instr(cmovg(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("cmovg", Src, Dest, !IO).
output_x86_64_instr(cmp(Src, Dest), !IO) :-
    instr_with_op_and_rmro("cmp", Src, Dest, !IO).
output_x86_64_instr(cmpxchg(Cmp, Xchg), !IO) :-
    instr_with_rmro_and_reg("cmpxchg", Cmp, Xchg, !IO). 
output_x86_64_instr(cmpxchg8b(MemRef), !IO) :-
    mem_ref_type(MemRef, MemRefType), 
    io.write_string("\tcmpxchg8b" ++ MemRefType, !IO).
output_x86_64_instr(dec(RegOrMemRef), !IO) :-
    instr_with_rmro("dec", RegOrMemRef, !IO).
output_x86_64_instr(div(RegOrMemRef), !IO) :-
    instr_with_rmro("div", RegOrMemRef, !IO).
output_x86_64_instr(enter(StackSize, NestingLevel), !IO) :-
    StackSize = uint16(Size),
    NestingLevel = uint8(Level),
    check_unsigned_int_size(16, Size, Result0),
    check_unsigned_int_size(8, Level, Result1),
    ( 
        Result0 = yes,
        Result1 = yes 
    ->
        io.write_string("\tenter\t", !IO),
        io.write_int(Size, !IO),
        io.write_string(",", !IO),
        io.write_int(Level, !IO),
        io.write_string("\t", !IO)
    ;
        unexpected(this_file, "output_x86_64_instr: enter unexpected")
    ).
output_x86_64_instr(idiv(RegOrMemRef), !IO) :-
    instr_with_rmro("idiv", RegOrMemRef, !IO).
output_x86_64_instr(imul(RegOrMemRef), !IO) :-
    instr_with_rmro("idiv", RegOrMemRef, !IO).
output_x86_64_instr(imul(Src, Dest), !IO) :-
    instr_with_rmro_and_reg("imul", Src, Dest, !IO).
output_x86_64_instr(imul(RegOrMemRefOp, Imm, Reg), !IO) :-
    io.write_string("\timul\t", !IO),
    rmro_type(RegOrMemRefOp, Type),
    io.write_string(Type ++ ", ", !IO),
    imm_op_type(Imm, ImmVal),
    io.write_string(ImmVal ++ ", ", !IO),
    reg_type(Reg, RegType),
    io.write_string(RegType ++ "\t", !IO).
output_x86_64_instr(inc(RegOrMemRef), !IO) :-
    instr_with_rmro("inc", RegOrMemRef, !IO).
output_x86_64_instr(jo(RelOffset), !IO) :-
    instr_with_rel_offset_op("jo", RelOffset, !IO).
output_x86_64_instr(jno(RelOffset), !IO) :-
    instr_with_rel_offset_op("jno", RelOffset, !IO).
output_x86_64_instr(jb(RelOffset), !IO) :-
    instr_with_rel_offset_op("jb", RelOffset, !IO).
output_x86_64_instr(jc(RelOffset), !IO) :-
    instr_with_rel_offset_op("jc", RelOffset, !IO).
output_x86_64_instr(jnae(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnae", RelOffset, !IO).
output_x86_64_instr(jnb(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnb", RelOffset, !IO).
output_x86_64_instr(jnc(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnc", RelOffset, !IO).
output_x86_64_instr(jae(RelOffset), !IO) :-
    instr_with_rel_offset_op("jae", RelOffset, !IO).
output_x86_64_instr(jz(RelOffset), !IO) :-
    instr_with_rel_offset_op("jz", RelOffset, !IO).
output_x86_64_instr(je(RelOffset), !IO) :-
    instr_with_rel_offset_op("je", RelOffset, !IO).
output_x86_64_instr(jnz(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnz", RelOffset, !IO).
output_x86_64_instr(jne(RelOffset), !IO) :-
    instr_with_rel_offset_op("jne", RelOffset, !IO).
output_x86_64_instr(jbe(RelOffset), !IO) :-
    instr_with_rel_offset_op("jbe", RelOffset, !IO).
output_x86_64_instr(jna(RelOffset), !IO) :-
    instr_with_rel_offset_op("jna", RelOffset, !IO).
output_x86_64_instr(jnbe(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnbe", RelOffset, !IO).
output_x86_64_instr(ja(RelOffset), !IO) :-
    instr_with_rel_offset_op("ja", RelOffset, !IO).
output_x86_64_instr(js(RelOffset), !IO) :-
    instr_with_rel_offset_op("js", RelOffset, !IO).
output_x86_64_instr(jns(RelOffset), !IO) :-
    instr_with_rel_offset_op("jns", RelOffset, !IO).
output_x86_64_instr(jp(RelOffset), !IO) :-
    instr_with_rel_offset_op("jp", RelOffset, !IO).
output_x86_64_instr(jpe(RelOffset), !IO) :-
    instr_with_rel_offset_op("jpe", RelOffset, !IO).
output_x86_64_instr(jnp(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnp", RelOffset, !IO).
output_x86_64_instr(jpo(RelOffset), !IO) :-
    instr_with_rel_offset_op("jpo", RelOffset, !IO).
output_x86_64_instr(jl(RelOffset), !IO) :-
    instr_with_rel_offset_op("jl", RelOffset, !IO).
output_x86_64_instr(jnge(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnge", RelOffset, !IO).
output_x86_64_instr(jnl(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnl", RelOffset, !IO).
output_x86_64_instr(jge(RelOffset), !IO) :-
    instr_with_rel_offset_op("jge", RelOffset, !IO).
output_x86_64_instr(jle(RelOffset), !IO) :-
    instr_with_rel_offset_op("jle", RelOffset, !IO).
output_x86_64_instr(jng(RelOffset), !IO) :-
    instr_with_rel_offset_op("jng", RelOffset, !IO).
output_x86_64_instr(jnle(RelOffset), !IO) :-
    instr_with_rel_offset_op("jnle", RelOffset, !IO).
output_x86_64_instr(jg(RelOffset), !IO) :-
    instr_with_rel_offset_op("jg", RelOffset, !IO).
output_x86_64_instr(jrcxz(RelOffset), !IO) :-
    instr_with_8bit_rel_offset_op("jrcxz", RelOffset, !IO).
output_x86_64_instr(jmp(Target), !IO) :-
    instr_with_rmrol("jmp", Target, !IO).
output_x86_64_instr(lea(Src, Dest), !IO) :-
    instr_with_mem_ref_and_reg_op("lea", Src, Dest, !IO).
output_x86_64_instr(leave, !IO) :-
    io.write_string("\tleave\t", !IO).
output_x86_64_instr(loop(RelOffset), !IO) :-
    instr_with_8bit_rel_offset_op("loop", RelOffset, !IO).
output_x86_64_instr(loope(RelOffset), !IO) :-
    instr_with_8bit_rel_offset_op("loope", RelOffset, !IO).
output_x86_64_instr(loopne(RelOffset), !IO) :-
    instr_with_8bit_rel_offset_op("loopne", RelOffset, !IO).
output_x86_64_instr(loopnz(RelOffset), !IO) :-
    instr_with_8bit_rel_offset_op("loopnz", RelOffset, !IO).
output_x86_64_instr(loopz(RelOffset), !IO) :-
    instr_with_8bit_rel_offset_op("loopz", RelOffset, !IO).
output_x86_64_instr(mov(Source, Dest), !IO) :-
    instr_with_op_and_rmro("mov", Source, Dest, !IO). 
output_x86_64_instr(mul(RegOrMemRef), !IO) :-
    instr_with_rmro("mul", RegOrMemRef, !IO).
output_x86_64_instr(neg(RegOrMemRef), !IO) :-
    instr_with_rmro("neg", RegOrMemRef, !IO).
output_x86_64_instr(nop, !IO) :-
    io.write_string("nop", !IO).
output_x86_64_instr(not_(RegOrMemRef), !IO) :-
    instr_with_rmro("not", RegOrMemRef, !IO).
output_x86_64_instr(or(Src, Dest), !IO) :-
    instr_with_op_and_rmro("or", Src, Dest, !IO).
output_x86_64_instr(pop(RegOrMemRefOp), !IO) :-
    instr_with_rmro("pop", RegOrMemRefOp, !IO).
output_x86_64_instr(popfq, !IO) :-
    io.write_string("\tpopfq\t", !IO).
output_x86_64_instr(push(Operand), !IO) :-
    io.write_string("\tpush\t", !IO),
    operand_type(Operand, OperandType),
    io.write_string(OperandType ++ "\t", !IO).
output_x86_64_instr(pushfq, !IO) :-
    io.write_string("\tpushfq\t", !IO).
output_x86_64_instr(rcl(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("rcl", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(rcr(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("rcr", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(ret, !IO) :-
    io.write_string("\tret\t", !IO).
output_x86_64_instr(ret(uint16(NumBytes)), !IO) :-
    check_unsigned_int_size(16, NumBytes, Result),
    ( 
        Result = yes,
        io.write_string("\tret\t", !IO),
        io.write_int(NumBytes, !IO),
        io.write_string("\t", !IO)
    ;
        Result = no,
        unexpected(this_file, "output_x86_64_instr: ret unexpected")
    ).
output_x86_64_instr(rol(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("rol", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(ror(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("ror", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(sal(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("sal", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(shl(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("shl", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(sar(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("sar", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(sbb(Src, Dest), !IO) :-
    instr_with_op_and_rmro("sbb", Src, Dest, !IO).
output_x86_64_instr(seto(RegOrMemRef), !IO) :-
    instr_with_rmro("seto", RegOrMemRef, !IO).
output_x86_64_instr(setno(RegOrMemRef), !IO) :-
    instr_with_rmro("setno", RegOrMemRef, !IO).
output_x86_64_instr(setb(RegOrMemRef), !IO) :-
    instr_with_rmro("setb", RegOrMemRef, !IO).
output_x86_64_instr(setc(RegOrMemRef), !IO) :-
    instr_with_rmro("setc", RegOrMemRef, !IO).
output_x86_64_instr(setnae(RegOrMemRef), !IO) :-
    instr_with_rmro("setnae", RegOrMemRef, !IO).
output_x86_64_instr(setnb(RegOrMemRef), !IO) :-
    instr_with_rmro("setnb", RegOrMemRef, !IO).
output_x86_64_instr(setnc(RegOrMemRef), !IO) :-
    instr_with_rmro("setnc", RegOrMemRef, !IO).
output_x86_64_instr(setae(RegOrMemRef), !IO) :-
    instr_with_rmro("setae", RegOrMemRef, !IO).
output_x86_64_instr(setz(RegOrMemRef), !IO) :-
    instr_with_rmro("setz", RegOrMemRef, !IO).
output_x86_64_instr(sete(RegOrMemRef), !IO) :-
    instr_with_rmro("sete", RegOrMemRef, !IO).
output_x86_64_instr(setnz(RegOrMemRef), !IO) :-
    instr_with_rmro("setnz", RegOrMemRef, !IO).
output_x86_64_instr(setne(RegOrMemRef), !IO) :-
    instr_with_rmro("setne", RegOrMemRef, !IO).
output_x86_64_instr(setbe(RegOrMemRef), !IO) :-
    instr_with_rmro("setbe", RegOrMemRef, !IO).
output_x86_64_instr(setna(RegOrMemRef), !IO) :-
    instr_with_rmro("setna", RegOrMemRef, !IO).
output_x86_64_instr(setnbe(RegOrMemRef), !IO) :-
    instr_with_rmro("setnbe", RegOrMemRef, !IO).
output_x86_64_instr(seta(RegOrMemRef), !IO) :-
    instr_with_rmro("seta", RegOrMemRef, !IO).
output_x86_64_instr(sets(RegOrMemRef), !IO) :-
    instr_with_rmro("sets", RegOrMemRef, !IO).
output_x86_64_instr(setns(RegOrMemRef), !IO) :-
    instr_with_rmro("setns", RegOrMemRef, !IO).
output_x86_64_instr(setp(RegOrMemRef), !IO) :-
    instr_with_rmro("setp", RegOrMemRef, !IO).
output_x86_64_instr(setpe(RegOrMemRef), !IO) :-
    instr_with_rmro("setpe", RegOrMemRef, !IO).
output_x86_64_instr(setnp(RegOrMemRef), !IO) :-
    instr_with_rmro("setnp", RegOrMemRef, !IO).
output_x86_64_instr(setpo(RegOrMemRef), !IO) :-
    instr_with_rmro("setpo", RegOrMemRef, !IO).
output_x86_64_instr(setl(RegOrMemRef), !IO) :-
    instr_with_rmro("sel", RegOrMemRef, !IO).
output_x86_64_instr(setnge(RegOrMemRef), !IO) :-
    instr_with_rmro("setnge", RegOrMemRef, !IO).
output_x86_64_instr(setnl(RegOrMemRef), !IO) :-
    instr_with_rmro("setnl", RegOrMemRef, !IO).
output_x86_64_instr(setge(RegOrMemRef), !IO) :-
    instr_with_rmro("setge", RegOrMemRef, !IO).
output_x86_64_instr(setle(RegOrMemRef), !IO) :-
    instr_with_rmro("setle", RegOrMemRef, !IO).
output_x86_64_instr(setng(RegOrMemRef), !IO) :-
    instr_with_rmro("setng", RegOrMemRef, !IO).
output_x86_64_instr(setnle(RegOrMemRef), !IO) :-
    instr_with_rmro("setnle", RegOrMemRef, !IO).
output_x86_64_instr(setg(RegOrMemRef), !IO) :-
    instr_with_rmro("setg", RegOrMemRef, !IO).
output_x86_64_instr(shld(ClRegOrImm, RegOrMemRef, Reg), !IO) :-
    instr_with_crio_rmro_and_reg("shld", ClRegOrImm, RegOrMemRef, Reg, !IO).
output_x86_64_instr(shr(ClRegOrImm, RegOrMemRef), !IO) :-
    instr_with_crio_and_rmro("shr", ClRegOrImm, RegOrMemRef, !IO).
output_x86_64_instr(shrd(ClRegOrImm, RegOrMemRef, Reg), !IO) :-
    instr_with_crio_rmro_and_reg("shrd", ClRegOrImm, RegOrMemRef, Reg, !IO).
output_x86_64_instr(stc, !IO) :-
    io.write_string("\tstc\t", !IO).
output_x86_64_instr(std, !IO) :-
    io.write_string("\tstd\t", !IO).
output_x86_64_instr(sub(Src, Dest), !IO) :-
    instr_with_op_and_rmro("sub", Src, Dest, !IO).
output_x86_64_instr(test(Src1, Src2), !IO) :-
    instr_with_rmro_and_rio("test", Src1, Src2, !IO).
output_x86_64_instr(xadd(Src, Dest), !IO) :-
    instr_with_reg_and_rmro("xadd", Src, Dest, !IO).
output_x86_64_instr(xchg(Src1, Src2), !IO) :-
    instr_with_rmro_and_rmro("xchg", Src1, Src2, !IO).
output_x86_64_instr(xor(Src, Dest), !IO) :-
    instr_with_op_and_rmro("xor", Src, Dest, !IO).


:- pred output_comment(string::in, io::di, io::uo) is det.

output_comment(Comment, !IO) :-
    ( string.length(Comment) > 0 ->
        io.write_string("\t# ", !IO),
        io.write_string(Comment, !IO)
    ;   
        true
    ),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%
%
% Output of x86_64 operands.
%

    % Output a string representation of an immediate value. 
    %
:- pred imm_op_type(imm_operand::in, string::out) is det. 

imm_op_type(imm8(int8(Val)), ImmVal) :-
    ImmVal = "$" ++ string.int_to_string(Val).
imm_op_type(imm16(int16(Val)), ImmVal) :-
    ImmVal = "$" ++ string.int_to_string(Val).
imm_op_type(imm32(int32(Val)), ImmVal) :-
    ImmVal = "$" ++ string.int_to_string(Val).

    % Output a string representation of a general-purpose register. 
    %
:- pred reg_type(gp_reg::in, string::out) is det. 

reg_type(rax, "%rax").
reg_type(rbx, "%rbx").
reg_type(rcx, "%rcx").
reg_type(rdx, "%rdx").
reg_type(rbp, "%rbp").
reg_type(rsi, "%rsi").
reg_type(rdi, "%rdi").
reg_type(rsp, "%rsp").
reg_type(r8, "%r8").
reg_type(r9, "%r9").
reg_type(r10, "%r10").
reg_type(r11, "%r11").
reg_type(r12, "%r12").
reg_type(r13, "%r13").
reg_type(r14, "%r14").
reg_type(r15, "%r15").

    % Output a string representation of a memory reference.
    %
:- pred mem_ref_type(mem_ref::in, string::out) is det. 

mem_ref_type(mem_abs(DirectMemRef), MemRefVal) :-
    base_address_type(DirectMemRef, MemRefVal).
mem_ref_type(mem_rip(InstrPtr), MemRefVal) :-
    instr_ptr_type(InstrPtr, MemRefVal).

    % Output a string representation of a base address in a memory reference. 
    %
:- pred base_address_type(base_address::in, string::out) is det.

base_address_type(base_reg(Offset, Reg), BaseAddress) :-
    reg_type(Reg, RegType),
    ( Offset = 0 ->
        BaseAddress = "(" ++ RegType ++ ")"
    ;
        BaseAddress = string.int_to_string(Offset) ++ "(" ++ RegType ++ ")" 
    ).
base_address_type(base_expr(Expr), DispType) :-
    DispType = "$" ++ Expr.

    % Output a string representation of RIP relative addressing. 
    % 
:- pred instr_ptr_type(instr_ptr::in, string::out) is det. 

instr_ptr_type(rip_constant(int32(Constant)), InstrPtrType) :-
    check_signed_int_size(32, Constant, Result),
    (
        Result = yes,
        InstrPtrType = string.int_to_string(Constant) ++ "(%rip)"
    ;
        Result = no,
        unexpected(this_file, "instr_ptr_type: int32 unexpected")
    ).
instr_ptr_type(rip_expr(Symbol), InstrPtrType) :-
    InstrPtrType = Symbol ++ "(%rip)".

    % Output a string representation of a relative offset.
    %
:- pred rel_offset_type(rel_offset::in, string::out) is det. 

rel_offset_type(ro8(int8(Val)), RelOffsetVal) :-
    check_signed_int_size(8, Val, Result),
    ( 
        Result = yes,
        ( Val = 0 ->
            RelOffsetVal = ""
        ;
            RelOffsetVal = string.int_to_string(Val)
        )
    ;
        Result = no,
        unexpected(this_file, "rel_offset_type: ro8 unexpected")
    ).
rel_offset_type(ro16(int16(Val)), RelOffsetVal) :-
    check_signed_int_size(16, Val, Result),
    ( 
        Result = yes,
        ( Val = 0 ->
            RelOffsetVal = ""
        ;
            RelOffsetVal = string.int_to_string(Val)
        )
    ;
        Result = no,
        unexpected(this_file, "rel_offset_type: ro16 unexpected")
    ).
rel_offset_type(ro32(int32(Val)), RelOffsetVal) :-
    check_signed_int_size(32, Val, Result),
    ( 
        Result = yes,
        ( Val = 0 ->
            RelOffsetVal = ""
        ;
            RelOffsetVal = string.int_to_string(Val)
        )
    ;
        Result = no,
        unexpected(this_file, "rel_offset_type: ro32 unexpected")
    ).

    % Output a string representation of a general operand type. 
    %
:- pred operand_type(operand::in, string::out) is det. 

operand_type(operand_reg(Reg), RegType) :-
    reg_type(Reg, RegType).
operand_type(operand_imm(Imm), ImmVal) :-
    imm_op_type(Imm, ImmVal).
operand_type(operand_mem_ref(MemRef), MemRefVal) :-
    mem_ref_type(MemRef, MemRefVal).

    % Output a string representation of an operand whose type is a register/
    % memory reference. 
    %
:- pred rmro_type(reg_or_mem_ref_op::in, string::out) is det. 

rmro_type(rmro_reg(Reg), RegType) :-
    reg_type(Reg, RegType).
rmro_type(rmro_mem_ref(MemRef), MemRefVal) :-
    mem_ref_type(MemRef, MemRefVal).

    % Output a string representation of an operand whose type is a register
    % or an immediate value. 
    %
:- pred rio_type(reg_or_imm_op::in, string::out) is det. 

rio_type(rio_reg(Reg), RegType) :-
    reg_type(Reg, RegType).
rio_type(rio_imm(Imm), ImmVal) :-
    imm_op_type(Imm, ImmVal).

    % Output a string representation of an operand whose type is a cl register
    % or an unsigned immediate value. 
    %
:- pred crio_type(cl_reg_or_imm_op::in, string::out) is det. 

crio_type(crio_reg_cl(Reg), RegType) :-
    ( Reg = rcx ->
        RegType = "%cl"
    ;
        unexpected(this_file, "crio_type: crio_reg_cl unexpected")
    ).
crio_type(crio_imm8(int8(Val)), ImmVal) :-
    check_unsigned_int_size(8, Val, Result),
    ( 
        Result = yes,
        ImmVal = "$" ++ string.int_to_string(Val)
    ;
        Result = no,
        unexpected(this_file, "crio_type: crio_imm8 unexpected")
    ).

    % Output a string representation of an operand whose type is either
    % a register, memory reference, signed relative offset or a label.
    %
:- pred rmrol_type(rmrol::in, string::out) is det. 

rmrol_type(rmrol_reg(Reg), RegType) :-
    reg_type(Reg, RegType).
rmrol_type(rmrol_mem_ref(MemRef), MemRefVal) :-
    mem_ref_type(MemRef, MemRefVal).
rmrol_type(rmrol_rel_offset(RelOffset), RelOffsetVal) :-
    rel_offset_type(RelOffset, RelOffsetVal).
rmrol_type(rmrol_label(LabelName), LabelOut) :-
    LabelOut = LabelName. 

%-----------------------------------------------------------------------------%
%
% Subsection of "Output of x86_64 instructions".
%

    % Output an instruction with a register/memory reference as an operand.
    %
:- pred instr_with_rmro(string::in, reg_or_mem_ref_op::in, 
    io::di, io::uo) is det. 

instr_with_rmro(InstrName, RegOrMemRef, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    rmro_type(RegOrMemRef, RegOrMemRefType),
    io.write_string(RegOrMemRefType ++ "\t\t", !IO).

    % Output an instruction with a signed offset relative to the instruction 
    % pointer as an operand. 
    %
:- pred instr_with_rel_offset_op(string::in, rel_offset::in, 
    io::di, io::uo) is det. 

instr_with_rel_offset_op(InstrName, RelOffset, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    rel_offset_type(RelOffset, RelOffsetType),
    io.write_string(RelOffsetType ++ "\t\t", !IO).

    % Output an instruction with a signed 8-bit offset relative to the 
    % instruction pointer as an operand. 
    %
:- pred instr_with_8bit_rel_offset_op(string::in, rel_offset::in, 
    io::di, io::uo) is det. 

instr_with_8bit_rel_offset_op(InstrName, RelOffset, !IO) :-
   ( 
        RelOffset = ro8(int8(Val)),
        check_signed_int_size(8, Val, Result),
        Result = yes 
   ->
        io.write_string("\t" ++ InstrName ++ "\t", !IO),
        io.write_int(Val, !IO),
        io.write_string("\t\t", !IO)
   ;
        unexpected(this_file, "instr_with_8bit_rel_offset_op: unexpected")
   ).

    % Output an instruction with either a register, memory reference,
    % relative offset or a label as its operand.
    %
:- pred instr_with_rmrol(string::in, rmrol::in, io::di, io::uo) is det. 

instr_with_rmrol(InstrName, Target, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    rmrol_type(Target, TargetType), 
    io.write_string(TargetType ++ "\t\t", !IO).

    % Output an instruction with a general operand and a register/memory 
    % reference as the first and second operand respectively. 
    %
:- pred instr_with_op_and_rmro(string::in, operand::in, reg_or_mem_ref_op::in, 
    io::di, io::uo) is det. 

instr_with_op_and_rmro(InstrName, SrcOperand, DestRegOrMemRefOp, !IO) :-
    ( 
        SrcOperand = operand_mem_ref(_),
        DestRegOrMemRefOp = rmro_mem_ref(_)
    ->
        % Both operands cannot be of type memory reference.
        unexpected(this_file, "instr_with_op_and_rmro: unexpected")
    ;
        io.write_string("\t" ++ InstrName ++ "\t", !IO),
        operand_type(SrcOperand, OperandType), 
        io.write_string(OperandType ++ ", ", !IO),
        rmro_type(DestRegOrMemRefOp, DestType), 
        io.write_string(DestType ++ "\t", !IO)
    ).

    % Output an instruction with a register/memory reference and a register as
    % the first and second operand respectively. 
    %
:- pred instr_with_rmro_and_reg(string::in, reg_or_mem_ref_op::in, gp_reg::in, 
    io::di, io::uo) is det. 

instr_with_rmro_and_reg(InstrName, SrcRegOrMemRefOp, DestReg, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    rmro_type(SrcRegOrMemRefOp, SrcType), 
    io.write_string(SrcType ++ ", ", !IO),
    reg_type(DestReg, RegType), 
    io.write_string(RegType ++ "\t", !IO).

    % Output an instruction with a register/memory reference and a register/
    % immediate value as the first and second operand respectively.
    %
:- pred instr_with_rmro_and_rio(string::in, reg_or_mem_ref_op::in, 
    reg_or_imm_op::in, io::di, io::uo) is det. 

instr_with_rmro_and_rio(InstrName, SrcRegOrMemRefOp, DestRegOrImmOp, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    rmro_type(SrcRegOrMemRefOp, SrcType), 
    io.write_string(SrcType ++ ",", !IO),
    rio_type(DestRegOrImmOp, DestType), 
    io.write_string(DestType ++ "\t", !IO).

    % Output an instruction with a register/memory reference as the first and
    % second operand respectively.
    %
:- pred instr_with_rmro_and_rmro(string::in, reg_or_mem_ref_op::in, 
    reg_or_mem_ref_op::in, io::di, io::uo) is det. 

instr_with_rmro_and_rmro(InstrName, RegOrMemRefOp0, RegOrMemRefOp1, !IO) :-
    ( 
        RegOrMemRefOp0 = rmro_mem_ref(_),
        RegOrMemRefOp1 = rmro_mem_ref(_)
    ->
        unexpected(this_file, "instr_with_rmro_and_rmro: unexpected")
    ;
        io.write_string("\t" ++ InstrName ++ "\t", !IO),
        rmro_type(RegOrMemRefOp0, Type0), 
        io.write_string(Type0 ++ ", ", !IO),
        rmro_type(RegOrMemRefOp1, Type1), 
        io.write_string(Type1 ++ "\t", !IO)
    ).

    % Output an instruction with a register and a register/memory reference as
    % the first and second operand respectively. 
    %
:- pred instr_with_reg_and_rmro(string::in, gp_reg::in, reg_or_mem_ref_op::in,  
    io::di, io::uo) is det. 

instr_with_reg_and_rmro(InstrName, SrcReg, DestRegOrMemRefOp, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    reg_type(SrcReg, RegType), 
    io.write_string(RegType ++ ", ", !IO),
    rmro_type(DestRegOrMemRefOp, DestType), 
    io.write_string(DestType ++ "\t", !IO).

    % Output an instruction with a cl register/immediate value and a register/
    % memory reference as the first and second operand respectively. 
    %
:- pred instr_with_crio_and_rmro(string::in, cl_reg_or_imm_op::in,  
    reg_or_mem_ref_op::in, io::di, io::uo) is det. 

instr_with_crio_and_rmro(InstrName, ClRegOrImm, RegOrMemRef, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    crio_type(ClRegOrImm, ClRegOrImmType),
    io.write_string(ClRegOrImmType ++ ", ", !IO),
    rmro_type(RegOrMemRef, RegOrMemRefType),
    io.write_string(RegOrMemRefType ++ "\t", !IO).

    % Output an instruction with a cl register/immediate value, a register/
    % memory reference and a register as the first, second and third operand 
    % respectively. 
    %
:- pred instr_with_crio_rmro_and_reg(string::in, cl_reg_or_imm_op::in,  
    reg_or_mem_ref_op::in, gp_reg::in, io::di, io::uo) is det. 

instr_with_crio_rmro_and_reg(InstrName, ClRegOrImm, RegOrMemRef, Reg, !IO) :-
    io.write_string("\t" ++ InstrName ++ "\t", !IO),
    crio_type(ClRegOrImm, ClRegOrImmType),
    io.write_string(ClRegOrImmType ++ "\t", !IO),
    rmro_type(RegOrMemRef, RegOrMemRefType),
    io.write_string(RegOrMemRefType ++ ", ", !IO),
    reg_type(Reg, RegType),
    io.write_string(RegType ++ "\t", !IO).

    % Output an instruction with a memory reference and a register as the first
    % and second operand respectively.
    %
:- pred instr_with_mem_ref_and_reg_op(string::in, mem_ref::in, 
    gp_reg::in, io::di, io::uo) is det. 

instr_with_mem_ref_and_reg_op(InstrName, SrcMemRef, DestReg, !IO) :-
    mem_ref_type(SrcMemRef, MemRefType),
    io.write_string("\t" ++ InstrName ++ "\t" ++ MemRefType, !IO),
    reg_type(DestReg, DestRegType),
    io.write_string(", " ++ DestRegType ++ "\t", !IO).

%-----------------------------------------------------------------------------%

    % Check whether an unsigned int (Val) with n bits (BitSize) is within the 
    % range of 0 and (2^n)-1.  
    %
:- pred check_unsigned_int_size(int::in, int::in, bool::out) is det.

check_unsigned_int_size(BitSize, Val, Result) :-
    MaxInt = (1 << BitSize) - 1,
    ( 
        Val >=  0,
        Val =< MaxInt
    ->
        Result = yes
    ;
        Result = no
    ).

    % Check whether a signed int (Val) with n bits (BitSize) is within the 
    % range of -2^(n-1) and 2^(n-1)-1.
    %
:- pred check_signed_int_size(int::in, int::in, bool::out) is det.  

check_signed_int_size(BitSize, Val, Result) :-
    MinInt = - (1 << (BitSize - 1)),
    MaxInt = (1 << (BitSize - 1)) - 1,
    ( 
        Val >= MinInt,
        Val =< MaxInt 
    ->
        Result = yes
    ;
        Result = no
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "x86_64_out.m".

:- end_module x86_64_out.
%-----------------------------------------------------------------------------%
