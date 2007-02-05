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

:- pred output_x86_64_instruction(x86_64_instruction::in, 
    io::di, io::uo) is det.

:- pred operand_type(operand::in, string::out) is det. 

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
output_x86_64_pseudo_op(x86_64_pseudo_else, !IO) :-
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
output_x86_64_pseudo_op(x86_64_pseudo_if(Expr), !IO) :-
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
                unexpected(this_file, "output_x86_64_pseudo_op: section:" 
                    ++ " check_section_type unexpected")
            )
        ;
            unexpected(this_file, "output_x86_64_pseudo_op: section:" 
                ++ " check_section_flags_and_type unexpected")
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
output_x86_64_pseudo_op(x86_64_pseudo_type(Name, Desc), !IO) :-
    ( check_pseudo_type_desc(Desc) ->
        io.write_string("\t.type\t" ++ Name ++ "," ++ Desc ++ "\n", !IO)
    ;
       unexpected(this_file, "output_x86_64_pseudo_op: x86_64_pseudo_type:"
            ++ " unexpected: check_pseudo_type_desc failed") 
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
    io.write_string(string.word_wrap("\"" ++ Arg ++ "\"", comment_length), !IO),
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
            unexpected(this_file, "check_section_flags_and_type:" 
               ++ " unexpected: flag")
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

:- func comment_length = int.

comment_length = 68.

%-----------------------------------------------------------------------------%
%
% Output x86_64 instructions.
%

    % Output x86_64 instruction and x86_64_comment. 
    %
output_x86_64_instruction(x86_64_instr(Instr, Comment), !IO) :-
    output_x86_64_comment(Comment, !IO),
    output_x86_64_instr_list(Instr, !IO),
    io.write_string("\n", !IO).

:- pred output_x86_64_instr_list(list(x86_64_instr)::in, io::di, io::uo) is det.

output_x86_64_instr_list(Instrs, !IO) :-
    list.foldl(output_x86_64_instr, Instrs, !IO).

:- pred output_x86_64_instr(x86_64_instr::in, io::di, io::uo) is det.

output_x86_64_instr(x86_64_comment(Comment), !IO) :-
    ( string.length(Comment) > 0 ->
        io.write_string("\t# ", !IO),
        ( string.length(Comment) > comment_length ->
            string.split(Comment, comment_length, Comment1, Comment2),
            io.write_string(string.word_wrap(Comment1, comment_length), !IO),
            io.write_string("\n", !IO),
            output_x86_64_instr(x86_64_comment(Comment2), !IO)
        ;   
            io.write_string(string.word_wrap(Comment, comment_length), !IO)
        )
    ;
        true
    ).
output_x86_64_instr(x86_64_label(LabelName), !IO) :-
    ( string.length(LabelName) > 0 ->
        io.write_string("\n" ++ LabelName ++ ":", !IO)
    ;
        true
    ).
output_x86_64_instr(x86_64_directive(PseudoOp), !IO) :-
    output_x86_64_pseudo_op(PseudoOp, !IO).
output_x86_64_instr(x86_64_instr(Instr), !IO) :-
    output_x86_64_inst(Instr, !IO),
    io.write_string("\n", !IO).


    % Output a single x86_64 instruction and its operands (if any). 
    %
:- pred output_x86_64_inst(x86_64_inst::in, io::di, io::uo) is det.

output_x86_64_inst(adc(Src, Dest), !IO) :-
    output_instr_not_imm_dest("adc", Src, yes(Dest), !IO).
output_x86_64_inst(add(Src, Dest), !IO) :-
    output_instr_not_imm_dest("add", Src, yes(Dest), !IO).
output_x86_64_inst(and(Src, Dest), !IO) :-
    output_instr_not_imm_dest("and", Src, yes(Dest), !IO).
output_x86_64_inst(bs(Src, Dest, Cond), !IO) :-
    check_operand_not_immediate(Src, Result1),
    (
        Result1 = yes,
        operand_type(Src, SrcType),
        check_operand_register(Dest, DestRes),
        (
            DestRes = yes,
            ( Cond = f ->
                Instr = "bsf"
            ;
                Cond = r ->
                Instr = "bsr"
            ;
                unexpected(this_file, "output_x86_64_inst: bs: unexpected:" 
                    ++ " invalid condition third operand")
            ),
            io.write_string("\t" ++ Instr ++ "\t", !IO),
            operand_type(Dest, DestType),
            io.write_string(SrcType ++ ", " ++ DestType ++ "\t", !IO)
        ;
            DestRes = no,
            unexpected(this_file, "output_x86_64_instr: bs: unexpected:"
                ++ " second operand is not a register")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: bsf: unexpected: first"
            ++ " operand is an immediate value")
    ).
output_x86_64_inst(bswap(Op), !IO) :-
    check_operand_register(Op, Result),
    (
        Result = yes,
        operand_type(Op, RegType), 
        io.write_string("\tbswap\t" ++ RegType ++ "\t\t", !IO)
    ;
        Result = no,
        unexpected(this_file, "output_x86_64_instr: bswap: unexpected: operand"
            ++ " is not a register")
    ). 
output_x86_64_inst(bt(Src, Idx), !IO) :-
    output_bit_test_instr("bt", Src, Idx, !IO).
output_x86_64_inst(btc(Src, Idx), !IO) :-
    output_bit_test_instr("btc", Src, Idx, !IO).
output_x86_64_inst(btr(Src, Idx), !IO) :-
    output_bit_test_instr("btr", Src, Idx, !IO).
output_x86_64_inst(bts(Src, Idx), !IO) :-
    output_bit_test_instr("bts", Src, Idx, !IO).
output_x86_64_inst(call(Target), !IO) :-
    check_operand_not_immediate(Target, Result),
    (
        Result = yes,
        operand_type(Target, TargetType),
        io.write_string("\tcall\t" ++ TargetType ++ "\t\t", !IO)
    ;
        Result = no,
        unexpected(this_file, "output_x86_64_instr: call: unexpected:" 
            ++ " invalid target operand")
    ).
output_x86_64_inst(cbw, !IO) :-
    io.write_string("\tcbw\t", !IO).
output_x86_64_inst(cwde, !IO) :-
    io.write_string("\tcwde\t", !IO).
output_x86_64_inst(cdqe, !IO) :-
    io.write_string("\tcdqe\t", !IO).
output_x86_64_inst(cwd, !IO) :-
    io.write_string("\tcwd\t", !IO).
output_x86_64_inst(cdq, !IO) :-
    io.write_string("\tcdq\t", !IO).
output_x86_64_inst(cqo, !IO) :-
    io.write_string("\tcqo\t", !IO).
output_x86_64_inst(clc, !IO) :-
    io.write_string("\tclc\t", !IO).
output_x86_64_inst(cld, !IO) :-
    io.write_string("\tcld\t", !IO).
output_x86_64_inst(cmc, !IO) :-
    io.write_string("\tcmc\t", !IO).
output_x86_64_inst(cmov(Src, Dest, Cond), !IO) :-
    output_instr_with_condition("cmov", Src, yes(Dest), Cond, !IO).
output_x86_64_inst(cmp(Src, Dest), !IO) :-
    output_instr_not_imm_dest("cmp", Src, yes(Dest), !IO).
output_x86_64_inst(cmpxchg(Src, Dest), !IO) :-
    check_operand_not_immediate(Src, Result1),
    (
        Result1 = yes,
        check_operand_register(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Src, Op1),
            operand_type(Dest, Op2),
            io.write_string("\tcmp\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: xmpxchg: unexpected:"
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: xmpxchg: unexpected:"
            ++ " invalid first operand")
    ).
output_x86_64_inst(cmpxchg8b(Op), !IO) :-
    check_operand_not_mem_ref(Op, Result),
    (
        Result = no,
        operand_type(Op, OpType),
        io.write_string("\tcmpxchg8b" ++ OpType, !IO)
    ;
        Result = yes,
        unexpected(this_file, "output_x86_64_instr: cmpxchg8b: unexpected:"
            ++ "invalid operand")
    ).
output_x86_64_inst(dec(Operand), !IO) :-
    output_instr_not_imm_dest("dec", Operand, no, !IO).
output_x86_64_inst(div(Operand), !IO) :-
    output_instr_not_imm_dest("div", Operand, no, !IO).
output_x86_64_inst(enter(StackSize, NestingLevel), !IO) :-
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
        unexpected(this_file, "output_x86_64_instr: enter: unexpected:"
            ++ " check_unsigned_int_size failed")
    ).
output_x86_64_inst(idiv(Operand), !IO) :-
    output_instr_not_imm_dest("idiv", Operand, no, !IO).
output_x86_64_inst(imul(Src, Dest, Mult), !IO) :-
    operand_type(Src, SrcType),
    io.write_string("\timul\t" ++ SrcType, !IO),
    (
        Dest = yes(DestRes),
        check_operand_register(DestRes, Result1),
        (
            Result1 = yes,
            operand_type(DestRes, DestType)
        ;
            Result1 = no,
            TempReg = operand_reg(gp_reg(13)),
            operand_type(TempReg, DestType)
        ),
        io.write_string(", " ++ DestType, !IO),
        (
            Mult = yes(MultRes),
            operand_type(MultRes, Op3),
            io.write_string(", " ++ Op3 ++ " ", !IO)
        ;
            Mult = no,
            io.write_string("\t", !IO)
        )
    ;
        Dest = no,
        io.write_string("\t\t", !IO)
   ).
output_x86_64_inst(inc(Operand), !IO) :-
    output_instr_not_imm_dest("inc", Operand, no, !IO).
output_x86_64_inst(j(Offset, Cond), !IO) :-
    output_instr_with_condition("j", Offset, no, Cond, !IO).
output_x86_64_inst(jrcxz(RelOffset), !IO) :-
    output_instr_8bit_rel_offset("jrcxz", RelOffset, !IO).
output_x86_64_inst(jmp(Target), !IO) :-
    operand_type(Target, Op),
    io.write_string("\tjmp\t" ++ Op ++ "\t\t", !IO). 
output_x86_64_inst(lea(Src, Dest), !IO) :-
    check_operand_not_mem_ref(Src, Result1),
    (
        Result1 = no,
        check_operand_register(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Src, Op1),
            operand_type(Dest, Op2),
            io.write_string("\tlea\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_inst: lea: unexpected:"
                ++ " invalid second operand")
        )
    ;
        Result1 = yes,
        unexpected(this_file, "output_x86_64_inst: lea: unexpected:"
            ++ " invalid first operand")
    ).
output_x86_64_inst(leave, !IO) :-
    io.write_string("\tleave\t", !IO).
output_x86_64_inst(loop(RelOffset), !IO) :-
    output_instr_8bit_rel_offset("loop", RelOffset, !IO).
output_x86_64_inst(loope(RelOffset), !IO) :-
    output_instr_8bit_rel_offset("loope", RelOffset, !IO).
output_x86_64_inst(loopne(RelOffset), !IO) :-
    output_instr_8bit_rel_offset("loopne", RelOffset, !IO).
output_x86_64_inst(loopnz(RelOffset), !IO) :-
    output_instr_8bit_rel_offset("loopnz", RelOffset, !IO).
output_x86_64_inst(loopz(RelOffset), !IO) :-
    output_instr_8bit_rel_offset("loopz", RelOffset, !IO).
output_x86_64_inst(mov(Src, Dest), !IO) :-
    output_instr_not_imm_dest("mov", Src, yes(Dest), !IO).
output_x86_64_inst(mul(Operand), !IO) :-
    output_instr_not_imm_dest("mul", Operand, no, !IO).
output_x86_64_inst(neg(Operand), !IO) :-
    output_instr_not_imm_dest("neg", Operand, no, !IO).
output_x86_64_inst(nop, !IO) :-
    io.write_string("nop", !IO).
output_x86_64_inst(x86_64_instr_not(Operand), !IO) :-
    output_instr_not_imm_dest("not", Operand, no, !IO).
output_x86_64_inst(or(Src, Dest), !IO) :-
    output_instr_not_imm_dest("or", Src, yes(Dest), !IO).
output_x86_64_inst(pop(Operand), !IO) :-
    output_instr_not_imm_dest("pop", Operand, no, !IO).
output_x86_64_inst(popfq, !IO) :-
    io.write_string("\tpopfq\t", !IO).
output_x86_64_inst(push(Operand), !IO) :-
    io.write_string("\tpush\t", !IO),
    operand_type(Operand, OperandType),
    io.write_string(OperandType ++ "\t", !IO).
output_x86_64_inst(pushfq, !IO) :-
    io.write_string("\tpushfq\t", !IO).
output_x86_64_inst(rc(Amnt, Dest, Cond), !IO) :-
    check_rc_first_operand(Amnt, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            io.write_string("\trc\t" ++ Cond, !IO),
            io.write_string(Op1 ++ ", " ++ Op2 ++ "\t", !IO)
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: rc: unexpected"
               ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: rc: unexpected"
            ++ " invalid first operand")
    ).
output_x86_64_inst(ret(Op), !IO) :-
    ( 
        Op = yes(OpRes),
        OpRes = uint16(NumBytes)
    ->
        check_unsigned_int_size(16, NumBytes, Result),
        ( 
            Result = yes,
            io.write_string("\tret\t", !IO),
            io.write_int(NumBytes, !IO),
            io.write_string("\t", !IO)
        ;
            Result = no,
            unexpected(this_file, "output_x86_64_instr: ret: unexpected:"
                ++ "check_unsigned_int_size failed")
        )
    ;
        Op = no
    ->
        io.write_string("\tret\t\t", !IO)
    ;
        unexpected(this_file, "output_x86_64_instr: ret: unexpected" 
            ++ " invalid operand")
    ).
output_x86_64_inst(ro(Amnt, Dest, Dir), !IO) :-
    check_operand_not_mem_ref(Amnt, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            io.write_string("\tro" ++ Dir ++ "\t", !IO),
            io.write_string(Op1 ++ ", " ++ Op2 ++ "\t\t", !IO)
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: ro: unexpected:"
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: ro: unexpected" 
            ++ " invalid first operand")
    ).
output_x86_64_inst(sal(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            io.write_string("\tsal\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: sal: unexpected:" 
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: sal: unexpected:"
            ++ "invalid first operand")
    ).
output_x86_64_inst(shl(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            io.write_string("\tshl\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: shl: unexpected:"
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: shl: unexpected:" 
            ++ " invalid first operand")
    ).
output_x86_64_inst(sar(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            io.write_string("\tsar\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: sar: unexpected:" 
                ++ "invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: sar: unexpected:"
            ++ " invalid first operand")
    ).
output_x86_64_inst(sbb(Src, Dest), !IO) :-
    output_instr_not_imm_dest("sbb", Src, yes(Dest), !IO).
output_x86_64_inst(set(Operand, Cond), !IO) :-
    check_operand_not_immediate(Operand, Result),
    (
        Result = yes,
        output_instr_with_condition("set", Operand, no, Cond, !IO)
    ;
        Result = no,
        unexpected(this_file, "output_x86_64_instr: set: unexpected" 
            ++ " invalid first operand")
    ).
output_x86_64_inst(shld(Amnt, Dest1, Reg), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest1, Result2),
        ( 
            Result2 = yes,
            check_operand_register(Reg, Result3),
            ( 
                Result3 = yes,
                operand_type(Amnt, Op1),
                operand_type(Amnt, Op2),
                operand_type(Amnt, Op3),
                io.write_string("\tshld\t" ++ Op1 ++ ", ", !IO),
                io.write_string(Op2 ++ ", " ++ Op3 ++ "\t", !IO)
            ;
                Result3 = no,
                unexpected(this_file, "output_x86_64_instr: shld: unexpected:"
                    ++ "invalid third operand")
            )
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: shld: unexpected:"
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: shld: unexpected"
            ++ " invalid first operand")
    ).
output_x86_64_inst(shr(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        ( 
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            io.write_string("\tshr\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: shr: unexpected"
               ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: shr: unexpected" 
            ++ " invalid first operand")
    ).
output_x86_64_inst(shrd(Amnt, Dest1, Reg), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest1, Result2),
        ( 
            Result2 = yes,
            check_operand_register(Reg, Result3),
            ( 
                Result3 = yes,
                operand_type(Amnt, Op1),
                operand_type(Amnt, Op2),
                operand_type(Amnt, Op3),
                io.write_string("\tshrd\t" ++ Op1 ++ ", ", !IO),
                io.write_string(Op2 ++ ", " ++ Op3 ++ "\t", !IO)
            ;
                Result3 = no,
                unexpected(this_file, "output_x86_64_instr: shrd: unexpected"
                    ++ " invalid third operand")
            )
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: shrd: unexpected"
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: shrd: unexpected:" 
          ++ " invalid first operand")
    ).
output_x86_64_inst(stc, !IO) :-
    io.write_string("\tstc\t", !IO).
output_x86_64_inst(std, !IO) :-
    io.write_string("\tstd\t", !IO).
output_x86_64_inst(sub(Src, Dest), !IO) :-
    output_instr_not_imm_dest("sub", Src, yes(Dest), !IO).
output_x86_64_inst(test(Src1, Src2), !IO) :-
    check_operand_not_mem_ref(Src1, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Src2, Result2),
        (
            Result2 = yes,
            operand_type(Src1, Op1),
            operand_type(Src2, Op2),
            io.write_string("\ttest\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: test: unexpected" 
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: test: unexpected"
            ++ " invalid first operand")
    ).
output_x86_64_inst(xadd(Src, Dest), !IO) :-
    check_operand_register(Src, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Src, Op1),
            operand_type(Dest, Op2),
            io.write_string("\txadd\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: unexpected
                xadd second operand is an immediate value")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: unexpected
            xadd first operand is not a register")
    ).
output_x86_64_inst(xchg(Src1, Src2), !IO) :-
    check_operand_reg_or_mem(Src1, Result1),
    (
        Result1 = yes,
        check_operand_reg_or_mem(Src2, Result2),
        (
            Result2 = yes,
            operand_type(Src1, Op1),
            operand_type(Src2, Op2),
            io.write_string("\txchg\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
        ;
            Result2 = no,
            unexpected(this_file, "output_x86_64_instr: xchg: unexpected"
                ++ " invalid second operand")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_x86_64_instr: xchg: unexpected" 
             ++ " invalid second operand")
    ).
output_x86_64_inst(xor(Src, Dest), !IO) :-
    output_instr_not_imm_dest("xor", Src, yes(Dest), !IO).


:- pred output_x86_64_comment(string::in, io::di, io::uo) is det.

output_x86_64_comment(Comment, !IO) :-
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


:- func reg_type(gp_reg) = string. 

reg_type(gp_reg(RegNum)) = "%r" ++ string.int_to_string(RegNum).

    % Output a string representation of a memory reference.
    %
:- pred mem_ref_type(x86_64_mem_ref::in, string::out) is det. 

mem_ref_type(mem_abs(DirectMemRef), MemRefVal) :-
    base_address_type(DirectMemRef, MemRefVal).
mem_ref_type(mem_rip(InstrPtr), MemRefVal) :-
    instr_ptr_type(InstrPtr, MemRefVal).

    % Output a string representation of a base address in a memory reference. 
    %
:- pred base_address_type(base_address::in, string::out) is det.

base_address_type(base_reg(Offset, Reg), BaseAddress) :-
    ( Offset = 0 ->
        BaseAddress = "(" ++ reg_type(Reg) ++ ")"
    ;
        BaseAddress = string.int_to_string(Offset) ++ "(" ++ reg_type(Reg) ++ ")"
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
        unexpected(this_file, "instr_ptr_type: rip_constant: unexpected"
            ++ " check_signed_int_size failed")
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
        unexpected(this_file, "rel_offset_type: ro8(int8): unexpected:"
            ++ " check_signed_int_size failed")
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
        unexpected(this_file, "rel_offset_type: ro16(int16): unexpected"
            ++ " check_signed_int_size failed")
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
        unexpected(this_file, "rel_offset_type: ro32(int32): unexpected"
            ++ " check_signed_int_size failed")
    ).


operand_type(operand_reg(Reg), RegType) :-
    RegType = reg_type(Reg).
operand_type(operand_imm(Imm), ImmVal) :-
    imm_op_type(Imm, ImmVal).
operand_type(operand_mem_ref(MemRef), MemRefVal) :-
    mem_ref_type(MemRef, MemRefVal).
operand_type(operand_rel_offset(RelOffset), RelOffsetType) :-
    rel_offset_type(RelOffset, RelOffsetType).
operand_type(operand_label(Label), (Label)).

%-----------------------------------------------------------------------------%
%
% Subsection of "Output of x86_64 instructions".
%

    % Output an instruction with either one or two operand(s). If the second
    % operand is present, it cannot be an immediate operand. 
    %
:- pred output_instr_not_imm_dest(string::in, operand::in, maybe(operand)::in,
    io::di, io::uo) is det. 

output_instr_not_imm_dest(Instr, Op1, Op2, !IO) :-
    operand_type(Op1, Op1Type),
    ( 
        Op2 = yes(Op2Result),
        check_not_both_memory_ops(Op1, Op2Result, Result1),
        ( 
            Result1 = yes,
            operand_type(Op2Result, Op2Type),
            check_operand_not_immediate(Op2Result, Result2),
            (
                Result2 = yes,
                io.write_string("\t" ++ Instr ++ "\t", !IO),
                io.write_string(Op1Type ++ ", " ++ Op2Type ++ "\t", !IO)
            ;
                Result2 = no,
                io.write_string("\tmov\t" ++ Op2Type ++ ", %r13\t", !IO),
                io.write_string("# move immediate to temp reg\n", !IO),
                io.write_string("\t" ++ Instr ++ "\t", !IO),
                io.write_string(Op1Type ++ ", " ++ "%r13\t", !IO)
            )
        ;
            Result1 = no,
            unexpected(this_file, "output_instr_not_imm_dest: unexpected:"
                ++ " invalid operands - two memory references are not allowed")
        )
    ;
        Op2 = no,
        io.write_string(Op1Type ++ "\t\t", !IO)
    ).

    % Output an instruction with a signed offset relative to the instruction 
    % pointer as an operand. 
    % Output an instruction with a signed 8-bit offset relative to the 
    % instruction pointer as an operand. 
    %
:- pred output_instr_8bit_rel_offset(string::in, operand::in, 
    io::di, io::uo) is det. 

output_instr_8bit_rel_offset(InstrName, RelOffset, !IO) :-
   check_operand_rel_offset(RelOffset, Result1),
   ( 
        Result1 = yes,
        operand_type(RelOffset, RelOffsetType),
        ( string.to_int(RelOffsetType, Val) ->
            check_signed_int_size(8, Val, Result2),
            (
                Result2 = yes,
                io.write_string("\t" ++ InstrName ++ "\t", !IO),
                io.write_int(Val, !IO),
                io.write_string("\t\t", !IO)
            ;
                Result2 = no,
                unexpected(this_file, "output_instr_8bit_rel_offset:" 
                    ++ " unexpected: check_signed_int_size failed")
            )
        ;
            unexpected(this_file, "output_instr_8bit_rel_offset: unexpected:"
                ++ " string.to_int failed")
        )
   ;
        Result1 = no,
        unexpected(this_file, "output_instr_8bit_rel_offset: unexpected:"
            ++ " invalid operand - operand is not a relative offset")
   ).

:- pred output_bit_test_instr(string::in, operand::in, operand::in, io::di, 
    io::uo) is det. 

output_bit_test_instr(Instr, Src, Idx, !IO) :-
    check_operand_not_immediate(Src, Result1),
    (
        Result1 = yes,
        operand_type(Src, Op1),
        check_operand_not_mem_ref(Idx, Result2),
        (
            Result2 = yes,
            operand_type(Idx, Op2),
            ( string.to_int(Op2, IdxInt) ->
                check_signed_int_size(8, IdxInt, Result3),
                ( 
                    Result3 = yes,
                    io.write_string("\t" ++ Instr ++ "\t", !IO),
                    io.write_string(Op1 ++ ", " ++ Op2 ++ "\t", !IO)
                ;
                    Result3 = no,
                    unexpected(this_file, "output_bit_test_instr: bt:"
                        ++ " unexpected: invalid second operand")
                )
            ;
                unexpected(this_file, "output_bit_test_instr: unexpected:"
                    ++ " string.to_int failed")
            )
        ;
            Result2 = no,
            unexpected(this_file, "output_bit_test_instr: bt: unexpected:" 
                ++ " invalid second operand - memory reference is not allowed")
        )
    ;
        Result1 = no,
        unexpected(this_file, "output_bit_test_instr: bt: unexpected:"
            ++ " invalid first operand - immediate value is not allowed")
    ).

:- pred output_instr_with_condition(string::in, operand::in, maybe(operand)::in,
    condition::in, io::di, io::uo) is det. 

output_instr_with_condition(Instr, Op1, Op2, Cond, !IO) :-
    check_operand_not_immediate(Op1, Result1),
    (
        Result1 = yes,
        instr_condition(Cond, CondRes),
        io.write_string("\t" ++ Instr, !IO),
        io.write_string(CondRes ++ "\t", !IO),
        operand_type(Op1, Op1Type),
        io.write_string(Op1Type, !IO),
        (
            Op2 = yes(Op2Res),
            check_operand_register(Op2Res, Result3),
            (
                Result3 = yes,
                operand_type(Op2Res, Op2Type),
                io.write_string(", " ++ Op2Type, !IO)
            ;
                Result3 = no,
                    unexpected(this_file, "output_instr_with_condition:"
                        ++ " invalid second operand")
            )
       ;
            Op2 = no,
            io.write_string("\t\t", !IO)
       )
    ;            
        Result1 = no,
        unexpected(this_file, "output_instr_with_condition: unexpected:" 
            ++ "invalid first operand - immediate value is not allowed")
   ).

%-----------------------------------------------------------------------------%

:- pred check_rc_first_operand(operand::in, bool::out) is det. 

check_rc_first_operand(Op, Result) :-
    ( Op = operand_imm(_) ->
        operand_type(Op, OpType),
        ( string.to_int(OpType, OpInt) ->
            check_unsigned_int_size(8, OpInt, Result1),
            ( 
                Result1 = yes,
                Result = yes
            ;
                Result1 = no,
                Result = no
            )
        ;
            unexpected(this_file, "check_rc_first_operand: unexpected:" 
                ++ " string.to_int")
        )
    ;
        Op = operand_reg(_) ->
        check_operand_register(Op, Result2),
        (   
            Result2 = yes,
            Result = yes
       ;
            Result2 = no,
            Result = no
        )
    ;
        unexpected(this_file, "check_rc_first_operand: unexpected:" 
            ++ " invalid operand")
    ). 

:- pred check_not_both_memory_ops(operand::in, operand::in, bool::out) is det. 

check_not_both_memory_ops(Op1, Op2, Result) :-
    (
        Op1 = operand_mem_ref(_),
        Op2 = operand_mem_ref(_)
    ->
        Result = no
    ;
        Result = yes    
    ).

:- pred check_operand_not_immediate(operand::in, bool::out) is det. 

check_operand_not_immediate(Operand, Result) :-
    ( Operand = operand_imm(_) ->
        Result = no
    ;
        Result = yes
    ).

:- pred check_operand_reg_or_mem(operand::in, bool::out) is det. 

check_operand_reg_or_mem(Operand, Result) :-
    ( Operand = operand_reg(_) ->
        Result = yes
    ;
        Operand = operand_mem_ref(_) ->
        Result = yes
    ;
        Result = no
    ).

:- pred check_operand_unsigned_imm_or_reg(operand::in, bool::out) is det. 

check_operand_unsigned_imm_or_reg(Operand, Result) :-
    ( Operand = operand_imm(Imm) ->
        imm_op_type(Imm, ImmType), 
        ( string.to_int(ImmType, ImmInt) ->
            ( 
                check_unsigned_int_size(32, ImmInt, Result1),
                (
                    Result1 = yes,
                    Result = yes
                ;
                    Result1 = no,
                    Result = no
                )
            )
        ;
            unexpected(this_file, "check_operand_unsigned_imm_or_reg:"
                ++ " unexpected: string.to_int failed")
        )
    ;
        Result = no
    ).

:- pred check_operand_register(operand::in, bool::out) is det. 

check_operand_register(Operand, Result) :-
    ( Operand = operand_reg(_) ->
        Result = yes
    ;
        Result =  no
    ).

:- pred check_operand_not_mem_ref(operand::in, bool::out) is det. 

check_operand_not_mem_ref(Operand, Result) :-
    ( Operand = operand_mem_ref(_) ->
        Result = no
    ;
        Result = yes
    ).

:- pred check_operand_rel_offset(operand::in, bool::out) is det. 

check_operand_rel_offset(Operand, Result) :-
    ( Operand = operand_rel_offset(_) ->
        Result = yes 
    ;
        Result = no
    ).

    % Check if the argument for a conditional instruction is valid. 
    %
:- pred instr_condition(condition::in, string::out) is det. 

instr_condition(o, "o").
instr_condition(no, "no").
instr_condition(b, "b").
instr_condition(c, "c").
instr_condition(nae, "nae").
instr_condition(nb, "nb").
instr_condition(nc, "nc").
instr_condition(ae, "ae").
instr_condition(z, "z").
instr_condition(e, "e").
instr_condition(nz, "nz").
instr_condition(ne, "ne").
instr_condition(be, "be").
instr_condition(na, "na").
instr_condition(nbe, "nbe").
instr_condition(a, "a").
instr_condition(s, "s").
instr_condition(ns, "ns").
instr_condition(p, "p").
instr_condition(pe, "pe").
instr_condition(np, "np").
instr_condition(po, "po").
instr_condition(l, "l").
instr_condition(nge, "nge").
instr_condition(nl, "nl").
instr_condition(ge, "ge").
instr_condition(le, "le").
instr_condition(ng, "ng").
instr_condition(nle, "nle").
instr_condition(g, "g").

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
