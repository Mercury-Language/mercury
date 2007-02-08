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
% This module defines routines for writing x86_64 assembler instructions
% to string writer streams that are attached to the I/O state. 
% (There's no particularly good reason for this latter restriction so 
% it can safely be dropped if necessary.)
%
%-----------------------------------------------------------------------------%

:- module ll_backend.x86_64_out.
:- interface.

:- import_module ll_backend.x86_64_instrs.

:- import_module io.
:- import_module stream.

%-----------------------------------------------------------------------------%

    % Output an x86_64_instruction to the given stream.
    %
:- pred output_x86_64_instruction(Stream::in, x86_64_instruction::in, 
    io::di, io::uo) is det <= stream.writer(Stream, string, io). 

    % XXX this is misnamed: it should be operand_to_string.
    %
:- pred operand_type(operand::in, string::out) is det. 

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module stream.string_writer.
:- import_module string.
:- import_module type_desc.

%-----------------------------------------------------------------------------%
%
% Output x86_64 pseudo-op for GNU as
%

:- pred output_x86_64_pseudo_op(Stream::in, pseudo_op::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

output_x86_64_pseudo_op(Stream, abort, !IO) :-
    put(Stream, "\t.abort\n", !IO).
output_x86_64_pseudo_op(Stream, align(Bytes, FillVal, SkipBytes), !IO) :-
    output_pseudo_op_with_int_args(Stream, ".align", Bytes, FillVal,
        SkipBytes, !IO).
output_x86_64_pseudo_op(Stream, ascii(Literals), !IO) :-
    output_pseudo_op_str_args(Stream, ".ascii", Literals, !IO).
output_x86_64_pseudo_op(Stream, asciiz(Literals), !IO) :-
    output_pseudo_op_str_args(Stream, ".asciiz", Literals, !IO).
output_x86_64_pseudo_op(Stream, balign(Bytes, FillVal, SkipBytes), !IO) :-
    output_pseudo_op_with_int_args(Stream, ".balign", Bytes, FillVal,
        SkipBytes, !IO).
output_x86_64_pseudo_op(Stream, byte(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".byte", ExprList, !IO).
output_x86_64_pseudo_op(Stream, comm(Symbol, Length, Align0), !IO) :-
    put(Stream, "\t.comm\t" ++ Symbol ++ ",", !IO),
    put_int(Stream, Length, !IO),
    ( 
        Align0 = yes(Align1),
        put(Stream, ",", !IO),
        put_int(Stream, Align1, !IO)
    ;
        Align0 = no
    ),
    put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, data(Subsection0), !IO) :-
    put(Stream, "\t.data\t", !IO),
    ( 
        Subsection0 = yes(Subsection1),
        put_int(Stream, Subsection1, !IO)
    ;
        Subsection0 = no
    ),
    put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, desc(Symbol, AbsExpr), !IO) :-
    put(Stream, "\t.desc\t" ++ Symbol ++ "," ++ AbsExpr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, def(Name), !IO) :-
    put(Stream, "\t.def\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, dim, !IO) :-
    put(Stream, "\t.dim\n", !IO).
output_x86_64_pseudo_op(Stream, double(NumList), !IO) :-
    output_pseudo_op_float_args(Stream, ".double", NumList, !IO).
output_x86_64_pseudo_op(Stream, eject, !IO) :-
    put(Stream, "\t.eject\n", !IO).
output_x86_64_pseudo_op(Stream, x86_64_pseudo_else, !IO) :-
    put(Stream, "\t.else\n", !IO).
output_x86_64_pseudo_op(Stream, elseif, !IO) :-
    put(Stream, "\t.elseif\n", !IO).
output_x86_64_pseudo_op(Stream, end, !IO) :-
    put(Stream, "\t.end\n", !IO).
output_x86_64_pseudo_op(Stream, endef, !IO) :-
    put(Stream, "\t.endef\n", !IO).
output_x86_64_pseudo_op(Stream, endfunc, !IO) :-
    put(Stream, "\t.endfunc\n", !IO).
output_x86_64_pseudo_op(Stream, endif, !IO) :-
    put(Stream, "\t.endif\n", !IO).
output_x86_64_pseudo_op(Stream, endm, !IO) :-
    put(Stream, "\t.endm\n", !IO).
output_x86_64_pseudo_op(Stream, equ(Symbol, Expr), !IO) :-
    put(Stream, "\t.equ\t" ++ Symbol ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, equiv(Symbol, Expr), !IO) :-
    put(Stream, "\t.equiv\t" ++ Symbol ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, err, !IO) :-
    put(Stream, "\t.err\n", !IO).
output_x86_64_pseudo_op(Stream, exitm, !IO) :-
    put(Stream, "\t.exitm\n", !IO).
output_x86_64_pseudo_op(Stream, extern, !IO) :-
    put(Stream, "\t.extern\n", !IO).
output_x86_64_pseudo_op(Stream, fail_(Expr), !IO) :-
    put(Stream, "\t.fail\t" ++ Expr++ "\n", !IO).
output_x86_64_pseudo_op(Stream, file(Name), !IO) :-
    put(Stream, "\t.file\t\"" ++ Name ++ "\"\n", !IO).
output_x86_64_pseudo_op(Stream, fill(Repeat, Size, Val), !IO) :-
    output_pseudo_op_with_int_args(Stream, ".repeat", Repeat, Size, Val, !IO).
output_x86_64_pseudo_op(Stream, float(NumList), !IO) :-
    output_pseudo_op_float_args(Stream, ".float", NumList, !IO).
output_x86_64_pseudo_op(Stream, func_(Name, Label), !IO) :-
    put(Stream, "\t.func\t" ++ Name ++ "," ++ Label ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, global(Symbol), !IO) :-
    put(Stream, "\t.global\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, globl(Symbol), !IO) :-
    put(Stream, "\t.globl\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, hidden(Name), !IO) :-
    put(Stream, "\t.hidden\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, hword(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".hword", ExprList, !IO).
output_x86_64_pseudo_op(Stream, ident, !IO) :-
    put(Stream, "\t.ident\n", !IO).
output_x86_64_pseudo_op(Stream, x86_64_pseudo_if(Expr), !IO) :-
    put(Stream, "\t.if\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifdef(Symbol), !IO) :-
    put(Stream, "\t.ifdef\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifc(Str1, Str2), !IO) :-
    put(Stream, "\t.ifc\t" ++ Str1 ++ "," ++ Str2 ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifeq(Expr), !IO) :-
    put(Stream, "\t.ifeq\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifge(Expr), !IO) :-
    put(Stream, "\t.ifge\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifgt(Expr), !IO) :-
    put(Stream, "\t.ifgt\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifle(Expr), !IO) :-
    put(Stream, "\t.ifle\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, iflt(Expr), !IO) :-
    put(Stream, "\t.iflt\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifnc(Str1, Str2), !IO) :-
    put(Stream, "\t.ifnc\t" ++ Str1 ++ "," ++ Str2 ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifndef(Symbol), !IO) :-
    put(Stream, "\t.ifndef\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifnotdef(Symbol), !IO) :-
    put(Stream, "\t.ifnotdef\t" ++ Symbol ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifne(Expr), !IO) :-
    put(Stream, "\t.ifne\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, ifnes(Str1, Str2), !IO) :-
    put(Stream, "\t.ifnes\t" ++ Str1 ++ "," ++ Str2 ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, include(File), !IO) :-
    put(Stream, "\t.include\t" ++ "\"" ++ File ++ "\"\n", !IO).
output_x86_64_pseudo_op(Stream, int(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".int", ExprList, !IO).
output_x86_64_pseudo_op(Stream, internal(Name), !IO) :-
    put(Stream, "\t.internal\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, lcomm(Symbol, Length), !IO) :-
    put(Stream, "\tlcomm\t" ++ Symbol, !IO),
    put_int(Stream, Length, !IO),
    put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, line(LineNum), !IO) :-
    put(Stream, "\t.line\t", !IO),
    put_int(Stream, LineNum, !IO),
    put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, list, !IO) :-
    put(Stream, "\t.list\n", !IO).
output_x86_64_pseudo_op(Stream, long(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".long", ExprList, !IO).
output_x86_64_pseudo_op(Stream, macro, !IO) :-
    put(Stream, "\t.macro\n", !IO).
output_x86_64_pseudo_op(Stream, nolist, !IO) :-
    put(Stream, "\t.nolist\n", !IO).
output_x86_64_pseudo_op(Stream, p2align(PowBit, FillVal, SkipBytes), !IO) :-
    output_pseudo_op_with_int_args(Stream, ".p2align", PowBit, FillVal,
        SkipBytes, !IO).
output_x86_64_pseudo_op(Stream, popsection, !IO) :-
    put(Stream, "\t.popsection\n", !IO).
output_x86_64_pseudo_op(Stream, previous, !IO) :-
    put(Stream, "\t.previous\n", !IO).
output_x86_64_pseudo_op(Stream, print(Str), !IO) :-
    put(Stream, "\t.print\t" ++ Str ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, protected(Name), !IO) :-
    put(Stream, "\t.protected\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, psize(Lines, Cols), !IO) :-
    output_pseudo_op_with_int_args(Stream, ".psize", Lines, Cols, no, !IO).
output_x86_64_pseudo_op(Stream, purgem(Name), !IO) :-
    put(Stream, "\t.purgem\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, pushsection(Name, Subsection), !IO) :-
    put(Stream, "\t.pushsection\t" ++ Name, !IO),
    put_int(Stream, Subsection, !IO),
    put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, quad(BigNumList), !IO) :-
    output_pseudo_op_str_args(Stream, "\t.quad\t", BigNumList, !IO).
output_x86_64_pseudo_op(Stream, rept(Count), !IO) :-
    put(Stream, "\t.rept\t", !IO),
    put_int(Stream, Count, !IO),
    put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, sbttl(SubHeading), !IO) :-
    put(Stream, "\t.sbttl\t" ++ "\"" ++ SubHeading ++ "\"\n", !IO).
output_x86_64_pseudo_op(Stream, scl(Class), !IO) :-
    put(Stream, "\t.scl\t" ++ Class ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, section(Name, Flags0, Type0, EntSize0), !IO) :-
    put(Stream, "\t.section\t" ++ Name, !IO),
    ( 
        Flags0 = yes(Flags1),
        check_section_flags_and_type(Flags1, Type0, Result0),
        ( 
            Result0 = yes,
            Type0 = yes(Type1)
        ->
            put(Stream, ",\"" ++ Flags1 ++ "\"", !IO),
            ( check_pseudo_section_type(Type1) ->
                put(Stream, "," ++ Type1, !IO)
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
        put(Stream, ",", !IO),
        put_int(Stream, EntSize1, !IO)
     ;
        EntSize0 = no
     ),
     put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, set(Symbol, Expr), !IO) :-
    put(Stream, "\t.set\t" ++ Symbol ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, short(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".short", ExprList, !IO).
output_x86_64_pseudo_op(Stream, single(FloatList), !IO) :-
    output_pseudo_op_float_args(Stream, ".single", FloatList, !IO).
output_x86_64_pseudo_op(Stream, size(Name, Expr), !IO) :-
    put(Stream, "\t.size\t" ++ Name ++ "," ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, skip(Size, Val), !IO) :-
    output_pseudo_op_with_int_args(Stream, ".skip", Size, Val, no, !IO).
output_x86_64_pseudo_op(Stream, sleb128(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".sleb128\t", ExprList, !IO).
output_x86_64_pseudo_op(Stream, space(Size, Fill), !IO) :-
    output_pseudo_op_with_int_args(Stream, ".space", Size, Fill, no, !IO).
output_x86_64_pseudo_op(Stream, string(StrList), !IO) :-
    output_pseudo_op_str_args(Stream, ".string", StrList, !IO).
output_x86_64_pseudo_op(Stream, struct(Expr), !IO) :-
    put(Stream, "\t.struct\t" ++ Expr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, subsection(Name), !IO) :-
    put(Stream, "\t.subsection\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, symver(Name, Alias), !IO) :-
    put(Stream, "\t.symver\t" ++ Name ++ "," ++ Alias ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, tag(Name), !IO) :-
    put(Stream, "\t.tag\t" ++ Name ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, text(Subsection0), !IO) :-
    put(Stream, "\ttext\t", !IO),
    ( 
        Subsection0 = yes(Subsection1),
        put_int(Stream, Subsection1, !IO)
    ;
        Subsection0 = no
    ),
    put(Stream, "\n", !IO).
output_x86_64_pseudo_op(Stream, title(Heading), !IO) :-
    put(Stream, "\t.title\t" ++ Heading ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, x86_64_pseudo_type(Name, Desc), !IO) :-
    ( check_pseudo_type_desc(Desc) ->
        put(Stream, "\t.type\t" ++ Name ++ "," ++ Desc ++ "\n", !IO)
    ;
       unexpected(this_file, "output_x86_64_pseudo_op: x86_64_pseudo_type:"
            ++ " unexpected: check_pseudo_type_desc failed") 
    ).
output_x86_64_pseudo_op(Stream, uleb128(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".uleb128", ExprList, !IO).
output_x86_64_pseudo_op(Stream, val(Addr), !IO) :-
    put(Stream, "\t.val\t" ++ Addr ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, version(Note), !IO) :-
    put(Stream, "\t.version\t" ++ Note ++ "\n", !IO).
output_x86_64_pseudo_op(Stream, weak(NameList), !IO) :-
    output_pseudo_op_str_args(Stream, ".weak", NameList, !IO).
output_x86_64_pseudo_op(Stream, word(ExprList), !IO) :-
    output_pseudo_op_str_args(Stream, ".word", ExprList, !IO).

    % Output pseudo-op name with 1, 2 or 3 integer arguments. 
    %
:- pred output_pseudo_op_with_int_args(Stream::in, string::in, int::in,
    maybe(int)::in, maybe(int)::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

output_pseudo_op_with_int_args(Stream, OpName, Arg1, Arg2, Arg3, !IO) :-
    put(Stream, "\t" ++ OpName ++ "\t", !IO),
    put_int(Stream, Arg1, !IO),
    ( 
        Arg2 = yes(Arg2Out),
        put(Stream, ",", !IO),
        put_int(Stream, Arg2Out, !IO)
    ;
        Arg2 = no
    ),
    ( 
        Arg3 = yes(Arg3Out),
        ( 
            Arg2 = no,
            put(Stream, ",,", !IO),
            put_int(Stream, Arg3Out, !IO)
        ;
            Arg2 = yes(_),
            put(Stream, ",", !IO),
            put_int(Stream, Arg3Out, !IO)
        )
    ;
        Arg3 = no
    ),
    put(Stream, "\n", !IO).

    % Output pseudo-op having list of float as its argument.
    %
:- pred output_pseudo_op_float_args(Stream::in, string::in, list(float)::in, 
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

output_pseudo_op_float_args(Stream, OpName, FloatArgs, !IO) :-
    put(Stream, "\t" ++ OpName ++ "\t", !IO),
    pseudo_op_float_args_while(Stream, FloatArgs, !IO),
    put(Stream, "\n", !IO).
 
:- pred pseudo_op_float_args_while(Stream::in, list(float)::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

pseudo_op_float_args_while(_, [], !IO).
pseudo_op_float_args_while(Stream, [Arg | Args], !IO) :-
    put_float(Stream, Arg, !IO),
    ( 
        Args = [],
        pseudo_op_float_args_while(Stream, Args, !IO)
    ;
        Args = [_ | _],
        put(Stream, ",", !IO),
        pseudo_op_float_args_while(Stream, Args, !IO)
    ).

    % Output a pseudo-op that has a list of string as it's
    % argument.
    %
:- pred output_pseudo_op_str_args(Stream::in, string::in, list(string)::in, 
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

output_pseudo_op_str_args(Stream, OpName, StrArgs, !IO) :-
    put(Stream, "\t" ++ OpName ++ "\t", !IO),
    pseudo_op_str_args_while(Stream, StrArgs, !IO),
    put(Stream, "\n", !IO).
 
:- pred pseudo_op_str_args_while(Stream::in, list(string)::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

pseudo_op_str_args_while(_, [], !IO).
pseudo_op_str_args_while(Stream, [Arg | Args], !IO) :-
    put(Stream, string.word_wrap("\"" ++ Arg ++ "\"", comment_length), !IO),
    ( 
        Args = [],
        pseudo_op_str_args_while(Stream, Args, !IO)
    ;
        Args = [_ | _],
        put(Stream, ",", !IO),
        pseudo_op_str_args_while(Stream, Args, !IO)
    ).

    % Check if the FLAGS and TYPE argumentis of '.section' pseudo-op
    % are valid.
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

    % Check if the FLAGS argument of '.section' pseudo-op is valid.
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
% Output x86_64 instructions
%

    % Output x86_64 instruction and x86_64_comment. 
    % 
    % XXX There should be a way of turning of comments since we don't
    % really want to output them in __asm__ blocks.
    % 
output_x86_64_instruction(Stream, x86_64_instr(Instr, Comment), !IO) :-
    output_x86_64_comment(Stream, Comment, !IO),
    output_x86_64_instr_list(Stream, Instr, !IO),
    put(Stream, "\n", !IO).

:- pred output_x86_64_instr_list(Stream::in, list(x86_64_instr)::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

output_x86_64_instr_list(Stream, Instrs, !IO) :-
    list.foldl(output_x86_64_instr(Stream), Instrs, !IO).

:- pred output_x86_64_instr(Stream::in, x86_64_instr::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

output_x86_64_instr(Stream, x86_64_comment(Comment), !IO) :-
    ( string.length(Comment) > 0 ->
        put(Stream, "\t# ", !IO),
        ( string.length(Comment) > comment_length ->
            string.split(Comment, comment_length, Comment1, Comment2),
            put(Stream, string.word_wrap(Comment1, comment_length), !IO),
            put(Stream, "\n", !IO),
            output_x86_64_instr(Stream, x86_64_comment(Comment2), !IO)
        ;   
            put(Stream, string.word_wrap(Comment, comment_length), !IO)
        )
    ;
        true
    ).
output_x86_64_instr(Stream, x86_64_label(LabelName), !IO) :-
    ( string.length(LabelName) > 0 ->
        put(Stream, "\n" ++ LabelName ++ ":", !IO)
    ;
        true
    ).
output_x86_64_instr(Stream, x86_64_directive(PseudoOp), !IO) :-
    output_x86_64_pseudo_op(Stream, PseudoOp, !IO).
output_x86_64_instr(Stream, x86_64_instr(Instr), !IO) :-
    output_x86_64_inst(Stream, Instr, !IO),
    put(Stream, "\n", !IO).

    % Output a single x86_64 instruction and its operands (if any). 
    %
:- pred output_x86_64_inst(Stream::in, x86_64_inst::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

output_x86_64_inst(Stream, adc(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "adc", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, add(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "add", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, and(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "and", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, bs(Src, Dest, Cond), !IO) :-
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
            put(Stream, "\t" ++ Instr ++ "\t", !IO),
            operand_type(Dest, DestType),
            put(Stream, SrcType ++ ", " ++ DestType ++ "\t", !IO)
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
output_x86_64_inst(Stream, bswap(Op), !IO) :-
    check_operand_register(Op, Result),
    (
        Result = yes,
        operand_type(Op, RegType), 
        put(Stream, "\tbswap\t" ++ RegType ++ "\t\t", !IO)
    ;
        Result = no,
        unexpected(this_file, "output_x86_64_instr: bswap: unexpected: operand"
            ++ " is not a register")
    ). 
output_x86_64_inst(Stream, bt(Src, Idx), !IO) :-
    output_bit_test_instr(Stream, "bt", Src, Idx, !IO).
output_x86_64_inst(Stream, btc(Src, Idx), !IO) :-
    output_bit_test_instr(Stream, "btc", Src, Idx, !IO).
output_x86_64_inst(Stream, btr(Src, Idx), !IO) :-
    output_bit_test_instr(Stream, "btr", Src, Idx, !IO).
output_x86_64_inst(Stream, bts(Src, Idx), !IO) :-
    output_bit_test_instr(Stream, "bts", Src, Idx, !IO).
output_x86_64_inst(Stream, call(Target), !IO) :-
    check_operand_not_immediate(Target, Result),
    (
        Result = yes,
        operand_type(Target, TargetType),
        put(Stream, "\tcall\t" ++ TargetType ++ "\t\t", !IO)
    ;
        Result = no,
        unexpected(this_file, "output_x86_64_instr: call: unexpected:" 
            ++ " invalid target operand")
    ).
output_x86_64_inst(Stream, cbw, !IO) :-
    put(Stream, "\tcbw\t", !IO).
output_x86_64_inst(Stream, cwde, !IO) :-
    put(Stream, "\tcwde\t", !IO).
output_x86_64_inst(Stream, cdqe, !IO) :-
    put(Stream, "\tcdqe\t", !IO).
output_x86_64_inst(Stream, cwd, !IO) :-
    put(Stream, "\tcwd\t", !IO).
output_x86_64_inst(Stream, cdq, !IO) :-
    put(Stream, "\tcdq\t", !IO).
output_x86_64_inst(Stream, cqo, !IO) :-
    put(Stream, "\tcqo\t", !IO).
output_x86_64_inst(Stream, clc, !IO) :-
    put(Stream, "\tclc\t", !IO).
output_x86_64_inst(Stream, cld, !IO) :-
    put(Stream, "\tcld\t", !IO).
output_x86_64_inst(Stream, cmc, !IO) :-
    put(Stream, "\tcmc\t", !IO).
output_x86_64_inst(Stream, cmov(Src, Dest, Cond), !IO) :-
    output_instr_with_condition(Stream, "cmov", Src, yes(Dest), Cond, !IO).
output_x86_64_inst(Stream, cmp(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "cmp", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, cmpxchg(Src, Dest), !IO) :-
    check_operand_not_immediate(Src, Result1),
    (
        Result1 = yes,
        check_operand_register(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Src, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\tcmp\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
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
output_x86_64_inst(Stream, cmpxchg8b(Op), !IO) :-
    check_operand_not_mem_ref(Op, Result),
    (
        Result = no,
        operand_type(Op, OpType),
        put(Stream, "\tcmpxchg8b" ++ OpType, !IO)
    ;
        Result = yes,
        unexpected(this_file, "output_x86_64_instr: cmpxchg8b: unexpected:"
            ++ "invalid operand")
    ).
output_x86_64_inst(Stream, dec(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "dec", Operand, no, !IO).
output_x86_64_inst(Stream, div(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "div", Operand, no, !IO).
output_x86_64_inst(Stream, enter(StackSize, NestingLevel), !IO) :-
    StackSize = uint16(Size),
    NestingLevel = uint8(Level),
    check_unsigned_int_size(16, Size, Result0),
    check_unsigned_int_size(8, Level, Result1),
    ( 
        Result0 = yes,
        Result1 = yes 
    ->
        put(Stream, "\tenter\t", !IO),
        put_int(Stream, Size, !IO),
        put(Stream, ",", !IO),
        put_int(Stream, Level, !IO),
        put(Stream, "\t", !IO)
    ;
        unexpected(this_file, "output_x86_64_instr: enter: unexpected:"
            ++ " check_unsigned_int_size failed")
    ).
output_x86_64_inst(Stream, idiv(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "idiv", Operand, no, !IO).
output_x86_64_inst(Stream, imul(Src, Dest, Mult), !IO) :-
    operand_type(Src, SrcType),
    put(Stream, "\timul\t" ++ SrcType, !IO),
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
        put(Stream, ", " ++ DestType, !IO),
        (
            Mult = yes(MultRes),
            operand_type(MultRes, Op3),
            put(Stream, ", " ++ Op3 ++ " ", !IO)
        ;
            Mult = no,
            put(Stream, "\t", !IO)
        )
    ;
        Dest = no,
        put(Stream, "\t\t", !IO)
   ).
output_x86_64_inst(Stream, inc(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "inc", Operand, no, !IO).
output_x86_64_inst(Stream, j(Offset, Cond), !IO) :-
    output_instr_with_condition(Stream, "j", Offset, no, Cond, !IO).
output_x86_64_inst(Stream, jrcxz(RelOffset), !IO) :-
    output_instr_8bit_rel_offset(Stream, "jrcxz", RelOffset, !IO).
output_x86_64_inst(Stream, jmp(Target), !IO) :-
    operand_type(Target, Op),
    put(Stream, "\tjmp\t" ++ Op ++ "\t\t", !IO). 
output_x86_64_inst(Stream, lea(Src, Dest), !IO) :-
    check_operand_not_mem_ref(Src, Result1),
    (
        Result1 = no,
        check_operand_register(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Src, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\tlea\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
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
output_x86_64_inst(Stream, leave, !IO) :-
    put(Stream, "\tleave\t", !IO).
output_x86_64_inst(Stream, loop(RelOffset), !IO) :-
    output_instr_8bit_rel_offset(Stream, "loop", RelOffset, !IO).
output_x86_64_inst(Stream, loope(RelOffset), !IO) :-
    output_instr_8bit_rel_offset(Stream, "loope", RelOffset, !IO).
output_x86_64_inst(Stream, loopne(RelOffset), !IO) :-
    output_instr_8bit_rel_offset(Stream, "loopne", RelOffset, !IO).
output_x86_64_inst(Stream, loopnz(RelOffset), !IO) :-
    output_instr_8bit_rel_offset(Stream, "loopnz", RelOffset, !IO).
output_x86_64_inst(Stream, loopz(RelOffset), !IO) :-
    output_instr_8bit_rel_offset(Stream, "loopz", RelOffset, !IO).
output_x86_64_inst(Stream, mov(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "mov", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, mul(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "mul", Operand, no, !IO).
output_x86_64_inst(Stream, neg(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "neg", Operand, no, !IO).
output_x86_64_inst(Stream, nop, !IO) :-
    put(Stream, "nop", !IO).
output_x86_64_inst(Stream, x86_64_instr_not(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "not", Operand, no, !IO).
output_x86_64_inst(Stream, or(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "or", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, pop(Operand), !IO) :-
    output_instr_not_imm_dest(Stream, "pop", Operand, no, !IO).
output_x86_64_inst(Stream, popfq, !IO) :-
    put(Stream, "\tpopfq\t", !IO).
output_x86_64_inst(Stream, push(Operand), !IO) :-
    put(Stream, "\tpush\t", !IO),
    operand_type(Operand, OperandType),
    put(Stream, OperandType ++ "\t", !IO).
output_x86_64_inst(Stream, pushfq, !IO) :-
    put(Stream, "\tpushfq\t", !IO).
output_x86_64_inst(Stream, rc(Amnt, Dest, Cond), !IO) :-
    check_rc_first_operand(Amnt, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\trc\t" ++ Cond, !IO),
            put(Stream, Op1 ++ ", " ++ Op2 ++ "\t", !IO)
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
output_x86_64_inst(Stream, ret(Op), !IO) :-
    ( 
        Op = yes(OpRes),
        OpRes = uint16(NumBytes)
    ->
        check_unsigned_int_size(16, NumBytes, Result),
        ( 
            Result = yes,
            put(Stream, "\tret\t", !IO),
            put_int(Stream, NumBytes, !IO),
            put(Stream, "\t", !IO)
        ;
            Result = no,
            unexpected(this_file, "output_x86_64_instr: ret: unexpected:"
                ++ "check_unsigned_int_size failed")
        )
    ;
        Op = no
    ->
        put(Stream, "\tret\t\t", !IO)
    ;
        unexpected(this_file, "output_x86_64_instr: ret: unexpected" 
            ++ " invalid operand")
    ).
output_x86_64_inst(Stream, ro(Amnt, Dest, Dir), !IO) :-
    check_operand_not_mem_ref(Amnt, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\tro" ++ Dir ++ "\t", !IO),
            put(Stream, Op1 ++ ", " ++ Op2 ++ "\t\t", !IO)
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
output_x86_64_inst(Stream, sal(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\tsal\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
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
output_x86_64_inst(Stream, shl(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\tshl\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
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
output_x86_64_inst(Stream, sar(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\tsar\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO) 
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
output_x86_64_inst(Stream, sbb(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "sbb", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, set(Operand, Cond), !IO) :-
    check_operand_not_immediate(Operand, Result),
    (
        Result = yes,
        output_instr_with_condition(Stream, "set", Operand, no, Cond, !IO)
    ;
        Result = no,
        unexpected(this_file, "output_x86_64_instr: set: unexpected" 
            ++ " invalid first operand")
    ).
output_x86_64_inst(Stream, shld(Amnt, Dest1, Reg), !IO) :-
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
                put(Stream, "\tshld\t" ++ Op1 ++ ", ", !IO),
                put(Stream, Op2 ++ ", " ++ Op3 ++ "\t", !IO)
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
output_x86_64_inst(Stream, shr(Amnt, Dest), !IO) :-
    check_operand_unsigned_imm_or_reg(Amnt, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        ( 
            Result2 = yes,
            operand_type(Amnt, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\tshr\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
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
output_x86_64_inst(Stream, shrd(Amnt, Dest1, Reg), !IO) :-
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
                put(Stream, "\tshrd\t" ++ Op1 ++ ", ", !IO),
                put(Stream, Op2 ++ ", " ++ Op3 ++ "\t", !IO)
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
output_x86_64_inst(Stream, stc, !IO) :-
    put(Stream, "\tstc\t", !IO).
output_x86_64_inst(Stream, std, !IO) :-
    put(Stream, "\tstd\t", !IO).
output_x86_64_inst(Stream, sub(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "sub", Src, yes(Dest), !IO).
output_x86_64_inst(Stream, test(Src1, Src2), !IO) :-
    check_operand_not_mem_ref(Src1, Result1),
    (
        Result1 = yes,
        check_operand_not_immediate(Src2, Result2),
        (
            Result2 = yes,
            operand_type(Src1, Op1),
            operand_type(Src2, Op2),
            put(Stream, "\ttest\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
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
output_x86_64_inst(Stream, xadd(Src, Dest), !IO) :-
    check_operand_register(Src, Result1),
    ( 
        Result1 = yes,
        check_operand_not_immediate(Dest, Result2),
        (
            Result2 = yes,
            operand_type(Src, Op1),
            operand_type(Dest, Op2),
            put(Stream, "\txadd\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
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
output_x86_64_inst(Stream, xchg(Src1, Src2), !IO) :-
    check_operand_reg_or_mem(Src1, Result1),
    (
        Result1 = yes,
        check_operand_reg_or_mem(Src2, Result2),
        (
            Result2 = yes,
            operand_type(Src1, Op1),
            operand_type(Src2, Op2),
            put(Stream, "\txchg\t" ++ Op1 ++ ", " ++ Op2 ++ "\t", !IO)
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
output_x86_64_inst(Stream, xor(Src, Dest), !IO) :-
    output_instr_not_imm_dest(Stream, "xor", Src, yes(Dest), !IO).


:- pred output_x86_64_comment(Stream::in, string::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

output_x86_64_comment(Stream, Comment, !IO) :-
    ( string.length(Comment) > 0 ->
        put(Stream, "\t# ", !IO),
        put(Stream, Comment, !IO)
    ;   
        true
    ),
    put(Stream, "\n", !IO).

%-----------------------------------------------------------------------------%
%
% Conversion x86_64 operands to strings
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
        BaseAddress = string.int_to_string(Offset) ++
            "(" ++ reg_type(Reg) ++ ")"
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
% Auxiliary predicates for outputting x86_64 instructions
%

    % Output an instruction with either one or two operand(s).
    % If the second operand is present, it must not be immediate operand. 
    %
:- pred output_instr_not_imm_dest(Stream::in, string::in, operand::in,
    maybe(operand)::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

output_instr_not_imm_dest(Stream, Instr, Op1, Op2, !IO) :-
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
                put(Stream, "\t" ++ Instr ++ "\t", !IO),
                put(Stream, Op1Type ++ ", " ++ Op2Type ++ "\t", !IO)
            ;
                Result2 = no,
                put(Stream, "\tmov\t" ++ Op2Type ++ ", %r13\t", !IO),
                put(Stream, "# move immediate to temp reg\n", !IO),
                put(Stream, "\t" ++ Instr ++ "\t", !IO),
                put(Stream, Op1Type ++ ", " ++ "%r13\t", !IO)
            )
        ;
            Result1 = no,
            unexpected(this_file, "output_instr_not_imm_dest: unexpected:"
                ++ " invalid operands - two memory references are not allowed")
        )
    ;
        Op2 = no,
        put(Stream, Op1Type ++ "\t\t", !IO)
    ).

    % Output an instruction with a signed 8-bit offset relative to the 
    % instruction pointer as an operand. 
    %
:- pred output_instr_8bit_rel_offset(Stream::in, string::in, operand::in, 
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

output_instr_8bit_rel_offset(Stream, InstrName, RelOffset, !IO) :-
   check_operand_rel_offset(RelOffset, Result1),
   ( 
        Result1 = yes,
        operand_type(RelOffset, RelOffsetType),
        ( string.to_int(RelOffsetType, Val) ->
            check_signed_int_size(8, Val, Result2),
            (
                Result2 = yes,
                put(Stream, "\t" ++ InstrName ++ "\t", !IO),
                put_int(Stream, Val, !IO),
                put(Stream, "\t\t", !IO)
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

:- pred output_bit_test_instr(Stream::in, string::in, operand::in,
    operand::in, io::di, io::uo) is det <= stream.writer(Stream, string, io).

output_bit_test_instr(Stream, Instr, Src, Idx, !IO) :-
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
                    put(Stream, "\t" ++ Instr ++ "\t", !IO),
                    put(Stream, Op1 ++ ", " ++ Op2 ++ "\t", !IO)
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

:- pred output_instr_with_condition(Stream::in, string::in, operand::in,
    maybe(operand)::in, condition::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

output_instr_with_condition(Stream, Instr, Op1, Op2, Cond, !IO) :-
    check_operand_not_immediate(Op1, Result1),
    (
        Result1 = yes,
        instr_condition(Cond, CondRes),
        put(Stream, "\t" ++ Instr, !IO),
        put(Stream, CondRes ++ "\t", !IO),
        operand_type(Op1, Op1Type),
        put(Stream, Op1Type, !IO),
        (
            Op2 = yes(Op2Res),
            check_operand_register(Op2Res, Result3),
            (
                Result3 = yes,
                operand_type(Op2Res, Op2Type),
                put(Stream, ", " ++ Op2Type, !IO)
            ;
                Result3 = no,
                    unexpected(this_file, "output_instr_with_condition:"
                        ++ " invalid second operand")
            )
       ;
            Op2 = no,
            put(Stream, "\t\t", !IO)
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

%-----------------------------------------------------------------------------%
:- end_module x86_64_out.
%-----------------------------------------------------------------------------%
