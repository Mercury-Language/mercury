%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: x86_64_instrs.m.
% Main author: fhandoko.
%
% This module contains the representations of the x86_64 instructions. 
%
%-----------------------------------------------------------------------------%

:- module ll_backend.x86_64_instrs.
:- interface.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type x86_64_instruction
    --->    x86_64_instr(
                x86_64_instr_name   :: x86_64_op,
                x86_64_comment      :: string
            ).

:- type label_name == string.

:- type x86_64_instr
    --->    comment(string)
    ;       label(label_name)
    ;       directives(list(pseudo_op))
    ;       instrs(list(x86_64_instruction)).

%-----------------------------------------------------------------------------%

    % Signed integers of various sizes.
    %
:- type int8 ---> int8(int).        % In bottom 8 bits.
:- type int16 ---> int16(int).      % In bottom 16 bits.
:- type int32 ---> int32(int).      % in bottom 32 bits.

    % Unsigned integers of various sizes.
    %
:- type uint8 ---> uint8(int).      % In bottom 8 bits.
:- type uint16 ---> uint16(int).    % In bottom 16 bits.
:- type uint32 ---> uint32(int).    % In bottom 32 bits.


%-----------------------------------------------------------------------------%
%
% x86_64 pseudo-ops.
%

    % GNU assembler pseudo-ops for the x86_64. 
    % Details on GNU assembler 'as' documentation in the section entitled 
    % "Pseudo Ops".
    %
:- type pseudo_op 
    --->    abort
            % stop the assembly immediately.

    ;       align(
                align_bits          :: int,
                align_fill_value    :: maybe(int),
                align_skip_bytes    :: maybe(int)
            )
            % Advance the location counter until it is a multiple of 
            % 'align_bits'. The second and third arguments are optional.
            % 'align_fill_value': value to be stored in the padding bytes. 
            % 'align_skip_bytes': maximum number of bytes to be skipped.
    
    ;       ascii(
                ascii_literals     :: list(string)
            )
            % 'ascii_literals' contain zero or more string literals. Assembles
            % each literal into consecutive addresses.

    ;       asciiz(
                asciiz_literals     :: list(string)
            )
            % Simiar to '.ascii', but each string is followed by a zero byte.

    ;       balign(
                balign_bits         :: int,
                balign_fill_value   :: maybe(int),
                balign_skip_bytes   :: maybe(int)
            )
            % Similar to 'align'.
    
    ;       byte(
                byte_exprs          :: list(string) 
            )
            % 'byte_exprs' contains zero or more expressions. Each expression is
            % assembled into the next byte.

    ;       comm(
                comm_symbol         :: string,
                comm_length         :: int,
                comm_align          :: maybe(int)
            )
            % Declare a common symbol 'comm_symbol'. Allocate 'comm_length' 
            % bytes if the symbol has not been defined. 
            % (optional) 'comm_align' is the desired alignment of the symbol.
    
    ;       data(
                data_subsection     :: maybe(int)
            )
            % Tells assembler to assemble the following statements onto the end
            % of the data subsection numbered 'data_subsection'.

    ;       desc(
                desc_symbol         :: string,
                desc_abs_expr       :: string
            )
            % Set the descriptor of the symbol to the low 16 bits of 
            % 'desc_abs_expr'.

    ;       def(
                def_name            :: string
            )
            % Begin defining debugging information for a symbol 'def_name'. 

    ;       dim
            % Tell to include auxiliary debugging information in the symbol 
            % table.

    ;       double(
                double_nums         :: list(float)
            )
            % 'double_nums' contains zero or more floating point numbers.
    
    ;       eject
            % Force a page break.

    ;       else_
            % As in 'if-then-else' conditional expression. 

    ;       elseif
            % Shorthand for the beginning a new '.if' block.

    ;       end
            % Mark the end of the assembly file.

    ;       endef
            % End of a symbol definition begun with '.def'.

    ;       endfunc
            % End of a function specified with '.func'.

    ;       endif
            % End of a block of conditional code.

    ;       endm
            % End of assembly macro. 

    ;       equ(
                equ_symbol          :: string,
                equ_expr            :: string
            )
            % Set the value of 'equ_symbol' to 'equ_expr'.

    ;       equiv(
                equiv_symbol        :: string,
                equiv_expr          :: string
            )
            % Like '.equ' except that the assembler will signal an error
            % if 'equiv_symbol' is already defined.

    ;       err
            % Print an error message.

    ;       exitm
            % Exit early from the current macro definition.
            
    ;       extern
            % Ignored. '.extern' is accepted for compatibility only. 

    ;       fail_(
                fail_expr           :: string
            )
            % Generate an error or a warning. If 'fail_expr' is 500 or more, it
            % prints a warning message. Otherwise, it prints an error message.
    
    ;       file(
                file_name           :: string
            )
            % Start a new logical file. 

    ;       fill(
                fill_repeat         :: int,
                fill_size           :: maybe(int),
                fill_value          :: maybe(int)
            )
            % Emits 'fill_repeat' copies of 'fill_size' byte. The contents of 
            % each 'fil_repeat' bytes is taken from an 8-byte number. The 
            % highest order 4 bytes are zero. The lowest order 4 bytes are 
            % 'fill_value'. The last 2 arguments are optional.

    ;       float(
                float_nums          :: list(float)
            )
            % 'float_nums' contains zero or more floating point numbers. 

    ;       func_(
                func_name           :: string,
                func_label          :: string
            )
            % Emits debugging information to denote function 'func_name' and
            % is ignored unless the file is assembled with debugging enabled.
            % 'func_label' is the entry point of the function. 

    ;       global(
                global_symbol       :: string
            )
            % makes the global_symbol' visible to other programs that are linked
            % with it.

    ;       globl(
                globl_symbol        :: string
            )
            % makes the globl_symbol' visible to other programs that are linked

    ;       hidden(
                hidden_name         :: string
            )
            % Override the named symbol default visibility (which is set by
            % their binding: local, global or weak).

    ;       hword(
                hword_exprs         :: list(string)
            )
            % 'hword_exprs' contains zero or more expressions and emit a 16-bit 
            % number for each. Synonym for '.short'.

    ;       ident
            % To place tags in object files.

    ;       if_(
                if_expr             :: string
            )
            % Mark the beginning of a conditional section.

    ;       ifdef(
                ifdef_symbol        :: string
            )
            % Assemble the following section of code if 'ifdef_symbol' has been
            % defined.

    ;       ifc(
                ifc_string1         :: string,
                ifc_string2         :: string
            )
            % Assemble the following section of code if the two strings are the
            % same. 

    ;       ifeq(
                ifeq_expr           :: string
            )
            % Assemble the following section of code if the argument is zero.

    ;       ifge(
                ifge_expr           :: string
            )
            % Assemble the following section of code if the argument is greater
            % than equal to zero.

    ;       ifgt(
                ifgt_expr           :: string
            )
            % Assemble the following section of code if the argument is greater
            % than zero.

    ;       ifle(
                ifle_expr           :: string
            )
            % Assemble the following section of code if the argument is less
            % than equal to zero.

    ;       iflt(
                iflt_expr           :: string
            )
            % Assemble the following section of code if the argument is less
            % than zero.

    ;       ifnc(
                ifnc_string1        :: string,
                ifnc_string2        :: string
            )
            % Assemble the following section of code if the two strings are not
            % the same. 

    ;       ifndef(
                ifndef_symbol       :: string
            )
            % Assemble the following section of code if 'ifndef_symbol' has not
            % been defined. 

    ;       ifnotdef(
                ifnotdef_symbol     :: string
            )
            % Synonym for ifndef.

    ;       ifne(
                ifne_expr           :: string
            )
            % Assemble the following section of code if the argument is not 
            % equal to zero.

    ;       ifnes(
                ifnes_string1       :: string,
                ifnes_string2       :: string
            )
            % Assemble the following section of code if the two strings are not
            % the same.

    ;       include(
                include_file        :: string
            )
            % Include supporting 'include_file'.

    ;       int(
                int_exprs           :: list(string)
            )
            % 'int_exprs' contains zero or more expressions. For each 
            % expression, emit a number that, at run time, is the value of that
            % expression. 

    ;       internal(
                internal_name       :: string
            )
            % Like '.hidden' pseudo-op.

    ;       lcomm(
                lcomm_symbol        :: string,
                lcomm_length        :: int
            )
            % Reserve 'lcomm_length' bytes for a local common denoted by 
            % 'lcomm_symbol'.

    ;       line(
                line_number         :: int
            )
            % Change the logical line number.

    ;       list
            % Control whether or not assembly listings are generated. This 
            % increments the internal counter.

    ;       long(
                long_exprs          :: list(string)
            )
            % Same as '.int'.

    ;       macro
            % Define macro to generate assembly output. 

    ;       nolist
            % Control whether or not assembly listings are generated. It 
            % decrements an internal counter. 

    ;       p2align(
                p2align_pow_bit     :: int,
                p2align_fill_value  :: maybe(int),
                p2align_skip_bytes  :: maybe(int)
            )
            % Advances the location counter until it is a multiple of 
            % 2 ^ ('p2align_pow_bit')
            % p2align_fill_value: value to be stored in padding bytes. 
            % 'p2align_skip_bytes: maximum bytes to be skipped. 

    ;       popsection
            % Replace the current section (and subsection) with the top section 
            % (and subsection) on the section stack. 

    ;       previous
            % Swap the current section (and subsection) with most recently 
            % referenced secction (and subsection) prior to this one. 

    ;       print(string)
            % print string on the standard output.

    ;       protected(
                protected_name      :: string
            )
            % Like '.hidden' or '.internal'

    ;       psize(
                psize_lines       :: int,
                psize_cols        :: maybe(int)
            )
            % Declare the number of lines specified by 'psize_lines' and
            % an optional number of columns specified by 'psize_cols'.

    ;       purgem(
                purgem_name         :: string
            )
            % Undefine the macro 'purgem_name' so that later uses of the string
            % will not be expanded.

    ;       pushsection(
                pushsection_name    :: string,
                pushsection_subsect :: int
           )
            % Push the current section (and subsection) onto the top of the 
            % section stack and then replace the current section and subsection
            % with 'pushsection_name' and 'pushsection_subsect'.

    ;       quad(
                quad_bignums        :: list(string)
            )
            % 'quad_bignums' contains zero or more bignums. For each bignum, it 
            % emits an 8-byte-integer. 

    ;       rept(
                rept_count          :: int
            )
            % Repeat the sequence of lines between the '.rept' directive and the
            % next '.endr' directive 'rept_count' times.

    ;       sbttl(
                sbttl_subheading   :: string
            )
            % Use 'subttl_subheading' as the title (immediately after the title 
            % line).

    ;       scl(
                scl_class           :: string
            )
            % Set the storage-class value for a symbol.

    ;       section(
                section_name        :: string,
                section_flags       :: maybe(string),
                section_type        :: maybe(string),
                section_entsize     :: maybe(int)
            )
            % ELF section stack manipulation directive. 

    ;       set(
                set_symbol          :: string,
                set_expression      :: string
            )
            % Set the value of 'set_symbol' to 'set_expression'

    ;       short(
                short_exprs         :: list(string)
            )

    ;       single(
                single_nums         :: list(float)
            )
            % Has the same effect as '.float'.

    ;       size(
                size_name           :: string,
                size_exprs          :: string
            )
            % Set the size associated with symbol'size_name'. The size in bytes
            % is computed from 'size_expr'.

    ;       skip(
                skip_size           :: int,
                skip_value          :: maybe(int)
            )
            % Emit 'skip_size' bytes, each of value 'skip_value' (optional).
    
    ;       sleb128(
                sleb128_exprs       :: list(string)
            )
            % Stand for "signed little endian base 128". It is a variable length
            % representation of numbers used by the DWARF symbolic.

    ;       space(
                space_size          :: int,
                space_fill          :: maybe(int)
            )
            % Emit 'space_size' bytes, each of value 'space_fill' (optional).
            
    ;       string(
                string_str          :: list(string)
            )
            % Copy the characters in each string inside 'string_strs' to the 
            % object file. 

    ;       struct(
                struct_expr         :: string
            )
            % Switch to the absolute section and set the section offset to
            % 'struct_expr'.

    ;       subsection(
                subsection_name     :: string
            )
            % Replace the current subsection with 'subsection_name'.

    ;       symver(
                symver_name         :: string,
                symver_alias        :: string
            )
            % If 'symver_name' is defined within the file being assembled, it 
            % creates a symbol alias with the name 'symver_alias'

    ;       tag(
                tag_struct_name     :: string
            )
            % Include auxiliary debugging information in the symbol table.

    ;       text(
                text_subsection     :: maybe(int)
            )
            % Assemble the following statements onto the end of the text 
            % subsection numbered zero or 'text_subsection' (if present).

    ;       title(
                title_heading       :: string
            )
            % Use 'title_heading' as the title when generating assembly listing.

    ;       type_(
                type_name           :: string,
                type_desc           :: string
            )
            % Set the type of symbol'type_name' to be either a function or an
            % object symbol.

    ;       uleb128(
                uleb128_exprs       :: list(string)
            )
            % Stands for "unsigned little endian base 128". It is a variable 
            % length representation of numbers used by the DWARF symbolic
            % debugging format.

    ;       val(
                val_addr            :: string
            )
            % Record 'val_addr' as the value attribute of a symbol table entry.

    ;       version(
                version_note        :: string
            )
            % Create a '.note' section and places into it an ELF formatted note 
            % of type NT_VERSION>

    ;       weak(
                weak_names          :: list(string)
            )
            % Set the weak attribute on the list of symbol in 'weak_names'. If 
            % the symbol has not existed, it will be created. 

    ;       word(
                word_exprs          :: list(string)
            ).
            % 'word_exprs' contains zero or more expressions. 

%-----------------------------------------------------------------------------%
%
% x86_64 registers.
%

    % 64-bit RFLAGS register holding various status flags and one control flag.
    % Details on amd64-prog-man-vol1 manual p38.
    %
:- type flags_reg
    --->    rflags(int).

    % General purpose registers on the x86_64.
    % Details on amd64-prog-man-vol1 manual p27.
    %
:- type gp_reg
    --->    rax
    ;       rbx
    ;       rcx
    ;       rdx
    ;       rbp
    ;       rsi
    ;       rdi
    ;       rsp
    ;       r8
    ;       r9
    ;       r10
    ;       r11
    ;       r12
    ;       r13
    ;       r14
    ;       r15.

    % 64-bit instruction pointer register on the x86_64. Instruction pointer
    % RIP is used as a base register for relative addressing. x86_64
    % instructions using RIP can be:
    %   rip_constant(%rip)
    %   rip_expr(%rip)
    % Details on GNU assembler 'as' documentation in the section entitled 
    % "Machine Dependencies: i386-Dependent: i386-Memory".
    %
:- type instr_ptr
    --->    rip_constant(
                rip_byte_offset  :: int32
            )

    ;       rip_expr(
                rip_expr         :: string
            ).

%-----------------------------------------------------------------------------%
%
% Operands for x86_64 instructions.
%

    % Immediate operands for the x86_64 instruction.
    %   
:- type imm_operand
    --->    imm8(int8)    
    ;       imm16(int16)
    ;       imm32(int32).
    
    % Memory reference operands for the x86_64 instruction can be an absolute
    % address (given as a displacement from the base address of a data segment)
    % or an instruction-relative address. 
    % Details on amd64-prog-man-vol1 p19.  
    %
:- type mem_ref
    --->    mem_abs(
                mem_abs_address         :: base_address
            )

    ;       mem_rip(
                instr_rel_address       :: instr_ptr
            ).

    % Base address can be relative to the content of a general-purpose register 
    % or relative value of an expression (such as arithmetic expression 
    % containing labels in data segment).
    %
:- type base_address
    --->    base_reg(
                base_offset             :: int,
                base_reg                :: gp_reg
            )

    ;       base_expr(
                base_expr               :: string
            ).
    
    % All operands for the x86_64 instructions. 
    %
:- type operand
    --->    operand_reg(gp_reg)
    ;       operand_imm(imm_operand)
    ;       operand_mem_ref(mem_ref).


%
% Subtypes of the operand type.
% XXX maybe we should use inst-subtyping for these?
%

    % x86_64_instruction's operand is either the content a general-purpose
    % register or the content of a memory reference. 
    %   
:- type reg_or_mem_ref_op
    --->    rmro_reg(gp_reg)
    ;       rmro_mem_ref(mem_ref).
    
    % x86_64_instruction's operand is either the contents of a general-purpose
    % register or the value of an immediate operand. 
    %   
:- type reg_or_imm_op
    --->    rio_reg(gp_reg)
    ;       rio_imm(imm_operand).

    % x86_64_instruction's operand is either the content of CL register (the 
    % bottom 16 bits of RCX register) or an 8-bit immediate value. 
    %   
:- type cl_reg_or_imm_op
    --->    crio_reg_cl(gp_reg)
    ;       crio_imm8(int8).

    % x86_64 instruction's operand is an offset relative to the instruction 
    % pointer. 
    %
:- type rel_offset
    --->    ro8(int8)       % Signed 8-bit  offset (in bottom 8 bits).
    ;       ro16(int16)     % Signed 16-bit offset (in bottom 16 bits).
    ;       ro32(int32).    % Signed 32-bit offset (in bottom 32 bits).

    % x86_64 instruction's operand can be a register, memory reference, 
    % signed relative offset or a label. 
    %
:- type rmrol
    --->    rmrol_reg(gp_reg)
    ;       rmrol_mem_ref(mem_ref)
    ;       rmrol_rel_offset(rel_offset)
    ;       rmrol_label(
                rmrol_label_name    :: string
            ).

%-----------------------------------------------------------------------------%
%
% x86_64 instructions.
%

    % We use At&T style instructions where the source comes before the
    % destination.
    % 
:- type x86_64_op
    --->    adc(
                adc_src     :: operand,
                adc_dst     :: reg_or_mem_ref_op 
            )
            % Add with carry. Add 'adc_src' to 'adc_dst' + carry flag (CF).
            % Details on amd64-prog-man-vol3 manual p65.

    ;       add(
                add_src     :: operand,
                add_dst     :: reg_or_mem_ref_op 
            )
            % Signed or unsigned add. Add 'adc_src' to 'adc_dst'.
            % Details on amd64-prog-man-vol3 manual p67.

    ;       and(
                and_src     :: operand,
                and_dst     :: reg_or_mem_ref_op 
            )
            % Performs a bitwise AND operation on both operands.
            % Details on amd64-prog-man-vol3 manual p69.

    ;       bsf(
                bsf_src     :: reg_or_mem_ref_op,
                bsf_dst     :: gp_reg
            )
            % Bit scan forward. Searches the value in 'bsf_src' for the least
            % significant set bit. 
            % Details on amd64-prog-man-vol3 manual p74.

    ;        bsr(
                bsr_src     :: reg_or_mem_ref_op,
                bsr_dst     :: gp_reg
            )
            % Bit scan reverse. Searches the value in 'bsf_src' for the most
            % significant set bit. 
            % Details on amd64-prog-man-vol3 manual p75.

    ;       bswap(gp_reg)
            % Byte swap. Reverses the byte order of the specified 'gp_reg'.
            % Details on amd64-prog-man-vol3 manual p76.

    ;       bt(
                bt_src      :: reg_or_mem_ref_op,
                bt_idx      :: reg_or_imm_op
            )
            % Bit test. Copies a bit specified by 'bt_idx' from a bit string in
            % 'bt_src' to the carry flag (CF) or RFLAGS register.
            % Details on amd64-prog-man-vol3 manual p77.

    ;       btc(
                btc_src     :: reg_or_mem_ref_op,
                btc_idx     :: reg_or_imm_op
            )
            % Bit test and complement. Complements 'btc_src' after performing 
            % 'bt' operation.
            % Details on amd64-prog-man-vol3 manual p79.

    ;       btr(
                btr_src     :: reg_or_mem_ref_op,
                btr_idx     :: reg_or_imm_op
            )
            % Bit test and reverse. Clears 'btr_src' to 0 after performing 'bt'
            % operation.
            % Details on amd64-prog-man-vol3 manual p81.

    ;       bts(
                bts_src     :: reg_or_mem_ref_op,
                bts_idx     :: reg_or_imm_op
            )
            % Bit test and set. Sets 'bts_src' to 1 after performing 'bt'
            % operation.
            % Details on amd64-prog-man-vol3 manual p83.

    ;       call(rmrol)
            % Call with target specified by 'rmrol': register, memory location,
            % relative offset or a label.
            % Details on amd64-prog-man-vol3 manual p85.

    ;       cbw
            % Sign-extend AL into AX.
            % Details on amd64-prog-man-vol3 manual p94.

    ;       cwde
            % Sign-extend AX into EAX.
            % Details on amd64-prog-man-vol3 manual p94.

    ;       cdqe
            % Sign-extend EAX into RAX.
            % Details on amd64-prog-man-vol3 manual p94.

    ;       cwd
            % Sign-extend AX into DX:AX.
            % Details on amd64-prog-man-vol3 manual p95.

    ;       cdq
            % Sign-extend EAX into EDX:EAX.
            % Details on amd64-prog-man-vol3 manual p95.

    ;       cqo
            % Sign-extend RAX into RDX:RAX.
            % Details on amd64-prog-man-vol3 manual p95.

    ;       clc
            % Clears the carry flag (CF) in the rFLAGS register to zero.
            % Details on amd64-prog-man-vol3 manual p96.

    ;       cld
            % Clears the direction flag (DF) in the rFLAGS register to zero.
            % Details on amd64-prog-man-vol3 manual p97.

    ;       cmc
            % Complements the carry flag bit (CF) bit in the rFLAGS register.
            % Details on amd64-prog-man-vol3 manual p100.

    ;       cmovo(
                cmovo_src       :: reg_or_mem_ref_op,
                cmovo_dest      :: gp_reg
            )
            % Moves if overflow (OF = 1).
            % Details on amd64-prog-man-vol3 manual p101.

    ;       cmovno(
                cmovno_src      :: reg_or_mem_ref_op,
                cmovno_dest     :: gp_reg
            )
            % Moves if not overflow (OF = 0).
            % Details on amd64-prog-man-vol3 manual p101.

    ;       cmovb(
                cmovb_src       :: reg_or_mem_ref_op,
                cmovb_dest      :: gp_reg
            )
            % Moves if below (CF = 1).
            % Details on amd64-prog-man-vol3 manual p101.

    ;       cmovc(
                cmovc_src       :: reg_or_mem_ref_op,
                cmovc_dest      :: gp_reg
            )
            % Moves if carry (CF = 1).
            % Details on amd64-prog-man-vol3 manual p101.

    ;       cmovnae(
                cmovnae_src     :: reg_or_mem_ref_op,
                cmovnae_dest    :: gp_reg
            )
            % Moves if not above or equal (CF = 1).
            % Details on amd64-prog-man-vol3 manual p101.

    ;       cmovnb(
                cmovnb_src      :: reg_or_mem_ref_op,
                cmovnb_dest     :: gp_reg
            )
            % Moves if not below (CF = 0).
            % Details on amd64-prog-man-vol3 manual p101.

    ;       cmovnc(
                cmovnc_src      :: reg_or_mem_ref_op,
                cmovnc_dest     :: gp_reg
            )
            % Moves if not carry (CF = 0).
            % Details on amd64-prog-man-vol3 manual p102.

     ;      cmovae(
                cmovae_src      :: reg_or_mem_ref_op,
                cmovae_dest     :: gp_reg
            )
            % Moves if above or equal (CF = 0).
            % Details on amd64-prog-man-vol3 manual p102.

     ;      cmovz(
                cmovz_src       :: reg_or_mem_ref_op,
                cmovz_dest      :: gp_reg
            )
            % Moves if zero (ZF = 1).
            % Details on amd64-prog-man-vol3 manual p102.

     ;      cmove(
                cmove_src       :: reg_or_mem_ref_op,
                cmove_dest      :: gp_reg
            )
            % Moves if equal (ZF = 1).
            % Details on amd64-prog-man-vol3 manual p102.

     ;      cmovnz(
                cmovnz_src      :: reg_or_mem_ref_op,
                cmovnz_dest     :: gp_reg
            )
            % Moves if not zero (ZF = 0).
            % Details on amd64-prog-man-vol3 manual p102.

    ;      cmovne(
                cmovne_src      :: reg_or_mem_ref_op,
                cmovne_dest     :: gp_reg
            )
            % Moves if not equal (ZF = 0).
            % Details on amd64-prog-man-vol3 manual p102.

    ;      cmovbe(
                cmovbe_src      :: reg_or_mem_ref_op,
                cmovbe_dest     :: gp_reg
            )
            % Moves if below or equal (CF = 1 or ZF = 1).
            % Details on amd64-prog-man-vol3 manual p102.

    ;       cmovna(
                cmovna_src      :: reg_or_mem_ref_op,
                cmovna_dest     :: gp_reg
            )
            % Moves if not above (CF = 1 or ZF = 1).
            % Details on amd64-prog-man-vol3 manual p102.

    ;       cmovnbe(
                cmovnbe_src     :: reg_or_mem_ref_op,
                cmovnbe_dest    :: gp_reg
            )
            % Moves if not below or equal (CF = 0 or ZF = 0).
            % Details on amd64-prog-man-vol3 manual p102.

    ;       cmova(
                cmova_src       :: reg_or_mem_ref_op,
                cmova_dest      :: gp_reg
            )
            % Moves if above (CF = 1 or ZF = 0).
            % Details on amd64-prog-man-vol3 manual p102.

    ;       cmovs(
                cmovs_src       :: reg_or_mem_ref_op,
                cmovs_dest      :: gp_reg
            )
            % Moves if sign (SF = 1).
            % Details on amd64-prog-man-vol3 manual p102.

    ;       cmovns(
                cmovns_src      :: reg_or_mem_ref_op,
                cmovns_dest     :: gp_reg
            )
            % Moves if not sign (SF = 0).
            % Details on amd64-prog-man-vol3 manual p102.

    ;       cmovp(
                cmovp_src       :: reg_or_mem_ref_op,
                cmovp_dest      :: gp_reg
            )
            % Moves if parity (PF = 1).
            % Details on amd64-prog-man-vol3 manual p102.

    ;       cmovpe(
                cmovpe_src      :: reg_or_mem_ref_op,
                cmovpe_dest     :: gp_reg
            )
            % Moves if parity even (PF = 1).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovnp(
                cmovnp_src      :: reg_or_mem_ref_op,
                cmovnp_dest     :: gp_reg
            )
            % Moves if not parity (PF = 0).
            % Details on amd64-prog-man-vol3 manual p103.
            
    ;       cmovpo(
                cmovpo_src      :: reg_or_mem_ref_op,
                cmovpo_dest     :: gp_reg
            )
            % Moves if parity odd (PF = 0).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovl(
                cmovl_src       :: reg_or_mem_ref_op,
                cmovl_dest      :: gp_reg
            )
            % Moves if less (SF <> OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovnge(
                cmovnge_src     :: reg_or_mem_ref_op,
                cmovnge_dest    :: gp_reg
            )
            % Moves if not greater or equal (SF <> OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovnl(
                cmovnl_src      :: reg_or_mem_ref_op,
                cmovnl_dest     :: gp_reg
            )
            % Moves if not less (SF = OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovge(
                cmovge_src      :: reg_or_mem_ref_op,
                cmovge_dest     :: gp_reg
            )
            % Moves if greater or equal (SF = OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovle(
                cmovle_src      :: reg_or_mem_ref_op,
                cmovle_dest     :: gp_reg
            )
            % Moves if less or equal (ZF = 1 or SF <> OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovng(
                cmovng_src      :: reg_or_mem_ref_op,
                cmovng_dest     :: gp_reg
            )
            % Moves if not greater (ZF = 1 or SF <> OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovnle(
                cmovnle_src     :: reg_or_mem_ref_op,
                cmovnle_dest    :: gp_reg
            )
            % Moves if not less or equal (ZF = 0 or SF = OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmovg(
                cmovg_src       :: reg_or_mem_ref_op,
                cmovg_dest      :: gp_reg
            )
            % Moves if greater (ZF = 0 or SF = OF).
            % Details on amd64-prog-man-vol3 manual p103.

    ;       cmp(
                cmp_src         :: operand,
                cmp_dest        :: reg_or_mem_ref_op 
            )
            % Compares 'cmp_src' with 'cmp_dest'. 
            % Details on amd64-prog-man-vol3 manual p105.

    ;       cmpxchg(
                cmpxchg_cmp     :: reg_or_mem_ref_op,
                cmpxchg_xchg    :: gp_reg
            )
            % Compares the value in RAX with the value in 'cmpxchg_cmp'.
            % If equal, copies the value in 'cmpxchg_xchg' to 'cmpxchg_cmp'.
            % Details on amd64-prog-man-vol3 manual p111.

    ;       cmpxchg8b(mem_ref)
            % Compares the value in EDX:EAX with the value in 'mem_ref'.
            % Details on amd64-prog-man-vol3 manual p113.

    ;       dec(reg_or_mem_ref_op)
            % Decrements the contents of 'reg_or_mem_ref_op'.
            % Details on amd64-prog-man-vol3 manual p120.

    ;       div(reg_or_mem_ref_op)
            % Unsigned divide RDX:RAX by the value in 'reg_or_mem_ref_op'. 
            % Details on amd64-prog-man-vol3 manual p122.

    ;       enter(
                enter_stack_size    :: uint16,
                enter_nesting_level :: uint8
            )
            % Creates a procedure stack frame with a stack size specified in 
            % 'enter_stack_size' and nesting level specified in 
            % 'enter_nesting_level'(0 to 31).
            % Details on amd64-prog-man-vol3 manual p124.

    ;       idiv(reg_or_mem_ref_op)
            % Signed divide RDX:RAX by the value in 'reg_or_mem_ref_op'. 
            % Details on amd64-prog-man-vol3 manual p126.

    ;       imul(reg_or_mem_ref_op)
            % Signed multiply RAX by the value in 'reg_or_mem_ref_op'. 
            % Details on amd64-prog-man-vol3 manual p128.

    ;       imul(
                imul1_src           :: reg_or_mem_ref_op,
                imul1_dest          :: gp_reg
            )
            % Signed multiply 'imul1_src' by 'imul1_dest'. 
            % Details on amd64-prog-man-vol3 manual p129.

    ;       imul(
                imul2_src           :: reg_or_mem_ref_op,
                imul2_multiplicand  :: imm_operand, 
                imul2_dest          :: gp_reg
            )
            % Signed multiply 'imul2_src' by 'imul2_multiplicand'.
            % Details on amd64-prog-man-vol3 manual p129.

    ;       inc(reg_or_mem_ref_op)
            % Increments the contents of 'reg_or_mem_ref_op'.
            % Details on amd64-prog-man-vol3 manual p133.

    ;       jo(rel_offset)
            % Jumps if overflow (OF = 1).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jno(rel_offset)
            % Jumps if not overflow (OF = 0).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jb(rel_offset)
            % Jumps if below (CF = 1).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jc(rel_offset)
            % Jumps if carry (CF = 1).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jnae(rel_offset)
            % Jumps if not above or equal (CF = 1).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jnb(rel_offset)
            % Jumps if not below (CF = 0).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jnc(rel_offset)
            % Jumps if not carry (CF = 0).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jae(rel_offset)
            % Jumps if above or equal (CF = 0).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jz(rel_offset)
            % Jumps if zero (ZF = 1).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       je(rel_offset)
            % Jumps if equal (ZF = 1).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jnz(rel_offset)
            % Jumps if not zero (ZF = 0).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jne(rel_offset)
            % Jumps if not equal (ZF = 0).
            % Details on amd64-prog-man-vol3 manual p147.

    ;       jbe(rel_offset)
            % Jumps if below or equal (CF = 1 or ZF = 1).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jna(rel_offset)
            % Jumps if not above (CF = 1 or ZF = 1).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jnbe(rel_offset)
            % Jumps if not below or equal (CF = 0 or ZF = 0).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       ja(rel_offset)
            % Jumps if above (CF = 0 or ZF = 0).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       js(rel_offset)
            % Jumps if sign (SF = 1).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jns(rel_offset)
            % Jumps if not sign (SF = 0).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jp(rel_offset)
            % Jumps if parity (PF = 1).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jpe(rel_offset)
            % Jumps if parity even (PF = 1).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jnp(rel_offset)
            % Jumps if not parity (PF = 0).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jpo(rel_offset)
            % Jumps if parity odd (PF = 0).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jl(rel_offset)
            % Jumps if less (SF <> OF).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jnge(rel_offset)
            % Jumps if not greater or equal (SF <> OF).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jnl(rel_offset)
            % Jumps if not less (SF = OF).
            % Details on amd64-prog-man-vol3 manual p148.

    ;       jge(rel_offset)
            % Jumps if greater or equal (SF = OF).
            % Details on amd64-prog-man-vol3 manual p149.

    ;       jle(rel_offset)
            % Jumps if less or equal (ZF = 1 or SF <> OF).
            % Details on amd64-prog-man-vol3 manual p149.

    ;       jng(rel_offset)
            % Jumps if not greater (ZF = 1 or SF <> OF).
            % Details on amd64-prog-man-vol3 manual p149.

    ;       jnle(rel_offset)
            % Jumps if not less or equal (ZF = 0 and SF = OF).
            % Details on amd64-prog-man-vol3 manual p149.

    ;       jg(rel_offset)
            % Jumps if greater (ZF = 0 and SF = OF).
            % Details on amd64-prog-man-vol3 manual p149.

    ;       jrcxz(rel_offset)
            % Jumps to the target instruction located at the specified 8-bit
            % relative offset if RCX is zero. 
            % Details on amd64-prog-man-vol3 manual p150.

    ;       jmp(rmrol)
            % Jumps with target specified in 'rmrol': register, memory location,
            % relative offset or label.
            % Details on amd64-prog-man-vol3 manual p152.

    ;       lea(
                lea_src         :: mem_ref,
                lea_dest        :: gp_reg
            )
            % Stores effective address 'lea_src' into 'lea_dest'.
            % Details on amd64-prog-man-vol3 manual p163.

    ;       leave
            % Sets RSP to the value in RBP and pop RBP. 
            % Details on amd64-prog-man-vol3 manual p164.

    ;       loop(
                loop_rel_8bit   :: rel_offset
            )
            % Decrements RCX then jump if RCX is not zero
            % Details on amd64-prog-man-vol3 manual p169.

    ;       loope(
                loope_rel_8bit  :: rel_offset
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 1.
            % Details on amd64-prog-man-vol3 manual p169.

    ;       loopne(
                loopne_rel_8bit :: rel_offset
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 0.
            % Details on amd64-prog-man-vol3 manual p169.

    ;       loopnz(
                loopnz_rel_8bit :: rel_offset
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 0.
            % Details on amd64-prog-man-vol3 manual p170.

    ;       loopz(
                loopz_rel_8bit  :: rel_offset
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 1.
            % Details on amd64-prog-man-vol3 manual p170.

    ;       mov(
                mov_src          :: operand,
                mov_dest         :: reg_or_mem_ref_op
            )
            % Copies 'mov_src' to 'mov_dest'.
            % Details on amd64-prog-man-vol3 manual p173.

    ;       mul(reg_or_mem_ref_op)
            % Unsigned multiply 'reg_or_mem_ref_op' by RAX.
            % Details on amd64-prog-man-vol3 manual p190.

    ;       neg(reg_or_mem_ref_op)
            % Performs a two's complement negation of 'reg_or_mem_ref_op'.
            % Details on amd64-prog-man-vol3 manual p192.

    ;       nop
            % Increments RIP to point to next instruction.
            % Details on amd64-prog-man-vol3 manual p194.

    ;       not_(reg_or_mem_ref_op)
            % Performs one's complement negation (NOT) of 'reg_or_mem_ref_op'.
            % Details on amd64-prog-man-vol3 manual p195.

    ;       or(
                or_src           :: operand,
                or_dest          :: reg_or_mem_ref_op
            )
            % Performs a logical OR on the bits in 'or_src' and 'or_dest'.
            % Details on amd64-prog-man-vol3 manual p196.

    ;       pop(reg_or_mem_ref_op)
            % Pops the stack into 'reg_or_mem_ref_op'.
            % Details on amd64-prog-man-vol3 manual p204.

    ;       popfq
            % Pops a quadword from the stack to the RFLAGS register. 
            % Details on amd64-prog-man-vol3 manual p208.

    ;       push(operand)
            % Pushes the content of operand onto the stack. 
            % Details on amd64-prog-man-vol3 manual p215.

    ;       pushfq
            % Pushes the RFLAGS quadword onto the stack. 
            % Details on amd64-prog-man-vol3 manual p218.

    ;       ret
            % Near return to the calling procedure. 
            % Details on amd64-prog-man-vol3 manual p224.

    ;       rcl(
                rcl_amount       :: cl_reg_or_imm_op,
                rcl_dest         :: reg_or_mem_ref_op
            )
            % Rotate bits in 'rcl_dest' to the left and through the carry flag
            % by the number of bit positions as specified in 'rcl_amount'. 
            % Details on amd64-prog-man-vol3 manual p220.

    ;       rcr(
                rcr_amount      :: cl_reg_or_imm_op,
                rcr_dest        :: reg_or_mem_ref_op
            )
            % Rotate bits in 'rcr_dest' to the right and through the carry flag 
            % by the number of bit positions as specified in 'rcr_amount'. 
            % Details on amd64-prog-man-vol3 manual p222.

    ;       ret(uint16)
            % Near return to the calling procedure, then pop the specified 
            % number of bytes from stack. 
            % Details on amd64-prog-man-vol3 manual p224.

    ;       rol(
                rol_amount      :: cl_reg_or_imm_op,
                rol_dest        :: reg_or_mem_ref_op
            )
            % Rotates the bits of 'rol_dest' left by 'rol_amount' bits. 
            % Details on amd64-prog-man-vol3 manual p230.

    ;       ror(
                ror_amount      :: cl_reg_or_imm_op,
                ror_dest        :: reg_or_mem_ref_op
            )
            % Rotates the bits of 'ror_dest' right by 'ror_amount bits'. 
            % Details on amd64-prog-man-vol3 manual p232.

    ;       sal(
                sal_amount      :: cl_reg_or_imm_op,
                sal_dest        :: reg_or_mem_ref_op
            )
            % Shift 'sal_dest' left by 'sal_amount'. 
            % Details on amd64-prog-man-vol3 manual p235.

    ;       shl(
                shl_amount      :: cl_reg_or_imm_op,
                shl_dest        :: reg_or_mem_ref_op
            )
            % Shift 'shl_dest' left by 'shl_amount'. 
            % Details on amd64-prog-man-vol3 manual p235.

    ;       sar(
                sar_amount      :: cl_reg_or_imm_op,
                sar_dest        :: reg_or_mem_ref_op
            )
            % Shift 'sar_dest' right by 'sar_amount'. 
            % Details on amd64-prog-man-vol3 manual p238.

    ;       sbb(
                sbb_src         :: operand,
                sbb_dest        :: reg_or_mem_ref_op
            )
            % Subtracts 'sbb_src' from 'sbb_dest' with borrow.
            % Details on amd64-prog-man-vol3 manual p241.

    ;       seto(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If overflow 
            % (OF = 1), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setno(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not overflow 
            % (OF = 0), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setb(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is below
            % (CF = 1), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setc(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is carry 
            % (CF = 1), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setnae(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is not above
            % or equal (CF = 1), set the value in the specified register or an 
            % 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setnb(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is not below
            % (CF = 0), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setnc(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is not carry
            % (CF = 0), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setae(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is above or 
            % equal (CF = 0), set the value in the specified register or an 
            % 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setz(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If zero (ZF = 1),
            % set the value in the specified register or an 8-bit memory 
            % reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       sete(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If equal (ZF = 1),
            % set the value in the specified register or an 8-bit memory 
            % reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setnz(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not zero 
            % (ZF = 0), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setne(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not equal 
            % (ZF = 0), set the value in the specified register or an 8-bit 
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p246.

    ;       setbe(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If below or equal 
            % (CF = 1 or ZF = 1), set the value in the specified register or 
            % an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setna(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not above
            % (CF = 1 or ZF = 1), set the value in the specified register or 
            % an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setnbe(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not below or 
            % equal (CF = 0 and ZF = 0), set the value in the specified
            % register or an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       seta(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If above
            % (CF = 0 and ZF = 0), set the value in the specified
            % register or an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       sets(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is sign,
            % (SF = 1), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setns(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is not sign,
            % (SF = 0), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setp(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If flag is parity
            % (PF = 1), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setpe(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If parity even,
            % (PF = 1), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setnp(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not parity,
            % (PF = 0), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setpo(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If parity odd,
            % (PF = 0), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setl(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If less (SF <> OF),
            % set the value in the specified register or an 8-bit memory 
            % reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setnge(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not greater or 
            % equal (SF <> OF), set the value in the specified register or an 
            % 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setnl(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not less 
            % (SF = OF), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setge(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If greater or equal
            % (SF = OF), set the value in the specified register or an 8-bit
            % memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setle(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If less or equal
            % (ZF = 1 or SF <> OF), set the value in the specified register or
            % an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setng(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not greater
            % (ZF = 1 or SF <> OF), set the value in the specified register or
            % an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setnle(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If not less or equal
            % (ZF = 0 and SF = OF), set the value in the specified register or
            % an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       setg(reg_or_mem_ref_op)
            % Check the status flag in the RFLAGS register. If greater
            % (ZF = 0 and SF = OF), set the value in the specified register or
            % an 8-bit memory reference to 1.
            % Details on amd64-prog-man-vol3 manual p247.

    ;       shld(
                shld_amount         :: cl_reg_or_imm_op,
                shld_dest1          :: reg_or_mem_ref_op,
                shld_dest2          :: gp_reg
            )
            % Shift 'shld_dest1' to the left by 'shld_amount' and shift in a bit
            % pattern in 'shld_dest2' from the right. 
            % Details on amd64-prog-man-vol3 manual p251.

    ;       shr(
                shr_amount          :: cl_reg_or_imm_op,
                shr_dest            :: reg_or_mem_ref_op
            )
            % Shift 'shr_dest' right by 'shr_amount'. 
            % Details on amd64-prog-man-vol3 manual p253.

    ;       shrd(
                shrd_amount         :: cl_reg_or_imm_op,
                shrd_dest1          :: reg_or_mem_ref_op,
                shrd_dest2          :: gp_reg
            )
            % Shift 'shrd_dest1' to the right by 'shrd_amount' and shift in
            % a bit pattern in 'shrd_dest2' from the left. 
            % Details on amd64-prog-man-vol3 manual p255.

    ;       stc
            % Sets the carry flag (CF) in the RFLAGS register to 1. 
            % Details on amd64-prog-man-vol3 manual p257.

    ;       std
            % Sets the direction flag (DF) in the rflags register to 1. 
            % Details on amd64-prog-man-vol3 manual p258.

    ;       sub(
                sub_src             :: operand,
                sub_dest            :: reg_or_mem_ref_op
            )
            % Subtracts 'sub_src' from 'sub_dest'. 
            % Details on amd64-prog-man-vol3 manual p261.

    ;       test(
                test_src1           :: reg_or_mem_ref_op,
                test_src2           :: reg_or_imm_op
            )
            % Performs a bitwise AND on 'test_src1' and 'test_src2'.
            % Details on amd64-prog-man-vol3 manual p264.

    ;       xadd(
                xadd_src            :: gp_reg,
                xadd_dest           :: reg_or_mem_ref_op
            )
            % Exchanges the contents of 'xadd_src' with 'xadd_dest', load 
            % their sum into 'xadd_dest'. 
            % Details on amd64-prog-man-vol3 manual p266.

    ;       xchg(
                xchg_src1           :: reg_or_mem_ref_op,
                xchg_src2           :: reg_or_mem_ref_op
            )
            % Exchanges the contents of 'xchg_src1' and 'xchg_src2'.
            % Details on amd64-prog-man-vol3 manual p268.
        
    ;       xor(
                xor_src :: operand,
                xor_dest :: reg_or_mem_ref_op
            ).
            % Performs a bitwise XOR on 'xor_src' with 'xor_dest' and stores
            % the result in xor_dest.  
            % Details on amd64-prog-man-vol3 manual p272.

%-----------------------------------------------------------------------------%
