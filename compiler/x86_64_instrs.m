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

:- import_module hlds.hlds_pred.
:- import_module hlds.code_model.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.

:- import_module counter.
:- import_module list.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

    % We turn a llds.c_file into this.
    %
:- type x86_64_module
    --->    x86_64_module(
                x86_64_modulename :: module_name,
                                  % The name of this x86_64 module.
                x86_64_procs      :: list(list(x86_64_procedure))
                                  % The code.
            ).

:- func init_x86_64_module(module_name) = x86_64_module.

:- func init_x86_64_proc(c_procedure) =  x86_64_procedure.

:- func init_x86_64_instruction =  x86_64_instruction.

%-----------------------------------------------------------------------------%

    % We turn an llds.c_procedure into one of these.
    % XXX Do we really need to replicate all these fields from the llds?
    %
:- type x86_64_procedure
    --->    x86_64_procedure(
                x86_64_name             :: string,
                                        % Predicate name.
                x86_64_arity            :: int,
                                        % Original arity. 
                x86_64_id               :: pred_proc_id, 
                                        % The pred_proc_id of this code.
                x86_64_code_model       :: code_model,
                                        % The code model of the procedure.
                x86_64_code             :: list(x86_64_instruction),
                                        % The code for this procedure.
                x86_64_proc_label       :: proc_label,
                                        % Proc_label of this procedure.
                x86_64_label_nums       :: counter,
                x86_64_may_alter_rtti   :: may_alter_rtti,
                                        % The compiler is allowed to perform
                                        % optimizations on this procedure
                                        % that could alter RTTI information
                                        % (e.g. the set of variables live at
                                        % a label) only if this field is set
                                        % to `may_alter_rtti'.
                x86_64_c_global_vars    :: set(string)
    ).

%-----------------------------------------------------------------------------%

:- type x86_64_instruction
    --->    x86_64_instr(
                x86_64_inst         :: list(x86_64_instr),
                x86_64_inst_comment :: string
            ).

:- type label_name == string.

:- type x86_64_instr
    --->    x86_64_comment(string)
    ;       x86_64_label(label_name)
    ;       x86_64_directive(pseudo_op)
    ;       x86_64_instr(x86_64_inst).

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

    % Conditional direction.
    %
:- type direction 
    --->    f                       % Forward.
    ;       r.                      % Reverse. 

    % Condition of flags register. 
    %
:- type condition
    --->    o                       % Overflow (OF = 1).
    ;       no                      % Not Overflow (OF = 0).
    ;       b                       % Below (CF = 1).
    ;       c                       % Carry (CF = 1).
    ;       nae                     % Not Above or Equal (CF = 1).
    ;       nb                      % Not Below (CF = 0).
    ;       nc                      % Not Carry (CF = 0).
    ;       ae                      % Above or Equal (CF = 0).
    ;       z                       % Zero (ZF = 1).
    ;       e                       % Equal (ZF = 1).
    ;       nz                      % Not Zero (ZF = 0).
    ;       ne                      % Not Equal (ZF = 0).
    ;       be                      % Below or Equal (CF = 1 or ZF = 1).
    ;       na                      % Not Above (CF = 1 or ZF = 1).
    ;       nbe                     % Not Below or Equal (CF = 0 or ZF = 0).
    ;       a                       % Above (CF = 0 or ZF = 0).
    ;       s                       % Sign (SF = 1).
    ;       ns                      % Not Sign (SF = 0).
    ;       p                       % Parity (PF = 1).
    ;       pe                      % Parity even (PF = 1).
    ;       np                      % Not parity (PF = 0).
    ;       po                      % Parity odd (PF = 0).
    ;       l                       % Less (SF <> OF).
    ;       nge                     % Not Greater or Equal (SF <> OF).
    ;       nl                      % Not Less (SF = OF).
    ;       ge                      % Greater or Equal (SF = OF).
    ;       le                      % Less or Equal (ZF = 1 or SF <> OF).
    ;       ng                      % Not Greater (ZF = 1 or SF <> OF).
    ;       nle                     % Not Less or Equal (ZF = 0 and SF = OF).
    ;       g.                      % Greater (ZF = 0 and SF = OF).

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

    ;       x86_64_pseudo_else
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

    ;       x86_64_pseudo_if(
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

    ;       x86_64_pseudo_type(
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
    ---> gp_reg(int).  

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
:- type x86_64_mem_ref
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
    ;       operand_mem_ref(x86_64_mem_ref)
    ;       operand_rel_offset(rel_offset)
    ;       operand_label(string).

%
% Subtypes of the operand type.
% XXX maybe we should use inst-subtyping for these?
%
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
    ;       rmrol_mem_ref(x86_64_mem_ref)
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
:- type x86_64_inst
    --->    adc(
                adc_src     :: operand,
                            % immediate, register or memory location
                adc_dst     :: operand
                            % register or memory location
            )
            % Add with carry. Add 'adc_src' to 'adc_dst' + carry flag (CF).
            % Cannot add two memory operands. 
            % Details on amd64-prog-man-vol3 manual p65.

    ;       add(
                add_src     :: operand,
                            % immediate, register or memory location
                add_dst     :: operand
                            % register or memory location
            )
            % Signed or unsigned add. Add 'adc_src' to 'adc_dst'.
            % Cannot add two memory operands. 
            % Details on amd64-prog-man-vol3 manual p67.

    ;       and(
                and_src     :: operand,
                            % immediate, register or memory location
                and_dst     :: operand
                            % register or memory location
            )
            % Performs a bitwise AND operation on both operands.
            % Cannot and two memory operands. 
            % Details on amd64-prog-man-vol3 manual p69.

    ;       bs(
                bs_src      :: operand,
                            % register or memory location
                bs_dst      :: operand,
                            % register
                bs_cond     :: direction
                            % "F" for forward, "R" for reverse"
            )
            % Bit scan. Searches the value in 'bs_src' for the least
            % significant set bit  if 'bs_cond' is "F". Searches
            % for the most significant set bit if 'bs_cond' is "R". 
            % Details on amd64-prog-man-vol3 manual p(74-75).

    ;       bswap(
                bswap_reg   :: operand
            )
            % Byte swap. Reverses the byte order of the specified 'operand'.
            % Details on amd64-prog-man-vol3 manual p76.

    ;       bt(
                bt_src      :: operand,
                            % register or memory reference
                bt_idx      :: operand
                            % register or 8-bit immediate value
            )
            % Bit test. Copies a bit specified by 'bt_idx' from a bit string in
            % 'bt_src' to the carry flag (CF) or RFLAGS register.
            % Details on amd64-prog-man-vol3 manual p77.

    ;       btc(
                btc_src     :: operand,
                            % register or memory reference
                btc_idx     :: operand
                            % register or 8-bit immediate value
            )
            % Bit test and complement. Complements 'btc_src' after performing 
            % 'bt' operation.
            % Details on amd64-prog-man-vol3 manual p79.

    ;       btr(
                btr_src     :: operand,
                            % register or memory reference
                btr_idx     :: operand
                            % register or 8-bit immediate value
            )
            % Bit test and reverse. Clears 'btr_src' to 0 after performing 'bt'
            % operation.
            % Details on amd64-prog-man-vol3 manual p81.

    ;       bts(
                bts_src     :: operand,
                            % register or memory reference
                bts_idx     :: operand
                            % register or 8-bit immediate value
            )
            % Bit test and set. Sets 'bts_src' to 1 after performing 'bt'
            % operation.
            % Details on amd64-prog-man-vol3 manual p83.

    ;       call(
                call_target :: operand
                            % label, register, memory reference or rel offset
            )
            % Call with target specified by call_target
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

    ;       cmov(
                cmov_src        :: operand,
                                % memory or register
                cmov_dest       :: operand,
                                % register
                cmov_cmp_op     :: condition
            )
            % Moves with comparison operation defined in 'cmov_cmp_op'.
            % Details on amd64-prog-man-vol3 manual p(101-103).

    ;       cmp(
                cmp_src         :: operand,
                                % register, memory location or immediate value
                cmp_dest        :: operand
                                % register or memory location
            )
            % Compares 'cmp_src' with 'cmp_dest'. 
            % Details on amd64-prog-man-vol3 manual p105.

    ;       cmpxchg(
                cmpxchg_src     :: operand,
                                % register or memory location
                cmpxchg_dest    :: operand
                                % register
            )
            % Compares the value in RAX with the value in 'cmpxchg_dest'.
            % If equal, copies the value in 'cmpxchg_src' to 'cmpxchg_dest'.
            % Details on amd64-prog-man-vol3 manual p111.

    ;       cmpxchg8b(
                cmpxchg8b_mem   :: operand
                                % memory reference
            )
            % Compares the value in EDX:EAX with the value in 'mem_ref'.
            % Details on amd64-prog-man-vol3 manual p113.

    ;       dec(
                dec_op          :: operand
                                % register or memory reference
            )
            % Decrements the contents of 'dec_op'.
            % Details on amd64-prog-man-vol3 manual p120.

    ;       div(
                div_op          :: operand
                                % register or memory reference
            )
            % Unsigned divide RDX:RAX by the value in 'div_op'. 
            % Details on amd64-prog-man-vol3 manual p122.

    ;       enter(
                enter_stack_size    :: uint16,
                enter_nesting_level :: uint8
            )
            % Creates a procedure stack frame with a stack size specified in 
            % 'enter_stack_size' and nesting level specified in 
            % 'enter_nesting_level'(0 to 31).
            % Details on amd64-prog-man-vol3 manual p124.

    ;       idiv(
                idiv_op             :: operand
                                    % register or memory reference
            )
            % Signed divide RDX:RAX by the value in 'operand'. 
            % Details on amd64-prog-man-vol3 manual p126.

    ;       imul(
                imul_src            :: operand,
                                    % register, memory location, immediate value
                imul_dest           :: maybe(operand),
                                    % register
                imul_multiplicand   :: maybe(operand)
            )
            % Signed multiply 'imul_src' by 'imul_dest' (if specified).
            % Otherwise multiply 'imul_src' by the value in RAX register.
            % Details on amd64-prog-man-vol3 manual p(128-129).

    ;       inc(
                inc_op              :: operand
                                    % register or memory location
            )
            % Increments the contents of 'inc_op'.
            % Details on amd64-prog-man-vol3 manual p133.

    ;       j(
                j_target            :: operand,
                                    % relative offset
                j_condition         :: condition
            )
            % Conditional jump. 
            % Details on amd64-prog-man-vol3 manual p(147-149).

    ;       jrcxz(
                jrcxz_8bit_off  :: operand
            )
            % Jumps to the target instruction located at 'jrcxz_8bit_off'
            % if RCX is zero. 
            % Details on amd64-prog-man-vol3 manual p150.

    ;       jmp(
                jmp_op          :: operand
            )
            % Jumps with target specified in 'jmp_op'
            % Details on amd64-prog-man-vol3 manual p152.

    ;       lea(
                lea_src         :: operand,
                                % memory location
                lea_dest        :: operand
                                % register
            )
            % Stores effective address 'lea_src' into 'lea_dest'.
            % Details on amd64-prog-man-vol3 manual p163.

    ;       leave
            % Sets RSP to the value in RBP and pop RBP. 
            % Details on amd64-prog-man-vol3 manual p164.

    ;       loop(
                loop_rel_8bit   :: operand
            )
            % Decrements RCX then jump if RCX is not zero
            % Details on amd64-prog-man-vol3 manual p169.

    ;       loope(
                loope_rel_8bit  :: operand
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 1.
            % Details on amd64-prog-man-vol3 manual p169.

    ;       loopne(
                loopne_rel_8bit :: operand
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 0.
            % Details on amd64-prog-man-vol3 manual p169.

    ;       loopnz(
                loopnz_rel_8bit :: operand
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 0.
            % Details on amd64-prog-man-vol3 manual p170.

    ;       loopz(
                loopz_rel_8bit  :: operand
            )
            % Decrements RCX then jump if RCX is not zero and ZF = 1.
            % Details on amd64-prog-man-vol3 manual p170.

    ;       mov(
                mov_src          :: operand,
                                 % register, memory location or immediate value 
                mov_dest         :: operand
                                 % register or memory location
            )
            % Copies 'mov_src' to 'mov_dest'. 'mov_dest' cannot be immediate op.
            % Details on amd64-prog-man-vol3 manual p173.

    ;       mul(
                mul_op          :: operand
                                % register or memory location
            )
            % Unsigned multiply 'mul_op' by RAX.
            % Details on amd64-prog-man-vol3 manual p190.

    ;       neg(
                neg_op          :: operand
                                % register or memory location
            )
            % Performs a two's complement negation of 'operand'.
            % Details on amd64-prog-man-vol3 manual p192.

    ;       nop
            % Increments RIP to point to next instruction.
            % Details on amd64-prog-man-vol3 manual p194.

    ;       x86_64_instr_not(
                not_op          :: operand
                                % register or memory location
            )
            % Performs one's complement negation (NOT) of 'operand'.
            % Details on amd64-prog-man-vol3 manual p195.

    ;       or(
                or_src           :: operand,
                                % register, memory location or immediate value
                or_dest          :: operand
                                % register or memory location
            )
            % Performs a logical OR on the bits in 'or_src' and 'or_dest'.
            % Details on amd64-prog-man-vol3 manual p196.

    ;       pop(
                pop_op           :: operand
                                % register or memory location. 
            )
            % Pops the stack into 'operand'.
            % Details on amd64-prog-man-vol3 manual p204.

    ;       popfq
            % Pops a quadword from the stack to the RFLAGS register. 
            % Details on amd64-prog-man-vol3 manual p208.

    ;       push(
                push_op           :: operand
                                  % register, memory location or immediate value
            )
            % Pushes the content of operand onto the stack. 
            % Details on amd64-prog-man-vol3 manual p215.

    ;       pushfq
            % Pushes the RFLAGS quadword onto the stack. 
            % Details on amd64-prog-man-vol3 manual p218.

    ;       rc(
                rc_amount        :: operand,
                                % unsigned immediate value or register
                rc_dest          :: operand,
                                % register or memory location
                rc_cond          :: string
                                % "R" for right, "L" for left
            )
            % Rotate bits in 'rc_dest' according to 'rc_cond' direction 
            % and through the carry flag by the number of bit positions as 
            % specified in 'rc_amount'. 
            % Details on amd64-prog-man-vol3 manual p(220-222).

    ;       ret(
                ret_op          :: maybe(uint16)
            )
            % Near return to the calling procedure, then pop the specified 
            % number of bytes from stack (if specified). 
            % Details on amd64-prog-man-vol3 manual p224.

    ;       ro(
                ro_amount       :: operand,
                                % unsigned immediate value or a register
                ro_dest         :: operand,
                                % register or memory location
                ro_dir          :: string
                                % "L" for left, "R" for right
            )
            % Rotates the bits of 'rol_dest' to the 'ro_dir' direction by 
            % 'rol_amount' bits. 
            % Details on amd64-prog-man-vol3 manual p(230-232).

    ;       sal(
                sal_amount      :: operand,
                                % unsigned immediate value or register
                sal_dest        :: operand 
                                % register or memory location
            )
            % Shift 'sal_dest' left by 'sal_amount'. 
            % Details on amd64-prog-man-vol3 manual p235.

    ;       shl(
                shl_amount      :: operand,
                                % unsigned immediate value or register
                shl_dest        :: operand 
                                % register or memory location
            )
            % Alias to 'sal'.
            % Details on amd64-prog-man-vol3 manual p235.

    ;       sar(
                sar_amount      :: operand,
                                % unsigned immediate value or register
                sar_dest        :: operand 
                                % register or memory location
            )
            % Shift 'sar_dest' right by 'sar_amount'. 
            % Details on amd64-prog-man-vol3 manual p238.

    ;       sbb(
                sbb_src         :: operand,
                                % immediate value, register or memory location
                sbb_dest        :: operand 
                                % register or memory location
            )
            % Subtracts 'sbb_src' from 'sbb_dest' with borrow.
            % Details on amd64-prog-man-vol3 manual p241.

    ;       set(
                set_dest        :: operand,
                                % register or memory location
                set_cond        :: condition
            )
            % Check the status flag in the RFLAGS register. Set the value in 
            % the specified register/8-bit memory reference in 'set_dest' to 1
            % according to the 'set_cond'. 
            % Details on amd64-prog-man-vol3 manual p(246-247).

    ;       shld(
                shld_amount         :: operand,
                                    % register or immediate value
                shld_dest1          :: operand,
                                    % register or memory location
                shld_dest2          :: operand  
                                    % register
            )
            % Shift 'shld_dest1' to the left by 'shld_amount' and shift in a bit
            % pattern in 'shld_dest2' from the right. 
            % Details on amd64-prog-man-vol3 manual p251.

    ;       shr(
                shr_amount          :: operand,
                                    % register or immediate value
                shr_dest            :: operand 
                                    % register or memory location
            )
            % Shift 'shr_dest' right by 'shr_amount'. 
            % Details on amd64-prog-man-vol3 manual p253.

    ;       shrd(
                shrd_amount         :: operand,
                                    % register or immediate value
                shrd_dest1          :: operand,
                                    % register or memory location
                shrd_dest2          :: operand 
                                    % register
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
                                    % imm value, register or memory location
                sub_dest            :: operand 
                                    % register or memory location
            )
            % Subtracts 'sub_src' from 'sub_dest'. 
            % Details on amd64-prog-man-vol3 manual p261.

    ;       test(
                test_src1           :: operand,
                                    % imm value or register
                test_src2           :: operand
                                    % register or memory location
            )
            % Performs a bitwise AND on 'test_src1' and 'test_src2'.
            % Details on amd64-prog-man-vol3 manual p264.

    ;       xadd(
                xadd_src            :: operand,
                                    % register
                xadd_dest           :: operand
                                    % register or memory location 
            )
            % Exchanges the contents of 'xadd_src' with 'xadd_dest', load 
            % their sum into 'xadd_dest'. 
            % Details on amd64-prog-man-vol3 manual p266.

    ;       xchg(
                xchg_src1           :: operand,
                                    % register or memory location
                xchg_src2           :: operand
                                    % register or memory location
            )
            % Exchanges the contents of 'xchg_src1' and 'xchg_src2'.
            % Details on amd64-prog-man-vol3 manual p268.
        
    ;       xor(
                xor_src :: operand,
                xor_dest :: operand 
            ).
            % Performs a bitwise XOR on 'xor_src' with 'xor_dest' and stores
            % the result in xor_dest.  
            % Details on amd64-prog-man-vol3 manual p272.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

init_x86_64_module(CModule) = x86_64_module(CModule, []).  

init_x86_64_proc(CProc) = 
    x86_64_procedure(CProc ^ cproc_name,  CProc ^ cproc_orig_arity,
        CProc ^ cproc_id, CProc ^ cproc_code_model, [], 
        CProc ^ cproc_proc_label, CProc ^ cproc_label_nums,
        CProc ^ cproc_may_alter_rtti, CProc ^ cproc_c_global_vars).

init_x86_64_instruction = x86_64_instr([], ""). 

%-----------------------------------------------------------------------------%
