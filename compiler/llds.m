%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% LLDS - The Low-Level Data Structure.

% This module defines the LLDS data structure itself.

% Main authors: conway, fjh.

%-----------------------------------------------------------------------------%

:- module llds.

:- interface.

:- import_module tree, shapes.
:- import_module bool, list, set, term, std_util.

%-----------------------------------------------------------------------------%

:- type code_model
	--->	model_det		% functional & total
	;	model_semi		% just functional
	;	model_non.		% not functional

:- type c_file	
	--->	c_file(
			string,		% filename
			c_header_info,
			list(c_module)
		).

:- type c_header_info 	==	list(c_header_code).	% in reverse order
:- type c_body_info 	==	list(c_body_code).	% in reverse order

:- type c_header_code	==	pair(string, term__context).
:- type c_body_code	==	pair(string, term__context).

:- type c_module
		% a bunch of low-level C code
	--->	c_module(
			string,			% module name
			list(c_procedure) 	% code
		)

		% readonly data, usually containing a typeinfo structure
	;	c_data(
			string,			% The basename of this C file.
			data_name,		% A representation of the name
						% of the variable; it will be
						% qualified with the basename.
			bool,			% Does it have to use Word *
						% instead of Word?
			bool,			% Should this item be exported?
			list(maybe(rval)),	% The arguments of the create.
			list(pred_proc_id)	% The procedures referenced.
						% Used by dead_proc_elim.
		)

		% some C code from a pragma(c_code) declaration
	;	c_code(
			string,			% C code
			term__context		% source code location
		)

		% Code from `pragma export' decls.
	;	c_export(
			list(c_export)
		).

:- type c_procedure
	--->	c_procedure(
			string,			% predicate name
			int,			% arity
			llds__proc_id,		% mode number
			list(instruction)	% the code for this procedure
		).

:- type llds__proc_id	==	int.

	% the code for `pragma export' is generated directly as strings
	% by export.m.
:- type c_export	==	string.

	% we build up instructions as trees and then flatten
	% the tree to get a list.
:- type code_tree	==	tree(list(instruction)).

:- type instruction	==	pair(instr, string).
			%	instruction, comment

:- type call_model
	--->	det
	;	semidet
	;	nondet(bool).

	% `instr' defines the various LLDS virtual machine instructions.
	% Each instruction gets compiled to a simple piece of C code
	% or a macro invocation.
:- type instr
	--->	comment(string)
			% Insert a comment into the output code.

	;	livevals(set(lval))
			% A list of which registers and stack locations
			% are currently live.

	;	block(int, int, list(instruction))
			% block(NumIntTemps, NumFloatTemps, Instrs):
			% A list of instructions that make use of
			% some local temporary variables.

	;	assign(lval, rval)
			% assign(Location, Value):
			% Assign the value specified by rval to the location
			% specified by lval.

	;	call(code_addr, code_addr, list(liveinfo), call_model)
			% call(Target, Continuation, _, _) is the same as
			% succip = Continuation; goto(Target).
			% The third argument is the shape table for the
			% values live on return. The last gives the model
			% of the called procedure, and if it is nondet,
			% says whether tail recursion is applicable to the call.

	;	mkframe(string, int, code_addr)
			% mkframe(Comment, SlotCount, FailureContinuation)
			% creates a nondet stack frame.

	;	modframe(code_addr)
			% modframe(FailureContinuation) is the same as
			% current_redoip = FailureContinuation.

	;	label(label)
			% Defines a label that can be used as the
			% target of calls, gotos, etc.

	;	goto(code_addr)
			% goto(Target)
			% Branch to the specified address.
			% Note that jumps to do_fail, etc., can get
			% optimized into the invocations of macros fail(), etc..

	;	computed_goto(rval, list(label))
			% Evaluate rval, which should be an integer,
			% and jump to the (rval+1)th label in the list.
			% e.g. computed_goto(2, [A, B, C, D])
			% will branch to label C.

	;	c_code(string)
			% Do whatever is specified by the string,
			% which can be any piece of C code that
			% does not have any non-local flow of control.

	;	if_val(rval, code_addr)
			% If rval is true, then goto code_addr.

	;	incr_hp(lval, maybe(tag), rval)
			% Get a memory block of a size given by an rval
			% and put its address in the given lval,
			% possibly after tagging it with a given tag.

	;	mark_hp(lval)
			% Tell the heap sub-system to store a marker
			% (for later use in restore_hp/1 instructions)
			% in the specified lval

	;	restore_hp(rval)
			% The rval must be a marker as returned by mark_hp/1.
			% The effect is to deallocate all the memory which
			% was allocated since that call to mark_hp.

	;	store_ticket(lval)
			% Get a ticket from the constraint solver,
			% push it onto the ticket stack,
			% and store its address in the lval.

	;	restore_ticket(rval)
			% Restore the the constraint solver to the state
			% it was in when the ticket pointed to by the
			% specified rval was obtained with store_ticket().
			% Reset the ticket stack so that the specified
			% rval is now at the top of the ticket stack.

	;	discard_ticket
			% Pop the top ticket off the ticket stack.

	;	incr_sp(int, string)
			% Increment the det stack pointer. The string is
			% the name of the procedure, for use in stack dumps.
			% It is used only in grades in which stack dumps are
			% enabled (i.e. not in grades where SPEED is defined).

	;	decr_sp(int)
			% Decrement the det stack pointer.

	;	pragma_c(list(pragma_c_decl), list(pragma_c_input), string,
			list(pragma_c_output)).
			% The local variable decs, placing the inputs in the
			% variables, the c code, and where to
			% find the outputs for pragma(c_code, ... ) decs.


	% pragma_c_decl holds the information needed for a variable
	% declaration for a pragma_c instruction.
:- type pragma_c_decl
	--->	pragma_c_decl(type, string).
				% Type name, variable name.

	% A pragma_c_input represents the code that initializes one
	% of the input variables for a pragma_c instruction.
:- type pragma_c_input
	--->	pragma_c_input(string, type, rval).
				% variable name, type, variable value.

	% A pragma_c_output represents the code that stores one of
	% of the outputs for a pragma_c instruction.
:- type pragma_c_output  
	--->	pragma_c_output(lval, type, string).
				% where to put the output val, type and name
				% of variable containing the output val

	% Each call instruction has a list of liveinfo,
	% which stores information about which variables
	% are live at the point of that call.  The information
	% is intended for use by the non-conservative garbage collector.
:- type liveinfo
	--->	live_lvalue(
			lval,
				% What stackslot/reg does
				% this lifeinfo structure
				% refer to?
			shape_num,
				% What is the shape of this (bound) variable?
			maybe(list(lval))
				% Where are the typeinfos that determine the
				% types of the actual parameters of the type
				% parameters of this shape (if it is
				% polymorphic), in the order of the arguments.
		).

	% An lval represents a data location or register that can be used
	% as the target of an assignment.
:- type lval --->

	/* virtual machine registers */

		reg(reg)	% one of the general-purpose virtual machine
				% registers (either an int or float reg)
	;	succip		% virtual machine register holding the
				% return address for det/semidet code
	;	maxfr		% virtual machine register holding a pointer
				% to the top of nondet stack
	;	curfr		% virtual machine register holding a pointer
				% to the current nondet stack frame
	;	hp		% virtual machine register holding the heap
				% pointer
	;	sp		% virtual machine register point to the
				% top of det stack
	;	temp(temp_reg)	% a local temporary register
				% These temporary registers are actually
				% local variables declared in `block'
				% instructions.  They may only be
				% used inside blocks.  The code generator
				% doesn't generate these; they are introduced
				% by value numbering.  The basic idea is
				% to improve efficiency by using local
				% variables that the C compiler may be
				% able to allocate in a register rather than
				% using stack slots.

	/* values on the stack */

	;	stackvar(int)	% det stack slots (numbered starting from 1)
				% relative to the current value of `sp'
				% these are used for both det and semidet code
	;	framevar(int)	% nondet stack slots (numbered starting from 0)
				% relative to the current value of `curfr'

	;	succip(rval)	% the succip slot of the specified
				% nondet stack frame; holds the code address
				% to jump to on successful exit from this
				% nondet procedure
	;	redoip(rval)	% the redoip slot of the specified
				% nondet stack frame; holds the code address
				% to jump to on failure
	;	succfr(rval)	% the succfr slot of the specified
				% nondet stack frame; holds the address of
				% caller's nondet stack frame.  On successful
				% exit from this nondet procedure, we will
				% set curfr to this value.
	;	prevfr(rval)	% the prevfr slot of the specified
				% nondet stack frame; holds the address of
				% the previous frame on the nondet stack.

	/* values on the heap */

	;	field(tag, rval, rval)
				% field(Tag, Address, FieldNum)
				% selects a field of a compound term

	/* pseudo-values */

	;	lvar(var).	% the location of the specified variable
				% `var' lvals are used during code generation,
				% but should not be present in the LLDS at any
				% stage after code generation.

	% An rval is an expression that represents a value.
:- type rval	
	--->	lval(lval)
		% The value of an `lval' rval is just the value stored in
		% the specified lval.
	;	var(var)
		% The value of a `var' rval is just the value of the
		% specified variable.
		% `var' rvals are used during code generation,
		% but should not be present in the LLDS at any
		% stage after code generation.
	;	create(tag, list(maybe(rval)), bool, int)
		% create(Tag, Arguments, IsUnique, LabelNumber):
		% A `create' instruction is used during code generation
		% for creating a term, either on the heap or
		% (if the term is constant) as a static constant.
		% After code generation, only constant term create() rvals
		% should be present in the LLDS, others will get transformed
		% to incr_hp(..., Tag, Size) plus assignments to the fields.
		%
		% The boolean should be true if the term
		% must be unique (e.g. if we're doing to do
		% destructive update on it).  This will prevent the term
		% from being used for other purposes as well; unique terms
		% are always created on the heap, not as constants, and
		% we must not do common term elimination on them.
		%
		% The label number is needed for the case when
		% we can construct the term at compile-time
		% and just reference the label.
	;	mkword(tag, rval)
		% given a pointer and a tag,
		% mkword returns a tagged pointer
	;	const(rval_const)
	;	unop(unary_op, rval)
	;	binop(binary_op, rval, rval).

:- type rval_const
	--->	true
	;	false
	;	int_const(int)
	;	float_const(float)
	;	string_const(string)
	;	code_addr_const(code_addr)
	;	data_addr_const(data_addr).

:- type data_addr
	--->	data_addr(string, data_name, bool).
			% module name; which var; does it have any
			% addresses inside it (i.e. Word or Word *)?

:- type data_name
	--->	common(int)
	;	base_type_info(string, arity)
			% type name, type arity
	;	base_type_layout(string, arity).
			% type name, type arity

:- type unary_op
	--->	mktag
	;	tag
	;	unmktag
	;	mkbody
	;	body
	;	unmkbody
	;	cast_to_unsigned
	;	hash_string
	;	bitwise_complement
	;	(not).

:- type binary_op
	--->	(+)	% integer arithmetic
	;	(-)
	;	(*)
	;	(/)
	;	(mod)
	;	(<<)	% left shift
	;	(>>)	% right shift
	;	(&)	% bitwise and
	;	('|')	% bitwise or
	;	(^)	% bitwise xor
	;	(and)	% logical and
	;	(or)	% logical or
	;	eq	% ==
	;	ne	% !=
	;	array_index
	;	str_eq	% string comparisons
	;	str_ne
	;	str_lt
	;	str_gt
	;	str_le
	;	str_ge
	;	(<)	% integer comparions
	;	(>)
	;	(<=)
	;	(>=)
	;	float_plus
	;	float_minus
	;	float_times
	;	float_divide
	;	float_eq
	;	float_ne
	;	float_lt
	;	float_gt
	;	float_le
	;	float_ge.

	% one of the general-purpose virtual machine registers
:- type reg	
	--->	r(int)		% integer regs
	;	f(int).		% floating point regs

:- type temp_reg == reg.

:- type label
	--->	local(proc_label, int)	% internal to procedure
	;	c_local(proc_label)	% internal to C module
	;	local(proc_label)	% internal to Mercury module
	;	exported(proc_label).	% exported from Mercury module

:- type code_addr
	--->	label(label)		% a label defined in this Mercury module
	;	imported(proc_label)	% a label from another Mercury module
	;	succip			% the address in the `succip' register
	;	do_succeed(bool)	% the bool is `yes' if there are any
					% alternatives left.  If the bool is
					% `no', we do a succeed_discard()
					% rather than a succeed().
	;	do_redo
	;	do_fail
	;	do_det_closure
	;	do_semidet_closure
	;	do_nondet_closure.

	% A proc_label is a label used for the entry point to a procedure
:- type proc_label
	--->	proc(string, pred_or_func, string, int, int)
		%	 module, predicate/function, name, arity, mode #
	;	special_proc(string, string, sym_name, int, int).
		%	module, pred name, type name, type arity, mode #

	% A tag (used in mkword, create and field expressions
	% and in incr_hp instructions) is a small integer.
:- type tag	==	int.
