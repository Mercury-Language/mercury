%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% LLDS - The Low-Level Data Structure.

% This module defines both the LLDS data structure itself.

% Main authors: conway, fjh.

%-----------------------------------------------------------------------------%

:- module llds.

:- interface.

:- import_module tree, shapes.
:- import_module bool, string, int, float, list, set, term, std_util.

%-----------------------------------------------------------------------------%

:- type code_model	--->	model_det		% functional & total
			;	model_semi		% just functional
			;	model_non.		% not functional

:- type c_file		--->	c_file(
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

		% readonly data containing a typeinfo structure
	;	c_typeinfo(
			string,			% the name of the C variable
			list(proc_label)	% type-specific procedures
		)

		% some C code from a pragma(c_code) declaration
	;	c_code(
			string,			% C code
			term__context		% source code location
		)

		% Code from pragma(export, ...) decls.
	;	c_export(
			list(c_export)
		).

:- type c_procedure	--->	c_procedure(string, int, llds__proc_id,
						list(instruction)).
			%	predicate name, arity, mode, code

:- type c_export	==	string.

:- type llds__proc_id == int.

:- type code_tree	==	tree(list(instruction)).

:- type instruction	==	pair(instr, string).
			%	instruction, comment

:- type call_model	--->	det ; semidet ; nondet(bool).

:- type instr
	--->	comment(string)
			% Insert a comment into the output code.

	;	livevals(set(lval))
			% A list of which registers and stack locations
			% are currently live.

	;	block(int, int, list(instruction))
			% A list of instructions that make use of
			% some local temporary variables.

	;	assign(lval, rval)
			% Assign the value specified by rval to the location
			% specified by lval.

	;	call(code_addr, code_addr, list(liveinfo), call_model)
			% call(Target, Continuation, _, _) is the same as
			% succip = Continuation; goto(Target).
			% The third argument is the shape table for the
			% values live on return. The last gives the model
			% of the called procedure, and if it is nondet,
			% says whether tail recursion is applicable to the call.

%	;	call_closure(code_model, code_addr, list(liveinfo))
			% Setup the arguments and branch to a higher
			% order call. The closure is in r1 and the
			% input arguments are in r2, r3, ...
			% The output arguments are in r1, r2, ...
			% except for semidet calls, where they are in r2, ...

	;	mkframe(string, int, code_addr)
			% mkframe(Comment, SlotCount, FailureContinuation)
			% creates a nondet stack frame.

	;	modframe(code_addr)
			% modframe(FailureContinuation) is the same as
			% current_redoip = FailureContinuation.

	;	label(label)

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
			% Get a ticket from the constraint solver, and store it
			% in the lval.

	;	restore_ticket(rval)
			% Restore the the constraint solver to the ticket given
			% in the rval.

	;	discard_ticket
			% Decrement the ticket stack by the size of a solver
			% stack frame.

	;	incr_sp(int)
			% Increment the det stack pointer.

	;	decr_sp(int)
			% Decrement the det stack pointer.

	;	pragma_c(list(pragma_c_decl), list(pragma_c_input), string,
			list(pragma_c_output)).
			% The local variable decs, placing the inputs in the
			% variables, the c code, and where to
			% find the outputs for pragma(c_code, ... ) decs.


:- type pragma_c_decl	--->	pragma_c_decl(type, string).
				% Type name, variable name.
:- type pragma_c_input	--->	pragma_c_input(string, type, rval).
				% variable name, type, variable value.
:- type pragma_c_output   --->	pragma_c_output(lval, type, string).
				% where to put the output val, type and name
				% of variable containing the output val


:- type liveinfo	--->	live_lvalue(lval, shape_num).

:- type lval		--->	reg(reg)	% either an int or float reg
			;	stackvar(int)	% det stack slots
			;	framevar(int)	% nondet stack slots
			;	succip		% det return address
			;	maxfr		% top of nondet stack
			;	curfr		% nondet stack frame pointer
			;	succip(rval)	% the succip of the named
						% nondet stack frame
			;	redoip(rval)	% the redoip of the named
						% nondet stack frame
			;	succfr(rval)
			;	prevfr(rval)
			;	hp		% heap pointer
			;	sp		% top of det stack
			;	field(tag, rval, rval)
			;	lvar(var)
			;	temp(reg).	% only inside blocks

:- type rval		--->	lval(lval)
			;	var(var)
			;	create(tag, list(maybe(rval)), int)
				% tag, arguments, label number
				% The label number is needed for the case when
				% we can construct the term at compile-time
				% and just reference the label.
				% Only constant term create() rvals should
				% get output, others will get transformed
				% to incr_hp(..., Tag, Size) plus
				% assignments to the fields
			;	mkword(tag, rval)
			;	const(rval_const)
			;	unop(unary_op, rval)
			;	binop(binary_op, rval, rval).

:- type rval_const	--->	true
			;	false
			;	int_const(int)
			;	float_const(float)
			;	string_const(string)
			;	address_const(code_addr).

:- type unary_op	--->	mktag
			;	tag
			;	unmktag
			;	mkbody
			;	body
			;	unmkbody
			;	cast_to_unsigned
			;	hash_string
			;	bitwise_complement
			;	(not).

:- type binary_op	--->	(+)	% integer arithmetic
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

:- type reg		--->	r(int)		% integer regs
			;	f(int).		% floating point regs

	% local(proc_label)
	%	Local entry label.
	% local(proc_label, int)
	%	Internal local label which can only be accessed externaly if
	%	it is a continuation label.
	% exported(proc_label)
	%	Entry label, which can be accessed from any where.

:- type label
	--->		local(proc_label)
	;		local(proc_label, int)
	;		exported(proc_label).

:- type code_addr
	--->		label(label)
	;		imported(proc_label)
	;		succip
	;		do_succeed(bool)
	;		do_redo
	;		do_fail
	;		do_det_closure
	;		do_semidet_closure
	;		do_nondet_closure.

:- type proc_label
	--->	proc(string, string, int, int)
		%	 module, predicate name, predicate arity, mode #
	;	special_proc(string, string, sym_name, int, int).
		%	module, pred name, type name, type arity, mode #

:- type tag		==	int.
