%-----------------------------------------------------------------------------%

% LLDS - The Low-Level Data Structure.

% Main authors: conway, fjh.

%-----------------------------------------------------------------------------%

:- module llds.		
:- interface.
:- import_module io, std_util, list, bintree_set, term, string, int.
		% and float, eventually.

%-----------------------------------------------------------------------------%

:- type c_file		--->	c_file(string, list(c_module)).
			%	filename, modules

:- type c_module	--->	c_module(string, list(c_procedure)).
			%	module name, code

:- type c_procedure	--->	c_procedure(string, int, llds__proc_id,
						list(instruction)).
			%	predicate name, arity, mode, code
:- type llds__proc_id == int.

:- type instruction	==	pair(instr, string).
			%	 instruction, comment

:- type instr	
	--->	comment(string)
			% Insert a comment into the output code.

	;	livevals(bool, bintree_set(lval))
			% A list of which registers and stack locations
			% are currently live; the bool is true if this is
			% at the end of an extended basic block

	;	block(int, list(instruction))
			% a list of instructions that make use of
			% some local temporary variables

	;	assign(lval, rval)
			% Assign the value specified by rval to the location
			% specified by lval

	;	call(code_addr, code_addr, list(live_lvalue)) 
			% call(Target, Continuation) is the same as
			% succip = Continuation; goto(Target)

	;	call_closure(bool, code_addr) 
			% setup the arguments and branch to a higher
			% order call. The closure is in r1.

	;	mkframe(string, int, code_addr)
			% mkframe(Comment, SlotCount, FailureContinuation)
			% creates a nondet stack frame

	;	modframe(code_addr)
			% modframe(FailureContinuation) is the same as
			% current_redoip = FailureContinuation

	;	label(label)

	;	goto(code_addr)
			% Branch to the specified address.
			% Note that jumps to do_fail, etc., get
			% optimized into calls to fail(), etc..

	;	computed_goto(rval, list(label))
			% Evaluate rval, which should be an integer,
			% and jump to the (rval+1)th label in the list.
			% e.g. computed_goto(2, [A, B, C, D])
			% will branch to label C.

	;	c_code(string)
			% do whatever is specified by the string,
			% which can be any piece of C code that
			% does not have any non-local flow of control

	;	if_val(rval, code_addr)
			% if rval is true, then goto code_addr.

	;	incr_sp(int)
			% increment the det stack pointer

	;	decr_sp(int).
			% decrement the det stack pointer

:- type lval		--->	reg(reg)	% either an int or float reg
			;	stackvar(int)	% det stack slots
			;	framevar(int)	% nondet stack slots
			;	succip		% det return address
			;	maxfr		% top of nondet stack
			;	curredoip	% the redoip of the current
						% nondet stack frame
			;	hp		% heap pointer
			;	sp		% top of det stack
			;	field(tag, rval, rval)
			;	lvar(var)
			;	temp(int).	% only inside blocks

:- type live_lvalue	--->
		live_lvalue(lval, int).
			% XXX this tuple will need shape information

:- type rval		--->	lval(lval)
			;	var(var)
			;	create(tag, list(maybe(rval)), int)
				% tag, arguments, label number
				% The label number is needed for the case when
				% we can construct the term at compile-time
				% and just reference the label.
				% Only constant term create() rvals should
				% get output, others will get transformed
				% to mkword(Tag, heap_alloc(Size)) plus
				% assignments to the fields
			;	heap_alloc(rval)
			;	mkword(tag, rval)
			;	const(rval_const)
			;	unop(unary_op, rval)
			;	binop(binary_op, rval, rval).

			/* any additions to `rval' also require additions in
			   code_info__generate_expression
			   code_info__generate_expression_vars
			   code_info__expression_dependencies
			   value_number__make_live
			*/

:- type unary_op	--->	mktag
			;	tag
			;	unmktag
			;	mkbody
			;	body
			;	unmkbody
			;	cast_to_unsigned
			;	hash_string
			;	bitwise_complement
			;	not.

:- type rval_const	--->	true
			;	false
			;	int_const(int)
			;	string_const(string)
			;	pred_const(code_addr).

:- type binary_op	--->	(+)	% integer arithmetic
			;	(-)
			;	(*)
			;	(/)
			;	(mod)
			;	(<<)	% left shift
			;	(>>)	% right shift
			;	(&)	% bitwise and
			;	(|)	% bitwise or
			;	(^)	% bitwise xor
			;	and	% logical and
			;	or	% logical or
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
			;	(>=).

:- type reg		--->	r(int)		% integer regs
			;	f(int).		% floating point regs

:- type label
	--->		local(proc_label)
	;		local(proc_label, int)
	;		exported(proc_label).

:- type code_addr
	--->		label(label)
	;		imported(proc_label)
	;		succip
	;		do_succeed
	;		do_redo
	;		do_fail.

:- type proc_label 	
	--->	proc(string, string, int, int)
		%	 module, predicate name, predicate arity, mode #
	;	unify_proc(string, string, int, int).
		%	module, type name, type arity, mode #

:- type tag		==	int.

	% Given a 'c_file' structure, open the appropriate .mod file
	% and output the code into that file.

:- pred output_c_file(c_file, io__state, io__state).
:- mode output_c_file(in, di, uo) is det.

	% Convert an lval to a string description of that lval.

:- pred llds__lval_to_string(lval, string).
:- mode llds__lval_to_string(in, out) is semidet.

:- pred llds__binary_op_to_string(binary_op, string).
:- mode llds__binary_op_to_string(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, globals, options.

%-----------------------------------------------------------------------------%

	% The following code is very straightforward and
	% unremarkable.  The only thing of note is that is
	% uses the logical io library, and that it uses DCGs
	% to avoid having to explicitly shuffle the state-of-the-world
	% arguments around all the time, as discussed in my hons thesis. -fjh.

output_c_file(c_file(Name, Modules)) -->
	{ string__append(Name, ".mod", FileName) },
	io__tell(FileName, Result),
	(
		{ Result = ok }
	->
		io__write_string("#include \"imp.h\"\n"),
		output_c_module_list(Modules),
		io__told
	;
		io__progname("llds.nl", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(FileName),
		io__write_string("' for output\n")
	).

:- pred output_c_module_list(list(c_module), io__state, io__state).
:- mode output_c_module_list(in, di, uo) is det.

output_c_module_list([]) --> [].
output_c_module_list([M|Ms]) -->
	output_c_module(M),
	output_c_module_list(Ms).

:- pred output_c_module(c_module, io__state, io__state).
:- mode output_c_module(in, di, uo) is det.

output_c_module(c_module(Name,Predicates)) -->
	io__write_string("\n"),
	io__write_string("/* this code automatically generated - do no edit.*/\n"),
	io__write_string("\n"),
	io__write_string("BEGIN_MODULE(mercury__"),
	io__write_string(Name),
	io__write_string(")\n"),
	io__write_string("\t/* no module initialization in automatically generated code */\n"),
	io__write_string("BEGIN_CODE\n"),
	io__write_string("\n"),
	output_c_procedure_list(Predicates),
	io__write_string("END_MODULE\n").

:- pred output_c_procedure_list(list(c_procedure), io__state, io__state).
:- mode output_c_procedure_list(in, di, uo) is det.

output_c_procedure_list([]) --> [].
output_c_procedure_list([P|Ps]) -->
	output_c_procedure(P),
	output_c_procedure_list(Ps).

:- pred output_c_procedure(c_procedure, io__state, io__state).
:- mode output_c_procedure(in, di, uo) is det.

output_c_procedure(c_procedure(Name,Arity,ModeNum0,Instructions)) -->
	io__write_string("\n/*-------------------------------------"),
	io__write_string("------------------------------------*/\n"),
	io__write_string("/* code for predicate "),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(" in mode "),
	{ ModeNum is ModeNum0 mod 10000 },	% strip off the priority
	io__write_int(ModeNum),
	io__write_string(" */\n"),
	output_instruction_list(Instructions).

:- pred output_instruction_list(list(instruction), io__state, io__state).
:- mode output_instruction_list(in, di, uo) is det.

output_instruction_list([]) --> [].
output_instruction_list([Inst - Comment|Instructions]) -->
	output_instruction(Inst),
	io__write_string("\n"),
	globals__io_lookup_bool_option(mod_comments, PrintModComments),
	(
		{ Comment \= "" },
		{ PrintModComments = yes }
	->
		io__write_string("\t\t/* "),
		io__write_string(Comment),
		io__write_string(" */\n")
	;
		[]
	),
	output_instruction_list(Instructions).

:- pred output_instruction(instr, io__state, io__state).
:- mode output_instruction(in, di, uo) is det.

output_instruction(comment(Comment)) -->
	globals__io_lookup_bool_option(mod_comments, PrintModComments),
	(
		{ Comment \= "" },
		{ PrintModComments = yes }
	->
		io__write_strings(["/* ", Comment, " */"])
	;
		[]
	).

output_instruction(livevals(Terminate, LiveVals)) -->
	globals__io_lookup_bool_option(mod_comments, PrintModComments),
	(
		{ PrintModComments = yes }
	->
		io__write_string("/*\n * Live lvalues "),
		( { Terminate = yes } ->
			io__write_string("at end of block:\n")
		;
			io__write_string("at start or middle of block:\n")
		),
		{ bintree_set__to_sorted_list(LiveVals, LiveValsList) },
		output_livevals(LiveValsList),
		io__write_string(" */")
	;
		[]
	).

output_instruction(block(N, Instrs)) -->
	io__write_string("\t{\n"),
	io__write_string("\tWord "),
	output_temp_decls(N),
	io__write_string("\n"),
	output_instruction_list(Instrs),
	io__write_string("\t}\n").

output_instruction(assign(Lval, Rval)) -->
	io__write_string("\t{ "),
	output_lval_decls(Lval),
	output_rval_decls(Rval),
	output_lval(Lval),
	io__write_string(" = "),
	output_rval(Rval),
	io__write_string("; }").

output_instruction(call(Target, Continuation, LiveVals)) -->
	io__write_string("\t{ "),
	output_code_addr_decls(Target),
	output_code_addr_decls(Continuation),
	output_call(Target, Continuation),
	io__write_string(" }\n/*\n"),
	output_gc_livevals(LiveVals).

output_instruction(call_closure(IsSemidet, Continuation)) -->
	io__write_string("\t{ "),
	output_code_addr_decls(Continuation),
	output_call_closure(IsSemidet, Continuation),
	io__write_string(" }").

output_instruction(c_code(C_Code_String)) -->
	io__write_string("\t"),
	io__write_string(C_Code_String).

output_instruction(mkframe(Str, Num, FailureContinuation)) -->
	io__write_string("\t{ "),
	output_code_addr_decls(FailureContinuation),
	io__write_string("mkframe(\""),
	io__write_string(Str),
	io__write_string("\", "),
	io__write_int(Num),
	io__write_string(", "),
	output_code_addr(FailureContinuation),
	io__write_string("); }").

output_instruction(modframe(FailureContinuation)) -->
	io__write_string("\t{ "),
	output_code_addr_decls(FailureContinuation),
	io__write_string("modframe("),
	output_code_addr(FailureContinuation),
	io__write_string("); }").

output_instruction(label(Label)) -->
	output_label(Label),
	io__write_string(":\n\t;").
	
output_instruction(goto(CodeAddr)) -->
	io__write_string("\t{ "),
	output_code_addr_decls(CodeAddr),
	output_goto(CodeAddr),
	io__write_string(" }").

output_instruction(computed_goto(Rval, Labels)) -->
	io__write_string("\t{ "),
	output_rval_decls(Rval),
	io__write_string("COMPUTED_GOTO("),
	output_rval(Rval),
	io__write_string(",\n\t\t"),
	output_label_list(Labels),
	io__write_string("); }").

output_instruction(if_val(Rval, Target)) -->
	io__write_string("\t{ "),
	output_rval_decls(Rval),
	output_code_addr_decls(Target),
	io__write_string("if ("),
	output_rval(Rval),
	io__write_string(") "),
	output_goto(Target),
	io__write_string(" }").

output_instruction(incr_sp(N)) -->
	io__write_string("\t"),
	io__write_string("incr_sp("),
	io__write_int(N),
	io__write_string(");").

output_instruction(decr_sp(N)) -->
	io__write_string("\t"),
	io__write_string("decr_sp("),
	io__write_int(N),
	io__write_string(");").

:- pred output_livevals(list(lval), io__state, io__state).
:- mode output_livevals(in, di, uo) is det.

output_livevals([]) --> [].
output_livevals([Lval|Lvals]) -->
	io__write_string(" *\t"),
	output_lval(Lval),
	io__write_string("\n"),
	output_livevals(Lvals).

:- pred output_gc_livevals(list(live_lvalue), io__state, io__state).
:- mode output_gc_livevals(in, di, uo) is det.

output_gc_livevals([]) -->
	io__write_string(" */").
output_gc_livevals([live_lvalue(Lval, Shape)|Lvals]) -->
	io__write_string(" *\t"),
	output_lval(Lval),
	io__write_string("\t"),
	io__write_int(Shape),
	io__write_string("\n"),
	output_gc_livevals(Lvals).

:- pred output_temp_decls(int, io__state, io__state).
:- mode output_temp_decls(in, di, uo) is det.

output_temp_decls(N) --> 
	output_temp_decls_2(1, N).

:- pred output_temp_decls_2(int, int, io__state, io__state).
:- mode output_temp_decls_2(in, in, di, uo) is det.

output_temp_decls_2(Next, Max) --> 
	( { Next < Max } ->
		( { Next > 0 } ->
			io__write_string(", ")
		;
			[]
		),
		io__write_string("temp"),
		io__write_int(Next),
		{ Next1 is Next + 1 },
		output_temp_decls_2(Next1, Max)
	;
		[]
	).

:- pred output_rval_decls(rval, io__state, io__state).
:- mode output_rval_decls(in, di, uo) is det.

output_rval_decls(lval(Lval)) --> 
	output_lval_decls(Lval).
output_rval_decls(var(_)) --> 
	{ error("output_rval_decls: unexpected var") }.
output_rval_decls(mkword(_, Rval)) --> 
	output_rval_decls(Rval).
output_rval_decls(const(_)) --> [].
output_rval_decls(unop(_, Rval)) -->
	output_rval_decls(Rval).
output_rval_decls(binop(_, Rval1, Rval2)) -->
	output_rval_decls(Rval1),
	output_rval_decls(Rval2).
output_rval_decls(heap_alloc(Rval)) -->
	output_rval_decls(Rval).
output_rval_decls(create(_Tag, ArgVals, Label)) -->
	output_cons_arg_decls(ArgVals),
	io__write_string("static const Word mercury_const_"),
	io__write_int(Label),
	io__write_string("[] = {\n\t\t"),
	output_cons_args(ArgVals),
	io__write_string("};\n\t  ").

:- pred output_cons_arg_decls(list(maybe(rval)), io__state, io__state).
:- mode output_cons_arg_decls(in, di, uo) is det.

output_cons_arg_decls([]) --> [].
output_cons_arg_decls([Arg | Args]) -->
	( { Arg = yes(Rval) } ->
		output_rval_decls(Rval)
	;
		[]
	),
	output_cons_arg_decls(Args).

:- pred output_cons_args(list(maybe(rval)), io__state, io__state).
:- mode output_cons_args(in, di, uo) is det.

output_cons_args([]) --> [].
output_cons_args([Arg | Args]) -->
	( { Arg = yes(Rval) } ->
		output_rval(Rval)
	;
		% `Arg = no' means the argument is uninitialized,
		% but that would mean the term isn't ground
		{ error("output_cons_args: missing argument") }
	),
	( { Args \= [] } ->
		io__write_string(",\n\t\t"),
		output_cons_args(Args)
	;
		io__write_string("\n\t  ")
	).

:- pred output_lval_decls(lval, io__state, io__state).
:- mode output_lval_decls(in, di, uo) is det.

output_lval_decls(field(_, Rval, FieldNum)) -->
	output_rval_decls(Rval),
	output_rval_decls(FieldNum).
output_lval_decls(reg(_)) --> [].
output_lval_decls(stackvar(_)) --> [].
output_lval_decls(framevar(_)) --> [].
output_lval_decls(succip) --> [].
output_lval_decls(maxfr) --> [].
output_lval_decls(curredoip) --> [].
output_lval_decls(hp) --> [].
output_lval_decls(sp) --> [].
output_lval_decls(lvar(_)) --> [].
output_lval_decls(temp(_)) --> [].

:- pred output_code_addr_decls(code_addr, io__state, io__state).
:- mode output_code_addr_decls(in, di, uo) is det.

output_code_addr_decls(succip) --> [].
output_code_addr_decls(do_fail) --> [].
output_code_addr_decls(do_succeed) --> [].
output_code_addr_decls(do_redo) --> [].
output_code_addr_decls(label(_)) --> [].
output_code_addr_decls(imported(ProcLabel)) -->
	io__write_string("extern EntryPoint ENTRY("),
	output_proc_label(ProcLabel),
	io__write_string(");\n\t  ").

:- pred output_goto(code_addr, io__state, io__state).
:- mode output_goto(in, di, uo) is det.

	% Note that we do some optimization here:
	% instead of always outputting `GOTO(<label>)', we
	% output different things for each different kind of label.

output_goto(succip) -->
	io__write_string("proceed();").
output_goto(do_fail) -->
	io__write_string("fail();").
output_goto(do_succeed) -->
	io__write_string("succeed();").
output_goto(do_redo) -->
	io__write_string("redo();").
output_goto(imported(ProcLabel)) -->
	io__write_string("tailcall(ENTRY("),
	output_proc_label(ProcLabel),
	io__write_string("));").
output_goto(label(Label)) -->
	(
		{ Label = local(_) ; Label = exported(_) }
	->
		io__write_string("localtailcall("),
		output_label(Label),
		io__write_string(");")
	;
		{ Label = local(_,_) }
	->
		io__write_string("GOTO_LABEL("),
		output_label(Label),
		io__write_string(");")
	;
		% just in case - this will always work,
		% but shouldn't be needed
		io__write_string("GOTO(LABEL("),
		output_label(Label),
		io__write_string("));")
	).

:- pred output_call(code_addr, code_addr, io__state, io__state).
:- mode output_call(in, in, di, uo) is det.

	% Note that we also do some optimization here by
	% outputting `localcall' rather than `call' for
	% calls to local labels.

output_call(Target, Continuation) -->
	( { Target = label(Label) } ->
		io__write_string("localcall("),
		output_label(Label),
		io__write_string(",\n\t\t"),
		output_code_addr(Continuation),
		io__write_string(");")
	;
		io__write_string("call("),
		output_code_addr(Target),
		io__write_string(",\n\t\t"),
		output_code_addr(Continuation),
		io__write_string(");")
	).

:- pred output_call_closure(bool, code_addr, io__state, io__state).
:- mode output_call_closure(in, in, di, uo) is det.

	% Note that we also do some optimization here by
	% outputting `localcall' rather than `call' for
	% calls to local labels.

output_call_closure(IsSemidet, Continuation) -->
	(
		{ IsSemidet = no }
	->
		io__write_string("call_closure("),
		output_code_addr(Continuation),
		io__write_string(");")
	;
		io__write_string("call_semidet_closure("),
		output_code_addr(Continuation),
		io__write_string(");")
	).

:- pred output_code_addr(code_addr, io__state, io__state).
:- mode output_code_addr(in, di, uo) is det.

output_code_addr(succip) -->
	io__write_string("succip").
output_code_addr(do_fail) -->
	io__write_string("ENTRY(do_fail)").
output_code_addr(do_succeed) -->
	io__write_string("ENTRY(do_succeed)").
output_code_addr(do_redo) -->
	io__write_string("ENTRY(do_redo)").
output_code_addr(label(Label)) -->
	io__write_string("LABEL("),
	output_label(Label),
	io__write_string(")").
output_code_addr(imported(ProcLabel)) -->
	io__write_string("ENTRY("),
	output_proc_label(ProcLabel),
	io__write_string(")").

:- pred output_label_list(list(label), io__state, io__state).
:- mode output_label_list(in, di, uo) is det.

output_label_list([]) --> [].
output_label_list([Label | Labels]) -->
	io__write_string("LABEL("),
	output_label(Label),
	io__write_string(")"),
	output_label_list_2(Labels).

:- pred output_label_list_2(list(label), io__state, io__state).
:- mode output_label_list_2(in, di, uo) is det.

output_label_list_2([]) --> [].
output_label_list_2([Label | Labels]) -->
	io__write_string(" AND\n\t\t"),
	io__write_string("LABEL("),
	output_label(Label),
	io__write_string(")"),
	output_label_list_2(Labels).

:- pred output_label(label, io__state, io__state).
:- mode output_label(in, di, uo) is det.

output_label(exported(ProcLabel)) -->
	output_proc_label(ProcLabel).
output_label(local(ProcLabel)) -->
	output_proc_label(ProcLabel),
	io__write_string("_l").		% l for "local".
output_label(local(ProcLabel, Num)) -->
	output_proc_label(ProcLabel),
	io__write_string("_i"),		% i for "internal" (not Intel ;-)
	io__write_int(Num).

:- pred output_proc_label(proc_label, io__state, io__state).
:- mode output_proc_label(in, di, uo) is det.

	% XXX we need to do something with the module name.

output_proc_label(proc(_Module, Pred0, Arity, ModeNum0)) -->
	output_label_prefix,
	%%% io__write_string(Module),
	{ llds__name_mangle(Pred0 ,Pred) },
	io__write_string(Pred),
	io__write_string("_"),
	io__write_int(Arity),
	io__write_string("_"),
	{ ModeNum is ModeNum0 mod 10000 },	% strip off the priority
	io__write_int(ModeNum).

output_proc_label(unify_proc(_Module, TypeName, TypeArity, ModeNum0)) -->
	output_label_prefix,
	%%% io__write_string(Module),
	io__write_string("unify_"),
	io__write_string(TypeName),
	io__write_string("_"),
	io__write_int(TypeArity),
	io__write_string("_"),
	{ ModeNum is ModeNum0 mod 10000 },	% strip off the priority
	io__write_int(ModeNum).

	% To ensure that Mercury labels don't clash with C symbols, we
	% prefix them with `mercury__'.

:- pred output_label_prefix(io__state, io__state).
:- mode output_label_prefix(di, uo) is det.

output_label_prefix -->
	io__write_string("mercury"),
	io__write_string("__").

:- pred output_reg(reg, io__state, io__state).
:- mode output_reg(in, di, uo) is det.

output_reg(r(N)) -->
	{ (N < 1, N > 32) ->
		error("Reg number out of range")
	;
		true
	},
	io__write_string("r"),
	io__write_int(N).
output_reg(f(_)) -->
	{ error("Floating point registers not implemented") }.

:- pred output_tag(tag, io__state, io__state).
:- mode output_tag(in, di, uo) is det.

output_tag(Tag) -->
	io__write_string("mktag("),
	io__write_int(Tag),
	io__write_string(")").

:- pred output_rval(rval, io__state, io__state).
:- mode output_rval(in, di, uo) is det.

output_rval(const(Const)) -->
	output_rval_const(Const).
output_rval(unop(UnaryOp, Exprn)) -->
	output_unary_op(UnaryOp),
	io__write_string("("),
	output_rval(Exprn),
	io__write_string(")").
output_rval(binop(Op, X, Y)) -->
	( { Op = array_index } ->
		io__write_string("((Word *)"),
		output_rval(X),
		io__write_string(")["),
		output_rval(Y),
		io__write_string("]")
	; { llds__string_op(Op, OpStr) } ->
		io__write_string("(strcmp((char *)"),
		output_rval(X),
		io__write_string(", (char *)"),
		output_rval(Y),
		io__write_string(")"),
		io__write_string(" "),
		io__write_string(OpStr),
		io__write_string("0)")
	;
		io__write_string("("),
		output_rval(X),
		io__write_string(" "),
		output_binary_op(Op),
		io__write_string(" "),
		output_rval(Y),
		io__write_string(")")
	).
output_rval(mkword(Tag, Exprn)) -->
	io__write_string("mkword("),
	output_tag(Tag),
	io__write_string(", "),
	output_rval(Exprn),
	io__write_string(")").
output_rval(lval(Lval)) -->
	output_rval_lval(Lval).
output_rval(heap_alloc(Exprn)) -->
	io__write_string("heap_alloc("),
	output_rval(Exprn),
	io__write_string(")").
output_rval(create(Tag, _Args, LabelNum)) -->
		% emit a reference to the static constant which we
		% declared in output_rval_decls.
	io__write_string("mkword(mktag("),
	io__write_int(Tag),
	io__write_string("), "),
	io__write_string("mercury_const_"),
	io__write_int(LabelNum),
	io__write_string(")").
output_rval(var(_)) -->
	{ error("Cannot output a var(_) expression in code") }.

:- pred output_unary_op(unary_op, io__state, io__state).
:- mode output_unary_op(in, di, uo) is det.

output_unary_op(mktag) -->
	io__write_string("mktag").
output_unary_op(tag) -->
	io__write_string("tag").
output_unary_op(unmktag) -->
	io__write_string("unmktag").
output_unary_op(mkbody) -->
	io__write_string("mkbody").
output_unary_op(body) -->
	io__write_string("body").
output_unary_op(unmkbody) -->
	io__write_string("unmkbody").
output_unary_op(hash_string) -->
	io__write_string("hash_string").
output_unary_op(bitwise_complement) -->
	io__write_string("~").
output_unary_op(not) -->
	io__write_string("!").
output_unary_op(cast_to_unsigned) -->
	io__write_string("(unsigned)").

:- pred llds__string_op(binary_op, string).
:- mode llds__string_op(in, out) is semidet.

llds__string_op(str_eq, "==").
llds__string_op(str_ne, "!=").
llds__string_op(str_le, "<=").
llds__string_op(str_ge, ">=").
llds__string_op(str_lt, "<").
llds__string_op(str_gt, ">").

:- pred output_rval_const(rval_const, io__state, io__state).
:- mode output_rval_const(in, di, uo) is det.

output_rval_const(int_const(N)) -->
	io__write_int(N).
output_rval_const(string_const(String)) -->
	io__write_string("string_const(\""),
	output_c_quoted_string(String),
	{ string__length(String, StringLength) },
	io__write_string("\", "),
	io__write_int(StringLength),
	io__write_string(")").
output_rval_const(true) -->
	io__write_string("TRUE").
output_rval_const(false) -->
	io__write_string("FALSE").
output_rval_const(pred_const(CodeAddress)) -->
	io__write_string("(int) "),
	output_code_addr(CodeAddress).

:- pred output_lval(lval, io__state, io__state).
:- mode output_lval(in, di, uo) is det.

output_lval(reg(R)) -->
	output_reg(R).
output_lval(stackvar(N)) -->
	{ (N < 0) ->
		error("stack var out of range")
	;
		true
	},
	io__write_string("detstackvar("),
	io__write_int(N),
	io__write_string(")").
output_lval(framevar(N)) -->
	{ (N < 0) ->
		error("frame var out of range")
	;
		true
	},
	io__write_string("framevar("),
	io__write_int(N),
	io__write_string(")").
output_lval(succip) -->
	io__write_string("LVALUE_CAST(Word,succip)").
output_lval(sp) -->
	io__write_string("LVALUE_CAST(Word,sp)").
output_lval(hp) -->
	io__write_string("LVALUE_CAST(Word,hp)").
output_lval(maxfr) -->
	io__write_string("LVALUE_CAST(Word,maxfr)").
output_lval(curredoip) -->
	io__write_string("LVALUE_CAST(Word,curredoip)").
output_lval(field(Tag, Rval, FieldNum)) -->
	io__write_string("field("),
	output_tag(Tag),
	io__write_string(", "),
	output_rval(Rval),
	io__write_string(", "),
	output_rval(FieldNum),
	io__write_string(")").
output_lval(lvar(_)) -->
	{ error("Illegal to output an lvar") }.
output_lval(temp(N)) -->
	io__write_string("temp"),
	io__write_int(N).

:- pred output_rval_lval(lval, io__state, io__state).
:- mode output_rval_lval(in, di, uo) is det.

output_rval_lval(reg(R)) -->
	io__write_string("(int) "),
	output_reg(R).
output_rval_lval(stackvar(N)) -->
	{ (N < 0) ->
		error("stack var out of range")
	;
		true
	},
	io__write_string("(int) detstackvar("),
	io__write_int(N),
	io__write_string(")").
output_rval_lval(framevar(N)) -->
	{ (N < 0) ->
		error("nondet stack var out of range")
	;
		true
	},
	io__write_string("(int) framevar("),
	io__write_int(N),
	io__write_string(")").
output_rval_lval(succip) -->
	io__write_string("(int) succip").
output_rval_lval(sp) -->
	io__write_string("(int) sp").
output_rval_lval(hp) -->
	io__write_string("(int) hp").
output_rval_lval(maxfr) -->
	io__write_string("(int) maxfr").
output_rval_lval(curredoip) -->
	io__write_string("(int) curredoip").
output_rval_lval(field(Tag, Rval, FieldNum)) -->
	io__write_string("(int) field("),
	output_tag(Tag),
	io__write_string(", "),
	output_rval(Rval),
	io__write_string(", "),
	output_rval(FieldNum),
	io__write_string(")").
output_rval_lval(lvar(_)) -->
	{ error("Illegal to output an lvar") }.
output_rval_lval(temp(N)) -->
	io__write_string("temp"),
	io__write_int(N).

%-----------------------------------------------------------------------------%

:- pred output_c_quoted_string(string, io__state, io__state).
:- mode output_c_quoted_string(in, di, uo) is det.

output_c_quoted_string(S0) -->
	( { string__first_char(S0, Char, S1) } ->
		( { quote_c_char(Char, QuoteChar) } ->
			io__write_char('\\'),
			io__write_char(QuoteChar)
		;
			io__write_char(Char)
		),
		output_c_quoted_string(S1)
	;
		[]
	).

:- pred quote_c_char(character, character).
:- mode quote_c_char(in, out) is semidet.

quote_c_char('\"', '"').
quote_c_char('\\', '\\').
quote_c_char('\n', 'n').
quote_c_char('\t', 't').
quote_c_char('\b', 'b').

%-----------------------------------------------------------------------------%

:- pred output_binary_op(binary_op, io__state, io__state).
:- mode output_binary_op(in, di, uo) is det.

output_binary_op(Op) -->
	{ llds__binary_op_to_string(Op, String) },
	io__write_string(String).

llds__binary_op_to_string(+, "+").
llds__binary_op_to_string(-, "-").
llds__binary_op_to_string(*, "*").
llds__binary_op_to_string(/, "/").
llds__binary_op_to_string(<<, "<<").
llds__binary_op_to_string(>>, ">>").
llds__binary_op_to_string(&, "&").
llds__binary_op_to_string(|, "|").
llds__binary_op_to_string(^, "^").
llds__binary_op_to_string(mod, "%").
llds__binary_op_to_string(eq, "==").
llds__binary_op_to_string(ne, "!=").
llds__binary_op_to_string(and, "&&").
llds__binary_op_to_string(or, "||").
llds__binary_op_to_string(<, "<").
llds__binary_op_to_string(>, ">").
llds__binary_op_to_string(<=, "<=").
llds__binary_op_to_string(>=, ">=").
	% The following is just for debugging purposes -
	% string operators are not output as `str_eq', etc.
llds__binary_op_to_string(array_index, "array_index").
llds__binary_op_to_string(str_eq, "str_eq").
llds__binary_op_to_string(str_ne, "str_ne").
llds__binary_op_to_string(str_lt, "str_lt").
llds__binary_op_to_string(str_gt, "str_gt").
llds__binary_op_to_string(str_le, "str_le").
llds__binary_op_to_string(str_ge, "str_ge").

%-----------------------------------------------------------------------------%

:- pred clause_num_to_string(int::in, string::out) is det.

clause_num_to_string(N, Str) :-
	( clause_num_to_string_2(N, Str0) ->
		Str = Str0
	;
		error("clause_num_to_string failed")
	).

:- pred clause_num_to_string_2(int::in, string::out) is semidet.

clause_num_to_string_2(N, Str) :-
	(
		N < 26
	->
		int_to_letter(N, Str)
	;
		N_Low is N mod 26,
		N_High is N // 26,
		int_to_letter(N_Low, L),
		clause_num_to_string(N_High, S),
		string__append(S, L, Str)
	).

:- pred int_to_letter(int, string).
:- mode int_to_letter(in, out) is semidet.

	% This code is boring, but portable - it works even for EBCDIC ;-)

int_to_letter(0, "a").
int_to_letter(1, "b").
int_to_letter(2, "c").
int_to_letter(3, "d").
int_to_letter(4, "e").
int_to_letter(5, "f").
int_to_letter(6, "g").
int_to_letter(7, "h").
int_to_letter(8, "i").
int_to_letter(9, "j").
int_to_letter(10, "k").
int_to_letter(11, "l").
int_to_letter(12, "m").
int_to_letter(13, "n").
int_to_letter(14, "o").
int_to_letter(15, "p").
int_to_letter(16, "q").
int_to_letter(17, "r").
int_to_letter(18, "s").
int_to_letter(19, "t").
int_to_letter(20, "u").
int_to_letter(21, "v").
int_to_letter(22, "w").
int_to_letter(23, "x").
int_to_letter(24, "y").
int_to_letter(25, "z").

llds__lval_to_string(framevar(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("framevar(", N_String, Tmp),
	string__append(Tmp, ")", Description).
llds__lval_to_string(stackvar(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("stackvar(", N_String, Tmp),
	string__append(Tmp, ")", Description).
llds__lval_to_string(reg(Reg), Description) :-
	llds__reg_to_string(Reg, Reg_String),
	string__append("reg(", Reg_String, Tmp),
	string__append(Tmp, ")", Description).

:- pred llds__reg_to_string(reg, string).
:- mode llds__reg_to_string(in, out) is det.

llds__reg_to_string(r(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("r(", N_String, Tmp),
	string__append(Tmp, ")", Description).
llds__reg_to_string(f(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("f(", N_String, Tmp),
	string__append(Tmp, ")", Description).


	% XXX This is a quick hack!

:- pred llds__name_mangle(string, string).
:- mode llds__name_mangle(in, out) is det.

llds__name_mangle(Pred0, Pred) :-
	string__to_char_list(Pred0, CharList0),
	llds__name_mangle_2(CharList0, CharList),
	string__from_char_list(CharList, Pred).

:- pred llds__name_mangle_2(list(character), list(character)).
:- mode llds__name_mangle_2(in, out) is det.

llds__name_mangle_2([], []).
llds__name_mangle_2([C|Cs], Chars) :-
	llds__mangle_char(C, Chars0),
	llds__name_mangle_2(Cs, Chars1),
	list__append(Chars0, Chars1, Chars).

:- pred llds__mangle_char(character, list(character)).
:- mode llds__mangle_char(in, out) is det.

llds__mangle_char(C, Chars) :-
	(
		C = ('=')
	->
		Chars = ['_','_','e','q','_','_']
	;
		Chars = [C]
	).

:- end_module llds.

%-----------------------------------------------------------------------------%
