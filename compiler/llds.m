%-----------------------------------------------------------------------------%

% LLDS - The Low-Level Data Structure.

% Main authors: conway, fjh.

:- module llds.		

:- import_module io, list, string, int.

%-----------------------------------------------------------------------------%

:- interface.

:- type int == integer.

:- type c_file		--->	c_file(string, list(c_module)).
			%	filename, modules

:- type c_module	--->	c_module(string, list(c_procedure)).
			%	module name, code

:- type c_procedure	--->	c_procedure(string, int, mode_id,
						list(instruction)).
			%	predicate name, arity, mode, code
:- type instruction	--->	instr - string.	
			%	 instruction, comment

:- type instr		--->	assign(lval, rval)
			;	call(code_addr, label)  % pred, continuation
			;	tailcall(code_addr)
			;	proceed
			;	label(label)
			;	goto(label)
			;	if_tag(reg, tag, label)
					% branch to label if tag doesn't
					% match reg's tag.
			;	incr_sp(integer)
			;	decr_sp(integer)
			;	incr_hp(integer).

:- type lval		--->	reg(reg)
			;	field(tag, reg, integer).

:- type rval		--->	lval(lval)
			;	mkword(tag, reg)
			;	iconst(integer)		% integer constants
			;	sconst(string)		% string constants
			;	binop(operator,rval,rval).

:- type operator	--->	(+)
			;	(-)
			;	(*)
			;	(/).

:- type reg		--->	r(integer)		% integer regs
			;	succip
			;	hp
			;	sp
			;	stackvar(integer)	% det stack slots
			;	f(integer).		% floating point regs

:- type code_addr 	--->	nonlocal(string, string, integer, integer)
				%	module, predicate, arity, mode #
			;	local(label).

:- type label 		--->	entrylabel(string, string, integer, integer)
				%	 module, predicate, arity, mode #
			;	label(string, string, int, int, int).
				% module, predicate, arity, mode #, #

:- type tag		=	integer.

:- pred output_c_file(c_file, io__state, io__state).
:- mode output_c_file(i, di, uo).

	% Given a 'c_file' structure, open the appropriate .xmod file
	% and output the code into that file.
	% (We use .xmod instead of .mod to distinguish the compiler-generated
	% files from the hand-written ones.)

%-----------------------------------------------------------------------------%

:- implementation.

	% The following code is very straightforward and
	% unremarkable.  The only thing of note is that is
	% uses the logical io library, and that it uses DCGs
	% to avoid having to explicitly shuffle the state-of-the-world
	% arguments around all the time, as discussed in my hons thesis. -fjh.

output_c_file(c_file(Name, Modules)) -->
	{ string__append(Name, ".xmod", FileName) },
	io__tell(FileName, Result),
	(if { Result = ok } then
		io__write_string("#include \"imp.h\"\n"),
		output_c_module_list(Modules),
		io__told
	else
		io__progname(ProgName),
		io__write_string(ProgName),
		io__write_string(": can't open '"),
		io__write_string(FileName),
		io__write_string(" for output")
	).

:- pred output_c_module_list(list(c_module), io__state, io__state).
:- mode output_c_module_list(i, di, uo).

output_c_module_list([]) --> [].
output_c_module_list([M|Ms]) -->
	output_c_module(M),
	output_c_module_list(Ms).

:- pred output_c_module(c_module, io__state, io__state).
:- mode output_c_module(i, di, uo).

output_c_module(c_module(Name,Predicates)) -->
	io__write_string("\n"),
	io__write_string("/* this code automatically generated - do no edit.*/\n"),
	io__write_string("\n"),
	io__write_string("BEGIN_MODULE("),
	io__write_string(Name),
	io__write_string(")\n"),
	io__write_string("\t/* no module initialization in automatically generated code */\n"),
	io__write_string("BEGIN_CODE\n"),
	io__write_string("\n"),
	output_c_procedure_list(Predicates),
	io__write_string("END_MODULE\n").

:- pred output_c_procedure_list(list(c_procedure), io__state, io__state).
:- mode output_c_procedure_list(i, di, uo).

output_c_procedure_list([]) --> [].
output_c_procedure_list([P|Ps]) -->
	output_c_procedure(P),
	output_c_procedure_list(Ps).

:- pred output_c_procedure(c_procedure, io__state, io__state).
:- mode output_c_procedure(i, di, uo).

output_c_procedure(c_procedure(Name,Arity,Mode,Instructions)) -->
	io__write_string("/* code for predicate "),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(" in mode "),
	io__write_int(Mode),
	io__write_string(" */\n"),
	output_instruction_list(Instructions).

:- pred output_instruction_list(list(instruction), io__state, io__state).
:- mode output_instruction_list(i, di, uo).

output_instruction_list([]) --> [].
output_instruction_list([Inst - Comment|Instructions]) -->
	output_instruction(Inst),
	(if { Comment \= "" } then
		io__write_string("\t/* "),
		io__write_string(Comment),
		io__write_string(" */\n")
	else
		io__write_string("\n")
	),
	output_instruction_list(Instructions).

:- pred output_instruction(instr, io__state, io__state).
:- mode output_instruction(i, di, uo).

output_instruction(assign(Lval, Rval)) -->
	io__write_string("\t"),
	output_lval(Lval),
	io__write_string(" = "),
	output_rval(Rval),
	io__write_string(";").

output_instruction(call(local(Label),ContLabel)) -->
	io__write_string("\t"),
	io__write_string("call(LABEL("),
	output_label(Label),
	io__write_string("), LABEL("),
	output_label(ContLabel),
	io__write_string("));").

output_instruction(call(nonlocal(Pred, Mode),ContLabel)) -->
	io__write_string("\t"),
	io__write_string("callentry("),
	io__write_string(Pred),
	io__write_string("_"),
	io__write_int(Mode),
	io__write_string("), LABEL("),
	output_label(ContLabel),
	io__write_string("));").
	
output_instruction(tailcall(local(Label))) -->
	io__write_string("\t"),
	io__write_string("tailcall(LABEL("),
	output_label(Label),
	io__write_string("));").

output_instruction(tailcall(nonlocal(Pred, Mode))) -->
	io__write_string("\t"),
	io__write_string("tailcallentry("),
	io__write_string(Pred),
	io__write_string("_"),
	io__write_int(Mode),
	io__write_string(");").
	
output_instruction(proceed) -->
	io__write_string("\t"),
	io__write_string("proceed();").

output_instruction(label(Label)) -->
	output_label(Label),
	io__write_string(":\n\t;").
	
output_instruction(goto(Label)) -->
	io__write_string("\t"),
	io__write_string("GOTO(LABEL("),
	output_label(Label),
	io__write_string("))").

output_instruction(if_tag(Reg, Tag, Label)) -->
	io__write_string("\t"),
	io__write_string("if(tag("),
	output_reg(Reg),
	io__write_string(") != "),
	output_tag(Tag),
	io__write_string(") GOTO(LABEL("),
	output_label(Label),
	io__write_string("));").

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

output_instruction(incr_hp(N)) -->
	io__write_string("\t"),
	io__write_string("incr_hp("),
	io__write_int(N),
	io__write_string(");").

:- pred output_label(label, io__state, io__state).
:- mode output_label(i, di, uo).

output_label(entrylabel(Module, Pred, Arity, Mode)) -->
	io__write_string(Module),
	io__write_string("__"),
	io__write_string(Pred),
	io__write_string("_"),
	io__write_int(Arity),
	io__write_string("_"),
	io__write_int(Mode).
output_label(label(Module, Pred, Arity, Mode, Num)) -->
	io__write_string(Module),
	io__write_string("__"),
	io__write_string(Pred),
	io__write_string("_"),
	io__write_int(Arity),
	io__write_string("_"),
	io__write_int(Mode),
	io__write_string("_"),
	io__write_int(Num).

:- pred output_reg(reg, io__state, io__state).
:- mode output_reg(i, di, uo).

output_reg(r(N)) -->
	{ require((N >= 1, N =< 32), "Reg number out of range") },
	io__write_string("r"),
	io__write_int(N).
output_reg(succip) -->
	io__write_string("LVALUE_CAST(Word,succip)").
output_reg(sp) -->
	io__write_string("LVALUE_CAST(Word,sp)").
output_reg(hp) -->
	io__write_string("LVALUE_CAST(Word,hp)").
output_reg(stackvar(N)) -->
	{ require(N >= 0, "stack var out of range") },
	io__write_string("stackvar("),
	io__write_int(N),
	io__write_string(")").
output_reg(f(_)) -->
	{ error("Floating point registers not implemented") }.

:- pred output_tag(tag, io__state, io__state).
:- mode output_tag(i, di, uo).

output_tag(Tag) -->
	io__write_string("mktag("),
	io__write_int(Tag),
	io__write_string(")").

:- pred output_rval(rval, io__state, io__state).
:- mode output_rval(i, di, uo).

output_rval(binop(Op, X, Y)) -->
	io__write_string("("),
	output_rval(X),
	output_operator(Op),
	output_rval(Y),
	io__write_string(")").
output_rval(iconst(N)) -->
	io__write_int(N).
% XXX sconsts are not handled yet.
output_rval(mkword(Tag, Exprn)) -->
	io__write_string("mkword("),
	output_tag(Tag),
	io__write_string(", "),
	output_rval(Exprn),
	io__write_string(")").
output_rval(lval(Lval)) -->
	output_lval(Lval).

:- pred output_lval(lval, io__state, io__state).
:- mode output_lval(i, di, uo).

output_lval(reg(R)) -->
	output_reg(R).
output_lval(field(Tag, Lval, FieldNum)) -->
	io__write_string("field("),
	output_tag(Tag),
	io__write_string(", "),
	output_lval(Lval),
	io__write_string(", "),
	io__write_int(FieldNum),
	io__write_string(")").

:- pred output_operator(operator, io__state, io__state).
:- mode output_operator(input, di, do).

output_operator(+) -->
	io__write_string("+").
output_operator(-) -->
	io__write_string("-").
output_operator(*) -->
	io__write_string("*").
output_operator(/) -->
	io__write_string("/").

%-----------------------------------------------------------------------------%

:- pred clause_num_to_string(int::i, string::o).

clause_num_to_string(N, Str) :-
	(if N < 26 then
		int_to_letter(N, Str)
	else
		N_Low is N mod 26,
		N_High is N // 26,
		int_to_letter(N_Low, L),
		clause_num_to_string(N_High, S),
		string__append(S, L, Str)
	).

:- pred int_to_letter(int, string).
:- mode int_to_letter(i, o).

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

:- end_module llds.

%-----------------------------------------------------------------------------%
