%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prog_out.
:- import_module prog_io.

	% This module defines some predicates which output various parts
	% of the parse tree created by prog_io.

	% WARNING - this module is mostly junk at the moment
	% (the format of the output is pretty terrible,
	% and it includes calls to write/1 in various places).
	% Consider it as just a debugging aid.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% write out the list of error/warning messages which is
	% returned when a module is parsed.

prog_out__write_messages([]) --> [].
prog_out__write_messages([Msg - Term | Rest]) -->
	io__write_string(Msg),
	io__write_string(": "),
	{ varset__init(VarSet) },	% XXX variable names in error messages
	io__write_term_nl(VarSet, Term),
	prog_out__write_messages(Rest).

	% write out a whole module

prog_out__write_prog(module(Name, Items)) -->
	io__write_string(":- module "),
	io__write_string(Name),
	io__write_string(".\n"),
	prog_out__write_items(Items).

	% write out a list of program items

prog_out__write_items([]) --> [].
prog_out__write_items([Item|Items]) -->
	prog_out__write_item(Item),
	io__write_string("\n"),
	prog_out__write_items(Items).

	% write out a top-level program item

prog_out__write_item(clause(VarSet, SymName, Args, Body)) -->
	prog_out__write_sym_name(SymName),
	prog_out__write_args(VarSet, Args),
	(if {Body = true} then
		io__write_string(".\n")
	else
		io__write_string(" :-"),
		prog_out__write_goal(Body, 1, ',', VarSet),
		io__write_string(".\n")
	).
	% XXX these are basically just debugging stubs
prog_out__write_item(module_defn(_VarSet, ModuleDefn)) -->
	{ write(ModuleDefn), write('.'), nl }.
prog_out__write_item(type_defn(_VarSet, Defn, _Condition)) -->
	{ write(Defn), write('.'), nl }.
prog_out__write_item(mode_defn(_VarSet, Defn, _Condition)) -->
	{ write(Defn), write('.'), nl }.
prog_out__write_item(inst_defn(_VarSet, Defn, _Condition)) -->
	{ write(Defn), write('.'), nl }.
prog_out__write_item(pred(VarSet, Name, Args, _Condition)) -->
	io__write_string(":- pred "),
	prog_out__write_sym_name(Name),
	prog_out__write_pred_args(VarSet, Args),
	io__write_string(".\n").
prog_out__write_item(rule(VarSet, Name, Args, _Condition)) -->
	io__write_string(":- rule "),
	prog_out__write_sym_name(Name),
	prog_out__write_pred_args(VarSet, Args),
	io__write_string(".\n").
prog_out__write_item(mode(_VarSet, Name, Args, _Condition)) -->
	io__write_string(":- mode "),
	prog_out__write_sym_name(Name),
	{ write(Args), write('.'), nl }.

	% write out a (possibly qualified) symbol name

prog_out__write_sym_name(qualified(ModuleSpec,Name)) -->
	prog_out__write_module_spec(ModuleSpec),
	io__write_string(":"),
	io__write_string(Name).
prog_out__write_sym_name(unqualified(Name)) -->
	io__write_string(Name).

	% write out a module specifier

prog_out__write_module_spec(ModuleSpec) -->
	io__write_string(ModuleSpec).

	% write out the arguments to a functor:
	% if there are no arguments, then don't write anything,
	% otherwise enclose them in parentheses and separate them with commas.

	% XXX need to think about operators & operator precedence

prog_out__write_args(_, []) --> [].
prog_out__write_args(VarSet, [X|Xs]) -->
	io__write_string("("),
	io__write_term(VarSet, X),
	prog_out__write_args_2(VarSet, Xs),
	io__write_string(")").

prog_out__write_args_2(_VarSet, []) --> [].
prog_out__write_args_2(VarSet, [X|Xs]) --> 
	io__write_string(", "),
	io__write_term(VarSet, X),
	prog_out__write_args_2(VarSet, Xs).

	% write out the type arguments to a :- pred declaration.
	% if there are no arguments, then don't write anything,
	% otherwise enclose them in parentheses and separate them with commas.
	% XXX need to think about operators & operator precedence

prog_out__write_pred_args(_, []) --> [].
prog_out__write_pred_args(VarSet, [X|Xs]) -->
	io__write_string("("),
	prog_out__write_pred_arg(VarSet, X),
	prog_out__write_pred_args_2(VarSet, Xs),
	io__write_string(")").

prog_out__write_pred_args_2(_VarSet, []) --> [].
prog_out__write_pred_args_2(VarSet, [X|Xs]) --> 
	io__write_string(", "),
	prog_out__write_pred_arg(VarSet, X),
	prog_out__write_pred_args_2(VarSet, Xs).

prog_out__write_pred_arg(VarSet, type_only(Type)) -->
	io__write_term(VarSet, Type).
prog_out__write_pred_arg(VarSet, type_and_mode(Type, Mode)) -->
	io__write_term(VarSet, Type),
	prog_out__write_mode(VarSet, Mode).


prog_out__write_mode(_VarSet, Mode) -->		% XXX
	{ write(Mode), write('.'), nl }.

/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
 * 
 * All rights are reserved.
 *
 * Author: Philip Dart, 1988
 *		Based on a theme by Lawrence Byrd and Lee Naish.
 *	Fixed again by Lee Naish 9/88
 */

% May bear some vague resemblance to some code written by Lawrence Byrd
% at Edinburgh a long time ago.

prog_out__writeDCGClause(Head, Body, VarSet) -->
	% prog_out__get_op_prec("-->", 1, Prec),
	{ Prec = 1199 },
	prog_out__qwrite(Prec, VarSet, Head),
	io__write_string(" -->"),
	prog_out__write_goal(Body, 1, ',', VarSet).

:- type context ---> '(' ; (';') ; (then) ; (else) ; ','.

:- pred prog_out__write_goal(goal, int, context, varset, io__state, io__state).
:- mode prog_out__write_goal(input, input, input, input, di, uo).

prog_out__write_goal(fail, I0, T, _VarSet) -->
	prog_out__beforelit(T, I0),
	io__write_string("fail").

prog_out__write_goal(true, I0, T, _VarSet) -->
	prog_out__beforelit(T, I0),
	io__write_string("true").

prog_out__write_goal(some(Vars,Goal), I0, T, VarSet) -->
	prog_out__beforelit(T, I0),
	io__write_string("some ["),
	prog_out__write_var_list(Vars, VarSet),
	io__write_string("] ("),
	{ I1 is I0 + 1 },
	prog_out__write_goal(Goal, I1, '(', VarSet),
	io__write_string("\n"),
	prog_out__indent(I0),
	io__write_string(")").

prog_out__write_goal(all(Vars,Goal), I0, T, VarSet) -->
	prog_out__beforelit(T, I0),
	io__write_string("all ["),
	prog_out__write_var_list(Vars, VarSet),
	io__write_string("] ("),
	{ I1 is I0 + 1 },
	prog_out__write_goal(Goal, I1, '(', VarSet),
	io__write_string("\n"),
	prog_out__indent(I0),
	io__write_string(")").

prog_out__write_goal((P, Q), I0, T, VarSet) -->
	prog_out__write_goal(P, I0, T, VarSet),
	io__write_string(","),
	{if T = (',') then I = I0 else I is I0 + 1},
	prog_out__write_goal(Q, I, (','), VarSet).

prog_out__write_goal(if_then_else(Vars,C,A,B), I, T, VarSet) -->
	{if T = (then) then I1 is I + 1 else I1 = I},
	(if {T = (else)} then
		[]
	else
		io__write_string("\n"),
		prog_out__indent(I1)
	),
	io__write_string(" if "),
	prog_out__write_some_vars(VarSet, Vars),
	prog_out__write_goal(C, I, '(', VarSet),
	io__write_string(" then"),
	prog_out__write_goal(A, I1, (then), VarSet),
	io__write_string("\n"),
	prog_out__indent(I1),
	io__write_string("else"),
	prog_out__write_goal(B, I1, (else), VarSet),
	(if {T \= (else)} then
		io__write_string("\n"),
		prog_out__indent(I1),
		io__write_string(")")
	else
		[]
	).

prog_out__write_goal(if_then(Vars,C,A), I, T, VarSet) -->
	{if T = (then) then I1 is I + 1 else I1 = I},
	(if {T = (else)} then
		[]
	else
		io__write_string("\n"),
		prog_out__indent(I1)
	),
	io__write_string(" if "),
	prog_out__write_some_vars(VarSet, Vars),
	prog_out__write_goal(C, I, '(', VarSet),
	io__write_string(" then"),
	prog_out__write_goal(A, I1, (then), VarSet),
	(if {T \= (else)} then
		io__write_string("\n"),
		prog_out__indent(I1),
		io__write_string(")")
	else
		[]
	).

prog_out__write_goal((P ; Q), I, T, VarSet) -->
	(if {T = (;)} then
		io__write_string("\t\n"),
		prog_out__write_goal(P, I, (;), VarSet)
	else
		io__write_string("\n"),
		prog_out__indent(I),
		io__write_string("("),
		prog_out__write_goal(P, I, '(', VarSet)
	),
	io__write_string("\n"),
	prog_out__indent(I),
	io__write_string(";"),
	prog_out__write_goal(Q, I, (;), VarSet),
	(if {T = (;)} then
		[]
	else	
		io__write_string("\n"),
		prog_out__indent(I),
		io__write_string(")")
	).

prog_out__write_goal(not(_Vars, A), I, _, VarSet) -->	% XXX
	io__write_string("not("),
	prog_out__write_goal(A, I, '(', VarSet),
	io__write_string(")").

prog_out__write_goal(call(X), I, T, VarSet) -->
	prog_out__beforelit(T, I),
		% Pos 1 of (,) has lowest prec of constructs
	% prog_out__get_op_prec(",", 1, Prec),
	{ Prec = 999 },
	prog_out__qwrite(Prec, VarSet, X).

prog_out__write_var_list(_VarSet, Vars) -->
	{ write(Vars) }.	% XXX

prog_out__write_some_vars(_VarSet, Vars) -->
	{ write('some '), write(Vars) }.	% XXX

:- pred prog_out__beforelit(context, int, io__state, io__state).
:- mode prog_out__beforelit(input, input, di, uo).

prog_out__beforelit('(', _) -->
	io__write_string("\t").
prog_out__beforelit((;), I) -->
	io__write_string("\n"),
	{ I1 is I + 1 },
	prog_out__indent(I1),
	io__write_string("\t").
prog_out__beforelit((then), I) -->
	io__write_string("\n"),
	{ I1 is I + 1 },
	prog_out__indent(I1).
prog_out__beforelit((else), I) -->
	io__write_string("\n"),
	{ I1 is I + 1 },
	prog_out__indent(I1).
prog_out__beforelit(',', I) -->
	io__write_string("\n"),
	prog_out__indent(I).

:- pred prog_out__indent(int, io__state, io__state).
:- mode prog_out__indent(int, di, uo).
prog_out__indent(N) -->
	(if {N > 0} then
		io__write_string("\t"),
		{ N1 is N - 1 },
		prog_out__indent(N1)
	else
		[]
	).

:- pred prog_out__qwrite(int, varset, term, io__state, io__state).
:- mode prog_out__qwrite(input, input, input, di, uo).

	% XXX problems with precedence

prog_out__qwrite(_Prec, VarSet, X) -->
	io__write_term(VarSet, X).

:- pred prog_out__get_op_prec(string, int, int, io__state, io__state).
:- mode prog_out__get_op_prec(input, input, output, di, uo).

prog_out__get_op_prec(Op, Pos, Prec) -->
	io__current_ops(Ops),
	{ get_prec_and_type(Op, Ops, Prec1, Type),
	  prog_out__op_adj(Pos, Type, Adj),
	  Prec is Prec1 - Adj
	}.

get_prec_and_type(ThisOp, [Op|Ops], Prec, Type) :-
	(if some [Prec1, Type1]
		Op = op(Prec1, Type1, ThisOp)
	then
		Prec = Prec1,
		Type = Type1
	else
		get_prec_and_type(ThisOp, Ops, Prec, Type)
	).

:- pred prog_out__op_adj(int, op_type, int).
:- mode prog_out__op_adj(input, input, output).

prog_out__op_adj(1, xfx, 1).
prog_out__op_adj(1, xfy, 1).
prog_out__op_adj(1, fxy, 1).
prog_out__op_adj(1, fxx, 1).
prog_out__op_adj(1, yfx, 0).
prog_out__op_adj(1, yfy, 0).
prog_out__op_adj(1, fyx, 0).
prog_out__op_adj(1, fyy, 0).
prog_out__op_adj(2, xfx, 1).
prog_out__op_adj(2, xfy, 0).
prog_out__op_adj(2, fxy, 0).
prog_out__op_adj(2, fxx, 1).
prog_out__op_adj(2, yfx, 1).
prog_out__op_adj(2, yfy, 0).
prog_out__op_adj(2, fyx, 1).
prog_out__op_adj(2, fyy, 0).
prog_out__op_adj(1,  xf, 1).
prog_out__op_adj(1,  fx, 1).
prog_out__op_adj(1,  yf, 0).
prog_out__op_adj(1,  fy, 0).

/****** JUNK
%-----------------------------------------------------------------------------%

	% This is how types are represented.

			% one day we might allow types to take
			% value parameters as well as type parameters.

% type_defn/3 define above

:- type type_defn	--->	du_type(sym_name, list(type_param),
						list(constructor))
			;	uu_type(sym_name, list(type_param), list(type))
			;	eqv_type(sym_name, list(type_param), type).

	% XXX constructor should be pair(sym_name, list(type)) not term.
:- type constructor	==	term.

	% XXX type parameters should be variables not terms
:- type type_param	=	term.

:- type (type)		=	term.

	% Types may have arbitrary assertions associated with them
	% (eg. you can define a type which represents sorted lists).
	% The compiler will ignore these assertions - they are intended
	% to be used by other tools, such as the debugger.

:- type condition	--->	true
			;	where(term).

%-----------------------------------------------------------------------------%

	% This is how instantiatednesses and modes are represented.
	% Note that while we use the normal term data structure to represent 
	% type terms (see above), we need a separate data structure for inst 
	% terms.

% inst_defn/3 defined above

:- type inst_defn	--->	inst_defn(sym_name, list(inst_param), inst).

	% XXX inst parameters should be variables not terms.
:- type inst_param	==	term.

:- type (inst)		--->	free
			;	bound(list(bound_inst))
			;	ground
			;	inst_var(var)
			;	user_defined_inst(sym_name, list(inst)).

:- type bound_inst	--->	functor(const, list(inst)).


% mode_defn/3 defined above

:- type mode_defn	--->	mode_defn(sym_name, list(inst_param), mode).

:- type (mode)		--->	((inst) -> (inst))
			;	user_defined_mode(sym_name, list(inst)).

% mode/4 defined above

%-----------------------------------------------------------------------------%
	
	% This is how module-system declarations (such as imports
	% and exports) are represented.

:- type module_defn	--->	module(module_name)
			;	interface
			;	implementation
			;	end_module(module_name)
			;	export(sym_list)
			;	import(sym_list)
			;	use(sym_list).
:- type sym_list	--->	sym(list(sym_specifier))
			;	pred(list(pred_specifier))
			;	cons(list(pred_specifier))
			;	op(list(op_specifier))
			;	adt(list(sym_name_specifier))
	 		;	type(list(sym_name_specifier))
	 		;	module(list(module_specifier)).
:- type sym_specifier	--->	sym(sym_name_specifier)
			;	typed_sym(typed_cons_specifier)
			;	pred(pred_specifier)
			;	cons(cons_specifier)
			;	op(op_specifier)
			;	adt(sym_name_specifier)
	 		;	type(sym_name_specifier)
	 		;	module(module_specifier).
:- type pred_specifier	--->	sym(sym_name_specifier)
			;	name_args(sym_name, list(type)).
:- type cons_specifier	--->	sym(sym_name_specifier)
			;	typed(typed_cons_specifier).
:- type typed_cons_specifier --->	
				name_args(sym_name, list(type))
			;	name_res(sym_name_specifier, type)
			;	name_args_res(sym_name,
						list(type), type).
:- type op_specifier	--->	sym(sym_name_specifier)
			% XXX operator fixity specifiers not yet implemented
			;	fixity(sym_name_specifier, fixity).
:- type fixity		--->	infix ; prefix ; postfix.
:- type sym_name_specifier ---> name(sym_name)
			;	name_arity(sym_name, integer).

:- type module_name 	== 	string.

%-----------------------------------------------------------------------------%
JUNK ******/
