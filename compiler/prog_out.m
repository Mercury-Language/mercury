%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prog_out.
:- import_module prog_io.

% Main author: fjh.

% This module defines some predicates which output various parts
% of the parse tree created by prog_io.

% WARNING - this module is mostly junk at the moment!
% Only the first hundred lines or so are meaningful.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- pred prog_out__write_messages(message_list, io__state, io__state).
:- mode prog_out__write_messages(input, di, uo).

:- pred prog_out__write_context(term__context, io__state, io__state).
:- mode prog_out__write_context(input, di, uo).

:- pred prog_out__write_sym_name(sym_name, io__state, io__state).
:- mode prog_out__write_sym_name(input, di, uo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% write out the list of error/warning messages which is
	% returned when a module is parsed.

prog_out__write_messages([]) --> [].
prog_out__write_messages([Message | Messages]) -->
	prog_out__write_message(Message),
	prog_out__write_messages(Messages).

:- pred prog_out__write_message(pair(string, term), io__state, io__state).
:- mode prog_out__write_message(input, di, uo).

prog_out__write_message(Msg - Term) -->
	(
		{ Term = term_functor(_Functor, _Args, Context) }
	->
		prog_out__write_context(Context)
	),
	io__write_string(Msg),
	(
		{ Term = term_functor(term_atom(""), [], _Context2) }
	->
		io__write_string(".\n")
	;
		io__write_string(": "),
		{ varset__init(VarSet) }, % XXX variable names in error messages
		io__write_term_nl(VarSet, Term)
	).

%-----------------------------------------------------------------------------%

	% Write out the information in term context (at the moment, just
	% the line number) in a form suitable for the beginning of an
	% error message.

prog_out__write_context(Context) -->
	{ term__context_file(Context, FileName) },
	{ term__context_line(Context, LineNumber) },
	( { FileName = "" } ->
		[]
	;
		io__write_string(FileName),
		io__write_string(":"),
		io__write_int(LineNumber),
		io__write_string(": ")
	).

%-----------------------------------------------------------------------------%

	% write out a (possibly qualified) symbol name

prog_out__write_sym_name(qualified(ModuleSpec,Name)) -->
	prog_out__write_module_spec(ModuleSpec),
	io__write_string(":"),
	io__write_string(Name).
prog_out__write_sym_name(unqualified(Name)) -->
	io__write_string(Name).

	% write out a module specifier

:- pred prog_out__write_module_spec(module_specifier, io__state, io__state).
:- mode prog_out__write_module_spec(input, input, output).

prog_out__write_module_spec(ModuleSpec) -->
	io__write_string(ModuleSpec).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% THE REMAINDER OF THIS FILE IS JUNK THAT IS NOT USED.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

/*****************************

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
prog_out__write_item(nothing) --> [].
prog_out__write_item(module_defn(_VarSet, ModuleDefn)) -->
	io__write_anything(ModuleDefn),
	io__write_string(".\n").
prog_out__write_item(type_defn(_VarSet, Defn, _Condition)) -->
	io__write_anything(Defn),
	io__write_string(".\n").
prog_out__write_item(mode_defn(_VarSet, Defn, _Condition)) -->
	io__write_anything(Defn),
	io__write_string(".\n").
prog_out__write_item(inst_defn(_VarSet, Defn, _Condition)) -->
	io__write_anything(Defn),
	io__write_string(".\n").
prog_out__write_item(pred(VarSet, Name, Args, _Det, _Condition)) -->
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
	io__write_anything(Args),
	io__write_string(".\n").

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
	io__write_anything(Mode),
	io__write_string(".\n").

% Please note that this code is the property of
% the University of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
% 
% All rights are reserved.
%
% Author: Philip Dart, 1988
%		Based on a theme by Lawrence Byrd and Lee Naish.
%	Fixed again by Lee Naish 9/88

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
	(if {T = (else)} then
		[]
	else
		io__write_string("\n"),
		prog_out__indent(I1),
		io__write_string(")")
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
	(if {T = (else)} then
		[]
	else
		io__write_string("\n"),
		prog_out__indent(I1),
		io__write_string(")")
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
	io__write_anything(Vars).

prog_out__write_some_vars(_VarSet, Vars) -->
	io__write_string("some "),
	io__write_anything(Vars).		% XXX

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
% prog_out__op_adj(1, yfy, 0).
prog_out__op_adj(1, fyx, 0).
prog_out__op_adj(1, fyy, 0).
prog_out__op_adj(2, xfx, 1).
prog_out__op_adj(2, xfy, 0).
prog_out__op_adj(2, fxy, 0).
prog_out__op_adj(2, fxx, 1).
prog_out__op_adj(2, yfx, 1).
% prog_out__op_adj(2, yfy, 0).
prog_out__op_adj(2, fyx, 1).
prog_out__op_adj(2, fyy, 0).
prog_out__op_adj(1,  xf, 1).
prog_out__op_adj(1,  fx, 1).
prog_out__op_adj(1,  yf, 0).
prog_out__op_adj(1,  fy, 0).

******************************/
