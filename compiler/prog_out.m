%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module prog_out.

% Main author: fjh.

% This module defines some predicates which output various parts
% of the parse tree created by prog_io.

% WARNING - this module is mostly junk at the moment!
% Only the first hundred lines or so are meaningful.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.
:- import_module prog_data, io, term.

:- pred prog_out__write_messages(message_list, io__state, io__state).
:- mode prog_out__write_messages(in, di, uo) is det.

:- pred prog_out__write_context(term__context, io__state, io__state).
:- mode prog_out__write_context(in, di, uo) is det.

:- pred prog_out__write_sym_name(sym_name, io__state, io__state).
:- mode prog_out__write_sym_name(in, di, uo) is det.

:- pred prog_out__write_module_spec(module_specifier, io__state, io__state).
:- mode prog_out__write_module_spec(in, di, uo) is det.

:- pred prog_out__write_module_list(list(module_name), io__state, io__state).
:- mode prog_out__write_module_list(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, string, list, varset, std_util, term_io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% write out the list of error/warning messages which is
	% returned when a module is parsed.

prog_out__write_messages([]) --> [].
prog_out__write_messages([Message | Messages]) -->
	prog_out__write_message(Message),
	prog_out__write_messages(Messages).

:- pred prog_out__write_message(pair(string, term), io__state, io__state).
:- mode prog_out__write_message(in, di, uo) is det.

prog_out__write_message(Msg - Term) -->
	(
		{ Term = term__functor(_Functor, _Args, Context) }
	->
		prog_out__write_context(Context)
	;
		[]
	),
	io__write_string(Msg),
	(
		{ Term = term__functor(term__atom(""), [], _Context2) }
	->
		io__write_string(".\n")
	;
		io__write_string(": "),
		{ varset__init(VarSet) }, % XXX variable names in error messages
		term_io__write_term_nl(VarSet, Term)
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
		{ string__format("%s:%03d: ", [s(FileName), i(LineNumber)],
			ContextMessage) }, 
		io__write_string(ContextMessage)
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

prog_out__write_module_spec(ModuleSpec) -->
	io__write_string(ModuleSpec).

%-----------------------------------------------------------------------------%

prog_out__write_module_list([Import1, Import2, Import3 | Imports]) --> 
	io__write_strings(["`", Import1, "', "]),
	write_module_list([Import2, Import3 | Imports]).
prog_out__write_module_list([Import1, Import2]) -->
	io__write_strings(["`", Import1, "' and `", Import2 ,"'"]).
prog_out__write_module_list([Import]) -->
	io__write_strings(["`", Import, "'"]).
prog_out__write_module_list([]) -->
	{ error("prog_out__write_module_list") }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% THE REMAINDER OF THIS FILE IS JUNK THAT IS NOT USED.
% It has been made obsolete by mercury_to_mercury.m.
% However, the code below handles operator precedence better
% than mercury_to_mercury.m.

/**************************

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
:- mode prog_out__write_goal(in, in, in, in, di, uo) is det.

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

prog_out__write_goal(not(A), I, _, VarSet) -->
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
:- mode prog_out__beforelit(in, in, di, uo) is det.

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
:- mode prog_out__indent(int, di, uo) is det.
prog_out__indent(N) -->
	(if {N > 0} then
		io__write_string("\t"),
		{ N1 is N - 1 },
		prog_out__indent(N1)
	else
		[]
	).

:- pred prog_out__qwrite(int, varset, term, io__state, io__state).
:- mode prog_out__qwrite(in, in, in, di, uo) is det.

	% XXX problems with precedence

prog_out__qwrite(_Prec, VarSet, X) -->
	term_io__write_term(VarSet, X).

:- pred prog_out__get_op_prec(string, int, int, io__state, io__state).
:- mode prog_out__get_op_prec(in, in, out, di, uo) is det.

prog_out__get_op_prec(Op, Pos, Prec) -->
	term_io__current_ops(Ops),
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
:- mode prog_out__op_adj(in, in, out) is det.

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
