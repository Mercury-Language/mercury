%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module parse_tree__prog_out.

% Main author: fjh.

% This module defines some predicates which output various parts
% of the parse tree created by prog_io.

% WARNING - this module is mostly junk at the moment!
% Only the first hundred lines or so are meaningful.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

:- pred maybe_report_stats(bool::in, io::di, io::uo) is det.
:- pred maybe_write_string(bool::in, string::in, io::di, io::uo) is det.
:- pred maybe_flush_output(bool::in, io::di, io::uo) is det.

:- pred report_error(string::in, io::di, io::uo) is det.
:- pred report_error(io__output_stream::in, string::in, io::di, io::uo) is det.

:- pred prog_out__write_messages(message_list::in, io::di, io::uo) is det.

:- pred prog_out__write_context(prog_context::in, io::di, io::uo) is det.

:- pred prog_out__context_to_string(prog_context::in, string::out) is det.

	% Write out a symbol name, with special characters escaped,
	% but without any quotes.  This is suitable for use in
	% error messages, where the caller should print out an
	% enclosing forward/backward-quote pair (`...').
:- pred prog_out__write_sym_name(sym_name::in, io::di, io::uo) is det.

:- pred prog_out__write_sym_name_and_arity(sym_name_and_arity::in,
	io::di, io::uo) is det.

	% Write out a symbol name, enclosed in single forward quotes ('...')
	% if necessary, and with any special characters escaped.
	% The output should be a syntactically valid Mercury term.
:- pred prog_out__write_quoted_sym_name(sym_name::in, io::di, io::uo) is det.

	% sym_name_and_arity_to_string(SymName, String):
	%	convert a symbol name and arity to a "<Name>/<Arity>" string,
	%	with module qualifiers separated by
	%	the standard Mercury module qualifier operator.
:- pred prog_out__sym_name_and_arity_to_string(sym_name_and_arity::in,
	string::out) is det.
:- func prog_out__sym_name_and_arity_to_string(sym_name_and_arity) = string.

:- pred prog_out__write_module_spec(module_specifier::in, io::di, io::uo)
	is det.

:- pred prog_out__write_module_list(list(module_name)::in,
	io::di, io::uo) is det.

:- pred prog_out__write_list(list(T)::in,
	pred(T, io__state, io__state)::in(pred(in, di, uo) is det),
	io::di, io::uo) is det.

:- pred prog_out__write_promise_type(promise_type::in, io::di, io::uo) is det.

:- func prog_out__promise_to_string(promise_type) = string.
:- mode prog_out__promise_to_string(in) = out is det.
:- mode prog_out__promise_to_string(out) = in is semidet.
:- mode prog_out__promise_to_string(out) = out is multi.

	% Print "predicate" or "function" depending on the given value.
:- pred write_pred_or_func(pred_or_func::in, io::di, io::uo) is det.

	% Return "predicate" or "function" depending on the given value.
:- func pred_or_func_to_full_str(pred_or_func) = string.

	% Return "pred" or "func" depending on the given value.
:- func pred_or_func_to_str(pred_or_func) = string.

	% Print out a purity name.
:- pred write_purity(purity::in, io::di, io::uo) is det.

	% Get a purity name as a string.
:- pred purity_name(purity, string).
:- mode purity_name(in, out) is det.
:- mode purity_name(out, in) is semidet.

	% Print out a purity prefix.
	% This works under the assumptions that all purity names but `pure'
	% are operators, and that we never need `pure' indicators/declarations.
:- pred write_purity_prefix(purity::in, io::di, io::uo) is det.
:- func purity_prefix_to_string(purity) = string.

	% Convert an evaluation method to a string.
:- func eval_method_to_string(eval_method) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp__prim_data.

:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

maybe_report_stats(yes, !IO) :-
	io__report_stats(!IO).
maybe_report_stats(no, !IO).

maybe_write_string(yes, String, !IO) :-
	io__write_string(String, !IO).
maybe_write_string(no, _, !IO).

maybe_flush_output(yes, !IO) :-
	io__flush_output(!IO).
maybe_flush_output(no, !IO).

report_error(ErrorMessage, !IO) :-
	io__write_string("Error: ", !IO),
	io__write_string(ErrorMessage, !IO),
	io__write_string("\n", !IO),
	io__set_exit_status(1, !IO).

report_error(Stream, ErrorMessage, !IO) :-
	io__set_output_stream(Stream, OldStream, !IO),
	report_error(ErrorMessage, !IO),
	io__set_output_stream(OldStream, _, !IO).

	% write out the list of error/warning messages which is
	% returned when a module is parsed.

prog_out__write_messages([], !IO).
prog_out__write_messages([Message | Messages], !IO) :-
	prog_out__write_message(Message, !IO),
	prog_out__write_messages(Messages, !IO).

:- pred prog_out__write_message(pair(string, term)::in,
	io::di, io::uo) is det.

prog_out__write_message(Msg - Term, !IO) :-
	(
		Term = term__functor(_Functor, _Args, Context0)
	->
		Context0 = term__context(File, Line),
		Context = term__context(File, Line),
		prog_out__write_context(Context, !IO)
	;
		true
	),
	io__write_string(Msg, !IO),
	(
		Term = term__functor(term__atom(""), [], _Context2)
	->
		io__write_string(".\n", !IO)
	;
		io__write_string(": ", !IO),
		varset__init(VarSet),
			% XXX variable names in error messages
		term_io__write_term_nl(VarSet, Term, !IO)
	).

%-----------------------------------------------------------------------------%

	% Write out the information in term context (at the moment, just
	% the line number) in a form suitable for the beginning of an
	% error message.

prog_out__write_context(Context, !IO) :-
	prog_out__context_to_string(Context, ContextMessage),
	io__write_string(ContextMessage, !IO).

%-----------------------------------------------------------------------------%

	% Write to a string the information in term context (at the moment,
	% just the line number) in a form suitable for the beginning of an
	% error message.

prog_out__context_to_string(Context, ContextMessage) :-
	term__context_file(Context, FileName),
	term__context_line(Context, LineNumber),
	( FileName = "" ->
		ContextMessage = ""
	;
		string__format("%s:%03d: ", [s(FileName), i(LineNumber)],
			ContextMessage)
	).

%-----------------------------------------------------------------------------%

	% write out a (possibly qualified) symbol name

prog_out__write_sym_name(qualified(ModuleSpec,Name), !IO) :-
	prog_out__write_module_spec(ModuleSpec, !IO),
	io__write_string(".", !IO),
	term_io__write_escaped_string(Name, !IO).
prog_out__write_sym_name(unqualified(Name), !IO) :-
	term_io__write_escaped_string(Name, !IO).

prog_out__write_sym_name_and_arity(Name / Arity, !IO) :-
	prog_out__write_sym_name(Name, !IO),
	io__write_string("/", !IO),
	io__write_int(Arity, !IO).

prog_out__write_quoted_sym_name(SymName, !IO) :-
	io__write_string("'", !IO),
	prog_out__write_sym_name(SymName, !IO),
	io__write_string("'", !IO).

prog_out__sym_name_and_arity_to_string(SymName/Arity, String) :-
	mdbcomp__prim_data__sym_name_to_string(SymName, SymNameString),
	string__int_to_string(Arity, ArityString),
	string__append_list([SymNameString, "/", ArityString], String).

prog_out__sym_name_and_arity_to_string(SymName/Arity) = String :-
	prog_out__sym_name_and_arity_to_string(SymName/Arity, String).

	% write out a module specifier

prog_out__write_module_spec(ModuleSpec) -->
	prog_out__write_sym_name(ModuleSpec).

%-----------------------------------------------------------------------------%

prog_out__write_module_list(Modules, !IO) :-
	prog_out__write_list(Modules, write_module, !IO).

:- pred write_module(module_name::in, io__state::di, io__state::uo) is det.

write_module(Module, !IO) :-
	io__write_string("`", !IO),
	prog_out__write_sym_name(Module, !IO),
	io__write_string("'", !IO).

prog_out__write_list([Import1, Import2, Import3 | Imports], Writer, !IO) :-
	call(Writer, Import1, !IO),
	io__write_string(", ", !IO),
	prog_out__write_list([Import2, Import3 | Imports], Writer, !IO).
prog_out__write_list([Import1, Import2], Writer, !IO) :-
	call(Writer, Import1, !IO),
	io__write_string(" and ", !IO),
	call(Writer, Import2, !IO).
prog_out__write_list([Import], Writer, !IO) :-
	call(Writer, Import, !IO).
prog_out__write_list([], _, !IO) :-
	error("prog_out__write_module_list").

prog_out__promise_to_string(true) = "promise".
prog_out__promise_to_string(exclusive) = "promise_exclusive".
prog_out__promise_to_string(exhaustive) =  "promise_exhaustive".
prog_out__promise_to_string(exclusive_exhaustive) =
		"promise_exclusive_exhaustive".

prog_out__write_promise_type(PromiseType, !IO) :-
	io__write_string(prog_out__promise_to_string(PromiseType), !IO).

write_pred_or_func(PorF, !IO) :-
	io__write_string(pred_or_func_to_full_str(PorF), !IO).

pred_or_func_to_full_str(predicate) = "predicate".
pred_or_func_to_full_str(function) = "function".

pred_or_func_to_str(predicate) = "pred".
pred_or_func_to_str(function) = "func".

write_purity_prefix(Purity, !IO) :-
	( Purity = pure ->
		true
	;
		write_purity(Purity, !IO),
		io__write_string(" ", !IO)
	).

purity_prefix_to_string(Purity) = String :-
	( Purity = pure ->
		String = ""
	;
		purity_name(Purity, PurityName),
		String = string__append(PurityName, " ")
	).

write_purity(Purity, !IO) :-
	purity_name(Purity, String),
	io__write_string(String, !IO).

purity_name(pure, "pure").
purity_name((semipure), "semipure").
purity_name((impure), "impure").

eval_method_to_string(eval_normal) =		"normal".
eval_method_to_string(eval_loop_check) =	"loop_check".
eval_method_to_string(eval_memo) =		"memo".
eval_method_to_string(eval_minimal(MinimalMethod)) = Str :-
	(
		MinimalMethod = own_stacks,
		Str = "minimal_model_own_stacks"
	;
		MinimalMethod = stack_copy,
		Str = "minimal_model_stack_copy"
	).
eval_method_to_string(eval_table_io(IsDecl, IsUnitize)) = Str :-
	(
                IsDecl = table_io_decl,
                DeclStr = "decl, "
        ;
                IsDecl = table_io_proc,
                DeclStr = "proc, "
        ),
        (
                IsUnitize = table_io_unitize,
                UnitizeStr = "unitize"
        ;
                IsUnitize = table_io_alone,
                UnitizeStr = "alone"
        ),
	Str = "table_io(" ++ DeclStr ++ UnitizeStr ++ ")".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% THE REMAINDER OF THIS FILE IS JUNK THAT IS NOT USED.
% It has been made obsolete by mercury_to_mercury.m.
% However, the code below handles operator precedence better
% than mercury_to_mercury.m.
%
% % Please note that this code is the property of
% % the University of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
% %
% % All rights are reserved.
% %
% % Author: Philip Dart, 1988
% %		Based on a theme by Lawrence Byrd and Lee Naish.
% %	Fixed again by Lee Naish 9/88
%
% % May bear some vague resemblance to some code written by Lawrence Byrd
% % at Edinburgh a long time ago.
%
% prog_out__writeDCGClause(Head, Body, VarSet) -->
% 	% prog_out__get_op_prec("-->", 1, Prec),
% 	{ Prec = 1199 },
% 	prog_out__qwrite(Prec, VarSet, Head),
% 	io__write_string(" -->"),
% 	prog_out__write_goal(Body, 1, ',', VarSet).
%
% :- type context ---> '(' ; (';') ; (then) ; (else) ; ','.
%
% :- pred prog_out__write_goal(goal, int, context, varset, io, io).
% :- mode prog_out__write_goal(in, in, in, in, di, uo) is det.
%
% prog_out__write_goal(fail, I0, T, _VarSet) -->
% 	prog_out__beforelit(T, I0),
% 	io__write_string("fail").
%
% prog_out__write_goal(true, I0, T, _VarSet) -->
% 	prog_out__beforelit(T, I0),
% 	io__write_string("true").
%
% prog_out__write_goal(some(Vars,Goal), I0, T, VarSet) -->
% 	prog_out__beforelit(T, I0),
% 	io__write_string("some ["),
% 	prog_out__write_var_list(Vars, VarSet),
% 	io__write_string("] ("),
% 	{ I1 is I0 + 1 },
% 	prog_out__write_goal(Goal, I1, '(', VarSet),
% 	io__write_string("\n"),
% 	prog_out__indent(I0),
% 	io__write_string(")").
%
% prog_out__write_goal(all(Vars,Goal), I0, T, VarSet) -->
% 	prog_out__beforelit(T, I0),
% 	io__write_string("all ["),
% 	prog_out__write_var_list(Vars, VarSet),
% 	io__write_string("] ("),
% 	{ I1 is I0 + 1 },
% 	prog_out__write_goal(Goal, I1, '(', VarSet),
% 	io__write_string("\n"),
% 	prog_out__indent(I0),
% 	io__write_string(")").
%
% prog_out__write_goal((P, Q), I0, T, VarSet) -->
% 	prog_out__write_goal(P, I0, T, VarSet),
% 	io__write_string(","),
% 	{if T = (',') then I = I0 else I is I0 + 1},
% 	prog_out__write_goal(Q, I, (','), VarSet).
%
% prog_out__write_goal(if_then_else(Vars,C,A,B), I, T, VarSet) -->
% 	{if T = (then) then I1 is I + 1 else I1 = I},
% 	(if {T = (else)} then
% 		[]
% 	else
% 		io__write_string("\n"),
% 		prog_out__indent(I1)
% 	),
% 	io__write_string(" if "),
% 	prog_out__write_some_vars(VarSet, Vars),
% 	prog_out__write_goal(C, I, '(', VarSet),
% 	io__write_string(" then"),
% 	prog_out__write_goal(A, I1, (then), VarSet),
% 	io__write_string("\n"),
% 	prog_out__indent(I1),
% 	io__write_string("else"),
% 	prog_out__write_goal(B, I1, (else), VarSet),
% 	(if {T = (else)} then
% 		[]
% 	else
% 		io__write_string("\n"),
% 		prog_out__indent(I1),
% 		io__write_string(")")
% 	).
%
% prog_out__write_goal(if_then(Vars,C,A), I, T, VarSet) -->
% 	{if T = (then) then I1 is I + 1 else I1 = I},
% 	(if {T = (else)} then
% 		[]
% 	else
% 		io__write_string("\n"),
% 		prog_out__indent(I1)
% 	),
% 	io__write_string(" if "),
% 	prog_out__write_some_vars(VarSet, Vars),
% 	prog_out__write_goal(C, I, '(', VarSet),
% 	io__write_string(" then"),
% 	prog_out__write_goal(A, I1, (then), VarSet),
% 	(if {T = (else)} then
% 		[]
% 	else
% 		io__write_string("\n"),
% 		prog_out__indent(I1),
% 		io__write_string(")")
% 	).
%
% prog_out__write_goal((P ; Q), I, T, VarSet) -->
% 	(if {T = (;)} then
% 		io__write_string("\t\n"),
% 		prog_out__write_goal(P, I, (;), VarSet)
% 	else
% 		io__write_string("\n"),
% 		prog_out__indent(I),
% 		io__write_string("("),
% 		prog_out__write_goal(P, I, '(', VarSet)
% 	),
% 	io__write_string("\n"),
% 	prog_out__indent(I),
% 	io__write_string(";"),
% 	prog_out__write_goal(Q, I, (;), VarSet),
% 	(if {T = (;)} then
% 		[]
% 	else
% 		io__write_string("\n"),
% 		prog_out__indent(I),
% 		io__write_string(")")
% 	).
%
% prog_out__write_goal(not(A), I, _, VarSet) -->
% 	io__write_string("not("),
% 	prog_out__write_goal(A, I, '(', VarSet),
% 	io__write_string(")").
%
% prog_out__write_goal(call(X), I, T, VarSet) -->
% 	prog_out__beforelit(T, I),
% 		% Pos 1 of (,) has lowest prec of constructs
% 	% prog_out__get_op_prec(",", 1, Prec),
% 	{ Prec = 999 },
% 	prog_out__qwrite(Prec, VarSet, X).
%
% prog_out__write_var_list(_VarSet, Vars) -->
% 	io__write_anything(Vars).
%
% prog_out__write_some_vars(_VarSet, Vars) -->
% 	io__write_string("some "),
% 	io__write_anything(Vars).		% XXX
%
% :- pred prog_out__beforelit(context, int, io__state, io__state).
% :- mode prog_out__beforelit(in, in, di, uo) is det.
%
% prog_out__beforelit('(', _) -->
% 	io__write_string("\t").
% prog_out__beforelit((;), I) -->
% 	io__write_string("\n"),
% 	{ I1 is I + 1 },
% 	prog_out__indent(I1),
% 	io__write_string("\t").
% prog_out__beforelit((then), I) -->
% 	io__write_string("\n"),
% 	{ I1 is I + 1 },
% 	prog_out__indent(I1).
% prog_out__beforelit((else), I) -->
% 	io__write_string("\n"),
% 	{ I1 is I + 1 },
% 	prog_out__indent(I1).
% prog_out__beforelit(',', I) -->
% 	io__write_string("\n"),
% 	prog_out__indent(I).
%
% :- pred prog_out__indent(int, io__state, io__state).
% :- mode prog_out__indent(int, di, uo) is det.
% prog_out__indent(N) -->
% 	(if {N > 0} then
% 		io__write_string("\t"),
% 		{ N1 is N - 1 },
% 		prog_out__indent(N1)
% 	else
% 		[]
% 	).
%
% :- pred prog_out__qwrite(int, varset, term, io__state, io__state).
% :- mode prog_out__qwrite(in, in, in, di, uo) is det.
%
% 	% XXX problems with precedence
%
% prog_out__qwrite(_Prec, VarSet, X) -->
% 	term_io__write_term(VarSet, X).
%
% :- pred prog_out__get_op_prec(string, int, int, io__state, io__state).
% :- mode prog_out__get_op_prec(in, in, out, di, uo) is det.
%
% prog_out__get_op_prec(Op, Pos, Prec) -->
% 	term_io__current_ops(Ops),
% 	{ get_prec_and_type(Op, Ops, Prec1, Type),
% 	  prog_out__op_adj(Pos, Type, Adj),
% 	  Prec is Prec1 - Adj
% 	}.
%
% get_prec_and_type(ThisOp, [Op|Ops], Prec, Type) :-
% 	(if some [Prec1, Type1]
% 		Op = op(Prec1, Type1, ThisOp)
% 	then
% 		Prec = Prec1,
% 		Type = Type1
% 	else
% 		get_prec_and_type(ThisOp, Ops, Prec, Type)
% 	).
%
% :- pred prog_out__op_adj(int, op_type, int).
% :- mode prog_out__op_adj(in, in, out) is det.
%
% prog_out__op_adj(1, xfx, 1).
% prog_out__op_adj(1, xfy, 1).
% prog_out__op_adj(1, fxy, 1).
% prog_out__op_adj(1, fxx, 1).
% prog_out__op_adj(1, yfx, 0).
% % prog_out__op_adj(1, yfy, 0).
% prog_out__op_adj(1, fyx, 0).
% prog_out__op_adj(1, fyy, 0).
% prog_out__op_adj(2, xfx, 1).
% prog_out__op_adj(2, xfy, 0).
% prog_out__op_adj(2, fxy, 0).
% prog_out__op_adj(2, fxx, 1).
% prog_out__op_adj(2, yfx, 1).
% % prog_out__op_adj(2, yfy, 0).
% prog_out__op_adj(2, fyx, 1).
% prog_out__op_adj(2, fyy, 0).
% prog_out__op_adj(1,  xf, 1).
% prog_out__op_adj(1,  fx, 1).
% prog_out__op_adj(1,  yf, 0).
% prog_out__op_adj(1,  fy, 0).
%
% ******************************/
