% Another calculator - parses and evaluates integer expression terms.
% This module demonstrates the use of user-defined operator precedence
% tables with parser__read_term.
%
% Note that unlike calculator.m, the expressions must be terminated with a `.'.
% This version also allows variable assignments of the form `X = Exp.'.
%
% Author: stayl.

% This source file is hereby placed in the public domain.  -stayl.

:- module calculator2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module exception, int, list, map, ops, parser, require.
:- import_module std_util, string, term, term_io, varset.

:- type calc_info == map(string, int).

main --> 
	main_2(map__init).

:- pred main_2(calc_info::in, io::di, io::uo) is cc_multi.

main_2(CalcInfo0) --> 
	io__write_string("calculator> "),
	io__flush_output,
	parser__read_term_with_op_table(calculator_op_table, Res),
	( { Res = error(Msg, _Line) },
		io__write_string(Msg),
		io__nl,
		main
	; { Res = eof },
		io__write_string("EOF\n")
	; { Res = term(VarSet, Term) },
		{
			Term = term__functor(term__atom("="),
				[term__variable(Var), ExprTerm0], _)
		->
			ExprTerm = ExprTerm0,
			varset__lookup_name(VarSet, Var, VarName),
			SetVar = yes(VarName)
		;
			ExprTerm = Term,
			SetVar = no
		},

		{ try(
			(pred(Num0::out) is det :-
				Num0 = eval_expr(CalcInfo0, VarSet, ExprTerm)
			), EvalResult) },
		(
			{ EvalResult = succeeded(Num) },
			io__write_int(Num),
			io__nl,
			{ SetVar = yes(VarToSet) ->
				map__set(CalcInfo0, VarToSet, Num, CalcInfo)
			;
				CalcInfo = CalcInfo0
			}
		;
			{ EvalResult = exception(Exception) },
			{ CalcInfo = CalcInfo0 },
			( { univ_to_type(Exception, EvalError) } ->
				report_eval_error(EvalError)
			;
				{ rethrow(EvalResult) }
			)
		),

		% recursively call ourself for the next term(s)
		main_2(CalcInfo)
	).

:- pred report_eval_error(eval_error::in, io::di, io::uo) is det.

report_eval_error(unknown_operator(Name, Arity)) -->
	io__write_string("unknown operator `"),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n").
report_eval_error(unknown_variable(Name)) -->
	io__write_string("unknown variable `"),
	io__write_string(Name),
	io__write_string("'.\n").
report_eval_error(unexpected_const(Const)) -->
	io__write_string("unexpected "),
	( { Const = term__integer(_) },
		{ error("report_eval_error") }
	; { Const = term__float(Float) },
		io__write_string(" float `"),
		io__write_float(Float),
		io__write_string("'")
	; { Const = term__string(String) },
		io__write_string(" string """),
		io__write_string(String),
		io__write_string("""")
	; { Const = term__atom(_) },
		{ error("report_eval_error") }
	),
	io__nl.

:- func eval_expr(calc_info, varset, term) = int.

eval_expr(CalcInfo, VarSet, term__variable(Var)) = Res :-
	varset__lookup_name(VarSet, Var, VarName),
	( map__search(CalcInfo, VarName, Res0) ->
		Res = Res0
	;
		throw(unknown_variable(VarName))
	).
eval_expr(CalcInfo, VarSet, term__functor(term__atom(Op), Args, _)) = Res :-
	(
		( Args = [Arg1],
			Res0 = eval_unop(Op, eval_expr(CalcInfo, VarSet, Arg1))
		; Args = [Arg1, Arg2],
			Res0 = eval_binop(Op,
				eval_expr(CalcInfo, VarSet, Arg1),
				eval_expr(CalcInfo, VarSet, Arg2))
		)
	->
		Res = Res0
	;
		throw(unknown_operator(Op, list__length(Args)))
	).
eval_expr(_, _, term__functor(term__integer(Int), _, _)) = Int.
eval_expr(_, _, term__functor(term__float(Float), _, Context)) =
		throw(unexpected_const(term__float(Float)) - Context).
eval_expr(_, _, term__functor(term__string(String), _, Context)) =
		throw(unexpected_const(term__string(String)) - Context).

:- func eval_unop(string, int) = int is semidet.

eval_unop("-", Num) = -Num.
eval_unop("+", Num) = Num.

:- func eval_binop(string, int, int) = int is semidet.

eval_binop("-", Num1, Num2) = Num1 - Num2.
eval_binop("+", Num1, Num2) = Num1 + Num2.
eval_binop("*", Num1, Num2) = Num1 * Num2.
eval_binop("//", Num1, Num2) = Num1 // Num2.

:- type eval_error
	--->	unknown_operator(
			string,		% name
			int		% arity
		)
	;	unknown_variable(string)
	;	unexpected_const(term__const)
	.

:- type calculator_op_table ---> calculator_op_table.

:- instance ops__op_table(calculator_op_table) where [
	ops__lookup_infix_op(_, "//", 400, y, x),
	ops__lookup_infix_op(_, "*", 400, y, x),
	ops__lookup_infix_op(_, "+", 500, y, x),
	ops__lookup_infix_op(_, "-", 500, y, x),
	ops__lookup_infix_op(_, "=", 700, x, x),

	ops__lookup_operator_term(_, _, _, _) :- fail,

	ops__lookup_prefix_op(_, "-", 200, x),
	ops__lookup_prefix_op(_, "+", 500, x),

	ops__lookup_postfix_op(_, _, _, _) :- fail,
	ops__lookup_binary_prefix_op(_, _, _, _, _) :- fail,

	ops__lookup_op(Table, Op) :- ops__lookup_infix_op(Table, Op, _, _, _),
	ops__lookup_op(Table, Op) :- ops__lookup_prefix_op(Table, Op, _, _),
	ops__lookup_op(Table, Op) :-
		ops__lookup_binary_prefix_op(Table, Op, _, _, _),
	ops__lookup_op(Table, Op) :- ops__lookup_postfix_op(Table, Op, _, _),

	ops__max_priority(_) = 700,
	ops__arg_priority(Table) = ops__max_priority(Table) + 1
].

