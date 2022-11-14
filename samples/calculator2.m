%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Another calculator - parses and evaluates integer expression terms.
% This module demonstrates the use of user-defined operator precedence
% tables with mercury_term_parser.read_term.
%
% Note that unlike calculator.m, the expressions must be terminated with a `.'.
% This version also allows variable assignments of the form `X = Exp.'.
%
% Author: stayl.
%
% This source file is hereby placed in the public domain.  -stayl.
%
%-----------------------------------------------------------------------------%

:- module calculator2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module mercury_term_parser.
:- import_module ops.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_int.
:- import_module term_io.
:- import_module univ.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type calc_info == map(string, int).

main(!IO) :-
    main_loop(map.init, !IO).

:- pred main_loop(calc_info::in, io::di, io::uo) is cc_multi.

main_loop(!.CalcInfo, !IO) :-
    io.write_string("calculator> ", !IO),
    io.flush_output(!IO),
    mercury_term_parser.read_term_with_op_table(calculator_op_table, Res, !IO),
    (
        Res = eof,
        io.write_string("EOF\n", !IO)
    ;
        Res = error(Msg, _Line),
        io.format("%s\n", [s(Msg)], !IO),
        main_loop(!.CalcInfo, !IO)
    ;
        Res = term(VarSet, Term),
        ( if
            Term = term.functor(term.atom("="),
                [term.variable(Var, _Context), ExprTerm0], _)
        then
            ExprTerm = ExprTerm0,
            varset.lookup_name(VarSet, Var, VarName),
            SetVar = yes(VarName)
        else
            ExprTerm = Term,
            SetVar = no
        ),

        try(
            ( pred(Num0::out) is det :-
                Num0 = eval_expr(!.CalcInfo, VarSet, ExprTerm)
            ), EvalResult),
        (
            EvalResult = succeeded(Num),
            io.format("%d\n", [i(Num)], !IO),
            (
                SetVar = yes(VarToSet),
                map.set(VarToSet, Num, !CalcInfo)
            ;
                SetVar = no
            )
        ;
            EvalResult = exception(Exception),
            ( if univ_to_type(Exception, EvalError) then
                report_eval_error(EvalError, !IO)
            else
                rethrow(EvalResult)
            )
        ),

        % Recursively call ourself for the next term(s).
        main_loop(!.CalcInfo, !IO)
    ).

:- pred report_eval_error(eval_error::in, io::di, io::uo) is det.

report_eval_error(Error, !IO) :-
    (
        Error = unknown_operator(Name, Arity),
        io.format("unknown operator `%s/%d'.\n", [s(Name), i(Arity)], !IO)
    ;
        Error = unknown_variable(Name),
        io.format("unknown variable `%s'.\n", [s(Name)], !IO)
    ;
        Error = unexpected_const(Const),
        io.write_string("unexpected ", !IO),
        (
            Const = term.float(Float),
            io.format("unexpected float `%f'\n", [f(Float)], !IO)
        ;
            Const = term.string(String),
            io.format("unexpected string ""%s""\n", [s(String)], !IO)
        ;
            ( Const = term.integer(_, _, _, _)
            ; Const = term.atom(_)
            ; Const = term.implementation_defined(_)
            ),
            error("report_eval_error")
        )
    ).

:- func eval_expr(calc_info, varset, term) = int.

eval_expr(CalcInfo, VarSet, Term) = Res :-
    (
        Term = term.variable(Var, _Context),
        varset.lookup_name(VarSet, Var, VarName),
        ( if map.search(CalcInfo, VarName, Res0) then
            Res = Res0
        else
            throw(unknown_variable(VarName))
        )
    ;
        Term = term.functor(term.atom(Op), Args, _),
        ( if
            (
                Args = [Arg1],
                Res0 = eval_unop(Op, eval_expr(CalcInfo, VarSet, Arg1))
            ;
                Args = [Arg1, Arg2],
                Res0 = eval_binop(Op,
                    eval_expr(CalcInfo, VarSet, Arg1),
                    eval_expr(CalcInfo, VarSet, Arg2))
            )
        then
            Res = Res0
        else
            throw(unknown_operator(Op, list.length(Args)))
        )
    ;
        Term = term.functor(Const, _, Context),
        Const = term.integer(_, _, _, _),
        ( if term_int.term_to_int(Term, Int) then
            Res = Int
        else
            throw(unexpected_const(Const) - Context)
        )
    ;
        Term = term.functor(term.float(Float), _, Context),
        throw(unexpected_const(term.float(Float)) - Context)
    ;
        Term = term.functor(term.string(String), _, Context),
        throw(unexpected_const(term.string(String)) - Context)
    ;
        Term =  term.functor(ImplDefConst, _, Context),
        ImplDefConst = term.implementation_defined(_),
        throw(unexpected_const(ImplDefConst) - Context)
    ).

:- func eval_unop(string, int) = int is semidet.

eval_unop("-", Num) = -Num.
eval_unop("+", Num) = Num.

:- func eval_binop(string, int, int) = int is semidet.

eval_binop("-", Num1, Num2) = Num1 - Num2.
eval_binop("+", Num1, Num2) = Num1 + Num2.
eval_binop("*", Num1, Num2) = Num1 * Num2.
eval_binop("//", Num1, Num2) = Num1 // Num2.

:- type eval_error
    --->    unknown_operator(
                string,     % name
                int         % arity
            )
    ;       unknown_variable(string)
    ;       unexpected_const(term.const).

:- type calculator_op_table
    --->    calculator_op_table.

:- pred ops_table(string::in, op_infos::out) is semidet.

ops_table(Op, OpInfos) :-
    (
        (Op = "//" ; Op = "*"),
        Infix = in(prio(1100u), arg_gt, arg_ge),
        OpInfos = op_infos(Infix, no_bin_pre, no_pre, no_post)
    ;
        (Op = "+" ; Op = "-"),
        Infix = in(prio(1000u), arg_gt, arg_ge),
        Prefix = pre(prio(1000u), arg_ge),
        OpInfos = op_infos(Infix, no_bin_pre, Prefix, no_post)
    ;
        Op = "=",
        Infix = in(prio(800u), arg_ge, arg_ge),
        OpInfos = op_infos(Infix, no_bin_pre, no_pre, no_post)
    ).

:- instance ops.op_table(calculator_op_table) where [
    ( ops.lookup_infix_op(_, Op, Priority, GtOrGeA, GtOrGeB) :-
        calculator2.ops_table(Op, OpInfos),
        OpInfos ^ oi_infix = in(Priority, GtOrGeA, GtOrGeB)
    ),

    ( ops.lookup_prefix_op(_, Op, Priority, GtOrGeA) :-
        calculator2.ops_table(Op, OpInfos),
        OpInfos ^ oi_prefix = pre(Priority, GtOrGeA)
    ),

    ops.lookup_postfix_op(_, _, _, _) :- fail,
    ops.lookup_binary_prefix_op(_, _, _, _, _) :- fail,

    ops.is_op(_Table, Op) :-
        ops_table(Op, _OpInfos),

    ops.lookup_op_infos(_, Op, OpInfos) :-
        ops_table(Op, OpInfos),

    ops.lookup_operator_term(_, _, _, _) :- fail,

    ops.tightest_op_priority(_) = prio(1300u),
    ops.loosest_op_priority(_) = prio(800u),
    ops.universal_priority(_) = prio(799u),
    ops.comma_priority(_) = prio(800u),
    ops.arg_priority(_) = prio(799u)
].

%-----------------------------------------------------------------------------%
:- end_module calculator2.
%-----------------------------------------------------------------------------%
