%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Another calculator - parses and evaluates integer expression terms.
% This module demonstrates the use of user-defined operator precedence
% tables with parser.read_term.
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
:- import_module ops.
:- import_module pair.
:- import_module parser.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module univ.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type calc_info == map(string, int).

main(!IO) :-
    main_2(map.init, !IO).

:- pred main_2(calc_info::in, io::di, io::uo) is cc_multi.

main_2(CalcInfo0, !IO) :-
    io.write_string("calculator> ", !IO),
    io.flush_output(!IO),
    parser.read_term_with_op_table(calculator_op_table, Res, !IO),
    (
        Res = error(Msg, _Line),
        io.write_string(Msg, !IO),
        io.nl(!IO),
        main(!IO)
    ;
        Res = eof,
        io.write_string("EOF\n", !IO)
    ;
        Res = term(VarSet, Term),
        (
            Term = term.functor(term.atom("="),
                [term.variable(Var, _Context), ExprTerm0], _)
        ->
            ExprTerm = ExprTerm0,
            varset.lookup_name(VarSet, Var, VarName),
            SetVar = yes(VarName)
        ;
            ExprTerm = Term,
            SetVar = no
        ),

        try(
            ( pred(Num0::out) is det :-
                Num0 = eval_expr(CalcInfo0, VarSet, ExprTerm)
            ), EvalResult),
        (
            EvalResult = succeeded(Num),
            io.write_int(Num, !IO),
            io.nl(!IO),
            ( SetVar = yes(VarToSet) ->
                map.set(VarToSet, Num, CalcInfo0, CalcInfo)
            ;
                CalcInfo = CalcInfo0
            )
        ;
            EvalResult = exception(Exception),
            CalcInfo = CalcInfo0,
            ( univ_to_type(Exception, EvalError) ->
                report_eval_error(EvalError, !IO)
            ;
                rethrow(EvalResult)
            )
        ),

        % Recursively call ourself for the next term(s).
        main_2(CalcInfo, !IO)
    ).

:- pred report_eval_error(eval_error::in, io::di, io::uo) is det.

report_eval_error(unknown_operator(Name, Arity), !IO) :-
    io.write_string("unknown operator `", !IO),
    io.write_string(Name, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string("'.\n", !IO).
report_eval_error(unknown_variable(Name), !IO) :-
    io.write_string("unknown variable `", !IO),
    io.write_string(Name, !IO),
    io.write_string("'.\n", !IO).
report_eval_error(unexpected_const(Const), !IO) :-
    io.write_string("unexpected ", !IO),
    (
        Const = term.float(Float),
        io.write_string(" float `", !IO),
        io.write_float(Float, !IO),
        io.write_string("'", !IO)
    ;
        Const = term.string(String),
        io.write_string(" string """, !IO),
        io.write_string(String, !IO),
        io.write_string("""", !IO)
    ;
        ( Const = term.integer(_)
        ; Const = term.atom(_)
        ; Const = term.implementation_defined(_)
        ),
        error("report_eval_error")
    ),
    io.nl(!IO).

:- func eval_expr(calc_info, varset, term) = int.

eval_expr(CalcInfo, VarSet, term.variable(Var, _Context)) = Res :-
    varset.lookup_name(VarSet, Var, VarName),
    ( map.search(CalcInfo, VarName, Res0) ->
        Res = Res0
    ;
        throw(unknown_variable(VarName))
    ).
eval_expr(CalcInfo, VarSet, term.functor(term.atom(Op), Args, _)) = Res :-
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
        throw(unknown_operator(Op, list.length(Args)))
    ).
eval_expr(_, _, term.functor(term.integer(Int), _, _)) = Int.
eval_expr(_, _, term.functor(term.float(Float), _, Context)) =
        throw(unexpected_const(term.float(Float)) - Context).
eval_expr(_, _, term.functor(term.string(String), _, Context)) =
        throw(unexpected_const(term.string(String)) - Context).
eval_expr(_,  _, term.functor(ImplDefConst, _, Context)) = _ :-
    ImplDefConst = term.implementation_defined(_),
    throw(unexpected_const(ImplDefConst) - Context).

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

:- type calculator_op_table ---> calculator_op_table.

:- pred calculator2.ops_table(string::in, op_info::out, list(op_info)::out)
    is semidet.
  
calculator2.ops_table("//", op_info(infix(y, x), 400), []).
calculator2.ops_table("*",  op_info(infix(y, x), 400), []).
calculator2.ops_table("+",  op_info(infix(y, x), 500),
    [op_info(prefix(x), 500)]).
calculator2.ops_table("-",  op_info(infix(y, x), 500),
    [op_info(prefix(x), 200)]).
calculator2.ops_table("=", op_info(infix(x, x), 700), []).

:- instance ops.op_table(calculator_op_table) where [
    
    ( ops.lookup_infix_op(_, Op, Priority, LeftAssoc, RightAssoc) :-
        calculator2.ops_table(Op, Info, _),
        Info = op_info(infix(LeftAssoc, RightAssoc), Priority)
    ),

    ops.lookup_operator_term(_, _, _, _) :- fail,

    ( ops.lookup_prefix_op(_, Op, Priority, LeftAssoc) :-
        calculator2.ops_table(Op, _, OtherInfo),
        OtherInfo = [op_info(prefix(LeftAssoc), Priority)]
    ),

    ops.lookup_postfix_op(_, _, _, _) :- fail,
    ops.lookup_binary_prefix_op(_, _, _, _, _) :- fail,

    ops.lookup_op(Table, Op) :- ops.lookup_infix_op(Table, Op, _, _, _),
    ops.lookup_op(Table, Op) :- ops.lookup_prefix_op(Table, Op, _, _),
    ops.lookup_op(Table, Op) :-
        ops.lookup_binary_prefix_op(Table, Op, _, _, _),
    ops.lookup_op(Table, Op) :- ops.lookup_postfix_op(Table, Op, _, _),
    
    ops.lookup_op_infos(_, Op, OpInfo, OtherInfos) :-
        calculator2.ops_table(Op, OpInfo, OtherInfos),

    ops.max_priority(_) = 700,
    ops.arg_priority(Table) = ops.max_priority(Table) + 1
].

%-----------------------------------------------------------------------------%
:- end_module calculator2.
%-----------------------------------------------------------------------------%
