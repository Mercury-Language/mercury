%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module imperative_solve_impl.

:- interface.

:- import_module assoc_list.

:- type var == string.
:- type env == assoc_list(var, int).

:- pred power_2_5(env::in, env::out) is semidet.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module pair.

power_2_5(!Env) :-
    power(2, 5, !Env).

:- pred store(env::in, var::in, int::in, env::out) is det.

store([], Key, Value, [Key - Value]).
store([Key0 - Value0 | T0], Key1, Value1, [Key - Value | T]) :-
    ( if Key0 = Key1 then
        Key = Key0,
        Value = Value1,
        T = T0
    else
        Key = Key0,
        Value = Value0,
        store(T0, Key1, Value1, T)
    ).

:- pred lookup(var::in, env::in, int::out) is semidet.

lookup(Key1, [Key2 - Value0 | T], Value) :-
    ( if Key1 = Key2 then
        Value = Value0
    else
        lookup(Key1, T, Value)
    ).

:- pred power(int::in, int::in, env::in, env::out) is semidet.

power(Base, Power, E1, EOut) :-
    execute_statement(let("base", int(Base)), E1, E2),
    execute_statement(let("power", int(Power)), E2, E3),
    execute_statement(seq(seq(let("x", int(1)), let("result", var("base"))),
       while_do('<'(var("x"), var("power")),
          seq(let("x", '+'(var("x"), int(1))),
              let("result", '*'(var("result"), var("base")))))),
                   E3, EOut).

:- type stmt
    --->    null
    ;       let(var, expr)
    ;       if(test, stmt, stmt)
    ;       repeat_until(stmt, test)
    ;       while_do(test, stmt)
    ;       seq(stmt, stmt).

:- pred execute_statement(stmt::in, env::in, env::out) is semidet.

execute_statement(null, Env, Env).
execute_statement(let(V, Expr), Env, NEnv) :-
    eval_expression(Expr, Env, Val),
    store(Env, V, Val, NEnv).
execute_statement(if(Tst, Thn, Els), Env, NEnv) :-
    eval_test(Tst, Env, Bool),
    execute_cond_continuation(Bool, Thn, Els, Env, NEnv).
execute_statement(repeat_until(Loop, Tst), Env, NEnv) :-
    execute_statement(Loop, Env, IntEnv),
    eval_test(Tst, IntEnv, Bool),
    execute_cond_continuation(Bool, null,
    repeat_until(Loop, Tst), IntEnv, NEnv).
execute_statement(while_do(Tst, Loop), Env, NEnv) :-
    eval_test(Tst, Env, Bool),
    execute_cond_continuation(Bool, seq(Loop, while_do(Tst, Loop)),
        null, Env, NEnv).
execute_statement(seq(St1, St2), Env, NEnv) :-
    execute_statement(St1, Env, IntEnv),
    execute_statement(St2, IntEnv, NEnv).

:- pred execute_cond_continuation(bool::in, stmt::in, stmt::in,
    env::in, env::out) is semidet.

execute_cond_continuation(yes, Thn, _Els, Env, NEnv) :-
    execute_statement(Thn, Env, NEnv).
execute_cond_continuation(no, _Thn, Els, Env, NEnv) :-
    execute_statement(Els, Env, NEnv).

:- type test
    --->    '<'(expr, expr)
    ;       '=<'(expr, expr)
    ;       '>'(expr, expr)
    ;       '>='(expr, expr).

:- pred eval_test(test::in, env::in, bool::out) is semidet.

eval_test('<'(X, Y), Env, Bool) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    get_bool(VX < VY, Bool).
eval_test('=<'(X, Y), Env, Bool) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    get_bool(VX =< VY, Bool).
eval_test('>'(X, Y), Env, Bool) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    get_bool(VX > VY, Bool).
eval_test('>='(X, Y), Env, Bool) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    get_bool(VX >= VY, Bool).

:- pred get_bool((pred)::in((pred) is semidet), bool::out) is det.

get_bool(Tst, Res) :-
    ( if call(Tst) then
        Res = yes
    else
        Res = no
    ).

:- type expr
    --->    int(int)
    ;       var(string)
    ;       '+'(expr, expr)
    ;       '-'(expr, expr)
    ;       '*'(expr, expr)
    ;       '/'(expr, expr).

:- pred eval_expression(expr::in, env::in, int::out) is semidet.

eval_expression(int(X), _Env, X).
eval_expression(var(V), Env, Val) :-
    lookup(V, Env, Val).
eval_expression('+'(X, Y), Env, Val) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    Val = VX + VY.
eval_expression('-'(X, Y), Env, Val) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    Val = VX - VY.
eval_expression('*'(X, Y), Env, Val) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    Val = VX * VY.
eval_expression('/'(X, Y), Env, Val) :-
    eval_expression(X, Env, VX),
    eval_expression(Y, Env, VY),
    Val = VX // VY.
