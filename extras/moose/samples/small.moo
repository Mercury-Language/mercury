:- module small.

:- parse(program/1, token, ('$'), xx, in, out).

:- rule program(list(defn)).
program(Defs) --->
	(
		definition(Def),
		{ Defs = [Def] }
	;
		program(Defs0), definition(Def),
		{ append(Defs0, [Def], Defs) }
	).

:- rule definition(defn).
definition(Def) --->
	(
		globals(Glob), { Def = glob(Glob) }
	;
		function(Fun), { Def = fun(Fun) }
	).

:- rule globals(pair((type), list(id))).
globals(Type - Vars)	--->	type(Type), vars(Vars), [';'].

:- rule function(fun).
function(Fun) --->
	type(RType), [id(Name)], ['('], parameters(Params), [')'],
	compound(Stmnt),
	{ Fun = fun(Name, RType, Params, Stmnt) }.

:- rule parameters(list(pair((type), id))).
parameters([]) --->	[].
parameters(PList) --->	parameter_list(PList).

:- rule parameter_list(list(pair((type), id))).
parameter_list(Params) --->
	(
		parameter(Param),
		{ Params = [Param] }
	;
		parameter(Param), [','], parameter_list(Params0),
		{ Params = [Params0|Params] }
	).

:- rule parameter(pair((type), id)).
parameter(Type - Id) --->
	type(Type), var(Id).

:- rule compound(statement).
compound(compound(Statements)) --->
	['{'], statements(Statements), ['}'].

:- rule statements(list(statement)).
statements(Statements) --->
	(
		statement(Statement),
		{ Statements = [Statement] }
	;
		statements(Statements0), statement(Statement),
		{ Statements = [Statement|Statements0] }
	).

:- rule statement(statement).
statement(Stmnt) --->
		compound(Stmnt)
	;	ifthenelse(Stmnt)
	;	while(Stmnt)
	;	assignment(Stmnt)
	.

:- rule ifthenelse(statement).
ifthenelse(Stmnt) --->
	(
		['if'], ['('], expression(Cond), [')'], statement(Then),
		{ Stmnt = ite(Cond, Then, compound([])) }
	;
		['if'], ['('], expression(Cond), [')'],
		statement(Then), ['else'], statement(Else),
		{ Stmnt = ite(Cond, Then, Else) }
	).

:- rule while(statement).
while(Stmnt)	--->
	['while'], ['('], expression(Cond), [')'], statement(Body),
	{ Stmnt = while(Cond, Body) }.

:- rule assignment(statement).
assignment(assign(Var, Expr)) --->
	var(Var), ['='], expression(Expr).

:- rule expression(expression).
expression(E) --->	expression(E0), ['+'], term(E1), { E = E0 + E1 }.
expression(E) --->	expression(E0), ['-'], term(E1), { E = E0 - E1 }.
expression(E) --->	term(E).

:- rule term(expression).
term(E) ---> term(E0), ['*'], factor(E1), { E = E0 * E1 }.
term(E) ---> term(E0), ['/'], factor(E1), { E = E0 / E1 }.
term(E) ---> factor(E).

:- rule factor(expression).
factor(E) --->
	(
		[id(Var)], { E = var(Var) }
	;
		[num(Num)], { E = const(Num) }
	;
		[id(Func)], ['('], vars(Args), [')'],
		{ E = fun(Func, Args) }
	;
		['('], expression(E), [')']
	).

:- rule vars(list(id)).
vars([])	--->	[].
vars(Vars)	---> vars1(Vars).

:- rule vars1(list(id)).
vars1(Vars) --->
	(
		var(Var),
		{ Vars = [Var] }
	;
		var(Var), [','], vars1(Vars0),
		{ Vars = [Var|Vars0] }
	).

:- rule var(id).
var(Id)	--->	[id(Id)].

:- rule type(type).
type(Type)	--->
	(
		[int],
		{ Type = int }
	;
		['('], types(Types), [')'],
		{ ( Types = [Type0] ->
			Type = Type0
		;
			Type = tuple(Types)
		) }
	).

:- rule types(list(type)).
types(Types)	--->
	(
		type(Type),
		{ Types = [Type] }
	;
		types(Types0), type(Type),
		{ append(Types0, [Type], Types) }
	).

