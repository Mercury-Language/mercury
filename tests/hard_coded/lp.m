%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lp.

:- interface.

:- import_module float.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module term.
:- import_module varset.

:- type coeff == pair(var, float).

:- type equation
    --->    eqn(list(coeff), operator, float).

:- type operator
    --->    (=<)
    ;       (=)
    ;       (>=).

:- type equations == list(equation).

:- type objective == list(coeff).

:- type direction
    --->    max
    ;       min.

:- type lp__result
    --->    unsatisfiable
    ;       satisfiable(float, map(var, float)).

%---------------------------------------------------------------------------%

:- pred lp_solve(equations, direction, objective, varset, lp__result,
    io__state, io__state).
:- mode lp_solve(in, in, in, in, out, di, uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module solutions.

lp_solve(Equations, Dir, Objective, Varset0, Result, IO0, IO) :-
    form_tableau(Equations, Dir, Objective, Varset0,
        VarNumbers, N, M, Tableau0),
    show_tableau(N, M, Tableau0, IO0, IO1),
    simplex(N, M, Tableau0, Tableau, Resolved, IO1, IO2),
    show_tableau(N, M, Tableau, IO2, IO),
    (
        Resolved = yes,
        index(Tableau, N, M, 0, M, ObjVal),
        GetObjVar = (pred(Var::out) is nondet :-
            list__member(Coeff, Objective),
            Coeff = Var - _
        ),
        solutions(GetObjVar, ObjVars),
        map__init(ObjVarVals0),
        list__foldl(objvarval(Tableau, VarNumbers, N, M), ObjVars,
            ObjVarVals0, ObjVarVals),
        Result = satisfiable(ObjVal, ObjVarVals)
    ;
        Resolved = no,
        Result = unsatisfiable
    ).

:- pred form_tableau(equations, direction, objective, varset,
    map(var, int), int, int, tableau).
:- mode form_tableau(in, in, in, in, out, out, out, out) is det.

form_tableau(Equations, Dir, Objective, Varset0,
        VarNumbers, NumNormEqns1, NumVars1, Tableau) :-
    N = NumNormEqns1,
    M = NumVars1,
    normalize_equations(Equations, [], NormalEquations0, Varset0, _Varset),
    list__map(simplify, NormalEquations0, NormalEquations1),
    list__reverse(NormalEquations1, NormalEquations),
    list__length(NormalEquations, NumNormEqns),
    collect_vars(NormalEquations, Objective, Vars),
    set__to_sorted_list(Vars, VarList),
    list__length(VarList, NumVars),
    NumVars1 = NumVars,
    NumNormEqns1 = NumNormEqns,
    init_tableau(NumNormEqns1, NumVars1, Tableau0),
    map__init(VarNumbers0),
    number_vars(VarList, 0, VarNumbers0, VarNumbers),
    (
        Dir = max,
        Neg = (pred(Pair0::in, Pair::out) is det :-
                Pair0 = V - X0,
                X1 = -X0,
                Pair = V - (X1)
        ),
        list__map(Neg, Objective, NegObjective)
    ;
        Dir = min,
        NegObjective = Objective
    ),
    insert_coeffs(NegObjective, 0, VarNumbers, N, M, Tableau0, Tableau1),
    insert_equations(NormalEquations, 1, NumVars1, VarNumbers,
        N, M, Tableau1, Tableau).

:- pred normalize_equations(equations, equations, equations, varset, varset).
:- mode normalize_equations(in, in, out, in, out) is det.

normalize_equations([], NormalEquations, NormalEquations, Varset, Varset).
normalize_equations([Eqn0 | Eqns], NEqns0, NEqns, Varset0, Varset) :-
    Eqn0 = eqn(Coeffs0, Op0, Const0),
    (
        Op0 = (=),
        Varset1 = Varset0,
        NEqns1 = [Eqn0 | NEqns0]
    ;
        Op0 = (=<),
        varset__new_var(Var, Varset0, Varset1),
        Eqn1 = eqn([Var - 1.0 | Coeffs0], (=), Const0),
        NEqns1 = [Eqn1 | NEqns0]
    ;
        Op0 = (>=),
        varset__new_var(Var, Varset0, Varset1),
        Eqn1 = eqn([Var - (-1.0) | Coeffs0], (=), Const0),
        NEqns1 = [Eqn1 | NEqns0]
    ),
    normalize_equations(Eqns, NEqns1, NEqns, Varset1, Varset).

:- pred simplify(equation, equation).
:- mode simplify(in, out) is det.

simplify(eqn(Coeffs0, Op, Const), eqn(Coeffs, Op, Const)) :-
    map__init(CoeffMap0),
    AddCoeff = (pred(Pair::in, Map0::in, Map::out) is det :-
        Pair = Var - Coeff,
        ( map__search(Map0, Var, Acc0) ->
            Acc1 = Acc0
        ;
            Acc1 = 0.0
        ),
        Acc = Acc1 + Coeff,
        map__set(Var, Acc, Map0, Map)
    ),
    list__foldl(AddCoeff, Coeffs0, CoeffMap0, CoeffMap),
    map__to_assoc_list(CoeffMap, Coeffs).

:- pred collect_vars(equations, objective, set(var)).
:- mode collect_vars(in, in, out) is det.

collect_vars(Eqns, Obj, Vars) :-
    GetVar = (pred(Var::out) is nondet :-
        (
            list__member(Eqn, Eqns),
            Eqn = eqn(Coeffs, _, _),
            list__member(Pair, Coeffs),
            Pair = Var - _
        ;
            list__member(Pair, Obj),
            Pair = Var - _
        )
    ),
    solutions(GetVar, VarList),
    set__list_to_set(VarList, Vars).

:- pred number_vars(list(var), int, map(var, int), map(var, int)).
:- mode number_vars(in, in, in, out) is det.

number_vars([], _, VarNumbers, VarNumbers).
number_vars([Var | Vars], N, VarNumbers0, VarNumbers) :-
    map__det_insert(Var, N, VarNumbers0, VarNumbers1),
    N1 = N + 1,
    number_vars(Vars, N1, VarNumbers1, VarNumbers).

:- pred insert_equations(equations, int, int, map(var, int),
    int, int, tableau, tableau).
:- mode insert_equations(in, in, in, in, in, in, in, out) is det.

insert_equations([], _, _, _, _, _, Tableau, Tableau).
insert_equations([Eqn | Eqns], Row, ConstCol, VarNumbers, N, M,
        Tableau0, Tableau) :-
    Eqn = eqn(Coeffs, _Op, Const),
    insert_coeffs(Coeffs, Row, VarNumbers, N, M, Tableau0, Tableau1),
    set_index(Tableau1, N, M, Row, ConstCol, Const, Tableau2),
    Row1 = Row + 1,
    insert_equations(Eqns, Row1, ConstCol, VarNumbers, N, M,
        Tableau2, Tableau).

:- pred insert_coeffs(list(coeff), int, map(var, int), int, int,
    tableau, tableau).
:- mode insert_coeffs(in, in, in, in, in, in, out) is det.

insert_coeffs([], _Row, _VarNumbers, _N, _M, Tableau, Tableau).
insert_coeffs([Coeff | Coeffs], Row, VarNumbers, N, M, Tableau0, Tableau) :-
    Coeff = Var - Const,
    map__lookup(VarNumbers, Var, Col),
    set_index(Tableau0, N, M, Row, Col, Const, Tableau1),
    insert_coeffs(Coeffs, Row, VarNumbers, N, M, Tableau1, Tableau).

:- pred objvarval(tableau, map(var, int), int, int, var,
    map(var, float), map(var, float)).
:- mode objvarval(in, in, in, in, in, in, out) is det.

objvarval(Tableau, VarNumbers, N, M, Var, ObjVarVals0, ObjVarVals) :-
    map__lookup(VarNumbers, Var, Col),
    SelectRow = (pred(VV::out) is nondet :-
        between(1, N, Row),
        index(Tableau, N, M, Row, Col, V),
        V = 1.0,
        index(Tableau, N, M, Row, M, VV)
    ),
    solutions(SelectRow, ObjVarValList),
    (
        ObjVarValList = [ObjVarVal | _]
    ->
        map__set(Var, ObjVarVal, ObjVarVals0, ObjVarVals)
    ;
        error("inconsistent simplex")
    ).

%---------------------------------------------------------------------------%

:- pred simplex(int, int, tableau, tableau, bool, io__state, io__state).
:- mode simplex(in, in, in, out, out, di, uo) is cc_multi.

simplex(N, M, A0, A, Result, IO0, IO) :-
    AllColumns = (pred(Col::out) is nondet :- between(0, M-1, Col)),
    MinAgg = (pred(Col::in, Min0::in, Min::out) is det :-
        (
            Min0 = no,
            index(A0, N, M, 0, Col, MinVal),
            ( MinVal < 0.0 ->
                Min = yes(Col - MinVal)
            ;
                Min = no
            )
        ;
            Min0 = yes(_ - MinVal0),
            index(A0, N, M, 0, Col, CellVal),
            ( CellVal < MinVal0 ->
                Min = yes(Col - CellVal)
            ;
                Min = Min0
            )
        )
    ),
    unsorted_aggregate(AllColumns, MinAgg, no, MinResult),
    (
        MinResult = no,
        A = A0,
        IO = IO0,
        Result = yes
    ;
        MinResult = yes(Q - _Val),
        AllRows = (pred(Row::out) is nondet :- between(1, N, Row)),
        MaxAgg = (pred(Row::in, Max0::in, Max::out) is det :-
            (
                Max0 = no,
                index(A0, N, M, Row, Q, MaxVal),
                ( MaxVal > 0.0 ->
                    index(A0, N, M, Row, M, MVal),
                    CVal = MVal/MaxVal,
                    Max = yes(Row - CVal)
                ;
                    Max = no
                )
            ;
                Max0 = yes(_ - MaxVal0),
                index(A0, N, M, Row, Q, CellVal),
                index(A0, N, M, Row, M, MVal),
                (
                    CellVal > 0.0,
                    MaxVal1 = MVal/CellVal,
                    MaxVal1 =< MaxVal0
                ->
                    Max = yes(Row - MaxVal1)
                ;
                    Max = Max0
                )
            )
        ),
        unsorted_aggregate(AllRows, MaxAgg, no, MaxResult),
        (
            MaxResult = no,
            A = A0,
            IO = IO0,
            Result = no
        ;
            MaxResult = yes(P - _),
            /*
            index(A0, N, M, P, Q, Apq),
            string__format("pivot on (%d, %d) = %f\n",
                [i(P), i(Q), f(Apq)], Str),
            io__write_string(Str, IO0, IO1),
            */
            IO1 = IO0,
            pivot(P, Q, N, M, A0, A1),
            show_tableau(N, M, A1, IO1, IO2),
            simplex(N, M, A1, A, Result, IO2, IO)
        )
    ).

%---------------------------------------------------------------------------%

:- type cell
    --->    cell(int, int).

:- pred pivot(int, int, int, int, tableau, tableau).
:- mode pivot(in, in, in, in, in, out) is cc_multi.

pivot(P, Q, N, M, A0, A) :-
    index(A0, N, M, P, Q, Apq),
    MostCells = (pred(Cell::out) is nondet :-
        between(0, N, J),
        J \= P,
        between(0, M, K),
        K \= Q,
        Cell = cell(J, K)
    ),
    ScaleCell = (pred(Cell::in, T0::in, T::out) is det :-
        Cell = cell(J, K),
        index(T0, N, M, J, K, Ajk),
        index(T0, N, M, J, Q, Ajq),
        index(T0, N, M, P, K, Apk),
        NewAjk = Ajk - Apk * Ajq / Apq,
        set_index(T0, N, M, J, K, NewAjk, T)
    ),
    unsorted_aggregate(MostCells, ScaleCell, A0, A1),
    QColumn = (pred(Cell::out) is nondet :-
        between(0, N, J),
        Cell = cell(J, Q)
    ),
    Zero = (pred(Cell::in, T0::in, T::out) is det :-
        Cell = cell(J, K),
        set_index(T0, N, M, J, K, 0.0, T)
    ),
    aggregate(QColumn, Zero, A1, A2),
    PRow = (pred(K::out) is nondet :- between(0, M, K)),
    ScaleRow = (pred(K::in, T0::in, T::out) is det :-
        index(T0, N, M, P, K, Apk),
        NewApk = Apk / Apq,
        set_index(T0, N, M, P, K, NewApk, T)
    ),
    aggregate(PRow, ScaleRow, A2, A3),
    set_index(A3, N, M, P, Q, 1.0, A).

%---------------------------------------------------------------------------%

:- type tableau == array(float).

:- pred init_tableau(int::in, int::in, tableau::out) is det.

init_tableau(Rows0, Cols0, Tableau) :-
    Rows = Rows0 + 1, Cols = Cols0 + 1,
    NumCells = Rows*Cols,
    array__init(NumCells, 0.0, Tableau).

:- pred index(tableau, int, int, int, int, float).
:- mode index(in, in, in, in, in, out) is det.

index(Tableau, Rows0, Cols0, J, K, R) :-
    _Rows = Rows0 + 1, Cols = Cols0 + 1,
    Index = J * Cols + K,
    array__lookup(Tableau, Index, R).

:- pred set_index(tableau, int, int, int, int, float, tableau).
:- mode set_index(in, in, in, in, in, in, out) is det.

set_index(Tableau0, Rows0, Cols0, J, K, R, Tableau) :-
    _Rows = Rows0 + 1, Cols = Cols0 + 1,
    Index = J * Cols + K,
    mkuniq(Tableau0, Tableau1),
    array__set(Index, R, Tableau1, Tableau).

:- pred mkuniq(array(float)::in, array(float)::array_uo) is det.

:- pragma foreign_proc("C",
    mkuniq(A::in, B::array_uo),
    [will_not_call_mercury, promise_pure],
"
    B = A;
").
:- pragma foreign_proc("Java",
    mkuniq(A::in, B::array_uo),
    [will_not_call_mercury, promise_pure],
"
    B = A;
").
:- pragma foreign_proc("C#",
    mkuniq(A::in, B::array_uo),
    [will_not_call_mercury, promise_pure],
"
    B = A;
").

%---------------------------------------------------------------------------%

:- import_module string.

:- pred show_tableau(int, int, tableau, io__state, io__state).
:- mode show_tableau(in, in, in, di, uo) is det.

show_tableau(_N, _M, _Tableau) --> [].

/*
show_tableau(N, M, Tableau) -->
    { string__format("Tableau (%d, %d):\n", [i(N), i(M)], Str) },
    io__write_string(Str),
    unsorted_aggregate(between(0, N), show_row(Tableau, M)).

:- pred show_row(tableau, int, int, io__state, io__state).
:- mode show_row(in, in, in, di, uo) is cc_multi.

show_row(Tableau, M, Row) -->
    unsorted_aggregate(between(0, M), show_cell(Tableau, Row)),
    io__write_string("\n").

:- pred show_cell(tableau, int, int, io__state, io__state).
:- mode show_cell(in, in, in, di, uo) is cc_multi.

show_cell(Tableau, Row, Col) -->
    { index(Tableau, Row, Col, Val) },
    { string__format("%2.2f\t", [f(Val)], Str) },
    io__write_string(Str).
*/

%---------------------------------------------------------------------------%

:- pred between(int, int, int).
:- mode between(in, in, out) is nondet.

between(Min, Max, I) :-
    Min =< Max,
    (
        I = Min
    ;
        Min1 = Min + 1,
        between(Min1, Max, I)
    ).

%---------------------------------------------------------------------------%
