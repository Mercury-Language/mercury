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

:- type lp_result
    --->    unsatisfiable
    ;       satisfiable(float, map(var, float)).

%---------------------------------------------------------------------------%

:- pred lp_solve(equations::in, direction::in, objective::in, varset::in,
    lp_result::out, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module solutions.

lp_solve(Equations, Dir, Objective, Varset0, Result, !IO) :-
    form_tableau(Equations, Dir, Objective, Varset0,
        VarNumbers, N, M, Tableau0),
    show_tableau(N, M, Tableau0, !IO),
    simplex(N, M, Tableau0, Tableau, Resolved, !IO),
    show_tableau(N, M, Tableau, !IO),
    (
        Resolved = yes,
        index(Tableau, N, M, 0, M, ObjVal),
        GetObjVar =
            ( pred(Var::out) is nondet :-
                list.member(Coeff, Objective),
                Coeff = Var - _
            ),
        solutions(GetObjVar, ObjVars),
        map.init(ObjVarVals0),
        list.foldl(objvarval(Tableau, VarNumbers, N, M), ObjVars,
            ObjVarVals0, ObjVarVals),
        Result = satisfiable(ObjVal, ObjVarVals)
    ;
        Resolved = no,
        Result = unsatisfiable
    ).

:- pred form_tableau(equations::in, direction::in, objective::in, varset::in,
    map(var, int)::out, int::out, int::out, tableau::out) is det.

form_tableau(Equations, Dir, Objective, Varset0,
        VarNumbers, NumNormEqns1, NumVars1, Tableau) :-
    N = NumNormEqns1,
    M = NumVars1,
    normalize_equations(Equations, [], NormalEquations0, Varset0, _Varset),
    list.map(simplify, NormalEquations0, NormalEquations1),
    list.reverse(NormalEquations1, NormalEquations),
    list.length(NormalEquations, NumNormEqns),
    collect_vars(NormalEquations, Objective, Vars),
    set.to_sorted_list(Vars, VarList),
    list.length(VarList, NumVars),
    NumVars1 = NumVars,
    NumNormEqns1 = NumNormEqns,
    init_tableau(NumNormEqns1, NumVars1, Tableau0),
    map.init(VarNumbers0),
    number_vars(VarList, 0, VarNumbers0, VarNumbers),
    (
        Dir = max,
        Neg =
            ( pred(Pair0::in, Pair::out) is det :-
                Pair0 = V - X0,
                X1 = -X0,
                Pair = V - (X1)
            ),
        list.map(Neg, Objective, NegObjective)
    ;
        Dir = min,
        NegObjective = Objective
    ),
    insert_coeffs(NegObjective, 0, VarNumbers, N, M, Tableau0, Tableau1),
    insert_equations(NormalEquations, 1, NumVars1, VarNumbers,
        N, M, Tableau1, Tableau).

:- pred normalize_equations(equations::in, equations::in, equations::out,
    varset::in, varset::out) is det.

normalize_equations([], NormalEquations, NormalEquations, Varset, Varset).
normalize_equations([Eqn0 | Eqns], NEqns0, NEqns, Varset0, Varset) :-
    Eqn0 = eqn(Coeffs0, Op0, Const0),
    (
        Op0 = (=),
        Varset1 = Varset0,
        NEqns1 = [Eqn0 | NEqns0]
    ;
        Op0 = (=<),
        varset.new_var(Var, Varset0, Varset1),
        Eqn1 = eqn([Var - 1.0 | Coeffs0], (=), Const0),
        NEqns1 = [Eqn1 | NEqns0]
    ;
        Op0 = (>=),
        varset.new_var(Var, Varset0, Varset1),
        Eqn1 = eqn([Var - (-1.0) | Coeffs0], (=), Const0),
        NEqns1 = [Eqn1 | NEqns0]
    ),
    normalize_equations(Eqns, NEqns1, NEqns, Varset1, Varset).

:- pred simplify(equation::in, equation::out) is det.

simplify(eqn(Coeffs0, Op, Const), eqn(Coeffs, Op, Const)) :-
    map.init(CoeffMap0),
    AddCoeff =
        ( pred(Pair::in, Map0::in, Map::out) is det :-
            Pair = Var - Coeff,
            ( if map.search(Map0, Var, Acc0) then
                Acc1 = Acc0
            else
                Acc1 = 0.0
            ),
            Acc = Acc1 + Coeff,
            map.set(Var, Acc, Map0, Map)
        ),
    list.foldl(AddCoeff, Coeffs0, CoeffMap0, CoeffMap),
    map.to_assoc_list(CoeffMap, Coeffs).

:- pred collect_vars(equations::in, objective::in, set(var)::out) is det.

collect_vars(Eqns, Obj, Vars) :-
    GetVar =
        ( pred(Var::out) is nondet :-
            (
                list.member(Eqn, Eqns),
                Eqn = eqn(Coeffs, _, _),
                list.member(Pair, Coeffs),
                Pair = Var - _
            ;
                list.member(Pair, Obj),
                Pair = Var - _
            )
        ),
    solutions(GetVar, VarList),
    set.list_to_set(VarList, Vars).

:- pred number_vars(list(var)::in, int::in,
    map(var, int)::in, map(var, int)::out) is det.

number_vars([], _, !VarNumbers).
number_vars([Var | Vars], N, !VarNumbers) :-
    map.det_insert(Var, N, !VarNumbers),
    number_vars(Vars, N + 1, !VarNumbers).

:- pred insert_equations(equations::in, int::in, int::in, map(var, int)::in,
    int::in, int::in, tableau::in, tableau::out) is det.

insert_equations([], _, _, _, _, _, !Tableau).
insert_equations([Eqn | Eqns], Row, ConstCol, VarNumbers, N, M, !Tableau) :-
    Eqn = eqn(Coeffs, _Op, Const),
    insert_coeffs(Coeffs, Row, VarNumbers, N, M, !Tableau),
    set_index(N, M, Row, ConstCol, Const, !Tableau),
    insert_equations(Eqns, Row + 1, ConstCol, VarNumbers, N, M, !Tableau).

:- pred insert_coeffs(list(coeff)::in, int::in, map(var, int)::in,
    int::in, int::in, tableau::in, tableau::out) is det.

insert_coeffs([], _Row, _VarNumbers, _N, _M, !Tableau).
insert_coeffs([Coeff | Coeffs], Row, VarNumbers, N, M, !Tableau) :-
    Coeff = Var - Const,
    map.lookup(VarNumbers, Var, Col),
    set_index(N, M, Row, Col, Const, !Tableau),
    insert_coeffs(Coeffs, Row, VarNumbers, N, M, !Tableau).

:- pred objvarval(tableau::in, map(var, int)::in, int::in, int::in, var::in,
    map(var, float)::in, map(var, float)::out) is det.

objvarval(Tableau, VarNumbers, N, M, Var, !ObjVarVals) :-
    map.lookup(VarNumbers, Var, Col),
    SelectRow =
        ( pred(VV::out) is nondet :-
            between(1, N, Row),
            index(Tableau, N, M, Row, Col, V),
            V = 1.0,
            index(Tableau, N, M, Row, M, VV)
        ),
    solutions(SelectRow, ObjVarValList),
    ( if ObjVarValList = [ObjVarVal | _] then
        map.set(Var, ObjVarVal, !ObjVarVals)
    else
        error("inconsistent simplex")
    ).

%---------------------------------------------------------------------------%

:- pred simplex(int::in, int::in, tableau::in, tableau::out, bool::out,
    io::di, io::uo) is cc_multi.

simplex(N, M, A0, A, Result, !IO) :-
    AllColumns = (pred(Col::out) is nondet :- between(0, M-1, Col)),
    MinAgg =
        ( pred(Col::in, Min0::in, Min::out) is det :-
            (
                Min0 = no,
                index(A0, N, M, 0, Col, MinVal),
                ( if MinVal < 0.0 then
                    Min = yes(Col - MinVal)
                else
                    Min = no
                )
            ;
                Min0 = yes(_ - MinVal0),
                index(A0, N, M, 0, Col, CellVal),
                ( if CellVal < MinVal0 then
                    Min = yes(Col - CellVal)
                else
                    Min = Min0
                )
            )
        ),
    unsorted_aggregate(AllColumns, MinAgg, no, MinResult),
    (
        MinResult = no,
        A = A0,
        Result = yes
    ;
        MinResult = yes(Q - _Val),
        AllRows = (pred(Row::out) is nondet :- between(1, N, Row)),
        MaxAgg =
            ( pred(Row::in, Max0::in, Max::out) is det :-
                (
                    Max0 = no,
                    index(A0, N, M, Row, Q, MaxVal),
                    ( if MaxVal > 0.0 then
                        index(A0, N, M, Row, M, MVal),
                        CVal = MVal/MaxVal,
                        Max = yes(Row - CVal)
                    else
                        Max = no
                    )
                ;
                    Max0 = yes(_ - MaxVal0),
                    index(A0, N, M, Row, Q, CellVal),
                    index(A0, N, M, Row, M, MVal),
                    ( if
                        CellVal > 0.0,
                        MaxVal1 = MVal/CellVal,
                        MaxVal1 =< MaxVal0
                    then
                        Max = yes(Row - MaxVal1)
                    else
                        Max = Max0
                    )
                )
            ),
        unsorted_aggregate(AllRows, MaxAgg, no, MaxResult),
        (
            MaxResult = no,
            A = A0,
            Result = no
        ;
            MaxResult = yes(P - _),
%           index(A0, N, M, P, Q, Apq),
%           io.format("pivot on (%d, %d) = %f\n",
%               [i(P), i(Q), f(Apq)], !IO),
            pivot(P, Q, N, M, A0, A1),
            show_tableau(N, M, A1, !IO),
            simplex(N, M, A1, A, Result, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- type cell
    --->    cell(int, int).

:- pred pivot(int::in, int::in, int::in, int::in, tableau::in, tableau::out)
    is cc_multi.

pivot(P, Q, N, M, A0, A) :-
    index(A0, N, M, P, Q, Apq),
    MostCells =
        ( pred(Cell::out) is nondet :-
            between(0, N, J),
            J \= P,
            between(0, M, K),
            K \= Q,
            Cell = cell(J, K)
        ),
    ScaleCell =
        ( pred(Cell::in, T0::in, T::out) is det :-
            Cell = cell(J, K),
            index(T0, N, M, J, K, Ajk),
            index(T0, N, M, J, Q, Ajq),
            index(T0, N, M, P, K, Apk),
            NewAjk = Ajk - Apk * Ajq / Apq,
            set_index(N, M, J, K, NewAjk, T0, T)
        ),
    unsorted_aggregate(MostCells, ScaleCell, A0, A1),
    QColumn =
        ( pred(Cell::out) is nondet :-
            between(0, N, J),
            Cell = cell(J, Q)
        ),
    Zero =
        ( pred(Cell::in, T0::in, T::out) is det :-
            Cell = cell(J, K),
            set_index(N, M, J, K, 0.0, T0, T)
        ),
    aggregate(QColumn, Zero, A1, A2),
    PRow = (pred(K::out) is nondet :- between(0, M, K)),
    ScaleRow =
        ( pred(K::in, T0::in, T::out) is det :-
            index(T0, N, M, P, K, Apk),
            NewApk = Apk / Apq,
            set_index(N, M, P, K, NewApk, T0, T)
        ),
    aggregate(PRow, ScaleRow, A2, A3),
    set_index(N, M, P, Q, 1.0, A3, A).

%---------------------------------------------------------------------------%

:- type tableau == array(float).

:- pred init_tableau(int::in, int::in, tableau::out) is det.

init_tableau(Rows0, Cols0, Tableau) :-
    Rows = Rows0 + 1,
    Cols = Cols0 + 1,
    NumCells = Rows * Cols,
    array.init(NumCells, 0.0, Tableau).

:- pred index(tableau::in, int::in, int::in, int::in, int::in, float::out)
    is det.

index(Tableau, Rows0, Cols0, J, K, R) :-
    _Rows = Rows0 + 1,
    Cols = Cols0 + 1,
    Index = J * Cols + K,
    array.lookup(Tableau, Index, R).

:- pred set_index(int::in, int::in, int::in, int::in, float::in,
    tableau::in, tableau::out) is det.

set_index(Rows0, Cols0, J, K, R, !Tableau) :-
    _Rows = Rows0 + 1,
    Cols = Cols0 + 1,
    Index = J * Cols + K,
    mkuniq(!Tableau),
    array.set(Index, R, !Tableau).

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

:- pred show_tableau(int::in, int::in, tableau::in, io::di, io::uo) is det.

show_tableau(_N, _M, _Tableau, !IO).

% show_tableau(N, M, Tableau) -->
%     { string.format("Tableau (%d, %d):\n", [i(N), i(M)], Str) },
%     io.write_string(Str),
%     unsorted_aggregate(between(0, N), show_row(Tableau, M)).
% 
% :- pred show_row(tableau::in, int::in, int::in, io::di, io::uo) is cc_multi.
% 
% show_row(Tableau, M, Row) -->
%     unsorted_aggregate(between(0, M), show_cell(Tableau, Row)),
%     io.write_string("\n").
% 
% :- pred show_cell(tableau::in, int::in, int::in, io::di, io::uo) is cc_multi.
% 
% show_cell(Tableau, Row, Col) -->
%     { index(Tableau, Row, Col, Val) },
%     { string.format("%2.2f\t", [f(Val)], Str) },
%     io.write_string(Str).

%---------------------------------------------------------------------------%

:- pred between(int::in, int::in, int::out) is nondet.

between(Min, Max, I) :-
    Min =< Max,
    (
        I = Min
    ;
        between(Min + 1, Max, I)
    ).

%---------------------------------------------------------------------------%
