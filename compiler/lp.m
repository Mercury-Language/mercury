%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997,2002-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: lp.m.
% Main author: conway.
%
% This module implements a linear constraint solver that finds an
% optimal solution to a set of linear [in]equalities with respect
% to an objective function. It does this using the simplex method.
%
% The form of an [in]equation is
%   a1.x1 + a2.x2 + ... + an.xn {=<,=,>=} b
% where all the numbers are floats, a variable xn may occur multiple
% times in an equation (or the objective function) - the solver simplifies
% all the inequations.
% By default, there is an additional constraint on each of the `xn's:
%   xn >= 0
% If you want xn to take on any value, you can include it in the list
% of URS (UnRestricted in Sign) variables.
%
% The objective function is simply a weighted sum of the variables.
%
% The `x's are represented by `term.var's. The varset from which
% they are allocated is passed to the solver because it needs to
% introduce new variables as part of the solving algorithm.
%
%-----------------------------------------------------------------------------%

:- module libs.lp.
:- interface.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type coeff == pair(var, float).

:- type equation
    --->    eqn(list(coeff), operator, float).

:- type operator
    --->    op_le   % less than or equal
    ;       op_eq   % equal
    ;       op_ge.  % greater than or equal

:- type equations == list(equation).

:- type objective == list(coeff).

:- type direction
    --->    max
    ;       min.

:- type lp.result
    --->    unsatisfiable
    ;       satisfiable(float, map(var, float)).

%-----------------------------------------------------------------------------%

    % lp_solve(Inequations, MaxOrMin, Objective, VarSet, URSVars, Result):
    %
    % Maximize (or minimize - depending on `MaxOrMin') `Objective' subject
    % to the constraints `Inequations'. The variables in the objective
    % and inequations are from `VarSet' which is passed in so that the solver
    % can allocate fresh variables as required. URSVars is the list of
    % variables that are unrestricted in sign. lp_solve binds Result
    % either to `unsatisfiable' if the there was no optimum value of the
    % objective function (i.e. the constraints were inconsistent, or the
    % objective function is unbounded by the constraints), or
    % `satisfiable(ObjVal, MapFromObjVarsToVals)'.
    %
:- pred lp_solve(equations::in, direction::in, objective::in, varset::in,
    list(var)::in, lp.result::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type lp_info
    --->    lp_info(
                lpi_varset                  :: varset,

                % Map from variables with URS to the corresponding pair
                % of variables that represent that variable in the standard
                % form (x = x' - x'', x', x'' >= 0).
                lpi_urs_map                 :: map(var, pair(var)),

                lpi_slack_vars              :: list(var),
                lpi_art_vars                :: list(var)
            ).

%-----------------------------------------------------------------------------%

lp_solve(Eqns0, Dir, Obj0, VarSet0, URSVars, Result) :-
    some [!Info] (
        lp_info_init(VarSet0, URSVars, !:Info),

        % Simplify the inequations and convert them to standard form by
        % introducing slack/excess/artificial variables. We also expand
        % URS variables by replacing them with the difference of two
        % fresh variables.
        standardize_equations(Eqns0, Eqns, !Info),

        % If we are maximizing the objective function, then we need to negate
        % all the coefficients in the objective.
        (
            Dir = max,
            negate_equation(eqn(Obj0, op_eq, 0.0), eqn(Obj1, _, _))
        ;
            Dir = min,
            Obj1 = Obj0
        ),
        simplify_coeffs(Obj1, Obj2),

        get_urs_vars(!.Info, URS),
        expand_urs_vars(Obj2, URS, Obj),
        list.length(Eqns, Rows),
        collect_vars(Eqns, Obj, Vars),
        set.to_sorted_list(Vars, VarList),
        list.length(VarList, Cols),
        map.init(VarNums0),
        number_vars(VarList, 0, VarNums0, VarNums),
        get_art_vars(!.Info, ArtVars),
        some [!Tableau] (
            init_tableau(Rows, Cols, VarNums, URS, !:Tableau),
            insert_equations(Eqns, 1, Cols, VarNums, !Tableau),
            (
                ArtVars = [_ | _],
                % There are one or more artificial variables, so we use
                % the two-phase method for solving the system.
                two_phase(Obj0, Obj, ArtVars, VarNums, !.Tableau, Result0)
            ;
                ArtVars = [],
                one_phase(Obj0, Obj, VarNums, !.Tableau, Result0)
            )
        ),
        (
            Dir = max,
            Result = Result0
        ;
            Dir = min,
            (
                Result0 = unsatisfiable,
                Result = Result0
            ;
                Result0 = satisfiable(NOptVal, OptCoffs),
                OptVal = -NOptVal,
                Result = satisfiable(OptVal, OptCoffs)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred one_phase(list(coeff)::in, list(coeff)::in, map(var, int)::in,
    tableau::in, lp.result::out) is det.

one_phase(Obj0, Obj, VarNums, Tableau0, Result) :-
    insert_coeffs(Obj, 0, VarNums, Tableau0, Tableau1),
    ObjVars = get_vars_from_coeffs(Obj0),
    optimize(ObjVars, Tableau1, _, Result).

:- func get_vars_from_coeffs(list(coeff)) = list(var).

get_vars_from_coeffs(Coeffs) = Vars :-
    get_vars_from_coeffs_2(Coeffs, set.init, SetVars),
    Vars = set.to_sorted_list(SetVars).

:- pred get_vars_from_coeffs_2(list(coeff)::in, set(var)::in, set(var)::out)
    is det.

get_vars_from_coeffs_2([], !SetVar).
get_vars_from_coeffs_2([Var - _ | Coeffs], !SetVar) :-
    set.insert(Var, !SetVar),
    get_vars_from_coeffs_2(Coeffs, !SetVar).

%-----------------------------------------------------------------------------%

:- pred two_phase(list(coeff)::in, list(coeff)::in, list(var)::in,
    map(var, int)::in, tableau::in, lp.result::out) is det.

two_phase(Obj0, Obj, ArtVars, VarNums, Tableau0, Result) :-
    % Do phase 1: minimize the sum of the artificial variables.
    some [!Tableau] (
        !:Tableau = Tableau0,
        construct_art_objective(ArtVars, ArtObj),
        insert_coeffs(ArtObj, 0, VarNums, !Tableau),
        ensure_zero_obj_coeffs(ArtVars, !Tableau),
        optimize(ArtVars, !Tableau, Res0),
        (
            Res0 = unsatisfiable,
            Result = unsatisfiable
        ;
            Res0 = satisfiable(Val, _ArtRes),
            ( if Val = 0.0 then
                fix_basis_and_rem_cols(ArtVars, !Tableau),
                % Do phase 2:
                %   insert the real objective,
                %   zero the objective coefficients of the
                %   basis variables,
                %   optimize the objective.
                insert_coeffs(Obj, 0, VarNums, !Tableau),
                get_basis_vars(!.Tableau, BasisVars),
                ensure_zero_obj_coeffs(BasisVars, !Tableau),
                ObjVars = get_vars_from_coeffs(Obj0),
                optimize(ObjVars, !.Tableau, _, Result)
            else
                Result = unsatisfiable
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred construct_art_objective(list(var)::in, list(coeff)::out) is det.

construct_art_objective([], []).
construct_art_objective([V | Vs], [V - (1.0) | Rest]) :-
    construct_art_objective(Vs, Rest).

%-----------------------------------------------------------------------------%

:- pred standardize_equations(equations::in, equations::out,
    lp_info::in, lp_info::out) is det.

standardize_equations(!Eqns, !Info) :-
    list.map_foldl(standardize_equation, !Eqns, !Info).

    % standardize_equation performs the following operations on an equation:
    %   - ensures the constant is >= 0 (multiplying by -1 if necessary)
    %   - introduces slack, excess and artificial variables
    %   - replace the URS variables with their corresponding difference pair.
    %
:- pred standardize_equation(equation::in, equation::out,
    lp_info::in, lp_info::out) is det.

standardize_equation(Eqn0, Eqn, !Info) :-
    Eqn0 = eqn(Coeffs0, op_le, Const0),
    ( if Const0 < 0.0 then
        negate_equation(Eqn0, Eqn1),
        standardize_equation(Eqn1, Eqn, !Info)
    else
        new_slack_var(Var, !Info),
        Coeffs = [Var - 1.0 | Coeffs0],
        simplify_eq(eqn(Coeffs, op_le, Const0), Eqn1),
        get_urs_vars(!.Info, URS),
        expand_urs_vars_e(Eqn1, URS, Eqn)
    ).

standardize_equation(Eqn0, Eqn, !Info) :-
    Eqn0 = eqn(Coeffs0, op_eq, Const0),
    ( if Const0 < 0.0 then
        negate_equation(Eqn0, Eqn1),
        standardize_equation(Eqn1, Eqn, !Info)
    else
        new_art_var(Var, !Info),
        Coeffs = [Var - 1.0 | Coeffs0],
        simplify_eq(eqn(Coeffs, op_le, Const0), Eqn1),
        get_urs_vars(!.Info, URS),
        expand_urs_vars_e(Eqn1, URS, Eqn)
    ).

standardize_equation(Eqn0, Eqn, !Info) :-
    Eqn0 = eqn(Coeffs0, op_ge, Const0),
    ( if Const0 < 0.0 then
        negate_equation(Eqn0, Eqn1),
        standardize_equation(Eqn1, Eqn, !Info)
    else
        new_slack_var(SVar, !Info),
        new_art_var(AVar, !Info),
        Coeffs = [SVar - (-1.0), AVar - (1.0) | Coeffs0],
        simplify_eq(eqn(Coeffs, op_ge, Const0), Eqn1),
        get_urs_vars(!.Info, URS),
        expand_urs_vars_e(Eqn1, URS, Eqn)
    ).

:- pred negate_equation(equation::in, equation::out) is det.

negate_equation(eqn(Coeffs0, Op0, Const0), eqn(Coeffs, Op, Const)) :-
    ( Op0 = op_le, Op = op_ge
    ; Op0 = op_eq, Op = op_eq
    ; Op0 = op_ge, Op = op_le
    ),
    Coeffs = list.map((func(V - X) = V - (-X)), Coeffs0),
    Const = -Const0.

:- pred simplify_eq(equation::in, equation::out) is det.

simplify_eq(eqn(Coeffs0, Op, Const), eqn(Coeffs, Op, Const)) :-
    simplify_coeffs(Coeffs0, Coeffs).

:- pred simplify_coeffs(list(coeff)::in, list(coeff)::out) is det.

simplify_coeffs(Coeffs0, Coeffs) :-
    map.init(CoeffMap0),
    AddCoeff =
        ( pred(Pair::in, Map0::in, Map::out) is det :-
            Pair = Var - Coeff,
            add_var(Var, Coeff, Map0, Map)
        ),
    list.foldl(AddCoeff, Coeffs0, CoeffMap0, CoeffMap),
    map.to_assoc_list(CoeffMap, Coeffs).

:- pred add_var(var::in, float::in,
    map(var, float)::in, map(var, float)::out) is det.

add_var(Var, Coeff, !Map) :-
    ( if map.search(!.Map, Var, Acc0) then
        Acc1 = Acc0
    else
        Acc1 = 0.0
    ),
    Acc = Acc1 + Coeff,
    map.set(Var, Acc, !Map).

:- pred expand_urs_vars_e(equation::in, map(var, pair(var))::in,
    equation::out) is det.

expand_urs_vars_e(eqn(Coeffs0, Op, Const), Vars, eqn(Coeffs, Op, Const)) :-
    expand_urs_vars(Coeffs0, Vars, Coeffs).

:- pred expand_urs_vars(list(coeff)::in, map(var, pair(var))::in,
    list(coeff)::out) is det.

expand_urs_vars(Coeffs0, Vars, Coeffs) :-
    expand_urs_vars(Coeffs0, Vars, [], Coeffs1),
    list.reverse(Coeffs1, Coeffs).

:- pred expand_urs_vars(list(coeff)::in, map(var, pair(var))::in,
        list(coeff)::in, list(coeff)::out) is det.

expand_urs_vars([], _Vars, !Coeffs).
expand_urs_vars([Var - Coeff | Rest], Vars, !Coeffs) :-
    ( if map.search(Vars, Var, PVar - NVar) then
        NCoeff = -Coeff,
        !:Coeffs = [NVar - NCoeff, PVar - Coeff | !.Coeffs]
    else
        !:Coeffs = [Var - Coeff | !.Coeffs]
    ),
    expand_urs_vars(Rest, Vars, !Coeffs).

%-----------------------------------------------------------------------------%

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
    solutions.solutions(GetVar, VarList),
    set.list_to_set(VarList, Vars).

:- pred number_vars(list(var)::in, int::in,
    map(var, int)::in, map(var, int)::out) is det.

number_vars([], _, !VarNums).
number_vars([Var | Vars], N, !VarNums) :-
    map.det_insert(Var, N, !VarNums),
    number_vars(Vars, N + 1, !VarNums).

:- pred insert_equations(equations::in, int::in, int::in, map(var, int)::in,
    tableau::in, tableau::out) is det.

insert_equations([], _, _, _, !Tableau).
insert_equations([Eqn | Eqns], Row, ConstCol, VarNums, !Tableau) :-
    Eqn = eqn(Coeffs, _Op, Const),
    insert_coeffs(Coeffs, Row, VarNums, !Tableau),
    set_index(Row, ConstCol, Const, !Tableau),
    insert_equations(Eqns, Row + 1, ConstCol, VarNums, !Tableau).

:- pred insert_coeffs(list(coeff)::in, int::in, map(var, int)::in,
    tableau::in, tableau::out) is det.

insert_coeffs([], _Row, _VarNums, !Tableau).
insert_coeffs([Coeff | Coeffs], Row, VarNums, !Tableau) :-
    Coeff = Var - Const,
    map.lookup(VarNums, Var, Col),
    set_index(Row, Col, Const, !Tableau),
    insert_coeffs(Coeffs, Row, VarNums, !Tableau).

%-----------------------------------------------------------------------------%

:- pred optimize(list(var)::in, tableau::in, tableau::out, lp.result::out)
    is det.

optimize(ObjVars, A0, A, Result) :-
    simplex(A0, A, Res0),
    (
        Res0 = no,
        Result = unsatisfiable
    ;
        Res0 = yes,
        rhs_col(A, M),
        index(A, 0, M, ObjVal),
        extract_objective(ObjVars, A, ObjMap),
        Result = satisfiable(ObjVal, ObjMap)
    ).

:- pred extract_objective(list(var)::in, tableau::in,
    map(var, float)::out) is det.

extract_objective(ObjVars, Tab, Res) :-
    list.foldl(extract_obj_var(Tab), ObjVars, map.init, Res).

:- pred extract_obj_var(tableau::in, var::in,
    map(var, float)::in, map(var, float)::out) is det.

extract_obj_var(Tab, Var, !Map) :-
    urs_vars(Tab, Vars),
    ( if map.search(Vars, Var, Pos - Neg) then
        extract_obj_var2(Tab, Pos, PosVal),
        extract_obj_var2(Tab, Neg, NegVal),
        Val = PosVal - NegVal
    else
        extract_obj_var2(Tab, Var, Val)
    ),
    map.set(Var, Val, !Map).

:- pred extract_obj_var2(tableau::in, var::in, float::out) is det.

extract_obj_var2(Tab, Var, Val) :-
    var_col(Tab, Var, Col),
    GetCell =
        ( pred(Val0::out) is nondet :-
            all_rows(Tab, Row),
            index(Tab, Row, Col, 1.0),
            rhs_col(Tab, RHS),
            index(Tab, Row, RHS, Val0)
        ),
    solutions.solutions(GetCell, Solns),
    ( if Solns = [Val1] then
        Val = Val1
    else
        Val = 0.0
    ).

:- pred simplex(tableau::in, tableau::out, bool::out) is det.

simplex(A0, A, Result) :-
    AllColumns = all_cols(A0),
    MinAgg =
        ( pred(Col::in, Min0::in, Min::out) is det :-
            (
                Min0 = no,
                index(A0, 0, Col, MinVal),
                ( if MinVal < 0.0 then
                    Min = yes(Col - MinVal)
                else
                    Min = no
                )
            ;
                Min0 = yes(_ - MinVal0),
                index(A0, 0, Col, CellVal),
                ( if CellVal < MinVal0 then
                    Min = yes(Col - CellVal)
                else
                    Min = Min0
                )
            )
        ),
    solutions.aggregate(AllColumns, MinAgg, no, MinResult),
    (
        MinResult = no,
        A = A0,
        Result = yes
    ;
        MinResult = yes(Q - _Val),
        AllRows = all_rows(A0),
        MaxAgg =
            ( pred(Row::in, Max0::in, Max::out) is det :-
                (
                    Max0 = no,
                    index(A0, Row, Q, MaxVal),
                    ( if MaxVal > 0.0 then
                        rhs_col(A0, RHSC),
                        index(A0, Row, RHSC, MVal),
                        CVal = MVal/MaxVal,
                        Max = yes(Row - CVal)
                    else
                        Max = no
                    )
                ;
                    Max0 = yes(_ - MaxVal0),
                    index(A0, Row, Q, CellVal),
                    rhs_col(A0, RHSC),
                    index(A0, Row, RHSC, MVal),
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
        solutions.aggregate(AllRows, MaxAgg, no, MaxResult),
        (
            MaxResult = no,
            A = A0,
            Result = no
        ;
            MaxResult = yes(P - _),
            pivot(P, Q, A0, A1),
            simplex(A1, A, Result)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred ensure_zero_obj_coeffs(list(var)::in, tableau::in, tableau::out)
    is det.

ensure_zero_obj_coeffs([], !Tableau).
ensure_zero_obj_coeffs([V | Vs], !Tableau) :-
    var_col(!.Tableau, V, Col),
    index(!.Tableau, 0, Col, Val),
    ( if Val = 0.0 then
        ensure_zero_obj_coeffs(Vs, !Tableau)
    else
        FindOne =
            ( pred(P::out) is nondet :-
                all_rows(!.Tableau, R),
                index(!.Tableau, R, Col, ValF0),
                ValF0 \= 0.0,
                P = R - ValF0
            ),
        solutions.solutions(FindOne, Ones),
        (
            Ones = [Row - Fac0 | _],
            Fac = -Val/Fac0,
            row_op(Fac, Row, 0, !Tableau),
            ensure_zero_obj_coeffs(Vs, !Tableau)
        ;
            Ones = [],
            unexpected($pred, "problem with artificial variable")
        )
    ).

:- pred fix_basis_and_rem_cols(list(var)::in, tableau::in, tableau::out)
    is det.

fix_basis_and_rem_cols([], !Tableau).
fix_basis_and_rem_cols([V | Vs], !Tableau) :-
    var_col(!.Tableau, V, Col),
    BasisAgg =
        ( pred(R::in, Ones0::in, Ones::out) is det :-
            index(!.Tableau, R, Col, Val),
            ( if Val = 0.0 then
                Ones = Ones0
            else
                Ones = [Val - R | Ones0]
            )
        ),
    solutions.aggregate(all_rows(!.Tableau), BasisAgg, [], Res),
    ( if Res = [1.0 - Row] then
        PivGoal =
            ( pred(Col1::out) is nondet :-
                all_cols(!.Tableau, Col1),
                Col \= Col1,
                index(!.Tableau, Row, Col1, Zz),
                Zz \= 0.0
            ),
        solutions.solutions(PivGoal, PivSolns),
        (
            PivSolns = [],
            remove_col(Col, !Tableau),
            remove_row(Row, !Tableau)
        ;
            PivSolns = [Col2 | _],
            pivot(Row, Col2, !Tableau),
            remove_col(Col, !Tableau)
        )
    else
        true
    ),
    remove_col(Col, !Tableau),
    fix_basis_and_rem_cols(Vs, !Tableau).

%-----------------------------------------------------------------------------%

:- type cell
    --->    cell(int, int).

:- pred pivot(int::in, int::in, tableau::in, tableau::out) is det.

pivot(P, Q, !Tableau) :-
    index(!.Tableau, P, Q, Apq),
    MostCells =
        ( pred(Cell::out) is nondet :-
            all_rows0(!.Tableau, J),
            J \= P,
            all_cols0(!.Tableau, K),
            K \= Q,
            Cell = cell(J, K)
        ),
    ScaleCell =
        ( pred(Cell::in, !.T::in, !:T::out) is det :-
            Cell = cell(J, K),
            index(!.T, J, K, Ajk),
            index(!.T, J, Q, Ajq),
            index(!.T, P, K, Apk),
            NewAjk = Ajk - Apk * Ajq / Apq,
            set_index(J, K, NewAjk, !T)
        ),
    solutions.aggregate(MostCells, ScaleCell, !Tableau),
    QColumn =
        ( pred(Cell::out) is nondet :-
            all_rows0(!.Tableau, J),
            Cell = cell(J, Q)
        ),
    Zero =
        ( pred(Cell::in, T0::in, T::out) is det :-
            Cell = cell(J, K),
            set_index(J, K, 0.0, T0, T)
        ),
    solutions.aggregate(QColumn, Zero, !Tableau),
    PRow = all_cols0(!.Tableau),
    ScaleRow =
        ( pred(K::in, T0::in, T::out) is det :-
            index(T0, P, K, Apk),
            NewApk = Apk / Apq,
            set_index(P, K, NewApk, T0, T)
        ),
    solutions.aggregate(PRow, ScaleRow, !Tableau),
    set_index(P, Q, 1.0, !Tableau).

:- pred row_op(float::in, int::in, int::in,
    tableau::in, tableau::out) is det.

row_op(Scale, From, To, !Tableau) :-
    AllCols = all_cols0(!.Tableau),
    AddRow =
        ( pred(Col::in, !.Tableau::in, !:Tableau::out) is det :-
            index(!.Tableau, From, Col, X),
            index(!.Tableau, To, Col, Y),
            Z = Y + (Scale * X),
            set_index(To, Col, Z, !Tableau)
        ),
    solutions.aggregate(AllCols, AddRow, !Tableau).

%-----------------------------------------------------------------------------%

:- type tableau
    --->    tableau(
                rows            :: int,
                cols            :: int,
                var_nums        :: map(var, int),
                urs_vars        :: map(var, pair(var)),
                shunned_rows    :: list(int),
                shunned_cols    :: list(int),
                cells           :: map(pair(int), float)
            ).

:- pred init_tableau(int::in, int::in, map(var, int)::in,
        map(var, pair(var))::in, tableau::out) is det.

init_tableau(Rows, Cols, VarNums, URSVars, Tableau) :-
    map.init(Cells),
    Tableau = tableau(Rows, Cols, VarNums, URSVars, [], [], Cells).

:- pred index(tableau::in, int::in, int::in, float::out) is det.

index(Tableau, J, K, R) :-
    Tableau = tableau(_, _, _, _, SR, SC, Cells),
    ( if
        ( list.member(J, SR)
        ; list.member(K, SC)
        )
    then
        unexpected($pred, "attempt to address shunned row/col")
    else
        true
    ),
    ( if map.search(Cells, J - K, R0) then
        R = R0
    else
        R = 0.0
    ).

:- pred set_index(int::in, int::in, float::in,
    tableau::in, tableau::out) is det.

set_index(J, K, R, !Tableau) :-
    !.Tableau = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells0),
    ( if
        ( list.member(J, SR)
        ; list.member(K, SC)
        )
    then
        unexpected($pred, "attempt to write shunned row/col")
    else
        true
    ),
    ( if R = 0.0 then
        map.delete(J - K, Cells0, Cells)
    else
        map.set(J - K, R, Cells0, Cells)
    ),
    !:Tableau = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells).

:- pred rhs_col(tableau::in, int::out) is det.

rhs_col(tableau(_, RHS, _, _, _, _, _), RHS).

:- pred all_rows0(tableau::in, int::out) is nondet.

all_rows0(Tableau, Row) :-
    Tableau = tableau(Rows, _Cols, _, _, SR, _, _),
    between(0, Rows, Row),
    not list.member(Row, SR).

:- pred all_rows(tableau::in, int::out) is nondet.

all_rows(Tableau, Row) :-
    Tableau = tableau(Rows, _Cols, _, _, SR, _, _),
    between(1, Rows, Row),
    not list.member(Row, SR).

:- pred all_cols0(tableau::in, int::out) is nondet.

all_cols0(Tableau, Col) :-
    Tableau = tableau(_Rows, Cols, _, _, _, SC, _),
    between(0, Cols, Col),
    not list.member(Col, SC).

:- pred all_cols(tableau::in, int::out) is nondet.

all_cols(Tableau, Col) :-
    Tableau = tableau(_Rows, Cols, _, _, _, SC, _),
    Cols1 = Cols - 1,
    between(0, Cols1, Col),
    not list.member(Col, SC).

:- pred var_col(tableau::in, var::in, int::out) is det.

var_col(Tableau, Var, Col) :-
    Tableau = tableau(_, _, VarCols, _, _, _, _),
    map.lookup(VarCols, Var, Col).

:- pred urs_vars(tableau::in, map(var, pair(var))::out) is det.

urs_vars(Tableau, Tableau ^ urs_vars).

:- pred remove_row(int::in, tableau::in, tableau::out) is det.

remove_row(R, Tableau0, Tableau) :-
    Tableau0 = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells),
    Tableau = tableau(Rows, Cols, VarNums, URS, [R | SR], SC, Cells).

:- pred remove_col(int::in, tableau::in, tableau::out) is det.

remove_col(C, Tableau0, Tableau) :-
    Tableau0 = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells),
    Tableau = tableau(Rows, Cols, VarNums, URS, SR, [C | SC], Cells).

:- pred get_basis_vars(tableau::in, list(var)::out) is det.

get_basis_vars(Tab, Vars) :-
    BasisCol =
        ( pred(C::out) is nondet :-
            all_cols(Tab, C),
            NonZeroGoal =
                ( pred(P::out) is nondet :-
                    all_rows(Tab, R),
                    index(Tab, R, C, Z),
                    Z \= 0.0,
                    P = R - Z
                ),
            solutions.solutions(NonZeroGoal, Solns),
            Solns = [_ - 1.0]
        ),
    solutions.solutions(BasisCol, Cols),
    BasisVars =
        ( pred(V::out) is nondet :-
            list.member(Col, Cols),
            Tab = tableau(_, _, VarCols, _, _, _, _),
            map.member(VarCols, V, Col)
        ),
    solutions.solutions(BasisVars, Vars).

%-----------------------------------------------------------------------------%

:- pred lp_info_init(varset::in, list(var)::in, lp_info::out) is det.

lp_info_init(VarSet0, URSVars, LPInfo) :-
    Introduce =
        ( pred(Orig::in, VP0::in, VP::out) is det :-
            VP0 = VS0 - VM0,
            varset.new_var(V1, VS0, VS1),
            varset.new_var(V2, VS1, VS),
            map.set(Orig, V1 - V2, VM0, VM),
            VP = VS - VM
        ),
    map.init(URSMap0),
    list.foldl(Introduce, URSVars, VarSet0 - URSMap0, VarSet - URSMap),
    LPInfo = lp_info(VarSet, URSMap, [], []).

:- pred new_slack_var(var::out, lp_info::in, lp_info::out) is det.

new_slack_var(Var, !Info) :-
    some [!VarSet] (
        get_varset(!.Info, !:VarSet),
        varset.new_var(Var, !VarSet),
        set_varset(!.VarSet, !Info)
    ),
    get_slack_vars(!.Info, Vars),
    set_slack_vars([Var | Vars], !Info).

:- pred new_art_var(var::out, lp_info::in, lp_info::out) is det.

new_art_var(Var, !Info) :-
    some [!VarSet] (
        get_varset(!.Info, !:VarSet),
        varset.new_var(Var, !VarSet),
        set_varset(!.VarSet, !Info)
    ),
    get_art_vars(!.Info, Vars),
    set_art_vars([Var | Vars], !Info).

:- pred get_varset(lp_info::in, varset::out) is det.
:- pred get_urs_vars(lp_info::in, map(var, pair(var))::out) is det.
:- pred get_slack_vars(lp_info::in, list(var)::out) is det.
:- pred get_art_vars(lp_info::in, list(var)::out) is det.

get_varset(Info, Info ^ lpi_varset).
get_urs_vars(Info, Info ^ lpi_urs_map).
get_slack_vars(Info, Info ^ lpi_slack_vars).
get_art_vars(Info, Info ^ lpi_art_vars).

:- pred set_varset(varset::in, lp_info::in, lp_info::out) is det.
:- pred set_slack_vars(list(var)::in, lp_info::in, lp_info::out) is det.
:- pred set_art_vars(list(var)::in, lp_info::in, lp_info::out) is det.

set_varset(VarSet, !Info) :-
    !Info ^ lpi_varset := VarSet.
set_slack_vars(Slack, !Info) :-
    !Info ^ lpi_slack_vars := Slack.
set_art_vars(Art, !Info) :-
    !Info ^ lpi_art_vars := Art.

%-----------------------------------------------------------------------------%

:- pred between(int::in, int::in, int::out) is nondet.

between(Min, Max, I) :-
    Min =< Max,
    (
        I = Min
    ;
        Min1 = Min + 1,
        between(Min1, Max, I)
    ).

%-----------------------------------------------------------------------------%
%
% Debugging predicates.
%

:- pred show_tableau(io.text_output_stream::in, tableau::in,
    io::di, io::uo) is det.
:- pragma consider_used(pred(show_tableau/4)).

show_tableau(OutputStream, Tableau, !IO) :-
    Tableau = tableau(N, M, _, _, _, _, _),
    io.format(OutputStream, "Tableau (%d, %d):\n", [i(N), i(M)], !IO),
    solutions.aggregate(all_rows0(Tableau),
        show_row(OutputStream, Tableau), !IO).

:- pred show_row(io.text_output_stream::in, tableau::in, int::in,
    io::di, io::uo) is det.

show_row(OutputStream, Tableau, Row, !IO) :-
    solutions.aggregate(all_cols0(Tableau),
        show_cell(OutputStream, Tableau, Row), !IO),
    io.nl(OutputStream, !IO).

:- pred show_cell(io.text_output_stream::in, tableau::in, int::in, int::in,
    io::di, io::uo) is det.

show_cell(OutputStream, Tableau, Row, Col, !IO) :-
    index(Tableau, Row, Col, Val),
    io.format(OutputStream, "%2.2f\t", [f(Val)], !IO).

%-----------------------------------------------------------------------------%
:- end_module libs.lp.
%-----------------------------------------------------------------------------%
