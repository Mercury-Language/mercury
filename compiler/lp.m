%-----------------------------------------------------------------------------%
% Copyright (C) 1997,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: lp.m
% main author: conway,	Oct 1997
%
% This module implements a linear constraint solver that finds an
% optimal solution to a set of linear [in]equalities with respect
% to some objective function. It does this using the simplex method.
%
% The form of an [in]equation is
% 	a1.x1 + a2.x2 + ... + an.xn {=<,=,>=} b
% where all the numbers are floats, a variable xn may occur multiple
% times in an equation (or the objective function) - the solver simplifies
% all the inequations.
% By default, there is an additional constraint on each of the `xn's:
%	xn >= 0
% If you want xn to take on any value, you can include it in the list
% of URS (UnRestricted in Sign) variables.
%
% The objective function is simply a weighted sum of the variables.
%
% The `x's are represented by `term__var's. The varset from which
% they are allocated is passed to the solver because it needs to
% introduce new variables as part of the solving algorithm.
%
%------------------------------------------------------------------------------%
:- module transform_hlds__lp.

:- interface.

%------------------------------------------------------------------------------%

:- import_module float, io, list, map, std_util, term, varset.

:- type coeff	==	pair(var, float).

:- type equation
	--->	eqn(list(coeff), operator, float).

:- type operator
	--->	(=<) ; (=) ; (>=) .

:- type equations	==	list(equation).

:- type objective	==	list(coeff).

:- type direction
	--->	max ; min .

:- type lp__result
	--->	unsatisfiable
	;	satisfiable(float, map(var, float))
	.

%------------------------------------------------------------------------------%

	% lp_solve(Inequations, MaxOrMin, Objective, Varset, URSVars,
	%		Result, IO0, IO)
	% maximize (or minimize - depending on `MaxOrMin') `Objective'
	% subject to the constraints `Inequations'. The variables in
	% the objective and inequations are from `Varset' which is passed
	% so that the solver can allocate fresh variables as required.
	% URSVars is the list of variable that are unrestricted in sign.
	% lp_solve binds Result either to `unsatisfiable' if the there
	% was no optimum value of the objective function (ie the
	% constraints were inconsistent, or the objective function
	% is unbounded by the constraints), or `satisfiable(ObjVal,
	% MapFromObjVarsToVals)'.

:- pred lp_solve(equations, direction, objective, varset, list(var),
		lp__result, io__state, io__state).
:- mode lp_solve(in, in, in, in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module bool, int, require, set, string.

:- type lp_info
	---> lp(
		varset,
		map(var, pair(var)),	% map from variables with URS to
					% the corresponding pair of variables
					% that represent that variable in
					% the standard form (x = x' - x'',
					% x', x'' >= 0).
		list(var),		% slack variables
		list(var)		% artificial variables
	).

lp_solve(Eqns, Dir, Obj, Varset0, URSVars, Result, IO0, IO) :-
	lp_info_init(Varset0, URSVars, Info0),
	lp_solve2(Eqns, Dir, Obj, Result, IO0, IO, Info0, _).

%
% lp_solve2(Eqns, Dir, Obj, Res, IO0, IO, LPInfo0, LPInfo) takes a list
% of inequations `Eqns', a direction for optimization `Dir', an objective
% function `Obj', an I/O state `IO0' and an lp_info structure `LPInfo0'.
% See inline comments for details on the algorithm.
%
:- pred lp_solve2(equations, direction, objective, lp__result,
		io__state, io__state, lp_info, lp_info).
:- mode lp_solve2(in, in, in, out, di, uo, in, out) is det.

lp_solve2(Eqns0, Dir, Obj0, Result, IO0, IO, Info0, Info) :-
		% simplify the inequations and convert them
		% to standard form by introducing slack/excess/
		% artificial variables. We also expand URS variables
		% by replacing them with the difference of two
		% fresh variables.
	standardize_equations(Eqns0, Eqns, Info0, Info1),

		% If we're maximizing the objective function then we need
		% to negate all the coefficients in the objective.w
	(
		Dir = max,
		negate_equation(eqn(Obj0, (=), 0.0), eqn(Obj1, _, _))
	;
		Dir = min,
		Obj1 = Obj0
	),
	simplify_coeffs(Obj1, Obj2),

	get_urs_vars(URS, Info1, _),
	expand_urs_vars(Obj2, URS, Obj),
	list__length(Eqns, Rows),
	collect_vars(Eqns, Obj, Vars),
	set__to_sorted_list(Vars, VarList),
	list__length(VarList, Cols),
	map__init(VarNums0),
	number_vars(VarList, 0, VarNums0, VarNums),
	get_art_vars(ArtVars, Info1, Info),
	init_tableau(Rows, Cols, VarNums, URS, Tableau0),
	insert_equations(Eqns, 1, Cols, VarNums,
		Tableau0, Tableau1),
	(
		ArtVars = [_|_],
		% There are one or more artificial variables, so we use
		% the two-phase method for solving the system.
		two_phase(Obj0, Obj, ArtVars, VarNums, Tableau1, Result0,
			IO0, IO)
	;
		ArtVars = [],
		one_phase(Obj0, Obj, VarNums, Tableau1, Result0, IO0, IO)
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
			OptVal is -NOptVal,
			Result = satisfiable(OptVal, OptCoffs)
		)
	).

%------------------------------------------------------------------------------%

:- pred one_phase(list(coeff), list(coeff), map(var, int), tableau,
		lp__result, io__state, io__state).
:- mode one_phase(in, in, in, in, out, di, uo) is det.

one_phase(Obj0, Obj, VarNums, Tableau0, Result, IO0, IO) :-
	insert_coeffs(Obj, 0, VarNums, Tableau0, Tableau1),
	GetObjVar = lambda([V::out] is nondet, (
		list__member(X, Obj0),
		X = V - _Cof
	)),
	solutions(GetObjVar, ObjVars),
	optimize(ObjVars, Tableau1, _, Result, IO0, IO).

%------------------------------------------------------------------------------%

:- pred two_phase(list(coeff), list(coeff), list(var), map(var, int),
		tableau, lp__result, io__state, io__state).
:- mode two_phase(in, in, in, in, in, out, di, uo) is det.

two_phase(Obj0, Obj, ArtVars, VarNums, Tableau0, Result, IO0, IO) :-
		% Do phase 1:
		%	minimize the sum of the artificial variables
	construct_art_objective(ArtVars, ArtObj),
	insert_coeffs(ArtObj, 0, VarNums, Tableau0, Tableau1a),
	ensure_zero_obj_coeffs(ArtVars, Tableau1a, Tableau1b),
	optimize(ArtVars, Tableau1b, Tableau1c,
		Res0, IO0, IO1),
	(
		Res0 = unsatisfiable,
		Result = unsatisfiable,
		IO = IO1
	;
		Res0 = satisfiable(Val, _ArtRes),
		( Val \= 0.0 ->
			Result = unsatisfiable,
			IO = IO1
		;
			fix_basis_and_rem_cols(ArtVars, Tableau1c, Tableau2),
				% Do phase 2:
				%	insert the real objective,
				%	zero the objective coefficients of the
				%	basis variables,
				%	optimize the objective.
			insert_coeffs(Obj, 0, VarNums, Tableau2, Tableau3),
			get_basis_vars(Tableau3, BasisVars),
			ensure_zero_obj_coeffs(BasisVars,
					Tableau3, Tableau4),
			GetObjVar = lambda([V::out] is nondet, (
				list__member(X, Obj0),
				X = V - _Cof
			)),
			solutions(GetObjVar, ObjVars),
			optimize(ObjVars, Tableau4, _, Result, IO1, IO)
		)
	).

%------------------------------------------------------------------------------%

:- pred construct_art_objective(list(var), list(coeff)).
:- mode construct_art_objective(in, out) is det.

construct_art_objective([], []).
construct_art_objective([V|Vs], [V - (1.0)|Rest]) :-
	construct_art_objective(Vs, Rest).

%------------------------------------------------------------------------------%

:- pred standardize_equations(equations, equations, lp_info, lp_info).
:- mode standardize_equations(in, out, in, out) is det.

standardize_equations(Eqns0, Eqns) -->
	list__map_foldl(standardize_equation, Eqns0, Eqns).

	% standardize_equation peforms the following operations on an
	% equation:
	%	- ensures the constant is >= 0 (multiplying by -1 if
	%		necessary)
	%	- introduces slack, excess and artificial variables
	%	- replace the URS variables with their corresponding
	%		difference pair
:- pred standardize_equation(equation, equation, lp_info, lp_info).
:- mode standardize_equation(in, out, in, out) is det.

standardize_equation(Eqn0, Eqn) -->
	{ Eqn0 = eqn(Coeffs0, (=<), Const0) },
	( { Const0 < 0.0 } ->
		{ negate_equation(Eqn0, Eqn1) },
		standardize_equation(Eqn1, Eqn) 
	;
		new_slack_var(Var),
		{ Coeffs = [Var - 1.0|Coeffs0] },
		{ simplify(eqn(Coeffs, (=<), Const0), Eqn1) },
		get_urs_vars(URS),
		{ expand_urs_vars_e(Eqn1, URS, Eqn) }
	).

standardize_equation(Eqn0, Eqn) -->
	{ Eqn0 = eqn(Coeffs0, (=), Const0) },
	( { Const0 < 0.0 } ->
		{ negate_equation(Eqn0, Eqn1) },
		standardize_equation(Eqn1, Eqn)
	;
		new_art_var(Var),
		{ Coeffs = [Var - 1.0|Coeffs0] },
		{ simplify(eqn(Coeffs, (=<), Const0), Eqn1) },
		get_urs_vars(URS),
		{ expand_urs_vars_e(Eqn1, URS, Eqn) }
	).

standardize_equation(Eqn0, Eqn) -->
	{ Eqn0 = eqn(Coeffs0, (>=), Const0) },
	( { Const0 < 0.0 } ->
		{ negate_equation(Eqn0, Eqn1) },
		standardize_equation(Eqn1, Eqn)
	;
		new_slack_var(SVar),
		new_art_var(AVar),
		{ Coeffs = [SVar - (-1.0), AVar - (1.0)|Coeffs0] },
		{ simplify(eqn(Coeffs, (>=), Const0), Eqn1) },
		get_urs_vars(URS),
		{ expand_urs_vars_e(Eqn1, URS, Eqn) }
	).

:- pred negate_equation(equation, equation).
:- mode negate_equation(in, out) is det.

negate_equation(eqn(Coeffs0, Op0, Const0), eqn(Coeffs, Op, Const)) :-
	(
		Op0 = (=<), Op = (>=)
	;
		Op0 = (=), Op = (=)
	;
		Op0 = (>=), Op = (=<)
	),
	Neg = lambda([Pair0::in, Pair::out] is det, (
		Pair0 = V - X0,
		X is -X0,
		Pair = V - X
	)),
	list__map(Neg, Coeffs0, Coeffs),
	Const is -Const0.

:- pred simplify(equation, equation).
:- mode simplify(in, out) is det.

simplify(eqn(Coeffs0, Op, Const), eqn(Coeffs, Op, Const)) :-
	simplify_coeffs(Coeffs0, Coeffs).

:- pred simplify_coeffs(list(coeff), list(coeff)).
:- mode simplify_coeffs(in, out) is det.

simplify_coeffs(Coeffs0, Coeffs) :-
	map__init(CoeffMap0),
	AddCoeff = lambda([Pair::in, Map0::in, Map::out] is det, (
		Pair = Var - Coeff,
		add_var(Map0, Var, Coeff, Map)
	)),
	list__foldl(AddCoeff, Coeffs0, CoeffMap0, CoeffMap),
	map__to_assoc_list(CoeffMap, Coeffs).

:- pred add_var(map(var, float), var, float, map(var, float)).
:- mode add_var(in, in, in, out) is det.

add_var(Map0, Var, Coeff, Map) :-
	( map__search(Map0, Var, Acc0) ->
		Acc1 = Acc0
	;
		Acc1 = 0.0
	),
	Acc is Acc1 + Coeff,
	map__set(Map0, Var, Acc, Map).

:- pred expand_urs_vars_e(equation, map(var, pair(var)), equation).
:- mode expand_urs_vars_e(in, in, out) is det.

expand_urs_vars_e(eqn(Coeffs0, Op, Const), Vars, eqn(Coeffs, Op, Const)) :-
	expand_urs_vars(Coeffs0, Vars, Coeffs).

:- pred expand_urs_vars(list(coeff), map(var, pair(var)), list(coeff)).
:- mode expand_urs_vars(in, in, out) is det.

expand_urs_vars(Coeffs0, Vars, Coeffs) :-
	expand_urs_vars(Coeffs0, Vars, [], Coeffs1),
	list__reverse(Coeffs1, Coeffs).

:- pred expand_urs_vars(list(coeff), map(var, pair(var)),
		list(coeff), list(coeff)).
:- mode expand_urs_vars(in, in, in, out) is det.

expand_urs_vars([], _Vars, Coeffs, Coeffs).
expand_urs_vars([Var - Coeff|Rest], Vars, Coeffs0, Coeffs) :-
	( map__search(Vars, Var, PVar - NVar) ->
		NCoeff is -Coeff,
		Coeffs1 = [NVar - NCoeff, PVar - Coeff|Coeffs0]
	;
		Coeffs1 = [Var - Coeff|Coeffs0]
	),
	expand_urs_vars(Rest, Vars, Coeffs1, Coeffs).

%------------------------------------------------------------------------------%

:- pred collect_vars(equations, objective, set(var)).
:- mode collect_vars(in, in, out) is det.

collect_vars(Eqns, Obj, Vars) :-
	GetVar = lambda([Var::out] is nondet, (
		(
			list__member(Eqn, Eqns),
			Eqn = eqn(Coeffs, _, _),
			list__member(Pair, Coeffs),
			Pair = Var - _
		;
			list__member(Pair, Obj),
			Pair = Var - _
		)
	)),
	solutions(GetVar, VarList),
	set__list_to_set(VarList, Vars).

:- pred number_vars(list(var), int, map(var, int), map(var, int)).
:- mode number_vars(in, in, in, out) is det.

number_vars([], _, VarNums, VarNums).
number_vars([Var|Vars], N, VarNums0, VarNums) :-
	map__det_insert(VarNums0, Var, N, VarNums1),
	N1 is N + 1,
	number_vars(Vars, N1, VarNums1, VarNums).

:- pred insert_equations(equations, int, int, map(var, int), tableau, tableau).
:- mode insert_equations(in, in, in, in, in, out) is det.

insert_equations([], _, _, _, Tableau, Tableau).
insert_equations([Eqn|Eqns], Row, ConstCol, VarNums, Tableau0, Tableau) :-
	Eqn = eqn(Coeffs, _Op, Const),
	insert_coeffs(Coeffs, Row, VarNums, Tableau0, Tableau1),
	set_index(Tableau1, Row, ConstCol, Const, Tableau2),
	Row1 is Row + 1,
	insert_equations(Eqns, Row1, ConstCol, VarNums, Tableau2, Tableau).

:- pred insert_coeffs(list(coeff), int, map(var, int), tableau, tableau).
:- mode insert_coeffs(in, in, in, in, out) is det.

insert_coeffs([], _Row, _VarNums, Tableau, Tableau).
insert_coeffs([Coeff|Coeffs], Row, VarNums, Tableau0, Tableau) :-
	Coeff = Var - Const,
	map__lookup(VarNums, Var, Col),
	set_index(Tableau0, Row, Col, Const, Tableau1),
	insert_coeffs(Coeffs, Row, VarNums, Tableau1, Tableau).

%------------------------------------------------------------------------------%

:- pred optimize(list(var), tableau, tableau, lp__result,
		io__state, io__state).
:- mode optimize(in, in, out, out, di, uo) is det.

optimize(ObjVars, A0, A, Result) -->
	simplex(A0, A, Res0),
	(
		{ Res0 = no },
		{ Result = unsatisfiable }
	;
		{ Res0 = yes },
		{ rhs_col(A, M) },
		{ index(A, 0, M, ObjVal) },
		{ extract_objective(ObjVars, A, ObjMap) },
		{ Result = satisfiable(ObjVal, ObjMap) }
	).

:- pred extract_objective(list(var), tableau, map(var, float)).
:- mode extract_objective(in, in, out) is det.

extract_objective(ObjVars, Tab, Res) :-
	map__init(Res0),
	list__foldl(extract_obj_var(Tab), ObjVars, Res0, Res).

:- pred extract_obj_var(tableau, var, map(var, float), map(var, float)).
:- mode extract_obj_var(in, in, in, out) is det.

extract_obj_var(Tab, Var, Map0, Map) :-
	urs_vars(Tab, Vars),
	( map__search(Vars, Var, Pos - Neg) ->
		extract_obj_var2(Tab, Pos, PosVal),
		extract_obj_var2(Tab, Neg, NegVal),
		Val is PosVal - NegVal
	;
		extract_obj_var2(Tab, Var, Val)
	),
	map__set(Map0, Var, Val, Map).

:- pred extract_obj_var2(tableau, var, float).
:- mode extract_obj_var2(in, in, out) is det.

extract_obj_var2(Tab, Var, Val) :-
	var_col(Tab, Var, Col),
	GetCell = lambda([Val0::out] is nondet, (
		all_rows(Tab, Row),
		index(Tab, Row, Col, 1.0),
		rhs_col(Tab, RHS),
		index(Tab, Row, RHS, Val0)
	)),
	solutions(GetCell, Solns),
	( Solns = [Val1] ->
		Val = Val1
	;
		Val = 0.0
	).

:- pred simplex(tableau, tableau, bool, io__state, io__state).
:- mode simplex(in, out, out, di, uo) is det.

simplex(A0, A, Result, IO0, IO) :-
	AllColumns = all_cols(A0),
	MinAgg = lambda([Col::in, Min0::in, Min::out] is det, (
		(
			Min0 = no,
			index(A0, 0, Col, MinVal),
			( MinVal < 0.0 ->
				Min = yes(Col - MinVal)
			;
				Min = no
			)
		;
			Min0 = yes(_ - MinVal0),
			index(A0, 0, Col, CellVal),
			( CellVal < MinVal0 ->
				Min = yes(Col - CellVal)
			;
				Min = Min0
			)
		)
	)),
	aggregate(AllColumns, MinAgg, no, MinResult),
	(
		MinResult = no,
		A = A0,
		IO = IO0,
		Result = yes
	;
		MinResult = yes(Q - _Val),
		AllRows = all_rows(A0),
		MaxAgg = lambda([Row::in, Max0::in, Max::out] is det, (
			(
				Max0 = no,
				index(A0, Row, Q, MaxVal),
				( MaxVal > 0.0 ->
					rhs_col(A0, RHSC),
					index(A0, Row, RHSC, MVal),
					CVal is MVal/MaxVal,
					Max = yes(Row - CVal)
				;
					Max = no
				)
			;
				Max0 = yes(_ - MaxVal0),
				index(A0, Row, Q, CellVal),
				rhs_col(A0, RHSC),
				index(A0, Row, RHSC, MVal),
				(
					CellVal > 0.0,
					MaxVal1 is MVal/CellVal,
					MaxVal1 =< MaxVal0
				->
					Max = yes(Row - MaxVal1)
				;
					Max = Max0
				)
			)
		)),
		aggregate(AllRows, MaxAgg, no, MaxResult),
		(
			MaxResult = no,
			A = A0,
			IO = IO0,
			Result = no
		;
			MaxResult = yes(P - _),
			pivot(P, Q, A0, A1),
			simplex(A1, A, Result, IO0, IO)
		)
	).

%------------------------------------------------------------------------------%

:- pred ensure_zero_obj_coeffs(list(var), tableau, tableau).
:- mode ensure_zero_obj_coeffs(in, in, out) is det.

ensure_zero_obj_coeffs([], Tableau, Tableau).
ensure_zero_obj_coeffs([V|Vs], Tableau0, Tableau) :-
	var_col(Tableau0, V, Col),
	index(Tableau0, 0, Col, Val),
	( Val = 0.0 ->
		ensure_zero_obj_coeffs(Vs, Tableau0, Tableau)
	;
		FindOne = lambda([P::out] is nondet, (
			all_rows(Tableau0, R),
			index(Tableau0, R, Col, ValF0),
			ValF0 \= 0.0,
			P = R - ValF0
		)),
		solutions(FindOne, Ones),
		(
			Ones = [Row - Fac0|_],
			Fac is -Val/Fac0,
			row_op(Fac, Row, 0, Tableau0, Tableau1),
			ensure_zero_obj_coeffs(Vs, Tableau1, Tableau)
		;
			Ones = [],
			error("problem with artificial variable")
		)
	).

:- pred fix_basis_and_rem_cols(list(var), tableau, tableau).
:- mode fix_basis_and_rem_cols(in, in, out) is det.

fix_basis_and_rem_cols([], Tab, Tab).
fix_basis_and_rem_cols([V|Vs], Tab0, Tab) :-
	var_col(Tab0, V, Col),
	BasisAgg = lambda([R::in, Ones0::in, Ones::out] is det, (
		index(Tab0, R, Col, Val),
		( Val = 0.0 ->
			Ones = Ones0
		;
			Ones = [Val - R|Ones0]
		)
	)),
	aggregate(all_rows(Tab0), BasisAgg, [], Res),
	(
		Res = [1.0 - Row]
	->
		PivGoal = lambda([Col1::out] is nondet, (
			all_cols(Tab0, Col1),
			Col \= Col1,
			index(Tab0, Row, Col1, Zz),
			Zz \= 0.0
		)),
		solutions(PivGoal, PivSolns),
		(
			PivSolns = [],
			remove_col(Col, Tab0, Tab0a),
			remove_row(Row, Tab0a, Tab1)
		;
			PivSolns = [Col2|_],
			pivot(Row, Col2, Tab0, Tab0a),
			remove_col(Col, Tab0a, Tab1)
		)
	;
		Tab1 = Tab0
	),
	remove_col(Col, Tab1, Tab2),
	fix_basis_and_rem_cols(Vs, Tab2, Tab).

%------------------------------------------------------------------------------%

:- type cell	--->	cell(int, int).

:- pred pivot(int, int, tableau, tableau).
:- mode pivot(in, in, in, out) is det.

pivot(P, Q, A0, A) :-
	index(A0, P, Q, Apq),
	MostCells = lambda([Cell::out] is nondet, (
		all_rows0(A0, J),
		J \= P,
		all_cols0(A0, K),
		K \= Q,
		Cell = cell(J, K)
	)),
	ScaleCell = lambda([Cell::in, T0::in, T::out] is det, (
		Cell = cell(J, K),
		index(T0, J, K, Ajk),
		index(T0, J, Q, Ajq),
		index(T0, P, K, Apk),
		NewAjk is Ajk - Apk * Ajq / Apq,
		set_index(T0, J, K, NewAjk, T)
	)),
	aggregate(MostCells, ScaleCell, A0, A1),
	QColumn = lambda([Cell::out] is nondet, (
		all_rows0(A1, J),
		Cell = cell(J, Q)
	)),
	Zero = lambda([Cell::in, T0::in, T::out] is det, (
		Cell = cell(J, K),
		set_index(T0, J, K, 0.0, T)
	)),
	aggregate(QColumn, Zero, A1, A2),
	PRow = all_cols0(A2),
	ScaleRow = lambda([K::in, T0::in, T::out] is det, (
		index(T0, P, K, Apk),
		NewApk is Apk / Apq,
		set_index(T0, P, K, NewApk, T)
	)),
	aggregate(PRow, ScaleRow, A2, A3),
	set_index(A3, P, Q, 1.0, A).

:- pred row_op(float, int, int, tableau, tableau).
:- mode row_op(in, in, in, in, out) is det.

row_op(Scale, From, To, A0, A) :-
	AllCols = all_cols0(A0),
	AddRow = lambda([Col::in, T0::in, T::out] is det, (
		index(T0, From, Col, X),
		index(T0, To, Col, Y),
		Z is Y + (Scale * X),
		set_index(T0, To, Col, Z, T)
	)),
	aggregate(AllCols, AddRow, A0, A).

%------------------------------------------------------------------------------%

:- type tableau
	---> tableau(
		int,
		int,
		map(var, int),
		map(var, pair(var)),
		list(int),	% shunned rows
		list(int),	% shunned cols
		map(pair(int), float)
	).

:- pred init_tableau(int::in, int::in, map(var, int)::in, 
		map(var, pair(var))::in, tableau::out) is det.

init_tableau(Rows, Cols, VarNums, URSVars, Tableau) :-
	map__init(Cells),
	Tableau = tableau(Rows, Cols, VarNums, URSVars, [], [], Cells).

:- pred index(tableau, int, int, float).
:- mode index(in, in, in, out) is det.

index(Tableau, J, K, R) :-
	Tableau = tableau(_, _, _, _, SR, SC, Cells),
	(
		( list__member(J, SR)
		; list__member(K, SC)
		)
	->
		error("attempt to address shunned row/col")
	;
		true
	),
	(
		map__search(Cells, J - K, R0)
	->
		R = R0
	;
		R = 0.0
	).

:- pred set_index(tableau, int, int, float, tableau).
:- mode set_index(in, in, in, in, out) is det.

set_index(Tableau0, J, K, R, Tableau) :-
	Tableau0 = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells0),
	(
		( list__member(J, SR)
		; list__member(K, SC)
		)
	->
		error("attempt to write shunned row/col")
	;
		true
	),
	( R = 0.0 ->
		map__delete(Cells0, J - K, Cells)
	;
		map__set(Cells0, J - K, R, Cells)
	),
	Tableau = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells).

:- pred rhs_col(tableau, int).
:- mode rhs_col(in, out) is det.

rhs_col(tableau(_, RHS, _, _, _, _, _), RHS).

:- pred all_rows0(tableau, int).
:- mode all_rows0(in, out) is nondet.

all_rows0(Tableau, Row) :-
	Tableau = tableau(Rows, _Cols, _, _, SR, _, _),
	between(0, Rows, Row),
	\+ list__member(Row, SR).

:- pred all_rows(tableau, int).
:- mode all_rows(in, out) is nondet.

all_rows(Tableau, Row) :-
	Tableau = tableau(Rows, _Cols, _, _, SR, _, _),
	between(1, Rows, Row),
	\+ list__member(Row, SR).

:- pred all_cols0(tableau, int).
:- mode all_cols0(in, out) is nondet.

all_cols0(Tableau, Col) :-
	Tableau = tableau(_Rows, Cols, _, _, _, SC, _),
	between(0, Cols, Col),
	\+ list__member(Col, SC).

:- pred all_cols(tableau, int).
:- mode all_cols(in, out) is nondet.

all_cols(Tableau, Col) :-
	Tableau = tableau(_Rows, Cols, _, _, _, SC, _),
	Cols1 is Cols - 1,
	between(0, Cols1, Col),
	\+ list__member(Col, SC).

:- pred var_col(tableau, var, int).
:- mode var_col(in, in, out) is det.

var_col(Tableau, Var, Col) :-
	Tableau = tableau(_, _, VarCols, _, _, _, _),
	map__lookup(VarCols, Var, Col).

:- pred urs_vars(tableau, map(var, pair(var))).
:- mode urs_vars(in, out) is det.

urs_vars(Tableau, URS) :-
	Tableau = tableau(_, _, _, URS, _, _, _).

:- pred remove_row(int, tableau, tableau).
:- mode remove_row(in, in, out) is det.

remove_row(R, Tableau0, Tableau) :-
	Tableau0 = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells),
	Tableau = tableau(Rows, Cols, VarNums, URS, [R|SR], SC, Cells).

:- pred remove_col(int, tableau, tableau).
:- mode remove_col(in, in, out) is det.

remove_col(C, Tableau0, Tableau) :-
	Tableau0 = tableau(Rows, Cols, VarNums, URS, SR, SC, Cells),
	Tableau = tableau(Rows, Cols, VarNums, URS, SR, [C|SC], Cells).

:- pred get_basis_vars(tableau, list(var)).
:- mode get_basis_vars(in, out) is det.

get_basis_vars(Tab, Vars) :-
	BasisCol = lambda([C::out] is nondet, (
		all_cols(Tab, C),
		NonZeroGoal = lambda([P::out] is nondet, (
			all_rows(Tab, R),
			index(Tab, R, C, Z),
			Z \= 0.0,
			P = R - Z
		)),
		solutions(NonZeroGoal, Solns),
		Solns = [_ - 1.0]
	)),
	solutions(BasisCol, Cols),
	BasisVars = lambda([V::out] is nondet, (
		list__member(Col, Cols),
		Tab = tableau(_, _, VarCols, _, _, _, _),
		map__member(VarCols, V, Col)
	)),
	solutions(BasisVars, Vars).

%------------------------------------------------------------------------------%

	% For debugging ....

:- pred show_tableau(tableau, io__state, io__state).
:- mode show_tableau(in, di, uo) is det.

show_tableau(Tableau) -->
	{ Tableau = tableau(N, M, _, _, _, _, _) },
	{ string__format("Tableau (%d, %d):\n", [i(N), i(M)], Str) },
	io__write_string(Str),
	aggregate(all_rows0(Tableau), show_row(Tableau)).

:- pred show_row(tableau, int, io__state, io__state).
:- mode show_row(in, in, di, uo) is det.

show_row(Tableau, Row) -->
	aggregate(all_cols0(Tableau), show_cell(Tableau, Row)),
	io__write_string("\n").

:- pred show_cell(tableau, int, int, io__state, io__state).
:- mode show_cell(in, in, in, di, uo) is det.

show_cell(Tableau, Row, Col) -->
	{ index(Tableau, Row, Col, Val) },
	{ string__format("%2.2f\t", [f(Val)], Str) },
	io__write_string(Str).

%------------------------------------------------------------------------------%

:- pred lp_info_init(varset, list(var), lp_info).
:- mode lp_info_init(in, in, out) is det.

lp_info_init(Varset0, URSVars, LPInfo) :-
	Introduce = lambda([Orig::in, VP0::in, VP::out] is det, (
		VP0 = VS0 - VM0,
		varset__new_var(VS0, V1, VS1),
		varset__new_var(VS1, V2, VS),
		map__set(VM0, Orig, V1 - V2, VM),
		VP = VS - VM
	)),
	map__init(URSMap0),
	list__foldl(Introduce, URSVars, Varset0 - URSMap0, Varset - URSMap),
	LPInfo = lp(Varset, URSMap, [], []).

:- pred new_slack_var(var::out, lp_info::in, lp_info::out) is det.

new_slack_var(Var) -->
	get_varset(Varset0),
	{ varset__new_var(Varset0, Var, Varset) },
	set_varset(Varset),
	get_slack_vars(Vars),
	set_slack_vars([Var|Vars]).

:- pred new_art_var(var::out, lp_info::in, lp_info::out) is det.

new_art_var(Var) -->
	get_varset(Varset0),
	{ varset__new_var(Varset0, Var, Varset) },
	set_varset(Varset),
	get_art_vars(Vars),
	set_art_vars([Var|Vars]).

:- pred get_varset(varset::out, lp_info::in, lp_info::out) is det.

get_varset(Varset, Info, Info) :-
	Info = lp(Varset, _URSVars, _Slack, _Art).

:- pred set_varset(varset::in, lp_info::in, lp_info::out) is det.

set_varset(Varset, Info0, Info) :-
	Info0 = lp(_Varset, URSVars, Slack, Art),
	Info  = lp(Varset, URSVars, Slack, Art).

:- pred get_urs_vars(map(var, pair(var))::out, lp_info::in, lp_info::out) is det.

get_urs_vars(URSVars, Info, Info) :-
	Info = lp(_Varset, URSVars, _Slack, _Art).

:- pred set_urs_vars(map(var, pair(var))::in, lp_info::in, lp_info::out) is det.

set_urs_vars(URSVars, Info0, Info) :-
	Info0 = lp(Varset, _URSVars, Slack, Art),
	Info  = lp(Varset, URSVars, Slack, Art).

:- pred get_slack_vars(list(var)::out, lp_info::in, lp_info::out) is det.

get_slack_vars(Slack, Info, Info) :-
	Info = lp(_Varset, _URSVars, Slack, _Art).

:- pred set_slack_vars(list(var)::in, lp_info::in, lp_info::out) is det.

set_slack_vars(Slack, Info0, Info) :-
	Info0 = lp(Varset, URSVars, _Slack, Art),
	Info  = lp(Varset, URSVars, Slack, Art).

:- pred get_art_vars(list(var)::out, lp_info::in, lp_info::out) is det.

get_art_vars(Art, Info, Info) :-
	Info = lp(_Varset, _URSVars, _Slack, Art).

:- pred set_art_vars(list(var)::in, lp_info::in, lp_info::out) is det.

set_art_vars(Art, Info0, Info) :-
	Info0 = lp(Varset, URSVars, Slack, _Art),
	Info  = lp(Varset, URSVars, Slack, Art).

%------------------------------------------------------------------------------%

:- pred between(int, int, int).
:- mode between(in, in, out) is nondet.

between(Min, Max, I) :-
	Min =< Max,
	(
		I = Min
	;
		Min1 is Min + 1,
		between(Min1, Max, I)
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
