%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: ztype_op.m
% main author: philip

:- module ztype_op.

:- interface.
:- import_module list, assoc_list, pair, maybe, word, ztype, zabstract.

:- func givenT(ident) = ztype.
:- func powerT(ztype) = ztype.
:- func cproductT(list(ztype)) = ztype.
:- func schemaT(slist) = ztype.
:- func unityT = ztype.
:- func abbreviationT(ident, ztype, ztarity) = ztype.
:- func parameterT(ident) = ztype.

:- func numType = ztype.
:- func stringType = ztype.

:- type maybe0 ---> yes ; no.

:- type powerResult ---> yes ; no(ztype).

:- pred powerType(expr, ztype, ztype, powerResult, subst, subst) is det.
:- mode powerType(in, in, out, out, in, out) is det.

:- pred powerType(ztype::in, ztype::out, maybe0::out) is det.
:- pred schemaType(ztype::in, slist::out) is semidet.


:- type tselectResult ---> yes ; outsideArity(ztype) ; nontuple(ztype).

:- pred tupleSelect(int, ztype, ztype, tselectResult, subst, subst).
:- mode tupleSelect(in, in, out, out, in, out) is det.

:- type bselectResult ---> yes(slist) ; nonexistent(ztype) ; nonbinding(ztype).

:- pred bindingSelect(ident, ztype, ztype, bselectResult, subst, subst).
:- mode bindingSelect(in, in, out, out, in, out) is det.

:- func ztypePortray(operators, ztype) = string.
:- func declPortray(operators, pair(ident, ztype)) = string.

:- func ztypeBase(func(ident) = string, ztype) = string.
:- mode ztypeBase(func(in) = out is det, in) = out is det.

:- type unbound ---> unbound(expr, varsource).

:- pred incompletelyDetermined(list(unbound), subst, subst).
:- mode incompletelyDetermined(out, in, out) is det.

:- type applicationFlag ---> is_monotonic ; is_unknown.

:- type applicationError
	--->	nonfunction(ztype)
	;	mismatch(ztype, ztype)
	;	yes.

:- pred applyTypes(applicationFlag, expr, expr, ztype, ztype, ztype,
						applicationError, subst, subst).
:- mode applyTypes(in, in, in, in, in, out, out, in, out) is det.

:- pred decorate(decoration, slist, slist).
:- mode decorate(in, in, out) is det.

% :- type gen == int.

:- type actualisationResult ---> ok ; arityError(int, int).

:- pred actualise(expr, ref, ztarity, ztype, maybe(list(ztype)), ztype,
					actualisationResult, subst, subst).
:- mode actualise(in, in, in, in, in, out, out, in, out) is det.

:- type dlist == assoc_list(ident, entry).

:- pred makeGeneric(subst::in, list(ident)::in, slist::in, dlist::out) is det.

:- type mismatch2 ---> mismatch(ztype, ztype).
:- type mismatch3 ---> mismatch(ident, ztype, ztype).

:- pred check_sameL(expr, list(ztype), ztype, maybe(mismatch2),
								subst, subst).
:- mode check_sameL(in, in, out, out, in, out) is det.

:- type unify ---> unified ; failed(subst).

:- pred ztunify(ztype, ztype, ztype, unify, subst, subst) is det.
:- mode ztunify(in, in, out, out, in, out) is det.

% :- pred ztunify(ztype, ztype, ztype, subst, subst) is semidet.
% :- mode ztunify(in, in, out, in, out) is semidet.

:- pred rename(renaming, slist, slist, list(mismatch3), subst, subst).
:- mode rename(in, in, out, out, in, out) is det.

:- pred slist_sort(slist, slist, list(mismatch3), subst, subst).
:- mode slist_sort(in, out, out, in, out) is det.

:- pred slist_merge(slist, slist, slist, list(mismatch3), subst, subst).
:- mode slist_merge(in, in, out, out, in, out) is det.

% slist_project(From, Onto, Result, Exists, Errors)
:- pred slist_project(slist, slist, slist, slist, list(mismatch3),
							subst, subst).
:- mode slist_project(in, in, out, out, out, in, out) is det.

:- pred slist_quantify(slist, slist, slist, list(mismatch3), subst, subst).
:- mode slist_quantify(in, in, out, out, in, out) is det.

% slist_hide(TypeIn, Idents, TypeOut, IdentsType)
:- pred slist_hide(slist, list(ident), slist, slist).
:- mode slist_hide(in, in, out, out) is det.

:- pred slist_ident_comp(sconn, pair(ident, T), pair(ident, T)). 
:- mode slist_ident_comp(in, in, out) is semidet. 
:- mode slist_ident_comp(in, out, in) is semidet. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.
:- import_module string, require, int, map, set,
			higher_order.	% only for string_portray_list

:- import_module solutions.

ztypePortray(_, given(I)) =
	identPortray(I).
ztypePortray(O, power(T)) = S :-
	string__append("P ", ztypePortray(O, T), S).
ztypePortray(O, cross(L)) =
	string_portray_list(ztypePortray(O), "(", " x ", ")", L).
ztypePortray(O, schema(L)) = 
	string_portray_list(declPortray(O), "<|", "; ", "|>", L).
ztypePortray(_, var(ztvar(R, I))) = S :-
	string__int_to_string(R, SR),
	string__int_to_string(I, SI),
	string__append_list(["@", SR, ":", SI], S).
ztypePortray(_, parameter(I)) =
	identPortray(I).
ztypePortray(_, unity) =
	"*errortype*".
ztypePortray(O, abbreviation(I, L, _, _, _T)) = S :-
	S0 = identPortray(I),
	( L = [],
		S = S0
	; L = [_|_],
		( L = [T1],     map__search(O, I, pregen) ->
			string__append_list(
				["(", S0, " ", ztypePortray(O, T1), ")"], S)
		; L = [T1, T2], map__search(O, I, ingen) ->
			string__append_list(["(", ztypePortray(O, T1), " ", S0,
					" ", ztypePortray(O, T2), ")"], S)
		;	S1 = string_portray_list(ztypePortray(O), L),
			string__append(S0, S1, S)
		)
	).

declPortray(O, I-T) = S :-
	string__append_list([identPortray(I), ": ", ztypePortray(O, T)], S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ztypeBase(F, given(I)) = F(I).
ztypeBase(F, power(T)) = S :-
	string__append_list(["power(", ztypeBase(F, T), ")"], S).
ztypeBase(F, cross(L)) =
	string_portray_list(ztypeBase(F), "cross(", ", ", ")", L).
ztypeBase(F, schema(L)) = 
	string_portray_list(
		func(_-T) = ztypeBase(F, T), "schema(", ", ", ")", L).
ztypeBase(_, var(ztvar(R, I))) = S :-
	string__int_to_string(R, SR),
	string__int_to_string(I, SI),
	string__append_list(["T", SR, "_", SI], S).	% should never occur
ztypeBase(F, parameter(I)) = F(I).			% should never occur
ztypeBase(_, unity) = "*errortype*".			% should never occur
ztypeBase(F, abbreviation(I, L, _, _, _T)) = S :-
	S0 = F(I),					% should never occur
	( L = [],
		S = S0
	; L = [_|_],
		S1 = string_portray_list(ztypeBase(F), L),
		string__append(S0, S1, S)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Constructor functions
givenT(I) = given(I).

powerT(T) = power(T).

cproductT(LT) = unity	  :- LT = [].
cproductT(LT) = cross(LT) :- LT = [_|_].

schemaT(DL) = unity	 :- DL = [].
schemaT(DL) = schema(DL) :- DL = [_|_].

unityT = unity.

parameterT(Id) = parameter(Id).

abbreviationT(I, T0, F) = abbreviation(I, varList(0, 1, F), N, F, T) :-
	( powerType(T0, T1, yes) ->
		T = T1,
		( T = abbreviation(_, _, N0, _, _) -> N = N0 + 1 ; N = 1 )
	;	error("abbreviationT/3---abbreviation of non-power type")
	).
%%%

numType = given(numIdent).
stringType = given(stringIdent).

powerType(Expr, T0, T, M) -->
	=(S),
	type_var(Expr, power, T),
	ztunify(T0, power(T), _, U),
	{ U = unified, M = yes
	; U = failed(_), M = no(ztapply(S, T0))
	}.

powerType(given(_), unity, no).
powerType(power(T), T, yes).
powerType(cross(_), unity, no).
powerType(schema(_), unity, no).
powerType(var(_), unity, no).
powerType(parameter(_), unity, no).
powerType(unity, unity, yes).
powerType(abbreviation(_, TL, _, _, T0), T, M) :-
	upAbbrevTree(TL, T0, ParentT), powerType(ParentT, T, M).

schemaType(schema(DL), DL).
schemaType(unity, []).

tupleSelect(N, T0, T, M) -->
	=(S),
	{T1 = ztapply(S, T0)},
	( {T1 = cross(L)} ->
		( {list__index1(L, N, T2)} ->
			{T = T2, M = yes}
	    	;	varsToUnity(T0), {T = unity, M = outsideArity(T1)}
		)
	; {T1 = unity} ->
		{T = unity, M = yes}
	;	varsToUnity(T0), {T = unity, M = nontuple(T1)}
	).

bindingSelect(I, T0, T, M) -->
	=(S),
	{T1 = ztapply(S, T0)},
	( {T1 = schema(DL)} ->
		( {assoc_list_lookup(I, DL, T2)} ->
			{T = T2, M = yes(DL)}
	    	;	varsToUnity(T0), {T = unity, M = nonexistent(T1)}
		)
	; 	( {T1 = unity} ->
			{T = unity, M = yes([])}
		;	varsToUnity(T0), {T = unity, M = nonbinding(T1)}
		)
	).

:- pred type_var(expr::in, varsource::in, ztype::out, subst::in, subst::out).
type_var(Expr, Source, var(V), S0-Ref, S) :-
	V = ztvar(Ref, 0),
	subst_insert(V, varinfo(Expr, Source, no, normal), S0-(Ref+1), S).

:- pred type_var(expr::in, varsource::in, apply_status::in, ztype::out,
							subst::in, subst::out).
type_var(Expr, Source, ApplyStatus, var(V), S0-Ref, S) :-
	V = ztvar(Ref, 0),
	subst_insert(V, varinfo(Expr, Source, no, ApplyStatus), S0-(Ref+1), S).

applyTypes(Flag, FunExpr, AppExpr, Function, Actual, Result, M) -->
	{ Flag = is_monotonic, AS = normal
	; Flag = is_unknown, AS = from_apply 
	},
	type_var(FunExpr, function_arg, Formal),
	type_var(AppExpr, function_result, AS, Result),
	ztunify(Function, power(cross([Formal, Result])), _, U),
	( {U = unified},
		ztunify(Formal, Actual, _, U1),
		( {U1 = unified, M = yes}
		; {U1 = failed(S),
			M = mismatch(ztapply(S, Formal), ztapply(S, Actual))}
		)
	; {U = failed(S), M = nonfunction(ztapply(S, Function))}
	).
	
:- pred do_decl(pred(ztype, ztype, subst, subst), pair(ident, ztype),
					pair(ident, ztype), subst, subst).
:- mode do_decl(pred(in, out, in, out) is det, in, out, in, out) is det.
do_decl(P, I-H0, I-H) --> P(H0, H).

:- pred do_decl(pred(ztype, ztype), pair(ident, ztype), pair(ident, ztype)).
:- mode do_decl(pred(in, out) is det, in, out) is det.
do_decl(P, I-H0, I-H) :- P(H0, H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- func formalsMap(list(ident)) = map(ident, ztvar).
formalsMap(F) = M :-
	P = (pred(I::in, I-ztvar(0, N)::out, N::in, N+1::out) is det),
	list__map_foldl(P, F, AL, 1, _), % Start numbering generic params from 1
	map__from_assoc_list(AL, M).

makeGeneric(Subst, F, DL, GDL) :-
	list__length(F, FL),
	M = formalsMap(F),
	P = (pred(I-T0::in, I-O::out) is det :-
			O = e(FL, T),
			makeGType(M, ztapply(Subst, T0), T)),
	list__map(P, DL, GDL).

:- pred makeGType(map(ident, ztvar)::in, ztype::in, ztype::out) is det.
makeGType(_, given(I), given(I)).
makeGType(M, power(T0), power(T)) :- makeGType(M, T0, T).
makeGType(M, cross(L0), cross(L)) :- list__map(makeGType(M), L0, L).
makeGType(M, schema(DL0), schema(DL)) :-
	list__map(do_decl(makeGType(M)), DL0, DL).
makeGType(_, var(_), _) :- error("makeGType/3: type variable in type").
makeGType(M, parameter(I), T) :-
	( map__search(M, I, V) ->
		T = var(V)
	;	string__append(
		    "makeGType/3: parameter not found--", identPortray(I), S),
		error(S)
	).
makeGType(_, unity, unity).
makeGType(M, abbreviation(I, L0, D, N, T), abbreviation(I, L, D, N, T)) :-
	list__map(makeGType(M), L0, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ref to implicit empty-set
check_sameL(Exp, L, T, M) -->
	type_var(Exp, extension, V),
	check_sameL1(V, L, T, M).

:- pred check_sameL1(ztype, list(ztype), ztype, maybe(mismatch2),
								subst, subst).
:- mode check_sameL1(in, in, out, out, in, out) is det.
check_sameL1(T, [], T, no) --> [].
check_sameL1(T0, [T1|L], T, M) -->
	ztunify(T0, T1, T3, U),
	{ U = unified,
		M = M1
	; U = failed(S),
		M = yes(mismatch(ztapply(S, T0), ztapply(S, T1)))
		% M1 ignored since T3 = unity and no other errors are possible
	},
	check_sameL1(T3, L, T, M1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list [var(Min) .. var(Max)]
:- func varList(int, int, int) = list(ztype).
varList(Ref, Min, Max) = L :-
	( Min > Max ->
		L = []
	;	L = [var(ztvar(Ref, Min))|varList(Ref, Min + 1, Max)]
	).

incompletelyDetermined(LE) -->
	=(Sub-_),
	{P0 = (pred(V-I::out) is nondet :-
		map__member(Sub, V, I), I = varinfo(_, _, no, _)),
	solutions(P0, LB)},		% select entries for unbound variables
	{P1 = (pred(V-varinfo(E, S, _, A)::in, in, out) is det -->
		subst_update(V, varinfo(E, S, yes(unity), A)))},
	list__foldl(P1, LB),		% bind those variables to unity
	{P2 = (pred(_V-varinfo(E, S, _, _)::in, unbound(E, S)::out) is det),
	list__map(P2, LB, LE)}.		% convert entries to error format

% completelyDetermined(Sub, T) :- ground(Sub, T).

% :- pred ground(subst::in, ztype::in) is semidet.
% ground(_, given(_)).
% ground(S, power(T)) :- ground(S, T).
% ground(S, cross(L)) :- list_and(ground(S), L).
% ground(S, schema(DL)) :-
% 	list_and((pred(_-T::in) is semidet :- ground(S, T)), DL).
% ground(S, var(V)) :- subst_lookup(S, V, varinfo(_, _, yes(T), _)), ground(S, T).
% ground(_, parameter(_)).
% ground(_, unity).
% ground(S, abbreviation(_, L, _, _, _)) :- list_and(ground(S), L).

%%%%%
actualise(Expr, Ref, F, T0, MActuals, T, Result) -->
	( {MActuals = no},
		{Result = ok},
		dupVars(Expr, Ref, 1, F, Actuals)
	; {MActuals = yes(Actuals0),
		list__length(Actuals0, A)},
		( {A = F} ->
			{Result = ok, Actuals = Actuals0}
		;	{Result = arityError(F, A)},
			list__foldl(varsToUnity, Actuals0),
			{list__duplicate(F, unity, Actuals)}
		)
	),
	{ F = 0 -> T = T0 ; ztdup(Actuals, T0, T) }.

:- pred dupVars(expr, ref, int, int, list(ztype), subst, subst).
:- mode dupVars(in, in, in, in, out, in, out) is det.
dupVars(Expr, Ref, Current, Arity, TL) -->
	( {Current > Arity} ->
		{TL = []}
	;	type_var(Expr, parameter(Ref, Current), V),
		dupVars(Expr, Ref, Current+1, Arity, TL1),
		{TL = [V|TL1]}
	).

:- pred ztdup(list(ztype), ztype, ztype).
:- mode ztdup(in, in, out) is det.
ztdup(_, given(I), given(I)).
ztdup(P, power(T0), power(T)) :- ztdup(P, T0, T).
ztdup(P, cross(L0), cross(L)) :- list__map(ztdup(P), L0, L).
ztdup(P, schema(DL0), schema(DL)) :- list__map(do_decl(ztdup(P)), DL0, DL).
ztdup(P, var(ztvar(_, I)), V) :- list__index1_det(P, I, V).	% Assert: _ = 0
ztdup(_, parameter(_I), _) :- error("ztdup/3: parameter being duplicated").
ztdup(_, unity, unity).
ztdup(P, abbreviation(I, L0, D, N, T), abbreviation(I, L, D, N, T)) :-
	list__map(ztdup(P), L0, L).
%%%%%

ztunify(T1, T2, T, U) -->
	( ztunify(T1, T2, T3) ->
		{T = T3, U = unified}
	;	=(S), {T = unity, U = failed(S)},
	% The following is to avoid spurious errors from unbound type vars
		varsToUnity(T1), varsToUnity(T2)
	).

:- pred varsToUnity(ztype, subst, subst).
:- mode varsToUnity(in, in, out) is det.
varsToUnity(given(_)) --> [].
varsToUnity(power(T)) --> varsToUnity(T).
varsToUnity(cross(L)) --> list__foldl(varsToUnity, L).
varsToUnity(schema(DL)) -->
	{P = (pred(_-H::in, L0::in, L::out) is det :- varsToUnity(H, L0, L))},
	list__foldl(P, DL).
varsToUnity(var(V)) -->
	% Keeping E and Src correct should be unnecessary since no
	% error should be reported on this variable
	subst_lookup(V, varinfo(E, Src, MT, A)),
	( {MT = no}, subst_update(V, varinfo(E, Src, yes(unity), A)) % bind
	; {MT = yes(T)}, varsToUnity(T)
	).
varsToUnity(parameter(_)) --> [].
varsToUnity(unity) --> [].
varsToUnity(abbreviation(_, L, _, _, _)) --> list__foldl(varsToUnity, L).

:- pred ztunify(ztype, ztype, ztype, subst, subst).
:- mode ztunify(in, in, out, in, out) is semidet.
ztunify(T1, T2, T) -->
	( {T1 = unity} -> varsToUnity(T2), {T = unity}
	; {T2 = unity} -> varsToUnity(T1), {T = unity}
	; {T1 = var(I1)} -> zvtunify(I1, T2, T)
	; {T2 = var(I2)} -> zvtunify(I2, T1, T)
	; {T1 = abbreviation(I, L, D, N, AT)} ->
		zabbtunify(I, L, D, N, AT, T2, T)
	; {T2 = abbreviation(I, L, D, N, AT)} ->
		zabbtunify(I, L, D, N, AT, T1, T)
	; {T1 = given(I)} ->
		{T2 = given(I), T = T2}
	; {T1 = parameter(I)} ->
		{T2 = parameter(I), T = T2}
	; {T1 = power(P1)} ->
		{T2 = power(P2), T = power(P)}, ztunify(P1, P2, P)
	; {T1 = cross(L1)} ->
		{T2 = cross(L2), T = cross(L)}, ztunifyL(L1, L2, L)
	; {T1 = schema(DL1)} ->
		{T2 = schema(DL2), T = schema(DL)}, ztunifyDL(DL1, DL2, DL)
		% sorting should be unnecessary
		% list__sort(DL1, SL1), list__sort(DL2, SL2)},
	; {error("ztunify")}
	).

:- pred zabbtunify(ident, list(ztype), depth, ztarity, ztype, ztype, ztype,
	subst, subst).
:- mode zabbtunify(in, in, in, in, in, in, out, in, out) is semidet.
zabbtunify(I1, L1, D1, N1, AT1, T2, T) -->
	( {T2 = abbreviation(I2, L2, D2, N2, AT2)} ->
		( {I1 = I2} ->
			ztunifyL(L1, L2, L),
			{T = abbreviation(I2, L, D2, N2, AT2)}
		; {D1 < D2} ->
			{upAbbrevTree(L2, AT2, ParentT2)},
			zabbtunify(I1, L1, D1, N1, AT1, ParentT2, T)
		; {D1 > D2} ->
			{upAbbrevTree(L1, AT1, ParentT1)},
			zabbtunify(I2, L2, D2, N2, AT2, ParentT1, T)
		; {D1 = D2},
			{upAbbrevTree(L1, AT1, ParentT1)},
			{upAbbrevTree(L2, AT2, ParentT2)},
			ztunify(ParentT1, ParentT2, T)
		)
	;	{upAbbrevTree(L1, AT1, ParentT1)},
		ztunify(ParentT1, T2, T)
	).

:- pred upAbbrevTree(list(ztype), ztype, ztype).
:- mode upAbbrevTree(in, in, out) is det.
upAbbrevTree(L, AT, ParentT) :- ztdup(L, AT, ParentT).

:- pred unAbbrev(ztype, ztype, subst, subst).
:- mode unAbbrev(in, out, in, out) is det.
unAbbrev(given(I), given(I)) --> [].
unAbbrev(power(T0), power(T)) --> unAbbrev(T0, T).
unAbbrev(cross(L0), cross(L)) --> list__map_foldl(unAbbrev, L0, L).
unAbbrev(schema(DL0), schema(DL)) -->
	list__map_foldl(do_decl(unAbbrev), DL0, DL).
unAbbrev(var(V), var(V)) -->
	subst_lookup(V, varinfo(C, I, MT, A)),
	( {MT = yes(VT0)},
		unAbbrev(VT0, VT),
		subst_update(V, varinfo(C, I, yes(VT), A)) 	% bind
	; {MT = no}
	).
unAbbrev(parameter(I), parameter(I)) --> [].
unAbbrev(unity, unity) --> [].
unAbbrev(abbreviation(_I, L0, _D, _N, T0), T) -->
	list__map_foldl(unAbbrev, L0, L),
	{upAbbrevTree(L, T0, T1)},
	unAbbrev(T1, T).	% BUG: This is inefficient because
				% the list of params will be unAbbrev'ed
				% for each level of abbreviation.

:- pred zvtunify(ztvar, ztype, ztype, subst, subst).
:- mode zvtunify(in, in, out, in, out) is semidet.
zvtunify(V, T0, T) -->
	( {T0 = var(V)} ->
		{T = T0}		% variable being unified with itself
	;	subst_lookup(V, varinfo(E, Src, MT, A)),
		( {A = normal, T1 = T0} ; {A = from_apply}, unAbbrev(T0, T1) ),
		( {MT = yes(VT)}, ztunify(VT, T1, T)
		; {MT = no}, zvtunifyFree(E, Src, A, V, T1, T)
		)
	).

:- pred zvtunifyFree(expr, varsource, apply_status, ztvar, ztype, ztype,
								subst, subst).
:- mode zvtunifyFree(in, in, in, in, in, out, in, out) is semidet.
zvtunifyFree(E, Src, A, V, T0, T) -->
	( {T0 = var(V1)} ->
		{compare(Op, V, V1)},
		( {Op = (<),
			T = T0},
			subst_update(V, varinfo(E, Src, yes(T0), A))% bind
		; {Op = (=),
			T = T0}	% variable being unified with itself (see above)
		; {Op = (>)},
			subst_lookup(V1, varinfo(E1, Src11, MT1, A1)),	  % bind
			subst_update(V1, varinfo(E1, Src11, yes(var(V)), A1)),
			( {MT1 = yes(VT1)},
				( {A = normal, VT2 = VT1}
				; {A = from_apply}, unAbbrev(VT1, VT2)
				),
				zvtunifyFree(E, Src, A, V, VT2, T)
			; {MT1 = no, T = var(V)}
			)
		)
	;	=(S), {not occurs(V, S, T0)},
		{T = T0},
		subst_update(V, varinfo(E, Src, yes(T0), A)) 	% bind
	).

:- pred occurs(ztvar::in, subst::in, ztype::in) is semidet.
occurs(V, S, power(T)) :- occurs(V, S, T).
occurs(V, S, cross(L)) :- list_or(occurs(V, S), L).
occurs(V, S, schema(DL)) :-
	P = (pred(_-T::in) is semidet :- occurs(V, S, T)),
	list_or(P, DL).
occurs(V, S, var(V1)) :-
	( V = V1 ->
		true
	;	subst_lookup(S, V1, varinfo(_, _, yes(T), _)),
		occurs(V, S, T)
	).
occurs(V, S, abbreviation(_, L, _, _, _)) :- list_or(occurs(V, S), L).

:- pred ztunifyL(list(ztype), list(ztype), list(ztype), subst, subst).
:- mode ztunifyL(in, in, out, in, out) is semidet.
ztunifyL([], [], []) --> [].
ztunifyL([H1|T1], [H2|T2], [H|T]) --> ztunify(H1, H2, H), ztunifyL(T1, T2, T).

:- pred ztunifyDL(slist, slist, slist, subst, subst).
:- mode ztunifyDL(in, in, out, in, out) is semidet.
ztunifyDL([], [], []) --> [].
ztunifyDL([(I-H1)|T1], [(I-H2)|T2], [(I-H)|T]) -->
	ztunify(H1, H2, H), ztunifyDL(T1, T2, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% slist service predicates

:- pred assoc_list_lookup(K, assoc_list(K, V), V).
:- mode assoc_list_lookup(in, in, out) is semidet.
assoc_list_lookup(K, [K0-V0|L], V) :-
	compare(O, K, K0),
	( O = (=), V = V0
	; O = (>), assoc_list_lookup(K, L, V)
	).

decorate(_, [], []).
decorate(D1, [id(M, N, D0)-E|T0], [id(M, N, D)-E|T]) :-
	list__append(D1, D0, D), decorate(D1, T0, T).

rename(R, D0, D, L) -->
	{map__from_assoc_list(R, M),
	P = (pred(In::in, Out::out) is det :-
		In = I0-E, (map__search(M, I0, I) -> Out = I-E ; Out = In)),
	list__map(P, D0, D1)},
	slist_sort(D1, D, L).	
% should check that all R have been (can be) applied

slist_sort([], [], []) -->
	[].
slist_sort(List, SortedList, L) -->
	{List = [_|List1]},
	( {List1 = [], SortedList = List, L = []}
	; {List1 = [_|_],
		list__length(List, Length)},
		( {list__split_list(Length // 2, List, Front, Back)} ->
			slist_sort(Front, SortedFront, L1),
			slist_sort(Back, SortedBack, L2),
			slist_merge(SortedFront, SortedBack, SortedList, L0),
			{list__append(L0, L1, L01), list__append(L2, L01, L)}
			% should use accumulators
		;       {error("slist_sort")}
		)
	).

slist_merge([], [], [], []) --> [].
slist_merge([],  B,  B, []) --> {B = [_|_]}.
slist_merge( A, [],  A, []) --> {A = [_|_]}.
slist_merge( A,  B,  C,  L) --> {A = [X|Xs], B = [Y|Ys]},
	{C = [Z|Zs], X = XK-XT, Y = YK-YT, compare(O, XK, YK)},
	( {O = (<), A1 = Xs, B1 =  B, Z = X, L = L0},
		slist_merge(A1, B1, Zs, L0)
	; {O = (>), A1 =  A, B1 = Ys, Z = Y, L = L0},
		slist_merge(A1, B1, Zs, L0)
	; {O = (=), A1 = Xs, B1 = Ys, Z = XK-ZT},
		ztunify(XT, YT, ZT, U),
		{ U = unified,
			L = L0
		; U = failed(S),
			L = [mismatch(XK, ztapply(S, XT), ztapply(S, YT))|L0]
		},
		slist_merge(A1, B1, Zs, L0)
	).

slist_project([], [], [], [], []) --> [].
slist_project([],  B, [], [], []) --> {B = [_|_]}.
slist_project( A, [], [],  A, []) --> {A = [_|_]}.
slist_project( A,  B,  C,  D,  L) --> {A = [X|Xs], B = [Y|Ys]},
	{X = XK-XT, Y = YK-YT, compare(O, XK, YK)},
	( {O = (<), A1 = Xs, B1 =  B, C1 = C, D = [X|D1], L1 = L},
		slist_project(A1, B1, C1, D1, L1)
	; {O = (>), A1 =  A, B1 = Ys, C1 = C, D1 = D, L1 = L},
		slist_project(A1, B1, C1, D1, L1)
	; {O = (=), A1 = Xs, B1 = Ys, C = [XK-ZT|C1], D1 = D},
		ztunify(XT, YT, ZT, U),
		{ U = unified,
			L = L1
		; U = failed(S),
			L = [mismatch(XK, ztapply(S, XT), ztapply(S, YT))|L1]
		},
		slist_project(A1, B1, C1, D1, L1)
	).

slist_quantify([], [], [], []) --> [].
slist_quantify([],  B, [], []) --> {B = [_|_]}.
slist_quantify( A, [],  A, []) --> {A = [_|_]}.
slist_quantify( A,  B,  C,  L) --> {A = [X|Xs], B = [Y|Ys]},
	{X = XK-XT, Y = YK-YT, compare(O, XK, YK)},
	( {O = (<), A1 = Xs, B1 =  B, C = [X|C1], L = L1},
		slist_quantify(A1, B1, C1, L1)
	; {O = (>), A1 =  A, B1 = Ys, C = C1, L = L1},
		slist_quantify(A1, B1, C1, L1)
	; {O = (=), A1 = Xs, B1 = Ys, C = C1},
		ztunify(XT, YT, _, U),
		{ U = unified,
			L = L1
		; U = failed(S),
			L = [mismatch(XK, ztapply(S, XT), ztapply(S, YT))|L1]
		},
		slist_quantify(A1, B1, C1, L1)
	).

slist_hide([], [], [], []).
slist_hide([],  B, [], []) :- B = [_|_].
slist_hide( A, [],  A, []) :- A = [_|_].
slist_hide( A,  B,  C, D) :-
	A = [X|Xs], B = [YK|Ys], X = XK-_,
	compare(O, XK, YK),
	( O = (<), C = [X|C1], D = D1, slist_hide(Xs, B, C1, D1)
	; O = (>), D = D1, slist_hide(A, Ys, C, D1)
	; O = (=), D = [X|D1], slist_hide(Xs, Ys, C, D1)
	).

slist_ident_comp(composition,
	id(no, Id, [prime|D])-X,
	id(no, Id, D)-X).
slist_ident_comp(piping,
	id(no, Id, [exclamation_mark|D])-X,
	id(no, Id, [question_mark|D])-X).


%%%
% Higher order utilities
%

        % list__or(Pred, List) applies Pred, a closure with one input argument,
	% to the Elements of List, and is false iff there is no elements for
	% which Pred is true.
:- pred list_or(pred(X), list(X)).
:- mode list_or(pred(in) is semidet, in) is semidet.
list_or(_, []) :- fail.
list_or(P, [H|T]) :- P(H) ; list_or(P, T).

        % list__and(Pred, List) applies Pred, a closure with one input argument,
	% to the Elements of List, and is true iff there is no elements for
	% which Pred is false.
:- pred list_and(pred(X), list(X)).
:- mode list_and(pred(in) is semidet, in) is semidet.
list_and(_, []) :- true.
list_and(P, [H|T]) :- P(H), list_and(P, T).
