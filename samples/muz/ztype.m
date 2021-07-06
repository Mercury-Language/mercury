%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: ztype.m
% main author: philip

:- module ztype.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module word.
:- import_module zabstract.

:- type ztype
    --->    given(ident)
    ;       power(ztype)
    ;       cross(list(ztype))
    ;       schema(slist)
    ;       var(ztvar)
    ;       parameter(ident)
    ;       unity
    ;       abbreviation(ident, list(ztype), depth, ztarity, ztype).

:- type slist == assoc_list(ident, ztype).

:- type ztvar
    --->    ztvar(ref, int).
            % ref is identifer reference that introduces the variable,
            % and the int is the ordinal of the generic parameter
            % represented by the variable.

:- type varsource
    --->    parameter(ref, int)
    ;       function_arg
    ;       function_result
    ;       power
    ;       extension.

:- type apply_status
    --->    normal
    ;   from_apply.

:- type varinfo
    --->    varinfo(expr, varsource, maybe(ztype), apply_status).

:- type subst == pair(map(ztvar, varinfo), ref).

:- func initSubst = subst.

:- func ztapply(subst, ztype) = ztype.

:- pred subst_lookup(subst::in, ztvar::in, varinfo::out) is det.
:- pred subst_lookup(ztvar::in, varinfo::out, subst::in, subst::out) is det.
:- pred subst_insert(ztvar::in, varinfo::in, subst::in, subst::out) is det.
:- pred subst_update(ztvar::in, varinfo::in, subst::in, subst::out) is det.

:- type depth == int.       % >= 1

:- type ztarity == int.     % >= 0

:- type entry
    --->    e(ztarity, ztype).

:- func powerEntry = entry.
:- func givenType(ident) = ztype.
:- func givenEntry(ident) = entry.
:- func branchEntry(ident, ztype) = entry.

%%%

% :- type ptype == pair(int, ztype).
:- type ptypes.

:- func initPtypes = ptypes.

% :- pred addPtypes(assoc_list(ref, ptype)::in, ptypes::in, ptypes::out) is det.

:- pred add_sexpr_type(ref::in, slist::in, ptypes::in, ptypes::out) is det.

:- pred find_sexpr_type(ref::in, slist::out, ptypes::in) is det.

% :- pred findPtypes(subst::in, ref::in, list(expr)::out) is semidet.

:- type gentypes.

:- func initGenTypes = gentypes.

:- pred substToGenTypes(subst::in, gentypes::out) is det.

:- pred getGenType(gentypes::in, ref::in, maybe(list(expr))::out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- implementation.

:- import_module builtin.
:- import_module int.
:- import_module require.
:- import_module string.

powerEntry = e(1, power(power(var(ztvar(0, 1))))).
givenType(Id) = power(given(Id)).
givenEntry(Id) = e(0, power(given(Id))).
branchEntry(Id, T) = e(0, power(cross([T, given(Id)]))).

% This type contains information about the inferred types
% for each parameter for every reference to a generic identifier.
:- type ptypes == map(ref, slist).

initSubst = S-0 :-
    map.init(S).

initPtypes = PM :-
    map.init(PM).

add_sexpr_type(R, SL, P0, P) :-
    map.det_insert(R, SL, P0, P).

% *** Make this semidet, and handle failure case in zclp.m ***
find_sexpr_type(R, SL, P) :-
    map.lookup(P, R, SL).
%   Pred = (pred(SL0::out) is nondet :-
%       multi_map.nondet_search(P, R, 0 - schema(SL0))),
%   solutions(Pred, Solutions),
%   ( if Solutions = [] then
%       string.format("find_sexpr_type/3: ref %p not found",
%           [i(R)], Mesg),
%       error(Mesg)
%   else if Solutions = [SL1] then
%       SL = SL1
%   else
%       string.format("find_sexpr_type/3: ref %p multiple entries",
%           [i(R)], Mesg),
%       error(Mesg)
%   ).

% findPtypes(Subst, Ref, to_exprL(L)) :-
%   Subst = Subst1-_,
%   map.to_assoc_list(Subst1, L0),
%   P = (pred(_ - Info::in, Ord - T::out) is semidet :-
%       Info = varinfo(_, parameter(Ref, Ord), yes(T0), _),
%       Ord > 0,
%       T = ztapply(Subst, T0)),
%       % 0 is used for the enclosing type, not just one parameter
%   list.filter_map(P, L0, L1),
%   % P = (pred(Ord - ztapply(Subst, T)::out) is nondet :-
%   %   map.member(Subst1, ztvar(Ord, _),
%   %       varinfo(ref(Ref, _, _)-_, _, yes(T), _)),
%   %   Ord > 0),
%   %   % 0 is used for the enclosing type, not just one parameter
%   % solutions(P, L1),
%   list.sort(L1, L2),
%   assoc_list.values(L2, L).

:- type gentypes == map(ref, list(expr)).

initGenTypes = M :-
    map.init(M).

:- type triple
    --->    triple(ref, int, expr).

% Reverse ordering used because list is built up in reverse
:- pred ordSort(triple::in, triple::in, comparison_result::out) is det.

ordSort(triple(_, Ord1, _), triple(_, Ord2, _), C) :-
    compare(C, Ord2, Ord1).

:- pred refSort(triple::in, triple::in, comparison_result::out) is det.

refSort(triple(Ref1, _, _), triple(Ref2, _, _), C) :-
    compare(C, Ref1, Ref2).

substToGenTypes(Subst, GenTypes) :-
    Subst = Subst1-_,
    map.to_assoc_list(Subst1, L0),
    P =
        ( pred(_ - Info::in, triple(Ref, Ord, T)::out) is semidet :-
            Info = varinfo(_, parameter(Ref, Ord), yes(T0), _),
            T = to_expr(ztapply(Subst, T0)) - 0
        ),
    list.filter_map(P, L0, L1),
    list.sort(ordSort, L1, L2),
    list.sort(refSort, L2, L3),
    listToGenTypes(L3, GenTypes).   % Group then insert in map

:- pred listToGenTypes(list(triple)::in, gentypes::out) is det.

listToGenTypes([], GenTypes) :-
    map.init(GenTypes).
listToGenTypes(List, GenTypes) :-
    List = [triple(Ref, _, Expr) | List1],
    map.init(M0),
    listToGenTypes(Ref, [Expr], List1, M0, GenTypes).

:- pred listToGenTypes(ref::in, list(expr)::in, list(triple)::in,
    gentypes::in, gentypes::out) is det.

listToGenTypes(Ref, ExprList, [], M0, M) :-
    map.det_insert(Ref, ExprList, M0, M).
listToGenTypes(Ref, ExprList, [triple(Ref1, _, Expr) | List], M0, M) :-
    ( if Ref = Ref1 then
        listToGenTypes(Ref, [Expr | ExprList], List, M0, M)
    else
        map.det_insert(Ref, ExprList, M0, M1),
        listToGenTypes(Ref1, [Expr], List, M1, M)
    ).

getGenType(GenTypes, Ref, M) :-
    ( if map.search(GenTypes, Ref, L) then
        M = yes(L)
    else
        M = no
    ).

:- func to_expr(ztype) = expr1.

to_expr(given(I)) = ref(0, I, no).
to_expr(power(T)) = powerset(to_expr(T)-0).
to_expr(cross(L)) = product(to_exprL(L)).
to_expr(schema(SL)) = sexp(sexpr(0, text(to_declL(SL), []), 0)).
to_expr(var(_)) = _ :-
    error("to_expr/2: converting non-ground type to expr").
to_expr(parameter(I)) = ref(0, I, no).
to_expr(unity) = _ :-
    error("to_expr/2: converting unity to expr").
to_expr(abbreviation(I, L, _, _, _)) = ref(0, I, yes(to_exprL(L))).

% *** Could sort this by type to produce simpler logic representation ***
:- func to_declL(slist) = list(decl).

to_declL([]) = [].
to_declL([I - T | L]) = [decl([I], to_expr(T)-0) | to_declL(L)].

:- func to_exprL(list(ztype)) = list(expr).

to_exprL([]) = [].
to_exprL([H | T]) = [to_expr(H)-0 | to_exprL(T)].

ztapply(_, G) = G :-
    G = given(_).
ztapply(S, power(T)) = power(ztapply(S, T)).
ztapply(S, cross(L0)) = cross(L) :-
    list.map(
        ( pred(T::in, U::out) is det :-
            U = ztapply(S, T)
        ), L0, L).
ztapply(S, schema(DL0)) = schema(DL) :-
    list.map(do_decl(ztapply(S)), DL0, DL).
ztapply(S, V) = T :-
    V = var(I),
    subst_lookup(S, I, varinfo(_, _, MT, _)),
    (MT = yes(T1), T = ztapply(S, T1) ; MT = no, T = V ).
ztapply(_, P) = P :-
    P = parameter(_).
ztapply(_, unity) = unity.
ztapply(S, abbreviation(I, L0, D, N, T)) = abbreviation(I, L , D, N, T) :-
    list.map(
        ( pred(IN::in, OUT::out) is det :-
            OUT = ztapply(S, IN)
        ), L0, L).

:- pred do_decl(func(ztype) = ztype, pair(ident, ztype), pair(ident, ztype)).
:- mode do_decl(func(in) = out is det, in, out) is det.

do_decl(F, I - H, I - F(H)).

subst_lookup(S-_, V, VI) :-
    ( if map.search(S, V, VI0) then
        VI = VI0
    else
        V = ztvar(Ref, Int),
        string.format("subst_lookup/4: var %i:%i not found",
            [i(Ref), i(Int)], Str),
        error(Str)
    ).

subst_lookup(V, VI, S, S) :-
    subst_lookup(S, V, VI).

subst_insert(V, VI, S0 - G, S - G) :-
    ( if map.insert(V, VI, S0, S1) then
        S = S1
    else
        V = ztvar(Ref, Int),
        string.format("subst_insert/4: var %i:%i already in subst",
                            [i(Ref), i(Int)], Str),
        error(Str)
    ).

subst_update(V, VI, S0 - G, S - G) :-
    ( if map.update(V, VI, S0, S1) then
        S = S1
    else
        V = ztvar(Ref, Int),
        string.format("subst_update/4: var %i:%i not found",
            [i(Ref), i(Int)], Str),
        error(Str)
    ).
