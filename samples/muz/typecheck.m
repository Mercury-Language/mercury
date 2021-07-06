%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: typecheck.m
% main author: philip

:- module typecheck.

:- interface.

:- import_module dict.
:- import_module io.
:- import_module list.
:- import_module word.
:- import_module zabstract.
:- import_module ztype.

% :- type typed_par == triple(par, subst, ptypes).

:- type type_result
    --->    yes(list(triple(par, subst, ptypes)))
    ;       no.

:- pred zcheck(flags::in, list(par)::in, type_result::out,
    dict::in, dict::out, io::di, io::uo) is det.

:- implementation.

:- import_module higher_order.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module ztype_op.

:- type cstate
    --->    c(io, status, dict, ptypes, subst, flags).

zcheck(F, Spec, Result, D0, D, IO0, IO) :-
    zcheck(Spec, TypedSpec,
        c(IO0, ok, D0, initPtypes,  initSubst,  F),
        c(IO1,  S, D ,          _,          _, _F)),
    unsafe_promise_unique(IO1, IO),
    ( S = ok, Result = yes(TypedSpec)
    ; S = error, Result = no
    ).

:- pred zcheck(list(par), list(triple(par, subst, ptypes)), cstate, cstate).
:- mode zcheck(in, out, in, out) is det.

zcheck(Spec, TypedSpec) -->
    ( if tdebugging then
        tout(["Starting type checking ...\n"])
    else
        {true}
    ),
    list.map_foldl(par_check0, Spec, TypedSpec),
    ( if tdebugging then
        tout(["... Finished type checking.\n"])
    else
        {true}
    ).

:- pred operators(operators::out, cstate::in, cstate::out) is det.
operators(operators(F), C, C) :-
    C = c(_, _, _, _, _, F).

:- pred abbreviations(abbreviations::out, cstate::in, cstate::out) is det.
abbreviations(abbreviations(F), C, C) :-
    C = c(_, _, _, _, _, F).

:- pred monotonics(monotonics::out, cstate::in, cstate::out) is det.
monotonics(monotonics(F), C, C) :-
    C = c(_, _, _, _, _, F).

:- pred tdebugging(cstate::in, cstate::out) is semidet.
tdebugging(C, C) :-
    C = c(_, _, _, _, _, F), debugging(F).

:- pred getDict(dict::out, cstate::in, cstate::out) is det.
getDict(D, C, C) :-
    C = c(_, _, D, _, _, _).

:- pred putDict(dict::in, cstate::in, cstate::out) is det.
putDict(D, c(IO, S, _, G, Sub, F), c(IO, S, D, G, Sub, F)).

:- pred getPtypes(ptypes::out, cstate::in, cstate::out) is det.
getPtypes(P, C, C) :-
    C = c(_, _, _, P, _, _).

:- pred putPtypes(ptypes::in, cstate::in, cstate::out) is det.
putPtypes(P, c(IO, S, D, _, Sub, F), c(IO, S, D, P, Sub, F)).

:- pred getSubst(subst::out, cstate::in, cstate::out) is det.
getSubst(S, C, C) :- C = c(_, _, _, _, S, _).

:- pred putSubst(subst::in, cstate::in, cstate::out) is det.
putSubst(Sub, c(IO, S, D, P, _, F), c(IO, S, D, P, Sub, F)).

:- pred substEnv(pred(Type, Result, subst, subst), Type, Result,
    cstate, cstate).
:- mode substEnv(pred(out, out, in, out) is det, out, out, in, out) is det.
substEnv(P, T, R) -->
    getSubst(S0),
    {P(T, R, S0, S)},
    putSubst(S).

:- pred add_sexpr_type(ref::in, slist::in, cstate::in, cstate::out) is det.
add_sexpr_type(Ref, DL, c(IO, S, D, PM0, Sub, F), c(IO, S, D, PM, Sub, F)) :-
    ztype.add_sexpr_type(Ref, DL, PM0, PM).

:- pred tout(list(string), cstate, cstate).
:- mode tout(in, in, out /*cstate*/) is det.
tout(ML, c(IO0, S, D, G, Sub, F), c(IO, S, D, G, Sub, F)) :-
    unsafe_promise_unique(IO0, IO1),
    io.write_strings(ML, IO1, IO).

:- type terror_poly_type
    --->    s(string)
    ;       t(ztype)
    ;       e(expr)
    ;       d(ident)
    ;       i(int).

:- type telist == list(terror_poly_type).

:- pred te_To_String(operators, terror_poly_type, string).
:- mode te_To_String(in, in, out) is det.

te_To_String(_, s(S), S).
te_To_String(O, t(T), ztypePortray(O, T)).
te_To_String(O, e(E), exprPortray(O, E)).
te_To_String(_, d(I), identPortray(I)).
te_To_String(_, i(I), S) :-
    string.int_to_string(I, S).

:- pred terror(int, telist, cstate, cstate).
:- mode terror(in, in, in, out /*cstate*/) is det.
terror(LN, TL, c(IO0, _, D, G, Sub, F), c(IO, error, D, G, Sub, F)) :-
    string.int_to_string(LN, LNS),
    list.map(te_To_String(operators(F)), TL, ML),
    list.append(ML, [".\n"], ML1),
    unsafe_promise_unique(IO0, IO1),
    terror1(LNS, ML1, IO1, IO).

:- pred terror1(string::in, list(string)::in, io::di, io::uo) is det.

terror1(LNS, ML1) -->
    unsafe_promise_unique,
    io.input_stream_name(N),
    io.stderr_stream(StdErr),
    io.write_strings(StdErr, [N, ":", LNS, ": " | ML1]).

:- pred mismatch3(zcontext, list(mismatch3), cstate, cstate).
:- mode mismatch3(in, in, in, out /*cstate*/) is det.

mismatch3(_, []) --> [].
mismatch3(C, [mismatch(I, XT, YT) | ML]) -->
    terror(C,
        [s("Type mismatch in declarations of "),
        d(I), s("--\n\t"),
        t(XT), s(", "), t(YT)]),
    mismatch3(C, ML).

:- pred addgD(zcontext, ident, entry, cstate, cstate).
:- mode addgD(in, in, in, in, out /*cstate*/) is det.

addgD(C, I, N0) -->
    abbreviations(A),
    { if list.member(I, A) then
        N0 = e(F, T0),
        T = powerT(abbreviationT(I, T0, F)),
        N = e(F, T)
    else
        N = N0
    },
    getDict(D0),
    ( if {dict.insert(I, N, D0, D)} then
        putDict(D),
        {N = e(FD, TD)}, debugDict("<--- ", I, FD, TD)
    else
        terror(C, [
            s("Global identifier "), d(I), s(" already declared")])
    ).

:- pred lookupD(zcontext, ident, ztarity, ztype, cstate, cstate).
:- mode lookupD(in, in, out, out, in, out /*cstate*/) is det.
lookupD(C, I, F, T) -->
    getDict(D),
    ( if {dict.search(I, e(F0, T0), D)} then
        {F = F0, T = T0}
    else
        terror(C, [s("Identifier "), d(I), s(" is not declared")]),
        {F = 0, T = unityT}
    ),
    debugDict("---> ", I, F, T).

:- pred debugDict(string, ident, ztarity, ztype, cstate, cstate).
:- mode debugDict(in, in, in, in, in, out) is det.

debugDict(S, I, F, T) -->
    ( if tdebugging then
        operators(O), {DS = declPortray(O, I - T)},
        ( if {F = 0} then
            tout([S, DS, "\n"])
        else
            {string.int_to_string(F, FS)},
            tout([S, "[", FS, "]:", DS, "\n"])
        )
    else
        {true}
    ).

:- pred actuals_check(maybe(list(expr)), maybe(list(ztype)), cstate, cstate).
:- mode actuals_check(in, out, in, out /*cstate*/) is det.

actuals_check(no, no) --> [].
actuals_check(yes(A0), yes(AT)) -->
    list.map_foldl(set_check("Actual generic parameter"), A0, AT).

:- pred actualiseResult(zcontext, ident, actualisationResult, cstate, cstate).
:- mode actualiseResult(in, in, in, in, out /*cstate*/) is det.

actualiseResult(_, _, ok) --> [].
actualiseResult(C, I, arityError(F, AN)) -->
    terror(C,
        [s("Identifier "), d(I), s(" has "),
        i(F), s(" formals but "), i(AN),s(" actuals")]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- type global ---> global(zcontext, ident, ztype).

:- pred par_check0(par, triple(par, subst, ptypes), cstate, cstate).
:- mode par_check0(in, out, in, out /*cstate*/) is det.

par_check0(Par, triple(Par, S, P)) -->
    {Par = Par1 - C},
    par_check(C, Par1, F, LG0),
    getSubst(S0),
    {incompletelyDetermined(EL, S0, S)},
    % psubst(S),
    getPtypes(P),
    % For VarSource type see below or in ztype.m ---
    % should use this to improve error messages!
    {P0 =
        ( pred(unbound(Expr, _VarSource)::in, in, out) is det -->
            {Expr = _ - VC},
            terror(VC, [
                s("Implicit type parameter not determined--"),
                    s("\n\tExpression: "), e(Expr)
                ])
        )},
    list.foldl(P0, EL),
    { if F = [] then
        P1 =
            ( pred(I - T::in, U::out) is det :-
                U = I - e(0, ztapply(S, T))
            ),
        list.map(P1, LG0, LG)
    else
        makeGeneric(S, F, LG0, LG)
    },
    {P2 = (pred(I - E::in, in, out) is det --> addgD(C, I, E))},
    list.foldl(P2, LG),
    putPtypes(initPtypes),
    putSubst(initSubst).

%:- type varsource
%---> parameter(ref, int) ; function_arg ; function_result ; power ; extension.

:- pred psubst(subst::in, cstate::in, cstate::out) is det.
psubst(Sub, c(IO0, E, D, G, S, F), c(IO, E, D, G, S, F)) :-
    unsafe_promise_unique(IO0, IO1),
    Sub = Sub1 - V,
    map.to_assoc_list(Sub1, AL),
    io.write(AL - V, IO1, IO2),
    io.nl(IO2, IO).

:- pred par_check(zcontext, par1, formals, slist, cstate, cstate).
:- mode par_check(in, in, out, out, in, out /*cstate*/) is det.
par_check(_C, given(L), [], LG) -->
    {P = (pred(I::in, I - givenType(I)::out) is det),
    list.map(P, L, LG)}.
%par_check(C, let(S) - C) --> resetGen, sexpr_check(C, S, _T).
par_check(_C, sdef(I, F, SExpr), F, [I - powerT(schemaT(T))]) -->
    getDict(D),
        installFormals(F),
        sexpr_check(SExpr, T),
    putDict(D).
par_check(_C, eqeq(I, F, X), F, [I - T]) -->
    getDict(D),
        installFormals(F),
        expr_check(X, T),
    putDict(D).
par_check(C, data(Ref, I, L), [], LG) -->
    addgD(C, I, e(0, givenType(I))),  % I may be referred to in the branches
    list.map_foldl(branch_check(I), L, LG),
    add_sexpr_type(Ref, LG).
par_check(_C, zpred(P), [], []) -->
    zpred_check(P).
par_check(_C, define(F, SExpr), F, LG) -->
    getDict(D),
        installFormals(F),
        sexpr_check(SExpr, LG),
    putDict(D).

:- pred installFormals(list(ident)::in, cstate::in, cstate::out) is det.
installFormals([]) --> [].  % This clause avoids trace
installFormals(F) -->
    {F = [_ | _]},
    {Function = (pred(I::in, I - powerT(parameterT(I))::out) is det)},
    {list.map(Function, F, DL)},
    install_dlist(DL).

:- pred branch_check(ident, branch, pair(ident, ztype), cstate, cstate).
:- mode branch_check(in, in, out, in, out /*cstate*/) is det.
branch_check(Id, branch(Ref, I, M), I - T) -->
    (
        {M = yes(X),
    % T should be T0 \pfun givenT(Id) to make use of abbreviations
        Args = [T0, givenT(Id)],
        F = ((func) = powerT(cproductT(Args)))},
        set_check("Branch expression", X, T0),
        try_abbrev_type(X, Ref, "\\pfun", F, Args, T)
    ;
        {M = no,
        T = givenT(Id)}
    ).

:- pred sexpr_checkCT(sexpr, ztype, cstate, cstate).
:- mode sexpr_checkCT(in, out, in, out /*cstate*/) is det.
sexpr_checkCT(sexpr(Ref, SExpr, C), T) -->
    ( if {SExpr = text(L0, PL)} then
        declL_checkCT(C, L0, DL, TL),
        install_dlist(DL),
        list.foldl(zpred_check, PL),
        {(if TL = [T0] then T = T0 else T = cproductT(TL))}
    else
        sexpr_check(Ref, C, SExpr, DL),
        install_dlist(DL),
        {T = schemaT(DL)}
    ),
    add_sexpr_type(Ref, DL).

:- pred declL_checkCT(zcontext, list(decl), slist, list(ztype), cstate, cstate).
:- mode declL_checkCT(in, in, out, out, in, out /*cstate*/) is det.
declL_checkCT(_, [], [], []) -->
    [].
declL_checkCT(C, [H | L], DTL, TTL) -->
    decl_checkCT(H, DT, TT),
    declL_checkCT(C, L, DTL0, TTL0),
    substEnv(slist_merge(DT, DTL0), DTL, ML), mismatch3(C, ML),
    {list.append(TT, TTL0, TTL)}.

:- pred decl_checkCT(decl, slist, list(ztype), cstate, cstate).
:- mode decl_checkCT(in, out, out, in, out /*cstate*/) is det.
decl_checkCT(decl(L0, X), L, TL) -->
    set_check("Declaration expression", X, T),
    {list.sort(L0, L1),
    list.remove_adjacent_dups(L1, L2),
    list.map(
        ( pred(I::in, O::out) is det :-
            O = I - T
        ), L2, L)},
    {list.length(L0, N),
    list.duplicate(N, T, TL)}.
decl_checkCT(include(S), T, [schemaT(T)]) -->
    sexpr_check(S, T).

:- pred set_check(string, expr, ztype, cstate, cstate).
:- mode set_check(in, in, out, in, out /*cstate*/) is det.
set_check(Str, X, T) -->
    expr_check(X, T0),
    substEnv(powerType(X, T0), T, M),
    (
        {M = yes}
    ;
        {M = no(ET)},
        {X = _ - C},
        terror(C, [
            s(Str), s(" must be set-valued---"),
            s("\n\tExpression: "), e(X),
            s("\n\tType: "), t(ET)
        ])
    ).

:- pred zpred_check(zpred, cstate, cstate).
:- mode zpred_check(in, in, out /*cstate*/) is det.

zpred_check(equality(X0, X1) - C) -->
    expr_check(X0, T0), expr_check(X1, T1),
    {O = " = "},
    substEnv(ztunify(T0, T1), _, U),
    (
        {U = unified}
    ;
        {U = failed(FS)},
        terror(C, [
            s("Type mismatch in equation--"),
            s("\n\tEquation: "), e(X0), s(O), e(X1),
            s("\n\tTypes: "),
                t(ztapply(FS, T0)), s(O), t(ztapply(FS, T1))
        ])
    ).
zpred_check(membership(X0, X1) - C) -->
    expr_check(X0, T0), set_check("Relation or 2nd arg to \\in", X1, T1),
    {O = " \\in "},
    substEnv(ztunify(T0, T1), _, U),
    (
        {U = unified}
    ;
        {U = failed(FS)},
        terror(C, [
            s("Type mismatch in predicate--"),
            s("\n\tPredicate: "), e(X0), s(O), e(X1),
            s("\n\tTypes: "), t(ztapply(FS, T0)), s(O),
                t(ztapply(FS, powerT(T1)))
        ])
    ).
zpred_check(truth - _) --> [].
zpred_check(falsehood - _) --> [].
zpred_check(negation(P) - _) --> zpred_check(P).
zpred_check(lbpred(_, P0, P1) - _) --> zpred_check(P0), zpred_check(P1).
zpred_check(quantification(_, S, P) - _) -->
    sexpr_check(S, SList),
    getDict(D),
    install_dlist(SList),
    zpred_check(P),
    putDict(D).
zpred_check(sexpr(S) - C) -->
    % ***BUG
    % Need to check that SList is already defined and in the environment
    sexpr_check(S, SList),
    check_slist_defined(C, SList).
zpred_check(let(L, P) - C) -->
    getDict(D),
    assoc_list_ident_expr_check(L, SL0),
    substEnv(slist_sort(SL0), SL, ML), mismatch3(C, ML),
    install_dlist(SL),
    zpred_check(P),
    putDict(D).

:- pred check_slist_defined(zcontext, slist, cstate, cstate).
:- mode check_slist_defined(in, in, in, out /*cstate*/) is det.
check_slist_defined(C, SList) -->
    {P =
        ( pred(I - _::in, I - T::out, in, out) is det -->
            lookupD(C, I, _, T)
        )},
    list.map_foldl(P, SList, EnvList),
    substEnv(slist_merge(SList, EnvList), _Type, ML), mismatch3(C, ML).

%%%
% 5 EXPRESSION

:- pred expr_check(expr, ztype, cstate, cstate).
:- mode expr_check(in, out, in, out /*cstate*/) is det.
% 5.2 Identifier
% 5.3 Generic Instantiation
expr_check(Expr, T) -->
    {Expr = ref(Ref, I, MActuals) - C},
    lookupD(C, I, F, T0),
    actuals_check(MActuals, MActualTypes),
    substEnv(actualise(Expr, Ref, F, T0, MActualTypes), T, Result),
    actualiseResult(C, I, Result).

% 5.4 Number Literal
expr_check(number(_) - _, numType) --> [].
% 5.5 String Literal
expr_check(stringl(_) - _, stringType) --> [].
% 5.6 Set Extension
expr_check(X, T) -->
    {X = display(Ref, D, L) - C},
    list.map_foldl(expr_check, L, TL),
    substEnv(check_sameL(X, TL), T0, M),
    (
        {D = set, T = powerT(T0)}
    ;
        {D = seq},
        {F = ((func) = powerT(cproductT([numType, T0])))},
        try_abbrev_type(X, Ref, "\\seq", F, [T0], T)
    ;
        {D = bag},
        {F = ((func) = powerT(cproductT([T0, numType])))},
        try_abbrev_type(X, Ref, "\\bag", F, [T0], T)
    ),
    (
        {M = no}
    ;
        {M = yes(mismatch(T1, T2))},
        terror(C, [
            s("Type mismatch in elements of "),
                s(display_to_string(D)), s("--"),
            s("\n\tExpression: "), e(X),
            s("\n\tTypes: "), t(T1), s(", "), t(T2)])
    ).

% 5.7 Set Comprehension
expr_check(setcomp(S, M) - _, powerT(T)) -->
    getDict(D),
        sexpr_checkCT(S, T0),
        ( {M = yes(X)}, expr_check(X, T)
        ; {M = no, T = T0}
        ),
    putDict(D).
expr_check(lambda(S, X) - _, powerT(cproductT([T0, T1]))) -->
    getDict(D),
    sexpr_checkCT(S, T0),
    expr_check(X, T1),
    putDict(D).
% 5.8 Power Set (see 5.3)
expr_check(powerset(X) - _, powerT(T)) -->
    expr_check(X, T).
% 5.9 Tuple
expr_check(tuple(L) - _, cproductT(TL)) -->
    list.map_foldl(expr_check, L, TL).
% 5.10 Cartesian Product
expr_check(product(L) - _, powerT(cproductT(TL))) -->
    list.map_foldl(set_check("Cross - product argument"), L, TL).
% 5.11 Tuple Selection
expr_check(X, T) -->
    {X = tupleselection(X0, SelStr) - C},
    expr_check(X0, T0),
    { if string.to_int(SelStr, N0) then
        N = N0
    else
        error("expr_check/2: string.to_int failed")
    },
    substEnv(tupleSelect(N, T0), T, M),
    (
        {M = yes}
    ;
        {M = outsideArity(ET)},
        terror(C, [
            s("Tuple selection outside 1..arity--"),
            s("\n\tExpression: "), e(X),
            s("\n\tTuple type: "), t(ET)
        ])
    ;
        {M = nontuple(ET)},
        terror(C, [
            s("Tuple selection from non-tuple--"),
            s("\n\tExpression: "), e(X),
            s("\n\tNon-tuple type: "), t(ET)
        ])
    ).
% 5.12 Binding Extension
% (not yet implemented)
expr_check(let(L, X) - C, T) -->
    getDict(D),
    assoc_list_ident_expr_check(L, SL0),
    substEnv(slist_sort(SL0), SL, ML),
    mismatch3(C, ML),
    install_dlist(SL),
    expr_check(X, T),
    putDict(D).
% 5.13 Theta Expression
expr_check(X, schemaT(DL)) -->
    {X = theta(Ref, S, D) - C},
    sexpr_check(S, DL0),
    {decorate(D, DL0, DL1)},
    theta_check(C, DL0, DL1, DL),
    add_sexpr_type(Ref, DL).
% 5.14 Schema Expression
expr_check(sexp(S) - _, powerT(schemaT(T))) --> sexpr_check(S, T).
% 5.15 Binding Selection
expr_check(X, T) -->
    {X = select(Ref, X0, I) - C},
    expr_check(X0, T0),
    substEnv(bindingSelect(I, T0), T, M),
    (
        {M = yes(DL)}
    ;
        {M = nonexistent(ET), DL = []},
        terror(C, [
            s("Selection of non-existent component--"),
            s("\n\tExpression: "), e(X),
            s("\n\tSchema type: "), t(ET)
        ])
    ;
        {M = nonbinding(ET), DL = []},
        terror(C, [
            s("Selection from non-schema--"),
            s("\n\tExpression: "), e(X),
            s("\n\tNon-schema type: "), t(ET)
        ])
    ),
    add_sexpr_type(Ref, DL).
% 5.16 Function Application
expr_check(X, ResultT) -->
    {X = zapply(_Ref, Function, Actual) - C},
    expr_check(Function, FunctionT), expr_check(Actual, ActualT),
    % If FunctionT has generic parameters and
    % not (Function is an ident and the ident is tame)
    % then generate conservative types in the application
    ( if
        {Function = ref(_, I, _) - _},
        monotonics(MF),
        {list.member(I, MF)}
    then
        {Flag = is_monotonic}
    else
        {Flag = is_unknown}
    ),
    substEnv(applyTypes(Flag, Function, X, FunctionT, ActualT), ResultT, M),
    (
        {M = yes}
    ;
        {M = nonfunction(FunctionE)},
        terror(C, [
            s("Application of non-function--"),
            s("\n\tExpression: "), e(X),
            s("\n\tNon-function type: "), t(FunctionE)
        ])
    ;
        {M = mismatch(FormalE, ActualE)},
            terror(C, [
            s("Type mismatch in function application--"),
            s("\n\tExpression: "), e(X),
            s("\n\tExpected: "), t(FormalE),
            s("\n\tFound: "), t(ActualE)
        ])
    ).
% 5.17 Definite Description
expr_check(mu(S, M) - _, T) -->
    getDict(D),
    sexpr_checkCT(S, T0),
    ( {M = yes(X)}, expr_check(X, T)
    ; {M = no, T = T0}
    ),
    putDict(D).
% 5.18 Conditional Expression
expr_check(if(P, X0, X1) - C, T) -->
    zpred_check(P), expr_check(X0, T0), expr_check(X1, T1),
    substEnv(ztunify(T0, T1), T, U),
    (
        {U = unified}
    ;
        {U = failed(FS)},
        terror(C, [s("Type mismatch in conditional expression--"),
           s("\n\tExpression: if ... then "), e(X0), s(" else "), e(X1),
           s("\n\tTypes: if ... then "), t(ztapply(FS, T0)),
             s(" else "), t(ztapply(FS, T1))
        ])
    ).
% 5.19 Substitution
% (not yet implemented)

:- pred try_abbrev_type(expr, ref, word, (func) = ztype, list(ztype), ztype,
    cstate, cstate).
:- mode try_abbrev_type(in, in, in, (func) = out is det, in, out, in, out)
    is det.

try_abbrev_type(Expr, Ref, Name, F, ActualTypes, T) -->
    getDict(Dict),
    {list.length(ActualTypes, Length)},
    ( if
        {dict.search(id(no, Name, []), e(Length, Type0), Dict),
            powerType(Type0, Type1, yes)}
    then
        substEnv(actualise(Expr, Ref, Length, Type1, yes(ActualTypes)), T, _)
    else
        {T = apply(F)}
    ).

:- pred theta_check(zcontext, slist, slist, slist, cstate, cstate).
:- mode theta_check(in, in, in, out, in, out /*cstate*/) is det.

theta_check(_, [], [], []) --> [].
theta_check(_, [_|_], [], _) -->
    {error("theta_check/6: length mismatch")}.
theta_check(_, [], [_|_], _) -->
    {error("theta_check/6: length mismatch")}.
theta_check(C, [I0 - SchemaType | T0], [I1 - _SType | T1], [I0 - Type | T]) -->
    % assert(SchemaType = _SType)
    lookupD(C, I1, _F, EnvType),
    substEnv(ztunify(SchemaType, EnvType), Type, U),
    (
        {U = unified}     % BUG: should check that Type is ground
    ;
        {U = failed(FS)},
        terror(C, [s("Type mismatch in theta expression--"),
           s("\n\tIdentifiers (theta::env): "), d(I0), s("::"), d(I1),
           s("\n\tTypes: (theta/env) "), t(ztapply(FS, SchemaType)),
                     s("::"), t(ztapply(FS, EnvType))
        ])
    ),
    theta_check(C, T0, T1, T).

%%%

:- pred sexpr_check(sexpr, slist, cstate, cstate).
:- mode sexpr_check(in, out, in, out /*cstate*/) is det.

sexpr_check(sexpr(Ref, S, C), TL) -->
    sexpr_check(C, Ref, S, TL),
    add_sexpr_type(Ref, TL).

:- pred sexpr_check(zcontext, ref, sexpr1, slist, cstate, cstate).
:- mode sexpr_check(in, in, in, out, in, out /*cstate*/) is det.

sexpr_check(C, Ref, SRef, DL) -->
    {SRef = ref(I, MActual)},
    lookupD(C, I, F, T0),
    actuals_check(MActual, MActualTypes),
    {Expr = sexp(sexpr(Ref, SRef, C)) - C},
    substEnv(actualise(Expr, Ref, F, T0, MActualTypes), T, Result),
    actualiseResult(C, I, Result),
    ( if {powerType(T, TS, yes), schemaType(TS, DL0)} then
        {DL = DL0}
    else
        {DL = []},
        terror(C,
            [s("Identifier "), d(I),
            s(" expected to be a schema name")])
    ).
sexpr_check(C, _, text(DL, PL), SL) -->
    declL_checkCT(C, DL, SL, _TL),
    install_dlist(SL),
    list.foldl(zpred_check, PL).
sexpr_check(_, _, negation(X), T) -->
    sexpr_check(X, T).
sexpr_check(C, _, lbpred(_, X0, X1), T) -->
    sexpr_check(X0, T1), sexpr_check(X1, T2),
    substEnv(slist_merge(T1, T2), T, ML), mismatch3(C, ML).
sexpr_check(C, _, projection(Ref, X0, X1), T) -->
    sexpr_check(X0, T0), sexpr_check(X1, T1),
    getSubst(S0),
    {slist_project(T0, T1, T, ET, ML, S0, S)},
    putSubst(S),
    mismatch3(C, ML),
    add_sexpr_type(Ref, ET).
sexpr_check(_, _, hide(Ref, X, L), T) -->
    sexpr_check(X, T0),
    {list.sort_and_remove_dups(L, L1)},
    {slist_hide(T0, L1, T, T1)},
    add_sexpr_type(Ref, T1).
sexpr_check(C, _, quantification(_, SExpr, X), T) -->
    sexpr_check(SExpr, T0),
    getDict(D),
    install_dlist(T0),
    sexpr_check(X, T1),
    putDict(D),
    substEnv(slist_quantify(T1, T0), T, ML),
    mismatch3(C, ML).
sexpr_check(C, _, renaming(X, R), DL) -->
    sexpr_check(X, DL0),
    substEnv(rename(R, DL0), DL, ML),
    mismatch3(C, ML).
sexpr_check(C, _, bsexpr(Ref, SConn, X0, X1), T) -->
    sexpr_check(X0, T0),
    sexpr_check(X1, T1),
    % split list eg. into single prime+others for composition
    {P = (pred(I::in, O::out) is semidet :- slist_ident_comp(SConn, I, O))},
    {list.filter_map(P, T0, Mid0, TL)},
    substEnv(slist_sort(Mid0), Mid, ML1),
    mismatch3(C, ML1),
    substEnv(slist_merge(T1, Mid), TR, ML2),
    mismatch3(C, ML2),
    substEnv(slist_merge(TL, TR), T, ML3),
    mismatch3(C, ML3),
    add_sexpr_type(Ref, Mid).
sexpr_check(_, _, decoration(X, D), DL) -->
    sexpr_check(X, DL0),
    {decorate(D, DL0, DL)}.
sexpr_check(_, _, pre(Ref, X), T) -->
    sexpr_check(X, T0),
    {P =
        ( pred(id(no, _Id, [D | _]) - _::in) is semidet :-
            (D = prime ; D = exclamation_mark)
        )},
    {list.filter(P, T0, T1, T)},
    add_sexpr_type(Ref, T1).

:- pred assoc_list_ident_expr_check(list(pair(ident, expr))::in, slist::out,
    cstate::in, cstate::out) is det.

assoc_list_ident_expr_check([], []) --> [].
assoc_list_ident_expr_check([I - X | L], [I - T | LT]) -->
    expr_check(X, T),
    assoc_list_ident_expr_check(L, LT).

%   {P = (pred(I - X::in, I - T::out, in, out) is det --> expr_check(X, T))},
%   list.map_foldl(P, L, LT)

:- pred install_dlist(slist, cstate, cstate).
:- mode install_dlist(in, in, out /*cstate*/) is det.

install_dlist(L) -->
    ( if tdebugging then
        operators(O),
        {IS = declPortray(O, id(no, "", []) - schemaT(L))},
        tout(["<--- ", IS, "\n"])
    else
        {true}
    ),
    {list.map(pred(I - T::in, I - e(0, T)::out) is det, L, L1)},
    getDict(D0),
    {dict.overlay(L1, D0, D)},
    putDict(D).
