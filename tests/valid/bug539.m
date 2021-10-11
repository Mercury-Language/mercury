% vim: ts=4 sw=4 et ft=mercury

% Compile with:
%   mmc -s asm_fast.gc --optimise-constructor-last-call -C soupy

% Uncaught Mercury exception:
% Software Error: predicate `ll_backend.var_locn.actually_place_var'/6:
% Unexpected: placing nondummy var 6 which has no state
% Stack dump follows:
%    0       pred throw/1-0 (erroneous) (exception.m:313)
%    1       pred error/1-0 (erroneous) (require.m:172)
%    2       pred unexpected/2-0 (erroneous) (require.m:203)
%    3       pred actually_place_var/6-0 (det) (var_locn.m:1807)
%    4       pred var_locn_place_var/5-0 (det) (var_locn.m:1706)
%    5       pred actually_place_vars/4-0 (det) (var_locn.m:1730)
%    6       pred var_locn_place_vars/4-0 (det) (var_locn.m:1722)
%    7       pred setup_call/7-0 (det) (code_loc_dep.m:3325)

:- module bug539.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%--------------------------------------------------------------------%

:- implementation.

:- type expr
    --->    box
    ;       pair(expr, expr)
    ;       app(fun, expr).

:- type fun
    --->    destruct_list(expr, fun)
    ;       map_pair(fun, fun).

:- type expr_type
    --->    box_type
    ;       pair_type(expr_type, expr_type)
    ;       list_type(expr_type).

%--------------------------------------------------------------------%

main(!IO) :-
    type_check(box, _T).

:- pred type_check(expr::in, expr_type::out) is det.

type_check(E, T) :-
    (
        E = box,
        T = box_type
    ;
        E = pair(E1, _E2),
        type_check(E1, T1),
        % don't need this call to reproduce
        % type_check(E2, T2),
        T2 = box_type,
        T = pair_type(T1, T2)
    ;
        E = app(Fun, _E0),
        % don't need this call to reproduce
        % type_check(E0, T0),
        T0 = box_type,
        ( if fun_type(Fun, T0, T1) then
            T = T1
        else
            T = box_type
        )
    ).

:- pred fun_type(fun::in, expr_type::in, expr_type::out) is semidet.

fun_type(Fun, ArgT, T) :-
    (
        Fun = destruct_list(E, Fun0),
        ArgT = list_type(T0),
        % The output of this call is used in two places:
        %
        % - it is an output of fun_type; and
        % - it is an input in the T = Tx test unification.
        %
        % The problem is caused by lco.m replacing the callee of this call
        % with the lco-optimized version of type_check in the lco-optimized
        % version of fun_type. Both lco-optimized procedures have AddrOfT
        % as their sole output argument. In the lco-optimized fun_type,
        % replacing this call with LCMCpr_type_check_1(E, AddrOfT) effectively
        % subcontracts the task of binding T to LCMCpr_type_check_1.
        % The bug is caused by the fact it does NOT then PICK UP the value
        % of T (which it could do by dereferencing AddrOfT after the call
        % to LCMCpr_type_check_1). This leaves T itself undefined,
        % which is why when the code generator needs the value of T
        % in the T = Tx unification, it does not find it.
        %
        % I (zs) see two possible solutions.
        %
        % - We can change lco.m to code to do add T = dereference(AddrOfT)
        %   after the call to LCMCpr_type_check_1, or in general after any
        %   lco-optimized call that was  NOT originally a self-recursive call.
        %   (For self-recursive calls, we already check for the absence of
        %   other consumers such as T = Tx.) If the value of T is not needed
        %   by later code, as it is here by T = Tx, then a later invocation
        %   of simplification will delete this addition.
        %
        % - When considering whether to lco-optimize a non-self-recursive call,
        %   we could check whether any of its outputs are needed by later code,
        %   and perform the optimization only if none of them are so needed.
        type_check(E, T),
        fun_type(Fun0, pair_type(T0, list_type(T0)), Tx),
        T = Tx
    ;
        Fun = map_pair(Fun1, Fun2),
        ArgT = pair_type(FirstT0, SecondT0),
        fun_type(Fun1, FirstT0, FirstT),
        fun_type(Fun2, SecondT0, SecondT),
        T = pair_type(FirstT, SecondT)
    ).

:- end_module bug539.
