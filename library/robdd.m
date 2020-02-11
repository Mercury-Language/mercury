%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006, 2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: robdd.m.
% Main author: dmo.
% Stability: low.
%
% This module contains a Mercury interface to Peter Schachte's C
% implementation of Reduced Ordered Binary Decision Diagrams (ROBDDs).
% ROBDDs are an efficient representation for Boolean constraints.
%
% Boolean variables are represented using the type var(T) from the `term'
% library module (see the `term' module documentation for more information).
%
% Example usage:
%
%   % Create some variables.
%   term.init_var_supply(VarSupply0),
%   term.create_var(VarSupply0, A, VarSupply1),
%   term.create_var(VarSupply1, B, VarSupply2),
%   term.create_var(VarSupply2, C, VarSupply),
%
%   % Create some ROBDDs.
%   R1 = ( var(A) =:= var(B) * (~var(C)) ),
%   R2 = ( var(A) =< var(B) ),
%
%   % Test if R1 entails R2 (should succeed).
%   R1 `entails` R2,
%
%   % Project R1 onto A and B.
%   R3 = restrict(C, R1),
%
%   % Test R2 and R3 for equivalence (should succeed).
%   R2 = R3.
%
% ROBDDs are implemented so that two ROBDDs, R1 and R2, represent
% the same Boolean constraint if and only iff `R1 = R2'. Checking
% equivalence of ROBDDs is fast since it involves only a single
% pointer comparison.
%
% XXX This module is not yet sufficiently well tested or documented to be
% included in the publically available part of the library, so at the moment
% it is not included in the list of modules mentioned in the library manual.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module robdd.
:- interface.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module sparse_bitset.
:- import_module term.

:- type robdd(T).
:- type robdd == robdd(generic).

:- type vars(T) == sparse_bitset(var(T)). % XXX experiment with different reps.

:- func empty_vars_set = vars(T).

    % Constants.
    %
:- func one = robdd(T).
:- func zero = robdd(T).

    % If-then-else.
    %
:- func ite(robdd(T), robdd(T), robdd(T)) = robdd(T).

% The functions *, +, =<, =:=, =\= and ~ correspond to the names
% used in the SICStus clp(B) library.

    % Conjunction.
    %
:- func robdd(T) * robdd(T) = robdd(T).

    % Disjunction.
    %
:- func robdd(T) + robdd(T) = robdd(T).

    % Implication.
    %
:- func (robdd(T) =< robdd(T)) = robdd(T).

    % Equivalence.
    %
:- func (robdd(T) =:= robdd(T)) = robdd(T).

    % Non-equivalence (XOR).
    %
:- func (robdd(T) =\= robdd(T)) = robdd(T).

    % Negation.
    %
:- func (~ robdd(T)) = robdd(T).

%---------------------------------------------------------------------------%

    % var(X) is the ROBDD that is true iff X is true.
    %
:- func var(var(T)) = robdd(T).

% The following functions operate on individual variables and are
% more efficient than the more generic versions above that take
% ROBDDs as input.

    % not_var(V) = ~ var(V).
    %
:- func not_var(var(T)) = robdd(T).

    % ite_var(V, FA, FB) = ite(var(V), FA, FB).
    %
:- func ite_var(var(T), robdd(T), robdd(T)) = robdd(T).

    % eq_vars(X, Y) = ( var(X) =:= var(Y) ).
    %
:- func eq_vars(var(T), var(T)) = robdd(T).

    % neq_vars(X, Y) = ( var(X) =\= var(Y) ).
    %
:- func neq_vars(var(T), var(T)) = robdd(T).

    % imp_vars(X, Y) = ( var(X) =< var(Y) ).
    %
:- func imp_vars(var(T), var(T)) = robdd(T).

    % conj_vars([V1, V2, ..., Vn]) = var(V1) * var(V2) * ... * var(Vn).
    %
:- func conj_vars(vars(T)) = robdd(T).

    % conj_not_vars([V1, V2, ..., Vn]) = not_var(V1) * ... * not_var(Vn).
    %
:- func conj_not_vars(vars(T)) = robdd(T).

    % disj_vars([V1, V2, ..., Vn]) = var(V1) + var(V2) + ... + var(Vn).
    %
:- func disj_vars(vars(T)) = robdd(T).

    % at_most_one_of(Vs) =
    %   foreach pair Vi, Vj in Vs where Vi \= Vj. ~(var(Vi) * var(Vj)).
    %
:- func at_most_one_of(vars(T)) = robdd(T).

    % var_restrict_true(V, F) = restrict(V, F * var(V)).
    %
:- func var_restrict_true(var(T), robdd(T)) = robdd(T).

    % var_restrict_false(V, F) = restrict(V, F * not_var(V)).
    %
:- func var_restrict_false(var(T), robdd(T)) = robdd(T).

%---------------------------------------------------------------------------%

    % X `entails` Y:
    %
    % Succeed iff X entails Y.
    % Does not create any new ROBDD nodes.
    %
:- pred robdd(T) `entails` robdd(T).
:- mode in `entails` in is semidet.

    % Succeed iff the var is entailed by the ROBDD.
    %
:- pred var_entailed(robdd(T)::in, var(T)::in) is semidet.

:- type entailment_result(T)
    --->    all_vars
    ;       some_vars(vars :: T).

:- type vars_entailed_result(T) == entailment_result(vars(T)).

    % Return the set of vars entailed by the ROBDD.
    %
:- func vars_entailed(robdd(T)) = vars_entailed_result(T).

    % Return the set of vars disentailed by the ROBDD.
    %
:- func vars_disentailed(robdd(T)) = vars_entailed_result(T).

    % definite_vars(R, T, F) <=>
    %   T = vars_entailed(R) /\ F = vars_disentailed(R)
    %
:- pred definite_vars(robdd(T)::in, vars_entailed_result(T)::out,
    vars_entailed_result(T)::out) is det.

%---------------------------------------------------------------------------%

% We use this type to represent equivalence classes. Each equivalence class
% has a leader, which represents the entire class; this will be variable
% in the equivalence class that has the smallest variable number.
%
% The map maps each variable to its leader. This specifically includes the
% leader itself, i.e. there will be an entry mapping the leader to itself.
% Several of the predicates below depend on this invariant.

:- type leader_map(T) == map(var(T), var(T)).

:- type equiv_vars(T)
    --->    equiv_vars(
                leader_map  :: leader_map(T)
            ).

:- type equivalent_result(T) == entailment_result(equiv_vars(T)).

:- func equivalent_vars(robdd(T)) = equivalent_result(T).

%---------------------------------------------------------------------------%

% It is an invariant in all maps in an imp_vars that all keys are
% less than or equal to all the values they map to.
%
% There is no requirement that a key of the any of the maps, including
% imps and rev_imps, appears among the values it maps to, although logically
% both K => K and ~K => ~K hold.

:- type imp_map(T) == map(var(T), vars(T)).

:- type imp_vars(T)
    --->    imp_vars(
                imps            :: imp_map(T), %  K =>  V  (~K \/  V)
                rev_imps        :: imp_map(T), % ~K => ~V  ( K \/ ~V)
                dis_imps        :: imp_map(T), %  K => ~V  (~K \/ ~V)
                rev_dis_imps    :: imp_map(T)  % ~K =>  V  ( K \/  V)
            ).

:- func extract_implications(robdd(T)) = imp_vars(T).

%---------------------------------------------------------------------------%

    % Existentially quantify away the var in the ROBDD.
    %
:- func restrict(var(T), robdd(T)) = robdd(T).

    % Existentially quantify away all vars greater than the specified var.
    %
:- func restrict_threshold(var(T), robdd(T)) = robdd(T).

    % Existentially quantify away all vars for which the predicate fails.
    %
:- func restrict_filter(pred(var(T)), robdd(T)) = robdd(T).
:- mode restrict_filter(pred(in) is semidet, in) = out is det.

    % restrict_filter(P, D, R):
    %
    % Existentially quantify away all vars for which P fails,
    % except, if D fails for a var, do not existentially quantify
    % away that var or any greater than it. This means that D can be
    % used to set a depth limit on the existential quantification.
    %
:- func restrict_filter(pred(var(T)), pred(var(T)), robdd(T)) = robdd(T).
:- mode restrict_filter(pred(in) is semidet, pred(in) is semidet, in) = out
    is det.

:- func restrict_true_false_vars(vars(T), vars(T), robdd(T)) = robdd(T).

    % Given a leader map, remove all but the least variable in each
    % equivalence class from the ROBDD.
    % Note: the leader map MUST correspond to actual equivalences within
    % the ROBDD, (e.g. have been produced by 'equivalent_vars/1').
    %
:- func squeeze_equiv(equiv_vars(T), robdd(T)) = robdd(T).

    % make_equiv(Equivalences) = Robdd:
    %
    % Robdd is the constraint representing all the equivalences
    % in Equivalences.
    %
:- func make_equiv(equiv_vars(T)) = robdd(T).

    % add_equivalences(Equivalences, Robbd0) = Robdd:
    %
    % Robdd is the constraint Robdd0 conjoined with robdds representing
    % all the equivalences in Equivalences.
    %
:- func add_equivalences(equiv_vars(T), robdd(T)) = robdd(T).

    % add_implications(Implications, Robbd0) = Robdd:
    %
    % Robdd is the constraint Robdd0 conjoined with robdds representing
    % all the implications in Implications.
    %
:- func add_implications(imp_vars(T), robdd(T)) = robdd(T).

    % remove_implications_from_robdd(Implications, Robbd0) = Robdd:
    %
    % Robdd is the constraint Robdd0 conjoined with robdds representing
    % all the implications in Implications.
    %
:- func remove_implications(imp_vars(T), robdd(T)) = robdd(T).

%---------------------------------------------------------------------------%

:- type literal(T)
    --->    pos(var(T))
    ;   neg(var(T)).

    % Convert the ROBDD to disjunctive normal form.
    %
:- func dnf(robdd(T)) = list(list(literal(T))).

%   % Convert the ROBDD to conjunctive normal form.
%   %
% :- func cnf(robdd(T)) = list(list(literal(T))).

    % Print out the ROBDD in disjunctive normal form.
    %
:- pred print_robdd(robdd(T)::in, io::di, io::uo) is det.

    % robdd_to_dot(ROBDD, WriteVar, FileName, !IO):
    %
    % Output the ROBDD in a format that can be processed by the
    % graph-drawing program `dot'.
    %
:- pred robdd_to_dot(robdd(T)::in, write_var(T)::in(write_var), string::in,
    io::di, io::uo) is det.

    % robdd_to_dot(ROBDD, WriteVar, !IO):
    %
    % Output the ROBDD in a format that can be processed by the
    % graph-drawing program `dot'.
    %
:- pred robdd_to_dot(robdd(T)::in, write_var(T)::in(write_var),
    io::di, io::uo) is det.

:- type write_var(T) == pred(var(T), io, io).
:- inst write_var == (pred(in, di, uo) is det).

    % Apply the variable substitution to the ROBDD.
    %
:- func rename_vars(func(var(T)) = var(T), robdd(T)) = robdd(T).

    % Succeed iff ROBDD = one or ROBDD = zero.
    %
:- pred is_terminal(robdd(T)::in) is semidet.

    % Output the number of nodes and the depth of the ROBDD.
    %
:- pred size(robdd(T)::in, int::out, int::out) is det.

    % Output the number of nodes, the depth of the ROBDD and the
    % variables it contains.
    %
:- pred size(robdd(T)::in, int::out, int::out, list(var(T))::out) is det.

    % Succeed iff the var is constrained by the ROBDD.
    %
:- pred var_is_constrained(robdd(T)::in, var(T)::in) is semidet.

    % Succeed iff all the vars in the set are constrained by the ROBDD.
    %
:- pred vars_are_constrained(robdd(T)::in, vars(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % labelling(Vars, ROBDD, TrueVars, FalseVars):
    %
    % Takes a set of Vars and an ROBDD and returns a value assignment
    % for those Vars that is a model of the Boolean function
    % represented by the ROBDD.
    % The value assignment is returned in the two sets TrueVars (set
    % of variables assigned the value 1) and FalseVars (set of
    % variables assigned the value 0).
    %
    % XXX should try using sparse_bitset here.
    %
:- pred labelling(vars(T)::in, robdd(T)::in, vars(T)::out, vars(T)::out)
    is nondet.

    % minimal_model(Vars, ROBDD, TrueVars, FalseVars):
    %
    % Takes a set of Vars and an ROBDD and returns a value assignment
    % for those Vars that is a minimal model of the Boolean function
    % represented by the ROBDD.
    % The value assignment is returned in the two sets TrueVars (set
    % of variables assigned the value 1) and FalseVars (set of
    % variables assigned the value 0).
    %
    % XXX should try using sparse_bitset here.
    %
:- pred minimal_model(vars(T)::in, robdd(T)::in, vars(T)::out, vars(T)::out)
    is nondet.

%---------------------------------------------------------------------------%

    % Zero the internal caches used for ROBDD operations.
    % This allows nodes in the caches to be garbage-collected.
    % This operation is pure and does not perform any I/O, but we need
    % to either declare it impure or pass io.states to ensure that
    % the compiler won't try to optimise away the call.
    %
:- pred clear_caches(io::di, io::uo) is det.

:- impure pred clear_caches is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set_bbbtree.
:- import_module set_unordlist.
:- import_module string.

:- type robdd(T)
    --->    robdd(int).

% :- type robdd(T) ---> robdd(c_pointer).
% Can't use a c_pointer since we want to memo ROBDD operations and
% pragma memo does not support c_pointers.

empty_vars_set = sparse_bitset.init.

:- pragma foreign_decl("C", local, "
#define MR_ROBDD_CLEAR_CACHES
#define MR_ROBDD_COMPUTED_TABLE
#define MR_ROBDD_EQUAL_TEST
#define MR_ROBDD_USE_ITE_CONSTANT
#define MR_ROBDD_NEW
#define MR_ROBDD_RESTRICT_SET

#include ""bryant.h""

#ifdef  MR_HIGHLEVEL_DATA
  typedef   MR_Box  MR_ROBDD_NODE_TYPE;
#else
  typedef   MR_Word MR_ROBDD_NODE_TYPE;
#endif
").

:- pragma foreign_code("C", "
#define NDEBUG
#include ""bryant.c""
").

:- pragma no_inline(one/0).
:- pragma foreign_proc("C",
    one = (F::out),
    [will_not_call_mercury, promise_pure],
"
    F = (MR_ROBDD_NODE_TYPE) MR_ROBDD_trueVar();
").

:- pragma no_inline(zero/0).
:- pragma foreign_proc("C",
    zero = (F::out),
    [will_not_call_mercury, promise_pure],
"
    F = (MR_ROBDD_NODE_TYPE) MR_ROBDD_falseVar();
").

:- pragma no_inline(var/1).
:- pragma foreign_proc("C",
    var(V::in) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    F = (MR_ROBDD_NODE_TYPE) MR_ROBDD_variableRep(V);
").

:- pragma no_inline(ite/3).
:- pragma foreign_proc("C",
    ite(F::in, G::in, H::in) = (ITE::out),
    [will_not_call_mercury, promise_pure],
"
    ITE = (MR_ROBDD_NODE_TYPE) MR_ROBDD_ite((MR_ROBDD_node *) F,
        (MR_ROBDD_node *) G, (MR_ROBDD_node *) H);
").

:- pragma no_inline(ite_var/3).
:- pragma foreign_proc("C",
    ite_var(V::in, G::in, H::in) = (ITE::out),
    [will_not_call_mercury, promise_pure],
"
    ITE = (MR_ROBDD_NODE_TYPE) MR_ROBDD_ite_var(V, (MR_ROBDD_node *) G,
        (MR_ROBDD_node *) H);
").

:- pragma promise_pure('*'/2).
X * Y = R :-
    R = glb(X, Y),

    % XXX debugging code.
    % ( if R = zero then
    ( if (X = zero ; Y = zero) then
        impure report_zero_constraint
    else
        true
    ).

:- func glb(robdd(T), robdd(T)) = robdd(T).
:- pragma foreign_proc("C",
    glb(X::in, Y::in) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    F = (MR_ROBDD_NODE_TYPE) MR_ROBDD_glb((MR_ROBDD_node *) X,
        (MR_ROBDD_node *) Y);
").

% XXX
:- impure pred report_zero_constraint is det.
:- pragma foreign_proc("C",
    report_zero_constraint,
    [will_not_call_mercury],
"
    fprintf(stderr, ""Zero constraint!!!\\n"");
").

:- pragma no_inline((+)/2).
:- pragma foreign_proc("C",
    (X::in) + (Y::in) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    F = (MR_ROBDD_NODE_TYPE) MR_ROBDD_lub((MR_ROBDD_node *) X,
        (MR_ROBDD_node *) Y);
").

:- pragma no_inline((=<)/2).
:- pragma foreign_proc("C",
    ((X::in) =< (Y::in)) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    F = (MR_ROBDD_NODE_TYPE) MR_ROBDD_implies((MR_ROBDD_node *) X,
        (MR_ROBDD_node *) Y);
").

(F =:= G) = ite(F, G, ~G).

(F =\= G) = ite(F, ~G, G).

(~F) = ite(F, zero, one).

:- pragma no_inline(entails/2).
:- pragma foreign_proc("C",
    entails(X::in, Y::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = (MR_ROBDD_ite_constant((MR_ROBDD_node *) X,
        (MR_ROBDD_node *) Y, MR_ROBDD_one) == MR_ROBDD_one);
").

:- pragma no_inline(var_entailed/2).
:- pragma foreign_proc("C",
    var_entailed(F::in, V::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = MR_ROBDD_var_entailed((MR_ROBDD_node *) F,
        (int) V);
").

% :- pragma memo(vars_entailed/1).

vars_entailed(R) =
    ( if R = one then
        some_vars(empty_vars_set)
    else if R = zero then
        all_vars
    else
        ( if R ^ fa = zero then
            (vars_entailed(R ^ tr) `intersection` vars_entailed(R ^ fa))
                `insert` R ^ value
        else
            vars_entailed(R ^ tr) `intersection` vars_entailed(R ^ fa)
        )
    ).

% :- pragma memo(vars_disentailed/1).

vars_disentailed(R) =
    ( if R = one then
        some_vars(empty_vars_set)
    else if R = zero then
        all_vars
    else
        ( if R ^ tr = zero then
            (vars_disentailed(R ^ tr) `intersection`
                vars_disentailed(R ^ fa)) `insert` R ^ value
        else
            vars_disentailed(R ^ tr) `intersection`
                vars_disentailed(R ^ fa)
        )
    ).

% :- pragma memo(definite_vars/3).

definite_vars(R, T, F) :-
    ( if R = one then
        T = some_vars(empty_vars_set),
        F = some_vars(empty_vars_set)
    else if R = zero then
        T = all_vars,
        F = all_vars
    else
        definite_vars(R ^ tr, T_tr, F_tr),
        definite_vars(R ^ fa, T_fa, F_fa),
        T0 = T_tr `intersection` T_fa,
        F0 = F_tr `intersection` F_fa,
        ( if R ^ fa = zero then
            T = T0 `insert` R ^ value,
            F = F0
        else if R ^ tr = zero then
            T = T0,
            F = F0 `insert` R ^ value
        else
            T = T0,
            F = F0
        )
    ).

equivalent_vars(R) = rev_map(equivalent_vars_2(R)).

:- type leader_to_eqvclass(T) ---> leader_to_eqvclass(map(var(T), vars(T))).

:- func equivalent_vars_2(robdd(T)) =
    entailment_result(leader_to_eqvclass(T)).

% :- pragma memo(equivalent_vars_2/1).

equivalent_vars_2(R) = EQ :-
    ( if R = one then
        EQ = some_vars(leader_to_eqvclass(map.init))
    else if R = zero then
        EQ = all_vars
    else
        EQVars = vars_entailed(R ^ tr) `intersection`
                vars_disentailed(R ^ fa),
        EQ0 = equivalent_vars_2(R ^ tr) `intersection`
                equivalent_vars_2(R ^ fa),
        (
            EQVars = all_vars,
            error("equivalent_vars: unexpected result")
            % If this condition occurs it means the ROBDD
            % invariants have been violated somewhere since
            % both branches of R must have been zero.
        ;
            EQVars = some_vars(Vars),
            ( if is_empty(Vars) then
                EQ = EQ0
            else
                (
                    EQ0 = all_vars,
                    error("equivalent_vars: unexpected result")
                    % If this condition occurs it means
                    % the ROBDD invariants have been
                    % violated somewhere since both
                    % branches of R must have been zero.
                ;
                    EQ0 = some_vars(
                        leader_to_eqvclass(M0)),
                    map.det_insert(R ^ value, Vars,
                        M0, M),
                    EQ = some_vars(leader_to_eqvclass(M))
                )
            )
        )
    ).

:- func rev_map(entailment_result(leader_to_eqvclass(T))) =
    equivalent_result(T).

rev_map(all_vars) = all_vars.
rev_map(some_vars(leader_to_eqvclass(EQ0))) = some_vars(equiv_vars(EQ)) :-
    map.foldl2(
        ( pred(V::in, Vs::in, Seen0::in, Seen::out, in, out) is det -->
            ( if { Seen0 `contains` V } then
                { Seen = Seen0 }
            else
                ^ elem(V) := V,
                sparse_bitset.foldl((pred(Ve::in, in, out) is det -->
                    ^ elem(Ve) := V
                    ), Vs),
                { Seen = Seen0 `sparse_bitset.union` Vs }
            )
        ), EQ0, sparse_bitset.init, _, map.init, EQ).

extract_implications(R) = implication_result_to_imp_vars(implications_2(R)).

:- type implication_result(T)
    --->    implication_result(
                imp_res(T), %  K ->  V
                imp_res(T), % ~K -> ~V
                imp_res(T), %  K -> ~V
                imp_res(T)  % ~K ->  V
            ).

:- type imp_res(T) == entailment_result(imp_res_2(T)).
:- type imp_res_2(T)
    --->    imps(map(var(T), vars_entailed_result(T))).

:- func implications_2(robdd(T)) = implication_result(T).
% :- pragma memo(implications_2/1).

implications_2(R) = implication_result(Imps, RevImps, DisImps, RevDisImps) :-
    ( if R = one then
        Imps = some_vars(imps(map.init)),
        RevImps = Imps,
        DisImps = Imps,
        RevDisImps = Imps
    else if R = zero then
        Imps = all_vars,
        RevImps = Imps,
        DisImps = Imps,
        RevDisImps = Imps
    else
        TTVars = vars_entailed(R ^ tr),
        FFVars = vars_disentailed(R ^ fa),
        TFVars = vars_disentailed(R ^ tr),
        FTVars = vars_entailed(R ^ fa),

        implications_2(R ^ tr) =
            implication_result(Imps0, RevImps0, DisImps0, RevDisImps0),
        implications_2(R ^ fa) =
            implication_result(Imps1, RevImps1, DisImps1, RevDisImps1),

        Imps2 = merge_imp_res(TTVars, FTVars, Imps0, Imps1),
        RevImps2 = merge_imp_res(TFVars, FFVars, RevImps0, RevImps1),
        DisImps2 = merge_imp_res(TFVars, FFVars, DisImps0, DisImps1),
        RevDisImps2 = merge_imp_res(TTVars, FTVars, RevDisImps0, RevDisImps1),

        % Imps2 = Imps0 `intersection` Imps1,
        % RevImps2 = RevImps0 `intersection` RevImps1,
        % DisImps2 = DisImps0 `intersection` DisImps1,
        % RevDisImps2 = RevDisImps0 `intersection` RevDisImps1,

        Imps = Imps2 ^ elem(R ^ value) := TTVars,
        RevImps = RevImps2 ^ elem(R ^ value) := FFVars,
        DisImps = DisImps2 ^ elem(R ^ value) := TFVars,
        RevDisImps = RevDisImps2 ^ elem(R ^ value) := FTVars
    ).

:- func merge_imp_res(vars_entailed_result(T), vars_entailed_result(T),
    imp_res(T), imp_res(T)) = imp_res(T).

merge_imp_res(_, _, all_vars, all_vars) = all_vars.
merge_imp_res(_, _, some_vars(Imps), all_vars) = some_vars(Imps).
merge_imp_res(_, _, all_vars, some_vars(Imps)) = some_vars(Imps).
merge_imp_res(TVars, FVars, some_vars(ImpsA), some_vars(ImpsB)) =
    some_vars(merge_imp_res_2(TVars, FVars, ImpsA, ImpsB)).

:- func merge_imp_res_2(vars_entailed_result(T), vars_entailed_result(T),
    imp_res_2(T), imp_res_2(T)) = imp_res_2(T).

merge_imp_res_2(EntailedVarsA, EntailedVarsB, imps(ImpsA), imps(ImpsB)) =
        imps(Imps) :-
    KeysA = map.sorted_keys(ImpsA),
    KeysB = map.sorted_keys(ImpsB),
    Keys = list.merge_and_remove_dups(KeysA, KeysB),
    Imps = list.foldl((func(V, M) = M ^ elem(V) := VsA `intersection` VsB :-
            VsA = ( if VsA0 = ImpsA ^ elem(V) then VsA0 else EntailedVarsA ),
            VsB = ( if VsB0 = ImpsB ^ elem(V) then VsB0 else EntailedVarsB )
        ), Keys, map.init).

:- func implication_result_to_imp_vars(implication_result(T)) = imp_vars(T).

implication_result_to_imp_vars(ImpRes) = ImpVars :-
    ImpRes = implication_result(I0, RI0, DI0, RDI0),
    I = imp_res_to_imp_map(I0),
    RI = imp_res_to_imp_map(RI0),
    DI = imp_res_to_imp_map(DI0),
    RDI = imp_res_to_imp_map(RDI0),
    ImpVars = imp_vars(I, RI, DI, RDI).

:- func imp_res_to_imp_map(imp_res(T)) = imp_map(T).

imp_res_to_imp_map(all_vars) = map.init.
imp_res_to_imp_map(some_vars(imps(IRMap))) =
    map.foldl(func(V, MaybeVs, M) =
        ( if
            MaybeVs = some_vars(Vs),
            is_non_empty(Vs)
        then
            M ^ elem(V) := Vs
        else
            M
        ), IRMap, init).

remove_implications(ImpRes, R0) = R :-
    remove_implications_2(ImpRes, sparse_bitset.init, sparse_bitset.init,
        R0, R, map.init, _).

:- pred remove_implications_2(imp_vars(T)::in, vars(T)::in,
    vars(T)::in, robdd(T)::in, robdd(T)::out,
    robdd_cache(T)::in, robdd_cache(T)::out) is det.

remove_implications_2(ImpRes, True, False, R0, R) -->
    ( if { is_terminal(R0) } then
        { R = R0 }
    else if { True `contains` R0 ^ value } then
        remove_implications_2(ImpRes, True, False, R0 ^ tr, R)
    else if { False `contains` R0 ^ value } then
        remove_implications_2(ImpRes, True, False, R0 ^ fa, R)
    else if R1 =^ elem(R0) then
        { R = R1 }
    else
        { TrueT = True `union` ImpRes ^ imps ^ get(R0 ^ value) },
        { FalseT = False `union` ImpRes ^ dis_imps ^ get(R0 ^ value) },
        remove_implications_2(ImpRes, TrueT, FalseT, R0 ^ tr, RT),

        { TrueF = True `union` ImpRes ^ rev_dis_imps ^ get(R0 ^ value)},
        { FalseF = False `union` ImpRes ^ rev_imps ^ get(R0 ^ value) },
        remove_implications_2(ImpRes, TrueF, FalseF, R0 ^ fa, RF),

        { R = make_node(R0 ^ value, RT, RF) },
        ^ elem(R0) := R
    ).

:- func get(var(T), imp_map(T)) = vars(T).

get(K, IM) =
    ( if Vs = IM ^ elem(K) then
        % In case Vs doesn't already contain K
        Vs `insert` K
    else
        init
    ).

:- typeclass intersectable(T) where [
    func T `intersection` T = T
].

:- instance intersectable(sparse_bitset(T)) where [
    func(intersection/2) is sparse_bitset.intersect
].

:- instance intersectable(entailment_result(T)) <= intersectable(T) where [
    ( all_vars `intersection` R = R ),
    ( some_vars(Vs) `intersection` all_vars = some_vars(Vs) ),
    ( some_vars(Vs0) `intersection` some_vars(Vs1) =
        some_vars(Vs0 `intersection` Vs1) )
].

:- instance intersectable(leader_to_eqvclass(T)) where [
    ( leader_to_eqvclass(MapA) `intersection` leader_to_eqvclass(MapB) =
        leader_to_eqvclass(map.foldl((func(V, VsA, M) =
            ( if Vs = VsA `intersect` (MapB ^ elem(V)) then
                ( if is_empty(Vs) then
                    M
                else
                    M ^ elem(V) := Vs
                )
            else
                M
            )), MapA, map.init))
    )
].

:- instance intersectable(imp_res_2(T)) where [
    imps(MapA) `intersection` imps(MapB) =
        imps(map.intersect(intersection, MapA, MapB))
].

:- func 'elem :='(var(T), imp_res(T), vars_entailed_result(T)) = imp_res(T).

'elem :='(_, all_vars, _) = all_vars.
'elem :='(V, some_vars(imps(M0)), Vs) = some_vars(imps(M0 ^ elem(V) := Vs)).

:- func vars_entailed_result(T) `insert` var(T) = vars_entailed_result(T).

all_vars `insert` _ = all_vars.
some_vars(Vs) `insert` V = some_vars(Vs `insert` V).

% Access to the struct members.
% WARNING! These functions are unsafe. You must not call these functions
% on the terminal robdds (i.e. `zero' and `one').
:- func value(robdd(T)) = var(T).
:- func tr(robdd(T)) = robdd(T).
:- func fa(robdd(T)) = robdd(T).

:- pragma no_inline(value/1).
:- pragma foreign_proc("C",
    value(F::in) = (Value::out),
    [will_not_call_mercury, promise_pure],
"
    Value = (MR_ROBDD_NODE_TYPE) ((MR_ROBDD_node *) F)->value;
").

:- pragma no_inline(tr/1).
:- pragma foreign_proc("C",
    tr(F::in) = (Tr::out),
    [will_not_call_mercury, promise_pure],
"
    Tr = (MR_ROBDD_NODE_TYPE) ((MR_ROBDD_node *) F)->tr;
").

:- pragma no_inline(fa/1).
:- pragma foreign_proc("C",
    fa(F::in) = (Fa::out),
    [will_not_call_mercury, promise_pure],
"
    Fa = (MR_ROBDD_NODE_TYPE) ((MR_ROBDD_node *) F)->fa;
").

%---------------------------------------------------------------------------%

% :- pragma memo(dnf/1).

dnf(R) =
    ( if R = zero then
        []
    else if R = one then
        [[]]
    else
        list.map(func(L) = [pos(R ^ value) | L], dnf(R ^ tr)) ++
        list.map(func(L) = [neg(R ^ value) | L], dnf(R ^ fa))
    ).

% cnf(R) =
%   ( if R = zero then
%       [[]]
%   else if R = one then
%       []
%   else
%       [pos(R ^ value) | cnf(R ^ tr)] `merge_cnf`
%       [neg(R ^ value) | cnf(R ^ fa)]
%   ).
%
% :- func merge_cnf(list(list(literal(T))), list(list(literal(T)))) =
%       list(list(literal(T))).
%
% merge_cnf(As, Bs) =
%   ( As = [] ->
%       Bs
%   ; Bs = [] ->
%       As
%   ; if As = [[]] ->
%       As
%   ; Bs = [[]] % XXX check
%   ;
%       foldl(func(A, Cs0) =
%           foldl(func(B, Cs1) = [A ++ B | Cs1], Bs, Cs0),
%           As, [])
%   ).

% :- pragma foreign_proc("C",
%   print_robdd(F::in, IO0::di, IO::uo),
%   [will_not_call_mercury],
% "
%   printOut((MR_ROBDD_node *) F);
%   update_io(IO0, IO);
% ").

print_robdd(F) -->
    ( if { F = one } then
        io.write_string("TRUE\n")
    else if { F = zero } then
        io.write_string("FALSE\n")
    else
        { init(Trues) },
        { init(Falses) },
        print_robdd_2(F, Trues, Falses)
    ).

:- pred print_robdd_2(robdd(T)::in, set_unordlist(var(T))::in,
    set_unordlist(var(T))::in, io::di, io::uo) is det.

print_robdd_2(F, Trues, Falses) -->
    ( if { F = one } then
        { All = to_sorted_list(Trues `union` Falses) },
        io.write_string("("),
        list.foldl((pred(Var::in, di, uo) is det -->
            { if Var `set_unordlist.member` Trues then
                C = ' '
            else
                C = ('~')
            },
            { term.var_to_int(Var, N) },
            io.format(" %c%02d", [c(C), i(N)])
        ), All),
        io.write_string(")\n")
    else if { F \= zero } then
        print_robdd_2(F ^ tr, Trues `insert` F ^ value, Falses),
        print_robdd_2(F ^ fa, Trues, Falses `insert` F ^ value)
    else
        % Don't do anything for zero terminal
        []
    ).

:- pragma no_inline(restrict/2).
:- pragma foreign_proc("C",
    restrict(V::in, F::in) = (R::out),
    [will_not_call_mercury, promise_pure],
"
    R = (MR_ROBDD_NODE_TYPE) MR_ROBDD_restrict(V, (MR_ROBDD_node *) F);
").

:- pragma no_inline(restrict_threshold/2).
:- pragma foreign_proc("C",
    restrict_threshold(V::in, F::in) = (R::out),
    [will_not_call_mercury, promise_pure],
"
    R = (MR_ROBDD_NODE_TYPE) MR_ROBDD_restrictThresh(V,
        (MR_ROBDD_node *) F);
").

% :- pragma memo(rename_vars/2).

rename_vars(Subst, F) =
    ( if is_terminal(F) then
        F
    else
        ite(var(Subst(F ^ value)),
            rename_vars(Subst, F ^ tr),
            rename_vars(Subst, F ^ fa))
    ).

% make_node(Var, Then, Else).
% The make_node() function. WARNING!! If you use this function you are
% responsible for making sure that the ROBDD invariant holds that all the
% variables in both the Then and Else sub graphs are > Var.

:- func make_node(var(T), robdd(T), robdd(T)) = robdd(T).
:- pragma no_inline(make_node/3).
:- pragma foreign_proc("C",
    make_node(Var::in, Then::in, Else::in) = (Node::out),
    [will_not_call_mercury, promise_pure],
"
    Node = (MR_ROBDD_NODE_TYPE) MR_ROBDD_make_node((int) Var,
        (MR_ROBDD_node *) Then, (MR_ROBDD_node *) Else);
").

not_var(V) = make_node(V, zero, one).

eq_vars(VarA, VarB) = F :-
    compare(R, VarA, VarB),
    (
        R = (=),
        F = one
    ;
        R = (<),
        F = make_node(VarA, var(VarB), not_var(VarB))
    ;
        R = (>),
        F = make_node(VarB, var(VarA), not_var(VarA))
    ).

neq_vars(VarA, VarB) = F :-
    compare(R, VarA, VarB),
    (
        R = (=),
        F = zero
    ;
        R = (<),
        F = make_node(VarA, not_var(VarB), var(VarB))
    ;
        R = (>),
        F = make_node(VarB, not_var(VarA), var(VarA))
    ).

imp_vars(VarA, VarB) = F :-
    compare(R, VarA, VarB),
    (
        R = (=),
        F = one
    ;
        R = (<),
        F = make_node(VarA, var(VarB), one)
    ;
        R = (>),
        F = make_node(VarB, one, not_var(VarA))
    ).

conj_vars(Vars) = foldr(func(V, R) = make_node(V, R, zero), Vars, one).

conj_not_vars(Vars) = foldr(func(V, R) = make_node(V, zero, R), Vars, one).

disj_vars(Vars) = foldr(func(V, R) = make_node(V, one, R), Vars, zero).

at_most_one_of(Vars) = at_most_one_of_2(Vars, one, one).

:- func at_most_one_of_2(vars(T), robdd(T), robdd(T)) = robdd(T).

at_most_one_of_2(Vars, OneOf0, NoneOf0) = R :-
    list.foldl2(
        (pred(V::in, One0::in, One::out, None0::in, None::out) is det :-
            None = make_node(V, zero, None0),
            One = make_node(V, None0, One0)
        ), list.reverse(to_sorted_list(Vars)),
        OneOf0, R, NoneOf0, _).

% :- pragma memo(var_restrict_true/2).

var_restrict_true(V, F0) = F :-
    ( if is_terminal(F0) then
        F = F0
    else
        compare(R, F0 ^ value, V),
        (
            R = (<),
            F = make_node(F0 ^ value,
                var_restrict_true(V, F0 ^ tr),
                var_restrict_true(V, F0 ^ fa))
        ;
            R = (=),
            F = F0 ^ tr
        ;
            R = (>),
            F = F0
        )
    ).

% :- pragma memo(var_restrict_false/2).

var_restrict_false(V, F0) = F :-
    ( if is_terminal(F0) then
        F = F0
    else
        compare(R, F0 ^ value, V),
        (
            R = (<),
            F = make_node(F0 ^ value,
                var_restrict_false(V, F0 ^ tr),
                var_restrict_false(V, F0 ^ fa))
        ;
            R = (=),
            F = F0 ^ fa
        ;
            R = (>),
            F = F0
        )
    ).

restrict_true_false_vars(TrueVars, FalseVars, R0) = R :-
% The following code may be useful during debugging, but it is commented out
% since it should not be needed otherwise.
%   size(R0, _Nodes, _Depth), % XXX
%   P = (pred(V::in, di, uo) is det --> io.write_int(var_to_int(V))), % XXX
%   unsafe_perform_io(robdd_to_dot(R0, P, "rtf.dot")), % XXX
    restrict_true_false_vars_2(TrueVars, FalseVars, R0, R,
        init, _).

:- pred restrict_true_false_vars_2(vars(T)::in, vars(T)::in,
    robdd(T)::in, robdd(T)::out,
    robdd_cache(T)::in, robdd_cache(T)::out) is det.

restrict_true_false_vars_2(TrueVars0, FalseVars0, R0, R, Seen0, Seen) :-
    ( if is_terminal(R0) then
        R = R0,
        Seen = Seen0
    else if is_empty(TrueVars0), is_empty(FalseVars0) then
        R = R0,
        Seen = Seen0
    else if search(Seen0, R0, R1) then
        R = R1,
        Seen = Seen0
    else
        Var = R0 ^ value,
        TrueVars = TrueVars0 `remove_leq` Var,
        FalseVars = FalseVars0 `remove_leq` Var,
        ( if TrueVars0 `contains` Var then
            restrict_true_false_vars_2(TrueVars, FalseVars,
                R0 ^ tr, R, Seen0, Seen2)
        else if FalseVars0 `contains` Var then
            restrict_true_false_vars_2(TrueVars, FalseVars,
                R0 ^ fa, R, Seen0, Seen2)
        else
            restrict_true_false_vars_2(TrueVars, FalseVars,
                R0 ^ tr, R_tr, Seen0, Seen1),
            restrict_true_false_vars_2(TrueVars, FalseVars,
                R0 ^ fa, R_fa, Seen1, Seen2),
            R = make_node(R0 ^ value, R_tr, R_fa)
        ),
        Seen = det_insert(Seen2, R0, R)
    ).

:- pred robdd_hash(robdd(T)::in, int::out) is det.

robdd_hash(R, H) :-
    int.hash(node_num(R), H).

restrict_filter(P, F0) =
    restrict_filter(P, (pred(_::in) is semidet :- true), F0).

restrict_filter(P, D, F0) = F :-
    filter_2(P, D, F0, F, map.init, _, map.init, _).

:- type robdd_cache(T) == map(robdd(T), robdd(T)).
:- type var_cache(T) == map(var(T), bool).

:- pred filter_2(pred(var(T)), pred(var(T)), robdd(T), robdd(T),
    var_cache(T), var_cache(T), robdd_cache(T), robdd_cache(T)).
:- mode filter_2(pred(in) is semidet, pred(in) is semidet, in, out, in, out,
    in, out) is det.

filter_2(P, D, F0, F, SeenVars0, SeenVars, SeenNodes0, SeenNodes) :-
    ( if is_terminal(F0) then
        F = F0,
        SeenVars = SeenVars0,
        SeenNodes = SeenNodes0
    else if not D(F0 ^ value) then
        F = F0,
        SeenVars = SeenVars0,
        SeenNodes = SeenNodes0
    else if map.search(SeenNodes0, F0, F1) then
        F = F1,
        SeenVars = SeenVars0,
        SeenNodes = SeenNodes0
    else
        filter_2(P, D, F0 ^ tr, Ftrue, SeenVars0, SeenVars1, SeenNodes0,
            SeenNodes1),
        filter_2(P, D, F0 ^ fa, Ffalse, SeenVars1, SeenVars2, SeenNodes1,
            SeenNodes2),
        V = F0 ^ value,
        ( if map.search(SeenVars0, V, SeenF) then
            SeenVars = SeenVars2,
            (
                SeenF = yes,
                F = make_node(V, Ftrue, Ffalse)
            ;
                SeenF = no,
                F = Ftrue + Ffalse
            )
        else if P(V) then
            F = make_node(V, Ftrue, Ffalse),
            map.det_insert(V, yes, SeenVars2, SeenVars)
        else
            F = Ftrue + Ffalse,
            map.det_insert(V, no, SeenVars2, SeenVars)
        ),
        map.det_insert(F0, F, SeenNodes2, SeenNodes)
    ).

squeeze_equiv(equiv_vars(LeaderMap), R0) =
    ( if Max = map.max_key(LeaderMap) then
        restrict_filter(
            ( pred(V::in) is semidet :-
                map.search(LeaderMap, V, L) => L = V
            ),
            ( pred(V::in) is semidet :-
                not compare(>, V, Max)
            ), R0)
    else
        R0
    ).

make_equiv(equiv_vars(LeaderMap)) =
    make_equiv_2(map.to_sorted_assoc_list(LeaderMap), init).

:- func make_equiv_2(assoc_list(var(T)), vars(T)) = robdd(T).

make_equiv_2([], _) = one.
make_equiv_2([Var - LeaderVar | Vs], Trues) = Robdd :-
    ( if Var = LeaderVar then
        Robdd = make_node(Var, make_equiv_2(Vs, Trues `insert` Var),
            make_equiv_2(Vs, Trues))
    else if Trues `contains` LeaderVar then
        Robdd = make_node(Var, make_equiv_2(Vs, Trues), zero)
    else
        var_to_int(Var, VarNum),
        var_to_int(LeaderVar, LeaderVarNum),
        require(LeaderVarNum < VarNum, "make_equiv_2: unordered vars"),
        % Since LeaderVar < Var, and every leader must appear in an equivalence
        % with itself, an ancestor invocation of this predicate must already
        % have seen LeaderVar. If it is not in Trues, it must therefore
        % be false.
        Robdd = make_node(Var, zero, make_equiv_2(Vs, Trues))
    ).

add_equivalences(equiv_vars(LeaderMap), R0) = R :-
    add_equivalences_2(map.to_sorted_assoc_list(LeaderMap), init, R0, R,
        map.init, _).

:- pred add_equivalences_2(assoc_list(var(T))::in, vars(T)::in,
    robdd(T)::in, robdd(T)::out,
    robdd_cache(T)::in, robdd_cache(T)::out) is det.

add_equivalences_2([], _, R, R, !Cache).
add_equivalences_2([Var - LeaderVar | Vs], Trues, R0, R, !Cache) :-
    ( if R0 = zero then
        R = zero
    else if R1 = !.Cache ^ elem(R0) then
        R = R1
    else if R0 = one then
        R = make_equiv_2([Var - LeaderVar | Vs], Trues),
        !:Cache = !.Cache ^ elem(R0) := R
    else if compare((<), R0 ^ value, Var) then
        add_equivalences_2([Var - LeaderVar | Vs], Trues,
            R0 ^ tr, Rtr, !Cache),
        add_equivalences_2([Var - LeaderVar | Vs], Trues,
            R0 ^ fa, Rfa, !Cache),
        % This step can make R exponentially bigger than R0.
        R = make_node(R0 ^ value, Rtr, Rfa),
        !:Cache = !.Cache ^ elem(R0) := R
    else if compare((<), Var, R0 ^ value) then
        ( if LeaderVar = Var then
            add_equivalences_2(Vs, Trues `insert` Var,
                R0, Rtr, !Cache),
            add_equivalences_2(Vs, Trues, R0, Rfa, !Cache),
            % This step can make R exponentially bigger than R0.
            R = make_node(Var, Rtr, Rfa),
            !:Cache = !.Cache ^ elem(R0) := R
        else if Trues `contains` LeaderVar then
            add_equivalences_2(Vs, Trues, R0, Rtr, !Cache),
            R = make_node(Var, Rtr, zero),
            !:Cache = !.Cache ^ elem(R0) := R
        else
            add_equivalences_2(Vs, Trues, R0, Rfa, !Cache),
            % Since LeaderVar < Var, and every leader must
            % appear in an equivalence with itself, an ancestor
            % invocation of this predicate must already have seen
            % LeaderVar. If it is not in Trues, it must
            % therefore be false.
            R = make_node(Var, zero, Rfa),
            !:Cache = !.Cache ^ elem(R0) := R
        )
    else if LeaderVar = Var then
        add_equivalences_2(Vs, Trues `insert` Var, R0 ^ tr, Rtr, !Cache),
        add_equivalences_2(Vs, Trues, R0 ^ fa, Rfa, !Cache),
        R = make_node(Var, Rtr, Rfa),
        !:Cache = !.Cache ^ elem(R0) := R
    else if Trues `contains` LeaderVar then
        add_equivalences_2(Vs, Trues, R0 ^ tr, Rtr, !Cache),
        R = make_node(Var, Rtr, zero),
        !:Cache = !.Cache ^ elem(R0) := R
    else
        add_equivalences_2(Vs, Trues, R0 ^ fa, Rfa, !Cache),
        % Since LeaderVar < Var, and every leader must
        % appear in an equivalence with itself, an ancestor
        % invocation of this predicate must already have seen
        % LeaderVar. If it is not in Trues, it must
        % therefore be false.
        R = make_node(Var, zero, Rfa),
        !:Cache = !.Cache ^ elem(R0) := R
    ).

%---------------------------------------------------------------------------%

% XXX this could be made much more efficient by doing something similar
% to what we do in add_equivalences.

add_implications(ImpVars, R) = R ^
        add_implications_2(not_var, var, Imps) ^
        add_implications_2(var, not_var, RevImps) ^
        add_implications_2(not_var, not_var, DisImps) ^
        add_implications_2(var, var, RevDisImps) :-
    ImpVars = imp_vars(Imps, RevImps, DisImps, RevDisImps).

:- func add_implications_2(func(var(T)) = robdd(T), func(var(T)) = robdd(T),
        imp_map(T), robdd(T)) = robdd(T).

add_implications_2(FA, FB, IM, R0) =
    map.foldl(func(VA, Vs, R1) =
        foldl(func(VB, R2) = R2 * (FA(VA) + FB(VB)), Vs, R1),
        IM, R0).

%---------------------------------------------------------------------------%

:- pragma no_inline(is_terminal/1).
:- pragma foreign_proc("C",
    is_terminal(F::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = MR_ROBDD_IS_TERMINAL(F);
").

size(F, Nodes, Depth) :-
    size(F, Nodes, Depth, _).

size(F, Nodes, Depth, Vars) :-
    size_2(F, 0, Nodes, 0, Depth, 0, set_bbbtree.init, Seen),
    Vars = sort_and_remove_dups(list.map(value, to_sorted_list(Seen))).

    % XXX should see if sparse_bitset is more efficient than set_bbbtree.
:- pred size_2(robdd(T)::in, int::in, int::out, int::in, int::out, int::in,
        set_bbbtree(robdd(T))::in, set_bbbtree(robdd(T))::out) is det.

size_2(F, Nodes0, Nodes, Depth0, Depth, Val0, Seen0, Seen) :-
    ( if is_terminal(F) then
        Nodes = Nodes0, Depth = Depth0, Seen = Seen0
    else if term.var_to_int(F ^ value) =< Val0 then
        error("robdd invariant broken (possible loop)")
    else if F `member` Seen0 then
        Nodes = Nodes0, Depth = Depth0, Seen = Seen0
    else
        Val = term.var_to_int(F ^ value),
        size_2(F ^ tr, Nodes0+1, Nodes1, Depth0, Depth1, Val, Seen0, Seen1),
        size_2(F ^ fa, Nodes1, Nodes, Depth0, Depth2, Val, Seen1, Seen2),
        max(Depth1, Depth2, Max),
        Depth = Max + 1,
        Seen = Seen2 `insert` F
    ).

var_is_constrained(F, V) :-
    ( if is_terminal(F) then
        fail
    else
        compare(R, F ^ value, V),
        (
            R = (<),
            ( var_is_constrained(F ^ tr, V)
            ; var_is_constrained(F ^ fa, V)
            )
        ;
            R = (=)
        )
    ).

vars_are_constrained(F, Vs) :-
    vars_are_constrained_2(F, to_sorted_list(Vs)).

:- pred vars_are_constrained_2(robdd(T)::in, list(var(T))::in) is semidet.

vars_are_constrained_2(_, []).
vars_are_constrained_2(F, Vs) :-
    Vs = [V | Vs1],
    ( if is_terminal(F) then
        fail
    else
        compare(R, F ^ value, V),
        (
            R = (<),
            Vs2 = Vs
        ;
            R = (=),
            Vs2 = Vs1
        ),
        ( vars_are_constrained_2(F ^ tr, Vs2)
        ; vars_are_constrained_2(F ^ fa, Vs2)
        )
    ).

robdd_to_dot(Robdd, WV, Filename) -->
    io.tell(Filename, Result),
    (
        { Result = ok },
        robdd_to_dot(Robdd, WV),
        io.told
    ;
        { Result = error(Err) },
        io.stderr_stream(StdErr),
        io.nl(StdErr),
        io.write_string(StdErr, io.error_message(Err)),
        io.nl(StdErr)
    ).

robdd_to_dot(Robdd, WV) -->
    io.write_string(
"digraph G{
    center=true;
    size=""7,11"";
    ordering=out;
    node [shape=record,height=.1];
    concentrate=true;
"),
    { multi_map.init(Ranks0) },
    robdd_to_dot_2(Robdd, WV, set_bbbtree.init, _, Ranks0, Ranks),
    map.foldl((pred(_::in, Nodes::in, di, uo) is det -->
        io.write_string("{rank = same; "),
        list.foldl((pred(Node::in, di, uo) is det -->
            io.format("%s; ", [s(node_name(Node))])), Nodes),
        io.write_string("}\n")
        ), Ranks),
    io.write_string("}\n").

    % XXX should see if sparse_bitset is more efficient than set_bbbtree.
:- pred robdd_to_dot_2(robdd(T)::in, write_var(T)::in(write_var),
    set_bbbtree(robdd(T))::in, set_bbbtree(robdd(T))::out,
    multi_map(var(T), robdd(T))::in,
    multi_map(var(T), robdd(T))::out,
    io::di, io::uo) is det.

robdd_to_dot_2(Robdd, WV, Seen0, Seen, Ranks0, Ranks) -->
    ( if { is_terminal(Robdd) } then
        { Seen = Seen0 },
        { Ranks = Ranks0 }
    else if { Robdd `member` Seen0 } then
        { Seen = Seen0 },
        { Ranks = Ranks0 }
    else
        robdd_to_dot_2(Robdd ^ tr, WV, Seen0, Seen1, Ranks0, Ranks1),
        robdd_to_dot_2(Robdd ^ fa, WV, Seen1, Seen2, Ranks1, Ranks2),
        write_node(Robdd, WV),
        write_edge(Robdd, Robdd ^ tr, yes),
        write_edge(Robdd, Robdd ^ fa, no),
        { Seen = Seen2 `insert` Robdd },
        { multi_map.set( Robdd ^ value, Robdd, Ranks2, Ranks) }
    ).

:- pred write_node(robdd(T)::in, write_var(T)::in(write_var),
    io::di, io::uo) is det.

write_node(R, WV) -->
    io.format("%s [label=""<f0> %s|<f1> ",
        [s(node_name(R)), s(terminal_name(R ^ tr))]),
    WV(R ^ value),
    io.format("|<f2> %s", [s(terminal_name(R ^ fa))]),
    io.write_string("""];\n").

:- func node_name(robdd(T)) = string.

node_name(R) =
    ( if R = one then
        "true"
    else if R = zero then
        "false"
    else
        string.format("node%d", [i(node_num(R))])
    ).

:- func node_num(robdd(T)) = int.
:- pragma foreign_proc("C",
    node_num(R::in) = (N::out),
    [will_not_call_mercury, promise_pure],
"
    N = (MR_Integer) R;
").

:- func terminal_name(robdd(T)) = string.

terminal_name(R) =
    ( if R = zero then
        "0"
    else if R = one then
        "1"
    else
        ""
    ).

:- pred write_edge(robdd(T)::in, robdd(T)::in, bool::in,
    io::di, io::uo) is det.

write_edge(R0, R1, Arc) -->
    ( if { is_terminal(R1) } then
        []
    else
        io.format("""%s"":%s -> ""%s"":f1 [label=""%s""];\n",
            [s(node_name(R0)), s(if Arc = yes then "f0" else "f2"),
            s(node_name(R1)), s(if Arc = yes then "t" else "f")])
    ).

labelling(Vars, R, TrueVars, FalseVars) :-
    labelling_2(to_sorted_list(Vars), R, empty_vars_set, TrueVars,
        empty_vars_set, FalseVars).

:- pred labelling_2(list(var(T))::in, robdd(T)::in, vars(T)::in,
    vars(T)::out, vars(T)::in, vars(T)::out) is nondet.

labelling_2([], _, TrueVars, TrueVars, FalseVars, FalseVars).
labelling_2([V | Vs], R0, TrueVars0, TrueVars, FalseVars0, FalseVars) :-
    R = var_restrict_false(V, R0),
    R \= zero,
    labelling_2(Vs, R, TrueVars0, TrueVars, FalseVars0 `insert` V, FalseVars).
labelling_2([V | Vs], R0, TrueVars0, TrueVars, FalseVars0, FalseVars) :-
    R = var_restrict_true(V, R0),
    R \= zero,
    labelling_2(Vs, R, TrueVars0 `insert` V, TrueVars, FalseVars0, FalseVars).

minimal_model(Vars, R, TrueVars, FalseVars) :-
    ( if is_empty(Vars) then
        TrueVars = empty_vars_set,
        FalseVars = empty_vars_set
    else
        minimal_model_2(to_sorted_list(Vars), R, empty_vars_set,
            TrueVars0, empty_vars_set, FalseVars0),
        (
            TrueVars = TrueVars0,
            FalseVars = FalseVars0
        ;
            minimal_model(Vars, R * (~conj_vars(TrueVars0)),
                TrueVars, FalseVars)
        )
    ).

:- pred minimal_model_2(list(var(T))::in, robdd(T)::in, vars(T)::in,
    vars(T)::out, vars(T)::in, vars(T)::out) is semidet.

minimal_model_2([], _, TrueVars, TrueVars, FalseVars, FalseVars).
minimal_model_2([V | Vs], R0, TrueVars0, TrueVars, FalseVars0, FalseVars) :-
    R1 = var_restrict_false(V, R0),
    ( if R1 = zero then
        R2 = var_restrict_true(V, R0),
        R2 \= zero,
        minimal_model_2(Vs, R2, TrueVars0 `insert` V, TrueVars,
            FalseVars0, FalseVars)
    else
        minimal_model_2(Vs, R1, TrueVars0, TrueVars,
            FalseVars0 `insert` V, FalseVars)
    ).

%---------------------------------------------------------------------------%

:- pragma promise_pure(clear_caches/2).
clear_caches(!IO) :-
    impure clear_caches.

:- pragma no_inline(clear_caches/0).
:- pragma foreign_proc("C",
    clear_caches,
    [will_not_call_mercury],
"
    MR_ROBDD_init_caches();
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
