%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% Author: Ralph Becket
%
% Simple solver type supporting equality and disequality constraints.
%
%-----------------------------------------------------------------------------%

:- module eqneq.
:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

    % An eqneq(T) variable may be bound to a ground value of type T or it may
    % be unbound.  Equality and disequality constraints may be posted between
    % eqneq(T) variables.
    %
:- solver type eqneq(T).

    % Construct a new eqneq.
    %
:- pred new(eqneq(T)::oa) is det.

    % Construct N new eqneqs.
    %
:- pred n_new(int::in, list(eqneq(T))::oa) is det.

    % Post an equality/disequality constraint between two eqneqs.
    %
:- pred eq(eqneq(T)::ia, eqneq(T)::ia) is semidet.
:- pred neq(eqneq(T)::ia, eqneq(T)::ia) is semidet.

    % Bind an eqneq to a ground value.
    %
:- pred bind(eqneq(T)::ia, T::in) is semidet.

    % Ask for the value, if any, bound to an eqneq.
    %
:- impure pred ask_value(eqneq(T)::ia, T::out) is semidet.

    % Utility predicate.
    %
:- pred all_different(list(eqneq(T))::ia) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module univ.

:- pragma require_feature_set([trailing]).

%-----------------------------------------------------------------------------%

    % An eqneq is represented by an eqneq_id, which is a key in the
    % mutable constraint_store map.
    %
:- solver type eqneq(T) where
    representation is eqneq_id,
    equality is eq.

:- type eqneq_id == int.

    % The constraint_store mutable maps eqneq_ids to eqneq_reps.
    %
:- mutable(constraint_store, constraint_store, map.init, ground).

:- type constraint_store == map(eqneq_id, eqneq_rep).

    % The eqneq_id_counter mutable is used to allocate new eqneq_ids.
    %
:- mutable(eqneq_id_counter, counter, counter.init(0), ground).

    % Each eqneq is either free, bound, or aliased to another eqneq.
    %
:- type eqneq_rep
    --->    free_neq(set(eqneq_id))
            % A free eqneq node records the set of variables it is
            % not equal to.
    ;       alias(eqneq_id)
            % An aliased eqneq is represented by another eqneq.
    ;       bound(univ).
            % A bound eqneq has a value stored as a univ.

%-----------------------------------------------------------------------------%

    % Create a new, free, eqneq.
    %
new(EqNeq) :-
    promise_pure (
        semipure get_eqneq_id_counter(EqNeqIdCounter0),
        semipure get_constraint_store(ConstraintStore0),
        counter.allocate(EqNeqId, EqNeqIdCounter0, EqNeqIdCounter),
        NeqSet = set.init,
        ConstraintStore = ConstraintStore0 ^ elem(EqNeqId) := free_neq(NeqSet),
        impure set_eqneq_id_counter(EqNeqIdCounter),
        impure set_constraint_store(ConstraintStore),
        impure EqNeq = 'representation to any eqneq/1'(EqNeqId)
    ).

%-----------------------------------------------------------------------------%

    % Create a list of new, free, eqneqs.
    %
n_new(N, EqNeqs) :-
    ( if N =< 0 then
        EqNeqs = []
    else
        new(EqNeq),
        n_new(N - 1, EqNeqs0),
        EqNeqs = [EqNeq | EqNeqs0]
    ).

%-----------------------------------------------------------------------------%

    % Follow the alias chain for an eqneq to the end, keeping track of the
    % number of links in the chain (this information helps keep alias chains
    % short).
    %
:- pred deref(constraint_store::in, eqneq_id::in, eqneq_id::out,
    eqneq_rep::out, int::out) is det.

deref(ConstraintStore, EqNeqId0, EqNeqId, EqNeqRep, Depth) :-
    deref_2(ConstraintStore, EqNeqId0, EqNeqId, EqNeqRep, 0, Depth).

:- pred deref_2(constraint_store::in, eqneq_id::in, eqneq_id::out,
    eqneq_rep::out, int::in, int::out) is det.

deref_2(ConstraintStore, EqNeqId0, EqNeqId, EqNeqRep, Depth0, Depth) :-
    EqNeqRep0 = ConstraintStore ^ det_elem(EqNeqId0),
    ( if EqNeqRep0 = alias(EqNeqId1) then
        deref_2(ConstraintStore, EqNeqId1, EqNeqId, EqNeqRep,
            Depth0 + 1, Depth)
    else
        EqNeqId = EqNeqId0,
        EqNeqRep = EqNeqRep0,
        Depth = Depth0
    ).

%-----------------------------------------------------------------------------%

    % Constrain two eqneqs to be equal.  We ensure that neq sets are kept
    % up-to-date after unifications.
    %
eq(EqNeqA, EqNeqB) :-
    promise_pure (
        impure EqNeqIdA0 = 'representation of any eqneq/1'(EqNeqA),
        impure EqNeqIdB0 = 'representation of any eqneq/1'(EqNeqB),
        semipure get_constraint_store(ConstraintStore0),

        deref(ConstraintStore0, EqNeqIdA0, EqNeqIdA, EqNeqRepA, DepthA),
        deref(ConstraintStore0, EqNeqIdB0, EqNeqIdB, EqNeqRepB, DepthB),
        ( if EqNeqIdA = EqNeqIdB then
            % eqneqs with the same id are already equated.
            true
        else
            (
                % If both are free, then check the neq constraints and
                % make one an alias of the other, unifying the neq
                % constraints.  We use the depth information to keep
                % alias chains short.
                EqNeqRepA = free_neq(NeqSetA0),
                EqNeqRepB = free_neq(NeqSetB0),

                NeqSetA = update_neq_set(ConstraintStore0, NeqSetA0),
                not set.member(EqNeqIdA, NeqSetB0),
                NeqSetB = update_neq_set(ConstraintStore0, NeqSetB0),
                not set.member(EqNeqIdB, NeqSetA0),
                NeqSetAB = set.union(NeqSetA, NeqSetB),

                ( if DepthB =< DepthA then
                    ConstraintStore =
                       (( ConstraintStore0
                        ^ elem(EqNeqIdA) := free_neq(NeqSetAB) )
                        ^ elem(EqNeqIdB) := alias(EqNeqIdA)   )
                  else
                    ConstraintStore =
                       (( ConstraintStore0
                        ^ elem(EqNeqIdB) := free_neq(NeqSetAB) )
                        ^ elem(EqNeqIdA) := alias(EqNeqIdB)   )
                ),
                impure set_constraint_store(ConstraintStore)
            ;
                % A is free, B is bound: we need only check the
                % neq constraints.
                EqNeqRepA = free_neq(NeqSetA0),
                EqNeqRepB = bound(_UnivB),

                NeqSetA = update_neq_set(ConstraintStore0, NeqSetA0),
                not set.member(EqNeqIdB, NeqSetA),
                ConstraintStore =
                    ConstraintStore0 ^ elem(EqNeqIdA) := EqNeqRepB,
                impure set_constraint_store(ConstraintStore)
            ;
                % B is free, A is bound: we need only check the
                % neq constraints.
                EqNeqRepA = bound(_UnivA),
                EqNeqRepB = free_neq(NeqSetB0),

                NeqSetB = update_neq_set(ConstraintStore0, NeqSetB0),
                not set.member(EqNeqIdA, NeqSetB),
                ConstraintStore =
                    ConstraintStore0 ^ elem(EqNeqIdB) := EqNeqRepA,
                impure set_constraint_store(ConstraintStore)
            ;
                % Both are bound.
                EqNeqRepA = bound(UnivA),
                EqNeqRepB = bound(UnivB),

                UnivA = UnivB
            )
        )
    ).

:- func update_neq_set(constraint_store, set(eqneq_id)) = set(eqneq_id).

update_neq_set(ConstraintStore, Neqs0) = Neqs :-
    P =
        ( func(EqNeqId0, NeqSet0) = NeqSet :-
            deref(ConstraintStore, EqNeqId0, EqNeqId, _EqNeqRep, _Depth),
            NeqSet = set.insert(NeqSet0, EqNeqId)
        ),
    Neqs = set.fold(P, Neqs0, set.init).

%-----------------------------------------------------------------------------%

    % Constrain two eqneqs to be different.
    %
neq(EqNeqA, EqNeqB) :-
    promise_pure (
        impure EqNeqIdA0 = 'representation of any eqneq/1'(EqNeqA),
        impure EqNeqIdB0 = 'representation of any eqneq/1'(EqNeqB),
        semipure get_constraint_store(ConstraintStore0),

        deref(ConstraintStore0, EqNeqIdA0, EqNeqIdA, EqNeqRepA, _DepthA),
        deref(ConstraintStore0, EqNeqIdB0, EqNeqIdB, EqNeqRepB, _DepthB),

        EqNeqIdA \= EqNeqIdB,
        (
            % Both eqneqs are free.
            EqNeqRepA = free_neq(NeqSetA0),
            EqNeqRepB = free_neq(NeqSetB0),

            NeqSetA = set.insert(NeqSetA0, EqNeqIdB),
            NeqSetB = set.insert(NeqSetB0, EqNeqIdA),
            ConstraintStore =
               (( ConstraintStore0 ^ elem(EqNeqIdA) := free_neq(NeqSetA) )
                                   ^ elem(EqNeqIdB) := free_neq(NeqSetB) ),
            impure set_constraint_store(ConstraintStore)
        ;
            % A is free, B is bound.
            EqNeqRepA = free_neq(NeqSetA0),
            EqNeqRepB = bound(_UnivB),

            NeqSetA = set.insert(NeqSetA0, EqNeqIdB),
            ConstraintStore =
                ConstraintStore0 ^ elem(EqNeqIdA) := free_neq(NeqSetA),
            impure set_constraint_store(ConstraintStore)
        ;
            % A is bound, B is free.
            EqNeqRepA = bound(_UnivA),
            EqNeqRepB = free_neq(NeqSetB0),

            NeqSetB = set.insert(NeqSetB0, EqNeqIdA),
            ConstraintStore =
                ConstraintStore0 ^ elem(EqNeqIdB) := free_neq(NeqSetB),
            impure set_constraint_store(ConstraintStore)
        ;
            % A and B are bound.
            EqNeqRepA = bound(UnivA),
            EqNeqRepB = bound(UnivB),

            UnivA \= UnivB
        )
    ).

%-----------------------------------------------------------------------------%

    % Bind an eqneq to a ground value.
    %
bind(EqNeq, Value) :-
    promise_pure (
        impure EqNeqId0 = 'representation of any eqneq/1'(EqNeq),
        semipure get_constraint_store(ConstraintStore0),
        deref(ConstraintStore0, EqNeqId0, EqNeqId, EqNeqRep, _Depth),
        (
            EqNeqRep = free_neq(NeqSet),
            not (
                set.member(EqNeqIdX0, NeqSet),
                deref(ConstraintStore0, EqNeqIdX0, _EqNeqIdX, EqNeqRepX,
                    _DepthX),
                EqNeqRepX = bound(UnivX),
                univ_to_type(UnivX, Value)
            ),
            ConstraintStore =
                ConstraintStore0 ^ elem(EqNeqId) := bound(univ(Value)),
            impure set_constraint_store(ConstraintStore)
        ;
            EqNeqRep = bound(Univ),
            univ_to_type(Univ, Value)
        )
    ).

%-----------------------------------------------------------------------------%

    % Ask for the value bound to an eqneq, if any.
    %
ask_value(EqNeq, Value) :-
    impure EqNeqId0 = 'representation of any eqneq/1'(EqNeq),
    semipure get_constraint_store(ConstraintStore0),
    deref(ConstraintStore0, EqNeqId0, _EqNeqId, EqNeqRep, _Depth),
    EqNeqRep = bound(Univ),
    univ_to_type(Univ, Value).

%-----------------------------------------------------------------------------%

all_different([]).
all_different([X | Xs]) :-
    all_different_2(X, Xs),
    all_different(Xs).

:- pred all_different_2(eqneq(T)::ia, list(eqneq(T))::ia) is semidet.

all_different_2(_, []).
all_different_2(X, [Y | Ys]) :-
    neq(X, Y),
    all_different_2(X, Ys).

%-----------------------------------------------------------------------------%
:- end_module eqneq.
%-----------------------------------------------------------------------------%
