%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2000-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: inst_test.m.
% Author: fjh.
%
% Predicates to test various properties of insts.
%
% NOTE: `not_reached' insts are considered to satisfy all of these predicates
% except inst_is_clobbered.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.inst_test.
:- interface.

:- import_module hlds.
:- import_module hlds.instmap.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Succeed if the inst is fully ground (i.e. contains only `ground',
    % `bound', and `not_reached' insts, with no `free' or `any' insts).
    % This predicate succeeds for non-default function insts so some care
    % needs to be taken since these insts may not be replaced by a less
    % precise inst that uses the higher-order mode information.
    %
:- pred inst_is_ground(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is not partly free (i.e. contains only `any',
    % `ground', `bound', and `not_reached' insts, with no `free' insts).
    % This predicate succeeds for non-default function insts so some care
    % needs to be taken since these insts may not be replaced by a less
    % precise inst that uses the higher-order mode information.
    %
:- pred inst_is_ground_or_any(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is `unique'.
    %
    % XXX The documentation on the code used to say: "inst_is_unique succeeds
    % iff the inst passed is unique or free. Abstract insts are not considered
    % unique.". The part about free is dubious.
    %
:- pred inst_is_unique(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is `mostly_unique' or `unique'.
    %
    % XXX The documentation on the code used to say: " inst_is_mostly_unique
    % succeeds iff the inst passed is unique, mostly_unique, or free.
    % Abstract insts are not considered unique.". The part about free is
    % dubious.
    %
:- pred inst_is_mostly_unique(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is not `mostly_unique' or `unique', i.e.
    % if it is shared or free. It fails for abstract insts.
    %
:- pred inst_is_not_partly_unique(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is not `unique', i.e. if it is mostly_unique,
    % shared, or free. It fails for abstract insts.
    %
:- pred inst_is_not_fully_unique(module_info::in, mer_inst::in) is semidet.

    % inst_is_clobbered succeeds iff the inst passed is `clobbered'
    % or `mostly_clobbered' or if it is a user-defined inst which
    % is defined as one of those.
    %
:- pred inst_is_clobbered(module_info::in, mer_inst::in) is semidet.

    % inst_is_free succeeds iff the inst passed is `free'
    % or is a user-defined inst which is defined as `free'.
    % Abstract insts must not be free.
    %
:- pred inst_is_free(module_info::in, mer_inst::in) is semidet.

:- pred inst_is_any(module_info::in, mer_inst::in) is semidet.

    % inst_is_bound succeeds iff the inst passed is not `free'
    % or is a user-defined inst which is not defined as `free'.
    % Abstract insts must be bound.
    %
:- pred inst_is_bound(module_info::in, mer_inst::in) is semidet.

:- pred inst_is_bound_to_functors(module_info::in, mer_inst::in,
    list(bound_inst)::out) is semidet.

%-----------------------------------------------------------------------------%

:- pred inst_results_bound_inst_list_is_ground(module_info::in,
    inst_test_results::in, list(bound_inst)::in) is semidet.

:- pred inst_results_bound_inst_list_is_ground_mt(module_info::in,
    maybe(mer_type)::in, inst_test_results::in, list(bound_inst)::in)
    is semidet.

:- pred inst_results_bound_inst_list_is_ground_or_any(module_info::in,
    inst_test_results::in, list(bound_inst)::in) is semidet.

:- pred bound_inst_list_is_unique(module_info::in, list(bound_inst)::in)
    is semidet.

:- pred bound_inst_list_is_mostly_unique(module_info::in, list(bound_inst)::in)
    is semidet.

:- pred bound_inst_list_is_not_partly_unique(module_info::in,
    list(bound_inst)::in) is semidet.

:- pred bound_inst_list_is_not_fully_unique(module_info::in,
    list(bound_inst)::in) is semidet.

:- pred bound_inst_list_is_free(module_info::in, list(bound_inst)::in)
    is semidet.

%-----------------------------------------------------------------------------%

:- pred inst_list_is_ground(module_info::in, list(mer_inst)::in) is semidet.

:- pred inst_list_is_ground_or_any(module_info::in, list(mer_inst)::in)
    is semidet.

:- pred inst_list_is_unique(module_info::in, list(mer_inst)::in) is semidet.

:- pred inst_list_is_mostly_unique(module_info::in, list(mer_inst)::in)
    is semidet.

:- pred inst_list_is_not_partly_unique(module_info::in, list(mer_inst)::in)
    is semidet.

:- pred inst_list_is_not_fully_unique(module_info::in, list(mer_inst)::in)
    is semidet.

:- pred inst_list_is_free(module_info::in, list(mer_inst)::in) is semidet.

    % Given a list of insts, and a corresponding list of livenesses, return
    % true iff for every element in the list of insts, either the element is
    % ground or the corresponding element in the liveness list is dead.
    %
:- pred inst_list_is_ground_or_dead(module_info::in,
    list(is_live)::in, list(mer_inst)::in) is semidet.

    % Given a list of insts, and a corresponding list of livenesses, return
    % true iff for every element in the list of insts, either the element is
    % ground or any, or the corresponding element in the liveness list is
    % dead.
    %
:- pred inst_list_is_ground_or_any_or_dead(module_info::in,
    list(is_live)::in, list(mer_inst)::in) is semidet.

%-----------------------------------------------------------------------------%

    % Succeed iff the specified inst contains (directly or indirectly) the
    % specified inst_name.
    %
:- pred inst_contains_inst_name(module_info::in, inst_name::in, mer_inst::in)
    is semidet.

%-----------------------------------------------------------------------------%

    % For a non-solver type t (i.e. any type declared without using the
    % `solver' keyword), the inst `any' should be considered to be equivalent
    % to a bound inst i where i contains all the functors of the type t and
    % each argument has inst `any'.
    %
    % Note that pred and func types are considered solver types, since
    % higher-order terms that contain non-local solver variables are
    % themselves not ground -- they only become ground when all non-locals do.
    % However, functions with the default inst can still be treated as ground,
    % since they are det and therefore cannot bind any non-local solver
    % variables that may be present.
    %
:- pred maybe_any_to_bound(module_info::in, maybe(mer_type)::in,
    uniqueness::in, ho_inst_info::in, mer_inst::out) is semidet.

%---------------------------------------------------------------------------%

    % Succeed iff the inst is any or contains any.
    %
:- pred inst_contains_any(module_info::in, mer_inst::in) is semidet.

    % Succeed iff the given var's inst is any or contains any.
    %
:- pred var_inst_contains_any(module_info::in, instmap::in, prog_var::in)
    is semidet.

:- pred inst_contains_higher_order(module_info::in, mer_inst::in) is semidet.

    % Return true if the given inst may restrict the set of function symbols
    % that may be successfully unified with the variable that has this inst.
    %
:- func inst_may_restrict_cons_ids(module_info, mer_inst) = bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.passes_aux.
:- import_module parse_tree.prog_type.

:- import_module io.
:- import_module require.
:- import_module set.
:- import_module set_tree234.

%-----------------------------------------------------------------------------%

inst_is_ground(ModuleInfo, Inst) :-
    % inst_is_ground succeeds iff the inst passed is `ground' or the
    % equivalent. Abstract insts are not considered ground.
    promise_pure (
        semipure lookup_inst_is_ground(Inst, Found, OldIsGround),
        (
            Found = yes,
            trace [compiletime(flag("inst-is-ground-perf")), io(!IO)] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.write_string(DebugStream, "inst_is_ground hit\n", !IO)
            ),
            % Succeed if OldIsGround = yes, fail if OldIsGround = no.
            OldIsGround = yes
        ;
            Found = no,
            trace [compiletime(flag("inst-is-ground-perf")), io(!IO)] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.write_string(DebugStream, "inst_is_ground miss\n", !IO)
            ),
            ( if inst_is_ground_mt(ModuleInfo, no, Inst) then
                impure record_inst_is_ground(Inst, yes)
                % Succeed.
            else
                impure record_inst_is_ground(Inst, no),
                fail
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% The expansion of terms by the superhomogeneous transformation generates code
% that looks like this:
%
%   V1 = [],
%   V2 = e1,
%   V3 = [V2 | V1],
%   V4 = e2,
%   V5 = [V3 | V4]
%
% The insts on those unifications will contain insts from earlier unifications.
% For example, the inst on the unification building V5 will give V5 an inst
% that contains the insts of V3 and V4.
%
% If there are N elements in a list, testing the insts of the N variables
% representing the N cons cells in the list would ordinarily take O(N^2) steps.
% Since N could be very large, this is disastrous.
%
% We avoid quadratic performance by caching the results of recent calls
% to inst_is_ground for insts that are susceptible to this problem.
% This way, the test on the inst of e.g. V5 will find the results of the tests
% on the insts of V3 and V4 already available. This reduces the overall
% complexity of testing the insts of those N variables to O(n).
%
% The downsides of this cache include the costs of the lookups, and
% the fact that it keeps the cached insts alive.
%
% Note that we do not need to record the ModuleInfo argument of inst_is_ground,
% since it is needed only to interpret insts that need access to the mode
% tables. If we get a result for an inst with one ModuleInfo, we should get
% the exact same result with any later ModuleInfo. The conservative nature
% of the Boehm collector means that an inst address recorded in the cache
% will always point to the original inst; the address cannot be reused until
% the cache entry is itself reused.

:- pragma foreign_decl("C",
"
typedef struct {
    MR_Word     iig_inst_addr;
    MR_Word     iig_is_ground;
} InstIsGroundCacheEntry;

#define INST_IS_GROUND_CACHE_SIZE 1307

/*
** Every entry should be implicitly initialized to zeros. Since zero is
** not a valid address for an inst, uninitialized entries cannot be mistaken
** for filled-in entries.
*/

static  InstIsGroundCacheEntry
                inst_is_ground_cache[INST_IS_GROUND_CACHE_SIZE];
").

    % Look up Inst in the cache. If it is there, return Found = yes
    % and set MayOccur. Otherwise, return Found = no.
    %
:- semipure pred lookup_inst_is_ground(mer_inst::in,
    bool::out, bool::out) is det.

:- pragma foreign_proc("C",
    lookup_inst_is_ground(Inst::in, Found::out, IsGround::out),
    [will_not_call_mercury, promise_semipure],
"
    MR_Unsigned hash;

    hash = (MR_Unsigned) Inst;
    hash = hash >> MR_LOW_TAG_BITS;
    hash = hash % INST_IS_GROUND_CACHE_SIZE;

    if (inst_is_ground_cache[hash].iig_inst_addr == Inst) {
        Found = MR_BOOL_YES;
        IsGround = inst_is_ground_cache[hash].iig_is_ground;
    } else {
        Found = MR_BOOL_NO;
        IsGround = MR_NO;
    }
").

lookup_inst_is_ground(_, no, no) :-
    semipure semipure_true.

    % Record the result for Inst in the cache.
    %
:- impure pred record_inst_is_ground(mer_inst::in, bool::in) is det.

:- pragma foreign_proc("C",
    record_inst_is_ground(Inst::in, IsGround::in),
    [will_not_call_mercury],
"
    MR_Unsigned hash;

    hash = (MR_Unsigned) Inst;
    hash = hash >> MR_LOW_TAG_BITS;
    hash = hash % INST_IS_GROUND_CACHE_SIZE;
    /* We overwrite any existing entry in the slot. */
    inst_is_ground_cache[hash].iig_inst_addr = Inst;
    inst_is_ground_cache[hash].iig_is_ground = IsGround;
").

record_inst_is_ground(_, _) :-
    impure impure_true.

%-----------------------------------------------------------------------------%

:- pred inst_is_ground_mt(module_info::in, maybe(mer_type)::in, mer_inst::in)
    is semidet.

inst_is_ground_mt(ModuleInfo, MaybeType, Inst) :-
    Expansions0 = set_tree234.init,
    inst_is_ground_mt_1(ModuleInfo, MaybeType, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_ground_mt_1(module_info::in, maybe(mer_type)::in, mer_inst::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

inst_is_ground_mt_1(ModuleInfo, MaybeType, Inst, !Expansions) :-
    % XXX This special casing of any/2 was introduced in version 1.65
    % of this file. The log message for that version gives a reason why
    % this special casing is required, but I (zs) don't believe it,
    % at least not without more explanation.
    ( if Inst = any(_, _) then
        ( if set_tree234.contains(!.Expansions, Inst) then
            true
        else
            inst_is_ground_mt_2(ModuleInfo, MaybeType, Inst, !Expansions)
        )
    else
        % XXX Make this work on Inst's *address*.
        ( if set_tree234.insert_new(Inst, !Expansions) then
            % Inst was not yet in Expansions, but we have now inserted it.
            inst_is_ground_mt_2(ModuleInfo, MaybeType, Inst, !Expansions)
        else
            % Inst was already in !.Expansions.
            true
        )
    ).

:- pred inst_is_ground_mt_2(module_info::in, maybe(mer_type)::in, mer_inst::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

inst_is_ground_mt_2(ModuleInfo, MaybeType, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = free
        ; Inst = free(_)
        ),
        fail
    ;
        ( Inst = not_reached
        ; Inst = ground(_, _)
        )
    ;
        Inst = bound(_, InstResults, BoundInsts),
        inst_results_bound_inst_list_is_ground_mt_2(ModuleInfo, MaybeType,
            InstResults, BoundInsts, !Expansions)
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_ground_mt_1(ModuleInfo, MaybeType, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_ground_mt_1(ModuleInfo, MaybeType, NextInst, !Expansions)
    ;
        Inst = any(Uniq, HOInstInfo),
        maybe_any_to_bound(ModuleInfo, MaybeType, Uniq, HOInstInfo, NextInst),
        inst_is_ground_mt_1(ModuleInfo, MaybeType, NextInst, !Expansions)
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

%-----------------------------------------------------------------------------%

inst_is_ground_or_any(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_ground_or_any_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_ground_or_any_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_ground_or_any_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = ground(_, _)
        ; Inst = any(_, _)
        ; Inst = not_reached
        )
    ;
        Inst = bound(_, InstResults, BoundInsts),
        inst_results_bound_inst_list_is_ground_or_any_2(ModuleInfo,
            InstResults, BoundInsts, !Expansions)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = abstract_inst(_, _)   % XXX is this right?
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_ground_or_any_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( if set.insert_new(Inst, !Expansions) then
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_ground_or_any_2(ModuleInfo, NextInst, !Expansions)
        else
            true
        )
    ).

%-----------------------------------------------------------------------------%

inst_is_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_unique_2(ModuleInfo, Inst, !Expansions) :-
    (
        ( Inst = ground(unique, _)
        ; Inst = any(unique, _)
        ; Inst = not_reached
        ; Inst = free               % XXX I don't think this is right [zs].
        )
    ;
        ( Inst = ground(shared, _)
        ; Inst = bound(shared, _, _)
        ; Inst = any(shared, _)
        ),
        fail
    ;
        Inst = bound(unique, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            fail
        ;
            ( InstResults = inst_test_no_results
            ; InstResults = inst_test_results(_, _, _, _, _, _)
            ),
            bound_inst_list_is_unique_2(ModuleInfo, BoundInsts, !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( if set.insert_new(Inst, !Expansions) then
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_unique_2(ModuleInfo, NextInst, !Expansions)
        else
            true
        )
    ).

%-----------------------------------------------------------------------------%

inst_is_mostly_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_mostly_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_mostly_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_mostly_unique_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = ground(unique, _)
        ; Inst = ground(mostly_unique, _)
        ; Inst = any(unique, _)
        ; Inst = any(mostly_unique, _)
        )
    ;
        Inst = bound(unique, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            fail
        ;
            ( InstResults = inst_test_no_results
            ; InstResults = inst_test_results(_, _, _, _, _, _)
            ),
            bound_inst_list_is_mostly_unique_2(ModuleInfo, BoundInsts,
                !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_mostly_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( if set.insert_new(Inst, !Expansions) then
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_mostly_unique_2(ModuleInfo, NextInst, !Expansions)
        else
            true
        )
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

%-----------------------------------------------------------------------------%

inst_is_not_partly_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_not_partly_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_not_partly_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_not_partly_unique_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = any(shared, _)
        ; Inst = ground(shared, _)
        )
    ;
        Inst = bound(shared, InstResult, BoundInsts),
        (
            InstResult = inst_test_results_fgtc
        ;
            ( InstResult = inst_test_no_results
            ; InstResult = inst_test_results(_, _, _, _, _, _)
            ),
            bound_inst_list_is_not_partly_unique_2(ModuleInfo, BoundInsts,
                !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_not_partly_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( if set.insert_new(Inst, !Expansions) then
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_not_partly_unique_2(ModuleInfo, NextInst, !Expansions)
        else
            true
        )
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

%-----------------------------------------------------------------------------%

inst_is_not_fully_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_not_fully_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded - we
    % use this to avoid going into an infinite loop.
    %
:- pred inst_is_not_fully_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_not_fully_unique_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = ground(shared, _)
        ; Inst = ground(mostly_unique, _)
        ; Inst = any(shared, _)
        ; Inst = any(mostly_unique, _)
        )
    ;
        Inst = bound(Uniq, InstResult, BoundInsts),
        ( Uniq = shared
        ; Uniq = mostly_unique
        ),
        (
            InstResult = inst_test_results_fgtc
        ;
            ( InstResult = inst_test_no_results
            ; InstResult = inst_test_results(_, _, _, _, _, _)
            ),
            bound_inst_list_is_not_fully_unique_2(ModuleInfo, BoundInsts,
                !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_not_fully_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( if set.insert_new(Inst, !Expansions) then
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_not_fully_unique_2(ModuleInfo, NextInst, !Expansions)
        else
            true
        )
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

%-----------------------------------------------------------------------------%

inst_is_clobbered(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ),
        fail
    ;
        ( Inst = any(mostly_clobbered, _)
        ; Inst = any(clobbered, _)
        ; Inst = ground(clobbered, _)
        ; Inst = ground(mostly_clobbered, _)
        ; Inst = bound(clobbered, _, _)
        ; Inst = bound(mostly_clobbered, _, _)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_clobbered(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_clobbered(ModuleInfo, NextInst)
    ).

inst_is_free(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        ( Inst = free
        ; Inst = free(_)
        )
    ;
        ( Inst = ground(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = any(_, _)
        ; Inst = not_reached
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_free(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_free(ModuleInfo, NextInst)
    ).

inst_is_any(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        Inst = any(_, _)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = ground(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = not_reached
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_any(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_any(ModuleInfo, NextInst)
    ).

inst_is_bound(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        ( Inst = ground(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = any(_, _)
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ; Inst = not_reached
        )
    ;
        ( Inst = free
        ; Inst = free(_)
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_bound(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_bound(ModuleInfo, NextInst)
    ).

inst_is_bound_to_functors(ModuleInfo, Inst, Functors) :-
    % inst_is_bound_to_functors succeeds iff the inst passed is
    % `bound(_Uniq, Functors)' or is a user-defined inst which expands to
    % `bound(_Uniq, Functors)'.
    %
    require_complete_switch [Inst]
    (
        Inst = bound(_Uniq, _InstResult, Functors)
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_bound_to_functors(ModuleInfo, SubInst, Functors)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_bound_to_functors(ModuleInfo, NextInst, Functors)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = any(_, _)
        ; Inst = ground(_, _)
        ; Inst = abstract_inst(_, _)
        ; Inst = not_reached
        ),
        fail
    ).

%-----------------------------------------------------------------------------%

inst_results_bound_inst_list_is_ground(ModuleInfo, InstResults, BoundInsts) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_mt(ModuleInfo, no, BoundInsts)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_mt(ModuleInfo, no, BoundInsts)
    ).

inst_results_bound_inst_list_is_ground_mt(ModuleInfo, MaybeType, InstResults,
        BoundInsts) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_mt(ModuleInfo, MaybeType, BoundInsts)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_mt(ModuleInfo, MaybeType, BoundInsts)
    ).

:- pred inst_results_bound_inst_list_is_ground_mt_2(module_info::in,
    maybe(mer_type)::in, inst_test_results::in, list(bound_inst)::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

inst_results_bound_inst_list_is_ground_mt_2(ModuleInfo, MaybeType, InstResults,
        BoundInsts, !Expansions) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_mt_2(ModuleInfo, MaybeType, BoundInsts,
                !Expansions)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_mt_2(ModuleInfo, MaybeType, BoundInsts,
            !Expansions)
    ).

%-----------------------------------------------------------------------------%

inst_results_bound_inst_list_is_ground_or_any(ModuleInfo, InstResults,
        BoundInsts) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            ( GroundnessResult = inst_result_is_not_ground
            ; GroundnessResult = inst_result_groundness_unknown
            ),
            bound_inst_list_is_ground_or_any(ModuleInfo, BoundInsts)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_or_any(ModuleInfo, BoundInsts)
    ).

:- pred inst_results_bound_inst_list_is_ground_or_any_2(module_info::in,
    inst_test_results::in, list(bound_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_results_bound_inst_list_is_ground_or_any_2(ModuleInfo, InstResults,
        BoundInsts, !Expansions) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_or_any_2(ModuleInfo, BoundInsts,
                !Expansions)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_or_any_2(ModuleInfo, BoundInsts, !Expansions)
    ).

%-----------------------------------------------------------------------------%

:- pred bound_inst_list_is_ground_mt(module_info::in, maybe(mer_type)::in,
    list(bound_inst)::in) is semidet.

bound_inst_list_is_ground_mt(_, _, []).
bound_inst_list_is_ground_mt(ModuleInfo, MaybeType,
        [BoundInst | BoundInsts]) :-
    BoundInst = bound_functor(Name, Args),
    maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, Name,
        list.length(Args), MaybeTypes),
    inst_list_is_ground_mt(ModuleInfo, MaybeTypes, Args),
    bound_inst_list_is_ground_mt(ModuleInfo, MaybeType, BoundInsts).

:- pred bound_inst_list_is_ground_mt_2(module_info::in, maybe(mer_type)::in,
    list(bound_inst)::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

bound_inst_list_is_ground_mt_2(_, _, [], !Expansions).
bound_inst_list_is_ground_mt_2(ModuleInfo, MaybeType, [BoundInst | BoundInsts],
        !Expansions) :-
    BoundInst = bound_functor(Name, Args),
    maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, Name,
        list.length(Args), MaybeTypes),
    inst_list_is_ground_mt_2(ModuleInfo, MaybeTypes, Args, !Expansions),
    bound_inst_list_is_ground_mt_2(ModuleInfo, MaybeType, BoundInsts,
        !Expansions).

:- pred bound_inst_list_is_ground_or_any(module_info::in,
    list(bound_inst)::in) is semidet.

bound_inst_list_is_ground_or_any(_, []).
bound_inst_list_is_ground_or_any(ModuleInfo, [BoundInst | BoundInsts]) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_ground_or_any(ModuleInfo, Args),
    bound_inst_list_is_ground_or_any(ModuleInfo, BoundInsts).

:- pred bound_inst_list_is_ground_or_any_2(module_info::in,
    list(bound_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_ground_or_any_2(_, [], !Expansions).
bound_inst_list_is_ground_or_any_2(ModuleInfo, [BoundInst | BoundInsts],
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_ground_or_any_2(ModuleInfo, Args, !Expansions),
    bound_inst_list_is_ground_or_any_2(ModuleInfo, BoundInsts, !Expansions).

bound_inst_list_is_unique(_, []).
bound_inst_list_is_unique(ModuleInfo, [BoundInst | BoundInsts]) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_unique(ModuleInfo, Args),
    bound_inst_list_is_unique(ModuleInfo, BoundInsts).

:- pred bound_inst_list_is_unique_2(module_info::in, list(bound_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_unique_2(_, [], !Expansions).
bound_inst_list_is_unique_2(ModuleInfo, [BoundInst | BoundInsts],
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_unique_2(ModuleInfo, Args, !Expansions),
    bound_inst_list_is_unique_2(ModuleInfo, BoundInsts, !Expansions).

bound_inst_list_is_mostly_unique(_, []).
bound_inst_list_is_mostly_unique(ModuleInfo, [BoundInst | BoundInsts]) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_mostly_unique(ModuleInfo, Args),
    bound_inst_list_is_mostly_unique(ModuleInfo, BoundInsts).

:- pred bound_inst_list_is_mostly_unique_2(module_info::in,
    list(bound_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_mostly_unique_2(_, [], !Expansions).
bound_inst_list_is_mostly_unique_2(ModuleInfo, [BoundInst | BoundInsts],
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_mostly_unique_2(ModuleInfo, Args, !Expansions),
    bound_inst_list_is_mostly_unique_2(ModuleInfo, BoundInsts, !Expansions).

bound_inst_list_is_not_partly_unique(_, []).
bound_inst_list_is_not_partly_unique(ModuleInfo, [BoundInst | BoundInsts]) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_partly_unique(ModuleInfo, Args),
    bound_inst_list_is_not_partly_unique(ModuleInfo, BoundInsts).

:- pred bound_inst_list_is_not_partly_unique_2(module_info::in,
    list(bound_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_not_partly_unique_2(_, [], !Expansions).
bound_inst_list_is_not_partly_unique_2(ModuleInfo, [BoundInst | BoundInsts],
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_partly_unique_2(ModuleInfo, Args, !Expansions),
    bound_inst_list_is_not_partly_unique_2(ModuleInfo, BoundInsts,
        !Expansions).

bound_inst_list_is_not_fully_unique(_, []).
bound_inst_list_is_not_fully_unique(ModuleInfo, [BoundInst | BoundInsts]) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_fully_unique(ModuleInfo, Args),
    bound_inst_list_is_not_fully_unique(ModuleInfo, BoundInsts).

:- pred bound_inst_list_is_not_fully_unique_2(module_info::in,
    list(bound_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_not_fully_unique_2(_, [], !Expansions).
bound_inst_list_is_not_fully_unique_2(ModuleInfo, [BoundInst | BoundInsts],
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_fully_unique_2(ModuleInfo, Args, !Expansions),
    bound_inst_list_is_not_fully_unique_2(ModuleInfo, BoundInsts,
        !Expansions).

bound_inst_list_is_free(_, []).
bound_inst_list_is_free(ModuleInfo, [BoundInst | BoundInsts]) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_free(ModuleInfo, Args),
    bound_inst_list_is_free(ModuleInfo, BoundInsts).

%-----------------------------------------------------------------------------%

inst_list_is_ground(_, []).
inst_list_is_ground(ModuleInfo, [Inst | Insts]) :-
    inst_is_ground(ModuleInfo, Inst),
    inst_list_is_ground(ModuleInfo, Insts).

:- pred inst_list_is_ground_mt(module_info::in, list(maybe(mer_type))::in,
    list(mer_inst)::in) is semidet.

inst_list_is_ground_mt(_, [], []).
inst_list_is_ground_mt(ModuleInfo, [MaybeType | MaybeTypes], [Inst | Insts]) :-
    inst_is_ground_mt(ModuleInfo, MaybeType, Inst),
    inst_list_is_ground_mt(ModuleInfo, MaybeTypes, Insts).

:- pred inst_list_is_ground_mt_2(module_info::in,
    list(maybe(mer_type))::in, list(mer_inst)::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

inst_list_is_ground_mt_2(_, [], [], !Expansions).
inst_list_is_ground_mt_2(_, [], [_ | _], !Expansions) :-
    unexpected($pred, "length mismatch").
inst_list_is_ground_mt_2(_, [_ | _], [], !Expansions) :-
    unexpected($pred, "length mismatch").
inst_list_is_ground_mt_2(ModuleInfo, [MaybeType | MaybeTypes], [Inst | Insts],
        !Expansions) :-
    inst_is_ground_mt_1(ModuleInfo, MaybeType, Inst, !Expansions),
    inst_list_is_ground_mt_2(ModuleInfo, MaybeTypes, Insts, !Expansions).

inst_list_is_ground_or_any(_, []).
inst_list_is_ground_or_any(ModuleInfo, [Inst | Insts]) :-
    inst_is_ground_or_any(ModuleInfo, Inst),
    inst_list_is_ground_or_any(ModuleInfo, Insts).

:- pred inst_list_is_ground_or_any_2(module_info::in, list(mer_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_ground_or_any_2(_, [], !Expansions).
inst_list_is_ground_or_any_2(ModuleInfo, [Inst | Insts], !Expansions) :-
    inst_is_ground_or_any_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_ground_or_any_2(ModuleInfo, Insts, !Expansions).

inst_list_is_unique(_, []).
inst_list_is_unique(ModuleInfo, [Inst | Insts]) :-
    inst_is_unique(ModuleInfo, Inst),
    inst_list_is_unique(ModuleInfo, Insts).

:- pred inst_list_is_unique_2(module_info::in, list(mer_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_unique_2(_, [], !Expansions).
inst_list_is_unique_2(ModuleInfo, [Inst | Insts], !Expansions) :-
    inst_is_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_unique_2(ModuleInfo, Insts, !Expansions).

inst_list_is_mostly_unique(_, []).
inst_list_is_mostly_unique(ModuleInfo, [Inst | Insts]) :-
    inst_is_mostly_unique(ModuleInfo, Inst),
    inst_list_is_mostly_unique(ModuleInfo, Insts).

:- pred inst_list_is_mostly_unique_2(module_info::in, list(mer_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_mostly_unique_2(_, [], !Expansions).
inst_list_is_mostly_unique_2(ModuleInfo, [Inst | Insts], !Expansions) :-
    inst_is_mostly_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_mostly_unique_2(ModuleInfo, Insts, !Expansions).

inst_list_is_not_partly_unique(_, []).
inst_list_is_not_partly_unique(ModuleInfo, [Inst | Insts]) :-
    inst_is_not_partly_unique(ModuleInfo, Inst),
    inst_list_is_not_partly_unique(ModuleInfo, Insts).

:- pred inst_list_is_not_partly_unique_2(module_info::in, list(mer_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_not_partly_unique_2(_, [], !Expansions).
inst_list_is_not_partly_unique_2(ModuleInfo, [Inst | Insts], !Expansions) :-
    inst_is_not_partly_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_not_partly_unique_2(ModuleInfo, Insts, !Expansions).

inst_list_is_not_fully_unique(_, []).
inst_list_is_not_fully_unique(ModuleInfo, [Inst | Insts]) :-
    inst_is_not_fully_unique(ModuleInfo, Inst),
    inst_list_is_not_fully_unique(ModuleInfo, Insts).

:- pred inst_list_is_not_fully_unique_2(module_info::in, list(mer_inst)::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_not_fully_unique_2(_, [], !Expansions).
inst_list_is_not_fully_unique_2(ModuleInfo, [Inst | Insts], !Expansions) :-
    inst_is_not_fully_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_not_fully_unique_2(ModuleInfo, Insts, !Expansions).

inst_list_is_free(_, []).
inst_list_is_free(ModuleInfo, [Inst | Insts]) :-
    inst_is_free(ModuleInfo, Inst),
    inst_list_is_free(ModuleInfo, Insts).

inst_list_is_ground_or_dead(_, [], []).
inst_list_is_ground_or_dead(ModuleInfo, [Live | Lives], [Inst | Insts]) :-
    (
        Live = is_live,
        inst_is_ground(ModuleInfo, Inst)
    ;
        Live = is_dead
    ),
    inst_list_is_ground_or_dead(ModuleInfo, Lives, Insts).

inst_list_is_ground_or_any_or_dead(_, [], []).
inst_list_is_ground_or_any_or_dead(ModuleInfo,
        [Live | Lives], [Inst | Insts]) :-
    (
        Live = is_live,
        inst_is_ground_or_any(ModuleInfo, Inst)
    ;
        Live = is_dead
    ),
    inst_list_is_ground_or_any_or_dead(ModuleInfo, Lives, Insts).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_contains_inst_name(ModuleInfo, InstName, Inst) :-
    set.init(Expansions0),
    inst_contains_inst_name_2(ModuleInfo, InstName, Inst, yes,
        Expansions0, _Expansions).

:- type inst_names == set(inst_name).

:- pred inst_contains_inst_name_2(module_info::in, inst_name::in, mer_inst::in,
    bool::out, inst_names::in, inst_names::out) is det.

inst_contains_inst_name_2(ModuleInfo, InstName, Inst, Contains, !Expansions) :-
    (
        ( Inst = abstract_inst(_, _)
        ; Inst = any(_, _)
        ; Inst = free
        ; Inst = free(_)
        ; Inst = ground(_, _)
        ; Inst = inst_var(_)
        ; Inst = not_reached
        ),
        Contains = no
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_contains_inst_name_2(ModuleInfo, InstName, SubInst, Contains,
            !Expansions)
    ;
        Inst = defined_inst(ThisInstName),
        ( if InstName = ThisInstName then
            Contains = yes
        else
            ( if set.insert_new(ThisInstName, !Expansions) then
                inst_lookup(ModuleInfo, ThisInstName, ThisInst),
                set.insert(ThisInstName, !Expansions),
                inst_contains_inst_name_2(ModuleInfo, InstName, ThisInst,
                    Contains, !Expansions)
            else
                Contains = no
            )
        )
    ;
        Inst = bound(_Uniq, InstResults, ArgInsts),
        % XXX This code has a performance problem.
        %
        % The problem is that e.g. in a list of length N, you will have N
        % variables for the skeletons whose insts contain an average of N/2
        % occurences of `bound' each, so the complexity of running
        % inst_contains_inst_name_2 on all their insts is quadratic in N.
        %
        % The inst_test result argument of bound/3 is an attempt at solving
        % this problem.
        %
        % We could also try to solve this performance problem with a cache
        % of the results of recent invocations of inst_contains_inst_name.
        (
            InstResults = inst_test_results_fgtc,
            Contains = no
        ;
            InstResults = inst_test_results(_, _, InstNamesResult, _, _, _),
            (
                InstNamesResult =
                    inst_result_contains_inst_names_known(InstNameSet),
                ( if set.contains(InstNameSet, InstName) then
                    % The Inst may contain InstName, and probably does,
                    % but verify it.
                    bound_inst_list_contains_inst_name(ModuleInfo, InstName,
                        ArgInsts, Contains, !Expansions)
                else
                    Contains = no
                )
            ;
                InstNamesResult = inst_result_contains_inst_names_unknown,
                bound_inst_list_contains_inst_name(ModuleInfo, InstName,
                    ArgInsts, Contains, !Expansions)
            )
        ;
            InstResults = inst_test_no_results,
            bound_inst_list_contains_inst_name(ModuleInfo, InstName,
                ArgInsts, Contains, !Expansions)
        )
    ).

:- pred bound_inst_list_contains_inst_name(module_info::in,
    inst_name::in, list(bound_inst)::in, bool::out,
    inst_names::in, inst_names::out) is det.

bound_inst_list_contains_inst_name(_ModuleInfo, _InstName, [],
        no, !Expansions).
bound_inst_list_contains_inst_name(ModuleInfo, InstName,
        [BoundInst | BoundInsts], Contains, !Expansions) :-
    BoundInst = bound_functor(_Functor, ArgInsts),
    inst_list_contains_inst_name(ModuleInfo, InstName, ArgInsts, Contains1,
        !Expansions),
    (
        Contains1 = yes,
        Contains = yes
    ;
        Contains1 = no,
        bound_inst_list_contains_inst_name(ModuleInfo, InstName, BoundInsts,
            Contains, !Expansions)
    ).

:- pred inst_list_contains_inst_name(module_info::in, inst_name::in,
    list(mer_inst)::in, bool::out, inst_names::in, inst_names::out) is det.

inst_list_contains_inst_name(_ModuleInfo, _InstName, [], no, !Expansions).
inst_list_contains_inst_name(ModuleInfo, InstName, [Inst | Insts], Contains,
        !Expansions) :-
    inst_contains_inst_name_2(ModuleInfo, InstName, Inst, Contains1,
        !Expansions),
    (
        Contains1 = yes,
        Contains = yes
    ;
        Contains1 = no,
        inst_list_contains_inst_name(ModuleInfo, InstName, Insts, Contains,
            !Expansions)
    ).

%-----------------------------------------------------------------------------%

maybe_any_to_bound(ModuleInfo, yes(Type), Uniq, none_or_default_func, Inst) :-
    not type_is_solver_type(ModuleInfo, Type),
    ( if type_constructors(ModuleInfo, Type, Constructors) then
        type_to_ctor_det(Type, TypeCtor),
        constructors_to_bound_any_insts(ModuleInfo, Uniq, TypeCtor,
            Constructors, BoundInsts0),
        list.sort_and_remove_dups(BoundInsts0, BoundInsts),
        % If all the constructors are constant, then Inst will be ground
        % and will not contain any.
        InstResult = inst_test_results(
            inst_result_groundness_unknown,
            inst_result_contains_any_unknown,
            inst_result_contains_inst_names_known(set.init),
            inst_result_contains_inst_vars_known(set.init),
            inst_result_contains_types_known(set.init),
            inst_result_type_ctor_propagated(TypeCtor)
        ),
        Inst = bound(Uniq, InstResult, BoundInsts)
    else if type_may_contain_solver_type(ModuleInfo, Type) then
        % For a type for which constructors are not available (e.g. an
        % abstract type) and which may contain solver types, we fail, meaning
        % that we will use `any' for this type.
        fail
    else
        Inst = ground(Uniq, none_or_default_func)
    ).

:- pred type_may_contain_solver_type(module_info::in, mer_type::in) is semidet.

type_may_contain_solver_type(ModuleInfo, Type) :-
    TypeCtorCat = classify_type(ModuleInfo, Type),
    type_may_contain_solver_type_2(TypeCtorCat) = yes.

:- func type_may_contain_solver_type_2(type_ctor_category) = bool.

type_may_contain_solver_type_2(CtorCat) = MayContainSolverType :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ),
        MayContainSolverType = no
    ;
        ( CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; CtorCat = ctor_cat_user(cat_user_general)
        ),
        MayContainSolverType = yes
    ).

%-----------------------------------------------------------------------------%

inst_contains_any(ModuleInfo, Inst) :-
    set.init(Expansions),
    inst_contains_any_2(ModuleInfo, Inst, Expansions) = yes.

:- func inst_contains_any_2(module_info, mer_inst, set(inst_name)) = bool.

inst_contains_any_2(ModuleInfo, Inst, !.Expansions) = ContainsAny :-
    (
        Inst = any(_, _),
        ContainsAny = yes
    ;
        Inst = bound(_, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            ContainsAny = no
        ;
            InstResults = inst_test_results(_, AnyResults, _, _, _, _),
            (
                AnyResults = inst_result_does_not_contain_any,
                ContainsAny = no
            ;
                AnyResults = inst_result_does_contain_any,
                ContainsAny = yes
            ;
                AnyResults = inst_result_contains_any_unknown,
                ContainsAny = bound_inst_list_contains_any(ModuleInfo,
                    BoundInsts, !.Expansions)
            )
        ;
            InstResults = inst_test_no_results,
            ContainsAny = bound_inst_list_contains_any(ModuleInfo, BoundInsts,
                !.Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = defined_inst(InstName),
        ( if set.member(InstName, !.Expansions) then
            ContainsAny = no
        else
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, SubInst),
            ContainsAny =
                inst_contains_any_2(ModuleInfo, SubInst, !.Expansions)
        )
    ;
        Inst = constrained_inst_vars(_, SubInst),
        ContainsAny = inst_contains_any_2(ModuleInfo, SubInst, !.Expansions)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ; Inst = ground(_, _)
        ; Inst = abstract_inst(_, _)
        ),
        ContainsAny = no
    ).

:- func inst_list_contains_any(module_info, list(mer_inst), set(inst_name))
    = bool.

inst_list_contains_any(_ModuleInfo, [], _Expansions) = no.
inst_list_contains_any(ModuleInfo, [Inst | Insts], Expansions) = ContainsAny :-
    HeadContainsAny = inst_contains_any_2(ModuleInfo, Inst, Expansions),
    (
        HeadContainsAny = yes,
        ContainsAny = yes
    ;
        HeadContainsAny = no,
        ContainsAny = inst_list_contains_any(ModuleInfo, Insts, Expansions)
    ).

:- func bound_inst_list_contains_any(module_info, list(bound_inst),
    set(inst_name)) = bool.

bound_inst_list_contains_any(_ModuleInfo, [], _Expansions) = no.
bound_inst_list_contains_any(ModuleInfo, [BoundInst | BoundInsts],
        Expansions) = ContainsAny :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    HeadContainsAny =
        inst_list_contains_any(ModuleInfo, ArgInsts, Expansions),
    (
        HeadContainsAny = yes,
        ContainsAny = yes
    ;
        HeadContainsAny = no,
        ContainsAny = bound_inst_list_contains_any(ModuleInfo, BoundInsts,
            Expansions)
    ).

%---------------------------------------------------------------------------%

var_inst_contains_any(ModuleInfo, Instmap, Var) :-
    instmap_lookup_var(Instmap, Var, Inst),
    inst_contains_any(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

inst_contains_higher_order(ModuleInfo, Inst) :-
    set.init(Expansions),
    inst_contains_higher_order_2(ModuleInfo, Inst, Expansions) = yes.

:- func inst_contains_higher_order_2(module_info, mer_inst, set(inst_name))
    = bool.

inst_contains_higher_order_2(ModuleInfo, Inst, !.Expansions) = ContainsHO :-
    (
        ( Inst = ground(_, HOInstInfo)
        ; Inst = any(_, HOInstInfo)
        ),
        ContainsHO = ho_inst_info_contains_higher_order(HOInstInfo)
    ;
        Inst = bound(_, _, BoundInsts),
        ContainsHO = bound_inst_list_contains_higher_order(ModuleInfo,
            BoundInsts, !.Expansions)
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = defined_inst(InstName),
        ( if set.member(InstName, !.Expansions) then
            ContainsHO = no
        else
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, SubInst),
            ContainsHO =
                inst_contains_higher_order_2(ModuleInfo, SubInst, !.Expansions)
        )
    ;
        Inst = constrained_inst_vars(_, SubInst),
        ContainsHO =
            inst_contains_higher_order_2(ModuleInfo, SubInst, !.Expansions)
    ;
        Inst = abstract_inst(_, ArgInsts),
        ContainsHO = inst_list_contains_higher_order(ModuleInfo, ArgInsts,
            !.Expansions)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ),
        ContainsHO = no
    ).

:- func inst_list_contains_higher_order(module_info, list(mer_inst),
    set(inst_name)) = bool.

inst_list_contains_higher_order(_ModuleInfo, [], _Expansions) = no.
inst_list_contains_higher_order(ModuleInfo, [Inst | Insts], Expansions) =
        ContainsHO :-
    HeadContainsHO =
        inst_contains_higher_order_2(ModuleInfo, Inst, Expansions),
    (
        HeadContainsHO = yes,
        ContainsHO = yes
    ;
        HeadContainsHO = no,
        ContainsHO =
            inst_list_contains_higher_order(ModuleInfo, Insts, Expansions)
    ).

:- func bound_inst_list_contains_higher_order(module_info, list(bound_inst),
    set(inst_name)) = bool.

bound_inst_list_contains_higher_order(_ModuleInfo, [], _Expansions) = no.
bound_inst_list_contains_higher_order(ModuleInfo, [BoundInst | BoundInsts],
        Expansions) = ContainsHO :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    HeadContainsHO =
        inst_list_contains_higher_order(ModuleInfo, ArgInsts, Expansions),
    (
        HeadContainsHO = yes,
        ContainsHO = yes
    ;
        HeadContainsHO = no,
        ContainsHO = bound_inst_list_contains_higher_order(ModuleInfo,
            BoundInsts, Expansions)
    ).

:- func ho_inst_info_contains_higher_order(ho_inst_info) = bool.

ho_inst_info_contains_higher_order(HOInstInfo) = ContainsHO :-
    (
        HOInstInfo = higher_order(_),
        ContainsHO = yes
    ;
        HOInstInfo = none_or_default_func,
        ContainsHO = no
    ).

%---------------------------------------------------------------------------%

inst_may_restrict_cons_ids(ModuleInfo, Inst) = MayRestrict :-
    (
        ( Inst = any(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = inst_var(_)
        ; Inst = constrained_inst_vars(_, _)    % XXX is this right?
        ; Inst = abstract_inst(_, _)
        ),
        MayRestrict = yes
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ; Inst = ground(_, _)
        ),
        MayRestrict = no
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NewInst),
        MayRestrict = inst_may_restrict_cons_ids(ModuleInfo, NewInst)
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.inst_test.
%-----------------------------------------------------------------------------%
