%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2020-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: direct_arg_in_out.m.
% Main author: zs.
%
% This module addresses a problem that can arise when a procedure fills in
% one or more fields in an argument that was originally passed to it
% in a partially instantiated form.
%
% In the vast majority of cases, such arguments need no special handling.
% The caller passes a tagged pointer to a partially-filled-in heap cell,
% and the callee simply fills in the parts of the heap cell corresponding
% to the field or fields that it instantiates. When the callee returns,
% the caller will find those fields filled in.
%
% The problem arises when the function symbol whose field is being filled in
% has the direct_arg representation. This representation is applicable
% only to function symbols with a single argument, and only when the argument
% type's representation guarantees that the argument's value will contain
% all zeroes in its primary tag bits. In such cases, the compiler
% represents the function with a direct_arg_tag(N) cons tag, which means that
% the representation of this function symbol, applied to its single argument,
% will be the value of the argument, with the guaranteed-to-be-zero bits
% in the argument value replaced by N. (This primary tag value may, or may not,
% be needed to distinguish this function symbol from any other function symbols
% in the whole term's type.)
%
% The problem that this module handles is that when a callee fills in
% the argument value of such a term, this update affects only the callee's
% own local variables. It does *not* affect any heap cells, nor anything
% else that the caller can see. Without compensation for this effect,
% the translated program will contain a bug. (See test cases gh72[ab...].m
% in tests/hard_coded.) This module is the needed compensation.
%
% Since the problem is that instantiations of such partial terms
% are not visible in the caller, the solution is to *make* them visible
% in the caller.
%
% One possible way to do that would be to modify the LLDS and MLDS code
% generators to pass such arguments not by value, but by value-result.
% (In Ada terms, this would mean treating them not as "in" arguments,
% but as "in-out" arguments.) The other way to do it is via a program
% transformation that replaces each such argument with a pair of arguments,
% one input, and one output, arranging that after each call, all uses
% of the passed input variable are replaced by the corresponding output
% variable.
%
% This module solves the problem via the transformation approach.
% I (zs) have several reasons for preferring this approach.
%
% The first reason is the obvious one: the code generator approach requires
% modifying two backends, while one transformation works for all backends.
%
% The second reason is separation of concerns. The code in this module
% implementing the transformation has to worry *only* about solving this
% problem, while respecting the HLDS invariants, of course. Any modifications
% to the code generators would have to worry about interactions between
% this concern and all the other things that code generators have to
% manage (such as stack slot allocation in the LLDS backend), while also
% respecting the usual invariants of the code generators. I worry that
% unanticipated interactions with the existing concerns would be
% a lingering source of bugs in the future.
%
% The third and fourth reasons are two other aspects of reliability.
% The output of this transformation is subject to some automatic checks
% when we recalculate the instmap deltas in the transformed procedure bodies,
% and a human can see the effect of the transformation by comparing HLDS dumps
% from before and after. The output of a modified code generator can have
% no automatic checks short of running the resulting executable on test inputs,
% and even visual checking of the generated code would be much harder.
%
% The fifth reason is performance. I expect that the vast majority
% of compiler invocations will have no partially instantiated arguments
% involving direct args. That means that this transformation wouldn't
% even be invoked, and the only cost of this approach would consist of
% the checks that say "no, we don't need this transformation". I am sure
% that the performance cost of any changes to the code generators
% would have to be paid on every compilation.
%
%---------------------------------------------------------------------------%
%
% The first part of the transformation is to have the compiler find
% all arguments in all procedures that are subject to this problem.
% This is true for an argument variable V with initial inst I and
% final inst F if
%
% - I is not ground,
% - F further instantiates one of the arguments of one of the function symbols
%   in V's type, and
% - at least one of those function symbols is represented by a direct arg tag.
%
% We call such arguments "daio arguments" (direct arg in out), and
% we call procedures that have any daio arguments "daio procedures".
%
% The second part is to modify the argument lists of daio procedures
% to replace each daio argument V with a pair or arguments, V and V',
% where V' is a clone of V. In the updated argument list, we change
% the mode of V from I -> F to I -> clobbered, and we give the clone
% variable V' the mode free -> F. The idea is that at the end of the
% procedure body, we will assign V to V', and that the caller will pick up
% the updated value of V from the argument position of V', since this will be
% an output argument. Due to the change in signature, we do this modification
% in a clone of the original procedure, leaving the original unchanged.
%
% The third part of the solution is to consistently modify all procedure
% bodies to implement that idea. When we find a call to a daio procedure,
% we create clones of all its daio variables, update the argument vector
% to pass the clones of the daio arguments as well, redirect the call
% to the daio clone of the callee procedure, and ensure that we never again
% refer to the original, pre-clone version of each such daio variable.
% In straight-line code that follows such calls, we can achieve this
% by simple substitution, but we also have to handle situations in which
% a branched control structure (if-then-else, disjunction, switch or
% atomic goal) may need to clone different sets of daio variables in its
% different branches. We ensure that the code following the branched
% control structure gets a consistent view of every daio variable by,
% for each daio variable that is updated in any branch (call it X0),
% picking the variable representing the final version in one branch (say X5),
% and copying the final version of that original variable in every other branch
% (say X1, X2 etc, or X0 itself) to this same variable (X5). We also ensure
% that the original version (X0 in this example) is clobbered in every branch,
% even the ones that do not mention it at all, since this is required to ensure
% that the different branches have consistent instmap_deltas.
%
% Note that this whole transformation is needed only if the set of daio
% procedures is not empty, but, for the vast majority of modules being
% compiled, this set *will* be empty. mercury_compile_middle_passes.m,
% the one module that can invoke do_direct_arg_in_out_transform_in_module,
% does so only if the set is non-empty. To minimize the cost of computing
% the set of daio procedures, we do not have a separate pass for it.
% Instead, we compute it in two parts. For almost all procedures, we
% test whether they are daio procedures in simplify_proc.m, as part of
% the tasks we do at the end of the compiler front end. However, the
% procedures that implement lambda expressions do not yet exist as separate
% procedures at that time, so we get lambda.m to do the same test
% (using the same predicate, find_and_record_any_direct_arg_in_out_posns)
% on the procedures it creates.
%
% This optimization is why mercury_compile_middle_passes.m invokes
% this module after the lambda expansion transformation.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.direct_arg_in_out.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.

%---------------------%

    % find_and_record_any_direct_arg_in_out_posns(PredId, ProcId, VarTypes,
    %     Vars, Modes, !ModuleInfo):
    % 
    % Given a procedure proc(PredId, ProcId) whose arguments Vars have
    % the types recorded in VarTypes and the modes recorded in Modes,
    % find out whether any of their arguments are daio arguments.
    % If yes, then update the module_info to record that this procedure
    % is a daio procedure with the computed set of daio arguments.
    % This record will be used by do_direct_arg_in_out_transform_in_module
    % to perform the transformation described at the top of this module.
    %
    % This predicate is intended to be called from simplify_proc.m and from
    % lambda.m, as also described above.
    %
:- pred find_and_record_any_direct_arg_in_out_posns(pred_id::in, proc_id::in,
    var_table::in, list(prog_var)::in, list(mer_mode)::in,
    module_info::in, module_info::out) is det.

%---------------------%

    % Implement the transformation described at the top-of-module module
    % comment above.
    %
:- pred do_direct_arg_in_out_transform_in_module(direct_arg_proc_map::in,
    module_info::in, module_info::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.

:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

find_and_record_any_direct_arg_in_out_posns(PredId, ProcId, VarTypes,
        Vars, Modes, !ModuleInfo) :-
    find_direct_arg_in_out_posns(!.ModuleInfo, VarTypes, 1, Vars, Modes,
        DirectArgInOutPosns, ProblemPosns),
    (
        ProblemPosns = [],
        (
            DirectArgInOutPosns = []
        ;
            DirectArgInOutPosns = [HeadPosn | TailPosns],
            OoMDirectArgInOutPosns = one_or_more(HeadPosn, TailPosns),
            module_info_get_direct_arg_proc_map(!.ModuleInfo,
                DirectArgProcMap0),
            DirectArgProc = direct_arg_clone_proc(OoMDirectArgInOutPosns),
            map.det_insert(proc(PredId, ProcId), DirectArgProc,
                DirectArgProcMap0, DirectArgProcMap),
            module_info_set_direct_arg_proc_map(DirectArgProcMap, !ModuleInfo)
        )
    ;
        ProblemPosns = [HeadPosn | TailPosns],
        OoMDirectArgPosns = one_or_more(HeadPosn, TailPosns),
        module_info_get_direct_arg_proc_map(!.ModuleInfo, DirectArgProcMap0),
        DirectArgProc = direct_arg_problem_proc(OoMDirectArgPosns,
            DirectArgInOutPosns),
        map.det_insert(proc(PredId, ProcId), DirectArgProc,
            DirectArgProcMap0, DirectArgProcMap),
        module_info_set_direct_arg_proc_map(DirectArgProcMap, !ModuleInfo)
    ).

    % Given a procedure's headvars and their modes, return
    %
    % - the list of argument positions that need to be cloned, because
    %   their type says that some of their function symbols use the direct arg
    %   tag, and their mode says that this procedure fills in the argument(s)
    %   of one or more of those function symbols, and
    %
    % - the list of problem argument positions, whose modes do not contain the
    %   information we need to decide whether or not they need to be cloned.
    %
:- pred find_direct_arg_in_out_posns(module_info::in, var_table::in,
    int::in, list(prog_var)::in, list(mer_mode)::in,
    list(int)::out, list(int)::out) is det.

find_direct_arg_in_out_posns(_, _, _, [], [], [], []).
find_direct_arg_in_out_posns(_, _, _, [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
find_direct_arg_in_out_posns(_, _, _, [_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
find_direct_arg_in_out_posns(ModuleInfo, VarTypes, CurArgNum,
        [Var | Vars], [Mode | Modes], DirectArgInOutPosns, ProblemPosns) :-
    find_direct_arg_in_out_posns(ModuleInfo, VarTypes, CurArgNum + 1,
        Vars, Modes, TailDirectArgInOutPosns, TailProblemPosns),
    is_direct_arg_in_out_posn(ModuleInfo, VarTypes, Var, Mode, IsDAIO),
    (
        IsDAIO = mode_is_not_daio,
        DirectArgInOutPosns = TailDirectArgInOutPosns,
        ProblemPosns = TailProblemPosns
    ;
        IsDAIO = mode_is_daio,
        DirectArgInOutPosns = [CurArgNum | TailDirectArgInOutPosns],
        ProblemPosns = TailProblemPosns
    ;
        IsDAIO = mode_may_be_daio,
        DirectArgInOutPosns = TailDirectArgInOutPosns,
        ProblemPosns = [CurArgNum | TailProblemPosns]
    ).

:- pred is_direct_arg_in_out_posn(module_info::in, var_table::in,
    prog_var::in, mer_mode::in, is_mode_daio::out) is det.

is_direct_arg_in_out_posn(ModuleInfo, VarTypes, Var, Mode, IsDAIO) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_var_type(VarTypes, Var, Type),
    ( if
        type_to_ctor(Type, TypeCtor),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn)
    then
        get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(_, _, _, MaybeRepn, _),
            (
                MaybeRepn = no,
                unexpected($pred, "MaybeRepn = no")
            ;
                MaybeRepn = yes(Repn)
            ),
            CtorRepns = Repn ^ dur_ctor_repns,
            gather_direct_arg_functors(CtorRepns, [], DirectArgFunctors),
            (
                DirectArgFunctors = [],
                IsDAIO = mode_is_not_daio
            ;
                DirectArgFunctors = [_ | _],
                FromToInsts = mode_to_from_to_insts(ModuleInfo, Mode),
                FromToInsts = from_to_insts(FromInst0, ToInst0),
                inst_expand_and_remove_constrained_inst_vars(ModuleInfo,
                    FromInst0, FromInst),
                inst_expand_and_remove_constrained_inst_vars(ModuleInfo,
                    ToInst0, ToInst),
                IsDAIO = mode_needs_direct_arg_in_out(ModuleInfo,
                    DirectArgFunctors, FromInst, ToInst)
            )
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            ),
            % Equivalence types should have been expanded out by now.
            %
            % Function symbols of foreign types cannot have direct_arg tags.
            %
            % Deconstruction does not work on solver types themselves;
            % it works only on their representation types.
            %
            % Values of abstract types cannot be deconstructed because
            % their function symbols are invisible, being on the other side
            % of an abstraction barrier.
            IsDAIO = mode_is_not_daio
        )
    else
        % The call to type_to_ctor can fail only if Type is a type variable.
        % Since this procedure does not know Var's type, it cannot have
        % a type-specific mode for it.
        IsDAIO = mode_is_not_daio
    ).

:- pred gather_direct_arg_functors(list(constructor_repn)::in,
    list(sym_name)::in, list(sym_name)::out) is det.

gather_direct_arg_functors([], !DirectArgFunctors).
gather_direct_arg_functors([CtorRepn | CtorRepns], !DirectArgFunctors) :-
    ( if CtorRepn ^ cr_tag = direct_arg_tag(_) then
        list.length(CtorRepn ^ cr_args, Arity),
        % Direct arg cons ids must have arity 1.
        expect(unify(Arity, 1), $pred, "direct arg functor arity != 1"),
        DirectArgFunctor = CtorRepn ^ cr_name,
        !:DirectArgFunctors = [DirectArgFunctor | !.DirectArgFunctors]
    else
        true
    ),
    gather_direct_arg_functors(CtorRepns, !DirectArgFunctors).

:- type is_mode_daio
    --->    mode_is_not_daio
    ;       mode_is_daio
    ;       mode_may_be_daio.

:- func mode_needs_direct_arg_in_out(module_info, list(sym_name),
    mer_inst, mer_inst) = is_mode_daio.

mode_needs_direct_arg_in_out(ModuleInfo, DirectArgFunctors, FromInst, ToInst)
        = IsDAIO :-
    (
        ( FromInst = free
        ; FromInst = free(_)
        ; FromInst = any(_, _)
        ; FromInst = not_reached
        ; FromInst = ground(_, _)
        ; FromInst = inst_var(_)
        ; FromInst = abstract_inst(_, _)
        ),
        IsDAIO = mode_is_not_daio
    ;
        FromInst = bound(_FromUniq, _FromResults, FromBoundInsts),
        some_bound_inst_has_direct_arg_free(ModuleInfo, DirectArgFunctors,
            FromBoundInsts, FreeArgDirectArgFunctors),
        (
            FreeArgDirectArgFunctors = [],
            IsDAIO = mode_is_not_daio
        ;
            FreeArgDirectArgFunctors = [_ | _],
            (
                ( ToInst = free
                ; ToInst = free(_)
                ),
                % ToInst cannot be less instantiated than FromInst.
                unexpected($pred, "bound to free")
            ;
                ToInst = abstract_inst(_, _),
                % XXX In this extremely rare case, we have no idea whether
                % the actual inst that this abstract inst stands for
                % requires its argument to be cloned or not. We *could*
                % clone all such arguments, but we have seen no need for it
                % just yet.
                % XXX I (zs) do not even know whether the compiler
                % permits any procedure whose mode includes an abstract inst
                % to pass semantic analysis, though as far as I can tell,
                % the reference manual does not prohibit this. (Though it
                % also does not explicitly say that it is permitted.)
                % 
                % XXX We could return messages as error_specs,
                % instead of as abort messages.
                unexpected($pred,
                    "NYI: abstract inst in predicate mode in a module " ++
                    "that uses partially-instantiated direct_arg terms")
            ;
                ToInst = inst_var(_),
                % XXX Another instance of the problem described for
                % abstract_inst.
                unexpected($pred,
                    "NYI: inst var in predicate mode in a module " ++
                    "that uses partially-instantiated direct_arg terms")
            ;
                ( ToInst = any(_, _)
                ; ToInst = not_reached
                ),
                IsDAIO = mode_is_not_daio
            ;
                ToInst = ground(_, _),
                IsDAIO = mode_is_daio
            ;
                ToInst = bound(_ToUniq, _ToResults, ToBoundInsts),
                some_bound_inst_has_direct_arg_out(ModuleInfo,
                    FreeArgDirectArgFunctors, ToBoundInsts,
                    SomeDirectArgIsBound, CanSeeAllArgModes),
                (
                    CanSeeAllArgModes = cannot_see_all_arg_modes,
                    IsDAIO = mode_may_be_daio
                ;
                    CanSeeAllArgModes = can_see_all_arg_modes,
                    (
                        SomeDirectArgIsBound = no_direct_arg_is_bound,
                        IsDAIO = mode_is_not_daio
                    ;
                        SomeDirectArgIsBound = some_direct_arg_is_bound,
                        IsDAIO = mode_is_daio
                    )
                )
            ;
                ToInst = constrained_inst_vars(_, _),
                unexpected($pred, "unexpanded constrained_inst_vars")
            ;
                ToInst = defined_inst(_),
                unexpected($pred, "unexpanded defined_inst")
            )
        )
    ;
        FromInst = constrained_inst_vars(_, _),
        unexpected($pred, "unexpanded constrained_inst_vars")
    ;
        FromInst = defined_inst(_),
        unexpected($pred, "unexpanded defined_inst")
    ).

:- pred some_bound_inst_has_direct_arg_free(module_info::in,
    list(sym_name)::in, list(bound_inst)::in, list(sym_name)::out) is det.

some_bound_inst_has_direct_arg_free(_, _, [], []).
some_bound_inst_has_direct_arg_free(ModuleInfo, DirectArgFunctors,
        [FromBoundInst | FromBoundInsts], !:FreeArgDirectArgFunctors) :-
    some_bound_inst_has_direct_arg_free(ModuleInfo, DirectArgFunctors,
        FromBoundInsts, !:FreeArgDirectArgFunctors),
    FromBoundInst = bound_functor(ConsId, ArgInsts0),
    ( if
        ConsId = cons(SymName, Arity, _TypeCtor),
        Arity = 1,
        list.member(SymName, DirectArgFunctors),
        ArgInsts0 = [ArgInst0],
        inst_expand_and_remove_constrained_inst_vars(ModuleInfo,
            ArgInst0, ArgInst),
        ( ArgInst = free
        ; ArgInst = free(_)
        )
    then
        !:FreeArgDirectArgFunctors = [SymName | !.FreeArgDirectArgFunctors]
    else
        true
    ).

:- type is_some_direct_arg_bound
    --->    no_direct_arg_is_bound
    ;       some_direct_arg_is_bound.

:- type can_see_all_arg_modes
    --->    cannot_see_all_arg_modes
    ;       can_see_all_arg_modes.

:- pred some_bound_inst_has_direct_arg_out(module_info::in,
    list(sym_name)::in, list(bound_inst)::in,
    is_some_direct_arg_bound::out, can_see_all_arg_modes::out) is det.

some_bound_inst_has_direct_arg_out(_, _, [],
        no_direct_arg_is_bound, can_see_all_arg_modes).
some_bound_inst_has_direct_arg_out(ModuleInfo, FreeArgDirectArgFunctors,
        [ToBoundInst | ToBoundInsts],
        SomeDirectArgIsBound, CanSeeAllArgModes) :-
    some_bound_inst_has_direct_arg_out(ModuleInfo, FreeArgDirectArgFunctors,
        ToBoundInsts, TailSomeDirectArgIsBound, TailCanSeeAllArgModes),
    ToBoundInst = bound_functor(ConsId, ArgInsts0),
    ( if
        ConsId = cons(SymName, Arity, _TypeCtor),
        Arity = 1,
        list.member(SymName, FreeArgDirectArgFunctors)
    then
        (
            ArgInsts0 = [ArgInst0]
        ;
            ( ArgInsts0 = []
            ; ArgInsts0 = [_, _ | _]
            ),
            unexpected($pred, "Arity = 1 but ArgInsts0 != [_]")
        ),
        inst_expand_and_remove_constrained_inst_vars(ModuleInfo,
            ArgInst0, ArgInst),
        (
            ( ArgInst = free
            ; ArgInst = free(_)
            ),
            SomeDirectArgIsBound = TailSomeDirectArgIsBound,
            CanSeeAllArgModes = TailCanSeeAllArgModes
        ;
            ArgInst = not_reached,
            unexpected($pred, "not_reached arg in reachable term")
        ;
            ( ArgInst = any(_, _)
            ; ArgInst = ground(_, _)
            ; ArgInst = bound(_, _, _)
            ),
            % The arg of ConsId was free in FromInst, but it is NOT free
            % in ToInst.
            SomeDirectArgIsBound = some_direct_arg_is_bound,
            CanSeeAllArgModes = TailCanSeeAllArgModes
        ;
            ( ArgInst = inst_var(_)
            ; ArgInst = abstract_inst(_, _)
            ),
            SomeDirectArgIsBound = TailSomeDirectArgIsBound,
            CanSeeAllArgModes = cannot_see_all_arg_modes
        ;
            ( ArgInst = constrained_inst_vars(_, _)
            ; ArgInst = defined_inst(_)
            ),
            unexpected($pred, "unexpanded inst")
        )
    else
        SomeDirectArgIsBound = TailSomeDirectArgIsBound,
        CanSeeAllArgModes = TailCanSeeAllArgModes
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type direct_arg_proc_in_out
    --->    direct_arg_proc_in_out(
                pred_proc_id,
                one_or_more(int)
            ).

:- type direct_arg_proc_in_out_map ==
    map(pred_proc_id, direct_arg_proc_in_out).

:- type clone_in_out_map == map(pred_proc_id, one_or_more(int)).

%---------------------------------------------------------------------------%

do_direct_arg_in_out_transform_in_module(DirectArgProcMap,
        !ModuleInfo, !:Specs) :-
    !:Specs = [],
    % Phase zero: generate an error message for every foreign_export pragma
    % for every daio procedure, and disregard such pragmas. Logically, this
    % belongs in a later phase, but it is simplest to do this task before
    % we delete the originals of cloned procedures.
    module_info_get_pragma_exported_procs(!.ModuleInfo, ExportedProcsCord0),
    ExportedProcs0 = cord.list(ExportedProcsCord0),
    list.foldl2(
        generate_error_if_cloned_proc_is_exported(!.ModuleInfo,
            DirectArgProcMap),
        ExportedProcs0, [], RevExportedProcs, !Specs),
    list.reverse(RevExportedProcs, ExportedProcs),
    ExportedProcsCord = cord.from_list(ExportedProcs),
    module_info_set_pragma_exported_procs(ExportedProcsCord, !ModuleInfo),

    % Phase one: for every daio procedure, create a clone procedure
    % that includes a clone for every daio argument variable.
    % Then delete the original procedure, to ensure that later passes
    % detect any references to them that were accidentally left by phase two.
    map.foldl4(make_direct_arg_clone_or_spec, DirectArgProcMap,
        map.init, DirectArgProcInOutMap, map.init, CloneInOutMap,
        !ModuleInfo, !Specs),

    % Phase two: Transform the bodies of all procedures in the module to refer
    % to the clones, and not the originals, ensuring that every access to
    % a daio variable is updated to refer to its most recent clone.
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl2(
        transform_direct_arg_in_out_calls_in_pred(DirectArgProcMap,
            DirectArgProcInOutMap, CloneInOutMap),
        PredIds, !ModuleInfo, !Specs),

    % Phase three: replace all references to the now-deleted procedures
    % in the class and instance tables with references to their clones.
    module_info_get_class_table(!.ModuleInfo, ClassTable0),
    map.map_values_only(transform_class(DirectArgProcInOutMap),
        ClassTable0, ClassTable),
    module_info_set_class_table(ClassTable, !ModuleInfo),

    module_info_get_instance_table(!.ModuleInfo, InstanceTable0),
    map.map_values_only(transform_class_instances(DirectArgProcInOutMap),
        InstanceTable0, InstanceTable),
    module_info_set_instance_table(InstanceTable, !ModuleInfo).

:- pred make_direct_arg_clone_or_spec(pred_proc_id::in, direct_arg_proc::in,
    direct_arg_proc_in_out_map::in, direct_arg_proc_in_out_map::out,
    clone_in_out_map::in, clone_in_out_map::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

make_direct_arg_clone_or_spec(PredProcId, DirectArgProc,
        !DirectArgInOutMap, !CloneInOutMap, !ModuleInfo, !Specs) :-
    (
        DirectArgProc = direct_arg_clone_proc(OoMInOutArgs),
        make_direct_arg_in_out_clone(PredProcId, OoMInOutArgs, ProcInOut,
            !CloneInOutMap, !ModuleInfo),
        map.det_insert(PredProcId, ProcInOut, !DirectArgInOutMap)
    ;
        DirectArgProc = direct_arg_problem_proc(OoMProblemArgs, _InOutArgs),
        generate_problem_proc_error(!.ModuleInfo, PredProcId, OoMProblemArgs,
            Spec),
        !:Specs = [Spec | !.Specs]
    ).

:- pred make_direct_arg_in_out_clone(pred_proc_id::in, one_or_more(int)::in,
    direct_arg_proc_in_out::out, clone_in_out_map::in, clone_in_out_map::out,
    module_info::in, module_info::out) is det.

make_direct_arg_in_out_clone(PredProcId, OoMInOutArgs, ProcInOut,
        !CloneInOutMap, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no
    ;
        VeryVerbose = yes,
        trace [io(!IO)] (
            write_proc_progress_message(!.ModuleInfo,
                "Cloning", PredProcId, !IO)
        )
    ),

    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    % We want the clone procedure to replace the original in all respects.
    % We give it the same name and same proc_id as original, which will
    % lead to the same name in the generated target-language code.
    % We delete the originals of cloned procs to ensure that any references
    % that we accidentally leave around that still refer to them after our
    % transformation will be detected.
    map.det_remove(ProcId, ProcInfo, ProcTable0, ProcTable),
    ( if map.is_empty(ProcTable) then
        % If there are no procedures left in the predicate,
        % delete the predicate as well.
        module_info_remove_predicate(PredId, !ModuleInfo)
    else
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ),

    proc_prepare_to_clone(ProcInfo, HeadVars, Goal, VarTable,
        RttiVarMaps, InstVarSet, DeclaredModes, Modes, _MaybeArgLives,
        MaybeDeclaredDetism, Detism, EvalMethod, _ModeErrors,
        MainContext, ItemNumber, CanProcess, _MaybeHeadModesConstr,
        _DetismDecl, _CseNopullContexts, MaybeUntupleInfo, VarNameRemap,
        _StateVarWarnings, DeletedCallees, IsAddressTaken,
        HasForeignProcExports, HasParallelConj, HasUserEvent, HasTailCallEvent,
        OisuKinds, MaybeRequireTailRecursion, RegR_HeadVars, MaybeArgPassInfo,
        MaybeSpecialReturn, InitialLiveness, StackSlots, NeedsMaxfrSlot,
        MaybeCallTableTip, MaybeTableIOInfo, MaybeTableAttrs,
        MaybeObsoleteInFavourOf, MaybeDeepProfProcInfo, MaybeArgSizes,
        MaybeTermInfo, Term2Info, MaybeExceptionInfo, MaybeTrailingInfo,
        MaybeMMTablingInfo, SharingInfo, ReuseInfo),
    pred_prepare_to_clone(PredInfo0, ModuleName, PredName, OrigArity,
        PredOrFunc, Origin, Status, Markers, ArgTypes,
        DeclTypeVarSet, TypeVarSet, ExistQVars, PolyAddedArgs, ClassContext,
        ClausesInfo, _ProcTable, Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap, UnprovenBodyConstraints,
        InstGraphInfo, ArgModesMaps, PredVarNameRemap, Assertions,
        ObsoleteInFavourOf, FormatCall, InstanceMethodArgTypes),
    OoMInOutArgs = one_or_more(HeadArgPos, TailArgPosns),
    clone_daio_pred_proc_args(!.ModuleInfo, 1, HeadArgPos, TailArgPosns,
        ArgTypes, HeadVars, Modes, CloneArgTypes, CloneHeadVars, CloneModes,
        VarTable, CloneVarTable),
    (
        DeclaredModes = maybe.no,
        CloneDeclaredModes = maybe.no
    ;
        DeclaredModes = maybe.yes(_),
        CloneDeclaredModes = maybe.yes(CloneModes)
    ),
    CloneMaybeArgLives = maybe.no,          % Rebuilt on demand from modes.
    CloneModeErrors = [],                   % All users of this field have run.
    CloneMaybeHeadModesConstr = maybe.no,   % This field has no current users.
    CloneDetismDecl = detism_decl_none,
    CloneCseNopullContexts = [],            % All users of this field have run.
    CloneStateVarWarnings = [],             % All users of this field have run.
    proc_create(CloneHeadVars, Goal, CloneVarTable,
        RttiVarMaps, InstVarSet, CloneDeclaredModes, CloneModes,
        CloneMaybeArgLives, MaybeDeclaredDetism, Detism, EvalMethod,
        CloneModeErrors, MainContext, ItemNumber, CanProcess,
        CloneMaybeHeadModesConstr, CloneDetismDecl, CloneCseNopullContexts,
        MaybeUntupleInfo, VarNameRemap, CloneStateVarWarnings,
        DeletedCallees, IsAddressTaken, HasForeignProcExports, HasParallelConj,
        HasUserEvent, HasTailCallEvent, OisuKinds, MaybeRequireTailRecursion,
        RegR_HeadVars, MaybeArgPassInfo, MaybeSpecialReturn, InitialLiveness,
        StackSlots, NeedsMaxfrSlot, MaybeCallTableTip, MaybeTableIOInfo,
        MaybeTableAttrs, MaybeObsoleteInFavourOf, MaybeDeepProfProcInfo,
        MaybeArgSizes, MaybeTermInfo, Term2Info, MaybeExceptionInfo,
        MaybeTrailingInfo, MaybeMMTablingInfo, SharingInfo, ReuseInfo,
        CloneProcInfo),

    ClonePredName = string.format("direct_arg_in_out_%d_%s",
        [i(proc_id_to_int(ProcId)), s(PredName)]),
    CloneOrigin = origin_proc_transform(proc_transform_direct_arg_in_out,
        Origin, PredId, ProcId),
    CloneProcTable = map.singleton(ProcId, CloneProcInfo),
    pred_create(ModuleName, ClonePredName, OrigArity, PredOrFunc,
        CloneOrigin, Status, Markers, CloneArgTypes,
        DeclTypeVarSet, TypeVarSet, ExistQVars, PolyAddedArgs, ClassContext,
        ClausesInfo, CloneProcTable, Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap, UnprovenBodyConstraints,
        InstGraphInfo, ArgModesMaps, PredVarNameRemap, Assertions,
        ObsoleteInFavourOf, FormatCall, InstanceMethodArgTypes, ClonePredInfo),

    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_insert(ClonePredInfo, ClonePredId,
        PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo),

    ClonePredProcId = proc(ClonePredId, ProcId),
    ProcInOut = direct_arg_proc_in_out(ClonePredProcId, OoMInOutArgs),
    map.det_insert(ClonePredProcId, OoMInOutArgs, !CloneInOutMap),

    trace [compile_time(flag("daio-debug")), io(!IO)] (
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),
        io.format(Stream, "duplicated proc(%d, %d) %s -> %s:\n\t",
            [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId)),
            s(PredName), s(ClonePredName)], !IO),
        io.write_line(Stream, ProcInOut, !IO),
        io.write_string(Stream, "old args: ", !IO),
        io.write_line(Stream, HeadVars, !IO),
        io.write_string(Stream, "new args: ", !IO),
        io.write_line(Stream, CloneHeadVars, !IO)
    ).

:- pred clone_daio_pred_proc_args(module_info::in, int::in,
    int::in, list(int)::in,
    list(mer_type)::in, list(prog_var)::in, list(mer_mode)::in,
    list(mer_type)::out, list(prog_var)::out, list(mer_mode)::out,
    var_table::in, var_table::out) is det.

clone_daio_pred_proc_args(ModuleInfo, CurArgNum, HeadArgPosn, TailArgPosns,
        Types, Vars, Modes, CloneTypes, CloneVars, CloneModes, !VarTypes) :-
    ( if
        Types = [HeadType | TailTypes],
        Vars = [HeadVar | TailVars],
        Modes = [HeadMode | TailModes]
    then
        ( if HeadArgPosn = CurArgNum then
            % The NewVar we add to the list of headvars is only a placeholder.
            % It will never be used, and after we have transformed the
            % procedure body, replace_cloned_headvars will replace it
            % with the variable that holds the final version of HeadVar
            % in the body. The reason why we nevertheless include NewVar 
            % in the argument list is that
            %
            % (a) we have to add the type and mode of the cloned variable
            %     to the representation of the cloned procedure, and
            % (b) many utility routines that operate on that representation
            %     insist, quite rightly, on the number of headvars
            %     matching the number of argument types and modes.
            lookup_var_entry(!.VarTypes, HeadVar, HeadVarEntry),
            HeadVarEntry = vte(HeadVarName, _HeadVarType, _HeadVarIsDummy),
            NewVarName = maybe_add_headvar_clone_suffix(HeadVarName),
            % Getting the type of NewVar from HeadVar's type in the argument
            % list rather than from VarTable preserves old behavior.
            NewVarIsDummy = is_type_a_dummy(ModuleInfo, HeadType),
            NewVarEntry = vte(NewVarName, HeadType, NewVarIsDummy),
            add_var_entry(NewVarEntry, NewVar, !VarTypes),
            daio_mode_to_mode_pair(ModuleInfo, HeadMode,
                ClobberedHeadMode, CloneMode),
            (
                TailArgPosns = [],
                CloneTypes = [HeadType, HeadType | TailTypes],
                CloneVars = [HeadVar, NewVar | TailVars],
                CloneModes = [ClobberedHeadMode, CloneMode | TailModes]
            ;
                TailArgPosns = [HeadTailArgPosn | TailTailArgPosns],
                clone_daio_pred_proc_args(ModuleInfo, CurArgNum + 1,
                    HeadTailArgPosn, TailTailArgPosns,
                    TailTypes, TailVars, TailModes,
                    TailCloneTypes, TailCloneVars, TailCloneModes, !VarTypes),
                CloneTypes = [HeadType, HeadType | TailCloneTypes],
                CloneVars = [HeadVar, NewVar | TailCloneVars],
                CloneModes = [ClobberedHeadMode, CloneMode | TailCloneModes]
            )
        else
            clone_daio_pred_proc_args(ModuleInfo, CurArgNum + 1,
                HeadArgPosn, TailArgPosns, TailTypes, TailVars, TailModes,
                TailCloneTypes, TailCloneVars, TailCloneModes, !VarTypes),
            CloneTypes = [HeadType | TailCloneTypes],
            CloneVars = [HeadVar | TailCloneVars],
            CloneModes = [HeadMode | TailCloneModes]
        )
    else
        unexpected($pred, "ran out of arguments")
    ).

:- pred daio_mode_to_mode_pair(module_info::in, mer_mode::in,
    mer_mode::out, mer_mode::out) is det.

daio_mode_to_mode_pair(ModuleInfo, Mode, ClobberedMode, CloneMode) :-
    mode_get_insts(ModuleInfo, Mode, FromInst, ToInst),
    ClobberedFromInst = clobber_daio_inst(ModuleInfo, FromInst),
    ClobberedMode = from_to_mode(FromInst, ClobberedFromInst),
    CloneMode = from_to_mode(free, ToInst).

:- func clobber_daio_inst(module_info, mer_inst) = mer_inst.

clobber_daio_inst(ModuleInfo, Inst0) = ClobberedInst :-
    inst_expand(ModuleInfo, Inst0, Inst),
    (
        Inst = bound(_Uniq, TestResults, BoundInsts),
        (
            TestResults = inst_test_results(_GroundNess, _ContainsAny,
                _ContainsInstNames, _ContainsInstVars, _ContainsTypes,
                _TypeCtorPropagated),
            % None of the above six categories can be affected by
            % applying clobber_daio_bound_inst to BoundInsts.
            % This goal, and the switch around it, is here in case
            % in the future we add a test that *can* be affected.
            ClobberedTestResults = TestResults
        ;
            TestResults = inst_test_no_results,
            ClobberedTestResults = TestResults
        ;
            TestResults = inst_test_results_fgtc,
            ClobberedTestResults = TestResults
        ),
        ClobberedBoundInsts =
            list.map(clobber_daio_bound_inst(ModuleInfo), BoundInsts),
        ClobberedInst = bound(clobbered, ClobberedTestResults,
            ClobberedBoundInsts)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = ground(_, _)
        ; Inst = any(_, _)
        ; Inst = not_reached
        ; Inst = inst_var(_)
        ; Inst = abstract_inst(_, _)
        ),
        unexpected($pred, "inst is not a daio inst")
    ;
        ( Inst = constrained_inst_vars(_, _)
        ; Inst = defined_inst(_)
        ),
        unexpected($pred, "unexpanded inst")
    ).

:- func clobber_daio_bound_inst(module_info, bound_inst) = bound_inst.

clobber_daio_bound_inst(ModuleInfo, BoundInst) = ClobberedBoundInst :-
    BoundInst = bound_functor(ConsId, ArgInsts),
    ClobberedArgInsts = list.map(clobber_daio_arg_inst(ModuleInfo), ArgInsts),
    ClobberedBoundInst = bound_functor(ConsId, ClobberedArgInsts).

:- func clobber_daio_arg_inst(module_info, mer_inst) = mer_inst.

clobber_daio_arg_inst(ModuleInfo, Inst0) = ClobberedInst :-
    inst_expand(ModuleInfo, Inst0, Inst),
    (
        Inst = ground(_Uniq, HOInstInfo),
        ClobberedInst = ground(clobbered, HOInstInfo)
    ;
        ( Inst = bound(_, _, _)
        ; Inst = free
        ; Inst = free(_)
        ),
        ClobberedInst = ground(clobbered, none_or_default_func)
    ;
        ( Inst = any(_, _)
        ; Inst = not_reached
        ; Inst = inst_var(_)
        ; Inst = abstract_inst(_, _)
        ),
        unexpected($pred, "inst is not a daio arg inst")
    ;
        ( Inst = constrained_inst_vars(_, _)
        ; Inst = defined_inst(_)
        ),
        unexpected($pred, "unexpanded arg inst")
    ).

%---------------------------------------------------------------------------%

:- pred transform_direct_arg_in_out_calls_in_pred(direct_arg_proc_map::in,
    direct_arg_proc_in_out_map::in, clone_in_out_map::in,
    pred_id::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_direct_arg_in_out_calls_in_pred(DirectArgProcMap,
        DirectArgProcInOutMap, CloneInOutMap, PredId, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.map_foldl2(
        maybe_transform_direct_arg_in_out_calls_in_proc(DirectArgProcMap,
            DirectArgProcInOutMap, CloneInOutMap, PredId),
        ProcTable0, ProcTable, !ModuleInfo, !Specs),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred maybe_transform_direct_arg_in_out_calls_in_proc(
    direct_arg_proc_map::in, direct_arg_proc_in_out_map::in,
    clone_in_out_map::in, pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_transform_direct_arg_in_out_calls_in_proc(DirectArgProcMap,
        DirectArgProcInOutMap, CloneInOutMap, PredId, ProcId,
        !ProcInfo, !ModuleInfo, !Specs) :-
    ( if proc_info_is_valid_mode(!.ProcInfo) then
        transform_direct_arg_in_out_calls_in_proc(DirectArgProcMap,
            DirectArgProcInOutMap, CloneInOutMap, PredId, ProcId,
            !ProcInfo, !ModuleInfo, !Specs)
    else
        true
    ).

:- pred transform_direct_arg_in_out_calls_in_proc(direct_arg_proc_map::in,
    direct_arg_proc_in_out_map::in, clone_in_out_map::in,
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_direct_arg_in_out_calls_in_proc(DirectArgProcMap,
        DirectArgProcInOutMap, CloneInOutMap, PredId, ProcId,
        !ProcInfo, !ModuleInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no
    ;
        VeryVerbose = yes,
        trace [io(!IO)] (
            write_proc_progress_message(!.ModuleInfo,
                "Direct arg in out transforming", PredId, ProcId, !IO)
        )
    ),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    proc_info_get_goal(!.ProcInfo, Goal0),
    module_info_get_name(!.ModuleInfo, ModuleName),
    trace [compile_time(flag("daio-debug")), io(!IO)] (
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),
        io.format(Stream, "transforming proc(%d, %d)\n",
            [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))], !IO),
        dump_goal_nl(Stream, !.ModuleInfo, vns_var_table(VarTable0),
            Goal0, !IO)
    ),
    bimap.init(VarMap0),
    Info0 = daio_info(!.ModuleInfo, DirectArgProcInOutMap, VarTable0, []),
    proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),
    expand_daio_in_goal(Goal0, Goal, InstMap0, VarMap0, VarMap, Info0, Info),
    PredProcId = proc(PredId, ProcId),
    proc_info_get_headvars(!.ProcInfo, HeadVars0),
    Info = daio_info(_, _, VarTable, CloneForeignProcs),
    proc_info_set_var_table(VarTable, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),

    ( if
        map.search(CloneInOutMap, PredProcId, OoMInOutArgs),
        CloneForeignProcs = []
    then
        OoMInOutArgs = one_or_more(HeadInOutArg, TailInOutArgs),
        replace_cloned_headvars(VarMap, 1, HeadInOutArg, TailInOutArgs,
            HeadVars0, HeadVars),
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_debug_output_stream(Globals, ModuleName, Stream, !IO),
            io.write_string(Stream, "replace_cloned_headvars:\n", !IO),
            io.write_line(Stream, HeadVars0, !IO),
            io.write_line(Stream, HeadVars, !IO)
        ),
        proc_info_set_headvars(HeadVars, !ProcInfo)
    else
        true
    ),
    % If any of the foreign_procs we have invoked appears in
    % DirectArgProcMap as a procedure that needs to be cloned,
    % we generate an error message for it, since
    %
    % (a) this is easier than implementing and then *documenting*
    % an argument passing mechanism for daio arguments to and from
    % foreign coode, and
    %
    % (b) we have seen no need so far for *any* passing of partially
    % instantiated terms to or from foreign code, let alone any that
    % involve direct arg tags.
    %
    % Since this pass is executed before any pass that does inlining,
    % the only call_foreign_proc goals we should have seen is an
    % invocation of *this* procedure, but we iterate over all the
    % marked-to-be-cloned foreign procs we have seen just in case
    % this changes in the future.
    list.foldl(
        maybe_add_foreign_proc_error(!.ModuleInfo, DirectArgProcMap,
            DirectArgProcInOutMap),
        CloneForeignProcs, !Specs),
    requantify_proc_general(ord_nl_maybe_lambda, !ProcInfo),
    recompute_instmap_delta_proc(recomp_atomics,
        !ProcInfo, !ModuleInfo).

    % The purpose of this predicate is described in the long comment
    % about headvars in clone_daio_pred_proc_args.
    %
:- pred replace_cloned_headvars(direct_arg_var_map::in, int::in,
    int::in, list(int)::in, list(prog_var)::in, list(prog_var)::out) is det.

replace_cloned_headvars(_, _, _, _, [], _) :-
        unexpected($pred, "ran out of headvars").
replace_cloned_headvars(VarMap, CurArgNum, HeadInOutArg, TailInOutArgs,
        [HeadVar | TailVars0], Vars) :-
    ( if CurArgNum = HeadInOutArg then
        list.det_head_tail(TailVars0, _CloneVar, TailVars1),
        bimap.lookup(VarMap, HeadVar, UpdatedHeadVar),
        (
            TailInOutArgs = [],
            Vars = [HeadVar, UpdatedHeadVar | TailVars1]
        ;
            TailInOutArgs = [HeadTailInOutArg | TailTailInOutArgs],
            replace_cloned_headvars(VarMap, CurArgNum + 1,
                HeadTailInOutArg, TailTailInOutArgs, TailVars1, TailVars),
            Vars = [HeadVar, UpdatedHeadVar | TailVars]
        )
    else
        replace_cloned_headvars(VarMap, CurArgNum + 1,
            HeadInOutArg, TailInOutArgs, TailVars0, TailVars),
        Vars = [HeadVar | TailVars]
    ).

%---------------------------------------------------------------------------%

    % An entry VarA -> VarB means that VarB is the current version of VarA.
    % The two vars of an entry will always be distinct; we never record
    % a variable as being the current version of itself.
    %
    % When we first create a clone (VarB) of an original variable (VarA),
    % we add an entry to this map. When we later find an update of VarB
    % (to say VarC), we will update the VarA -> VarB entry to VarA -> VarC.
    % Our need to be able to find VarA at that time is why this is a *bi*map.
    %
:- type direct_arg_var_map == bimap(prog_var, prog_var).

:- pred expand_daio_in_goal(hlds_goal::in, hlds_goal::out, instmap::in,
    direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

expand_daio_in_goal(Goal0, Goal, InstMap0, !VarMap, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    VarRename = bimap.forward_map(!.VarMap),
    rename_vars_in_goal_info(need_not_rename, VarRename, GoalInfo0, GoalInfo1),
    (
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        rename_vars_in_goal_expr(need_not_rename, VarRename,
            GoalExpr0, GoalExpr1),
        GoalExpr1 = plain_call(CalleePredId, CalleeProcId, Args0,
            BuiltinState, MaybeUnifyContext, _SymName),
        ProcMap = !.Info ^ daio_proc_map,
        CalleePredProcId = proc(CalleePredId, CalleeProcId),
        ( if map.search(ProcMap, CalleePredProcId, CloneProc) then
            trace [compile_time(flag("daio-debug")), io(!IO)] (
                get_daio_debug_stream(!.Info, Stream, !IO),
                io.format(Stream, "call to proc(%d, %d)\n",
                    [i(pred_id_to_int(CalleePredId)),
                    i(proc_id_to_int(CalleeProcId))], !IO)
            ),
            CloneProc = direct_arg_proc_in_out(ClonePredProcId, OoMInOutArgs),
            ClonePredProcId = proc(ClonePredId, CloneProcId),
            OoMInOutArgs = one_or_more(HeadInOutArg, TailInOutArgs),
            clone_in_out_args_in_plain_call(1, HeadInOutArg, TailInOutArgs,
                Args0, Args, !VarMap, !Info),
            ModuleInfo = !.Info ^ daio_module_info,
            module_info_get_name(ModuleInfo, ModuleName),
            module_info_pred_info(ModuleInfo, ClonePredId, ClonePredInfo),
            pred_info_get_name(ClonePredInfo, ClonePredName),
            CloneSymName = qualified(ModuleName, ClonePredName),
            GoalExpr = plain_call(ClonePredId, CloneProcId, Args,
                BuiltinState, MaybeUnifyContext, CloneSymName)
        else
            GoalExpr = GoalExpr1
        )
    ;
        GoalExpr0 = generic_call(GenericCall, ArgVars0, ArgModes0, RegTypes,
            Detism),
        clone_in_out_args_in_generic_call(ArgVars0, ArgVars,
            ArgModes0, ArgModes, !VarMap, !Info),
        % The float regs pass is invoked well after this pass.
        expect(unify(RegTypes, arg_reg_types_unset), $pred,
            "arg reg types set"),
        GoalExpr1 = generic_call(GenericCall, ArgVars, ArgModes, RegTypes,
            Detism),
        rename_vars_in_goal_expr(need_not_rename, VarRename,
            GoalExpr1, GoalExpr)
    ;
        GoalExpr0 = call_foreign_proc(Attrs, CalleePredId, CalleeProcId,
            Args0, ExtraArgs, TraceCond, Impl),
        CalleePredProcId = proc(CalleePredId, CalleeProcId),
        ProcMap = !.Info ^ daio_proc_map,
        ( if map.search(ProcMap, CalleePredProcId, CloneProc) then
            CloneProc = direct_arg_proc_in_out(ClonePredProcId, OoMInOutArgs),
            ClonePredProcId = proc(ClonePredId, CloneProcId),
            OoMInOutArgs = one_or_more(HeadInOutArg, TailInOutArgs),
            ModuleInfo = !.Info ^ daio_module_info,
            module_info_proc_info(ModuleInfo, ClonePredProcId, CloneProcInfo),
            proc_info_get_argmodes(CloneProcInfo, CloneArgModes),
            clone_in_out_args_in_call_foreign_proc(1, HeadInOutArg,
                TailInOutArgs, Args0, CloneArgModes, Args, !VarMap, !Info),
            GoalExpr1 = call_foreign_proc(Attrs, ClonePredId, CloneProcId,
                Args, ExtraArgs, TraceCond, Impl),
            rename_vars_in_goal_expr(need_not_rename, VarRename,
                GoalExpr1, GoalExpr),
            CloneForeignProcs0 = !.Info ^ daio_clone_foreign_procs,
            CloneForeignProcs = [CalleePredProcId | CloneForeignProcs0],
            !Info ^ daio_clone_foreign_procs := CloneForeignProcs
        else
            rename_vars_in_goal_expr(need_not_rename, VarRename,
                GoalExpr0, GoalExpr)
        )
    ;
        GoalExpr0 = unify(_, _, _, Unification, _),
        ( if
            Unification = construct(_, ConsId, _, _, _, _, _),
            ConsId = closure_cons(ShroudedPredProcId, _EvalMethod),
            ClosurePredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
            ProcMap = !.Info ^ daio_proc_map,
            map.contains(ProcMap, ClosurePredProcId)
        then
            % We did allow a closure to be constructed from a clone procedure,
            % but that leaves the HLDS in an inconsistent state, e.g. the type
            % and inst of the closure variable needs to be updated for the
            % extra arguments, and it gets a lot more complicated than that.
            % While the float registers pass is the only part of the compiler
            % known to break on the inconsistency, that may only be due to the
            % the rarity of higher order terms with daio arguments. Leaving the
            % HLDS in an inconsistent state is a bad idea anyway.
            % See discussion on m-rev on 2021 Jan 25.
            sorry($pred,
                "cannot construct closure with partially-instantiated " ++
                "direct_arg arguments")
        else
            GoalExpr1 = GoalExpr0
        ),
        rename_vars_in_goal_expr(need_not_rename, VarRename,
            GoalExpr1, GoalExpr2),
        expand_daio_in_unify(GoalInfo1, GoalExpr2, GoalExpr, InstMap0,
            !VarMap, !Info)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        expand_daio_in_conj(Goals0, Goals, InstMap0, !VarMap, !Info),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_daio_debug_stream(!.Info, Stream, !IO),
            dump_varmap(!.Info, Stream, "disj before", !.VarMap, !IO),
            io.flush_output(Stream, !IO)
        ),
        expand_daio_in_branches(GoalInfo0, InstMap0, Goals0, Goals,
            !VarMap, !Info),
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_daio_debug_stream(!.Info, Stream, !IO),
            dump_varmap(!.Info, Stream, "disj after", !.VarMap, !IO),
            io.flush_output(Stream, !IO)
        ),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var0, CanFail, Cases0),
        rename_var(need_not_rename, VarRename, Var0, Var),
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_daio_debug_stream(!.Info, Stream, !IO),
            dump_varmap(!.Info, Stream, "switch before", !.VarMap, !IO),
            io.flush_output(Stream, !IO)
        ),
        expand_daio_in_branches(GoalInfo0, InstMap0, Cases0, Cases,
            !VarMap, !Info),
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_daio_debug_stream(!.Info, Stream, !IO),
            dump_varmap(!.Info, Stream, "switch after", !.VarMap, !IO),
            io.flush_output(Stream, !IO)
        ),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        % Bindings made in negated goals are not visible to any code
        % that follows the negation.
        expand_daio_in_goal(SubGoal0, SubGoal, InstMap0, !.VarMap, _, !Info),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        expand_daio_in_if_then_else(GoalInfo0, Cond0, Cond, Then0, Then,
            Else0, Else, InstMap0, !VarMap, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % SubGoal0 can't have any partially instantiated terms.
            GoalExpr = GoalExpr0
        else
            expand_daio_in_goal(SubGoal0, SubGoal, InstMap0, !VarMap, !Info),
            GoalExpr = scope(Reason, SubGoal)
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            expand_daio_in_branches(GoalInfo1, InstMap0,
                [MainGoal0 | OrElseGoals0], MainOrElseGoals, !VarMap, !Info),
            list.det_head_tail(MainOrElseGoals, MainGoal, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            expand_daio_in_goal(SubGoal0, SubGoal, InstMap0, !VarMap, !Info),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo1).

%---------------------%

:- pred clone_in_out_args_in_plain_call(int::in, int::in, list(int)::in,
    list(prog_var)::in, list(prog_var)::out,
    direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

clone_in_out_args_in_plain_call(_, _, _, [], _, !VarMap, !Info) :-
    unexpected($pred, "ran out of arguments").
clone_in_out_args_in_plain_call(CurArgNum, HeadInOutArg, TailInOutArgs,
        [HeadVar0 | TailVars0], Vars, !VarMap, !Info) :-
    ( if CurArgNum = HeadInOutArg then
        make_and_record_new_clone_var("plain", HeadVar0, CloneVar,
            !VarMap, !Info),
        (
            TailInOutArgs = [],
            Vars = [HeadVar0, CloneVar | TailVars0]
        ;
            TailInOutArgs = [HeadTailInOutArg | TailTailInOutArgs],
            clone_in_out_args_in_plain_call(CurArgNum + 1,
                HeadTailInOutArg, TailTailInOutArgs,
                TailVars0, TailVars, !VarMap, !Info),
            Vars = [HeadVar0, CloneVar | TailVars]
        )
    else
        clone_in_out_args_in_plain_call(CurArgNum + 1,
            HeadInOutArg, TailInOutArgs, TailVars0, TailVars, !VarMap, !Info),
        Vars = [HeadVar0 | TailVars]
    ).

%---------------------%

:- pred clone_in_out_args_in_generic_call(
    list(prog_var)::in, list(prog_var)::out,
    list(mer_mode)::in, list(mer_mode)::out,
    direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

clone_in_out_args_in_generic_call([], [], [], [], !VarMap, !Info).
clone_in_out_args_in_generic_call([], _, [_ | _], _, !VarMap, !Info) :-
    unexpected($pred, "list length mismatch").
clone_in_out_args_in_generic_call([_ | _], _, [], _, !VarMap, !Info) :-
    unexpected($pred, "list length mismatch").
clone_in_out_args_in_generic_call([HeadVar0 | TailVars0], Vars,
        [HeadMode0 | TailModes0], Modes, !VarMap, !Info) :-
    ModuleInfo0 = !.Info ^ daio_module_info,
    VarTable0 = !.Info ^ daio_var_table,
    is_direct_arg_in_out_posn(ModuleInfo0, VarTable0, HeadVar0, HeadMode0,
        IsDAIO),
    (
        IsDAIO = mode_is_daio,
        make_and_record_new_clone_var("generic", HeadVar0, CloneVar,
            !VarMap, !Info),
        daio_mode_to_mode_pair(ModuleInfo0, HeadMode0,
            ClobberedHeadMode, CloneMode),
        clone_in_out_args_in_generic_call(TailVars0, TailVars,
            TailModes0, TailModes, !VarMap, !Info),
        Vars = [HeadVar0, CloneVar | TailVars],
        Modes = [ClobberedHeadMode, CloneMode | TailModes]
    ;
        ( IsDAIO = mode_is_not_daio
        ; IsDAIO = mode_may_be_daio
        ),
        % What we do in the mode_may_be_daio case does not matter,
        % because the errors we generate when the callee is compiled
        % will prevent the code we generate from being linked into
        % an executable.
        clone_in_out_args_in_generic_call(TailVars0, TailVars,
            TailModes0, TailModes, !VarMap, !Info),
        Vars = [HeadVar0 | TailVars],
        Modes = [HeadMode0 | TailModes]
    ).

%---------------------%

:- pred clone_in_out_args_in_call_foreign_proc(int::in, int::in, list(int)::in,
    list(foreign_arg)::in, list(mer_mode)::in, list(foreign_arg)::out,
    direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

clone_in_out_args_in_call_foreign_proc(_, _, _, [], _, _, !VarMap, !Info) :-
    unexpected($pred, "ran out of arguments").
clone_in_out_args_in_call_foreign_proc(_, _, _, [_ | _], [], _,
        !VarMap, !Info) :-
    % The clone has strictly more arguments than the procedure it was cloned
    % from, so there should be strictly more modes than foreign_args in the
    % call_foreign_proc.
    unexpected($pred, "ran out of modes").
clone_in_out_args_in_call_foreign_proc(CurArgNum, HeadInOutArg, TailInOutArgs,
        [HeadArg0 | TailArgs0], [HeadCloneMode0 | TailCloneModes0], Args,
        !VarMap, !Info) :-
    ( if CurArgNum = HeadInOutArg then
        HeadArg0 = foreign_arg(HeadVar0, HeadMaybeNameMode0,
            HeadType, HeadBoxPolicy),
        make_and_record_new_clone_var("foreign", HeadVar0, CloneVar,
            !VarMap, !Info),
        list.det_head_tail(TailCloneModes0, CloneMode, TailCloneModes1),
        (
            HeadMaybeNameMode0 = no,
            HeadMaybeNameMode = no,
            CloneMaybeNameMode = no
        ;
            HeadMaybeNameMode0 = yes(HeadNameMode0),
            HeadNameMode0 = foreign_arg_name_mode(HeadName, _HeadMode0),
            HeadNameMode = foreign_arg_name_mode(HeadName, HeadCloneMode0),
            HeadMaybeNameMode = yes(HeadNameMode),

            % This shouldn't clash with the C names of any other arguments,
            % even if there were a clash, it wouldn't matter, since we are
            % generating an error for the call_foreign_proc.
            CloneName = "clone_of_" ++ HeadName,
            CloneNameMode = foreign_arg_name_mode(CloneName, CloneMode),
            CloneMaybeNameMode = yes(CloneNameMode)
        ),
        HeadArg = foreign_arg(HeadVar0, HeadMaybeNameMode,
            HeadType, HeadBoxPolicy),
        % The clone has the same type and uses the box policy as the original.
        CloneArg = foreign_arg(CloneVar, CloneMaybeNameMode,
            HeadType, HeadBoxPolicy),
        (
            TailInOutArgs = [],
            Args = [HeadArg, CloneArg | TailArgs0]
        ;
            TailInOutArgs = [HeadTailInOutArg | TailTailInOutArgs],
            clone_in_out_args_in_call_foreign_proc(CurArgNum + 1,
                HeadTailInOutArg, TailTailInOutArgs,
                TailArgs0, TailCloneModes1, TailArgs, !VarMap, !Info),
            Args = [HeadArg, CloneArg | TailArgs]
        )
    else
        clone_in_out_args_in_call_foreign_proc(CurArgNum + 1,
            HeadInOutArg, TailInOutArgs, TailArgs0, TailCloneModes0, TailArgs,
            !VarMap, !Info),
        Args = [HeadArg0 | TailArgs]
    ).

%---------------------%

:- pred make_and_record_new_clone_var(string::in, prog_var::in, prog_var::out,
    direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

make_and_record_new_clone_var(From, HeadVar0, CloneVar, !VarMap, !Info) :-
    make_new_clone_var(HeadVar0, CloneVar, !Info),
    ( if bimap.reverse_search(!.VarMap, OrigVar, HeadVar0) then
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_daio_debug_stream(!.Info, Stream, !IO),
            io.format(Stream, "%s update mapping %d -> %d\n",
                [s(From), i(term.var_to_int(HeadVar0)),
                i(term.var_to_int(CloneVar))], !IO),
            io.flush_output(Stream, !IO)
        ),
        bimap.set(OrigVar, CloneVar, !VarMap)
    else
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_daio_debug_stream(!.Info, Stream, !IO),
            io.format(Stream, "%s insert mapping %d -> %d\n",
                [s(From), i(term.var_to_int(HeadVar0)),
                i(term.var_to_int(CloneVar))], !IO),
            io.flush_output(Stream, !IO)
        ),
        bimap.det_insert(HeadVar0, CloneVar, !VarMap)
    ).

%---------------------%

:- pred expand_daio_in_unify(hlds_goal_info::in,
    hlds_goal_expr::in(goal_expr_unify), hlds_goal_expr::out,
    instmap::in, direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

expand_daio_in_unify(GoalInfo0, GoalExpr0, GoalExpr, InstMap0,
        !VarMap, !Info) :-
    GoalExpr0 = unify(_LHS, _RHS0, _Mode, Unification0, _Context),
    (
        ( Unification0 = construct(_, _, _, _, _, _, _)
        ; Unification0 = assign(_, _)
        ; Unification0 = simple_test(_, _)
        ),
        GoalExpr = GoalExpr0
    ;
        Unification0 = deconstruct(X, ConsId, _Ys, UnifyModes,
            _CanFail, _CanCgc),
        ModuleInfo = !.Info ^ daio_module_info,
        trace [compile_time(flag("daio-debug")), io(!IO)] (
            get_daio_debug_stream(!.Info, Stream, !IO),
            dump_goal_nl(Stream, ModuleInfo,
                vns_var_table(!.Info ^ daio_var_table),
                hlds_goal(GoalExpr0, GoalInfo0), !IO),
            io.flush_output(Stream, !IO)
        ),
        ( if
            UnifyModes = [UnifyMode],
            UnifyMode = unify_modes_li_lf_ri_rf(LI, LF, _RI, _RF),
            ( LI = free ; LI = free(_) ),
            not ( LF = free ; LF = free(_) ),
            ConsId = cons(ConsIdSymName, ConsIdArity, ConsIdTypeCtor),
            module_info_get_type_table(ModuleInfo, TypeTable),
            search_type_ctor_defn(TypeTable, ConsIdTypeCtor, TypeDefn),
            get_type_defn_body(TypeDefn, TypeBody),
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(_, _, _, MaybeRepn, _),
            (
                MaybeRepn = no,
                unexpected($pred, "MaybeRepn = no")
            ;
                MaybeRepn = yes(Repn)
            ),
            CtorRepns = Repn ^ dur_ctor_repns,
            find_named_ctor_repn(CtorRepns, ConsIdSymName, ConsIdArity,
                ConsIdCtorRepn),
            ConsIdCtorRepn ^ cr_tag = direct_arg_tag(_Ptag)
        then
            make_new_clone_var(X, CloneX, !Info),
            bimap.set(X, CloneX, !VarMap),
            module_info_get_predicate_table(ModuleInfo, PredTable),
            CopySymName = qualified(mercury_private_builtin_module,
                "partial_inst_copy"),
            predicate_table_lookup_pred_sym_arity_one(PredTable,
                is_fully_qualified, CopySymName, user_arity(2), CopyPredId),
            proc_id_to_int(CopyProcId, 0),
            MaybeUnifyContext = no,
            CopyGoalExpr = plain_call(CopyPredId, CopyProcId, [X, CloneX],
                inline_builtin, MaybeUnifyContext, CopySymName),
            Context = goal_info_get_context(GoalInfo0),
            set_of_var.list_to_set([X, CloneX], NonLocals),
            instmap_lookup_var(InstMap0, X, InitInstOfX),
            Clobbered = ground(clobbered, none_or_default_func),
            InstMapDelta = instmap_delta_from_assoc_list(
                [X - Clobbered, CloneX - InitInstOfX]),
            goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
                Context, CopyGoalInfo),
            CopyGoal = hlds_goal(CopyGoalExpr, CopyGoalInfo),
            Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
            GoalExpr = conj(plain_conj, [Goal0, CopyGoal]),
            trace [compile_time(flag("daio-debug")), io(!IO)] (
                get_daio_debug_stream(!.Info, Stream, !IO),
                io.write_string(Stream, "CopyGoal:\n", !IO),
                dump_goal_nl(Stream, ModuleInfo,
                    vns_var_table(!.Info ^ daio_var_table), CopyGoal, !IO),
                io.flush_output(Stream, !IO)
            )
        else
            GoalExpr = GoalExpr0
        )
    ;
        Unification0 = complicated_unify(_, _, _),
        unexpected($pred, "complicated_unify")
    ).

:- pred find_named_ctor_repn(list(constructor_repn)::in,
    sym_name::in, arity::in, constructor_repn::out) is det.

find_named_ctor_repn([], _, _, _) :-
    unexpected($pred, "did not find constructor").
find_named_ctor_repn([Ctor | Ctors], SymName, Arity, SearchCtor) :-
    ( if
        Ctor ^ cr_name = SymName,
        list.length(Ctor ^ cr_args, Arity)
    then
        SearchCtor = Ctor
    else
        find_named_ctor_repn(Ctors, SymName, Arity, SearchCtor)
    ).

%---------------------%

:- pred expand_daio_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

expand_daio_in_conj([], [], _, !VarMap, !Info).
expand_daio_in_conj([Goal0 | Goals0], [Goal | Goals], InstMap0,
        !VarMap, !Info) :-
    expand_daio_in_goal(Goal0, Goal, InstMap0, !VarMap, !Info),
    update_instmap(Goal0, InstMap0, InstMap1),
    expand_daio_in_conj(Goals0, Goals, InstMap1, !VarMap, !Info).

%---------------------%
%
% Ensure that every branch of a branched control structure ends up with
% the same variable representing the current verion of every direct-arg-in-out
% variable that is nonlocal in the branched control structure.
% (If a direct-arg-in-out variable is local to a branch, it needs no merging.)
%
% If a direct-arg-in-out variable was live before the branched control
% structure, then we merge, since it may be used after the branched
% control structure. (If it isn't used after, the merging is useless,
% but also harmless, and the goals that do the merging should be removed
% by the simplification we do on the post-transformation form of the
% procedure.) So if any branch whose end is reachable updates an
% direct-arg-in-out variable, we ensure that all such branches will end up
% with the same variable representing the current version of that original
% direct-arg-in-out variable.
%
% If a direct-arg-in-out variable was not live before the branched control
% structure, then it must be born in every branch whose end is reachable.
% In such cases as well we ensure that all branches whose end is reachable
% end up with the same variable representing the current version of that
% original direct-arg-in-out variable.
%

:- type arm_varmap(G)
    --->    arm_varmap(G, direct_arg_var_map).
:- type goal_varmap == arm_varmap(hlds_goal).
:- type case_varmap == arm_varmap(case).

:- typeclass goal_like(G) where [
    pred end_is_reachable(G::in) is semidet,
    pred append_goal(G::in, hlds_goal::in, G::out) is det,
    pred expand_daio_in_goal_like_varmap(direct_arg_var_map::in,
        G::in, instmap::in, arm_varmap(G)::out,
        daio_info::in, daio_info::out) is det
].

:- instance goal_like(hlds_goal) where [
    pred(end_is_reachable/1) is goal_end_is_reachable,
    pred(append_goal/3) is append_goal_to_goal,
    pred(expand_daio_in_goal_like_varmap/6) is expand_daio_in_goal_varmap
].

:- instance goal_like(case) where [
    pred(end_is_reachable/1) is case_end_is_reachable,
    pred(append_goal/3) is append_goal_to_case,
    pred(expand_daio_in_goal_like_varmap/6) is expand_daio_in_case_varmap
].

:- pred expand_daio_in_goal_likes_varmaps(direct_arg_var_map::in,
    list(G)::in, instmap::in, list(arm_varmap(G))::out,
    daio_info::in, daio_info::out) is det <= goal_like(G).

expand_daio_in_goal_likes_varmaps(_InitVarMap, [], _, [], !Info).
expand_daio_in_goal_likes_varmaps(InitVarMap, [Goal0 | Goals0],
        InstMap0, [GoalVarMap | GoalsVarMaps], !Info) :-
    expand_daio_in_goal_like_varmap(InitVarMap, Goal0, InstMap0,
        GoalVarMap, !Info),
    expand_daio_in_goal_likes_varmaps(InitVarMap, Goals0, InstMap0,
        GoalsVarMaps, !Info).

%---------------------%

:- pred goal_end_is_reachable(hlds_goal::in) is semidet.

goal_end_is_reachable(Goal) :-
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    % We want to test whether the end of the branch is reachable.
    % Any reachable to unreachable transition may come inside Goal,
    % which is what InstMapDelta reports, or before we start executing
    % Goal. However, this pass is invoked just after the simplification
    % pass, and that pass removes all unreachable code, so if we get here,
    % the latter should never be the case.
    instmap_delta_is_reachable(InstMapDelta).

:- pred case_end_is_reachable(case::in) is semidet.

case_end_is_reachable(case(_, _, Goal)) :-
    goal_end_is_reachable(Goal).

%---------------------%

:- pred append_goal_to_goal(hlds_goal::in, hlds_goal::in,
    hlds_goal::out) is det.

append_goal_to_goal(Goal0, AssignGoal, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if GoalExpr0 = conj(plain_conj, Conjuncts0) then
        % This append could be expensive if this loop is executed many times,
        % but the probability of that should be vanishingly small.
        GoalExpr = conj(plain_conj, Conjuncts0 ++ [AssignGoal])
    else
        GoalExpr = conj(plain_conj, [Goal0, AssignGoal])
    ),
    % The nonlocals set and instmap_delta of Goal will be fixed up
    % once the whole procedure body has been transformed.
    Goal = hlds_goal(GoalExpr, GoalInfo0).

:- pred append_goal_to_case(case::in, hlds_goal::in, case::out) is det.

append_goal_to_case(Case0, AssignGoal, Case) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    append_goal_to_goal(Goal0, AssignGoal, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

%---------------------%

:- pred expand_daio_in_goal_varmap(direct_arg_var_map::in,
    hlds_goal::in, instmap::in, goal_varmap::out,
    daio_info::in, daio_info::out) is det.

expand_daio_in_goal_varmap(InitVarMap, Goal0, InstMap0, GoalVarMap, !Info) :-
    expand_daio_in_goal(Goal0, Goal, InstMap0, InitVarMap, VarMap, !Info),
    GoalVarMap = arm_varmap(Goal, VarMap).

:- pred expand_daio_in_case_varmap(direct_arg_var_map::in,
    case::in, instmap::in, case_varmap::out,
    daio_info::in, daio_info::out) is det.

expand_daio_in_case_varmap(InitVarMap, Case0, InstMap0, CaseVarMap, !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    expand_daio_in_goal(Goal0, Goal, InstMap0, InitVarMap, VarMap, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    CaseVarMap = arm_varmap(Case, VarMap).

%---------------------%

:- pred expand_daio_in_branches(hlds_goal_info::in, instmap::in,
    list(G)::in, list(G)::out,
    direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det <= goal_like(G).

expand_daio_in_branches(GoalInfo0, InstMap0, Arms0, Arms,
        InitVarMap, MergedVarMap, !Info) :-
    expand_daio_in_goal_likes_varmaps(InitVarMap,
        Arms0, InstMap0, ArmsVarMaps, !Info),
    % Our caller must pass as the original goal_info, *before* any
    % daio variables in the original code have been replaced by their
    % current variants. If we get a goal_info for which substitution
    % has already been done, then the intersection below will miss
    % variables that we need to merge.
    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    NonLocalsSet = set_of_var.bitset_to_set(NonLocals),
    VarMapVars0 = set.list_to_set(bimap.ordinates(InitVarMap)),
    gather_orig_vars(ArmsVarMaps, VarMapVars0, VarMapVars),
    set.intersect(NonLocalsSet, VarMapVars, VarsToMerge),
    merge_branch_goals_varmaps(InitVarMap, set.to_sorted_list(VarsToMerge),
        MergedVarMapEntries, ArmsVarMaps, MergedArmsVarMaps, !Info),
    bimap.det_from_assoc_list(MergedVarMapEntries, MergedVarMap),
    trace [compile_time(flag("daio-debug")), io(!IO)] (
        get_daio_debug_stream(!.Info, Stream, !IO),
        VarTable = !.Info ^ daio_var_table,
        io.format(Stream, "nonlocals: %s\n",
            [s(mercury_vars_to_string(VarTable, print_name_and_num,
                set.to_sorted_list(NonLocalsSet)))], !IO),
        io.format(Stream, "varmap vars: %s\n",
            [s(mercury_vars_to_string(VarTable, print_name_and_num,
                set.to_sorted_list(VarMapVars)))], !IO),
        io.format(Stream, "vars to merg: %s\n",
            [s(mercury_vars_to_string(VarTable, print_name_and_num,
                set.to_sorted_list(VarsToMerge)))], !IO),
        dump_varmap(!.Info, Stream, "before branch", InitVarMap, !IO),
        dump_varmap(!.Info, Stream, "after branch", MergedVarMap, !IO)
    ),
    Arms = list.map(project_arm, MergedArmsVarMaps).

:- pred expand_daio_in_if_then_else(hlds_goal_info::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, instmap::in,
    direct_arg_var_map::in, direct_arg_var_map::out,
    daio_info::in, daio_info::out) is det.

expand_daio_in_if_then_else(GoalInfo0, Cond0, Cond, Then0, Then,
        Else0, Else, InstMap0, InitVarMap, MergedVarMap, !Info) :-
    expand_daio_in_goal(Cond0, Cond, InstMap0, InitVarMap, CondVarMap, !Info),
    update_instmap(Cond0, InstMap0, InstMap1),
    expand_daio_in_goal(Then0, Then1, InstMap1, CondVarMap, ThenVarMap, !Info),
    expand_daio_in_goal(Else0, Else1, InstMap0, InitVarMap, ElseVarMap, !Info),
    ThenArmVarMap1 = arm_varmap(Then1, ThenVarMap),
    ElseArmVarMap1 = arm_varmap(Else1, ElseVarMap),

    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    VarMapVars = set.list_to_set(bimap.ordinates(InitVarMap) ++
        bimap.ordinates(ThenVarMap) ++ bimap.ordinates(ElseVarMap)),
    set.intersect(set_of_var.bitset_to_set(NonLocals), VarMapVars,
        VarsToMerge),
    merge_branch_goals_varmaps(InitVarMap, set.to_sorted_list(VarsToMerge),
        MergedVarMapEntries, [ThenArmVarMap1, ElseArmVarMap1],
        MergedDisjunctsVarMaps, !Info),
    bimap.det_from_assoc_list(MergedVarMapEntries, MergedVarMap),
    (
        MergedDisjunctsVarMaps = [arm_varmap(Then, _), arm_varmap(Else, _)]
    ;
        ( MergedDisjunctsVarMaps = []
        ; MergedDisjunctsVarMaps = [_]
        ; MergedDisjunctsVarMaps = [_, _, _ | _]
        ),
        unexpected($pred, "then and else not length 2")
    ).

%---------------------%

:- pred merge_branch_goals_varmaps(direct_arg_var_map::in, list(prog_var)::in,
    assoc_list(prog_var, prog_var)::out,
    list(arm_varmap(G))::in, list(arm_varmap(G))::out,
    daio_info::in, daio_info::out) is det <= goal_like(G).

merge_branch_goals_varmaps(_, [], [], !GoalsVarMaps, !Info).
merge_branch_goals_varmaps(EntryVarMap, [OrigVar | OrigVars],
        [OrigVar - MergeVar | OrigMergeVars], !GoalsVarMaps, !Info) :-
    ( if
        bimap.search(EntryVarMap, OrigVar, EntryVar),
        entry_var_is_ever_changed(OrigVar, EntryVar, !.GoalsVarMaps) = no
    then
        MergeVar = EntryVar
    else
        make_new_clone_var(OrigVar, MergeVar, !Info),
        % Note that the assignment we add here would be accepted
        % by mode analysis if it appeared in the source code *only*
        % if OrigVar is ground. If it could still be only partially
        % instantiated, as happens in e.g. tests/hard_coded/gh72.m,
        % mode analysis would reject the code we create here,
        % because neither of the two variables being unified here
        % would be ground. However, this pass is done *after* mode analysis,
        % so this is not an issue, *unless* some later pass repeats
        % a full mode analysis. (The recomputation of instmap deltas,
        % which we invoke once the transformation of the procedure body
        % is complete, does not care about such details.)
        %
        % The assignment we do here is semantically ok because it does not
        % actually create a free-free alias between the two variables,
        % since this unification is both the last appearance of (the current
        % version of) OrigVar and the first appearance of MergeVar.
        % Free-free alias is a problem *only* between two variables
        % that can be alive at the same time. This pass ensures the
        % disjointness of the two variables' lifefimes by construction,
        % but (in the absence of alias tracking) mode analysis cannot
        % check this.
        add_assign_of_merge_var(OrigVar, MergeVar, !GoalsVarMaps)
    ),
    merge_branch_goals_varmaps(EntryVarMap, OrigVars, OrigMergeVars,
        !GoalsVarMaps, !Info).

:- func entry_var_is_ever_changed(prog_var, prog_var,
    list(arm_varmap(G))) = bool <= goal_like(G).

entry_var_is_ever_changed(_OrigVar, _EntryVar, []) = no.
entry_var_is_ever_changed(OrigVar, EntryVar, [GoalVarMap | GoalsVarMaps]) =
        IsChanged :-
    IsChangedTail = entry_var_is_ever_changed(OrigVar, EntryVar, GoalsVarMaps),
    GoalVarMap = arm_varmap(Goal, VarMap),
    bimap.lookup(VarMap, OrigVar, AfterVar),
    ( if AfterVar = EntryVar then
        IsChanged = IsChangedTail
    else
        ( if end_is_reachable(Goal) then
            IsChanged = yes
        else
            IsChanged = IsChangedTail
        )
    ).

:- pred add_assign_of_merge_var(prog_var::in, prog_var::in,
    list(arm_varmap(G))::in, list(arm_varmap(G))::out) is det <= goal_like(G).

add_assign_of_merge_var(_OrigVar, _MergeVar, [], []).
add_assign_of_merge_var(OrigVar, MergeVar,
        [GoalVarMap0 | GoalVarMaps0], [GoalVarMap | GoalsVarMaps]) :-
    GoalVarMap0 = arm_varmap(Goal0, VarMap),
    ( if end_is_reachable(Goal0) then
        ( if bimap.search(VarMap, OrigVar, CurVarPrime) then
            CurVar = CurVarPrime
        else
            CurVar = OrigVar
        ),
        UnifyMainContext = umc_implicit("direct_arg_in_out_call"),
        UnifySubContexts = [],
        make_simple_assign(MergeVar, CurVar,
            UnifyMainContext, UnifySubContexts, AssignGoal),
        append_goal(Goal0, AssignGoal, Goal)
    else
        % There is no point in adding dead code to the end of Goal0.
        Goal = Goal0
    ),
    GoalVarMap = arm_varmap(Goal, VarMap),
    add_assign_of_merge_var(OrigVar, MergeVar, GoalVarMaps0, GoalsVarMaps).

%---------------------%

:- pred gather_orig_vars(list(arm_varmap(T))::in,
    set(prog_var)::in, set(prog_var)::out) is det.

gather_orig_vars([], !OrigVars).
gather_orig_vars([ArmVarMap | ArmVarMaps], !OrigVars) :-
    ArmVarMap = arm_varmap(_, VarMap),
    set.insert_list(bimap.ordinates(VarMap), !OrigVars),
    gather_orig_vars(ArmVarMaps, !OrigVars).

:- func project_arm(arm_varmap(T)) = T.

project_arm(arm_varmap(Arm, _VarMap)) = Arm.

%---------------------%

:- pred make_new_clone_var(prog_var::in, prog_var::out,
    daio_info::in, daio_info::out) is det.

make_new_clone_var(OldVar, NewVar, !Info) :-
    VarTable0 = !.Info ^ daio_var_table,
    lookup_var_entry(VarTable0, OldVar, OldVarEntry),
    OldVarEntry = vte(OldVarName, OldVarType, OldVarIsDummy),
    NewVarName = maybe_add_goal_clone_suffix(OldVarName),
    NewVarEntry = vte(NewVarName, OldVarType, OldVarIsDummy),
    add_var_entry(NewVarEntry, NewVar, VarTable0, VarTable),
    !Info ^ daio_var_table := VarTable.

%---------------------------------------------------------------------------%

:- pred transform_class(direct_arg_proc_in_out_map::in,
    hlds_class_defn::in, hlds_class_defn::out) is det.

transform_class(DirectArgProcInOutMap, Class0, Class) :-
    PredProcIds0 = Class0 ^ classdefn_method_ppids,
    list.map(transform_class_instance_proc(DirectArgProcInOutMap),
        PredProcIds0, PredProcIds),
    Class = Class0 ^ classdefn_method_ppids := PredProcIds.

:- pred transform_class_instances(direct_arg_proc_in_out_map::in,
    list(hlds_instance_defn)::in, list(hlds_instance_defn)::out) is det.

transform_class_instances(DirectArgProcInOutMap, Instances0, Instances) :-
    list.map(transform_class_instance(DirectArgProcInOutMap),
        Instances0, Instances).

:- pred transform_class_instance(direct_arg_proc_in_out_map::in,
    hlds_instance_defn::in, hlds_instance_defn::out) is det.

transform_class_instance(DirectArgProcInOutMap, Instance0, Instance) :-
    MaybeMethodPredProcIds0 = Instance0 ^ instdefn_maybe_method_ppids,
    (
        MaybeMethodPredProcIds0 = no,
        Instance = Instance0
    ;
        MaybeMethodPredProcIds0 = yes(MethodPredProcIds0),
        list.map(transform_class_instance_proc(DirectArgProcInOutMap),
            MethodPredProcIds0, MethodPredProcIds),
        MaybeMethodPredProcIds = yes(MethodPredProcIds),
        Instance = Instance0 ^ instdefn_maybe_method_ppids
            := MaybeMethodPredProcIds
    ).

:- pred transform_class_instance_proc(direct_arg_proc_in_out_map::in,
    pred_proc_id::in, pred_proc_id::out) is det.

transform_class_instance_proc(DirectArgProcInOutMap, PredProcId0, PredProcId) :-
    ( if map.search(DirectArgProcInOutMap, PredProcId0, ProcInOut) then
        ProcInOut = direct_arg_proc_in_out(PredProcId, _ArgPosns)
    else
        PredProcId = PredProcId0
    ).

%---------------------------------------------------------------------------%

:- type daio_info
    --->    daio_info(
                % These two fields remain constant during the traversal
                % of a procedure body.
                daio_module_info            :: module_info,
                daio_proc_map               :: direct_arg_proc_in_out_map,

                % We update this field as we create new clone variables.
                daio_var_table              :: var_table,

                % We update this field as we find call_foreign_proc goals
                % whose procedure is in the daio_proc_map.
                daio_clone_foreign_procs    :: list(pred_proc_id)
            ).

:- pred get_daio_debug_stream(daio_info::in, io.text_output_stream::out,
    io::di, io::uo) is det.

get_daio_debug_stream(Info, Stream, !IO) :-
    ModuleInfo = Info ^ daio_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    get_debug_output_stream(Globals, ModuleName, Stream, !IO).

:- pred dump_varmap(daio_info::in, io.text_output_stream::in,
    string::in, direct_arg_var_map::in, io::di, io::uo) is det.

dump_varmap(Info, Stream, Desc, VarMap, !IO) :-
    io.format(Stream, "%s:\n", [s(Desc)], !IO),
    bimap.to_assoc_list(VarMap, VarMapAL),
    list.foldl(dump_varmap_entry(Info, Stream), VarMapAL, !IO),
    io.format(Stream, "%s end\n", [s(Desc)], !IO).

:- pred dump_varmap_entry(daio_info::in, io.text_output_stream::in,
    pair(prog_var, prog_var)::in, io::di, io::uo) is det.

dump_varmap_entry(Info, Stream, FromVar - ToVar, !IO) :-
    VarTable = Info ^ daio_var_table,
    FromVarName = mercury_var_to_string(VarTable, print_name_and_num, FromVar),
    ToVarName = mercury_var_to_string(VarTable, print_name_and_num, ToVar),
    io.format(Stream, "\t%s -> %s\n", [s(FromVarName), s(ToVarName)], !IO).

%---------------------------------------------------------------------------%
%
% Debugging the transformations performed by this module is easier
% if clone variables differ from their progenitor variables not only
% in variable number, but also in the name. These functions exist to
% allow a distinguishing suffix to be added to the ends of the names
% of cloned variables.
%
% However, since the foreign language variable names used by code inside
% foreign_procs must match the original variable names used by the programmer,
% both these functions must return their arguments unchanged if we want
% passing cloned vars to foreign_procs to work.
%

:- func maybe_add_goal_clone_suffix(string) = string.

maybe_add_goal_clone_suffix(OldVarName) = CloneVarName :-
    ( if OldVarName = "" then
        CloneVarName = ""
    else
        CloneVarName = OldVarName   % ++ "_clone"
    ).

:- func maybe_add_headvar_clone_suffix(string) = string.

maybe_add_headvar_clone_suffix(OldVarName) = CloneVarName :-
    ( if OldVarName = "" then
        CloneVarName = ""
    else
        CloneVarName = OldVarName   % ++ "_clone"
    ).

%---------------------------------------------------------------------------%

:- pred generate_problem_proc_error(module_info::in, pred_proc_id::in,
    one_or_more(int)::in, error_spec::out) is det.

generate_problem_proc_error(ModuleInfo, PredProcId, OoMProblemArgs, Spec) :-
    OoMProblemArgs = one_or_more(HeadProblemArg, TailProblemArgs),
    ProcDescPieces = describe_one_proc_name(ModuleInfo,
        should_not_module_qualify, PredProcId),
    (
        TailProblemArgs = [],
        Pieces = [words("Error: the compiler cannot implement"),
            words("argument passing for the"), nth_fixed(HeadProblemArg),
            words("argument of")] ++ ProcDescPieces ++ [suffix(","),
            words("because the type of this argument"),
            words("uses the"), quote("direct_arg"),
            words("data representations optimization,"),
            words("which requires special handling when"),
            words("used with partially instantiated data structures,"),
            words("but the mode of this argument,"),
            words("containing either abstract insts or inst vars,"),
            words("prevents the compiler from knowing whether"),
            words("to apply this special handling or not."), nl]
    ;
        TailProblemArgs = [_ | _],
        ProblemArgPieces = list.map((func(N) = nth_fixed(N)),
            [HeadProblemArg | TailProblemArgs]),
        ProblemArgListPieces =
            component_list_to_pieces("and", ProblemArgPieces),
        Pieces = [words("Error: the compiler cannot implement"),
            words("argument passing for the")] ++ ProblemArgListPieces ++
            [words("arguments of")] ++ ProcDescPieces ++ [suffix(","),
            words("because the types of these arguments"),
            words("use the"), quote("direct_arg"),
            words("data representations optimization,"),
            words("which requires special handling when"),
            words("used with partially instantiated data structures,"),
            words("but the modes of these arguments,"),
            words("containing either abstract insts or inst vars,"),
            words("prevent the compiler from knowing whether"),
            words("to apply this special handling or not."), nl]
    ),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    Spec = simplest_spec($pred, severity_error, phase_direct_arg_in_out,
        Context, Pieces).

%---------------------%

:- pred generate_error_if_cloned_proc_is_exported(module_info::in,
    direct_arg_proc_map::in, pragma_exported_proc::in,
    list(pragma_exported_proc)::in, list(pragma_exported_proc)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_error_if_cloned_proc_is_exported(ModuleInfo, DirectArgProcMap,
        ExportedProc0, !RevExportedProcs, !Specs) :-
    ExportedProc0 = pragma_exported_proc(Lang, PredId, ProcId,
        ExportedName, Context),
    PredProcId = proc(PredId, ProcId),
    ( if
        map.search(DirectArgProcMap, PredProcId, _),
        % The direct arg representation optimization works only in C,
        % so we clone procedures only when targeting C.
        Lang = lang_c
    then
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_name(PredInfo, PredName),
        pred_info_get_orig_arity(PredInfo, OrigArity),
        % We don't print the module qualifier anyway.
        PredSNA = sym_name_arity(unqualified(PredName), OrigArity),
        generate_foreign_export_error(PredSNA, ExportedName, Context, Spec),
        !:Specs = [Spec | !.Specs]
    else
        !:RevExportedProcs = [ExportedProc0 | !.RevExportedProcs]
    ).

:- pred generate_foreign_export_error(sym_name_arity::in, string::in,
    prog_context::in, error_spec::out) is det.

generate_foreign_export_error(PredSNA, ExportedName, Context, Spec) :-
    Pieces = [words("Error: the C code for"),
        unqual_sym_name_arity(PredSNA), words("cannot be exported to C"),
        words("as"), quote(ExportedName), suffix(","),
        words("because"), unqual_sym_name_arity(PredSNA), words("has"),
        words("a nonstandard and undocumented calling convention"),
        words("due to interactions between its use of"),
        words("partially instantiated data structures"),
        words("and the"), quote("direct_arg"),
        words("data representation optimization."), nl],
    Spec = simplest_spec($pred, severity_error, phase_direct_arg_in_out,
        Context, Pieces).

%---------------------%

:- pred maybe_add_foreign_proc_error(module_info::in,
    direct_arg_proc_map::in, direct_arg_proc_in_out_map::in, pred_proc_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_add_foreign_proc_error(ModuleInfo, DirectArgProcMap,
        DirectArgProcInOutMap, PredProcId, !Specs) :-
    ( if map.search(DirectArgProcMap, PredProcId, DirectArgProc) then
        % The original procedure has been deleted; we want
        % generate_call_foreign_proc_error to look up its name and context
        % in its clone.
        map.lookup(DirectArgProcInOutMap, PredProcId, ProcInOut),
        ProcInOut = direct_arg_proc_in_out(ClonePredProcId, _),
        generate_call_foreign_proc_error(ModuleInfo, ClonePredProcId, DirectArgProc,
            Spec),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- pred generate_call_foreign_proc_error(module_info::in, pred_proc_id::in,
    direct_arg_proc::in, error_spec::out) is det.

generate_call_foreign_proc_error(ModuleInfo, PredProcId, DirectArgProc, Spec) :-
    StartPieces = [words("Error: a procedure implemented using a"),
        pragma_decl("foreign_proc"), words("declaration"),
        words("may not have any arguments"),
        words("whose types use the"), quote("direct_arg"),
        words("data representations optimization,"),
        words("and whose modes indicate that they fill in"),
        words("partially instantiated terms.")],
    ProcDescPieces = describe_one_proc_name(ModuleInfo,
        should_not_module_qualify, PredProcId),
    OfProcDescPieces = [words("of")] ++ ProcDescPieces,
    (
        DirectArgProc = direct_arg_clone_proc(OoMCloneArgs),
        OoMCloneArgs = one_or_more(HeadCloneArg, TailCloneArgs),
        Pieces = StartPieces ++
            args_violate_prohibition_pieces(OfProcDescPieces,
                HeadCloneArg, TailCloneArgs) ++
            [suffix("."), nl]
    ;
        DirectArgProc = direct_arg_problem_proc(OoMProblemArgs, CloneArgs),
        OoMProblemArgs = one_or_more(HeadProblemArg, TailProblemArgs),
        (
            CloneArgs = [],
            Pieces = StartPieces ++
                args_may_violate_prohibition_pieces(OfProcDescPieces,
                    HeadProblemArg, TailProblemArgs) ++
                [suffix("."), nl]
        ;
            CloneArgs = [HeadCloneArg | TailCloneArgs],
            Pieces = StartPieces ++
                args_violate_prohibition_pieces(OfProcDescPieces,
                    HeadCloneArg, TailCloneArgs) ++
                [suffix(","), words("and"), lower_case_next_if_not_first] ++
                args_may_violate_prohibition_pieces(OfProcDescPieces,
                    HeadProblemArg, TailProblemArgs) ++
                [words("as well."), nl]
        )
    ),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    Spec = simplest_spec($pred, severity_error, phase_direct_arg_in_out,
        Context, Pieces).

:- func args_violate_prohibition_pieces(list(format_piece),
    int, list(int)) = list(format_piece).

args_violate_prohibition_pieces(OfProcDescPieces, HeadArg, TailArgs)
        = Pieces :-
    (
        TailArgs = [],
        Pieces =
            [words("Argument"), int_fixed(HeadArg)] ++ OfProcDescPieces ++
            [words("violates this prohibition")]
    ;
        TailArgs = [_ | _],
        ArgPieces = list.map((func(N) = int_fixed(N)), [HeadArg | TailArgs]),
        ArgsPieces = component_list_to_pieces("and", ArgPieces),
        Pieces =
            [words("Arguments")] ++ ArgsPieces ++ OfProcDescPieces ++
            [words("violate this prohibition")]
    ).

:- func args_may_violate_prohibition_pieces(list(format_piece),
    int, list(int)) = list(format_piece).

args_may_violate_prohibition_pieces(OfProcDescPieces, HeadArg, TailArgs)
        = Pieces :-
    (
        TailArgs = [],
        Pieces =
            [words("Argument"), int_fixed(HeadArg)] ++ OfProcDescPieces ++
            [words("may violate this prohibition")]
    ;
        TailArgs = [_ | _],
        ArgPieces = list.map((func(N) = int_fixed(N)), [HeadArg | TailArgs]),
        ArgsPieces = component_list_to_pieces("and", ArgPieces),
        Pieces =
            [words("Arguments")] ++ ArgsPieces ++ OfProcDescPieces ++
            [words("may violate this prohibition")]
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.direct_arg_in_out.
%---------------------------------------------------------------------------%
