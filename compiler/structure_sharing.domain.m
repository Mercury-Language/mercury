%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2008, 2010-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: structure_sharing.domain.m.
% Main author: nancy.
%
% This module defines the abstract domain for representing structure sharing
% between data structures.
%
% This domain forms a complete lattice. It has a bottom element (representing
% the definite absence of any possible structure sharing), and a top element
% (that represents any possible structure sharing), a least upper bound
% operation, and a comparison predicate (is_subsumed_by).
%
% The auxiliary functions needed for using the domain within the abstract
% semantics on which the implementation of the analysis is based are:
%
%   * project: limit the abstract information to include only the
%       information regarding some given set of variables.
%
%   * rename: given a mapping of variables, this operation renames every
%       occurrence of a variable to its mapping.
%
%   * init: create an initial empty sharing set that represents the absence
%     of any possible sharing.
%
%   * comb: combine new sharing information (that usually stems from a
%     procedure call) with existing sharing such that the result correctly
%     approximates the real sharing that would exist when new concrete
%     sharing is added to existing sharing.
%
%   * add: add the sharing created by a primitive operation (unification)
%     to any existing sharing.
%
% Additional operations:
%
%   * extend_datastruct: compute the set of datastructures referring to the
%     same memory space as a given datastructure, using sharing information;
%     needed by the reuse analysis to check whether a given datastructure is
%     the last pointer to the memory space it refers to.
%
%   * conversion operations between the public and private representation
%     for sharing sets.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_sharing.domain.
:- interface.

:- import_module analysis.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

    % The hidden representation for structure sharing.
    %
:- type sharing_as.

    % Operations w.r.t. the "bottom" element of the lattice.
    %
:- func sharing_as_init = sharing_as.
:- pred sharing_as_is_bottom(sharing_as::in) is semidet.

    % Operations w.r.t. the "top" element of the lattice. When sharing
    % becomes top, it is useful to know why it has become top.
    %
:- func sharing_as_top_no_feedback = sharing_as.
:- func sharing_as_top_sharing(top_feedback) = sharing_as.
:- func sharing_as_top_sharing_accumulate(top_feedback, sharing_as)
    = sharing_as.
:- pred sharing_as_is_top(sharing_as::in) is semidet.

    % Return the size of the sharing set. Fail when sharing is top.
    %
:- func sharing_as_size(sharing_as) = int is semidet.

    % Return a short description of the sharing information.
    %
:- func sharing_as_short_description(sharing_as) = string.

    % Projection operation.
    % This operation reduces the sharing information to information
    % regarding a given set of variables only.
    % Some properties of a call `project(Vars, SharingIn, SharingOut)':
    %   * vars(SharingOut) is a subset of Vars.
    %   * vars(SharingIn minus SharingOut) union Vars = emptyset.
    %
:- pred sharing_as_project(prog_vars::in,
    sharing_as::in, sharing_as::out) is det.
:- func sharing_as_project(prog_vars, sharing_as) = sharing_as.
:- pred sharing_as_project_set(set(prog_var)::in,
    sharing_as::in, sharing_as::out) is det.

    % Renaming operation.
    % This operation renames the variables and type variables occurring
    % in the sharing information according to a variable and type variable
    % mapping.
    %
:- pred sharing_as_rename(prog_var_renaming::in, tsubst::in,
    sharing_as::in, sharing_as::out) is det.

    % sharing_as_rename_using_module_info(ModuleInfo, PPId,
    %   ActualVars, ActualTypes, CallerTVarset, CallerHeadVarParams,
    %   FormalSharing, ActualSharing):
    %
    % Renaming of the formal description of data structure sharing to the
    % actual description of the sharing. The information about the formal
    % variables needs to be extracted from the module information.
    % A list of variables and types is used as the actual variables and types.
    % The type variables set in the actual context must also be specified.
    %
:- pred sharing_as_rename_using_module_info(module_info::in,
    pred_proc_id::in, prog_vars::in, list(mer_type)::in, tvarset::in,
    external_type_params::in, sharing_as::in, sharing_as::out) is det.

    % One of the cornerstone operations of using the program analysis system
    % is to provide a "comb" (combination) operation that combines new
    % information with old information to obtain a new description. Within the
    % analysis system, the "comb" operation is used to combine the information
    % of a predicate call (the new information) to the information that is
    % already available before the predicate call (the old information). The
    % result must be a correct description of the execution state that would
    % be obtained if the predicate call is executed in the circumstances
    % described by the old information.
    %
    % comb is not commutative!
    % The correct call is:
    % Result = comb(ModuleInfo, ProcInfo, NewSharing, OldSharing).
    %
:- func sharing_as_comb(module_info, proc_info, sharing_as, sharing_as)
    = sharing_as.

    % Add the sharing created by a unification to the already existing sharing
    % information.
    %
:- func add_unify_sharing(module_info, proc_info, unification, hlds_goal_info,
    sharing_as) = sharing_as.

    % Add the sharing created by a call to some foreign code. This
    % sharing corresponds to the sharing information with which the
    % foreign code was manually annotated, or can be predicted to
    % "bottom", and in the worst case to "top".
    %
:- pred add_foreign_proc_sharing(module_info::in, pred_info::in, proc_info::in,
    pred_proc_id::in, pragma_foreign_proc_attributes::in,
    list(foreign_arg)::in, prog_context::in, sharing_as::in, sharing_as::out)
    is det.

    % Compare two sharing sets. A sharing set Set1 is subsumed by a sharing set
    % Set2 iff the total set of sharing represented by Set1 is a subset of the
    % total set of sharing represented by Set2. This means that for every
    % structure sharing pair in Set1, there exists a structure sharing pair P2
    % in Set2, such that P1 is subsumed by P2 (i.e. P2 at least describes the
    % same sharing as P1).
    %
:- pred sharing_as_is_subsumed_by(module_info::in, proc_info::in,
    sharing_as::in, sharing_as::in) is semidet.

:- pred sharing_as_and_status_is_subsumed_by(module_info::in, proc_info::in,
    sharing_as_and_status::in, sharing_as_and_status::in) is semidet.

    % Compute the least upper bound.
    %
:- func sharing_as_least_upper_bound(module_info, proc_info,
    sharing_as, sharing_as) = sharing_as.
:- func sharing_as_least_upper_bound_of_list(module_info, proc_info,
    list(sharing_as)) = sharing_as.

    % Compute the set of data structures whose memory representation coincide
    % with the memory representation of the given datastructure.
    % This corresponds to the "extend" operation used in Nancy's Phd.
    % The operation produces a software error when called with a top alias
    % description.
    %
:- func extend_datastruct(module_info, proc_info, sharing_as, datastruct)
    = list(datastruct).
:- func extend_datastructs(module_info, proc_info, sharing_as,
    list(datastruct)) = list(datastruct).

    % apply_widening(ModuleInfo, ProcInfo, WideningLimit, WideningDone,
    %   SharingIn, SharingOut):
    %
    % Perform type widening on the structure sharing information if the
    % size of the set is larger than the indicated widening limit (unless the
    % limit equals zero, in which case no widening is performed). The boolean
    % WideningDone is set to true if indeed widening has been done.
    %
    % Type widening consists of mapping each selector within the
    % structure sharing set to the type selector that designates the
    % type of the node that is selected by that selector, which widens the
    % information that is represented by the initial structure sharing
    % description.
    %
:- pred apply_widening(module_info::in, proc_info::in, int::in, bool::out,
    sharing_as::in, sharing_as::out) is det.

    % Conversion between the public and the private representation for
    % structure sharing sets.
    %
:- func from_structure_sharing_domain(structure_sharing_domain) = sharing_as.
:- func to_structure_sharing_domain(sharing_as) = structure_sharing_domain.

%---------------------------------------------------------------------------%
%
% Sharing table
%

% This table is used to temporarily record the sharing analysis results,
% instead of saving in the HLDS and having to continuously convert between the
% public and private representation of structure sharing.

    % Mapping between pred_proc_ids and sharing information that has been
    % derived for the corresponding procedure definitions.
    %
:- type sharing_as_table == map(pred_proc_id, sharing_as_and_status).

:- type sharing_as_and_status
    --->    sharing_as_and_status(
                sharing_as,
                analysis_status
            ).

    % Initialisation.
    %
:- func sharing_as_table_init = sharing_as_table.

    % Look up the sharing information of a specific procedure. Fail if the
    % procedure id is not in the map.
    %
:- pred sharing_as_table_search(pred_proc_id::in, sharing_as_table::in,
    sharing_as_and_status::out) is semidet.

    % Set the sharing information for a given pred_proc_id.
    %
:- pred sharing_as_table_set(pred_proc_id::in, sharing_as_and_status::in,
    sharing_as_table::in, sharing_as_table::out) is det.

%---------------------------------------------------------------------------%

    % Lookup the sharing information of a called procedure (given its
    % pred_id and proc_id), and combine it with any existing
    % sharing information.
    %
:- pred lookup_sharing_and_comb(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, pred_id::in, proc_id::in, prog_vars::in,
    sharing_as::in, sharing_as::out) is det.

    % Lookup the sharing information in the sharing table, or if it is not
    % in there, try to predict it using the information available in the
    % module_info.
    %
    % Lookup the sharing information of a procedure identified by its
    % pred_proc_id.
    % 1 - look in sharing_as_table (as we might already have analysed
    %     the predicate, if defined in same module, or analysed in other
    %     imported module)
    % 2 - try to predict bottom;
    % 3 - react appropriately if the calls happen to be to
    %     * either compiler generated predicates
    %     * or predicates from builtin.m and private_builtin.m
    %     * `:- pragma external_{pred/func}' procedures
    %
:- pred lookup_sharing_or_predict(module_info::in, sharing_as_table::in,
    pred_proc_id::in, sharing_as::out, analysis_status::out, bool::out) is det.

    % Succeeds if the sharing of a procedure can safely be approximated by
    % "bottom", simply by looking at the modes and types of the arguments,
    % or because the procedure is of a generated special predicate.
    %
:- pred bottom_sharing_is_safe_approximation(module_info::in, pred_info::in,
    proc_info::in) is semidet.

    % Succeeds if the sharing of a call can safely be approximated by
    % "bottom", simply by looking at the modes and types of the arguments.
    %
:- pred bottom_sharing_is_safe_approximation_by_args(module_info::in,
    list(mer_mode)::in, list(mer_type)::in) is semidet.

    % Load all the structure sharing information present in the HLDS into
    % a sharing table.
    %
:- func load_structure_sharing_table(module_info) = sharing_as_table.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_top_functor.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_llds.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.selector.
:- import_module transform_hlds.ctgc.util.

:- import_module assoc_list.
:- import_module exception.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module univ.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type sharing_as
    --->    sharing_as_real_as(sharing_set)
    ;       sharing_as_bottom
    ;       sharing_as_top(set(top_feedback)).

%---------------------------------------------------------------------------%

sharing_as_init = sharing_as_bottom.

sharing_as_is_bottom(sharing_as_bottom).

sharing_as_top_no_feedback = sharing_as_top(set.init).

sharing_as_top_sharing(Msg) = sharing_as_top(set.make_singleton_set(Msg)).

sharing_as_top_sharing_accumulate(Msg, SharingAs) = TopSharing :-
    (
        ( SharingAs = sharing_as_real_as(_)
        ; SharingAs = sharing_as_bottom
        ),
        TopSharing = sharing_as_top_sharing(Msg)
    ;
        SharingAs = sharing_as_top(Msgs0),
        Msgs = set.insert(Msgs0, Msg),
        TopSharing = sharing_as_top(Msgs)
    ).

sharing_as_is_top(sharing_as_top(_)).

sharing_as_size(sharing_as_bottom) = 0.
sharing_as_size(sharing_as_real_as(SharingSet)) = sharing_set_size(SharingSet).

sharing_as_short_description(sharing_as_bottom) = "b".
sharing_as_short_description(sharing_as_top(_)) = "t".
sharing_as_short_description(sharing_as_real_as(SharingSet)) =
    string.from_int(sharing_set_size(SharingSet)).

    % inproject = projection such that result contains information about
    %   selection of variables only.
    %
    % outproject = projection such that result contains information about
    %   all variables _except_ the selection of variables.
    %
:- type projection_type
    --->    inproject
    ;       outproject.

sharing_as_project(ListVars, !SharingAs) :-
    sharing_as_project_with_type(inproject, ListVars, !SharingAs).
sharing_as_project(ListVars, SharingAs) = NewSharingAs :-
    sharing_as_project(ListVars, SharingAs, NewSharingAs).

:- pred sharing_as_project_with_type(projection_type::in, prog_vars::in,
    sharing_as::in, sharing_as::out) is det.

sharing_as_project_with_type(ProjectionType, ListVars, !SharingAs) :-
    (
        !.SharingAs = sharing_as_bottom
    ;
        !.SharingAs = sharing_as_real_as(SharingSet0),
        sharing_set_project(ProjectionType, ListVars, SharingSet0, SharingSet),
        !:SharingAs = wrap(SharingSet)
    ;
        !.SharingAs = sharing_as_top(_)
    ).

sharing_as_project_set(SetVars, !SharingAs) :-
    sharing_as_project(set.to_sorted_list(SetVars), !SharingAs).

sharing_as_rename(MapVar, TypeSubst, !SharingAs) :-
    (
        !.SharingAs = sharing_as_real_as(SharingSet0),
        sharing_set_rename(MapVar, TypeSubst, SharingSet0, SharingSet),
        !:SharingAs = sharing_as_real_as(SharingSet)
    ;
        !.SharingAs = sharing_as_bottom
    ;
        !.SharingAs = sharing_as_top(_)
    ).

sharing_as_rename_using_module_info(ModuleInfo, PPId, ActualVars, ActualTypes,
        CallerTypeVarSet, CallerExternalTypeParams,
        FormalSharing, ActualSharing) :-
    VarRenaming = get_variable_renaming(ModuleInfo, PPId, ActualVars),
    TypeSubst = get_type_substitution(ModuleInfo, PPId, ActualTypes,
        CallerTypeVarSet, CallerExternalTypeParams),
    sharing_as_rename(VarRenaming, TypeSubst, FormalSharing, ActualSharing).

sharing_as_comb(ModuleInfo, ProcInfo, NewSharing, OldSharing) =
        ResultSharing :-
    (
        NewSharing = sharing_as_real_as(NewSharingSet),
        (
            OldSharing = sharing_as_real_as(OldSharingSet),
            promise_equivalent_solutions [MaybeExcp] (
                try(
                    ( pred(CombSet::out) is det :-
                        CombSet = sharing_set_comb(ModuleInfo, ProcInfo,
                            NewSharingSet, OldSharingSet)
                    ), MaybeExcp)
            ),
            (
                MaybeExcp = succeeded(SharingSet),
                ResultSharing = wrap(SharingSet)
            ;
                MaybeExcp = exception(Excp),
                ( if univ_to_type(Excp, encounter_existential_subtype) then
                    Reason = top_cannot_improve("existential subtype"),
                    ResultSharing = sharing_as_top_sharing(Reason)
                else
                    rethrow(MaybeExcp)
                )
            )
        ;
            OldSharing = sharing_as_bottom,
            ResultSharing = NewSharing
        ;
            OldSharing = sharing_as_top(_),
            ResultSharing = OldSharing
        )
    ;
        NewSharing = sharing_as_bottom,
        ResultSharing = OldSharing
    ;
        NewSharing = sharing_as_top(MsgNew),
        ( if OldSharing = sharing_as_top(MsgOld) then
            ResultSharing = sharing_as_top(set.union(MsgNew, MsgOld))
        else
            ResultSharing = NewSharing
        )
    ).

add_unify_sharing(ModuleInfo, ProcInfo, Unification, GoalInfo, OldSharing)
        = NewSharing :-
    UnifSharing = sharing_from_unification(ModuleInfo, ProcInfo, Unification,
        GoalInfo),
    ResultSharing = sharing_as_comb(ModuleInfo, ProcInfo,
        UnifSharing, OldSharing),
    %
    % When the unification is a construction unification, some local variables
    % may become totally useless for the rest of the code (deaths), and so any
    % structure sharing involving these variables may safely be removed.
    %
    % NOTE: this "useless" sharing information can not be removed earlier as
    % it can contribute to new sharing with the comb operation.
    %
    ( if Unification = construct(_, _, _, _, _, _, _) then
        NewSharing = optimization_remove_deaths(ProcInfo,
            GoalInfo, ResultSharing)
    else
        NewSharing = ResultSharing
    ).

    % Describe the sharing that is created between the variables involved in
    % unification.
    %
:- func sharing_from_unification(module_info, proc_info, unification,
    hlds_goal_info) = sharing_as.

sharing_from_unification(ModuleInfo, ProcInfo, Unification, GoalInfo)
        = Sharing :-
    (
        Unification = construct(Var, ConsId, Args0, _, _, _, _),
        ( if var_needs_sharing_analysis(ModuleInfo, ProcInfo, Var) then
            list.drop_while(is_introduced_typeinfo_arg(ProcInfo), Args0,
                Args),
            number_args(Args, NumberedArgs),
            some [!SharingSet] (
                !:SharingSet = sharing_set_init,
                list.foldl(
                    add_var_arg_sharing(ModuleInfo, ProcInfo, Var, ConsId),
                    NumberedArgs, !SharingSet),
                create_internal_sharing(ModuleInfo, ProcInfo, Var, ConsId,
                    NumberedArgs, !SharingSet),
                Sharing = wrap(!.SharingSet)
            )
        else
            Sharing = sharing_as_init
        )
    ;
        Unification = deconstruct(Var, ConsId, Args0, _, _, _),
        list.drop_while(is_introduced_typeinfo_arg(ProcInfo), Args0,
            Args),
        number_args(Args, NumberedArgs),
        optimize_for_deconstruct(GoalInfo, NumberedArgs, ReducedNumberedArgs),
        some [!SharingSet] (
            !:SharingSet = sharing_set_init,
            list.foldl(add_var_arg_sharing(ModuleInfo, ProcInfo, Var, ConsId),
                ReducedNumberedArgs, !SharingSet),
            Sharing = wrap(!.SharingSet)
        )
    ;
        Unification = assign(X, Y),
        ( if var_needs_sharing_analysis(ModuleInfo, ProcInfo, X) then
            new_entry(ModuleInfo, ProcInfo,
                datastruct_init(X) - datastruct_init(Y),
                sharing_set_init, SharingSet),
            Sharing = wrap(SharingSet)
        else
            Sharing = sharing_as_init
        )
    ;
        Unification = simple_test(_, _),
        Sharing = sharing_as_init
    ;
        Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated_unify")
    ).

:- pred is_introduced_typeinfo_arg(proc_info::in, prog_var::in) is semidet.

is_introduced_typeinfo_arg(ProcInfo, Var) :-
    proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    is_introduced_type_info_type(Type).

:- pred number_args(prog_vars::in, list(pair(int, prog_var))::out) is det.

number_args(Args, NumberedArgs) :-
    NumberArg =
        ( pred(A::in, AP::out, !.N::in, !:N::out) is det :-
            AP = !.N - A,
            !:N = !.N + 1
        ),
    list.map_foldl(NumberArg, Args, NumberedArgs, 1, _).

:- pred add_var_arg_sharing(module_info::in, proc_info::in, prog_var::in,
    cons_id::in, pair(int, prog_var)::in,
    sharing_set::in, sharing_set::out) is det.

add_var_arg_sharing(ModuleInfo, ProcInfo, Var, ConsId, N - Arg, !Sharing) :-
    ( if var_needs_sharing_analysis(ModuleInfo, ProcInfo, Arg) then
        Data1 = datastruct_init_with_pos(Var, ConsId, N),
        Data2 = datastruct_init(Arg),
        new_entry(ModuleInfo, ProcInfo, Data1 - Data2, !Sharing)
    else
        true
    ).

    % When two positions within the constructed term refer to the same
    % variable, this must be recorded as an extra sharing pair.
    % E.g.: X = f(Y,Y), then the sharing between f/1 and f/2 must be recorded.
    % XXX Different implementation!
    %
:- pred create_internal_sharing(module_info::in, proc_info::in,
    prog_var::in, cons_id::in, list(pair(int, prog_var))::in,
    sharing_set::in, sharing_set::out) is det.

create_internal_sharing(ModuleInfo, ProcInfo, Var, ConsId, NumberedArgs,
        !Sharing) :-
    % For every argument and the occurrence of the variable of that argument
    % in the rest of the arguments, add the appropriate sharing pair.
    (
        NumberedArgs = [First | Remainder],
        First = Pos1 - Var1,
        AddPair =
            ( pred(OtherNumberedArg::in, !.Sharing::in, !:Sharing::out)
                    is det :-
                ( if OtherNumberedArg = Pos2 - Var1 then
                    % Create sharing between Pos1 and Pos2
                    Data1 = datastruct_init_with_pos(Var, ConsId, Pos1),
                    Data2 = datastruct_init_with_pos(Var, ConsId, Pos2),
                    new_entry(ModuleInfo, ProcInfo, Data1 - Data2, !Sharing)
                else
                    true
                )
            ),
        list.foldl(AddPair, Remainder, !Sharing),
        create_internal_sharing(ModuleInfo, ProcInfo, Var, ConsId, Remainder,
            !Sharing)
    ;
        NumberedArgs = []
    ).

    % For deconstructions, a huge optimization can be made by avoiding the
    % construction of sharing between variables that are not used in the
    % remainder of the code anyway. The set of used args is known as the
    % pre-birth set as computed by the liveness-pass (liveness.m).
    %
    % XXX Why was the original implementation so complicated?
    %
:- pred optimize_for_deconstruct(hlds_goal_info::in,
    list(pair(int, prog_var))::in, list(pair(int, prog_var))::out) is det.

optimize_for_deconstruct(GoalInfo, !NumberedArgs) :-
    hlds_llds.goal_info_get_pre_births(GoalInfo, PreBirthSet),
    IsPreBirthArg =
        ( pred(NumberedArg::in) is semidet :-
            Var = snd(NumberedArg),
            set_of_var.member(PreBirthSet, Var)
        ),
    list.filter(IsPreBirthArg, !NumberedArgs).

:- func optimization_remove_deaths(proc_info, hlds_goal_info,
    sharing_as) = sharing_as.

optimization_remove_deaths(ProcInfo, GoalInfo, Sharing0) = Sharing :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    HeadVarsSet = set_of_var.list_to_set(HeadVars),
    goal_info_get_post_deaths(GoalInfo, Deaths0),

    % Make sure to keep all the information about the headvars,
    % even if they are in the post deaths set.
    set_of_var.difference(Deaths0, HeadVarsSet, Deaths),
    DeathsList = set_of_var.to_sorted_list(Deaths),
    sharing_as_project_with_type(outproject, DeathsList, Sharing0, Sharing).

add_foreign_proc_sharing(ModuleInfo, PredInfo, ProcInfo, ForeignPPId,
        Attributes, Args, GoalContext, OldSharing, NewSharing) :-
    ForeignSharing = sharing_as_for_foreign_proc(ModuleInfo,
        Attributes, ForeignPPId, GoalContext),

    ActualVars = list.map(foreign_arg_var, Args),
    proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
    lookup_var_types(VarTypes, ActualVars, ActualTypes),
    pred_info_get_typevarset(PredInfo, CallerTypeVarSet),
    pred_info_get_external_type_params(PredInfo, CallerExternalTypeParams),

    % XXX We should pass VarTypes instead of ActualTypes.
    sharing_as_rename_using_module_info(ModuleInfo, ForeignPPId,
        ActualVars, ActualTypes, CallerTypeVarSet,
        CallerExternalTypeParams, ForeignSharing, ActualSharing),

    NewSharing = sharing_as_comb(ModuleInfo, ProcInfo, ActualSharing,
        OldSharing).

:- func sharing_as_for_foreign_proc(module_info,
    pragma_foreign_proc_attributes, pred_proc_id, prog_context) = sharing_as.

sharing_as_for_foreign_proc(ModuleInfo, Attributes, ForeignPPId,
        ProgContext) = SharingAs :-
    ( if
        sharing_as_from_user_annotated_sharing(Attributes, SharingAs0)
    then
        SharingAs = SharingAs0
    else if
        predict_called_pred_is_bottom(ModuleInfo, ForeignPPId)
    then
        SharingAs = sharing_as_bottom
    else
        context_to_string(ProgContext, ContextString),
        Msg = "foreign proc with unknown sharing ("
            ++ ContextString ++ ")",
        SharingAs = sharing_as_top_sharing(top_cannot_improve(Msg))
    ).

:- pred sharing_as_from_user_annotated_sharing(
    pragma_foreign_proc_attributes::in, sharing_as::out) is semidet.

sharing_as_from_user_annotated_sharing(Attributes, UserSharingAs) :-
    UserSharing = get_user_annotated_sharing(Attributes),
    UserSharing = user_sharing(SharingDomain, _MaybeTypes),
    % Accept only the value "bottom" and "real" for the structure sharing.
    % If the user has annotated the sharing with unknown sharing, we might
    % try to predict bottom anyway.
    some [!SharingAs] (
        (
            SharingDomain = structure_sharing_bottom,
            !:SharingAs = sharing_as_bottom
        ;
            SharingDomain = structure_sharing_real(_SharingPairs),
            !:SharingAs = from_structure_sharing_domain(SharingDomain)

            % XXX
            % I have the feeling that renaming should not be needed at this
            % place anymore, assuming that every foreign_proc call is
            % correctly handled at the add_pragma stage?
        ),
        UserSharingAs = !.SharingAs
    ).

sharing_as_is_subsumed_by(ModuleInfo, ProcInfo, Sharing1, Sharing2) :-
    (
        Sharing2 = sharing_as_top(_)
    ;
        Sharing1 = sharing_as_bottom
    ;
        Sharing1 = sharing_as_real_as(SharingSet1),
        Sharing2 = sharing_as_real_as(SharingSet2),
        sharing_set_is_subsumed_by(ModuleInfo, ProcInfo, SharingSet1,
            SharingSet2)
    ).

sharing_as_and_status_is_subsumed_by(ModuleInfo, ProcInfo,
        SharingAs_Status1, SharingAs_Status2) :-
    SharingAs_Status1 = sharing_as_and_status(Sharing1, _Status1),
    SharingAs_Status2 = sharing_as_and_status(Sharing2, _Status2),
    sharing_as_is_subsumed_by(ModuleInfo, ProcInfo, Sharing1, Sharing2).
    % XXX do we need to compare Status1 and Status2?

sharing_as_least_upper_bound(ModuleInfo, ProcInfo, Sharing1, Sharing2)
        = Sharing :-
    (
        Sharing1 = sharing_as_bottom,
        Sharing = Sharing2
    ;
        Sharing1 = sharing_as_top(Msg1),
        ( if Sharing2 = sharing_as_top(Msg2) then
            Sharing = sharing_as_top(set.union(Msg1, Msg2))
        else
            Sharing = Sharing1
        )
    ;
        Sharing1 = sharing_as_real_as(SharingSet1),
        (
            Sharing2 = sharing_as_bottom,
            Sharing = Sharing1
        ;
            Sharing2 = sharing_as_top(_),
            Sharing = Sharing2
        ;
            Sharing2 = sharing_as_real_as(SharingSet2),
            promise_equivalent_solutions [MaybeExcp] (
                try(
                    ( pred(SharingSet3::out) is det :-
                        SharingSet3 = sharing_set_least_upper_bound(ModuleInfo,
                            ProcInfo, SharingSet1, SharingSet2)
                    ), MaybeExcp)
            ),
            (
                MaybeExcp = succeeded(SharingSet),
                Sharing = sharing_as_real_as(SharingSet)
            ;
                MaybeExcp = exception(Excp),
                ( if univ_to_type(Excp, encounter_existential_subtype) then
                    Reason = top_cannot_improve("existential subtype"),
                    Sharing = sharing_as_top_sharing(Reason)
                else
                    rethrow(MaybeExcp)
                )
            )
        )
    ).

sharing_as_least_upper_bound_of_list(ModuleInfo, ProcInfo, SharingList) =
    list.foldl(sharing_as_least_upper_bound(ModuleInfo, ProcInfo), SharingList,
        sharing_as_init).

extend_datastruct(ModuleInfo, ProcInfo, SharingAs, Datastruct)
        = Datastructures :-
    (
        SharingAs = sharing_as_bottom,
        Datastructures = [Datastruct]
    ;
        SharingAs = sharing_as_real_as(SharingSet),
        Datastructures = sharing_set_extend_datastruct(ModuleInfo, ProcInfo,
            Datastruct, SharingSet)
    ;
        SharingAs = sharing_as_top(_),
        unexpected($pred, "top sharing set")
    ).

extend_datastructs(ModuleInfo, ProcInfo, SharingAs, Datastructs)
        = ExtendedDatastructs :-
    DataLists = list.map(extend_datastruct(ModuleInfo, ProcInfo,
        SharingAs), Datastructs),
    ExtendedDatastructs = list.foldl(
        datastruct_lists_least_upper_bound(ModuleInfo, ProcInfo),
        DataLists, []).

apply_widening(ModuleInfo, ProcInfo, WideningLimit, WideningDone, !Sharing):-
    (
        !.Sharing = sharing_as_bottom,
        WideningDone = no
    ;
        !.Sharing = sharing_as_top(_),
        WideningDone = no
    ;
        !.Sharing = sharing_as_real_as(SharingSet0),
        ( if WideningLimit = 0 then
            WideningDone = no
        else if WideningLimit > sharing_set_size(SharingSet0) then
            WideningDone = no
        else
            sharing_set_apply_widening(ModuleInfo, ProcInfo,
                SharingSet0, SharingSet),
            !:Sharing = sharing_as_real_as(SharingSet),
            WideningDone = yes
        )
    ).

from_structure_sharing_domain(SharingDomain) = SharingAs :-
    (
        SharingDomain = structure_sharing_bottom,
        SharingAs = sharing_as_bottom
    ;
        SharingDomain = structure_sharing_real(StructureSharing),
        SharingSet = from_sharing_pair_list(StructureSharing),
        wrap(SharingSet, SharingAs)
    ;
        SharingDomain = structure_sharing_top(Reasons),
        SharingAs = sharing_as_top(Reasons)
    ).

to_structure_sharing_domain(SharingAs) = SharingDomain :-
    (
        SharingAs = sharing_as_bottom,
        SharingDomain = structure_sharing_bottom
    ;
        SharingAs = sharing_as_real_as(SharingSet),
        SharingDomain = structure_sharing_real(
            to_sharing_pair_list(SharingSet))
    ;
        SharingAs = sharing_as_top(Msgs),
        SharingDomain = structure_sharing_top(Msgs)
    ).

%---------------------------------------------------------------------------%
%
% sharing_as_table
%

sharing_as_table_init = map.init.

sharing_as_table_search(PPId, Table, SharingAs_Status) :-
    map.search(Table, PPId, SharingAs_Status).

sharing_as_table_set(PPId, SharingAs_Status, !Table) :-
    !Table ^ elem(PPId) := SharingAs_Status.

%---------------------------------------------------------------------------%

lookup_sharing_and_comb(ModuleInfo, PredInfo, ProcInfo, SharingTable,
        PredId, ProcId, ActualVars, !Sharing):-
    PPId = proc(PredId, ProcId),

    % XXX do we need to combine the analysis status of sharing information we
    % use with the analysis status of direct and indirect analyses?
    lookup_sharing_or_predict(ModuleInfo, SharingTable, PPId, FormalSharing,
        _Status, _IsPredicted),

    proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
    lookup_var_types(VarTypes, ActualVars, ActualTypes),

    pred_info_get_typevarset(PredInfo, CallerTypeVarSet),
    pred_info_get_univ_quant_tvars(PredInfo, CallerExternalTypeParams),
    % XXX We should pass VarTypes instead of ActualTypes.
    sharing_as_rename_using_module_info(ModuleInfo, PPId,
        ActualVars, ActualTypes, CallerTypeVarSet, CallerExternalTypeParams,
        FormalSharing, ActualSharing),

    !:Sharing = sharing_as_comb(ModuleInfo, ProcInfo,
        ActualSharing, !.Sharing).

lookup_sharing_or_predict(ModuleInfo, SharingTable, PPId, SharingAs, Status,
        IsPredicted) :-
    ( if
        % look up in SharingTable
        sharing_as_table_search(PPId, SharingTable,
            sharing_as_and_status(SharingAs0, Status0))
    then
        SharingAs = SharingAs0,
        Status = Status0,
        IsPredicted = no
    else if
        % or predict bottom sharing
        %
        % If it is neither in the fixpoint table, nor in the sharing
        % table, then this means that we have never analysed the called
        % procedure, yet in some cases we can still simply predict that
        % the sharing the called procedure creates is bottom.
        predict_called_pred_is_bottom(ModuleInfo, PPId)
    then
        SharingAs = sharing_as_init,
        Status = optimal,
        IsPredicted = yes
    else if
        PPId = proc(PredId, _),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_status(PredInfo, PredStatus),
        PredStatus = pred_status(status_external(_))
    then
        SharingAs = sharing_as_top_sharing(top_cannot_improve(
            "external predicate")),
        Status = optimal,
        IsPredicted = no
    else
        % or use top-sharing with appropriate message.
        SharingAs = top_sharing_not_found(PPId),
        Status = optimal,
        IsPredicted = no
    ).

:- pred predict_called_pred_is_bottom(module_info::in, pred_proc_id::in)
    is semidet.

predict_called_pred_is_bottom(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    (
        % 1. inferred determinism is erroneous/failure.
        proc_info_get_inferred_determinism(ProcInfo, Determinism),
        (
            Determinism = detism_erroneous
        ;
            Determinism = detism_failure
        )
    ;
        % 2. bottom_sharing_is_safe_approximation
        bottom_sharing_is_safe_approximation(ModuleInfo, PredInfo, ProcInfo)
    ;
        % 3. call to a compiler generate special predicate:
        % "unify", "index", "compare" or "initialise".
        pred_info_get_origin(PredInfo, Origin),
        Origin = origin_special_pred(_, _)
    ).

:- func top_sharing_not_found(pred_proc_id) = sharing_as.

top_sharing_not_found(PPId) = TopSharing :-
    ShroudedPredProcId = shroud_pred_proc_id(PPId),
    Reason = top_failed_lookup(ShroudedPredProcId),
    TopSharing = sharing_as_top_sharing(Reason).

%---------------------------------------------------------------------------%

bottom_sharing_is_safe_approximation(ModuleInfo, PredInfo, ProcInfo) :-
    (
        % Generated special predicates don't introduce sharing.
        pred_info_get_origin(PredInfo, Origin),
        Origin = origin_special_pred(_, _)
    ;
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_argmodes(ProcInfo, Modes),
        proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
        lookup_var_types(VarTypes, HeadVars, Types),
        bottom_sharing_is_safe_approximation_by_args(ModuleInfo, Modes, Types)
    ).

bottom_sharing_is_safe_approximation_by_args(ModuleInfo, Modes, Types) :-
    ModeTypePairs = assoc_list.from_corresponding_lists(Modes, Types),
    Test =
        ( pred(Pair::in) is semidet :-
            Pair = Mode - Type,

            % Mode is not unique nor clobbered.
            mode_get_insts(ModuleInfo, Mode, _LeftInst, RightInst),
            not inst_is_unique(ModuleInfo, RightInst),
            not inst_is_clobbered(ModuleInfo, RightInst),

            % Mode is output.
            mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
            TopFunctorMode = top_out,

            % Type is one which we care about for structure sharing/reuse.
            type_needs_sharing_analysis(ModuleInfo, Type)
        ),
    list.filter(Test, ModeTypePairs, TrueModeTypePairs),
    TrueModeTypePairs = [].

%---------------------------------------------------------------------------%
% Type: sharing_set.
% Definition of the (hidden) representation for lists of sharing data
% structures.
% XXX The definition and implementation of sharing_set should be in a separate
% (sub)module. Yet this gave compilation errors. To do.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Theoretically, a structure sharing set is a set of structure sharing
% pairs, i.e., pairs of data structures reflecting the possible sharing between
% them. However, lookup operations occur often, and have a dramatic performance
% using this simple representation. We therefore use a hierarchy of different
% map-types to represent a structure sharing set.
%
% Hence: In the sharing_set we map a prog_var to a description of the
% structure sharing pairs it is involved in. These structure sharing pairs
% are represented as a map mapping selectors on data structure sets.
% Suppose (Sel-Data) is in the map to which prog_var V relates to, then this
% means that the memory space for V ^ Sel might share with the memory spaces
% used for each of the data structures in Data.
% Note that in the sharing_set type we explicitly keep track of the number of
% structure sharing pairs at each level of the map.
%
% Example:
% A set of one sharing pair expressing that the memory space
% for the term of a variable X might be the same as the memory space used
% to store the term of a variable Y:
% * Plain representation would be: { (X^\epsilon - Y^\epsilon) } -- where
%   \epsilon stands for the empty selector in this formulation.
% * Optimised representation:
%   sharing_set(
%       2,      % number of structure sharing pairs in the
%               % sharing set. If X-Y is sharing, then also
%               % Y-X, hence in this case "2".
%           {
%           X -> selector_sharing_set(
%               1,      % number of data structures with
%                   % which terms of X share in this set.
%               {  \epsilon -> Y^\epsilon }
%           )
%           Y -> selector_sharing_set(
%               1,
%               { \epsilon -> X^\epsilon }
%           )
%           }
%   )
%---------------------------------------------------------------------------%
%

:- type sharing_set
    --->    sharing_set(
                int,    % number of structure sharing pairs represented
                        % by the set.
                map(prog_var, selector_sharing_set)
            ).

:- type selector_sharing_set
    --->    selector_sharing_set(
                int,    % number of data structures represented by this
                        % set.
                map(selector, data_set)
            ).

:- type data_set
    --->    datastructures(
                int,    % size of the set of data structures.
                set(datastruct)
            ).

%---------------------------------------------------------------------------%
%
% sharing_set predicates/functions
%

:- func sharing_set_init = sharing_set is det.

sharing_set_init = sharing_set(0, map.init).

:- pred sharing_set_is_empty(sharing_set::in) is semidet.

sharing_set_is_empty(sharing_set(0, _Map)).

:- func sharing_set_size(sharing_set) = int.

sharing_set_size(sharing_set(Size, _)) = Size.

:- pred wrap(sharing_set::in, sharing_as::out) is det.

wrap(SharingSet, SharingAs) :-
    ( if sharing_set_is_empty(SharingSet) then
        SharingAs = sharing_as_bottom
    else
        SharingAs = sharing_as_real_as(SharingSet)
    ).

:- func wrap(sharing_set) = sharing_as.

wrap(SharingSet) = SharingAs :-
    wrap(SharingSet, SharingAs).

:- pred sharing_set_project(projection_type::in, prog_vars::in,
    sharing_set::in, sharing_set::out) is det.

sharing_set_project(ProjectionType, Vars, SharingSet0, SharingSet) :-
    SharingSet0 = sharing_set(_, Map0),
    (
        ProjectionType = inproject,
        map.select(Map0, set.list_to_set(Vars), Map)
    ;
        ProjectionType = outproject,
        map.keys(Map0, AllVars),
        set.difference(set.list_to_set(AllVars), set.list_to_set(Vars),
            Remainder),
        map.select(Map0, Remainder, Map)
    ),
    map.foldl(project_and_update_sharing_set(ProjectionType, Vars),
        Map, sharing_set_init, SharingSet).

:- pred project_and_update_sharing_set(projection_type::in, prog_vars::in,
    prog_var::in, selector_sharing_set::in, sharing_set::in, sharing_set::out)
    is det.

project_and_update_sharing_set(ProjectionType, Vars, Var, SelSet0, !SS) :-
    selector_sharing_set_project(ProjectionType, Vars, SelSet0, SelSet),
    ( if selector_sharing_set_is_empty(SelSet) then
        true
    else
        !.SS = sharing_set(Size0, M0),
        map.det_insert(Var, SelSet, M0, M),
        Size = Size0 + selector_sharing_set_size(SelSet),
        !:SS = sharing_set(Size, M)
    ).

:- pred sharing_set_rename(prog_var_renaming::in, tsubst::in,
    sharing_set::in, sharing_set::out) is det.

sharing_set_rename(Dict, TypeSubst, SharingSet0, SharingSet) :-
    SharingSet0 = sharing_set(Size, Map0),
    map.foldl(do_sharing_set_rename(Dict, TypeSubst), Map0, map.init, Map),
    SharingSet  = sharing_set(Size, Map).

:- pred do_sharing_set_rename(prog_var_renaming::in, tsubst::in,
    prog_var::in, selector_sharing_set::in,
    map(prog_var, selector_sharing_set)::in,
    map(prog_var, selector_sharing_set)::out) is det.

do_sharing_set_rename(Dict, TypeSubst, Var0, SelectorSet0, !Map) :-
    selector_sharing_set_rename(Dict, TypeSubst, SelectorSet0, SelectorSet1),
    map.lookup(Dict, Var0, Var),
    % Two variables can be renamed to the same new variable,
    % e.g. append(X, X, Y).
    ( if map.search(!.Map, Var, SelectorSet2) then
        selector_sharing_set_add(SelectorSet1, SelectorSet2, SelectorSet),
        map.det_update(Var, SelectorSet, !Map)
    else
        map.det_insert(Var, SelectorSet1, !Map)
    ).

    % The implementation for combining sharing sets is to compute the
    % alternating closure of those sets.
    % Adapted copy/paste of the definition as given in Phd Nancy (definition
    % 6.31):
    % Let X, Y be sets of pairs of elements of any kind, then
    % altclos(X, Y) = { (a_0, a_n) |
    %   a sequence (a_0, a_1).(a_1, a_2). ... .(a_(n_1),a_n) can be constructed
    %   where adjoining pairs stem from either X or Y, i.e., if (a_i, a_(i+1))
    %   \in X, then (a_(i+1), a_(i+2)) \from Y.
    %
    % Although the theoretic definition is commutative, the implementation uses
    % some of the known properties of existings sharing and additional sharing,
    % which simplifies the implementation, yet makes it non-commutative.
    % Instead of computing needless chains of sharing pairs, the alternating
    % closure of a NewSharingSet, and an OldSharingSet, is equivalent to the
    % least upper bound of the sets (Corollary 10.1):
    %   * NewSharingSet
    %   * OldSharingSet
    %   * sharing pairs formed using paths of length two (direction irrelevant)
    %   * sharing pairs formed using paths of length three N - O - N, where
    %   N are sharing pairs from NewSharingSet, and O is a sharing pair from
    %   OldSharingSet.
    %
:- func sharing_set_comb(module_info, proc_info, sharing_set, sharing_set) =
    sharing_set.

sharing_set_comb(ModuleInfo, ProcInfo, NewSharingSet, OldSharingSet)
        = ResultSharingSet :-
    % paths of length 2:
    OldNewSharingSet = sharing_set_altclos_2(ModuleInfo, ProcInfo,
        NewSharingSet, OldSharingSet),
    % paths of length 3:
    NewOldNewSharingSet = sharing_set_altclos_3_directed(ModuleInfo, ProcInfo,
        NewSharingSet, OldSharingSet),
    % combine it all:
    ResultSharingSet = sharing_set_least_upper_bound_list(ModuleInfo, ProcInfo,
        [NewSharingSet, OldSharingSet, OldNewSharingSet], NewOldNewSharingSet).

    % SharingSet1 is subsumed by SharingSet2 iff every sharing pair
    % represented by SharingSet1 is subsumed by any of the sharing pairs
    % represented by SharingSet2.
    % XXX For the moment SharingSet1 is converted to a list of sharing pairs,
    % and each of the sharing pairs is checked for subsumption. This should be
    % optimised to follow the inner structure of sharing_set instead.
    %
:- pred sharing_set_is_subsumed_by(module_info::in, proc_info::in,
    sharing_set::in, sharing_set::in) is semidet.

sharing_set_is_subsumed_by(ModuleInfo, ProcInfo, SharingSet1, SharingSet2):-
    list.all_true(sharing_set_subsumes_sharing_pair(ModuleInfo, ProcInfo,
        SharingSet2), to_sharing_pair_list(SharingSet1)).

:- func sharing_set_least_upper_bound(module_info, proc_info,
    sharing_set, sharing_set) = sharing_set.

sharing_set_least_upper_bound(ModuleInfo, ProcInfo, Set1, Set2) = Union :-
    % Folding over the map could be done, but the union is easier to describe
    % by picking each of the sharing pairs represented by the smallest sharing
    % set, and adding them to the other sharing set.
    Set1 = sharing_set(Size1, _),
    Set2 = sharing_set(Size2, _),
    ( if Size1 < Size2 then
        Pairs = to_sharing_pair_list(Set1),
        Set = Set2
    else
        Pairs = to_sharing_pair_list(Set2),
        Set = Set1
    ),
    new_entries(ModuleInfo, ProcInfo, Pairs, Set, Union).

:- func sharing_set_extend_datastruct(module_info, proc_info, datastruct,
    sharing_set) = list(datastruct).

sharing_set_extend_datastruct(ModuleInfo, ProcInfo, Datastruct, SharingSet)
        = [Datastruct | Datastructures] :-
    SharingSet = sharing_set(_, SharingMap),
    Var = Datastruct ^ sc_var,
    Selector = Datastruct ^ sc_selector,
    ( if map.search(SharingMap, Var, SelectorSet) then
        % The type of the variable is needed to be able to compare
        % datastructures.
        proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
        lookup_var_type(VarTypes, Var, VarType),
        Datastructures = selector_sharing_set_extend_datastruct(ModuleInfo,
            ProcInfo, VarType, Selector, SelectorSet)
    else
        Datastructures = []
    ).

:- pred sharing_set_apply_widening(module_info::in, proc_info::in,
    sharing_set::in, sharing_set::out) is det.

sharing_set_apply_widening(ModuleInfo, ProcInfo, !SharingSet):-
    !.SharingSet = sharing_set(_, SharingMap0),
    map.map_foldl(
        selector_sharing_set_apply_widening(ModuleInfo, ProcInfo),
        SharingMap0, SharingMap, 0, NewSize),
    !:SharingSet = sharing_set(NewSize, SharingMap).

    % Conversion between list of sharing data structures to
    % sharing_set's and vice versa.
    %
    % NOTE: from_sharing_pair_list assumes that the sharing set is minimal, ie,
    % there are no two sharing pairs in that set such that one sharing pair
    % subsumes the other pair.
    %
:- func from_sharing_pair_list(structure_sharing) = sharing_set.

from_sharing_pair_list(SharingPairs) = SharingSet :-
    list.foldl(new_entry_no_controls, SharingPairs, sharing_set_init,
        SharingSet).

    % Add a new sharing pair into the existing sharing set. This operation
    % takes into account subsumption:
    % * if the new sharing pair is subsumed by any of the sharing already
    % present in the set, then this operation is a null operation.
    % * if the new sharing pair subsumes any of the existing sharing, then
    % all the subsumed sharing pairs must be removed.
    %
    % XXX
    % Due to the above checks, this operation may become quite costly. To
    % verify!
    %
:- pred new_entry(module_info::in, proc_info::in, structure_sharing_pair::in,
    sharing_set::in, sharing_set::out) is det.

new_entry(ModuleInfo, ProcInfo, SharingPair0, !SharingSet) :-
    SharingPair0 = DataX0 - DataY0,
    % Normalize the sharing pair before doing anything.
    DataX = normalize_datastruct(ModuleInfo, ProcInfo, DataX0),
    DataY = normalize_datastruct(ModuleInfo, ProcInfo, DataY0),
    SharingPair = DataX - DataY,
    ( if
        (
            % Ignore sharing pairs which are exactly the same.
            DataX = DataY
        ;
            sharing_set_subsumes_sharing_pair(ModuleInfo, ProcInfo,
                !.SharingSet, SharingPair)
        )
    then
        true
    else
        sharing_set_subsumed_subset(ModuleInfo, ProcInfo,
            !.SharingSet, SharingPair, SubsumedPairs),
        % For any two pairs (A,B) and (B,A) keep only one pair in the list.
        % Otherwise we'll get an assertion failure in `remove_entries' when we
        % try to remove (B,A) after having removed (A,B).
        remove_swapped_dup_pairs(SubsumedPairs, [], SubsumedPairsNoDups),
        remove_entries(SubsumedPairsNoDups, !SharingSet),
        new_entry_no_controls(SharingPair, !SharingSet)
    ).

:- pred new_entry_no_controls(structure_sharing_pair::in,
    sharing_set::in, sharing_set::out) is det.

new_entry_no_controls(SharingPair, !SS) :-
    SharingPair = Data1 - Data2,
    new_directed_entry(Data1, Data2, !SS),
    ( if datastruct_equal(Data1, Data2) then
        true
    else
        new_directed_entry(Data2, Data1, !SS)
    ).

:- pred remove_entries(structure_sharing::in,
    sharing_set::in, sharing_set::out) is det.

remove_entries(SharingPairs, !SS):-
    list.foldl(remove_entry, SharingPairs, !SS).

    % Remove a structure sharing pair that is known to be explicitly
    % represented in the sharing set.
    % Software error if the sharing pair is not part of the set.
    %
:- pred remove_entry(structure_sharing_pair::in,
    sharing_set::in, sharing_set::out) is det.

remove_entry(SharingPair, !SharingSet) :-
    SharingPair = Data1 - Data2,
    remove_directed_entry(Data1, Data2, !SharingSet),
    ( if datastruct_equal(Data1, Data2) then
        true
    else
        remove_directed_entry(Data2, Data1, !SharingSet)
    ).

:- pred remove_directed_entry(datastruct::in, datastruct::in,
    sharing_set::in, sharing_set::out) is det.

remove_directed_entry(FromData, ToData, SharingSet0, SharingSet) :-
    FromVar = FromData ^ sc_var,
    FromSel = FromData ^ sc_selector,

    SharingSet0 = sharing_set(Size0, SharingMap0),
    map.lookup(SharingMap0, FromVar, SelSharingSet0),
    SelSharingSet0 = selector_sharing_set(SelSize0, SelSharingMap0),
    map.lookup(SelSharingMap0, FromSel, DataSet0),
    DataSet0 = datastructures(DataSize0, Data0),
    ( if set.remove(ToData, Data0, Data) then
        DataSize = DataSize0 - 1,
        SelSize = SelSize0 - 1,
        Size = Size0 - 1,

        ( if Size = 0 then
            SharingSet = sharing_set(Size, map.init)
        else if SelSize = 0 then
            map.delete(FromVar, SharingMap0, SharingMap),
            SharingSet = sharing_set(Size, SharingMap)
        else if DataSize = 0 then
            map.delete(FromSel, SelSharingMap0, SelSharingMap),
            SelSharingSet = selector_sharing_set(SelSize, SelSharingMap),
            map.det_update(FromVar, SelSharingSet, SharingMap0, SharingMap),
            SharingSet = sharing_set(Size, SharingMap)
        else
            DataSet = datastructures(DataSize, Data),
            map.det_update(FromSel, DataSet, SelSharingMap0, SelSharingMap),
            SelSharingSet = selector_sharing_set(SelSize, SelSharingMap),
            map.det_update(FromVar, SelSharingSet, SharingMap0, SharingMap),
            SharingSet = sharing_set(Size, SharingMap)
        )
    else
        unexpected($pred, "removing non-existant sharing pair")
    ).

:- func to_sharing_pair_list(sharing_set) = structure_sharing.

to_sharing_pair_list(SharingSet0) = SharingPairs :-
    SharingSet = without_doubles(SharingSet0),
    SharingSet = sharing_set(_, SharingMap),
    map.foldl(to_sharing_pair_list_2, SharingMap, [], SharingPairs).

:- pred to_sharing_pair_list_2(prog_var::in, selector_sharing_set::in,
    structure_sharing::in, structure_sharing::out) is det.

to_sharing_pair_list_2(ProgVar, SelSharingSet, !Pairs) :-
    SelSharingSet = selector_sharing_set(_, SelectorMap),
    map.foldl(to_sharing_pair_list_3(ProgVar), SelectorMap, !Pairs).

:- pred to_sharing_pair_list_3(prog_var::in, selector::in, data_set::in,
    structure_sharing::in, structure_sharing::out) is det.

to_sharing_pair_list_3(ProgVar, Selector, Dataset, !Pairs) :-
    Dataset = datastructures(_, Datastructs),
    set.fold(to_sharing_pair_list_4(ProgVar, Selector), Datastructs, !Pairs).

:- pred to_sharing_pair_list_4(prog_var::in, selector::in, datastruct::in,
    structure_sharing::in, structure_sharing::out) is det.

to_sharing_pair_list_4(ProgVar, Selector, Datastruct, !Pairs) :-
    SharingPair =
        Datastruct - datastruct_init_with_selector(ProgVar, Selector),
    !:Pairs = [SharingPair | !.Pairs].

%---------------------------------------------------------------------------%

    % XXX new implementation.
    %
:- func sharing_set_altclos_2(module_info, proc_info, sharing_set,
    sharing_set) = sharing_set.

sharing_set_altclos_2(ModuleInfo, ProcInfo, NewSharingSet, OldSharingSet)
        = ResultSharingSet :-
    NewSharingSet = sharing_set(_, NewMap),
    OldSharingSet = sharing_set(_, OldMap),

    % compute common vars:
    map.keys(NewMap, NewVars),
    map.keys(OldMap, OldVars),
    set.list_to_set(NewVars, NewVarsSet),
    set.list_to_set(OldVars, OldVarsSet),
    set.intersect(NewVarsSet, OldVarsSet, CommonVarsSet),
    set.to_sorted_list(CommonVarsSet, CommonVars),

    % downsize the maps to contain only the keys regarding the common vars:
    % XXX Is this really an optimisation?
    map.select(NewMap, CommonVarsSet, NewMap1),
    map.select(OldMap, CommonVarsSet, OldMap1),

    proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
    %
    % for each common var V, compute the sharing pairs A-B, such that
    % \exists X where var(X) = V, and X-A \in NewSharingSet, and X-B \in
    % OldSharingSet.
    %
    list.foldl(
        ( pred(Var::in, !.SS::in, !:SS::out) is det :-
            lookup_var_type(VarTypes, Var, Type),
            map.lookup(NewMap1, Var, NewSelSet),
            map.lookup(OldMap1, Var, OldSelSet),
            SharingPairs = selector_sharing_set_altclos(ModuleInfo, ProcInfo,
                Type, NewSelSet, OldSelSet),
            new_entries(ModuleInfo, ProcInfo, SharingPairs, !SS)
        ), CommonVars, sharing_set_init, ResultSharingSet).

    % sharing_set_altclos_3_directed(ModuleInfo, ProcInfo, NewSharingSet,
    %   OldSharingSet) =
    % Compute the sharing pairs A-B such that exists X-Y \in OldSharingSet and
    %   A - X \subsumed by NewSharingSet,
    %   Y - B \subsumed by NewSharingSet.
    % XXX New implementation.
    %
:- func sharing_set_altclos_3_directed(module_info, proc_info,
    sharing_set, sharing_set) = sharing_set.

sharing_set_altclos_3_directed(ModuleInfo, ProcInfo,
        NewSharingSet, OldSharingSet) = ResultSharingSet :-
    NewSharingSet = sharing_set(_, NewMap),
    OldSharingSet = sharing_set(_, OldMap),

    % compute common vars:
    map.keys(NewMap, NewVars),
    map.keys(OldMap, OldVars),
    set.list_to_set(NewVars, NewVarsSet),
    set.list_to_set(OldVars, OldVarsSet),
    set.intersect(NewVarsSet, OldVarsSet, CommonVarsSet),
    set.to_sorted_list(CommonVarsSet, CommonVars),

    % downsize OldSharingSet to contain only information regarding the
    % common vars, hence a full projection...
    sharing_set_project(inproject, CommonVars,
        OldSharingSet, OldSharingSetProjected),
    % As each and every pair within OldMapProjected needs to be looked at,
    % we might as well use the full list representation:
    OldSharingPairs = to_sharing_pair_list(OldSharingSetProjected),
    %
    % Now for each sharing pair X-Y represented by OldMapProjected, find
    % all the datastructures in NewSharingMap that cover X, find
    % all the datastructures in NewSharingMap that cover Y, and compute
    % the crossproduct of both sets returning the set of new sharing pairs.
    %
    list.foldl(
        pred(X - Y::in, SS0::in, SS::out) is det :-
        (
            ExtendedX = sharing_set_extend_datastruct(ModuleInfo, ProcInfo, X,
                NewSharingSet),
            ExtendedY = sharing_set_extend_datastruct(ModuleInfo, ProcInfo, Y,
                NewSharingSet),
            set_cross_product(set.list_to_set(ExtendedX),
                set.list_to_set(ExtendedY), SetPairs),
            new_entries(ModuleInfo, ProcInfo,
                set.to_sorted_list(SetPairs), SS0, SS)
        ),
        OldSharingPairs,
        sharing_set_init,
        ResultSharingSet).

:- func sharing_set_least_upper_bound_list(module_info, proc_info,
    list(sharing_set), sharing_set) = sharing_set.

sharing_set_least_upper_bound_list(ModuleInfo, ProcInfo, ListSharingSet,
        SharingSet0) =
    list.foldl(sharing_set_least_upper_bound(ModuleInfo, ProcInfo),
        ListSharingSet, SharingSet0).

    % Determine if the sharing set subsumes the sharing information
    % represented by the structure sharing pair.
    % This means: \exists A-B \in SharingSet, such that A-B is more general
    % than the given structure sharing pair.
    %
:- pred sharing_set_subsumes_sharing_pair(module_info::in, proc_info::in,
    sharing_set::in, structure_sharing_pair::in) is semidet.

sharing_set_subsumes_sharing_pair(ModuleInfo, ProcInfo, SharingSet,
        SharingPair):-
    SharingSet = sharing_set(_, SharingMap),

    SharingPair = Data1 - Data2,
    Data1 = selected_cel(Var1, Sel1),
    Data2 = selected_cel(Var2, Sel2),
    trace [
        compile_time(flag("check_selector_normalization")),
        run_time(env("check_selector_normalization"))
    ] (
        check_normalized(ModuleInfo, Type1, Sel1),
        check_normalized(ModuleInfo, Type2, Sel2)
    ),

    proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
    lookup_var_type(VarTypes, Var1, Type1),
    lookup_var_type(VarTypes, Var2, Type2),

    map.search(SharingMap, Var1, SelSharingSet),
    SelSharingSet = selector_sharing_set(_, SelSharingMap),
    map.keys(SelSharingMap, SelectorList),

    % Find at least one selector in SelectorList that is more general
    % than Sel1 (with a specific extension), and whose associated dataset
    % contains at least one datastructure that is more general than Data2
    % (with that same extension).
    some [Sel] (
        list.member(Sel, SelectorList),
        trace [
            compile_time(flag("check_selector_normalization")),
            run_time(env("check_selector_normalization"))
        ] (
            check_normalized(ModuleInfo, Type1, Sel)
        ),
        selector_subsumed_by(ModuleInfo, already_normalized,
            Sel1, Sel, Type1, Extension),
        map.search(SelSharingMap, Sel, datastructures(_, DatastructureSet)),

        some [Datastructure] (
            set.member(Datastructure, DatastructureSet),
            Var2 = Datastructure ^ sc_var,
            DatastructureSel = Datastructure ^ sc_selector,
            trace [
                compile_time(flag("check_selector_normalization")),
                run_time(env("check_selector_normalization"))
            ] (
                check_normalized(ModuleInfo, Type2, DatastructureSel)
            ),
            selector_subsumed_by(ModuleInfo, already_normalized,
                Sel2, DatastructureSel, Type2, Extension)
        )
    ).

    % Return the list of sharing pairs included in the sharing set that are
    % less or equal to the given sharing pair.
    %
:- pred sharing_set_subsumed_subset(module_info::in, proc_info::in,
    sharing_set::in, structure_sharing_pair::in, structure_sharing::out)
    is det.

sharing_set_subsumed_subset(ModuleInfo, ProcInfo, SharingSet, SharingPair,
        SubsumedPairs) :-
    SharingSet = sharing_set(_, SharingMap),

    SharingPair = Data1 - Data2,
    Data1 = selected_cel(Var1, Sel1),
    Data2 = selected_cel(Var2, Sel2),
    trace [
        compile_time(flag("check_selector_normalization")),
        run_time(env("check_selector_normalization"))
    ] (
        check_normalized(ModuleInfo, Type1, Sel1),
        check_normalized(ModuleInfo, Type2, Sel2)
    ),

    proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
    lookup_var_type(VarTypes, Var1, Type1),
    lookup_var_type(VarTypes, Var2, Type2),

    ( if map.search(SharingMap, Var1, SelSharingSet) then
        SelSharingSet = selector_sharing_set(_, SelSharingMap),

        % Determine all Selector-Dataset pairs where
        %   * Selector is less or equal to Sel1 wrt some extension E,
        %   * Dataset is a set of datastructures less or equal to Data2
        %     (wrt the same extension E).
        map.keys(SelSharingMap, SelectorList),
        list.filter_map(
            ( pred(Selector::in, SPairs::out) is semidet :-
                trace [
                    compile_time(flag("check_selector_normalization")),
                    run_time(env("check_selector_normalization"))
                ] (
                    check_normalized(ModuleInfo, Type1, Selector)
                ),
                selector_subsumed_by(ModuleInfo, already_normalized,
                    Selector, Sel1, Type1, Extension),
                map.search(SelSharingMap, Selector, Dataset),
                Dataset = datastructures(_, Datastructs),
                list.filter_map(
                    ( pred(D::in, Pair::out) is semidet :-
                        Var2 = D ^ sc_var,
                        DSel = D ^ sc_selector,
                        trace [
                            compile_time(flag("check_selector_normalization")),
                            run_time(env("check_selector_normalization"))
                        ] (
                            check_normalized(ModuleInfo, Type2, DSel)
                        ),
                        selector_subsumed_by(ModuleInfo, already_normalized,
                            DSel, Sel2, Type2, Extension),
                        Pair = datastruct_init_with_selector(Var1, Selector)
                            - D
                    ),
                    to_sorted_list(Datastructs),
                    SPairs),
                SPairs = [_ | _]
            ),
            SelectorList,
            ListSubsumedPairs),
        list.condense(ListSubsumedPairs, SubsumedPairs)
    else
        SubsumedPairs = []
    ).

:- pred remove_swapped_dup_pairs(list(structure_sharing_pair)::in,
    list(structure_sharing_pair)::in, list(structure_sharing_pair)::out)
    is det.

remove_swapped_dup_pairs([], Acc, Acc).
remove_swapped_dup_pairs([H | T], Acc0, Acc) :-
    H = A - B,
    ( if list.member(B - A, Acc0) then
        remove_swapped_dup_pairs(T, Acc0, Acc)
    else
        remove_swapped_dup_pairs(T, [H | Acc0], Acc)
    ).

:- pred check_normalized(module_info::in, mer_type::in, selector::in) is det.

check_normalized(ModuleInfo, Type, Sel) :-
    normalize_selector_with_type_information(ModuleInfo, Type, Sel, Norm),
    ( if Sel = Norm then
        true
    else
        unexpected($pred, "unnormalized selector")
    ).

:- pred new_entries(module_info::in, proc_info::in, structure_sharing::in,
    sharing_set::in, sharing_set::out) is det.

new_entries(ModuleInfo, ProcInfo, SharingPairs, !SS) :-
    list.foldl(new_entry(ModuleInfo, ProcInfo), SharingPairs, !SS).

:- pred new_directed_entry(datastruct::in, datastruct::in,
    sharing_set::in, sharing_set::out) is det.

new_directed_entry(FromData, ToData, SharingSet0, SharingSet):-
    SharingSet0 = sharing_set(Size0, Map0),
    Var = FromData ^ sc_var,
    Selector = FromData ^ sc_selector,
    ( if map.search(Map0, Var, Selectors0) then
        ( if
            selector_sharing_set_new_entry(Selector, ToData,
                Selectors0, Selectors)
        then
            map.det_update(Var, Selectors, Map0, Map),
            Size = Size0 + 1
        else
            Map = Map0,
            Size = Size0
        )
    else
        ( if
            selector_sharing_set_new_entry(Selector, ToData,
                selector_sharing_set_init, Selectors)
        then
            map.det_insert(Var, Selectors, Map0, Map),
            Size = Size0 + 1
        else
            unexpected($pred, "impossible option")
        )
    ),
    SharingSet = sharing_set(Size, Map).

:- func without_doubles(sharing_set) = sharing_set.

without_doubles(SharingSet0) = SharingSet :-
    SharingSet0 = sharing_set(_, Map0),
    map.foldl(without_doubles_2, Map0, sharing_set_init, SharingSet).

:- pred without_doubles_2(prog_var::in, selector_sharing_set::in,
    sharing_set::in, sharing_set::out) is det.

without_doubles_2(ProgVar, SelectorSS, !SS) :-
    SelectorSS = selector_sharing_set(_, SelMap),
    map.foldl(without_doubles_3(ProgVar), SelMap, !SS).

:- pred without_doubles_3(prog_var::in, selector::in, data_set::in,
    sharing_set::in, sharing_set::out) is det.

without_doubles_3(ProgVar, Selector, DataSet, !SS) :-
    DataSet = datastructures(_, DS),
    set.fold(without_doubles_4(ProgVar, Selector), DS, !SS).

:- pred without_doubles_4(prog_var::in, selector::in, datastruct::in,
    sharing_set::in, sharing_set::out) is det.

without_doubles_4(ProgVar, Selector, Datastruct, !SS) :-
    ( if
        directed_entry_is_member(
            datastruct_init_with_selector(ProgVar, Selector),
            Datastruct, !.SS)
    then
        true
    else
        new_directed_entry(Datastruct,
            datastruct_init_with_selector(ProgVar, Selector), !SS)
    ).

:- pred directed_entry_is_member(datastruct::in, datastruct::in,
    sharing_set::in) is semidet.

directed_entry_is_member(FromData, ToData, SharingSet) :-
    SharingSet = sharing_set(_, Map),
    Var = FromData ^ sc_var,
    Selector = FromData ^ sc_selector,
    map.search(Map, Var, SelSharingSet),
    SelSharingSet = selector_sharing_set(_, SelectorMap),
    map.search(SelectorMap, Selector, Dataset),
    Dataset = datastructures(_, Datastructures),
    set.member(ToData, Datastructures).

%---------------------------------------------------------------------------%
%
% selector_sharing_set predicates/functions
%

:- func selector_sharing_set_init = selector_sharing_set.

selector_sharing_set_init = selector_sharing_set(0, map.init).

:- pred selector_sharing_set_is_empty(selector_sharing_set::in) is semidet.

selector_sharing_set_is_empty(selector_sharing_set(0, _Map)).

:- func selector_sharing_set_size(selector_sharing_set) = int.

selector_sharing_set_size(selector_sharing_set(Size, _)) = Size.

:- pred selector_sharing_set_project(projection_type::in, prog_vars::in,
    selector_sharing_set::in, selector_sharing_set::out) is det.

selector_sharing_set_project(ProjectionType, Vars,
        SelSharingSet0, SelSharingSet):-
    SelSharingSet0 = selector_sharing_set(_, Map0),
    map.foldl(selector_sharing_set_project_2(ProjectionType, Vars),
        Map0, selector_sharing_set_init, SelSharingSet).

:- pred selector_sharing_set_project_2(projection_type::in, prog_vars::in,
    selector::in, data_set::in, selector_sharing_set::in,
    selector_sharing_set::out) is det.

selector_sharing_set_project_2(ProjectionType, Vars, Selector, DataSet0, !SS):-
    data_set_project(ProjectionType, Vars, DataSet0, DataSet),
    ( if data_set_is_empty(DataSet) then
        true
    else
        !.SS = selector_sharing_set(Size0, Map0),
        map.det_insert(Selector, DataSet, Map0, Map),
        Size = Size0 + data_set_size(DataSet),
        !:SS = selector_sharing_set(Size, Map)
    ).

:- pred selector_sharing_set_rename(prog_var_renaming::in,
    tsubst::in, selector_sharing_set::in, selector_sharing_set::out) is det.

selector_sharing_set_rename(Dict, Subst, SelSharingSet0, SelSharingSet):-
    SelSharingSet0 = selector_sharing_set(Size, Map0),
    map.foldl(selector_sharing_set_rename_2(Dict, Subst), Map0, map.init, Map),
    SelSharingSet = selector_sharing_set(Size, Map).

:- pred selector_sharing_set_rename_2(prog_var_renaming::in,
    tsubst::in, selector::in, data_set::in,
    map(selector, data_set)::in, map(selector, data_set)::out) is det.

selector_sharing_set_rename_2(Dict, Subst, Selector0, DataSet0, !Map) :-
    rename_selector(Subst, Selector0, Selector),
    data_set_rename(Dict, Subst, DataSet0, DataSet),
    ( if map.search(!.Map, Selector, DataSetOld) then
        % This can happen if Subst maps two different type variables to the
        % same type.
        data_set_add(DataSet, DataSetOld, CombinedDataSet),
        map.set(Selector, CombinedDataSet, !Map)
    else
        map.det_insert(Selector, DataSet, !Map)
    ).

:- pred selector_sharing_set_add(selector_sharing_set::in,
    selector_sharing_set::in, selector_sharing_set::out) is det.

selector_sharing_set_add(SelectorSetA, SelectorSetB, SelectorSet):-
    SelectorSetA = selector_sharing_set(_, MapA),
    SelectorSetB = selector_sharing_set(_, MapB),
    map.foldl(selector_sharing_set_add_2, MapA, MapB, Map),
    map.foldl(sum_data_set_sizes, Map, 0, Size),
    SelectorSet = selector_sharing_set(Size, Map).

:- pred selector_sharing_set_add_2(selector::in, data_set::in,
    map(selector, data_set)::in, map(selector, data_set)::out) is det.

selector_sharing_set_add_2(Sel, DataSet0, !Map) :-
    ( if map.search(!.Map, Sel, DataSet1) then
        data_set_add(DataSet0, DataSet1, DataSet),
        map.det_update(Sel, DataSet, !Map)
    else
        map.det_insert(Sel, DataSet0, !Map)
    ).

    % selector_sharing_set_new_entry(Selector, Datastruct, SS0, SS):
    % Adds Datastruct into SS0 using Selector as a key. Fails if that
    % Datastructs is already present with that selector.
    %
:- pred selector_sharing_set_new_entry(selector::in, datastruct::in,
    selector_sharing_set::in, selector_sharing_set::out) is semidet.

selector_sharing_set_new_entry(Selector, Datastruct,
        SelSharingSet0, SelSharingSet) :-
    SelSharingSet0 = selector_sharing_set(Size0, Map0),
    ( if map.search(Map0, Selector, DataSet0) then
        data_set_new_entry(Datastruct, DataSet0, DataSet),
        Size = Size0 + 1,
        map.det_update(Selector, DataSet, Map0, Map)
    else
        data_set_new_entry(Datastruct, data_set_init, DataSet),
        Size = Size0 + 1,
        map.det_insert(Selector, DataSet, Map0, Map)
    ),
    SelSharingSet = selector_sharing_set(Size, Map).

:- func selector_sharing_set_altclos(module_info, proc_info, mer_type,
    selector_sharing_set, selector_sharing_set) = structure_sharing.

selector_sharing_set_altclos(ModuleInfo, ProcInfo, Type, NewSelSet, OldSelSet)
        = NewSharingPairs :-
    NewSelSet = selector_sharing_set(_, NewMap),
    OldSelSet = selector_sharing_set(_, OldMap),
    % Get the selectors.
    map.keys(NewMap, NewSelectors),
    map.keys(OldMap, OldSelectors),
    % For each selector in NewSelectors, verify each selector in OldSelector,
    % if either one is less or equal to the other, then generate the structure
    % sharing pair as appropriate.
    NewSharingPairs = list.condense(list.map(
        selector_sharing_set_altclos_2(ModuleInfo, ProcInfo, Type, NewMap,
            OldMap, OldSelectors),
        NewSelectors)).

:- func selector_sharing_set_altclos_2(module_info, proc_info, mer_type,
    map(selector, data_set), map(selector, data_set), list(selector),
    selector) = structure_sharing.

selector_sharing_set_altclos_2(ModuleInfo, ProcInfo, Type, NewMap, OldMap,
        OldSelectors, NewSel) = SharingPairs :-
    map.lookup(NewMap, NewSel, NewSelDataSet),
    SharingPairs = list.condense(list.map(
        selector_sharing_set_altclos_3(ModuleInfo, ProcInfo, Type,
            NewSelDataSet, OldMap, NewSel),
        OldSelectors)).

:- func selector_sharing_set_altclos_3(module_info, proc_info, mer_type,
    data_set, map(selector, data_set), selector, selector) = structure_sharing.

selector_sharing_set_altclos_3(ModuleInfo, ProcInfo, Type, NewSelDataSet,
        OldMap, NewSel, OldSel) = SharingPairs :-
    map.lookup(OldMap, OldSel, OldSelDataSet),
    SharingPairs = basic_closure(ModuleInfo, ProcInfo, Type, NewSelDataSet,
        OldSelDataSet, NewSel, OldSel).

:- func selector_sharing_set_extend_datastruct(module_info, proc_info,
    mer_type, selector, selector_sharing_set) = list(datastruct).

selector_sharing_set_extend_datastruct(ModuleInfo, ProcInfo, VarType, Selector,
        SelectorSharingSet) = Datastructures :-
    SelectorSharingSet = selector_sharing_set(_, SelectorMap),
    DoExtend = selector_sharing_set_extend_datastruct_2(ModuleInfo, ProcInfo,
        VarType, Selector),
    Datastructures0 = map.map_values(DoExtend, SelectorMap),
    Datastructures = list.condense(map.values(Datastructures0)).

:- func selector_sharing_set_extend_datastruct_2(module_info, proc_info,
    mer_type, selector, selector, data_set) = list(datastruct).

selector_sharing_set_extend_datastruct_2(ModuleInfo, ProcInfo, VarType,
        BaseSelector, Selector, Dataset0) = Datastructures :-
    % If Selector is more general than BaseSelector, i.e.
    % BaseSelector = Selector.Extension, apply this extension
    % to all the datastructs associated with Selector, and add them
    % to the set of datastructs collected.
    ( if
        selector_subsumed_by(ModuleInfo, need_normalization,
            BaseSelector, Selector, VarType, Extension)
    then
        data_set_termshift(ModuleInfo, ProcInfo, Dataset0, Extension, Dataset),
        Dataset = datastructures(_, Data),
        Datastructures = set.to_sorted_list(Data)
    else
        Datastructures = []
    ).

:- pred selector_sharing_set_apply_widening(module_info::in, proc_info::in,
    prog_var::in, selector_sharing_set::in, selector_sharing_set::out,
    int::in, int::out) is det.

selector_sharing_set_apply_widening(ModuleInfo, ProcInfo, ProgVar,
        !SelectorSharingSet, !SharingSetSize):-
    !.SelectorSharingSet = selector_sharing_set(_, DataMap0),
    map.foldl2(
        selector_sharing_set_apply_widening_2(ModuleInfo, ProcInfo, ProgVar),
        DataMap0, map.init, DataMap, 0, DataMapSize),
    !:SharingSetSize = !.SharingSetSize + DataMapSize,
    !:SelectorSharingSet = selector_sharing_set(DataMapSize, DataMap).

:- pred selector_sharing_set_apply_widening_2(module_info::in, proc_info::in,
    prog_var::in, selector::in, data_set::in, map(selector, data_set)::in,
    map(selector, data_set)::out, int::in, int::out) is det.

selector_sharing_set_apply_widening_2(ModuleInfo, ProcInfo, ProgVar,
        Selector, DataSet0, !DataMap, !DataMapSize) :-
    % Widening of the associated datastructures.
    data_set_apply_widening(ModuleInfo, ProcInfo, DataSet0, DataSet1),

    % Widening of the ProgVar-Selector datastructure.
    datastruct_apply_widening(ModuleInfo, ProcInfo,
        datastruct_init_with_selector(ProgVar, Selector), NewDataStruct),
    NewSelector = NewDataStruct ^ sc_selector,

    % Remove any occurrence of ProgVar-NewSelector in the data set, i.e. before
    % widening the left- and right-hand sides of a sharing pair were different,
    % but after widening they became identical.
    data_set_delete_entry(NewDataStruct, DataSet1, DataSet2),

    % Check if NewSelector is already in the resulting DataMap, if so,
    % compute the least upper bound of the associated data_set's.
    ( if map.search(!.DataMap, NewSelector, ExistingDataSet) then
        ExistingDataSetSize = data_set_size(ExistingDataSet),
        DataSetFinal = data_set_least_upper_bound(ModuleInfo, ProcInfo,
            DataSet2, ExistingDataSet),
        DataSetFinalSize = data_set_size(DataSetFinal),
        map.det_update(NewSelector, DataSetFinal, !DataMap),
        !:DataMapSize = !.DataMapSize - ExistingDataSetSize + DataSetFinalSize
    else
        map.det_insert(NewSelector, DataSet2, !DataMap),
        !:DataMapSize = !.DataMapSize + data_set_size(DataSet2)
    ).

:- pred sum_data_set_sizes(selector::in, data_set::in,
    int::in, int::out) is det.

sum_data_set_sizes(_, DataSet, Size0, Size) :-
    Size = Size0 + data_set_size(DataSet).

:- func basic_closure(module_info, proc_info, mer_type,
    data_set, data_set, selector, selector) = structure_sharing.

basic_closure(ModuleInfo, ProcInfo, Type, NewDataSet, OldDataSet,
        NewSel, OldSel) = SharingPairs :-
    % three cases:
    % 1. NewSel <= OldSel then generate sharing pairs.
    % 2. OldSel <= NewSel then generate sharing pairs.
    % 3. NewSel and OldSet not comparable then no new sharing pairs.

    ( if
        % NewSel <= OldSel ie, \exists Extension: OldSel.Extension = NewSel.
        selector_subsumed_by(ModuleInfo, already_normalized,
            NewSel, OldSel, Type, Extension)
    then
        data_set_termshift(ModuleInfo, ProcInfo, OldDataSet, Extension,
            TermShiftedOldDataSet),
        SharingPairs = data_set_directed_closure(TermShiftedOldDataSet,
            NewDataSet)
    else if
        % OldSel <= NewSel ie, \exists Extension: NewSel.Extension = OldSel.
        selector_subsumed_by(ModuleInfo, already_normalized,
            OldSel, NewSel, Type, Extension)
    then
        data_set_termshift(ModuleInfo, ProcInfo, NewDataSet, Extension,
            TermShiftedNewDataSet),
        SharingPairs = data_set_directed_closure(TermShiftedNewDataSet,
            OldDataSet)
    else
        % uncomparable
        SharingPairs = []
    ).

%---------------------------------------------------------------------------%
%
% data_set predicates/functions
%

:- func data_set_init = data_set.

data_set_init = datastructures(0, set.init).

:- pred data_set_is_empty(data_set::in) is semidet.

data_set_is_empty(datastructures(0, _Set)).

:- func data_set_size(data_set) = int.

data_set_size(datastructures(Size, _)) = Size.

:- pred data_set_project(projection_type::in, prog_vars::in,
    data_set::in, data_set::out) is det.

data_set_project(ProjectionType, Vars, !DataSet) :-
    data_set_filter(data_set_project_test(ProjectionType, Vars), !DataSet).

:- pred data_set_rename(prog_var_renaming::in, tsubst::in,
    data_set::in, data_set::out) is det.

data_set_rename(Dict, Subst, !DataSet) :-
    !.DataSet = datastructures(_Size, Datastructs0),
    Datastructs = set.map(rename_datastruct(Dict, Subst), Datastructs0),
    !:DataSet = datastructures(set.count(Datastructs), Datastructs).

:- pred data_set_termshift(module_info::in, proc_info::in, data_set::in,
    selector::in, data_set::out) is det.

data_set_termshift(ModuleInfo, ProcInfo, DataSet0, Selector, DataSet) :-
    DataSet0 = datastructures(Size, Set0),
    Set = set.map(datastruct_termshift(ModuleInfo, ProcInfo, Selector), Set0),
    DataSet = datastructures(Size, Set).

:- pred data_set_add(data_set::in, data_set::in, data_set::out) is det.

data_set_add(DataSetA, DataSetB, DataSet) :-
    DataSetA = datastructures(_, DataA),
    DataSetB = datastructures(_, DataB),
    Data = set.union(DataA, DataB),
    DataSet = datastructures(set.count(Data), Data).

:- pred data_set_new_entry(datastruct::in,
    data_set::in, data_set::out) is semidet.

data_set_new_entry(Datastruct, DataSet0, DataSet) :-
    DataSet0 = datastructures(Size0, Datastructs0),
    not set.member(Datastruct, Datastructs0),
    set.insert(Datastruct, Datastructs0, Datastructs),
    Size = Size0 + 1,
    DataSet = datastructures(Size, Datastructs).

:- pred data_set_delete_entry(datastruct::in,
    data_set::in, data_set::out) is det.

data_set_delete_entry(Datastruct, DataSet0, DataSet) :-
    DataSet0 = datastructures(Size0, Datastructs0),
    ( if set.contains(Datastructs0, Datastruct) then
        set.delete(Datastruct, Datastructs0, Datastructs),
        Size = Size0 - 1,
        DataSet = datastructures(Size, Datastructs)
    else
        DataSet = DataSet0
    ).

:- func data_set_directed_closure(data_set, data_set) = structure_sharing.

data_set_directed_closure(FromData, ToData) = SharingPairs :-
    FromData = datastructures(_, DataSet1),
    ToData = datastructures(_, DataSet2),
    set_cross_product(DataSet1, DataSet2, SetOfPairs),
    set.to_sorted_list(SetOfPairs, SharingPairs).

:- pred data_set_apply_widening(module_info::in, proc_info::in,
    data_set::in, data_set::out) is det.

data_set_apply_widening(ModuleInfo, ProcInfo, !DataSet):-
    % XXX ProcInfo could be replaced by a mercury type, as all the
    % datastructures within one single dataset have the same type
    % (as they are sharing with one and the same datastruct, hence,
    % they must have the same type as that datastruct).
    !.DataSet = datastructures(_, Datastructs0),
    set.fold(data_set_widen_and_add(ModuleInfo, ProcInfo),
        Datastructs0, set.init, Datastructs),
    !:DataSet = datastructures(set.count(Datastructs), Datastructs).

:- pred data_set_widen_and_add(module_info::in, proc_info::in, datastruct::in,
    set(datastruct)::in, set(datastruct)::out) is det.

data_set_widen_and_add(ModuleInfo, ProcInfo, Data0, !Datastructs):-
    datastruct_apply_widening(ModuleInfo, ProcInfo, Data0, Data),
    data_set_add_datastruct(ModuleInfo, ProcInfo, Data, !Datastructs).

:- func data_set_least_upper_bound(module_info, proc_info,
    data_set, data_set) = data_set.

data_set_least_upper_bound(ModuleInfo, ProcInfo, DataSet1, DataSet2)
        = DataSet :-
    DataSet1 = datastructures(_, Datastructs1),
    DataSet2 = datastructures(_, Datastructs2),
    set.fold(data_set_add_datastruct(ModuleInfo, ProcInfo),
        Datastructs1, Datastructs2, Datastructs),
    DataSet = datastructures(set.count(Datastructs), Datastructs).

:- pred data_set_add_datastruct(module_info::in, proc_info::in,
    datastruct::in, set(datastruct)::in, set(datastruct)::out) is det.

data_set_add_datastruct(ModuleInfo, ProcInfo, Data, !Datastructs) :-
    ( if
        % Perform the simple check of exact occurrence in the set first...
        set.member(Data, !.Datastructs)
    then
        true
    else if
        datastruct_subsumed_by_list(ModuleInfo, ProcInfo, Data,
            set.to_sorted_list(!.Datastructs))
    then
        true
    else
        set.insert(Data, !Datastructs)
    ).

:- pred data_set_project_test(projection_type::in, prog_vars::in,
    datastruct::in) is semidet.

data_set_project_test(ProjectionType, Vars, Data) :-
    Var = Data ^ sc_var,
    (
        ProjectionType = inproject,
        list.member(Var, Vars)
    ;
        ProjectionType = outproject,
        not list.member(Var, Vars)
    ).

:- pred set_cross_product(set(datastruct)::in, set(datastruct)::in,
    set(pair(datastruct, datastruct))::out) is det.

set_cross_product(SetA, SetB, CrossProduct):-
    solutions_set(cross_product(SetA, SetB), CrossProduct).

:- pred cross_product(set(datastruct)::in, set(datastruct)::in,
    pair(datastruct, datastruct)::out) is nondet.

cross_product(SetA, SetB, Pair) :-
    set.member(ElemA, SetA),
    set.member(ElemB, SetB),
    Pair = ElemA - ElemB.

:- pred data_set_filter(pred(datastruct)::in(pred(in) is semidet),
    data_set::in, data_set::out) is det.

data_set_filter(Pred, !DataSet) :-
    !.DataSet = datastructures(_, Datastructs0),
    Datastructs = set.filter(Pred, Datastructs0),
    !:DataSet = datastructures(set.count(Datastructs), Datastructs).

%---------------------------------------------------------------------------%

load_structure_sharing_table(ModuleInfo) = SharingTable :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    list.foldl(load_structure_sharing_table_2(ModuleInfo), PredIds,
        sharing_as_table_init, SharingTable).

:- pred load_structure_sharing_table_2(module_info::in, pred_id::in,
    sharing_as_table::in, sharing_as_table::out) is det.

load_structure_sharing_table_2(ModuleInfo, PredId, !SharingTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_procids(PredInfo),
    list.foldl(load_structure_sharing_table_3(ModuleInfo, PredId),
        ProcIds, !SharingTable).

:- pred load_structure_sharing_table_3(module_info::in, pred_id::in,
    proc_id::in, sharing_as_table::in, sharing_as_table::out) is det.

load_structure_sharing_table_3(ModuleInfo, PredId, ProcId, !SharingTable) :-
    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_structure_sharing(ProcInfo, MaybePublicSharing),
    (
        MaybePublicSharing = yes(
            structure_sharing_domain_and_status(PublicSharing, Status)),
        PPId = proc(PredId, ProcId),
        PrivateSharing = from_structure_sharing_domain(PublicSharing),
        sharing_as_table_set(PPId,
            sharing_as_and_status(PrivateSharing, Status), !SharingTable)
    ;
        MaybePublicSharing = no
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_sharing.domain.
%---------------------------------------------------------------------------%
