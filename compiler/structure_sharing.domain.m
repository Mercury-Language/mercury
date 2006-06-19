%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%   * extend_datastruct: compute the set of datastructures referring to the
%     same memory space as a given datastructure, using sharing information;
%     needed by the reuse analysis to check whether a given datastructure is
%     the last pointer to the memory space it refers to.
%
%   * conversion operations between the public and private representation
%     for sharing sets.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_sharing.domain.
:- interface.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

    % The hidden representation for structure sharing.
    %
:- type sharing_as.

    % Operations w.r.t. the "bottom" element of the lattice.
    %
:- func sharing_as_init = sharing_as.
:- pred sharing_as_is_bottom(sharing_as::in) is semidet.

    % Operations w.r.t. the "top" element of the lattice. When sharing
    % becomes top, it is useful to know why it has become top. This can
    % be recorded and passed to the top-value as a string.
    %
:- func sharing_as_top_sharing(string) = sharing_as.
:- func sharing_as_top_sharing_accumulate(string, sharing_as) = sharing_as.
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
    %   ActualVars, ActualTypes, ActualTVarset, FormalSharing, ActualSharing):
    %
    % Renaming of the formal description of data structure sharing to the
    % actual description of the sharing. The information about the formal
    % variables needs to be extracted from the module information. 
    % A list of variables and types is used as the actual variables and types.
    % The type variables set in the actual context must also be specified.
    % 
    %
:- pred sharing_as_rename_using_module_info(module_info::in,
    pred_proc_id::in, prog_vars::in, list(mer_type)::in, tvarset::in,
    sharing_as::in, sharing_as::out) is det.

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

    % XXX Not yet implemented.
% :- func add_foreign_code_sharing(module_info, pred_proc_id, goal_info
    % pragma_foreign_proc_attributes, list(foreign_arg),
    % sharing_as) = sharing_as.

    % Compare two sharing sets. A sharing set Set1 is subsumed by a sharing set
    % Set2 iff the total set of sharing represented by Set1 is a subset of the
    % total set of sharing represented by Set2. This means that for every
    % structure sharing pair in Set1, there exists a structure sharing pair P2
    % in Set2, such that P1 is subsumed by P2 (i.e. P2 at least describes the
    % same sharing as P1).
    %
:- pred sharing_as_is_subsumed_by(module_info::in, proc_info::in,
    sharing_as::in, sharing_as::in) is semidet.

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

%-----------------------------------------------------------------------------%
%
% Sharing table
%

% This table is used to temporarily record the sharing analysis results,
% instead of saving in the HLDS and having to continuously convert between the
% public and private representation of structure sharing.

    % Mapping between pred_proc_ids and sharing information that has been
    % derived for the corresponding procedure definitions.
    %
:- type sharing_as_table == map(pred_proc_id, sharing_as).

    % Initialisation.
    %
:- func sharing_as_table_init = sharing_as_table.

    % Look up the sharing information of a specific procedure. Fail if the
    % procedure id is not in the map.
    %
:- func sharing_as_table_search(pred_proc_id, sharing_as_table)
    = sharing_as is semidet.

    % Set the sharing information for a given pred_proc_id.
    %
:- pred sharing_as_table_set(pred_proc_id::in, sharing_as::in,
    sharing_as_table::in, sharing_as_table::out) is det.

%-----------------------------------------------------------------------------%

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
    %
:- pred lookup_sharing_or_predict(module_info::in, sharing_as_table::in,
    pred_proc_id::in, sharing_as::out) is det.

    % Succeeds if the sharing of a procedure can safely be approximated by
    % "bottom", simply by looking at the modes and types of the arguments.
    %
:- pred bottom_sharing_is_safe_approximation(module_info::in,
    proc_info::in) is semidet.

    % Load all the structure sharing information present in the HLDS into
    % a sharing table. 
    %
:- func load_structure_sharing_table(module_info) = sharing_as_table.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_llds.
:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.selector.
:- import_module transform_hlds.ctgc.util.

:- import_module assoc_list.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type sharing_as
    --->    sharing_as_real_as(sharing_set)
    ;       sharing_as_bottom
    ;       sharing_as_top(list(string)).

sharing_as_init = sharing_as_bottom.
sharing_as_is_bottom(sharing_as_bottom).
sharing_as_top_sharing(Msg) = sharing_as_top([Msg]).
sharing_as_top_sharing_accumulate(Msg, SharingAs) = TopSharing :-
    (
        SharingAs = sharing_as_real_as(_),
        Msgs = [Msg]
    ;
        SharingAs = sharing_as_bottom,
        Msgs = [Msg]
    ;
        SharingAs = sharing_as_top(Msgs0),
        list.cons(Msg, Msgs0, Msgs)
    ),
    TopSharing = sharing_as_top(Msgs).

sharing_as_is_top(sharing_as_top(_)).

sharing_as_size(sharing_as_bottom) = 0.
sharing_as_size(sharing_as_real_as(SharingSet)) =  sharing_set_size(SharingSet).

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
        ActualTVarset, FormalSharing, ActualSharing):-
    VarRenaming = get_variable_renaming(ModuleInfo, PPId, ActualVars),
    TypeRenaming = get_type_substitution(ModuleInfo, PPId, ActualTypes,
        ActualTVarset), 
    sharing_as_rename(VarRenaming, TypeRenaming, FormalSharing, ActualSharing).

sharing_as_comb(ModuleInfo, ProcInfo, NewSharing, OldSharing) = ResultSharing :-
    (
        NewSharing = sharing_as_real_as(NewSharingSet),
        (
            OldSharing = sharing_as_real_as(OldSharingSet),
            ResultSharing = wrap(sharing_set_comb(ModuleInfo, ProcInfo,
                NewSharingSet, OldSharingSet))
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
        ( OldSharing = sharing_as_top(MsgOld) ->
            ResultSharing = sharing_as_top(list.append(MsgNew, MsgOld))
        ;
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
    ( Unification = construct(_, _, _, _, _, _, _) ->
        NewSharing = optimization_remove_deaths(ProcInfo,
            GoalInfo, ResultSharing)
    ;
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
        list.takewhile(is_introduced_typeinfo_arg(ProcInfo), Args0,
            _TypeInfoArgs, Args),
        number_args(Args, NumberedArgs),
        some [!SharingSet] (
            !:SharingSet = sharing_set_init,
            list.foldl(add_var_arg_sharing(ModuleInfo, ProcInfo, Var, ConsId),
                NumberedArgs, !SharingSet),
            create_internal_sharing(ModuleInfo, ProcInfo, Var, ConsId,
                NumberedArgs, !SharingSet),
            Sharing = wrap(!.SharingSet)
        )
    ;
        Unification = deconstruct(Var, ConsId, Args0, _, _, _),
        list.takewhile(is_introduced_typeinfo_arg(ProcInfo), Args0,
            _TypeInfoArgs, Args),
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
        ( arg_has_primitive_type(ModuleInfo, ProcInfo, X) ->
            Sharing = sharing_as_init
        ;
            new_entry(ModuleInfo, ProcInfo,
                datastruct_init(X) - datastruct_init(Y),
                sharing_set_init, SharingSet),
            Sharing = wrap(SharingSet)
        )
    ;
        Unification = simple_test(_, _),
        Sharing = sharing_as_init
    ;
        Unification = complicated_unify(_, _, _),
        unexpected(this_file, "complicated_unify during sharing analysis.")
    ).

:- pred is_introduced_typeinfo_arg(proc_info::in, prog_var::in) is semidet.

is_introduced_typeinfo_arg(ProcInfo, Var) :-
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var, Type),
    is_introduced_type_info_type(Type).

:- pred number_args(prog_vars::in, list(pair(int, prog_var))::out) is det.

number_args(Args, NumberedArgs) :-
    NumberArg = (pred(A::in, AP::out, !.N::in, !:N::out) is det :-
        AP = !.N - A,
        !:N = !.N + 1
    ),
    list.map_foldl(NumberArg, Args, NumberedArgs, 1, _).

:- pred add_var_arg_sharing(module_info::in, proc_info::in, prog_var::in,
    cons_id::in, pair(int, prog_var)::in,
    sharing_set::in, sharing_set::out) is det.

add_var_arg_sharing(ModuleInfo, ProcInfo, Var, ConsId, N - Arg, !Sharing) :-
    ( arg_has_primitive_type(ModuleInfo, ProcInfo, Arg) ->
        true
    ;
        Data1 = datastruct_init_with_pos(Var, ConsId, N),
        Data2 = datastruct_init(Arg),
        new_entry(ModuleInfo, ProcInfo, Data1 - Data2, !Sharing)
    ).

:- pred arg_has_primitive_type(module_info::in, proc_info::in,
    prog_var::in) is semidet.

arg_has_primitive_type(ModuleInfo, ProcInfo, Var):-
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var, Type),
    type_is_atomic(Type, ModuleInfo).

    % When two positions within the constructed term refer to the same variable,
    % this must be recorded as an extra sharing pair.
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
        AddPair = (pred(OtherNumberedArg::in,
                !.Sharing::in, !:Sharing::out) is det :-
            ( OtherNumberedArg = Pos2 - Var1 ->
                % Create sharing between Pos1 and Pos2
                Data1 = datastruct_init_with_pos(Var, ConsId, Pos1),
                Data2 = datastruct_init_with_pos(Var, ConsId, Pos2),
                new_entry(ModuleInfo, ProcInfo, Data1 - Data2, !Sharing)
            ;
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
    IsPreBirthArg = (pred(NumberedArg::in) is semidet :-
        Var = snd(NumberedArg),
        set.member(Var, PreBirthSet)
    ),
    list.filter(IsPreBirthArg, !NumberedArgs).

:- func optimization_remove_deaths(proc_info, hlds_goal_info,
    sharing_as) = sharing_as.

optimization_remove_deaths(ProcInfo, GoalInfo, Sharing0) = Sharing :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    set.list_to_set(HeadVars, HeadVarsSet),
    goal_info_get_post_deaths(GoalInfo, Deaths0),
    %
    % Make sure to keep all the information about the headvars,
    % even if they are in the post deaths set.
    %
    set.difference(Deaths0, HeadVarsSet, Deaths),
    set.to_sorted_list(Deaths, DeathsList),
    sharing_as_project_with_type(outproject, DeathsList, Sharing0, Sharing).

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

sharing_as_least_upper_bound(ModuleInfo, ProcInfo, Sharing1, Sharing2)
        = Sharing :-
    (
        Sharing1 = sharing_as_bottom,
        Sharing = Sharing2
    ;
        Sharing1 = sharing_as_top(Msg1),
        ( Sharing2 = sharing_as_top(Msg2) ->
            Sharing = sharing_as_top(list.append(Msg1, Msg2))
        ;
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
            Sharing = sharing_as_real_as(
                sharing_set_least_upper_bound(ModuleInfo,
                ProcInfo, SharingSet1, SharingSet2))
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
        unexpected(this_file, "extend_datastruct with top sharing set.")
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
        ( WideningLimit = 0 ->
            WideningDone = no
        ; WideningLimit > sharing_set_size(SharingSet0) ->
            WideningDone = no
        ;
            sharing_set_apply_widening(ModuleInfo, ProcInfo,
                SharingSet0, SharingSet),
            !:Sharing = sharing_as_real_as(SharingSet),
            WideningDone = yes
        )
    ).

from_structure_sharing_domain(SharingDomain) = SharingAs :-
    (
        SharingDomain = bottom,
        SharingAs = sharing_as_bottom
    ;
        SharingDomain = real(StructureSharing),
        SharingSet = from_sharing_pair_list(StructureSharing),
        wrap(SharingSet, SharingAs)
    ;
        SharingDomain = top(Msgs),
        SharingAs = sharing_as_top(Msgs)
    ).

to_structure_sharing_domain(SharingAs) = SharingDomain :-
    (
        SharingAs = sharing_as_bottom,
        SharingDomain = bottom
    ;
        SharingAs = sharing_as_real_as(SharingSet),
        SharingDomain = real(to_sharing_pair_list(SharingSet))
    ;
        SharingAs = sharing_as_top(Msgs),
        SharingDomain = top(Msgs)
    ).

%-----------------------------------------------------------------------------%
%
% sharing_as_table
%

sharing_as_table_init = map.init.
sharing_as_table_search(PPId, Table) = Table ^ elem(PPId).
sharing_as_table_set(PPId, Sharing, !Table) :-
    !:Table = !.Table ^ elem(PPId) := Sharing.

%-----------------------------------------------------------------------------%

lookup_sharing_and_comb(ModuleInfo, PredInfo, ProcInfo, SharingTable, 
        PredId, ProcId, ActualVars, !Sharing):- 
    PPId = proc(PredId, ProcId),
    
    lookup_sharing_or_predict(ModuleInfo, SharingTable, PPId, FormalSharing),

    proc_info_get_vartypes(ProcInfo, VarTypes), 
    list.map(map.lookup(VarTypes), ActualVars, ActualTypes), 
       
    pred_info_get_typevarset(PredInfo, ActualTVarset), 
    sharing_as_rename_using_module_info(ModuleInfo, PPId, 
        ActualVars, ActualTypes, ActualTVarset, FormalSharing,
        ActualSharing),

    !:Sharing = sharing_as_comb(ModuleInfo, ProcInfo, 
        ActualSharing, !.Sharing).

lookup_sharing_or_predict(ModuleInfo, SharingTable, PPId, SharingAs) :- 
    (
        % look up in SharingTable
        SharingAs0 = sharing_as_table_search(PPId, SharingTable)
    ->
        SharingAs = SharingAs0
    ;
        % or predict bottom sharing
        %
        % If it is neither in the fixpoint table, nor in the sharing
        % table, then this means that we have never analysed the called
        % procedure, yet in some cases we can still simply predict that
        % the sharing the called procedure creates is bottom.
        predict_called_pred_is_bottom(ModuleInfo, PPId)
    ->
        SharingAs = sharing_as_init
    ;
        % or use top-sharing with appropriate message.
        SharingAs = top_sharing_not_found(ModuleInfo, PPId)
    ).

:- pred predict_called_pred_is_bottom(module_info::in, pred_proc_id::in)
    is semidet.

predict_called_pred_is_bottom(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    (
        % 1. inferred determinism is erroneous/failure.
        proc_info_get_inferred_determinism(ProcInfo, Determinism),
        (
            Determinism = erroneous
        ;
            Determinism = failure
        )
    ;
        % 2. bottom_sharing_is_safe_approximation
        bottom_sharing_is_safe_approximation(ModuleInfo, ProcInfo)
    ;
        % 3. call to a compiler generate special predicate:
        % "unify", "index", "compare" or "initialise".
        pred_info_get_origin(PredInfo, Origin),
        Origin = special_pred(_)
    ;
        % 4. (XXX UNSAFE!! To verify) any call to private_builtin and builtin
        % procedures.
        PredModule = pred_info_module(PredInfo),
        any_mercury_builtin_module(PredModule)
    ).

:- func top_sharing_not_found(module_info, pred_proc_id) = sharing_as.

top_sharing_not_found(ModuleInfo, PPId) = TopSharing :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _),
    PPId = proc(PredId, ProcId),
    PredModuleName = pred_info_module(PredInfo),

    TopSharing = sharing_as_top_sharing("Lookup sharing failed for " ++
        sym_name_to_escaped_string(PredModuleName) ++ "." ++
        pred_info_name(PredInfo) ++ "/" ++
        int_to_string(pred_info_orig_arity(PredInfo)) ++ " (id = " ++
        int_to_string(pred_id_to_int(PredId)) ++ "," ++
        int_to_string(proc_id_to_int(ProcId))).

%-----------------------------------------------------------------------------%

load_structure_sharing_table(ModuleInfo) = SharingTable :-
    module_info_predids(ModuleInfo, PredIds),
    list.foldl(load_structure_sharing_table_2(ModuleInfo), PredIds,
        sharing_as_table_init, SharingTable).

:- pred load_structure_sharing_table_2(module_info::in, pred_id::in,
    sharing_as_table::in, sharing_as_table::out) is det.

load_structure_sharing_table_2(ModuleInfo, PredId, !SharingTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(load_structure_sharing_table_3(ModuleInfo, PredId),
        ProcIds, !SharingTable).

:- pred load_structure_sharing_table_3(module_info::in, pred_id::in,
    proc_id::in, sharing_as_table::in, sharing_as_table::out) is det.

load_structure_sharing_table_3(ModuleInfo, PredId, ProcId, !SharingTable) :-
    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_structure_sharing(ProcInfo, MaybePublicSharing),
    (
        MaybePublicSharing = yes(PublicSharing),
        PPId = proc(PredId, ProcId),
        PrivateSharing = from_structure_sharing_domain(PublicSharing),
        sharing_as_table_set(PPId, PrivateSharing, !SharingTable)
    ;
        MaybePublicSharing = no
    ).
%-----------------------------------------------------------------------------%
    
    % Succeeds if the sharing of a procedure can safely be approximated by
    % "bottom", simply by looking at the modes and types of the arguments.
    %
bottom_sharing_is_safe_approximation(ModuleInfo, ProcInfo) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    list.map(map.lookup(VarTypes), HeadVars, Types),

    ModeTypePairs = assoc_list.from_corresponding_lists(Modes, Types),

    Test = (pred(Pair::in) is semidet :-
        Pair = Mode - Type,

        % mode is not unique nor clobbered.
        mode_get_insts(ModuleInfo, Mode, _LeftInst, RightInst),
        \+ inst_is_unique(ModuleInfo, RightInst),
        \+ inst_is_clobbered(ModuleInfo, RightInst),

        % mode is output.
        mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
        ArgMode = top_out,

        % type is not primitive
        \+ type_is_atomic(Type, ModuleInfo)
    ),
    list.filter(Test, ModeTypePairs, TrueModeTypePairs),
    TrueModeTypePairs = [].

%-----------------------------------------------------------------------------%
% Type: sharing_set.
% Definition of the (hidden) representation for lists of sharing data
% structures.
% XXX The definition and implementation of sharing_set should be in a separate
% (sub)module. Yet this gave compilation errors. To do.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
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
%-----------------------------------------------------------------------------%
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

%-----------------------------------------------------------------------------%
%
% sharing_set predicates/functions
%

:- func sharing_set_init = sharing_set is det.
:- pred sharing_set_is_empty(sharing_set::in) is semidet.
:- func sharing_set_size(sharing_set) = int.

:- pred wrap(sharing_set::in, sharing_as::out) is det.
:- func wrap(sharing_set) = sharing_as.

:- pred sharing_set_project(projection_type::in, prog_vars::in,
    sharing_set::in, sharing_set::out) is det.

:- pred sharing_set_rename(prog_var_renaming::in, tsubst::in,
    sharing_set::in, sharing_set::out) is det.

:- func sharing_set_comb(module_info, proc_info, sharing_set, sharing_set) =
    sharing_set.

:- pred sharing_set_is_subsumed_by(module_info::in, proc_info::in,
    sharing_set::in, sharing_set::in) is semidet.

:- func sharing_set_least_upper_bound(module_info, proc_info,
    sharing_set, sharing_set) = sharing_set.

:- func sharing_set_extend_datastruct(module_info, proc_info, datastruct,
    sharing_set) = list(datastruct).

:- pred sharing_set_apply_widening(module_info::in, proc_info::in,
    sharing_set::in, sharing_set::out) is det.

    % Conversion between list of sharing data structures to
    % sharing_set's and vice versa.
    %
    % NOTE: from_sharing_pair_list assumes that the sharing set is minimal, ie,
    % there are no two sharing pairs in that set such that one sharing pair
    % subsumes the other pair.
    %
:- func from_sharing_pair_list(structure_sharing) = sharing_set.
:- func to_sharing_pair_list(sharing_set) = structure_sharing.

%-----------------------------------------------------------------------------%

sharing_set_init = sharing_set(0, map.init).
sharing_set_is_empty(sharing_set(0, _Map)).
sharing_set_size(sharing_set(Size, _)) = Size.

wrap(SharingSet, SharingAs) :-
    ( sharing_set_is_empty(SharingSet) ->
        SharingAs = sharing_as_bottom
    ;
        SharingAs = sharing_as_real_as(SharingSet)
    ).

wrap(SharingSet) = SharingAs :-
    wrap(SharingSet, SharingAs).

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
    ( selector_sharing_set_is_empty(SelSet) ->
        true
    ;
        !.SS = sharing_set(Size0, M0),
        map.det_insert(M0, Var, SelSet, M),
        Size = Size0 + selector_sharing_set_size(SelSet),
        !:SS = sharing_set(Size, M)
    ).

sharing_set_rename(Dict, TypeSubst, SharingSet0, SharingSet) :-
    SharingSet0 = sharing_set(Size, Map0),
    map.foldl(do_sharing_set_rename(Dict, TypeSubst), Map0, map.init, Map),
    SharingSet  = sharing_set(Size, Map).

:- pred do_sharing_set_rename(prog_var_renaming::in, tsubst::in,
    prog_var::in, selector_sharing_set::in,
    map(prog_var, selector_sharing_set)::in,
    map(prog_var, selector_sharing_set)::out) is det.

do_sharing_set_rename(Dict, TypeSubst, Var0, SelectorSet0, !Map) :-
    selector_sharing_set_rename(Dict, TypeSubst, SelectorSet0, SelectorSet),
    map.lookup(Dict, Var0, Var),
    % XXX old code pretends that 2 vars can be renamed to
    % one and the same new variable. Is that so?
    % To check.
    % (
    %   map.search(!.Map, Var, SelectorSet2)
    % ->
    %   % can occur when 2 vars are renamed to
    %   % the same var (call: append(X,X,Y))
    %   selector_sharing_set_add(SelectorSet1,
    %       SelectorSet2, SelectorSet),
    %   map.det_update(!.Map, Var, SelectorSet, !:Map)
    % ;
    %   map.det_insert(!.Map, Var, SelectorSet1, !:Map)
    % )
    svmap.det_insert(Var, SelectorSet, !Map).

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
        [NewSharingSet, OldSharingSet, OldNewSharingSet, NewOldNewSharingSet]).

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

    proc_info_get_vartypes(ProcInfo, VarTypes),
    %
    % for each common var V, compute the sharing pairs A-B, such that
    % \exists X where var(X) = V, and X-A \in NewSharingSet, and X-B \in
    % OldSharingSet.
    %
    list.foldl(
        (pred(Var::in, !.SS::in, !:SS::out) is det :-
            map.lookup(VarTypes, Var, Type),
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
            set_cross_product(set.from_list(ExtendedX),
                set.from_list(ExtendedY), SetPairs),
            new_entries(ModuleInfo, ProcInfo,
                set.to_sorted_list(SetPairs), SS0, SS)
        ),
        OldSharingPairs,
        sharing_set_init,
        ResultSharingSet).

    % SharingSet1 is subsumed by SharingSet2 iff every sharing pair
    % represented by SharingSet1 is subsumed by any of the sharing pairs
    % represented by SharingSet2.
    % XXX For the moment SharingSet1 is converted to a list of sharing pairs,
    % and each of the sharing pairs is checked for subsumption. This should be
    % optimised to follow the inner structure of sharing_set instead.
    %
sharing_set_is_subsumed_by(ModuleInfo, ProcInfo, SharingSet1, SharingSet2):-
    list.all_true(sharing_set_subsumes_sharing_pair(ModuleInfo, ProcInfo,
        SharingSet2), to_sharing_pair_list(SharingSet1)).

sharing_set_least_upper_bound(ModuleInfo, ProcInfo, Set1, Set2) = Union :-
    % Folding over the map could be done, but the union is easier to describe
    % by picking each of the sharing pairs represented by the smallest sharing
    % set, and adding them to the other sharing set.
    Set1 = sharing_set(Size1, _),
    Set2 = sharing_set(Size2, _),
    ( Size1 < Size2 ->
        Pairs = to_sharing_pair_list(Set1),
        Set = Set2
    ;
        Pairs = to_sharing_pair_list(Set2),
        Set = Set1
    ),
    new_entries(ModuleInfo, ProcInfo, Pairs, Set, Union).

:- func sharing_set_least_upper_bound_list(module_info, proc_info,
    list(sharing_set)) = sharing_set.

sharing_set_least_upper_bound_list(ModuleInfo, ProcInfo, ListSharingSet) =
    list.foldl(sharing_set_least_upper_bound(ModuleInfo, ProcInfo),
        ListSharingSet, sharing_set_init).

sharing_set_extend_datastruct(ModuleInfo, ProcInfo, Datastruct, SharingSet)
        = Datastructures :-
    SharingSet = sharing_set(_, SharingMap),
    Var = Datastruct ^ sc_var,
    Selector = Datastruct ^ sc_selector,
    ( map.search(SharingMap, Var, SelectorSet) ->
        % The type of the variable is needed to be able to compare
        % datastructures.
        %
        proc_info_get_vartypes(ProcInfo, VarTypes),
        map.lookup(VarTypes, Var, VarType),
        Datastructures = selector_sharing_set_extend_datastruct(ModuleInfo,
            VarType, Selector, SelectorSet)
    ;
        Datastructures = []
    ).

sharing_set_apply_widening(ModuleInfo, ProcInfo, !SharingSet):-
    !.SharingSet = sharing_set(_, SharingMap0),
    map.map_foldl(
        selector_sharing_set_apply_widening(ModuleInfo, ProcInfo),
        SharingMap0, SharingMap, 0, NewSize),
    !:SharingSet = sharing_set(NewSize, SharingMap).

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
    % Normalize the sharing pair before doing anything.
    SharingPair0 = DataX0 - DataY0,
    SharingPair = normalize_datastruct(ModuleInfo, ProcInfo, DataX0) -
        normalize_datastruct(ModuleInfo, ProcInfo, DataY0),

    (
        sharing_set_subsumes_sharing_pair(ModuleInfo, ProcInfo,
            !.SharingSet, SharingPair)
    ->
        true
    ;
        sharing_set_subsumed_subset(ModuleInfo, ProcInfo,
            !.SharingSet, SharingPair, SubsumedPairs),
        remove_entries(SubsumedPairs, !SharingSet),
        new_entry_no_controls(SharingPair, !SharingSet)
    ).

:- pred new_entry_no_controls(structure_sharing_pair::in,
    sharing_set::in, sharing_set::out) is det.

new_entry_no_controls(SharingPair, !SS) :-
    SharingPair = Data1 - Data2,
    new_directed_entry(Data1, Data2, !SS),
    ( datastruct_equal(Data1, Data2) ->
        true
    ;
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
    ( datastruct_equal(Data1, Data2) ->
        true
    ;
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
    ( set.remove(Data0, ToData, Data) ->
        DataSize = DataSize0 - 1,
        SelSize = SelSize0 - 1,
        Size = Size0 - 1,

        ( Size = 0 ->
            SharingSet = sharing_set(Size, map.init)
        ; SelSize = 0 ->
            map.delete(SharingMap0, FromVar, SharingMap),
            SharingSet = sharing_set(Size, SharingMap)
        ; DataSize = 0 ->
            map.delete(SelSharingMap0, FromSel, SelSharingMap),
            SelSharingSet = selector_sharing_set(SelSize, SelSharingMap),
            map.det_update(SharingMap0, FromVar, SelSharingSet, SharingMap),
            SharingSet = sharing_set(Size, SharingMap)
        ;
            DataSet = datastructures(DataSize, Data),
            map.det_update(SelSharingMap0, FromSel, DataSet, SelSharingMap),
            SelSharingSet = selector_sharing_set(SelSize, SelSharingMap),
            map.det_update(SharingMap0, FromVar, SelSharingSet, SharingMap),
            SharingSet = sharing_set(Size, SharingMap)
        )
    ;
        unexpected(this_file, "Removing non-existant sharing pair.")
    ).

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
    Var1 = Data1 ^ sc_var,
    Sel1 = Data1 ^ sc_selector,
    Var2 = Data2 ^ sc_var,
    Sel2 = Data2 ^ sc_selector,

    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var1, Type1),
    map.lookup(VarTypes, Var2, Type2),

    map.search(SharingMap, Var1, SelSharingSet),
    SelSharingSet = selector_sharing_set(_, SelSharingMap),
    map.keys(SelSharingMap, SelectorList),

        % Find at least one selector in SelectorList that is more general
        % than Sel1 (with a specific extension), and whose associated dataset
        % contains at least one datastructure that is more general than Data2
        % (with that same extension).
    list.find_first_map(
        pred(Sel::in, Data::out) is semidet :-
        (
            subsumed_by(ModuleInfo, Sel1, Sel, Type1, Extension),
            map.search(SelSharingMap, Sel, DataSet),
            DataSet = datastructures(_, DatastructureSet),
            MatchedDatastructs = list.filter(
                pred(Datastructure::in) is semidet :-
                (
                    Var2 = Datastructure ^ sc_var,
                    ctgc.selector.subsumed_by(ModuleInfo, Sel2,
                        Datastructure ^ sc_selector, Type2, Extension)
                ),
                to_sorted_list(DatastructureSet)),
            % The list of matched datastructures contains at least one element.
            MatchedDatastructs = [Data| _]
        ),
        SelectorList,
        _).

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
    Var1 = Data1 ^ sc_var,
    Sel1 = Data1 ^ sc_selector,
    Var2 = Data2 ^ sc_var,
    Sel2 = Data2 ^ sc_selector,

    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var1, Type1),
    map.lookup(VarTypes, Var2, Type2),

    ( map.search(SharingMap, Var1, SelSharingSet) ->
        SelSharingSet = selector_sharing_set(_, SelSharingMap),
        %
        % Determine all Selector-Dataset pairs where
        %   * Selector is less or equal to Sel1 wrt some extension E,
        %   * Dataset is a set of datastructures less or equal to Data2
        %     (wrt the same extension E).
        %
        map.keys(SelSharingMap, SelectorList),
        list.filter_map(
            pred(Selector::in, SPairs::out) is semidet :-
            (
                ctgc.selector.subsumed_by(ModuleInfo, Selector, Sel1,
                    Type1, Extension),
                map.search(SelSharingMap, Selector, Dataset),
                Dataset = datastructures(_, Datastructs),
                list.filter_map(
                    pred(D::in, Pair::out) is semidet :-
                    (
                        Var2 = D ^ sc_var,
                        ctgc.selector.subsumed_by(ModuleInfo, D ^ sc_selector,
                            Sel2, Type2, Extension),
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
    ;
        SubsumedPairs = []
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
    ( map.search(Map0, Var, Selectors0) ->
        (
            selector_sharing_set_new_entry(Selector, ToData,
                Selectors0, Selectors)
        ->
            map.det_update(Map0, Var, Selectors, Map),
            Size = Size0 + 1
        ;
            Map = Map0,
            Size = Size0
        )
    ;
        (
            selector_sharing_set_new_entry(Selector, ToData,
                selector_sharing_set_init, Selectors)
        ->
            map.det_insert(Map0, Var, Selectors, Map),
            Size = Size0 + 1
        ;
            unexpected(this_file, "new_directed_entry: Impossible option.")
        )
    ),
    SharingSet = sharing_set(Size, Map).

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
    (
        directed_entry_is_member(
            datastruct_init_with_selector(ProgVar, Selector),
            Datastruct, !.SS)
    ->
        true
    ;
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

%-----------------------------------------------------------------------------%
%
% selector_sharing_set predicates/functions
%

:- func selector_sharing_set_init = selector_sharing_set.

:- pred selector_sharing_set_is_empty(selector_sharing_set::in) is semidet.

:- func selector_sharing_set_size(selector_sharing_set) = int.

:- pred selector_sharing_set_project(projection_type::in, prog_vars::in,
    selector_sharing_set::in, selector_sharing_set::out) is det.

:- pred selector_sharing_set_rename(prog_var_renaming::in,
    tsubst::in, selector_sharing_set::in, selector_sharing_set::out) is det.

    % selector_sharing_set_new_entry(Selector, Datastruct, SS0, SS):
    % Adds Datastruct into SS0 using Selector as a key. Fails if that
    % Datastructs is already present with that selector.
    %
:- pred selector_sharing_set_new_entry(selector::in, datastruct::in,
    selector_sharing_set::in, selector_sharing_set::out) is semidet.

:- func selector_sharing_set_altclos(module_info, proc_info, mer_type,
    selector_sharing_set, selector_sharing_set) = structure_sharing.

:- func selector_sharing_set_extend_datastruct(module_info,
    mer_type, selector, selector_sharing_set) = list(datastruct).

:- pred selector_sharing_set_apply_widening(module_info::in, proc_info::in,
    prog_var::in, selector_sharing_set::in, selector_sharing_set::out,
    int::in, int::out) is det.

selector_sharing_set_init = selector_sharing_set(0, map.init).
selector_sharing_set_is_empty(selector_sharing_set(0, _Map)).
selector_sharing_set_size(selector_sharing_set(Size,_)) = Size.

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
    ( data_set_is_empty(DataSet) ->
        true
    ;
        !.SS = selector_sharing_set(Size0, Map0),
        map.det_insert(Map0, Selector, DataSet, Map),
        Size = Size0 + data_set_size(DataSet),
        !:SS = selector_sharing_set(Size, Map)
    ).

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
    svmap.det_insert(Selector, DataSet, !Map).

selector_sharing_set_new_entry(Selector, Datastruct,
        SelSharingSet0, SelSharingSet) :-
    SelSharingSet0 = selector_sharing_set(Size0, Map0),
    ( map.search(Map0, Selector, DataSet0) ->
        data_set_new_entry(Datastruct, DataSet0, DataSet),
        Size = Size0 + 1,
        map.det_update(Map0, Selector, DataSet, Map)
    ;
        data_set_new_entry(Datastruct, data_set_init, DataSet),
        Size = Size0 + 1,
        map.det_insert(Map0, Selector, DataSet, Map)
    ),
    SelSharingSet = selector_sharing_set(Size, Map).

selector_sharing_set_altclos(ModuleInfo, ProcInfo, Type, NewSelSet, OldSelSet)
        = NewSharingPairs :-

    NewSelSet = selector_sharing_set(_, NewMap),
    OldSelSet = selector_sharing_set(_, OldMap),
    %
    % Get the selectors.
    %
    map.keys(NewMap, NewSelectors),
    map.keys(OldMap, OldSelectors),
    %
    % For each selector in NewSelectors, verify each selector in OldSelector,
    % if either one is less or equal to the other, then generate the structure
    % sharing pair as appropriate.
    %
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

:- func basic_closure(module_info, proc_info, mer_type,
    data_set, data_set, selector, selector) = structure_sharing.

basic_closure(ModuleInfo, _ProcInfo, Type, NewDataSet, OldDataSet,
        NewSel, OldSel) = SharingPairs :-
    % three cases:
    % 1. NewSel <= OldSel then generate sharing pairs.
    % 2. OldSel <= NewSel then generate sharing pairs.
    % 3. NewSel and OldSet not comparable then no new sharing pairs.

    (
        % NewSel <= OldSel ie, \exists Extension: OldSel.Extension = NewSel.
        ctgc.selector.subsumed_by(ModuleInfo, NewSel, OldSel, Type, Extension)
    ->
        data_set_termshift(OldDataSet, Extension, TermShiftedOldDataSet),
        SharingPairs = data_set_directed_closure(TermShiftedOldDataSet,
            NewDataSet)
    ;
        % OldSel <= NewSel ie, \exists Extension: NewSel.Extension = OldSel.
        ctgc.selector.subsumed_by(ModuleInfo, OldSel, NewSel, Type, Extension)
    ->
        data_set_termshift(NewDataSet, Extension, TermShiftedNewDataSet),
        SharingPairs = data_set_directed_closure(TermShiftedNewDataSet,
            OldDataSet)
    ;
        % uncomparable
        SharingPairs = []
    ).

selector_sharing_set_extend_datastruct(ModuleInfo, VarType, Selector,
        SelectorSharingSet) = Datastructures :-
    SelectorSharingSet = selector_sharing_set(_, SelectorMap),
    Datastructures =
        list.condense(
            map.values(
                map.map_values(
                    selector_sharing_set_extend_datastruct_2(ModuleInfo,
                        VarType, Selector),
                    SelectorMap)
            )
        ).

:- func selector_sharing_set_extend_datastruct_2(module_info,
    mer_type, selector, selector, data_set) = list(datastruct).

selector_sharing_set_extend_datastruct_2(ModuleInfo, VarType, BaseSelector,
        Selector, Dataset0) = Datastructures :-
    % If Sel is more general than Selector, i.e.
    % Selector = Sel.Extension, apply this extension
    % to all the datastructs associated with Sel, and add them
    % to the set of datastructs collected.
    (
        ctgc.selector.subsumed_by(ModuleInfo, BaseSelector,
            Selector, VarType, Extension)
    ->
        data_set_termshift(Dataset0, Extension, Dataset),
        Dataset = datastructures(_, Data),
        Datastructures = set.to_sorted_list(Data)
    ;
        Datastructures = []
    ).

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
        Selector, DataSet, !DataMap, !DataMapSize) :-
    % Widening of the associated datastructures.
    data_set_apply_widening(ModuleInfo, ProcInfo, DataSet, DataSet1),

    % Widening of the ProgVar-Selector datastructure.
    datastruct_apply_widening(ModuleInfo, ProcInfo,
        datastruct_init_with_selector(ProgVar, Selector), NewDataStruct),
    NewSelector = NewDataStruct ^ sc_selector,

    % Check if NewSelector is already in the resulting DataMap, if so,
    % compute the least upper bound of the associated data_set's.
    ( map.search(!.DataMap, NewSelector, ExistingDataSet) ->
        ExistingDataSetSize = data_set_size(ExistingDataSet),
        DataSetFinal = data_set_least_upper_bound(ModuleInfo, ProcInfo,
            DataSet1, ExistingDataSet),
        DataSetFinalSize = data_set_size(DataSetFinal),
        svmap.det_update(NewSelector, DataSetFinal, !DataMap),
        !:DataMapSize = !.DataMapSize - ExistingDataSetSize + DataSetFinalSize
    ;
        svmap.det_insert(NewSelector, DataSet1, !DataMap),
        !:DataMapSize = !.DataMapSize + data_set_size(DataSet1)
    ).

%-----------------------------------------------------------------------------%
%
% data_set predicates/functions
%

:- func data_set_init = data_set.

:- pred data_set_is_empty(data_set::in) is semidet.

:- func data_set_size(data_set) = int.

:- pred data_set_project(projection_type::in, prog_vars::in,
    data_set::in, data_set::out) is det.

:- pred data_set_rename(prog_var_renaming::in, tsubst::in,
    data_set::in, data_set::out) is det.

:- pred data_set_termshift(data_set::in, selector::in, data_set::out) is det.

:- pred data_set_new_entry(datastruct::in, data_set::in, data_set::out)
    is semidet.

:- func data_set_directed_closure(data_set, data_set) = structure_sharing.

:- pred data_set_apply_widening(module_info::in, proc_info::in,
    data_set::in, data_set::out) is det.

:- func data_set_least_upper_bound(module_info, proc_info,
    data_set, data_set) = data_set.

data_set_init = datastructures(0, set.init).

data_set_is_empty(datastructures(0, _Set)).

data_set_size(datastructures(Size, _)) = Size.

data_set_project(ProjectionType, Vars, !DataSet) :-
    data_set_filter(data_set_project_test(ProjectionType, Vars), !DataSet).

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

data_set_rename(Dict, Subst, !DataSet) :-
    !.DataSet = datastructures(_Size, Datastructs0),
    Datastructs = set.map(rename_datastruct(Dict, Subst), Datastructs0),
    !:DataSet = datastructures(set.count(Datastructs), Datastructs).

data_set_termshift(DataSet0, Selector, DataSet) :-
    DataSet0 = datastructures(Size, Set0),
    Set = set.map(datastruct_termshift(Selector), Set0),
    DataSet = datastructures(Size, Set).

data_set_new_entry(Datastruct,  DataSet0, DataSet) :-
    DataSet0 = datastructures(Size0, Datastructs0),
    \+ set.member(Datastruct, Datastructs0),
    set.insert(Datastructs0, Datastruct, Datastructs),
    Size = Size0 + 1,
    DataSet = datastructures(Size, Datastructs).

data_set_directed_closure(FromData, ToData) = SharingPairs :-
    FromData = datastructures(_, DataSet1),
    ToData = datastructures(_, DataSet2),
    set_cross_product(DataSet1, DataSet2, SetOfPairs),
    set.to_sorted_list(SetOfPairs, SharingPairs).

:- pred set_cross_product(set(T1)::in, set(T2)::in,
    set(pair(T1, T2))::out) is det.

set_cross_product(SetA, SetB, CrossProduct):-
    solutions_set(cross_product(SetA, SetB), CrossProduct).

:- pred cross_product(set(T1)::in, set(T2)::in, pair(T1, T2)::out) is nondet.

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
    (
        % Perform the simple check of exact occurrence in the set first...
        set.member(Data, !.Datastructs)
    ->
        true
    ;
        datastruct_subsumed_by_list(ModuleInfo, ProcInfo, Data,
            set.to_sorted_list(!.Datastructs))
    ->
        true
    ;
        svset.insert(Data, !Datastructs)
    ).

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

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_sharing.domain.m".

%-----------------------------------------------------------------------------%
:- end_module structure_sharing.domain.
%-----------------------------------------------------------------------------%
