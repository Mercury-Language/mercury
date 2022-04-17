%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: polymorphism.m.
% Main authors: fjh and zs.
%
% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphism, including
% typeclasses, by passing extra `type_info' and `typeclass_info' arguments.
% These arguments are structures that contain, amongst other things,
% higher order predicate terms for the polymorphic procedures or methods.
%
% See notes/type_class_transformation.html for a description of the
% transformation and data structures used to implement type classes.
%
% XXX The way the code in this module handles existential type classes
% and type class constraints is a bit ad hoc, in general; there are
% definitely parts of this code (marked with XXXs below) that could
% do with a rewrite to make it more consistent and hence more maintainable.
%
%---------------------------------------------------------------------------%
%
% Transformation of polymorphic code:
%
% Every polymorphic predicate is transformed so that it takes one additional
% argument for every type variable in the predicate's type declaration.
% The argument gives information about the type, including higher order
% predicate variables for each of the builtin polymorphic operations
% (currently unify/2, compare/3).
%
%---------------------------------------------------------------------------%
%
% Representation of type information:
%
% IMPORTANT: ANY CHANGES TO THE DOCUMENTATION HERE MUST BE REFLECTED BY
% SIMILAR CHANGES TO THE #defines IN "runtime/mercury_type_info.h" AND
% TO THE TYPE SPECIALIZATION CODE IN "compiler/higher_order.m".
%
% Type information is represented using one or two cells. The cell which
% is always present is the type_ctor_info structure, whose structure is
% defined in runtime/mercury_type_info.h. The other cell is the type_info
% structure, laid out like this:
%
%   word 0      <pointer to the type_ctor_info structure>
%   word 1+     <the type_infos for the type params, at least one>
%
%   (but see note below for how variable arity types differ)
%
%---------------------------------------------------------------------------%
%
% Optimization of common case (zero arity types):
%
% The type_info structure itself is redundant if the type has no type
% parameters (i.e. its arity is zero). Therefore if the arity is zero,
% we pass the address of the type_ctor_info structure directly, instead of
% wrapping it up in another cell. The runtime system will look at the first
% field of the cell it is passed. If this field is zero, the cell is a
% type_ctor_info structure for an arity zero type. If this field is not zero,
% the cell is a new type_info structure, with the first field being the
% pointer to the type_ctor_info structure.
%
%---------------------------------------------------------------------------%
%
% Variable arity types:
%
% There is a slight variation on this for variable-arity type constructors, of
% there are exactly three: pred, func and tuple. Typeinfos of these types
% always have a pointer to the pred/0, func/0 or tuple/0 type_ctor_info,
% regardless of their true arity, so we store the real arity in the type_info
% as well.
%
%   word 0      <pointer to the arity 0 type_ctor_info structure>
%   word 1      <arity of predicate>
%   word 2+     <the type_infos for the type params, if any>
%
%---------------------------------------------------------------------------%
%
% Sharing type_ctor_info structures:
%
% For compilation models that can put code addresses in static ground terms,
% we can arrange to create one copy of the type_ctor_info structure statically,
% avoiding the need to create other copies at runtime. For compilation models
% that cannot put code addresses in static ground terms, there are a couple
% of things we could do:
%
%   1. allocate all cells at runtime.
%   2. use a shared static type_ctor_info, but initialize its code
%      addresses during startup (that is, during the module
%      initialization code).
%
% We use option 2.
%
%---------------------------------------------------------------------------%
%
% Example of transformation:
%
% Take the following code as an example, ignoring the requirement for
% superhomogeneous form for clarity:
%
%   :- pred p(T1).
%   :- pred q(T2).
%   :- pred r(T3).
%
%   p(X) :- q([X]), r(0).
%
% We add an extra argument for each type variable:
%
%   :- pred p(type_info(T1), T1).
%   :- pred q(type_info(T2), T2).
%   :- pred r(type_info(T3), T3).
%
% We transform the body of p to this:
%
%   p(TypeInfoT1, X) :-
%       TypeCtorInfoT2 = type_ctor_info(list/1),
%       TypeInfoT2 = type_info(TypeCtorInfoT2, TypeInfoT1),
%       q(TypeInfoT2, [X]),
%       TypeInfoT3 = type_ctor_info(int/0),
%       r(TypeInfoT3, 0).
%
% Note that type_ctor_infos are actually generated as references to a
% single shared type_ctor_info.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Transformation of code using existentially quantified types:
%
% The transformation for existential types is similar to the transformation
% for universally quantified types, except that the type_infos and
% type_class_infos have mode `out' rather than mode `in'.
%
% The argument passing convention is that the new parameters
% introduced by this pass are placed in the following order:
%
%   First the type_infos for unconstrained universally quantified type
%   variables, in the order that the type variables first appear in the
%   argument types;
%
%   then the type_infos for unconstrained existentially quantified type
%   variables, in the order that the type variables first appear in the
%   argument types;
%
%   then the typeclass_infos for universally quantified constraints,
%   in the order that the constraints appear in the class context;
%
%   then the typeclass_infos for existentially quantified constraints,
%   in the order that the constraints appear in the class context;
%
%   and finally the original arguments of the predicate.
%
% Bear in mind that for the purposes of this (and most other) calculations,
% the return parameter of a function counts as the _last_ argument.
%
% The convention for class method implementations is slightly different
% to match the order that the type_infos and typeclass_infos are passed
% in by do_call_class_method (in runtime/mercury_ho_call.c):
%
%   First the type_infos for the unconstrained type variables in the
%   instance declaration, in the order that the type variables first appear
%   in the instance arguments;
%
%   then the typeclass_infos for the class constraints on the instance
%   declaration, in the order that the constraints appear in the declaration;
%
%   then the remainder of the arguments as above.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Run the polymorphism pass over the whole HLDS.
    %
:- pred polymorphism_process_module(module_info::in, module_info::out,
    list(pred_id)::out, maybe_safe_to_continue::out, list(error_spec)::out)
    is det.

%---------------------------------------------------------------------------%

    % Run the polymorphism pass over a single pred. This is used to transform
    % clauses introduced by unify_proc.m for complicated unification predicates
    % for types for which unification predicates are generated lazily.
    %
    % This predicate should be used with caution. polymorphism.m expects that
    % the argument types of called predicates have not been transformed yet.
    % This predicate will not work correctly after the original pass of
    % polymorphism has been run if the predicate to be processed calls
    % any polymorphic predicates which require type_infos or typeclass_infos
    % to be added to the argument list.
    %
    % For backwards compatibility, this predicate also does the tasks
    % that older versions of the polymorphism pass used to do: copying
    % goals from clauses to procedures, and doing the post-copying parts
    % of the polymorphism transformation.
    %
:- pred polymorphism_process_generated_pred(pred_id::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

    % Add the type_info variables for a complicated unification to
    % the appropriate fields in the unification and the goal_info.
    %
    % Exported for modecheck_unify.m.
    %
:- pred unification_typeinfos_rtti_varmaps(mer_type::in, rtti_varmaps::in,
    unification::in, unification::out, hlds_goal_info::in, hlds_goal_info::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.introduce_exists_casts.
:- import_module check_hlds.polymorphism_info.
:- import_module check_hlds.polymorphism_lambda.
:- import_module check_hlds.polymorphism_type_class_info.
:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.from_ground_term_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% This whole section just traverses the module structure.
% We do two passes, the first to fix up the clauses_info and proc_infos
% (and in fact everything except the pred_info argtypes), the second to fix up
% the pred_info argtypes. The reason we need two passes is that the first pass
% looks at the argtypes of the called predicates, and so we need to make
% sure we don't muck them up before we have finished the first pass.
%

polymorphism_process_module(!ModuleInfo, ExistsCastPredIds,
        SafeToContinue, Specs) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.keys(PredIdTable0, PredIds0),
    list.foldl3(maybe_polymorphism_process_pred, PredIds0,
        safe_to_continue, SafeToContinue, [], Specs, !ModuleInfo),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable1),
    map.keys(PredIdTable1, PredIds1),
    list.foldl2(fixup_pred_polymorphism, PredIds1, [], ExistsCastPredIds,
        !ModuleInfo).

:- pred maybe_polymorphism_process_pred(pred_id::in,
    maybe_safe_to_continue::in, maybe_safe_to_continue::out,
    list(error_spec)::in, list(error_spec)::out,
    module_info::in, module_info::out) is det.

maybe_polymorphism_process_pred(PredId, !SafeToContinue,
        !Specs, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ( if
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),
        no_type_info_builtin(PredModule, PredName, PredArity)
    then
        true
    else
        polymorphism_process_pred_msg(PredId, !SafeToContinue,
            !Specs, !ModuleInfo)
    ).

%---------------------------------------------------------------------------%

:- pred fixup_pred_polymorphism(pred_id::in,
    list(pred_id)::in, list(pred_id)::out,
    module_info::in, module_info::out) is det.

fixup_pred_polymorphism(PredId, !ExistsCastPredIds, !ModuleInfo) :-
    % Recompute the arg types by finding the headvars and the var->type mapping
    % (from the clauses_info) and applying the type mapping to the extra
    % headvars to get the new arg types. Note that we are careful to only apply
    % the mapping to the extra head vars, not to the originals, because
    % otherwise we would stuff up the arg types for unification predicates for
    % equivalence types.

    % Since polymorphism transforms not just the procedures defined
    % in the module being compiled, but also all the procedures in
    % all the imported modules, this message can be printed A LOT,
    % even though it is almost never of interest.
    % That is why we enable it only when requested.
    trace [compiletime(flag("poly_msgs")), io(!IO)] (
        write_pred_progress_message(!.ModuleInfo,
            "Fixup pred polymorphism for", PredId, !IO)
    ),

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_vartypes(ClausesInfo0, VarTypes0),
    clauses_info_get_headvars(ClausesInfo0, HeadVars),

    pred_info_get_arg_types(PredInfo0, TypeVarSet, ExistQVars, ArgTypes0),
    proc_arg_vector_partition_poly_args(HeadVars, ExtraHeadVarList,
        OldHeadVarList),

    lookup_var_types(VarTypes0, ExtraHeadVarList, ExtraArgTypes),
    ArgTypes = ExtraArgTypes ++ ArgTypes0,
    pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes,
        PredInfo0, PredInfo1),

    % If the clauses bind some existentially quantified type variables,
    % introduce exists_casts goals for affected head variables, including
    % the new type_info and typeclass_info arguments. Make sure the types
    % of the internal versions of type_infos for those type variables
    % in the variable types map are as specific as possible.

    ( if
        ExistQVars = [_ | _],
        % This can fail for unification procedures of equivalence types.
        lookup_var_types(VarTypes0, OldHeadVarList, OldHeadVarTypes),
        type_list_subsumes(ArgTypes0, OldHeadVarTypes, Subn),
        not map.is_empty(Subn)
    then
        pred_info_set_existq_tvar_binding(Subn, PredInfo1, PredInfo),
        !:ExistsCastPredIds = [PredId | !.ExistsCastPredIds]
    else
        PredInfo = PredInfo1
    ),

    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

%---------------------------------------------------------------------------%

:- pred polymorphism_process_pred_msg(pred_id::in,
    maybe_safe_to_continue::in, maybe_safe_to_continue::out,
    list(error_spec)::in, list(error_spec)::out,
    module_info::in, module_info::out) is det.

polymorphism_process_pred_msg(PredId, !SafeToContinue, !Specs, !ModuleInfo) :-
    % Since polymorphism transforms not just the procedures defined
    % in the module being compiled, but also all the procedures in
    % all the imported modules, this message can be printed A LOT,
    % even though it is almost never of interest.
    % That is why we enable it only when requested.
    trace [compiletime(flag("poly_msgs")), io(!IO)] (
        write_pred_progress_message(!.ModuleInfo,
            "Transforming polymorphism for", PredId, !IO)
    ),
    polymorphism_process_pred(PredId, PredSafeToContinue, !Specs, !ModuleInfo),
    (
        PredSafeToContinue = safe_to_continue
    ;
        PredSafeToContinue = unsafe_to_continue,
        !:SafeToContinue = unsafe_to_continue
    ).

polymorphism_process_generated_pred(PredId, !ModuleInfo) :-
    polymorphism_process_pred(PredId, SafeToContinue, [], Specs, !ModuleInfo),
    expect(unify(Specs, []), $pred,
        "generated pred has errors"),
    expect(unify(SafeToContinue, safe_to_continue), $pred,
        "generated pred has errors"),
    fixup_pred_polymorphism(PredId, [], ExistsPredIds, !ModuleInfo),
    copy_clauses_to_procs_for_pred_in_module_info(PredId, !ModuleInfo),
    list.foldl(introduce_exists_casts_poly, ExistsPredIds, !ModuleInfo).

:- pred polymorphism_process_pred(pred_id::in, maybe_safe_to_continue::out,
    list(error_spec)::in, list(error_spec)::out,
    module_info::in, module_info::out) is det.

polymorphism_process_pred(PredId, SafeToContinue, !Specs, !ModuleInfo) :-
    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        % Replace 99999 with the id of the predicate you want to debug.
        ( if pred_id_to_int(PredId) = 99999 then
            poly_info_set_selected_pred(is_selected_pred, !IO)
        else
            true
        )
    ),

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),

    % Run the polymorphism pass over the clauses_info, updating the headvars,
    % goals, varsets, types, etc., and computing some information in the
    % poly_info.

    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    polymorphism_process_clause_info(!.ModuleInfo, PredInfo0,
        ClausesInfo0, ClausesInfo, PolyInfo, ExtraArgModes),
    poly_info_get_module_info(PolyInfo, !:ModuleInfo),
    poly_info_get_const_struct_db(PolyInfo, ConstStructDb),
    module_info_set_const_struct_db(ConstStructDb, !ModuleInfo),

    poly_info_get_typevarset(PolyInfo, TypeVarSet),
    pred_info_set_typevarset(TypeVarSet, PredInfo0, PredInfo1),
    pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo2),

    poly_info_get_errors(PolyInfo, PredSpecs),
    (
        PredSpecs = [],
        SafeToContinue = safe_to_continue
    ;
        PredSpecs = [_ | _],
        SafeToContinue = unsafe_to_continue,
        !:Specs = PredSpecs ++ !.Specs
    ),

    % Do a pass over the proc_infos, updating all the argmodes with
    % modes for the extra arguments.
    pred_info_get_proc_table(PredInfo2, ProcMap0),
    map.map_values_only(add_extra_arg_modes_to_proc(ExtraArgModes),
        ProcMap0, ProcMap),
    pred_info_set_proc_table(ProcMap, PredInfo2, PredInfo),

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        poly_info_set_selected_pred(is_not_selected_pred, !IO)
    ),

    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred polymorphism_process_clause_info(module_info::in, pred_info::in,
    clauses_info::in, clauses_info::out, poly_info::out,
    poly_arg_vector(mer_mode)::out) is det.

polymorphism_process_clause_info(ModuleInfo0, PredInfo0, !ClausesInfo, !:Info,
        ExtraArgModes) :-
    init_poly_info(ModuleInfo0, PredInfo0, !.ClausesInfo, !:Info),
    !.ClausesInfo = clauses_info(_VarSet, ExplicitVarTypes, _TVarNameMap,
        _VarTypes, HeadVars0, ClausesRep0, ItemNumbers,
        _RttiVarMaps, HaveForeignClauses, HadSyntaxErrors),

    setup_headvars(PredInfo0, HeadVars0, HeadVars,
        ExtraArgModes, UnconstrainedTVars,
        ExtraTypeInfoHeadVars, ExistTypeClassInfoHeadVars, !Info),

    get_clause_list_for_replacement(ClausesRep0, Clauses0),
    list.map_foldl(
        polymorphism_process_clause(PredInfo0, HeadVars0, HeadVars,
            UnconstrainedTVars, ExtraTypeInfoHeadVars,
            ExistTypeClassInfoHeadVars),
        Clauses0, Clauses, !Info),

    % Set the new values of the fields in clauses_info.
    poly_info_get_varset(!.Info, VarSet),
    poly_info_get_var_types(!.Info, VarTypes),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps),
    set_clause_list(Clauses, ClausesRep),
    init_vartypes(TVarNameMap), % This is only used while adding the clauses.
    !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, TVarNameMap,
        VarTypes, HeadVars, ClausesRep, ItemNumbers,
        RttiVarMaps, HaveForeignClauses, HadSyntaxErrors).

:- pred polymorphism_process_clause(pred_info::in,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::in,
    list(tvar)::in, list(prog_var)::in, list(prog_var)::in,
    clause::in, clause::out, poly_info::in, poly_info::out) is det.

polymorphism_process_clause(PredInfo0, OldHeadVars, NewHeadVars,
        UnconstrainedTVars, ExtraTypeInfoHeadVars,
        ExistTypeClassInfoHeadVars, !Clause, !Info) :-
    ( if pred_info_is_imported(PredInfo0) then
        true
    else
        Goal0 = !.Clause ^ clause_body,

        % Process any polymorphic calls inside the goal.
        empty_cache_maps(!Info),
        poly_info_set_num_reuses(0, !Info),
        polymorphism_process_goal(Goal0, Goal1, !Info),

        % Generate code to construct the typeclass_infos and type_infos
        % for existentially quantified type vars.
        produce_existq_tvars(PredInfo0, OldHeadVars,
            UnconstrainedTVars, ExtraTypeInfoHeadVars,
            ExistTypeClassInfoHeadVars, Goal1, Goal2, !Info),

        pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
        fixup_quantification(NewHeadVars, ExistQVars, Goal2, Goal, !Info),
        !Clause ^ clause_body := Goal
    ).

:- pred add_extra_arg_modes_to_proc(poly_arg_vector(mer_mode)::in,
    proc_info::in, proc_info::out) is det.

add_extra_arg_modes_to_proc(ExtraArgModes, !ProcInfo) :-
    ( if proc_info_is_valid_mode(!.ProcInfo) then
        % Add the ExtraArgModes to the proc_info argmodes.
        % XXX ARGVEC - revisit this when the proc_info uses proc_arg_vectors.
        proc_info_get_argmodes(!.ProcInfo, ArgModes1),
        ExtraArgModesList = poly_arg_vector_to_list(ExtraArgModes),
        ArgModes = ExtraArgModesList ++ ArgModes1,
        proc_info_set_argmodes(ArgModes, !ProcInfo)
    else
        true
    ).

    % XXX document me
    %
    % XXX the following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred setup_headvars(pred_info::in, proc_arg_vector(prog_var)::in,
    proc_arg_vector(prog_var)::out, poly_arg_vector(mer_mode)::out,
    list(tvar)::out, list(prog_var)::out, list(prog_var)::out,
    poly_info::in, poly_info::out) is det.

setup_headvars(PredInfo, !HeadVars, ExtraArgModes,
        UnconstrainedTVars, ExtraHeadTypeInfoVars,
        ExistHeadTypeClassInfoVars, !Info) :-
    pred_info_get_origin(PredInfo, Origin),
    ExtraArgModes0 = poly_arg_vector_init : poly_arg_vector(mer_mode),
    (
        Origin = origin_instance_method(_, InstanceMethodConstraints),
        setup_headvars_instance_method(PredInfo,
            InstanceMethodConstraints, !HeadVars,
            UnconstrainedTVars, ExtraHeadTypeInfoVars,
            ExistHeadTypeClassInfoVars,
            ExtraArgModes0, ExtraArgModes, !Info)
    ;
        ( Origin = origin_special_pred(_, _)
        ; Origin = origin_class_method(_, _)
        ; Origin = origin_transformed(_, _, _)
        ; Origin = origin_created(_)
        ; Origin = origin_assertion(_, _)
        ; Origin = origin_lambda(_, _, _)
        ; Origin = origin_solver_type(_, _, _)
        ; Origin = origin_tabling(_, _)
        ; Origin = origin_mutable(_, _, _)
        ; Origin = origin_initialise
        ; Origin = origin_finalise
        ; Origin = origin_user(_)
        ),
        pred_info_get_class_context(PredInfo, ClassContext),
        InstanceTVars = [],
        InstanceUnconstrainedTVars = [],
        InstanceUnconstrainedTypeInfoVars = [],
        setup_headvars_2(PredInfo, ClassContext, InstanceTVars,
            InstanceUnconstrainedTVars, InstanceUnconstrainedTypeInfoVars,
            !HeadVars, UnconstrainedTVars,
            ExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
            ExtraArgModes0, ExtraArgModes, !Info)
    ).

    % For class method implementations, do_call_class_method takes the
    % type_infos and typeclass_infos from the typeclass_info and pastes them
    % onto the front of the argument list. We need to match that order here.
    %
:- pred setup_headvars_instance_method(pred_info::in,
    instance_method_constraints::in,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::out,
    list(tvar)::out, list(prog_var)::out, list(prog_var)::out,
    poly_arg_vector(mer_mode)::in, poly_arg_vector(mer_mode)::out,
    poly_info::in, poly_info::out) is det.

setup_headvars_instance_method(PredInfo,
        InstanceMethodConstraints, !HeadVars,
        UnconstrainedTVars, ExtraHeadTypeInfoVars,
        ExistHeadTypeClassInfoVars, !ExtraArgModes, !Info) :-
    InstanceMethodConstraints = instance_method_constraints(_,
        InstanceTypes, InstanceConstraints, ClassContext),

    type_vars_in_types(InstanceTypes, InstanceTVars),
    get_unconstrained_tvars(InstanceTVars, InstanceConstraints,
        UnconstrainedInstanceTVars),
    pred_info_get_arg_types(PredInfo, ArgTypeVarSet, _, _),
    make_head_vars(UnconstrainedInstanceTVars,
        ArgTypeVarSet, UnconstrainedInstanceTypeInfoVars, !Info),
    make_typeclass_info_head_vars(do_record_type_info_locns,
        InstanceConstraints, InstanceHeadTypeClassInfoVars, !Info),

    proc_arg_vector_set_instance_type_infos(UnconstrainedInstanceTypeInfoVars,
        !HeadVars),
    proc_arg_vector_set_instance_typeclass_infos(InstanceHeadTypeClassInfoVars,
         !HeadVars),

    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var,
        InstanceHeadTypeClassInfoVars, RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),

    in_mode(InMode),
    list.duplicate(list.length(UnconstrainedInstanceTypeInfoVars),
        InMode, UnconstrainedInstanceTypeInfoModes),
    list.duplicate(list.length(InstanceHeadTypeClassInfoVars),
        InMode, InstanceHeadTypeClassInfoModes),
    poly_arg_vector_set_instance_type_infos(
        UnconstrainedInstanceTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_instance_typeclass_infos(
        InstanceHeadTypeClassInfoModes, !ExtraArgModes),

    setup_headvars_2(PredInfo, ClassContext,
        InstanceTVars,
        UnconstrainedInstanceTVars, UnconstrainedInstanceTypeInfoVars,
        !HeadVars,
        UnconstrainedTVars, ExtraHeadTypeInfoVars,
        ExistHeadTypeClassInfoVars, !ExtraArgModes, !Info).

:- pred setup_headvars_2(pred_info::in, prog_constraints::in,
    list(tvar)::in, list(tvar)::in, list(prog_var)::in,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::out,
    list(tvar)::out, list(prog_var)::out, list(prog_var)::out,
    poly_arg_vector(mer_mode)::in, poly_arg_vector(mer_mode)::out,
    poly_info::in, poly_info::out) is det.

setup_headvars_2(PredInfo, ClassContext,
        InstanceTVars, UnconstrainedInstanceTVars,
        UnconstrainedInstanceTypeInfoVars, HeadVars0,
        HeadVars, AllUnconstrainedTVars,
        AllExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
        !ExtraArgModes, !Info) :-
    % Grab the appropriate fields from the pred_info.
    pred_info_get_arg_types(PredInfo, ArgTypeVarSet, ExistQVars, ArgTypes),

    % Insert extra head variables to hold the address of the type_infos
    % and typeclass_infos. We insert one variable for each unconstrained
    % type variable (for the type_info) and one variable for each
    % constraint (for the typeclass_info).
    %
    % The order of these variables is important, and must match the order
    % specified at the top of this file.

    % Make a fresh variable for each class constraint, returning a list of
    % variables that appear in the constraints, along with the location of
    % the type infos for them. For the existential constraints, we want
    % the rtti_varmaps to contain the internal view of the types (that is,
    % with type variables bound) so we may need to look up the actual
    % constraints in the constraint map. For the universal constraints there
    % is no distinction between the internal views and the external view, so
    % we just use the constraints from the class context.
    ClassContext = constraints(UnivConstraints, ExistConstraints),
    prog_type.constraint_list_get_tvars(UnivConstraints,
        UnivConstrainedTVars),
    prog_type.constraint_list_get_tvars(ExistConstraints,
        ExistConstrainedTVars),
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    get_improved_exists_head_constraints(ConstraintMap, ExistConstraints,
        ActualExistConstraints),
    ( if
        pred_info_get_markers(PredInfo, PredMarkers),
        check_marker(PredMarkers, marker_class_method)
    then
        % For class methods we record the type_info_locns even for the
        % existential constraints. It is easier to do it here than when we
        % are expanding class method bodies, and we know there won't be any
        % references to the type_info after the instance method call so
        % recording them now won't be a problem.
        RecordExistQLocns = do_record_type_info_locns
    else
        RecordExistQLocns = do_not_record_type_info_locns
    ),
    make_typeclass_info_head_vars(RecordExistQLocns, ActualExistConstraints,
        ExistHeadTypeClassInfoVars, !Info),
    make_typeclass_info_head_vars(do_record_type_info_locns, UnivConstraints,
        UnivHeadTypeClassInfoVars, !Info),

    type_vars_in_types(ArgTypes, HeadTypeVars),
    list.delete_elems(HeadTypeVars, UnivConstrainedTVars,
        UnconstrainedTVars0),
    list.delete_elems(UnconstrainedTVars0, ExistConstrainedTVars,
        UnconstrainedTVars1),

    % Typeinfos for the instance tvars have already been introduced by
    % setup_headvars_instance_method.
    list.delete_elems(UnconstrainedTVars1, InstanceTVars,
        UnconstrainedTVars2),
    list.remove_dups(UnconstrainedTVars2, UnconstrainedTVars),

    (
        ExistQVars = [],
        % Optimize common case.
        UnconstrainedUnivTVars = UnconstrainedTVars,
        UnconstrainedExistTVars = [],
        ExistHeadTypeInfoVars = []
    ;
        ExistQVars = [_ | _],
        list.delete_elems(UnconstrainedTVars, ExistQVars,
            UnconstrainedUnivTVars),
        list.delete_elems(UnconstrainedTVars, UnconstrainedUnivTVars,
            UnconstrainedExistTVars),
        make_head_vars(UnconstrainedExistTVars, ArgTypeVarSet,
            ExistHeadTypeInfoVars, !Info)
    ),

    make_head_vars(UnconstrainedUnivTVars, ArgTypeVarSet,
        UnivHeadTypeInfoVars, !Info),
    ExtraHeadTypeInfoVars = UnivHeadTypeInfoVars ++ ExistHeadTypeInfoVars,

    AllExtraHeadTypeInfoVars = UnconstrainedInstanceTypeInfoVars
        ++ ExtraHeadTypeInfoVars,
    list.condense([UnconstrainedInstanceTVars, UnconstrainedUnivTVars,
        UnconstrainedExistTVars], AllUnconstrainedTVars),

    proc_arg_vector_set_univ_type_infos(UnivHeadTypeInfoVars,
        HeadVars0, HeadVars1),
    proc_arg_vector_set_exist_type_infos(ExistHeadTypeInfoVars,
        HeadVars1, HeadVars2),
    proc_arg_vector_set_univ_typeclass_infos(UnivHeadTypeClassInfoVars,
        HeadVars2, HeadVars3),
    proc_arg_vector_set_exist_typeclass_infos(ExistHeadTypeClassInfoVars,
        HeadVars3, HeadVars),

    % Figure out the modes of the introduced type_info and typeclass_info
    % arguments.

    in_mode(In),
    out_mode(Out),
    list.length(UnconstrainedUnivTVars, NumUnconstrainedUnivTVars),
    list.length(UnconstrainedExistTVars, NumUnconstrainedExistTVars),
    list.length(UnivHeadTypeClassInfoVars, NumUnivClassInfoVars),
    list.length(ExistHeadTypeClassInfoVars, NumExistClassInfoVars),
    list.duplicate(NumUnconstrainedUnivTVars, In, UnivTypeInfoModes),
    list.duplicate(NumUnconstrainedExistTVars, Out, ExistTypeInfoModes),
    list.duplicate(NumUnivClassInfoVars, In, UnivTypeClassInfoModes),
    list.duplicate(NumExistClassInfoVars, Out, ExistTypeClassInfoModes),
    poly_arg_vector_set_univ_type_infos(UnivTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_exist_type_infos(ExistTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_univ_typeclass_infos(UnivTypeClassInfoModes,
        !ExtraArgModes),
    poly_arg_vector_set_exist_typeclass_infos(ExistTypeClassInfoModes,
        !ExtraArgModes),

    % Add the locations of the typeinfos for unconstrained, universally
    % quantified type variables to the initial rtti_varmaps. Also add the
    % locations of typeclass_infos.
    some [!RttiVarMaps] (
        poly_info_get_rtti_varmaps(!.Info, !:RttiVarMaps),

        list.map(var_as_type_info_locn, UnivHeadTypeInfoVars, UnivTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedUnivTVars, UnivTypeLocns, !RttiVarMaps),

        list.map(var_as_type_info_locn, ExistHeadTypeInfoVars, ExistTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedExistTVars, ExistTypeLocns, !RttiVarMaps),

        list.map(var_as_type_info_locn,
            UnconstrainedInstanceTypeInfoVars, UnconstrainedInstanceTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedInstanceTVars, UnconstrainedInstanceTypeLocns,
            !RttiVarMaps),

        list.foldl(rtti_reuse_typeclass_info_var, UnivHeadTypeClassInfoVars,
            !RttiVarMaps),

        poly_info_set_rtti_varmaps(!.RttiVarMaps, !Info)
    ).

:- pred make_head_vars(list(tvar)::in, tvarset::in,
    list(prog_var)::out, poly_info::in, poly_info::out) is det.

make_head_vars([], _, [], !Info).
make_head_vars([TypeVar | TypeVars], TypeVarSet, TypeInfoVars, !Info) :-
    poly_info_get_tvar_kind_map(!.Info, TVarKindMap),
    get_tvar_kind(TVarKindMap, TypeVar, Kind),
    Type = type_variable(TypeVar, Kind),
    new_type_info_var(Type, type_info, Var, !Info),
    ( if varset.search_name(TypeVarSet, TypeVar, TypeVarName) then
        poly_info_get_varset(!.Info, VarSet0),
        VarName = "TypeInfo_for_" ++ TypeVarName,
        varset.name_var(Var, VarName, VarSet0, VarSet),
        poly_info_set_varset(VarSet, !Info)
    else
        true
    ),
    make_head_vars(TypeVars, TypeVarSet, TypeInfoVars1, !Info),
    TypeInfoVars = [Var | TypeInfoVars1].

:- pred var_as_type_info_locn(prog_var::in, type_info_locn::out) is det.

var_as_type_info_locn(Var, type_info(Var)).

    % Generate code to produce the values of type_infos and typeclass_infos
    % for existentially quantified type variables in the head.
    %
    % XXX The following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred produce_existq_tvars(pred_info::in, proc_arg_vector(prog_var)::in,
    list(tvar)::in, list(prog_var)::in, list(prog_var)::in,
    hlds_goal::in, hlds_goal::out, poly_info::in, poly_info::out) is det.

produce_existq_tvars(PredInfo, HeadVars, UnconstrainedTVars,
        TypeInfoHeadVars, ExistTypeClassInfoHeadVars, Goal0, Goal, !Info) :-
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_tvar_kind_map(PredInfo, KindMap),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % Generate code to produce values for any existentially quantified
    % typeclass_info variables in the head.

    PredExistConstraints = PredClassContext ^ exist_constraints,
    get_improved_exists_head_constraints(ConstraintMap, PredExistConstraints,
        ActualExistConstraints),
    ExistQVarsForCall = [],
    Goal0 = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    make_typeclass_info_vars(ActualExistConstraints, ExistQVarsForCall,
        Context, ExistTypeClassVarsMCAs, ExtraTypeClassGoals, !Info),
    assoc_list.keys(ExistTypeClassVarsMCAs, ExistTypeClassVars),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var, ExistTypeClassVars,
        RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),
    make_complicated_unify_assigns(ExistTypeClassInfoHeadVars,
        ExistTypeClassVars, ExtraTypeClassUnifyGoals),

    % Figure out the bindings for any unconstrained existentially quantified
    % type variables in the head.

    ( if
        vartypes_is_empty(VarTypes0)
    then
        % This can happen for compiler generated procedures.
        map.init(PredToActualTypeSubst)
    else if
        HeadVarList = proc_arg_vector_to_list(HeadVars),
        lookup_var_types(VarTypes0, HeadVarList, ActualArgTypes),
        type_list_subsumes(ArgTypes, ActualArgTypes, ArgTypeSubst)
    then
        PredToActualTypeSubst = ArgTypeSubst
    else
        % This can happen for unification procedures of equivalence types
        % error("polymorphism.m: type_list_subsumes failed")
        map.init(PredToActualTypeSubst)
    ),

    % Apply the type bindings to the unconstrained type variables to give
    % the actual types, and then generate code to initialize the type_infos
    % for those types.

    apply_subst_to_tvar_list(KindMap, PredToActualTypeSubst,
        UnconstrainedTVars, ActualTypes),
    polymorphism_do_make_type_info_vars(ActualTypes, Context,
        TypeInfoVarsMCAs, ExtraTypeInfoGoals, !Info),
    assoc_list.keys(TypeInfoVarsMCAs, TypeInfoVars),
    make_complicated_unify_assigns(TypeInfoHeadVars, TypeInfoVars,
        ExtraTypeInfoUnifyGoals),
    list.condense([[Goal0 | ExtraTypeClassGoals], ExtraTypeClassUnifyGoals,
        ExtraTypeInfoGoals, ExtraTypeInfoUnifyGoals], GoalList),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

:- pred get_improved_exists_head_constraints(constraint_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

get_improved_exists_head_constraints(ConstraintMap,  ExistConstraints,
        ActualExistConstraints) :-
    list.length(ExistConstraints, NumExistConstraints),
    ( if
        search_hlds_constraint_list(ConstraintMap, unproven,
            goal_id_for_head_constraints, NumExistConstraints,
            ActualExistConstraintsPrime)
    then
        ActualExistConstraints = ActualExistConstraintsPrime
    else
        % Some predicates, for example typeclass methods and predicates for
        % which we inferred the type, don't have constraint map entries for
        % the head constraints. In these cases we can just use the external
        % constraints, since there can't be any difference between them and
        % the internal ones.
        ActualExistConstraints = ExistConstraints
    ).

%---------------------------------------------------------------------------%

:- pred polymorphism_process_goal(hlds_goal::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        % We don't need to add type_infos for higher order calls, since the
        % type_infos are added when the closures are constructed, not when
        % they are called.
        GoalExpr0 = generic_call(_, _, _, _, _),
        Goal = Goal0
    ;
        GoalExpr0 = plain_call(PredId, _, ArgVars0, _, _, _),
        polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
            ExtraVars, ExtraGoals, !Info),
        ArgVars = ExtraVars ++ ArgVars0,
        CallExpr = GoalExpr0 ^ call_args := ArgVars,
        Call = hlds_goal(CallExpr, GoalInfo),
        GoalList = ExtraGoals ++ [Call],
        conj_list_to_goal(GoalList, GoalInfo0, Goal)
    ;
        GoalExpr0 = call_foreign_proc(_, PredId, _, _, _, _, _),
        poly_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),

        ( if no_type_info_builtin(PredModule, PredName, PredArity) then
            Goal = Goal0
        else
            polymorphism_process_foreign_proc(PredInfo, GoalExpr0, GoalInfo0,
                Goal, !Info)
        )
    ;
        GoalExpr0 = unify(LHSVar, RHS, Mode, Unification, UnifyContext),
        polymorphism_process_unify(LHSVar, RHS, Mode, Unification, UnifyContext,
            GoalInfo0, Goal, !Info)
    ;
        % The rest of the cases just process goals recursively.
        (
            GoalExpr0 = conj(ConjType, Goals0),
            (
                ConjType = plain_conj,
                polymorphism_process_plain_conj(Goals0, Goals, !Info)
            ;
                ConjType = parallel_conj,
                get_cache_maps_snapshot("parconj", InitialSnapshot, !Info),
                polymorphism_process_par_conj(Goals0, Goals, InitialSnapshot,
                    !Info)
                % Unlike with disjunctions, we do not have to reset to
                % InitialSnapshot.
            ),
            GoalExpr = conj(ConjType, Goals)
        ;
            GoalExpr0 = disj(Goals0),
            get_cache_maps_snapshot("disj", InitialSnapshot, !Info),
            polymorphism_process_disj(Goals0, Goals, InitialSnapshot, !Info),
            set_cache_maps_snapshot("after disj", InitialSnapshot, !Info),
            GoalExpr = disj(Goals)
        ;
            GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
            get_cache_maps_snapshot("ite", InitialSnapshot, !Info),
            polymorphism_process_goal(Cond0, Cond, !Info),
            % If we allowed a type_info created inside Cond to be reused
            % in Then, then we are adding an output variable to Cond.
            % If Cond scope had no outputs to begin with, this would change
            % its determinism.
            set_cache_maps_snapshot("before then", InitialSnapshot, !Info),
            polymorphism_process_goal(Then0, Then, !Info),
            set_cache_maps_snapshot("before else", InitialSnapshot, !Info),
            polymorphism_process_goal(Else0, Else, !Info),
            set_cache_maps_snapshot("after ite", InitialSnapshot, !Info),
            GoalExpr = if_then_else(Vars, Cond, Then, Else)
        ;
            GoalExpr0 = negation(SubGoal0),
            get_cache_maps_snapshot("neg", InitialSnapshot, !Info),
            polymorphism_process_goal(SubGoal0, SubGoal, !Info),
            set_cache_maps_snapshot("after neg", InitialSnapshot, !Info),
            GoalExpr = negation(SubGoal)
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            get_cache_maps_snapshot("switch", InitialSnapshot, !Info),
            polymorphism_process_cases(Cases0, Cases, InitialSnapshot, !Info),
            set_cache_maps_snapshot("after switch", InitialSnapshot, !Info),
            GoalExpr = switch(Var, CanFail, Cases)
        ;
            GoalExpr0 = scope(Reason0, SubGoal0),
            (
                Reason0 = from_ground_term(TermVar, Kind),
                (
                    Kind = from_ground_term_initial,
                    polymorphism_process_from_ground_term_initial(TermVar,
                        GoalInfo0, SubGoal0, GoalExpr, !Info)
                ;
                    ( Kind = from_ground_term_construct
                    ; Kind = from_ground_term_deconstruct
                    ; Kind = from_ground_term_other
                    ),
                    polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                    GoalExpr = scope(Reason0, SubGoal)
                )
            ;
                Reason0 = promise_solutions(_, _),
                % polymorphism_process_goal may cause SubGoal to bind
                % variables (such as PolyConst variables) that SubGoal0 does
                % not bind. If we allowed such variables to be reused outside
                % the scope, that would change the set of variables that the
                % promise would have to cover. We cannot expect and do not want
                % user level programmers making promises about variables added
                % by the compiler.
                get_cache_maps_snapshot("promise_solns", InitialSnapshot,
                    !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after promise_solns", InitialSnapshot,
                    !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                ( Reason0 = disable_warnings(_, _)
                ; Reason0 = promise_purity(_)
                ; Reason0 = require_detism(_)
                ; Reason0 = require_complete_switch(_)
                ; Reason0 = require_switch_arms_detism(_, _)
                ; Reason0 = commit(_)
                ; Reason0 = barrier(_)
                ; Reason0 = loop_control(_, _, _)
                ),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                Reason0 = exist_quant(_),
                % If we allowed a type_info created inside SubGoal to be reused
                % outside GoalExpr, then we are adding an output variable to
                % the scope. If the scope had no outputs to begin with, this
                % would change the determinism of the scope.
                %
                % However, using a type_info from before the scope in SubGoal
                % is perfectly ok.

                get_cache_maps_snapshot("exists", InitialSnapshot, !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after exists", InitialSnapshot,
                    !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                Reason0 = trace_goal(_, _, _, _, _),
                % Trace goal scopes will be deleted after semantic analysis
                % if their compile-time condition turns out to be false.
                % If we let later code use type_infos introduced inside the
                % scope, the deletion of the scope would leave those variables
                % undefined.
                %
                % We *could* evaluate the compile-time condition here to know
                % whether the deletion will happen or not, but doing so would
                % require breaching the separation between compiler passes.

                get_cache_maps_snapshot("trace", InitialSnapshot, !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after trace", InitialSnapshot, !Info),
                GoalExpr = scope(Reason0, SubGoal)
            )
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, Vars,
                MainGoal0, OrElseGoals0, OrElseInners),
            get_cache_maps_snapshot("atomic", InitialSnapshot, !Info),
            polymorphism_process_goal(MainGoal0, MainGoal, !Info),
            polymorphism_process_disj(OrElseGoals0, OrElseGoals,
                InitialSnapshot, !Info),
            set_cache_maps_snapshot("after atomic", InitialSnapshot, !Info),
            ShortHand = atomic_goal(GoalType, Outer, Inner, Vars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            % We don't let the code inside and outside the try goal share
            % type_info variables for the same reason as with lambda
            % expressions; because those pieces of code will end up
            % in different procedures. However, for try goals, this is true
            % even for the first and second conjuncts.
            get_cache_maps_snapshot("try", InitialSnapshot, !Info),
            ( if
                SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo),
                SubGoalExpr0 = conj(plain_conj, Conjuncts0),
                Conjuncts0 = [ConjunctA0, ConjunctB0]
            then
                empty_cache_maps(!Info),
                polymorphism_process_goal(ConjunctA0, ConjunctA, !Info),
                empty_cache_maps(!Info),
                polymorphism_process_goal(ConjunctB0, ConjunctB, !Info),

                Conjuncts = [ConjunctA, ConjunctB],
                SubGoalExpr = conj(plain_conj, Conjuncts),
                SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo)
            else
                unexpected($pred, "malformed try goal")
            ),
            set_cache_maps_snapshot("after try", InitialSnapshot, !Info),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ).

%---------------------------------------------------------------------------%

:- pred polymorphism_process_from_ground_term_initial(prog_var::in,
    hlds_goal_info::in, hlds_goal::in, hlds_goal_expr::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_from_ground_term_initial(TermVar, GoalInfo0, SubGoal0,
        GoalExpr, !Info) :-
    SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo0),
    ( if SubGoalExpr0 = conj(plain_conj, SubGoals0Prime) then
        SubGoals0 = SubGoals0Prime
    else
        unexpected($pred, "from_ground_term_initial goal is not plain conj")
    ),
    polymorphism_process_fgti_goals(SubGoals0,
        [], ConstructOrderMarkedSubGoals,
        fgt_invariants_kept, InvariantsStatus, !Info),
    (
        InvariantsStatus = fgt_invariants_kept,
        Reason = from_ground_term(TermVar, from_ground_term_initial),
        GoalExpr = scope(Reason, SubGoal0)
    ;
        InvariantsStatus = fgt_invariants_broken,
        introduce_partial_fgt_scopes(GoalInfo0, SubGoalInfo0,
            ConstructOrderMarkedSubGoals, deconstruct_top_down, SubGoal),
        % Delete the scope wrapper around SubGoal0.
        SubGoal = hlds_goal(GoalExpr, _)
    ).

:- pred polymorphism_process_fgti_goals(list(hlds_goal)::in,
    list(fgt_marked_goal)::in, list(fgt_marked_goal)::out,
    fgt_invariants_status::in, fgt_invariants_status::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_fgti_goals([], !ConstructOrderMarkedGoals,
        !InvariantsStatus, !Info).
polymorphism_process_fgti_goals([Goal0 | Goals0], !ConstructOrderMarkedGoals,
        !InvariantsStatus, !Info) :-
    % This is used only if polymorphism_fgt_sanity_tests is enabled.
    OldInfo = !.Info,
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        GoalExpr0 = unify(LHSVarPrime, RHS, ModePrime, UnificationPrime,
            UnifyContextPrime),
        RHS = rhs_functor(ConsIdPrime, _, RHSVarsPrime)
    then
        LHSVar = LHSVarPrime,
        Mode = ModePrime,
        Unification = UnificationPrime,
        UnifyContext = UnifyContextPrime,
        ConsId = ConsIdPrime,
        RHSVars = RHSVarsPrime
    else
        unexpected($pred,
            "from_ground_term_initial conjunct is not functor unify")
    ),
    polymorphism_process_unify_functor(LHSVar, ConsId, RHSVars, Mode,
        Unification, UnifyContext, GoalInfo0, Goal, Changed, !Info),
    (
        Changed = no,
        trace [compiletime(flag("polymorphism_fgt_sanity_tests"))] (
            poly_info_get_varset(OldInfo, VarSetBefore),
            MaxVarBefore = varset.max_var(VarSetBefore),
            poly_info_get_num_reuses(OldInfo, NumReusesBefore),

            poly_info_get_varset(!.Info, VarSetAfter),
            MaxVarAfter = varset.max_var(VarSetAfter),
            poly_info_get_num_reuses(!.Info, NumReusesAfter),

            expect(unify(MaxVarBefore, MaxVarAfter), $pred,
                "MaxVarBefore != MaxVarAfter"),
            expect(unify(NumReusesBefore, NumReusesAfter), $pred,
                "NumReusesBefore != NumReusesAfter"),
            expect(unify(Goal0, Goal), $pred,
                "Goal0 != Goal")
        ),
        MarkedGoal = fgt_kept_goal(Goal0, LHSVar, RHSVars)
    ;
        Changed = yes,
        MarkedGoal = fgt_broken_goal(Goal, LHSVar, RHSVars),
        !:InvariantsStatus = fgt_invariants_broken
    ),
    !:ConstructOrderMarkedGoals = [MarkedGoal | !.ConstructOrderMarkedGoals],
    polymorphism_process_fgti_goals(Goals0, !ConstructOrderMarkedGoals,
        !InvariantsStatus, !Info).

%---------------------------------------------------------------------------%

:- pred polymorphism_process_unify(prog_var::in, unify_rhs::in,
    unify_mode::in, unification::in, unify_context::in, hlds_goal_info::in,
    hlds_goal::out, poly_info::in, poly_info::out) is det.

polymorphism_process_unify(LHSVar, RHS0, Mode, Unification0, UnifyContext,
        GoalInfo0, Goal, !Info) :-
    (
        RHS0 = rhs_var(_RHSVar),

        % Var-var unifications (simple_test, assign, or complicated_unify)
        % are basically left unchanged. Complicated unifications will
        % eventually get converted into calls, but that is done later on,
        % by simplify.m, not now. At this point we just need to figure out
        % which type_info/typeclass_info variables the unification might need,
        % and insert them in the nonlocals. We have to do that for all var-var
        % unifications, because at this point we haven't done mode analysis so
        % we don't know which ones will become complicated_unifies.
        % Note that we also store the type_info/typeclass_info variables
        % in a field in the unification, which quantification.m uses when
        % requantifying things.

        poly_info_get_var_types(!.Info, VarTypes),
        lookup_var_type(VarTypes, LHSVar, Type),
        unification_typeinfos(Type, Unification0, Unification,
            GoalInfo0, GoalInfo, _Changed, !Info),
        Goal = hlds_goal(unify(LHSVar, RHS0, Mode, Unification, UnifyContext),
            GoalInfo)
    ;
        RHS0 = rhs_functor(ConsId, _, Args),
        polymorphism_process_unify_functor(LHSVar, ConsId, Args, Mode,
            Unification0, UnifyContext, GoalInfo0, Goal, _Changed, !Info)
    ;
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals0, ArgVarsModes, Det, LambdaGoal0),
        % For lambda expressions, we must recursively traverse the lambda goal.
        % Any type_info variables needed by the lambda goal are created in the
        % lambda goal (not imported from the outside), and any type_info
        % variables created by the lambda goal are not available outside.
        % This is because, after lambda expansion, the code inside and outside
        % the lambda goal will end up in different procedures.
        get_cache_maps_snapshot("lambda", InitialSnapshot, !Info),
        empty_cache_maps(!Info),
        polymorphism_process_goal(LambdaGoal0, LambdaGoal1, !Info),
        set_cache_maps_snapshot("after lambda", InitialSnapshot, !Info),

        assoc_list.keys(ArgVarsModes, ArgVars),
        % Currently we don't allow lambda goals to be existentially typed.
        ExistQVars = [],
        fixup_lambda_quantification(LambdaNonLocals0, ArgVars,
            ExistQVars, LambdaGoal1, LambdaGoal,
            LambdaTiTciVars, PossibleNonLocalTiTciVars, !Info),
        LambdaNonLocals1 =
            set_of_var.to_sorted_list(LambdaTiTciVars) ++
            set_of_var.to_sorted_list(PossibleNonLocalTiTciVars) ++
            LambdaNonLocals0,
        list.sort_and_remove_dups(LambdaNonLocals1, LambdaNonLocals),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals, ArgVarsModes, Det, LambdaGoal),
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.union(PossibleNonLocalTiTciVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),

        % Complicated (in-in) argument unifications are impossible for lambda
        % expressions, so we don't need to worry about adding the type_infos
        % that would be required for such unifications.
        GoalExpr = unify(LHSVar, RHS, Mode, Unification0, UnifyContext),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred unification_typeinfos(mer_type::in,
    unification::in, unification::out, hlds_goal_info::in, hlds_goal_info::out,
    bool::out, poly_info::in, poly_info::out) is det.

unification_typeinfos(Type, !Unification, !GoalInfo, Changed, !Info) :-
    % Compute the type_info/type_class_info variables that would be used
    % if this unification ends up being a complicated_unify.
    type_vars_in_type(Type, TypeVars),
    (
        TypeVars = [],
        Changed = no
    ;
        TypeVars = [_ | _],
        list.map_foldl(get_type_info_locn, TypeVars, TypeInfoLocns, !Info),
        add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo),
        Changed = yes
    ).

unification_typeinfos_rtti_varmaps(Type, RttiVarMaps,
        !Unification, !GoalInfo) :-
    % This variant is for use by modecheck_unify.m. During mode checking,
    % all the type_infos should appear in the type_info_varmap.

    % Compute the type_info/type_class_info variables that would be used
    % if this unification ends up being a complicated_unify.
    type_vars_in_type(Type, TypeVars),
    list.map(rtti_lookup_type_info_locn(RttiVarMaps), TypeVars,
        TypeInfoLocns),
    add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo).

:- pred add_unification_typeinfos(list(type_info_locn)::in,
    unification::in, unification::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo) :-
    list.map(type_info_locn_var, TypeInfoLocns, TypeInfoVars0),
    list.remove_dups(TypeInfoVars0, TypeInfoVars),

    % Insert the TypeInfoVars into the nonlocals field of the goal_info
    % for the unification goal.
    NonLocals0 = goal_info_get_nonlocals(!.GoalInfo),
    set_of_var.insert_list(TypeInfoVars, NonLocals0, NonLocals),
    goal_info_set_nonlocals(NonLocals, !GoalInfo),

    % Also save those type_info vars into a field in the complicated_unify,
    % so that quantification.m can recompute variable scopes properly.
    % This field is also used by modecheck_unify.m -- for complicated
    % unifications, it checks that all these variables are ground.
    (
        !.Unification = complicated_unify(Modes, CanFail, _),
        !:Unification = complicated_unify(Modes, CanFail, TypeInfoVars)
    ;
        % This can happen if an earlier stage of compilation has already
        % determined that this unification is particular kind of unification.
        % In that case, the type_info vars won't be needed.
        ( !.Unification = construct(_, _, _, _, _, _, _)
        ; !.Unification = deconstruct(_, _, _, _, _, _)
        ; !.Unification = assign(_, _)
        ; !.Unification = simple_test(_, _)
        )
    ).

:- pred polymorphism_process_unify_functor(prog_var::in, cons_id::in,
    list(prog_var)::in, unify_mode::in, unification::in, unify_context::in,
    hlds_goal_info::in, hlds_goal::out, bool::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_unify_functor(X0, ConsId0, ArgVars0, Mode0, Unification0,
        UnifyContext, GoalInfo0, Goal, Changed, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo0),
    poly_info_get_var_types(!.Info, VarTypes0),
    lookup_var_type(VarTypes0, X0, TypeOfX),
    list.length(ArgVars0, Arity),

    % We replace any unifications with higher order pred constants
    % by lambda expressions. For example, we replace
    %
    %   X = list.append(Y)     % Y::in, X::out
    %
    % with
    %
    %   X = (
    %       pred(A1::in, A2::out) is ... :- list.append(Y, A1, A2)
    %   )
    %
    % We do this because it makes two things easier. First, mode analysis
    % needs to check that the lambda goal doesn't bind any nonlocal variables
    % (e.g. `Y' in above example). This would require a bit of moderately
    % tricky special case code if we didn't expand them here. Second, this pass
    % (polymorphism.m) is a lot easier if we don't have to handle higher order
    % constants. If it turns out that the predicate was nonpolymorphic,
    % lambda.m will turn the lambda expression back into a higher order
    % constant again.
    %
    % Note that this transformation is also done by modecheck_unify.m, in case
    % we are rerunning mode analysis after lambda.m has already been run;
    % any changes to the code here will also need to be duplicated there.

    ( if
        % Check if variable has a higher order type.
        ConsId0 = closure_cons(ShroudedPredProcId, _),
        proc(PredId, ProcId0) = unshroud_pred_proc_id(ShroudedPredProcId),
        type_is_higher_order_details(TypeOfX, Purity, _PredOrFunc, EvalMethod,
            CalleeArgTypes)
    then
        % An `invalid_proc_id' means the predicate is multi-moded. We can't
        % pick the right mode yet. Perform the rest of the transformation with
        % any mode (the first) but mark the goal with a feature so that mode
        % checking knows to fix it up later.
        ( if ProcId0 = invalid_proc_id then
            module_info_pred_info(ModuleInfo0, PredId, PredInfo),
            ProcIds = pred_info_valid_procids(PredInfo),
            (
                ProcIds = [ProcId | _],
                goal_info_add_feature(feature_lambda_undetermined_mode,
                    GoalInfo0, GoalInfo1)
            ;
                ProcIds = [],
                unexpected($pred, "no modes")
            )
        else
            ProcId = ProcId0,
            GoalInfo1 = GoalInfo0
        ),
        % Convert the higher order pred term to a lambda goal.
        poly_info_get_varset(!.Info, VarSet0),
        Context = goal_info_get_context(GoalInfo0),
        convert_pred_to_lambda_goal(Purity, EvalMethod, X0, PredId, ProcId,
            ArgVars0, CalleeArgTypes, UnifyContext, GoalInfo1, Context,
            ModuleInfo0, MaybeRHS0, VarSet0, VarSet, VarTypes0, VarTypes),
        poly_info_set_varset_types(VarSet, VarTypes, !Info),
        (
            MaybeRHS0 = ok1(RHS0),
            % Process the unification in its new form.
            polymorphism_process_unify(X0, RHS0, Mode0, Unification0,
                UnifyContext, GoalInfo1, Goal, !Info)
        ;
            MaybeRHS0 = error1(Specs),
            poly_info_get_errors(!.Info, Specs0),
            poly_info_set_errors(Specs ++ Specs0, !Info),
            % It doesn't matter what Goal we return, since it won't be used.
            RHS = rhs_functor(some_int_const(int_const(42)),
                is_not_exist_constr, []),
            polymorphism_process_unify(X0, RHS, Mode0, Unification0,
                UnifyContext, GoalInfo1, Goal, !Info)
        ),
        Changed = yes
    else if
        % Is this a construction or deconstruction of an existentially
        % typed data type?
        %
        % Check whether the functor had a "new " prefix.
        % If so, assume it is a construction, and strip off the prefix.
        % Otherwise, assume it is a deconstruction.

        ConsId0 = cons(Functor0, Arity, ConsTypeCtor),
        ( if remove_new_prefix(Functor0, OrigFunctor) then
            ConsId = cons(OrigFunctor, Arity, ConsTypeCtor),
            IsExistConstr = is_exist_constr
        else
            ConsId = ConsId0,
            IsExistConstr = is_not_exist_constr
        ),

        % Check whether the functor (with the "new " prefix removed)
        % is an existentially typed functor.
        type_util.get_existq_cons_defn(ModuleInfo0, TypeOfX, ConsId, ConsDefn)
    then
        % Add extra arguments to the unification for the
        % type_info and/or type_class_info variables.

        lookup_var_types(VarTypes0, ArgVars0, ActualArgTypes),
        polymorphism_process_existq_unify_functor(ConsDefn,
            IsExistConstr, ActualArgTypes, TypeOfX, GoalInfo0,
            ExtraVars, ExtraGoals, !Info),
        ArgVars = ExtraVars ++ ArgVars0,
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(ExtraVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),

        % Some of the argument unifications may be complicated unifications,
        % which may need type_infos.
        unification_typeinfos(TypeOfX, Unification0, Unification,
            GoalInfo1, GoalInfo, _Changed, !Info),

        UnifyExpr = unify(X0, rhs_functor(ConsId, IsExistConstr, ArgVars),
            Mode0, Unification, UnifyContext),
        Unify = hlds_goal(UnifyExpr, GoalInfo),
        GoalList = ExtraGoals ++ [Unify],
        conj_list_to_goal(GoalList, GoalInfo0, Goal),
        Changed = yes
    else
        % We leave construction/deconstruction unifications alone.
        % Some of the argument unifications may be complicated unifications,
        % which may need type_infos.

        % XXX Return original Goal0 if Changed = no.
        unification_typeinfos(TypeOfX, Unification0, Unification,
            GoalInfo0, GoalInfo, Changed, !Info),
        RHS = rhs_functor(ConsId0, is_not_exist_constr, ArgVars0),
        GoalExpr = unify(X0, RHS, Mode0, Unification, UnifyContext),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

%---------------------------------------------------------------------------%

    % Compute the extra arguments that we need to add to a unification with
    % an existentially quantified data constructor.
    %
:- pred polymorphism_process_existq_unify_functor(ctor_defn::in,
    is_exist_constr::in, list(mer_type)::in, mer_type::in,
    hlds_goal_info::in, list(prog_var)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_existq_unify_functor(CtorDefn, IsExistConstr,
        ActualArgTypes, ActualRetType, GoalInfo,
        ExtraVars, ExtraGoals, !Info) :-
    CtorDefn = ctor_defn(CtorTypeVarSet, CtorKindMap,
        CtorMaybeExistConstraints, CtorArgTypes, CtorRetType),

    % Rename apart the type variables in the constructor definition.
    poly_info_get_typevarset(!.Info, TypeVarSet0),
    tvarset_merge_renaming(TypeVarSet0, CtorTypeVarSet, TypeVarSet,
        CtorToParentRenaming),
    (
        CtorMaybeExistConstraints = exist_constraints(CtorExistConstraints),
        % XXX Could we use _Ctor{Unc,C}onstrainedExistQVars to avoid
        % some of the work below?
        CtorExistConstraints = cons_exist_constraints(CtorExistQVars,
            CtorExistentialConstraints,
            _CtorUnconstrainedExistQVars, _CtorConstrainedExistQVars),
        apply_variable_renaming_to_tvar_list(CtorToParentRenaming,
            CtorExistQVars, ParentExistQVars),
        apply_variable_renaming_to_prog_constraint_list(CtorToParentRenaming,
            CtorExistentialConstraints, ParentExistentialConstraints),
        list.length(ParentExistentialConstraints, NumExistentialConstraints),

        % Compute the set of _unconstrained_ existentially quantified type
        % variables, and then apply the type bindings to those type variables
        % to figure out what types they are bound to.
        constraint_list_get_tvars(ParentExistentialConstraints,
            ParentExistConstrainedTVars),
        list.delete_elems(ParentExistQVars, ParentExistConstrainedTVars,
            ParentUnconstrainedExistQVars),
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedExistQVars, ActualExistentialTypes)
    ;
        CtorMaybeExistConstraints = no_exist_constraints,
        NumExistentialConstraints = 0,
        ActualExistentialTypes = []
    ),
    apply_variable_renaming_to_tvar_kind_map(CtorToParentRenaming,
        CtorKindMap, ParentKindMap),
    apply_variable_renaming_to_type_list(CtorToParentRenaming,
        CtorArgTypes, ParentArgTypes),
    apply_variable_renaming_to_type(CtorToParentRenaming, CtorRetType,
        ParentRetType),
    poly_info_set_typevarset(TypeVarSet, !Info),

    % Compute the type bindings resulting from the functor's actual argument
    % and return types. These are the ones that might bind the ExistQVars.
    type_list_subsumes_det([ParentRetType | ParentArgTypes],
        [ActualRetType | ActualArgTypes], ParentToActualTypeSubst),

    % Create type_class_info variables for the type class constraints.
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    GoalId = goal_info_get_goal_id(GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    (
        IsExistConstr = is_exist_constr,
        % Assume it is a construction.
        lookup_hlds_constraint_list(ConstraintMap, unproven, GoalId,
            NumExistentialConstraints, ActualExistentialConstraints),
        make_typeclass_info_vars(ActualExistentialConstraints, [], Context,
            ExtraTypeClassVarsMCAs, ExtraTypeClassGoals, !Info),
        assoc_list.keys(ExtraTypeClassVarsMCAs, ExtraTypeClassVars)
    ;
        IsExistConstr = is_not_exist_constr,
        % Assume it is a deconstruction.
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumExistentialConstraints, ActualExistentialConstraints),
        make_existq_typeclass_info_vars(ActualExistentialConstraints, Context,
            ExtraTypeClassVars, ExtraTypeClassGoals, !Info)
    ),

    % Create type_info variables for the _unconstrained_ existentially
    % quantified type variables.
    polymorphism_do_make_type_info_vars(ActualExistentialTypes, Context,
        ExtraTypeInfoVarsMCAs, ExtraTypeInfoGoals, !Info),
    assoc_list.keys(ExtraTypeInfoVarsMCAs, ExtraTypeInfoVars),

    % The type_class_info variables go AFTER the type_info variables
    % (for consistency with the order for argument passing,
    % and because the RTTI support in the runtime system relies on it)

    ExtraGoals = ExtraTypeInfoGoals ++ ExtraTypeClassGoals,
    ExtraVars = ExtraTypeInfoVars ++ ExtraTypeClassVars.

%---------------------------------------------------------------------------%

:- pred polymorphism_process_foreign_proc(pred_info::in,
    hlds_goal_expr::in(bound(call_foreign_proc(ground,ground,ground,ground,
    ground,ground,ground))), hlds_goal_info::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_foreign_proc(PredInfo, GoalExpr0, GoalInfo0,
        Goal, !Info) :-
    % Insert the type_info vars into the argname map, so that the foreign_proc
    % can refer to the type_info variable for type T as `TypeInfo_for_T'.
    GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
        Args0, ProcExtraArgs, MaybeTraceRuntimeCond, Impl),
    ArgVars0 = list.map(foreign_arg_var, Args0),
    polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
        ExtraVars, ExtraGoals, !Info),
    polymorphism_process_foreign_proc_args(PredInfo, Impl,
        ExtraVars, ExtraArgs),
    Args = ExtraArgs ++ Args0,

    % Plug it all back together.
    CallExpr = call_foreign_proc(Attributes, PredId, ProcId,
        Args, ProcExtraArgs, MaybeTraceRuntimeCond, Impl),
    Call = hlds_goal(CallExpr, GoalInfo),
    GoalList = ExtraGoals ++ [Call],
    conj_list_to_goal(GoalList, GoalInfo0, Goal).

:- pred polymorphism_process_foreign_proc_args(pred_info::in,
    pragma_foreign_proc_impl::in, list(prog_var)::in, list(foreign_arg)::out)
    is det.

polymorphism_process_foreign_proc_args(PredInfo, Impl, Vars, Args) :-
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, ExistQVars,
        PredArgTypes),

    % Find out which variables are constrained (so that we don't add
    % type_infos for them).
    pred_info_get_class_context(PredInfo, constraints(UnivCs, ExistCs)),
    UnivVars0 = list.map(get_constrained_vars, UnivCs),
    list.condense(UnivVars0, UnivConstrainedVars),
    ExistVars0 = list.map(get_constrained_vars, ExistCs),
    list.condense(ExistVars0, ExistConstrainedVars),

    type_vars_in_types(PredArgTypes, PredTypeVars0),
    list.remove_dups(PredTypeVars0, PredTypeVars1),
    list.delete_elems(PredTypeVars1, UnivConstrainedVars, PredTypeVars2),
    list.delete_elems(PredTypeVars2, ExistConstrainedVars, PredTypeVars),

    % The argument order is described at the top of this file.

    in_mode(In),
    out_mode(Out),

    list.map(foreign_proc_add_typeclass_info(Out, Impl, PredTypeVarSet),
        ExistCs, ExistTypeClassArgInfos),
    list.map(foreign_proc_add_typeclass_info(In, Impl, PredTypeVarSet),
        UnivCs, UnivTypeClassArgInfos),
    TypeClassArgInfos = UnivTypeClassArgInfos ++ ExistTypeClassArgInfos,

    list.filter(list.contains(ExistQVars), PredTypeVars,
        ExistUnconstrainedVars, UnivUnconstrainedVars),

    list.map_foldl(foreign_proc_add_typeinfo("Out", Out, Impl, PredTypeVarSet),
        ExistUnconstrainedVars, ExistTypeArgInfos, 1, _),
    list.map_foldl(foreign_proc_add_typeinfo("In", In, Impl, PredTypeVarSet),
        UnivUnconstrainedVars, UnivTypeArgInfos, 1, _),
    TypeInfoArgInfos = UnivTypeArgInfos ++ ExistTypeArgInfos,

    ArgInfos = TypeInfoArgInfos ++ TypeClassArgInfos,

    % Insert type_info/typeclass_info types for all the inserted
    % type_info/typeclass_info vars into the argument type list.

    TypeInfoTypes = list.map((func(_) = type_info_type), PredTypeVars),
    TypeClassInfoType = typeclass_info_type,
    list.length(UnivCs, NumUnivCs),
    list.length(ExistCs, NumExistCs),
    list.duplicate(NumUnivCs + NumExistCs, TypeClassInfoType,
        TypeClassInfoTypes),
    OrigArgTypes = TypeInfoTypes ++ TypeClassInfoTypes,

    make_foreign_args(Vars, ArgInfos, OrigArgTypes, Args).

:- func get_constrained_vars(prog_constraint) = list(tvar).

get_constrained_vars(Constraint) = CVars :-
    Constraint = constraint(_, CTypes),
    type_vars_in_types(CTypes, CVars).

:- pred foreign_proc_add_typeclass_info(mer_mode::in,
    pragma_foreign_proc_impl::in, tvarset::in, prog_constraint::in,
    foreign_arg_name_mode_box::out) is det.

foreign_proc_add_typeclass_info(Mode, Impl, TypeVarSet, Constraint,
        MaybeArgNameBox) :-
    Constraint = constraint(SymName, Types),
    Name = sym_name_to_string_sep(SymName, "__"),
    type_vars_in_types(Types, TypeVars),
    TypeVarNames = list.map(underscore_and_tvar_name(TypeVarSet), TypeVars),
    string.append_list(["TypeClassInfo_for_", Name | TypeVarNames],
        ConstraintVarName),
    % If the variable name corresponding to the typeclass_info isn't mentioned
    % in the C code fragment, don't pass the variable to the C code at all.
    ( if foreign_proc_uses_variable(Impl, ConstraintVarName) then
        MaybeArgName = yes(foreign_arg_name_mode(ConstraintVarName, Mode))
    else
        MaybeArgName = no
    ),
    MaybeArgNameBox =
        foreign_arg_name_mode_box(MaybeArgName, bp_native_if_possible).

:- pred foreign_proc_add_typeinfo(string::in, mer_mode::in,
    pragma_foreign_proc_impl::in, tvarset::in,
    tvar::in, foreign_arg_name_mode_box::out, int::in, int::out) is det.

foreign_proc_add_typeinfo(InOut, Mode, Impl, TypeVarSet, TVar, MaybeArgNameBox,
        !N) :-
    ( if varset.search_name(TypeVarSet, TVar, TypeVarName) then
        OldCVarName = "TypeInfo_for_" ++ TypeVarName,
        NewCVarName = "TypeInfo_" ++ InOut ++ "_" ++ string.int_to_string(!.N),
        % If the variable name corresponding to the type_info isn't mentioned
        % in the C code fragment, don't pass the variable to the C code at all.
        ( if
            ( foreign_proc_uses_variable(Impl, OldCVarName)
            ; foreign_proc_uses_variable(Impl, NewCVarName)
            )
        then
            MaybeArgName = yes(foreign_arg_name_mode(OldCVarName, Mode))
        else
            MaybeArgName = no
        )
    else
        MaybeArgName = no
    ),
    MaybeArgNameBox =
        foreign_arg_name_mode_box(MaybeArgName, bp_native_if_possible),
    !:N = !.N + 1.

:- func underscore_and_tvar_name(tvarset, tvar) = string.

underscore_and_tvar_name(TypeVarSet, TVar) = TVarName :-
    varset.lookup_name(TypeVarSet, TVar, TVarName0),
    TVarName = "_" ++ TVarName0.

:- pred polymorphism_process_plain_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_process_plain_conj([], [], !Info).
polymorphism_process_plain_conj([Goal0 | Goals0], [Goal | Goals], !Info) :-
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_plain_conj(Goals0, Goals, !Info).

:- pred polymorphism_process_par_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, cache_maps::in, poly_info::in, poly_info::out)
    is det.

polymorphism_process_par_conj([], [], _, !Info).
polymorphism_process_par_conj([Goal0 | Goals0], [Goal | Goals],
        InitialSnapshot, !Info) :-
    % Any variable that a later parallel conjunct reuses from an earlier
    % parallel conjunct (a) will definitely require synchronization, whose
    % cost will be greater than the cost of building a typeinfo from scratch,
    % and (b) may drastically reduce the available parallelism, if the earlier
    % conjunct produces the variable late but the later conjunct requires it
    % early.
    set_cache_maps_snapshot("par conjunct", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_par_conj(Goals0, Goals, InitialSnapshot, !Info).

:- pred polymorphism_process_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    cache_maps::in, poly_info::in, poly_info::out) is det.

polymorphism_process_disj([], [], _, !Info).
polymorphism_process_disj([Goal0 | Goals0], [Goal | Goals], InitialSnapshot,
        !Info) :-
    set_cache_maps_snapshot("disjunct", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_disj(Goals0, Goals, InitialSnapshot, !Info).

:- pred polymorphism_process_cases(list(case)::in, list(case)::out,
    cache_maps::in, poly_info::in, poly_info::out) is det.

polymorphism_process_cases([], [], _, !Info).
polymorphism_process_cases([Case0 | Cases0], [Case | Cases], InitialSnapshot,
        !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_cache_maps_snapshot("case", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    polymorphism_process_cases(Cases0, Cases, InitialSnapshot, !Info).

%---------------------------------------------------------------------------%

    % XXX document me
    %
    % XXX the following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred polymorphism_process_call(pred_id::in, list(prog_var)::in,
    hlds_goal_info::in, hlds_goal_info::out,
    list(prog_var)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
        ExtraVars, ExtraGoals, !Info) :-
    poly_info_get_var_types(!.Info, VarTypes),
    poly_info_get_typevarset(!.Info, TypeVarSet0),
    poly_info_get_module_info(!.Info, ModuleInfo),

    % The order of the added variables is important, and must match the
    % order specified at the top of this file.

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_tvar_kind_map(PredInfo, PredKindMap),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % VarTypes, TypeVarSet* etc come from the caller.
    % PredTypeVarSet, PredArgTypes, PredExistQVars, etc come
    % directly from the callee.
    % ParentArgTypes, ParentExistQVars etc come from a version
    % of the callee that has been renamed apart from the caller.
    %
    % The difference between e.g. PredArgTypes and ParentArgTypes is the
    % application of PredToParentTypeRenaming, which maps the type variables
    % in the callee to new type variables in the caller. Adding the new type
    % variables to TypeVarSet0 yields TypeVarSet.

    ( if varset.is_empty(PredTypeVarSet) then
        % Optimize a common case.
        map.init(PredToParentTypeRenaming),
        TypeVarSet = TypeVarSet0,
        ParentArgTypes = PredArgTypes,
        ParentKindMap = PredKindMap,
        ParentTVars = [],
        ParentExistQVars = []
    else
        % This merge might be a performance bottleneck?
        tvarset_merge_renaming(TypeVarSet0, PredTypeVarSet, TypeVarSet,
            PredToParentTypeRenaming),
        apply_variable_renaming_to_type_list(PredToParentTypeRenaming,
            PredArgTypes, ParentArgTypes),
        type_vars_in_types(ParentArgTypes, ParentTVars),
        apply_variable_renaming_to_tvar_kind_map(PredToParentTypeRenaming,
            PredKindMap, ParentKindMap),
        apply_variable_renaming_to_tvar_list(PredToParentTypeRenaming,
            PredExistQVars, ParentExistQVars)
    ),

    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( if
        (
            % Optimize for the common case of nonpolymorphic call
            % with no constraints.
            ParentTVars = [],
            PredClassContext = constraints([], [])
        ;
            % Some builtins don't need or want the type_info.
            no_type_info_builtin(PredModule, PredName, PredArity)
        )
    then
        GoalInfo = GoalInfo0,
        ExtraGoals = [],
        ExtraVars = []
    else
        poly_info_set_typevarset(TypeVarSet, !Info),

        % Compute which "parent" type variables are constrained
        % by the type class constraints.
        apply_variable_renaming_to_prog_constraints(PredToParentTypeRenaming,
            PredClassContext, ParentClassContext),
        ParentClassContext = constraints(ParentUnivConstraints,
            ParentExistConstraints),
        constraint_list_get_tvars(ParentUnivConstraints,
            ParentUnivConstrainedTVars),
        constraint_list_get_tvars(ParentExistConstraints,
            ParentExistConstrainedTVars),

        % Calculate the set of unconstrained type vars. Split these into
        % existential and universal type vars.
        list.remove_dups(ParentTVars, ParentUnconstrainedTVars0),
        list.delete_elems(ParentUnconstrainedTVars0,
            ParentUnivConstrainedTVars, ParentUnconstrainedTVars1),
        list.delete_elems(ParentUnconstrainedTVars1,
            ParentExistConstrainedTVars, ParentUnconstrainedTVars),
        list.delete_elems(ParentUnconstrainedTVars, ParentExistQVars,
            ParentUnconstrainedUnivTVars),
        list.delete_elems(ParentUnconstrainedTVars,
            ParentUnconstrainedUnivTVars, ParentUnconstrainedExistTVars),

        % Calculate the "parent to actual" binding.
        lookup_var_types(VarTypes, ArgVars0, ActualArgTypes),
        type_list_subsumes_det(ParentArgTypes, ActualArgTypes,
            ParentToActualTypeSubst),

        % Make the universally quantified typeclass_infos for the call.
        poly_info_get_constraint_map(!.Info, ConstraintMap),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        list.length(ParentUnivConstraints, NumUnivConstraints),
        lookup_hlds_constraint_list(ConstraintMap, unproven, GoalId,
            NumUnivConstraints, ActualUnivConstraints),
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentExistQVars, ActualExistQVarTypes),
        ( if
            prog_type.type_list_to_var_list(ActualExistQVarTypes,
                ActualExistQVars0)
        then
            ActualExistQVars = ActualExistQVars0
        else
            unexpected($pred, "existq_tvar bound")
        ),
        Context = goal_info_get_context(GoalInfo0),
        make_typeclass_info_vars(ActualUnivConstraints, ActualExistQVars,
            Context, ExtraUnivClassVarsMCAs, ExtraUnivClassGoals, !Info),
        assoc_list.keys(ExtraUnivClassVarsMCAs, ExtraUnivClassVars),

        % Make variables to hold any existentially quantified typeclass_infos
        % in the call, insert them into the typeclass_info map.
        list.length(ParentExistConstraints, NumExistConstraints),
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumExistConstraints, ActualExistConstraints),
        make_existq_typeclass_info_vars(ActualExistConstraints, Context,
            ExtraExistClassVars, ExtraExistClassGoals, !Info),

        % Make variables to hold typeinfos for unconstrained universal type
        % vars.
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedUnivTVars, ActualUnconstrainedUnivTypes),
        polymorphism_do_make_type_info_vars(ActualUnconstrainedUnivTypes,
            Context, ExtraUnivTypeInfoVarsMCAs,
            ExtraUnivTypeInfoGoals, !Info),
        assoc_list.keys(ExtraUnivTypeInfoVarsMCAs,
            ExtraUnivTypeInfoVars),

        % Make variables to hold typeinfos for unconstrained existential type
        % vars.
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedExistTVars, ActualUnconstrainedExistTypes),
        polymorphism_do_make_type_info_vars(ActualUnconstrainedExistTypes,
            Context, ExtraExistTypeInfoVarsMCAs,
            ExtraExistTypeInfoGoals, !Info),
        assoc_list.keys(ExtraExistTypeInfoVarsMCAs,
            ExtraExistTypeInfoVars),

        % Add up the extra vars and goals.
        ExtraGoals = ExtraUnivClassGoals ++ ExtraExistClassGoals
            ++ ExtraUnivTypeInfoGoals ++ ExtraExistTypeInfoGoals,
        ExtraVars = ExtraUnivTypeInfoVars ++ ExtraExistTypeInfoVars
            ++ ExtraUnivClassVars ++ ExtraExistClassVars,

        % Update the nonlocals.
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(ExtraVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo)
    ).

%---------------------------------------------------------------------------%

    % Add the type_info variables for a new call goal. This predicate assumes
    % that process_module has already been run so the called pred has already
    % been processed.
    %
    % XXX This predicate does not yet handle calls whose arguments include
    % existentially quantified types or type class constraints.
    %
:- pred polymorphism_process_new_call(pred_info::in, proc_info::in,
    pred_id::in, proc_id::in, list(prog_var)::in, builtin_state::in,
    maybe(call_unify_context)::in, sym_name::in, hlds_goal_info::in,
    hlds_goal::out, poly_info::in, poly_info::out) is det.
:- pragma consider_used(pred(polymorphism_process_new_call/12)).

polymorphism_process_new_call(CalleePredInfo, CalleeProcInfo, PredId, ProcId,
        CallArgs0, BuiltinState, MaybeCallUnifyContext, SymName,
        GoalInfo0, Goal, !Info) :-
    % document me better
    %
    poly_info_get_typevarset(!.Info, TVarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    lookup_var_types(VarTypes0, CallArgs0, ActualArgTypes0),
    pred_info_get_arg_types(CalleePredInfo, PredTVarSet, _PredExistQVars,
        PredArgTypes),
    proc_info_get_headvars(CalleeProcInfo, CalleeHeadVars),
    proc_info_get_rtti_varmaps(CalleeProcInfo, CalleeRttiVarMaps),

    % Work out how many type_info args we need to prepend.
    NCallArgs0 = list.length(ActualArgTypes0),
    NPredArgs  = list.length(PredArgTypes),
    NExtraArgs = NPredArgs - NCallArgs0,
    ( if
        list.drop(NExtraArgs, PredArgTypes, OrigPredArgTypes0),
        list.take(NExtraArgs, CalleeHeadVars, CalleeExtraHeadVars0)
    then
        OrigPredArgTypes = OrigPredArgTypes0,
        CalleeExtraHeadVars = CalleeExtraHeadVars0
    else
        unexpected($pred, "extra args not found")
    ),

    % Work out the bindings of type variables in the call.
    tvarset_merge_renaming(TVarSet0, PredTVarSet, TVarSet,
        PredToParentRenaming),
    apply_variable_renaming_to_type_list(PredToParentRenaming,
        OrigPredArgTypes, OrigParentArgTypes),
    type_list_subsumes_det(OrigParentArgTypes, ActualArgTypes0,
        ParentToActualTSubst),
    poly_info_set_typevarset(TVarSet, !Info),

    % Look up the type variables that the type_infos in the caller are for,
    % and apply the type bindings to calculate the types that the caller
    % should pass type_infos for.
    GetTypeInfoTypes =
        ( pred(ProgVar::in, TypeInfoType::out) is det :-
            rtti_varmaps_var_info(CalleeRttiVarMaps, ProgVar, VarInfo),
            (
                VarInfo = type_info_var(TypeInfoType)
            ;
                VarInfo = typeclass_info_var(_),
                unexpected($pred,
                    "unsupported: constraints on initialisation preds")
            ;
                VarInfo = non_rtti_var,
                unexpected($pred,
                    "missing rtti_var_info for initialisation pred")
            )
        ),
    list.map(GetTypeInfoTypes, CalleeExtraHeadVars, PredTypeInfoTypes),
    apply_variable_renaming_to_type_list(PredToParentRenaming,
        PredTypeInfoTypes, ParentTypeInfoTypes),
    apply_rec_subst_to_type_list(ParentToActualTSubst, ParentTypeInfoTypes,
        ActualTypeInfoTypes),

    % Construct goals to make the required type_infos.
    Ctxt = term.context_init,
    polymorphism_do_make_type_info_vars(ActualTypeInfoTypes, Ctxt,
        ExtraArgsConstArgs, ExtraGoals, !Info),
    assoc_list.keys(ExtraArgsConstArgs, ExtraArgs),
    CallArgs = ExtraArgs ++ CallArgs0,
    NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
    NonLocals1 = set_of_var.list_to_set(ExtraArgs),
    set_of_var.union(NonLocals0, NonLocals1, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    CallGoalExpr = plain_call(PredId, ProcId, CallArgs, BuiltinState,
        MaybeCallUnifyContext, SymName),
    CallGoal = hlds_goal(CallGoalExpr, GoalInfo),
    conj_list_to_goal(ExtraGoals ++ [CallGoal], GoalInfo, Goal).

%---------------------------------------------------------------------------%

    % If the pred we are processing is a polymorphic predicate, or contains
    % polymorphically-typed goals, we may need to fix up the quantification
    % (nonlocal variables) of the goal so that it includes the extra type_info
    % variables and typeclass_info variables that we added to the headvars
    % or the arguments of existentially typed predicate calls, function calls
    % and deconstruction unifications.
    %
    % Type(class)-infos added for ground types passed to predicate calls,
    % function calls and existentially typed construction unifications
    % do not require requantification because they are local to the conjunction
    % containing the type(class)-info construction and the goal which uses the
    % type(class)-info. The nonlocals for those goals are adjusted by
    % the code which creates/alters them. However, reusing a type_info changes
    % it from being local to nonlocal.
    %
:- pred fixup_quantification(proc_arg_vector(prog_var)::in,
    existq_tvars::in, hlds_goal::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

fixup_quantification(HeadVars, ExistQVars, Goal0, Goal, !Info) :-
    ( if
        % Optimize a common case.
        ExistQVars = [],
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_varmaps_no_tvars(RttiVarMaps0),
        poly_info_get_num_reuses(!.Info, NumReuses),
        NumReuses = 0,
        poly_info_get_must_requantify(!.Info, MustRequantify),
        MustRequantify = no_must_requantify
    then
        Goal = Goal0
    else
        poly_info_get_varset(!.Info, VarSet0),
        poly_info_get_var_types(!.Info, VarTypes0),
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        OutsideVars = proc_arg_vector_to_set(HeadVars),
        implicitly_quantify_goal_general(ordinary_nonlocals_maybe_lambda,
            set_to_bitset(OutsideVars), _Warnings, Goal0, Goal,
            VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
        poly_info_set_varset_types_rtti(VarSet, VarTypes, RttiVarMaps, !Info)
    ).

    % If the lambda goal we are processing is polymorphically typed, we may
    % need to fix up the quantification (nonlocal variables) so that it
    % includes the type_info variables and typeclass_info variables for
    % any polymorphically typed variables in the nonlocals set or in the
    % arguments (either the lambda vars or the implicit curried argument
    % variables). We return this set of LambdaTiTciVars. Including typeinfos
    % for arguments which are not in the nonlocals set of the lambda goal,
    % i.e. unused arguments, is necessary only if typeinfo_liveness is set,
    % but we do it always, since we don't have the options available here,
    % and the since cost is pretty minimal.
    %
    % We also need to include in the nonlocals set of the lambda expression
    % any type_info and/or typeclass_info variables we have added to the
    % goal inside the lambda. In rare cases such as tests/valid/gh98.m,
    % a typeclass_info that we inserted into the goal inside the lambda
    % is defined outside the lambda *without* being in LambdaTiTciVars.
    % We therefore return all type_info/typeclass_info variables that occur
    % in the transformed lambda goal in AllTiTciGoalVars, for our caller
    % to likewise include in the nonlocals set of the lambda goal.
    %
:- pred fixup_lambda_quantification(list(prog_var)::in,
    list(prog_var)::in, existq_tvars::in, hlds_goal::in, hlds_goal::out,
    set_of_progvar::out, set_of_progvar::out,
    poly_info::in, poly_info::out) is det.

fixup_lambda_quantification(LambdaNonLocals0, ArgVars, ExistQVars, !Goal,
        LambdaTiTciVars, AllTiTciGoalVars, !Info) :-
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    ( if rtti_varmaps_no_tvars(RttiVarMaps0) then
        set_of_var.init(LambdaTiTciVars),
        set_of_var.init(AllTiTciGoalVars)
    else
        poly_info_get_varset(!.Info, VarSet0),
        poly_info_get_var_types(!.Info, VarTypes0),
        !.Goal = hlds_goal(_, GoalInfo0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(LambdaNonLocals0, NonLocals, BothNonLocals),
        set_of_var.insert_list(ArgVars, BothNonLocals, NonLocalsWithArgVars),
        goal_util.extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps0,
            VarTypes0, ExistQVars, NonLocalsWithArgVars,
            LambdaTiTciVars),

        goal_vars(!.Goal, GoalVars),
        IsTiOrTci =
            ( pred(Var::in) is semidet :-
                lookup_var_type(VarTypes0, Var, VarType),
                ( VarType = type_info_type
                ; VarType = typeclass_info_type
                )
            ),
        set_of_var.filter(IsTiOrTci, GoalVars, AllTiTciGoalVars),
        % Our caller will include AllTiTciGoalVars in the nonlocals set
        % of the rhs_lambda_goal, since some of them may actually come
        % from code outside the lambda. However, since some (or even all)
        % of the variables in this set may actually be local, we insist
        % on the *whole* procedure body, not just the code of this lambda,
        % being requantified once its polymorphism transformation has been
        % completed. (This works both for variables in AllTiTciGoalVars
        % that were added by polymorphism, and those which were there before.)
        %
        % Note that all the variables added by the polymorphism transformation
        % of !Goal that may be nonlocal are type_infos and typeclass_infos.
        % The polymorphism transformation can also add integer variables
        % for use in e.g. getting a type_info out of a particular slot
        % of a typeclass_info, but we set the set of cached int vars
        % in the poly_info to empty at the start of transformation of !Goal,
        % and throw away the updated caches at its end, so any integer vars
        % created by the transformation inside !:Goal will be local to !:Goal.
        poly_info_set_must_requantify(!Info),
        set_of_var.union(NonLocalsWithArgVars, AllTiTciGoalVars,
            PossibleOutsideVars),
        implicitly_quantify_goal_general(ordinary_nonlocals_maybe_lambda,
            PossibleOutsideVars, _Warnings, !Goal,
            VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
        poly_info_set_varset_types_rtti(VarSet, VarTypes, RttiVarMaps, !Info)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism.
%---------------------------------------------------------------------------%
