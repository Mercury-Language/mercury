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
% This module and its subcontractors implement a pass over the HLDS.
% This pass does a syntactic transformation to implement both parametric
% and ad-hoc (typeclass-based) polymorphism, by passing extra `type_info'
% and `typeclass_info' arguments between predicates and functions.
% These arguments are structures that contain, amongst other things,
% higher order predicate terms for the polymorphic procedures or methods.
%
% See notes/type_class_transformation.html for a description of the
% transformation and data structures used to implement type classes.
%
% XXX The way the code in this pass handles existential type classes
% and type class constraints is a bit ad hoc, in general; there are
% definitely parts of this code (marked with XXXs) that could do with
% a rewrite to make it more consistent, and hence more maintainable.
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
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Run the polymorphism pass over the whole HLDS.
    %
:- pred polymorphism_process_module(io.text_output_stream::in,
    module_info::in, module_info::out, list(pred_id)::out,
    maybe_safe_to_continue::out, list(error_spec)::out) is det.

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

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.introduce_exists_casts.
:- import_module check_hlds.polymorphism_clause.
:- import_module check_hlds.polymorphism_info.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.passes_aux.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module term_context.
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

polymorphism_process_module(ProgressStream, !ModuleInfo, ExistsCastPredIds,
        SafeToContinue, Specs) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.keys(PredIdTable0, PredIds0),
    list.foldl3(maybe_polymorphism_process_pred(ProgressStream), PredIds0,
        safe_to_continue, SafeToContinue, [], Specs, !ModuleInfo),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable1),
    map.keys(PredIdTable1, PredIds1),
    list.foldl2(polymorphism_update_arg_types(yes(ProgressStream)), PredIds1,
        [], ExistsCastPredIds, !ModuleInfo).

:- pred maybe_polymorphism_process_pred(io.text_output_stream::in, pred_id::in,
    maybe_safe_to_continue::in, maybe_safe_to_continue::out,
    list(error_spec)::in, list(error_spec)::out,
    module_info::in, module_info::out) is det.

maybe_polymorphism_process_pred(ProgressStream, PredId, !SafeToContinue,
        !Specs, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ( if
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        pred_info_get_orig_arity(PredInfo, pred_form_arity(PredFormArityInt)),
        no_type_info_builtin(PredModule, PredName, PredFormArityInt)
    then
        true
    else
        polymorphism_process_pred_msg(ProgressStream, PredId, !SafeToContinue,
            !Specs, !ModuleInfo)
    ).

%---------------------------------------------------------------------------%

:- pred polymorphism_process_pred_msg(io.text_output_stream::in, pred_id::in,
    maybe_safe_to_continue::in, maybe_safe_to_continue::out,
    list(error_spec)::in, list(error_spec)::out,
    module_info::in, module_info::out) is det.

polymorphism_process_pred_msg(ProgressStream, PredId,
        !SafeToContinue, !Specs, !ModuleInfo) :-
    % Since polymorphism transforms not just the procedures defined
    % in the module being compiled, but also all the procedures in
    % all the imported modules, this message can be printed A LOT,
    % even though it is almost never of interest.
    % That is why we enable it only when requested.
    trace [compiletime(flag("poly_msgs")), io(!IO)] (
        maybe_write_pred_progress_message(ProgressStream, !.ModuleInfo,
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
    polymorphism_update_arg_types(maybe.no, PredId, [], ExistsPredIds,
        !ModuleInfo),
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

    % Run the polymorphism pass over the clauses_info, updating the headvars,
    % goals, varsets, types, etc., and computing some information in the
    % poly_info.
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    init_poly_info(!.ModuleInfo, PredInfo0, ClausesInfo0, PolyInfo0),
    polymorphism_process_clause_info(PredInfo0, ExtraArgModes,
        ClausesInfo0, ClausesInfo, PolyInfo0, PolyInfo),
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
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        poly_info_set_selected_pred(is_not_selected_pred, !IO)
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred polymorphism_update_arg_types(maybe(io.text_output_stream)::in,
    pred_id::in, list(pred_id)::in, list(pred_id)::out,
    module_info::in, module_info::out) is det.

polymorphism_update_arg_types(MaybeProgressStream, PredId,
        !ExistsCastPredIds, !ModuleInfo) :-
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
        (
            MaybeProgressStream = no
        ;
            MaybeProgressStream = yes(ProgressStream),
            maybe_write_pred_progress_message(ProgressStream, !.ModuleInfo,
                "Update polymorphism arg types for", PredId, !IO)
        )
    ),

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_var_table(ClausesInfo0, VarTable0),
    clauses_info_get_headvars(ClausesInfo0, HeadVars),
    proc_arg_vector_partition_poly_args(HeadVars, ExtraHeadVarList,
        OldHeadVarList),
    % We need ExistQVars whether or not ExtraHeadVarList is empty or not.
    pred_info_get_arg_types(PredInfo0, TypeVarSet, ExistQVars, ArgTypes0),
    list.length(ExtraHeadVarList, NumExtraHeadVars),
    (
        ExtraHeadVarList = [],
        PredInfo2 = PredInfo0
    ;
        ExtraHeadVarList = [_ | _],
        lookup_var_types(VarTable0, ExtraHeadVarList, ExtraArgTypes),
        ArgTypes = ExtraArgTypes ++ ArgTypes0,
        pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes,
            PredInfo0, PredInfo1),
        pred_info_get_format_call(PredInfo1, MaybeFormatCall1),
        (
            MaybeFormatCall1 = no,
            PredInfo2 = PredInfo1
        ;
            MaybeFormatCall1 = yes(format_call(Context, OoMFormatStrsValues1)),
            % Update the argument numbers in the format_call field 
            % to account for the new arguments we just added at the front
            % of the argument list.
            one_or_more.map(increment_arg_nums(NumExtraHeadVars),
                OoMFormatStrsValues1, OoMFormatStrsValues2),
            MaybeFormatCall2 = yes(format_call(Context, OoMFormatStrsValues2)),
            pred_info_set_format_call(MaybeFormatCall2, PredInfo1, PredInfo2)
        )
    ),

    % If the clauses bind some existentially quantified type variables,
    % introduce exists_casts goals for affected head variables, including
    % the new type_info and typeclass_info arguments. Make sure the types
    % of the internal versions of type_infos for those type variables
    % in the variable types map are as specific as possible.

    ( if
        ExistQVars = [_ | _],
        % This can fail for unification procedures of equivalence types.
        lookup_var_types(VarTable0, OldHeadVarList, OldHeadVarTypes),
        type_list_subsumes(ArgTypes0, OldHeadVarTypes, Subn),
        not map.is_empty(Subn)
    then
        pred_info_set_existq_tvar_binding(Subn, PredInfo2, PredInfo3),
        !:ExistsCastPredIds = [PredId | !.ExistsCastPredIds]
    else
        PredInfo3 = PredInfo2
    ),

    pred_info_set_polymorphism_added_args(NumExtraHeadVars,
        PredInfo3, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred increment_arg_nums(int::in,
    format_string_values::in, format_string_values::out) is det.

increment_arg_nums(Inc, FSV0, FSV) :-
    FSV0 = format_string_values(OrigFormatStrArgNum, OrigValuesListArgNum,
        CurFormatStrArgNum, CurValuesListArgNum),
    FSV = format_string_values(OrigFormatStrArgNum, OrigValuesListArgNum,
        CurFormatStrArgNum + Inc, CurValuesListArgNum + Inc).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism.
%---------------------------------------------------------------------------%
