%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_solver.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type solver_aux_pred_info
    --->    solver_aux_pred_info(
                sym_name,
                list(type_param),
                tvarset,
                solver_type_details,
                prog_context,
                item_mercury_status,
                need_qualifier
            ).

    % A solver type t defined with
    %
    % :- solver type st
    %   where representation is rt,     % type
    %         ground         is gi,     % inst
    %         any            is ai, ... % inst
    %
    % requires the following auxiliary predicates:
    %
    % :- impure func 'representation of ground st'(st::in) =
    %           (rt::out(gi)) is det.
    % :- impure func 'representation of any st'(st::in(any)) =
    %           (rt::out(ai)) is det.
    %
    % :- impure func 'representation to ground st'(rt::in(gi)) =
    %           (st::out) is det.
    % :- impure func 'representation to any st'(rt::in(ai)) =
    %           (st::out(any)) is det.
    %
    % Declare these auxiliary predicates. We need the declarations available
    % whether the solver type is defined in this module or not.
    %
:- pred add_solver_type_aux_pred_decls(solver_aux_pred_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Define the auxiliary predicates above IF the solver type is defined
    % in this module; we don't want them to be doubly defined if the solver
    % type is defined in another module.
    %
:- pred add_solver_type_aux_pred_defns_if_local(solver_aux_pred_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.add_pred.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module map.
:- import_module maybe.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_solver_type_aux_pred_decls(SolverAuxPredInfo, !ModuleInfo, !Specs) :-
    SolverAuxPredInfo = solver_aux_pred_info(TypeSymName, TypeParams, TVarSet,
        SolverTypeDetails, Context, ItemMercuryStatus, NeedQual),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),

    % XXX kind inference:
    % We set the kinds to `star'. This will be different when we have
    % a kind system.
    prog_type.var_list_to_type_list(map.init, TypeParams, Args),
    SolverType = defined_type(TypeSymName, Args, kind_star),
    list.length(TypeParams, Arity),

    RepnType = SolverTypeDetails ^ std_representation_type,
    AnyInst = SolverTypeDetails ^ std_any_inst,
    GndInst = SolverTypeDetails ^ std_ground_inst,

    InAnyMode = in_mode(AnyInst),
    InGndMode = in_mode(GndInst),
    OutAnyMode = out_mode(AnyInst),
    OutGndMode = out_mode(GndInst),

    InstVarSet = varset.init,
    ExistQTVars = [],
    NoConstraints = constraints([], []),

    % The `:- impure
    %   func 'representation of ground st'(st::in(gi)) =
    %           (rt::out) is det' declaration.
    %
    ToGndRepnSymName = solver_to_ground_repn_symname(TypeSymName, Arity),
    ToGndRepnArgTypesModes =
        [type_and_mode(SolverType, in_mode),
         type_and_mode(RepnType,   OutGndMode)],
    ToGndAttrs = item_compiler_attributes(
        compiler_origin_solver_type(TypeSymName, Arity,
            solver_type_to_ground_pred)),
    ToGndMaybeAttrs = item_origin_compiler(ToGndAttrs),
    ToGndPredDecl = item_pred_decl_info(ToGndRepnSymName, pf_function,
        ToGndRepnArgTypesModes, maybe.no, maybe.no, yes(detism_det),
        ToGndMaybeAttrs, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, -1),

    % The `:- impure
    %   func 'representation of any st'(st::in(ai)) =
    %           (rt::out(any)) is det' declaration.
    %
    ToAnyRepnSymName = solver_to_any_repn_symname(TypeSymName, Arity),
    ToAnyRepnArgTypesModes =
        [type_and_mode(SolverType, in_any_mode),
         type_and_mode(RepnType,   OutAnyMode)],
    ToAnyAttrs = item_compiler_attributes(
        compiler_origin_solver_type(TypeSymName, Arity,
            solver_type_to_any_pred)),
    ToAnyMaybeAttrs = item_origin_compiler(ToAnyAttrs),
    ToAnyPredDecl = item_pred_decl_info(ToAnyRepnSymName, pf_function,
        ToAnyRepnArgTypesModes, maybe.no, maybe.no, yes(detism_det),
        ToAnyMaybeAttrs, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, -1),

    % The `:- impure
    %   func 'representation to ground st'(rt::in(gi)) =
    %           (st::out) is det' declaration.
    %
    FromGndRepnSymName = repn_to_ground_solver_symname(TypeSymName, Arity),
    FromGndRepnArgTypesModes =
        [type_and_mode(RepnType,   InGndMode),
         type_and_mode(SolverType, out_mode)],
    FromGndAttrs = item_compiler_attributes(
        compiler_origin_solver_type(TypeSymName, Arity,
            solver_type_from_ground_pred)),
    FromGndMaybeAttrs = item_origin_compiler(FromGndAttrs),
    FromGndPredDecl = item_pred_decl_info(FromGndRepnSymName, pf_function,
        FromGndRepnArgTypesModes, maybe.no, maybe.no, yes(detism_det),
        FromGndMaybeAttrs, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, -1),

    % The `:- impure
    %   func 'representation to any st'(rt::in(ai)) =
    %           (st::out(any)) is det' declaration.
    %
    FromAnyRepnSymName = repn_to_any_solver_symname(TypeSymName, Arity),
    FromAnyRepnArgTypesModes =
        [type_and_mode(RepnType,   InAnyMode),
         type_and_mode(SolverType, out_any_mode)],
    FromAnyAttrs = item_compiler_attributes(
        compiler_origin_solver_type(TypeSymName, Arity,
            solver_type_from_any_pred)),
    FromAnyMaybeAttrs = item_origin_compiler(FromAnyAttrs),
    FromAnyPredDecl = item_pred_decl_info(FromAnyRepnSymName, pf_function,
        FromAnyRepnArgTypesModes, maybe.no, maybe.no, yes(detism_det),
        FromAnyMaybeAttrs, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, -1),

    module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual,
        ToGndPredDecl, _MaybeToGndPredProcId, !ModuleInfo, !Specs),
    module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual,
        ToAnyPredDecl, _MaybeToAnyPredProcId, !ModuleInfo, !Specs),
    module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual,
        FromGndPredDecl, _MaybeFromGndPredProcId, !ModuleInfo, !Specs),
    module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual,
        FromAnyPredDecl, _MaybeFromAnyPredProcId, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

add_solver_type_aux_pred_defns_if_local(SolverAuxPredInfo,
        !ModuleInfo, !QualInfo, !Specs) :-
    SolverAuxPredInfo = solver_aux_pred_info(_TypeSymName, _TypeParams,
        _TVarSet, _SolverTypeDetails, _Context, ItemMercuryStatus, _NeedQual),
    (
        ItemMercuryStatus = item_defined_in_this_module(_),
        add_solver_type_aux_pred_defns(SolverAuxPredInfo,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        ItemMercuryStatus = item_defined_in_other_module(_)
    ).

:- pred add_solver_type_aux_pred_defns(solver_aux_pred_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_aux_pred_defns(SolverAuxPredInfo,
        !ModuleInfo, !QualInfo, !Specs) :-
    SolverAuxPredInfo = solver_aux_pred_info(TypeSymName, TypeParams,
        _TVarSet, SolverTypeDetails, Context, ItemMercuryStatus, _NeedQual),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),

    list.length(TypeParams, Arity),

    AnyInst = SolverTypeDetails ^ std_any_inst,
    GroundInst = SolverTypeDetails ^ std_ground_inst,

    InAnyMode = in_mode(AnyInst),
    InGroundMode = in_mode(GroundInst),
    OutAnyMode = out_mode(AnyInst),
    OutGroundMode = out_mode(GroundInst),

    ProgVarSet0 = varset.init,
    varset.new_var(X, ProgVarSet0, ProgVarSet1),
    varset.new_var(Y, ProgVarSet1, ProgVarSet),

    InstVarSet = varset.init,

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        Lang = lang_c
    ;
        Target = target_csharp,
        Lang = lang_csharp
    ;
        Target = target_java,
        Lang = lang_java
    ),

    Attrs0 = default_attributes(Lang),
    some [!Attrs] (
        !:Attrs = Attrs0,
        set_may_call_mercury(proc_will_not_call_mercury, !Attrs),
        set_thread_safe(proc_thread_safe, !Attrs),
        set_terminates(proc_terminates, !Attrs),
        set_may_modify_trail(proc_will_not_modify_trail, !Attrs),
        Attrs = !.Attrs
    ),

    Impl = fp_impl_ordinary("Y = X;", yes(Context)),

    % The `func(in) = out(<i_ground>) is det' mode.
    %
    ToGroundRepnSymName = solver_to_ground_repn_symname(TypeSymName, Arity),
    XTGPragmaVar = pragma_var(X, "X", in_mode, bp_native_if_possible),
    YTGPragmaVar = pragma_var(Y, "Y", OutGroundMode, bp_native_if_possible),
    ToGroundRepnArgs = [XTGPragmaVar, YTGPragmaVar],
    ToGroundRepnFPInfo = pragma_info_foreign_proc(
        Attrs,
        ToGroundRepnSymName,
        pf_function,
        ToGroundRepnArgs,
        ProgVarSet,
        InstVarSet,
        Impl
    ),
    add_pragma_foreign_proc(ToGroundRepnFPInfo, PredStatus, Context, no,
        !ModuleInfo, !Specs),

    % The `func(in(any)) = out(<i_any>) is det' mode.
    %
    ToAnyRepnSymName = solver_to_any_repn_symname(TypeSymName, Arity),
    XTAPragmaVar = pragma_var(X, "X", in_any_mode, bp_native_if_possible),
    YTAPragmaVar = pragma_var(Y, "Y", OutAnyMode, bp_native_if_possible),
    ToAnyRepnArgs = [XTAPragmaVar, YTAPragmaVar],
    ToAnyRepnFPInfo = pragma_info_foreign_proc(
        Attrs,
        ToAnyRepnSymName,
        pf_function,
        ToAnyRepnArgs,
        ProgVarSet,
        InstVarSet,
        Impl
    ),
    add_pragma_foreign_proc(ToAnyRepnFPInfo, PredStatus, Context, no,
        !ModuleInfo, !Specs),

    % The `func(in(<i_ground>)) = out is det' mode.
    %
    FromGroundRepnSymName = repn_to_ground_solver_symname(TypeSymName, Arity),
    XFGPragmaVar = pragma_var(X, "X", InGroundMode, bp_native_if_possible),
    YFGPragmaVar = pragma_var(Y, "Y", out_mode, bp_native_if_possible),
    FromGroundRepnArgs = [XFGPragmaVar, YFGPragmaVar],
    FromGroundRepnFPInfo = pragma_info_foreign_proc(
        Attrs,
        FromGroundRepnSymName,
        pf_function,
        FromGroundRepnArgs,
        ProgVarSet,
        InstVarSet,
        Impl
    ),
    add_pragma_foreign_proc(FromGroundRepnFPInfo, PredStatus, Context, no,
        !ModuleInfo, !Specs),

    % The `func(in(<i_any>)) = out(any) is det' mode.
    %
    FromAnyRepnSymName = repn_to_any_solver_symname(TypeSymName, Arity),
    XFAPragmaVar = pragma_var(X, "X", InAnyMode, bp_native_if_possible),
    YFAPragmaVar = pragma_var(Y, "Y", out_any_mode, bp_native_if_possible),
    FromAnyRepnArgs = [XFAPragmaVar, YFAPragmaVar],
    FromAnyRepnFPInfo = pragma_info_foreign_proc(
        Attrs,
        FromAnyRepnSymName,
        pf_function,
        FromAnyRepnArgs,
        ProgVarSet,
        InstVarSet,
        Impl
    ),
    add_pragma_foreign_proc(FromAnyRepnFPInfo, PredStatus, Context, no,
        !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

    % Obtain the solver type conversion function sym_names from
    % the solver type sym_name.
    %
:- func solver_to_ground_repn_symname(sym_name, arity) = sym_name.

solver_to_ground_repn_symname(SymName, Arity) =
    solver_conversion_fn_symname("representation of ground ", SymName, Arity).

:- func solver_to_any_repn_symname(sym_name, arity) = sym_name.

solver_to_any_repn_symname(SymName, Arity) =
    solver_conversion_fn_symname("representation of any ", SymName, Arity).

:- func repn_to_ground_solver_symname(sym_name, arity) = sym_name.

repn_to_ground_solver_symname(SymName, Arity) =
    solver_conversion_fn_symname("representation to ground ", SymName, Arity).

:- func repn_to_any_solver_symname(sym_name, arity) = sym_name.

repn_to_any_solver_symname(SymName, Arity) =
    solver_conversion_fn_symname("representation to any ", SymName, Arity).

:- func solver_conversion_fn_symname(string, sym_name, arity) = sym_name.

solver_conversion_fn_symname(Prefix, unqualified(Name), Arity) =
    unqualified(Prefix ++ Name ++ "/" ++ int_to_string(Arity)).
solver_conversion_fn_symname(Prefix, qualified(ModuleNames, Name), Arity) =
    qualified(ModuleNames, Prefix ++ Name ++ "/" ++ int_to_string(Arity)).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_solver.
%-----------------------------------------------------------------------------%
