%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_solver.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type solver_aux_pred_info
    --->    solver_aux_pred_info(
                sym_name,
                list(type_param),
                tvarset,
                solver_type_details,
                prog_context
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
:- pred get_solver_type_aux_pred_decls(solver_aux_pred_info::in,
    list(item_pred_decl_info)::out) is det.

    % Define the auxiliary predicates declared above. It is the caller's
    % resposibility to call this predicate only if the solver type is defined
    % in this module, since we don't want them to be doubly defined
    % both in this module and in the module that defines the solver type.
    %
:- pred get_solver_type_aux_pred_defns(compilation_target::in,
    solver_aux_pred_info::in, list(item_foreign_proc)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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

get_solver_type_aux_pred_decls(SolverAuxPredInfo, PredDecls) :-
    SolverAuxPredInfo = solver_aux_pred_info(TypeSymName, TypeParams,
        TVarSet, SolverTypeDetails, Context),
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
    NoWithType = maybe.no,
    NoWithInst = maybe.no,
    DetismDet = yes(detism_det),

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
        ToGndRepnArgTypesModes, NoWithType, NoWithInst, DetismDet,
        ToGndMaybeAttrs, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, item_no_seq_num),

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
        ToAnyRepnArgTypesModes, NoWithType, NoWithInst, DetismDet,
        ToAnyMaybeAttrs, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, item_no_seq_num),

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
        FromGndRepnArgTypesModes, NoWithType, NoWithInst, DetismDet,
        FromGndMaybeAttrs, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, item_no_seq_num),

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
    FromAnyOrigin = item_origin_compiler(FromAnyAttrs),
    FromAnyPredDecl = item_pred_decl_info(FromAnyRepnSymName, pf_function,
        FromAnyRepnArgTypesModes, NoWithType, NoWithInst, DetismDet,
        FromAnyOrigin, TVarSet, InstVarSet, ExistQTVars, purity_impure,
        NoConstraints, Context, item_no_seq_num),

    PredDecls =
        [ToGndPredDecl, ToAnyPredDecl, FromGndPredDecl, FromAnyPredDecl].

%-----------------------------------------------------------------------------%

get_solver_type_aux_pred_defns(Target, SolverAuxPredInfo,
        PragmaForeignProcs) :-
    SolverAuxPredInfo = solver_aux_pred_info(TypeSymName, TypeParams,
        _TVarSet, SolverTypeDetails, Context),
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
    PragmaToGroundRepnFPInfo =
        item_pragma_info(ToGroundRepnFPInfo, Context, item_no_seq_num),

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
    PragmaToAnyRepnFPInfo =
        item_pragma_info(ToAnyRepnFPInfo, Context, item_no_seq_num),

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
    PragmaFromGroundRepnFPInfo =
        item_pragma_info(FromGroundRepnFPInfo, Context, item_no_seq_num),

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
    PragmaFromAnyRepnFPInfo =
        item_pragma_info(FromAnyRepnFPInfo, Context, item_no_seq_num),

    PragmaForeignProcs =
        [PragmaToGroundRepnFPInfo,
        PragmaToAnyRepnFPInfo,
        PragmaFromGroundRepnFPInfo,
        PragmaFromAnyRepnFPInfo].

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
