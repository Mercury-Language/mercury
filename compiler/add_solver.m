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
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % A solver type t defined with
    %
    % :- solver type st
    %   where representation is rt,     % type
    %         initialisation is ip,     % pred
    %         ground         is gi,     % inst
    %         any            is ai, ... % inst
    %
    % causes the following to be introduced:
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
:- pred add_solver_type_decl_items(tvarset::in, sym_name::in,
    list(type_param)::in, solver_type_details::in, prog_context::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_solver_type_clause_items(sym_name::in, list(type_param)::in,
    solver_type_details::in, prog_context::in,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.make_hlds.add_pred.
:- import_module libs.globals.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_solver_type_decl_items(TVarSet, TypeSymName, TypeParams,
        SolverTypeDetails, Context, !Status, !ModuleInfo, !Specs) :-

    % XXX kind inference:
    % We set the kinds to `star'.  This will be different when we have a
    % kind system.
    prog_type.var_list_to_type_list(map.init, TypeParams, Args),
    SolverType        = defined_type(TypeSymName, Args, kind_star),
    Arity             = length(TypeParams),

    RepnType          = SolverTypeDetails ^ std_representation_type,
    AnyInst           = SolverTypeDetails ^ std_any_inst,
    GroundInst        = SolverTypeDetails ^ std_ground_inst,

    InAnyMode         = in_mode(AnyInst),
    InGroundMode      = in_mode(GroundInst),

    OutAnyMode        = out_mode(AnyInst),
    OutGroundMode     = out_mode(GroundInst),

    InstVarSet        = varset.init,
    ExistQTVars       = [],

    init_markers(NoMarkers),

    % Insert the conversion function declarations.

        % The `:- impure
        %   func 'representation of ground st'(st::in(gi)) =
        %           (rt::out) is det' declaration.
        %
    ToGroundRepnSymName  = solver_to_ground_repn_symname(TypeSymName, Arity),
    ToGroundRepnArgTypes =
        [type_and_mode(SolverType, in_mode      ),
         type_and_mode(RepnType,   OutGroundMode)],
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, pf_function,
        ToGroundRepnSymName, ToGroundRepnArgTypes, yes(detism_det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
        !ModuleInfo, !Specs),

        % The `:- impure
        %   func 'representation of any st'(st::in(ai)) =
        %           (rt::out(any)) is det' declaration.
        %
    ToAnyRepnSymName     = solver_to_any_repn_symname(TypeSymName, Arity),
    ToAnyRepnArgTypes    =
        [type_and_mode(SolverType, in_any_mode ),
         type_and_mode(RepnType,   OutAnyMode)],
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, pf_function,
        ToAnyRepnSymName, ToAnyRepnArgTypes, yes(detism_det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
        !ModuleInfo, !Specs),

        % The `:- impure
        %   func 'representation to ground st'(rt::in(gi)) =
        %           (st::out) is det' declaration.
        %
    FromGroundRepnSymName  = repn_to_ground_solver_symname(TypeSymName, Arity),
    FromGroundRepnArgTypes =
        [type_and_mode(RepnType,   InGroundMode   ),
         type_and_mode(SolverType, out_mode       )],
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, pf_function,
        FromGroundRepnSymName, FromGroundRepnArgTypes, yes(detism_det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
        !ModuleInfo, !Specs),

        % The `:- impure
        %   func 'representation to any st'(rt::in(ai)) =
        %           (st::out(any)) is det' declaration.
        %
    FromAnyRepnSymName  = repn_to_any_solver_symname(TypeSymName, Arity),
    FromAnyRepnArgTypes =
        [type_and_mode(RepnType,   InAnyMode   ),
         type_and_mode(SolverType, out_any_mode)],
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, pf_function,
        FromAnyRepnSymName, FromAnyRepnArgTypes, yes(detism_det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
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

add_solver_type_clause_items(TypeSymName, TypeParams, SolverTypeDetails,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    Arity = length(TypeParams),

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
    ;
        Target = target_erlang,
        Lang = lang_erlang
    ;
        ( Target = target_il
        ; Target = target_x86_64
        ),
        WhatMsg = "solver type conversion functions for this backend",
        sorry($module, WhatMsg)
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

    (
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_java
        ),
        Impl = fp_impl_ordinary("Y = X;", yes(Context))
    ;
        Lang = lang_erlang,
        Impl = fp_impl_ordinary("Y = X", yes(Context))
    ),
    
    % The `func(in) = out(<i_ground>) is det' mode.
    %
    ToGroundRepnSymName = solver_to_ground_repn_symname(TypeSymName, Arity),
    XTGPragmaVar = pragma_var(X, "X", in_mode, native_if_possible),
    YTGPragmaVar = pragma_var(Y, "Y", OutGroundMode, native_if_possible),
    ToGroundRepnArgs = [XTGPragmaVar, YTGPragmaVar],
    ToGroundRepnForeignProc = pragma_foreign_proc(
        pragma_info_foreign_proc(
            Attrs,
            ToGroundRepnSymName,
            pf_function,
            ToGroundRepnArgs,
            ProgVarSet,
            InstVarSet,
            Impl
        )
    ),
    ToGroundRepnItemPragma = item_pragma_info(compiler(solver_type),
        ToGroundRepnForeignProc, Context, -1),
    ToGroundRepnItem = item_pragma(ToGroundRepnItemPragma),
    add_item_pass_3(ToGroundRepnItem, !Status, !ModuleInfo,
        !QualInfo, !Specs),

    % The `func(in(any)) = out(<i_any>) is det' mode.
    %
    ToAnyRepnSymName = solver_to_any_repn_symname(TypeSymName, Arity),
    XTAPragmaVar = pragma_var(X, "X", in_any_mode, native_if_possible),
    YTAPragmaVar = pragma_var(Y, "Y", OutAnyMode, native_if_possible),
    ToAnyRepnArgs = [XTAPragmaVar, YTAPragmaVar],
    ToAnyRepnForeignProc = pragma_foreign_proc(
        pragma_info_foreign_proc(
            Attrs,
            ToAnyRepnSymName,
            pf_function,
            ToAnyRepnArgs,
            ProgVarSet,
            InstVarSet,
            Impl
        )
    ),
    ToAnyRepnItemPragma = item_pragma_info(compiler(solver_type),
        ToAnyRepnForeignProc, Context, -1),
    ToAnyRepnItem = item_pragma(ToAnyRepnItemPragma),
    add_item_pass_3(ToAnyRepnItem, !Status, !ModuleInfo,
        !QualInfo, !Specs),

    % The `func(in(<i_ground>)) = out is det' mode.
    %
    FromGroundRepnSymName = repn_to_ground_solver_symname(TypeSymName, Arity),
    XFGPragmaVar = pragma_var(X, "X", InGroundMode, native_if_possible),
    YFGPragmaVar = pragma_var(Y, "Y", out_mode, native_if_possible),
    FromGroundRepnArgs = [XFGPragmaVar, YFGPragmaVar],
    FromGroundRepnForeignProc = pragma_foreign_proc(
        pragma_info_foreign_proc(
            Attrs,
            FromGroundRepnSymName,
            pf_function,
            FromGroundRepnArgs,
            ProgVarSet,
            InstVarSet,
            Impl
        )
    ),
    FromGroundRepnItemPragma = item_pragma_info(compiler(solver_type),
        FromGroundRepnForeignProc, Context, -1),
    FromGroundRepnItem = item_pragma(FromGroundRepnItemPragma),
    add_item_pass_3(FromGroundRepnItem, !Status, !ModuleInfo,
        !QualInfo, !Specs),

    % The `func(in(<i_any>)) = out(any) is det' mode.
    %
    FromAnyRepnSymName = repn_to_any_solver_symname(TypeSymName, Arity),
    XFAPragmaVar = pragma_var(X, "X", InAnyMode, native_if_possible),
    YFAPragmaVar = pragma_var(Y, "Y", out_any_mode, native_if_possible),
    FromAnyRepnArgs = [XFAPragmaVar, YFAPragmaVar],
    FromAnyRepnForeignProc = pragma_foreign_proc(
        pragma_info_foreign_proc(
            Attrs,
            FromAnyRepnSymName,
            pf_function,
            FromAnyRepnArgs,
            ProgVarSet,
            InstVarSet,
            Impl
        )
    ),
    FromAnyRepnItemPragma = item_pragma_info(compiler(solver_type),
        FromAnyRepnForeignProc, Context, -1),
    FromAnyRepnItem = item_pragma(FromAnyRepnItemPragma),
    add_item_pass_3(FromAnyRepnItem, !Status, !ModuleInfo,
        !QualInfo, !Specs).

%-----------------------------------------------------------------------------%
 :- end_module add_solver.
%-----------------------------------------------------------------------------%
