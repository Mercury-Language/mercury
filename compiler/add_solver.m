%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds__make_hlds__add_solver.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % A solver type t defined with
    %
    % :- solver type st
    %   where representation is rt,     % type
    %         initialisation is ip,     % pred
    %         ground         is gi,     % inst
    %         any            is ai, ...     % inst
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
    io::di, io::uo) is det.

:- pred add_solver_type_clause_items(sym_name::in, list(type_param)::in,
    solver_type_details::in, import_status::in, import_status::out,
    prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.make_hlds.add_pred.
:- import_module libs.globals.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module map.
:- import_module string.
:- import_module std_util.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_solver_type_decl_items(TVarSet, TypeSymName, TypeParams,
        SolverTypeDetails, Context, !Status, !ModuleInfo, !IO) :-

    % XXX kind inference:
    % We set the kinds to `star'.  This will be different when we have a
    % kind system.
    prog_type.var_list_to_type_list(map__init, TypeParams, Args),
    SolverType        = defined(TypeSymName, Args, star),
    Arity             = length(TypeParams),

    RepnType          = SolverTypeDetails ^ representation_type,
    AnyInst           = SolverTypeDetails ^ any_inst,
    GroundInst        = SolverTypeDetails ^ ground_inst,

    InAnyMode         = in_mode(AnyInst),
    InGroundMode      = in_mode(GroundInst),

    OutAnyMode        = out_mode(AnyInst),
    OutGroundMode     = out_mode(GroundInst),

    InstVarSet        = varset__init,
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
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, function,
        ToGroundRepnSymName, ToGroundRepnArgTypes, yes(det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
        !ModuleInfo, !IO),

        % The `:- impure
        %   func 'representation of any st'(st::in(ai)) =
        %           (rt::out(any)) is det' declaration.
        %
    ToAnyRepnSymName     = solver_to_any_repn_symname(TypeSymName, Arity),
    ToAnyRepnArgTypes    =
        [type_and_mode(SolverType, in_any_mode ),
         type_and_mode(RepnType,   OutAnyMode)],
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, function,
        ToAnyRepnSymName, ToAnyRepnArgTypes, yes(det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
        !ModuleInfo, !IO),

        % The `:- impure
        %   func 'representation to ground st'(rt::in(gi)) =
        %           (st::out) is det' declaration.
        %
    FromGroundRepnSymName  = repn_to_ground_solver_symname(TypeSymName, Arity),
    FromGroundRepnArgTypes =
        [type_and_mode(RepnType,   InGroundMode   ),
         type_and_mode(SolverType, out_mode       )],
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, function,
        FromGroundRepnSymName, FromGroundRepnArgTypes, yes(det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
        !ModuleInfo, !IO),

        % The `:- impure
        %   func 'representation to any st'(rt::in(ai)) =
        %           (st::out(any)) is det' declaration.
        %
    FromAnyRepnSymName  = repn_to_any_solver_symname(TypeSymName, Arity),
    FromAnyRepnArgTypes =
        [type_and_mode(RepnType,   InAnyMode   ),
         type_and_mode(SolverType, out_any_mode)],
    module_add_pred_or_func(TVarSet, InstVarSet, ExistQTVars, function,
        FromAnyRepnSymName, FromAnyRepnArgTypes, yes(det),
        purity_impure, constraints([], []), NoMarkers, Context, !.Status, _,
        !ModuleInfo, !IO).

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
        !Status, Context, !ModuleInfo, !QualInfo, !IO) :-
    Arity             = length(TypeParams),

    AnyInst           = SolverTypeDetails ^ any_inst,
    GroundInst        = SolverTypeDetails ^ ground_inst,

    InAnyMode         = in_mode(AnyInst),
    InGroundMode      = in_mode(GroundInst),

    OutAnyMode        = out_mode(AnyInst),
    OutGroundMode     = out_mode(GroundInst),

    ProgVarSet0           = varset__init,
    varset__new_var(ProgVarSet0, X, ProgVarSet1),
    varset__new_var(ProgVarSet1, Y, ProgVarSet),
    
    InstVarSet = varset__init,
    
    Attrs0            = default_attributes(c),
    some [!Attrs] (
        !:Attrs = Attrs0,
        set_may_call_mercury(will_not_call_mercury, !Attrs),
        set_thread_safe(thread_safe, !Attrs),
        set_terminates(terminates, !Attrs),
        Attrs = !.Attrs
    ),

    Impl              = ordinary("Y = X;", yes(Context)),

        % The `func(in) = out(<i_ground>) is det' mode.
        %
    ToGroundRepnSymName = solver_to_ground_repn_symname(TypeSymName, Arity),
    ToGroundRepnArgs = [ pragma_var(X, "X", in_mode      ),
                         pragma_var(Y, "Y", OutGroundMode) ],
    ToGroundRepnForeignProc =
        foreign_proc(
            Attrs,
            ToGroundRepnSymName,
            function,
            ToGroundRepnArgs,
            ProgVarSet,
            InstVarSet, 
            Impl
        ),
    ToGroundRepnItem = pragma(compiler(solver_type), ToGroundRepnForeignProc),
    add_item_clause(ToGroundRepnItem, !Status, Context, !ModuleInfo, !QualInfo,
        !IO),

        % The `func(in(any)) = out(<i_any>) is det' mode.
        %
    ToAnyRepnSymName = solver_to_any_repn_symname(TypeSymName, Arity),
    ToAnyRepnArgs = [ pragma_var(X, "X", in_any_mode),
                      pragma_var(Y, "Y", OutAnyMode ) ],
    ToAnyRepnForeignProc =
        foreign_proc(
            Attrs,
            ToAnyRepnSymName,
            function,
            ToAnyRepnArgs,
            ProgVarSet,
            InstVarSet,
            Impl
        ),
    ToAnyRepnItem = pragma(compiler(solver_type), ToAnyRepnForeignProc),
    add_item_clause(ToAnyRepnItem, !Status, Context, !ModuleInfo, !QualInfo,
        !IO),

        % The `func(in(<i_ground>)) = out is det' mode.
        %
    FromGroundRepnSymName = repn_to_ground_solver_symname(TypeSymName, Arity),
    FromGroundRepnArgs        = [ pragma_var(X, "X", InGroundMode),
                          pragma_var(Y, "Y", out_mode) ],
    FromGroundRepnForeignProc =
        foreign_proc(
            Attrs,
            FromGroundRepnSymName,
            function,
            FromGroundRepnArgs,
            ProgVarSet,
            InstVarSet,
            Impl
        ),
    FromGroundRepnItem = pragma(compiler(solver_type),
        FromGroundRepnForeignProc),
    add_item_clause(FromGroundRepnItem, !Status, Context, !ModuleInfo,
        !QualInfo, !IO),

        % The `func(in(<i_any>)) = out(any) is det' mode.
        %
    FromAnyRepnSymName = repn_to_any_solver_symname(TypeSymName, Arity),
    FromAnyRepnArgs = [ pragma_var(X, "X", InAnyMode   ),
                        pragma_var(Y, "Y", out_any_mode) ],
    FromAnyRepnForeignProc =
        foreign_proc(
            Attrs,
            FromAnyRepnSymName,
            function,
            FromAnyRepnArgs,
            ProgVarSet,
            InstVarSet,
            Impl
        ),
    FromAnyRepnItem = pragma(compiler(solver_type), FromAnyRepnForeignProc),
    add_item_clause(FromAnyRepnItem, !Status, Context, !ModuleInfo, !QualInfo,
        !IO).

%-----------------------------------------------------------------------------%
 :- end_module add_solver.
%-----------------------------------------------------------------------------%
