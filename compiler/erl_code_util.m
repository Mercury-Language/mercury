%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: erl_code_util.m.
% Main author: wangp.
% 
% This module is part of the Erlang code generator.
% 
%-----------------------------------------------------------------------------%

:- module erl_backend.erl_code_util.
:- interface.

:- import_module erl_backend.elds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The `erl_gen_info' ADT.
%

    % The `erl_gen_info' type holds information used during
    % ELDS code generation for a given procedure.
    %
:- type erl_gen_info.

    % Initialize the erl_gen_info, so that it is ready for generating code
    % for the given procedure.
    %
:- func erl_gen_info_init(module_info, pred_id, proc_id) = erl_gen_info.

:- pred erl_gen_info_get_module_info(erl_gen_info::in, module_info::out)
    is det.
:- pred erl_gen_info_get_varset(erl_gen_info::in, prog_varset::out) is det.
:- pred erl_gen_info_get_var_types(erl_gen_info::in, vartypes::out) is det.
:- pred erl_gen_info_get_input_vars(erl_gen_info::in, prog_vars::out) is det.
:- pred erl_gen_info_get_output_vars(erl_gen_info::in, prog_vars::out) is det.

    % Create a new variable.
    %
:- pred erl_gen_info_new_named_var(string::in, prog_var::out,
    erl_gen_info::in, erl_gen_info::out) is det.

    % Create multiple new variables.
    %
:- pred erl_gen_info_new_vars(int::in, prog_vars::out,
    erl_gen_info::in, erl_gen_info::out) is det.

    % Create multiple new variables, which have names beginning with
    % underscores.
    %
:- pred erl_gen_info_new_anonymous_vars(int::in, prog_vars::out,
    erl_gen_info::in, erl_gen_info::out) is det.

    % Lookup the types of a list of variables.
    %
:- pred erl_variable_types(erl_gen_info::in, prog_vars::in,
    list(mer_type)::out) is det.

    % Lookup the type of a variable.
    %
:- pred erl_variable_type(erl_gen_info::in, prog_var::in, mer_type::out) is det.

    % Add the given string as the name of an environment variable used by
    % the function being generated.
    %
:- pred erl_gen_info_add_env_var_name(string::in,
    erl_gen_info::in, erl_gen_info::out) is det.

    % Get the names of the used environment variables.
    %
:- pred erl_gen_info_get_env_vars(erl_gen_info::in, set(string)::out) is det.

%-----------------------------------------------------------------------------%
%
% Various utility routines used for ELDS code generation
%

:- type opt_dummy_args
    --->    opt_dummy_args
    ;       no_opt_dummy_args.

    % erl_gen_arg_list(ModuleInfo, OptDummyArgs, Vars, VarTypes, VarModes,
    %   InputVars, OutputVars)
    %
    % Separate procedure call arguments into inputs and output variables.
    % If OptDummyArgs is `opt_dummy_args' then variables which are of dummy
    % types or have argument mode `top_unused' will be ignored, i.e. not appear
    % in either InputVars or OutputVars.
    %
:- pred erl_gen_arg_list(module_info::in, opt_dummy_args::in,
    list(T)::in, list(mer_type)::in, list(mer_mode)::in,
    list(T)::out, list(T)::out) is det.

    % As above but takes arg_modes instead of mer_modes.
    %
:- pred erl_gen_arg_list_arg_modes(module_info::in, opt_dummy_args::in,
    list(T)::in, list(mer_type)::in, list(arg_mode)::in,
    list(T)::out, list(T)::out) is det.

    % erl_fix_success_expr(InstMap, Goal, MaybeExpr0, MaybeExpr, !Info)
    %
    % Success expressions may contain assignments.  Assignments to local
    % variables may be incorrect or raise warnings from the Erlang compiler if
    % a success expression is duplicated.  Hence we rename away local variables
    % when duplicating a success expression.
    %
    % This predicate renames any local variables appearing in the success
    % expression (if any) to fresh variables, where local variables are those
    % which are not bound in InstMap and not bound within Goal.
    %
:- pred erl_fix_success_expr(instmap::in, hlds_goal::in,
    maybe(elds_expr)::in, maybe(elds_expr)::out,
    erl_gen_info::in, erl_gen_info::out) is det.

    % Return the set of non-dummy variables non-local to a goal which are bound
    % by that goal.
    %
:- pred erl_bound_nonlocals_in_goal(erl_gen_info::in, instmap::in,
    hlds_goal::in, set(prog_var)::out) is det.

    % erl_bind_unbound_vars(Info, VarsToBind, Goal, InstMap, !Statement)
    %
    % For any variables in VarsToBind which are not bound in Goal, add
    % assignment expressions to !Statement.  This is necessary to ensure that
    % all branches of ELDS code bind the same variables, to avoid warnings from
    % the Erlang compiler when one branch doesn't bind all the variables
    % because it has determinism `erroneous'.  The values given to the
    % variables do not matter since this is only done to appease the
    % Erlang compiler.
    %
    % VarsToBind must not include dummy variables.
    %
:- pred erl_bind_unbound_vars(erl_gen_info::in, set(prog_var)::in,
    hlds_goal::in, instmap::in, elds_expr::in, elds_expr::out) is det.

    % erl_var_or_dummy_replacement(ModuleInfo, VarTypes, DummyRepl, Var) = Expr
    %
    % Return DummyRepl if Var is of a dummy type, otherwise return
    % Var.
    %
:- func erl_var_or_dummy_replacement(module_info, vartypes, elds_term,
    prog_var) = elds_expr.

    % erl_create_renaming(Vars, Subst, !Info):
    %
    % Create a substitution for each variable in Vars to a fresh variable.
    %
:- pred erl_create_renaming(prog_vars::in, prog_var_renaming::out,
    erl_gen_info::in, erl_gen_info::out) is det.

    % erl_rename_vars_in_expr(Subn, Expr0, Expr):
    %
    % Substitute every occurrence of any variable for a substitute that appears
    % in the mapping Subn.  Variables which do not appear in Subn are left
    % unsubstituted.
    %
:- pred erl_rename_vars_in_expr(prog_var_renaming::in,
    elds_expr::in, elds_expr::out) is det.

    % erl_rename_vars_in_expr_except(KeepVars, Expr0, Expr, !Info):
    %
    % Rename all variables in Expr0 to fresh variables, except for the
    % variables in the set KeepVars.
    %
:- pred erl_rename_vars_in_expr_except(set(prog_var)::in,
    elds_expr::in, elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

    % erl_expr_vars(Expr, Vars)
    %
    % Vars is the set of variables appearing in Expr.
    %
:- pred erl_expr_vars(elds_expr::in, set(prog_var)::out) is det.

    % Return a rough indication of the "size" of an expression, where each
    % simple constant has a value of 1.  This is used to decide if an
    % expression is too big to duplicate.
    %
:- func erl_expr_size(elds_expr) = int.

%-----------------------------------------------------------------------------%

:- func erl_base_typeclass_info_method_offset = int.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module libs.compiler_util.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module svset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% The definition of the `erl_gen_info' ADT.
%

% The `erl_gen_info' type holds information used during Erlang code generation
% for a given procedure.

:- type erl_gen_info
    --->    erl_gen_info(
                % These fields remain constant for each procedure,
                % except for the varset and the set of environment variables,
                % which can be added to.

                module_info         :: module_info,
                pred_id             :: pred_id,
                proc_id             :: proc_id,
                varset              :: prog_varset,
                var_types           :: vartypes,

                % input_vars and output_vars do not include variables of dummy
                % types.
                input_vars          :: prog_vars,
                output_vars         :: prog_vars,

                % Set of environment variables used by this procedure.
                env_var_names       :: set(string)
            ).

erl_gen_info_init(ModuleInfo, PredId, ProcId) = Info :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_argmodes(ProcInfo, HeadModes),
    pred_info_get_arg_types(PredInfo, HeadTypes),
    erl_gen_arg_list(ModuleInfo, opt_dummy_args,
        HeadVars, HeadTypes, HeadModes, InputVars, OutputVars),
    EnvVars = set.init,
    Info = erl_gen_info(
        ModuleInfo,
        PredId,
        ProcId,
        VarSet,
        VarTypes,
        InputVars,
        OutputVars,
        EnvVars
    ).

erl_gen_info_get_module_info(Info, Info ^ module_info).
erl_gen_info_get_varset(Info, Info ^ varset).
erl_gen_info_get_var_types(Info, Info ^ var_types).
erl_gen_info_get_input_vars(Info, Info ^ input_vars).
erl_gen_info_get_output_vars(Info, Info ^ output_vars).

:- pred erl_gen_info_set_varset(prog_varset::in,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_info_set_varset(VarSet, Info, Info ^ varset := VarSet).

erl_gen_info_new_named_var(Name, NewVar, !Info) :-
    erl_gen_info_get_varset(!.Info, VarSet0),
    varset.new_named_var(VarSet0, Name, NewVar, VarSet),
    erl_gen_info_set_varset(VarSet, !Info).

erl_gen_info_new_vars(Num, NewVars, !Info) :-
    erl_gen_info_get_varset(!.Info, VarSet0),
    varset.new_vars(VarSet0, Num, NewVars, VarSet),
    erl_gen_info_set_varset(VarSet, !Info).

erl_gen_info_new_anonymous_vars(Num, NewVars, !Info) :-
    erl_gen_info_get_varset(!.Info, VarSet0),
    list.map_foldl(erl_gen_info_new_anonymous_var, 1 .. Num, NewVars,
        VarSet0, VarSet),
    erl_gen_info_set_varset(VarSet, !Info).

:- pred erl_gen_info_new_anonymous_var(int::in, prog_var::out,
    prog_varset::in, prog_varset::out) is det.

erl_gen_info_new_anonymous_var(_Num, NewVar, VarSet0, VarSet) :-
    varset.new_named_var(VarSet0, "_", NewVar, VarSet).

erl_variable_types(Info, Vars, Types) :-
    list.map(erl_variable_type(Info), Vars, Types).

erl_variable_type(Info, Var, Type) :-
    erl_gen_info_get_var_types(Info, VarTypes),
    map.lookup(VarTypes, Var, Type).

erl_gen_info_add_env_var_name(Name, !Info) :-
    EnvVarNames0 = !.Info ^ env_var_names,
    set.insert(EnvVarNames0, Name, EnvVarNames),
    !:Info = !.Info ^ env_var_names := EnvVarNames.

erl_gen_info_get_env_vars(Info, Info ^ env_var_names).

%-----------------------------------------------------------------------------%
%
% Various utility routines used for ELDS code generation
%

    % XXX arg_info.partition_* does a similar thing but returns sets instead
    % of lists
    %
erl_gen_arg_list(ModuleInfo, OptDummyArgs, VarNames, ArgTypes, Modes,
        Inputs, Outputs) :-
    modes_to_arg_modes(ModuleInfo, Modes, ArgTypes, ArgModes),
    erl_gen_arg_list_arg_modes(ModuleInfo, OptDummyArgs,
        VarNames, ArgTypes, ArgModes, Inputs, Outputs).

erl_gen_arg_list_arg_modes(ModuleInfo, OptDummyArgs,
        VarNames, ArgTypes, ArgModes, Inputs, Outputs) :-
    (
        VarNames = [],
        ArgTypes = [],
        ArgModes = []
    ->
        Inputs = [],
        Outputs = []
    ;
        VarNames = [VarName | VarNames1],
        ArgTypes = [ArgType | ArgTypes1],
        ArgModes = [ArgMode | ArgModes1]
    ->
        erl_gen_arg_list_arg_modes(ModuleInfo, OptDummyArgs,
            VarNames1, ArgTypes1, ArgModes1, Inputs1, Outputs1),
        (
            OptDummyArgs = opt_dummy_args,
            ( is_dummy_argument_type(ModuleInfo, ArgType)
            ; ArgMode = top_unused
            )
        ->
            % Exclude arguments of type io.state etc.
            % Also exclude those with arg_mode `top_unused'.
            Inputs = Inputs1,
            Outputs = Outputs1
        ;
            ArgMode = top_in
        ->
            % It's an input argument.
            Inputs = [VarName | Inputs1],
            Outputs = Outputs1
        ;
            % It's an output argument.
            Inputs = Inputs1,
            Outputs = [VarName | Outputs1]
        )
    ;
        unexpected(this_file, "erl_gen_arg_list_arg_modes: length mismatch")
    ).

%-----------------------------------------------------------------------------%

erl_fix_success_expr(InstMap0, Goal, MaybeExpr0, MaybeExpr, !Info) :-
    (
        MaybeExpr0 = yes(Expr0),
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        update_instmap(Goal, InstMap0, InstMap),
        instmap_bound_vars(InstMap, ModuleInfo, BoundVars),
        erl_rename_vars_in_expr_except(BoundVars, Expr0, Expr, !Info),
        MaybeExpr = yes(Expr)
    ;
        MaybeExpr0 = no,
        MaybeExpr = no
    ).

%-----------------------------------------------------------------------------%

erl_bound_nonlocals_in_goal(Info, InstMap, Goal, BoundNonLocals) :-
    erl_gen_info_get_module_info(Info, ModuleInfo),
    erl_gen_info_get_var_types(Info, VarTypes),
    Goal = hlds_goal(_, GoalInfo),
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
    BoundNonLocals = set.filter(is_bound_and_not_dummy(ModuleInfo, VarTypes,
        InstMap, InstmapDelta), NonLocals).

:- pred is_bound_and_not_dummy(module_info::in, vartypes::in, instmap::in,
    instmap_delta::in, prog_var::in) is semidet.

is_bound_and_not_dummy(ModuleInfo, VarTypes, InstMap, InstmapDelta, Var) :-
    var_is_bound_in_instmap_delta(ModuleInfo, InstMap, InstmapDelta, Var),
    map.lookup(VarTypes, Var, Type),
    not is_dummy_argument_type(ModuleInfo, Type).

erl_bind_unbound_vars(Info, VarsToBind, Goal, InstMap,
        Statement0, Statement) :-
    erl_bound_nonlocals_in_goal(Info, InstMap, Goal, Bound),
    NotBound = set.difference(VarsToBind, Bound),
    (if set.empty(NotBound) then
        Statement = Statement0
    else
        % We arbitrarily assign all the variables to the atom `false'.
        NotBoundList = set.to_sorted_list(NotBound),
        Assignments = list.map(var_eq_false, NotBoundList),
        Statement = join_exprs(elds_block(Assignments), Statement0)
    ).

%-----------------------------------------------------------------------------%

erl_var_or_dummy_replacement(ModuleInfo, VarTypes, DummyVarReplacement, Var) =
    (if
        map.search(VarTypes, Var, Type),
        is_dummy_argument_type(ModuleInfo, Type)
    then
        elds_term(DummyVarReplacement)
    else
        expr_from_var(Var)
    ).

%-----------------------------------------------------------------------------%

erl_create_renaming(Vars, Subst, !Info) :-
    erl_gen_info_get_varset(!.Info, VarSet0),
    list.foldl2(erl_create_renaming_2, Vars, VarSet0, VarSet, map.init, Subst),
    erl_gen_info_set_varset(VarSet, !Info).

:- pred erl_create_renaming_2(prog_var::in, prog_varset::in, prog_varset::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

erl_create_renaming_2(OldVar, VarSet0, VarSet, !Subst) :-
    ( varset.search_name(VarSet0, OldVar, Name) ->
        varset.new_named_var(VarSet0, Name, NewVar, VarSet)
    ;
        varset.new_var(VarSet0, NewVar, VarSet)
    ),
    map.det_insert(!.Subst, OldVar, NewVar, !:Subst).

:- pred erl_rename_vars_in_exprs(prog_var_renaming::in,
    list(elds_expr)::in, list(elds_expr)::out) is det.

erl_rename_vars_in_exprs(Subn, Exprs0, Exprs) :-
    list.map(erl_rename_vars_in_expr(Subn), Exprs0, Exprs).

erl_rename_vars_in_expr(Subn, Expr0, Expr) :-
    (
        Expr0 = elds_block(Exprs0),
        erl_rename_vars_in_exprs(Subn, Exprs0, Exprs),
        Expr = elds_block(Exprs)
    ;
        Expr0 = elds_term(Term0),
        erl_rename_vars_in_term(Subn, Term0, Term),
        Expr = elds_term(Term)
    ;
        Expr0 = elds_eq(ExprA0, ExprB0),
        erl_rename_vars_in_expr(Subn, ExprA0, ExprA),
        erl_rename_vars_in_expr(Subn, ExprB0, ExprB),
        Expr = elds_eq(ExprA, ExprB)
    ;
        Expr0 = elds_unop(Op, ExprA0),
        erl_rename_vars_in_expr(Subn, ExprA0, ExprA),
        Expr = elds_unop(Op, ExprA)
    ;
        Expr0 = elds_binop(Op, ExprA0, ExprB0),
        erl_rename_vars_in_expr(Subn, ExprA0, ExprA),
        erl_rename_vars_in_expr(Subn, ExprB0, ExprB),
        Expr = elds_binop(Op, ExprA, ExprB)
    ;
        Expr0 = elds_call(CallTarget0, ExprsB0),
        erl_rename_vars_in_call_target(Subn, CallTarget0, CallTarget),
        erl_rename_vars_in_exprs(Subn, ExprsB0, ExprsB),
        Expr = elds_call(CallTarget, ExprsB)
    ;
        Expr0 = elds_fun(Clause0),
        erl_rename_vars_in_clause(Subn, Clause0, Clause),
        Expr = elds_fun(Clause)
    ;
        Expr0 = elds_case_expr(ExprA0, Cases0),
        erl_rename_vars_in_expr(Subn, ExprA0, ExprA),
        erl_rename_vars_in_cases(Subn, Cases0, Cases),
        Expr = elds_case_expr(ExprA, Cases)
    ;
        Expr0 = elds_try(ExprA0, Cases0, MaybeCatch0, MaybeAfter0),
        erl_rename_vars_in_expr(Subn, ExprA0, ExprA),
        erl_rename_vars_in_cases(Subn, Cases0, Cases),
        (
            MaybeCatch0 = yes(Catch0),
            erl_rename_vars_in_catch(Subn, Catch0, Catch),
            MaybeCatch = yes(Catch)
        ;
            MaybeCatch0 = no,
            MaybeCatch = no
        ),
        (
            MaybeAfter0 = yes(After0),
            erl_rename_vars_in_expr(Subn, After0, After),
            MaybeAfter = yes(After)
        ;
            MaybeAfter0 = no,
            MaybeAfter = no
        ),
        Expr = elds_try(ExprA, Cases, MaybeCatch, MaybeAfter)
    ;
        Expr0 = elds_throw(ExprA0),
        erl_rename_vars_in_expr(Subn, ExprA0, ExprA),
        Expr = elds_throw(ExprA)
    ;
        Expr0 = elds_send(ExprA0, ExprB0),
        erl_rename_vars_in_expr(Subn, ExprA0, ExprA),
        erl_rename_vars_in_expr(Subn, ExprB0, ExprB),
        Expr = elds_send(ExprA, ExprB)
    ;
        Expr0 = elds_receive(Cases0),
        erl_rename_vars_in_cases(Subn, Cases0, Cases),
        Expr = elds_receive(Cases)
    ;
        ( Expr0 = elds_rtti_ref(_)
        ; Expr0 = elds_foreign_code(_, _)
        ),
        Expr = Expr0
    ).

:- pred erl_rename_vars_in_terms(prog_var_renaming::in,
    list(elds_term)::in, list(elds_term)::out) is det.

erl_rename_vars_in_terms(Subn, Terms0, Terms) :-
    list.map(erl_rename_vars_in_term(Subn), Terms0, Terms).

:- pred erl_rename_vars_in_term(prog_var_renaming::in,
    elds_term::in, elds_term::out) is det.

erl_rename_vars_in_term(Subn, Term0, Term) :-
    (
        ( Term0 = elds_int(_)
        ; Term0 = elds_float(_)
        ; Term0 = elds_string(_)
        ; Term0 = elds_char(_)
        ; Term0 = elds_atom_raw(_)
        ; Term0 = elds_atom(_)
        ; Term0 = elds_anon_var
        ; Term0 = elds_fixed_name_var(_)
        ),
        Term = Term0
    ;
        Term0 = elds_tuple(Exprs0),
        erl_rename_vars_in_exprs(Subn, Exprs0, Exprs),
        Term = elds_tuple(Exprs)
    ;
        Term0 = elds_var(Var0),
        Var = (if map.search(Subn, Var0, Var1) then Var1 else Var0),
        Term = elds_var(Var)
    ).

:- pred erl_rename_vars_in_call_target(prog_var_renaming::in,
    elds_call_target::in, elds_call_target::out) is det.

erl_rename_vars_in_call_target(Subn, Target0, Target) :-
    (
        ( Target0 = elds_call_plain(_)
        ; Target0 = elds_call_builtin(_)
        ),
        Target = Target0
    ;
        Target0 = elds_call_ho(Expr0),
        erl_rename_vars_in_expr(Subn, Expr0, Expr),
        Target = elds_call_ho(Expr)
    ).

:- pred erl_rename_vars_in_clause(prog_var_renaming::in,
    elds_clause::in, elds_clause::out) is det.

erl_rename_vars_in_clause(Subn, Clause0, Clause) :-
    Clause0 = elds_clause(Pattern0, Expr0),
    erl_rename_vars_in_terms(Subn, Pattern0, Pattern),
    erl_rename_vars_in_expr(Subn, Expr0, Expr),
    Clause = elds_clause(Pattern, Expr).

:- pred erl_rename_vars_in_cases(prog_var_renaming::in,
    list(elds_case)::in, list(elds_case)::out) is det.

erl_rename_vars_in_cases(Subn, Cases0, Cases) :-
    list.map(erl_rename_vars_in_case(Subn), Cases0, Cases).

:- pred erl_rename_vars_in_case(prog_var_renaming::in,
    elds_case::in, elds_case::out) is det.

erl_rename_vars_in_case(Subn, Case0, Case) :-
    Case0 = elds_case(Pattern0, Expr0),
    erl_rename_vars_in_term(Subn, Pattern0, Pattern),
    erl_rename_vars_in_expr(Subn, Expr0, Expr),
    Case = elds_case(Pattern, Expr).

:- pred erl_rename_vars_in_catch(prog_var_renaming::in,
    elds_catch::in, elds_catch::out) is det.

erl_rename_vars_in_catch(Subn, Catch0, Catch) :-
    Catch0 = elds_catch(PatternA0, PatternB0, Expr0),
    erl_rename_vars_in_term(Subn, PatternA0, PatternA),
    erl_rename_vars_in_term(Subn, PatternB0, PatternB),
    erl_rename_vars_in_expr(Subn, Expr0, Expr),
    Catch = elds_catch(PatternA, PatternB, Expr).

%-----------------------------------------------------------------------------%

erl_rename_vars_in_expr_except(ExceptVars, Expr0, Expr, !Info) :-
    erl_expr_vars(Expr0, Vars0),
    Vars = set.difference(Vars0, ExceptVars),
    erl_create_renaming(set.to_sorted_list(Vars), Subn, !Info),
    erl_rename_vars_in_expr(Subn, Expr0, Expr).

%-----------------------------------------------------------------------------%

erl_expr_vars(Expr, Set) :-
    erl_vars_in_expr(Expr, set.init, Set).

:- pred erl_vars_in_exprs(list(elds_expr)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_exprs(Exprs, !Set) :-
    list.foldl(erl_vars_in_expr, Exprs, !Set).

:- pred erl_vars_in_expr(elds_expr::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_expr(Expr, !Set) :-
    (
        Expr = elds_block(Exprs),
        erl_vars_in_exprs(Exprs, !Set)
    ;
        Expr = elds_term(Term),
        erl_vars_in_term(Term, !Set)
    ;
        Expr = elds_eq(ExprA, ExprB),
        erl_vars_in_expr(ExprA, !Set),
        erl_vars_in_expr(ExprB, !Set)
    ;
        Expr = elds_unop(_Op, ExprA),
        erl_vars_in_expr(ExprA, !Set)
    ;
        Expr = elds_binop(_Op, ExprA, ExprB),
        erl_vars_in_expr(ExprA, !Set),
        erl_vars_in_expr(ExprB, !Set)
    ;
        Expr = elds_call(CallTarget, ExprsB),
        erl_vars_in_call_target(CallTarget, !Set),
        erl_vars_in_exprs(ExprsB, !Set)
    ;
        Expr = elds_fun(Clause),
        erl_vars_in_clause(Clause, !Set)
    ;
        Expr = elds_case_expr(ExprA, Cases),
        erl_vars_in_expr(ExprA, !Set),
        erl_vars_in_cases(Cases, !Set)
    ;
        Expr = elds_try(ExprA, Cases, MaybeCatch, MaybeAfter),
        erl_vars_in_expr(ExprA, !Set),
        erl_vars_in_cases(Cases, !Set),
        (
            MaybeCatch = yes(Catch),
            erl_vars_in_catch(Catch, !Set)
        ;
            MaybeCatch = no
        ),
        (
            MaybeAfter = yes(After),
            erl_vars_in_expr(After, !Set)
        ;
            MaybeAfter = no
        )
    ;
        Expr = elds_throw(ExprA),
        erl_vars_in_expr(ExprA, !Set)
    ;
        Expr = elds_send(ExprA, ExprB),
        erl_vars_in_expr(ExprA, !Set),
        erl_vars_in_expr(ExprB, !Set)
    ;
        Expr = elds_receive(Cases),
        erl_vars_in_cases(Cases, !Set)
    ;
        ( Expr = elds_rtti_ref(_)
        ; Expr = elds_foreign_code(_, _)
        )
    ).

:- pred erl_vars_in_terms(list(elds_term)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_terms(Terms, !Set) :-
    list.foldl(erl_vars_in_term, Terms, !Set).

:- pred erl_vars_in_term(elds_term::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_term(Term, !Set) :-
    (
        ( Term = elds_int(_)
        ; Term = elds_float(_)
        ; Term = elds_string(_)
        ; Term = elds_char(_)
        ; Term = elds_atom_raw(_)
        ; Term = elds_atom(_)
        ; Term = elds_anon_var
        ; Term = elds_fixed_name_var(_)
        )
    ;
        Term = elds_tuple(Exprs),
        erl_vars_in_exprs(Exprs, !Set)
    ;
        Term = elds_var(Var),
        svset.insert(Var, !Set)
    ).

:- pred erl_vars_in_call_target(elds_call_target::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_call_target(Target, !Set) :-
    (
        ( Target = elds_call_plain(_)
        ; Target = elds_call_builtin(_)
        )
    ;
        Target = elds_call_ho(Expr),
        erl_vars_in_expr(Expr, !Set)
    ).

:- pred erl_vars_in_clause(elds_clause::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_clause(Clause, !Set) :-
    Clause = elds_clause(Pattern, Expr),
    erl_vars_in_terms(Pattern, !Set),
    erl_vars_in_expr(Expr, !Set).

:- pred erl_vars_in_cases(list(elds_case)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_cases(Cases, !Set) :-
    list.foldl(erl_vars_in_case, Cases, !Set).

:- pred erl_vars_in_case(elds_case::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_case(Case, !Set) :-
    Case = elds_case(Pattern, Expr),
    erl_vars_in_term(Pattern, !Set),
    erl_vars_in_expr(Expr, !Set).

:- pred erl_vars_in_catch(elds_catch::in,
    set(prog_var)::in, set(prog_var)::out) is det.

erl_vars_in_catch(Catch, !Set) :-
    Catch = elds_catch(PatternA, PatternB, Expr),
    erl_vars_in_term(PatternA, !Set),
    erl_vars_in_term(PatternB, !Set),
    erl_vars_in_expr(Expr, !Set).

%-----------------------------------------------------------------------------%

:- func erl_exprs_size(list(elds_expr)) = int.

erl_exprs_size(Exprs) = sum(list.map(erl_expr_size, Exprs)).

erl_expr_size(Expr) = Size :-
    (
        Expr = elds_block(Exprs),
        Size = erl_exprs_size(Exprs)
    ;
        Expr = elds_term(Term),
        Size = erl_term_size(Term)
    ;
        Expr = elds_eq(ExprA, ExprB),
        Size = erl_expr_size(ExprA) + erl_expr_size(ExprB)
    ;
        Expr = elds_unop(_Op, ExprA),
        Size = erl_expr_size(ExprA)
    ;
        Expr = elds_binop(_Op, ExprA, ExprB),
        Size = erl_expr_size(ExprA) + erl_expr_size(ExprB)
    ;
        Expr = elds_call(CallTarget, Exprs),
        Size = erl_call_target_size(CallTarget) + erl_exprs_size(Exprs)
    ;
        Expr = elds_fun(elds_clause(Terms, ExprA)),
        Size = 1 + erl_terms_size(Terms) + erl_expr_size(ExprA)
    ;
        Expr = elds_case_expr(ExprA, Cases),
        Size = 1 + erl_expr_size(ExprA) + erl_cases_size(Cases)
    ;
        Expr = elds_try(ExprA, Cases, MaybeCatch, MaybeAfter),
        (
            MaybeCatch = yes(elds_catch(TermA, TermB, CatchExpr)),
            CatchSize = erl_term_size(TermA) + erl_term_size(TermB) +
                erl_expr_size(CatchExpr)
        ;
            MaybeCatch = no,
            CatchSize = 0
        ),
        (
            MaybeAfter = yes(AfterExpr),
            AfterSize = erl_expr_size(AfterExpr)
        ;
            MaybeAfter = no,
            AfterSize = 0
        ),
        Size = 1 + erl_expr_size(ExprA) + erl_cases_size(Cases) +
            CatchSize + AfterSize
    ;
        Expr = elds_throw(ExprA),
        Size = 1 + erl_expr_size(ExprA)
    ;
        Expr = elds_send(ExprA, ExprB),
        Size = 1 + erl_expr_size(ExprA) + erl_expr_size(ExprB)
    ;
        Expr = elds_receive(Cases),
        Size = 1 + erl_cases_size(Cases)
    ;
        Expr = elds_rtti_ref(_),
        Size = 1
    ;
        Expr = elds_foreign_code(_, _),
        % Arbitrary number.
        Size = 10000
    ).

:- func erl_terms_size(list(elds_term)) = int.

erl_terms_size(Terms) = sum(list.map(erl_term_size, Terms)).

:- func erl_term_size(elds_term) = int.

erl_term_size(Term) = Size :-
    (
        ( Term = elds_int(_)
        ; Term = elds_float(_)
        ; Term = elds_string(_)
        ; Term = elds_char(_)
        ; Term = elds_atom_raw(_)
        ; Term = elds_atom(_)
        ; Term = elds_var(_)
        ; Term = elds_anon_var
        ; Term = elds_fixed_name_var(_)
        ),
        Size = 1
    ;
        Term = elds_tuple(Exprs),
        Size = 1 + erl_exprs_size(Exprs)
    ).

:- func erl_call_target_size(elds_call_target) = int.

erl_call_target_size(elds_call_plain(_)) = 1.
erl_call_target_size(elds_call_builtin(_)) = 1.
erl_call_target_size(elds_call_ho(Expr)) = erl_expr_size(Expr).

:- func erl_cases_size(list(elds_case)) = int.

erl_cases_size(Cases) = 1 + sum(list.map(erl_case_size, Cases)).

:- func erl_case_size(elds_case) = int.

erl_case_size(Case) = Size :-
    Case = elds_case(Pattern, Expr),
    Size = 1 + erl_term_size(Pattern) + erl_expr_size(Expr).

:- func sum(list(int)) = int.

sum(Xs) = list.foldl(int.plus, Xs, 0).

%-----------------------------------------------------------------------------%

    % This function returns the offset to add to the method number
    % for a type class method to get its field number within the
    % base_typeclass_info.
    %   field 0 is num_extra
    %   field 1 is num_constraints
    %   field 2 is num_superclasses
    %   field 3 is class_arity
    %   field 4 is num_methods
    %   field 5 is the 1st method
    %   field 6 is the 2nd method
    %   etc.
    %   (See the base_typeclass_info type in rtti.m or the
    %   description in notes/type_class_transformation.html for
    %   more information about the layout of base_typeclass_infos.)
    % Hence the offset is 4.
    %
erl_base_typeclass_info_method_offset = 4.

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "erl_code_util.m".

%-----------------------------------------------------------------------------%
:- end_module erl_code_util.
%-----------------------------------------------------------------------------%
