%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_code_util.m.
% Main author: fjh.
%
% This module is part of the MLDS code generator; it contains utility
% predicates.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_code_util.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%
%
% Various utility routines used for MLDS code generation.
%

    % Generate an MLDS assignment statement.
    %
:- func ml_gen_assign(mlds_lval, mlds_rval, prog_context) = mlds_stmt.

    % Generate a block statement, i.e. `{ <Decls>; <Stmts>; }'.
    % But if the block consists only of a single statement with no
    % declarations, then just return that statement.
    %
:- func ml_gen_block(list(mlds_local_var_defn), list(mlds_function_defn),
    list(mlds_stmt), prog_context) = mlds_stmt.

:- type gen_pred ==
    pred(list(mlds_local_var_defn), list(mlds_function_defn), list(mlds_stmt),
        ml_gen_info, ml_gen_info).
:- inst gen_pred == (pred(out, out, out, in, out) is det).

    % Given closures to generate code for two conjuncts, generate code
    % for their conjunction.
    %
:- pred ml_combine_conj(code_model::in, prog_context::in,
    gen_pred::in(gen_pred), gen_pred::in(gen_pred),
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

    % Given a function label and the statement which will comprise
    % the function body for that function, generate an mlds_function_defn
    % which defines that function.
    %
:- pred ml_gen_nondet_label_func(ml_gen_info::in, mlds_maybe_aux_func_id::in,
    prog_context::in, mlds_stmt::in, mlds_function_defn::out) is det.

    % Given a function label, the function parameters, and the statement
    % which will comprise the function body for that function,
    % generate an mlds_function_defn which defines that function.
    %
:- pred ml_gen_label_func(ml_gen_info::in, mlds_maybe_aux_func_id::in,
    mlds_func_params::in, prog_context::in, mlds_stmt::in,
    mlds_function_defn::out) is det.

    % Test to see if the procedure is a model_det function whose function
    % result has an output mode (whose type is not a dummy argument type
    % like io.state), and if so, bind RetVar to the procedure's return value.
    % These procedures need to handled specially: for such functions,
    % we map the Mercury function result to an MLDS return value.
    %
:- pred ml_is_output_det_function(module_info::in, pred_proc_id::in,
    prog_var::out) is semidet.
:- pred proc_is_output_det_function(module_info::in,
    pred_info::in, proc_info::in, prog_var::out) is semidet.

%---------------------------------------------------------------------------%
%
% Routines for generating expressions.
%

    % conjunction: ml_gen_and(X,Y) = binop((and), X, Y),
    % except that it does some constant folding on the result.
    %
:- func ml_gen_and(mlds_rval, mlds_rval) = mlds_rval.

    % negation: ml_gen_not(X) = unop(std_unop(not), X),
:- func ml_gen_not(mlds_rval) = mlds_rval.

:- func ml_int_tag_to_rval_const(int_tag, mer_type, mlds_type) = mlds_rval.

%---------------------------------------------------------------------------%
%
% Routines for generating types.
%

    % Convert a Mercury type to an MLDS type.
    %
:- pred ml_gen_type(ml_gen_info::in, mer_type::in, mlds_type::out) is det.

    % Convert the element type for an array_index operator to an MLDS type.
    %
:- func ml_gen_array_elem_type(array_elem_type) = mlds_type.

    % Return the MLDS type corresponding to a Mercury string type.
    %
:- func ml_string_type = mlds_type.

    % Return the MLDS type corresponding to a Mercury int type.
    %
:- func ml_int_type = mlds_type.

    % Return the MLDS type corresponding to a Mercury char type.
    %
:- func ml_char_type = mlds_type.

    % Allocate one or several fresh type variables, with kind `star',
    % to use as the Mercury types of boxed objects (e.g. to get the
    % argument types for tuple constructors or closure constructors).
    % Note that this should only be used in cases where the tvarset
    % doesn't matter.
    %
:- func ml_make_boxed_type = mer_type.
:- func ml_make_boxed_types(arity) = list(mer_type).

    % Return the MLDS type corresponding to the `jmercury.runtime.MercuryType'
    % interface.
    %
:- func ml_java_mercury_type_interface = mlds_type.

    % Return the MLDS type corresponding to the `jmercury.runtime.MercuryEnum'
    % class.
    %
:- func ml_java_mercury_enum_class = mlds_type.

%---------------------------------------------------------------------------%
%
% Routines for generating labels and entity names.
%

    % Generate the mlds_function_name and module name for the entry point
    % function corresponding to a given procedure.
    %
:- pred ml_gen_proc_label(module_info::in, pred_proc_id::in,
    mlds_module_name::out, mlds_plain_func_name::out) is det.

    % Generate an mlds_function_name for a continuation function with the
    % given sequence number. The pred_id and proc_id specify the procedure
    % that this continuation function is part of.
    %
:- func ml_gen_nondet_label(module_info, pred_proc_id,
    mlds_maybe_aux_func_id) = mlds_plain_func_name.

    % Allocate a new function label and return an rval containing the
    % function's address. If parameters are not given, we assume it is
    % a continuation function, and give it the appropriate arguments
    % (depending on whether we are doing nested functions or not).
    %
:- pred ml_gen_new_func_label(maybe(mlds_func_params)::in,
    mlds_maybe_aux_func_id::out, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate the mlds_pred_label and module name for a given procedure.
    %
:- pred ml_gen_pred_label(module_info::in, pred_proc_id::in,
    mlds_pred_label::out, mlds_module_name::out) is det.

:- pred ml_gen_pred_label_from_rtti(module_info::in, rtti_proc_label::in,
    mlds_pred_label::out, mlds_module_name::out) is det.

    % Allocate a new label name, for use in label statements.
    %
:- pred ml_gen_new_label(mlds_label::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% Routines for dealing with variables.
%

    % Generate a list of the mlds_lvals corresponding to a given list
    % of prog_vars.
    %
:- pred ml_gen_var_list(ml_gen_info::in, list(prog_var)::in,
    list(mlds_lval)::out) is det.

    % Generate the mlds_lval corresponding to a given prog_var.
    %
:- pred ml_gen_var(ml_gen_info::in, prog_var::in, mlds_lval::out) is det.

    % Generate the mlds_lval corresponding to a given prog_var,
    % with a given type.
    %
:- pred ml_gen_var_with_type(ml_gen_info::in, prog_var::in, mer_type::in,
    mlds_lval::out) is det.

    % Lookup the types of a list of variables.
    %
:- pred ml_variable_types(ml_gen_info::in, list(prog_var)::in,
    list(mer_type)::out) is det.

    % Lookup the type of a variable.
    %
:- pred ml_variable_type(ml_gen_info::in, prog_var::in, mer_type::out) is det.

    % Generate the MLDS variable names for a list of variables.
    %
:- func ml_gen_local_var_names(prog_varset::in, list(prog_var)::in)
    = (list(mlds_local_var_name)::out(list_skel(lvn_prog_var))) is det.

    % Generate the MLDS variable name for a variable.
    %
:- func ml_gen_local_var_name(prog_varset::in, prog_var::in)
    = (mlds_local_var_name::out(lvn_prog_var)) is det.

    % Generate a declaration for an MLDS variable, given its HLDS type.
    %
:- pred ml_gen_local_var_decl(mlds_local_var_name::in, mer_type::in,
    prog_context::in, mlds_local_var_defn::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate a declaration for an MLDS variable, given its MLDS type
    % and the code to trace it for accurate GC (if needed).
    %
:- func ml_gen_mlds_var_decl(mlds_local_var_name, mlds_type,
    mlds_gc_statement, prog_context) = mlds_local_var_defn.

    % Generate a declaration for an MLDS variable, given its MLDS type
    % and initializer, and given the code to trace it for accurate GC
    % (if needed).
    %
:- func ml_gen_mlds_var_decl_init(mlds_local_var_name, mlds_type,
    mlds_initializer, mlds_gc_statement, prog_context) = mlds_local_var_defn.

    % Return the declaration flags appropriate for a public field
    % in the derived constructor class of a discriminated union.
    %
:- func ml_gen_public_field_decl_flags = mlds_field_var_decl_flags.

%---------------------------------------------------------------------------%
%
% Routines for dealing with fields.
%

    % Given the user-specified field name, if any, and the argument number
    % (starting from one), generate an MLDS field name for the target language
    % type that represents the function symbol's cell when we are generating
    % code with --high-level-data.
    %
:- func ml_gen_hld_field_name(maybe(ctor_field_name), int) =
    mlds_field_var_name.

    % Succeed iff the specified type must be boxed when used as a field.
    % XXX Currently we box such types even for the other MLDS based back-ends
    % that don't need it, e.g. the .NET back-end.
    %
:- pred ml_must_box_field_type(module_info::in, mer_type::in, arg_width::in)
    is semidet.

:- pred ml_gen_box_const_rval(module_info::in, prog_context::in,
    mlds_type::in, bool::in, mlds_rval::in, mlds_rval::out,
    ml_global_data::in, ml_global_data::out) is det.

    % Given a source type and a destination type, and given an source rval
    % holding a value of the source type, produce an rval that converts
    % the source rval to the destination type.
    %
:- pred ml_gen_box_or_unbox_rval(module_info::in, mer_type::in, mer_type::in,
    box_policy::in, mlds_rval::in, mlds_rval::out) is det.

    % ml_gen_box_or_unbox_lval(CallerType, CalleeType, VarLval, VarName,
    %   Context, ForClosureWrapper, ArgNum,
    %   ArgLval, ConvDecls, ConvInputStmts, ConvOutputStmts):
    %
    % This is like `ml_gen_box_or_unbox_rval', except that it works on lvals
    % rather than rvals. Given a source type and a destination type,
    % a source lval holding a value of the source type, and a name to base
    % the name of the local temporary variable on, this procedure produces
    % an lval of the destination type, the declaration for the local temporary
    % used (if any), code to assign from the source lval (suitable converted)
    % to the destination lval, and code to assign from the destination lval
    % (suitable converted) to the source lval.
    %
    % If ForClosureWrapper = yes, then the type_info for type variables
    % in CallerType may not be available in the current procedure, so the GC
    % tracing code for the ConvDecls (if any) should obtain the type_info
    % from the ArgNum-th entry in the `type_params' local.
    % (If ForClosureWrapper = no, then ArgNum is unused.)
    %
:- pred ml_gen_box_or_unbox_lval(mer_type::in, mer_type::in, box_policy::in,
    mlds_lval::in, mlds_local_var_name::in, prog_context::in, bool::in,
    int::in, mlds_lval::out, list(mlds_local_var_defn)::out,
    list(mlds_stmt)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_local_for_output_arg(VarName, Type, ArgNum, Context,
    %   LocalVarDefn):
    %
    % Generate a declaration for a local variable with the specified
    % VarName and Type. However, don't use the normal GC tracing code;
    % instead, generate GC tracing code that gets the typeinfo from
    % the ArgNum-th entry in `type_params'.
    %
:- pred ml_gen_local_for_output_arg(mlds_local_var_name::in, mer_type::in,
    int::in, prog_context::in, mlds_local_var_defn::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% Routines for handling success and failure.
%

    % Generate code to succeed in the given code_model.
    %
:- pred ml_gen_success(code_model::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate code to fail in the given code_model.
    %
:- pred ml_gen_failure(code_model::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate the declaration for the built-in `succeeded' flag.
    % (`succeeded' is a boolean variable used to record
    % the success or failure of model_semi procedures.)
    %
:- func ml_gen_succeeded_var_decl(prog_context) = mlds_local_var_defn.

    % Return the lval for the `succeeded' flag.
    % (`succeeded' is a boolean variable used to record
    % the success or failure of model_semi procedures.)
    %
:- pred ml_success_lval(mlds_lval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Return an rval which will test the value of the `succeeded' flag.
    % (`succeeded' is a boolean variable used to record
    % the success or failure of model_semi procedures.)
    %
:- pred ml_gen_test_success(mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate code to set the `succeeded' flag to the specified truth value.
    %
:- pred ml_gen_set_success(mlds_rval::in, prog_context::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate the declaration for the specified `cond' variable.
    % (`cond' variables are boolean variables used to record
    % the success or failure of model_non conditions of if-then-elses.)
    %
:- func ml_gen_cond_var_decl(cond_seq, prog_context) = mlds_local_var_defn.

    % Return the lval for the specified `cond' flag.
    % (`cond' variables are boolean variables used to record
    % the success or failure of model_non conditions of if-then-elses.)
    %
:- pred ml_cond_var_lval(cond_seq::in, mlds_lval::out) is det.

    % Return an rval which will test the value of the specified `cond'
    % variable. (`cond' variables are boolean variables used to record
    % the success or failure of model_non conditions of if-then-elses.)
    %
:- pred ml_gen_test_cond_var(cond_seq::in, mlds_rval::out) is det.

    % Generate code to set the specified `cond' variable to the
    % specified truth value.
    %
:- pred ml_gen_set_cond_var(cond_seq::in, mlds_rval::in, prog_context::in,
    mlds_stmt::out) is det.

    % Return the success continuation that was passed as the current function's
    % argument(s). The success continuation consists of two parts, the `cont'
    % argument, and the `cont_env' argument. The `cont' argument is a
    % continuation function that will be called when a model_non goal succeeds.
    % The `cont_env' argument is a pointer to the environment (set of local
    % variables in the containing procedure) for the continuation function.
    % (If we are using gcc nested function, the `cont_env' is not used.)
    % The output variable lvals and types need to be supplied when generating
    % a continuation using --nondet-copy-out, otherwise they should be empty.
    %
:- pred ml_initial_cont(ml_gen_info::in, list(mlds_lval)::in,
    list(mer_type)::in, success_cont::out) is det.

    % Generate code to call the current success continuation.
    % This is used for generating success when in a model_non context.
    %
:- pred ml_gen_call_current_success_cont(prog_context::in,
    mlds_stmt::out, ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% Routines for dealing with the environment pointer used for nested functions.
%

    % Return an rval for a pointer to the current environment (the set of local
    % variables in the containing procedure). Note that we generate this
    % as a dangling reference. The ml_elim_nested pass will insert the
    % declaration of the env_ptr variable. At this point, the type of these
    % rvals is `mlds_unknown_type'.
    %
:- pred ml_get_env_ptr(mlds_rval::out) is det.

    % Return an mlds_argument for a pointer to the current environment
    % (the set of local variables in the containing procedure).
    %
:- pred ml_declare_env_ptr_arg(mlds_argument::out) is det.

%---------------------------------------------------------------------------%
%
% Magic numbers relating to the representation of
% typeclass_infos, base_typeclass_infos, and closures.
%

    % This function returns the offset to add to the argument number
    % of a closure arg to get its field number.
    %
:- func ml_closure_arg_offset = int.

    % This function returns the offset to add to the argument number
    % of a typeclass_info arg to get its field number.
    %
:- func ml_typeclass_info_arg_offset = int.

    % This function returns the offset to add to the method number for a type
    % class method to get its field number within the base_typeclass_info.
    %
:- func ml_base_typeclass_info_method_offset = int.

%---------------------------------------------------------------------------%
%
% Routines for dealing with lookup tables.
%

:- pred ml_generate_constants_for_arms(list(prog_var)::in, list(hlds_goal)::in,
    list(list(mlds_rval))::out, ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_constants_for_arm(list(prog_var)::in, hlds_goal::in,
    list(mlds_rval)::out, ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_field_assign(mlds_lval::in, mlds_type::in,
    mlds_field_id::in, mlds_vector_common::in, mlds_type::in,
    mlds_rval::in, prog_context::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_field_assigns(list(prog_var)::in, list(mlds_type)::in,
    list(mlds_field_id)::in, mlds_vector_common::in, mlds_type::in,
    mlds_rval::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% Miscellaneous routines.
%

    % Add the qualifier `builtin' to any unqualified name.
    % Although the builtin types `int', `float', etc. are treated as part
    % of the `builtin' module, for historical reasons they don't have
    % any qualifiers in the HLDS, so we need to add the `builtin'
    % qualifier before converting such names to MLDS.
    %
:- func fixup_builtin_module(module_name) = module_name.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.foreign.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_accurate_gc.
:- import_module ml_backend.ml_code_gen.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Code for various utility routines.
%

ml_gen_assign(Lval, Rval, Context) = Stmt :-
    Assign = assign(Lval, Rval),
    Stmt = ml_stmt_atomic(Assign, Context).

ml_gen_block(LocalVarDefns, FuncDefns, Stmts, Context) = Block :-
    ( if
        LocalVarDefns = [],
        FuncDefns = [],
        Stmts = [SingleStmt]
    then
        Block = SingleStmt
    else
        Block = ml_stmt_block(LocalVarDefns, FuncDefns, Stmts, Context)
    ).

ml_combine_conj(FirstCodeModel, Context, DoGenFirst, DoGenRest,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
    (
        % model_det goal:
        %     <First, Rest>
        % ===>
        %     <do First>
        %     <Rest>
        %
        FirstCodeModel = model_det,
        DoGenFirst(FirstLocalVarDefns, FirstFuncDefns, FirstStmts, !Info),
        DoGenRest(RestLocalVarDefns, RestFuncDefns, RestStmts, !Info),
        LocalVarDefns = FirstLocalVarDefns ++ RestLocalVarDefns,
        FuncDefns = FirstFuncDefns ++ RestFuncDefns,
        Stmts = FirstStmts ++ RestStmts
    ;
        % model_semi goal:
        %     <Goal, Goals>
        % ===>
        %     MR_bool succeeded;
        %
        %     <succeeded = Goal>;
        %     if (succeeded) {
        %         <Goals>;
        %     }
        % except that we hoist any declarations generated for <Goals>
        % to the outer scope, rather than inside the `if', so that they remain
        % in scope for any later goals which follow this (this is needed for
        % declarations of static consts).
        % XXX We haven't put static consts into blocks for a long time;
        % they are now in the global data field in !Info.

        FirstCodeModel = model_semi,
        DoGenFirst(FirstLocalVarDefns, FirstFuncDefns, FirstStmts, !Info),
        ml_gen_test_success(Succeeded, !Info),
        DoGenRest(RestLocalVarDefns, RestFuncDefns, RestStmts, !Info),
        ThenStmt = ml_gen_block([], [], RestStmts, Context),
        ITEStmt = ml_stmt_if_then_else(Succeeded, ThenStmt, no, Context),
        LocalVarDefns = FirstLocalVarDefns ++ RestLocalVarDefns,
        FuncDefns = FirstFuncDefns ++ RestFuncDefns,
        Stmts = FirstStmts ++ [ITEStmt]
    ;
        % model_non goal:
        %     <First, Rest>
        % ===>
        %     succ_func() {
        %         <Rest && SUCCEED()>;
        %     }
        %
        %     <First && succ_func()>;
        %
        % except that we hoist any declarations generated for <First>
        % to the top of the scope, rather than inside or after the
        % succ_func(), so that they remain in scope for any code
        % following them.
        %
        % XXX The pattern above leads to deep nesting for long conjunctions;
        % we should avoid that.
        %

        FirstCodeModel = model_non,

        % allocate a name for the `succ_func'
        ml_gen_new_func_label(no, RestFuncLabel, RestFuncLabelRval, !Info),

        % generate <First && succ_func()>
        ml_get_env_ptr(EnvPtrRval),
        SuccessCont = success_cont(RestFuncLabelRval, EnvPtrRval, [], []),
        ml_gen_info_push_success_cont(SuccessCont, !Info),
        DoGenFirst(FirstLocalVarDefns, FirstFuncDefns, FirstStmts, !Info),
        ml_gen_info_pop_success_cont(!Info),

        % generate the `succ_func'
        % push nesting level
        DoGenRest(RestLocalVarDefns, RestFuncDefns, RestStmts, !Info),
        RestStmt = ml_gen_block(RestLocalVarDefns, RestFuncDefns, RestStmts,
            Context),
        % pop nesting level
        ml_gen_nondet_label_func(!.Info, RestFuncLabel, Context,
            RestStmt, RestFunc),

        LocalVarDefns = FirstLocalVarDefns,
        FuncDefns = FirstFuncDefns ++ [RestFunc],
        Stmts = FirstStmts
    ).

ml_gen_nondet_label_func(Info, MaybeAux, Context, Stmt, Func) :-
    ml_declare_env_ptr_arg(EnvPtrArg),
    FuncParams = mlds_func_params([EnvPtrArg], []),
    ml_gen_label_func(Info, MaybeAux, FuncParams, Context, Stmt, Func).

ml_gen_label_func(Info, MaybeAux, FuncParams, Context, Stmt, Func) :-
    % Compute the function name.
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_pred_proc_id(Info, PredProcId),
    FuncName = ml_gen_nondet_label(ModuleInfo, PredProcId, MaybeAux),

    % Compute the function definition.
    DeclFlags = ml_gen_label_func_decl_flags,
    MaybePredProcId = no,
    Body = body_defined_here(Stmt),
    Attributes = [],
    EnvVarNames = set.init,
    Func = mlds_function_defn(mlds_function_name(FuncName), Context,
        DeclFlags, MaybePredProcId, FuncParams, Body, Attributes,
        EnvVarNames, no).

    % Return the declaration flags appropriate for a label func (a label func
    % is a function used as a continuation when generating nondet code).
    %
:- func ml_gen_label_func_decl_flags = mlds_function_decl_flags.

ml_gen_label_func_decl_flags = DeclFlags :-
    Access = acc_local,
    PerInstance = per_instance,
    DeclFlags = init_function_decl_flags(Access, PerInstance).

ml_is_output_det_function(ModuleInfo, PredProcId, RetArgVar) :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    proc_is_output_det_function(ModuleInfo, PredInfo, ProcInfo, RetArgVar).

proc_is_output_det_function(ModuleInfo, PredInfo, ProcInfo, RetArgVar) :-
    pred_info_is_pred_or_func(PredInfo) = pf_function,
    proc_info_interface_code_model(ProcInfo) = model_det,

    proc_info_get_argmodes(ProcInfo, Modes),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    proc_info_get_headvars(ProcInfo, ArgVars),
    modes_to_top_functor_modes(ModuleInfo, Modes, ArgTypes, TopFunctorModes),
    pred_args_to_func_args(TopFunctorModes,
        _InputTopFunctorModes, RetTopFunctorMode),
    pred_args_to_func_args(ArgTypes, _InputArgTypes, RetArgType),
    pred_args_to_func_args(ArgVars, _InputArgVars, RetArgVar),

    RetTopFunctorMode = top_out,
    check_dummy_type(ModuleInfo, RetArgType) = is_not_dummy_type.

%---------------------------------------------------------------------------%
%
% Code for generating expressions.
%

ml_gen_and(X, Y) =
    ( if X = ml_const(mlconst_true) then
        Y
    else if Y = ml_const(mlconst_true) then
        X
    else
        ml_binop(logical_and, X, Y)
    ).

ml_gen_not(X) = ml_unop(std_unop(logical_not), X).

ml_int_tag_to_rval_const(IntTag, MerType, MLDS_Type) = Rval :-
    (
        IntTag = int_tag_int(Int),
        ( if MerType = int_type then
            Rval = ml_const(mlconst_int(Int))
        else if MerType = char_type then
            Rval = ml_const(mlconst_char(Int))
        else
            Rval = ml_const(mlconst_enum(Int, MLDS_Type))
        )
    ;
        IntTag = int_tag_uint(UInt),
        Rval = ml_const(mlconst_uint(UInt))
    ;
        IntTag = int_tag_int8(Int8),
        Rval = ml_const(mlconst_int8(Int8))
    ;
        IntTag = int_tag_uint8(UInt8),
        Rval = ml_const(mlconst_uint8(UInt8))
    ;
        IntTag = int_tag_int16(Int16),
        Rval = ml_const(mlconst_int16(Int16))
    ;
        IntTag = int_tag_uint16(UInt16),
        Rval = ml_const(mlconst_uint16(UInt16))
    ;
        IntTag = int_tag_int32(Int32),
        Rval = ml_const(mlconst_int32(Int32))
    ;
        IntTag = int_tag_uint32(UInt32),
        Rval = ml_const(mlconst_uint32(UInt32))
    ).

%---------------------------------------------------------------------------%
%
% Code for generating types.
%

ml_gen_type(Info, Type, MLDS_Type) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type).

ml_gen_array_elem_type(ElemType) = MLDS_Type :-
    (
        ElemType = array_elem_scalar(ScalarElem),
        MLDS_Type = ml_gen_scalar_array_elem_type(ScalarElem)
    ;
        ElemType = array_elem_struct(_ScalarElems),
        unexpected($pred, "struct")
    ).

:- func ml_gen_scalar_array_elem_type(scalar_array_elem_type) = mlds_type.

ml_gen_scalar_array_elem_type(scalar_elem_string) = ml_string_type.
ml_gen_scalar_array_elem_type(scalar_elem_int) = mlds_native_int_type.
ml_gen_scalar_array_elem_type(scalar_elem_generic) = mlds_generic_type.

ml_string_type =
    mercury_type(string_type, ctor_cat_builtin(cat_builtin_string),
        non_foreign_type(string_type)).

ml_int_type =
    mercury_type(int_type, ctor_cat_builtin(cat_builtin_int(int_type_int)),
        non_foreign_type(int_type)).

ml_char_type =
    mercury_type(char_type, ctor_cat_builtin(cat_builtin_char),
        non_foreign_type(char_type)).

ml_make_boxed_type = BoxedType :-
    varset.init(TypeVarSet0),
    varset.new_var(BoxedTypeVar, TypeVarSet0, _TypeVarSet),
    prog_type.var_to_type(map.init, BoxedTypeVar, BoxedType).

ml_make_boxed_types(Arity) = BoxedTypes :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, BoxedTypeVars, TypeVarSet0, _TypeVarSet),
    prog_type.var_list_to_type_list(map.init, BoxedTypeVars, BoxedTypes).

ml_java_mercury_type_interface = TypeInterfaceDefn :-
    InterfaceModuleName =
        mercury_module_name_to_mlds(java_mercury_runtime_package_name),
    TypeInterface =
        qual_class_name(InterfaceModuleName, module_qual, "MercuryType"),
    TypeInterfaceDefn = mlds_class_type(TypeInterface, 0, mlds_interface).

ml_java_mercury_enum_class = EnumClassDefn :-
    InterfaceModuleName =
        mercury_module_name_to_mlds(java_mercury_runtime_package_name),
    EnumClass =
        qual_class_name(InterfaceModuleName, module_qual, "MercuryEnum"),
    EnumClassDefn = mlds_class_type(EnumClass, 0, mlds_class).

%---------------------------------------------------------------------------%
%
% Code for generating mlds_function_names.
%

ml_gen_proc_label(ModuleInfo, PredProcId, MLDS_ModuleName, MLDS_Name) :-
    ml_gen_func_label(ModuleInfo, PredProcId, proc_func, MLDS_ModuleName,
        MLDS_Name).

ml_gen_nondet_label(ModuleInfo, PredProcId, MaybeAux) = MLDS_Name :-
    ml_gen_func_label(ModuleInfo, PredProcId, MaybeAux,
        _MLDS_ModuleName, MLDS_Name).

:- pred ml_gen_func_label(module_info::in, pred_proc_id::in,
    mlds_maybe_aux_func_id::in,
    mlds_module_name::out, mlds_plain_func_name::out) is det.

ml_gen_func_label(ModuleInfo, PredProcId, MaybeAux, ModuleName, FuncName) :-
    ml_gen_pred_label(ModuleInfo, PredProcId, PredLabel, ModuleName),
    PredProcId = proc(PredId, ProcId),
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    FuncName = mlds_plain_func_name(FuncLabel, PredId).

ml_gen_new_func_label(MaybeParams, MaybeAux, FuncLabelRval, !Info) :-
    ml_gen_info_new_aux_func_id(MaybeAux, !Info),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_info_get_pred_proc_id(!.Info, PredProcId),
    ml_gen_pred_label(ModuleInfo, PredProcId, PredLabel, PredModule),
    (
        MaybeParams = yes(Params),
        Signature = mlds_get_func_signature(Params)
    ;
        MaybeParams = no,
        ArgTypes = [mlds_generic_env_ptr_type],
        Signature = mlds_func_signature(ArgTypes, [])
    ),
    PredProcId = proc(_PredId, ProcId),
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    QualFuncLabel = qual_func_label(PredModule, FuncLabel),
    FuncLabelRval = ml_const(mlconst_code_addr(
        mlds_code_addr(QualFuncLabel, Signature))).

ml_gen_pred_label(ModuleInfo, PredProcId, MLDS_PredLabel, MLDS_Module) :-
    PredProcId = proc(PredId, ProcId),
    RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
    ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcLabel,
        MLDS_PredLabel, MLDS_Module).

ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcLabel, MLDS_PredLabel,
        MLDS_Module) :-
    RttiProcLabel = rtti_proc_label(PredOrFunc, ThisModule, PredModule,
        PredName, PredArity, _ArgTypes, PredId, ProcId,
        _HeadVarsWithNames, _TopFunctorModes, Detism,
        PredIsImported, _PredIsPseudoImported,
        Origin, _ProcIsExported, _ProcIsImported),
    ( if Origin = origin_special_pred(SpecialPred, TypeCtor) then
        ( if
            % All type_ctors other than tuples here should be module qualified,
            % since builtin types are handled separately in polymorphism.m.
            TypeCtor = type_ctor(TypeCtorSymName, TypeArity),
            (
                TypeCtorSymName = unqualified(TypeName),
                type_ctor_is_tuple(TypeCtor),
                TypeModule = mercury_public_builtin_module
            ;
                TypeCtorSymName = qualified(TypeModule, TypeName)
            )
        then
            ( if
                ThisModule \= TypeModule,
                SpecialPred = spec_pred_unify,
                not hlds_pred.in_in_unification_proc_id(ProcId)
            then
                % This is a locally-defined instance of a unification procedure
                % for a type defined in some other module.
                DefiningModule = ThisModule,
                MaybeDeclaringModule = yes(TypeModule)
            else
                % The module declaring the type is the same as the module
                % defining this special pred.
                DefiningModule = TypeModule,
                MaybeDeclaringModule = no
            ),
            MLDS_PredLabel = mlds_special_pred_label(PredName,
                MaybeDeclaringModule, TypeName, TypeArity)
        else
            unexpected($pred,
                "cannot make label for special pred `" ++ PredName ++ "'")
        )
    else
        ( if
            % Work out which module supplies the code for the predicate.
            ThisModule \= PredModule,
            PredIsImported = no
        then
            % This predicate is a specialized version of a pred from a
            % `.opt' file.
            DefiningModule = ThisModule,
            MaybeDeclaringModule = yes(PredModule)
        else
            % The predicate was declared in the same module that it is
            % defined in
            DefiningModule = PredModule,
            MaybeDeclaringModule = no
        ),
        ( if
            PredOrFunc = pf_function,
            not ml_is_output_det_function(ModuleInfo, proc(PredId, ProcId), _)
        then
            NonOutputFunc = yes
        else
            NonOutputFunc = no
        ),
        determinism_to_code_model(Detism, CodeModel),
        MLDS_PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDeclaringModule,
            PredName, PredArity, CodeModel, NonOutputFunc)
    ),
    MLDS_Module = mercury_module_name_to_mlds(DefiningModule).

ml_gen_new_label(Label, !Info) :-
    ml_gen_info_new_label(LabelNum, !Info),
    Label = "label_" ++ string.int_to_string(LabelNum).

%---------------------------------------------------------------------------%
%
% Code for dealing with variables.
%

ml_gen_var_list(_Info, [], []).
ml_gen_var_list(Info, [Var | Vars], [Lval | Lvals]) :-
    ml_gen_var(Info, Var, Lval),
    ml_gen_var_list(Info, Vars, Lvals).

ml_gen_var(Info, Var, Lval) :-
    % First check the var_lvals override mapping; if an lval has been set
    % for this variable, use it.

    ml_gen_info_get_var_lvals(Info, VarLvals),
    ( if map.search(VarLvals, Var, VarLval) then
        Lval = VarLval
    else
        % Otherwise just look up the variable's type and generate an lval
        % for it using the ordinary algorithm.

        ml_variable_type(Info, Var, Type),
        ml_gen_var_with_type(Info, Var, Type, Lval)
    ).

ml_gen_var_with_type(Info, Var, Type, Lval) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    IsDummy = check_dummy_type(ModuleInfo, Type),
    (
        IsDummy = is_dummy_type,
        % The variable won't have been declared, so we need to generate
        % a dummy lval for this variable.
        ml_gen_type(Info, Type, MLDS_Type),
        Lval = ml_global_var(global_dummy_var, MLDS_Type)
    ;
        IsDummy = is_not_dummy_type,
        ml_gen_info_get_varset(Info, VarSet),
        VarName = ml_gen_local_var_name(VarSet, Var),
        ml_gen_type(Info, Type, MLDS_Type),
        VarLval = ml_local_var(VarName, MLDS_Type),

        % Output variables may be passed by reference...
        ml_gen_info_get_byref_output_vars(Info, ByRefOutputVars),
        ( if list.member(Var, ByRefOutputVars) then
            Lval = ml_mem_ref(ml_lval(VarLval), MLDS_Type)
        else
            Lval = VarLval
        )
    ).

ml_variable_types(_Info, [], []).
ml_variable_types(Info, [Var | Vars], [Type | Types]) :-
    ml_variable_type(Info, Var, Type),
    ml_variable_types(Info, Vars, Types).

ml_variable_type(Info, Var, Type) :-
    ml_gen_info_get_var_types(Info, VarTypes),
    lookup_var_type(VarTypes, Var, Type).

ml_gen_local_var_names(_VarSet, []) = [].
ml_gen_local_var_names(VarSet, [Var | Vars]) = [MLDSVarName | MLDSVarNames] :-
    MLDSVarName = ml_gen_local_var_name(VarSet, Var),
    MLDSVarNames = ml_gen_local_var_names(VarSet, Vars).

ml_gen_local_var_name(VarSet, Var) = MLDSVarName :-
    term.var_to_int(Var, VarNumber),
    ( if varset.search_name(VarSet, Var, VarName) then
        MLDSVarName = lvn_prog_var(VarName, VarNumber)
    else
        MLDSVarName = lvn_prog_var("", VarNumber)
    ).

ml_gen_local_var_decl(VarName, Type, Context, Defn, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_gc_statement(VarName, Type, Context, GCStmt, !Info),
    Defn = ml_gen_mlds_var_decl(VarName,
        mercury_type_to_mlds_type(ModuleInfo, Type), GCStmt, Context).

ml_gen_mlds_var_decl(DataName, MLDS_Type, GCStmt, Context) =
    ml_gen_mlds_var_decl_init(DataName, MLDS_Type, no_initializer, GCStmt,
        Context).

ml_gen_mlds_var_decl_init(DataName, MLDS_Type, Initializer, GCStmt, Context) =
    mlds_local_var_defn(DataName, Context, MLDS_Type, Initializer, GCStmt).

ml_gen_public_field_decl_flags =
    mlds_field_var_decl_flags(per_instance, modifiable).

%---------------------------------------------------------------------------%
%
% Code for dealing with fields.
%

ml_gen_hld_field_name(MaybeFieldName, ArgNum) = FieldVarName :-
    % If the programmer specified a field name, we use that,
    % otherwise we just use `F' followed by the field number.
    (
        MaybeFieldName = yes(ctor_field_name(QualifiedFieldName,
            _FieldNameCtxt)),
        FieldName = unqualify_name(QualifiedFieldName)
    ;
        MaybeFieldName = no,
        FieldName = "F" ++ string.int_to_string(ArgNum)
    ),
    FieldVarName = fvn_du_ctor_field_hld(FieldName).

ml_must_box_field_type(ModuleInfo, Type, Width) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
    (
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_erlang
        ),
        classify_type(ModuleInfo, Type) = Category,
        MustBox = ml_must_box_field_type_category(Category, UnboxedFloat,
            Width)
    ;
        Target = target_java,
        MustBox = no
    ),
    MustBox = yes.

:- func ml_must_box_field_type_category(type_ctor_category, bool, arg_width)
    = bool.

ml_must_box_field_type_category(CtorCat, UnboxedFloat, Width) = MustBox :-
    (
        ( CtorCat = ctor_cat_builtin(cat_builtin_int(_))
        ; CtorCat = ctor_cat_builtin(cat_builtin_string)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_user(_)
        ),
        MustBox = no
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        MustBox = yes
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        (
            UnboxedFloat = yes,
            MustBox = no
        ;
            UnboxedFloat = no,
            (
                Width = full_word,
                MustBox = yes
            ;
                Width = double_word,
                MustBox = no
            ;
                ( Width = partial_word_first(_)
                ; Width = partial_word_shifted(_, _)
                ),
                unexpected($pred, "partial word for float")
            )
        )
    ).

ml_gen_box_const_rval(ModuleInfo, Context, MLDS_Type, DoubleWidth, Rval,
        BoxedRval, !GlobalData) :-
    ( if
        ( MLDS_Type = mercury_type(type_variable(_, _), _, _)
        ; MLDS_Type = mlds_generic_type
        )
    then
        BoxedRval = Rval
    else if
        % For the MLDS->C back-end, we need to handle constant floats
        % specially. Boxed floats normally get heap allocated, whereas for
        % other types boxing is just a cast (casts are OK in static
        % initializers, but calls to malloc() are not).
        ( MLDS_Type = mercury_type(builtin_type(builtin_type_float), _, _)
        ; MLDS_Type = mlds_native_float_type
        ),
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        Target = target_c
    then
        HaveUnboxedFloats = ml_global_data_have_unboxed_floats(!.GlobalData),
        ( if
            HaveUnboxedFloats = do_not_have_unboxed_floats,
            DoubleWidth = no
        then
            % Generate a local static constant for this float.
            module_info_get_name(ModuleInfo, ModuleName),
            MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
            Initializer = init_obj(Rval),
            ml_gen_static_scalar_const_addr(MLDS_ModuleName, mgcv_float,
                MLDS_Type, Initializer, Context, ConstAddrRval, !GlobalData),

            % Return as the boxed rval the address of that constant,
            % cast to mlds_generic_type.
            BoxedRval = ml_unop(cast(mlds_generic_type), ConstAddrRval)
        else
            % This is not a real box, but a cast. The "box" is required as it
            % may be further cast to pointer types.
            BoxedRval = ml_unop(box(MLDS_Type), Rval)
        )
    else
        BoxedRval = ml_unop(box(MLDS_Type), Rval)
    ).

ml_gen_box_or_unbox_rval(ModuleInfo, SourceType, DestType, BoxPolicy, VarRval,
        ArgRval) :-
    % Convert VarRval, of type SourceType, to ArgRval, of type DestType.
    (
        BoxPolicy = bp_always_boxed,
        ArgRval = VarRval
    ;
        BoxPolicy = bp_native_if_possible,
        ( if
            % If converting from polymorphic type to concrete type, then unbox.
            SourceType = type_variable(_, _),
            DestType \= type_variable(_, _)
        then
            MLDS_DestType = mercury_type_to_mlds_type(ModuleInfo, DestType),
            ArgRval = ml_unop(unbox(MLDS_DestType), VarRval)
        else if
            % If converting from concrete type to polymorphic type, then box.
            SourceType \= type_variable(_, _),
            DestType = type_variable(_, _)
        then
            MLDS_SourceType =
                mercury_type_to_mlds_type(ModuleInfo, SourceType),
            ArgRval = ml_unop(box(MLDS_SourceType), VarRval)
        else if
            % If converting to float, cast to mlds_generic_type and then unbox.
            DestType = builtin_type(builtin_type_float),
            SourceType \= builtin_type(builtin_type_float)
        then
            MLDS_DestType = mercury_type_to_mlds_type(ModuleInfo, DestType),
            ArgRval = ml_unop(unbox(MLDS_DestType),
                ml_unop(cast(mlds_generic_type), VarRval))
        else if
            % If converting from float, box and then cast the result.
            SourceType = builtin_type(builtin_type_float),
            DestType \= builtin_type(builtin_type_float)
        then
            MLDS_SourceType =
                mercury_type_to_mlds_type(ModuleInfo, SourceType),
            MLDS_DestType = mercury_type_to_mlds_type(ModuleInfo, DestType),
            ArgRval = ml_unop(cast(MLDS_DestType),
                ml_unop(box(MLDS_SourceType), VarRval))
        else if
            % If converting from an array(T) to array(X) where X is a concrete
            % instance, we should insert a cast to the concrete instance.
            % Also when converting to array(T) from array(X) we should cast
            % to array(T).
            type_to_ctor_and_args(SourceType, SourceTypeCtor, SourceTypeArgs),
            type_to_ctor_and_args(DestType, DestTypeCtor, DestTypeArgs),
            (
                type_ctor_is_array(SourceTypeCtor),
                SourceTypeArgs = [type_variable(_, _)]
            ;
                type_ctor_is_array(DestTypeCtor),
                DestTypeArgs = [type_variable(_, _)]
            ),
            % Don't insert redundant casts if the types are the same, since
            % the extra assignments introduced can inhibit tail call
            % optimisation.
            SourceType \= DestType
        then
            MLDS_DestType = mercury_type_to_mlds_type(ModuleInfo, DestType),
            ArgRval = ml_unop(cast(MLDS_DestType), VarRval)
        else if
            % If converting from one concrete type to a different one, then
            % cast. This is needed to handle construction/deconstruction
            % unifications for no_tag types.
            %
            not type_unify(SourceType, DestType, [], map.init, _)
        then
            MLDS_DestType = mercury_type_to_mlds_type(ModuleInfo, DestType),
            ArgRval = ml_unop(cast(MLDS_DestType), VarRval)
        else
            % Otherwise leave unchanged.
            ArgRval = VarRval
        )
    ).

ml_gen_box_or_unbox_lval(CallerType, CalleeType, BoxPolicy, VarLval, VarName,
        Context, ForClosureWrapper, ArgNum, ArgLval, ConvDecls,
        ConvInputStmts, ConvOutputStmts, !Info) :-
    % First see if we can just convert the lval as an rval;
    % if no boxing/unboxing is required, then ml_box_or_unbox_rval
    % will return its argument unchanged, and so we are done.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_box_or_unbox_rval(ModuleInfo, CalleeType, CallerType, BoxPolicy,
        ml_lval(VarLval), BoxedRval),
    ( if BoxedRval = ml_lval(VarLval) then
        ArgLval = VarLval,
        ConvDecls = [],
        ConvInputStmts = [],
        ConvOutputStmts = []
    else
        % If that didn't work, then we need to declare a fresh variable
        % to use as the arg, and to generate statements to box/unbox
        % that fresh arg variable and assign it to/from the output
        % argument whose address we were passed.

        % Generate a declaration for the fresh variable.
        %
        % Note that generating accurate GC tracing code for this variable
        % requires some care, because CalleeType might be a type variable
        % from the callee, not from the caller, and we can't generate
        % type_infos for type variables from the callee. Hence we need to call
        % the version of ml_gen_gc_statement which takes two types.
        % The CalleeType is used to determine the type for the temporary
        % variable declaration, but the CallerType is used to construct
        % the type_info.

        ml_gen_info_new_conv_var(ConvVarSeq, !Info),
        ConvVarSeq = conv_seq(ConvVarNum),
        ( if VarName = lvn_prog_var(ProgVarName, ProgVarNum) then
            ArgVarName =
                lvn_prog_var_conv(ConvVarNum, ProgVarName, ProgVarNum)
        else
            VarNameStr = ml_local_var_name_to_string(VarName),
            ArgVarName =
                lvn_comp_var(lvnc_non_prog_var_conv(ConvVarNum, VarNameStr))
        ),
        ml_gen_type(!.Info, CalleeType, MLDS_CalleeType),
        (
            ForClosureWrapper = yes,
            % For closure wrappers, the argument type_infos are stored in
            % the `type_params' local, so we need to handle the GC tracing
            % code specially.
            ( if CallerType = type_variable(_, _) then
                ml_gen_local_for_output_arg(ArgVarName, CalleeType, ArgNum,
                    Context, ArgVarDecl, !Info)
            else
                unexpected($pred,
                    "invalid CalleeType for closure wrapper")
            )
        ;
            ForClosureWrapper = no,
            ml_gen_gc_statement_poly(ArgVarName, CalleeType, CallerType,
                Context, GC_Stmts, !Info),
            ArgVarDecl = ml_gen_mlds_var_decl(ArgVarName, MLDS_CalleeType,
                GC_Stmts, Context)
        ),
        ConvDecls = [ArgVarDecl],

        % Create the lval for the variable and use it for the argument lval.
        ArgLval = ml_local_var(ArgVarName, MLDS_CalleeType),

        CallerIsDummy = check_dummy_type(ModuleInfo, CallerType),
        (
            CallerIsDummy = is_dummy_type,
            % If it is a dummy argument type (e.g. io.state),
            % then we don't need to bother assigning it.
            ConvInputStmts = [],
            ConvOutputStmts = []
        ;
            CallerIsDummy = is_not_dummy_type,
            % Generate statements to box/unbox the fresh variable and assign it
            % to/from the output argument whose address we were passed.

            % Assign to the freshly generated arg variable.
            ml_gen_box_or_unbox_rval(ModuleInfo, CallerType, CalleeType,
                BoxPolicy, ml_lval(VarLval), ConvertedVarRval),
            AssignInputStmt = ml_gen_assign(ArgLval, ConvertedVarRval,
                Context),
            ConvInputStmts = [AssignInputStmt],

            % Assign from the freshly generated arg variable.
            ml_gen_box_or_unbox_rval(ModuleInfo, CalleeType, CallerType,
                BoxPolicy, ml_lval(ArgLval), ConvertedArgRval),
            AssignOutputStmt = ml_gen_assign(VarLval, ConvertedArgRval,
                Context),
            ConvOutputStmts = [AssignOutputStmt]
        )
    ).

ml_gen_local_for_output_arg(VarName, Type, ArgNum, Context, LocalVarDefn,
        !Info) :-
    % Generate a declaration for a corresponding local variable.
    % However, don't use the normal GC tracing code; instead, we need to get
    % the typeinfo from `type_params', using the following code:
    %
    %   MR_TypeInfo     type_info;
    %   MR_MemoryList   allocated_memory_cells = NULL;
    %   type_info = MR_make_type_info_maybe_existq(type_params,
    %       closure_layout->MR_closure_arg_pseudo_type_info[<ArgNum> - 1],
    %           NULL, NULL, &allocated_memory_cells);
    %
    %   private_builtin__gc_trace_1_0(type_info, &<VarName>);
    %
    %   MR_deallocate(allocated_memory_cells);
    %

    ClosureLayoutPtrName = lvn_comp_var(lvnc_closure_layout_ptr),
    % This type is really `const MR_Closure_Layout *', but there is no easy
    % way to represent that in the MLDS; using MR_Box instead works fine.
    ClosureLayoutPtrType = mlds_generic_type,
    ClosureLayoutPtrLval =
        ml_local_var(ClosureLayoutPtrName, ClosureLayoutPtrType),

    TypeParamsName = lvn_comp_var(lvnc_type_params),
    % This type is really MR_TypeInfoParams, but there is no easy way to
    % represent that in the MLDS; using MR_Box instead works fine.
    TypeParamsType = mlds_generic_type,
    TypeParamsLval = ml_local_var(TypeParamsName, TypeParamsType),

    TypeInfoName = lvn_comp_var(lvnc_type_info),
    % The type for this should match the type of the first argument
    % of private_builtin.gc_trace/1, i.e. `mutvar(T)', which is a no_tag type
    % whose representation is c_pointer.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    TypeInfoMercuryType = c_pointer_type,
    TypeInfoType = mercury_type_to_mlds_type(ModuleInfo, TypeInfoMercuryType),
    TypeInfoLval = ml_local_var(TypeInfoName, TypeInfoType),
    TypeInfoDecl = ml_gen_mlds_var_decl(TypeInfoName, TypeInfoType,
        gc_no_stmt, Context),

    ml_gen_gc_statement_with_typeinfo(VarName, Type, ml_lval(TypeInfoLval),
        Context, GCStmt0, !Info),

    (
        ( GCStmt0 = gc_trace_code(CallTraceFuncCode)
        ; GCStmt0 = gc_initialiser(CallTraceFuncCode)
        ),
        MakeTypeInfoCodeC = inline_target_code(ml_target_c, [
            raw_target_code("{\n"),
            raw_target_code("MR_MemoryList allocated_mem = NULL;\n"),
            target_code_output(TypeInfoLval),
            raw_target_code(" = (MR_C_Pointer) " ++
                "MR_make_type_info_maybe_existq(\n\t"),
            target_code_input(ml_lval(TypeParamsLval)),
            raw_target_code(", ((MR_Closure_Layout *)\n\t"),
            target_code_input(ml_lval(ClosureLayoutPtrLval)),
            raw_target_code(string.format(")->" ++
                "MR_closure_arg_pseudo_type_info[%d - 1],\n\t" ++
                "NULL, NULL, &allocated_mem);\n",
                [i(ArgNum)]))
        ]),
        MakeTypeInfoCode = ml_stmt_atomic(MakeTypeInfoCodeC, Context),
        DeallocateCodeC = inline_target_code(ml_target_c, [
            raw_target_code("MR_deallocate(allocated_mem);\n"),
            raw_target_code("}\n")
        ]),
        DeallocateCode = ml_stmt_atomic(DeallocateCodeC, Context),
        % XXX MLDS_DEFN
        GCTraceCode = ml_stmt_block([TypeInfoDecl], [],
            [MakeTypeInfoCode, CallTraceFuncCode, DeallocateCode], Context),
        GCStmt = gc_trace_code(GCTraceCode)
    ;
        GCStmt0 = gc_no_stmt,
        GCStmt = GCStmt0
    ),
    LocalVarDefn = ml_gen_mlds_var_decl(VarName,
        mercury_type_to_mlds_type(ModuleInfo, Type), GCStmt, Context).

%---------------------------------------------------------------------------%
%
% Code for handling success and failure.
%

ml_gen_success(CodeModel, Context, Stmts, !Info) :-
    (
        CodeModel = model_det,
        %
        % det succeed:
        %   <do true>
        % ===>
        %   /* just fall through */
        %
        Stmts = []
    ;
        CodeModel = model_semi,
        %
        % semidet succeed:
        %   <do true>
        % ===>
        %   succeeded = MR_TRUE;
        %
        ml_gen_set_success(ml_const(mlconst_true), Context, SetSuccessTrue,
            !Info),
        Stmts = [SetSuccessTrue]
    ;
        CodeModel = model_non,
        %
        % nondet succeed:
        %   <true && SUCCEED()>
        % ===>
        %   SUCCEED()
        %
        ml_gen_call_current_success_cont(Context, CallCont, !Info),
        Stmts = [CallCont]
    ).

ml_gen_failure(CodeModel, Context, Stmts, !Info) :-
    (
        CodeModel = model_det,
        unexpected($pred, "`fail' has determinism `det'")
    ;
        CodeModel = model_semi,
        %
        % semidet fail:
        %   <do fail>
        % ===>
        %   succeeded = MR_FALSE;
        %
        ml_gen_set_success(ml_const(mlconst_false), Context, SetSuccessFalse,
            !Info),
        Stmts = [SetSuccessFalse]
    ;
        CodeModel = model_non,
        %
        % nondet fail:
        %   <fail && SUCCEED()>
        % ===>
        %   /* just fall through */
        %
        Stmts = []
    ).

%---------------------------------------------------------------------------%

ml_gen_succeeded_var_decl(Context) =
    ml_gen_mlds_var_decl(lvn_comp_var(lvnc_succeeded), mlds_native_bool_type,
        gc_no_stmt, Context).

ml_success_lval(SucceededLval, !Info) :-
    SucceededLval =
        ml_local_var(lvn_comp_var(lvnc_succeeded), mlds_native_bool_type),
    ml_gen_info_set_used_succeeded_var(yes, !Info).

ml_gen_test_success(SucceededRval, !Info) :-
    ml_success_lval(SucceededLval, !Info),
    SucceededRval = ml_lval(SucceededLval).

ml_gen_set_success(Value, Context, Stmt, !Info) :-
    ml_success_lval(Succeeded, !Info),
    Stmt = ml_gen_assign(Succeeded, Value, Context).

%---------------------------------------------------------------------------%

    % Generate the name for the specified `cond_<N>' variable.
    %
:- func ml_gen_cond_var_name(cond_seq) = mlds_local_var_name.

ml_gen_cond_var_name(CondSeq) = VarName :-
    CondSeq = cond_seq(CondNum),
    VarName = lvn_comp_var(lvnc_cond(CondNum)).

ml_gen_cond_var_decl(CondSeq, Context) =
    ml_gen_mlds_var_decl(ml_gen_cond_var_name(CondSeq), mlds_native_bool_type,
        gc_no_stmt, Context).

ml_cond_var_lval(CondSeq, CondVarLval) :-
    CondVarLval =
        ml_local_var(ml_gen_cond_var_name(CondSeq), mlds_native_bool_type).

ml_gen_test_cond_var(CondVar, CondVarRval) :-
    ml_cond_var_lval(CondVar, CondVarLval),
    CondVarRval = ml_lval(CondVarLval).

ml_gen_set_cond_var(CondVar, Value, Context, Stmt) :-
    ml_cond_var_lval(CondVar, CondVarLval),
    Stmt = ml_gen_assign(CondVarLval, Value, Context).

%---------------------------------------------------------------------------%

ml_initial_cont(Info, OutputVarLvals0, OutputVarTypes0, Cont) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_skip_dummy_argument_types(OutputVarTypes0, OutputVarLvals0,
        ModuleInfo, OutputVarTypes, OutputVarLvals),
    list.map(ml_gen_type(Info), OutputVarTypes, MLDS_OutputVarTypes),

    % We expect OutputVarlvals0 and OutputVarTypes0 to be empty if
    % `--nondet-copy-out' is not enabled.

    ContLval = ml_local_var(lvn_comp_var(lvnc_cont),
        mlds_cont_type(MLDS_OutputVarTypes)),
    ContEnvLval = ml_local_var(lvn_comp_var(lvnc_cont_env_ptr),
        mlds_generic_env_ptr_type),
    Cont = success_cont(ml_lval(ContLval), ml_lval(ContEnvLval),
        MLDS_OutputVarTypes, OutputVarLvals).

:- pred ml_skip_dummy_argument_types(list(mer_type)::in, list(T)::in,
    module_info::in, list(mer_type)::out, list(T)::out) is det.

ml_skip_dummy_argument_types([], [], _, [], []).
ml_skip_dummy_argument_types([Type | Types0], [Var | Vars0], ModuleInfo,
        Types, Vars) :-
    ml_skip_dummy_argument_types(Types0, Vars0, ModuleInfo, Types1, Vars1),
    IsDummy = check_dummy_type(ModuleInfo, Type),
    (
        IsDummy = is_dummy_type,
        Types = Types1,
        Vars = Vars1
    ;
        IsDummy = is_not_dummy_type,
        Types = [Type | Types1],
        Vars = [Var | Vars1]
    ).
ml_skip_dummy_argument_types([_ | _], [], _, _, _) :-
    unexpected($pred, "length mismatch").
ml_skip_dummy_argument_types([], [_ | _], _, _, _) :-
    unexpected($pred, "length mismatch").

ml_gen_call_current_success_cont(Context, Stmt, !Info) :-
    ml_gen_info_current_success_cont(!.Info, SuccCont),
    SuccCont = success_cont(FuncRval, EnvPtrRval, ArgTypes0, ArgLvals0),
    ArgTypes = ArgTypes0 ++ [mlds_generic_env_ptr_type],
    ArgRvals0 = list.map(func(Lval) = ml_lval(Lval), ArgLvals0),
    ArgRvals =ArgRvals0 ++ [EnvPtrRval],
    RetTypes = [],
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    RetLvals = [],
    CallKind = ordinary_call,
    set.init(Markers),
    Stmt = ml_stmt_call(Signature, FuncRval, ArgRvals, RetLvals,
        CallKind, Markers, Context).

%---------------------------------------------------------------------------%
%
% Routines for dealing with the environment pointer used for nested functions.
%

ml_get_env_ptr(ml_lval(EnvPtrLval)) :-
    EnvPtrLval = ml_local_var(lvn_comp_var(lvnc_env_ptr), mlds_unknown_type).

ml_declare_env_ptr_arg(Arg) :-
    VarName = lvn_comp_var(lvnc_env_ptr_arg),
    Type = mlds_generic_env_ptr_type,
    % The env_ptr_arg always points to the stack, since continuation
    % environments are always allocated on the stack (unless
    % put_nondet_env_on_heap is true, which won't be the case when
    % doing our own GC -- this is enforced in handle_options.m).
    % So the GC doesn't need to trace it.
    GCStatement = gc_no_stmt,
    Arg = mlds_argument(VarName, Type, GCStatement).

%---------------------------------------------------------------------------%

    % This function returns the offset to add to the argument
    % number of a closure arg to get its field number.
    %   field 0 is the closure layout
    %   field 1 is the closure address
    %   field 2 is the number of arguments
    %   field 3 is the 1st argument field
    %   field 4 is the 2nd argument field,
    %   etc.
    % Hence the offset to add to the argument number
    % to get the field number is 2.
    %
ml_closure_arg_offset = 2.

    % This function returns the offset to add to the argument
    % number of a typeclass_info arg to get its field number.
    % The Nth extra argument to pass to the method is
    % in field N of the typeclass_info, so the offset is zero.
    %
ml_typeclass_info_arg_offset = 0.

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
ml_base_typeclass_info_method_offset = 4.

%---------------------------------------------------------------------------%
%
% Routines for dealing with lookup tables.
%

ml_generate_constants_for_arms(_Vars, [], [], !Info).
ml_generate_constants_for_arms(Vars, [Goal | Goals], [Soln | Solns], !Info) :-
    ml_generate_constants_for_arm(Vars, Goal, Soln, !Info),
    ml_generate_constants_for_arms(Vars, Goals, Solns, !Info).

ml_generate_constants_for_arm(Vars, Goal, Soln, !Info) :-
    ml_gen_info_get_const_var_map(!.Info, InitConstVarMap),
    ml_gen_goal(model_det, Goal, _LocalVarDefns, _FuncDefns, _Stmts, !Info),
    ml_gen_info_get_const_var_map(!.Info, FinalConstVarMap),
    list.map(lookup_ground_rval(FinalConstVarMap), Vars, Soln),
    ml_gen_info_set_const_var_map(InitConstVarMap, !Info).

:- pred lookup_ground_rval(ml_ground_term_map::in, prog_var::in,
    mlds_rval::out) is det.

lookup_ground_rval(FinalConstVarMap, Var, Rval) :-
    % We can do a map.lookup instead of a map.search here because
    % - we execute this code only if we have already determined that
    %   goal_is_conj_of_unify succeeds for this arm,
    % - we don't even start looking for lookup switches unless we know
    %   that the mark_static_terms pass has been run, and
    % - for every arm on which goal_is_conj_of_unify succeeds,
    %   mark_static_terms will mark all the variables to which Var
    %   may be bound as being constructed statically. (There can be no need
    %   to construct them dynamically, since all the arm's nonlocals are
    %   output, which means none of them can be input.)
    map.lookup(FinalConstVarMap, Var, GroundTerm),
    GroundTerm = ml_ground_term(Rval, _, _).

ml_generate_field_assign(OutVarLval, FieldType, FieldId, VectorCommon,
        StructType, IndexRval, Context, Stmt, !Info) :-
    BaseRval = ml_vector_common_row_addr(VectorCommon, IndexRval),
    FieldLval = ml_field(yes(0), BaseRval, FieldId, FieldType, StructType),
    AtomicStmt = assign(OutVarLval, ml_lval(FieldLval)),
    Stmt = ml_stmt_atomic(AtomicStmt, Context).

ml_generate_field_assigns(OutVars, FieldTypes, FieldIds, VectorCommon,
        StructType, IndexRval, Context, Stmts, !Info) :-
    ( if
        OutVars = [],
        FieldTypes = [],
        FieldIds = []
    then
        Stmts = []
    else if
        OutVars = [HeadOutVar | TailOutVars],
        FieldTypes = [HeadFieldType | TailFieldTypes],
        FieldIds = [HeadFieldId | TailFieldIds]
    then
        ml_gen_var(!.Info, HeadOutVar, HeadOutVarLval),
        ml_generate_field_assign(HeadOutVarLval, HeadFieldType, HeadFieldId,
            VectorCommon, StructType, IndexRval, Context, HeadStmt,
            !Info),
        ml_generate_field_assigns(TailOutVars, TailFieldTypes, TailFieldIds,
            VectorCommon, StructType, IndexRval, Context, TailStmts,
            !Info),
        Stmts = [HeadStmt | TailStmts]
    else
        unexpected($pred, "mismatched lists")
    ).

%---------------------------------------------------------------------------%
%
% Miscellaneous routines.
%

fixup_builtin_module(ModuleName0) = ModuleName :-
    ( if ModuleName0 = unqualified("") then
        ModuleName = mercury_public_builtin_module
    else
        ModuleName = ModuleName0
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_code_util.
%---------------------------------------------------------------------------%
