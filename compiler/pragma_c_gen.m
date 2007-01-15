%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pragma_c_gen.m
% Main authors: dgj, conway, zs.
%
% The code in this module generates code for call_foreign_proc goals.
%
% The schemes we use to generate code for model_det and model_semi
% call_foreign_proc goals are quite similar, so we handle them together.
% The code that does this is reasonably simple.
%
% The scheme for model_non call_foreign_procs is substantially different,
% so we handle them separately.
%
%---------------------------------------------------------------------------%

:- module ll_backend.pragma_c_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred generate_foreign_proc_code(code_model::in,
    pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    list(foreign_arg)::in, list(foreign_arg)::in,
    maybe(trace_expr(trace_runtime))::in, pragma_foreign_code_impl::in,
    hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- func foreign_proc_struct_name(module_name, string, int, proc_id) = string.

    % The name of the variable model_semi foreign_procs in C assign to
    % to indicate success or failure. Exported for llds_out.m.
    %
:- func foreign_proc_succ_ind_name = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.name_mangle.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_util.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

% The code we generate for an ordinary (model_det or model_semi)
% call_foreign_proc goal must be able to fit into the middle of a procedure,
% since such call_foreign_proc goals can be inlined. This code is of the
% following form:
%
%   <save live variables onto the stack> /* see note (1) below */
%   {
%       <declaration of one local variable for each arg>
%       #define MR_PROC_LABEL <procedure label> /* see note (5) below */
%
%       <assignment of input values from registers to local variables>
%       MR_save_registers(); /* see notes (1) and (2) below */
%       { <the c code itself> }
%       <for semidet code, check of r1>
%       #ifndef MR_CONSERVATIVE_GC
%           MR_restore_registers(); /* see notes (1) and (3) below */
%       #endif
%       <assignment of the output values from local variables to registers>
%
%       #undef MR_PROC_LABEL /* see note (5) below */
%   }
%
% In the case of a semidet foreign_proc, the above is followed by
%
%   goto skip_label;
%   fail_label:
%   <code to fail>
%   skip_label:
%
% and the <check of r1> is of the form
%
%   if (!r1) MR_GOTO_LABEL(fail_label);
%
% In the case of a foreign_proc with determinism failure, the above
% is followed by
%
%    <code to fail>
%
% and the <check of r1> is empty.
%
% The code we generate for nondet call_foreign_proc goals assumes that this
% code is the only thing between the procedure prolog and epilog; such
% call_foreign_proc goals therefore cannot be inlined. The code of the
% procedure is of one of the following two forms:
%
% form 1 (duplicated common code):
% <proc entry label and comments>
% <mkframe including space for the save struct, redoip = do_fail>
% <#define MR_ORDINARY_SLOTS>
% <--- boundary between prolog and code generated here --->
% <set redoip to point to &&xxx_i1>
% <code for entry to a disjunction and first disjunct>
% {
%   <declaration of one local variable for each input and output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment of input values from registers to local variables>
%   <assignment to save struct pointer>
%   MR_save_registers(); /* see notes (1) and (2) below */
%   #define MR_PROC_LABEL <procedure label> /* see note (5) below */
%   #define SUCCEED()   goto callsuccesslabel
%   #define SUCCEED_LAST()  goto calllastsuccesslabel
%   #define FAIL()      fail()
%   { <the user-written call c code> }
%   { <the user-written shared c code> }
% callsuccesslabel:
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed()
% calllastsuccesslabel: /* see note (4) below) */
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed_discard()
%   #undef SUCCEED
%   #undef SUCCEED_LAST
%   #undef FAIL
%   #undef MR_PROC_LABEL /* see note (5) below */
% }
% MR_define_label(xxx_i1)
% <code for entry to a later disjunct>
% {
%   <declaration of one local variable for each output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment to save struct pointer>
%   MR_save_registers(); /* see notes (1) and (2) below */
%   #define MR_PROC_LABEL <procedure label> /* see note (5) below */
%   #define SUCCEED()   goto retrysuccesslabel
%   #define SUCCEED_LAST()  goto retrylastsuccesslabel
%   #define FAIL()      fail()
%   { <the user-written retry c code> }
%   { <the user-written shared c code> }
% retrysuccesslabel:
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed()
% retrylastsuccesslabel: /* see note (4) below) */
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed_discard()
%   #undef SUCCEED
%   #undef SUCCEED_LAST
%   #undef FAIL
%   #undef MR_PROC_LABEL /* see note (5) below */
% }
% <--- boundary between code generated here and epilog --->
% <#undef MR_ORDINARY_SLOTS>
%
% form 2 (shared common code):
% <proc entry label and comments>
% <mkframe including space for the save struct, redoip = do_fail>
% <#define MR_ORDINARY_SLOTS>
% <--- boundary between prolog and code generated here --->
% <set redoip to point to &&xxx_i1>
% <code for entry to a disjunction and first disjunct>
% {
%   <declaration of one local variable for each input and output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment of input values from registers to local variables>
%   <assignment to save struct pointer>
%   MR_save_registers(); /* see notes (1) and (2) below */
%   #define MR_PROC_LABEL <procedure label> /* see note (5) below */
%   #define SUCCEED()   goto callsuccesslabel
%   #define SUCCEED_LAST()  goto calllastsuccesslabel
%   #define FAIL()      fail()
%   { <the user-written call c code> }
%   MR_GOTO_LABEL(xxx_i2)
% callsuccesslabel: /* see note (4) below */
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed()
% calllastsuccesslabel: /* see note (4) below */
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed_discard()
%   #undef SUCCEED
%   #undef SUCCEED_LAST
%   #undef FAIL
%   #undef MR_PROC_LABEL /* see note (5) below */
% }
% MR_define_label(xxx_i1)
% <code for entry to a later disjunct>
% {
%   <declaration of one local variable for each output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment to save struct pointer>
%   MR_save_registers(); /* see notes (1) and (2) below */
%   #define MR_PROC_LABEL <procedure label> /* see note (5) below */
%   #define SUCCEED()   goto retrysuccesslabel
%   #define SUCCEED_LAST()  goto retrylastsuccesslabel
%   #define FAIL()      fail()
%   { <the user-written retry c code> }
%   MR_GOTO_LABEL(xxx_i2)
% retrysuccesslabel: /* see note (4) below */
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed()
% retrylastsuccesslabel: /* see note (4) below */
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed_discard()
%   #undef SUCCEED
%   #undef SUCCEED_LAST
%   #undef FAIL
%   #undef MR_PROC_LABEL /* see note (5) below */
% }
% MR_define_label(xxx_i2)
% {
%   <declaration of one local variable for each output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment to save struct pointer>
%   #define MR_PROC_LABEL <procedure label> /* see note (5) below */
%   #define SUCCEED()   goto sharedsuccesslabel
%   #define SUCCEED_LAST()  goto sharedlastsuccesslabel
%   #define FAIL()      fail()
%   { <the user-written shared c code> }
% sharedsuccesslabel:
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed()
% sharedlastsuccesslabel: /* see note (4) below */
%   MR_restore_registers(); /* see notes (1) and (3) below */
%   <assignment of the output values from local variables to registers>
%   succeed_discard()
%   #undef SUCCEED
%   #undef SUCCEED_LAST
%   #undef FAIL
%   #undef MR_PROC_LABEL /* see note (5) below */
% }
% <--- boundary between code generated here and epilog --->
% <#undef MR_ORDINARY_SLOTS>
%
% The first form is more time efficient, since it does not include the jumps
% from the call code and retry code to the shared code and the following
% initialization of the save struct pointer in the shared code block,
% while the second form can lead to smaller code since it does not include
% the shared C code (which can be quite big) twice.
%
% Programmers may indicate which form they wish the compiler to use;
% if they don't, the compiler will choose form 1 if the shared code fragment
% is "short", and form 2 if it is "long".
%
% The procedure prolog creates a nondet stack frame that includes space for
% a struct that is saved across calls. Since the position of this struct in
% the nondet stack frame is not known until the procedure prolog is created,
% which is *after* the call to generate_foreign_proc_code, the prolog will
% #define MR_ORDINARY_SLOTS as the number of ordinary slots in the nondet
% frame. From the size of the fixed portion of the nondet stack frame, from
% MR_ORDINARY_SLOTS and from the size of the save struct itself, one can
% calculate the address of the save struct itself. The epilog will #undef
% MR_ORDINARY_SLOTS. It need not do anything else, since all the normal epilog
% stuff has been done in the code above.
%
% Unlike with ordinary foreign_procs, with nondet foreign_procs there are never
% any live variables to save at the start, except for the input variables,
% and saving these is a job for the included C code. Also unlike ordinary
% foreign_procs, nondet C codes are never followed by any other code,
% so the exprn_info component of the code generator state need not be
% kept up to date.
%
% Depending on the value of options such as generate_trace, use_trail, and
% reclaim_heap_on_nondet_failure, we may need to include some code before
% the call and retry labels. The generation of this code should follow
% the same rules as the generation of similar code in nondet disjunctions.
%
% Notes:
%
% 1 These parts are only emitted if the C code may call Mercury. If the pragma
%   foreign_proc had a will_not_call_mercury annotation, they will not be
%   emitted.
%
% 2 The call to MR_save_registers() is needed so that if the C code calls
%   Mercury code, we can call MR_restore_registers() on entry to the Mercury
%   code (see export.m) to get the right values of `sp', `hp', `curfr' and
%   `maxfr' for the recursive invocation of Mercury.
%
% 3 The call to MR_restore_registers() is needed in case the C code calls
%   Mercury code which allocates some data on the heap, and this data is
%   returned from Mercury through C back to Mercury. In that case, we need
%   to keep the value of `hp' that was set by the recursive invocation of
%   Mercury. The Mercury calling convention guarantees that when calling det
%   or semidet code, the values of `sp', `curfr', and `maxfr' will be
%   preserved, so if we're using conservative gc, there is nothing that
%   needs restoring.
%
%   When calling nondet code, maxfr may be changed. This is why we must call
%   MR_restore_registers() from the code we generate for nondet foreign_procs
%   even if we are not using conservative gc.
%
% 4 These labels and the code following them can be optimized away by the C
%   compiler if the macro that branches to them is not invoked in the preceding
%   body of included C code. We cannot optimize them away ourselves, since
%   these macros can be invoked from other macros, and thus we do not have
%   a sure test of whether the code fragments invoke the macros.
%
% 5 We insert a #define for MR_PROC_LABEL, so that the C code in the Mercury
%   standard library that allocates memory manually can use MR_PROC_LABEL as
%   the procname argument to incr_hp_msg(), for memory profiling. Hard-coding
%   the procname argument in the C code would be wrong, since it wouldn't
%   handle the case where the original foreign_proc gets inlined and optimized
%   away. Of course we also need to #undef it afterwards.

%---------------------------------------------------------------------------%

generate_foreign_proc_code(CodeModel, Attributes, PredId, ProcId,
        Args, ExtraArgs, MaybeTraceRuntimeCond, PragmaImpl, GoalInfo, Code,
        !CI) :-
    (
        PragmaImpl = fc_impl_ordinary(C_Code, Context),
        (
            MaybeTraceRuntimeCond = no,
            CanOptAwayUnnamedArgs = yes,
            generate_ordinary_foreign_proc_code(CodeModel, Attributes,
                PredId, ProcId, Args, ExtraArgs, C_Code, Context, GoalInfo,
                CanOptAwayUnnamedArgs, Code, !CI)
        ;
            MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
            expect(unify(Args, []), this_file,
                "generate_foreign_proc_code: args runtime cond"),
            expect(unify(ExtraArgs, []), this_file,
                "generate_foreign_proc_code: extra args runtime cond"),
            expect(unify(CodeModel, model_semi), this_file,
                "generate_foreign_proc_code: non-semi runtime cond"),
            generate_trace_runtime_cond_foreign_proc_code(TraceRuntimeCond,
                Code, !CI)
        )
    ;
        PragmaImpl = fc_impl_model_non(Fields, FieldsContext,
            First, FirstContext, Later, LaterContext,
            Treat, Shared, SharedContext),
        expect(unify(ExtraArgs, []), this_file,
            "generate_foreign_proc_code: extra args nondet"),
        require(unify(MaybeTraceRuntimeCond, no),
            "generate_foreign_proc_code: runtime cond nondet"),
        CanOptAwayUnnamedArgs = yes,
        generate_nondet_foreign_proc_code(CodeModel, Attributes,
            PredId, ProcId, Args, Fields, FieldsContext,
            First, FirstContext, Later, LaterContext,
            Treat, Shared, SharedContext, CanOptAwayUnnamedArgs, Code, !CI)
    ;
        PragmaImpl = fc_impl_import(Name, HandleReturn, Vars, Context),
        expect(unify(ExtraArgs, []), this_file,
            "generate_foreign_proc_code: extra args import"),
        require(unify(MaybeTraceRuntimeCond, no),
            "generate_foreign_proc_code: runtime cond import"),
        C_Code = HandleReturn ++ " " ++ Name ++ "(" ++ Vars ++ ");",

        % The imported function was generated with all arguments present.
        CanOptAwayUnnamedArgs = no,
        generate_ordinary_foreign_proc_code(CodeModel, Attributes,
            PredId, ProcId, Args, ExtraArgs, C_Code, Context, GoalInfo,
            CanOptAwayUnnamedArgs, Code, !CI)
    ).

%---------------------------------------------------------------------------%

:- pred generate_trace_runtime_cond_foreign_proc_code(
    trace_expr(trace_runtime)::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_trace_runtime_cond_foreign_proc_code(RuntimeExpr, Code, !CI) :-
    generate_runtime_cond_code(RuntimeExpr, CondRval, !CI),
    code_info.get_next_label(SuccessLabel, !CI),
    code_info.generate_failure(FailCode, !CI),
    CondCode = node([
        llds_instr(if_val(CondRval, code_label(SuccessLabel)),
            "environment variable tests")
    ]),
    SuccessLabelCode = node([
        llds_instr(label(SuccessLabel),
            "environment variable tests successful")
    ]),
    Code = tree_list([CondCode, FailCode, SuccessLabelCode]).

:- pred generate_runtime_cond_code(trace_expr(trace_runtime)::in,
    rval::out, code_info::in, code_info::out) is det.

generate_runtime_cond_code(Expr, CondRval, !CI) :-
    (
        Expr = trace_base(trace_envvar(EnvVar)),
        get_used_env_vars(!.CI, UsedEnvVars0),
        set.insert(UsedEnvVars0, EnvVar, UsedEnvVars),
        set_used_env_vars(UsedEnvVars, !CI),
        EnvVarRval = lval(global_var_ref(env_var_ref(EnvVar))),
        ZeroRval = const(llconst_int(0)),
        CondRval = binop(ne, EnvVarRval, ZeroRval)
    ;
        Expr = trace_op(TraceOp, ExprA, ExprB),
        generate_runtime_cond_code(ExprA, RvalA, !CI),
        generate_runtime_cond_code(ExprB, RvalB, !CI),
        (
            TraceOp = trace_or,
            Op = logical_or
        ;
            TraceOp = trace_and,
            Op = logical_and
        ),
        CondRval = binop(Op, RvalA, RvalB)
    ).

%---------------------------------------------------------------------------%

:- pred generate_ordinary_foreign_proc_code(code_model::in,
    pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    list(foreign_arg)::in, list(foreign_arg)::in, string::in,
    maybe(prog_context)::in, hlds_goal_info::in, bool::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_ordinary_foreign_proc_code(CodeModel, Attributes, PredId, ProcId,
        Args, ExtraArgs, C_Code, Context, GoalInfo, CanOptAwayUnnamedArgs,
        Code, !CI) :-
    % Extract the attributes.
    MayCallMercury = get_may_call_mercury(Attributes),
    ThreadSafe = get_thread_safe(Attributes),

    % The maybe_thread_safe attribute should have been changed
    % to the real value by now.
    (
        ThreadSafe = proc_thread_safe
    ;
        ThreadSafe = proc_maybe_thread_safe,
        unexpected(this_file, "generate_ordinary_foreign_proc_code: " ++
            "maybe_thread_safe encountered.")
    ;
        ThreadSafe = proc_not_thread_safe
    ),
    % First we need to get a list of input and output arguments.
    ArgInfos = code_info.get_pred_proc_arginfo(!.CI, PredId, ProcId),
    make_c_arg_list(Args, ArgInfos, OrigCArgs),
    code_info.get_module_info(!.CI, ModuleInfo),
    make_extra_c_arg_list(ExtraArgs, ModuleInfo, ArgInfos, ExtraCArgs),
    list.append(OrigCArgs, ExtraCArgs, CArgs),
    foreign_proc_select_in_args(CArgs, InCArgs),
    foreign_proc_select_out_args(CArgs, OutCArgs),

    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    set.init(DeadVars0),
    find_dead_input_vars(InCArgs, PostDeaths, DeadVars0, DeadVars),

    % Generate code to <save live variables on stack>.
    (
        MayCallMercury = proc_will_not_call_mercury,
        SaveVarsCode = empty
    ;
        MayCallMercury = proc_may_call_mercury,
        % The C code might call back Mercury code which clobbers the succip.
        code_info.succip_is_used(!CI),

        % The C code might call back Mercury code which clobbers the
        % other registers, so we need to save any live variables
        % (other than the output args) onto the stack.
        get_c_arg_list_vars(OutCArgs, OutVars),
        set.list_to_set(OutVars, OutVarsSet),
        code_info.save_variables(OutVarsSet, _, SaveVarsCode, !CI)
    ),

    % Generate the values of input variables.
    % (NB we need to be careful that the rvals generated here
    % remain valid below.)
    get_foreign_proc_input_vars(InCArgs, InputDescs, CanOptAwayUnnamedArgs,
        InputVarsCode, !CI),

    % We cannot kill the forward dead input arguments until we have
    % finished generating the code producing the input variables.
    % (The forward dead variables will be dead after the call_foreign_proc,
    % but are live during its input phase.)
    code_info.make_vars_forward_dead(DeadVars, !CI),

    % Generate <declaration of one local variable for each arg>.
    make_foreign_proc_decls(CArgs, ModuleInfo, CanOptAwayUnnamedArgs, Decls),

    % Generate #define MR_PROC_LABEL <procedure label> /* see note (5) */
    % and #undef MR_PROC_LABEL.
    code_info.get_pred_id(!.CI, CallerPredId),
    code_info.get_proc_id(!.CI, CallerProcId),
    make_proc_label_hash_define(ModuleInfo, CallerPredId, CallerProcId,
        ProcLabelHashDefine, ProcLabelHashUndef),

    % <assignment of input values from registers to local vars>
    InputComp = foreign_proc_inputs(InputDescs),

    % MR_save_registers(); /* see notes (1) and (2) above */
    (
        MayCallMercury = proc_will_not_call_mercury,
        SaveRegsComp = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), "")
    ;
        MayCallMercury = proc_may_call_mercury,
        SaveRegsComp = foreign_proc_raw_code(cannot_branch_away,
            proc_affects_liveness, live_lvals_info(set.init),
            "\tMR_save_registers();\n")
    ),

    % Code fragments to obtain and release the global lock.
    (
        ThreadSafe = proc_thread_safe,
        ObtainLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), ""),
        ReleaseLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), "")
    ;
        ThreadSafe = proc_not_thread_safe,
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        c_util.quote_string(Name, MangledName),
        ObtainLockStr = "\tMR_OBTAIN_GLOBAL_LOCK("""
            ++ MangledName ++ """);\n",
        ObtainLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            ObtainLockStr),
        ReleaseLockStr = "\tMR_RELEASE_GLOBAL_LOCK("""
            ++ MangledName ++ """);\n",
        ReleaseLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            ReleaseLockStr)
    ),

    % <The C code itself>
    AffectsLiveness = get_affects_liveness(Attributes),
    C_Code_Comp = foreign_proc_user_code(Context, AffectsLiveness, C_Code),

    % <for semidet code, check of SUCCESS_INDICATOR>
    goal_info_get_determinism(GoalInfo, Detism),
    ( CodeModel = model_semi ->
        ( Detism = detism_failure ->
            CheckSuccess_Comp = foreign_proc_noop,
            MaybeFailLabel = no
        ;
            code_info.get_next_label(FailLabel, !CI),
            CheckSuccess_Comp = foreign_proc_fail_to(FailLabel),
            MaybeFailLabel = yes(FailLabel)
        ),
        DefSuccessComp = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            "\tMR_bool " ++ foreign_proc_succ_ind_name ++ ";\n" ++
            "#undef SUCCESS_INDICATOR\n" ++
            "#define SUCCESS_INDICATOR " ++
                foreign_proc_succ_ind_name ++ "\n"),
        UndefSuccessComp = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            "#undef SUCCESS_INDICATOR\n" ++
            "#define SUCCESS_INDICATOR MR_r1\n")
    ;
        CheckSuccess_Comp = foreign_proc_noop,
        MaybeFailLabel = no,
        DefSuccessComp = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), ""),
        UndefSuccessComp = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), "")
    ),

    % #ifndef MR_CONSERVATIVE_GC
    %   MR_restore_registers(); /* see notes (1) and (3) above */
    % #endif
    (
        MayCallMercury = proc_will_not_call_mercury,
        RestoreRegsComp = foreign_proc_noop
    ;
        MayCallMercury = proc_may_call_mercury,
        RestoreRegsComp = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            "#ifndef MR_CONSERVATIVE_GC\n\t" ++
                "MR_restore_registers();\n#endif\n")
    ),

    % The C code may have called Mercury code which clobbered the regs,
    % in which case we need to tell the code_info that they have been
    % clobbered.
    (
        MayCallMercury = proc_will_not_call_mercury
    ;
        MayCallMercury = proc_may_call_mercury,
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
        ( instmap_delta_is_reachable(InstMapDelta) ->
            OkToDelete = no
        ;
            OkToDelete = yes
        ),
        code_info.clear_all_registers(OkToDelete, !CI)
    ),

    % <assignment of the output values from local variables to registers>
    foreign_proc_acquire_regs(OutCArgs, Regs, !CI),
    place_foreign_proc_output_args_in_regs(OutCArgs, Regs,
        CanOptAwayUnnamedArgs, OutputDescs, !CI),
    OutputComp = foreign_proc_outputs(OutputDescs),

    % Join all the components of the foreign_proc_code together.
    Components = [ProcLabelHashDefine, DefSuccessComp, InputComp,
        SaveRegsComp, ObtainLock, C_Code_Comp, ReleaseLock,
        CheckSuccess_Comp, RestoreRegsComp,
        OutputComp, UndefSuccessComp, ProcLabelHashUndef],
    MaybeMayDupl = get_may_duplicate(Attributes),
    (
        MaybeMayDupl = yes(MayDupl)
    ;
        MaybeMayDupl = no,
        (
            ExtraArgs = [],
            MayDupl = proc_may_duplicate
        ;
            ExtraArgs = [_ | _],
            MayDupl = proc_may_not_duplicate
        )
    ),
    ExtraAttributes = get_extra_attributes(Attributes),
    ( list.member(refers_to_llds_stack, ExtraAttributes) ->
        RefersToLLDSSTack = yes
    ;
        RefersToLLDSSTack = no
    ),
    PragmaCCode = node([
        llds_instr(foreign_proc_code(Decls, Components, MayCallMercury,
            no, no, no, MaybeFailLabel, RefersToLLDSSTack, MayDupl),
            "foreign_proc inclusion")
    ]),
    %
    % For semidet code, we need to insert the failure handling code here:
    %
    %   goto skip_label;
    %   fail_label:
    %   <code to fail>
    %   skip_label:
    %
    % For code with determinism failure, we need to insert the failure
    % handling code here:
    %
    %   <code to fail>
    %
    ( MaybeFailLabel = yes(TheFailLabel) ->
        code_info.get_next_label(SkipLabel, !CI),
        code_info.generate_failure(FailCode, !CI),
        GotoSkipLabelCode = node([
            llds_instr(goto(code_label(SkipLabel)), "Skip past failure code")
        ]),
        SkipLabelCode = node([llds_instr(label(SkipLabel), "")]),
        FailLabelCode = node([llds_instr(label(TheFailLabel), "")]),
        FailureCode = tree_list([GotoSkipLabelCode, FailLabelCode,
            FailCode, SkipLabelCode])
    ; Detism = detism_failure ->
        code_info.generate_failure(FailureCode, !CI)
    ;
        FailureCode = empty
    ),

    % Join all code fragments together.
    Code = tree_list([SaveVarsCode, InputVarsCode, PragmaCCode, FailureCode]).

:- pred make_proc_label_hash_define(module_info::in, pred_id::in, proc_id::in,
    foreign_proc_component::out, foreign_proc_component::out) is det.

make_proc_label_hash_define(ModuleInfo, PredId, ProcId,
        ProcLabelHashDef, ProcLabelHashUndef) :-
    ProcLabelHashDef = foreign_proc_raw_code(cannot_branch_away,
        proc_does_not_affect_liveness, live_lvals_info(set.init),
        "#define\tMR_PROC_LABEL\t" ++
            make_proc_label_string(ModuleInfo, PredId, ProcId) ++ "\n"),
    ProcLabelHashUndef = foreign_proc_raw_code(cannot_branch_away,
        proc_does_not_affect_liveness, live_lvals_info(set.init),
        "#undef\tMR_PROC_LABEL\n").

:- func make_proc_label_string(module_info, pred_id, proc_id) = string.

make_proc_label_string(ModuleInfo, PredId, ProcId) = ProcLabelString :-
    CodeAddr = make_entry_label(ModuleInfo, PredId, ProcId, no),
    ( CodeAddr = code_imported_proc(ProcLabel) ->
        ProcLabelString = proc_label_to_c_string(ProcLabel, yes)
    ; CodeAddr = code_label(Label) ->
        ProcLabelString = label_to_c_string(Label, yes)
    ;
        unexpected(this_file, "code_addr in make_proc_label_hash_define")
    ).

%-----------------------------------------------------------------------------%

:- pred generate_nondet_foreign_proc_code(code_model::in,
    pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    list(foreign_arg)::in,
    string::in, maybe(prog_context)::in,
    string::in, maybe(prog_context)::in,
    string::in, maybe(prog_context)::in,
    foreign_proc_shared_code_treatment::in,
    string::in, maybe(prog_context)::in, bool::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_nondet_foreign_proc_code(CodeModel, Attributes, PredId, ProcId,
        Args, _Fields, _FieldsContext,
        First, FirstContext, Later, LaterContext, Treat, Shared, SharedContext,
        CanOptAwayUnnamedArgs, Code, !CI) :-
    expect(unify(CodeModel, model_non), this_file,
        "inappropriate code model for nondet foreign_proc"),
    % Extract the may_call_mercury attribute.
    MayCallMercury = get_may_call_mercury(Attributes),
    AffectsLiveness = get_affects_liveness(Attributes),

    % Generate #define MR_PROC_LABEL <procedure label> /* see note (5) */
    % and #undef MR_PROC_LABEL.
    code_info.get_module_info(!.CI, ModuleInfo),
    code_info.get_pred_id(!.CI, CallerPredId),
    code_info.get_proc_id(!.CI, CallerProcId),
    make_proc_label_hash_define(ModuleInfo, CallerPredId, CallerProcId,
        ProcLabelDefine, ProcLabelUndef),

    % Generate a unique prefix for the C labels that we will define.
    ProcLabelString = make_proc_label_string(ModuleInfo,
        PredId, ProcId),

    % Get a list of input and output arguments.
    ArgInfos = code_info.get_pred_proc_arginfo(!.CI, PredId, ProcId),
    make_c_arg_list(Args, ArgInfos, CArgs),
    foreign_proc_select_in_args(CArgs, InCArgs),
    foreign_proc_select_out_args(CArgs, OutCArgs),
    make_foreign_proc_decls(CArgs, ModuleInfo, CanOptAwayUnnamedArgs, Decls),
    make_foreign_proc_decls(OutCArgs, ModuleInfo, CanOptAwayUnnamedArgs,
        OutDecls),

    input_descs_from_arg_info(!.CI, InCArgs, CanOptAwayUnnamedArgs,
        InputDescs),
    output_descs_from_arg_info(!.CI, OutCArgs, CanOptAwayUnnamedArgs,
        OutputDescs),

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    StructName = foreign_proc_struct_name(ModuleName, PredName, Arity, ProcId),
    SaveStructDecl = foreign_proc_struct_ptr_decl(StructName, "LOCALS"),
    string.format("\tLOCALS = (struct %s *) ((char *)
        (MR_curfr + 1 - MR_ORDINARY_SLOTS - MR_NONDET_FIXED_SIZE)
        - sizeof(struct %s));\n",
        [s(StructName), s(StructName)],
        InitSaveStruct),

    code_info.get_next_label(RetryLabel, !CI),
    ModFrameCode = node([
        llds_instr(assign(redoip_slot(lval(curfr)),
            const(llconst_code_addr(code_label(RetryLabel)))),
            "Set up backtracking to retry label")
    ]),
    RetryLabelCode = node([
        llds_instr(label(RetryLabel), "Start of the retry block")
    ]),

    code_info.get_globals(!.CI, Globals),

    globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
        ReclaimHeap),
    code_info.maybe_save_hp(ReclaimHeap, SaveHeapCode, MaybeHpSlot, !CI),
    code_info.maybe_restore_hp(MaybeHpSlot, RestoreHeapCode),

    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    code_info.maybe_save_ticket(UseTrail, SaveTicketCode, MaybeTicketSlot,
        !CI),
    code_info.maybe_reset_ticket(MaybeTicketSlot, reset_reason_undo,
        RestoreTicketCode),
    (
        FirstContext = yes(ActualFirstContext)
    ;
        FirstContext = no,
        term.context_init(ActualFirstContext)
    ),
    maybe_generate_foreign_proc_event_code(nondet_foreign_proc_first,
        ActualFirstContext, FirstTraceCode, !CI),
    (
        LaterContext = yes(ActualLaterContext)
    ;
        LaterContext = no,
        term.context_init(ActualLaterContext)
    ),
    maybe_generate_foreign_proc_event_code(nondet_foreign_proc_later,
        ActualLaterContext, LaterTraceCode, !CI),

    FirstDisjunctCode = tree_list([SaveHeapCode, SaveTicketCode,
         FirstTraceCode]),
    LaterDisjunctCode = tree_list([RestoreHeapCode, RestoreTicketCode,
         LaterTraceCode]),

    % MR_save_registers(); /* see notes (1) and (2) above */
    % MR_restore_registers(); /* see notes (1) and (3) above */
    (
        MayCallMercury = proc_will_not_call_mercury,
        SaveRegs = "",
        RestoreRegs = ""
    ;
        MayCallMercury = proc_may_call_mercury,
        SaveRegs = "\tMR_save_registers();\n",
        RestoreRegs = "\tMR_restore_registers();\n"
    ),

    Succeed  = "\tMR_succeed();\n",
    SucceedDiscard = "\tMR_succeed_discard();\n",

    CallDef1 = "#define\tSUCCEED     \tgoto MR_call_success_"
        ++ ProcLabelString ++ "\n",
    CallDef2 = "#define\tSUCCEED_LAST\tgoto MR_call_success_last_"
        ++ ProcLabelString ++ "\n",
    CallDef3 = "#define\tFAIL\tMR_fail()\n",

    CallSuccessLabel     = "MR_call_success_"
        ++ ProcLabelString ++ ":\n",
    CallLastSuccessLabel = "MR_call_success_last_"
        ++ ProcLabelString ++ ":\n",

    RetryDef1 = "#define\tSUCCEED     \tgoto MR_retry_success_"
        ++ ProcLabelString ++ "\n",
    RetryDef2 = "#define\tSUCCEED_LAST\tgoto MR_retry_success_last_"
        ++ ProcLabelString ++ "\n",
    RetryDef3 = "#define\tFAIL\tMR_fail()\n",

    RetrySuccessLabel     = "MR_retry_success_"
        ++ ProcLabelString ++ ":\n",
    RetryLastSuccessLabel = "MR_retry_success_last_"
        ++ ProcLabelString ++ ":\n",

    Undef1 = "#undef\tSUCCEED\n",
    Undef2 = "#undef\tSUCCEED_LAST\n",
    Undef3 = "#undef\tFAIL\n",

    MD = proc_may_not_duplicate,
    (
        % Use the form that duplicates the common code if the programmer
        % asked for it, or if the code is small enough for its duplication
        % not to have a significant effect on code size. (This form generates
        % slightly faster code.) However, if `pragma no_inline' is specified,
        % then we don't duplicate the code unless the programmer asked for it
        % -- the code may contain static variable declarations, so duplicating
        % it could change the semantics.

        % We use the number of semicolons in the code as an indication of
        % how many C statements it has and thus how big its object code
        % is likely to be.
        (
            Treat = shared_code_duplicate
        ;
            Treat = shared_code_automatic,
            \+ pred_info_requested_no_inlining(PredInfo),
            CountSemis = (pred(Char::in, Count0::in, Count::out) is det :-
                ( Char = (;) ->
                    Count = Count0 + 1
                ;
                    Count = Count0
                )
            ),
            string.foldl(CountSemis, Shared, 0, Semis),
            Semis < 32
        )
    ->
        CallDecls = [SaveStructDecl | Decls],
        CallComponents = [
            foreign_proc_inputs(InputDescs),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                InitSaveStruct),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SaveRegs),
            ProcLabelDefine,
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, CallDef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, CallDef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, CallDef3),
            foreign_proc_user_code(FirstContext, AffectsLiveness, First),
            foreign_proc_user_code(SharedContext, AffectsLiveness, Shared),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                CallSuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                Succeed),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                CallLastSuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SucceedDiscard),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef3),
            ProcLabelUndef
        ],
        CallBlockCode = node([
            llds_instr(foreign_proc_code(CallDecls, CallComponents,
                MayCallMercury, no, no, no, no, yes, MD),
                "Call and shared foreign_proc inclusion")
        ]),

        RetryDecls = [SaveStructDecl | OutDecls],
        RetryComponents = [
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                InitSaveStruct),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SaveRegs),
            ProcLabelDefine,
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, RetryDef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, RetryDef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, RetryDef3),
            foreign_proc_user_code(LaterContext, AffectsLiveness, Later),
            foreign_proc_user_code(SharedContext, AffectsLiveness, Shared),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RetrySuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                Succeed),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RetryLastSuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SucceedDiscard),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef3),
            ProcLabelUndef
        ],
        RetryBlockCode = node([
            llds_instr(foreign_proc_code(RetryDecls, RetryComponents,
                MayCallMercury, no, no, no, no, yes, MD),
                "Retry and shared foreign_proc inclusion")
        ]),

        Code = tree_list([ModFrameCode, FirstDisjunctCode, CallBlockCode,
            RetryLabelCode, LaterDisjunctCode, RetryBlockCode])
    ;
        code_info.get_next_label(SharedLabel, !CI),
        SharedLabelCode = node([
            llds_instr(label(SharedLabel), "Start of the shared block")
        ]),

        SharedDef1 =
            "#define\tSUCCEED     \tgoto MR_shared_success_"
            ++ ProcLabelString ++ "\n",
        SharedDef2 =
            "#define\tSUCCEED_LAST\tgoto MR_shared_success_last_"
            ++ ProcLabelString ++ "\n",
        SharedDef3 = "#define\tFAIL\tMR_fail()\n",

        SharedSuccessLabel     = "MR_shared_success_"
            ++ ProcLabelString ++ ":\n",
        SharedLastSuccessLabel = "MR_shared_success_last_"
            ++ ProcLabelString ++ ":\n",

        LabelStr = label_to_c_string(SharedLabel, yes),
        string.format("\tMR_GOTO_LABEL(%s);\n", [s(LabelStr)],
            GotoSharedLabel),

        CallDecls = [SaveStructDecl | Decls],
        CallComponents = [
            foreign_proc_inputs(InputDescs),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                InitSaveStruct),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SaveRegs),
            ProcLabelDefine,
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, CallDef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, CallDef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, CallDef3),
            foreign_proc_user_code(FirstContext, AffectsLiveness, First),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                GotoSharedLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                CallSuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                Succeed),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                CallLastSuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SucceedDiscard),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef3),
            ProcLabelUndef
        ],
        CallBlockCode = node([
            llds_instr(foreign_proc_code(CallDecls, CallComponents,
                MayCallMercury, yes(SharedLabel), no, no, no, yes, MD),
                "Call foreign_proc inclusion")
        ]),

        RetryDecls = [SaveStructDecl | OutDecls],
        RetryComponents = [
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                InitSaveStruct),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SaveRegs),
            ProcLabelDefine,
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, RetryDef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, RetryDef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, RetryDef3),
            foreign_proc_user_code(LaterContext, AffectsLiveness, Later),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                GotoSharedLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RetrySuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                Succeed),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RetryLastSuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SucceedDiscard),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef3),
            ProcLabelUndef
        ],
        RetryBlockCode = node([
            llds_instr(foreign_proc_code(RetryDecls, RetryComponents,
                MayCallMercury, yes(SharedLabel), no, no, no, yes, MD),
                "Retry foreign_proc inclusion")
        ]),

        SharedDecls = [SaveStructDecl | OutDecls],
        SharedComponents = [
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                InitSaveStruct),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SaveRegs),
            ProcLabelDefine,
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SharedDef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SharedDef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SharedDef3),
            foreign_proc_user_code(SharedContext, AffectsLiveness,
                Shared),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SharedSuccessLabel),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                Succeed),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SharedLastSuccessLabel),
            foreign_proc_raw_code(can_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                RestoreRegs),
            foreign_proc_outputs(OutputDescs),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info,
                SucceedDiscard),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef1),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef2),
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, no_live_lvals_info, Undef3),
            ProcLabelUndef
        ],
        SharedBlockCode = node([
            llds_instr(foreign_proc_code(SharedDecls, SharedComponents,
                MayCallMercury, no, no, no, no, yes, MD),
                "Shared foreign_proc inclusion")
        ]),

        Code = tree_list([ModFrameCode, FirstDisjunctCode, CallBlockCode,
            RetryLabelCode, LaterDisjunctCode, RetryBlockCode, SharedLabelCode,
            SharedBlockCode])
    ).

%---------------------------------------------------------------------------%

:- type c_arg
    --->    c_arg(
                prog_var,
                maybe(string),  % name
                mer_type,       % original type before
                                % inlining/specialization
                                % (the actual type may be an instance
                                % of this type, if this type is polymorphic).
                box_policy,
                arg_info
            ).

:- pred make_c_arg_list(list(foreign_arg)::in, list(arg_info)::in,
    list(c_arg)::out) is det.

make_c_arg_list([], [], []).
make_c_arg_list([Arg | ArgTail], [ArgInfo | ArgInfoTail], [CArg | CArgTail]) :-
    Arg = foreign_arg(Var, MaybeNameMode, Type, BoxPolicy),
    (
        MaybeNameMode = yes(Name - _),
        MaybeName = yes(Name)
    ;
        MaybeNameMode = no,
        MaybeName = no
    ),
    CArg = c_arg(Var, MaybeName, Type, BoxPolicy, ArgInfo),
    make_c_arg_list(ArgTail, ArgInfoTail, CArgTail).
make_c_arg_list([], [_ | _], _) :-
    unexpected(this_file, "make_c_arg_list length mismatch").
make_c_arg_list([_ | _], [], _) :-
    unexpected(this_file, "make_c_arg_list length mismatch").

%---------------------------------------------------------------------------%

:- pred make_extra_c_arg_list(list(foreign_arg)::in, module_info::in,
    list(arg_info)::in, list(c_arg)::out) is det.

make_extra_c_arg_list(ExtraArgs, ModuleInfo, ArgInfos, ExtraCArgs) :-
    get_highest_arg_num(ArgInfos, 0, MaxArgNum),
    make_extra_c_arg_list_seq(ExtraArgs, ModuleInfo, MaxArgNum, ExtraCArgs).

:- pred get_highest_arg_num(list(arg_info)::in, int::in, int::out) is det.

get_highest_arg_num([], !Max).
get_highest_arg_num([arg_info(Loc, _) | ArgInfos], !Max) :-
    int.max(Loc, !Max),
    get_highest_arg_num(ArgInfos, !Max).

:- pred make_extra_c_arg_list_seq(list(foreign_arg)::in, module_info::in,
    int::in, list(c_arg)::out) is det.

make_extra_c_arg_list_seq([], _, _, []).
make_extra_c_arg_list_seq([ExtraArg | ExtraArgs], ModuleInfo, LastReg,
        [CArg | CArgs]) :-
    ExtraArg = foreign_arg(Var, MaybeNameMode, OrigType, BoxPolicy),
    (
        MaybeNameMode = yes(Name - Mode),
        mode_to_arg_mode(ModuleInfo, Mode, OrigType, ArgMode)
    ;
        MaybeNameMode = no,
        unexpected(this_file, "make_extra_c_arg_list_seq: no name")
    ),
    NextReg = LastReg + 1,
    % Extra args are always input.
    ArgInfo = arg_info(NextReg, ArgMode),
    CArg = c_arg(Var, yes(Name), OrigType, BoxPolicy, ArgInfo),
    make_extra_c_arg_list_seq(ExtraArgs, ModuleInfo, NextReg, CArgs).

%---------------------------------------------------------------------------%

:- pred get_c_arg_list_vars(list(c_arg)::in, list(prog_var)::out) is det.

get_c_arg_list_vars([], []).
get_c_arg_list_vars([Arg | Args], [Var | Vars]) :-
    Arg = c_arg(Var, _, _, _, _),
    get_c_arg_list_vars(Args, Vars).

%---------------------------------------------------------------------------%

    % foreign_proc_select_out_args returns the list of variables which are outputs
    % for a procedure.
    %
:- pred foreign_proc_select_out_args(list(c_arg)::in, list(c_arg)::out) is det.

foreign_proc_select_out_args([], []).
foreign_proc_select_out_args([Arg | Rest], Out) :-
    foreign_proc_select_out_args(Rest, OutTail),
    Arg = c_arg(_, _, _, _, ArgInfo),
    ArgInfo = arg_info(_Loc, Mode),
    ( Mode = top_out ->
        Out = [Arg | OutTail]
    ;
        Out = OutTail
    ).

    % foreign_proc_select_in_args returns the list of variables
    % which are inputs for a procedure.
    %
:- pred foreign_proc_select_in_args(list(c_arg)::in, list(c_arg)::out) is det.

foreign_proc_select_in_args([], []).
foreign_proc_select_in_args([Arg | Rest], In) :-
    foreign_proc_select_in_args(Rest, InTail),
    Arg = c_arg(_, _, _, _, ArgInfo),
    ArgInfo = arg_info(_Loc, Mode),
    ( Mode = top_in ->
        In = [Arg | InTail]
    ;
        In = InTail
    ).

%---------------------------------------------------------------------------%

    % var_should_be_passed determines whether or not a variable with the given
    % user-defined name (if any) should be passed to the foreign_proc.
    %
    % Named non-singleton variables are always passed. Unnamed or singleton
    % variables are ignored if we are allowed to optimize them away, but we
    % aren't, we replace their name with UnnamedArgN.
    %
    % Singleton vars should be ignored when generating the declarations for
    % call_foreign_proc arguments because:
    %
    %   - they should not appear in the C code
    %   - they could clash with the system name space
    %
:- func var_should_be_passed(bool, prog_var, maybe(string)) = maybe(string).

var_should_be_passed(CanOptAwayUnnamedArgs, Var, MaybeName)
        = MaybeUseName :-
    (
        MaybeName = yes(Name),
        not string.first_char(Name, '_', _)
    ->
        MaybeUseName = yes(Name)
    ;
        (
            CanOptAwayUnnamedArgs = yes,
            MaybeUseName = no
        ;
            CanOptAwayUnnamedArgs = no,
            UseName = "UnnamedArg" ++ int_to_string(term.var_to_int(Var)),
            MaybeUseName = yes(UseName)
        )
    ).

%---------------------------------------------------------------------------%

    % make_foreign_proc_decls returns the list of foreign_proc_decls for the
    % foreign_proc_code instruction in the LLDS. It is essentially a list of
    % pairs of type and variable name, so that declarations of the form
    % "Type Name;" can be made.
    %
:- pred make_foreign_proc_decls(list(c_arg)::in, module_info::in, bool::in,
    list(foreign_proc_decl)::out) is det.

make_foreign_proc_decls([], _, _, []).
make_foreign_proc_decls([Arg | Args], Module, CanOptAwayUnnamedArgs, Decls) :-
    make_foreign_proc_decls(Args, Module, CanOptAwayUnnamedArgs, DeclsTail),
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, _ArgInfo),
    MaybeName = var_should_be_passed(CanOptAwayUnnamedArgs, Var, MaybeArgName),
    (
        MaybeName = yes(Name),
        (
            BoxPolicy = native_if_possible,
            OrigTypeString = foreign.to_type_string(lang_c, Module, OrigType)
        ;
            BoxPolicy = always_boxed,
            OrigTypeString = "MR_Word"
        ),
        Decl = foreign_proc_arg_decl(OrigType, OrigTypeString, Name),
        Decls = [Decl | DeclsTail]
    ;
        MaybeName = no,
        Decls = DeclsTail
    ).

%---------------------------------------------------------------------------%

:- pred find_dead_input_vars(list(c_arg)::in, set(prog_var)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

find_dead_input_vars([], _, !DeadVars).
find_dead_input_vars([Arg | Args], PostDeaths, !DeadVars) :-
    Arg = c_arg(Var, _MaybeName, _Type, _BoxPolicy, _ArgInfo),
    ( set.member(Var, PostDeaths) ->
        set.insert(!.DeadVars, Var, !:DeadVars)
    ;
        true
    ),
    find_dead_input_vars(Args, PostDeaths, !DeadVars).

%---------------------------------------------------------------------------%

    % get_foreign_proc_input_vars returns a list of foreign_proc_inputs
    % for the foreign_proc_code instruction in the LLDS. It is essentially
    % a list of the input variables, and the corresponding rvals assigned
    % to those (C) variables.
    %
:- pred get_foreign_proc_input_vars(list(c_arg)::in,
    list(foreign_proc_input)::out, bool::in, code_tree::out,
    code_info::in, code_info::out) is det.

get_foreign_proc_input_vars([], [], _, empty, !CI).
get_foreign_proc_input_vars([Arg | Args], Inputs, CanOptAwayUnnamedArgs, Code,
        !CI) :-
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, _ArgInfo),
    MaybeName = var_should_be_passed(CanOptAwayUnnamedArgs, Var, MaybeArgName),
    (
        MaybeName = yes(Name),
        VarType = variable_type(!.CI, Var),
        code_info.produce_variable(Var, FirstCode, Rval, !CI),
        MaybeForeign = get_maybe_foreign_type_info(!.CI, OrigType),
        code_info.get_module_info(!.CI, ModuleInfo),
        ( is_dummy_argument_type(ModuleInfo, VarType) ->
            IsDummy = yes
        ;
            IsDummy = no
        ),
        Input = foreign_proc_input(Name, VarType, IsDummy, OrigType, Rval,
            MaybeForeign, BoxPolicy),
        get_foreign_proc_input_vars(Args, Inputs1, CanOptAwayUnnamedArgs,
            RestCode, !CI),
        Inputs = [Input | Inputs1],
        Code = tree(FirstCode, RestCode)
    ;
        MaybeName = no,
        % Just ignore the argument.
        get_foreign_proc_input_vars(Args, Inputs, CanOptAwayUnnamedArgs, Code,
            !CI)
    ).

:- func get_maybe_foreign_type_info(code_info, mer_type) =
    maybe(foreign_proc_type).

get_maybe_foreign_type_info(CI, Type) = MaybeForeignTypeInfo :-
    code_info.get_module_info(CI, Module),
    module_info_get_type_table(Module, Types),
    (
        type_to_ctor_and_args(Type, TypeId, _SubTypes),
        map.search(Types, TypeId, Defn),
        hlds_data.get_type_defn_body(Defn, Body),
        Body = hlds_foreign_type(
            foreign_type_body(_MaybeIL, MaybeC, _MaybeJava))
    ->
        (
            MaybeC = yes(Data),
            Data = foreign_type_lang_data(c_type(Name), _, Assertions),
            MaybeForeignTypeInfo = yes(foreign_proc_type(Name, Assertions))
        ;
            MaybeC = no,
            % This is ensured by check_foreign_type in make_hlds.
            unexpected(this_file,
                "get_maybe_foreign_type_name: no c foreign type")
        )
    ;
        MaybeForeignTypeInfo = no
    ).

%---------------------------------------------------------------------------%

    % foreign_proc_acquire_regs acquires a list of registers in which to place
    % each of the given arguments.
    %
:- pred foreign_proc_acquire_regs(list(c_arg)::in, list(lval)::out,
    code_info::in, code_info::out) is det.

foreign_proc_acquire_regs([], [], !CI).
foreign_proc_acquire_regs([Arg | Args], [Reg | Regs], !CI) :-
    Arg = c_arg(Var, _, _, _, _),
    code_info.acquire_reg_for_var(Var, Reg, !CI),
    foreign_proc_acquire_regs(Args, Regs, !CI).

%---------------------------------------------------------------------------%

    % place_foreign_proc_output_args_in_regs returns a list of
    % foreign_proc_outputs, which are pairs of names of output registers
    % and (C) variables which hold the output value.
    %
:- pred place_foreign_proc_output_args_in_regs(list(c_arg)::in, list(lval)::in,
    bool::in, list(foreign_proc_output)::out, code_info::in, code_info::out)
    is det.

place_foreign_proc_output_args_in_regs([], [], _, [], !CI).
place_foreign_proc_output_args_in_regs([Arg | Args], [Reg | Regs],
        CanOptAwayUnnamedArgs, Outputs, !CI) :-
    place_foreign_proc_output_args_in_regs(Args, Regs, CanOptAwayUnnamedArgs,
        OutputsTail, !CI),
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, _ArgInfo),
    code_info.release_reg(Reg, !CI),
    ( code_info.variable_is_forward_live(!.CI, Var) ->
        code_info.set_var_location(Var, Reg, !CI),
        MaybeForeign = get_maybe_foreign_type_info(!.CI, OrigType),
        MaybeName = var_should_be_passed(CanOptAwayUnnamedArgs, Var,
            MaybeArgName),
        (
            MaybeName = yes(Name),
            code_info.get_module_info(!.CI, ModuleInfo),
            VarType = variable_type(!.CI, Var),
            ( is_dummy_argument_type(ModuleInfo, VarType) ->
                IsDummy = yes
            ;
                IsDummy = no
            ),
            PragmaCOutput = foreign_proc_output(Reg, VarType, IsDummy,
                OrigType, Name, MaybeForeign, BoxPolicy),
            Outputs = [PragmaCOutput | OutputsTail]
        ;
            MaybeName = no,
            Outputs = OutputsTail
        )
    ;
        Outputs = OutputsTail
    ).
place_foreign_proc_output_args_in_regs([_ | _], [], _, _, !CI) :-
    unexpected(this_file,
        "place_foreign_proc_output_args_in_regs: length mismatch").
place_foreign_proc_output_args_in_regs([], [_ | _], _, _, !CI) :-
    unexpected(this_file,
        "place_foreign_proc_output_args_in_regs: length mismatch").

%---------------------------------------------------------------------------%

    % input_descs_from_arg_info returns a list of foreign_proc_inputs, which
    % are pairs of rvals and (C) variables which receive the input value.
    %
:- pred input_descs_from_arg_info(code_info::in, list(c_arg)::in,
    bool::in, list(foreign_proc_input)::out) is det.

input_descs_from_arg_info(_, [], _, []).
input_descs_from_arg_info(CI, [Arg | Args], CanOptAwayUnnamedArgs, Inputs) :-
    input_descs_from_arg_info(CI, Args, CanOptAwayUnnamedArgs, InputsTail),
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, ArgInfo),
    MaybeName = var_should_be_passed(CanOptAwayUnnamedArgs, Var, MaybeArgName),
    (
        MaybeName = yes(Name),
        VarType = variable_type(CI, Var),
        ArgInfo = arg_info(N, _),
        Reg = reg(reg_r, N),
        MaybeForeign = get_maybe_foreign_type_info(CI, OrigType),
        code_info.get_module_info(CI, ModuleInfo),
        ( is_dummy_argument_type(ModuleInfo, VarType) ->
            IsDummy = yes
        ;
            IsDummy = no
        ),
        Input = foreign_proc_input(Name, VarType, IsDummy, OrigType, lval(Reg),
            MaybeForeign, BoxPolicy),
        Inputs = [Input | InputsTail]
    ;
        MaybeName = no,
        Inputs = InputsTail
    ).

%---------------------------------------------------------------------------%

    % output_descs_from_arg_info returns a list of foreign_proc_outputs, which
    % are pairs of names of output registers and (C) variables which hold the
    % output value.
    %
:- pred output_descs_from_arg_info(code_info::in, list(c_arg)::in,
    bool::in, list(foreign_proc_output)::out) is det.

output_descs_from_arg_info(_, [], _, []).
output_descs_from_arg_info(CI, [Arg | Args], CanOptAwayUnnamedArgs, Outputs) :-
    output_descs_from_arg_info(CI, Args, CanOptAwayUnnamedArgs, OutputsTail),
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, ArgInfo),
    MaybeName = var_should_be_passed(CanOptAwayUnnamedArgs, Var, MaybeArgName),
    (
        MaybeName = yes(Name),
        VarType = variable_type(CI, Var),
        ArgInfo = arg_info(N, _),
        Reg = reg(reg_r, N),
        MaybeForeign = get_maybe_foreign_type_info(CI, OrigType),
        code_info.get_module_info(CI, ModuleInfo),
        ( is_dummy_argument_type(ModuleInfo, VarType) ->
            IsDummy = yes
        ;
            IsDummy = no
        ),
        Output = foreign_proc_output(Reg, VarType, IsDummy, OrigType, Name,
            MaybeForeign, BoxPolicy),
        Outputs = [Output | OutputsTail]
    ;
        MaybeName = no,
        Outputs = OutputsTail
    ).

%---------------------------------------------------------------------------%

foreign_proc_struct_name(ModuleName, PredName, Arity, ProcId) =
    "mercury_save__" ++ sym_name_mangle(ModuleName) ++ "__" ++
        name_mangle(PredName) ++ "__" ++ int_to_string(Arity) ++ "_" ++
        int_to_string(proc_id_to_int(ProcId)).

foreign_proc_succ_ind_name = "MercurySuccessIndicator".

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "pragma_c_gen.m".

%---------------------------------------------------------------------------%
:- end_module pragma_c_gen.
%---------------------------------------------------------------------------%
