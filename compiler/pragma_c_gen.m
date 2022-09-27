%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
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

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred generate_code_for_foreign_proc(code_model::in,
    pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    list(foreign_arg)::in, list(foreign_arg)::in,
    maybe(trace_expr(trace_runtime))::in, pragma_foreign_proc_impl::in,
    hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- func foreign_proc_struct_name(module_name, string, int, proc_id) = string.

    % The name of the variable model_semi foreign_procs in C assign to
    % to indicate success or failure. Exported for llds_out.m.
    %
:- func foreign_proc_succ_ind_name = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.name_mangle.
:- import_module check_hlds.
:- import_module check_hlds.mode_top_functor.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

% The code we generate for an ordinary (model_det or model_semi)
% call_foreign_proc goal must be able to fit into the middle of a procedure,
% since such call_foreign_proc goals can be inlined. This code is of the
% following form:
%
%   <save live variables onto the stack> /* see note (1) below */
%   {
%       <declaration of one local variable for each arg>
%       #define MR_ALLOC_ID <allocation id> /* see note (5) below */
%       #define MR_PROC_LABEL <procedure label>
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
%       #undef MR_ALLOC_ID /* see note (5) below */
%       #undef MR_PROC_LABEL
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
%   #define MR_ALLOC_ID <allocation id> /* see note (5) below */
%   #define MR_PROC_LABEL <procedure label>
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
%   #undef MR_ALLOC_ID /* see note (5) below */
%   #undef MR_PROC_LABEL
% }
% MR_define_label(xxx_i1)
% <code for entry to a later disjunct>
% {
%   <declaration of one local variable for each output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment to save struct pointer>
%   MR_save_registers(); /* see notes (1) and (2) below */
%   #define MR_ALLOC_ID <allocation id> /* see note (5) below */
%   #define MR_PROC_LABEL <procedure label>
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
%   #undef MR_ALLOC_ID /* see note (5) below */
%   #undef MR_PROC_LABEL
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
%   #define MR_ALLOC_ID <allocation id> /* see note (5) below */
%   #define MR_PROC_LABEL <procedure label>
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
%   #undef MR_ALLOC_ID /* see note (5) below */
%   #undef MR_PROC_LABEL
% }
% MR_define_label(xxx_i1)
% <code for entry to a later disjunct>
% {
%   <declaration of one local variable for each output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment to save struct pointer>
%   MR_save_registers(); /* see notes (1) and (2) below */
%   #define MR_ALLOC_ID <allocation id> /* see note (5) below */
%   #define MR_PROC_LABEL <procedure label>
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
%   #undef MR_ALLOC_ID /* see note (5) below */
%   #undef MR_PROC_LABEL
% }
% MR_define_label(xxx_i2)
% {
%   <declaration of one local variable for each output arg>
%   <declaration of one local variable to point to save struct>
%   <assignment to save struct pointer>
%   #define MR_ALLOC_ID <allocation id> /* see note (5) below */
%   #define MR_PROC_LABEL <procedure label>
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
%   #undef MR_ALLOC_ID /* see note (5) below */
%   #undef MR_PROC_LABEL
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
% which is *after* the call to generate_code_for_foreign_proc, the prolog will
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
% 5 We insert a #define for MR_ALLOC_ID so that the C code that allocates
%   memory manually can use MR_ALLOC_ID as an argument to incr_hp_msg(), for
%   memory profiling. It replaces an older macro MR_PROC_LABEL, which is
%   retained only for backwards compatibility.

%---------------------------------------------------------------------------%

generate_code_for_foreign_proc(CodeModel, Attributes, PredId, ProcId,
        Args, ExtraArgs, MaybeTraceRuntimeCond, PragmaImpl, GoalInfo, Code,
        !CI, !CLD) :-
    PragmaImpl = fp_impl_ordinary(C_Code, Context),
    (
        MaybeTraceRuntimeCond = no,
        generate_ordinary_foreign_proc_code(CodeModel, Attributes,
            PredId, ProcId, Args, ExtraArgs, C_Code, Context, GoalInfo,
            Code, !CI, !CLD)
    ;
        MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
        expect(unify(Args, []), $pred, "args runtime cond"),
        expect(unify(ExtraArgs, []), $pred, "extra args runtime cond"),
        expect(unify(CodeModel, model_semi), $pred, "non-semi runtime cond"),
        generate_trace_runtime_cond_foreign_proc_code(TraceRuntimeCond,
            Code, !CI, !CLD)
    ).

%---------------------------------------------------------------------------%

:- pred generate_trace_runtime_cond_foreign_proc_code(
    trace_expr(trace_runtime)::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_trace_runtime_cond_foreign_proc_code(RuntimeExpr, Code, !CI, !CLD) :-
    generate_runtime_cond_code(RuntimeExpr, CondRval, !CI),
    get_next_label(SuccessLabel, !CI),
    remember_position(!.CLD, BeforeFailure),
    generate_failure(FailCode, !CI, !.CLD),
    reset_to_position(BeforeFailure, !.CI, !:CLD),

    CondCode = singleton(
        llds_instr(if_val(CondRval, code_label(SuccessLabel)),
            "environment variable tests")
    ),
    SuccessLabelCode = singleton(
        llds_instr(label(SuccessLabel),
            "environment variable tests successful")
    ),
    Code = CondCode ++ FailCode ++ SuccessLabelCode.

:- pred generate_runtime_cond_code(trace_expr(trace_runtime)::in,
    rval::out, code_info::in, code_info::out) is det.

generate_runtime_cond_code(Expr, CondRval, !CI) :-
    (
        Expr = trace_base(trace_envvar(EnvVar)),
        get_used_env_vars(!.CI, UsedEnvVars0),
        set.insert(EnvVar, UsedEnvVars0, UsedEnvVars),
        set_used_env_vars(UsedEnvVars, !CI),
        EnvVarRval = lval(global_var_ref(env_var_ref(EnvVar))),
        ZeroRval = const(llconst_int(0)),
        CondRval = binop(ne(int_type_int), EnvVarRval, ZeroRval)
    ;
        Expr = trace_not(ExprA),
        generate_runtime_cond_code(ExprA, RvalA, !CI),
        CondRval = unop(logical_not, RvalA)
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
    maybe(prog_context)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_ordinary_foreign_proc_code(CodeModel, Attributes, PredId, ProcId,
        Args, ExtraArgs, C_Code, Context, GoalInfo, Code, !CI, !CLD) :-
    % First we need to get a list of input and output arguments.
    ArgInfos = get_pred_proc_arginfo(!.CI, PredId, ProcId),
    make_c_arg_list(Args, ArgInfos, OrigCArgs),
    get_module_info(!.CI, ModuleInfo),
    make_extra_c_arg_list(ExtraArgs, ModuleInfo, ArgInfos, ExtraCArgs),
    list.append(OrigCArgs, ExtraCArgs, CArgs),
    foreign_proc_select_in_out_args(CArgs, InCArgs, OutCArgs),

    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    DeadVars0 = set_of_var.init,
    find_dead_input_vars(InCArgs, PostDeaths, DeadVars0, DeadVars),

    % Generate code to <save live variables on stack>.
    MayCallMercury = get_may_call_mercury(Attributes),
    (
        MayCallMercury = proc_will_not_call_mercury,
        SaveVarsCode = cord.empty
    ;
        MayCallMercury = proc_may_call_mercury,
        % The C code might call back Mercury code which clobbers the succip.
        succip_is_used(!CI),

        % The C code might call back Mercury code which clobbers the
        % other registers, so we need to save any live variables
        % (other than the output args) onto the stack.
        get_c_arg_list_vars(OutCArgs, OutVars),
        set_of_var.list_to_set(OutVars, OutVarsSet),
        save_variables(OutVarsSet, _, SaveVarsCode, !.CI, !CLD)
    ),

    % Generate the values of input variables.
    % (NB we need to be careful that the rvals generated here
    % remain valid below.)
    get_foreign_proc_input_vars(InCArgs, InputDescs, InputVarsCode,
        !.CI, !CLD),

    % We cannot kill the forward dead input arguments until we have
    % finished generating the code producing the input variables.
    % (The forward dead variables will be dead after the call_foreign_proc,
    % but are live during its input phase.)
    make_vars_forward_dead(DeadVars, !CLD),

    % Generate <declaration of one local variable for each arg>.
    make_foreign_proc_decls(!.CI, C_Code, 1, 1, CArgs, Decls,
        TICopyIns, TICopyOuts),

    % Generate #define MR_ALLOC_ID and #undef MR_ALLOC_ID /* see note (5) */
    make_alloc_id_hash_define(C_Code, Context, AllocIdHashDefine,
        AllocIdHashUndef, !CI),

    % Generate #define MR_PROC_LABEL and #undef MR_PROC_LABEL
    % for backwards compatibility with older hand-written code.
    get_pred_id(!.CI, CallerPredId),
    get_proc_id(!.CI, CallerProcId),
    make_proc_label_hash_define(ModuleInfo, CallerPredId, CallerProcId,
        ProcLabelHashDefine, ProcLabelHashUndef),

    % <assignment of input values from registers to local vars>
    InputComp = foreign_proc_inputs(InputDescs),

    string.append_list(TICopyIns, TICopyInStr),
    string.append_list(TICopyOuts, TICopyOutStr),
    TICopyInComp = foreign_proc_raw_code(cannot_branch_away,
        proc_does_not_affect_liveness, live_lvals_info(set.init),
        TICopyInStr),
    TICopyOutComp = foreign_proc_raw_code(cannot_branch_away,
        proc_does_not_affect_liveness, live_lvals_info(set.init),
        TICopyOutStr),

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
    ThreadSafe = get_thread_safe(Attributes),
    (
        ThreadSafe = proc_thread_safe,
        ObtainLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), ""),
        ReleaseLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), "")
    ;
        ThreadSafe = proc_maybe_thread_safe,
        % The maybe_thread_safe attribute should have been changed
        % to the real value by now.
        unexpected($pred, "maybe_thread_safe")
    ;
        ThreadSafe = proc_not_thread_safe,
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        MangledName = quote_string_c(Name),
        ObtainLockStr = "\tMR_OBTAIN_GLOBAL_LOCK(" ++ MangledName ++ ");\n",
        ObtainLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            ObtainLockStr),
        ReleaseLockStr = "\tMR_RELEASE_GLOBAL_LOCK(" ++ MangledName ++ ");\n",
        ReleaseLock = foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            ReleaseLockStr)
    ),

    % <The C code itself>
    AffectsLiveness = get_affects_liveness(Attributes),
    C_Code_Comp = foreign_proc_user_code(Context, AffectsLiveness, C_Code),

    % <for semidet code, check of SUCCESS_INDICATOR>
    Detism = goal_info_get_determinism(GoalInfo),
    (
        CodeModel = model_semi,
        ( if Detism = detism_failure then
            CheckSuccess_Comp = foreign_proc_noop,
            MaybeFailLabel = no
        else
            get_next_label(FailLabel, !CI),
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
        % We generate model_non foreign_procs for fact tables.
        ( CodeModel = model_det
        ; CodeModel = model_non
        ),
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
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        ( if instmap_delta_is_reachable(InstMapDelta) then
            OkToDelete = no
        else
            OkToDelete = yes
        ),
        clear_all_registers(OkToDelete, !CLD)
    ),

    % <assignment of the output values from local variables to registers>
    get_exprn_opts(!.CI, ExprnOpts),
    UseFloatRegs = get_float_registers(ExprnOpts),
    (
        UseFloatRegs = use_float_registers,
        FloatRegType = reg_f
    ;
        UseFloatRegs = do_not_use_float_registers,
        FloatRegType = reg_r
    ),
    foreign_proc_acquire_regs(FloatRegType, OutCArgs, Regs, !CLD),
    place_foreign_proc_output_args_in_regs(OutCArgs, Regs, OutputDescs,
        !.CI, !CLD),
    OutputComp = foreign_proc_outputs(OutputDescs),

    % Join all the components of the foreign_proc_code together.
    Components = [ProcLabelHashDefine | AllocIdHashDefine] ++
        [DefSuccessComp, InputComp, TICopyInComp, SaveRegsComp,
        ObtainLock, C_Code_Comp, ReleaseLock,
        CheckSuccess_Comp, RestoreRegsComp, TICopyOutComp,
        OutputComp, UndefSuccessComp,
        ProcLabelHashUndef | AllocIdHashUndef],
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
    ( if list.member(refers_to_llds_stack, ExtraAttributes) then
        RefersToLLDSSTack = yes
    else
        RefersToLLDSSTack = no
    ),
    PragmaCCode = singleton(
        llds_instr(foreign_proc_code(Decls, Components, MayCallMercury,
            no, no, no, MaybeFailLabel, no, RefersToLLDSSTack, MayDupl),
            "foreign_proc inclusion")
    ),

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

    (
        MaybeFailLabel = yes(TheFailLabel),
        get_next_label(SkipLabel, !CI),
        remember_position(!.CLD, BeforeFailure),
        generate_failure(FailCode, !CI, !.CLD),
        reset_to_position(BeforeFailure, !.CI, !:CLD),

        GotoSkipLabelCode = singleton(
            llds_instr(goto(code_label(SkipLabel)), "Skip past failure code")
        ),
        SkipLabelCode = singleton(
            llds_instr(label(SkipLabel), "")
        ),
        FailLabelCode = singleton(
            llds_instr(label(TheFailLabel), "")
        ),
        FailureCode = GotoSkipLabelCode ++ FailLabelCode ++ FailCode ++
            SkipLabelCode
    ;
        MaybeFailLabel = no,
        ( if Detism = detism_failure then
            remember_position(!.CLD, BeforeFailure),
            generate_failure(FailureCode, !CI, !.CLD),
            reset_to_position(BeforeFailure, !.CI, !:CLD)
        else
            FailureCode = cord.empty
        )
    ),

    % Join all code fragments together.
    Code = SaveVarsCode ++ InputVarsCode ++ PragmaCCode ++ FailureCode.

:- pred make_proc_label_hash_define(module_info::in, pred_id::in, proc_id::in,
    foreign_proc_component::out, foreign_proc_component::out) is det.

make_proc_label_hash_define(ModuleInfo, PredId, ProcId,
        ProcLabelHashDef, ProcLabelHashUndef) :-
    ProcLabelStr = make_proc_label_string(ModuleInfo, PredId, ProcId),
    ProcLabelHashDef = simple_foreign_proc_raw_code(
        "#define\tMR_PROC_LABEL\t" ++ ProcLabelStr ++ "\n"),
    ProcLabelHashUndef = simple_foreign_proc_raw_code(
        "#undef\tMR_PROC_LABEL\n").

:- func make_proc_label_string(module_info, pred_id, proc_id) = string.

make_proc_label_string(ModuleInfo, PredId, ProcId) = ProcLabelString :-
    CodeAddr = make_entry_label(ModuleInfo, PredId, ProcId,
        for_from_everywhere),
    ( if CodeAddr = code_imported_proc(ProcLabel) then
        ProcLabelString = proc_label_to_c_string(add_label_prefix, ProcLabel)
    else if CodeAddr = code_label(Label) then
        ProcLabelString = label_to_c_string(add_label_prefix, Label)
    else
        unexpected($pred, "code_addr")
    ).

:- pred make_alloc_id_hash_define(string::in, maybe(prog_context)::in,
    list(foreign_proc_component)::out, list(foreign_proc_component)::out,
    code_info::in, code_info::out) is det.

make_alloc_id_hash_define(C_Code, MaybeContext,
        AllocIdHashDefine, AllocIdHashUndef, !CI) :-
    code_info.get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, profile_memory, ProfileMemory),
    ( if
        ProfileMemory = yes,
        string.sub_string_search(C_Code, "MR_ALLOC_ID", _)
    then
        (
            MaybeContext = yes(Context)
        ;
            MaybeContext = no,
            Context = dummy_context
        ),
        add_alloc_site_info(Context, "unknown", 0, AllocId, !CI),
        AllocIdHashDefine = [
            simple_foreign_proc_raw_code("#define\tMR_ALLOC_ID\t"),
            foreign_proc_alloc_id(AllocId),
            simple_foreign_proc_raw_code("\n")
        ],
        AllocIdHashUndef = [
            simple_foreign_proc_raw_code("#undef\tMR_ALLOC_ID\n")
        ]
    else
        AllocIdHashDefine = [],
        AllocIdHashUndef = []
    ).

:- func simple_foreign_proc_raw_code(string) = foreign_proc_component.

simple_foreign_proc_raw_code(Code) =
    foreign_proc_raw_code(cannot_branch_away, proc_does_not_affect_liveness,
        live_lvals_info(set.init), Code).

%-----------------------------------------------------------------------------%

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
make_c_arg_list([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
make_c_arg_list([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
make_c_arg_list([Arg | ArgTail], [ArgInfo | ArgInfoTail], [CArg | CArgTail]) :-
    Arg = foreign_arg(Var, MaybeNameMode, Type, BoxPolicy),
    (
        MaybeNameMode = yes(foreign_arg_name_mode(Name, _)),
        MaybeName = yes(Name)
    ;
        MaybeNameMode = no,
        MaybeName = no
    ),
    CArg = c_arg(Var, MaybeName, Type, BoxPolicy, ArgInfo),
    make_c_arg_list(ArgTail, ArgInfoTail, CArgTail).

%---------------------------------------------------------------------------%

:- pred make_extra_c_arg_list(list(foreign_arg)::in, module_info::in,
    list(arg_info)::in, list(c_arg)::out) is det.

make_extra_c_arg_list(ExtraArgs, ModuleInfo, ArgInfos, ExtraCArgs) :-
    get_highest_arg_num(ArgInfos, 0, MaxR, 0, _MaxF),
    make_extra_c_arg_list_seq(ExtraArgs, ModuleInfo, MaxR, ExtraCArgs).

:- pred get_highest_arg_num(list(arg_info)::in, int::in, int::out,
    int::in, int::out) is det.

get_highest_arg_num([], !MaxR, !MaxF).
get_highest_arg_num([arg_info(Loc, _) | ArgInfos], !MaxR, !MaxF) :-
    (
        Loc = reg(reg_r, RegNum),
        int.max(RegNum, !MaxR)
    ;
        Loc = reg(reg_f, RegNum),
        int.max(RegNum, !MaxF)
    ),
    get_highest_arg_num(ArgInfos, !MaxR, !MaxF).

:- pred make_extra_c_arg_list_seq(list(foreign_arg)::in, module_info::in,
    int::in, list(c_arg)::out) is det.

make_extra_c_arg_list_seq([], _, _, []).
make_extra_c_arg_list_seq([ExtraArg | ExtraArgs], ModuleInfo, LastReg,
        [CArg | CArgs]) :-
    ExtraArg = foreign_arg(Var, MaybeNameMode, OrigType, BoxPolicy),
    (
        MaybeNameMode = yes(foreign_arg_name_mode(Name, Mode)),
        mode_to_top_functor_mode(ModuleInfo, Mode, OrigType, TopFunctorMode)
    ;
        MaybeNameMode = no,
        unexpected($pred, "no name")
    ),
    % Extra args are always input, and passed in regular registers.
    RegType = reg_r,
    NextReg = LastReg + 1,
    ArgInfo = arg_info(reg(RegType, NextReg), TopFunctorMode),
    CArg = c_arg(Var, yes(Name), OrigType, BoxPolicy, ArgInfo),
    make_extra_c_arg_list_seq(ExtraArgs, ModuleInfo, NextReg, CArgs).

%---------------------------------------------------------------------------%

:- pred get_c_arg_list_vars(list(c_arg)::in, list(prog_var)::out) is det.

get_c_arg_list_vars([], []).
get_c_arg_list_vars([Arg | Args], [Var | Vars]) :-
    Arg = c_arg(Var, _, _, _, _),
    get_c_arg_list_vars(Args, Vars).

%---------------------------------------------------------------------------%

    % foreign_proc_select_in_out_args returns
    %
    % - the list of variables which are inputs for a procedure; and
    % - the list of variables which are outputs for a procedure.
    %
:- pred foreign_proc_select_in_out_args(list(c_arg)::in,
    list(c_arg)::out, list(c_arg)::out) is det.

foreign_proc_select_in_out_args([], [], []).
foreign_proc_select_in_out_args([Arg | Args], InArgs, OutArgs) :-
    foreign_proc_select_in_out_args(Args, TailInArgs, TailOutArgs),
    Arg = c_arg(_, _, _, _, ArgInfo),
    ArgInfo = arg_info(_Loc, Mode),
    (
        Mode = top_in,
        InArgs = [Arg | TailInArgs],
        OutArgs = TailOutArgs
    ;
        Mode = top_out,
        InArgs = TailInArgs,
        OutArgs = [Arg | TailOutArgs]
    ;
        Mode = top_unused,
        InArgs = TailInArgs,
        OutArgs = TailOutArgs
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
:- func var_should_be_passed(maybe(string)) = maybe(string).

var_should_be_passed(MaybeName) = MaybeUseName :-
    ( if
        MaybeName = yes(Name),
        not string.first_char(Name, '_', _)
    then
        MaybeUseName = yes(Name)
    else
        MaybeUseName = no
    ).

%---------------------------------------------------------------------------%

    % make_foreign_proc_decls returns the list of foreign_proc_decls for the
    % foreign_proc_code instruction in the LLDS. It is essentially a list of
    % pairs of type and variable name, so that declarations of the form
    % "Type Name;" can be made.
    %
:- pred make_foreign_proc_decls(code_info::in, string::in, int::in, int::in,
    list(c_arg)::in, list(foreign_proc_decl)::out,
    list(string)::out, list(string)::out) is det.

make_foreign_proc_decls(_, _, _, _, [], [], [], []).
make_foreign_proc_decls(CI, Code, !.TIIn, !.TIOut,
        [Arg | Args], Decls, TICopyIns, TICopyOuts) :-
    make_foreign_proc_decl(CI, Code, !TIIn, !TIOut,
        Arg, HeadDecls, HeadTICopyIns, HeadTICopyOuts),
    make_foreign_proc_decls(CI, Code, !.TIIn, !.TIOut,
        Args, TailDecls, TailTICopyIns, TailTICopyOuts),
    Decls = HeadDecls ++ TailDecls,
    TICopyIns = HeadTICopyIns ++ TailTICopyIns,
    TICopyOuts = HeadTICopyOuts ++ TailTICopyOuts.

:- pred make_foreign_proc_decl(code_info::in, string::in,
    int::in, int::out, int::in, int::out,
    c_arg::in, list(foreign_proc_decl)::out,
    list(string)::out, list(string)::out) is det.

make_foreign_proc_decl(CI, Code, !TIIn, !TIOut, Arg, Decls,
        TICopyIns, TICopyOuts) :-
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, ArgInfo),
    MaybeUseArgName = var_should_be_passed(MaybeArgName),
    (
        MaybeUseArgName = yes(ArgName),
        get_module_info(CI, ModuleInfo),
        (
            BoxPolicy = bp_native_if_possible,
            OrigTypeString = exported_type_to_c_string(ModuleInfo, OrigType)
        ;
            BoxPolicy = bp_always_boxed,
            OrigTypeString = "MR_Word"
        ),
        ArgDecl = foreign_proc_arg_decl(OrigType, OrigTypeString, ArgName),
        ( if is_comp_gen_type_info_arg(CI, Var, ArgName) then
            % For a transition period, we make compiler-generate type_info
            % arguments visible in Code in two variables:
            %
            % - ArgName, whose name is given by the HLDS, which (for now)
            %   uses the old naming scheme (TypeInfo_for_<TypeVarName>), and
            %
            % - SeqArgName, whose name is given by the new naming scheme
            %   (TypeInfo_{In,Out}_<SeqNum>).
            ArgInfo = arg_info(_Loc, Mode),
            (
                (
                    Mode = top_in,
                    string.format("TypeInfo_In_%d", [i(!.TIIn)], SeqArgName),
                    !:TIIn = !.TIIn + 1,
                    % For inputs, get_foreign_proc_input_vars defines
                    % the name given in the HLDS, i.e. ArgName, so we copy
                    % ArgName to SeqArgName.
                    string.format("\t%s = %s;\n", [s(SeqArgName), s(ArgName)],
                        TICopyIn),
                    TICopyIns = [TICopyIn],
                    TICopyOuts = []
                ;
                    Mode = top_out,
                    string.format("TypeInfo_Out_%d", [i(!.TIOut)], SeqArgName),
                    !:TIOut = !.TIOut + 1,
                    % For outputs, the value is given by Code. Before the
                    % transition to the new scheme, variable names following
                    % the new naming scheme should not appear in Code.
                    % (Theoretically, they could, but none appear in *our* code,
                    % and if they appear in anyone else's, they qualify as
                    % implementors, which means they are on their own.)
                    % So in this case, we assign the ArgName computed by Code
                    % to SeqArgName, so that a later version of the compiler
                    % could take the value of the corresponding HLDS variable
                    % from there.
                    %
                    % After Code has been updated to assign to SeqArgName,
                    % we assign it to ArgName, because the later version of
                    % the compiler mentioned above may not have arrived yet.
                    % (For now, updated code will still have to mention ArgName,
                    % probably in a comment, to avoid a singleton variable
                    % warning from report_missing_tvar_in_foreign_code.)
                    ( if string.sub_string_search(Code, SeqArgName, _) then
                        % SeqArgName occurs in Code, so assign it to ArgName.
                        string.format("\t%s = %s;\n",
                            [s(ArgName), s(SeqArgName)], TICopyOut)
                    else
                        % SeqArgName does not occur in Code, so assign ArgName
                        % to it.
                        string.format("\t%s = %s;\n",
                            [s(SeqArgName), s(ArgName)], TICopyOut)
                    ),
                    TICopyIns = [],
                    TICopyOuts = [TICopyOut]
                ),
                SeqArgDecl = foreign_proc_arg_decl(OrigType, OrigTypeString,
                    SeqArgName),
                Decls = [ArgDecl, SeqArgDecl]
            ;
                Mode = top_unused,
                % Just in case Code refers to it *despite* the mode.
                Decls = [ArgDecl],
                TICopyIns = [],
                TICopyOuts = []
            )
        else
            Decls = [ArgDecl],
            TICopyIns = [],
            TICopyOuts = []
        )
    ;
        MaybeUseArgName = no,
        Decls = [],
        TICopyIns = [],
        TICopyOuts = []
    ).

%---------------------------------------------------------------------------%

:- pred find_dead_input_vars(list(c_arg)::in, set_of_progvar::in,
    set_of_progvar::in, set_of_progvar::out) is det.

find_dead_input_vars([], _, !DeadVars).
find_dead_input_vars([Arg | Args], PostDeaths, !DeadVars) :-
    Arg = c_arg(Var, _MaybeName, _Type, _BoxPolicy, _ArgInfo),
    ( if set_of_var.member(PostDeaths, Var) then
        set_of_var.insert(Var, !DeadVars)
    else
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
    list(foreign_proc_input)::out, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

get_foreign_proc_input_vars([], [], cord.empty, _CI, !CLD).
get_foreign_proc_input_vars([Arg | Args], Inputs, Code, CI, !CLD) :-
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, _ArgInfo),
    MaybeName = var_should_be_passed(MaybeArgName),
    (
        MaybeName = yes(Name),
        get_module_info(CI, ModuleInfo),
        VarType = variable_type(CI, Var),
        IsDummy = is_type_a_dummy(ModuleInfo, VarType),
        (
            IsDummy = is_not_dummy_type,
            produce_variable(Var, HeadCode, Rval, !CLD)
        ;
            IsDummy = is_dummy_type,
            % The variable may not have a state.
            HeadCode = cord.empty,
            Rval = const(llconst_int(0))
        ),
        MaybeForeign = get_maybe_foreign_type_info(ModuleInfo, OrigType),
        HeadInput = foreign_proc_input(Name, VarType, IsDummy, OrigType,
            Rval, MaybeForeign, BoxPolicy),
        get_foreign_proc_input_vars(Args, TailInputs, TailCode, CI, !CLD),
        Inputs = [HeadInput | TailInputs],
        Code = HeadCode ++ TailCode
    ;
        MaybeName = no,
        % Just ignore the argument.
        get_foreign_proc_input_vars(Args, Inputs, Code, CI, !CLD)
    ).

:- func get_maybe_foreign_type_info(module_info, mer_type) =
    maybe(foreign_proc_type).

get_maybe_foreign_type_info(ModuleInfo, Type) = MaybeForeignTypeInfo :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    ( if
        type_to_ctor(Type, TypeCtor),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_foreign_type(
            foreign_type_body(MaybeC, _MaybeJava, _MaybeCSharp))
    then
        (
            MaybeC = yes(Data),
            Data = type_details_foreign(c_type(Name), _, Assertions),
            MaybeForeignTypeInfo = yes(foreign_proc_type(Name, Assertions))
        ;
            MaybeC = no,
            % This is ensured by check_foreign_type in make_hlds.
            unexpected($pred, "no c foreign type")
        )
    else
        MaybeForeignTypeInfo = no
    ).

:- pred is_comp_gen_type_info_arg(code_info::in, prog_var::in,
    string::in) is semidet.

is_comp_gen_type_info_arg(CI, Var, ArgName) :-
    % This predicate and ml_is_comp_gen_type_info_arg should be kept in sync.
    string.prefix(ArgName, "TypeInfo_for_"),
    get_var_table(CI, VarTable),
    lookup_var_entry(VarTable, Var, Entry),
    Entry ^ vte_type = defined_type(TypeCtorSymName, [], kind_star),
    TypeCtorSymName = qualified(TypeCtorModuleName, TypeCtorName),
    TypeCtorModuleName = mercury_private_builtin_module,
    TypeCtorName = "type_info".

%---------------------------------------------------------------------------%

    % foreign_proc_acquire_regs acquires a list of registers in which to place
    % each of the given arguments.
    %
:- pred foreign_proc_acquire_regs(reg_type::in,
    list(c_arg)::in, list(lval)::out,
    code_loc_dep::in, code_loc_dep::out) is det.

foreign_proc_acquire_regs(_, [], [], !CLD).
foreign_proc_acquire_regs(FloatRegType, [Arg | Args], [Reg | Regs], !CLD) :-
    Arg = c_arg(Var, _, VarType, BoxPolicy, _),
    foreign_proc_arg_reg_type(FloatRegType, VarType, BoxPolicy, RegType),
    acquire_reg_for_var(Var, RegType, Reg, !CLD),
    foreign_proc_acquire_regs(FloatRegType, Args, Regs, !CLD).

:- pred foreign_proc_arg_reg_type(reg_type::in, mer_type::in, box_policy::in,
    reg_type::out) is det.

foreign_proc_arg_reg_type(FloatRegType, VarType, BoxPolicy, RegType) :-
    (
        BoxPolicy = bp_native_if_possible,
        ( if VarType = float_type then
            RegType = FloatRegType
        else
            RegType = reg_r
        )
    ;
        BoxPolicy = bp_always_boxed,
        RegType = reg_r
    ).

%---------------------------------------------------------------------------%

    % place_foreign_proc_output_args_in_regs returns a list of
    % foreign_proc_outputs, which are pairs of names of output registers
    % and (C) variables which hold the output value.
    %
:- pred place_foreign_proc_output_args_in_regs(list(c_arg)::in, list(lval)::in,
    list(foreign_proc_output)::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

place_foreign_proc_output_args_in_regs([], [], [], _CI, !CLD).
place_foreign_proc_output_args_in_regs([_ | _], [], _, _CI, !CLD) :-
    unexpected($pred, "length mismatch").
place_foreign_proc_output_args_in_regs([], [_ | _], _, _CI, !CLD) :-
    unexpected($pred, "length mismatch").
place_foreign_proc_output_args_in_regs([Arg | Args], [Reg | Regs],
        Outputs, CI, !CLD) :-
    place_foreign_proc_output_args_in_regs(Args, Regs,
        OutputsTail, CI, !CLD),
    Arg = c_arg(Var, MaybeArgName, OrigType, BoxPolicy, _ArgInfo),
    release_reg(Reg, !CLD),
    ( if variable_is_forward_live(!.CLD, Var) then
        set_var_location(Var, Reg, !CLD),
        get_module_info(CI, ModuleInfo),
        MaybeForeign = get_maybe_foreign_type_info(ModuleInfo, OrigType),
        MaybeName = var_should_be_passed(MaybeArgName),
        (
            MaybeName = yes(Name),
            VarType = variable_type(CI, Var),
            IsDummy = is_type_a_dummy(ModuleInfo, VarType),
            PragmaCOutput = foreign_proc_output(Reg, VarType, IsDummy,
                OrigType, Name, MaybeForeign, BoxPolicy),
            Outputs = [PragmaCOutput | OutputsTail]
        ;
            MaybeName = no,
            Outputs = OutputsTail
        )
    else
        Outputs = OutputsTail
    ).

%---------------------------------------------------------------------------%

foreign_proc_struct_name(ModuleName, PredName, Arity, ProcId) =
    "mercury_save__" ++ sym_name_mangle(ModuleName) ++ "__" ++
        name_mangle(PredName) ++ "__" ++ int_to_string(Arity) ++ "_" ++
        int_to_string(proc_id_to_int(ProcId)).

foreign_proc_succ_ind_name = "MercurySuccessIndicator".

%---------------------------------------------------------------------------%
:- end_module ll_backend.pragma_c_gen.
%---------------------------------------------------------------------------%
