%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: code_util.m.

% Various utilities routines for code generation and recognition of builtins.

%-----------------------------------------------------------------------------%

:- module ll_backend.code_util.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.globals.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module std_util.

%-----------------------------------------------------------------------------%

    % Create a code address which holds the address of the specified procedure.
    % The `immed' argument should be `no' if the the caller wants the returned
    % address to be valid from everywhere in the program. If being valid from
    % within the current procedure is enough, this argument should be `yes'
    % wrapped around the value of the --procs-per-c-function option and the
    % current procedure id. Using an address that is only valid from within
    % the current procedure may make jumps more efficient.
    %
:- type immed == maybe(pair(int, pred_proc_id)).
:- pred make_entry_label(module_info::in, pred_id::in, proc_id::in, immed::in,
    code_addr::out) is det.

:- pred make_entry_label_from_rtti(rtti_proc_label::in, immed::in,
    code_addr::out) is det.

    % Create a label which holds the address of the specified procedure,
    % which must be defined in the current module (procedures that are
    % imported from other modules have representations only as code_addrs,
    % not as labels, since their address is not known at C compilation time).
    % The fourth argument has the same meaning as for make_entry_label.
    %
:- pred make_local_entry_label(module_info::in, pred_id::in, proc_id::in,
    immed::in, label::out) is det.

    % Create a label internal to a Mercury procedure.
    %
:- pred make_internal_label(module_info::in, pred_id::in, proc_id::in, int::in,
    label::out) is det.

:- pred extract_proc_label_from_code_addr(code_addr::in, proc_label::out)
    is det.

:- pred arg_loc_to_register(arg_loc::in, lval::out) is det.

:- pred max_mentioned_reg(list(lval)::in, int::out) is det.
:- pred max_mentioned_abs_reg(list(abs_locn)::in, int::out) is det.

:- pred goal_may_alloc_temp_frame(hlds_goal::in) is semidet.

    % Negate a condition.
    % This is used mostly just to make the generated code more readable.
    %
:- pred neg_rval(rval::in, rval::out) is det.

:- pred negate_the_test(list(instruction)::in, list(instruction)::out) is det.

    % These predicates return the set of lvals referenced in an rval
    % and an lval respectively. Lvals referenced indirectly through
    % lvals of the form var(_) are not counted.
    %
:- pred lvals_in_rval(rval::in, list(lval)::out) is det.
:- pred lvals_in_lval(lval::in, list(lval)::out) is det.
:- pred lvals_in_lvals(list(lval)::in, list(lval)::out) is det.

    % Given a procedure that already has its arg_info field filled in,
    % return a list giving its input variables and their initial locations.
    %
:- pred build_input_arg_list(proc_info::in, assoc_list(prog_var, lval)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Utility predicates used to implement trailing
%
    % Tests if we should add trail ops to the code we generate for
    % the given goal.  This will be 'no' unless we are compiling
    % in trailing grade.  It may also be 'no' in trailing grades if
    % we are optimizing trail usage and trail usage analysis tells
    % us that it is safe to omit the trail ops.
    %  
:- func should_add_trail_ops(globals, hlds_goal) = add_trail_ops.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

make_entry_label(ModuleInfo, PredId, ProcId, Immed, ProcAddr) :-
    RttiProcLabel = rtti.make_rtti_proc_label(ModuleInfo, PredId, ProcId),
    make_entry_label_from_rtti(RttiProcLabel, Immed, ProcAddr).

make_entry_label_from_rtti(RttiProcLabel, Immed, ProcAddr) :-
    ( RttiProcLabel ^ proc_is_imported = yes ->
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        ProcAddr = imported(ProcLabel)
    ;
        make_local_entry_label_from_rtti(RttiProcLabel, Immed, Label),
        ProcAddr = label(Label)
    ).

make_local_entry_label(ModuleInfo, PredId, ProcId, Immed, Label) :-
    RttiProcLabel = rtti.make_rtti_proc_label(ModuleInfo, PredId, ProcId),
    make_local_entry_label_from_rtti(RttiProcLabel, Immed, Label).

:- pred make_local_entry_label_from_rtti(rtti_proc_label::in,
    immed::in, label::out) is det.

make_local_entry_label_from_rtti(RttiProcLabel, Immed, Label) :-
    ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
    (
        Immed = no,
        % If we want to define the label or use it to put it into a data
        % structure, a label that is usable only within the current C module
        % won't do.
        ( RttiProcLabel ^ proc_is_exported = yes ->
            EntryType = exported
        ;
            EntryType = local
        ),
        Label = entry(EntryType, ProcLabel)
    ;
        Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId)),
        choose_local_label_type(ProcsPerFunc, CurPredId, CurProcId,
            RttiProcLabel^pred_id, RttiProcLabel^proc_id,
            ProcLabel, Label)
    ).

:- pred choose_local_label_type(int::in, pred_id::in, proc_id::in,
    pred_id::in, proc_id::in, proc_label::in, label::out) is det.

choose_local_label_type(ProcsPerFunc, CurPredId, CurProcId,
        PredId, ProcId, ProcLabel, Label) :-
    (
        % If we want to branch to the label now, we prefer a form that is
        % usable only within the current C module, since it is likely to be
        % faster.
        (
            ProcsPerFunc = 0
        ;
            PredId = CurPredId,
            ProcId = CurProcId
        )
    ->
        EntryType = c_local
    ;
        EntryType = local
    ),
    Label = entry(EntryType, ProcLabel).

%-----------------------------------------------------------------------------%

make_internal_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    Label = internal(LabelNum, ProcLabel).

extract_proc_label_from_code_addr(CodeAddr, ProcLabel) :-
    ( CodeAddr = label(Label) ->
        ProcLabel = get_proc_label(Label)
    ; CodeAddr = imported(ProcLabelPrime) ->
        ProcLabel = ProcLabelPrime
    ;
        unexpected(this_file, "extract_label_from_code_addr failed")
    ).

%-----------------------------------------------------------------------------%

arg_loc_to_register(ArgLoc, reg(r, ArgLoc)).

%-----------------------------------------------------------------------------%

max_mentioned_reg(Lvals, MaxRegNum) :-
    max_mentioned_reg_2(Lvals, 0, MaxRegNum).

:- pred max_mentioned_reg_2(list(lval)::in, int::in, int::out) is det.

max_mentioned_reg_2([], !MaxRegNum).
max_mentioned_reg_2([Lval | Lvals], !MaxRegNum) :-
    ( Lval = reg(r, N) ->
        int.max(N, !MaxRegNum)
    ;
        true
    ),
    max_mentioned_reg_2(Lvals, !MaxRegNum).

max_mentioned_abs_reg(Lvals, MaxRegNum) :-
    max_mentioned_abs_reg_2(Lvals, 0, MaxRegNum).

:- pred max_mentioned_abs_reg_2(list(abs_locn)::in, int::in, int::out) is det.

max_mentioned_abs_reg_2([], !MaxRegNum).
max_mentioned_abs_reg_2([Lval | Lvals], !MaxRegNum) :-
    ( Lval = abs_reg(N) ->
        int.max(N, !MaxRegNum)
    ;
        true
    ),
    max_mentioned_abs_reg_2(Lvals, !MaxRegNum).

%-----------------------------------------------------------------------------%

goal_may_alloc_temp_frame(Goal) :-
    goal_may_alloc_temp_frame(Goal, yes).

:- pred goal_may_alloc_temp_frame(hlds_goal::in, bool::out) is det.

goal_may_alloc_temp_frame(Goal - _GoalInfo, May) :-
    goal_may_alloc_temp_frame_2(Goal, May).

:- pred goal_may_alloc_temp_frame_2(hlds_goal_expr::in, bool::out)
    is det.

goal_may_alloc_temp_frame_2(generic_call(_, _, _, _), no).
goal_may_alloc_temp_frame_2(call(_, _, _, _, _, _), no).
goal_may_alloc_temp_frame_2(unify(_, _, _, _, _), no).
    % We cannot safely say that a foreign code fragment does not allocate
    % temporary nondet frames without knowing all the #defined macros
    % that expand to mktempframe and variants thereof. The performance
    % impact of being too conservative is probably not too bad.
goal_may_alloc_temp_frame_2(foreign_proc(_, _, _, _, _, _), yes).
goal_may_alloc_temp_frame_2(scope(_, Goal), May) :-
    Goal = _ - GoalInfo,
    goal_info_get_code_model(GoalInfo, CodeModel),
    ( CodeModel = model_non ->
        May = yes
    ;
        goal_may_alloc_temp_frame(Goal, May)
    ).
goal_may_alloc_temp_frame_2(not(Goal), May) :-
    goal_may_alloc_temp_frame(Goal, May).
goal_may_alloc_temp_frame_2(conj(_ConjType, Goals), May) :-
    goal_list_may_alloc_temp_frame(Goals, May).
goal_may_alloc_temp_frame_2(disj(Goals), May) :-
    goal_list_may_alloc_temp_frame(Goals, May).
goal_may_alloc_temp_frame_2(switch(_Var, _Det, Cases), May) :-
    cases_may_alloc_temp_frame(Cases, May).
goal_may_alloc_temp_frame_2(if_then_else(_Vars, C, T, E), May) :-
    ( goal_may_alloc_temp_frame(C, yes) ->
        May = yes
    ; goal_may_alloc_temp_frame(T, yes) ->
        May = yes
    ;
        goal_may_alloc_temp_frame(E, May)
    ).
goal_may_alloc_temp_frame_2(shorthand(ShorthandGoal), May) :-
    goal_may_alloc_temp_frame_2_shorthand(ShorthandGoal,May).

:- pred goal_may_alloc_temp_frame_2_shorthand(shorthand_goal_expr::in,
    bool::out) is det.

goal_may_alloc_temp_frame_2_shorthand(bi_implication(G1, G2), May) :-
    ( goal_may_alloc_temp_frame(G1, yes) ->
        May = yes
    ;
        goal_may_alloc_temp_frame(G2, May)
    ).

:- pred goal_list_may_alloc_temp_frame(list(hlds_goal)::in, bool::out) is det.

goal_list_may_alloc_temp_frame([], no).
goal_list_may_alloc_temp_frame([Goal | Goals], May) :-
    ( goal_may_alloc_temp_frame(Goal, yes) ->
        May = yes
    ;
        goal_list_may_alloc_temp_frame(Goals, May)
    ).

:- pred cases_may_alloc_temp_frame(list(case)::in, bool::out) is det.

cases_may_alloc_temp_frame([], no).
cases_may_alloc_temp_frame([case(_, Goal) | Cases], May) :-
    ( goal_may_alloc_temp_frame(Goal, yes) ->
        May = yes
    ;
        cases_may_alloc_temp_frame(Cases, May)
    ).

%-----------------------------------------------------------------------------%

neg_rval(Rval, NegRval) :-
    ( neg_rval_2(Rval, NegRval0) ->
        NegRval = NegRval0
    ;
        NegRval = unop(logical_not, Rval)
    ).

:- pred neg_rval_2(rval::in, rval::out) is semidet.

neg_rval_2(const(Const), const(NegConst)) :-
    (
        Const = true,
        NegConst = false
    ;
        Const = false,
        NegConst = true
    ).
neg_rval_2(unop(logical_not, Rval), Rval).
neg_rval_2(binop(Op, X, Y), binop(NegOp, X, Y)) :-
    neg_op(Op, NegOp).

:- pred neg_op(binary_op::in, binary_op::out) is semidet.

neg_op(eq, ne).
neg_op(ne, eq).
neg_op(int_lt, int_ge).
neg_op(int_le, int_gt).
neg_op(int_gt, int_le).
neg_op(int_ge, int_lt).
neg_op(str_eq, str_ne).
neg_op(str_ne, str_eq).
neg_op(str_lt, str_ge).
neg_op(str_le, str_gt).
neg_op(str_gt, str_le).
neg_op(str_ge, str_lt).
neg_op(float_eq, float_ne).
neg_op(float_ne, float_eq).
neg_op(float_lt, float_ge).
neg_op(float_le, float_gt).
neg_op(float_gt, float_le).
neg_op(float_ge, float_lt).

negate_the_test([], _) :-
    unexpected(this_file, "negate_the_test on empty list").
negate_the_test([Instr0 | Instrs0], Instrs) :-
    ( Instr0 = if_val(Test, Target) - Comment ->
        neg_rval(Test, NewTest),
        Instrs = [if_val(NewTest, Target) - Comment]
    ;
        negate_the_test(Instrs0, Instrs1),
        Instrs = [Instr0 | Instrs1]
    ).

%-----------------------------------------------------------------------------%

lvals_in_lvals([], []).
lvals_in_lvals([First | Rest], Lvals) :-
    lvals_in_lval(First, FirstLvals),
    lvals_in_lvals(Rest, RestLvals),
    list.append(FirstLvals, RestLvals, Lvals).

lvals_in_rval(lval(Lval), [Lval | Lvals]) :-
    lvals_in_lval(Lval, Lvals).
lvals_in_rval(var(_), []).
lvals_in_rval(mkword(_, Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).
lvals_in_rval(const(_), []).
lvals_in_rval(unop(_, Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).
lvals_in_rval(binop(_, Rval1, Rval2), Lvals) :-
    lvals_in_rval(Rval1, Lvals1),
    lvals_in_rval(Rval2, Lvals2),
    list.append(Lvals1, Lvals2, Lvals).
lvals_in_rval(mem_addr(MemRef), Lvals) :-
    lvals_in_mem_ref(MemRef, Lvals).

lvals_in_lval(reg(_, _), []).
lvals_in_lval(stackvar(_), []).
lvals_in_lval(framevar(_), []).
lvals_in_lval(succip, []).
lvals_in_lval(maxfr, []).
lvals_in_lval(curfr, []).
lvals_in_lval(succip(Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).
lvals_in_lval(redofr(Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).
lvals_in_lval(redoip(Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).
lvals_in_lval(succfr(Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).
lvals_in_lval(prevfr(Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).
lvals_in_lval(hp, []).
lvals_in_lval(sp, []).
lvals_in_lval(field(_, Rval1, Rval2), Lvals) :-
    lvals_in_rval(Rval1, Lvals1),
    lvals_in_rval(Rval2, Lvals2),
    list.append(Lvals1, Lvals2, Lvals).
lvals_in_lval(lvar(_), []).
lvals_in_lval(temp(_, _), []).
lvals_in_lval(mem_ref(Rval), Lvals) :-
    lvals_in_rval(Rval, Lvals).

:- pred lvals_in_mem_ref(mem_ref::in, list(lval)::out) is det.

lvals_in_mem_ref(stackvar_ref(_), []).
lvals_in_mem_ref(framevar_ref(_), []).
lvals_in_mem_ref(heap_ref(Rval, _, _), Lvals) :-
    lvals_in_rval(Rval, Lvals).

%-----------------------------------------------------------------------------%

build_input_arg_list(ProcInfo, VarLvals) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_arg_info(ProcInfo, ArgInfos),
    assoc_list.from_corresponding_lists(HeadVars, ArgInfos, VarArgInfos),
    build_input_arg_list_2(VarArgInfos, VarLvals).

:- pred build_input_arg_list_2(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, lval)::out) is det.

build_input_arg_list_2([], []).
build_input_arg_list_2([V - Arg | Rest0], VarArgs) :-
    Arg = arg_info(Loc, Mode),
    ( Mode = top_in ->
        arg_loc_to_register(Loc, Reg),
        VarArgs = [V - Reg | VarArgs0]
    ;
        VarArgs = VarArgs0
    ),
    build_input_arg_list_2(Rest0, VarArgs0).

%---------------------------------------------------------------------------%
%
% Utility predicates used to implement trailing
%

should_add_trail_ops(Globals, Goal) = AddTrailOps :-
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    (
        UseTrail = no,
        AddTrailOps = no
    ;
        UseTrail = yes,
        globals.lookup_bool_option(Globals, disable_trail_ops,
            DisableTrailOps),
        (
            DisableTrailOps = yes,
            AddTrailOps = no
        ;
            DisableTrailOps = no,
            globals.lookup_bool_option(Globals, optimize_trail_usage,
                OptTrailUsage),
            (
                OptTrailUsage = no,
                AddTrailOps = yes
            ;
                OptTrailUsage = yes,
                ( goal_cannot_modify_trail(Goal) ->
                    AddTrailOps = no
                ;
                    AddTrailOps = yes
                )
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "code_util.m".

%-----------------------------------------------------------------------------%
:- end_module code_util.
%-----------------------------------------------------------------------------%
