%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: exprn_aux.m.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.exprn_aux.
:- interface.

:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Determine whether an rval_const can be used as the initializer
    % of a C static constant.
    %
:- pred const_is_constant(rval_const::in, exprn_opts::in, bool::out) is det.

    % transform_lval_in_instr(Transform, !Instr, !Acc):
    %
    % Transform all lvals in !.Instr with the predicate Transform.
    % An accumulator is threaded through.
    %
:- pred transform_lval_in_instr(transform_lval(T)::in(transform_lval),
    instruction::in, instruction::out, T::in, T::out) is det.

:- pred transform_lval_in_rval(transform_lval(T)::in(transform_lval),
    rval::in, rval::out, T::in, T::out) is det.

:- type transform_lval(T)   == pred(lval, lval, T, T).
:- inst transform_lval      == (pred(in, out, in, out) is det).

    % substitute_lval_in_instr(OldLval, NewLval, !Instr, !SubstCount):
    %
    % Substitute all occurrences of OldLval in !.Instr with NewLval.
    % Return the number of substitutions performed as the difference
    % between !.SubstCount and !:SubstCount.
    %
    % The other substitute_lval_in_* predicates are similar,
    % although many do not return substitution counts.
    %
:- pred substitute_lval_in_instr(lval::in, lval::in,
    instruction::in, instruction::out, int::in, int::out) is det.

:- pred substitute_lval_in_lval(lval::in, lval::in, lval::in, lval::out)
    is det.

:- pred substitute_lval_in_rval(lval::in, lval::in, rval::in, rval::out)
    is det.

:- pred substitute_rval_in_rval(rval::in, rval::in, rval::in, rval::out)
    is det.

:- pred substitute_vars_in_rval(assoc_list(prog_var, rval)::in,
    rval::in, rval::out) is det.

:- pred substitute_rvals_in_rval(assoc_list(rval, rval)::in,
    rval::in, rval::out) is det.

:- pred vars_in_lval(lval::in, list(prog_var)::out) is det.

:- pred vars_in_rval(rval::in, list(prog_var)::out) is det.

:- pred simplify_rval(rval::in, rval::out) is det.

    % The following predicates take an lval/rval (list)
    % and return a list of the code and data addresses that it references.
    %
:- pred rval_list_addrs(list(rval)::in,
    list(code_addr)::out, list(data_id)::out) is det.

:- pred lval_list_addrs(list(lval)::in,
    list(code_addr)::out, list(data_id)::out) is det.

:- pred rval_addrs(rval::in, list(code_addr)::out, list(data_id)::out)
    is det.

:- pred lval_addrs(lval::in, list(code_addr)::out, list(data_id)::out)
    is det.

:- func var_lval_to_rval(prog_var, lval) = rval.

:- func lval_to_rval(lval) = rval.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module getopt_io.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%-----------------------------------------------------------------------------%

% Determine whether a const (well, what _we_ consider to be a const)
% is constant as far as the C compiler is concerned -- specifically,
% determine whether it can be used as the initializer of a C static constant.

const_is_constant(llconst_true, _, yes).
const_is_constant(llconst_false, _, yes).
const_is_constant(llconst_int(_), _, yes).
const_is_constant(llconst_foreign(_, _), _, yes).
const_is_constant(llconst_float(_), ExprnOpts, IsConst) :-
    SGFloats = ExprnOpts ^ static_ground_floats,
    (
        SGFloats = have_static_ground_floats,
        IsConst = yes
    ;
        SGFloats = do_not_have_static_ground_floats,
        IsConst = no
    ).
const_is_constant(llconst_string(_), _, yes).
const_is_constant(llconst_multi_string(_), _, yes).
const_is_constant(llconst_code_addr(CodeAddr), ExprnOpts, IsConst) :-
    addr_is_constant(CodeAddr, ExprnOpts, IsConst).
const_is_constant(llconst_data_addr(_, _), _, yes).

:- pred addr_is_constant(code_addr::in, exprn_opts::in, bool::out) is det.

addr_is_constant(code_label(Label), ExprnOpts, IsConst) :-
    label_is_constant(Label, ExprnOpts, IsConst).
addr_is_constant(code_imported_proc(_), ExprnOpts, IsConst) :-
    StaticCodeAddrs = ExprnOpts ^ static_code_addresses,
    (
        StaticCodeAddrs = have_static_code_addresses,
        IsConst = yes
    ;
        StaticCodeAddrs = do_not_have_static_code_addresses,
        IsConst = no
    ).
addr_is_constant(code_succip, _, no).
addr_is_constant(do_succeed(_), _, no).
addr_is_constant(do_redo, _, no).
addr_is_constant(do_fail, _, no).
addr_is_constant(do_trace_redo_fail_shallow, _, no).
addr_is_constant(do_trace_redo_fail_deep, _, no).
addr_is_constant(do_call_closure(_), _, no).
addr_is_constant(do_call_class_method(_), _, no).
addr_is_constant(do_not_reached, _, no).

:- pred label_is_constant(label::in, exprn_opts::in, bool::out) is det.

label_is_constant(Label, ExprnOpts, IsConst) :-
    (
        Label = entry_label(EntryLabelType, _),
        (

            ( EntryLabelType = entry_label_exported
            ; EntryLabelType = entry_label_local
            ),
            StaticCodeAddrs = ExprnOpts ^ static_code_addresses,
            (
                StaticCodeAddrs = have_static_code_addresses,
                IsConst = yes
            ;
                StaticCodeAddrs = do_not_have_static_code_addresses,
                IsConst = no
            )
        ;
            EntryLabelType = entry_label_c_local,
            IsConst = yes
        )
    ;
        Label = internal_label(_, _),
        IsConst = yes
    ).

%-----------------------------------------------------------------------------%

vars_in_rval(lval(Lval), Vars) :-
    vars_in_lval(Lval, Vars).
vars_in_rval(var(Var), [Var]).
vars_in_rval(mkword(_, Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_rval(const(_Conts), []).
vars_in_rval(unop(_Unop, Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_rval(binop(_Binop, RvalA, RvalB), Vars) :-
    vars_in_rval(RvalA, VarsA),
    vars_in_rval(RvalB, VarsB),
    Vars = VarsA ++ VarsB.
vars_in_rval(mem_addr(MemRef), Vars) :-
    vars_in_mem_ref(MemRef, Vars).

vars_in_lval(reg(_Type, _RegNum), []).
vars_in_lval(temp(_Type, _TmpNum), []).
vars_in_lval(succip, []).
vars_in_lval(maxfr, []).
vars_in_lval(curfr, []).
vars_in_lval(hp, []).
vars_in_lval(sp, []).
vars_in_lval(parent_sp, []).
vars_in_lval(stackvar(_SlotNum), []).
vars_in_lval(parent_stackvar(_SlotNum), []).
vars_in_lval(framevar(_SlotNum), []).
vars_in_lval(succip_slot(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_lval(redoip_slot(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_lval(redofr_slot(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_lval(succfr_slot(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_lval(prevfr_slot(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_lval(field(_MaybeTag, RvalA, RvalB), Vars) :-
    vars_in_rval(RvalA, VarsA),
    vars_in_rval(RvalB, VarsB),
    Vars = VarsA ++ VarsB.
vars_in_lval(mem_ref(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_lval(global_var_ref(_), []).
vars_in_lval(lvar(Var), [Var]).

:- pred vars_in_mem_ref(mem_ref::in, list(prog_var)::out) is det.

vars_in_mem_ref(stackvar_ref(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_mem_ref(framevar_ref(Rval), Vars) :-
    vars_in_rval(Rval, Vars).
vars_in_mem_ref(heap_ref(BaseRval, _Tag, FieldRval), BaseVars ++ FieldVars) :-
    vars_in_rval(BaseRval, BaseVars),
    vars_in_rval(FieldRval, FieldVars).

%-----------------------------------------------------------------------------%

transform_lval_in_instr(Transform, Instr0, Instr, !Acc) :-
    Instr0 = llds_instr(Uinstr0, Comment),
    transform_lval_in_uinstr(Transform, Uinstr0, Uinstr, !Acc),
    Instr = llds_instr(Uinstr, Comment).

:- pred transform_lval_in_uinstr(transform_lval(T)::in(transform_lval),
    instr::in, instr::out, T::in, T::out) is det.

transform_lval_in_uinstr(Transform, Uinstr0, Uinstr, !Acc) :-
    (
        ( Uinstr0 = comment(_Comment)
        ; Uinstr0 = llcall(_, _, _, _, _, _)
        ; Uinstr0 = mkframe(_, _)
        ; Uinstr0 = label(_)
        ; Uinstr0 = goto(_)
        ; Uinstr0 = prune_ticket
        ; Uinstr0 = discard_ticket
        ; Uinstr0 = incr_sp(_, _, _)
        ; Uinstr0 = decr_sp(_)
        ; Uinstr0 = decr_sp_and_return(_)
        ; Uinstr0 = fork_new_child(_, _)
        ),
        Uinstr = Uinstr0
    ;
        Uinstr0 = livevals(LvalSet0),
        set.to_sorted_list(LvalSet0, Lvals0),
        list.map_foldl(Transform, Lvals0, Lvals, !Acc),
        set.list_to_set(Lvals, LvalSet),
        Uinstr = livevals(LvalSet)
    ;
        Uinstr0 = block(TempR, TempF, Instrs0),
        list.map_foldl(transform_lval_in_instr(Transform),
            Instrs0, Instrs, !Acc),
        Uinstr = block(TempR, TempF, Instrs)
    ;
        Uinstr0 = assign(Lval0, Rval0),
        Transform(Lval0, Lval, !Acc),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = assign(Lval, Rval)
    ;
        Uinstr0 = keep_assign(Lval0, Rval0),
        Transform(Lval0, Lval, !Acc),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = keep_assign(Lval, Rval)
    ;
        Uinstr0 = computed_goto(Rval0, Labels),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = computed_goto(Rval, Labels)
    ;
        Uinstr0 = arbitrary_c_code(AffectsLiveness, LiveLvals0, Code),
        transform_lval_in_live_lval_info(Transform, LiveLvals0, LiveLvals,
            !Acc),
        Uinstr = arbitrary_c_code(AffectsLiveness, LiveLvals, Code)
    ;
        Uinstr0 = if_val(Rval0, CodeAddr),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = if_val(Rval, CodeAddr)
    ;
        Uinstr0 = save_maxfr(Lval0),
        Transform(Lval0, Lval, !Acc),
        Uinstr = save_maxfr(Lval)
    ;
        Uinstr0 = restore_maxfr(Lval0),
        Transform(Lval0, Lval, !Acc),
        Uinstr = restore_maxfr(Lval)
    ;
        Uinstr0 = incr_hp(Lval0, MaybeTag, MO, Rval0, TypeCtor,
            MayUseAtomic, MaybeRegionRval0, MaybeReuse0),
        Transform(Lval0, Lval, !Acc),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        (
            MaybeRegionRval0 = no,
            MaybeRegionRval = no
        ;
            MaybeRegionRval0 = yes(RegionRval0),
            transform_lval_in_rval(Transform, RegionRval0, RegionRval, !Acc),
            MaybeRegionRval = yes(RegionRval)
        ),
        (
            MaybeReuse0 = no_llds_reuse,
            MaybeReuse = no_llds_reuse
        ;
            MaybeReuse0 = llds_reuse(ReuseRval0, MaybeFlagLval0),
            transform_lval_in_rval(Transform, ReuseRval0, ReuseRval, !Acc),
            (
                MaybeFlagLval0 = no,
                MaybeFlagLval = no
            ;
                MaybeFlagLval0 = yes(FlagLval0),
                Transform(FlagLval0, FlagLval, !Acc),
                MaybeFlagLval = yes(FlagLval)
            ),
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval)
        ),
        Uinstr = incr_hp(Lval, MaybeTag, MO, Rval, TypeCtor,
            MayUseAtomic, MaybeRegionRval, MaybeReuse)
    ;
        Uinstr0 = mark_hp(Lval0),
        Transform(Lval0, Lval, !Acc),
        Uinstr = mark_hp(Lval)
    ;
        Uinstr0 = restore_hp(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = restore_hp(Rval)
    ;
        Uinstr0 = free_heap(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = free_heap(Rval)
    ;
        Uinstr0 = push_region_frame(StackId, EmbeddedStackFrame),
        Uinstr = push_region_frame(StackId, EmbeddedStackFrame)
    ;
        Uinstr0 = region_fill_frame(FillOp, EmbeddedStackFrame, IdRval0,
            NumLval0, AddrLval0),
        transform_lval_in_rval(Transform, IdRval0, IdRval, !Acc),
        Transform(NumLval0, NumLval, !Acc),
        Transform(AddrLval0, AddrLval, !Acc),
        Uinstr = region_fill_frame(FillOp, EmbeddedStackFrame, IdRval,
            NumLval, AddrLval)
    ;
        Uinstr0 = region_set_fixed_slot(SetOp, EmbeddedStackFrame,
            ValueRval0),
        transform_lval_in_rval(Transform, ValueRval0, ValueRval, !Acc),
        Uinstr = region_set_fixed_slot(SetOp, EmbeddedStackFrame,
            ValueRval)
    ;
        Uinstr0 = use_and_maybe_pop_region_frame(UseOp, EmbeddedStackFrame),
        Uinstr = use_and_maybe_pop_region_frame(UseOp, EmbeddedStackFrame)
    ;
        Uinstr0 = store_ticket(Lval0),
        Transform(Lval0, Lval, !Acc),
        Uinstr = store_ticket(Lval)
    ;
        Uinstr0 = reset_ticket(Rval0, Reason),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = reset_ticket(Rval, Reason)
    ;
        Uinstr0 = mark_ticket_stack(Lval0),
        Transform(Lval0, Lval, !Acc),
        Uinstr = mark_ticket_stack(Lval)
    ;
        Uinstr0 = prune_tickets_to(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        Uinstr = prune_tickets_to(Rval)
%   ;
%       % discard_tickets_to(_) is used only in hand-written code
%       Uinstr0 = discard_tickets_to(Rval0),
%       transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
%       Uinstr = discard_tickets_to(Rval)
    ;
        Uinstr0 = foreign_proc_code(Decls, Components0, MayCallMercury,
            MaybeLabel1, MaybeLabel2, MaybeLabel3, MaybeLabel4, MaybeLabel5,
            ReferStackSlot, MayDupl),
        list.map_foldl(transform_lval_in_component(Transform),
            Components0, Components, !Acc),
        Uinstr = foreign_proc_code(Decls, Components, MayCallMercury,
            MaybeLabel1, MaybeLabel2, MaybeLabel3, MaybeLabel4, MaybeLabel5,
            ReferStackSlot, MayDupl)
    ;
        Uinstr0 = init_sync_term(Lval0, BranchCount),
        Transform(Lval0, Lval, !Acc),
        Uinstr = init_sync_term(Lval, BranchCount)
    ;
        Uinstr0 = join_and_continue(Lval0, Label),
        Transform(Lval0, Lval, !Acc),
        Uinstr = join_and_continue(Lval, Label)
    ).

:- pred transform_lval_in_component(transform_lval(T)::in(transform_lval),
    foreign_proc_component::in, foreign_proc_component::out, T::in, T::out)
    is det.

transform_lval_in_component(Transform, Component0, Component, !Acc) :-
    (
        Component0 = foreign_proc_inputs(Inputs0),
        list.map_foldl(transform_lval_in_foreign_proc_input(Transform),
            Inputs0, Inputs, !Acc),
        Component = foreign_proc_inputs(Inputs)
    ;
        Component0 = foreign_proc_outputs(Outputs0),
        list.map_foldl(transform_lval_in_foreign_proc_output(Transform),
            Outputs0, Outputs, !Acc),
        Component = foreign_proc_outputs(Outputs)
    ;
        Component0 = foreign_proc_user_code(_, _, _),
        Component = Component0
    ;
        Component0 = foreign_proc_raw_code(CanBranchAway, AffectsLiveness,
            LvalSet0, Code),
        transform_lval_in_live_lval_info(Transform, LvalSet0, LvalSet, !Acc),
        Component = foreign_proc_raw_code(CanBranchAway, AffectsLiveness,
            LvalSet, Code)
    ;
        Component0 = foreign_proc_fail_to(_),
        Component = Component0
    ;
        Component0 = foreign_proc_noop,
        Component = Component0
    ).

:- pred transform_lval_in_live_lval_info(transform_lval(T)::in(transform_lval),
    c_code_live_lvals::in, c_code_live_lvals::out, T::in, T::out) is det.

transform_lval_in_live_lval_info(_,
        no_live_lvals_info, no_live_lvals_info, !Acc).
transform_lval_in_live_lval_info(Transform,
        live_lvals_info(LvalSet0), live_lvals_info(LvalSet), !Acc) :-
    Lvals0 = set.to_sorted_list(LvalSet0),
    list.map_foldl(Transform, Lvals0, Lvals, !Acc),
    set.list_to_set(Lvals, LvalSet).

:- pred transform_lval_in_foreign_proc_input(
    transform_lval(T)::in(transform_lval),
    foreign_proc_input::in, foreign_proc_input::out, T::in, T::out) is det.

transform_lval_in_foreign_proc_input(Transform, Out0, Out, !Acc) :-
    Out0 = foreign_proc_input(Name, VarType, IsDummy, OrigType, Rval0,
        MaybeForeign, BoxPolicy),
    transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
    Out = foreign_proc_input(Name, VarType, IsDummy, OrigType, Rval,
        MaybeForeign, BoxPolicy).

:- pred transform_lval_in_foreign_proc_output(
    transform_lval(T)::in(transform_lval),
    foreign_proc_output::in, foreign_proc_output::out, T::in, T::out) is det.

transform_lval_in_foreign_proc_output(Transform, Out0, Out, !Acc) :-
    Out0 = foreign_proc_output(Lval0, VarType, IsDummy, OrigType, Name,
        MaybeForeign, BoxPolicy),
    Transform(Lval0, Lval, !Acc),
    Out = foreign_proc_output(Lval, VarType, IsDummy, OrigType, Name,
        MaybeForeign, BoxPolicy).

transform_lval_in_rval(Transform, Rval0, Rval, !Acc) :-
    (
        Rval0 = lval(Lval0),
        Transform(Lval0, Lval, !Acc),
        Rval = lval(Lval)
    ;
        Rval0 = var(_Var),
        Rval = Rval0
    ;
        Rval0 = mkword(Tag, Rval1),
        transform_lval_in_rval(Transform, Rval1, Rval2, !Acc),
        Rval = mkword(Tag, Rval2)
    ;
        Rval0 = const(_Const),
        Rval = Rval0
    ;
        Rval0 = unop(Unop, Rval1),
        transform_lval_in_rval(Transform, Rval1, Rval2, !Acc),
        Rval = unop(Unop, Rval2)
    ;
        Rval0 = binop(Binop, Rval1, Rval2),
        transform_lval_in_rval(Transform, Rval1, Rval3, !Acc),
        transform_lval_in_rval(Transform, Rval2, Rval4, !Acc),
        Rval = binop(Binop, Rval3, Rval4)
    ;
        Rval0 = mem_addr(MemRef0),
        transform_lval_in_mem_ref(Transform, MemRef0, MemRef, !Acc),
        Rval = mem_addr(MemRef)
    ).

:- pred transform_lval_in_mem_ref(transform_lval(T)::in(transform_lval),
    mem_ref::in, mem_ref::out, T::in, T::out) is det.

transform_lval_in_mem_ref(Transform, MemRef0, MemRef, !Acc) :-
    (
        MemRef0 = stackvar_ref(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        MemRef = stackvar_ref(Rval)
    ;
        MemRef0 = framevar_ref(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !Acc),
        MemRef = framevar_ref(Rval)
    ;
        MemRef0 = heap_ref(BaseRval0, Tag, FieldRval0),
        transform_lval_in_rval(Transform, BaseRval0, BaseRval, !Acc),
        transform_lval_in_rval(Transform, FieldRval0, FieldRval, !Acc),
        MemRef = heap_ref(BaseRval, Tag, FieldRval)
    ).

%-----------------------------------------------------------------------------%

substitute_lval_in_instr(OldLval, NewLval, Instr0, Instr, !N) :-
    transform_lval_in_instr(substitute_lval_in_lval_count(OldLval, NewLval),
        Instr0, Instr, !N).

substitute_lval_in_lval(OldLval, NewLval, Lval0, Lval) :-
    substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval,
        0, _SubstCount).

substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval) :-
    transform_lval_in_rval(substitute_lval_in_lval_count(OldLval, NewLval),
        Rval0, Rval, 0, _SubstCount).

:- pred substitute_lval_in_lval_count(lval::in, lval::in,
    lval::in, lval::out, int::in, int::out) is det.

substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N) :-
    ( Lval0 = OldLval ->
        Lval = NewLval,
        !:N = !.N + 1
    ;
        substitute_lval_in_lval_count_2(OldLval, NewLval, Lval0, Lval, !N)
    ).

:- pred substitute_lval_in_lval_count_2(lval::in, lval::in,
    lval::in, lval::out, int::in, int::out) is det.

substitute_lval_in_lval_count_2(OldLval, NewLval, Lval0, Lval, !N) :-
    Transform = substitute_lval_in_lval_count(OldLval, NewLval),
    (
        ( Lval0 = reg(_Type, _RegNum)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ; Lval0 = temp(_Type, _TmpNum)
        ; Lval0 = stackvar(_SlotNum)
        ; Lval0 = parent_stackvar(_SlotNum)
        ; Lval0 = framevar(_SlotNum)
        ; Lval0 = lvar(_Var)
        ; Lval0 = global_var_ref(_GlobalVarName)
        ),
        Lval = Lval0
    ;
        Lval0 = succip_slot(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !N),
        Lval = succip_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !N),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !N),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !N),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !N),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = field(Tag, Rval1, Rval2),
        transform_lval_in_rval(Transform, Rval1, Rval3, !N),
        transform_lval_in_rval(Transform, Rval2, Rval4, !N),
        Lval = field(Tag, Rval3, Rval4)
    ;
        Lval0 = mem_ref(Rval0),
        transform_lval_in_rval(Transform, Rval0, Rval, !N),
        Lval = mem_ref(Rval)
    ).

substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval) :-
    ( Rval0 = OldRval ->
        Rval = NewRval
    ;
        (
            Rval0 = lval(Lval0),
            substitute_rval_in_lval(OldRval, NewRval, Lval0, Lval),
            Rval = lval(Lval)
        ;
            Rval0 = var(_Var),
            Rval = Rval0
        ;
            Rval0 = mkword(Tag, Rval1),
            substitute_rval_in_rval(OldRval, NewRval, Rval1, Rval2),
            Rval = mkword(Tag, Rval2)
        ;
            Rval0 = const(_Const),
            Rval = Rval0
        ;
            Rval0 = unop(Unop, Rval1),
            substitute_rval_in_rval(OldRval, NewRval, Rval1, Rval2),
            Rval = unop(Unop, Rval2)
        ;
            Rval0 = binop(Binop, Rval1, Rval2),
            substitute_rval_in_rval(OldRval, NewRval, Rval1, Rval3),
            substitute_rval_in_rval(OldRval, NewRval, Rval2, Rval4),
            Rval = binop(Binop, Rval3, Rval4)
        ;
            Rval0 = mem_addr(MemRef1),
            substitute_rval_in_mem_ref(OldRval, NewRval, MemRef1, MemRef2),
            Rval = mem_addr(MemRef2)
        )
    ).

:- pred substitute_rval_in_mem_ref(rval::in, rval::in,
    mem_ref::in, mem_ref::out) is det.

substitute_rval_in_mem_ref(OldRval, NewRval, MemRef0, MemRef) :-
    (
        MemRef0 = stackvar_ref(N),
        MemRef = stackvar_ref(N)
    ;
        MemRef0 = framevar_ref(N),
        MemRef = framevar_ref(N)
    ;
        MemRef0 = heap_ref(Rval0, Tag, N),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        MemRef = heap_ref(Rval, Tag, N)
    ).

:- pred substitute_rval_in_lval(rval::in, rval::in,
    lval::in, lval::out) is det.

substitute_rval_in_lval(OldRval, NewRval, Lval0, Lval) :-
    (
        ( Lval0 = reg(_, _)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ; Lval0 = temp(_, _)
        ; Lval0 = stackvar(_)
        ; Lval0 = parent_stackvar(_)
        ; Lval0 = framevar(_)
        ; Lval0 = global_var_ref(_)
        ; Lval0 = lvar(_)
        ),
        Lval = Lval0
    ;
        Lval0 = succip_slot(Rval0),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        Lval = succip_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = field(Tag, Rval1, Rval2),
        substitute_rval_in_rval(OldRval, NewRval, Rval1, Rval3),
        substitute_rval_in_rval(OldRval, NewRval, Rval2, Rval4),
        Lval = field(Tag, Rval3, Rval4)
    ;
        Lval0 = mem_ref(Rval0),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        Lval = mem_ref(Rval)
    ).

%-----------------------------------------------------------------------------%

substitute_vars_in_rval([], !Rval).
substitute_vars_in_rval([Var - Sub | Rest], !Rval) :-
    substitute_rval_in_rval(var(Var), Sub, !Rval),
    substitute_vars_in_rval(Rest, !Rval).

substitute_rvals_in_rval(RvalPairs, !Rval) :-
    % When we substitute one set of rvals for another, we face the problem
    % that the substitution may not be idempotent. We finesse this problem by
    % substituting unique new rvals for the original rvals, and then
    % substituting the replacement rvals for these unique rvals. We guarantee
    % the uniqueness of these rvals by using framevars with negative numbers
    % for them.
    substitute_rvals_in_rval_1(RvalPairs, 0, RvalUniqPairs, UniqRvalPairs),
    substitute_rvals_in_rval_2(RvalUniqPairs, !Rval),
    substitute_rvals_in_rval_2(UniqRvalPairs, !Rval).

:- pred substitute_rvals_in_rval_1(assoc_list(rval, rval)::in,
    int::in, assoc_list(rval, rval)::out, assoc_list(rval, rval)::out) is det.

substitute_rvals_in_rval_1([], _, [], []).
substitute_rvals_in_rval_1([Rval1 - Rval2 | RvalPairList], N0,
        [Rval1 - Uniq | RvalUniqList], [Uniq - Rval2 | UniqRvalList]) :-
    N1 = N0 - 1,
    Uniq = lval(framevar(N1)),
    substitute_rvals_in_rval_1(RvalPairList, N1, RvalUniqList, UniqRvalList).

:- pred substitute_rvals_in_rval_2(assoc_list(rval, rval)::in,
    rval::in, rval::out) is det.

substitute_rvals_in_rval_2([], !Rval).
substitute_rvals_in_rval_2([Left - Right | Rest], !Rval) :-
    substitute_rval_in_rval(Left, Right, !Rval),
    substitute_rvals_in_rval_2(Rest, !Rval).

%---------------------------------------------------------------------------%

simplify_rval(Rval0, Rval) :-
    ( simplify_rval_2(Rval0, Rval1) ->
        simplify_rval(Rval1, Rval)
    ;
        Rval = Rval0
    ).

:- pred simplify_rval_2(rval::in, rval::out) is semidet.

simplify_rval_2(Rval0, Rval) :-
    (
        Rval0 = lval(field(MaybeTag, Rval1, Num)),
        simplify_rval_2(Rval1, Rval2)
    ->
        Rval = lval(field(MaybeTag, Rval2, Num))
    ;
        Rval0 = unop(UnOp, Rval1),
        simplify_rval_2(Rval1, Rval2)
    ->
        Rval = unop(UnOp, Rval2)
    ;
        Rval0 = binop(BinOp, Rval1, Rval2),
        simplify_rval_2(Rval1, Rval3)
    ->
        Rval = binop(BinOp, Rval3, Rval2)
    ;
        Rval0 = binop(BinOp, Rval1, Rval2),
        simplify_rval_2(Rval2, Rval3)
    ->
        Rval = binop(BinOp, Rval1, Rval3)
    ;
        fail
    ).

:- pred simplify_args(list(maybe(rval))::in, list(maybe(rval))::out) is det.

simplify_args([], []).
simplify_args([MR0 | Ms0], [MR | Ms]) :-
    simplify_args(Ms0, Ms),
    simplify_arg(MR0, MR).

:- pred simplify_arg(maybe(rval)::in, maybe(rval)::out) is det.

simplify_arg(MaybeRval0, MaybeRval) :-
    (
        MaybeRval0 = yes(Rval0),
        simplify_rval_2(Rval0, Rval)
    ->
        MaybeRval = yes(Rval)
    ;
        MaybeRval = MaybeRval0
    ).

%-----------------------------------------------------------------------------%

rval_addrs(lval(Lval), CodeAddrs, DataIds) :-
    lval_addrs(Lval, CodeAddrs, DataIds).
rval_addrs(var(_Var), [], []).
rval_addrs(mkword(_Tag, Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
rval_addrs(const(Const), CodeAddrs, DataIds) :-
    ( Const = llconst_code_addr(CodeAddress) ->
        CodeAddrs = [CodeAddress],
        DataIds = []
    ; Const = llconst_data_addr(DataId, _) ->
        CodeAddrs = [],
        DataIds = [DataId]
    ;
        CodeAddrs = [],
        DataIds = []
    ).
rval_addrs(unop(_Unop, Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
rval_addrs(binop(_Binop, RvalA, RvalB), CodeAddrs, DataIds) :-
    rval_addrs(RvalA, CodeAddrsA, DataIdsA),
    rval_addrs(RvalB, CodeAddrsB, DataIdsB),
    CodeAddrs = CodeAddrsA ++ CodeAddrsB,
    DataIds = DataIdsA ++ DataIdsB.
rval_addrs(mem_addr(Rval), CodeAddrs, DataIds) :-
    mem_ref_addrs(Rval, CodeAddrs, DataIds).

lval_addrs(reg(_Type, _RegNum), [], []).
lval_addrs(stackvar(_SlotNum), [], []).
lval_addrs(parent_stackvar(_SlotNum), [], []).
lval_addrs(framevar(_SlotNum), [], []).
lval_addrs(succip, [], []).
lval_addrs(maxfr, [], []).
lval_addrs(curfr, [], []).
lval_addrs(prevfr_slot(Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
lval_addrs(succfr_slot(Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
lval_addrs(redofr_slot(Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
lval_addrs(redoip_slot(Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
lval_addrs(succip_slot(Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
lval_addrs(hp, [], []).
lval_addrs(sp, [], []).
lval_addrs(parent_sp, [], []).
lval_addrs(field(_Tag, RvalA, RvalB), CodeAddrs, DataIds) :-
    rval_addrs(RvalA, CodeAddrsA, DataIdsA),
    rval_addrs(RvalB, CodeAddrsB, DataIdsB),
    CodeAddrs = CodeAddrsA ++ CodeAddrsB,
    DataIds = DataIdsA ++ DataIdsB.
lval_addrs(lvar(_Var), [], []).
lval_addrs(temp(_Type, _TmpNum), [], []).
lval_addrs(mem_ref(Rval), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).
lval_addrs(global_var_ref(_), [], []).

rval_list_addrs([], [], []).
rval_list_addrs([Rval | Rvals], CodeAddrs, DataIds) :-
    rval_addrs(Rval, HeadCodeAddrs, HeadDataIds),
    rval_list_addrs(Rvals, TailCodeAddrs, TailDataIds),
    CodeAddrs = HeadCodeAddrs ++ TailCodeAddrs,
    DataIds = HeadDataIds ++ TailDataIds.

lval_list_addrs([], [], []).
lval_list_addrs([Lval | Lvals], CodeAddrs, DataIds) :-
    lval_addrs(Lval, HeadCodeAddrs, HeadDataIds),
    lval_list_addrs(Lvals, TailCodeAddrs, TailDataIds),
    CodeAddrs = HeadCodeAddrs ++ TailCodeAddrs,
    DataIds = HeadDataIds ++ TailDataIds.

:- pred mem_ref_addrs(mem_ref::in,
    list(code_addr)::out, list(data_id)::out) is det.

mem_ref_addrs(stackvar_ref(_SlotNum), [], []).
mem_ref_addrs(framevar_ref(_SlotNum), [], []).
mem_ref_addrs(heap_ref(Rval, _Tag, _FieldNum), CodeAddrs, DataIds) :-
    rval_addrs(Rval, CodeAddrs, DataIds).

    % Give a list of maybe(rval), return a list of the code and data
    % addresses that are referenced by that list.
    %
:- pred maybe_rval_list_addrs(list(maybe(rval))::in,
    list(code_addr)::out, list(data_id)::out) is det.

maybe_rval_list_addrs([], [], []).
maybe_rval_list_addrs([MaybeRval | MaybeRvals], CodeAddrs, DataIds) :-
    (
        MaybeRval = yes(Rval),
        rval_addrs(Rval, HeadCodeAddrs, HeadDataIds),
        maybe_rval_list_addrs(MaybeRvals, TailCodeAddrs, TailDataIds),
        CodeAddrs = HeadCodeAddrs ++ TailCodeAddrs,
        DataIds = HeadDataIds ++ TailDataIds
    ;
        MaybeRval = no,
        maybe_rval_list_addrs(MaybeRvals, CodeAddrs, DataIds)
    ).

var_lval_to_rval(_Var, Lval) = lval(Lval).

lval_to_rval(Lval) = lval(Lval).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "exprn_aux.m".

%-----------------------------------------------------------------------------%
:- end_module exprn_aux.
%-----------------------------------------------------------------------------%
