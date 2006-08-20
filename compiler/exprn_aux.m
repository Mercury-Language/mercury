%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: exprn_aux.m.
% 
%-----------------------------------------------------------------------------%

:- module ll_backend.exprn_aux.
:- interface.

:- import_module libs.options.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type exprn_opts
    --->    nlg_asm_sgt_ubf(
                non_local_gotos     :: bool,
                asm_labels          :: bool,
                static_ground_terms :: bool,
                unboxed_float       :: bool
            ).

:- pred init_exprn_opts(option_table::in, exprn_opts::out) is det.

    % Determine whether an rval_const can be used as the initializer
    % of a C static constant.
    %
:- pred const_is_constant(rval_const::in, exprn_opts::in, bool::out) is det.

:- pred rval_contains_lval(rval::in, lval::in) is semidet.

:- pred rval_contains_rval(rval, rval).
:- mode rval_contains_rval(in, in) is semidet.
:- mode rval_contains_rval(in, out) is multidet.

:- pred args_contain_rval(list(maybe(rval)), rval).
:- mode args_contain_rval(in, in) is semidet.
:- mode args_contain_rval(in, out) is nondet.

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
    list(code_addr)::out, list(data_addr)::out) is det.

:- pred lval_list_addrs(list(lval)::in,
    list(code_addr)::out, list(data_addr)::out) is det.

:- pred rval_addrs(rval::in, list(code_addr)::out, list(data_addr)::out)
    is det.

:- pred lval_addrs(lval::in, list(code_addr)::out, list(data_addr)::out)
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
:- import_module pair.
:- import_module set.

%-----------------------------------------------------------------------------%

init_exprn_opts(Options, ExprnOpts) :-
    getopt_io.lookup_bool_option(Options, gcc_non_local_gotos, NLG),
    getopt_io.lookup_bool_option(Options, asm_labels, ASM),
    getopt_io.lookup_bool_option(Options, static_ground_terms, SGT),
    getopt_io.lookup_bool_option(Options, unboxed_float, UBF),
    ExprnOpts = nlg_asm_sgt_ubf(NLG, ASM, SGT, UBF).

% Determine whether a const (well, what _we_ consider to be a const)
% is constant as far as the C compiler is concerned -- specifically,
% determine whether it can be used as the initializer of a C static
% constant.

const_is_constant(true, _, yes).
const_is_constant(false, _, yes).
const_is_constant(int_const(_), _, yes).
const_is_constant(float_const(_), ExprnOpts, IsConst) :-
    ExprnOpts = nlg_asm_sgt_ubf(_NLG, _ASM, StaticGroundTerms, UnboxedFloat),
    (
        UnboxedFloat = yes,
        % If we're using unboxed (single-precision) floats,
        % floating point values are always constants.
        IsConst = yes
    ;
        UnboxedFloat = no,
        % If we're using boxed floats, then we can generate a static constant
        % variable to hold a float constant, and gcc doesn't mind us converting
        % from its address to word in a static initializer. However, we only do
        % this if --static-ground-terms is enabled.
        IsConst = StaticGroundTerms
    ).
const_is_constant(string_const(_), _, yes).
const_is_constant(multi_string_const(_, _), _, yes).
const_is_constant(code_addr_const(CodeAddr), ExprnOpts, IsConst) :-
    addr_is_constant(CodeAddr, ExprnOpts, IsConst).
const_is_constant(data_addr_const(_, _), _, yes).

:- pred addr_is_constant(code_addr::in, exprn_opts::in, bool::out) is det.

addr_is_constant(label(Label), ExprnOpts, IsConst) :-
    ExprnOpts = nlg_asm_sgt_ubf(NonLocalGotos, AsmLabels, _SGT, _UBF),
    label_is_constant(Label, NonLocalGotos, AsmLabels, IsConst).
addr_is_constant(imported(_), ExprnOpts, IsConst) :-
    ExprnOpts = nlg_asm_sgt_ubf(NonLocalGotos, AsmLabels, _SGT, _UBF),
    globals.imported_is_constant(NonLocalGotos, AsmLabels, IsConst).
addr_is_constant(succip, _, no).
addr_is_constant(do_succeed(_), _, no).
addr_is_constant(do_redo, _, no).
addr_is_constant(do_fail, _, no).
addr_is_constant(do_trace_redo_fail_shallow, _, no).
addr_is_constant(do_trace_redo_fail_deep, _, no).
addr_is_constant(do_call_closure(_), _, no).
addr_is_constant(do_call_class_method(_), _, no).
addr_is_constant(do_not_reached, _, no).

:- pred label_is_constant(label::in, bool::in, bool::in, bool::out) is det.

label_is_constant(entry(exported, _), NonLocalGotos, AsmLabels, IsConst) :-
    globals.imported_is_constant(NonLocalGotos, AsmLabels, IsConst).
label_is_constant(entry(local, _), NonLocalGotos, AsmLabels, IsConst) :-
    globals.imported_is_constant(NonLocalGotos, AsmLabels, IsConst).
label_is_constant(entry(c_local, _), _NonLocalGotos, _AsmLabels, yes).
label_is_constant(internal(_, _), _NonLocalGotos, _AsmLabels, yes).

%-----------------------------------------------------------------------------%

rval_contains_lval(lval(Lval0), Lval) :-
    lval_contains_lval(Lval0, Lval).
rval_contains_lval(mkword(_, Rval), Lval) :-
    rval_contains_lval(Rval, Lval).
rval_contains_lval(unop(_, Rval), Lval) :-
    rval_contains_lval(Rval, Lval).
rval_contains_lval(binop(_, Rval0, Rval1), Lval) :-
    (
        rval_contains_lval(Rval0, Lval)
    ;
        rval_contains_lval(Rval1, Lval)
    ).

:- pred lval_contains_lval(lval::in, lval::in) is semidet.

lval_contains_lval(Lval0, Lval) :-
    ( Lval0 = Lval ->
        true
    ; Lval0 = field(_MaybeTag, Rval0, Rval1) ->
        (
            rval_contains_lval(Rval0, Lval)
        ;
            rval_contains_lval(Rval1, Lval)
        )
    ; Lval0 = lvar(_Var) ->
        unexpected(this_file, "lval_contains_lval: var! I can't tell")
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

rval_contains_rval(Rval0, Rval) :-
    (
        Rval0 = Rval
    ;
        (
            Rval0 = lval(Lval),
            lval_contains_rval(Lval, Rval)
        ;
            Rval0 = mkword(_, Rval1),
            rval_contains_rval(Rval1, Rval)
        ;
            Rval0 = unop(_Unop, Rval1),
            rval_contains_rval(Rval1, Rval)
        ;
            Rval0 = binop(_Binop, Rval1, Rval2),
            (
                rval_contains_rval(Rval1, Rval)
            ;
                rval_contains_rval(Rval2, Rval)
            )
        )
    ).

:- pred lval_contains_rval(lval, rval).
:- mode lval_contains_rval(in, in) is semidet.
:- mode lval_contains_rval(in, out) is nondet.

lval_contains_rval(field(_MaybeTag, Rval0, Rval1), Rval) :-
    (
        rval_contains_rval(Rval0, Rval)
    ;
        rval_contains_rval(Rval1, Rval)
    ).

args_contain_rval([M | Ms], Rval) :-
    (
        M = yes(Rval0),
        rval_contains_rval(Rval0, Rval)
    ;
        args_contain_rval(Ms, Rval)
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
vars_in_rval(binop(_Binop, Rval0, Rval1), Vars) :-
    vars_in_rval(Rval0, Vars0),
    vars_in_rval(Rval1, Vars1),
    list.append(Vars0, Vars1, Vars).
vars_in_rval(mem_addr(MemRef), Vars) :-
    vars_in_mem_ref(MemRef, Vars).

vars_in_lval(reg(_Type, _RegNum), []).
vars_in_lval(temp(_Type, _TmpNum), []).
vars_in_lval(succip, []).
vars_in_lval(maxfr, []).
vars_in_lval(curfr, []).
vars_in_lval(hp, []).
vars_in_lval(sp, []).
vars_in_lval(stackvar(_SlotNum), []).
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
vars_in_lval(field(_MaybeTag, Rval0, Rval1), Vars) :-
    vars_in_rval(Rval0, Vars0),
    vars_in_rval(Rval1, Vars1),
    list.append(Vars0, Vars1, Vars).
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

substitute_lval_in_lval(OldLval, NewLval, Lval0, Lval) :-
    substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval,
        0, _SubstCount).

substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval) :-
    substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval,
        0, _SubstCount).

substitute_lval_in_instr(OldLval, NewLval, Instr0, Instr, !N) :-
    Instr0 = Uinstr0 - Comment,
    substitute_lval_in_uinstr(OldLval, NewLval, Uinstr0, Uinstr, !N),
    Instr = Uinstr - Comment.

:- pred substitute_lval_in_uinstr(lval::in, lval::in,
    instr::in, instr::out, int::in, int::out) is det.

substitute_lval_in_uinstr(OldLval, NewLval, Uinstr0, Uinstr, !N) :-
    (
        ( Uinstr0 = comment(_Comment)
        ; Uinstr0 = llcall(_, _, _, _, _, _)
        ; Uinstr0 = mkframe(_, _)
        ; Uinstr0 = label(_)
        ; Uinstr0 = goto(_)
        ; Uinstr0 = prune_ticket
        ; Uinstr0 = discard_ticket
        ; Uinstr0 = incr_sp(_, _)
        ; Uinstr0 = decr_sp(_)
        ; Uinstr0 = decr_sp_and_return(_)
        ; Uinstr0 = fork(_, _, _)
        ),
        Uinstr = Uinstr0
    ;
        Uinstr0 = livevals(LvalSet0),
        set.to_sorted_list(LvalSet0, Lvals0),
        list.map_foldl(substitute_lval_in_lval_count(OldLval, NewLval),
            Lvals0, Lvals, !N),
        set.list_to_set(Lvals, LvalSet),
        Uinstr = livevals(LvalSet)
    ;
        Uinstr0 = block(TempR, TempF, Instrs0),
        list.map_foldl(substitute_lval_in_instr(OldLval, NewLval),
            Instrs0, Instrs, !N),
        Uinstr = block(TempR, TempF, Instrs)
    ;
        Uinstr0 = assign(Lval0, Rval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = assign(Lval, Rval)
    ;
        Uinstr0 = computed_goto(Rval0, Labels),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = computed_goto(Rval, Labels)
    ;
        Uinstr0 = arbitrary_c_code(Code, LiveLvals0),
        substitute_lval_in_live_lval_info(OldLval, NewLval,
            LiveLvals0, LiveLvals, !N),
        Uinstr = arbitrary_c_code(Code, LiveLvals)
    ;
        Uinstr0 = if_val(Rval0, CodeAddr),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = if_val(Rval, CodeAddr)
    ;
        Uinstr0 = save_maxfr(Lval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = save_maxfr(Lval)
    ;
        Uinstr0 = restore_maxfr(Lval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = restore_maxfr(Lval)
    ;
        Uinstr0 = incr_hp(Lval0, MaybeTag, MO, Rval0, TypeCtor, MayUseAtomic),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = incr_hp(Lval, MaybeTag, MO, Rval, TypeCtor, MayUseAtomic)
    ;
        Uinstr0 = mark_hp(Lval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = mark_hp(Lval)
    ;
        Uinstr0 = restore_hp(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = restore_hp(Rval)
    ;
        Uinstr0 = free_heap(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = free_heap(Rval)
    ;
        Uinstr0 = store_ticket(Lval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = store_ticket(Lval)
    ;
        Uinstr0 = reset_ticket(Rval0, Reason),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = reset_ticket(Rval, Reason)
    ;
        Uinstr0 = mark_ticket_stack(Lval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = mark_ticket_stack(Lval)
    ;
        Uinstr0 = prune_tickets_to(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Uinstr = prune_tickets_to(Rval)
%   ;
%       % discard_tickets_to(_) is used only in hand-written code
%       Uinstr0 = discard_tickets_to(Rval0),
%       substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval, !N),
%       Uinstr = discard_tickets_to(Rval)
    ;
        Uinstr0 = pragma_c(Decls, Components0, MayCallMercury,
            MaybeLabel1, MaybeLabel2, MaybeLabel3, MaybeLabel4,
            ReferStackSlot, MayDupl),
        list.map_foldl(substitute_lval_in_component(OldLval, NewLval),
            Components0, Components, !N),
        Uinstr = pragma_c(Decls, Components, MayCallMercury,
            MaybeLabel1, MaybeLabel2, MaybeLabel3, MaybeLabel4,
            ReferStackSlot, MayDupl)
    ;
        Uinstr0 = init_sync_term(Lval0, BranchCount),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = init_sync_term(Lval, BranchCount)
    ;
        Uinstr0 = join_and_terminate(Lval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = join_and_terminate(Lval)
    ;
        Uinstr0 = join_and_continue(Lval0, Label),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Uinstr = join_and_continue(Lval, Label)
    ).

:- pred substitute_lval_in_component(lval::in, lval::in,
    pragma_c_component::in, pragma_c_component::out, int::in, int::out) is det.

substitute_lval_in_component(OldLval, NewLval,
        Component0, Component, !N) :-
    (
        Component0 = pragma_c_inputs(Inputs0),
        list.map_foldl(substitute_lval_in_pragma_c_input(OldLval, NewLval),
            Inputs0, Inputs, !N),
        Component = pragma_c_inputs(Inputs)
    ;
        Component0 = pragma_c_outputs(Outputs0),
        list.map_foldl(substitute_lval_in_pragma_c_output(OldLval, NewLval),
            Outputs0, Outputs, !N),
        Component = pragma_c_outputs(Outputs)
    ;
        Component0 = pragma_c_user_code(_, _),
        Component = Component0
    ;
        Component0 = pragma_c_raw_code(Code, CanBranchAway, LvalSet0),
        substitute_lval_in_live_lval_info(OldLval, NewLval,
            LvalSet0, LvalSet, !N),
        Component = pragma_c_raw_code(Code, CanBranchAway, LvalSet)
    ;
        Component0 = pragma_c_fail_to(_),
        Component = Component0
    ;
        Component0 = pragma_c_noop,
        Component = Component0
    ).

:- pred substitute_lval_in_live_lval_info(lval::in, lval::in,
    c_code_live_lvals::in, c_code_live_lvals::out, int::in, int::out) is det.

substitute_lval_in_live_lval_info(_OldLval, _NewLval,
        no_live_lvals_info, no_live_lvals_info, !N).
substitute_lval_in_live_lval_info(OldLval, NewLval,
        live_lvals_info(LvalSet0), live_lvals_info(LvalSet), !N) :-
    Lvals0 = set.to_sorted_list(LvalSet0),
    list.map_foldl(substitute_lval_in_lval_count(OldLval, NewLval),
        Lvals0, Lvals, !N),
    set.list_to_set(Lvals, LvalSet).

:- pred substitute_lval_in_pragma_c_input(lval::in, lval::in,
    pragma_c_input::in, pragma_c_input::out, int::in, int::out) is det.

substitute_lval_in_pragma_c_input(OldLval, NewLval, Out0, Out,
        !N) :-
    Out0 = pragma_c_input(Name, VarType, IsDummy, OrigType, Rval0,
        MaybeForeign, BoxPolicy),
    substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
    Out = pragma_c_input(Name, VarType, IsDummy, OrigType, Rval,
        MaybeForeign, BoxPolicy).

:- pred substitute_lval_in_pragma_c_output(lval::in, lval::in,
    pragma_c_output::in, pragma_c_output::out, int::in, int::out) is det.

substitute_lval_in_pragma_c_output(OldLval, NewLval, Out0, Out, !N) :-
    Out0 = pragma_c_output(Lval0, VarType, IsDummy, OrigType, Name,
        MaybeForeign, BoxPolicy),
    substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
    Out = pragma_c_output(Lval, VarType, IsDummy, OrigType, Name,
        MaybeForeign, BoxPolicy).

:- pred substitute_lval_in_rval_count(lval::in, lval::in,
    rval::in, rval::out, int::in, int::out) is det.

substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N) :-
    (
        Rval0 = lval(Lval0),
        substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval, !N),
        Rval = lval(Lval)
    ;
        Rval0 = var(_Var),
        Rval = Rval0
    ;
        Rval0 = mkword(Tag, Rval1),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval1, Rval2, !N),
        Rval = mkword(Tag, Rval2)
    ;
        Rval0 = const(_Const),
        Rval = Rval0
    ;
        Rval0 = unop(Unop, Rval1),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval1, Rval2, !N),
        Rval = unop(Unop, Rval2)
    ;
        Rval0 = binop(Binop, Rval1, Rval2),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval1, Rval3, !N),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval2, Rval4, !N),
        Rval = binop(Binop, Rval3, Rval4)
    ;
        Rval0 = mem_addr(MemRef0),
        substitute_lval_in_mem_ref(OldLval, NewLval, MemRef0, MemRef, !N),
        Rval = mem_addr(MemRef)
    ).

:- pred substitute_lval_in_mem_ref(lval::in, lval::in,
    mem_ref::in, mem_ref::out, int::in, int::out) is det.

substitute_lval_in_mem_ref(OldLval, NewLval, MemRef0, MemRef, !N) :-
    (
        MemRef0 = stackvar_ref(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        MemRef = stackvar_ref(Rval)
    ;
        MemRef0 = framevar_ref(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        MemRef = framevar_ref(Rval)
    ;
        MemRef0 = heap_ref(BaseRval0, Tag, FieldRval0),
        substitute_lval_in_rval_count(OldLval, NewLval, BaseRval0, BaseRval,
            !N),
        substitute_lval_in_rval_count(OldLval, NewLval, FieldRval0, FieldRval,
            !N),
        MemRef = heap_ref(BaseRval, Tag, FieldRval)
    ).

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
    (
        ( Lval0 = reg(_Type, _RegNum)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = temp(_Type, _TmpNum)
        ; Lval0 = stackvar(_SlotNum)
        ; Lval0 = framevar(_SlotNum)
        ; Lval0 = lvar(_Var)
        ; Lval0 = global_var_ref(_GlobalVarName)
        ),
        Lval = Lval0
    ;
        Lval0 = succip_slot(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Lval = succip_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = field(Tag, Rval1, Rval2),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval1, Rval3, !N),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval2, Rval4, !N),
        Lval = field(Tag, Rval3, Rval4)
    ;
        Lval0 = mem_ref(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        Lval = mem_ref(Rval)
    ).

:- pred substitute_lval_in_args(lval::in, lval::in,
    list(maybe(rval))::in, list(maybe(rval))::out, int::in, int::out) is det.

substitute_lval_in_args(_OldLval, _NewLval, [], [], !N).
substitute_lval_in_args(OldLval, NewLval, [M0 | Ms0], [M | Ms], !N) :-
    substitute_lval_in_arg(OldLval, NewLval, M0, M, !N),
    substitute_lval_in_args(OldLval, NewLval, Ms0, Ms, !N).

:- pred substitute_lval_in_arg(lval::in, lval::in,
    maybe(rval)::in, maybe(rval)::out, int::in, int::out) is det.

substitute_lval_in_arg(OldLval, NewLval, MaybeRval0, MaybeRval, !N) :-
    (
        MaybeRval0 = yes(Rval0),
        substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval, !N),
        MaybeRval = yes(Rval)
    ;
        MaybeRval0 = no,
        MaybeRval = MaybeRval0
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
        ; Lval0 = temp(_, _)
        ; Lval0 = stackvar(_)
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

:- pred substitute_rval_in_args(rval::in, rval::in,
    list(maybe(rval))::in, list(maybe(rval))::out) is det.

substitute_rval_in_args(_OldRval, _NewRval, [], []).
substitute_rval_in_args(OldRval, NewRval, [M0 | Ms0], [M | Ms]) :-
    substitute_rval_in_arg(OldRval, NewRval, M0, M),
    substitute_rval_in_args(OldRval, NewRval, Ms0, Ms).

:- pred substitute_rval_in_arg(rval::in, rval::in,
    maybe(rval)::in, maybe(rval)::out) is det.

substitute_rval_in_arg(OldRval, NewRval, MaybeRval0, MaybeRval) :-
    (
        MaybeRval0 = yes(Rval0),
        substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval),
        MaybeRval = yes(Rval)
    ;
        MaybeRval0 = no,
        MaybeRval = MaybeRval0
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

rval_addrs(lval(Lval), CodeAddrs, DataAddrs) :-
    lval_addrs(Lval, CodeAddrs, DataAddrs).
rval_addrs(var(_Var), [], []).
rval_addrs(mkword(_Tag, Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
rval_addrs(const(Const), CodeAddrs, DataAddrs) :-
    ( Const = code_addr_const(CodeAddress) ->
        CodeAddrs = [CodeAddress],
        DataAddrs = []
    ; Const = data_addr_const(DataAddress, _) ->
        CodeAddrs = [],
        DataAddrs = [DataAddress]
    ;
        CodeAddrs = [],
        DataAddrs = []
    ).
rval_addrs(unop(_Unop, Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
rval_addrs(binop(_Binop, Rval1, Rval2), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval1, CodeAddrs1, DataAddrs1),
    rval_addrs(Rval2, CodeAddrs2, DataAddrs2),
    list.append(CodeAddrs1, CodeAddrs2, CodeAddrs),
    list.append(DataAddrs1, DataAddrs2, DataAddrs).
rval_addrs(mem_addr(Rval), CodeAddrs, DataAddrs) :-
    mem_ref_addrs(Rval, CodeAddrs, DataAddrs).

lval_addrs(reg(_Type, _RegNum), [], []).
lval_addrs(stackvar(_SlotNum), [], []).
lval_addrs(framevar(_SlotNum), [], []).
lval_addrs(succip, [], []).
lval_addrs(maxfr, [], []).
lval_addrs(curfr, [], []).
lval_addrs(prevfr_slot(Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
lval_addrs(succfr_slot(Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
lval_addrs(redofr_slot(Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
lval_addrs(redoip_slot(Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
lval_addrs(succip_slot(Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
lval_addrs(hp, [], []).
lval_addrs(sp, [], []).
lval_addrs(field(_Tag, Rval1, Rval2), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval1, CodeAddrs1, DataAddrs1),
    rval_addrs(Rval2, CodeAddrs2, DataAddrs2),
    list.append(CodeAddrs1, CodeAddrs2, CodeAddrs),
    list.append(DataAddrs1, DataAddrs2, DataAddrs).
lval_addrs(lvar(_Var), [], []).
lval_addrs(temp(_Type, _TmpNum), [], []).
lval_addrs(mem_ref(Rval), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).
lval_addrs(global_var_ref(_), [], []).

rval_list_addrs([], [], []).
rval_list_addrs([Rval | Rvals], CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs0, DataAddrs0),
    rval_list_addrs(Rvals, CodeAddrs1, DataAddrs1),
    list.append(CodeAddrs0, CodeAddrs1, CodeAddrs),
    list.append(DataAddrs0, DataAddrs1, DataAddrs).

lval_list_addrs([], [], []).
lval_list_addrs([Lval | Lvals], CodeAddrs, DataAddrs) :-
    lval_addrs(Lval, CodeAddrs0, DataAddrs0),
    lval_list_addrs(Lvals, CodeAddrs1, DataAddrs1),
    list.append(CodeAddrs0, CodeAddrs1, CodeAddrs),
    list.append(DataAddrs0, DataAddrs1, DataAddrs).

:- pred mem_ref_addrs(mem_ref::in,
    list(code_addr)::out, list(data_addr)::out) is det.

mem_ref_addrs(stackvar_ref(_SlotNum), [], []).
mem_ref_addrs(framevar_ref(_SlotNum), [], []).
mem_ref_addrs(heap_ref(Rval, _Tag, _FieldNum), CodeAddrs, DataAddrs) :-
    rval_addrs(Rval, CodeAddrs, DataAddrs).

    % Give a list of maybe(rval), return a list of the code and data
    % addresses that are referenced by that list.
    %
:- pred maybe_rval_list_addrs(list(maybe(rval))::in,
    list(code_addr)::out, list(data_addr)::out) is det.

maybe_rval_list_addrs([], [], []).
maybe_rval_list_addrs([MaybeRval | MaybeRvals], CodeAddrs, DataAddrs) :-
    (
        MaybeRval = yes(Rval),
        rval_addrs(Rval, CodeAddrs0, DataAddrs0),
        maybe_rval_list_addrs(MaybeRvals, CodeAddrs1, DataAddrs1),
        list.append(CodeAddrs0, CodeAddrs1, CodeAddrs),
        list.append(DataAddrs0, DataAddrs1, DataAddrs)
    ;
        MaybeRval = no,
        maybe_rval_list_addrs(MaybeRvals, CodeAddrs, DataAddrs)
    ).

var_lval_to_rval(_Var, Lval) = lval(Lval).

lval_to_rval(Lval) = lval(Lval).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "exprn_aux.m".

%-----------------------------------------------------------------------------%
:- end_module exprn_aux.
%-----------------------------------------------------------------------------%
