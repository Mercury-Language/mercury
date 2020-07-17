%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: use_local_vars.m
% Author: zs.
%
% This module implements an LLDS->LLDS transformation that optimizes the
% sequence of instructions in a procedure body by replacing references to
% relatively expensive locations: fake registers (Mercury abstract machine
% registers that are not mapped to machine registers) or stack slots with
% references to cheaper locations: local variables in C blocks, which should
% be mapped to machine registers by the C compiler. The C blocks should be
% introduced later by wrap_blocks.m, possibly after the LLDS code has been
% transformed further. Wrap_blocks will know what local variables to declare
% in each block by looking for the temp(_, _) lvals that represent those local
% variables.
%
% This module looks for three patterns. The first is
%
%   <instruction that defines a fake register>
%   <instructions that use and possibly define the fake register>
%   <end of basic block, at which the fake register is not live>
%
% When it finds an occurrence of that pattern, it replaces all references to
% the fake register with a local variable.
%
% If the basic block jumps to a code address which is not a label (e.g.
% do_redo, do_fail), we consider all registers to be live at the end of the
% basic block. This is because livemap.m, which computes liveness information
% for us, does not know about liveness requirements introduced by backtracking.
% This is a conservative approximation. The union of the livenesses of all the
% labels that represent resume points is a better approximation, but it would
% be tedious to compute and is unlikely to yield significantly better code.
%
% The second pattern we look for is simply an instruction that defines a fake
% register or stack slot, followed by some uses of that register or stack slot
% before code that redefines the register or stack slot. When we find this
% pattern, we again replace all references to the fake register or stack slot
% with a local variable, but since this time we cannot be sure that the
% original lval will not be referred to, we assign the local variable to the
% lval as well. This is a win because the cost of the assignment is less than
% the savings from replacing the fake register or stack slot references with
% local variable references.
%
% The third pattern we look for consists of a sequence of instructions in which
% a false register or stack slot is used several times, including at least once
% in the first instruction as a part of a path to a memory location, before
% being redefined or maybe aliased. This typically occurs when the code
% generator fills in the fields of a structure or extracts the fields of a
% structure. Again, we replace the false register or stack slot with a
% temporary after assigning the value in the false register or stack slot to
% the temporary.
%
% If we cannot find out what registers are live at each label, we still look
% for the second and third patterns.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.use_local_vars.
:- interface.

:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module counter.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred use_local_vars_proc(list(instruction)::in, list(instruction)::out,
    int::in, int::in, maybe_auto_comments::in, proc_label::in,
    counter::in, counter::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.basic_block.
:- import_module ll_backend.code_util.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.livemap.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.opt_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

use_local_vars_proc(Instrs0, Instrs, NumRealRRegs, AccessThreshold,
        AutoComments, ProcLabel, !C) :-
    create_basic_blocks(Instrs0, Comments0, ProcLabel, !C, NewLabels,
        LabelSeq, BlockMap0),
    flatten_basic_blocks(LabelSeq, BlockMap0, TentativeInstrs,
        NumTentativeInstrs),
    % If the number of instructions in the procedure is too big,
    % then building MaybeLiveMap will probably take too long.
    ( if NumTentativeInstrs < local_vars_proc_size_limit then
        build_livemap(TentativeInstrs, MaybeLiveMap),
        extend_basic_blocks(LabelSeq, EBBLabelSeq, BlockMap0, EBBBlockMap0,
            NewLabels),
        list.foldl(use_local_vars_block(MaybeLiveMap, NumRealRRegs,
            AccessThreshold), EBBLabelSeq, EBBBlockMap0, EBBBlockMap),
        flatten_basic_blocks(EBBLabelSeq, EBBBlockMap, Instrs1, _),

        ( if
            MaybeLiveMap = yes(LiveMap),
            AutoComments = auto_comments
        then
            NewComment = "\n" ++ dump_livemap(yes(ProcLabel), LiveMap),
            NewCommentInstr = llds_instr(comment(NewComment), ""),
            Comments = Comments0 ++ [NewCommentInstr]
        else
            Comments = Comments0
        ),
        Instrs = Comments ++ Instrs1
    else
        Instrs = Instrs0
    ).

:- pred use_local_vars_block(maybe(livemap)::in, int::in, int::in, label::in,
    block_map::in, block_map::out) is det.

use_local_vars_block(MaybeLiveMap, NumRealRRegs, AccessThreshold, Label,
        !BlockMap) :-
    map.lookup(!.BlockMap, Label, BlockInfo0),
    BlockInfo0 = block_info(BlockLabel, LabelInstr, RestInstrs0, BlockSize,
        FallInto, JumpLabels, MaybeFallThrough),
    % The algorithm we use is half-quadratic, so using it on long instruction
    % lists is not a good idea. The correct fix of course would be to reduce
    % the complexity of the algorithm, but that is not trivial to do.
    ( if BlockSize < local_vars_block_size_limit then
        counter.init(1, TempCounter0),
        use_local_vars_instrs(RestInstrs0, RestInstrs,
            TempCounter0, TempCounter, NumRealRRegs,
            AccessThreshold, MaybeLiveMap, MaybeFallThrough),
        ( if TempCounter = TempCounter0 then
            true
        else
            BlockInfo = block_info(BlockLabel, LabelInstr, RestInstrs,
                BlockSize, FallInto, JumpLabels, MaybeFallThrough),
            map.det_update(Label, BlockInfo, !BlockMap)
        )
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred use_local_vars_instrs(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out, int::in, int::in, maybe(livemap)::in,
    maybe(label)::in) is det.

use_local_vars_instrs(!RestInstrs, !TempCounter,
        NumRealRRegs, AccessThreshold, MaybeLiveMap, MaybeFallThrough) :-
    opt_assign(!RestInstrs, !TempCounter, NumRealRRegs, [], MaybeLiveMap,
        MaybeFallThrough),
    ( if AccessThreshold >= 1 then
        opt_access(!RestInstrs, !TempCounter, NumRealRRegs,
            set.init, AccessThreshold)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred opt_assign(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out, int::in, list(lval)::in,
    maybe(livemap)::in, maybe(label)::in) is det.

opt_assign([], [], !TempCounter, _, _, _, _).
opt_assign([Instr0 | TailInstrs0], Instrs, !TempCounter, NumRealRRegs,
        !.AvoidLvals, MaybeLiveMap, MaybeFallThrough) :-
    Instr0 = llds_instr(Uinstr0, _Comment0),
    ( if
        (
            % We do not optimize keep_assign instructions.
            (
                Uinstr0 = assign(ToLval, _FromRval)
            ;
                Uinstr0 = incr_hp(ToLval, _MaybeTag, _SizeRval, _MO, _Type,
                    _Atomic, _, _)
            ),
            base_lval_worth_replacing(NumRealRRegs, ToLval),
            MaybeMore = no
        ;
            Uinstr0 = foreign_proc_code(_D, Comps, _MCM, _FNL, _FL, _FOL, _NF,
                _MDL, _S, _MD),
            opt_assign_find_output_in_components(Comps, NumRealRRegs,
                !.AvoidLvals, ToLval),
            MaybeMore = yes
        )
    then
        ( if
            ToLval = reg(_, _),
            find_compulsory_lvals(TailInstrs0, MaybeLiveMap, MaybeFallThrough,
                no, MaybeCompulsoryLvals),
            MaybeCompulsoryLvals = known(CompulsoryLvals),
            not set.member(ToLval, CompulsoryLvals)
        then
            counter.allocate(TempNum, !TempCounter),
            NewLval = temp(reg_type_for_lval(ToLval), TempNum),
            substitute_lval_in_defn(ToLval, NewLval, Instr0, Instr),
            list.map_foldl(
                exprn_aux.substitute_lval_in_instr(ToLval, NewLval),
                TailInstrs0, TailInstrs1, 0, _),
            (
                MaybeMore = no,
                opt_assign(TailInstrs1, TailInstrs, !TempCounter, NumRealRRegs,
                    [], MaybeLiveMap, MaybeFallThrough),
                Instrs = [Instr | TailInstrs]
            ;
                MaybeMore = yes,
                !:AvoidLvals = [ToLval | !.AvoidLvals],
                Instrs1 = [Instr | TailInstrs1],
                opt_assign(Instrs1, Instrs, !TempCounter, NumRealRRegs,
                    !.AvoidLvals, MaybeLiveMap, MaybeFallThrough)
            )
        else if
            counter.allocate(TempNum, !TempCounter),
            NewLval = temp(reg_type_for_lval(ToLval), TempNum),
            substitute_lval_in_instr_until_defn(ToLval, NewLval,
                TailInstrs0, TailInstrs1, 0, NumSubst),
            NumSubst > 1
        then
            substitute_lval_in_defn(ToLval, NewLval, Instr0, Instr),
            CopyInstr = llds_instr(assign(ToLval, lval(NewLval)), ""),
            (
                MaybeMore = no,
                opt_assign(TailInstrs1, TailInstrs, !TempCounter, NumRealRRegs,
                    [], MaybeLiveMap, MaybeFallThrough),
                Instrs = [Instr, CopyInstr | TailInstrs]
            ;
                MaybeMore = yes,
                !:AvoidLvals = [ToLval | !.AvoidLvals],
                Instrs1 = [Instr, CopyInstr | TailInstrs1],
                opt_assign(Instrs1, Instrs, !TempCounter, NumRealRRegs,
                    !.AvoidLvals, MaybeLiveMap, MaybeFallThrough)
            )
        else
            (
                MaybeMore = no,
                opt_assign(TailInstrs0, TailInstrs, !TempCounter, NumRealRRegs,
                    [], MaybeLiveMap, MaybeFallThrough),
                Instrs = [Instr0 | TailInstrs]
            ;
                MaybeMore = yes,
                !:AvoidLvals = [ToLval | !.AvoidLvals],
                Instrs1 = [Instr0 | TailInstrs0],
                disable_warning [suspicious_recursion] (
                    opt_assign(Instrs1, Instrs, !TempCounter, NumRealRRegs,
                        !.AvoidLvals, MaybeLiveMap, MaybeFallThrough)
                )
            )
        )
    else
        opt_assign(TailInstrs0, TailInstrs, !TempCounter, NumRealRRegs,
            [], MaybeLiveMap, MaybeFallThrough),
        Instrs = [Instr0 | TailInstrs]
    ).

:- pred opt_assign_find_output_in_components(list(foreign_proc_component)::in,
    int::in, list(lval)::in, lval::out) is semidet.

opt_assign_find_output_in_components([Comp | Comps], NumRealRRegs, AvoidLvals,
        ToLval) :-
    ( if
        Comp = foreign_proc_outputs(Outputs),
        opt_assign_find_output_in_outputs(Outputs, NumRealRRegs, AvoidLvals,
            ToLvalPrime)
    then
        ToLval = ToLvalPrime
    else
        opt_assign_find_output_in_components(Comps, NumRealRRegs, AvoidLvals,
            ToLval)
    ).

:- pred opt_assign_find_output_in_outputs(list(foreign_proc_output)::in,
    int::in, list(lval)::in, lval::out) is semidet.

opt_assign_find_output_in_outputs([Output | Outputs], NumRealRRegs, AvoidLvals,
        ToLval) :-
    Output = foreign_proc_output(Dest, _Type, _IsDummy, _VarName,
        _OrigType, _MaybeForeignType, _BoxPolicy),
    ( if
        base_lval_worth_replacing(NumRealRRegs, Dest),
        not list.member(Dest, AvoidLvals)
    then
        ToLval = Dest
    else
        opt_assign_find_output_in_outputs(Outputs, NumRealRRegs, AvoidLvals,
            ToLval)
    ).

%-----------------------------------------------------------------------------%

:- type maybe_compulsory_lvals
    --->    known(lvalset)
    ;       unknown_must_assume_all.

:- pred find_compulsory_lvals(list(instruction)::in, maybe(livemap)::in,
    maybe(label)::in, bool::in, maybe_compulsory_lvals::out) is det.

find_compulsory_lvals([], MaybeLiveMap, MaybeFallThrough, _PrevLivevals,
        MaybeCompulsoryLvals) :-
    (
        MaybeFallThrough = yes(FallThrough),
        (
            MaybeLiveMap = yes(LiveMap),
            map.lookup(LiveMap, FallThrough, CompulsoryLvals),
            MaybeCompulsoryLvals = known(CompulsoryLvals)
        ;
            MaybeLiveMap = no,
            MaybeCompulsoryLvals = unknown_must_assume_all
        )
    ;
        MaybeFallThrough = no,
        MaybeCompulsoryLvals = unknown_must_assume_all
    ).
find_compulsory_lvals([Instr | Instrs], MaybeLiveMap, MaybeFallThrough,
        PrevLivevals, !:MaybeCompulsoryLvals) :-
    Instr = llds_instr(Uinstr, _),
    ( if
        Uinstr = livevals(LiveLvals)
    then
        find_compulsory_lvals(Instrs, MaybeLiveMap, MaybeFallThrough,
            yes, !:MaybeCompulsoryLvals),
        union_maybe_compulsory_lvals(LiveLvals, !MaybeCompulsoryLvals)
    else if
        Uinstr = llcall(_, _, _, _, _, _)
    then
        expect(unify(PrevLivevals, yes), $pred, "call without livevals"),
        % The livevals instruction will include all the live lvals
        % in MaybeCompulsoryLvals after we return.
        !:MaybeCompulsoryLvals = known(set.init)
    else if
        Uinstr = goto(_Target),
        PrevLivevals = yes
    then
        % The livevals instruction will include all the live lvals
        % in MaybeCompulsoryLvals after we return.
        !:MaybeCompulsoryLvals = known(set.init)
    else
        possible_targets(Uinstr, Labels, NonLabelCodeAddrs),
        (
            NonLabelCodeAddrs = [],
            (
                Labels = [],
                % Optimize the common case.
                find_compulsory_lvals(Instrs, MaybeLiveMap, MaybeFallThrough,
                    no, !:MaybeCompulsoryLvals)
            ;
                Labels = [_ | _],
                (
                    MaybeLiveMap = yes(LiveMap),
                    list.map(map.lookup(LiveMap), Labels, LabelsLiveLvals),
                    AllLabelsLiveLvals = set.union_list(LabelsLiveLvals),
                    find_compulsory_lvals(Instrs, MaybeLiveMap,
                        MaybeFallThrough, no, !:MaybeCompulsoryLvals),
                    union_maybe_compulsory_lvals(AllLabelsLiveLvals,
                        !MaybeCompulsoryLvals)
                ;
                    MaybeLiveMap = no,
                    !:MaybeCompulsoryLvals = unknown_must_assume_all
                )
            )
        ;
            NonLabelCodeAddrs = [_ | _],
            !:MaybeCompulsoryLvals = unknown_must_assume_all
        )
    ).

:- pred union_maybe_compulsory_lvals(lvalset::in,
    maybe_compulsory_lvals::in, maybe_compulsory_lvals::out) is det.

union_maybe_compulsory_lvals(New, !MaybeCompulsoryLvals) :-
    (
        !.MaybeCompulsoryLvals = known(OldCompulsoryLvals),
        set.union(New, OldCompulsoryLvals, AllCompulsoryLvals),
        !:MaybeCompulsoryLvals = known(AllCompulsoryLvals)
    ;
        !.MaybeCompulsoryLvals = unknown_must_assume_all
    ).

%-----------------------------------------------------------------------------%

:- pred opt_access(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out, int::in, lvalset::in, int::in) is det.

opt_access([], [], !TempCounter, _, _, _).
opt_access([Instr0 | TailInstrs0], Instrs, !TempCounter, NumRealRRegs,
        AlreadyTried0, AccessThreshold) :-
    Instr0 = llds_instr(Uinstr0, _Comment0),
    ( if
        Uinstr0 = assign(ToLval, FromRval),
        SubLvals = lvals_in_lval(ToLval) ++ lvals_in_rval(FromRval),
        list.filter(
            base_lval_worth_replacing_not_tried(AlreadyTried0, NumRealRRegs),
            SubLvals, ReplaceableSubLvals),
        ReplaceableSubLvals = [ChosenLval | ChooseableRvals]
    then
        OrigTempCounter = !.TempCounter,
        counter.allocate(TempNum, !TempCounter),
        TempLval = temp(reg_type_for_lval(ChosenLval), TempNum),
        SubChosenLvals = lvals_in_lval(ChosenLval),
        expect(unify(SubChosenLvals, []), $pred,
            "nonempty SubChosenLvals"),
        substitute_lval_in_instr_until_defn(ChosenLval, TempLval,
            [Instr0 | TailInstrs0], Instrs1, 0, NumReplacements),
        set.insert(ChosenLval, AlreadyTried0, AlreadyTried1),
        ( if NumReplacements >= AccessThreshold then
            TempAssign = llds_instr(assign(TempLval, lval(ChosenLval)),
                "factor out common sub lval"),
            Instrs2 = [TempAssign | Instrs1],
            opt_access(Instrs2, Instrs, !TempCounter, NumRealRRegs,
                AlreadyTried1, AccessThreshold)
        else
            (
                ChooseableRvals = [_ | _],
                !:TempCounter = OrigTempCounter,
                opt_access([Instr0 | TailInstrs0], Instrs, !TempCounter,
                    NumRealRRegs, AlreadyTried1, AccessThreshold)
            ;
                ChooseableRvals = [],
                !:TempCounter = OrigTempCounter,
                opt_access(TailInstrs0, TailInstrs, !TempCounter, NumRealRRegs,
                    set.init, AccessThreshold),
                Instrs = [Instr0 | TailInstrs]
            )
        )
    else
        opt_access(TailInstrs0, TailInstrs, !TempCounter, NumRealRRegs,
            set.init, AccessThreshold),
        Instrs = [Instr0 | TailInstrs]
    ).

%-----------------------------------------------------------------------------%

:- pred base_lval_worth_replacing(int::in, lval::in) is semidet.

base_lval_worth_replacing(NumRealRRegs, Lval) :-
    (
        Lval = reg(reg_r, RegNum),
        RegNum > NumRealRRegs
    ;
        Lval = reg(reg_f, _)
    ;
        Lval = stackvar(_)
    ;
        Lval = framevar(_)
    ;
        Lval = double_stackvar(_, _)
    ).

:- pred base_lval_worth_replacing_not_tried(lvalset::in, int::in, lval::in)
    is semidet.

base_lval_worth_replacing_not_tried(AlreadyTried, NumRealRRegs, Lval) :-
    \+ set.member(Lval, AlreadyTried),
    base_lval_worth_replacing(NumRealRRegs, Lval).

:- func reg_type_for_lval(lval) = reg_type.

reg_type_for_lval(Lval) = RegType :-
    (
        Lval = reg(RegType, _)
    ;
        Lval = temp(RegType, _)
    ;
        Lval = double_stackvar(_, _),
        RegType = reg_f
    ;
        ( Lval = succip
        ; Lval = maxfr
        ; Lval = curfr
        ; Lval = hp
        ; Lval = sp
        ; Lval = parent_sp
        ; Lval = stackvar(_)
        ; Lval = parent_stackvar(_)
        ; Lval = framevar(_)
        ; Lval = succip_slot(_)
        ; Lval = succfr_slot(_)
        ; Lval = redoip_slot(_)
        ; Lval = redofr_slot(_)
        ; Lval = prevfr_slot(_)
        ; Lval = field(_, _, _)
        ; Lval = mem_ref(_)
        ; Lval = global_var_ref(_)
        ; Lval = lvar(_)
        ),
        RegType = reg_r
    ).

%-----------------------------------------------------------------------------%

    % When processing substituting e.g. tempr1 for e.g. r2 in the instruction
    % that defines r2, we must be careful to leave intact the value being
    % assigned. Given the instruction
    %
    %   r2 = field(0, r2, 5)
    %
    % we must generate
    %
    %   tempr1 = field(0, r2, 5)
    %
    % Generating
    %
    %   tempr1 = field(0, tempr1, 5)
    %
    % would introduce a bug, since the right hand side now refers to
    % an as yet undefined variable.
    %
:- pred substitute_lval_in_defn(lval::in, lval::in,
    instruction::in, instruction::out) is det.

substitute_lval_in_defn(OldLval, NewLval, Instr0, Instr) :-
    Instr0 = llds_instr(Uinstr0, Comment),
    ( if
        Uinstr0 = assign(ToLval, FromRval)
    then
        expect(unify(ToLval, OldLval), $pred, "mismatch in assign"),
        Uinstr = assign(NewLval, FromRval)
    else if
        Uinstr0 = incr_hp(ToLval, MaybeTag, SizeRval, MO, Type,
            MayUseAtomic, MaybeRegionRval, MaybeReuse)
    then
        expect(unify(ToLval, OldLval), $pred, "mismatch in incr_hp"),
        Uinstr = incr_hp(NewLval, MaybeTag, SizeRval, MO, Type,
            MayUseAtomic, MaybeRegionRval, MaybeReuse)
    else if
        Uinstr0 = foreign_proc_code(D, Comps0, MCM, FNL, FL, FOL, NF, MDL,
            S, MD)
    then
        substitute_lval_in_defn_components(OldLval, NewLval, Comps0, Comps,
            0, NumSubsts),
        expect(unify(NumSubsts, 1), $pred, "mismatch in foreign_proc_code"),
        Uinstr = foreign_proc_code(D, Comps, MCM, FNL, FL, FOL, NF, MDL, S, MD)
    else
        unexpected($pred, "unexpected instruction")
    ),
    Instr = llds_instr(Uinstr, Comment).

:- pred substitute_lval_in_defn_components(lval::in, lval::in,
    list(foreign_proc_component)::in, list(foreign_proc_component)::out,
    int::in, int::out) is det.

substitute_lval_in_defn_components(_OldLval, _NewLval, [], [], !NumSubsts).
substitute_lval_in_defn_components(OldLval, NewLval,
        [Comp0 | Comps0], [Comp | Comps], !NumSubsts) :-
    (
        Comp0 = foreign_proc_outputs(Outputs0),
        substitute_lval_in_defn_outputs(OldLval, NewLval,
            Outputs0, Outputs, !NumSubsts),
        Comp = foreign_proc_outputs(Outputs)
    ;
        ( Comp0 = foreign_proc_inputs(_)
        ; Comp0 = foreign_proc_user_code(_, _, _)
        ; Comp0 = foreign_proc_raw_code(_, _, _, _)
        ; Comp0 = foreign_proc_fail_to(_)
        ; Comp0 = foreign_proc_alloc_id(_)
        ; Comp0 = foreign_proc_noop
        ),
        Comp = Comp0
    ),
    substitute_lval_in_defn_components(OldLval, NewLval, Comps0, Comps,
        !NumSubsts).

:- pred substitute_lval_in_defn_outputs(lval::in, lval::in,
    list(foreign_proc_output)::in, list(foreign_proc_output)::out,
    int::in, int::out) is det.

substitute_lval_in_defn_outputs(_OldLval, _NewLval, [], [], !NumSubsts).
substitute_lval_in_defn_outputs(OldLval, NewLval,
        [Output0 | Outputs0], [Output | Outputs], !NumSubsts) :-
    Output0 = foreign_proc_output(Dest0, Type, IsDummy, VarName,
        OrigType, MaybeForeignType, BoxPolicy),
    ( if Dest0 = OldLval then
        Output = foreign_proc_output(NewLval, Type, IsDummy, VarName,
            OrigType, MaybeForeignType, BoxPolicy),
        !:NumSubsts = !.NumSubsts + 1
    else
        Output = Output0
    ),
    substitute_lval_in_defn_outputs(OldLval, NewLval, Outputs0, Outputs,
        !NumSubsts).

    % Substitute NewLval for OldLval in an instruction sequence
    % until we come an instruction that may define OldLval.
    % We do not worry about instructions that define a variable that
    % occurs in the access path to OldLval (and which therefore indirectly
    % modifies the value that OldLval refers to), because our caller will
    % call us only with OldLvals (and NewLvals for that matter) that have
    % no lvals in their access path. The NewLvals will be temporaries,
    % representing local variables in C blocks.
    %
    % When control leaves this instruction sequence via a if_val, goto or
    % call, the local variables of the block in which this instruction
    % sequence will go out of scope, so we must stop using them. At points
    % at which control can enter this instruction sequence, i.e. at labels,
    % the C block ends, so again we must stop using its local variables.
    % (Livevals pseudo-instructions occur only immediately before
    % instructions that cause control transfer, so we stop at them too.)
    %
    % Our caller ensures that we can also so stop at any point. By doing so
    % we may fail to exploit an optimization opportunity, but the code we
    % generate will still be correct. At the moment we stop at instructions
    % whose correct handling would be non-trivial and which rarely if ever
    % appear between the definition and a use of a location we want to
    % substitute. These include instructions that manipulate stack frames,
    % the heap, the trail and synchronization data.
    %
:- pred substitute_lval_in_instr_until_defn(lval::in, lval::in,
    list(instruction)::in, list(instruction)::out, int::in, int::out)
    is det.

substitute_lval_in_instr_until_defn(_, _, [], [], !N).
substitute_lval_in_instr_until_defn(OldLval, NewLval,
        [Instr0 | Instrs0], [Instr | Instrs], !N) :-
    substitute_lval_in_instr_until_defn_2(OldLval, NewLval,
        Instr0, Instr, Instrs0, Instrs, !N).

:- pred substitute_lval_in_instr_until_defn_2(lval::in, lval::in,
    instruction::in, instruction::out,
    list(instruction)::in, list(instruction)::out, int::in, int::out) is det.
:- pragma inline(substitute_lval_in_instr_until_defn_2/8).

substitute_lval_in_instr_until_defn_2(OldLval, NewLval, !Instr, !Instrs, !N) :-
    !.Instr = llds_instr(Uinstr0, Comment),
    (
        Uinstr0 = block(_, _, _),
        unexpected($pred, "block")
    ;
        Uinstr0 = assign(Lval, Rval0),
        Updates = assignment_updates_oldlval(Lval, OldLval),
        (
            Updates = yes,
            % If we alter any lval that occurs in OldLval, we must stop
            % the substitutions.
            exprn_aux.substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval),
            Uinstr = assign(Lval, Rval),
            !:Instr = llds_instr(Uinstr, Comment)
        ;
            Updates = no,
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        Uinstr0 = keep_assign(Lval, Rval0),
        Updates = assignment_updates_oldlval(Lval, OldLval),
        (
            Updates = yes,
            % If we alter any lval that occurs in OldLval, we must stop
            % the substitutions.
            exprn_aux.substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval),
            Uinstr = keep_assign(Lval, Rval),
            !:Instr = llds_instr(Uinstr, Comment)
        ;
            Updates = no,
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        Uinstr0 = lc_create_loop_control(_NumSlots, Lval),
        Updates = assignment_updates_oldlval(Lval, OldLval),
        (
            Updates = yes
            % If we alter any lval that occurs in OldLval, we must stop
            % the substitutions.
        ;
            Updates = no,
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        Uinstr0 = lc_wait_free_slot(Rval0, Lval, Label),
        Updates = assignment_updates_oldlval(Lval, OldLval),
        (
            Updates = yes,
            % If we alter any lval that occurs in OldLval, we must stop
            % the substitutions.
            exprn_aux.substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval),
            Uinstr = lc_wait_free_slot(Rval, Lval, Label),
            !:Instr = llds_instr(Uinstr, Comment)
        ;
            Updates = no,
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        ( Uinstr0 = incr_hp(Lval, _, _, _, _, _, _, _)
        ; Uinstr0 = save_maxfr(Lval)
        ; Uinstr0 = mark_hp(Lval)
        ),
        Updates = assignment_updates_oldlval(Lval, OldLval),
        (
            Updates = yes
            % If we alter any lval that occurs in OldLval, we must stop
            % the substitutions.
        ;
            Updates = no,
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        Uinstr0 = region_fill_frame(_, _, _, NumLval, AddrLval),
        ( if
            ( assignment_updates_oldlval(NumLval, OldLval) = yes
            ; assignment_updates_oldlval(AddrLval, OldLval) = yes
            )
        then
            % If we alter any lval that occurs in NumLval or AddrLval,
            % we must stop the substitutions.
            true
        else
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        ( Uinstr0 = restore_maxfr(_)
        ; Uinstr0 = restore_hp(_)
        ; Uinstr0 = push_region_frame(_, _)
        ; Uinstr0 = region_set_fixed_slot(_, _, _)
        ; Uinstr0 = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr0 = lc_spawn_off(_, _, _)
        ; Uinstr0 = lc_join_and_terminate(_, _)
        ),
        exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
        substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
    ;
        Uinstr0 = foreign_proc_code(_, Components, _, _, _, _, _, _, _, _),
        AffectsLiveness = components_affect_liveness(Components),
        (
            AffectsLiveness = no,
            Updates = components_update_oldlval(Components, OldLval),
            (
                Updates = yes
                % If we alter any lval that occurs in OldLval, we must stop
                % the substitutions.
            ;
                Updates = no,
                exprn_aux.substitute_lval_in_instr(OldLval, NewLval,
                    !Instr, !N),
                substitute_lval_in_instr_until_defn(OldLval, NewLval,
                    !Instrs, !N)
            )
        ;
            AffectsLiveness = yes
        )
    ;
        Uinstr0 = comment(_),
        substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
    ;
        Uinstr0 = if_val(_, _),
        exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
        substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
    ;
        Uinstr0 = computed_goto(_, _),
        exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N)
    ;
        ( Uinstr0 = label(_)
        ; Uinstr0 = livevals(_)
        ; Uinstr0 = llcall(_, _, _, _, _, _)
        ; Uinstr0 = mkframe(_, _)
        ; Uinstr0 = goto(_)
        ; Uinstr0 = free_heap(_)
        ; Uinstr0 = store_ticket(_)
        ; Uinstr0 = reset_ticket(_, _)
        ; Uinstr0 = discard_ticket
        ; Uinstr0 = prune_ticket
        ; Uinstr0 = mark_ticket_stack(_)
        ; Uinstr0 = prune_tickets_to(_)
        ; Uinstr0 = incr_sp(_, _, _)
        ; Uinstr0 = decr_sp(_)
        ; Uinstr0 = decr_sp_and_return(_)
        ; Uinstr0 = init_sync_term(_, _, _)
        ; Uinstr0 = fork_new_child(_, _)
        ; Uinstr0 = join_and_continue(_, _)
        ; Uinstr0 = arbitrary_c_code(_, _, _)
        )
    ).

:- func assignment_updates_oldlval(lval, lval) = bool.

assignment_updates_oldlval(Lval, OldLval) =
    ( if Lval = OldLval then
        % If we alter any lval that occurs in OldLval, we must stop the
        % substitutions. At the moment, the only lval OldLval can contain
        % is itself.
        yes
    else
        no
    ).

:- func components_update_oldlval(list(foreign_proc_component), lval) = bool.

components_update_oldlval([], _Lval) = no.
components_update_oldlval([Component | Components], Lval) = Updates :-
    ComponentUpdates = component_updates_oldlval(Component, Lval),
    (
        ComponentUpdates = yes,
        Updates = yes
    ;
        ComponentUpdates = no,
        Updates = components_update_oldlval(Components, Lval)
    ).

:- func component_updates_oldlval(foreign_proc_component, lval) = bool.

component_updates_oldlval(Component, Lval) = Updates :-
    (
        Component = foreign_proc_outputs(Outputs),
        ( if
            some [Output] (
                list.member(Output, Outputs),
                Output ^ out_arg_dest = Lval
            )
        then
            Updates = yes
        else
            Updates = no
        )
    ;
        ( Component = foreign_proc_inputs(_)
        ; Component = foreign_proc_fail_to(_)
        ; Component = foreign_proc_alloc_id(_)
        ; Component = foreign_proc_noop
        ; Component = foreign_proc_user_code(_, _, _)
        ; Component = foreign_proc_raw_code(_, _, _, _)
        ),
        Updates = no
    ).

:- func components_affect_liveness(list(foreign_proc_component))
    = bool.

components_affect_liveness([]) = no.
components_affect_liveness([Component | Components]) = Updates :-
    ComponentUpdates = component_affects_liveness(Component),
    (
        ComponentUpdates = yes,
        Updates = yes
    ;
        ComponentUpdates = no,
        Updates = components_affect_liveness(Components)
    ).

:- func component_affects_liveness(foreign_proc_component) = bool.

component_affects_liveness(Component) = Affects :-
    (
        ( Component = foreign_proc_inputs(_)
        ; Component = foreign_proc_outputs(_)
        ; Component = foreign_proc_fail_to(_)
        ; Component = foreign_proc_alloc_id(_)
        ; Component = foreign_proc_noop
        ),
        Affects = no
    ;
        ( Component = foreign_proc_user_code(_, AffectsLiveness, Code)
        ; Component = foreign_proc_raw_code(_, AffectsLiveness, _, Code)
        ),
        (
            AffectsLiveness = proc_affects_liveness,
            Affects = yes
        ;
            AffectsLiveness = proc_does_not_affect_liveness,
            Affects = no
        ;
            AffectsLiveness = proc_default_affects_liveness,
            ( if Code = "" then
                Affects = no
            else
                Affects = yes
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- func local_vars_proc_size_limit = int.

local_vars_proc_size_limit = 10000.

:- func local_vars_block_size_limit = int.

local_vars_block_size_limit = 200.

%-----------------------------------------------------------------------------%
:- end_module ll_backend.use_local_vars.
%-----------------------------------------------------------------------------%
