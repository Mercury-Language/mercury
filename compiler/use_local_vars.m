%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007 The University of Melbourne.
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
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module counter.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred use_local_vars_proc(list(instruction)::in, list(instruction)::out,
    int::in, int::in, bool::in, proc_label::in, counter::in, counter::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module ll_backend.basic_block.
:- import_module ll_backend.code_util.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.livemap.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.opt_util.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

use_local_vars_proc(Instrs0, Instrs, NumRealRRegs, AccessThreshold,
        AutoComments, ProcLabel, !C) :-
    create_basic_blocks(Instrs0, Comments0, ProcLabel, !C, NewLabels,
        LabelSeq, BlockMap0),
    flatten_basic_blocks(LabelSeq, BlockMap0, TentativeInstrs),
    build_livemap(TentativeInstrs, MaybeLiveMap),
    extend_basic_blocks(LabelSeq, EBBLabelSeq, BlockMap0, EBBBlockMap0,
        NewLabels),
    list.foldl(use_local_vars_block(MaybeLiveMap, NumRealRRegs,
        AccessThreshold), EBBLabelSeq, EBBBlockMap0, EBBBlockMap),
    flatten_basic_blocks(EBBLabelSeq, EBBBlockMap, Instrs1),

    (
        MaybeLiveMap = yes(LiveMap),
        AutoComments = yes
    ->
        NewComment = "\n" ++ dump_livemap(yes(ProcLabel), LiveMap),
        NewCommentInstr = llds_instr(comment(NewComment), ""),
        Comments = Comments0 ++ [NewCommentInstr]
    ;
        Comments = Comments0
    ),
    Instrs = Comments ++ Instrs1.

:- pred use_local_vars_block(maybe(livemap)::in, int::in, int::in, label::in,
    block_map::in, block_map::out) is det.

use_local_vars_block(MaybeLiveMap, NumRealRRegs, AccessThreshold, Label,
        !BlockMap) :-
    map.lookup(!.BlockMap, Label, BlockInfo0),
    BlockInfo0 = block_info(BlockLabel, LabelInstr, RestInstrs0,
        FallInto, JumpLabels, MaybeFallThrough),
    counter.init(1, TempCounter0),
    use_local_vars_instrs(RestInstrs0, RestInstrs, TempCounter0, TempCounter,
        NumRealRRegs, AccessThreshold, MaybeLiveMap, MaybeFallThrough),
    ( TempCounter = TempCounter0 ->
        true
    ;
        BlockInfo = block_info(BlockLabel, LabelInstr, RestInstrs, FallInto,
            JumpLabels, MaybeFallThrough),
        map.det_update(!.BlockMap, Label, BlockInfo, !:BlockMap)
    ).

%-----------------------------------------------------------------------------%

:- pred use_local_vars_instrs(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out, int::in, int::in, maybe(livemap)::in,
    maybe(label)::in) is det.

use_local_vars_instrs(!RestInstrs, !TempCounter,
        NumRealRRegs, AccessThreshold, MaybeLiveMap, MaybeFallThrough) :-
    opt_assign(!RestInstrs, !TempCounter, NumRealRRegs, MaybeLiveMap,
        MaybeFallThrough),
    ( AccessThreshold >= 1 ->
        opt_access(!RestInstrs, !TempCounter, NumRealRRegs,
            set.init, AccessThreshold)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred opt_assign(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out, int::in, maybe(livemap)::in, maybe(label)::in)
    is det.

opt_assign([], [], !TempCounter, _, _, _).
opt_assign([Instr0 | TailInstrs0], Instrs, !TempCounter, NumRealRRegs,
        MaybeLiveMap, MaybeFallThrough) :-
    Instr0 = llds_instr(Uinstr0, _Comment0),
    (
        ( Uinstr0 = assign(ToLval, _FromRval)
        ; Uinstr0 = incr_hp(ToLval, _MaybeTag, _SizeRval, _MO, _Type, _Atomic)
        ),
        base_lval_worth_replacing(NumRealRRegs, ToLval)
    ->
        (
            ToLval = reg(_, _),
            find_compulsory_lvals(TailInstrs0, MaybeLiveMap, MaybeFallThrough,
                no, MaybeCompulsoryLvals),
            MaybeCompulsoryLvals = known(CompulsoryLvals),
            not set.member(ToLval, CompulsoryLvals)
        ->
            counter.allocate(TempNum, !TempCounter),
            NewLval = temp(reg_r, TempNum),
            substitute_lval_in_defn(ToLval, NewLval, Instr0, Instr),
            list.map_foldl(
                exprn_aux.substitute_lval_in_instr(ToLval, NewLval),
                TailInstrs0, TailInstrs1, 0, _),
            opt_assign(TailInstrs1, TailInstrs, !TempCounter, NumRealRRegs,
                MaybeLiveMap, MaybeFallThrough),
            Instrs = [Instr | TailInstrs]
        ;
            counter.allocate(TempNum, !TempCounter),
            NewLval = temp(reg_r, TempNum),
            substitute_lval_in_instr_until_defn(ToLval, NewLval,
                TailInstrs0, TailInstrs1, 0, NumSubst),
            NumSubst > 1
        ->
            substitute_lval_in_defn(ToLval, NewLval, Instr0, Instr),
            CopyInstr = llds_instr(assign(ToLval, lval(NewLval)), ""),
            opt_assign(TailInstrs1, TailInstrs, !TempCounter, NumRealRRegs,
                MaybeLiveMap, MaybeFallThrough),
            Instrs = [Instr, CopyInstr | TailInstrs]
        ;
            opt_assign(TailInstrs0, TailInstrs, !TempCounter, NumRealRRegs,
                MaybeLiveMap, MaybeFallThrough),
            Instrs = [Instr0 | TailInstrs]
        )
    ;
        opt_assign(TailInstrs0, TailInstrs, !TempCounter, NumRealRRegs,
            MaybeLiveMap, MaybeFallThrough),
        Instrs = [Instr0 | TailInstrs]
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
    (
        Uinstr = livevals(LiveLvals)
    ->
        find_compulsory_lvals(Instrs, MaybeLiveMap, MaybeFallThrough,
            yes, !:MaybeCompulsoryLvals),
        union_maybe_compulsory_lvals(LiveLvals, !MaybeCompulsoryLvals)
    ;
        Uinstr = llcall(_, _, _, _, _, _)
    ->
        expect(unify(PrevLivevals, yes),
            this_file, "find_compulsory_lvals: call without livevals"),
        % The livevals instruction will include all the live lvals
        % in MaybeCompulsoryLvals after we return.
        !:MaybeCompulsoryLvals = known(set.init)
    ;
        Uinstr = goto(_Target),
        PrevLivevals = yes
    ->
        % The livevals instruction will include all the live lvals
        % in MaybeCompulsoryLvals after we return.
        !:MaybeCompulsoryLvals = known(set.init)
    ;
        possible_targets(Uinstr, Labels, NonLabelCodeAddrs),
        (
            NonLabelCodeAddrs = [],
            (
                Labels = [],
                % Optimize the common case
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
    (
        Uinstr0 = assign(ToLval, FromRval),
        lvals_in_lval(ToLval, ToSubLvals),
        lvals_in_rval(FromRval, FromSubLvals),
        list.append(ToSubLvals, FromSubLvals, SubLvals),
        list.filter(
            base_lval_worth_replacing_not_tried(AlreadyTried0, NumRealRRegs),
            SubLvals, ReplaceableSubLvals),
        ReplaceableSubLvals = [ChosenLval | ChooseableRvals]
    ->
        OrigTempCounter = !.TempCounter,
        counter.allocate(TempNum, !TempCounter),
        TempLval = temp(reg_r, TempNum),
        lvals_in_lval(ChosenLval, SubChosenLvals),
        expect(unify(SubChosenLvals, []),
            this_file, "opt_access: nonempty SubChosenLvals"),
        substitute_lval_in_instr_until_defn(ChosenLval, TempLval,
            [Instr0 | TailInstrs0], Instrs1, 0, NumReplacements),
        set.insert(AlreadyTried0, ChosenLval, AlreadyTried1),
        ( NumReplacements >= AccessThreshold ->
            TempAssign = llds_instr(assign(TempLval, lval(ChosenLval)),
                "factor out common sub lval"),
            Instrs2 = [TempAssign | Instrs1],
            opt_access(Instrs2, Instrs, !TempCounter, NumRealRRegs,
                AlreadyTried1, AccessThreshold)
        ; ChooseableRvals = [_ | _] ->
            !:TempCounter = OrigTempCounter,
            opt_access([Instr0 | TailInstrs0], Instrs, !TempCounter,
                NumRealRRegs, AlreadyTried1, AccessThreshold)
        ;
            !:TempCounter = OrigTempCounter,
            opt_access(TailInstrs0, TailInstrs, !TempCounter, NumRealRRegs,
                set.init, AccessThreshold),
            Instrs = [Instr0 | TailInstrs]
        )
    ;
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
        Lval = stackvar(_)
    ;
        Lval = framevar(_)
    ).

:- pred base_lval_worth_replacing_not_tried(lvalset::in, int::in, lval::in)
    is semidet.

base_lval_worth_replacing_not_tried(AlreadyTried, NumRealRRegs, Lval) :-
    \+ set.member(Lval, AlreadyTried),
    base_lval_worth_replacing(NumRealRRegs, Lval).

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
    ( Uinstr0 = assign(ToLval, FromRval) ->
        expect(unify(ToLval, OldLval),
            this_file, "substitute_lval_in_defn: mismatch in assign"),
        Uinstr = assign(NewLval, FromRval)
    ; Uinstr0 = incr_hp(ToLval, MaybeTag, SizeRval, MO, Type, MayUseAtomic) ->
        expect(unify(ToLval, OldLval),
            this_file, "substitute_lval_in_defn: mismatch in incr_hp"),
        Uinstr = incr_hp(NewLval, MaybeTag, SizeRval, MO, Type, MayUseAtomic)
    ;
        unexpected(this_file,
            "substitute_lval_in_defn: unexpected instruction")
    ),
    Instr = llds_instr(Uinstr, Comment).

    % Substitute NewLval for OldLval in an instruction sequence
    % until we come an instruction that may define OldLval.
    % We don't worry about instructions that define a variable that
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

substitute_lval_in_instr_until_defn_2(OldLval, NewLval, !Instr, !Instrs, !N) :-
    !.Instr = llds_instr(Uinstr0, _),
    (
        Uinstr0 = comment(_),
        substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
    ;
        Uinstr0 = livevals(_)
    ;
        Uinstr0 = block(_, _, _),
        unexpected(this_file,
            "substitute_lval_in_instr_until_defn: found block")
    ;
        Uinstr0 = assign(Lval, _),
        ( Lval = OldLval ->
            % If we alter any lval that occurs in OldLval, we must stop
            % the substitutions. At the moment, the only lval OldLval
            % contains is itself.
            true
        ;
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        Uinstr0 = llcall(_, _, _, _, _, _)
    ;
        Uinstr0 = mkframe(_, _)
    ;
        Uinstr0 = label(_)
    ;
        Uinstr0 = goto(_)
    ;
        Uinstr0 = computed_goto(_, _),
        exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N)
    ;
        Uinstr0 = if_val(_, _),
        exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N)
    ;
        Uinstr0 = save_maxfr(_)
    ;
        Uinstr0 = restore_maxfr(_)
    ;
        Uinstr0 = incr_hp(Lval, _, _, _, _, _),
        ( Lval = OldLval ->
            % If we alter any lval that occurs in OldLval, we must stop
            % the substitutions. At the moment, the only lval OldLval
            % contains is itself.
            true
        ;
            exprn_aux.substitute_lval_in_instr(OldLval, NewLval, !Instr, !N),
            substitute_lval_in_instr_until_defn(OldLval, NewLval, !Instrs, !N)
        )
    ;
        Uinstr0 = mark_hp(_)
    ;
        Uinstr0 = restore_hp(_)
    ;
        Uinstr0 = free_heap(_)
    ;
        Uinstr0 = store_ticket(_)
    ;
        Uinstr0 = reset_ticket(_, _)
    ;
        Uinstr0 = discard_ticket
    ;
        Uinstr0 = prune_ticket
    ;
        Uinstr0 = mark_ticket_stack(_)
    ;
        Uinstr0 = prune_tickets_to(_)
    ;
        Uinstr0 = incr_sp(_, _, _)
    ;
        Uinstr0 = decr_sp(_)
    ;
        Uinstr0 = decr_sp_and_return(_)
    ;
        Uinstr0 = init_sync_term(_, _)
    ;
        Uinstr0 = fork(_)
    ;
        Uinstr0 = join_and_continue(_, _)
    ;
        Uinstr0 = arbitrary_c_code(_, _, _)
    ;
        Uinstr0 = foreign_proc_code(_, _, _, _, _, _, _, _, _)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "use_local_vars.m".

%-----------------------------------------------------------------------------%
:- end_module use_local_vars.
%-----------------------------------------------------------------------------%
