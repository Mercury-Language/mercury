%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2002-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: peeophole.m:
% Authors: fjh, zs.
%
% Local LLDS to LLDS optimizations based on pattern-matching.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.peephole.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module ll_backend.llds.

:- import_module bool.
:- import_module list.

    % Peephole optimize a list of instructions.
    %
:- pred peephole_optimize(gc_method::in, maybe_opt_peep_mkword::in,
    list(instruction)::in, list(instruction)::out, bool::out) is det.

:- pred combine_decr_sp(list(instruction)::in, list(instruction)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module ll_backend.opt_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module uint8.

%-----------------------------------------------------------------------------%

    % Patterns that can be switched off.
    %
:- type pattern
    --->    pattern_incr_sp
    ;       pattern_mkword.

    % We zip down to the end of the instruction list, and start attempting
    % to optimize instruction sequences. As long as we can continue
    % optimizing the instruction sequence, we keep doing so;
    % when we find a sequence we can't optimize, we back up and try
    % to optimize the sequence starting with the previous instruction.
    %
peephole_optimize(GC_Method, OptPeepMkword, Instrs0, Instrs, Mod) :-
    invalid_peephole_opts(GC_Method, OptPeepMkword, InvalidPatterns),
    peephole_optimize_2(InvalidPatterns, Instrs0, Instrs, Mod).

:- pred peephole_optimize_2(list(pattern)::in, list(instruction)::in,
    list(instruction)::out, bool::out) is det.

peephole_optimize_2(_, [], [], no).
peephole_optimize_2(InvalidPatterns, [Instr0 | Instrs0], Instrs, Mod) :-
    peephole_optimize_2(InvalidPatterns, Instrs0, Instrs1, Mod0),
    peephole_opt_instr(Instr0, Instrs1, InvalidPatterns, Instrs, Mod1),
    ( if Mod0 = no, Mod1 = no then
        Mod = no
    else
        Mod = yes
    ).

    % Try to optimize the beginning of the given instruction sequence.
    % If successful, try it again.
    %
:- pred peephole_opt_instr(instruction::in, list(instruction)::in,
    list(pattern)::in, list(instruction)::out, bool::out) is det.

peephole_opt_instr(Instr0, Instrs0, InvalidPatterns, Instrs, Mod) :-
    opt_util.skip_comments(Instrs0, Instrs1),
    ( if
        peephole_match(Instr0, Instrs1, InvalidPatterns, Instrs2)
    then
        (
            Instrs2 = [Instr2 | Instrs3],
            peephole_opt_instr(Instr2, Instrs3, InvalidPatterns, Instrs, _)
        ;
            Instrs2 = [],
            Instrs = Instrs2
        ),
        Mod = yes
    else if
        peephole_match_norepeat(Instr0, Instrs1, InvalidPatterns, Instrs2)
    then
        Instrs = Instrs2,
        Mod = yes
    else
        Instrs = [Instr0 | Instrs0],
        Mod = no
    ).

%-----------------------------------------------------------------------------%

    % Build a map that associates each label in a computed goto with the
    % values of the switch rval that cause a jump to it.
    %
:- pred build_peephole_jump_label_map(list(maybe(label))::in, int::in,
    map(label, list(int))::in, map(label, list(int))::out) is semidet.

build_peephole_jump_label_map([], _, !LabelMap).
build_peephole_jump_label_map([MaybeLabel | MaybeLabels], Val, !LabelMap) :-
    MaybeLabel = yes(Label),
    ( if map.search(!.LabelMap, Label, Vals0) then
        map.det_update(Label, [Val | Vals0], !LabelMap)
    else
        map.det_insert(Label, [Val], !LabelMap)
    ),
    build_peephole_jump_label_map(MaybeLabels, Val + 1, !LabelMap).

    % If one of the two labels has only one associated value, return it and
    % the associated value as the first two output arguments, and the
    % remaining label as the last output argument.
    %
:- pred peephole_pick_one_val_label(pair(label, list(int))::in,
    pair(label, list(int))::in, label::out, int::out, label::out) is semidet.

peephole_pick_one_val_label(LabelVals1, LabelVals2, OneValLabel, Val,
        OtherLabel) :-
    LabelVals1 = Label1 - Vals1,
    LabelVals2 = Label2 - Vals2,
    ( if Vals1 = [Val1] then
        OneValLabel = Label1,
        Val = Val1,
        OtherLabel = Label2
    else if Vals2 = [Val2] then
        OneValLabel = Label2,
        Val = Val2,
        OtherLabel = Label1
    else
        fail
    ).

%-----------------------------------------------------------------------------%

    % Look for code patterns that can be optimized, and optimize them.
    % Unlike peephole_match_norepeat, this predicate guarantees that the
    % instruction sequence it returns on success won't be transformable
    % by the same transformation as it applies. This allows peephole_opt_instr
    % to call peephole_match repeatedly until it fails without the possibility
    % of an infinite loop.
    %
:- pred peephole_match(instruction::in, list(instruction)::in,
    list(pattern)::in, list(instruction)::out) is semidet.

    % A `computed_goto' with all branches pointing to the same label
    % can be replaced with an unconditional goto.
    %
    % A `computed_goto' with all branches but one pointing to the same label
    % can be replaced with a conditional branch followed by an unconditional
    % goto.
    %
peephole_match(Instr0, Instrs0, _, Instrs) :-
    Instr0 = llds_instr(Uinstr0, Comment0),
    Uinstr0 = computed_goto(SelectorRval, Labels),
    build_peephole_jump_label_map(Labels, 0, map.init, LabelMap),
    map.to_assoc_list(LabelMap, LabelValsList),
    ( if
        LabelValsList = [Label - _]
    then
        GotoInstr = llds_instr(goto(code_label(Label)), Comment0),
        Instrs = [GotoInstr | Instrs0]
    else if
        LabelValsList = [LabelVals1, LabelVals2],
        peephole_pick_one_val_label(LabelVals1, LabelVals2, OneValLabel,
            Val, OtherLabel)
    then
        CondRval = binop(eq(int_type_int), SelectorRval,
            const(llconst_int(Val))),
        CommentInstr = llds_instr(comment(Comment0), ""),
        BranchInstr = llds_instr(if_val(CondRval, code_label(OneValLabel)),
            ""),
        GotoInstr = llds_instr(goto(code_label(OtherLabel)), Comment0),
        Instrs = [CommentInstr, BranchInstr, GotoInstr | Instrs0]
    else
        fail
    ).

    % A conditional branch whose condition is constant
    % can be either eliminated or replaced by an unconditional goto.
    %
    % A conditional branch to an address followed by an unconditional
    % branch to the same address can be eliminated.
    %
    % A conditional branch to a label followed by that label
    % can be eliminated.
    %
peephole_match(Instr0, Instrs0, _, Instrs) :-
    Instr0 = llds_instr(Uinstr0, Comment0),
    Uinstr0 = if_val(Rval, CodeAddr),
    ( if
        opt_util.is_const_condition(Rval, Taken)
    then
        (
            Taken = yes,
            Instrs = [llds_instr(goto(CodeAddr), Comment0) | Instrs0]
        ;
            Taken = no,
            Instrs = Instrs0
        )
    else if
        opt_util.skip_comments(Instrs0, Instrs1),
        Instrs1 = [Instr1 | _],
        Instr1 = llds_instr(goto(CodeAddr), _)
    then
        Instrs = Instrs0
    else if
        CodeAddr = code_label(Label),
        opt_util.is_this_label_next(Label, Instrs0, _)
    then
        Instrs = Instrs0
    else
        fail
    ).

    % If a `mkframe' is followed by an assignment to its redoip slot,
    % with the instructions in between containing only straight-line code,
    % and no labels, we can delete the assignment and instead just set
    % the redoip directly in the `mkframe'. (If the code between the
    % mkframe and the redoip contains a label, then we need to keep the
    % redoip assign, in case we reach it via the label and not from the
    % mkframe above.
    %
    %   mkframe(NFI, _)             =>  mkframe(NFI, Redoip)
    %   <straightline instrs>           <straightline instrs>
    %   assign(redoip(lval(_)), Redoip)
    %
    % If a `mkframe' is followed by a test that can fail, we try to
    % swap the two instructions to avoid doing the mkframe unnecessarily.
    %
    %   mkframe(NFI, dofail)        =>  if_val(test, redo)
    %   if_val(test, redo/fail)         mkframe(NFI, dofail)
    %
    %   mkframe(NFI, label)         =>  if_val(test, redo)
    %   if_val(test, fail)              mkframe(NFI, label)
    %
    %   mkframe(NFI, label)         =>  mkframe(NFI, label)
    %   if_val(test, redo)              if_val(test, label)
    %
    % If a `mkframe' is followed directly by a `fail', we optimize away
    % the creation of the stack frame:
    %
    %   mkframe(NFI, <any>)         =>  goto redo
    %   goto fail
    %
    % This last pattern can be created by frameopt.m.
    %
    % These three classes of patterns are mutually exclusive because if_val
    % and goto are not straight-line code.
    %
    % The fourth pattern we look for can happen when predicates that are
    % actually semidet are declared to be nondet:
    %
    %   mkframe(NFI, dofail)
    %   <straight,nostack instrs>   =>  <straight,nostack instrs>
    %   succeed                         proceed
    %
    % The fifth pattern can happen e.g. for lookup switches:
    %
    %   mkframe(NFI, dofail)            mkframe(NFI, dofail)
    %   <nostack instrs, no labels> =>  <nostack instrs, no labels>
    %   succeed                         succeed_discard
    %
    % The fifth pattern may apply even if the fourth doesn't, since it permits
    % branches away between the mkframe and the succeed. However, if there are
    % none, and both patterns apply, we prefer to apply the fourth pattern's
    % transformation.
    %
peephole_match(Instr0, Instrs0, _, Instrs) :-
    Instr0 = llds_instr(Uinstr0, Comment0),
    Uinstr0 = mkframe(NondetFrameInfo, yes(Redoip0)),
    ( if
        % A mkframe sets curfr to point to the new frame
        % only for ordinary frames, not temp frames.
        (
            NondetFrameInfo = ordinary_frame(_, _),
            AllowedBases = [maxfr, curfr]
        ;
            NondetFrameInfo = temp_frame(_),
            AllowedBases = [maxfr]
        ),
        opt_util.next_assign_to_redoip(Instrs0, AllowedBases, [], Redoip1,
            Skipped, Rest),
        opt_util.touches_nondet_ctrl(Skipped) = no
    then
        Instrs1 = Skipped ++ Rest,
        NewInstr = llds_instr(mkframe(NondetFrameInfo, yes(Redoip1)),
            Comment0),
        Instrs = [NewInstr | Instrs1]
    else if
        opt_util.skip_comments_livevals(Instrs0, Instrs1),
        Instrs1 = [Instr1 | Instrs2],
        Instr1 = llds_instr(if_val(Test, Target), Comment1),
        ( if
            Redoip0 = do_fail,
            ( Target = do_redo ; Target = do_fail)
        then
            InstrsPrime = [
                llds_instr(if_val(Test, do_redo), Comment1),
                llds_instr(mkframe(NondetFrameInfo, yes(do_fail)), Comment0)
                | Instrs2
            ]
        else if
            Redoip0 = code_label(_)
        then
            ( if
                Target = do_fail
            then
                InstrsPrime = [
                    llds_instr(if_val(Test, do_redo), Comment1),
                    Instr0
                    | Instrs2
                ]
            else if
                Target = do_redo
            then
                InstrsPrime = [
                    Instr0,
                    llds_instr(if_val(Test, Redoip0), Comment1)
                    | Instrs2
                ]
            else
                fail
            )
        else
            fail
        )
    then
        Instrs = InstrsPrime
    else if
        opt_util.skip_comments_livevals(Instrs0, Instrs1),
        Instrs1 = [Instr1 | Instrs2],
        Instr1 = llds_instr(goto(do_fail), Comment2)
    then
        Instrs = [llds_instr(goto(do_redo), Comment2) | Instrs2]
    else if
        Redoip0 = do_fail,
        no_stack_straight_line(Instrs0, Straight, Instrs1),
        Instrs1 = [Instr1 | Instrs2],
        Instr1 = llds_instr(goto(do_succeed(_)), _)
    then
        GotoSuccip = llds_instr(goto(code_succip),
            "return from optimized away mkframe"),
        Instrs = Straight ++ [GotoSuccip | Instrs2]
    else if
        Redoip0 = do_fail,
        may_replace_succeed_with_succeed_discard(Instrs0, UntilSucceed,
            SucceedComment, Instrs2)
    then
        DiscardUinstr = goto(do_succeed(no)),
        DiscardComment = SucceedComment ++ " (added discard)",
        DiscardInstr = llds_instr(DiscardUinstr, DiscardComment),
        Instrs = [Instr0 | UntilSucceed] ++ [DiscardInstr | Instrs2]
    else
        fail
    ).

    % If a `store_ticket' is followed by a `reset_ticket',
    % we can delete the `reset_ticket'.
    %
    %   store_ticket(Lval)          =>  store_ticket(Lval)
    %   reset_ticket(Lval, _R)
    %
peephole_match(Instr0, Instrs0, _, Instrs) :-
    Instr0 = llds_instr(Uinstr0, Comment0),
    Uinstr0 = store_ticket(Lval),

    opt_util.skip_comments(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    Instr1 = llds_instr(reset_ticket(lval(Lval), _Reason), _Comment1),
    NewInstr2 = llds_instr(store_ticket(Lval), Comment0),
    Instrs = [NewInstr2 | Instrs2].

    % If an assignment to a redoip slot is followed by another, with
    % the instructions in between containing only straight-line code
    % without labels, we can delete one of the asignments:
    %
    %   assign(redoip(Fr), Redoip1) =>  assign(redoip(Fr), Redoip2)
    %   <straightline instrs>           <straightline instrs>
    %   assign(redoip(Fr), Redoip2)
    %
    % If an assignment of do_fail to the redoip slot of the current frame
    % is followed by straight-line instructions except possibly for if_val
    % with do_fail or do_redo as target, until a goto to do_succeed(no),
    % and if the nondet stack linkages are not touched by the
    % straight-line instructions, then we can discard the nondet stack
    % frame early.
    %
peephole_match(Instr0, Instrs0, _, Instrs) :-
    Instr0 = llds_instr(Uinstr0, Comment0),
    Uinstr0 = assign(redoip_slot(lval(Base)), Redoip0),
    ( if
        opt_util.next_assign_to_redoip(Instrs0, [Base], [], Redoip1,
            Skipped, Rest),
        opt_util.touches_nondet_ctrl(Skipped) = no
    then
        Instrs1 = Skipped ++ Rest,
        RedoipInstr = llds_instr(assign(redoip_slot(lval(Base)),
            const(llconst_code_addr(Redoip1))), Comment0),
        Instrs = [RedoipInstr | Instrs1]
    else if
        Base = curfr,
        Redoip0 = const(llconst_code_addr(do_fail)),
        opt_util.straight_alternative(Instrs0, Between, After),
        opt_util.touches_nondet_ctrl(Between) = no,
        string.sub_string_search(Comment0, "curfr==maxfr", _)
    then
        SucceedInstr = llds_instr(goto(do_succeed(yes)), "early discard"),
        Instrs = Between ++ [SucceedInstr] ++ After
    else
        fail
    ).

    % If a decr_sp follows an incr_sp of the same amount, with the code
    % in between not referencing the stack, except possibly for a
    % restoration of succip, then the two cancel out. Assignments to
    % stack slots are allowed and are thrown away.
    %
    %   incr_sp N
    %   <...>                       =>  <...>
    %   decr_sp N
    %
    %   incr_sp N
    %   <...>                       =>  <...>
    %   succip = detstackvar(N)
    %   decr_sp N
    %
peephole_match(Instr0, Instrs0, InvalidPatterns, Instrs) :-
    Instr0 = llds_instr(Uinstr0, _Comment0),
    Uinstr0 = incr_sp(N, _, _),
    not list.member(pattern_incr_sp, InvalidPatterns),
    ( if opt_util.no_stackvars_til_decr_sp(Instrs0, N, Between, Remain) then
        Instrs = Between ++ Remain
    else
        fail
    ).

%-----------------------------------------------------------------------------%

    % Look for code patterns that can be optimized, and optimize them.
    % See the comment at the top of peephole_match for the difference
    % between the two predicates.
    %
:- pred peephole_match_norepeat(instruction::in, list(instruction)::in,
    list(pattern)::in, list(instruction)::out) is semidet.

    % If none of the instructions in brackets can affect Lval, then
    % we can transform references to tag(Lval) to Ptag and body(Lval, Ptag)
    % to Base.
    %
    %   Lval = mkword(Ptag, Base)       Lval = mkword(Ptag, Base)
    %   <...>                       =>  <...>
    %   ... tag(Lval) ...               ... Ptag ...
    %   ... body(Lval, Ptag) ...        ... Base ...
    %
peephole_match_norepeat(Instr0, Instrs0, InvalidPatterns, Instrs) :-
    Instr0 = llds_instr(Uinstr0, _),
    Uinstr0 = assign(Lval, mkword(Ptag, Base)),
    not list.member(pattern_mkword, InvalidPatterns),
    replace_tagged_ptr_components_in_instrs(Lval, Ptag, Base,
        Instrs0, Instrs1),
    Instrs = [Instr0 | Instrs1].

%-----------------------------------------------------------------------------%

:- pred replace_tagged_ptr_components_in_instrs(lval::in, ptag::in, rval::in,
    list(instruction)::in, list(instruction)::out) is det.

replace_tagged_ptr_components_in_instrs(_, _, _, [], []).
replace_tagged_ptr_components_in_instrs(Lval, Ptag, Base, Instrs0, Instrs) :-
    Instrs0 = [HeadInstr0 | TailInstrs0],
    replace_tagged_ptr_components_in_instr(Lval, Ptag, Base,
        HeadInstr0, MaybeHeadInstr),
    (
        MaybeHeadInstr = no,
        Instrs = Instrs0
    ;
        MaybeHeadInstr = yes(HeadInstr),
        replace_tagged_ptr_components_in_instrs(Lval, Ptag, Base,
            TailInstrs0, TailInstrs),
        Instrs = [HeadInstr | TailInstrs]
    ).

:- pred replace_tagged_ptr_components_in_instr(lval::in, ptag::in, rval::in,
    instruction::in, maybe(instruction)::out) is det.

replace_tagged_ptr_components_in_instr(OldLval, OldPtag, OldBase,
        Instr0, MaybeInstr) :-
    Instr0 = llds_instr(Uinstr0, Comment),
    (
        Uinstr0 = assign(Lval, Rval0),
        ( if Lval = OldLval then
            MaybeInstr = no
        else if Lval = mem_ref(_) then
            MaybeInstr = no
        else
            replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
                Rval0, Rval),
            Uinstr = assign(Lval, Rval),
            Instr = llds_instr(Uinstr, Comment),
            MaybeInstr = yes(Instr)
        )
    ;
        Uinstr0 = keep_assign(Lval, Rval0),
        ( if Lval = OldLval then
            MaybeInstr = no
        else if Lval = mem_ref(_) then
            MaybeInstr = no
        else
            replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
                Rval0, Rval),
            Uinstr = keep_assign(Lval, Rval),
            Instr = llds_instr(Uinstr, Comment),
            MaybeInstr = yes(Instr)
        )
    ;
        Uinstr0 = computed_goto(Rval0, Targets),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            Rval0, Rval),
        Uinstr = computed_goto(Rval, Targets),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        Uinstr0 = if_val(Rval0, Target),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            Rval0, Rval),
        Uinstr = if_val(Rval, Target),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        Uinstr0 = incr_hp(Target, MaybePtag, MaybeOffset, SizeRval0,
            TypeMsg, MayUseAtomicAlloc, MaybeRegionId, MaybeReuse),
        ( if Target = OldLval then
            MaybeInstr = no
        else if Target = mem_ref(_) then
            MaybeInstr = no
        else
            replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
                SizeRval0, SizeRval),
            Uinstr = incr_hp(Target, MaybePtag, MaybeOffset, SizeRval,
                TypeMsg, MayUseAtomicAlloc, MaybeRegionId, MaybeReuse),
            Instr = llds_instr(Uinstr, Comment),
            MaybeInstr = yes(Instr)
        )
    ;
        Uinstr0 = restore_hp(Rval0),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            Rval0, Rval),
        Uinstr = restore_hp(Rval),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        Uinstr0 = free_heap(Rval0),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            Rval0, Rval),
        Uinstr = free_heap(Rval),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        Uinstr0 = reset_ticket(Rval0, Reason),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            Rval0, Rval),
        Uinstr = reset_ticket(Rval, Reason),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        Uinstr0 = prune_tickets_to(Rval0),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            Rval0, Rval),
        Uinstr = prune_tickets_to(Rval),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        Uinstr0 = lc_wait_free_slot(Rval0, Lval0, Label),
        ( if Lval0 = OldLval then
            MaybeInstr = no
        else if Lval0 = mem_ref(_) then
            MaybeInstr = no
        else
            replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
                Rval0, Rval),
            Uinstr = lc_wait_free_slot(Rval, Lval0, Label),
            Instr = llds_instr(Uinstr, Comment),
            MaybeInstr = yes(Instr)
        )
    ;
        Uinstr0 = lc_spawn_off(LCRval0, LCSRval0, Label),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            LCRval0, LCRval),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            LCSRval0, LCSRval),
        Uinstr = lc_spawn_off(LCRval, LCSRval, Label),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        Uinstr0 = lc_join_and_terminate(LCRval0, LCSRval0),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            LCRval0, LCRval),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            LCSRval0, LCSRval),
        Uinstr = lc_join_and_terminate(LCRval, LCSRval),
        Instr = llds_instr(Uinstr, Comment),
        MaybeInstr = yes(Instr)
    ;
        ( Uinstr0 = save_maxfr(Lval0)
        ; Uinstr0 = mark_hp(Lval0)
        ; Uinstr0 = store_ticket(Lval0)
        ; Uinstr0 = mark_ticket_stack(Lval0)
        ; Uinstr0 = lc_create_loop_control(_NumSlots, Lval0)
        ),
        ( if Lval0 = OldLval then
            MaybeInstr = no
        else if Lval0 = mem_ref(_) then
            MaybeInstr = no
        else
            MaybeInstr = yes(Instr0)
        )
    ;
        ( Uinstr0 = comment(_)
        ; Uinstr0 = livevals(_)
        ; Uinstr0 = restore_maxfr(_)
        ; Uinstr0 = prune_ticket
        ; Uinstr0 = discard_ticket
        ),
        MaybeInstr = yes(Instr0)
    ;
        ( Uinstr0 = block(_, _, _)
        ; Uinstr0 = llcall(_, _, _, _, _, _)
        ; Uinstr0 = mkframe(_, _)
        ; Uinstr0 = label(_)
        ; Uinstr0 = goto(_)
        ; Uinstr0 = arbitrary_c_code(_, _, _)
        ; Uinstr0 = foreign_proc_code(_, _, _, _, _, _, _, _, _, _)
        ; Uinstr0 = push_region_frame(_, _)
        ; Uinstr0 = region_fill_frame(_, _, _, _, _)
        ; Uinstr0 = region_set_fixed_slot(_, _, _)
        ; Uinstr0 = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr0 = incr_sp(_, _, _)
        ; Uinstr0 = decr_sp(_)
        ; Uinstr0 = decr_sp_and_return(_)
        ; Uinstr0 = init_sync_term(_, _, _)
        ; Uinstr0 = fork_new_child(_, _)
        ; Uinstr0 = join_and_continue(_, _)
        ),
        MaybeInstr = no
    ).

:- pred replace_tagged_ptr_components_in_rval(lval::in, ptag::in, rval::in,
    rval::in, rval::out) is det.

replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
        Rval0, Rval) :-
    (
        Rval0 = unop(UnOp, RvalA0),
        ( if
            UnOp = tag,
            RvalA0 = lval(OldLval)
        then
            OldPtag = ptag(OldPtagUint8),
            Rval = const(llconst_int(uint8.cast_to_int(OldPtagUint8)))
        else
            replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
                RvalA0, RvalA),
            Rval = unop(UnOp, RvalA)
        )
    ;
        Rval0 = binop(BinOp, RvalA0, RvalB0),
        ( if
            BinOp = body,
            RvalA0 = lval(OldLval),
            RvalB0 = const(llconst_int(RvalB0Int)),
            OldPtag = ptag(OldPtagUint8),
            RvalB0Int = uint8.cast_to_int(OldPtagUint8),
            OldBase = const(_)
        then
            Rval = OldBase
        else
            replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
                RvalA0, RvalA),
            replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
                RvalB0, RvalB),
            Rval = binop(BinOp, RvalA, RvalB)
        )
    ;
        Rval0 = mkword(Ptag, BaseRval0),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            BaseRval0, BaseRval),
        Rval = mkword(Ptag, BaseRval)
    ;
        Rval0 = cast(Type, BaseRval0),
        replace_tagged_ptr_components_in_rval(OldLval, OldPtag, OldBase,
            BaseRval0, BaseRval),
        Rval = cast(Type, BaseRval)
    ;
        ( Rval0 = lval(_)
        ; Rval0 = var(_)
        ; Rval0 = mkword_hole(_)
        ; Rval0 = const(_)
        ; Rval0 = mem_addr(_)
        ),
        Rval = Rval0
    ).

%-----------------------------------------------------------------------------%

    % Given a GC method, return the list of invalid peephole optimizations.
    %
:- pred invalid_peephole_opts(gc_method::in, maybe_opt_peep_mkword::in,
    list(pattern)::out) is det.

invalid_peephole_opts(GC_Method, OptPeepMkword, InvalidPatterns) :-
    (
        GC_Method = gc_accurate,
        InvalidPatterns0 = [pattern_incr_sp]
    ;
        ( GC_Method = gc_automatic
        ; GC_Method = gc_none
        ; GC_Method = gc_boehm
        ; GC_Method = gc_boehm_debug
        ; GC_Method = gc_hgc
        ),
        InvalidPatterns0 = []
    ),
    (
        OptPeepMkword = opt_peep_mkword,
        InvalidPatterns = InvalidPatterns0
    ;
        OptPeepMkword = do_not_opt_peep_mkword,
        InvalidPatterns = [pattern_mkword | InvalidPatterns0]
    ).

%-----------------------------------------------------------------------------%

combine_decr_sp([], []).
combine_decr_sp([Instr0 | Instrs0], Instrs) :-
    combine_decr_sp(Instrs0, Instrs1),
    ( if
        Instr0 = llds_instr(assign(succip, lval(stackvar(N))), _),
        opt_util.skip_comments_livevals(Instrs1, Instrs2),
        Instrs2 = [Instr2 | Instrs3],
        Instr2 = llds_instr(decr_sp(N), _),
        opt_util.skip_comments_livevals(Instrs3, Instrs4),
        Instrs4 = [Instr4 | Instrs5],
        Instr4 = llds_instr(goto(code_succip), Comment)
    then
        NewInstr = llds_instr(decr_sp_and_return(N), Comment),
        Instrs = [NewInstr | Instrs5]
    else
        Instrs = [Instr0 | Instrs1]
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.peephole.
%-----------------------------------------------------------------------------%
