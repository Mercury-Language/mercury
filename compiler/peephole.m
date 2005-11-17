%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% peephole.m - local LLDS to LLDS optimizations based on pattern-matching.

% Authors: fjh and zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__peephole.

:- interface.

:- import_module ll_backend.llds.
:- import_module libs.globals.

:- import_module bool.
:- import_module list.

    % Peephole optimize a list of instructions.
    %
:- pred peephole__optimize(gc_method::in, list(instruction)::in,
    list(instruction)::out, bool::out) is det.

:- pred combine_decr_sp(list(instruction)::in, list(instruction)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module ll_backend.code_util.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.opt_util.

:- import_module int.
:- import_module map.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Patterns that can be switched off.
    %
:- type pattern --->        incr_sp.

    % We zip down to the end of the instruction list, and start attempting
    % to optimize instruction sequences. As long as we can continue
    % optimizing the instruction sequence, we keep doing so;
    % when we find a sequence we can't optimize, we back up and try
    % to optimize the sequence starting with the previous instruction.
    %
peephole__optimize(GC_Method, Instrs0, Instrs, Mod) :-
    peephole__invalid_opts(GC_Method, InvalidPatterns),
    peephole__optimize_2(InvalidPatterns, Instrs0, Instrs, Mod).

:- pred peephole__optimize_2(list(pattern)::in, list(instruction)::in,
    list(instruction)::out, bool::out) is det.

peephole__optimize_2(_, [], [], no).
peephole__optimize_2(InvalidPatterns, [Instr0 - Comment | Instrs0],
        Instrs, Mod) :-
    peephole__optimize_2(InvalidPatterns, Instrs0, Instrs1, Mod0),
    peephole__opt_instr(Instr0, Comment, InvalidPatterns, Instrs1, Instrs,
        Mod1),
    ( Mod0 = no, Mod1 = no ->
        Mod = no
    ;
        Mod = yes
    ).

    % Try to optimize the beginning of the given instruction sequence.
    % If successful, try it again.
    %
:- pred peephole__opt_instr(instr::in, string::in, list(pattern)::in,
    list(instruction)::in, list(instruction)::out, bool::out) is det.

peephole__opt_instr(Instr0, Comment0, InvalidPatterns, Instrs0, Instrs, Mod) :-
    (
        opt_util__skip_comments(Instrs0, Instrs1),
        peephole__match(Instr0, Comment0, InvalidPatterns, Instrs1, Instrs2)
    ->
        (
            Instrs2 = [Instr2 - Comment2 | Instrs3],
            peephole__opt_instr(Instr2, Comment2, InvalidPatterns,
                Instrs3, Instrs, _)
        ;
            Instrs2 = [],
            Instrs = Instrs2
        ),
        Mod = yes
    ;
        Instrs = [Instr0 - Comment0 | Instrs0],
        Mod = no
    ).

%-----------------------------------------------------------------------------%

    % Build a map that associates each label in a computed goto with the
    % values of the switch rval that cause a jump to it.
    %
:- pred peephole__build_jump_label_map(list(label)::in, int::in,
    map(label, list(int))::in, map(label, list(int))::out) is det.

peephole__build_jump_label_map([], _, !LabelMap).
peephole__build_jump_label_map([Label | Labels], Val, !LabelMap) :-
    ( map__search(!.LabelMap, Label, Vals0) ->
        map__det_update(!.LabelMap, Label, [Val | Vals0], !:LabelMap)
    ;
        map__det_insert(!.LabelMap, Label, [Val], !:LabelMap)
    ),
    peephole__build_jump_label_map(Labels, Val + 1, !LabelMap).

    % If one of the two labels has only one associated value, return it and
    % the associated value as the first two output arguments, and the
    % remaining label as the last output argument.
    %
:- pred peephole__pick_one_val_label(pair(label, list(int))::in,
    pair(label, list(int))::in, label::out, int::out, label::out) is semidet.

peephole__pick_one_val_label(LabelVals1, LabelVals2, OneValLabel, Val,
        OtherLabel) :-
    LabelVals1 = Label1 - Vals1,
    LabelVals2 = Label2 - Vals2,
    ( Vals1 = [Val1] ->
        OneValLabel = Label1,
        Val = Val1,
        OtherLabel = Label2
    ; Vals2 = [Val2] ->
        OneValLabel = Label2,
        Val = Val2,
        OtherLabel = Label1
    ;
        fail
    ).

    % Look for code patterns that can be optimized, and optimize them.
    %
:- pred peephole__match(instr::in, string::in, list(pattern)::in,
    list(instruction)::in, list(instruction)::out) is semidet.

    % A `computed_goto' with all branches pointing to the same label
    % can be replaced with an unconditional goto.
    %
    % A `computed_goto' with all branches but one pointing to the same label
    % can be replaced with a conditional branch followed by an unconditional
    % goto.
    %
peephole__match(computed_goto(SelectorRval, Labels), Comment, _,
        Instrs0, Instrs) :-
    peephole__build_jump_label_map(Labels, 0, map__init, LabelMap),
    map__to_assoc_list(LabelMap, LabelValsList),
    (
        LabelValsList = [Label - _]
    ->
        GotoInstr = goto(label(Label)) - Comment,
        Instrs = [GotoInstr | Instrs0]
    ;
        LabelValsList = [LabelVals1, LabelVals2],
        peephole__pick_one_val_label(LabelVals1, LabelVals2, OneValLabel,
            Val, OtherLabel)
    ->
        CondRval = binop(eq, SelectorRval, const(int_const(Val))),
        CommentInstr = comment(Comment) - "",
        BranchInstr = if_val(CondRval, label(OneValLabel)) - "",
        GotoInstr = goto(label(OtherLabel)) - Comment,
        Instrs = [CommentInstr, BranchInstr, GotoInstr | Instrs0]
    ;
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
peephole__match(if_val(Rval, CodeAddr), Comment, _, Instrs0, Instrs) :-
    (
        opt_util__is_const_condition(Rval, Taken)
    ->
        (
            Taken = yes,
            Instrs = [goto(CodeAddr) - Comment | Instrs0]
        ;
            Taken = no,
            Instrs = Instrs0
        )
    ;
        opt_util__skip_comments(Instrs0, Instrs1),
        Instrs1 = [Instr1 | _],
        Instr1 = goto(CodeAddr) - _
    ->
        Instrs = Instrs0
    ;
        CodeAddr = label(Label),
        opt_util__is_this_label_next(Label, Instrs0, _)
    ->
        Instrs = Instrs0
    ;
        fail
    ).

    % If a `mkframe' is followed by an assignment to its redoip slot,
    % with the instructions in between containing only straight-line code,
    % we can delete the assignment and instead just set the redoip
    % directly in the `mkframe'.
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
    % These two classes of patterns are mutually exclusive because if_val
    % is not straight-line code.
    %
    % We also look for the following pattern, which can happen when predicates
    % that are actually semidet are declared to be nondet:
    %
    %   mkframe(NFI, dofail)
    %   <straight,nostack instrs>   =>  <straight,nostack instrs>
    %   succeed                         proceed
    %
peephole__match(mkframe(NondetFrameInfo, yes(Redoip1)), Comment, _,
        Instrs0, Instrs) :-
    (
        % A mkframe sets curfr to point to the new frame
        % only for ordinary frames, not temp frames.
        ( NondetFrameInfo = ordinary_frame(_, _, _) ->
            AllowedBases = [maxfr, curfr]
        ;
            AllowedBases = [maxfr]
        ),
        opt_util__next_assign_to_redoip(Instrs0, AllowedBases, [], Redoip2,
            Skipped, Rest),
        opt_util__touches_nondet_ctrl(Skipped, no)
    ->
        Instrs1 = Skipped ++ Rest,
        Instrs = [mkframe(NondetFrameInfo, yes(Redoip2)) - Comment | Instrs1]
    ;
        opt_util__skip_comments_livevals(Instrs0, Instrs1),
        Instrs1 = [Instr1 | Instrs2],
        Instr1 = if_val(Test, Target) - Comment2,
        (
            Redoip1 = do_fail,
            ( Target = do_redo ; Target = do_fail)
        ->
            InstrsPrime = [
                if_val(Test, do_redo) - Comment2,
                mkframe(NondetFrameInfo, yes(do_fail)) - Comment
                | Instrs2
            ]
        ;
            Redoip1 = label(_)
        ->
            (
                Target = do_fail
            ->
                InstrsPrime = [
                    if_val(Test, do_redo) - Comment2,
                    mkframe(NondetFrameInfo, yes(Redoip1)) - Comment
                    | Instrs2
                ]
            ;
                Target = do_redo
            ->
                InstrsPrime = [
                    mkframe(NondetFrameInfo, yes(Redoip1)) - Comment,
                    if_val(Test, Redoip1) - Comment2
                    | Instrs2
                ]
            ;
                fail
            )
        ;
            fail
        )
    ->
        Instrs = InstrsPrime
    ;
        Redoip1 = do_fail,
        no_stack_straight_line(Instrs0, Straight, Instrs1),
        Instrs1 = [Instr1 | Instrs2],
        Instr1 = goto(do_succeed(_)) - _
    ->
        GotoSuccip = goto(succip) - "return from optimized away mkframe",
        Instrs = Straight ++ [GotoSuccip | Instrs2]
    ;
        fail
    ).

    % If a `store_ticket' is followed by a `reset_ticket',
    % we can delete the `reset_ticket'.
    %
    %   store_ticket(Lval)          =>  store_ticket(Lval)
    %   reset_ticket(Lval, _R)
    %
peephole__match(store_ticket(Lval), Comment, _, Instrs0, Instrs) :-
    opt_util__skip_comments(Instrs0, Instrs1),
    Instrs1 = [reset_ticket(lval(Lval), _Reason) - _Comment2 | Instrs2],
    Instrs = [store_ticket(Lval) - Comment | Instrs2].

    % If an assignment to a redoip slot is followed by another, with
    % the instructions in between containing only straight-line code,
    % we can delete one of the asignments:
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
peephole__match(assign(redoip(lval(Base)), Redoip), Comment, _,
        Instrs0, Instrs) :-
    (
        opt_util__next_assign_to_redoip(Instrs0, [Base], [], Redoip2,
            Skipped, Rest),
        opt_util__touches_nondet_ctrl(Skipped, no)
    ->
        Instrs1 = Skipped ++ Rest,
        Instrs = [assign(redoip(lval(Base)),
            const(code_addr_const(Redoip2))) - Comment | Instrs1]
    ;
        Base = curfr,
        Redoip = const(code_addr_const(do_fail)),
        opt_util__straight_alternative(Instrs0, Between, After),
        opt_util__touches_nondet_ctrl(Between, no),
        string__sub_string_search(Comment, "curfr==maxfr", _)
    ->
        list__condense([Between,
            [goto(do_succeed(yes)) - "early discard"], After], Instrs)
    ;
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
peephole__match(incr_sp(N, _), _, InvalidPatterns, Instrs0, Instrs) :-
    \+ list__member(incr_sp, InvalidPatterns),
    ( opt_util__no_stackvars_til_decr_sp(Instrs0, N, Between, Remain) ->
        Instrs = Between ++ Remain
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

    % Given a GC method, return the list of invalid peephole optimizations.
    %
:- pred peephole__invalid_opts(gc_method::in, list(pattern)::out) is det.

peephole__invalid_opts(GC_Method, InvalidPatterns) :-
    ( GC_Method = accurate ->
        InvalidPatterns = [incr_sp]
    ;
        InvalidPatterns = []
    ).

%-----------------------------------------------------------------------------%

combine_decr_sp([], []).
combine_decr_sp([Instr0 | Instrs0], Instrs) :-
    combine_decr_sp(Instrs0, Instrs1),
    (
        Instr0 = assign(succip, lval(stackvar(N))) - _,
        opt_util__skip_comments_livevals(Instrs1, Instrs2),
        Instrs2 = [Instr2 | Instrs3],
        Instr2 = decr_sp(N) - _,
        opt_util__skip_comments_livevals(Instrs3, Instrs4),
        Instrs4 = [Instr4 | Instrs5],
        Instr4 = goto(succip) - Comment
    ->
        NewInstr = decr_sp_and_return(N) - Comment,
        Instrs = [NewInstr | Instrs5]
    ;
        Instrs = [Instr0 | Instrs1]
    ).

%-----------------------------------------------------------------------------%
:- end_module peephole.
%-----------------------------------------------------------------------------%
