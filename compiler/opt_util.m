%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: opt_util.m.
% Main author: zs.
%
% Utilities for LLDS to LLDS optimization.
%
%---------------------------------------------------------------------------%

:- module ll_backend.opt_util.
:- interface.

:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type instrmap == map(label, instruction).
:- type lvalmap == map(label, maybe(instruction)).
:- type tailmap == map(label, list(instruction)).
:- type succmap == map(label, bool).

%---------------------------------------------------------------------------%
%
% Finding the prologue.
%

:- pred get_prologue(list(instruction)::in, instruction::out,
    list(instruction)::out, list(instruction)::out) is det.

%---------------------------------------------------------------------------%
%
% Gathering and skipping comments.
%

:- pred gather_comments(list(instruction)::in,
    list(instruction)::out, list(instruction)::out) is det.

:- pred gather_comments_livevals(list(instruction)::in,
    list(instruction)::out, list(instruction)::out) is det.

    % Given a list of instructions, skip past any comment instructions
    % at the start and return the remaining instructions. We do this because
    % comment instructions get in the way of peephole optimization.
    %
:- pred skip_comments(list(instruction)::in, list(instruction)::out) is det.

:- pred skip_comments_livevals(list(instruction)::in,
    list(instruction)::out) is det.

:- pred skip_comments_labels(list(instruction)::in,
    list(instruction)::out) is det.

:- pred skip_comments_livevals_labels(list(instruction)::in,
    list(instruction)::out) is det.

%---------------------------------------------------------------------------%
%
% Tests of instruction list prefixes.
%

    % Check whether the named label follows without any intervening code.
    % If yes, return the instructions after the label.
    %
:- pred is_this_label_next(label::in, list(instruction)::in,
    list(instruction)::out) is semidet.

    % Is a proceed instruction (i.e. a goto(succip) instruction)
    % next in the instruction list, possibly preceded by a restoration
    % of succip and a det stack frame removal? If yes, return the
    % instructions up to the proceed.
    %
:- pred is_proceed_next(list(instruction)::in,
    list(instruction)::out) is semidet.

    % Is a proceed instruction (i.e. a goto(succip) instruction)
    % next in the instruction list, possibly preceded by an assignment
    % to r1, a restoration of succip and a det stack frame removal?
    % If yes, return the instructions up to the proceed.
    %
:- pred is_sdproceed_next(list(instruction)::in,
    list(instruction)::out) is semidet.

    % Same as the previous predicate, but also return whether it is
    % a success or a fail.
    %
:- pred is_sdproceed_next_sf(list(instruction)::in,
    list(instruction)::out, bool::out) is semidet.

    % Is a succeed instruction (i.e. a goto(do_succeed(_)) instruction)
    % next in the instruction list? If yes, return the instructions
    % up to and including the succeed.
    %
:- pred is_succeed_next(list(instruction)::in,
    list(instruction)::out) is semidet.

    % Is the following code a test of r1, followed in both continuations
    % by a semidet proceed? Is the code in both continuations the same,
    % modulo livevals annotations and the value assigned to r1? Is MR_TRUE
    % assigned to r1 in the success continuation and MR_FALSE in the failure
    % continuation? If the answer is yes to all these questions, return
    % the code shared by the two continuations.
    %
:- pred is_forkproceed_next(list(instruction)::in, tailmap::in,
    list(instruction)::out) is semidet.

    % Does the following code consist of straighline instructions that do not
    % modify nondet frame linkages, plus possibly if_val(..., dofail), and then
    % a succeed? If yes, then return all the instructions up to the succeed,
    % and all the following instructions.
    %
:- pred straight_alternative(list(instruction)::in,
    list(instruction)::out, list(instruction)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Finding things in instruction lists.
%

    % Find the first label in the instruction stream.
    %
:- pred find_first_label(list(instruction)::in, label::out) is det.

    % Skip to the next label, returning the code before the label,
    % and the label together with the code after the label.
    %
:- pred skip_to_next_label(list(instruction)::in,
    list(instruction)::out, list(instruction)::out) is det.

    % Find the instructions up to and including the next one that
    % cannot fall through.
    %
:- pred find_no_fallthrough(list(instruction)::in,
    list(instruction)::out) is det.

    % Find and return the initial sequence of instructions that do not
    % refer to stackvars and do not branch.
    %
:- pred no_stack_straight_line(list(instruction)::in,
    list(instruction)::out, list(instruction)::out) is det.

%---------------------%

    % Find the next assignment to the redoip of the frame whose address
    % is given by the base addresses in the second argument, provided
    % it is guaranteed to be reached from here, and guaranteed not to be
    % reached from anywhere else by a jump.
    %
:- pred next_assign_to_redoip(list(instruction)::in, list(lval)::in,
    list(instruction)::in, code_addr::out, list(instruction)::out,
    list(instruction)::out) is semidet.

    % Check whether the given instruction sequence consist of an initial
    % sequence of instructions that do not refer to stackvars and do not
    % contain any entry points or unconditional branches away, until a
    % goto(do_succeed(yes)) instruction. If yes, return that initial sequence,
    % the comment on the goto(do_succeed(yes)) instruction, and the
    % instructions after it.
    %
:- pred may_replace_succeed_with_succeed_discard(list(instruction)::in,
    list(instruction)::out, string::out, list(instruction)::out) is semidet.

    % See whether instructions until the next decr_sp (if any) refer to
    % any stackvars or branch away. If not, return the instructions up to
    % the decr_sp. A restoration of succip from the bottom stack slot
    % is allowed; this instruction is not returned in the output.
    % The same thing applies to assignments to detstackvars; these are
    % not useful if we throw away the stack frame.
    %
:- pred no_stackvars_til_decr_sp(list(instruction)::in, int::in,
    list(instruction)::out, list(instruction)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Filtering instruction lists.
%

    % Remove the labels from a block of code for jumpopt.
    %
:- pred filter_out_labels(list(instruction)::in, list(instruction)::out)
    is det.

    % Remove the assignment to r1 from the list returned by is_sdproceed_next.
    %
:- pred filter_out_r1(list(instruction)::in, maybe(rval_const)::out,
    list(instruction)::out) is det.

    % Remove any livevals instructions that do not precede an instruction
    % that needs one.
    %
:- pred filter_out_bad_livevals(list(instruction)::in, list(instruction)::out)
    is det.

    % Remove the livevals instruction from the list returned by
    % is_proceed_next.
    %
:- pred filter_out_livevals(list(instruction)::in, list(instruction)::out)
    is det.

    % Get just the livevals instructions from a list of instructions.
    %
:- pred filter_in_livevals(list(instruction)::in,
    list(instruction)::out) is det.

%---------------------%

    % Whenever the input list of instructions contains two livevals pseudo-ops
    % without an intervening no-fall-through instruction, ensure that the
    % first of these registers as live every lval that is live in the second,
    % except those that are assigned to by intervening instructions. This makes
    % the shadowing of the second livevals by the first benign.
    %
:- pred propagate_livevals(list(instruction)::in, list(instruction)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Simple tests.
%

    % See if these instructions touch nondet stack controls, i.e.
    % the virtual machine registers that point to the nondet stack
    % (curfr and maxfr) and the fixed slots in nondet stack frames.
    %
:- func touches_nondet_ctrl(list(instruction)) = bool.

    % Find out if an instruction sequence has both incr_sp and decr_sp.
    %
:- pred has_both_incr_decr_sp(list(instruction)::in) is semidet.

    % Check whether an instruction can possibly branch away.
    %
:- func can_instr_branch_away(instr) = bool.

    % Check whether an instruction can possibly fall through
    % to the next instruction without using its label.
    %
:- func can_instr_fall_through(instr) = bool.

    % See if the condition of an if-then-else is constant, and if yes,
    % whether the branch will be taken or not.
    %
:- pred is_const_condition(rval::in, bool::out) is semidet.

    % Check whether a code_addr, when the target of a goto, represents either
    % a call or a proceed/succeed; if so, it is the end of an extended basic
    % block and needs a livevals in front of it.
    %
:- func livevals_addr(code_addr) = bool.

%---------------------------------------------------------------------------%
%
% Counting.
%

    % Update the maximum R and F temp variable numbers used.
    %
:- pred count_temps_instr_list(list(instruction)::in,
    int::in, int::out, int::in, int::out) is det.

:- pred count_temps_instr(instr::in,
    int::in, int::out, int::in, int::out) is det.

    % Count the number of hp increments in a block of code.
    %
:- pred count_incr_hp(list(instruction)::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Finding possible jump targets.
%

    % Determine all the labels and code addresses which are referenced
    % by an instruction. The code addresses that are labels are returned
    % in both output arguments.
    %
:- pred instr_labels(instr::in, list(label)::out, list(code_addr)::out) is det.
:- pred instr_labels_only(instr::in, list(label)::out) is det.

    % Given an instruction, find the set of labels and other code addresses
    % to which it can cause control to transfer. In the case of calls, this
    % includes transfer via return from the called procedure.
    %
:- pred possible_targets(instr::in, list(label)::out, list(code_addr)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Finding stack references.
%

    % See whether a (list of) instructions or instruction components
    % references the current stack frame (on either stack).
    %
:- func lval_refers_stackvars(lval) = bool.
:- func rval_refers_stackvars(rval) = bool.
:- func instr_refers_to_stack(instruction) = bool.
:- func block_refers_to_stack(list(instruction)) = bool.

    % Find out what rvals, if any, are needed to access an lval.
    %
:- pred lval_access_rvals(lval::in, list(rval)::out) is det.

%---------------------------------------------------------------------------%
%
% Substitutions.
%

    % Replace references to one set of local labels with references to another
    % set, in one instruction or a list of instructions. Control references
    % (those that can cause a transfer of control from the instruction they
    % occur in to the replaced label, either directly or via return from a
    % called procedure) are always replaced; references that treat the label
    % as data are replaced iff the third argument is set to "yes".
    %
    % With replace_labels_instruction_list, the last arg says whether
    % it is OK to replace a label in a label instruction itself.
    %
:- pred replace_labels_instruction_list(
    list(instruction)::in, list(instruction)::out,
    map(label, label)::in, bool::in, bool::in) is det.

:- pred replace_labels_instruction(instruction::in, instruction::out,
    map(label, label)::in, bool::in) is det.

:- pred replace_labels_instr(instr::in, instr::out,
    map(label, label)::in, bool::in) is det.

:- pred replace_labels_comps(
    list(foreign_proc_component)::in, list(foreign_proc_component)::out,
    map(label, label)::in) is det.

:- pred replace_labels_code_addr(code_addr::in, code_addr::out,
    map(label, label)::in) is det.

:- pred replace_labels_maybe_label_list(list(maybe(label))::in,
    list(maybe(label))::out, map(label, label)::in) is det.

:- pred replace_labels_label(label::in, label::out, map(label, label)::in)
    is det.

%---------------------------------------------------------------------------%
%
% Printing.
%

    % Format a label or proc_label for verbose messages during compilation.
    %
:- func format_label(label) = string.
:- func format_proc_label(proc_label) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.code_util.
:- import_module ll_backend.exprn_aux.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.pred_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
%
% Finding the prologue.
%

get_prologue(Instrs0, LabelInstr, Comments, Instrs) :-
    gather_comments(Instrs0, Comments1, Instrs1),
    ( if
        Instrs1 = [Instr1 | Instrs2],
        Instr1 = llds_instr(label(_), _)
    then
        LabelInstr = Instr1,
        gather_comments(Instrs2, Comments2, Instrs),
        list.append(Comments1, Comments2, Comments)
    else
        unexpected($pred, "procedure does not begin with label")
    ).

%---------------------------------------------------------------------------%
%
% Gathering and skipping comments.
%

gather_comments(Instrs0, Comments, Instrs) :-
    ( if
        Instrs0 = [Instr0 | Instrs1],
        Instr0 = llds_instr(comment(_), _)
    then
        gather_comments(Instrs1, Comments0, Instrs),
        Comments = [Instr0 | Comments0]
    else
        Instrs = Instrs0,
        Comments = []
    ).

gather_comments_livevals(Instrs0, Comments, Instrs) :-
    ( if
        Instrs0 = [Instr0 | Instrs1],
        Instr0 = llds_instr(Uinstr0, _),
        ( Uinstr0 = comment(_)
        ; Uinstr0 = livevals(_)
        )
    then
        gather_comments_livevals(Instrs1, Comments0, Instrs),
        Comments = [Instr0 | Comments0]
    else
        Instrs = Instrs0,
        Comments = []
    ).

skip_comments(Instrs0, Instrs) :-
    ( if Instrs0 = [llds_instr(comment(_), _) | Instrs1] then
        skip_comments(Instrs1, Instrs)
    else
        Instrs = Instrs0
    ).

skip_comments_livevals(Instrs0, Instrs) :-
    ( if Instrs0 = [llds_instr(comment(_), _) | Instrs1] then
        skip_comments(Instrs1, Instrs)
    else if Instrs0 = [llds_instr(livevals(_), _) | Instrs1] then
        skip_comments_livevals(Instrs1, Instrs)
    else
        Instrs = Instrs0
    ).

skip_comments_labels(Instrs0, Instrs) :-
    ( if Instrs0 = [llds_instr(comment(_), _) | Instrs1] then
        skip_comments_labels(Instrs1, Instrs)
    else if Instrs0 = [llds_instr(label(_), _) | Instrs1] then
        skip_comments_labels(Instrs1, Instrs)
    else
        Instrs = Instrs0
    ).

skip_comments_livevals_labels(Instrs0, Instrs) :-
    ( if Instrs0 = [llds_instr(comment(_), _) | Instrs1] then
        skip_comments_livevals_labels(Instrs1, Instrs)
    else if Instrs0 = [llds_instr(livevals(_), _) | Instrs1] then
        skip_comments_livevals_labels(Instrs1, Instrs)
    else if Instrs0 = [llds_instr(label(_), _) | Instrs1] then
        skip_comments_livevals_labels(Instrs1, Instrs)
    else
        Instrs = Instrs0
    ).

%---------------------------------------------------------------------------%
%
% Tests of instruction list prefixes.
%

is_this_label_next(Label, [Instr | Moreinstr], Remainder) :-
    Instr = llds_instr(Uinstr, _Comment),
    ( if Uinstr = comment(_) then
        is_this_label_next(Label, Moreinstr, Remainder)
    else if Uinstr = livevals(_) then
        % XXX This is questionable.
        is_this_label_next(Label, Moreinstr, Remainder)
    else if Uinstr = label(NextLabel) then
        ( if Label = NextLabel then
            Remainder = Moreinstr
        else
            is_this_label_next(Label, Moreinstr, Remainder)
        )
    else
        fail
    ).

is_proceed_next(Instrs0, InstrsBetween) :-
    skip_comments_labels(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    ( if Instr1 = llds_instr(assign(succip, lval(stackvar(_))), _) then
        Instr1use = Instr1,
        skip_comments_labels(Instrs2, Instrs3)
    else
        Instr1use = llds_instr(comment("no succip restoration"), ""),
        Instrs3 = Instrs1
    ),
    Instrs3 = [Instr3 | Instrs4],
    ( if Instr3 = llds_instr(decr_sp(_), _) then
        Instr3use = Instr3,
        skip_comments_labels(Instrs4, Instrs5)
    else
        Instr3use = llds_instr(comment("no sp restoration"), ""),
        Instrs5 = Instrs3
    ),
    Instrs5 = [Instr5 | Instrs6],
    Instr5 = llds_instr(livevals(_), _),
    skip_comments_labels(Instrs6, Instrs7),
    Instrs7 = [Instr7 | _],
    Instr7 = llds_instr(goto(code_succip), _),
    InstrsBetween = [Instr1use, Instr3use, Instr5].

is_sdproceed_next(Instrs0, InstrsBetween) :-
    is_sdproceed_next_sf(Instrs0, InstrsBetween, _).

is_sdproceed_next_sf(Instrs0, InstrsBetween, Success) :-
    skip_comments_labels(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    ( if Instr1 = llds_instr(assign(succip, lval(stackvar(_))), _) then
        Instr1use = Instr1,
        skip_comments_labels(Instrs2, Instrs3)
    else
        Instr1use = llds_instr(comment("no succip restoration"), ""),
        Instrs3 = Instrs1
    ),
    Instrs3 = [Instr3 | Instrs4],
    ( if Instr3 = llds_instr(decr_sp(_), _) then
        Instr3use = Instr3,
        skip_comments_labels(Instrs4, Instrs5)
    else
        Instr3use = llds_instr(comment("no sp restoration"), ""),
        Instrs5 = Instrs3
    ),
    Instrs5 = [Instr5 | Instrs6],
    Instr5 = llds_instr(assign(reg(reg_r, 1), const(R1val)), _),
    (
        R1val = llconst_true,
        Success = yes
    ;
        R1val = llconst_false,
        Success = no
    ),
    skip_comments_labels(Instrs6, Instrs7),
    Instrs7 = [Instr7 | Instrs8],
    Instr7 = llds_instr(livevals(_), _),
    skip_comments_labels(Instrs8, Instrs9),
    Instrs9 = [Instr9 | _],
    Instr9 = llds_instr(goto(code_succip), _),
    InstrsBetween = [Instr1use, Instr3use, Instr5, Instr7].

is_succeed_next(Instrs0, InstrsBetweenIncl) :-
    skip_comments_labels(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    Instr1 = llds_instr(livevals(_), _),
    skip_comments_labels(Instrs2, Instrs3),
    Instrs3 = [Instr3 | _],
    Instr3 = llds_instr(goto(do_succeed(_)), _),
    InstrsBetweenIncl = [Instr1, Instr3].

is_forkproceed_next(Instrs0, Sdprocmap, Between) :-
    skip_comments_labels(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    Instr1 = llds_instr(Uinstr1, _),
    ( if
        Uinstr1 = if_val(lval(reg(reg_r, 1)), code_label(JumpLabel))
    then
        map.search(Sdprocmap, JumpLabel, BetweenJump),
        is_sdproceed_next(Instrs2, BetweenFall),
        filter_out_r1(BetweenJump, yes(llconst_true), BetweenTrue0),
        filter_out_livevals(BetweenTrue0, Between),
        filter_out_r1(BetweenFall, yes(llconst_false), BetweenFalse0),
        filter_out_livevals(BetweenFalse0, Between)
    else if
        Uinstr1 = if_val(unop(logical_not, lval(reg(reg_r, 1))),
            code_label(JumpLabel))
    then
        map.search(Sdprocmap, JumpLabel, BetweenJump),
        is_sdproceed_next(Instrs2, BetweenFall),
        filter_out_r1(BetweenJump, yes(llconst_false), BetweenFalse0),
        filter_out_livevals(BetweenFalse0, Between),
        filter_out_r1(BetweenFall, yes(llconst_true), BetweenTrue0),
        filter_out_livevals(BetweenTrue0, Between)
    else
        fail
    ).

straight_alternative(Instrs0, Between, After) :-
    straight_alternative_acc(Instrs0, [], BetweenRev, After),
    list.reverse(BetweenRev, Between).

:- pred straight_alternative_acc(list(instruction)::in,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::out) is semidet.

straight_alternative_acc([Instr0 | Instrs0], !Between, After) :-
    Instr0 = llds_instr(Uinstr0, _),
    ( if
        Uinstr0 = label(_)
    then
        fail
    else if
        Uinstr0 = goto(do_succeed(no))
    then
        After = Instrs0
    else if
        (
            can_instr_branch_away(Uinstr0) = no,
            touches_nondet_ctrl_instr(Uinstr0) = no
        ;
            Uinstr0 = if_val(_, CodeAddr),
            ( CodeAddr = do_fail ; CodeAddr = do_redo )
        )
    then
        !:Between = [Instr0 | !.Between],
        straight_alternative_acc(Instrs0, !Between, After)
    else
        fail
    ).

%---------------------------------------------------------------------------%
%
% Finding things in instruction lists.
%

find_first_label([], _) :-
    unexpected($pred, "cannot find first label").
find_first_label([Instr0 | Instrs0], Label) :-
    ( if Instr0 = llds_instr(label(LabelPrime), _) then
        Label = LabelPrime
    else
        find_first_label(Instrs0, Label)
    ).

skip_to_next_label([], [], []).
skip_to_next_label([Instr0 | Instrs0], Before, Remain) :-
    ( if Instr0 = llds_instr(label(_), _) then
        Before = [],
        Remain = [Instr0 | Instrs0]
    else
        skip_to_next_label(Instrs0, Before1, Remain),
        Before = [Instr0 | Before1]
    ).

find_no_fallthrough([], []).
find_no_fallthrough([Instr0 | Instrs0], Instrs) :-
    ( if
        Instr0 = llds_instr(Uinstr0, _),
        can_instr_fall_through(Uinstr0) = no
    then
        Instrs = [Instr0]
    else
        find_no_fallthrough(Instrs0, Instrs1),
        Instrs = [Instr0 | Instrs1]
    ).

no_stack_straight_line(Instrs0, StraightLine, Instrs) :-
    no_stack_straight_line_acc(Instrs0, [], RevStraightLine, Instrs),
    list.reverse(RevStraightLine, StraightLine).

:- pred no_stack_straight_line_acc(list(instruction)::in,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::out) is det.

no_stack_straight_line_acc([], !RevStraightLine, []).
no_stack_straight_line_acc([Instr0 | Instrs0], !RevStraightLine, Instrs) :-
    Instr0 = llds_instr(Uinstr, _),
    ( if
        (
            Uinstr = comment(_)
        ;
            Uinstr = livevals(_)
        ;
            Uinstr = assign(Lval, Rval),
            lval_refers_stackvars(Lval) = no,
            rval_refers_stackvars(Rval) = no
        )
    then
        !:RevStraightLine = [Instr0 | !.RevStraightLine],
        no_stack_straight_line_acc(Instrs0, !RevStraightLine, Instrs)
    else
        Instrs = [Instr0 | Instrs0]
    ).

%---------------------%

next_assign_to_redoip([Instr | Instrs], AllowedBases, RevSkip,
        Redoip, Skip, Rest) :-
    Instr = llds_instr(Uinstr, _Comment),
    ( if
        Uinstr = assign(redoip_slot(lval(Fr)),
            const(llconst_code_addr(Redoip0))),
        list.member(Fr, AllowedBases)
    then
        Redoip = Redoip0,
        list.reverse(RevSkip, Skip),
        Rest = Instrs
    else if
        Uinstr = mkframe(_, _)
    then
        fail
    else if
        Uinstr = label(_)
    then
        fail
    else
        CanBranchAway = can_instr_branch_away(Uinstr),
        (
            CanBranchAway = no,
            next_assign_to_redoip(Instrs, AllowedBases, [Instr | RevSkip],
                Redoip, Skip, Rest)
        ;
            CanBranchAway = yes,
            fail
        )
    ).

may_replace_succeed_with_succeed_discard(Instrs0, UntilSucceed, SucceedComment,
        Remain) :-
    may_replace_succeed_with_succeed_discard_acc(Instrs0, [], RevUntilSucceed,
        SucceedComment, Remain),
    list.reverse(RevUntilSucceed, UntilSucceed).

:- pred may_replace_succeed_with_succeed_discard_acc(list(instruction)::in,
    list(instruction)::in, list(instruction)::out, string::out,
    list(instruction)::out) is semidet.

may_replace_succeed_with_succeed_discard_acc([Instr0 | Instrs0],
        !RevUntilSucceed, SucceedComment, Remain) :-
    Instr0 = llds_instr(Uinstr, Comment),
    ( if
        Uinstr = goto(do_succeed(yes))
    then
        SucceedComment = Comment,
        Remain = Instrs0
    else if
        (
            Uinstr = assign(Lval, Rval),
            lval_refers_stackvars(Lval) = no,
            rval_refers_stackvars(Rval) = no
        ;
            ( Uinstr = comment(_)
            ; Uinstr = livevals(_)
            ; Uinstr = if_val(_, _)
            ; Uinstr = incr_hp(_, _, _, _, _, _, _, _)
            ; Uinstr = mark_hp(_)
            ; Uinstr = restore_hp(_)
            ; Uinstr = free_heap(_)
            ; Uinstr = store_ticket(_)
            ; Uinstr = store_ticket(_)
            ; Uinstr = reset_ticket(_, _)
            ; Uinstr = prune_ticket
            ; Uinstr = discard_ticket
            ; Uinstr = mark_ticket_stack(_)
            ; Uinstr = prune_tickets_to(_)
            )
        )
    then
        !:RevUntilSucceed = [Instr0 | !.RevUntilSucceed],
        may_replace_succeed_with_succeed_discard_acc(Instrs0, !RevUntilSucceed,
            SucceedComment, Remain)
    else
        fail
    ).

no_stackvars_til_decr_sp([Instr0 | Instrs0], FrameSize, Between, Remain) :-
    Instr0 = llds_instr(Uinstr0, _),
    (
        Uinstr0 = comment(_),
        no_stackvars_til_decr_sp(Instrs0, FrameSize, Between0, Remain),
        Between = [Instr0 | Between0]
    ;
        Uinstr0 = livevals(_),
        no_stackvars_til_decr_sp(Instrs0, FrameSize, Between0, Remain),
        Between = [Instr0 | Between0]
    ;
        Uinstr0 = assign(Lval, Rval),
        ( if
            Lval = stackvar(_),
            rval_refers_stackvars(Rval) = no
        then
            no_stackvars_til_decr_sp(Instrs0, FrameSize, Between, Remain)
        else if
            Lval = succip,
            Rval = lval(stackvar(FrameSize)),
            skip_comments(Instrs0, Instrs1),
            Instrs1 = [llds_instr(decr_sp(FrameSize), _) | Instrs2]
        then
            Between = [],
            Remain = Instrs2
        else
            lval_refers_stackvars(Lval) = no,
            rval_refers_stackvars(Rval) = no,
            no_stackvars_til_decr_sp(Instrs0, FrameSize, Between0, Remain),
            Between = [Instr0 | Between0]
        )
    ;
        Uinstr0 = incr_hp(Lval, _, _, Rval, _, _, MaybeRegionRval,
            MaybeReuse),
        lval_refers_stackvars(Lval) = no,
        rval_refers_stackvars(Rval) = no,
        (
            MaybeRegionRval = yes(RegionRval),
            rval_refers_stackvars(RegionRval) = no
        ;
            MaybeRegionRval = no
        ),
        (
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            rval_refers_stackvars(ReuseRval) = no,
            (
                MaybeFlagLval = yes(FlagLval),
                lval_refers_stackvars(FlagLval) = no
            ;
                MaybeFlagLval = no
            )
        ;
            MaybeReuse = no_llds_reuse
        ),
        no_stackvars_til_decr_sp(Instrs0, FrameSize, Between0, Remain),
        Between = [Instr0 | Between0]
    ;
        Uinstr0 = decr_sp(FrameSize),
        Between = [],
        Remain = Instrs0
    ).

%---------------------------------------------------------------------------%
%
% Filtering instruction lists.
%

filter_out_labels([], []).
filter_out_labels([Instr0 | Instrs0], Instrs) :-
    filter_out_labels(Instrs0, Instrs1),
    ( if Instr0 = llds_instr(label(_), _) then
        Instrs = Instrs1
    else
        Instrs = [Instr0 | Instrs1]
    ).

filter_out_r1([], no, []).
filter_out_r1([Instr0 | Instrs0], Success, Instrs) :-
    filter_out_r1(Instrs0, Success0, Instrs1),
    ( if Instr0 = llds_instr(assign(reg(reg_r, 1), const(Success1)), _) then
        Instrs = Instrs1,
        Success = yes(Success1)
    else
        Instrs = [Instr0 | Instrs1],
        Success = Success0
    ).

filter_out_bad_livevals([], []).
filter_out_bad_livevals([Instr0 | Instrs0], Instrs) :-
    filter_out_bad_livevals(Instrs0, Instrs1),
    ( if
        Instr0 = llds_instr(livevals(_), _),
        skip_comments(Instrs1, Instrs2),
        Instrs2 = [llds_instr(Uinstr2, _) | _],
        can_use_livevals(Uinstr2, no)
    then
        Instrs = Instrs1
    else
        Instrs = [Instr0 | Instrs1]
    ).

filter_out_livevals([], []).
filter_out_livevals([Instr0 | Instrs0], Instrs) :-
    filter_out_livevals(Instrs0, Instrs1),
    ( if Instr0 = llds_instr(livevals(_), _) then
        Instrs = Instrs1
    else
        Instrs = [Instr0 | Instrs1]
    ).

filter_in_livevals([], []).
filter_in_livevals([Instr0 | Instrs0], Instrs) :-
    filter_in_livevals(Instrs0, Instrs1),
    ( if Instr0 = llds_instr(livevals(_), _) then
        Instrs = [Instr0 | Instrs1]
    else
        Instrs = Instrs1
    ).

%---------------------%

propagate_livevals(Instrs0, Instrs) :-
    list.reverse(Instrs0, RevInstrs0),
    set.init(Livevals),
    propagate_livevals_acc(RevInstrs0, Livevals, RevInstrs),
    list.reverse(RevInstrs, Instrs).

:- pred propagate_livevals_acc(list(instruction)::in, set(lval)::in,
    list(instruction)::out) is det.

propagate_livevals_acc([], _, []).
propagate_livevals_acc([Instr0 | Instrs0], Livevals0,
        [Instr | Instrs]) :-
    Instr0 = llds_instr(Uinstr0, Comment),
    ( if Uinstr0 = livevals(ThisLivevals) then
        set.union(Livevals0, ThisLivevals, Livevals),
        Instr = llds_instr(livevals(Livevals), Comment)
    else
        Instr = Instr0,
        ( if Uinstr0 = assign(Lval, _) then
            set.delete(Lval, Livevals0, Livevals)
        else if can_instr_fall_through(Uinstr0) = no then
            set.init(Livevals)
        else
            Livevals = Livevals0
        )
    ),
    propagate_livevals_acc(Instrs0, Livevals, Instrs).

%---------------------------------------------------------------------------%
%
% Simple tests.
%

touches_nondet_ctrl([]) = no.
touches_nondet_ctrl([llds_instr(Uinstr, _) | Instrs]) = !:Touch :-
    !:Touch = touches_nondet_ctrl_instr(Uinstr),
    (
        !.Touch = yes
    ;
        !.Touch = no,
        !:Touch = touches_nondet_ctrl(Instrs)
    ).

:- func touches_nondet_ctrl_instr(instr) = bool.

touches_nondet_ctrl_instr(Uinstr) = Touch :-
    (
        ( Uinstr = comment(_)
        ; Uinstr = livevals(_)
        ; Uinstr = label(_)
        ; Uinstr = prune_ticket
        ; Uinstr = discard_ticket
        ; Uinstr = incr_sp(_, _, _)
        ; Uinstr = decr_sp(_)
        ; Uinstr = decr_sp_and_return(_)
        ; Uinstr = push_region_frame(_, _)
        ; Uinstr = use_and_maybe_pop_region_frame(_, _)
        ),
        Touch = no
    ;
        ( Uinstr = mkframe(_, _)
        ; Uinstr = goto(_)
        ; Uinstr = computed_goto(_, _)
        ; Uinstr = llcall(_, _, _, _, _, _) % This is a safe approximation.
        ; Uinstr = if_val(_, _)
        ; Uinstr = arbitrary_c_code(_, _, _)
        ; Uinstr = save_maxfr(_)
        ; Uinstr = restore_maxfr(_)
        ; Uinstr = init_sync_term(_, _, _)  % This is a safe approximation.
        ; Uinstr = fork_new_child(_, _)     % This is a safe approximation.
        ; Uinstr = join_and_continue(_, _)  % This is a safe approximation.
        ),
        Touch = yes
    ;
        ( Uinstr = mark_hp(Lval)
        ; Uinstr = store_ticket(Lval)
        ; Uinstr = mark_ticket_stack(Lval)
        ; Uinstr = lc_create_loop_control(_, Lval)
        ),
        Touch = touches_nondet_ctrl_lval(Lval)
    ;
        ( Uinstr = restore_hp(Rval)
        ; Uinstr = free_heap(Rval)
        ; Uinstr = region_set_fixed_slot(_SetOp, _EmbeddedStackFrame, Rval)
        ; Uinstr = reset_ticket(Rval, _)
        ; Uinstr = prune_tickets_to(Rval)
        ),
        Touch = touches_nondet_ctrl_rval(Rval)
    ;
        ( Uinstr = assign(Lval, Rval)
        ; Uinstr = keep_assign(Lval, Rval)
        ; Uinstr = lc_wait_free_slot(Rval, Lval, _)
        ),
        TouchLval = touches_nondet_ctrl_lval(Lval),
        TouchRval = touches_nondet_ctrl_rval(Rval),
        bool.or(TouchLval, TouchRval, Touch)
    ;
        ( Uinstr = lc_spawn_off(LCRval, LCSRval, _)
        ; Uinstr = lc_join_and_terminate(LCRval, LCSRval)
        ),
        TouchLC = touches_nondet_ctrl_rval(LCRval),
        TouchLCS = touches_nondet_ctrl_rval(LCSRval),
        bool.or(TouchLC, TouchLCS, Touch)
    ;
        Uinstr = block(_, _, _),
        % Blocks aren't introduced until after the last user of this predicate.
        unexpected($pred, "block")
    ;
        Uinstr = incr_hp(Lval, _, _, Rval, _, _, MaybeRegionRval,
            MaybeReuse),
        some [!Touch] (
            !:Touch = bool.or(
                touches_nondet_ctrl_lval(Lval),
                touches_nondet_ctrl_rval(Rval)),
            (
                MaybeRegionRval = yes(RegionRval),
                bool.or(touches_nondet_ctrl_rval(RegionRval), !Touch)
            ;
                MaybeRegionRval = no
            ),
            (
                MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
                bool.or(touches_nondet_ctrl_rval(ReuseRval), !Touch),
                (
                    MaybeFlagLval = yes(FlagLval),
                    bool.or(touches_nondet_ctrl_lval(FlagLval), !Touch)
                ;
                    MaybeFlagLval = no
                )
            ;
                MaybeReuse = no_llds_reuse
            ),
            Touch = !.Touch
        )
    ;
        Uinstr = region_fill_frame(_FillOp, _EmbeddedStackFrame, IdRval,
            NumLval, AddrLval),
        Touch = bool.or(
            touches_nondet_ctrl_rval(IdRval),
            bool.or(
                touches_nondet_ctrl_lval(NumLval),
                touches_nondet_ctrl_lval(AddrLval)))
    ;
        Uinstr = foreign_proc_code(_, Components, _, _, _, _, _, _, _, _),
        Touch = touches_nondet_ctrl_components(Components)
    ).

:- func touches_nondet_ctrl_lval(lval) = bool.

touches_nondet_ctrl_lval(reg(_, _)) = no.
touches_nondet_ctrl_lval(stackvar(_)) = no.
touches_nondet_ctrl_lval(parent_stackvar(_)) = no.
touches_nondet_ctrl_lval(framevar(_)) = no.
touches_nondet_ctrl_lval(double_stackvar(_, _)) = no.
touches_nondet_ctrl_lval(succip) = no.
touches_nondet_ctrl_lval(maxfr) = yes.
touches_nondet_ctrl_lval(curfr) = yes.
touches_nondet_ctrl_lval(succfr_slot(_)) = yes.
touches_nondet_ctrl_lval(prevfr_slot(_)) = yes.
touches_nondet_ctrl_lval(redofr_slot(_)) = yes.
touches_nondet_ctrl_lval(redoip_slot(_)) = yes.
touches_nondet_ctrl_lval(succip_slot(_)) = yes.
touches_nondet_ctrl_lval(hp) = no.
touches_nondet_ctrl_lval(sp) = no.
touches_nondet_ctrl_lval(parent_sp) = no.
touches_nondet_ctrl_lval(field(_, Rval1, Rval2)) = Touch :-
    Touch1 = touches_nondet_ctrl_rval(Rval1),
    Touch2 = touches_nondet_ctrl_rval(Rval2),
    bool.or(Touch1, Touch2, Touch).
touches_nondet_ctrl_lval(lvar(_)) = no.
touches_nondet_ctrl_lval(temp(_, _)) = no.
touches_nondet_ctrl_lval(mem_ref(Rval)) =
    touches_nondet_ctrl_rval(Rval).
touches_nondet_ctrl_lval(global_var_ref(_)) = no.

:- func touches_nondet_ctrl_rval(rval) = bool.

touches_nondet_ctrl_rval(lval(Lval)) =
    touches_nondet_ctrl_lval(Lval).
touches_nondet_ctrl_rval(var(_)) = no.
touches_nondet_ctrl_rval(mkword(_, Rval)) =
    touches_nondet_ctrl_rval(Rval).
touches_nondet_ctrl_rval(mkword_hole(_)) = no.
touches_nondet_ctrl_rval(const(_)) = no.
touches_nondet_ctrl_rval(cast(_, Rval)) =
    touches_nondet_ctrl_rval(Rval).
touches_nondet_ctrl_rval(unop(_, Rval)) =
    touches_nondet_ctrl_rval(Rval).
touches_nondet_ctrl_rval(binop(_, Rval1, Rval2)) = Touch :-
    Touch1 = touches_nondet_ctrl_rval(Rval1),
    Touch2 = touches_nondet_ctrl_rval(Rval2),
    bool.or(Touch1, Touch2, Touch).
touches_nondet_ctrl_rval(mem_addr(MemRef)) =
    touches_nondet_ctrl_mem_ref(MemRef).

:- func touches_nondet_ctrl_mem_ref(mem_ref) = bool.

touches_nondet_ctrl_mem_ref(stackvar_ref(_)) = no.
touches_nondet_ctrl_mem_ref(framevar_ref(_)) = no.
touches_nondet_ctrl_mem_ref(heap_ref(Rval, _, _)) =
    touches_nondet_ctrl_rval(Rval).

:- func touches_nondet_ctrl_components(list(foreign_proc_component)) = bool.

touches_nondet_ctrl_components([]) = no.
touches_nondet_ctrl_components([Comp | Comps]) = Touch :-
    Touch1 = touches_nondet_ctrl_component(Comp),
    Touch2 = touches_nondet_ctrl_components(Comps),
    bool.or(Touch1, Touch2, Touch).

    % The inputs and outputs components get emitted as simple straight-line
    % code that do not refer to control slots. The compiler does not generate
    % raw_code that refers to control slots. User code shouldn't either, but
    % until we have prohibited the use of ordinary pragma C codes for model_non
    % procedures, some user code will need to ignore this restriction.
    %
:- func touches_nondet_ctrl_component(foreign_proc_component) = bool.

touches_nondet_ctrl_component(foreign_proc_inputs(_)) = no.
touches_nondet_ctrl_component(foreign_proc_outputs(_)) = no.
touches_nondet_ctrl_component(foreign_proc_raw_code(_, _, _, _)) = no.
touches_nondet_ctrl_component(foreign_proc_user_code(_, _, _)) = yes.
touches_nondet_ctrl_component(foreign_proc_fail_to(_)) = no.
touches_nondet_ctrl_component(foreign_proc_alloc_id(_)) = no.
touches_nondet_ctrl_component(foreign_proc_noop) = no.

%---------------------%

has_both_incr_decr_sp(Instrs) :-
    has_both_incr_decr_sp_acc(Instrs, no, yes, no, yes).

:- pred has_both_incr_decr_sp_acc(list(instruction)::in,
    bool::in, bool::out, bool::in, bool::out) is det.

has_both_incr_decr_sp_acc([], !HasIncr, !HasDecr).
has_both_incr_decr_sp_acc([llds_instr(Uinstr, _) | Instrs],
        !HasIncr, !HasDecr) :-
    ( if Uinstr = incr_sp(_, _, _) then
        !:HasIncr = yes
    else
        true
    ),
    ( if Uinstr = decr_sp(_) then
        !:HasDecr = yes
    else
        true
    ),
    has_both_incr_decr_sp_acc(Instrs, !HasIncr, !HasDecr).

%---------------------%

can_instr_branch_away(Uinstr) = CanBranchAway :-
    (
        ( Uinstr = comment(_)
        ; Uinstr = livevals(_)
        ; Uinstr = assign(_, _)
        ; Uinstr = keep_assign(_, _)
        ; Uinstr = mkframe(_, _)
        ; Uinstr = label(_)
        ; Uinstr = arbitrary_c_code(_, _, _)
        ; Uinstr = save_maxfr(_)
        ; Uinstr = restore_maxfr(_)
        ; Uinstr = incr_hp(_, _, _, _, _, _, _, _)
        ; Uinstr = mark_hp(_)
        ; Uinstr = restore_hp(_)
        ; Uinstr = free_heap(_)
        ; Uinstr = push_region_frame(_, _)
        ; Uinstr = region_fill_frame(_, _, _, _, _)
        ; Uinstr = region_set_fixed_slot(_, _, _)
        ; Uinstr = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr = store_ticket(_)
        ; Uinstr = reset_ticket(_, _)
        ; Uinstr = discard_ticket
        ; Uinstr = prune_ticket
        ; Uinstr = mark_ticket_stack(_)
        ; Uinstr = prune_tickets_to(_)
        ; Uinstr = incr_sp(_, _, _)
        ; Uinstr = decr_sp(_)
        ; Uinstr = init_sync_term(_, _, _)
        ; Uinstr = fork_new_child(_, _)
        ; Uinstr = lc_create_loop_control(_, _)
        ; Uinstr = lc_wait_free_slot(_, _, _)
        ; Uinstr = lc_join_and_terminate(_, _)
        ),
        CanBranchAway = no
    ;
        ( Uinstr = block(_, _, _)
        ; Uinstr = llcall(_, _, _, _, _, _)
        ; Uinstr = goto(_)
        ; Uinstr = computed_goto(_, _)
        ; Uinstr = if_val(_, _)
        ; Uinstr = decr_sp_and_return(_)
        ; Uinstr = join_and_continue(_, _)
        ; Uinstr = lc_spawn_off(_, _, _)
        ),
        CanBranchAway = yes
    ;
        Uinstr = foreign_proc_code(_, Comps, _, _, _, _, _, _, _, _),
        CanBranchAway = can_components_branch_away(Comps)
    ).

:- func can_components_branch_away(list(foreign_proc_component)) = bool.

can_components_branch_away([]) = no.
can_components_branch_away([Component | Components]) = !:BranchAway :-
    !:BranchAway = can_component_branch_away(Component),
    (
        !.BranchAway = yes
    ;
        !.BranchAway = no,
        !:BranchAway = can_components_branch_away(Components)
    ).

    % The input and output components get expanded to straight line code.
    % Some of the raw_code components we generate for nondet pragma C codes
    % invoke succeed(), which definitely does branch away.
    % Also the raw_code components for semidet pragma C codes can
    % branch to a label on failure.
    % User-written C code cannot branch away because users do not know
    % how to do that. (They can call other functions, but those functions
    % will return, so control will still go to the instruction following
    % this one. We the developers could write C code that branched away,
    % but we are careful to preserve a declarative interface, and that
    % is incompatible with branching away.)
    %
:- func can_component_branch_away(foreign_proc_component) = bool.

can_component_branch_away(foreign_proc_inputs(_)) = no.
can_component_branch_away(foreign_proc_outputs(_)) = no.
can_component_branch_away(foreign_proc_raw_code(CanBranchAway, _, _, _))
        = CanBranchAwayBool :-
    (
        CanBranchAway = can_branch_away,
        CanBranchAwayBool = yes
    ;
        CanBranchAway = cannot_branch_away,
        CanBranchAwayBool = no
    ).
can_component_branch_away(foreign_proc_user_code(_, _, _)) = no.
can_component_branch_away(foreign_proc_fail_to(_)) = yes.
can_component_branch_away(foreign_proc_alloc_id(_)) = no.
can_component_branch_away(foreign_proc_noop) = no.

%---------------------%

can_instr_fall_through(comment(_)) = yes.
can_instr_fall_through(livevals(_)) = yes.
can_instr_fall_through(block(_, _, Instrs)) = FallThrough :-
    can_block_fall_through(Instrs, FallThrough).
can_instr_fall_through(assign(_, _)) = yes.
can_instr_fall_through(keep_assign(_, _)) = yes.
can_instr_fall_through(llcall(_, _, _, _, _, _)) = no.
can_instr_fall_through(mkframe(_, _)) = yes.
can_instr_fall_through(label(_)) = yes.
can_instr_fall_through(goto(_)) = no.
can_instr_fall_through(computed_goto(_, _)) = no.
can_instr_fall_through(arbitrary_c_code(_, _, _)) = yes.
can_instr_fall_through(if_val(_, _)) = yes.
can_instr_fall_through(save_maxfr(_)) = yes.
can_instr_fall_through(restore_maxfr(_)) = yes.
can_instr_fall_through(incr_hp(_, _, _, _, _, _, _, _)) = yes.
can_instr_fall_through(mark_hp(_)) = yes.
can_instr_fall_through(restore_hp(_)) = yes.
can_instr_fall_through(free_heap(_)) = yes.
can_instr_fall_through(push_region_frame(_, _)) = yes.
can_instr_fall_through(region_fill_frame(_, _, _, _, _)) = yes.
can_instr_fall_through(region_set_fixed_slot(_, _, _)) = yes.
can_instr_fall_through(use_and_maybe_pop_region_frame(_, _)) = yes.
can_instr_fall_through(store_ticket(_)) = yes.
can_instr_fall_through(reset_ticket(_, _)) = yes.
can_instr_fall_through(discard_ticket) = yes.
can_instr_fall_through(prune_ticket) = yes.
can_instr_fall_through(mark_ticket_stack(_)) = yes.
can_instr_fall_through(prune_tickets_to(_)) = yes.
can_instr_fall_through(incr_sp(_, _, _)) = yes.
can_instr_fall_through(decr_sp(_)) = yes.
can_instr_fall_through(decr_sp_and_return(_)) = no.
can_instr_fall_through(foreign_proc_code(_, _, _, _, _, _, _, _, _, _)) = yes.
can_instr_fall_through(init_sync_term(_, _, _)) = yes.
can_instr_fall_through(fork_new_child(_, _)) = yes.
can_instr_fall_through(join_and_continue(_, _)) = no.
can_instr_fall_through(lc_create_loop_control(_, _)) = yes.
can_instr_fall_through(lc_wait_free_slot(_, _, _)) = yes.
can_instr_fall_through(lc_spawn_off(_, _, _)) = yes.
can_instr_fall_through(lc_join_and_terminate(_, _)) = no.

    % Check whether an instruction sequence can possibly fall through
    % to the next instruction without using its label.
    %
:- pred can_block_fall_through(list(instruction)::in, bool::out) is det.

can_block_fall_through([], yes).
can_block_fall_through([llds_instr(Uinstr, _) | Instrs], FallThrough) :-
    ( if can_instr_fall_through(Uinstr) = no then
        FallThrough = no
    else
        can_block_fall_through(Instrs, FallThrough)
    ).

:- pred can_use_livevals(instr::in, bool::out) is det.

can_use_livevals(comment(_), no).
can_use_livevals(livevals(_), no).
can_use_livevals(block(_, _, _), no).
can_use_livevals(assign(_, _), no).
can_use_livevals(keep_assign(_, _), no).
can_use_livevals(llcall(_, _, _, _, _, _), yes).
can_use_livevals(mkframe(_, _), no).
can_use_livevals(label(_), no).
can_use_livevals(goto(_), yes).
can_use_livevals(computed_goto(_, _), no).
can_use_livevals(arbitrary_c_code(_, _, _), no).
can_use_livevals(if_val(_, _), yes).
can_use_livevals(save_maxfr(_), no).
can_use_livevals(restore_maxfr(_), no).
can_use_livevals(incr_hp(_, _, _, _, _, _, _, _), no).
can_use_livevals(mark_hp(_), no).
can_use_livevals(restore_hp(_), no).
can_use_livevals(free_heap(_), no).
can_use_livevals(push_region_frame(_, _), no).
can_use_livevals(region_fill_frame(_, _, _, _, _), no).
can_use_livevals(region_set_fixed_slot(_, _, _), no).
can_use_livevals(use_and_maybe_pop_region_frame(_, _), no).
can_use_livevals(store_ticket(_), no).
can_use_livevals(reset_ticket(_, _), no).
can_use_livevals(discard_ticket, no).
can_use_livevals(prune_ticket, no).
can_use_livevals(mark_ticket_stack(_), no).
can_use_livevals(prune_tickets_to(_), no).
can_use_livevals(incr_sp(_, _, _), no).
can_use_livevals(decr_sp(_), no).
can_use_livevals(decr_sp_and_return(_), yes).
can_use_livevals(foreign_proc_code(_, _, _, _, _, _, _, _, _, _), no).
can_use_livevals(init_sync_term(_, _, _), no).
can_use_livevals(fork_new_child(_, _), no).
can_use_livevals(join_and_continue(_, _), no).
can_use_livevals(lc_create_loop_control(_, _), no).
can_use_livevals(lc_wait_free_slot(_, _, _), no).
can_use_livevals(lc_spawn_off(_, _, _), yes).
can_use_livevals(lc_join_and_terminate(_, _), no).

%---------------------%

is_const_condition(TestRval, Taken) :-
    % We recognize only a subset of all constant conditions.
    % The time to extend this predicate is when the rest of the compiler
    % generates more complicated constant conditions.
    (
        TestRval = const(Const),
        ( if Const = llconst_true then
            Taken = yes
        else if Const = llconst_false then
            Taken = no
        else
            unexpected($pred, "non-boolean constant as if-then-else condition")
        )
    ;
        TestRval = unop(Op, SubRvalA),
        Op = logical_not,
        is_const_condition(SubRvalA, SubTaken),
        bool.not(SubTaken, Taken)
    ;
        TestRval = binop(Op, SubRvalA, SubRvalB),
        Op = eq(_),
        SubRvalA = SubRvalB,
        Taken = yes
    ).

%---------------------%

livevals_addr(code_label(Label)) = Result :-
    (
        Label = internal_label(_, _),
        Result = no
    ;
        Label = entry_label(_, _),
        Result = yes
    ).
livevals_addr(code_imported_proc(_)) = yes.
livevals_addr(code_succip) = yes.
livevals_addr(do_succeed(_)) = yes.
livevals_addr(do_redo) = no.
livevals_addr(do_fail) = no.
livevals_addr(do_trace_redo_fail_shallow) = no.
livevals_addr(do_trace_redo_fail_deep) = no.
livevals_addr(do_call_closure(_)) = yes.
livevals_addr(do_call_class_method(_)) = yes.
livevals_addr(do_not_reached) = no.

%---------------------------------------------------------------------------%
%
% Counting.
%

count_temps_instr_list([], !R, !F).
count_temps_instr_list([llds_instr(Uinstr, _Comment) | Instrs], !R, !F) :-
    count_temps_instr(Uinstr, !R, !F),
    count_temps_instr_list(Instrs, !R, !F).

count_temps_instr(comment(_), !R, !F).
count_temps_instr(livevals(_), !R, !F).
count_temps_instr(block(_, _, _), !R, !F).
count_temps_instr(assign(Lval, Rval), !R, !F) :-
    count_temps_lval(Lval, !R, !F),
    count_temps_rval(Rval, !R, !F).
count_temps_instr(keep_assign(Lval, Rval), !R, !F) :-
    count_temps_lval(Lval, !R, !F),
    count_temps_rval(Rval, !R, !F).
count_temps_instr(llcall(_, _, _, _, _, _), !R, !F).
count_temps_instr(mkframe(_, _), !R, !F).
count_temps_instr(label(_), !R, !F).
count_temps_instr(goto(_), !R, !F).
count_temps_instr(computed_goto(Rval, _), !R, !F) :-
    count_temps_rval(Rval, !R, !F).
count_temps_instr(if_val(Rval, _), !R, !F) :-
    count_temps_rval(Rval, !R, !F).
count_temps_instr(arbitrary_c_code(_, _, _), !R, !F).
count_temps_instr(save_maxfr(Lval), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(restore_maxfr(Lval), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(incr_hp(Lval, _, _, Rval, _, _, MaybeRegionRval,
        MaybeReuse), !R, !F) :-
    count_temps_lval(Lval, !R, !F),
    count_temps_rval(Rval, !R, !F),
    (
        MaybeRegionRval = yes(RegionRval),
        count_temps_rval(RegionRval, !R, !F)
    ;
        MaybeRegionRval = no
    ),
    (
        MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
        count_temps_rval(ReuseRval, !R, !F),
        (
            MaybeFlagLval = yes(FlagLval),
            count_temps_lval(FlagLval, !R, !F)
        ;
            MaybeFlagLval = no
        )
    ;
        MaybeReuse = no_llds_reuse
    ).
count_temps_instr(mark_hp(Lval), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(restore_hp(Rval), !R, !F) :-
    count_temps_rval(Rval, !R, !F).
count_temps_instr(free_heap(Rval), !R, !F) :-
    count_temps_rval(Rval, !R, !F).
count_temps_instr(push_region_frame(_StackId, _EmbeddedStackFrame), !R, !F).
count_temps_instr(region_fill_frame(_FillOp, _EmbeddedStackFrame, IdRval,
        NumLval, AddrLval), !R, !F) :-
    count_temps_rval(IdRval, !R, !F),
    count_temps_lval(NumLval, !R, !F),
    count_temps_lval(AddrLval, !R, !F).
count_temps_instr(region_set_fixed_slot(_SetlOp, _EmbeddedStackFrame,
        ValueRval), !R, !F) :-
    count_temps_rval(ValueRval, !R, !F).
count_temps_instr(use_and_maybe_pop_region_frame(_UseOp, _EmbeddedStackFrame),
        !R, !F).
count_temps_instr(store_ticket(Lval), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(reset_ticket(Rval, _Reason), !R, !F) :-
    count_temps_rval(Rval, !R, !F).
count_temps_instr(discard_ticket, !R, !F).
count_temps_instr(prune_ticket, !R, !F).
count_temps_instr(mark_ticket_stack(Lval), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(prune_tickets_to(Rval), !R, !F) :-
    count_temps_rval(Rval, !R, !F).
count_temps_instr(incr_sp(_, _, _), !R, !F).
count_temps_instr(decr_sp(_), !R, !F).
count_temps_instr(decr_sp_and_return(_), !R, !F).
count_temps_instr(foreign_proc_code(_, Comps, _, _, _, _, _, _, _, _),
        !R, !F) :-
    count_temps_components(Comps, !R, !F).
count_temps_instr(init_sync_term(Lval, _, _), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(fork_new_child(Lval, _), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(join_and_continue(Lval, _), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(lc_create_loop_control(_, Lval), !R, !F) :-
    count_temps_lval(Lval, !R, !F).
count_temps_instr(lc_wait_free_slot(Rval, Lval, _), !R, !F) :-
    count_temps_rval(Rval, !R, !F),
    count_temps_lval(Lval, !R, !F).
count_temps_instr(lc_spawn_off(LCRval, LCSRval, _), !R, !F) :-
    count_temps_rval(LCRval, !R, !F),
    count_temps_rval(LCSRval, !R, !F).
count_temps_instr(lc_join_and_terminate(LCRval, LCSRval), !R, !F) :-
    count_temps_rval(LCRval, !R, !F),
    count_temps_rval(LCSRval, !R, !F).

:- pred count_temps_components(list(foreign_proc_component)::in,
    int::in, int::out, int::in, int::out) is det.

count_temps_components([], !R, !F).
count_temps_components([Comp | Comps], !R, !F) :-
    count_temps_component(Comp, !R, !F),
    count_temps_components(Comps, !R, !F).

:- pred count_temps_component(foreign_proc_component::in,
    int::in, int::out, int::in, int::out) is det.

count_temps_component(Comp, !R, !F) :-
    (
        Comp = foreign_proc_inputs(Inputs),
        count_temps_inputs(Inputs, !R, !F)
    ;
        Comp = foreign_proc_outputs(Outputs),
        count_temps_outputs(Outputs, !R, !F)
    ;
        ( Comp = foreign_proc_user_code(_, _, _)
        ; Comp = foreign_proc_raw_code(_, _, _, _)
        ; Comp = foreign_proc_fail_to(_)
        ; Comp = foreign_proc_alloc_id(_)
        ; Comp = foreign_proc_noop
        )
    ).

:- pred count_temps_inputs(list(foreign_proc_input)::in,
    int::in, int::out, int::in, int::out) is det.

count_temps_inputs([], !R, !F).
count_temps_inputs([Input | Inputs], !R, !F) :-
    Input = foreign_proc_input(_VarName, _VarType, _IsDummy, _OrigType,
        ArgRval, _MaybeForeignType, _BoxPolicy),
    count_temps_rval(ArgRval, !R, !F),
    count_temps_inputs(Inputs, !R, !F).

:- pred count_temps_outputs(list(foreign_proc_output)::in,
    int::in, int::out, int::in, int::out) is det.

count_temps_outputs([], !R, !F).
count_temps_outputs([Output | Outputs], !R, !F) :-
    Output = foreign_proc_output(DestLval, _VarType, _IsDummy, _OrigType,
        _VarName, _MaybeForeignType, _BoxPolicy),
    count_temps_lval(DestLval, !R, !F),
    count_temps_outputs(Outputs, !R, !F).

:- pred count_temps_lval(lval::in, int::in, int::out, int::in, int::out)
    is det.

count_temps_lval(Lval, !R, !F) :-
    (
        ( Lval = reg(_, _)
        ; Lval = succip
        ; Lval = maxfr
        ; Lval = curfr
        ; Lval = hp
        ; Lval = sp
        ; Lval = parent_sp
        ; Lval = stackvar(_)
        ; Lval = framevar(_)
        ; Lval = parent_stackvar(_)
        ; Lval = double_stackvar(_, _)
        ; Lval = global_var_ref(_)
        )
    ;
        Lval = temp(reg_r, N),
        int.max(N, !R)
    ;
        Lval = temp(reg_f, N),
        int.max(N, !F)
    ;
        Lval = field(_, BaseAddrRval, FieldNumRval),
        count_temps_rval(BaseAddrRval, !R, !F),
        count_temps_rval(FieldNumRval, !R, !F)
    ;
        ( Lval = succip_slot(Rval)
        ; Lval = succfr_slot(Rval)
        ; Lval = redoip_slot(Rval)
        ; Lval = redofr_slot(Rval)
        ; Lval = prevfr_slot(Rval)
        ; Lval = mem_ref(Rval)
        ),
        count_temps_rval(Rval, !R, !F)
    ;
        Lval = lvar(_),
        unexpected($pred, "lvar")
    ).

:- pred count_temps_rval(rval::in, int::in, int::out, int::in, int::out)
    is det.

count_temps_rval(Rval, !R, !F) :-
    (
        Rval = lval(Lval),
        count_temps_lval(Lval, !R, !F)
    ;
        Rval = var(_),
        unexpected($pred, "var")
    ;
        Rval = mkword_hole(_Tag)
    ;
        Rval = const(_Const)
    ;
        ( Rval = mkword(_Tag, SubRval)
        ; Rval = cast(_Type, SubRval)
        ; Rval = unop(_Unop, SubRval)
        ),
        count_temps_rval(SubRval, !R, !F)
    ;
        Rval = binop(_Binop, SubRvalA, SubRvalB),
        count_temps_rval(SubRvalA, !R, !F),
        count_temps_rval(SubRvalB, !R, !F)
    ;
        Rval = mem_addr(MemRef),
        count_temps_mem_ref(MemRef, !R, !F)
    ).

:- pred count_temps_mem_ref(mem_ref::in, int::in, int::out, int::in, int::out)
    is det.

count_temps_mem_ref(MemRef, !R, !F) :-
    (
        ( MemRef = stackvar_ref(Rval)
        ; MemRef = framevar_ref(Rval)
        ),
        count_temps_rval(Rval, !R, !F)
    ;
        MemRef = heap_ref(CellRval, _MaybeTag, FieldNumRval),
        count_temps_rval(CellRval, !R, !F),
        count_temps_rval(FieldNumRval, !R, !F)
    ).

%---------------------%

count_incr_hp(Instrs, N) :-
    count_incr_hp_acc(Instrs, 0, N).

:- pred count_incr_hp_acc(list(instruction)::in, int::in, int::out) is det.

count_incr_hp_acc([], !N).
count_incr_hp_acc([llds_instr(Uinstr0, _) | Instrs], !N) :-
    ( if Uinstr0 = incr_hp(_, _, _, _, _, _, _, _) then
        !:N = !.N + 1
    else
        true
    ),
    count_incr_hp_acc(Instrs, !N).

%---------------------------------------------------------------------------%
%
% Finding possible jump targets.
%

instr_labels(Instr, Labels, CodeAddrs) :-
    instr_labels_2(Instr, Labels0, CodeAddrs1),
    instr_rvals_and_lvals(Instr, Rvals, Lvals),
    exprn_aux.rval_list_addrs(to_sorted_list(Rvals), CodeAddrs2, _),
    exprn_aux.lval_list_addrs(to_sorted_list(Lvals), CodeAddrs3, _),
    CodeAddrs = CodeAddrs1 ++ CodeAddrs2 ++ CodeAddrs3,
    find_label_code_addrs(CodeAddrs, Labels0, Labels).

instr_labels_only(Instr, Labels) :-
    instr_labels(Instr, Labels, _CodeAddrs).

    % Determine all the labels and code_addresses that are directly referenced
    % by an instruction (not counting ones referenced indirectly via rvals or
    % lvals).
    %
:- pred instr_labels_2(instr::in, list(label)::out, list(code_addr)::out)
    is det.

instr_labels_2(Uinstr, Labels, CodeAddrs) :-
    (
        ( Uinstr = comment(_)
        ; Uinstr = livevals(_)
        ; Uinstr = assign(_,_)
        ; Uinstr = keep_assign(_,_)
        ; Uinstr = mkframe(_, no)
        ; Uinstr = label(_)
        ; Uinstr = arbitrary_c_code(_, _, _)
        ; Uinstr = save_maxfr(_)
        ; Uinstr = restore_maxfr(_)
        ; Uinstr = incr_hp(_, _, _, _, _, _, _, _)
        ; Uinstr = mark_hp(_)
        ; Uinstr = restore_hp(_)
        ; Uinstr = free_heap(_)
        ; Uinstr = push_region_frame(_, _)
        ; Uinstr = region_fill_frame(_, _, _, _, _)
        ; Uinstr = region_set_fixed_slot(_, _, _)
        ; Uinstr = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr = store_ticket(_)
        ; Uinstr = reset_ticket(_, _)
        ; Uinstr = discard_ticket
        ; Uinstr = prune_ticket
        ; Uinstr = mark_ticket_stack(_)
        ; Uinstr = prune_tickets_to(_)
        ; Uinstr = incr_sp(_, _, _)
        ; Uinstr = decr_sp(_)
        ; Uinstr = init_sync_term(_, _, _)
        ; Uinstr = lc_create_loop_control(_, _)
        ; Uinstr = lc_join_and_terminate(_, _)
        ),
        Labels = [],
        CodeAddrs = []
    ;
        Uinstr = llcall(Target, Ret, _, _, _, _),
        Labels = [],
        CodeAddrs = [Target, Ret]
    ;
        ( Uinstr = mkframe(_, yes(Addr))
        ; Uinstr = goto(Addr)
        ; Uinstr = if_val(_, Addr)
        ),
        Labels = [],
        CodeAddrs = [Addr]
    ;
        Uinstr = decr_sp_and_return(_),
        % XXX decr_sp_and_return does refer to a code addr, but the code addr
        % it refers to is the original succip (now in a stack slot), which is
        % not necessarily the current succip. However, we introduce
        % decr_sp_and_return so late that this predicate should never be
        % invoked on such instructions.
        unexpected($pred, "decr_sp_and_return")
    ;
        Uinstr = fork_new_child(_, Child),
        Labels = [Child],
        CodeAddrs = []
    ;
        ( Uinstr = join_and_continue(_, Label)
        ; Uinstr = lc_wait_free_slot(_, _, Label)
        ; Uinstr = lc_spawn_off(_, _, Label)
        ),
        Labels = [Label],
        CodeAddrs = []
    ;
        Uinstr = block(_, _, Instrs),
        instr_list_labels(Instrs, Labels, CodeAddrs)
    ;
        Uinstr = computed_goto(_, MaybeLabels),
        possible_targets_maybe_labels(MaybeLabels, [], RevLabels),
        list.reverse(RevLabels, Labels),
        CodeAddrs = []
    ;
        Uinstr = foreign_proc_code(_, _, _, MaybeFixLabel, MaybeLayoutLabel,
            MaybeOnlyLayoutLabel, MaybeSubLabel, MaybeDefLabel, _, _),
        foreign_proc_labels(MaybeFixLabel, MaybeLayoutLabel,
            MaybeOnlyLayoutLabel, MaybeSubLabel, MaybeDefLabel, Labels),
        CodeAddrs = []
    ).

    % Find out which code addresses are also labels.
    %
:- pred find_label_code_addrs(list(code_addr)::in,
    list(label)::in, list(label)::out) is det.

find_label_code_addrs([], Labels, Labels).
find_label_code_addrs([CodeAddr | Rest], Labels0, Labels) :-
    ( if CodeAddr = code_label(Label) then
        Labels1 = [Label | Labels0]
    else
        Labels1 = Labels0
    ),
    find_label_code_addrs(Rest, Labels1, Labels).

possible_targets(Uinstr, Labels, CodeAddrs) :-
    (
        ( Uinstr = comment(_)
        ; Uinstr = livevals(_)
        ; Uinstr = assign(_,_)
        ; Uinstr = keep_assign(_,_)
        ; Uinstr = mkframe(_, _)
        ; Uinstr = label(_)
        ; Uinstr = arbitrary_c_code(_, _, _)
        ; Uinstr = save_maxfr(_)
        ; Uinstr = restore_maxfr(_)
        ; Uinstr = incr_hp(_, _, _, _, _, _, _, _)
        ; Uinstr = mark_hp(_)
        ; Uinstr = restore_hp(_)
        ; Uinstr = free_heap(_)
        ; Uinstr = push_region_frame(_, _)
        ; Uinstr = region_fill_frame(_, _, _, _, _)
        ; Uinstr = region_set_fixed_slot(_, _, _)
        ; Uinstr = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr = store_ticket(_)
        ; Uinstr = reset_ticket(_, _)
        ; Uinstr = discard_ticket
        ; Uinstr = prune_ticket
        ; Uinstr = mark_ticket_stack(_)
        ; Uinstr = prune_tickets_to(_)
        ; Uinstr = incr_sp(_, _, _)
        ; Uinstr = decr_sp(_)
        ; Uinstr = init_sync_term(_, _, _)
        ; Uinstr = fork_new_child(_, _)
        ; Uinstr = lc_create_loop_control(_, _)
        ; Uinstr = lc_join_and_terminate(_, _)
        ; Uinstr = lc_wait_free_slot(_, _, _Label)
        % The label in an lc_wait_free_slot instruction is NOT the possible
        % target of a branch.
        ),
        Labels = [],
        CodeAddrs = []
    ;
        Uinstr = llcall(_, Return, _, _, _, _),
        ( if Return = code_label(ReturnLabel) then
            Labels = [ReturnLabel],
            CodeAddrs = []
        else
            Labels = [],
            CodeAddrs = [Return]
        )
    ;
        ( Uinstr = goto(CodeAddr)
        ; Uinstr = if_val(_, CodeAddr)
        ),
        ( if CodeAddr = code_label(Label) then
            Labels = [Label],
            CodeAddrs = []
        else
            Labels = [],
            CodeAddrs = [CodeAddr]
        )
    ;
        Uinstr = decr_sp_and_return(_),
        % XXX see the comment in instr_labels_2.
        unexpected($pred, "decr_sp_and_return")
    ;
        ( Uinstr = join_and_continue(_, Label)
        ; Uinstr = lc_spawn_off(_, _, Label)
        ),
        Labels = [Label],
        CodeAddrs = []
    ;
        Uinstr = block(_, _, _),
        unexpected($pred, "block")
    ;
        Uinstr = computed_goto(_, MaybeLabels),
        possible_targets_maybe_labels(MaybeLabels, [], RevLabels),
        list.reverse(RevLabels, Labels),
        CodeAddrs = []
    ;
        Uinstr = foreign_proc_code(_, _, _, MaybeFixLabel, MaybeLayoutLabel,
            MaybeOnlyLayoutLabel, MaybeSubLabel, MaybeDefLabel, _, _),
        foreign_proc_labels(MaybeFixLabel, MaybeLayoutLabel,
            MaybeOnlyLayoutLabel, MaybeSubLabel, MaybeDefLabel, Labels),
        CodeAddrs = []
    ).

:- pred possible_targets_maybe_labels(list(maybe(label))::in,
    list(label)::in, list(label)::out) is det.

possible_targets_maybe_labels([], !RevLabels).
possible_targets_maybe_labels([MaybeLabel | MaybeLabels], !RevLabels) :-
    (
        MaybeLabel = yes(Label),
        !:RevLabels = [Label | !.RevLabels]
    ;
        MaybeLabel = no
    ),
    possible_targets_maybe_labels(MaybeLabels, !RevLabels).

:- pred foreign_proc_labels(maybe(label)::in, maybe(label)::in,
    maybe(label)::in, maybe(label)::in, maybe(label)::in, list(label)::out)
    is det.

foreign_proc_labels(MaybeFixedLabel, MaybeLayoutLabel,
        MaybeOnlyLayoutLabel, MaybeSubLabel, MaybeDefLabel, !:Labels) :-
    !:Labels = [],
    (
        MaybeFixedLabel = yes(FixedLabel),
        !:Labels = [FixedLabel | !.Labels]
    ;
        MaybeFixedLabel = no
    ),
    (
        MaybeLayoutLabel = yes(LayoutLabel),
        !:Labels = [LayoutLabel | !.Labels]
    ;
        MaybeLayoutLabel = no
    ),
    (
        MaybeOnlyLayoutLabel = yes(OnlyLayoutLabel),
        !:Labels = [OnlyLayoutLabel | !.Labels]
    ;
        MaybeOnlyLayoutLabel = no
    ),
    (
        MaybeSubLabel = yes(SubLabel),
        !:Labels = [SubLabel | !.Labels]
    ;
        MaybeSubLabel = no
    ),
    (
        MaybeDefLabel = yes(DefLabel),
        !:Labels = [DefLabel | !.Labels]
    ;
        MaybeDefLabel = no
    ).

    % Determine all the labels and code addresses which are referenced
    % by a list of instructions.
    %
:- pred instr_list_labels(list(instruction)::in,
    list(label)::out, list(code_addr)::out) is det.

instr_list_labels([], [], []).
instr_list_labels([llds_instr(Uinstr, _) | Instrs], Labels, CodeAddrs) :-
    instr_labels(Uinstr, HeadLabels, HeadCodeAddrs),
    instr_list_labels(Instrs, TailLabels, TailCodeAddrs),
    Labels = HeadLabels ++ TailLabels,
    CodeAddrs = HeadCodeAddrs ++ TailCodeAddrs.

%---------------------------------------------------------------------------%
%
% Finding stack references.
%

lval_refers_stackvars(reg(_, _)) = no.
lval_refers_stackvars(stackvar(_)) = yes.
lval_refers_stackvars(parent_stackvar(_)) = yes.
lval_refers_stackvars(framevar(_)) = yes.
lval_refers_stackvars(double_stackvar(_, _)) = yes.
lval_refers_stackvars(succip) = no.
lval_refers_stackvars(maxfr) = no.
lval_refers_stackvars(curfr) = no.
lval_refers_stackvars(succfr_slot(_)) = yes.
lval_refers_stackvars(prevfr_slot(_)) = yes.
lval_refers_stackvars(redofr_slot(_)) = yes.
lval_refers_stackvars(redoip_slot(_)) = yes.
lval_refers_stackvars(succip_slot(_)) = yes.
lval_refers_stackvars(hp) = no.
lval_refers_stackvars(sp) = no.
lval_refers_stackvars(parent_sp) = no.
lval_refers_stackvars(field(_, Rval, FieldNum)) =
    bool.or(
        rval_refers_stackvars(Rval),
        rval_refers_stackvars(FieldNum)).
lval_refers_stackvars(lvar(_)) = _ :-
    unexpected($pred, "lvar").
lval_refers_stackvars(temp(_, _)) = no.
lval_refers_stackvars(mem_ref(Rval)) =
    rval_refers_stackvars(Rval).
lval_refers_stackvars(global_var_ref(_)) = no.

:- func mem_ref_refers_stackvars(mem_ref) = bool.

mem_ref_refers_stackvars(stackvar_ref(_)) = yes.
mem_ref_refers_stackvars(framevar_ref(_)) = yes.
mem_ref_refers_stackvars(heap_ref(Rval1, _, Rval2)) =
    bool.or(rval_refers_stackvars(Rval1), rval_refers_stackvars(Rval2)).

rval_refers_stackvars(lval(Lval)) =
    lval_refers_stackvars(Lval).
rval_refers_stackvars(var(_)) = _ :-
    unexpected($pred, "var").
rval_refers_stackvars(mkword(_, Rval)) =
    rval_refers_stackvars(Rval).
rval_refers_stackvars(mkword_hole(_)) = no.
rval_refers_stackvars(const(_)) = no.
rval_refers_stackvars(cast(_, Rval)) =
    rval_refers_stackvars(Rval).
rval_refers_stackvars(unop(_, Rval)) =
    rval_refers_stackvars(Rval).
rval_refers_stackvars(binop(_, Rval1, Rval2)) =
    bool.or(rval_refers_stackvars(Rval1), rval_refers_stackvars(Rval2)).
rval_refers_stackvars(mem_addr(MemRef)) =
    mem_ref_refers_stackvars(MemRef).

:- func code_addr_refers_to_stack(code_addr) = bool.

code_addr_refers_to_stack(code_label(_)) = no.
code_addr_refers_to_stack(code_imported_proc(_)) = no.
code_addr_refers_to_stack(code_succip) = no.
code_addr_refers_to_stack(do_succeed(_)) = yes.
code_addr_refers_to_stack(do_redo) = yes.
code_addr_refers_to_stack(do_fail) = yes.
code_addr_refers_to_stack(do_trace_redo_fail_shallow) = yes.
code_addr_refers_to_stack(do_trace_redo_fail_deep) = yes.
code_addr_refers_to_stack(do_call_closure(_)) = no.
code_addr_refers_to_stack(do_call_class_method(_)) = no.
code_addr_refers_to_stack(do_not_reached) = no.

instr_refers_to_stack(llds_instr(Uinstr, _)) = Refers :-
    (
        ( Uinstr = comment(_)
        ; Uinstr = livevals(_)
        ; Uinstr = label(_)
        ; Uinstr = arbitrary_c_code(_, _, _)
        ; Uinstr = discard_ticket
        ; Uinstr = prune_ticket
        ; Uinstr = lc_join_and_terminate(_, _)
        ),
        Refers = no
    ;
        ( Uinstr = llcall(_, _, _, _, _, _)
        ; Uinstr = mkframe(_, _)
        ; Uinstr = push_region_frame(_, _)
        ; Uinstr = region_fill_frame(_, _, _, _, _)
        ; Uinstr = region_set_fixed_slot(_, _, _)
        ; Uinstr = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr = incr_sp(_, _, _)
        ; Uinstr = decr_sp(_)
        ; Uinstr = decr_sp_and_return(_)
        ; Uinstr = init_sync_term(_, _, _)
        ; Uinstr = fork_new_child(_, _)
        ; Uinstr = join_and_continue(_, _)
        ; Uinstr = lc_spawn_off(_, _, _)
        ),
        Refers = yes
    ;
        Uinstr = block(_, _, BlockInstrs),
        Refers = block_refers_to_stack(BlockInstrs)
    ;
        ( Uinstr = assign(Lval, Rval)
        ; Uinstr = keep_assign(Lval, Rval)
        ),
        Refers = bool.or(
            lval_refers_stackvars(Lval),
            rval_refers_stackvars(Rval))
    ;
        Uinstr = goto(CodeAddr),
        Refers = code_addr_refers_to_stack(CodeAddr)
    ;
        Uinstr = if_val(Rval, CodeAddr),
        Refers = bool.or(
            rval_refers_stackvars(Rval),
            code_addr_refers_to_stack(CodeAddr))
    ;
        ( Uinstr = save_maxfr(Lval)
        ; Uinstr = restore_maxfr(Lval)
        ; Uinstr = mark_hp(Lval)
        ; Uinstr = store_ticket(Lval)
        ; Uinstr = mark_ticket_stack(Lval)
        ; Uinstr = lc_create_loop_control(_, Lval)
        ),
        Refers = lval_refers_stackvars(Lval)
    ;
        ( Uinstr = computed_goto(Rval, _Labels)
        ; Uinstr = restore_hp(Rval)
        ; Uinstr = free_heap(Rval)
        ; Uinstr = reset_ticket(Rval, _Reason)
        ; Uinstr = prune_tickets_to(Rval)
        ),
        Refers = rval_refers_stackvars(Rval)
    ;
        Uinstr = incr_hp(Lval, _, _, Rval, _, _, MaybeRegionRval,
            MaybeReuse),
        some [!Refers] (
            !:Refers = bool.or(
                lval_refers_stackvars(Lval),
                rval_refers_stackvars(Rval)),
            (
                MaybeRegionRval = yes(RegionRval),
                bool.or(rval_refers_stackvars(RegionRval), !Refers)
            ;
                MaybeRegionRval = no
            ),
            (
                MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
                bool.or(rval_refers_stackvars(ReuseRval), !Refers),
                (
                    MaybeFlagLval = yes(FlagLval),
                    bool.or(lval_refers_stackvars(FlagLval), !Refers)
                ;
                    MaybeFlagLval = no
                )
            ;
                MaybeReuse = no_llds_reuse
            ),
            Refers = !.Refers
        )
    ;
        Uinstr = foreign_proc_code(_, Components, _, _, _, _, _, _, _, _),
        Refers = bool.or_list(list.map(foreign_proc_component_refers_stackvars,
            Components))
    ;
        Uinstr = lc_wait_free_slot(Rval, Lval, _),
        Refers = bool.or(
            rval_refers_stackvars(Rval),
            lval_refers_stackvars(Lval))
    ).

:- func foreign_proc_component_refers_stackvars(foreign_proc_component) = bool.

foreign_proc_component_refers_stackvars(Component) = Refers :-
    (
        Component = foreign_proc_inputs(Inputs),
        bool.or_list(list.map(foreign_proc_input_refers_stackvars, Inputs),
            Refers)
    ;
        Component = foreign_proc_outputs(Outputs),
        bool.or_list(list.map(foreign_proc_output_refers_stackvars, Outputs),
            Refers)
    ;
        ( Component = foreign_proc_user_code(_, _, _)
        ; Component = foreign_proc_raw_code(_, _, _, _)
        ; Component = foreign_proc_fail_to(_)
        ; Component = foreign_proc_alloc_id(_)
        ; Component = foreign_proc_noop
        ),
        Refers = no
    ).

:- func foreign_proc_input_refers_stackvars(foreign_proc_input) = bool.

foreign_proc_input_refers_stackvars(Input) = Refers :-
    Input = foreign_proc_input(_Name, _Type, IsDummy, _OrigType, Rval,
        _MaybeForeign, _BoxPolicy),
    (
        IsDummy = is_dummy_type,
        Refers = no
    ;
        IsDummy = is_not_dummy_type,
        Refers = rval_refers_stackvars(Rval)
    ).

:- func foreign_proc_output_refers_stackvars(foreign_proc_output) = bool.

foreign_proc_output_refers_stackvars(Input) = Refers :-
    Input = foreign_proc_output(Lval, _Type, IsDummy, _OrigType, _Name,
        _MaybeForeign, _BoxPolicy),
    (
        IsDummy = is_dummy_type,
        Refers = no
    ;
        IsDummy = is_not_dummy_type,
        Refers = lval_refers_stackvars(Lval)
    ).

block_refers_to_stack([]) = no.
block_refers_to_stack([Instr | Instrs]) = Refers :-
    instr_refers_to_stack(Instr) = InstrRefers,
    (
        InstrRefers = yes,
        Refers = yes
    ;
        InstrRefers = no,
        Instr = llds_instr(Uinstr, _),
        CanFallThrough = can_instr_fall_through(Uinstr),
        (
            CanFallThrough = yes,
            Refers = block_refers_to_stack(Instrs)
        ;
            CanFallThrough = no,
            Refers = no
        )
    ).

lval_access_rvals(reg(_, _), []).
lval_access_rvals(stackvar(_), []).
lval_access_rvals(parent_stackvar(_), []).
lval_access_rvals(framevar(_), []).
lval_access_rvals(double_stackvar(_, _), []).
lval_access_rvals(succip, []).
lval_access_rvals(maxfr, []).
lval_access_rvals(curfr, []).
lval_access_rvals(redoip_slot(Rval), [Rval]).
lval_access_rvals(succip_slot(Rval), [Rval]).
lval_access_rvals(redofr_slot(Rval), [Rval]).
lval_access_rvals(prevfr_slot(Rval), [Rval]).
lval_access_rvals(succfr_slot(Rval), [Rval]).
lval_access_rvals(hp, []).
lval_access_rvals(sp, []).
lval_access_rvals(parent_sp, []).
lval_access_rvals(field(_, Rval1, Rval2), [Rval1, Rval2]).
lval_access_rvals(temp(_, _), []).
lval_access_rvals(lvar(_), _) :-
    unexpected($pred, "lvar").
lval_access_rvals(mem_ref(Rval), [Rval]).
lval_access_rvals(global_var_ref(_), []).

%---------------------------------------------------------------------------%
%
% Substitutions.
%

replace_labels_instruction_list([], [], _, _, _).
replace_labels_instruction_list([Instr0 | Instrs0], [Instr | Instrs],
        ReplMap, ReplData, ReplLabel) :-
    ( if
        Instr0 = llds_instr(label(InstrLabel), Comment),
        ReplLabel = yes
    then
        replace_labels_label(InstrLabel, ReplInstrLabel, ReplMap),
        Instr = llds_instr(label(ReplInstrLabel), Comment)
    else
        replace_labels_instruction(Instr0, Instr, ReplMap, ReplData)
    ),
    replace_labels_instruction_list(Instrs0, Instrs,
        ReplMap, ReplData, ReplLabel).

replace_labels_instruction(Instr0, Instr, ReplMap, ReplData) :-
    Instr0 = llds_instr(Uinstr0, Comment),
    replace_labels_instr(Uinstr0, Uinstr, ReplMap, ReplData),
    Instr = llds_instr(Uinstr, Comment).

replace_labels_instr(Uinstr0, Uinstr, ReplMap, ReplData) :-
    (
        ( Uinstr0 = comment(_)
        ; Uinstr0 = livevals(_)
        ; Uinstr0 = discard_ticket
        ; Uinstr0 = prune_ticket
        ; Uinstr0 = incr_sp(_, _, _)
        ; Uinstr0 = decr_sp(_)
        ; Uinstr0 = decr_sp_and_return(_)
        ),
        Uinstr = Uinstr0
    ;
        Uinstr0 = block(R, F, Instrs0),
        % There should be no labels in Instrs0.
        replace_labels_instruction_list(Instrs0, Instrs,
            ReplMap, ReplData, no),
        Uinstr = block(R, F, Instrs)
    ;
        Uinstr0 = assign(Lval0, Rval0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap),
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0,
            Rval = Rval0
        ),
        Uinstr = assign(Lval, Rval)
    ;
        Uinstr0 = keep_assign(Lval0, Rval0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap),
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0,
            Rval = Rval0
        ),
        Uinstr = keep_assign(Lval, Rval)
    ;
        Uinstr0 = llcall(Target, Return0, LiveInfo, CXT, GP, CM),
        replace_labels_code_addr(Return0, Return, ReplMap),
        Uinstr = llcall(Target, Return, LiveInfo, CXT, GP, CM)
    ;
        Uinstr0 = mkframe(NondetFrameInfo, MaybeRedoip0),
        (
            ReplData = yes,
            (
                MaybeRedoip0 = yes(Redoip0),
                replace_labels_code_addr(Redoip0, Redoip, ReplMap),
                MaybeRedoip = yes(Redoip)
            ;
                MaybeRedoip0 = no,
                MaybeRedoip = no
            )
        ;
            ReplData = no,
            MaybeRedoip = MaybeRedoip0
        ),
        Uinstr = mkframe(NondetFrameInfo, MaybeRedoip)
    ;
        Uinstr0 = label(Label),
        ( if map.search(ReplMap, Label, _) then
            % The reason why we are replacing references to this label is that
            % it is being eliminated, and in fact should have been already
            % eliminated by the time replace_labels_instr is called.
            unexpected($pred, "eliminated label")
        else
            true
        ),
        Uinstr = label(Label)
    ;
        Uinstr0 = goto(Target0),
        replace_labels_code_addr(Target0, Target, ReplMap),
        Uinstr = goto(Target)
    ;
        Uinstr0 = computed_goto(Rval0, MaybeLabels0),
        (
            ReplData = yes,
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Rval = Rval0
        ),
        replace_labels_maybe_label_list(MaybeLabels0, MaybeLabels, ReplMap),
        Uinstr = computed_goto(Rval, MaybeLabels)
    ;
        Uinstr0 = arbitrary_c_code(AffectsLiveness, Lvals0, Code),
        (
            ReplData = yes,
            replace_labels_c_code_live_lvals(Lvals0, Lvals, ReplMap)
        ;
            ReplData = no,
            Lvals = Lvals0
        ),
        Uinstr = arbitrary_c_code(AffectsLiveness, Lvals, Code)
    ;
        Uinstr0 = if_val(Rval0, Target0),
        (
            ReplData = yes,
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Rval = Rval0
        ),
        replace_labels_code_addr(Target0, Target, ReplMap),
        Uinstr = if_val(Rval, Target)
    ;
        Uinstr0 = save_maxfr(Lval0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0
        ),
        Uinstr = save_maxfr(Lval)
    ;
        Uinstr0 = restore_maxfr(Lval0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0
        ),
        Uinstr = restore_maxfr(Lval)
    ;
        Uinstr0 = incr_hp(Lval0, MaybeTag, MO, Rval0, Msg, Atomic,
            MaybeRegionRval0, MaybeReuse0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap),
            replace_labels_rval(Rval0, Rval, ReplMap),
            (
                MaybeRegionRval0 = yes(RegionRval0),
                replace_labels_rval(RegionRval0, RegionRval, ReplMap),
                MaybeRegionRval = yes(RegionRval)
            ;
                MaybeRegionRval0 = no,
                MaybeRegionRval = MaybeRegionRval0
            ),
            (
                MaybeReuse0 = llds_reuse(ReuseRval0, MaybeFlagLval0),
                replace_labels_rval(ReuseRval0, ReuseRval, ReplMap),
                (
                    MaybeFlagLval0 = yes(FlagLval0),
                    replace_labels_lval(FlagLval0, FlagLval, ReplMap),
                    MaybeFlagLval = yes(FlagLval)
                ;
                    MaybeFlagLval0 = no,
                    MaybeFlagLval = no
                ),
                MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval)
            ;
                MaybeReuse0 = no_llds_reuse,
                MaybeReuse = no_llds_reuse
            )
        ;
            ReplData = no,
            Lval = Lval0,
            Rval = Rval0,
            MaybeRegionRval = MaybeRegionRval0,
            MaybeReuse = MaybeReuse0
        ),
        Uinstr = incr_hp(Lval, MaybeTag, MO, Rval, Msg, Atomic,
            MaybeRegionRval, MaybeReuse)
    ;
        Uinstr0 = mark_hp(Lval0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0
        ),
        Uinstr = mark_hp(Lval)
    ;
        Uinstr0 = restore_hp(Rval0),
        (
            ReplData = yes,
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Rval = Rval0
        ),
        Uinstr = restore_hp(Rval)
    ;
        Uinstr0 = free_heap(Rval0),
        (
            ReplData = yes,
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Rval = Rval0
        ),
        Uinstr = free_heap(Rval)
    ;
        Uinstr0 = push_region_frame(StackId, EmbeddedStackFrame),
        Uinstr = push_region_frame(StackId, EmbeddedStackFrame)
    ;
        Uinstr0 = region_fill_frame(FillOp, EmbeddedStackFrame, IdRval0,
            NumLval0, AddrLval0),
        (
            ReplData = yes,
            replace_labels_rval(IdRval0, IdRval, ReplMap),
            replace_labels_lval(NumLval0, NumLval, ReplMap),
            replace_labels_lval(AddrLval0, AddrLval, ReplMap)
        ;
            ReplData = no,
            IdRval = IdRval0,
            NumLval = NumLval0,
            AddrLval = AddrLval0
        ),
        Uinstr = region_fill_frame(FillOp, EmbeddedStackFrame, IdRval,
            NumLval, AddrLval)
    ;
        Uinstr0 = region_set_fixed_slot(SetOp, EmbeddedStackFrame,
            ValueRval0),
        (
            ReplData = yes,
            replace_labels_rval(ValueRval0, ValueRval, ReplMap)
        ;
            ReplData = no,
            ValueRval = ValueRval0
        ),
        Uinstr = region_set_fixed_slot(SetOp, EmbeddedStackFrame,
            ValueRval)
    ;
        Uinstr0 = use_and_maybe_pop_region_frame(UseOp, EmbeddedStackFrame),
        Uinstr = use_and_maybe_pop_region_frame(UseOp, EmbeddedStackFrame)
    ;
        Uinstr0 = store_ticket(Lval0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0
        ),
        Uinstr = store_ticket(Lval)
    ;
        Uinstr0 = reset_ticket(Rval0, Reason),
        (
            ReplData = yes,
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Rval = Rval0
        ),
        Uinstr = reset_ticket(Rval, Reason)
    ;
        Uinstr0 = mark_ticket_stack(Lval0),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0
        ),
        Uinstr = mark_ticket_stack(Lval)
    ;
        Uinstr0 = prune_tickets_to(Rval0),
        (
            ReplData = yes,
            replace_labels_rval(Rval0, Rval, ReplMap)
        ;
            ReplData = no,
            Rval = Rval0
        ),
        Uinstr = prune_tickets_to(Rval)
    ;
        Uinstr0 = foreign_proc_code(Decls, Comps0, MayCallMercury,
            MaybeFix, MaybeLayout, MaybeOnlyLayout, MaybeSub0, MaybeDef,
            StackSlotRef, MayDupl),
        (
            MaybeFix = no
        ;
            MaybeFix = yes(FixLabel0),
            replace_labels_label(FixLabel0, FixLabel, ReplMap),
            % We cannot replace the label in the C code string itself.
            expect(unify(FixLabel0, FixLabel), $pred,
                "trying to replace Mercury label in C code")
        ),
        (
            MaybeLayout = no
        ;
            MaybeLayout = yes(LayoutLabel0),
            replace_labels_label(LayoutLabel0, LayoutLabel, ReplMap),
            % We cannot replace a label that has a layout structure.
            expect(unify(LayoutLabel0, LayoutLabel), $pred,
                "trying to replace Mercury label with layout")
        ),
        (
            MaybeOnlyLayout = no
        ;
            MaybeOnlyLayout = yes(OnlyLayoutLabel0),
            replace_labels_label(OnlyLayoutLabel0, OnlyLayoutLabel, ReplMap),
            % We cannot replace a label that has a layout structure.
            expect(unify(OnlyLayoutLabel0, OnlyLayoutLabel), $pred,
                "trying to replace Mercury label with layout")
        ),
        (
            MaybeSub0 = no,
            MaybeSub = no,
            Comps = Comps0
        ;
            MaybeSub0 = yes(SubLabel0),
            replace_labels_label(SubLabel0, SubLabel, ReplMap),
            MaybeSub = yes(SubLabel),
            replace_labels_comps(Comps0, Comps, ReplMap)
        ),
        (
            MaybeDef = no
        ;
            MaybeDef = yes(DefLabel0),
            replace_labels_label(DefLabel0, DefLabel, ReplMap),
            % We cannot replace a label that has a layout structure.
            expect(unify(DefLabel0, DefLabel), $pred,
                "trying to replace Mercury label with layout")
        ),
        Uinstr = foreign_proc_code(Decls, Comps, MayCallMercury,
            MaybeFix, MaybeLayout, MaybeOnlyLayout, MaybeSub, MaybeDef,
            StackSlotRef, MayDupl)
    ;
        Uinstr0 = init_sync_term(Lval0, NumConjuncts, TSStringIndex),
        (
            ReplData = yes,
            replace_labels_lval(Lval0, Lval, ReplMap)
        ;
            ReplData = no,
            Lval = Lval0
        ),
        Uinstr = init_sync_term(Lval, NumConjuncts, TSStringIndex)
    ;
        Uinstr0 = fork_new_child(Lval0, Child0),
        replace_labels_lval(Lval0, Lval, ReplMap),
        replace_labels_label(Child0, Child, ReplMap),
        Uinstr = fork_new_child(Lval, Child)
    ;
        Uinstr0 = join_and_continue(Lval0, Label0),
        replace_labels_lval(Lval0, Lval, ReplMap),
        replace_labels_label(Label0, Label, ReplMap),
        Uinstr = join_and_continue(Lval, Label)
    ;
        Uinstr0 = lc_create_loop_control(NumSLots, Lval0),
        replace_labels_lval(Lval0, Lval, ReplMap),
        Uinstr = lc_create_loop_control(NumSLots, Lval)
    ;
        Uinstr0 = lc_wait_free_slot(Rval0, Lval0, Label0),
        replace_labels_rval(Rval0, Rval, ReplMap),
        replace_labels_lval(Lval0, Lval, ReplMap),
        replace_labels_label(Label0, Label, ReplMap),
        Uinstr = lc_wait_free_slot(Rval, Lval, Label)
    ;
        Uinstr0 = lc_spawn_off(LCRval0, LCSRval0, Label0),
        replace_labels_rval(LCRval0, LCRval, ReplMap),
        replace_labels_rval(LCSRval0, LCSRval, ReplMap),
        replace_labels_label(Label0, Label, ReplMap),
        Uinstr = lc_spawn_off(LCRval, LCSRval, Label)
    ;
        Uinstr0 = lc_join_and_terminate(LCRval0, LCSRval0),
        replace_labels_rval(LCRval0, LCRval, ReplMap),
        replace_labels_rval(LCSRval0, LCSRval, ReplMap),
        Uinstr = lc_join_and_terminate(LCRval, LCSRval)
    ).

replace_labels_comps([], [], _).
replace_labels_comps([Comp0 | Comps0], [Comp | Comps], ReplMap) :-
    replace_labels_comp(Comp0, Comp, ReplMap),
    replace_labels_comps(Comps0, Comps, ReplMap).

:- pred replace_labels_comp(
    foreign_proc_component::in, foreign_proc_component::out,
    map(label, label)::in) is det.

replace_labels_comp(Comp0, Comp, ReplMap) :-
    (
        ( Comp0 = foreign_proc_inputs(_)
        ; Comp0 = foreign_proc_outputs(_)
        ; Comp0 = foreign_proc_user_code(_, _, _)
        ; Comp0 = foreign_proc_raw_code(_, _, _, _)
        ; Comp0 = foreign_proc_alloc_id(_)
        ; Comp0 = foreign_proc_noop
        ),
        Comp = Comp0
    ;
        Comp0 = foreign_proc_fail_to(Label0),
        replace_labels_label(Label0, Label, ReplMap),
        Comp = foreign_proc_fail_to(Label)
    ).

:- pred replace_labels_c_code_live_lvals(
    c_code_live_lvals::in, c_code_live_lvals::out, map(label, label)::in)
    is det.

replace_labels_c_code_live_lvals(LiveLvals0, LiveLvals, ReplMap) :-
    (
        LiveLvals0 = no_live_lvals_info,
        LiveLvals = LiveLvals0
    ;
        LiveLvals0 = live_lvals_info(LvalSet0),
        set.to_sorted_list(LvalSet0, Lvals0),
        list.map(replace_labels_lval_map(ReplMap), Lvals0, Lvals),
        % We cannot replace the lvals inside the C code.
        expect(unify(Lvals0, Lvals), $pred, "some replacements"),
        LiveLvals = LiveLvals0
    ).

:- pred replace_labels_lval_map(map(label, label)::in, lval::in, lval::out)
    is det.

replace_labels_lval_map(ReplMap, Lval0, Lval) :-
    replace_labels_lval(Lval0, Lval, ReplMap).

:- pred replace_labels_lval(lval::in, lval::out, map(label, label)::in) is det.

replace_labels_lval(Lval0, Lval, ReplMap) :-
    (
        ( Lval0 = reg(_, _)
        ; Lval0 = temp(_, _)
        ; Lval0 = stackvar(_)
        ; Lval0 = framevar(_)
        ; Lval0 = parent_stackvar(_)
        ; Lval0 = double_stackvar(_, _)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ; Lval0 = lvar(_)
        ; Lval0 = global_var_ref(_)
        ),
        Lval = Lval0
    ;
        Lval0 = succip_slot(Rval0),
        replace_labels_rval(Rval0, Rval, ReplMap),
        Lval = succip_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        replace_labels_rval(Rval0, Rval, ReplMap),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        replace_labels_rval(Rval0, Rval, ReplMap),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        replace_labels_rval(Rval0, Rval, ReplMap),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        replace_labels_rval(Rval0, Rval, ReplMap),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = field(Tag, BaseRval0, OffsetRval0),
        replace_labels_rval(BaseRval0, BaseRval, ReplMap),
        replace_labels_rval(OffsetRval0, OffsetRval, ReplMap),
        Lval = field(Tag, BaseRval, OffsetRval)
    ;
        Lval0 = mem_ref(Rval0),
        replace_labels_rval(Rval0, Rval, ReplMap),
        Lval = mem_ref(Rval)
    ).

:- pred replace_labels_rval(rval::in, rval::out, map(label, label)::in) is det.

replace_labels_rval(Rval0, Rval, ReplMap) :-
    (
        Rval0 = lval(Lval0),
        replace_labels_lval(Lval0, Lval, ReplMap),
        Rval = lval(Lval)
    ;
        Rval0 = var(_Var),
        Rval = Rval0
    ;
        Rval0 = mkword(Tag, SubRval0),
        replace_labels_rval(SubRval0, SubRval, ReplMap),
        Rval = mkword(Tag, SubRval)
    ;
        Rval0 = mkword_hole(Tag),
        Rval = mkword_hole(Tag)
    ;
        Rval0 = const(Const0),
        replace_labels_rval_const(Const0, Const, ReplMap),
        Rval = const(Const)
    ;
        Rval0 = cast(Type, SubRvalA0),
        replace_labels_rval(SubRvalA0, SubRvalA, ReplMap),
        Rval = cast(Type, SubRvalA)
    ;
        Rval0 = unop(UnOp, SubRvalA0),
        replace_labels_rval(SubRvalA0, SubRvalA, ReplMap),
        Rval = unop(UnOp, SubRvalA)
    ;
        Rval0 = binop(BinOp, SubRvalA0, SubRvalB0),
        replace_labels_rval(SubRvalA0, SubRvalA, ReplMap),
        replace_labels_rval(SubRvalB0, SubRvalB, ReplMap),
        Rval = binop(BinOp, SubRvalA, SubRvalB)
    ;
        Rval0 = mem_addr(MemRef0),
        replace_labels_mem_ref(MemRef0, MemRef, ReplMap),
        Rval = mem_addr(MemRef)
    ).

:- pred replace_labels_mem_ref(mem_ref::in, mem_ref::out,
    map(label, label)::in) is det.

replace_labels_mem_ref(MemRef0, MemRef, ReplMap) :-
    (
        ( MemRef0 = stackvar_ref(_)
        ; MemRef0 = framevar_ref(_)
        ),
        MemRef = MemRef0
    ;
        MemRef0 = heap_ref(CellRval0, MaybeTag, FieldNumRval0),
        replace_labels_rval(CellRval0, CellRval, ReplMap),
        replace_labels_rval(FieldNumRval0, FieldNumRval, ReplMap),
        MemRef = heap_ref(CellRval, MaybeTag, FieldNumRval)
    ).

:- pred replace_labels_rval_const(rval_const::in, rval_const::out,
    map(label, label)::in) is det.

replace_labels_rval_const(Const0, Const, ReplMap) :-
    (
        ( Const0 = llconst_true
        ; Const0 = llconst_false
        ; Const0 = llconst_int(_)
        ; Const0 = llconst_uint(_)
        ; Const0 = llconst_int8(_)
        ; Const0 = llconst_uint8(_)
        ; Const0 = llconst_int16(_)
        ; Const0 = llconst_uint16(_)
        ; Const0 = llconst_int32(_)
        ; Const0 = llconst_uint32(_)
        ; Const0 = llconst_int64(_)
        ; Const0 = llconst_uint64(_)
        ; Const0 = llconst_foreign(_, _)
        ; Const0 = llconst_float(_)
        ; Const0 = llconst_string(_)
        ; Const0 = llconst_multi_string(_)
        ; Const0 = llconst_data_addr(_, _)
        ),
        Const = Const0
    ;
        Const0 = llconst_code_addr(Addr0),
        replace_labels_code_addr(Addr0, Addr, ReplMap),
        Const = llconst_code_addr(Addr)
    ).

replace_labels_code_addr(Addr0, Addr, ReplMap) :-
    (
        Addr0 = code_label(Label0),
        replace_labels_label(Label0, Label, ReplMap),
        Addr = code_label(Label)
    ;
        ( Addr0 = code_imported_proc(_)
        ; Addr0 = code_succip
        ; Addr0 = do_succeed(_)
        ; Addr0 = do_redo
        ; Addr0 = do_fail
        ; Addr0 = do_trace_redo_fail_shallow
        ; Addr0 = do_trace_redo_fail_deep
        ; Addr0 = do_call_closure(_)
        ; Addr0 = do_call_class_method(_)
        ; Addr0 = do_not_reached
        ),
        Addr = Addr0
    ).

replace_labels_maybe_label_list([], [], _ReplMap).
replace_labels_maybe_label_list([MaybeLabel0 | MaybeLabels0],
        [MaybeLabel | MaybeLabels], ReplMap) :-
    (
        MaybeLabel0 = yes(Label0),
        replace_labels_label(Label0, Label, ReplMap),
        MaybeLabel = yes(Label)
    ;
        MaybeLabel0 = no,
        MaybeLabel = no
    ),
    replace_labels_maybe_label_list(MaybeLabels0, MaybeLabels, ReplMap).

replace_labels_label(Label0, Label, ReplMap) :-
    ( if map.search(ReplMap, Label0, NewLabel) then
        Label = NewLabel
    else
        Label = Label0
    ).

%---------------------------------------------------------------------------%
%
% Printing.
%

format_label(internal_label(_, ProcLabel)) = format_proc_label(ProcLabel).
format_label(entry_label(_, ProcLabel)) = format_proc_label(ProcLabel).

format_proc_label(ordinary_proc_label(_Module, _PredOrFunc, _, Name,
        Arity, Mode)) =
    Name ++ "/" ++ int_to_string(Arity) ++ " mode " ++ int_to_string(Mode).
format_proc_label(special_proc_label(_Module, SpecialPredId, TypeModule,
        TypeName, TypeArity, Mode)) =
        PredName ++ "_" ++ TypeName ++ "/" ++ int_to_string(TypeArity)
            ++ " mode " ++ int_to_string(Mode) :-
    TypeCtor = type_ctor(qualified(TypeModule, TypeName), TypeArity),
    PredName = uci_pred_name(SpecialPredId, TypeCtor).

%---------------------------------------------------------------------------%
:- end_module ll_backend.opt_util.
%---------------------------------------------------------------------------%
