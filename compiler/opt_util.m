%-----------------------------------------------------------------------------%

% Utilities for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module opt_util.

:- interface.
:- import_module llds, list, std_util.

:- pred opt_util__skip_comments(list(instruction), list(instruction)).
:- mode opt_util__skip_comments(in, out) is det.

:- pred opt_util__skip_comments_labels(list(instruction), list(instruction)).
:- mode opt_util__skip_comments_labels(in, out) is det.

	% Find the next modframe if it is guaranteed to be reached from here

:- pred opt_util__next_modframe(list(instruction), list(instruction),
	code_addr, list(instruction), list(instruction)).
:- mode opt_util__next_modframe(in, in, out, out, out) is semidet.

	% Check whether the named label follows without any intervening code.
	% If yes, return the instructions after the label.

:- pred opt_util__is_this_label_next(label, list(instruction),
	list(instruction)).
:- mode opt_util__is_this_label_next(in, in, out) is semidet.

 	% Is a proceed instruction (i.e. a goto(succip) instruction)
 	% next in the instruction list, possibly preceded by a restoration
 	% of succip and a det stack frame removal? If yes, return the
 	% instructions up to the proceed.

:- pred opt_util__is_proceed_next(list(instruction), list(instruction)).
:- mode opt_util__is_proceed_next(in, out) is semidet.

	% Check whether an instruction can possibly branch away.

:- pred opt_util__can_instr_branch_away(instr, bool).
:- mode opt_util__can_instr_branch_away(in, out) is det.

	% Check whether an instruction can possibly fall through
	% to the next instruction without using its label.

:- pred opt_util__can_instr_fall_through(instr, bool).
:- mode opt_util__can_instr_fall_through(in, out) is det.

	% Check whether a code_addr, when the target of a goto, represents
	% either a call or a proceed/succeed; if so, it is the end of an
	% extended basic block and needs a livevals in front of it.

:- pred opt_util__livevals_addr(code_addr, bool).
:- mode opt_util__livevals_addr(in, out) is det.

	% Determine all the labels and code addresses which are referenced
	% by an instruction.

:- pred opt_util__instr_labels(instr, list(label), list(code_addr)).
:- mode opt_util__instr_labels(in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
% :- import_module opt_util, map, bintree_set.
% :- import_module string, require.

	% Given a list of instructions, skip past any comment instructions
	% at the start and return the remaining instructions.
	% We do this because comment instructions get in the way of
	% peephole optimization.

opt_util__skip_comments(Instrs0, Instrs) :-
	( Instrs0 = [comment(_) - _ | Instrs1] ->
		opt_util__skip_comments(Instrs1, Instrs)
	;
		Instrs = Instrs0
	).

opt_util__skip_comments_labels(Instrs0, Instrs) :-
	( Instrs0 = [comment(_) - _ | Instrs1] ->
		opt_util__skip_comments_labels(Instrs1, Instrs)
	; Instrs0 = [label(_) - _ | Instrs1] ->
		opt_util__skip_comments_labels(Instrs1, Instrs)
	;
		Instrs = Instrs0
	).

opt_util__next_modframe([Instr | Instrs], RevSkip, Redoip, Skip, Rest) :-
	Instr = Uinstr - _Comment,
	( Uinstr = modframe(Redoip0) ->
		Redoip = Redoip0,
		list__reverse(RevSkip, Skip),
		Rest = Instrs
	; Uinstr = mkframe(_, _, _) ->
		fail
	;
		opt_util__can_instr_branch_away(Uinstr, Canbranchaway),
		( Canbranchaway = no ->
			opt_util__next_modframe(Instrs, [Instr | RevSkip],
				Redoip, Skip, Rest)
		;
			fail
		)
	).

opt_util__is_this_label_next(Label, [Instr | Moreinstr], Remainder) :-
	Instr = Uinstr - _Comment,
	( Uinstr = comment(_) ->
		opt_util__is_this_label_next(Label, Moreinstr, Remainder)
	; Uinstr = label(NextLabel) ->
		( Label = NextLabel ->
			Remainder = Moreinstr
		;
			opt_util__is_this_label_next(Label, Moreinstr,
				Remainder)
		)
	;
		fail
	).

opt_util__is_proceed_next(Instrs0, Instrs_between) :-
	opt_util__skip_comments_labels(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = assign(succip, lval(stackvar(_))) - _,
	opt_util__skip_comments_labels(Instrs2, Instrs3),
	Instrs3 = [Instr3 | Instrs4],
	Instr3 = decr_sp(_) - _,
	opt_util__skip_comments_labels(Instrs4, Instrs5),
	Instrs5 = [Instr5 | _],
	Instr5 = goto(succip) - _,
	Instrs_between = [Instr1, Instr3].

opt_util__can_instr_branch_away(comment(_), no).
opt_util__can_instr_branch_away(livevals(_), no).
opt_util__can_instr_branch_away(block(_, _), no).
opt_util__can_instr_branch_away(assign(_, _), no).
opt_util__can_instr_branch_away(call(_, _), yes).
opt_util__can_instr_branch_away(mkframe(_, _, _), no).
opt_util__can_instr_branch_away(modframe(_), no).
opt_util__can_instr_branch_away(label(_), no).
opt_util__can_instr_branch_away(goto(_), yes).
opt_util__can_instr_branch_away(computed_goto(_, _), yes).
opt_util__can_instr_branch_away(c_code(_), no).
opt_util__can_instr_branch_away(if_val(_, _), yes).
opt_util__can_instr_branch_away(incr_sp(_), no).
opt_util__can_instr_branch_away(decr_sp(_), no).
opt_util__can_instr_branch_away(incr_hp(_), no).

opt_util__can_instr_fall_through(comment(_), yes).
opt_util__can_instr_fall_through(livevals(_), yes).
opt_util__can_instr_fall_through(block(_, _), yes).
opt_util__can_instr_fall_through(assign(_, _), yes).
opt_util__can_instr_fall_through(call(_, _), no).
opt_util__can_instr_fall_through(mkframe(_, _, _), yes).
opt_util__can_instr_fall_through(modframe(_), yes).
opt_util__can_instr_fall_through(label(_), yes).
opt_util__can_instr_fall_through(goto(_), no).
opt_util__can_instr_fall_through(computed_goto(_, _), no).
opt_util__can_instr_fall_through(c_code(_), yes).
opt_util__can_instr_fall_through(if_val(_, _), yes).
opt_util__can_instr_fall_through(incr_sp(_), yes).
opt_util__can_instr_fall_through(decr_sp(_), yes).
opt_util__can_instr_fall_through(incr_hp(_), yes).

opt_util__instr_labels(comment(_), [], []).
opt_util__instr_labels(livevals(_), [], []).
opt_util__instr_labels(block(_, _), [], []).
opt_util__instr_labels(assign(_,_), [], []).
opt_util__instr_labels(call(Target, Ret), [], [Target, Ret]).
opt_util__instr_labels(mkframe(_, _, Addr), [], [Addr]).
opt_util__instr_labels(modframe(Addr), [], [Addr]).
opt_util__instr_labels(label(_), [], []).
opt_util__instr_labels(goto(Addr), [], [Addr]).
opt_util__instr_labels(computed_goto(_, Labels), Labels, []).
opt_util__instr_labels(if_val(_, Addr), [], [Addr]).
opt_util__instr_labels(c_code(_), [], []).
opt_util__instr_labels(incr_hp(_), [], []).
opt_util__instr_labels(incr_sp(_), [], []).
opt_util__instr_labels(decr_sp(_), [], []).

opt_util__livevals_addr(label(local(_)), yes).
opt_util__livevals_addr(label(local(_, _)), no).
opt_util__livevals_addr(label(exported(_)), yes).
opt_util__livevals_addr(imported(_), yes).
opt_util__livevals_addr(succip, yes).
opt_util__livevals_addr(do_succeed, yes).
opt_util__livevals_addr(do_redo, no).
opt_util__livevals_addr(do_fail, no).

:- end_module opt_util.

%-----------------------------------------------------------------------------%
