%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% labelopt.m - module to eliminate useless labels and dead code.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module labelopt.

:- interface.

:- import_module bool, list, set.
:- import_module llds.

	% Build up a set showing which labels are branched to,
	% then traverse the instruction list removing unnecessary labels.
	% If the instruction before the label branches away, we also
	% remove the instruction block following the label.

:- pred labelopt_main(list(instruction), bool, list(instruction), bool).
:- mode labelopt_main(in, in, out, out) is det.

	% Build up a set showing which labels are branched to.

:- pred labelopt__build_useset(list(instruction), set(label)).
:- mode labelopt__build_useset(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util.
:- import_module std_util.

labelopt_main(Instrs0, Final, Instrs, Mod) :-
	labelopt__build_useset(Instrs0, Useset),
	labelopt__instr_list(Instrs0, yes, Useset, Instrs1, Mod),
	( Final = yes, Mod = yes ->
		labelopt_main(Instrs1, Final, Instrs, _)
	;
		Instrs = Instrs1
	).

%-----------------------------------------------------------------------------%

labelopt__build_useset(Instrs, Useset) :-
	set__init(Useset0),
	labelopt__build_useset_2(Instrs, Useset0, Useset).

:- pred labelopt__build_useset_2(list(instruction), set(label), set(label)).
:- mode labelopt__build_useset_2(in, in, out) is det.

labelopt__build_useset_2([], Useset, Useset).
labelopt__build_useset_2([Instr | Instructions], Useset0, Useset) :-
	Instr = Uinstr - _Comment,
	opt_util__instr_labels(Uinstr, Labels, _CodeAddresses),
	set__insert_list(Useset0, Labels, Useset1),
	labelopt__build_useset_2(Instructions, Useset1, Useset).

%-----------------------------------------------------------------------------%

	% Go through the given instruction sequence. When we find a label,
	% we check whether the label can be branched to either from within
	% the procedure or from the outside. If yes, we leave it alone.
	% If not, we delete it. We delete the following code as well if
	% the label was preceded by code that cannot fall through.

:- pred labelopt__instr_list(list(instruction), bool, set(label),
	list(instruction), bool).
:- mode labelopt__instr_list(in, in, in, out, out) is det.

labelopt__instr_list([], _Fallthrough, _Useset, [], no).
labelopt__instr_list([Instr0 | MoreInstrs0],
		Fallthrough, Useset, MoreInstrs, Mod) :-
	Instr0 = Uinstr0 - _Comment,
	( Uinstr0 = label(Label) ->
		(
			( Label = exported(_)
			; Label = local(_)
			; set__member(Label, Useset)
			)
		->
			ReplInstrs = [Instr0],
			Fallthrough1 = yes,
			Mod0 = no
		;
			labelopt__eliminate(Instr0, yes(Fallthrough),
				ReplInstrs, Mod0),
			Fallthrough1 = Fallthrough
		)
	;
		( Fallthrough = yes ->
			ReplInstrs = [Instr0],
			Mod0 = no
		;
			labelopt__eliminate(Instr0, no, ReplInstrs, Mod0)
		),
		opt_util__can_instr_fall_through(Uinstr0, Canfallthrough),
		( Canfallthrough = yes ->
			Fallthrough1 = Fallthrough
		;
			Fallthrough1 = no
		)
	),
	labelopt__instr_list(MoreInstrs0, Fallthrough1, Useset,
		MoreInstrs1, Mod1),
	list__append(ReplInstrs, MoreInstrs1, MoreInstrs),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

	% Instead of removing eliminated instructions from the instruction list,
	% we can replace them by placeholder comments. The original comment
	% field on the instruction is often enough to deduce what the
	% eliminated instruction was.

:- pred labelopt__eliminate(instruction, maybe(bool), list(instruction), bool).
:- mode labelopt__eliminate(in, in, out, out) is det.

labelopt__eliminate(Uinstr0 - Comment0, Label, Instr, Mod) :-
	labelopt_eliminate_total(Total),
	(
		Total = yes,
		Instr = [],
		Mod = yes
	;
		Total = no,
		( Uinstr0 = comment(_) ->
			Comment = Comment0,
			Uinstr = Uinstr0,
			Mod = no
		;
			( Label = yes(Follow) ->
				( Follow = yes ->
					Uinstr = comment("eliminated label only")
				;
					% Follow = no,
					Uinstr = comment("eliminated label and block")
				)
			;
				% Label = no,
				Uinstr = comment("eliminated instruction")
			),
			Comment = Comment0,
			Mod = yes
		),
		Instr = [Uinstr - Comment]
	).

:- pred labelopt_eliminate_total(bool).
:- mode labelopt_eliminate_total(out) is det.

labelopt_eliminate_total(yes).

%-----------------------------------------------------------------------------%
