%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% labelopt.m - module to eliminate useless labels and dead code.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module labelopt.

:- interface.

:- import_module list, llds.

	% Build up a set showing which labels are branched to,
	% then traverse the instruction list removing unnecessary labels.
	% If the instruction before the label branches away, we also
	% remove the instruction block following the label.

:- pred labelopt__main(list(instruction), list(instruction), bool).
:- mode labelopt__main(in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util, std_util, bintree_set.

:- type usemap == bintree_set(label).

labelopt__main(Instructions0, Instructions, Mod) :-
	bintree_set__init(Usemap0),
	labelopt__build_usemap(Instructions0, Usemap0, Usemap),
	labelopt__instr_list(Instructions0, yes, Usemap, Instructions, Mod).

%-----------------------------------------------------------------------------%

	% Build up a set showing which labels are branched to.

:- pred labelopt__build_usemap(list(instruction), usemap, usemap).
:- mode labelopt__build_usemap(in, di, uo) is det.

labelopt__build_usemap([], Usemap, Usemap).
labelopt__build_usemap([Instr | Instructions], Usemap0, Usemap) :-
	Instr = Uinstr - _Comment,
	opt_util__instr_labels(Uinstr, Labels, CodeAddresses),
	labelopt__label_list_build_usemap(Labels, Usemap0, Usemap1),
	labelopt__code_addr_list_build_usemap(CodeAddresses, Usemap1, Usemap2),
	labelopt__build_usemap(Instructions, Usemap2, Usemap).

	% We are not interested in code addresses that are not labels.

:- pred labelopt__code_addr_list_build_usemap(list(code_addr), usemap, usemap).
:- mode labelopt__code_addr_list_build_usemap(in, di, uo) is det.

labelopt__code_addr_list_build_usemap([], Usemap, Usemap).
labelopt__code_addr_list_build_usemap([Code_addr | Rest], Usemap0, Usemap) :-
	( Code_addr = label(Label) ->
		bintree_set__insert(Usemap0, Label, Usemap1)
	;
		Usemap1 = Usemap0
	),
	labelopt__code_addr_list_build_usemap(Rest, Usemap1, Usemap).

:- pred labelopt__label_list_build_usemap(list(label), usemap, usemap).
:- mode labelopt__label_list_build_usemap(in, di, uo) is det.

labelopt__label_list_build_usemap([], Usemap, Usemap).
labelopt__label_list_build_usemap([Label | Labels], Usemap0, Usemap) :-
	bintree_set__insert(Usemap0, Label, Usemap1),
	labelopt__label_list_build_usemap(Labels, Usemap1, Usemap).

%-----------------------------------------------------------------------------%

	% Go through the given instruction sequence. When we find a label,
	% we check whether the label can be branched to either from within
	% the procedure or from the outside. If yes, we leave it alone.
	% If not, we delete it. We delete the following code as well if
	% the label was preceded by code that cannot fall through.

:- pred labelopt__instr_list(list(instruction),
	bool, usemap, list(instruction), bool).
:- mode labelopt__instr_list(in, in, in, out, out) is det.

labelopt__instr_list([], _Fallthrough, _Usemap, [], no).
labelopt__instr_list([Instr0 | MoreInstrs0],
		Fallthrough, Usemap, MoreInstrs, Mod) :-
	( Instr0 = label(Label) - Comment ->
		(
		    (   Label = exported(_)
		    ;	Label = local(_)
		    ;   bintree_set__is_member(Label, Usemap)
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
		Instr0 = Uinstr0 - Comment,
		opt_util__can_instr_fall_through(Uinstr0, Canfallthrough),
		( Canfallthrough = yes ->
			Fallthrough1 = Fallthrough
		;
			Fallthrough1 = no
		)
	),
	labelopt__instr_list(MoreInstrs0, Fallthrough1, Usemap,
		MoreInstrs1, Mod1),
	list__append(ReplInstrs, MoreInstrs1, MoreInstrs),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

	% Instead of removing eliminated instructions from the instruction list,
	% we can replace them by placeholder comments. The original comment field
	% on the instruction is often enough to deduce what the eliminated
	% instruction was.

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
