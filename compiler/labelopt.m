%-----------------------------------------------------------------------------%

% labelopt.nl - module to eliminate useless labels and dead code.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module labelopt.

:- interface.

:- import_module list, llds.

:- pred labelopt__main(list(instruction), list(instruction), bool).
:- mode labelopt__main(in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util, std_util, bintree_set.

	% Build up a table showing which labels are branched to.
	% Then traverse the instruction list removing unnecessary labels.
	% If the instruction before the label branches away, we also
	% remove the instruction block following the label.

:- type usemap == bintree_set(label).

labelopt__main(Instructions0, Instructions, Mod) :-
	bintree_set__init(Usemap0),
	labelopt__build_usemap(Instructions0, Usemap0, Usemap),
	labelopt__instr_list(Instructions0, Usemap,
		Instructions, Mod).

:- pred labelopt__build_usemap(list(instruction), usemap, usemap).
:- mode labelopt__build_usemap(in, di, uo) is det.

labelopt__build_usemap([], Usemap, Usemap).
labelopt__build_usemap([Instr | Instructions], Usemap0, Usemap) :-
	Instr = Uinstr - _Comment,
	% format("looking at instr ~w", [Instr]), nl,
	opt_util__instr_labels(Uinstr, Labels, CodeAddresses),
	labelopt__label_list_build_usemap(Labels, Usemap0, Usemap1),
	labelopt__code_addr_list_build_usemap(CodeAddresses, Usemap1, Usemap2),
	labelopt__build_usemap(Instructions, Usemap2, Usemap).

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

:- pred labelopt__instr_list(list(instruction),
	usemap, list(instruction), bool).
:- mode labelopt__instr_list(in, in, out, out) is det.

labelopt__instr_list(Instrs0, Usemap, Instrs, Mod) :-
	labelopt__instr_list(Instrs0, yes, Usemap, Instrs, Mod).

:- pred labelopt__instr_list(list(instruction),
	bool, usemap, list(instruction), bool).
:- mode labelopt__instr_list(in, in, in, out, out) is det.

labelopt__instr_list([], _Fallthrough, _Usemap, [], no).
labelopt__instr_list([Instr0 | Moreinstrs0],
		Fallthrough, Usemap, [Instr | Moreinstrs], Mod) :-
	( Instr0 = label(Label) - Comment ->
		(
		    (   Label = exported(_)
		    ;	Label = local(_)
		    ;   bintree_set__is_member(Label, Usemap)
		    )
		->
			Instr = Instr0,
			Fallthrough1 = yes,
			Mod0 = no
		;
			labelopt__eliminate(Instr0, yes(Fallthrough), Instr,
				Mod0),
			Fallthrough1 = Fallthrough
		)
	;
		( Fallthrough = yes ->
			Instr = Instr0,
			Mod0 = no
		;
			labelopt__eliminate(Instr0, no, Instr, Mod0)
		),
		Instr0 = Uinstr0 - Comment,
		opt_util__can_instr_fall_through(Uinstr0, Canfallthrough),
		( Canfallthrough = yes ->
			Fallthrough1 = Fallthrough
		;
			Fallthrough1 = no
		)
	),
	labelopt__instr_list(Moreinstrs0, Fallthrough1, Usemap,
				Moreinstrs, Mod1),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

:- pred labelopt__eliminate(instruction, maybe(bool), instruction, bool).
:- mode labelopt__eliminate(in, in, out, out) is det.

labelopt__eliminate(Uinstr0 - Comment0, Label, Uinstr - Comment, Mod) :-
	( Uinstr0 = comment(_) ->
		Comment = Comment0,
		Uinstr = Uinstr0,
		Mod = no
	;
		(
			Label = yes(Follow)
		->
			(
				Follow = yes
			->
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
	).

%-----------------------------------------------------------------------------%
