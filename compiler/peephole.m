%-----------------------------------------------------------------------------%

% Peephole.nl - LLDS to LLDS peephole optimization.

% Main author: fjh.

%-----------------------------------------------------------------------------%

:- module peephole.		
:- interface.
:- import_module llds.

:- pred peephole__optimize(c_file, c_file).
:- mode peephole__optimize(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, std_util.

%-----------------------------------------------------------------------------%

	% Boring LLDS traversal code.

peephole__optimize(c_file(Name, Modules0), c_file(Name, Modules)) :-
	peephole__opt_module_list(Modules0, Modules).

:- pred peephole__opt_module_list(list(c_module), list(c_module)).
:- mode peephole__opt_module_list(in, out) is det.

peephole__opt_module_list([], []).
peephole__opt_module_list([M0|Ms0], [M|Ms]) :-
	peephole__opt_module(M0, M),
	peephole__opt_module_list(Ms0, Ms).

:- pred peephole__opt_module(c_module, c_module).
:- mode peephole__opt_module(in, out) is det.

peephole__opt_module(c_module(Name,Procs0), c_module(Name,Procs)) :-
	peephole__opt_proc_list(Procs0, Procs).

:- pred peephole__opt_proc_list(list(c_procedure), list(c_procedure)).
:- mode peephole__opt_proc_list(in, out) is det.

peephole__opt_proc_list([], []).
peephole__opt_proc_list([P0|Ps0], [P|Ps]) :-
	peephole__opt_proc(P0, P),
	peephole__opt_proc_list(Ps0, Ps).

:- pred peephole__opt_proc(c_procedure, c_procedure).
:- mode peephole__opt_proc(in, out) is det.

peephole__opt_proc(c_procedure(Name,Arity,Mode,Instructions0),
		   c_procedure(Name,Arity,Mode,Instructions)) :-
	peephole__opt_instr_list(Instructions0, Instructions).

%-----------------------------------------------------------------------------%

	% We zip down to the end of the instruction list, and start attempting
	% to optimize instruction sequences.  As long as we can continue
	% optimizing the instruction sequence, we keep doing so;
	% when we find a sequence we can't optimize, we back up try
	% so optimize the sequence starting with the previous instruction.

:- pred peephole__opt_instr_list(list(instruction), list(instruction)).
:- mode peephole__opt_instr_list(in, out) is det.

peephole__opt_instr_list([], []).
peephole__opt_instr_list([Instr0 - Comment|Instructions0], Instructions) :-
	peephole__opt_instr_list(Instructions0, Instructions1),
	peephole__opt_instr(Instr0, Comment, Instructions1, Instructions).

:- pred peephole__opt_instr(instr, string, list(instruction),
				list(instruction)).
:- mode peephole__opt_instr(in, in, in, out) is det.

peephole__opt_instr(Instr0, Comment0, Instructions0, Instructions) :-
	(
		peephole__opt_instr_2(Instr0, Comment0, Instructions0,
			Instructions1)
	->
		( Instructions1 = [Instr1 - Comment1 | Instructions2] ->
			peephole__opt_instr(Instr1, Comment1, Instructions2,
				Instructions)
		;
			Instructions = Instructions1
		)
	;
		Instructions = [Instr0 - Comment0 | Instructions0]
	).

%-----------------------------------------------------------------------------%

:- pred peephole__opt_instr_2(instr, string, list(instruction),
				list(instruction)).
:- mode peephole__opt_instr_2(in, in, in, out) is semidet.

	% A `call' followed by a `proceed' can be replaced with a `tailcall'.
	%
	%	call(Foo, &&ret);		tailcall(Foo)
	%     ret:			=>    ret:
	%	proceed				proceed
	%	
	% Note that we can't delete the `ret: proceed', since `ret'
	% might be branched to from elsewhere.

peephole__opt_instr_2(call(CodeAddress, ContLabel), Comment, Instrs0, Instrs) :-
	Instrs0 = [label(ContLabel) - _, proceed - _ | _],
	Instrs = [tailcall(CodeAddress) - Comment | Instrs0 ].

	% if a `mkframe' is immediately followed by a `modframe', we
	% can delete the `modframe' and instead just set the redoip
	% directly in the `mkframe'.
	%
	%	mkframe(D, S, _)	=>	mkframe(D, S, Redoip)
	%	modframe(Redoip)
	%

peephole__opt_instr_2(mkframe(Descr, Slots, _), Comment, Instrs0, Instrs) :-
	Instrs0 = [modframe(Redoip) - _ | Instrs1],
	Instrs = [mkframe(Descr, Slots, Redoip) - Comment | Instrs1].

	% a `goto' can be deleted if the target of the jump is the very
	% next instruction.
	%
	%	goto label;	=>	label:
	%     label:

peephole__opt_instr_2(goto(Label), _, Instrs, Instrs) :-
	Instrs = [label(Label) - _ | _].

	% a conditional branch over a branch can be replaced
	% by an inverse conditional branch
	%
	%	if (x) goto skip;		if (!x) goto somewhere
	%	goto somewhere;		=>    skip:
	%     skip:

peephole__opt_instr_2(if_val(Rval, Skip), _C1, Instrs0, Instrs) :-
	Instrs0 = [goto(Somewhere) - C2, label(Skip) - C3 | Instrs1],
	peephole__neg_rval(Rval, NotRval),
	Instrs = [if_val(NotRval, Somewhere) - C2, label(Skip) - C3 | Instrs1].

%-----------------------------------------------------------------------------%

:- pred peephole__neg_rval(rval, rval).
:- mode peephole__neg_rval(in, out) is det.

peephole__neg_rval(Rval, NegRval) :-
	( Rval = not(NegRval0) ->
		NegRval = NegRval0
	;	
		NegRval = not(Rval)
	).

%-----------------------------------------------------------------------------%

:- end_module peephole.

%-----------------------------------------------------------------------------%
