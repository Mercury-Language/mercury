%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Profiling module
% Main author: petdr.
%
% Notes:
%	Add's profiling information to the labels so that during the
%	final output of the code there is profiling code generated. 
%
%	XXX Garbage collection (shapes.m) identifies all the continuation
%	label's.  At a later date this phase should be merged with the gc
%	phase.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prof.

:- interface.

:- import_module llds, io, list, bintree_set.

	% Used by the back-end call by phases.
:- pred prof__main(list(c_procedure), list(c_procedure), io__state, io__state).
:- mode prof__main(in, out, di, uo) is det.

	% Used by the back-end call by preds.
:- pred prof__proc(c_procedure, c_procedure, io__state, io__state).
:- mode prof__proc(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%


:- implementation.

:- import_module globals, options.
:- import_module int, std_util, require.
:- import_module opt_util.


%-----------------------------------------------------------------------------%

prof__main([], []) --> [].
prof__main([P0|Ps0], [P|Ps]) -->
	io__write_string("In prof__main\n"),
	prof__proc(P0, P),
	prof__main(Ps0, Ps).

prof__proc(c_procedure(Name, Arity, Mode, Instructions0),
		c_procedure(Name, Arity, Mode, Instructions)) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	prof__locate_labels(VeryVerbose, Instructions0, LabelSet),
	prof__mark_labels(LabelSet, Instructions0, Instructions).


%-----------------------------------------------------------------------------%o


% prof__locate_labels:
%	Locate all the labels which are the continutation labels for calls or
%	nondet disjunctions, and store them in LabelSet.

:- pred prof__locate_labels(bool, list(instruction), bintree_set(label),
		io__state, io__state).
:- mode prof__locate_labels(in, in, out, di, uo) is det.

prof__locate_labels(_VeryVerbose, [], LabelSet) -->
	{ bintree_set__init(LabelSet) }.
prof__locate_labels(VeryVerbose, [Instr | Instrs], LabelSet) -->
	{ bintree_set__init(LabelSet0) },
	(
		{ Instr = call(_, label(ContLabel), _, _) - _Comment0 }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		{ Instr = assign(redoip(lval(maxfr)), 
			const(address_const(label(ContLabel)))) - _Comment1 }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		{ Instr = mkframe(_Comment2, _SlotCount, label(ContLabel)) 
				- _C }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		{ Instr = modframe(label(ContLabel)) - _Comment }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet0, LabelSet)
	).

:- pred prof__locate_labels_2(bool, list(instruction), bintree_set(label),
		bintree_set(label), io__state, io__state).
:- mode prof__locate_labels_2(in, in, in, out, di, uo) is det.

prof__locate_labels_2(_VeryVerbose, [], LS, LS, IO, IO).
prof__locate_labels_2(VeryVerbose, [Instr | Instrs], LabelSet0, LabelSet) -->
	(
		{ Instr = call(_, label(ContLabel), _, _) - _Comment0 }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		{ Instr = assign(redoip(lval(maxfr)), 
			const(address_const(label(ContLabel)))) - _Comment1 }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		{ Instr = mkframe(_Comment2, _SlotCount, label(ContLabel)) 
				- _C }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		{ Instr = modframe(label(ContLabel)) - _Comment }
	->
		{ bintree_set__insert(LabelSet0, ContLabel, LabelSet1) },
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet1, LabelSet)
	;
		prof__locate_labels_2(VeryVerbose, Instrs, LabelSet0, LabelSet)
	).


% prof__mark_labels:
%	Marks all labels which are accessible externally
%	NB. A continuation label should only be a internal label.

:- pred prof__mark_labels(bintree_set(label) ,list(instruction), 
				list(instruction), io__state, io__state).
:- mode prof__mark_labels(in, in, out, di, uo) is det.

prof__mark_labels(_LabelSet, [], [], IO, IO).
prof__mark_labels(LabelSet, [Instr0 | Instrs0], [Instr | Instrs]) -->
	prof__mark_labels(LabelSet, Instrs0, Instrs),
	(
		{ Instr0 = label(ContLabel) - Comment }
	->
		(
			{ bintree_set__member(ContLabel, LabelSet) }
		->
			(
				{ ContLabel = local(ProcLabel, N, _ContType) }
			->
				{ Instr = label(local(ProcLabel, N, local)) - Comment }
			;
				{ error("prof__mark_labels: Label not of type local") }
			)
		;
			{ Instr = Instr0 }
		)
	;
		{ Instr = Instr0 }
	).
