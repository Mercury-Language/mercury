%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% File: garbage_out.nl
% Main author: trd 
%
% This module creates the label table information and outputs it, for
% use by the link tool. It then appends the shape table to the end of
% this.
%
% We traverse the llds, and grab all the continuation labels and their
% corresponding shape information. 
%
% We don't yet handle some of the optimizations that mercury can throw
% at us - eg middle recursion optimization removes the stack frame 
% altogether.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module garbage_out.
:- interface.

:- import_module int, map, std_util, list, io, hlds, require,
		 llds, prog_io, type_util, string, term, shapes.


:- type garbage_output --->	garbage_output( cont_list, 
						shape_table, 
						abs_exports).

:- type cont_list	==	list(gc_label_info).

:- type gc_label_info 	--->	gc_label_info(code_addr, det, int, 
					list(liveinfo)).

:- type det		---> 	deterministic
			;	semideterministic % really just semidet
			;	nondeterministic.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% PUBLIC PREDICATES: 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred garbage_out__do_garbage_out(shape_info, c_file, io__state, io__state).
:- mode garbage_out__do_garbage_out(in, in, di, uo) is det.

:- implementation.

%-----------------------------------------------------------------------------%
% Out main predicate, it just collects and outputs the garbage.
% Note, we don't yet get the exported abstract type table.
%-----------------------------------------------------------------------------%
garbage_out__do_garbage_out(ShapeInfo, c_file(Name, Modules)) -->
	{ ShapeInfo = shape_info(ShapeTable, Abs_Exports) },
	{ string__append(Name, ".garb", FileName) },
	io__tell(FileName, Result),
	(
		{ Result = ok }
	->
		{ garbage_out__create_cont_list(Modules, CList) },
		garbage_out__output(CList, ShapeTable, Abs_Exports),
		io__told
	;
		io__progname("garbage_out.nl", ProgName),
                io__write_string("\n"),
                io__write_string(ProgName),
                io__write_string(": can't open `"),
                io__write_string(FileName),
                io__write_string("' for output\n")
        ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% LOCAL PREDICATES: 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%
% Want to output only the livevals for call, attach the continuation to them.
%-----------------------------------------------------------------------------%
:- pred garbage_out__create_cont_list(list(c_module), cont_list).
:- mode garbage_out__create_cont_list(in, out) is det.

garbage_out__create_cont_list([], []).
garbage_out__create_cont_list([M |Ms], C_List) :-
        garbage_out__create_cont_list(Ms, Cs),
	M = c_module(_ModName, C_Procs),
        garbage_out__create_cont_list_2(C_Procs, C),
	list__append(C, Cs, C_List).

%-----------------------------------------------------------------------------%
% Want to output only the livevals for call, attach the continuation to them.
%-----------------------------------------------------------------------------%
:- pred garbage_out__create_cont_list_2(list(c_procedure), cont_list).
:- mode garbage_out__create_cont_list_2(in, out) is det.

garbage_out__create_cont_list_2([], []).
garbage_out__create_cont_list_2([P |Ps], C_List) :-
	garbage_out__create_cont_list_2(Ps, Cs),
	P = c_procedure(_Name, _Arity, _ModeNum0, Instructions),
	garbage_out__proc_instr_list(Instructions, [], C),
	list__append(C, Cs, C_List).

%-----------------------------------------------------------------------------%
% Want to output only the livevals for call, attach the continuation to them.
%-----------------------------------------------------------------------------%
:- pred garbage_out__proc_instr_list(list(instruction), cont_list,  cont_list).
:- mode garbage_out__proc_instr_list(in, in, out) is det.

garbage_out__proc_instr_list([], Cs, Cs).
garbage_out__proc_instr_list([I - _Comment | Is ], Cs0, Cout) :-
	garbage_out__proc_instr(I, Cs0, Cs1),
	garbage_out__proc_instr_list(Is, Cs1, Cout). 

%-----------------------------------------------------------------------------%
% Process an instruction - find the gc_live_info and prepend it if it is a
% call, otherwise ignore it. 
%-----------------------------------------------------------------------------%
:- pred garbage_out__proc_instr(instr, cont_list,  cont_list).
:- mode garbage_out__proc_instr(in, in, out) is det.

garbage_out__proc_instr(I, Cs, Cout) :- 
	(
		(
			I = call(_Target, Contn, _, LiveInfo0)
		;
			I = call_closure(_, Contn, LiveInfo0)
		)
	->
		garbage_out__remove_fields(LiveInfo0, LiveInfo1),
		garbage_out__get_det(LiveInfo1, Det),
		list__length(LiveInfo1, Length),
		C = gc_label_info(Contn, Det, Length, LiveInfo1),
		Cout = [C | Cs]
	;
		Cout = Cs
	).

%-----------------------------------------------------------------------------%
% Strip the (erroneously present) fields(...) from the liveinfo. 
%-----------------------------------------------------------------------------%
:- pred garbage_out__remove_fields(list(liveinfo), list(liveinfo)).
:- mode garbage_out__remove_fields(in, out) is det.

garbage_out__remove_fields([], []).
garbage_out__remove_fields([L|Ls], Ms) :-
	garbage_out__remove_fields(Ls, Xs),
	(
		L = live_lvalue(field(_, _, _), _)
	->
		Ms = Xs
	;
		Ms = [L | Xs]
	).



%-----------------------------------------------------------------------------%
% Find the size of the stack frame (at the moment we assume that 
% size == maximum live 
%-----------------------------------------------------------------------------%




%-----------------------------------------------------------------------------%
% Find the determinism of this label by looking for framevars or stackvars
% or succip. If there is no succip, then we assume nondet. 
%-----------------------------------------------------------------------------%
:- pred garbage_out__get_det(list(liveinfo), det).
:- mode garbage_out__get_det(in, out) is det.

garbage_out__get_det([], nondeterministic).
garbage_out__get_det([L | Ls], Det) :-
	(
		(
			L = live_lvalue(stackvar(_), _)
		;
			L = live_lvalue(succip, _)
		)
	->
		Det = deterministic
	;
		(
			L = live_lvalue(framevar(_), _)
		;
			L = live_lvalue(maxfr, _)
		;
			L = live_lvalue(redoip(_), _)
		)
	->
		Det = nondeterministic
	;
		garbage_out__get_det(Ls, Det)
	).

%-----------------------------------------------------------------------------%
%
%-----------------------------------------------------------------------------%
:- pred garbage_out__output(cont_list, shape_table, abs_exports, 
				io__state, io__state).
:- mode garbage_out__output(in, in, in, di, uo) is det.

garbage_out__output(List, Shapes, Abs_Exports) --> 
	{ Garbage = garbage_output(List, Shapes, Abs_Exports) },
	io__write_anything(Garbage),
	io__write_string(".\n").


