%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: garbage_out.m
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
% altogether. We also use io__write_anything, which is a bit dodgy,
% and is a real hack.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module garbage_out.
:- interface.

:- import_module hlds_module, llds.
:- import_module int, list, io.

:- type garbage_output --->	garbage_output(
					cont_list, 
					shape_table, 
					abs_exports
				).

:- type cont_list	==	list(gc_label_info).

:- type gc_label_info 	--->	gc_label_info(
					code_addr,
					det,
					num_slots, 
					list(liveinfo)
				).

:- type num_slots	==	int.

:- type det		---> 	deterministic
			;	nondeterministic
			;	commit.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% PUBLIC PREDICATES: 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred garbage_out__do_garbage_out(shape_info, c_file, io__state, io__state).
:- mode garbage_out__do_garbage_out(in, in, di, uo) is det.

:- implementation.

:- import_module string, assoc_list, map, std_util, require.
:- import_module term, term_io, varset.
:- import_module prog_data, type_util, shapes.

%-----------------------------------------------------------------------------%
% Out main predicate, it just collects and outputs the garbage.
% Note, we don't yet get the exported abstract type table.
%-----------------------------------------------------------------------------%
garbage_out__do_garbage_out(ShapeInfo, c_file(Name, _C_Header, Modules)) -->
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
		io__progname_base("garbage_out.m", ProgName),
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
% Create the list of continuations.
%-----------------------------------------------------------------------------%
:- pred garbage_out__create_cont_list(list(c_module), cont_list).
:- mode garbage_out__create_cont_list(in, out) is det.

garbage_out__create_cont_list([], []).
garbage_out__create_cont_list([M |Ms], C_List) :-
        garbage_out__create_cont_list(Ms, Cs),
	( M = c_module(_ModName, C_Procs),
          garbage_out__create_cont_list_2(C_Procs, C),
	  list__append(C, Cs, C_List)
	; M = c_code(_, _),
	  C_List = Cs
	; M = c_export(_),
	  C_List = Cs
	).

%-----------------------------------------------------------------------------%
% Create the list of continuations.
%-----------------------------------------------------------------------------%
:- pred garbage_out__create_cont_list_2(list(c_procedure), cont_list).
:- mode garbage_out__create_cont_list_2(in, out) is det.

garbage_out__create_cont_list_2([], []).
garbage_out__create_cont_list_2([P |Ps], CList) :-
	P = c_procedure(_Name, _Arity, _ModeNum0, Instructions),
	garbage_out__proc_instr_list(Instructions, [], C),
	list__reverse(C, ReverseC),
	garbage_out__create_cont_list_2(Ps, Cs),
	list__append(ReverseC, Cs, CList).

%-----------------------------------------------------------------------------%
% Process the instruction list.
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
			I = call(_Target, Contn, LiveInfo0, _)
		;
			I = call_closure(_, Contn, LiveInfo0)
		)
	->
		garbage_out__remove_fields(LiveInfo0, LiveInfo1),
		garbage_out__get_det(LiveInfo1, none, Det),
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
% Find the determinism of this label by looking for framevars or stackvars
% or succip. If there is no succip, then we assume nondet. 
% XXX Should deal with this properly - commits must be detected also.
%-----------------------------------------------------------------------------%

:- type so_far_det	--->	det ; nondet ; commit ; none.

:- pred garbage_out__get_det(list(liveinfo), so_far_det, det).
:- mode garbage_out__get_det(in, in, out) is det.

garbage_out__get_det([], none, _) :-
	error("garbage_out__get_det: Unable to determine determinism.").
	% nondeterministic is a pretty safe bet though.
garbage_out__get_det([], commit, commit).
garbage_out__get_det([], nondet, nondeterministic).
garbage_out__get_det([], det, deterministic).

garbage_out__get_det([L | Ls], OldD, NewDet) :-
	(
		L = live_lvalue(stackvar(_), _)
	->
		(
			OldD = none,
			Det = det
		;
			OldD = nondet,
			Det = commit
		;
			OldD = det,
			Det = det
		;
			OldD = commit,
			Det = commit
		)
	;
		L = live_lvalue(framevar(_), _)
	->
		(
			OldD = none,
			Det = nondet
		;
			OldD = nondet,
			Det = OldD
		;
			OldD = det,
			Det = commit
		;
			OldD = commit,
			Det = commit
		)
	;
		Det = OldD
	),
	garbage_out__get_det(Ls, Det, NewDet).

%-----------------------------------------------------------------------------%
% Actually write the garbage information.
%-----------------------------------------------------------------------------%
:- pred garbage_out__output(cont_list, shape_table, abs_exports, 
				io__state, io__state).
:- mode garbage_out__output(in, in, in, di, uo) is det.

garbage_out__output(List, Shapes, Abs_Exports) --> 
	garbage_out__write_cont_list(List),
	garbage_out__write_shape_table(Shapes),
	{ map__to_assoc_list(Abs_Exports, Abs_Exports_List) },
	garbage_out__write_abs_exports(Abs_Exports_List).

%-----------------------------------------------------------------------------%
% Write the continuation list.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_cont_list(cont_list, io__state, io__state).
:- mode garbage_out__write_cont_list(in, di, uo) is det.

garbage_out__write_cont_list([]) --> { true }.
garbage_out__write_cont_list([G|Gs]) -->
	{ G = gc_label_info(Code_Addr, Det, Num_Slots, Live_Info_List) },
	io__write_string("continuation("),
	garbage_out__write_code_addr(Code_Addr),
	garbage_out__write_det(Det),
	io__write_string(", "),
	io__write_int(Num_Slots),
	io__write_string(", ["),
	garbage_out__write_liveinfo_list(Live_Info_List),
	io__write_string("]).\n"),
	garbage_out__write_cont_list(Gs).

:- pred garbage_out__write_det(det, io__state, io__state).
:- mode garbage_out__write_det(in, di, uo) is det.
garbage_out__write_det(deterministic) -->
	io__write_string(", deterministic").
garbage_out__write_det(nondeterministic) -->
	io__write_string(", nondeterministic").
garbage_out__write_det(commit) -->
	io__write_string(", commit").


%-----------------------------------------------------------------------------%
% Perhaps write a comma and a newline, this is used as a sort of 'if
% there is another item...'
%-----------------------------------------------------------------------------%
:- pred garbage_out__maybe_write_comma_newline(list(T), io__state, io__state).
:- mode garbage_out__maybe_write_comma_newline(in, di, uo) is det.
garbage_out__maybe_write_comma_newline([]) --> { true }.
garbage_out__maybe_write_comma_newline([_ | _]) --> io__write_string(",\n").

%-----------------------------------------------------------------------------%
% Perhaps write a comma this is used as a sort of 'if there is another item...'
%-----------------------------------------------------------------------------%
:- pred garbage_out__maybe_write_comma(list(T), io__state, io__state).
:- mode garbage_out__maybe_write_comma(in, di, uo) is det.
garbage_out__maybe_write_comma([]) --> { true }.
garbage_out__maybe_write_comma([_ | _]) --> io__write_string(",").

%-----------------------------------------------------------------------------%
% Perhaps write a comma and a space 
%-----------------------------------------------------------------------------%
:- pred garbage_out__maybe_write_comma_space(list(T), io__state, io__state).
:- mode garbage_out__maybe_write_comma_space(in, di, uo) is det.
garbage_out__maybe_write_comma_space([]) --> { true }.
garbage_out__maybe_write_comma_space([_ | _]) --> io__write_string(", ").

%-----------------------------------------------------------------------------%
% Write a continuation label (don't write anything that isn't a label).
% XXX Should we be getting imported labels here? I have assumed not.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_code_addr(code_addr, io__state, io__state).
:- mode garbage_out__write_code_addr(in, di, uo) is det.
garbage_out__write_code_addr(L) -->
	( 
		{ L = label(Label) }
	->
		output_label(Label)
	;
		{ error("garbage_out : Unexpected code_addr type") }
	).


%-----------------------------------------------------------------------------%
% Write the liveinfo list (a list of lvals and corresponding
% shape numbers).
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_liveinfo_list(list(liveinfo), io__state, io__state). 
:- mode garbage_out__write_liveinfo_list(in, di, uo) is det. 
garbage_out__write_liveinfo_list([]) --> { true }.
garbage_out__write_liveinfo_list([live_lvalue(L, S)| Ls]) --> 
	garbage_out__write_liveval(L),
	io__write_string(" - "),
	shapes__write_shape_num(S),
	garbage_out__maybe_write_comma_space(Ls),
	garbage_out__write_liveinfo_list(Ls).

%-----------------------------------------------------------------------------%
% Write a single lval.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_liveval(lval, io__state, io__state).
:- mode garbage_out__write_liveval(in, di, uo) is det.

garbage_out__write_liveval(hp) --> io__write_string("hp").
garbage_out__write_liveval(sp) --> io__write_string("sp").
garbage_out__write_liveval(succip) --> io__write_string("succip").
	% XXX possibly the next three lines are wrong - they should not
	% ignore the argument of redoip/succfr/prevfr
garbage_out__write_liveval(redoip(_)) --> io__write_string("redoip").
garbage_out__write_liveval(succip(_)) --> io__write_string("succip_slot").
garbage_out__write_liveval(succfr(_)) --> io__write_string("succfr").
garbage_out__write_liveval(prevfr(_)) --> io__write_string("prevfr").
garbage_out__write_liveval(curfr) --> io__write_string("curfr").
garbage_out__write_liveval(maxfr) --> io__write_string("maxfr").
garbage_out__write_liveval(stackvar(X)) --> 
	io__write_string("stackvar("),
	io__write_int(X),
	io__write_string(")").
garbage_out__write_liveval(framevar(X)) --> 
	io__write_string("framevar("),
	io__write_int(X),
	io__write_string(")").
garbage_out__write_liveval(reg(X)) --> 
	(
		{ X = r(Y) }
	->
		io__write_string("reg("),
		io__write_int(Y)
	;
		{ X = f(Y) }
	->
		io__write_string("freg("),
		io__write_int(Y)
	;
		{ error("garbage_out: Unexpected reg type, not f/1 or r/1") }
	),
	io__write_string(")").
garbage_out__write_liveval(field(_,_,_)) --> 
	{ error("garbage_out: Unexpected 'field/3' lval") }.
garbage_out__write_liveval(lvar(_)) --> 
	{ error("garbage_out: Unexpected 'lval/1' lval") }.
garbage_out__write_liveval(temp(_)) --> 
	{ error("garbage_out: Unexpected 'temp/1' lval") }.


%-----------------------------------------------------------------------------%
% We no longer care what the shape_ids are, as we don't need them. When we
% get to putting all the modules together, we can find a shape merely
% by knowing which module it belongs to, and what shape it is. Exported 
% abstract types will have to have a map from shape_id to 
% pair(module,shape_num), but that happens later.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_shape_table(shape_table, io__state, io__state).
:- mode garbage_out__write_shape_table(in, di, uo) is det.
garbage_out__write_shape_table(ShapeTable - _NextNum) -->
	{ map__values(ShapeTable, Shapes) },
	{ list__sort_and_remove_dups(Shapes, Sort_Shapes) },
	garbage_out__write_shapes(Sort_Shapes).


%-----------------------------------------------------------------------------%
% Write out the list of shapes.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_shapes(list(pair(shape_num, shape)), 
					io__state, io__state).
:- mode garbage_out__write_shapes(in, di, uo) is det.

garbage_out__write_shapes([]) --> { true }.
garbage_out__write_shapes([ShapeNum - Shape | Shapes]) --> 
	io__write_string("shapeinfo("),
	shapes__write_shape_num(ShapeNum),
	io__write_string(", "),
	garbage_out__write_shape(Shape),
	io__write_string(").\n"),
	garbage_out__write_shapes(Shapes).

%-----------------------------------------------------------------------------%
% Write a shape.
% We don't write the type of a polymorphic type, as I don't think we
% need it.
% XXX When writing out abstract shapes, we can do a bit better.
%     We can write the arguments of the shape as shape numbers too,
%     as they are often not abstract types themselves, eg pair(int, int).
%     We should add another argument of the abstract/1 functor, which
%     holds a list of shape numbers of the arguments to this functor.
%     But first we need to know --- are they in the shape table already?
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_shape(shape, io__state, io__state).
:- mode garbage_out__write_shape(in, di, uo) is det.
garbage_out__write_shape(quad(S1, S2, S3, S4)) -->
	io__write_string("quad("),
	garbage_out__write_shape_tag(S1),
	io__write_string(", "),
	garbage_out__write_shape_tag(S2),
	io__write_string(", "),
	garbage_out__write_shape_tag(S3),
	io__write_string(", "),
	garbage_out__write_shape_tag(S4),
	io__write_string(")").
garbage_out__write_shape(abstract(Type, Shape_List)) -->
	io__write_string("abstract("),
	garbage_out__write_type(Type),
	io__write_string(", ["),
	garbage_out__write_int_list(Shape_List),
	io__write_string("])").
garbage_out__write_shape(polymorphic(_Type, Var)) -->
	io__write_string("polymorphic("),
	io__write_int(Var),
	io__write_string(")").
garbage_out__write_shape(closure(Type)) -->
	io__write_string("closure("),
	garbage_out__write_type(Type),
	io__write_string(")").
garbage_out__write_shape(equivalent(ShapeNum)) -->
	io__write_string("equivalent("),
	shapes__write_shape_num(ShapeNum),
	io__write_string(")").


%-----------------------------------------------------------------------------%
% Write a shape_tag. A shape tag is either a constant tag, or
% a simple tag, consisting of a list of arguments of that functor,
% or a complicated tag, which consists of a list of lists of 
% functor arguments, since all those functors share the same tag.
% If it is complicated, we write out a list of simple shape tags,
% eg complicated([simple([32,65,11]), simple([11])) etc.
% XXX is it possible for a simple or a complicated to have an empty 
% list? Should be check for this?
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_shape_tag(shape_tag, io__state, io__state).
:- mode garbage_out__write_shape_tag(in, di, uo) is det.
garbage_out__write_shape_tag(constant) --> 
	io__write_string("constant").
garbage_out__write_shape_tag(simple(Shape_List)) --> 
	io__write_string("simple(["),
	garbage_out__write_shape_list(Shape_List),
	io__write_string("])").
garbage_out__write_shape_tag(complicated(Shape_List_List)) -->
	io__write_string("complicated(["),
	garbage_out__write_complicated(Shape_List_List),
	io__write_string("])").


%-----------------------------------------------------------------------------%
% Write a complicated shape tag.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_complicated(list(list(pair(shape_num, shape_id))),
					 io__state, io__state).
:- mode garbage_out__write_complicated(in, di, uo) is det.
garbage_out__write_complicated([]) --> { true }.
garbage_out__write_complicated([Simple | Complicateds]) -->
	garbage_out__write_shape_tag(simple(Simple)),
	garbage_out__maybe_write_comma_space(Complicateds),
	garbage_out__write_complicated(Complicateds).


%-----------------------------------------------------------------------------%
% XXX This comment is wrong.
% Write a type (actually, only write a type_id).
% For the moment this is only used for polymorphics and
% abstract types. For abstract types, we need the type_id so we can
% find the actually definition in the type table at some later stage.
% For polymorphics, we will handle at run time, so we don't really care
% what the types are. For something like bintree(K, V), we get the type_id 
% bintree/2, but the parameter is always treated as polymorphic, even
% if we knew it, because the module system hides bintree from us. Possibly
% at some later date we should write the arguments, so that bintree(int, int)
% can be done easily by filling in the polymorphic arguments, but that
% is a bit tricky... We need more info about the abstract exported types
% in order to do that.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_type(type, io__state, io__state).
:- mode garbage_out__write_type(in, di, uo) is det.
garbage_out__write_type(Type) -->
	{ varset__init(Varset) },
	term_io__write_term(Varset, Type).


%-----------------------------------------------------------------------------%
% Write out the shape list.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_shape_list(list(pair(shape_num, shape_id)), 
					io__state, io__state).
:- mode garbage_out__write_shape_list(in, di, uo) is det.
garbage_out__write_shape_list([]) --> {true}.
garbage_out__write_shape_list([ShapeNum - _ShapeId | Shape_List]) -->
	shapes__write_shape_num(ShapeNum),
	garbage_out__maybe_write_comma_space(Shape_List),
	garbage_out__write_shape_list(Shape_List).

%-----------------------------------------------------------------------------%
% Write a list of integers. 
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_int_list(list(shape_num), io__state, io__state).
:- mode garbage_out__write_int_list(in, di, uo) is det.
garbage_out__write_int_list([]) --> {true}.
garbage_out__write_int_list([ShapeNum | Shape_List]) -->
	shapes__write_shape_num(ShapeNum),
	garbage_out__maybe_write_comma_space(Shape_List),
	garbage_out__write_int_list(Shape_List).

%-----------------------------------------------------------------------------%
% Write out the abstract export table (all the shapes that are exported from
% this module that could be abstracts in another module).
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_abs_exports(list(pair(type_id, maybe_shape_num)),
		io__state, io__state).
:- mode garbage_out__write_abs_exports(in, di, uo) is det.
garbage_out__write_abs_exports(AE_List) -->
	garbage_out__write_abs_list(AE_List).

%-----------------------------------------------------------------------------%
% Output each item in the abstract exports list.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_abs_list(list(pair(type_id, maybe_shape_num)),
		io__state, io__state).
:- mode garbage_out__write_abs_list(in, di, uo) is det.
garbage_out__write_abs_list([]) --> [].
garbage_out__write_abs_list([T_Id - M_SN | As]) -->
	io__write_string("abs_info("),
	garbage_out__write_type_id(T_Id),
	(
		{ M_SN = no(Type) },
		io__write_string(", no("),
		garbage_out__write_type(Type)
	;
		{ M_SN = yes(S_Num) },
		io__write_string(", yes("),
		shapes__write_shape_num(S_Num)
	),
	io__write_string(")).\n"),
	garbage_out__write_abs_list(As).

%-----------------------------------------------------------------------------%
% Write a type id out.
%-----------------------------------------------------------------------------%
:- pred garbage_out__write_type_id(type_id, io__state, io__state).
:- mode garbage_out__write_type_id(in, di, uo) is det.
garbage_out__write_type_id(unqualified(TypeName) - Arity) -->
	io__write_strings(["unqualified(", TypeName, ")", " - "]),
	io__write_int(Arity).
garbage_out__write_type_id(qualified(Module,TypeName) - Arity) -->
	io__write_strings(["qualified(", Module, ", ", TypeName, ") - "]),
	io__write_int(Arity).
