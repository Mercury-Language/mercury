%-----------------------------------------------------------------------------%

% Main author: fjh.

% This program converts the parse tree structure provided by prog_io
% back into Mercury source text.

%-----------------------------------------------------------------------------%

:- module mercury_to_mercury.
:- interface.

:- import_module list, string, hlds, io, prog_io, varset, term.

:- pred convert_to_mercury(string, string, list(item_and_context),
				io__state, io__state).
:- mode convert_to_mercury(in, in, in, di, uo) is det.

:- pred mercury_output_pred_type(varset, sym_name, list(type),
		term__context, io__state, io__state).
:- mode mercury_output_pred_type(in, in, in, in, di, uo) is det.

:- pred mercury_output_mode_decl(varset, sym_name, list(mode), determinism,
			term__context, io__state, io__state).
:- mode mercury_output_mode_decl(in, in, in, in, in, di, uo) is det.

:- pred mercury_output_inst(inst, varset, io__state, io__state).
:- mode mercury_output_inst(in, in, di, uo) is det.

:- pred mercury_output_inst_list(list(inst), varset, io__state, io__state).
:- mode mercury_output_inst_list(in, in, di, uo) is det.

:- pred mercury_output_mode_list(list(mode), varset, io__state, io__state).
:- mode mercury_output_mode_list(in, in, di, uo) is det.

	% output a comma-separated list of variables

:- pred mercury_output_vars(list(var), varset, io__state, io__state).
:- mode mercury_output_vars(in, in, di, uo) is det.

:- pred mercury_output_var(var, varset, io__state, io__state).
:- mode mercury_output_var(in, in, di, uo) is det.

:- pred mercury_output_term(term, varset, io__state, io__state).
:- mode mercury_output_term(in, in, di, uo) is det.

:- pred mercury_output_hlds_goal(hlds__goal, varset, int, io__state, io__state).
:- mode mercury_output_hlds_goal(in, in, in, di, uo) is det.

:- pred mercury_output_newline(int, io__state, io__state).
:- mode mercury_output_newline(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, set, prog_out, prog_util, term_io, std_util.
:- import_module globals, options.

%-----------------------------------------------------------------------------%

convert_to_mercury(ProgName, OutputFileName, Items) -->
	io__stderr_stream(StdErr),
	io__tell(OutputFileName, Res),
	( { Res = ok } ->
		globals__lookup_option(verbose, bool(Verbose)),
		( { Verbose = yes } ->
			io__write_string(StdErr, "% Writing output to "),
			io__write_string(StdErr, OutputFileName),
			io__write_string(StdErr, "...\n")
		;
			[]
		),
		io__write_string(":- module "),
		mercury_output_bracketed_constant(term__atom(ProgName)),
		io__write_string(".\n"),
		mercury_output_item_list(Items),
		( { Verbose = yes } ->
			io__write_string(StdErr, "% done\n")
		;
			[]
		),
		io__told
	;
		io__write_string(StdErr, "Error: couldn't open file `"),
		io__write_string(StdErr, OutputFileName),
		io__write_string(StdErr, "' for output.\n")
	).

%-----------------------------------------------------------------------------%

	% output the declarations one by one 

:- pred mercury_output_item_list(list(item_and_context), io__state, io__state).
:- mode mercury_output_item_list(in, di, uo) is det.

mercury_output_item_list([]) --> [].
mercury_output_item_list([Item - Context | Items]) -->
	( mercury_output_item(Item, Context) ->
		[]
	;
		% mercury_output_item should always succeed
		% if it fails, report an internal error
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("mercury_to_mercury internal error.\n"),
		io__write_string("Failed to process the following item:\n"),
		io__write_anything(Item),
		io__write_string("\n"),
		io__set_output_stream(OldStream, _)
	),
	mercury_output_item_list(Items).

%-----------------------------------------------------------------------------%

:- pred mercury_output_item(item, term__context, io__state, io__state).
:- mode mercury_output_item(in, in, di, uo) is det.

	% dispatch on the different types of items

mercury_output_item(type_defn(VarSet, TypeDefn, _Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_type_defn(VarSet, TypeDefn, Context).

mercury_output_item(inst_defn(VarSet, InstDefn, _Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_inst_defn(VarSet, InstDefn, Context).

mercury_output_item(mode_defn(VarSet, ModeDefn, _Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_mode_defn(VarSet, ModeDefn, Context).

mercury_output_item(pred(VarSet, PredName, TypesAndModes, Det, _Cond), Context)
		-->
	maybe_output_line_number(Context),
	mercury_output_pred_decl(VarSet, PredName, TypesAndModes, Det, Context).

mercury_output_item(mode(VarSet, PredName, Modes, Det, _Cond), Context) -->
	maybe_output_line_number(Context),
	mercury_output_mode_decl(VarSet, PredName, Modes, Det, Context).

mercury_output_item(module_defn(VarSet, ModuleDefn), Context) -->
	maybe_output_line_number(Context),
	mercury_output_module_defn(VarSet, ModuleDefn, Context).

mercury_output_item(clause(VarSet, PredName, Args, Body), Context) -->
	maybe_output_line_number(Context),
	mercury_output_clause(VarSet, PredName, Args, Body, Context).

mercury_output_item(nothing, _) --> [].

%-----------------------------------------------------------------------------%

:- pred mercury_output_module_defn(varset, module_defn, term__context,
			io__state, io__state).
:- mode mercury_output_module_defn(in, in, in, di, uo) is det.

mercury_output_module_defn(_VarSet, Module, _Context) -->
	( { Module = import(module(ImportedModules)) } ->
		io__write_string(":- import_module "),
		mercury_write_module_spec_list(ImportedModules),
		io__write_string(".\n")
	; { Module = interface } ->
		io__write_string(":- interface.\n")
	; { Module = implementation } ->
		io__write_string(":- implementation.\n")
	;
		% XXX unimplemented
		io__write_string("% unimplemented module declaration\n")
	).

:- pred mercury_write_module_spec_list(list(module_specifier),
					io__state, io__state).
:- mode mercury_write_module_spec_list(in, di, uo) is det.

mercury_write_module_spec_list([]) --> [].
mercury_write_module_spec_list([ModuleName | ModuleNames]) -->
	mercury_output_bracketed_constant(term__atom(ModuleName)),
	( { ModuleNames = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_write_module_spec_list(ModuleNames)
	).

:- pred mercury_output_inst_defn(varset, inst_defn, term__context,
			io__state, io__state).
:- mode mercury_output_inst_defn(in, in, in, di, uo) is det.

:- mercury_output_inst_defn(_, X, _, _, _) when X.	% NU-Prolog indexing

mercury_output_inst_defn(VarSet, abstract_inst(Name, Args), Context) -->
	io__write_string(":- inst ("),
	{ unqualify_name(Name, Name2) },
	mercury_output_term(term__functor(term__atom(Name2), Args, Context),
			VarSet),
	io__write_string(").\n").
mercury_output_inst_defn(VarSet, eqv_inst(Name, Args, Body), Context) -->
	io__write_string(":- inst ("),
	{ unqualify_name(Name, Name2) },
	mercury_output_term(term__functor(term__atom(Name2), Args, Context),
			VarSet),
	io__write_string(") = "),
	mercury_output_inst(Body, VarSet),
	io__write_string(".\n").

mercury_output_inst_list([], _) --> [].
mercury_output_inst_list([Inst | Insts], VarSet) -->
	mercury_output_inst(Inst, VarSet),
	( { Insts = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_inst_list(Insts, VarSet)
	).

mercury_output_inst(free, _) -->
	io__write_string("free").
mercury_output_inst(bound(BoundInsts), VarSet) -->
	io__write_string("bound("),
	mercury_output_bound_insts(BoundInsts, VarSet),
	io__write_string(")").
mercury_output_inst(ground, _) -->
	io__write_string("ground").
mercury_output_inst(inst_var(Var), VarSet) -->
	mercury_output_var(Var, VarSet).
mercury_output_inst(abstract_inst(Name, Args), VarSet) -->
	mercury_output_inst_name(user_inst(Name, Args), VarSet).
mercury_output_inst(defined_inst(InstName), VarSet) -->
	mercury_output_inst_name(InstName, VarSet).
mercury_output_inst(not_reached, _) -->
	io__write_string("not_reached").

:- pred mercury_output_inst_name(inst_name, varset, io__state, io__state).
:- mode mercury_output_inst_name(in, in, di, uo).

mercury_output_inst_name(user_inst(Name, Args), VarSet) -->
	( { Args = [] } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_inst_list(Args, VarSet),
		io__write_string(")")
	).
mercury_output_inst_name(merge_inst(InstA, InstB), VarSet) -->
	io__write_string("$list__merge("),
	mercury_output_inst_list([InstA, InstB], VarSet),
	io__write_string(")").
mercury_output_inst_name(unify_inst(Liveness, InstA, InstB), VarSet) -->
	io__write_string("$unify("),
	( { Liveness = live } ->
		io__write_string("live, ")
	;
		io__write_string("dead, ")
	),
	mercury_output_inst_list([InstA, InstB], VarSet),
	io__write_string(")").
mercury_output_inst_name(ground_inst(InstName), VarSet) -->
	io__write_string("$ground("),
	mercury_output_inst_name(InstName, VarSet),
	io__write_string(")").

:- pred mercury_output_bound_insts(list(bound_inst), varset, io__state,
		io__state).
:- mode mercury_output_bound_insts(in, in, di, uo) is det.

mercury_output_bound_insts([], _) --> [].
mercury_output_bound_insts([functor(Name, Args) | BoundInsts], VarSet) -->
	( { Args = [] } ->
		mercury_output_bracketed_constant(Name)
	;
		term_io__write_constant(Name),
		io__write_string("("),
		mercury_output_inst_list(Args, VarSet),
		io__write_string(")")
	),
	( { BoundInsts = [] } ->
		[]
	;
		io__write_string(" ; "),
		mercury_output_bound_insts(BoundInsts, VarSet)
	).

:- pred mercury_output_mode_defn(varset, mode_defn, term__context,
			io__state, io__state).
:- mode mercury_output_mode_defn(in, in, in, di, uo) is det.

:- mercury_output_mode_defn(_, X, _, _, _) when X. 	% NU-Prolog indexing.

mercury_output_mode_defn(VarSet, eqv_mode(Name, Args, Mode), Context) -->
	io__write_string(":- mode ("),
	{ unqualify_name(Name, Name2) },
	mercury_output_term(term__functor(term__atom(Name2), Args, Context),
			VarSet),
	io__write_string(") :: "),
	mercury_output_mode(Mode, VarSet),
	io__write_string(".\n").

mercury_output_mode_list([], _VarSet) --> [].
mercury_output_mode_list([Mode | Modes], VarSet) -->
	mercury_output_mode(Mode, VarSet),
	( { Modes = [] } ->
		[]
	;
		io__write_string(", "),
		mercury_output_mode_list(Modes, VarSet)
	).

:- pred mercury_output_mode(mode, varset, io__state, io__state).
:- mode mercury_output_mode(in, in, di, uo) is det.

mercury_output_mode(InstA -> InstB, VarSet) -->
	io__write_string("("),
	mercury_output_inst(InstA, VarSet),
	io__write_string(" -> "),
	mercury_output_inst(InstB, VarSet),
	io__write_string(")").
mercury_output_mode(user_defined_mode(Name, Args), VarSet) -->
	( { Args = [] } ->
		mercury_output_bracketed_sym_name(Name)
	;
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_inst_list(Args, VarSet),
		io__write_string(")")
	).

%-----------------------------------------------------------------------------%

:- pred mercury_output_type_defn(varset, type_defn, term__context,
			io__state, io__state).
:- mode mercury_output_type_defn(in, in, in, di, uo) is det.

mercury_output_type_defn(VarSet, TypeDefn, Context) -->
	mercury_output_type_defn_2(TypeDefn, VarSet, Context).

:- pred mercury_output_type_defn_2(type_defn, varset, term__context,
			io__state, io__state).
:- mode mercury_output_type_defn_2(in, in, in, di, uo) is det.

mercury_output_type_defn_2(uu_type(_Name, _Args, _Body), _VarSet, Context) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string("warning: undiscriminated union types not yet supported.\n"),
	io__set_output_stream(OldStream, _).

mercury_output_type_defn_2(abstract_type(Name, Args), VarSet, Context) -->
	io__write_string(":- type ("),
	{ unqualify_name(Name, Name2) },
	mercury_output_term(term__functor(term__atom(Name2), Args, Context),
			VarSet),
	io__write_string(").\n").

mercury_output_type_defn_2(eqv_type(Name, Args, Body), VarSet, Context) -->
	io__write_string(":- type ("),
	{ unqualify_name(Name, Name2) },
	mercury_output_term(term__functor(term__atom(Name2), Args, Context),
			VarSet),
	io__write_string(") == "),
	mercury_output_term(Body, VarSet),
	io__write_string(".\n").

mercury_output_type_defn_2(du_type(Name, Args, Ctors), VarSet, Context) -->
	io__write_string(":- type ("),
	{ unqualify_name(Name, Name2) },
	mercury_output_term(term__functor(term__atom(Name2), Args, Context),
			VarSet),
	io__write_string(")\n\t--->\t"),
	mercury_output_ctors(Ctors, VarSet),
	io__write_string(".\n").

:- pred mercury_output_ctors(list(constructor), varset,
				io__state, io__state).
:- mode mercury_output_ctors(in, in, di, uo) is det.

mercury_output_ctors([], _) --> [].
mercury_output_ctors([Name - ArgTypes | Ctors], VarSet) -->
	% we need to quote ';'/2 and '{}'/2
	{ list__length(ArgTypes, Arity) },
	(
		{ Arity = 2 },
		{ Name = unqualified(";") ; Name = unqualified("{}") }
	->
		io__write_string("{ ")
	;
		[]
	),
	(
		{ ArgTypes = [ArgType | Rest] }
	->
		mercury_output_sym_name(Name),
		io__write_string("("),
		mercury_output_term(ArgType, VarSet),
		mercury_output_remaining_terms(Rest, VarSet),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(Name)
	),
	(
		{ Arity = 2 },
		{ Name = unqualified(";") ; Name = unqualified("{}") }
	->
		io__write_string(" }")
	;
		[]
	),
	( { Ctors \= [] } ->
		io__write_string("\n\t;\t")
	;	
		[]
	),
	mercury_output_ctors(Ctors, VarSet).

%-----------------------------------------------------------------------------%

:- pred mercury_output_pred_decl(varset, sym_name, list(type_and_mode),
		determinism, term__context, io__state, io__state).
:- mode mercury_output_pred_decl(in, in, in, in, in, di, uo) is det.

mercury_output_pred_decl(VarSet, PredName, TypesAndModes, Det, Context) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	mercury_output_pred_type(VarSet, PredName, Types, Context),
	(
		{ MaybeModes = yes(Modes) }
	->
		mercury_output_mode_decl(VarSet, PredName, Modes, Det, Context)
	;
		[]
	).

mercury_output_pred_type(VarSet, PredName, Types, _Context) -->
	io__write_string(":- pred "),
	(
		{ Types = [Type | Rest] }
	->
		mercury_output_sym_name(PredName),
		io__write_string("("),
		mercury_output_term(Type, VarSet),
		mercury_output_remaining_terms(Rest, VarSet),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(PredName)
	),

	% We need to handle is/2 specially, because it's used for
	% determinism annotations (`... is det'), and so the compiler
	% will misinterpret a bare `:- pred is(int, int_expr)' as
	% `:- pred int is int_expr' and then report some very confusing
	% error message.  Thus you _have_ to give a determinism
	% annotation in the pred declaration for is/2, eg.
	% `:- pred int(int, int_expr) is det.'
	% (Yes, this made me puke too.)

	(
		{ PredName = unqualified("is") },
		{ list__length(Types, 2) }
	->
		io__write_string(" is det")
	;
		[]
	),
	io__write_string(".\n").

:- pred mercury_output_remaining_terms(list(term), varset,
					io__state, io__state).
:- mode mercury_output_remaining_terms(in, in, di, uo) is det.

mercury_output_remaining_terms([], _VarSet) --> [].
mercury_output_remaining_terms([Term | Terms], VarSet) -->
	io__write_string(", "),
	mercury_output_term(Term, VarSet),
	mercury_output_remaining_terms(Terms, VarSet).

%-----------------------------------------------------------------------------%

	% Output a mode declaration for a predicate.

mercury_output_mode_decl(VarSet, PredName, Modes, Det, _Context) -->
	io__write_string(":- mode "),
	(
		{ Modes \= [] }
	->
		mercury_output_sym_name(PredName),
		io__write_string("("),
		mercury_output_mode_list(Modes, VarSet),
		io__write_string(")")
	;
		mercury_output_bracketed_sym_name(PredName)
	),
	( { Det = unspecified } ->
		[]
	;
		io__write_string(" is "),
		mercury_output_det(Det)
	),
	io__write_string(".\n").

:- pred mercury_output_det(determinism, io__state, io__state).
:- mode mercury_output_det(in, di, uo) is det.

mercury_output_det(det) -->
	io__write_string("det").
mercury_output_det(semidet) -->
	io__write_string("semidet").
mercury_output_det(nondet) -->
	io__write_string("nondet").
mercury_output_det(failure) -->
	io__write_string("failure").
mercury_output_det(erroneous) -->
	io__write_string("erroneous").

:- pred mercury_output_bracketed_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_bracketed_sym_name(in, di, uo) is det.

mercury_output_bracketed_sym_name(Name) -->
	{ unqualify_name(Name, Name2) },
	mercury_output_bracketed_constant(term__atom(Name2)).

:- pred mercury_output_sym_name(sym_name, io__state, io__state).
:- mode mercury_output_sym_name(in, di, uo) is det.

mercury_output_sym_name(Name) -->
	{ unqualify_name(Name, Name2) },
	term_io__write_constant(term__atom(Name2)).

%-----------------------------------------------------------------------------%

	% Output a clause.

:- pred mercury_output_clause(varset, sym_name, list(term), goal, term__context,
			io__state, io__state).
:- mode mercury_output_clause(in, in, in, in, in, di, uo) is det.

mercury_output_clause(VarSet, PredName, Args, Body, Context) -->
	{ unqualify_name(PredName, PredName2) },
	mercury_output_term(term__functor(term__atom(PredName2), Args, Context),
			VarSet),
	(
		{ Body = true }
	->
		[]
	;
		io__write_string(" :-\n\t"),
		mercury_output_goal(Body, VarSet, 1)
	),
	io__write_string(".\n").

:- pred mercury_output_goal(goal, varset, int, io__state, io__state).
:- mode mercury_output_goal(in, in, in, di, uo) is det.

mercury_output_goal(fail, _, _) -->
	io__write_string("fail").

mercury_output_goal(true, _, _) -->
	io__write_string("true").

mercury_output_goal(some(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		mercury_output_goal(Goal, VarSet, Indent)
	;
		io__write_string("(some ["),
		mercury_output_vars(Vars, VarSet),
		io__write_string("] "),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_goal(Goal, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

mercury_output_goal(all(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		mercury_output_goal(Goal, VarSet, Indent)
	;
		io__write_string("(all ["),
		mercury_output_vars(Vars, VarSet),
		io__write_string("] "),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_goal(Goal, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

mercury_output_goal(if_then_else(Vars, A, B, C), VarSet, Indent) -->
	io__write_string("(if"),
	mercury_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("then"),
	mercury_output_newline(Indent1),
	mercury_output_goal(B, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("else"),
	mercury_output_newline(Indent1),
	mercury_output_goal(C, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal(if_then(Vars, A, B), VarSet, Indent) -->
	io__write_string("(if"),
	mercury_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("then"),
	mercury_output_newline(Indent1),
	mercury_output_goal(B, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal(not(Vars, Goal), VarSet, Indent) -->
	io__write_string("(\+"),
	mercury_output_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(Goal, VarSet, Indent),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal((A,B), VarSet, Indent) -->
	mercury_output_goal(A, VarSet, Indent),
	io__write_string(","),
	mercury_output_newline(Indent),
	mercury_output_goal(B, VarSet, Indent).

mercury_output_goal((A;B), VarSet, Indent) -->
	io__write_string("("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_goal(A, VarSet, Indent1),
	mercury_output_disj(B, VarSet, Indent),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_goal(call(Term), VarSet, Indent) -->
	mercury_output_call(Term, VarSet, Indent).

mercury_output_goal(unify(A, B), VarSet, _Indent) -->
	mercury_output_term(A, VarSet),
	io__write_string(" = "),
	mercury_output_term(B, VarSet).

:- pred mercury_output_call(term, varset, int, io__state, io__state).
:- mode mercury_output_call(in, in, in, di, uo) is det.

mercury_output_call(Term, VarSet, _Indent) -->
	mercury_output_term(Term, VarSet).

:- pred mercury_output_disj(goal, varset, int, io__state, io__state).
:- mode mercury_output_disj(in, in, in, di, uo) is det.

mercury_output_disj(Goal, VarSet, Indent) -->
	mercury_output_newline(Indent),
	io__write_string(";"),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	(
		{ Goal = (A;B) }
	->
		mercury_output_goal(A, VarSet, Indent1),
		mercury_output_disj(B, VarSet, Indent)
	;
		mercury_output_goal(Goal, VarSet, Indent1)
	).

:- pred mercury_output_some(list(var), varset, io__state, io__state).
:- mode mercury_output_some(in, in, di, uo) is det.

mercury_output_some(Vars, VarSet) -->
	(
		{ Vars = [] }
	->
		[]
	;
		io__write_string(" some ["),
		mercury_output_vars(Vars, VarSet),
		io__write_string("]")
	).

%-----------------------------------------------------------------------------%

mercury_output_hlds_goal(Goal - GoalInfo, VarSet, Indent) -->
		% debugging aid
	{ goal_info_get_nonlocals(GoalInfo, NonLocalsSet) },
	{ set__to_sorted_list(NonLocalsSet, NonLocalsList) },
	io__write_string("% nonlocals: "),
	mercury_output_vars(NonLocalsList, VarSet),
	mercury_output_newline(Indent),

	mercury_output_hlds_goal_2(Goal, VarSet, Indent).

:- pred mercury_output_hlds_goal_2(hlds__goal_expr, varset, int,
					io__state, io__state).
:- mode mercury_output_hlds_goal_2(in, in, in, di, uo) is det.

mercury_output_hlds_goal_2(switch(Var, CasesList, _), VarSet, Indent) -->
	io__write_string("( % switch on `"),
	mercury_output_var(Var, VarSet),
	io__write_string("'"),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	( { CasesList = [Case | Cases] } ->
		mercury_output_hlds_case(Case, Var, VarSet, Indent1),
		mercury_output_hlds_cases(Cases, Var, VarSet, Indent1)
	;
		io__write_string("fail")
	),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_hlds_goal_2(some(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		mercury_output_hlds_goal(Goal, VarSet, Indent)
	;
		io__write_string("(some ["),
		mercury_output_vars(Vars, VarSet),
		io__write_string("] "),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_hlds_goal(Goal, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

mercury_output_hlds_goal_2(all(Vars, Goal), VarSet, Indent) -->
	( { Vars = [] } ->
		mercury_output_hlds_goal(Goal, VarSet, Indent)
	;
		io__write_string("(all ["),
		mercury_output_vars(Vars, VarSet),
		io__write_string("] "),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_hlds_goal(Goal, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

mercury_output_hlds_goal_2(if_then_else(Vars, A, B, C), VarSet, Indent) -->
	io__write_string("(if"),
	mercury_output_hlds_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_hlds_goal(A, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("then"),
	mercury_output_newline(Indent1),
	mercury_output_hlds_goal(B, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("else"),
	mercury_output_newline(Indent1),
	mercury_output_hlds_goal(C, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_hlds_goal_2(not(Vars, Goal), VarSet, Indent) -->
	io__write_string("(\+"),
	mercury_output_hlds_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	mercury_output_hlds_goal(Goal, VarSet, Indent),
	mercury_output_newline(Indent),
	io__write_string(")").

mercury_output_hlds_goal_2(conj(List), VarSet, Indent) -->
	( { List = [Goal | Goals] } ->
		io__write_string("("),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_hlds_goal(Goal, VarSet, Indent1),
		mercury_output_hlds_conj(Goals, VarSet, Indent),
		mercury_output_newline(Indent),
		io__write_string(")")
	;
		io__write_string("true")
	).

mercury_output_hlds_goal_2(disj(List), VarSet, Indent) -->
	( { List = [Goal | Goals] } ->
		io__write_string("("),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_hlds_goal(Goal, VarSet, Indent1),
		mercury_output_hlds_disj(Goals, VarSet, Indent),
		mercury_output_newline(Indent),
		io__write_string(")")
	;
		io__write_string("fail")
	).

mercury_output_hlds_goal_2(call(PredId, _ProcId, Args, _), VarSet, Indent) -->
	mercury_output_hlds_call(PredId, Args, VarSet, Indent).

mercury_output_hlds_goal_2(unify(A, B, _, _, _), VarSet, _Indent) -->
	mercury_output_term(A, VarSet),
	io__write_string(" = "),
	mercury_output_term(B, VarSet).

:- pred mercury_output_hlds_call(pred_id, list(term), varset, int,
				io__state, io__state).
:- mode mercury_output_hlds_call(in, in, in, in, di, uo) is det.

mercury_output_hlds_call(PredId, Args, VarSet, _Indent) -->
		% XXX module name
	{ predicate_name(PredId, PredName) },
	{ term__context_init(0, Context) },
	mercury_output_term(term__functor(term__atom(PredName), Args, Context),
				VarSet).

:- pred mercury_output_hlds_conj(list(hlds__goal), varset, int,
				io__state, io__state).
:- mode mercury_output_hlds_conj(in, in, in, di, uo) is det.

mercury_output_hlds_conj(GoalList, VarSet, Indent) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		io__write_string(","),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_hlds_goal(Goal, VarSet, Indent1),
		mercury_output_hlds_conj(Goals, VarSet, Indent)
	;
		[]
	).

:- pred mercury_output_hlds_disj(list(hlds__goal), varset, int,
				io__state, io__state).
:- mode mercury_output_hlds_disj(in, in, in, di, uo) is det.

mercury_output_hlds_disj(GoalList, VarSet, Indent) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		mercury_output_newline(Indent),
		io__write_string(";"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_hlds_goal(Goal, VarSet, Indent1),
		mercury_output_hlds_disj(Goals, VarSet, Indent)
	;
		[]
	).

:- pred mercury_output_hlds_case(case, var, varset, int,
				io__state, io__state).
:- mode mercury_output_hlds_case(in, in, in, in, di, uo) is det.

mercury_output_hlds_case(case(ConsId, ArgVars, Goal), Var, VarSet, Indent) -->
	{ term_list_to_var_list(Args, ArgVars) },
	{ cons_id_get_name(ConsId, Name) },
	{ term__context_init(0, Context) },
	mercury_output_var(Var, VarSet),
	io__write_string(" = "),
	mercury_output_term(term__functor(Name, Args, Context), VarSet),
	io__write_string(","),
	mercury_output_newline(Indent),
	mercury_output_hlds_goal(Goal, VarSet, Indent).

:- pred mercury_output_hlds_cases(list(case), var, varset, int,
				io__state, io__state).
:- mode mercury_output_hlds_cases(in, in, in, in, di, uo) is det.

mercury_output_hlds_cases(CasesList, Var, VarSet, Indent) -->
	(
		{ CasesList = [Case | Cases] }
	->
		mercury_output_newline(Indent),
		io__write_string(";"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		mercury_output_hlds_case(Case, Var, VarSet, Indent1),
		mercury_output_hlds_cases(Cases, Var, VarSet, Indent)
	;
		[]
	).

:- pred mercury_output_hlds_some(list(var), varset, io__state, io__state).
:- mode mercury_output_hlds_some(in, in, di, uo) is det.

	% quantification is all implicit by the time we get to the hlds.

mercury_output_hlds_some(_Vars, _VarSet) --> [].

%-----------------------------------------------------------------------------%

mercury_output_newline(Indent) -->
	io__write_char('\n'),
	mercury_output_tabs(Indent).

:- pred mercury_output_tabs(int, io__state, io__state).
:- mode mercury_output_tabs(in, di, uo) is det.

mercury_output_tabs(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_char('\t'),
		{ Indent1 is Indent - 1 },
		mercury_output_tabs(Indent1)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_output_list_args(term, varset, io__state, io__state).
:- mode mercury_output_list_args(in, in, di, uo) is det.

mercury_output_list_args(Term, VarSet) -->
	(
	    	{ Term = term__functor(term__atom("."), Args, _),
		  Args = [X, Xs]
	    	}
	->
		io__write_string(", "),
		mercury_output_term(X, VarSet),
		mercury_output_list_args(Xs, VarSet)
	;
		{ Term = term__functor(term__atom("[]"), [], _) }
	->
		[]
	;
		io__write_string(" | "),
		mercury_output_term(Term, VarSet)
	).

	% write a term to standard output.

mercury_output_term(term__variable(Var), VarSet) -->
	mercury_output_var(Var, VarSet).
mercury_output_term(term__functor(Functor, Args, _), VarSet) -->
	(
	    	{ Functor = term__atom("."),
		  Args = [X, Xs]
	    	}
	->
		io__write_string("["),
		mercury_output_term(X, VarSet),
		mercury_output_list_args(Xs, VarSet),
		io__write_string("]")
	;
		{ Args = [PrefixArg],
		  Functor = term__atom(FunctorName),
		  mercury_unary_prefix_op(FunctorName)
	    	}
	->
		io__write_string("("),
		io__write_string(FunctorName),
		io__write_string(" "),
		mercury_output_term(PrefixArg, VarSet),
		io__write_string(")")
	;
		{ Args = [PostfixArg],
		  Functor = term__atom(FunctorName),
		  mercury_unary_postfix_op(FunctorName)
	    	}
	->
		io__write_string("("),
		mercury_output_term(PostfixArg, VarSet),
		io__write_string(" "),
		io__write_string(FunctorName),
		io__write_string(")")
	;
		{ Args = [Arg1, Arg2],
		  Functor = term__atom(FunctorName),
		  mercury_infix_op(FunctorName)
		}
	->
		io__write_string("("),
		mercury_output_term(Arg1, VarSet),
		io__write_string(" "),
		io__write_string(FunctorName),
		io__write_string(" "),
		mercury_output_term(Arg2, VarSet),
		io__write_string(")")
	;
		{ Args = [Y | Ys] }
	->
		term_io__write_constant(Functor),
		io__write_string("("),
		mercury_output_term(Y, VarSet),
		mercury_output_remaining_terms(Ys, VarSet),
		io__write_string(")")
	;
		mercury_output_bracketed_constant(Functor)
	).

	% output a comma-separated list of variables

mercury_output_vars([], _VarSet) --> [].
mercury_output_vars([Var | Vars], VarSet) -->
	mercury_output_var(Var, VarSet),
	mercury_output_vars_2(Vars, VarSet).

:- pred mercury_output_vars_2(list(var), varset, io__state, io__state).
:- mode mercury_output_vars_2(in, in, di, uo) is det.

mercury_output_vars_2([], _VarSet) --> [].
mercury_output_vars_2([Var | Vars], VarSet) -->
	io__write_string(", "),
	mercury_output_var(Var, VarSet),
	mercury_output_vars_2(Vars, VarSet).

	% Output a single variable.
	% Variables that didn't have names are given the name "V_<n>"
	% where <n> is there variable id.
	% Variables whose name originally started with `V_' have their
	% name changed to start with `V__' to avoid name clashes.

mercury_output_var(Var, VarSet) -->
	(
		{ varset__lookup_name(VarSet, Var, Name) }
	->
		{ mercury_convert_var_name(Name, ConvertedName) },
		io__write_string(ConvertedName)
	;
		{ term__var_to_int(Var, Id),
		  string__int_to_string(Id, Num),
		  string__append("V_", Num, VarName)
		},
		io__write_string(VarName)
	).

:- pred mercury_output_bracketed_constant(const, io__state, io__state).
:- mode mercury_output_bracketed_constant(in, di, uo) is det.

mercury_output_bracketed_constant(Const) -->
	( { Const= term__atom(Op), mercury_op(Op) } ->
		io__write_string("("),
		term_io__write_constant(Const),
		io__write_string(")")
	;
		term_io__write_constant(Const)
	).

%-----------------------------------------------------------------------------%

	% Predicates to test whether a functor is a Mercury operator

:- pred mercury_op(string).
:- mode mercury_op(in) is semidet.

mercury_op(Op) :-
	(
	    (
		mercury_infix_op(Op)
	    ;
		mercury_binary_prefix_op(Op)
	    ;
		mercury_unary_prefix_op(Op)
	    ;
		mercury_unary_postfix_op(Op)
	    )
	->
		true
	;
		fail
	).

:- pred mercury_binary_prefix_op(string).
:- mode mercury_binary_prefix_op(in) is semidet.

mercury_binary_prefix_op("some").
mercury_binary_prefix_op("all").
mercury_binary_prefix_op("gSome").
mercury_binary_prefix_op("gAll").

:- pred mercury_infix_op(string).
:- mode mercury_infix_op(in) is semidet.

mercury_infix_op("-->").
mercury_infix_op(":-").
mercury_infix_op("::").
mercury_infix_op("where").
mercury_infix_op("sorted").	/* NU-Prolog */
mercury_infix_op("else").
mercury_infix_op("then").
mercury_infix_op(";").
mercury_infix_op("->").
mercury_infix_op(",").
mercury_infix_op("to").		/* NU-Prolog */
mercury_infix_op("<=").
mercury_infix_op("<=>").
mercury_infix_op("=>").
mercury_infix_op("when").	/* NU-Prolog */
mercury_infix_op("or").		/* NU-Prolog */
mercury_infix_op("and").	/* NU-Prolog */
mercury_infix_op("=").
mercury_infix_op("=..").
mercury_infix_op("=:=").
mercury_infix_op("=<").
mercury_infix_op("==").
mercury_infix_op("=\=").
mercury_infix_op(">").
mercury_infix_op(">=").
mercury_infix_op("<").
mercury_infix_op("<=").
mercury_infix_op("@<").		/* Prolog */
mercury_infix_op("@=<").	/* Prolog */
mercury_infix_op("@>").		/* Prolog */
mercury_infix_op("@>=").	/* Prolog */
mercury_infix_op("=").
mercury_infix_op("~=").		/* NU-Prolog */
mercury_infix_op("is").		
mercury_infix_op(".").		
mercury_infix_op(":").		
mercury_infix_op("+").
mercury_infix_op("-").
mercury_infix_op("/\\").
mercury_infix_op("\\/").
mercury_infix_op("*").
mercury_infix_op("/").
mercury_infix_op("//").
mercury_infix_op(">>").
mercury_infix_op("<<").
mercury_infix_op("**").
mercury_infix_op("mod").
mercury_infix_op("^").

:- pred mercury_unary_prefix_op(string).
:- mode mercury_unary_prefix_op(in) is semidet.

mercury_unary_prefix_op(":-").
mercury_unary_prefix_op(":-").
mercury_unary_prefix_op("?-").
mercury_unary_prefix_op("pred").
mercury_unary_prefix_op("type").
mercury_unary_prefix_op("useIf").
mercury_unary_prefix_op("::").
mercury_unary_prefix_op("delete").
mercury_unary_prefix_op("insert").
mercury_unary_prefix_op("update").
mercury_unary_prefix_op("sorted").
mercury_unary_prefix_op("if").
mercury_unary_prefix_op("dynamic").
mercury_unary_prefix_op("pure").
mercury_unary_prefix_op("\+").
mercury_unary_prefix_op("lib").
mercury_unary_prefix_op("listing").
mercury_unary_prefix_op("man").
mercury_unary_prefix_op("nospy").
mercury_unary_prefix_op("not").
mercury_unary_prefix_op("once").
mercury_unary_prefix_op("spy").
mercury_unary_prefix_op("wait").
mercury_unary_prefix_op("~").
mercury_unary_prefix_op("+").
mercury_unary_prefix_op("-").

:- pred mercury_unary_postfix_op(string).
:- mode mercury_unary_postfix_op(in) is semidet.

mercury_unary_postfix_op("sorted").

%-----------------------------------------------------------------------------%

	% Convert a Mercury variable into a Mercury variable name.  
	% This is tricky because the compiler may introduce new variables
	% who either don't have names at all, or whose names end in
	% some sequence of primes (eg. Var''').
	% We have to be careful that every possible variable
	% is mapped to a distinct name.  Variables without names are
	% given names starting with `V_' followed by a sequence of digits
	% corresponding to their variable id.
	% To ensure that this doesn't clash with any existing names,
	% any variables whose name originally started with `V_' get
	% another `V_' inserted at the start of their name.

	% Compiler's internal name	Converted name
	% ------------------------	--------------
	% none				V_[0-9]*
	% .*'+				V_[0-9]*_.*
	% V_.*				V_V_.*
	% anthing else			same as original name

:- pred mercury_convert_var_name(string, string).
:- mode mercury_convert_var_name(in, out) is det.

mercury_convert_var_name(Name, ConvertedName) :-
	( string__append(_, "'", Name) ->
		strip_trailing_primes(Name, StrippedName, NumPrimes),
		string__append("V_", StrippedName, Tmp1),
		string__int_to_string(NumPrimes, NumString),
		string__append(Tmp1, "_", Tmp2),
		string__append(Tmp2, NumString, ConvertedName)
	; string__prefix(Name, "V_") ->
		string__append("V_", Name, ConvertedName)
	;
		ConvertedName = Name
	).

:- pred strip_trailing_primes(string, string, int).
:- mode strip_trailing_primes(in, out, out) is det.

strip_trailing_primes(Name0, Name, Num) :-
	( string__append(Name1, "'", Name0) ->
		strip_trailing_primes(Name1, Name, Num0),
		Num is Num0 + 1
	;
		Num = 0,
		Name = Name0
	).
	
%-----------------------------------------------------------------------------%

:- pred maybe_output_line_number(term__context, io__state, io__state).
:- mode maybe_output_line_number(in, di, uo) is det.

maybe_output_line_number(Context) -->
	globals__lookup_option(line_numbers, bool(LineNumbers)),
	( { LineNumbers = yes } ->
		io__write_string("\t% "),
		prog_out__write_context(Context),
		io__write_string("\n")
	).

%-----------------------------------------------------------------------------%
