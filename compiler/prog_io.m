%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: prog_io.nl.
% Main author: fjh.
%
% This module defines a data structure for representing Mercury
% programs.
%
% In some ways the representation of programs here is considerably
% more complex than is necessary for the compiler.
% The basic reason for this is that it was designed to preserve
% as much information about the source code as possible, so that
% this representation could also be used for other tools such
% as Mercury-to-Goedel converters, pretty-printers, etc.
% Currently the only information that is lost is that comments and
% whitespace are stripped, any redundant parenthesization
% are lost, and DCG clauses get expanded.
% It would be a good idea to preserve all those too (well, maybe not
% the redundant parentheses), but right now it's not worth the effort.
%
% So that means that this phase of compilation is purely parsing.
% No simplifications are done (other than DCG expansion). 
% The results of this phase specify
% basically the same information as is contained in the source code,
% but in a parse tree rather than a flat file.
% Simplifications are done only by make_hlds.nl, which transforms
% the parse tree which we built here into the HLDS.
%
% Some of this code is a rather bad example of cut-and-paste style reuse.
% It should be cleaned up to eliminate most of the duplication.
% But that task really needs to wait until we implement higher-order
% predicates.  For the moment, just be careful that any changes
% you make are reflected correctly in all similar parts of this
% file.

% XXX todo:
%
% 1.  implement importing/exporting operators with a particular fixity
%     eg. :- import_op prefix(+). % only prefix +, not infix
%     (not important, but should be there for reasons of symmetry.)
% 2.  improve the handling of type and inst parameters 
%     (see XXX's below)
% 3.  improve the error reporting
% 4.  parse abstract inst definitions 
%
% Question: should we allow `:- rule' declarations???

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prog_io.
:- interface.
:- import_module string, int, list, varset, term, io, term_io.
:- import_module globals, options.

%-----------------------------------------------------------------------------%

	% This is how programs (and parse errors) are represented.

:- type message_list	==	list(pair(string, term)).
				% the error/warning message, and the
				% term to which it relates

:- type program		--->	module(
					module_name,
					item_list
				).

:- type item_list	==	list(item_and_context).

:- type item_and_context ==	pair(item, term__context).

:- type item		--->	clause(varset, sym_name, list(term), goal)
				%      VarNames, PredName, HeadArgs, ClauseBody
			; 	type_defn(varset, type_defn, condition)
			; 	inst_defn(varset, inst_defn, condition)
			; 	mode_defn(varset, mode_defn, condition)
			; 	module_defn(varset, module_defn)
			; 	pred(varset, sym_name, list(type_and_mode),
					determinism, condition)
				%      VarNames, PredName, ArgTypes,
				%	Deterministicness, Cond
			; 	rule(varset, sym_name, list(type), condition)
				%      VarNames, PredName, ArgTypes, Cond
			; 	mode(varset, sym_name, list(mode),
					determinism, condition)
				%      VarNames, PredName, ArgModes,
				%	Deterministicness, Cond
			;	nothing.
				% used for items that should be ignored
				% (currently only NU-Prolog `when' declarations,
				% which are silently ignored for backwards
				% compatibility).

:- type type_and_mode	--->	type_only(type)
			;	type_and_mode(type, mode).

:- type determinism	--->	det
			;	semidet
			;	nondet
			;	unspecified.

%-----------------------------------------------------------------------------%

	% Here's how clauses and goals are represented.
	% (Constructs like "=>", "<=", and "<=>" are considered to be
	% just higher-order predicates, and so aren't represented
	% specially here.)

% clause/4 defined above

:- type goal		--->	(goal,goal)
			;	fail	
					% could use conj(goals) instead 
			;	{goal;goal}	% {...} quotes ';'/2.
			;	true	
					% could use disj(goals) instead
			;	not(vars,goal)
			;	some(vars,goal)
			;	all(vars,goal)
			;	if_then(vars,goal,goal)
			;	if_then_else(vars,goal,goal,goal)
			;	call(term)
			;	unify(term, term).

:- type goals		==	list(goal).
:- type vars		==	list(var).

%-----------------------------------------------------------------------------%

	% This is how types are represented.

			% one day we might allow types to take
			% value parameters as well as type parameters.

% type_defn/3 define above

:- type type_defn	--->	du_type(sym_name, list(type_param),
						list(constructor))
			;	uu_type(sym_name, list(type_param), list(type))
			;	eqv_type(sym_name, list(type_param), type)
			;	abstract_type(sym_name, list(type_param)).

:- type constructor	==	pair(sym_name, list(type)).

	% XXX should type parameters be variables not terms ??
:- type type_param	==	term.

:- type (type)		==	term.

	% Types may have arbitrary assertions associated with them
	% (eg. you can define a type which represents sorted lists).
	% The compiler will ignore these assertions - they are intended
	% to be used by other tools, such as the debugger.

:- type condition	--->	true
			;	where(term).

%-----------------------------------------------------------------------------%

	% This is how instantiatednesses and modes are represented.
	% Note that while we use the normal term data structure to represent 
	% type terms (see above), we need a separate data structure for inst 
	% terms.

% inst_defn/3 defined above

:- type inst_defn	--->	eqv_inst(sym_name, list(inst_param), inst)
			;	abstract_inst(sym_name, list(inst_param)).

	% XXX should inst parameters be variables not terms ??
:- type inst_param	==	term.

:- type (inst)		--->	free
			;	bound(list(bound_inst))
					% The list must be sorted
			;	ground
			;	inst_var(var)
			;	abstract_inst(sym_name, list(inst))
			;	user_defined_inst(sym_name, list(inst)).

:- type bound_inst	--->	functor(const, list(inst)).


% mode_defn/3 defined above

:- type mode_defn	--->	eqv_mode(sym_name, list(inst_param), mode).

:- type (mode)		--->	((inst) -> (inst))
			;	user_defined_mode(sym_name, list(inst)).

% mode/4 defined above

%-----------------------------------------------------------------------------%
	
	% This is how module-system declarations (such as imports
	% and exports) are represented.

:- type module_defn	--->	module(module_name)
			;	interface
			;	implementation
			;	end_module(module_name)
			;	export(sym_list)
			;	import(sym_list)
			;	use(sym_list).
:- type sym_list	--->	sym(list(sym_specifier))
			;	pred(list(pred_specifier))
			;	cons(list(cons_specifier))
			;	op(list(op_specifier))
			;	adt(list(sym_name_specifier))
	 		;	type(list(sym_name_specifier))
	 		;	module(list(module_specifier)).
:- type sym_specifier	--->	sym(sym_name_specifier)
			;	typed_sym(typed_cons_specifier)
			;	pred(pred_specifier)
			;	cons(cons_specifier)
			;	op(op_specifier)
			;	adt(sym_name_specifier)
	 		;	type(sym_name_specifier)
	 		;	module(module_specifier).
:- type pred_specifier	--->	sym(sym_name_specifier)
			;	name_args(sym_name, list(type)).
:- type cons_specifier	--->	sym(sym_name_specifier)
			;	typed(typed_cons_specifier).
:- type typed_cons_specifier --->	
				name_args(sym_name, list(type))
			;	name_res(sym_name_specifier, type)
			;	name_args_res(sym_name,
						list(type), type).
:- type op_specifier	--->	sym(sym_name_specifier)
			% XXX operator fixity specifiers not yet implemented
			;	fixity(sym_name_specifier, fixity).
:- type fixity		--->	infix ; prefix ; postfix.
:- type sym_name_specifier ---> name(sym_name)
			;	name_arity(sym_name, int).
:- type sym_name 	--->	unqualified(string)
			;	qualified(module_specifier, string).

:- type module_specifier ==	string.
:- type module_name 	== 	string.

%-----------------------------------------------------------------------------%

% This module (prog_io) exports the following predicates:

%-----------------------------------------------------------------------------%

	% read_module(ModuleName, Error, Messages, Program)
	% reads and parses the module 'ModuleName'.  Error is `yes'
	% if a syntax error was detected and `no' otherwise,
	% Messages is a list of warning/error messages,
	% and Program is the parse tree.

:- pred prog_io__read_module(string, string, bool, message_list, item_list,
				io__state, io__state).
:- mode prog_io__read_module(in, in, out, out, out, di, uo).

%-----------------------------------------------------------------------------%

	% Convert a single term into a goal.

:- pred parse_goal(term, goal).
:- mode parse_goal(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

	% When actually reading in type declarations, we need to
	% check for errors.

:- type maybe(T)	--->	error(string, term)
			;	ok(T).
:- type maybe2(T1, T2)	--->	error(string, term)
			;	ok(T1, T2).
:- type maybe_functor	== 	maybe2(sym_name, list(term)).
:- type maybe_item_and_context
			==	maybe2(item, term__context).

% This implementation uses io__read_term to read in the program
% term at a time, and then converts those terms into clauses and
% declarations, checking for errors as it goes.
% Note that rather than using difference lists, we just
% build up the lists of items and messages in reverse order
% and then reverse them afterwards.  (Using difference lists would require
% late-input modes.)

prog_io__read_module(FileName, ModuleName, Error, Messages, Items) -->
	lookup_option(search_directories, accumulating(Dirs)),
	search_for_file(Dirs, FileName, R),
	( { R = ok } ->
		read_all_items(RevMessages, RevItems0, Error0),
		{
		  get_end_module(RevItems0, RevItems, EndModule),
		  reverse(RevMessages, Messages0),
		  reverse(RevItems, Items0),
		  check_begin_module(ModuleName,
				Messages0, Items0, Error0, EndModule,
				FileName, Messages, Items, Error)
		},
		io__seen
	;
		io__progname("prog_io.nl", Progname),
		{
		  string__append(Progname, ": can't open file `", Message1),
		  string__append(Message1, FileName, Message2),
		  string__append(Message2, "'", Message),
		  dummy_term(Term),
		  Messages = [Message - Term],
		  Error = yes,
		  Items = []
		}
	).

:- pred search_for_file(list(string), string, res, io__state, io__state).
:- mode search_for_file(in, in, out, di, uo) is det.

search_for_file([], _, error) --> [].
search_for_file([Dir | Dirs], FileName, R) -->
	% xxx Operating system dependency -- assumptions about "." and "/".
	% This will work on Unix and even on DOS, but maybe
	% there are operating systems somewhere for which this won't work.

	( { Dir = "." } ->
		{ ThisFileName = FileName }
	;
		{ string__append(Dir, "/", Tmp1) },
		{ string__append(Tmp1, FileName, ThisFileName) }
	),
	io__see(ThisFileName, R0),
	( { R0 = ok } ->
		{ R = ok }
	;
		search_for_file(Dirs, FileName, R)
	).

%-----------------------------------------------------------------------------%

	% extract the final `:- end_module' declaration if any

:- type module_end ---> no ; yes(module_name, term__context).

:- pred get_end_module(item_list, item_list, module_end).
:- mode get_end_module(in, out, out).

get_end_module(RevItems0, RevItems, EndModule) :-
	(
		RevItems0 = [
			module_defn(_VarSet, end_module(ModuleName)) - Context
			    | RevItems1]
	->
		RevItems = RevItems1,
		EndModule = yes(ModuleName, Context)
	;
		RevItems = RevItems0,
		EndModule = no
	).

%-----------------------------------------------------------------------------%

	% check that the module starts with a :- module declaration,
	% and that the end_module declaration (if any) is correct,
	% and construct the final parsing result.

:- pred check_begin_module(string, message_list, item_list, bool,
			   module_end, string, message_list, item_list, bool).
:- mode check_begin_module(in, in, in, in, in, in,
				out, out, out).

check_begin_module(ModuleName, Messages0, Items0, Error0, EndModule, FileName,
		Messages, Items, Error) :-

    % check that the first item is a `:- module ModuleName'
    % declaration

    (
        Items0 = [module_defn(_VarSet, module(ModuleName1)) - Context
              | Items1]
    ->
        % check that the end module declaration (if any)
        % matches the begin module declaration 

        ( %%% some [ModuleName2, Context2]
	    (
        	EndModule = yes(ModuleName2, Context2),
        	ModuleName1 \= ModuleName2
            )
        ->
	    dummy_term_with_context(Context2, Term),
            ThisError = 
"Error: `:- end_module' declaration doesn't match `:- module' declaration"
			- Term,
            append([ThisError], Messages0, Messages),
	    Items = Items1,
            Error = yes
        ;
	% check that the begin module declaration matches the expected name
	% of the module
	    ModuleName1 \= ModuleName
	->
	    dummy_term_with_context(Context, Term2),
            ThisError =
	        "Warning: incorrect module name in `:- module' declaration"
		    - Term2,
            Messages = [ThisError | Messages0],
	    Items = Items1,
	    Error = Error0
	;
	    Messages = Messages0,
	    Items = Items1,
	    Error = Error0
        )
    ;
	term__context_init(FileName, 1, Context),
	dummy_term_with_context(Context, Term2),
        ThisError = "Error: module should start with a `:- module' declaration"
		- Term2,
        Messages = [ThisError | Messages0],
	Items = Items0,
	Error = yes
    ).

	% Create a dummy term.
	% Used for error messages that are not associated with any
	% particular term or context.
:- pred dummy_term(term).
:- mode dummy_term(out).
dummy_term(Term) :-
	term__context_init(0, Context),
	dummy_term_with_context(Context, Term).

	% Create a dummy term with the specified context.
	% Used for error messages that are associated with some specific
	% context, but for which we don't want to print out the term
	% (or for which the term isn't available to be printed out).

:- pred dummy_term_with_context(term__context, term).
:- mode dummy_term_with_context(in, out).
dummy_term_with_context(Context, Term) :-
	Term = term__functor(term__atom(""), [], Context).

%-----------------------------------------------------------------------------%
 	% Read a source file from standard in, first reading in
	% the input term by term and then parsing those terms and producing
	% a high-level representation.
	% Parsing is actually a 3-stage process instead of the
	% normal two-stage process:
	%	lexical analysis (chars -> tokens),
	% 	parsing stage 1 (tokens -> terms),
	%	parsing stage 2 (terms -> items).
	% The final stage produces a list of program items, each of
	% which may be a declaration or a clause.


:- pred read_all_items(message_list, item_list, bool, io__state, io__state).
:- mode read_all_items(out, out, out, di, uo).

read_all_items(Messages, Items, Error) -->
	read_items_loop([], [], no, Messages, Items, Error).

%-----------------------------------------------------------------------------%

	% The loop is arranged somewhat carefully: we want it to
	% be tail recursive, and we want to do a small garbage collection
	% after we have read each item to minimize memory usage
	% and improve cache locality.  So each iteration calls
	% read_item(MaybeItem) - which does all the work for a single item -
	% via io__gc_call/1, which calls the goal with garbage collection.
	% This manual garbage collection won't be strictly necessary
	% when (if) we implement automatic garbage collection, but
	% it will probably still improve performance.
	%
	% Note: the following will NOT be tail recursive with our
	% implementation unless the compiler is smart enough to inline
	% read_items_loop_2.

:- pred read_items_loop(message_list, item_list, bool, message_list,
			item_list, bool, io__state, io__state).
:- mode read_items_loop(in, in, in, out, out, out, di, uo).

read_items_loop(Msgs1, Items1, Error1, Msgs, Items, Error) -->
	io__gc_call(read_item(MaybeItem)),
 	read_items_loop_2(MaybeItem, Msgs1, Items1, Error1, Msgs, Items, Error).

%-----------------------------------------------------------------------------%

:- pred read_items_loop_2(maybe_item_or_eof, message_list, item_list,
	bool, message_list, item_list, bool, io__state, io__state).
:- mode read_items_loop_2(in, in, in, in,
			out, out, out, di, uo).

% do a switch on the type of the next item

read_items_loop_2(eof, Msgs, Items, Error, Msgs, Items, Error) --> []. 
	% if the next item was end-of-file, then we're done.

read_items_loop_2(syntax_error(ErrorMsg, LineNumber), Msgs0, Items0, _Error0,
			Msgs, Items, Error) -->
	% if the next item was a syntax error, then insert it in
	% the list of messages and continue looping
	io__input_stream(Stream),
	io__stream_name(Stream, StreamName),
	{
	  term__context_init(StreamName, LineNumber, Context),
	  dummy_term_with_context(Context, Term),
	  ThisError = ErrorMsg - Term,
	  Msgs1 = [ThisError | Msgs0],
	  Items1 = Items0,
	  Error1 = yes
	},
	read_items_loop(Msgs1, Items1, Error1, Msgs, Items, Error).

read_items_loop_2(error(M,T), Msgs0, Items0, _Error0, Msgs, Items, Error) -->
	% if the next item was a semantic error, then insert it in
	% the list of messages and continue looping
	{
	  add_error(M, T, Msgs0, Msgs1),
	  Items1 = Items0,
	  Error1 = yes
	},
 	read_items_loop(Msgs1, Items1, Error1, Msgs, Items, Error).

read_items_loop_2(ok(Item, Context), Msgs0, Items0, Error0,
			Msgs, Items, Error) -->
	% if the next item was a valid item, then insert it in
	% the list of items and continue looping
	{
	  Msgs1 = Msgs0,
	  Items1 = [Item - Context | Items0],
	  Error1 = Error0
	},
 	read_items_loop(Msgs1, Items1, Error1, Msgs, Items, Error).

%-----------------------------------------------------------------------------%

	% read_item/1 reads a single item, and if it is a valid term
	% parses it.

:- type maybe_item_or_eof --->	eof
			;	syntax_error(string, int)
			;	error(string, term)
			;	ok(item, term__context).

:- pred read_item(maybe_item_or_eof, io__state, io__state).
:- mode read_item(out, di, uo).

read_item(MaybeItem) -->
	io__read_term(MaybeTerm),
	{ process_read_term(MaybeTerm, MaybeItem) }.

:- pred process_read_term(read_term, maybe_item_or_eof).
:- mode process_read_term(in, out).

process_read_term(eof, eof).
process_read_term(error(ErrorMsg, LineNumber),
			syntax_error(ErrorMsg, LineNumber)).
process_read_term(term(VarSet, Term), MaybeItemOrEof) :-
	parse_item(VarSet, Term, MaybeItem),
	convert_item(MaybeItem, MaybeItemOrEof).

:- pred convert_item(maybe_item_and_context, maybe_item_or_eof).
:- mode convert_item(in, out).

convert_item(ok(Item, Context), ok(Item, Context)).
convert_item(error(M,T), error(M,T)).

:- pred parse_item(varset, term, maybe_item_and_context).
:- mode parse_item(in, in, out).

parse_item(VarSet, Term, Result) :-
 	( %%% some [Decl, DeclContext]
		Term = term__functor(term__atom(":-"), [Decl], DeclContext)
	->
		% It's a declaration
		parse_decl(VarSet, Decl, R),
		add_context(R, DeclContext, Result)
	; %%% some [DCG_H, DCG_B, DCG_Context]
		% It's a DCG clause
		Term = term__functor(term__atom("-->"), [DCG_H, DCG_B],
			DCG_Context)
	->
		parse_dcg_clause(VarSet, DCG_H, DCG_B, DCG_Context, Result)
	;
		% It's either a fact or a rule
		( %%% some [H, B, TermContext]
			Term = term__functor(term__atom(":-"), [H,B], TermContext)
		->
			% it's a rule
			Head = H,
			Body = B,
			TheContext = TermContext
		;
			% it's a fact
			Head = Term,
			(
				Head = term__functor(_Functor, _Args,
							HeadContext)
			->
				TheContext = HeadContext
			;
					% term consists of just a single
					% variable - the context has been lost
				term__context_init(0, TheContext)
			),
			Body = term__functor(term__atom("true"), [], TheContext)
		),
		parse_goal(Body, Body2),
		parse_qualified_term(Head, "clause head", R2),
		process_clause(R2, VarSet, Body2, R3),
		add_context(R3, TheContext, Result)
	).

:- pred add_context(maybe(item), term__context, maybe_item_and_context).
:- mode add_context(in, in, out).

add_context(error(M, T), _, error(M, T)).
add_context(ok(Item), Context, ok(Item, Context)).

:- pred process_clause(maybe_functor, varset, goal, maybe(item)).
:- mode process_clause(in, in, in, out).
process_clause(ok(Name, Args), VarSet, Body,
		ok(clause(VarSet, Name, Args, Body))).
process_clause(error(ErrMessage, Term), _, _, error(ErrMessage, Term)).

:- pred join_error(bool, bool, bool).
:- mode join_error(in, in, out).
join_error(yes, _, yes).
join_error(no, Error, Error).

%-----------------------------------------------------------------------------%

	% Parse a goal.
	% We just check if it matches the appropriate pattern
	% for one of the builtins.  If it doens't match any of the
	% builtins, then it's just a predicate call.
	% XXX we should do more parsing here - type qualification and
	% module qualification should be parsed here.

parse_goal(Term, Goal) :-
	( %%% some [Goal2]
		parse_goal_2(Term, Goal2)
	->
		Goal = Goal2
	;
		Goal = call(Term)
	).

:- pred parse_goal_2(term, goal).
:- mode parse_goal_2(in, out).
parse_goal_2(term__functor(term__atom("true"),[],_), true).
parse_goal_2(term__functor(term__atom("fail"),[],_), fail).
parse_goal_2(term__functor(term__atom("="),[A,B],_), unify(A,B)).
parse_goal_2(term__functor(term__atom("->"),[A0,B0],_), if_then(Vars,A,B)) :-
	parse_some_vars_goal(A0, Vars, A),
	parse_goal(B0, B).
parse_goal_2(term__functor(term__atom(","),[A0,B0],_), (A,B)) :-
	parse_goal(A0, A),
	parse_goal(B0, B).
parse_goal_2(term__functor(term__atom(";"),[A0,B0],_), R) :-
	(
		A0 = term__functor(term__atom("->"), [X0,Y0], _Context)
	->
		parse_some_vars_goal(X0, Vars, X),
		parse_goal(Y0, Y),
		parse_goal(B0, B),
		R = if_then_else(Vars, X, Y, B)
	;
		parse_goal(A0, A),
		parse_goal(B0, B),
		R = (A;B)
	).
parse_goal_2(term__functor(term__atom("if"),
		[term__functor(term__atom("then"),[A0,B0],_)],_),
		if_then(Vars,A,B)) :-
	parse_some_vars_goal(A0, Vars, A),
	parse_goal(B0, B).
parse_goal_2( term__functor(term__atom("else"),[
		    term__functor(term__atom("if"),[
			term__functor(term__atom("then"),[A0,B0],_)
		    ],_),
		    C0
		],_),
		if_then_else(Vars,A,B,C)) :-
	parse_some_vars_goal(A0, Vars, A),
	parse_goal(B0, B),
	parse_goal(C0, C).
parse_goal_2( term__functor(term__atom("not"), [A0], _), not([],A) ) :-
	parse_goal(A0, A).
parse_goal_2( term__functor(term__atom("\\+"), [A0], _), not([],A) ) :-
	parse_goal(A0, A).
parse_goal_2( term__functor(term__atom("all"),[Vars0,A0],_),all(Vars,A) ):-
	term__vars(Vars0, Vars),
	parse_goal(A0, A).
parse_goal_2( term__functor(term__atom("some"),[Vars0,A0],_),some(Vars,A) ):-
	term__vars(Vars0, Vars),
	parse_goal(A0, A).

:- pred parse_some_vars_goal(term, vars, goal).
:- mode parse_some_vars_goal(in, out, out).
parse_some_vars_goal(A0, Vars, A) :-
	( 
		A0 = term__functor(term__atom("some"), [Vars0,A1], _Context)
	->
		term__vars(Vars0, Vars),
		parse_goal(A1, A)
	;
		Vars = [],
		parse_goal(A0, A)
	).

%-----------------------------------------------------------------------------%

:- pred parse_dcg_clause(varset, term, term, term__context,
			maybe_item_and_context).
:- mode parse_dcg_clause(in, in, in, in, out).

parse_dcg_clause(VarSet0, DCG_Head, DCG_Body, DCG_Context, Result) :-
	new_dcg_var(VarSet0, 0, VarSet1, N0, DCG_0_Var),
	parse_dcg_goal(DCG_Body, VarSet1, N0, DCG_0_Var,
			Body, VarSet, _N, DCG_Var),
	parse_qualified_term(DCG_Head, "DCG clause head", HeadResult),
	process_dcg_clause(HeadResult, VarSet, DCG_0_Var, DCG_Var, Body, R),
	add_context(R, DCG_Context, Result).

%-----------------------------------------------------------------------------%

	% Used to allocate fresh variables needed for the DCG expansion.

:- pred new_dcg_var(varset, int, varset, int, var).
:- mode new_dcg_var(in, in, out, out, out).

new_dcg_var(VarSet0, N0, VarSet, N, DCG_0_Var) :-
	string__int_to_string(N0, StringN),
	string__append("DCG_", StringN, VarName),
	varset__new_var(VarSet0, DCG_0_Var, VarSet1),
	varset__name_var(VarSet1, DCG_0_Var, VarName, VarSet),
	N is N0 + 1.

%-----------------------------------------------------------------------------%

	% Expand a DCG goal.

:- pred parse_dcg_goal(term, varset, int, var, goal, varset, int, var).
:- mode parse_dcg_goal(in, in, in, in, out, out, out, out).

parse_dcg_goal(Term0, VarSet0, N0, Var0, Goal, VarSet, N, Var) :-
	% First check for the special cases:
	(
		parse_dcg_goal_2(Term0, VarSet0, N0, Var0,
				Goal1, VarSet1, N1, Var1)
	->
		Goal = Goal1,
		VarSet = VarSet1,
		N = N1,
		Var = Var1
	;
		% It's the ordinary case of non-terminal.
		% Create a fresh var as the DCG output var from this goal,
		% and append the DCG argument pair to the non-terminal's
		% argument list.

		new_dcg_var(VarSet0, N0, VarSet, N, Var),
		( Term0 = term__functor(term__atom(Functor), Args0, Context) ->
			append(Args0, [
					term__variable(Var0),
					term__variable(Var)
				], Args),
			Term = term__functor(term__atom(Functor), Args, Context)
		;
			Term = term__functor(term__atom("call"), [
					Term,
					term__variable(Var0),
					term__variable(Var)
				], Context)
		),
		Goal = call(Term)
	).

	% parse_dcg_goal_2(Term, VarSet0, N0, Var0, Goal, VarSet, N, Var):
	% VarSet0/VarSet are an accumulator pair which we use to
	% allocate fresh DCG variables; N0 and N are an accumulator pair
	% we use to keep track of the number to give to the next DCG
	% variable (so that we can give it a semi-meaningful name "DCG_<N>"
	% for use in error messages, debugging, etc.).
	% Var0 and Var are an accumulator pair we use to keep track of
	% the current DCG variable.

:- pred parse_dcg_goal_2(term, varset, int, var, goal, varset, int, var).
:- mode parse_dcg_goal_2(in, in, in, in, out, out, out, out) is semidet.

	% Ordinary goal inside { curly braces }.
parse_dcg_goal_2(term__functor(term__atom("{}"),[G],_), VarSet, N, Var,
		Goal, VarSet, N, Var) :-
	parse_goal(G, Goal).

	% Empty list - just unify the input and output DCG args.
parse_dcg_goal_2(term__functor(term__atom("[]"),[],_), VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	new_dcg_var(VarSet0, N0, VarSet, N, Var),
	Goal = unify(term__variable(Var0), term__variable(Var)).

	% Non-empty list of terminals.  Append the DCG output arg
	% as the new tail of the list, and unify the result with
	% the DCG input arg.
parse_dcg_goal_2(term__functor(term__atom("."),[X,Xs],C), VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	new_dcg_var(VarSet0, N0, VarSet, N, Var),
	term_list_append_term(term__functor(term__atom("."),[X,Xs],C),
			term__variable(Var), Term), 
	Goal = unify(term__variable(Var0), Term).

	% Call to '='/1 - unify argument with DCG input arg.
parse_dcg_goal_2(term__functor(term__atom("="),[A],_), VarSet, N, Var,
		Goal, VarSet, N, Var) :-
	Goal = unify(A, term__variable(Var)).

	% If-then (Prolog syntax).
	% We need to add an else part to unify the DCG args.
parse_dcg_goal_2(term__functor(term__atom("->"),[A0,B0],_), VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	parse_some_vars_dcg_goal(A0, SomeVars, VarSet0, N0, Var0,
				A, VarSet1, N1, Var1),
	parse_dcg_goal(B0, VarSet1, N1, Var1, B, VarSet, N, Var),
	Goal = if_then_else(SomeVars, A, B,
		unify(term__variable(Var), term__variable(Var0))).

	% If-then (NU-Prolog syntax).
parse_dcg_goal_2(term__functor(term__atom("if"), [
			term__functor(term__atom("then"),[A0,B0],_)
		],_), VarSet0, N0, Var0, Goal, VarSet, N, Var) :-
	parse_some_vars_dcg_goal(A0, SomeVars, VarSet0, N0, Var0,
				A, VarSet1, N1, Var1),
	parse_dcg_goal(B0, VarSet1, N1, Var1, B, VarSet, N, Var),
	Goal = if_then_else(SomeVars, A, B,
		unify(term__variable(Var), term__variable(Var0))).

	% Conjunction.
parse_dcg_goal_2(term__functor(term__atom(","),[A0,B0],_), VarSet0, N0, Var0,
		(A,B), VarSet, N, Var) :-
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet1, N1, Var1),
	parse_dcg_goal(B0, VarSet1, N1, Var1, B, VarSet, N, Var).

	% Disjunction or if-then-else (Prolog syntax).
parse_dcg_goal_2(term__functor(term__atom(";"),[A0,B0],_), VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	(
		A0 = term__functor(term__atom("->"), [X0,Y0], _Context)
	->
		parse_some_vars_dcg_goal(X0, SomeVars, VarSet0, N0, Var0,
					X, VarSet1, N1, Var1),
		parse_dcg_goal(Y0, VarSet1, N1, Var1, Y, VarSet2, N2, Var),
		parse_dcg_goal(B0, VarSet2, N2, Var0, B, VarSet, N, VarB),
		Goal = if_then_else(SomeVars, X, Y,
			(B, unify(term__variable(Var), term__variable(VarB))))
	;
		parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet1, N1, Var),
		parse_dcg_goal(B0, VarSet1, N1, Var0, B, VarSet, N, VarB),
		Goal = (A ; B, unify(term__variable(Var), term__variable(VarB)))
	).

	% If-then-else (NU-Prolog syntax).
parse_dcg_goal_2( term__functor(term__atom("else"),[
		    term__functor(term__atom("if"),[
			term__functor(term__atom("then"),[A0,B0],_)
		    ],_),
		    C0
		],_), VarSet0, N0, Var0, Goal, VarSet, N, Var) :-
	parse_some_vars_dcg_goal(A0, SomeVars, VarSet0, N0, Var0,
				X, VarSet1, N1, Var1),
	parse_dcg_goal(B0, VarSet1, N1, Var1, Y, VarSet2, N2, Var),
	parse_dcg_goal(C0, VarSet2, N2, Var0, B, VarSet, N, VarB),
	Goal = if_then_else(SomeVars, X, Y,
		(B, unify(term__variable(Var), term__variable(VarB)))).

	% Negation (NU-Prolog syntax).
parse_dcg_goal_2( term__functor(term__atom("not"), [A0], _), VarSet0, N0, Var0,
		not([],A), VarSet, N, Var ) :-
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, _),
	Var = Var0.

	% Negation (Prolog syntax).
parse_dcg_goal_2( term__functor(term__atom("\\+"), [A0], _), VarSet0, N0, Var0,
		not([],A), VarSet, N, Var ) :-
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, _),
	Var = Var0.

	% Universal quantification.
parse_dcg_goal_2(term__functor(term__atom("all"),[Vars0,A0],_),
		VarSet0, N0, Var0, all(Vars,A), VarSet, N, Var) :-
	term__vars(Vars0, Vars),
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, Var).

	% Existential quantification.
parse_dcg_goal_2(term__functor(term__atom("some"),[Vars0,A0],_),
		VarSet0, N0, Var0, some(Vars,A), VarSet, N, Var) :-
	term__vars(Vars0, Vars),
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, Var).

:- pred parse_some_vars_dcg_goal(term, vars, varset, int, var,
				goal, varset, int, var).
:- mode parse_some_vars_dcg_goal(in, in, in, in, in,
				out, out, out, out).
parse_some_vars_dcg_goal(A0, SomeVars, VarSet0, N0, Var0, A, VarSet, N, Var) :-
	(
		A0 = term__functor(term__atom("some"), [SomeVars0,A1], _Context)
	->
		term__vars(SomeVars0, SomeVars),
		parse_dcg_goal(A1, VarSet0, N0, Var0, A, VarSet, N, Var)
	;
		SomeVars = [],
		parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, Var)
	).

	% term_list_append_term(ListTerm, Term, Result):
	% 	if ListTerm is a term representing a proper list, 
	%	this predicate will append the term Term
	%	onto the end of the list

:- pred term_list_append_term(term, term, term).
:- mode term_list_append_term(in, in, out).

term_list_append_term(List0, Term, List) :-
	( List0 = term__functor(term__atom("[]"), [], _Context) ->
		List = Term
	;
		List0 = term__functor(term__atom("."), [Head, Tail0], Context2),
		List = term__functor(term__atom("."), [Head, Tail], Context2),
		term_list_append_term(Tail0, Term, Tail)
	).

:- pred process_dcg_clause(maybe_functor, varset, var, var, goal, maybe(item)).
:- mode process_dcg_clause(in, in, in, in, in, out).
process_dcg_clause(ok(Name, Args0), VarSet, Var0, Var, Body,
		ok(clause(VarSet, Name, Args, Body))) :-
		append(Args0, [term__variable(Var0), term__variable(Var)], Args).
process_dcg_clause(error(ErrMessage, Term), _, _, _, _,
		error(ErrMessage, Term)).

%-----------------------------------------------------------------------------%

	% parse a declaration

:- pred parse_decl(varset, term, maybe(item)).
:- mode parse_decl(in, in, out).
parse_decl(VarSet, F, Result) :-
	( 
		F = term__functor(term__atom(Atom), As, _Context)
	->
		(
			process_decl(VarSet, Atom, As, R)
		->
			Result = R
		;
			Result = error("Unrecognized declaration", F)
		)
	;
		Result = error("Atom expected after `:-'", F)
	).

	% process_decl(VarSet, Atom, Args, Result) succeeds if Atom(Args)
	% is a declaration and binds Result to a representation of that
	% declaration.
:- pred process_decl(varset, string, list(term), maybe(item)).
:- mode process_decl(in, in, in, out).

process_decl(VarSet, "type", [TypeDecl], Result) :-
	parse_type_decl(VarSet, TypeDecl, Result).

process_decl(VarSet, "pred", [PredDecl], Result) :-
	parse_type_decl_pred(VarSet, PredDecl, Result).

/*** OBSOLETE
process_decl(VarSet, "rule", [RuleDecl], Result) :-
	parse_type_decl_rule(VarSet, RuleDecl, Result).
***/

process_decl(VarSet, "mode", [ModeDecl], Result) :-
	parse_mode_decl(VarSet, ModeDecl, Result).

process_decl(VarSet, "inst", [InstDecl], Result) :-
	parse_inst_decl(VarSet, InstDecl, Result).

process_decl(VarSet, "import_module", [ModuleSpec], Result) :-
	parse_import_module_decl(VarSet, ModuleSpec, Result).

process_decl(VarSet, "use_module", [ModuleSpec], Result) :-
	parse_use_module_decl(VarSet, ModuleSpec, Result).

process_decl(VarSet, "export_module", [ModuleSpec], Result) :-
	parse_export_module_decl(VarSet, ModuleSpec, Result).

process_decl(VarSet, "import_sym", [SymSpec], Result) :-
	parse_import_sym_decl(VarSet, SymSpec, Result).

process_decl(VarSet, "use_sym", [SymSpec], Result) :-
	parse_use_sym_decl(VarSet, SymSpec, Result).

process_decl(VarSet, "export_sym", [SymSpec], Result) :-
	parse_export_sym_decl(VarSet, SymSpec, Result).

process_decl(VarSet, "import_pred", [PredSpec], Result) :-
	parse_import_pred_decl(VarSet, PredSpec, Result).

process_decl(VarSet, "use_pred", [PredSpec], Result) :-
	parse_use_pred_decl(VarSet, PredSpec, Result).

process_decl(VarSet, "export_pred", [PredSpec], Result) :-
	parse_export_pred_decl(VarSet, PredSpec, Result).

process_decl(VarSet, "import_cons", [ConsSpec], Result) :-
	parse_import_cons_decl(VarSet, ConsSpec, Result).

process_decl(VarSet, "use_cons", [ConsSpec], Result) :-
	parse_use_cons_decl(VarSet, ConsSpec, Result).

process_decl(VarSet, "export_cons", [ConsSpec], Result) :-
	parse_export_cons_decl(VarSet, ConsSpec, Result).

process_decl(VarSet, "import_type", [TypeSpec], Result) :-
	parse_import_type_decl(VarSet, TypeSpec, Result).

process_decl(VarSet, "use_type", [TypeSpec], Result) :-
	parse_use_type_decl(VarSet, TypeSpec, Result).

process_decl(VarSet, "export_type", [TypeSpec], Result) :-
	parse_export_type_decl(VarSet, TypeSpec, Result).

process_decl(VarSet, "import_adt", [ADT_Spec], Result) :-
	parse_import_adt_decl(VarSet, ADT_Spec, Result).

process_decl(VarSet, "use_adt", [ADT_Spec], Result) :-
	parse_use_adt_decl(VarSet, ADT_Spec, Result).

process_decl(VarSet, "export_adt", [ADT_Spec], Result) :-
	parse_export_adt_decl(VarSet, ADT_Spec, Result).

process_decl(VarSet, "import_op", [OpSpec], Result) :-
	parse_import_op_decl(VarSet, OpSpec, Result).

process_decl(VarSet, "use_op", [OpSpec], Result) :-
	parse_use_op_decl(VarSet, OpSpec, Result).

process_decl(VarSet, "export_op", [OpSpec], Result) :-
	parse_export_op_decl(VarSet, OpSpec, Result).

process_decl(VarSet, "interface", [], ok(module_defn(VarSet, interface))).
process_decl(VarSet, "implementation", [],
				ok(module_defn(VarSet, implementation))).

process_decl(VarSet, "module", [ModuleName], Result) :-
	(
		ModuleName = term__functor(term__atom(Module), [], _Context)
	->
		Result = ok(module_defn(VarSet, module(Module)))
	;
		Result = error("Module name expected", ModuleName)
	).

process_decl(VarSet, "end_module", [ModuleName], Result) :-
	(
		ModuleName = term__functor(term__atom(Module), [], _Context)
	->
		Result = ok(module_defn(VarSet, end_module(Module)))
	;
		Result = error("Module name expected", ModuleName)
	).

	% NU-Prolog `when' declarations are silently ignored for
	% backwards compatibility.
process_decl(_VarSet, "when", [_Goal, _Cond], Result) :-
	Result = ok(nothing).

:- pred parse_type_decl(varset, term, maybe(item)).
:- mode parse_type_decl(in, in, out).
parse_type_decl(VarSet, TypeDecl, Result) :-
	( %%% some [R, Cond]
		parse_type_decl_type(TypeDecl, Cond, R) 
	->
		R1 = R,
		Cond1 = Cond
	;
		process_abstract_type(TypeDecl, R1),
		Cond1 = true
	),
	parse_type_decl_2(R1, VarSet, Cond1, Result).

:- pred parse_type_decl_2(maybe(type_defn), varset, condition, maybe(item)).
:- mode parse_type_decl_2(in, in, in, out).
parse_type_decl_2(error(Error, Term), _, _, error(Error, Term)).
parse_type_decl_2(ok(TypeDefn), VarSet, Cond,
					ok(type_defn(VarSet, TypeDefn, Cond))).
		% we should check the condition for errs
		% (don't bother at the moment, since we ignore
		% conditions anyhow :-)

%-----------------------------------------------------------------------------%

	% add a warning message to the list of messages

:- pred add_warning(string, term, message_list, message_list).
:- mode add_warning(in, in, out, in).
add_warning(Warning, Term, [Msg - Term | Msgs], Msgs) :-
	string__append("Warning: ", Warning, Msg).

	% add an error message to the list of messages

:- pred add_error(string, term, message_list, message_list).
:- mode add_error(in, in, in, out).
add_error(Error, Term, Msgs, [Msg - Term | Msgs]) :-
	string__append("Error: ", Error, Msg).

%-----------------------------------------------------------------------------%
	% parse_type_decl_type(Term, Condition, Result) succeeds
	% if Term is a "type" type declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.

:- pred parse_type_decl_type(term, condition, maybe(type_defn)).
:- mode parse_type_decl_type(in, in, out).

parse_type_decl_type(term__functor(term__atom("--->"),[H,B],_), Condition, R) :-
	get_condition(B, Body, Condition),
	process_du_type(H, Body, R).

parse_type_decl_type(term__functor(term__atom("="),[H,B],_), Condition, R) :-
	get_condition(B, Body, Condition),
	process_uu_type(H, Body, R).
	
parse_type_decl_type(term__functor(term__atom("=="),[H,B],_), Condition, R) :-
	get_condition(B, Body, Condition),
	process_eqv_type(H, Body, R).

%-----------------------------------------------------------------------------%
	% parse_type_decl_pred(Pred, Condition, Result) succeeds
	% if Pred is a predicate type declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.
:- pred parse_type_decl_pred(varset, term, maybe(item)).
:- mode parse_type_decl_pred(in, in, out).
parse_type_decl_pred(VarSet, Pred, R) :-
	get_condition(Pred, Body, Condition),
	get_determinism(Body, Body2, Determinism),
	process_pred(VarSet, Body2, Determinism, Condition, R).

%-----------------------------------------------------------------------------%

/*** OBSOLETE
	% parse_type_decl_rule(VarSet, Rule, Result) succeeds
	% if Rule is a "rule" type declaration, and binds Result to
	% a representation of the declaration.
	% ("rule" here means DCG predicate, not horn clause.)
:- pred parse_type_decl_rule(varset, term, maybe(item)).
:- mode parse_type_decl_rule(in, in, out).
parse_type_decl_rule(VarSet, Rule, R) :-
	get_condition(Rule, Body, Condition),
	process_mode(VarSet, Body, Condition, R).
****/

%-----------------------------------------------------------------------------%
	% parse_mode_decl_pred(Pred, Condition, Result) succeeds
	% if Pred is a predicate mode declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.
:- pred parse_mode_decl_pred(varset, term, maybe(item)).
:- mode parse_mode_decl_pred(in, in, out).
parse_mode_decl_pred(VarSet, Pred, R) :-
	get_condition(Pred, Body, Condition),
	get_determinism(Body, Body2, Determinism),
	process_mode(VarSet, Body2, Determinism, Condition, R).

%-----------------------------------------------------------------------------%

	% get_determinism(Term0, Term, Determinism) binds Determinism
	% to a representation of the determinism condition of Term0, if any,
	% and binds Term to the other part of Term0. If Term0 does not
	% contain a determinism, then Determinism is bound to `unspecified'.

:- pred get_determinism(term, term, determinism).
:- mode get_determinism(in, out, out).
get_determinism(B, Body, Determinism) :-
	( %%% some [Body1, Determinism1, Context]
		B = term__functor(term__atom("is"), [Body1, Determinism1],
					_Context)
	->
		Body = Body1,
		( 
		    (
			Determinism1 = term__functor(term__atom(Determinism2),
				[], _Context2),
			standard_det(Determinism2, Determinism3)
		    )
		->
			Determinism = Determinism3
		;
			% XXX should report a syntax error!!
			Determinism = unspecified
		)
	;
		Body = B,
		Determinism = unspecified
	).

:- pred standard_det(string, determinism).
:- mode standard_det(in, out).
standard_det("det", det).
standard_det("nondet", nondet).
standard_det("semidet", semidet).

%-----------------------------------------------------------------------------%

	% get_condition(Term0, Term, Condition) binds Condition
	% to a representation of the 'where' condition of Term0, if any,
	% and binds Term to the other part of Term0. If Term0 does not
	% contain a condition, then Condition is bound to true.

:- pred get_condition(term, term, condition).
:- mode get_condition(in, out, out).
get_condition(B, Body, Condition) :-
	( 
		B = term__functor(term__atom("where"), [Body1, Condition1],
					_Context)
	->
		Body = Body1,
		Condition = where(Condition1)
	;
		Body = B,
		Condition = true
	).

%-----------------------------------------------------------------------------%

	% This is for "Head = Body" (undiscriminated union) definitions.
:- pred process_uu_type(term, term, maybe(type_defn)).
:- mode process_uu_type(in, in, out).
process_uu_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_uu_type_2(Result0, Body, Result).

:- pred process_uu_type_2(maybe_functor, term, maybe(type_defn)).
:- mode process_uu_type_2(in, in, out).
process_uu_type_2(error(Error, Term), _, error(Error, Term)).
process_uu_type_2(ok(Name, Args), Body, ok(uu_type(Name,Args,List))) :-
		sum_to_list(Body, List).

%-----------------------------------------------------------------------------%

	% This is for "Head == Body" (equivalence) definitions.
:- pred process_eqv_type(term, term, maybe(type_defn)).
:- mode process_eqv_type(in, in, out).
process_eqv_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_eqv_type_2(Result0, Body, Result).

:- pred process_eqv_type_2(maybe_functor, term, maybe(type_defn)).
:- mode process_eqv_type_2(in, in, out).
process_eqv_type_2(error(Error, Term), _, error(Error, Term)).
process_eqv_type_2(ok(Name, Args), Body, ok(eqv_type(Name,Args,Body))).

%-----------------------------------------------------------------------------%

	% process_du_type(TypeHead, TypeBody, Result)
	% checks that its arguments are well formed, and if they are,
	% binds Result to a representation of the type information about the
	% TypeHead.
	% This is for "Head ---> Body" (constructor) definitions.
:- pred process_du_type(term, term, maybe(type_defn)).
:- mode process_du_type(in, in, out).
process_du_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_du_type_2(Result0, Body, Result).

:- pred process_du_type_2(maybe_functor, term, maybe(type_defn)).
:- mode process_du_type_2(in, in, out).
process_du_type_2(error(Error, Term), _, error(Error, Term)).
process_du_type_2(ok(Functor,Args), Body, Result) :-
	% check that body is a disjunction of constructors
	( %%% some [Constrs] 
		convert_constructors(Body, Constrs)
	->
		Result = ok(du_type(Functor, Args, Constrs))
	;
		Result = error("Invalid RHS of type definition", Body)
	).

%-----------------------------------------------------------------------------%

	% process_abstract_type(TypeHead, Result)
	% checks that its argument is well formed, and if it is,
	% binds Result to a representation of the type information about the
	% TypeHead.

:- pred process_abstract_type(term, maybe(type_defn)).
:- mode process_abstract_type(in, out).
process_abstract_type(Head, Result) :-
	dummy_term(Body),
	check_for_errors(Head, Body, Result0),
	process_abstract_type_2(Result0, Result).

:- pred process_abstract_type_2(maybe_functor, maybe(type_defn)).
:- mode process_abstract_type_2(in, out).
process_abstract_type_2(error(Error, Term), error(Error, Term)).
process_abstract_type_2(ok(Functor, Args), ok(abstract_type(Functor, Args))).

%-----------------------------------------------------------------------------%

	%  check a type definition for errors
	
:- pred check_for_errors(term, term, maybe_functor).
:- mode check_for_errors(in, in, out).
check_for_errors(Term, _, error("Variable on LHS of type definition", Term)) :-
	Term = term__variable(_).
check_for_errors(Term, Body, Result) :-
	Term = term__functor(_,_,_),
	parse_qualified_term(Term, "type definition", R),
	check_for_errors_2(R, Body, Term, Result).

:- pred check_for_errors_2(maybe_functor, term, term, maybe_functor).
:- mode check_for_errors_2(in, in, in, out).
check_for_errors_2(error(Msg, Term), _, _, error(Msg, Term)).
check_for_errors_2(ok(Name, Args), Body, Term, Result) :-
	check_for_errors_3(Name, Args, Body, Term, Result).

:- pred check_for_errors_3(sym_name, list(term), term, term, maybe_functor).
:- mode check_for_errors_3(in, in, in, in, out).
check_for_errors_3(Name, Args, Body, Term, Result) :-
	% check that all the head args are variables
	( %%%	some [Arg]
		(
			member(Arg, Args),
			Arg \= term__variable(_)
		)
	->
		Result = error("Type parameters must be variables", Arg)
	;
	% check that all the head arg variables are distinct
	  %%%	some [Arg2, OtherArgs]
		(
			member(Arg2, Args, [Arg2|OtherArgs]),
			member(Arg2, OtherArgs)
		)
	->
		Result = error("Repeated type parameters in LHS of type defn", Term)
	% check that all the variables in the body occur in the head
	; %%% some [Var2]
		(
			term__contains_var(Body, Var2),
			not term__contains_var_list(Args, Var2)
		)
	->
		Result = error("Free type parameter in RHS of type definition",
				Body)
	;
		Result = ok(Name, Args)
	).

%-----------------------------------------------------------------------------%

	% Convert a list of terms separated by semi-colons
	% (known as a "disjunction", even thought the terms aren't goals
	% in this case) into a list of constructors

:- pred convert_constructors(term, list(constructor)).
:- mode convert_constructors(in, out).
convert_constructors(Body, Constrs) :-
	disjunction_to_list(Body, List),
	convert_constructors_2(List, Constrs).

	% true if input argument is a valid list of constructors

:- pred convert_constructors_2(list(term), list(constructor)).
:- mode convert_constructors_2(in, out).
convert_constructors_2([], []).
convert_constructors_2(Term.Terms, Constr.Constrs) :-
	convert_constructor(Term, Constr),
	convert_constructors_2(Terms, Constrs).

	% true if input argument is a valid constructor.
	% Note that as a special case, one level of
	% curly braces around the constructor are ignored.
	% This is to allow you to define ';'/2 constructors.

:- pred convert_constructor(term, constructor).
:- mode convert_constructor(in, out).
convert_constructor(Term, Result) :-
	( 
		Term = term__functor(term__atom("{}"), [Term1], _Context)
	->
		Term2 = Term1
	;
		Term2 = Term
	),
	parse_qualified_term(Term2, "", ok(F, As)),
	convert_type_list(As, ArgTypes),
	Result = F - ArgTypes.

%-----------------------------------------------------------------------------%

	% convert a "disjunction" (bunch of terms separated by ';'s) to a list

:- pred disjunction_to_list(term, list(term)).
:- mode disjunction_to_list(in, out).
disjunction_to_list(Term, List) :-
	binop_term_to_list(";", Term, List).

	% convert a "conjunction" (bunch of terms separated by ','s) to a list

:- pred conjunction_to_list(term, list(term)).
:- mode conjunction_to_list(in, out).
conjunction_to_list(Term, List) :-
	binop_term_to_list(",", Term, List).

	% convert a "sum" (bunch of terms separated by '+' operators) to a list

:- pred sum_to_list(term, list(term)).
:- mode sum_to_list(in, out).
sum_to_list(Term, List) :-
	binop_term_to_list("+", Term, List).

	% general predicate to convert terms separated by any specified
	% operator into a list

:- pred binop_term_to_list(string, term, list(term)).
:- mode binop_term_to_list(in, in, out).
binop_term_to_list(Op, Term, List) :-
	binop_term_to_list_2(Op, Term, [], List).

:- pred binop_term_to_list_2(string, term, list(term), list(term)).
:- mode binop_term_to_list_2(in, in, in, out).
binop_term_to_list_2(Op, Term, List0, List) :-
	(
		Term = term__functor(term__atom(Op), [L, R], _Context)
	->
		binop_term_to_list_2(Op, R, List0, List1),
		binop_term_to_list_2(Op, L, List1, List)
	;
		List = [Term|List0]
	).

%-----------------------------------------------------------------------------%

	% parse a `:- pred p(...)' declaration

:- pred process_pred(varset, term, determinism, condition, maybe(item)).
:- mode process_pred(in, in, in, in, out).
process_pred(VarSet, PredType, Det, Cond, Result) :-
	parse_qualified_term(PredType, "`:- pred' declaration", R),
	process_pred_2(R, PredType, VarSet, Det, Cond, Result).

:- pred process_pred_2(maybe_functor, term, varset, determinism, condition,
			maybe(item)).
:- mode process_pred_2(in, in, in, in, in, out).
process_pred_2(ok(F, As0), PredType, VarSet, Det, Cond, Result) :-
	( %%% some [As]
		convert_type_and_mode_list(As0, As)
	->
		Result = ok(pred(VarSet, F, As, Det, Cond))
	;
		Result = error("Syntax error in :- pred declaration", PredType)
	).
process_pred_2(error(M, T), _, _, _, _, error(M, T)).

	% parse a `:- mode p(...)' declaration

:- pred process_mode(varset, term, determinism, condition, maybe(item)).
:- mode process_mode(in, in, in, in, out).
process_mode(VarSet, PredMode, Det, Cond, Result) :-
	parse_qualified_term(PredMode, "`:- mode' declaration", R),
	process_mode_2(R, PredMode, VarSet, Det, Cond, Result).

:- pred process_mode_2(maybe_functor, term, varset, determinism, condition,
			maybe(item)).
:- mode process_mode_2(in, in, in, in, in, out).
process_mode_2(ok(F, As0), PredMode, VarSet, Det, Cond, Result) :-
	( %%% some [As]
		convert_mode_list(As0, As)
	->
		Result = ok(mode(VarSet, F, As, Det, Cond))
	;
		Result = error("Syntax error in predicate mode declaration",
				PredMode)
	).
process_mode_2(error(M, T), _, _, _, _, error(M, T)).

/*** OBSOLETE
	% A rule declaration is just the same as a pred declaration,
	% except that it is for DCG rules, so there are two hidden arguments. 

:- pred process_rule(varset, term, condition, maybe(item)).
:- mode process_rule(in, in, in, out).
process_rule(VarSet, RuleType, Cond, Result) :-
	parse_qualified_term(RuleType, "`:- rule' declaration", R),
	process_rule_2(R, VarSet, Cond, Result).

:- pred process_rule_2(maybe_functor, varset, condition, maybe(item)).
:- mode process_rule_2(in, in, in, out).
process_rule_2(ok(F, As), VarSet, Cond, ok(rule(VarSet, F, As, Cond))).
process_rule_2(error(M, T), _, _, error(M, T)).
***/

/*** JUNK
process_rule(VarSet, RuleType, Cond, Result) :-
	varset__new_var(VarSet, Var, VarSet1),
	RuleType = term__functor(F, RuleArgs, _),
	append(RuleArgs, [Var, Var], PredArgs),
	PredType = term__functor(F, PredArgs, _),
	process_pred(VarSet1, PredType, Cond, Result).
***/

%-----------------------------------------------------------------------------%

	% parse a `:- inst foo = ...' definition

:- pred parse_inst_decl(varset, term, maybe(item)).
:- mode parse_inst_decl(in, in, out).
parse_inst_decl(VarSet, InstDefn, Result) :-
	(
		InstDefn = term__functor(term__atom("="), [H,B], _Context)
	->
		get_condition(B, Body, Condition),
		convert_inst_defn(H, Body, R),
		process_inst_defn(R, VarSet, Condition, Result)
	;
		Result = error("`=' expected in `:- inst' definition", InstDefn)
	).

		% we should check the condition for errs
		% (don't bother at the moment, since we ignore
		% conditions anyhow :-)

:- pred convert_inst_defn(term, term, maybe(inst_defn)).
:- mode convert_inst_defn(in, in, out).
convert_inst_defn(Head, Body, Result) :-
	parse_qualified_term(Head, "inst definition", R),
	convert_inst_defn_2(R, Head, Body, Result).

:- pred convert_inst_defn_2(maybe_functor, term, term, maybe(inst_defn)).
:- mode convert_inst_defn_2(in, in, in, out).
convert_inst_defn_2(error(M,T), _, _, error(M,T)).
convert_inst_defn_2(ok(Name, Args), Head, Body, Result) :-
	% check that all the head args are variables
	( %%%	some [Arg]
		(
			member(Arg, Args),
			Arg \= term__variable(_)
		)
	->
		Result = error("Inst parameters must be variables", Arg)
	;
	% check that all the head arg variables are distinct
	%%%	some [Arg2, OtherArgs]
		(
			member(Arg2, Args, Arg2.OtherArgs),
			member(Arg2, OtherArgs)
		)
	->
		Result = error("Repeated inst parameters in LHS of inst defn",
				Head)
	;
	% check that all the variables in the body occur in the head
	%%%	some [Var2]
		(
			term__contains_var(Body, Var2),
			not term__contains_var_list(Args, Var2)
		)
	->
		Result = error("Free inst parameter in RHS of inst definition",
				Body)
	;
		% should improve the error message here

		( %%% some [ConvertedBody]
			convert_inst(Body, ConvertedBody)
		->
			Result = ok(eqv_inst(Name, Args, ConvertedBody))
		;
			Result = error("Syntax error in inst body", Body)
		)
	).

:- pred convert_inst_list(list(term), list(inst)).
:- mode convert_inst_list(in, out).
convert_inst_list([], []).
convert_inst_list([H0|T0], [H|T]) :-
	convert_inst(H0, H),
	convert_inst_list(T0, T).

:- pred convert_inst(term, inst).
:- mode convert_inst(in, out).
convert_inst(term__variable(V), inst_var(V)).
convert_inst(term__functor(Name, Args0, Context), Result) :-
	( Name = term__atom("free"), Args0 = [] ->
		Result = free
	; Name = term__atom("ground"), Args0 = [] ->
		Result = ground
	;
		(   ( Name = term__atom("bound")
		    ; Name = term__atom("bound_unique")
		    ),
		    Args0 = [Disj]
		)
	->
		disjunction_to_list(Disj, List),
		convert_bound_inst_list(List, Functors0),
		sort(Functors0, Functors),
		Result = bound(Functors)
	;
		parse_qualified_term(term__functor(Name, Args0, Context),
			"", ok(QualifiedName, Args1)),
		convert_inst_list(Args1, Args),
		Result = user_defined_inst(QualifiedName, Args)
	).

:- pred convert_bound_inst_list(list(term), list(bound_inst)).
:- mode convert_bound_inst_list(in, out).
convert_bound_inst_list([], []).
convert_bound_inst_list([H0|T0], [H|T]) :-
	convert_bound_inst(H0, H),
	convert_bound_inst_list(T0, T).

:- pred convert_bound_inst(term, bound_inst).
:- mode convert_bound_inst(in, out).
convert_bound_inst(term__functor(Name, Args0, _), functor(Name, Args)) :-
	convert_inst_list(Args0, Args).

:- pred process_inst_defn(maybe(inst_defn), varset, condition, maybe(item)).
:- mode process_inst_defn(in, in, in, out).
process_inst_defn(error(Error, Term), _, _, error(Error, Term)).
process_inst_defn(ok(InstDefn), VarSet, Cond,
					ok(inst_defn(VarSet, InstDefn, Cond))).

%-----------------------------------------------------------------------------%

	% parse a `:- mode foo :: ...' or `:- mode foo = ...' definition.

:- pred parse_mode_decl(varset, term, maybe(item)).
:- mode parse_mode_decl(in, in, out).
parse_mode_decl(VarSet, ModeDefn, Result) :-
	( %%% some [H,B]
		mode_op(ModeDefn, H, B)
	->
		get_condition(B, Body, Condition),
		convert_mode_defn(H, Body, R),
		process_mode_defn(R, VarSet, Condition, Result)
	;
		parse_mode_decl_pred(VarSet, ModeDefn, Result)
	).

:- pred mode_op(term, term, term).
:- mode mode_op(in, in, out).
mode_op(term__functor(term__atom("::"),[H,B],_), H, B).
mode_op(term__functor(term__atom("="),[H,B],_), H, B).

:- pred convert_mode_defn(term, term, maybe(mode_defn)).
:- mode convert_mode_defn(in, in, out).
convert_mode_defn(Head, Body, Result) :-
	parse_qualified_term(Head, "mode definition", R),
	convert_mode_defn_2(R, Head, Body, Result).

:- pred convert_mode_defn_2(maybe_functor, term, term, maybe(mode_defn)).
:- mode convert_mode_defn_2(in, in, in, out).
convert_mode_defn_2(error(M,T), _, _, error(M,T)).
convert_mode_defn_2(ok(Name, Args), Head, Body, Result) :-
	% check that all the head args are variables
	( %%% some [Arg]
		(
			member(Arg, Args),
			Arg \= term__variable(_)
		)
	->
		Result = error("Mode parameters must be variables", Arg)
	;
	% check that all the head arg variables are distinct
		%%% some [Arg2, OtherArgs]
		(
			member(Arg2, Args, Arg2.OtherArgs),
			member(Arg2, OtherArgs)
		)
	->
		Result = error("Repeated parameters in LHS of mode defn",
				Head)
	% check that all the variables in the body occur in the head
	; %%% some [Var2]
		(
			term__contains_var(Body, Var2),
			not term__contains_var_list(Args, Var2)
		)
	->
		Result = error("Free inst parameter in RHS of mode definition",
				Body)
	;
		% should improve the error message here

		( %%% some [ConvertedBody]
			convert_mode(Body, ConvertedBody)
		->
			Result = ok(eqv_mode(Name, Args, ConvertedBody))
		;
			% catch-all error message - we should do
			% better than this
			Result = error("Syntax error in mode definition body",
					Body)
		)
	).

:- pred convert_type_and_mode_list(list(term), list(type_and_mode)).
:- mode convert_type_and_mode_list(in, out).
convert_type_and_mode_list([], []).
convert_type_and_mode_list([H0|T0], [H|T]) :-
	convert_type_and_mode(H0, H),
	convert_type_and_mode_list(T0, T).

:- pred convert_type_and_mode(term, type_and_mode).
:- mode convert_type_and_mode(in, out).
convert_type_and_mode(Term, Result) :-
	(
		Term = term__functor(term__atom("::"), [TypeTerm, ModeTerm],
				_Context)
	->
		convert_type(TypeTerm, Type),
		convert_mode(ModeTerm, Mode),
		Result = type_and_mode(Type, Mode)
	;
		convert_type(Term, Type),
		Result = type_only(Type)
	).

:- pred convert_mode_list(list(term), list(mode)).
:- mode convert_mode_list(in, out).
convert_mode_list([], []).
convert_mode_list([H0|T0], [H|T]) :-
	convert_mode(H0, H),
	convert_mode_list(T0, T).

:- pred convert_mode(term, mode).
:- mode convert_mode(in, out).
convert_mode(Term, Mode) :-
	(
		Term = term__functor(term__atom("->"), [InstA, InstB], _Context)
	->
		convert_inst(InstA, ConvertedInstA),
		convert_inst(InstB, ConvertedInstB),
		Mode = (ConvertedInstA -> ConvertedInstB)
	;
		parse_qualified_term(Term, "mode definition", R),
		R = ok(Name, Args),	% should improve error reporting
		convert_inst_list(Args, ConvertedArgs),
		Mode = user_defined_mode(Name, ConvertedArgs)
	).

:- pred process_mode_defn(maybe(mode_defn), varset, condition, maybe(item)).
:- mode process_mode_defn(in, in, in, out).
process_mode_defn(error(Error, Term), _, _, error(Error, Term)).
process_mode_defn(ok(ModeDefn), VarSet, Cond,
					ok(mode_defn(VarSet, ModeDefn, Cond))).

%-----------------------------------------------------------------------------%

% parse {import,use,export}_module declarations

:- pred parse_import_module_decl(varset, term, maybe(item)).
:- mode parse_import_module_decl(in, in, out).
parse_import_module_decl(VarSet, ModuleSpec, Result) :-
	parse_module_spec_list(ModuleSpec, R),
	process_import(R, VarSet, Result).

:- pred parse_use_module_decl(varset, term, maybe(item)).
:- mode parse_use_module_decl(in, in, out).
parse_use_module_decl(VarSet, ModuleSpec, Result) :-
	parse_module_spec_list(ModuleSpec, R),
	process_use(R, VarSet, Result).

:- pred parse_export_module_decl(varset, term, maybe(item)).
:- mode parse_export_module_decl(in, in, out).
parse_export_module_decl(VarSet, ModuleSpec, Result) :-
	parse_module_spec_list(ModuleSpec, R),
	process_export(R, VarSet, Result).

% parse {import,use,export}_sym declarations

:- pred parse_export_sym_decl(varset, term, maybe(item)).
:- mode parse_export_sym_decl(in, in, out).
parse_export_sym_decl(VarSet, SymSpec, Result) :-
	parse_sym_spec_list(SymSpec, R),
	process_export(R, VarSet, Result).

:- pred parse_import_sym_decl(varset, term, maybe(item)).
:- mode parse_import_sym_decl(in, in, out).
parse_import_sym_decl(VarSet, SymSpec, Result) :-
	parse_sym_spec_list(SymSpec, R),
	process_import(R, VarSet, Result).

:- pred parse_use_sym_decl(varset, term, maybe(item)).
:- mode parse_use_sym_decl(in, in, out).
parse_use_sym_decl(VarSet, SymSpec, Result) :-
	parse_sym_spec_list(SymSpec, R),
	process_use(R, VarSet, Result).

% parse {import,use,export}_pred declarations

:- pred parse_import_pred_decl(varset, term, maybe(item)).
:- mode parse_import_pred_decl(in, in, out).
parse_import_pred_decl(VarSet, PredSpec, Result) :-
	parse_pred_spec_list(PredSpec, R),
	process_import(R, VarSet, Result).

:- pred parse_use_pred_decl(varset, term, maybe(item)).
:- mode parse_use_pred_decl(in, in, out).
parse_use_pred_decl(VarSet, PredSpec, Result) :-
	parse_pred_spec_list(PredSpec, R),
	process_use(R, VarSet, Result).

:- pred parse_export_pred_decl(varset, term, maybe(item)).
:- mode parse_export_pred_decl(in, in, out).
parse_export_pred_decl(VarSet, PredSpec, Result) :-
	parse_pred_spec_list(PredSpec, R),
	process_export(R, VarSet, Result).

% parse {import,use,export}_cons declarations

:- pred parse_import_cons_decl(varset, term, maybe(item)).
:- mode parse_import_cons_decl(in, in, out).
parse_import_cons_decl(VarSet, ConsSpec, Result) :-
	parse_cons_spec_list(ConsSpec, R),
	process_import(R, VarSet, Result).

:- pred parse_use_cons_decl(varset, term, maybe(item)).
:- mode parse_use_cons_decl(in, in, out).
parse_use_cons_decl(VarSet, ConsSpec, Result) :-
	parse_cons_spec_list(ConsSpec, R),
	process_use(R, VarSet, Result).

:- pred parse_export_cons_decl(varset, term, maybe(item)).
:- mode parse_export_cons_decl(in, in, out).
parse_export_cons_decl(VarSet, ConsSpec, Result) :-
	parse_cons_spec_list(ConsSpec, R),
	process_export(R, VarSet, Result).

% parse {import,use,export}_type declarations

:- pred parse_import_type_decl(varset, term, maybe(item)).
:- mode parse_import_type_decl(in, in, out).
parse_import_type_decl(VarSet, TypeSpec, Result) :-
	parse_type_spec_list(TypeSpec, R),
	process_import(R, VarSet, Result).

:- pred parse_use_type_decl(varset, term, maybe(item)).
:- mode parse_use_type_decl(in, in, out).
parse_use_type_decl(VarSet, TypeSpec, Result) :-
	parse_type_spec_list(TypeSpec, R),
	process_use(R, VarSet, Result).

:- pred parse_export_type_decl(varset, term, maybe(item)).
:- mode parse_export_type_decl(in, in, out).
parse_export_type_decl(VarSet, TypeSpec, Result) :-
	parse_type_spec_list(TypeSpec, R),
	process_export(R, VarSet, Result).

% parse {import,use,export}_adt declarations

:- pred parse_import_adt_decl(varset, term, maybe(item)).
:- mode parse_import_adt_decl(in, in, out).
parse_import_adt_decl(VarSet, ADT_Spec, Result) :-
	parse_adt_spec_list(ADT_Spec, R),
	process_import(R, VarSet, Result).

:- pred parse_use_adt_decl(varset, term, maybe(item)).
:- mode parse_use_adt_decl(in, in, out).
parse_use_adt_decl(VarSet, ADT_Spec, Result) :-
	parse_adt_spec_list(ADT_Spec, R),
	process_use(R, VarSet, Result).

:- pred parse_export_adt_decl(varset, term, maybe(item)).
:- mode parse_export_adt_decl(in, in, out).
parse_export_adt_decl(VarSet, ADT_Spec, Result) :-
	parse_adt_spec_list(ADT_Spec, R),
	process_export(R, VarSet, Result).

% parse {import,use,export}_op declarations

:- pred parse_import_op_decl(varset, term, maybe(item)).
:- mode parse_import_op_decl(in, in, out).
parse_import_op_decl(VarSet, OpSpec, Result) :-
	parse_op_spec_list(OpSpec, R),
	process_import(R, VarSet, Result).

:- pred parse_use_op_decl(varset, term, maybe(item)).
:- mode parse_use_op_decl(in, in, out).
parse_use_op_decl(VarSet, OpSpec, Result) :-
	parse_op_spec_list(OpSpec, R),
	process_use(R, VarSet, Result).

:- pred parse_export_op_decl(varset, term, maybe(item)).
:- mode parse_export_op_decl(in, in, out).
parse_export_op_decl(VarSet, OpSpec, Result) :-
	parse_op_spec_list(OpSpec, R),
	process_export(R, VarSet, Result).

%-----------------------------------------------------------------------------%

	% Parse a comma-separated list (misleading described as
	% a "conjunction") of module specifiers.

:- pred parse_module_spec_list(term, maybe(sym_list)).
:- mode parse_module_spec_list(in, out).
parse_module_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_module_spec_list_2(List, R),
	process_module_spec_list(R, Result).

:- pred parse_module_spec_list_2(list(term), maybe(list(module_specifier))).
:- mode parse_module_spec_list_2(in, out).
parse_module_spec_list_2([], ok([])).
parse_module_spec_list_2(X.Xs, Result) :-
	parse_module_specifier(X, X_Result),
	parse_module_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

:- pred process_module_spec_list(maybe(list(module_specifier)),
				 maybe(sym_list)).
:- mode process_module_spec_list(in, out).
process_module_spec_list(ok(X), ok(module(X))).
process_module_spec_list(error(M, T), error(M, T)).

	% Parse a comma-separated list (misleading described as
	% a "conjunction") of symbol specifiers.

:- pred parse_sym_spec_list(term, maybe(sym_list)).
:- mode parse_sym_spec_list(in, out).
parse_sym_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_sym_spec_list_2(List, R),
	process_sym_spec_list(R, Result).

:- pred parse_sym_spec_list_2(list(term), maybe(list(sym_specifier))).
:- mode parse_sym_spec_list_2(in, out).
parse_sym_spec_list_2([], ok([])).
parse_sym_spec_list_2(X.Xs, Result) :-
	parse_symbol_specifier(X, X_Result),
	parse_sym_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

:- pred process_sym_spec_list(maybe(list(sym_specifier)),
				 maybe(sym_list)).
:- mode process_sym_spec_list(in, out).
process_sym_spec_list(ok(X), ok(sym(X))).
process_sym_spec_list(error(M, T), error(M, T)).


	% Parse a comma-separated list (misleading described as
	% a "conjunction") of predicate specifiers.

:- pred parse_pred_spec_list(term, maybe(sym_list)).
:- mode parse_pred_spec_list(in, out).
parse_pred_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_pred_spec_list_2(List, R),
	process_pred_spec_list(R, Result).

:- pred parse_pred_spec_list_2(list(term), maybe(list(pred_specifier))).
:- mode parse_pred_spec_list_2(in, out).
parse_pred_spec_list_2([], ok([])).
parse_pred_spec_list_2(X.Xs, Result) :-
	parse_predicate_specifier(X, X_Result),
	parse_pred_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

:- pred process_pred_spec_list(maybe(list(pred_specifier)),
				 maybe(sym_list)).
:- mode process_pred_spec_list(in, out).
process_pred_spec_list(ok(X), ok(pred(X))).
process_pred_spec_list(error(M, T), error(M, T)).


	% Parse a comma-separated list (misleading described as
	% a "conjunction") of constructor specifiers.

:- pred parse_cons_spec_list(term, maybe(sym_list)).
:- mode parse_cons_spec_list(in, out).
parse_cons_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_cons_spec_list_2(List, R),
	process_cons_spec_list(R, Result).

:- pred parse_cons_spec_list_2(list(term), maybe(list(cons_specifier))).
:- mode parse_cons_spec_list_2(in, out).
parse_cons_spec_list_2([], ok([])).
parse_cons_spec_list_2(X.Xs, Result) :-
	parse_constructor_specifier(X, X_Result),
	parse_cons_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

:- pred process_cons_spec_list(maybe(list(cons_specifier)),
				 maybe(sym_list)).
:- mode process_cons_spec_list(in, out).
process_cons_spec_list(ok(X), ok(cons(X))).
process_cons_spec_list(error(M, T), error(M, T)).


	% Parse a comma-separated list (misleading described as
	% a "conjunction") of type specifiers.

:- pred parse_type_spec_list(term, maybe(sym_list)).
:- mode parse_type_spec_list(in, out).
parse_type_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_type_spec_list_2(List, R),
	process_type_spec_list(R, Result).

:- pred parse_type_spec_list_2(list(term), maybe(list(sym_name_specifier))).
:- mode parse_type_spec_list_2(in, out).
parse_type_spec_list_2([], ok([])).
parse_type_spec_list_2(X.Xs, Result) :-
	parse_type_specifier(X, X_Result),
	parse_type_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

:- pred process_type_spec_list(maybe(list(sym_name_specifier)),
				 maybe(sym_list)).
:- mode process_type_spec_list(in, out).
process_type_spec_list(ok(X), ok(type(X))).
process_type_spec_list(error(M, T), error(M, T)).


	% Parse a comma-separated list (misleading described as
	% a "conjunction") of adt specifiers.

:- pred parse_adt_spec_list(term, maybe(sym_list)).
:- mode parse_adt_spec_list(in, out).
parse_adt_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_adt_spec_list_2(List, R),
	process_adt_spec_list(R, Result).

:- pred parse_adt_spec_list_2(list(term), maybe(list(sym_name_specifier))).
:- mode parse_adt_spec_list_2(in, out).
parse_adt_spec_list_2([], ok([])).
parse_adt_spec_list_2(X.Xs, Result) :-
	parse_adt_specifier(X, X_Result),
	parse_adt_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

:- pred process_adt_spec_list(maybe(list(sym_name_specifier)),
				 maybe(sym_list)).
:- mode process_adt_spec_list(in, out).
process_adt_spec_list(ok(X), ok(adt(X))).
process_adt_spec_list(error(M, T), error(M, T)).


	% Parse a comma-separated list (misleading described as
	% a "conjunction") of operator specifiers.

:- pred parse_op_spec_list(term, maybe(sym_list)).
:- mode parse_op_spec_list(in, out).
parse_op_spec_list(Term, Result) :-
	conjunction_to_list(Term, List),
	parse_op_spec_list_2(List, R),
	process_op_spec_list(R, Result).

:- pred parse_op_spec_list_2(list(term), maybe(list(op_specifier))).
:- mode parse_op_spec_list_2(in, out).
parse_op_spec_list_2([], ok([])).
parse_op_spec_list_2(X.Xs, Result) :-
	parse_op_specifier(X, X_Result),
	parse_op_spec_list_2(Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

:- pred process_op_spec_list(maybe(list(op_specifier)),
				 maybe(sym_list)).
:- mode process_op_spec_list(in, out).
process_op_spec_list(ok(X), ok(op(X))).
process_op_spec_list(error(M, T), error(M, T)).

%-----------------------------------------------------------------------------%

	% If a list of things contains multiple errors, then we only
	% report the first one.

:- pred combine_list_results(maybe(T), maybe(list(T)), maybe(list(T))).
:- mode combine_list_results(in, in, out).
combine_list_results(error(Msg, Term), _, error(Msg, Term)).
combine_list_results(ok(_), error(Msg, Term), error(Msg, Term)).
combine_list_results(ok(X), ok(Xs), ok([X|Xs])).

%-----------------------------------------------------------------------------%
%
%	A symbol specifier is one of
%
%		SymbolNameSpecifier
%			Matches any symbol matched by the SymbolNameSpecifier.
%		TypedConstructorSpecifier
%			Matches any constructors matched by the
%			TypedConstructorSpecifier.
%		cons(ConstructorSpecifier)
%			Matches only constructors.
%		pred(PredSpecifier)
%			Matches only predicates, ie. constructors of type
%			`pred'.
%		adt(SymbolNameSpecifier)
%			Matches only type names.
%		type(SymbolNameSpecifier)
%			Matches type names matched by the SymbolNameSpecifier,
%			and also matches any constructors for the matched type
%			names.
%		op(SymbolNameSpecifier)
%			Matches only operators.
%		module(ModuleSpecifier)
%			Matches all symbols in the specified module.

:- pred parse_symbol_specifier(term, maybe(sym_specifier)).
parse_symbol_specifier(Term, Result) :-
	(
	    Term = term__functor(term__atom("cons"), [ConsSpecTerm], _Context1)
	->
	    parse_constructor_specifier(ConsSpecTerm, ConsSpecResult),
	    process_cons_symbol_specifier(ConsSpecResult, Result)
	;
	    Term = term__functor(term__atom("pred"), [PredSpecTerm], _Context2)
	->
	    parse_predicate_specifier(PredSpecTerm, PredSpecResult),
	    process_pred_symbol_specifier(PredSpecResult, Result)
	;
	    Term = term__functor(term__atom("type"), [TypeSpecTerm], _Context3)
	->
	    parse_type_specifier(TypeSpecTerm, TypeSpecResult),
	    process_type_symbol_specifier(TypeSpecResult, Result)
	;
	    Term = term__functor(term__atom("adt"), [AdtSpecTerm], _Context4)
	->
	    parse_adt_specifier(AdtSpecTerm, AdtSpecResult),
	    process_adt_symbol_specifier(AdtSpecResult, Result)
	;
	    Term = term__functor(term__atom("op"), [OpSpecTerm], _Context5)
	->
	    parse_op_specifier(OpSpecTerm, OpSpecResult),
	    process_op_symbol_specifier(OpSpecResult, Result)
	;
	    Term = term__functor(term__atom("module"), [ModuleSpecTerm],
				_Context6)
	->
	    parse_module_specifier(ModuleSpecTerm, ModuleSpecResult),
	    process_module_symbol_specifier(ModuleSpecResult, Result)
	;
	    parse_constructor_specifier(Term, TermResult),
	    process_any_symbol_specifier(TermResult, Result)
	).

% 	Once we've parsed the appropriate type of symbol specifier, we
%	need to convert it to a sym_specifier, propagating errors upwards.

:- pred process_module_symbol_specifier(maybe(module_specifier),
					maybe(sym_specifier)).
:- mode process_module_symbol_specifier(in, out).
process_module_symbol_specifier(ok(OpSpec), ok(module(OpSpec))).
process_module_symbol_specifier(error(Msg, Term), error(Msg, Term)).

:- pred process_any_symbol_specifier(maybe(cons_specifier),
				     maybe(sym_specifier)).
:- mode process_any_symbol_specifier(in, out).
process_any_symbol_specifier(error(Msg, Term), error(Msg, Term)).
process_any_symbol_specifier(ok(sym(SymSpec)), ok(sym(SymSpec))).
process_any_symbol_specifier(ok(typed(ConsSpec)), ok(typed_sym(ConsSpec))).

:- pred process_pred_symbol_specifier(maybe(pred_specifier),
					maybe(sym_specifier)).
:- mode process_pred_symbol_specifier(in, out).
process_pred_symbol_specifier(error(Msg, Term), error(Msg, Term)).
process_pred_symbol_specifier(ok(PredSpec), ok(pred(PredSpec))).

:- pred process_cons_symbol_specifier(maybe(cons_specifier),
					maybe(sym_specifier)).
:- mode process_cons_symbol_specifier(in, out).
process_cons_symbol_specifier(error(Msg, Term), error(Msg, Term)).
process_cons_symbol_specifier(ok(ConsSpec), ok(cons(ConsSpec))).

:- pred process_type_symbol_specifier(maybe(sym_name_specifier),
					maybe(sym_specifier)).
:- mode process_type_symbol_specifier(in, out).
process_type_symbol_specifier(ok(SymSpec), ok(type(SymSpec))).
process_type_symbol_specifier(error(Msg, Term), error(Msg, Term)).

:- pred process_adt_symbol_specifier(maybe(sym_name_specifier),
					maybe(sym_specifier)).
:- mode process_adt_symbol_specifier(in, out).
process_adt_symbol_specifier(ok(SymSpec), ok(adt(SymSpec))).
process_adt_symbol_specifier(error(Msg, Term), error(Msg, Term)).

:- pred process_op_symbol_specifier(maybe(op_specifier),
					maybe(sym_specifier)).
:- mode process_op_symbol_specifier(in, out).
process_op_symbol_specifier(ok(OpSpec), ok(op(OpSpec))).
process_op_symbol_specifier(error(Msg, Term), error(Msg, Term)).

%-----------------------------------------------------------------------------%

%	A ModuleSpecifier is just an identifier.

:- pred parse_module_specifier(term, maybe(module_specifier)).
:- mode parse_module_specifier(in, out).
parse_module_specifier(Term, Result) :-
	(
		Term = term__functor(term__atom(ModuleName), [], _Context)
	->
		Result = ok(ModuleName)
	;
		Result = error("Module specifier should be an identifier", Term)
	).

%-----------------------------------------------------------------------------%

%	A ConstructorSpecifier is one of
%		SymbolNameSpecifier
%		TypedConstructorSpecifier
%
%	A TypedConstructorSpecifier is one of
%		SymbolNameSpecifier::Type
%			Matches only constructors with the specified result
%			type.
%		SymbolName(ArgType1, ..., ArgTypeN)
%			Matches only constructors with the specified argument
%			types.
%		SymbolName(ArgType1, ..., ArgTypeN)::Type
%			Matches only constructors with the specified argument
%			and result types.

:- pred parse_constructor_specifier(term, maybe(cons_specifier)).
:- mode parse_constructor_specifier(in, out).
parse_constructor_specifier(Term, Result) :-
    (
	Term = term__functor(term__atom("::"), [NameArgsTerm, TypeTerm], _Context)
    ->
	parse_arg_types_specifier(NameArgsTerm, NameArgsResult),
	parse_type(TypeTerm, TypeResult),
	process_typed_constructor_specifier(NameArgsResult, TypeResult, Result)
    ;
	parse_arg_types_specifier(Term, TermResult),
	process_untyped_constructor_specifier(TermResult, Result)
    ).

%-----------------------------------------------------------------------------%

%	A PredicateSpecifier is one of
%		SymbolName(ArgType1, ..., ArgTypeN)
%			Matches only predicates with the specified argument
%			types.
%		SymbolNameSpecifier

:- pred parse_predicate_specifier(term, maybe(pred_specifier)).
:- mode parse_predicate_specifier(in, out).
parse_predicate_specifier(Term, Result) :-
    (
	Term = term__functor(term__atom("/"), [_,_], _Context)
    ->
	parse_symbol_name_specifier(Term, NameResult),
        process_arity_predicate_specifier(NameResult, Result)
    ;
	parse_qualified_term(Term, "predicate specifier", TermResult),
	process_typed_predicate_specifier(TermResult, Result)
    ).

:- pred process_typed_predicate_specifier(maybe_functor, maybe(pred_specifier)).
:- mode process_typed_predicate_specifier(in, out).
process_typed_predicate_specifier(ok(Name, Args), ok(Result)) :-
    ( Args = [] ->
	Result = sym(name(Name))
    ;
	Result = name_args(Name, Args)
    ).
process_typed_predicate_specifier(error(Msg, Term), error(Msg, Term)).

:- pred process_arity_predicate_specifier(maybe(sym_name_specifier),
		maybe(pred_specifier)).
:- mode process_arity_predicate_specifier(in, out).
process_arity_predicate_specifier(ok(Result), ok(sym(Result))).
process_arity_predicate_specifier(error(Msg, Term), error(Msg, Term)).

%-----------------------------------------------------------------------------%

% 	Parsing the name & argument types of a constructor specifier is
% 	exactly the same as parsing a predicate specifier...

:- pred parse_arg_types_specifier(term, maybe(pred_specifier)).
:- mode parse_arg_types_specifier(in, out).
parse_arg_types_specifier(Term, Result) :-
    (
	Term = term__functor(term__atom("/"), [_,_], _Context)
    ->
	parse_symbol_name_specifier(Term, NameResult),
        process_arity_predicate_specifier(NameResult, Result)
    ;
	parse_qualified_term(Term, "constructor specifier", TermResult),
	process_typed_predicate_specifier(TermResult, Result)
    ).

% 	... but we have to convert the result back into the appropriate
% 	format.

:- pred process_typed_constructor_specifier(maybe(pred_specifier), maybe(type),
		maybe(cons_specifier)).
:- mode process_typed_constructor_specifier(in, in, out).
process_typed_constructor_specifier(error(Msg, Term), _, error(Msg, Term)).
process_typed_constructor_specifier(_, error(Msg, Term), error(Msg, Term)).
process_typed_constructor_specifier(ok(NameArgs), ok(ResType), ok(Result)) :-
	process_typed_cons_spec_2(NameArgs, ResType, Result).

:- pred process_typed_cons_spec_2(pred_specifier, type, cons_specifier).
:- mode process_typed_cons_spec_2(in, in, out).
process_typed_cons_spec_2(sym(Name), Res, typed(name_res(Name, Res))).
process_typed_cons_spec_2(name_args(Name, Args), Res,
			  typed(name_args_res(Name, Args, Res))).

:- pred process_untyped_constructor_specifier(maybe(pred_specifier),
		maybe(cons_specifier)).
:- mode process_untyped_constructor_specifier(in, out).
process_untyped_constructor_specifier(error(Msg, Term), error(Msg, Term)).
process_untyped_constructor_specifier(ok(NameArgs), ok(Result)) :-
	process_untyped_cons_spec_2(NameArgs, Result).

:- pred process_untyped_cons_spec_2(pred_specifier, cons_specifier).
:- mode process_untyped_cons_spec_2(in, out).
process_untyped_cons_spec_2(sym(Name), sym(Name)).
process_untyped_cons_spec_2(name_args(Name, Args),
			    typed(name_args(Name, Args))).

%-----------------------------------------------------------------------------%

%	A SymbolNameSpecifier is one of
%		SymbolName
%		SymbolName/Arity
%			Matches only symbols of the specified arity.
%	

:- pred parse_symbol_name_specifier(term, maybe(sym_name_specifier)).
:- mode parse_symbol_name_specifier(in, out).
parse_symbol_name_specifier(Term, Result) :-
    ( %%% some [NameTerm, ArityTerm, Context]
       	Term = term__functor(term__atom("/"), [NameTerm, ArityTerm], _Context)
    ->
        ( %%% some [Arity, Context2]
            ArityTerm = term__functor(term__integer(Arity), [], _Context2)
	->
            ( Arity >= 0 ->
		parse_symbol_name(NameTerm, NameResult),
		process_name_arity_specifier(NameResult, Arity, Result)
	    ;
		Result = error("Arity in symbol name specifier must be a non-negative integer", Term)
	    )
        ;
	    Result = error("Arity in symbol name specifier must be an integer", Term)
        )
    ;
	parse_symbol_name(Term, SymbolNameResult),
	process_name_specifier(SymbolNameResult, Result)
    ).

:- pred process_name_arity_specifier(maybe(sym_name), int,
		maybe(sym_name_specifier)).
:- mode process_name_arity_specifier(in, in, out).
process_name_arity_specifier(ok(Name), Arity, ok(name_arity(Name, Arity))).
process_name_arity_specifier(error(Error, Term), _, error(Error, Term)).

:- pred process_name_specifier(maybe(sym_name), maybe(sym_name_specifier)).
:- mode process_name_specifier(in, out).
process_name_specifier(ok(Name), ok(name(Name))).
process_name_specifier(error(Error, Term), error(Error, Term)).

%-----------------------------------------------------------------------------%

%	A QualifiedTerm is one of
%		Name(Args)
%		Module:Name(Args)
%	(or if Args is empty, one of
%		Name
%		Module:Name)

:- pred parse_qualified_term(term, string, maybe_functor).
:- mode parse_qualified_term(in, in, out).
parse_qualified_term(Term, Msg, Result) :-
    (
       	Term = term__functor(term__atom(":"), [ModuleTerm, NameArgsTerm],
		_Context)
    ->
        ( 
            NameArgsTerm = term__functor(term__atom(Name), Args, _Context2)
        ->
            ( 
                ModuleTerm = term__functor(term__atom(Module), [], _Context3)
	    ->
		Result = ok(qualified(Module, Name), Args)
	    ;
		Result = error("Module name identifier expected before ':' in qualified symbol name", Term)
            )
        ;
            Result = error("Identifier expected after ':' in qualified symbol name", Term)
	)
    ;
        ( 
            Term = term__functor(term__atom(Name2), Args2, _Context4)
        ->
            Result = ok(unqualified(Name2), Args2)
        ;
	    string__append("atom expected in ", Msg, ErrorMsg),
            Result = error(ErrorMsg, Term)
        )
    ).

%-----------------------------------------------------------------------------%

%	A SymbolName is one of
%		Name
%			Matches symbols with the specified name in the
%			current namespace.
%		Module:Name
%			Matches symbols with the specified name exported
%			by the specified module.

:- pred parse_symbol_name(term, maybe(sym_name)).
:- mode parse_symbol_name(in, out).
parse_symbol_name(Term, Result) :-
    ( 
       	Term = term__functor(term__atom(":"), [ModuleTerm, NameTerm], _Context)
    ->
        ( 
            NameTerm = term__functor(term__atom(Name), [], _Context1)
        ->
            (
                ModuleTerm = term__functor(term__atom(Module), [], _Context2)
	    ->
		Result = ok(qualified(Module, Name))
	    ;
		Result = error("Module name identifier expected before ':' in qualified symbol name", Term)
            )
        ;
            Result = error("Identifier expected after ':' in qualified symbol name", Term)
	)
    ;
        ( 
            Term = term__functor(term__atom(Name2), [], _Context3)
        ->
            Result = ok(unqualified(Name2))
        ;
            Result = error("Symbol name specifier expected", Term)
        )
    ).

%-----------------------------------------------------------------------------%

% convert a module definition to a program item,
% propagating errors upwards

:- pred process_import(maybe(sym_list), varset, maybe(item)).
:- mode process_import(in, in, out).
process_import(ok(X), VarSet, ok(module_defn(VarSet, import(X)))).
process_import(error(Msg, Term), _, error(Msg, Term)).

:- pred process_use(maybe(sym_list), varset, maybe(item)).
:- mode process_use(in, in, out).
process_use(ok(X), VarSet, ok(module_defn(VarSet, use(X)))).
process_use(error(Msg, Term), _, error(Msg, Term)).

:- pred process_export(maybe(sym_list), varset, maybe(item)).
:- mode process_export(in, in, out).
process_export(ok(X), VarSet, ok(module_defn(VarSet, export(X)))).
process_export(error(Msg, Term), _, error(Msg, Term)).

%-----------------------------------------------------------------------------%

%	A TypeSpecifier is just a symbol name specifier.

:- pred parse_type_specifier(term, maybe(sym_name_specifier)).
:- mode parse_type_specifier(in, out).
parse_type_specifier(Term, Result) :-
	parse_symbol_name_specifier(Term, Result).

%	An ADT_Specifier is just a symbol name specifier.

:- pred parse_adt_specifier(term, maybe(sym_name_specifier)).
:- mode parse_adt_specifier(in, out).
parse_adt_specifier(Term, Result) :-
	parse_symbol_name_specifier(Term, Result).

%-----------------------------------------------------------------------------%

%	For the moment, an OpSpecifier is just a symbol name specifier.
% 	XXX We should allow specifying the fixity of an operator

:- pred parse_op_specifier(term, maybe(op_specifier)).
:- mode parse_op_specifier(in, out).
parse_op_specifier(Term, Result) :-
	parse_symbol_name_specifier(Term, R),
	process_op_specifier(R, Result).

:- pred process_op_specifier(maybe(sym_name_specifier), maybe(op_specifier)).
:- mode process_op_specifier(in, out).
process_op_specifier(ok(X), ok(sym(X))).
process_op_specifier(error(M,T), error(M,T)).
	
%-----------------------------------------------------------------------------%

	% types are represented just as ordinary terms

:- pred parse_type(term, maybe(type)).
:- mode parse_type(in, out).
parse_type(T, ok(T)).

:- pred convert_type_list(list(term), list(type)).
:- mode convert_type_list(in, out).
convert_type_list([], []).
convert_type_list([H0|T0], [H|T]) :-
	convert_type(H0, H),
	convert_type_list(T0, T).

:- pred convert_type(term, type).
:- mode convert_type(in, out).
convert_type(T, T).

%-----------------------------------------------------------------------------%
