%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io.m.
% Main author: fjh.
%
% This module defines predicates for parsing Mercury programs.
%
% In some ways the representation of programs here is considerably
% more complex than is necessary for the compiler.
% The basic reason for this is that it was designed to preserve
% as much information about the source code as possible, so that
% this representation could also be used for other tools such
% as Mercury-to-Goedel converters, pretty-printers, etc.
% Currently the only information that is lost is that comments and
% whitespace are stripped, any redundant parenthesization
% are lost, distinctions between different spellings of the same
% operator (eg "\+" vs "not") are lost, and DCG clauses get expanded.
% It would be a good idea to preserve all those too (well, maybe not
% the redundant parentheses), but right now it's not worth the effort.
%
% So that means that this phase of compilation is purely parsing.
% No simplifications are done (other than DCG expansion). 
% The results of this phase specify
% basically the same information as is contained in the source code,
% but in a parse tree rather than a flat file.
% Simplifications are done only by make_hlds.m, which transforms
% the parse tree which we built here into the HLDS.
%
% Some of this code is a rather bad example of cut-and-paste style reuse.
% It should be cleaned up to eliminate most of the duplication.
% But that task really needs to wait until we implement higher-order
% predicates.  For the moment, just be careful that any changes
% you make are reflected correctly in all similar parts of this
% file.
%
% Implication and equivalence implemented by squirrel, who would also
% like to get her hands on this file and give it a good clean up and
% put it into good clean "mercury" style!

% Wishlist:
%
% 1.  implement importing/exporting operators with a particular fixity
%     eg. :- import_op prefix(+). % only prefix +, not infix
%     (not important, but should be there for reasons of symmetry.)
% 2.  improve the handling of type and inst parameters 
% 3.  improve the error reporting (most of the semidet preds should
%     be det and should return a meaningful indication of where an
%     error occured).

:- module prog_io.

:- interface.

:- import_module prog_data.
:- import_module string, list, varset, term, io.

%-----------------------------------------------------------------------------%

% This module (prog_io) exports the following predicates:

	% read_module(ModuleName, Error, Messages, Program)
	% reads and parses the module 'ModuleName'.  Error is `fatal'
	% if the file coudn't be opened, `yes'
	% if a syntax error was detected, and `no' otherwise.
	% Messages is a list of warning/error messages.
	% Program is the parse tree.

:- type module_error
	--->	no	% no errors
	;	yes	% some syntax errors
	;	fatal.	% couldn't open the file

:- pred prog_io__read_module(string, string, module_error,
	message_list, item_list, io__state, io__state).
:- mode prog_io__read_module(in, in, out, out, out, di, uo) is det.

	% Convert a single term into a goal.
	%
:- pred parse_goal(term, varset, goal, varset).
:- mode parse_goal(in, in, out, out) is det.

	% Convert a term, possibly starting with `some [Vars]', into
	% a list of variables and a goal. (If the term doesn't start
	% with `some [Vars]', we return an empty list of variables.)
	% 
:- pred parse_some_vars_goal(term, varset, vars, goal, varset).
:- mode parse_some_vars_goal(in, in, out, out, out) is det.

	% parse_lambda_expression/3 converts the first argument to a lambda/2
	% expression into a list of variables, a list of their corresponding
	% modes, and a determinism.
	% The syntax of a lambda expression is
	%	`lambda([Var1::Mode1, ..., VarN::ModeN] is Det, Goal)'
	% but this predicate just parses the first argument, i.e. the
	% 	`[Var1::Mode1, ..., VarN::ModeN] is Det'
	% part.
	%
:- pred parse_lambda_expression(term, list(term), list(mode), determinism).
:- mode parse_lambda_expression(in, out, out, out) is semidet.

	% parse_pred_expression/3 converts the first argument to a :-/2
	% higher-order pred expression into a list of variables, a list
	% of their corresponding modes, and a determinism.  This is just
	% a variant on parse_lambda_expression with a different syntax:
	% 	`(pred(Var1::Mode1, ..., VarN::ModeN) is Det :- Goal)'.
	%
:- pred parse_pred_expression(term, list(term), list(mode), determinism).
:- mode parse_pred_expression(in, out, out, out) is semidet.

	% parse_func_expression/3 converts the first argument to a :-/2
	% higher-order func expression into a list of variables, a list
	% of their corresponding modes, and a determinism.  The syntax
	% of a higher-order func expression is
	% 	`(func(Var1::Mode1, ..., VarN::ModeN) = (VarN1::ModeN1) is Det
	%		:- Goal)'.
	%
:- pred parse_func_expression(term, list(term), list(mode), determinism).
:- mode parse_func_expression(in, out, out, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The following /3, /4 and /5 predicates are to be used for reporting
% warnings to stderr.  This is preferable to using io__write_string, as
% this checks the halt-at-warn option
%
% This predicate is best used by predicates that do not have access to
% module_info for a particular module.  It sets the exit status to error
% when a warning is encountered in a module, and the --halt-at-warn
% option is set.

:- pred report_warning(string::in, io__state::di, io__state::uo) is det.

:- pred report_warning(io__output_stream::in, string::in, io__state::di,
                      io__state::uo) is det.

:- pred report_warning(string::in, int::in, string::in, io__state::di,
                      io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, hlds_pred, prog_util, globals, options.
:- import_module bool, int, std_util, term_io, dir, require.

%-----------------------------------------------------------------------------%

	% When actually reading in type declarations, we need to
	% check for errors.

:- type maybe1(T)	--->	error(string, term)
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
	globals__io_lookup_accumulating_option(search_directories, Dirs),
	search_for_file(Dirs, FileName, R),
	( { R = yes } ->
		read_all_items(ModuleName, RevMessages, RevItems0, Error0),
		{
		  get_end_module(RevItems0, RevItems, EndModule),
		  list__reverse(RevMessages, Messages0),
		  list__reverse(RevItems, Items0),
		  check_begin_module(ModuleName,
				Messages0, Items0, Error0, EndModule,
				FileName, Messages, Items, Error)
		},
		io__seen
	;
		io__progname_base("prog_io.m", Progname),
		{
		  string__append(Progname, ": can't open file `", Message1),
		  string__append(Message1, FileName, Message2),
		  string__append(Message2, "'", Message),
		  dummy_term(Term),
		  Messages = [Message - Term],
		  Error = fatal,
		  Items = []
		}
	).

:- pred search_for_file(list(string), string, bool, io__state, io__state).
:- mode search_for_file(in, in, out, di, uo) is det.

search_for_file([], _, no) --> [].
search_for_file([Dir | Dirs], FileName, R) -->
	{ dir__this_directory(Dir) ->
		ThisFileName = FileName
	;
		dir__directory_separator(Separator),
		string__first_char(Tmp1, Separator, FileName),
		string__append(Dir, Tmp1, ThisFileName)
	},
	io__see(ThisFileName, R0),
	( { R0 = ok } ->
		{ R = yes }
	;
		search_for_file(Dirs, FileName, R)
	).

%-----------------------------------------------------------------------------%

	% extract the final `:- end_module' declaration if any

:- type module_end ---> no ; yes(module_name, term__context).

:- pred get_end_module(item_list, item_list, module_end).
:- mode get_end_module(in, out, out) is det.

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

:- pred check_begin_module(string, message_list, item_list, module_error,
		module_end, string, message_list, item_list, module_error).
:- mode check_begin_module(in, in, in, in, in, in, out, out, out) is det.

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
            list__append([ThisError], Messages0, Messages),
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
:- mode dummy_term(out) is det.
dummy_term(Term) :-
	term__context_init(Context),
	dummy_term_with_context(Context, Term).

	% Create a dummy term with the specified context.
	% Used for error messages that are associated with some specific
	% context, but for which we don't want to print out the term
	% (or for which the term isn't available to be printed out).

:- pred dummy_term_with_context(term__context, term).
:- mode dummy_term_with_context(in, out) is det.
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


:- pred read_all_items(string, message_list, item_list, module_error,
			io__state, io__state).
:- mode read_all_items(in, out, out, out, di, uo) is det.

read_all_items(ModuleName, Messages, Items, Error) -->
	read_items_loop(ModuleName, [], [], no, Messages, Items, Error).

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

:- pred read_items_loop(string, message_list, item_list, module_error, 
			message_list, item_list, module_error, 
			io__state, io__state).
:- mode read_items_loop(in, in, in, in, out, out, out, di, uo) is det.

read_items_loop(ModuleName, Msgs1, Items1, Error1, Msgs, Items, Error) -->
	io__gc_call(read_item(ModuleName, MaybeItem)),
 	read_items_loop_2(ModuleName, MaybeItem, Msgs1, Items1, Error1, 
				Msgs, Items, Error).

%-----------------------------------------------------------------------------%

:- pred read_items_loop_2(string, maybe_item_or_eof, message_list, item_list,
			module_error, message_list, item_list, module_error,
			io__state, io__state).
:- mode read_items_loop_2(in, in, in, in, in, out, out, out, di, uo) is det.

:- pragma(inline, read_items_loop_2/10).

% do a switch on the type of the next item

read_items_loop_2(_ModuleName, eof, Msgs, Items, Error, Msgs, Items, Error) 
			--> []. 
	% if the next item was end-of-file, then we're done.

read_items_loop_2(ModuleName, syntax_error(ErrorMsg, LineNumber), Msgs0, 
			Items0, _Error0, Msgs, Items, Error) -->
	% if the next item was a syntax error, then insert it in
	% the list of messages and continue looping
	io__input_stream(Stream),
	io__input_stream_name(Stream, StreamName),
	{
	  term__context_init(StreamName, LineNumber, Context),
	  dummy_term_with_context(Context, Term),
	  ThisError = ErrorMsg - Term,
	  Msgs1 = [ThisError | Msgs0],
	  Items1 = Items0,
	  Error1 = yes
	},
	read_items_loop(ModuleName, Msgs1, Items1, Error1, Msgs, Items, Error).

read_items_loop_2(ModuleName, error(M, T), Msgs0, Items0, _Error0, Msgs, Items, 
			Error) -->
	% if the next item was a semantic error, then insert it in
	% the list of messages and continue looping
	{
	  add_error(M, T, Msgs0, Msgs1),
	  Items1 = Items0,
	  Error1 = yes
	},
 	read_items_loop(ModuleName, Msgs1, Items1, Error1, Msgs, Items, Error).

read_items_loop_2(ModuleName, ok(Item, Context), Msgs0, Items0, Error0,
			Msgs, Items, Error) -->
	% if the next item was a valid item, then insert it in
	% the list of items and continue looping
	{
	  Msgs1 = Msgs0,
	  Items1 = [Item - Context | Items0],
	  Error1 = Error0
	},
 	read_items_loop(ModuleName, Msgs1, Items1, Error1, Msgs, Items, Error).

%-----------------------------------------------------------------------------%

	% read_item/1 reads a single item, and if it is a valid term
	% parses it.

:- type maybe_item_or_eof --->	eof
			;	syntax_error(string, int)
			;	error(string, term)
			;	ok(item, term__context).

:- pred read_item(string, maybe_item_or_eof, io__state, io__state).
:- mode read_item(in, out, di, uo) is det.

read_item(ModuleName, MaybeItem) -->
	term_io__read_term(MaybeTerm),
	{ process_read_term(ModuleName, MaybeTerm, MaybeItem) }.

:- pred process_read_term(string, read_term, maybe_item_or_eof).
:- mode process_read_term(in, in, out) is det.

process_read_term(_ModuleName, eof, eof).
process_read_term(_ModuleName, error(ErrorMsg, LineNumber),
			syntax_error(ErrorMsg, LineNumber)).
process_read_term(ModuleName, term(VarSet, Term),
			MaybeItemOrEof) :-
	parse_item(ModuleName, VarSet, Term, MaybeItem),
	convert_item(MaybeItem, MaybeItemOrEof).

:- pred convert_item(maybe_item_and_context, maybe_item_or_eof).
:- mode convert_item(in, out) is det.

convert_item(ok(Item, Context), ok(Item, Context)).
convert_item(error(M, T), error(M, T)).

:- pred parse_item(string, varset, term, maybe_item_and_context). 
:- mode parse_item(in, in, in, out) is det.

parse_item(ModuleName, VarSet, Term, Result) :-
 	( %%% some [Decl, DeclContext]
		Term = term__functor(term__atom(":-"), [Decl], DeclContext)
	->
		% It's a declaration
		parse_decl(ModuleName, VarSet, Decl, R),
		add_context(R, DeclContext, Result)
	; %%% some [DCG_H, DCG_B, DCG_Context]
		% It's a DCG clause
		Term = term__functor(term__atom("-->"), [DCG_H, DCG_B],
			DCG_Context)
	->
		parse_dcg_clause(ModuleName, VarSet, DCG_H, DCG_B,
				DCG_Context, Result)
	;
		% It's either a fact or a rule
		( %%% some [H, B, TermContext]
			Term = term__functor(term__atom(":-"), [H, B],
						TermContext)
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
				term__context_init(TheContext)
			),
			Body = term__functor(term__atom("true"), [], TheContext)
		),
		parse_goal(Body, VarSet, Body2, VarSet2),
		(
			Head = term__functor(term__atom("="),
					[FuncHead, FuncResult], _)
		->
			parse_qualified_term(ModuleName, FuncHead,
				"equation head", R2),
			process_func_clause(R2, FuncResult, VarSet2, Body2, R3)
		;
			parse_qualified_term(ModuleName, Head, "clause head",
				R2),
			process_pred_clause(R2, VarSet2, Body2, R3)
		),
		add_context(R3, TheContext, Result)
	).

:- pred add_context(maybe1(item), term__context, maybe_item_and_context).
:- mode add_context(in, in, out) is det.

add_context(error(M, T), _, error(M, T)).
add_context(ok(Item), Context, ok(Item, Context)).

:- pred process_pred_clause(maybe_functor, varset, goal, maybe1(item)).
:- mode process_pred_clause(in, in, in, out) is det.
process_pred_clause(ok(Name, Args), VarSet, Body,
		ok(pred_clause(VarSet, Name, Args, Body))).
process_pred_clause(error(ErrMessage, Term), _, _, error(ErrMessage, Term)).

:- pred process_func_clause(maybe_functor, term, varset, goal, maybe1(item)).
:- mode process_func_clause(in, in, in, in, out) is det.
process_func_clause(ok(Name, Args), Result, VarSet, Body,
		ok(func_clause(VarSet, Name, Args, Result, Body))).
process_func_clause(error(ErrMessage, Term), _, _, _, error(ErrMessage, Term)).

%-----------------------------------------------------------------------------%

	% Parse a goal.
	% We just check if it matches the appropriate pattern
	% for one of the builtins.  If it doesn't match any of the
	% builtins, then it's just a predicate call.
	% XXX we should do more parsing here - type qualification
	% should be parsed here.
	%
	% We could do some error-checking here, but all errors are picked up
	% in either the type-checker or parser anyway.

parse_goal(Term, VarSet0, Goal, VarSet) :-
	(
		Term = term__functor(term__atom(Name), Args, Context),
		parse_goal_2(Name, Args, VarSet0, GoalExpr, VarSet1)
	->
		Goal = GoalExpr - Context,
		VarSet = VarSet1
	;
		(
			Term = term__functor(term__atom(Name), Terms, Context)
		->
			VarSet = VarSet0,
			( Name = ":" ->
				(
					Terms = [term__functor(term__atom(
							ModuleName), [], _), 
						term__functor(term__atom(
							PredName), Args, _)]
				->
					Goal = call(qualified(ModuleName, 
						PredName), Args) - Context
				;
				Term0 = term__functor(term__atom("call"), 
							[Term], Context),
				Goal = call(unqualified("call"), [Term0])
							- Context
				% Goal contains ill-formed qualified 
				% predicate calls..
				)
			;
				Goal = call(unqualified(Name), Terms) - Context
			)
		;
			(
				Term = term__functor(_, _, Context)
			;
				Term = term__variable(_),
				term__context_init(Context)
			),
			Term0 = term__functor(term__atom("call"), [Term],
							Context),
			Goal = call(unqualified("call"), [Term0]) - Context,
			VarSet = VarSet0
			% Term in Goal above is a term__constant or 
			% term__variable that is definately not a function call.
		)
	).

:- pred parse_goal_2(string, list(term), varset, goal_expr, varset).
:- mode parse_goal_2(in, in, in, out, out) is semidet.
parse_goal_2("true", [], V, true, V).
parse_goal_2("fail", [], V, fail, V).
parse_goal_2("=", [A, B], V, unify(A, B), V).
/******
	Since (A -> B) has different semantics in standard Prolog
	(A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
	for the moment we'll just disallow it.
parse_goal_2("->", [A0, B0], V0, if_then(Vars, A, B), V) :-
	parse_some_vars_goal(A0, V0, Vars, A, V1),
	parse_goal(B0, V1, B, V).
******/
parse_goal_2(",", [A0, B0], V0, (A, B), V) :-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).
parse_goal_2(";", [A0, B0], V0, R, V) :-
	(
		A0 = term__functor(term__atom("->"), [X0, Y0], _Context)
	->
		parse_some_vars_goal(X0, V0, Vars, X, V1),
		parse_goal(Y0, V1, Y, V2),
		parse_goal(B0, V2, B, V),
		R = if_then_else(Vars, X, Y, B)
	;
		parse_goal(A0, V0, A, V1),
		parse_goal(B0, V1, B, V),
		R = (A;B)
	).
/****
	For consistency we also disallow if-then
parse_goal_2("if",
		[term__functor(term__atom("then"), [A0, B0], _)], V0,
		if_then(Vars, A, B), V) :-
	parse_some_vars_goal(A0, V0, Vars, A, V1),
	parse_goal(B0, V1, B, V).
****/
parse_goal_2("else", [
		    term__functor(term__atom("if"), [
			term__functor(term__atom("then"), [A0, B0], _)
		    ], _),
		    C0
		], V0,
		if_then_else(Vars, A, B, C), V) :-
	parse_some_vars_goal(A0, V0, Vars, A, V1),
	parse_goal(B0, V1, B, V2),
	parse_goal(C0, V2, C, V).
parse_goal_2("not", [A0], V0, not(A), V) :-
	parse_goal(A0, V0, A, V).
parse_goal_2("\\+", [A0], V0, not(A), V) :-
	parse_goal(A0, V0, A, V).
parse_goal_2("all", [Vars0, A0], V0, all(Vars, A), V):-
	term__vars(Vars0, Vars),
	parse_goal(A0, V0, A, V).

	% handle implication
parse_goal_2("<=", [A0, B0], V0, implies(B, A), V):-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).

parse_goal_2("=>", [A0, B0], V0, implies(A, B), V):-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).

	% handle equivalence
parse_goal_2("<=>", [A0, B0], V0, equivalent(A, B), V):-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).

parse_goal_2("some", [Vars0, A0], V0, some(Vars, A), V):-
	term__vars(Vars0, Vars),
	parse_goal(A0, V0, A, V).

	% The following is a temporary hack to handle `is' in
	% the parser - we ought to handle it in the code generation -
	% but then `is/2' itself is a bit of a hack
	%
parse_goal_2("is", [A, B], V, unify(A, B), V).

parse_some_vars_goal(A0, VarSet0, Vars, A, VarSet) :-
	( 
		A0 = term__functor(term__atom("some"), [Vars0, A1], _Context)
	->
		term__vars(Vars0, Vars),
		parse_goal(A1, VarSet0, A, VarSet)
	;
		Vars = [],
		parse_goal(A0, VarSet0, A, VarSet)
	).

%-----------------------------------------------------------------------------%

parse_lambda_expression(LambdaExpressionTerm, Vars, Modes, Det) :-
	LambdaExpressionTerm = term__functor(term__atom("is"),
				[LambdaArgsTerm, DetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	parse_lambda_args(LambdaArgsTerm, Vars, Modes).

:- pred parse_lambda_args(term, list(term), list(mode)).
:- mode parse_lambda_args(in, out, out) is semidet.

parse_lambda_args(Term, Vars, Modes) :-
	( Term = term__functor(term__atom("."), [Head, Tail], _Context) ->
		parse_lambda_arg(Head, Var, Mode),
		Vars = [Var | Vars1],
		Modes = [Mode | Modes1],
		parse_lambda_args(Tail, Vars1, Modes1)
	; Term = term__functor(term__atom("[]"), [], _) ->
		Vars = [],
		Modes = []
	;
		Vars = [Var],
		Modes = [Mode],
		parse_lambda_arg(Term, Var, Mode)
	).

:- pred parse_lambda_arg(term, term, mode).
:- mode parse_lambda_arg(in, out, out) is semidet.

parse_lambda_arg(Term, VarTerm, Mode) :-
	Term = term__functor(term__atom("::"), [VarTerm, ModeTerm], _),
	convert_mode(ModeTerm, Mode).

%-----------------------------------------------------------------------------%

parse_pred_expression(PredTerm, Vars, Modes, Det) :-
	PredTerm = term__functor(term__atom("is"), [PredArgsTerm, DetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	PredArgsTerm = term__functor(term__atom("pred"), PredArgsList, _),
	parse_pred_expr_args(PredArgsList, Vars, Modes).

parse_func_expression(FuncTerm, Vars, Modes, Det) :-
	%
	% parse a func expression with specified modes and determinism
	%
	FuncTerm = term__functor(term__atom("is"), [EqTerm, DetTerm], _),
	EqTerm = term__functor(term__atom("="), [FuncArgsTerm, RetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	FuncArgsTerm = term__functor(term__atom("func"), FuncArgsList, _),
	parse_pred_expr_args(FuncArgsList, Vars0, Modes0),
	parse_lambda_arg(RetTerm, RetVar, RetMode),
	list__append(Vars0, [RetVar], Vars),
	list__append(Modes0, [RetMode], Modes).
parse_func_expression(FuncTerm, Vars, Modes, Det) :-
	%
	% parse a func expression with unspecified modes and determinism
	%
	FuncTerm = term__functor(term__atom("="), [FuncArgsTerm, RetVar], _),
	FuncArgsTerm = term__functor(term__atom("func"), Vars0, _),
	%
	% the argument modes default to `in',
	% the return mode defaults to `out',
	% and the determinism defaults to `det'.
	%
	InMode = user_defined_mode(unqualified("in"), []),
	OutMode = user_defined_mode(unqualified("out"), []),
	list__length(Vars0, NumVars),
	list__duplicate(NumVars, InMode, Modes0),
	RetMode = OutMode,
	Det = det,
	list__append(Modes0, [RetMode], Modes),
	list__append(Vars0, [RetVar], Vars).

:- pred parse_pred_expr_args(list(term), list(term), list(mode)).
:- mode parse_pred_expr_args(in, out, out) is semidet.

parse_pred_expr_args([], [], []).
parse_pred_expr_args([Term|Terms], [Arg|Args], [Mode|Modes]) :-
	parse_lambda_arg(Term, Arg, Mode),
	parse_pred_expr_args(Terms, Args, Modes).

%-----------------------------------------------------------------------------%

:- pred parse_dcg_clause(string, varset, term, term, term__context,
			maybe_item_and_context).
:- mode parse_dcg_clause(in, in, in, in, in, out) is det.

parse_dcg_clause(ModuleName, VarSet0, DCG_Head, DCG_Body, DCG_Context,
		Result) :-
	new_dcg_var(VarSet0, 0, VarSet1, N0, DCG_0_Var),
	parse_dcg_goal(DCG_Body, VarSet1, N0, DCG_0_Var,
			Body, VarSet, _N, DCG_Var),
	parse_qualified_term(ModuleName, DCG_Head, "DCG clause head",
			HeadResult),
	process_dcg_clause(HeadResult, VarSet, DCG_0_Var, DCG_Var, Body, R),
	add_context(R, DCG_Context, Result).

%-----------------------------------------------------------------------------%

	% Used to allocate fresh variables needed for the DCG expansion.

:- pred new_dcg_var(varset, int, varset, int, var).
:- mode new_dcg_var(in, in, out, out, out) is det.

new_dcg_var(VarSet0, N0, VarSet, N, DCG_0_Var) :-
	string__int_to_string(N0, StringN),
	string__append("DCG_", StringN, VarName),
	varset__new_var(VarSet0, DCG_0_Var, VarSet1),
	varset__name_var(VarSet1, DCG_0_Var, VarName, VarSet),
	N is N0 + 1.

%-----------------------------------------------------------------------------%

	% Expand a DCG goal.

:- pred parse_dcg_goal(term, varset, int, var, goal, varset, int, var).
:- mode parse_dcg_goal(in, in, in, in, out, out, out, out) is det.

parse_dcg_goal(Term0, VarSet0, N0, Var0, Goal, VarSet, N, Var) :-
	(
		Term0 = term__functor(term__atom(Functor), Args0, Context)
	->
		% First check for the special cases:
		(
			parse_dcg_goal_2(Functor, Args0, Context,
					VarSet0, N0, Var0,
					Goal1, VarSet1, N1, Var1)
		->
			Goal = Goal1,
			VarSet = VarSet1,
			N = N1,
			Var = Var1
		;
			% It's the ordinary case of non-terminal.
			% Create a fresh var as the DCG output var from this
			% goal, and append the DCG argument pair to the
			% non-terminal's argument list.
			new_dcg_var(VarSet0, N0, VarSet, N, Var),
			(
				Functor = ":" ,
				Args0 = [term__functor(term__atom(ModuleName),
						[], _), 
					term__functor(term__atom(PredName),
						Args_of_pred0, _)]
			->
				list__append(Args_of_pred0, [
						term__variable(Var0),
						term__variable(Var)
					], Args_of_pred),
				Goal = call(qualified(ModuleName, PredName), Args_of_pred) - Context
			;
				list__append(Args0, [
						term__variable(Var0),
						term__variable(Var)
					], Args),
				Goal = call(unqualified(Functor), Args) - Context
			)
		)
	;
		% A call to a free variable, or to a number or string.
		% Just translate it into a call to call/2 - the typecheck will
		% catch calls to numbers and strings.

		new_dcg_var(VarSet0, N0, VarSet, N, Var),
		term__context_init(CallContext),
		Term = term__functor(term__atom("call"), [
				Term0,
				term__variable(Var0),
				term__variable(Var)
			], CallContext),
		Goal = call(unqualified("call"), [Term]) - CallContext
	).

	% parse_dcg_goal_2(Functor, Args, Context, VarSet0, N0, Var0,
	%			Goal, VarSet, N, Var):
	% VarSet0/VarSet are an accumulator pair which we use to
	% allocate fresh DCG variables; N0 and N are an accumulator pair
	% we use to keep track of the number to give to the next DCG
	% variable (so that we can give it a semi-meaningful name "DCG_<N>"
	% for use in error messages, debugging, etc.).
	% Var0 and Var are an accumulator pair we use to keep track of
	% the current DCG variable.

:- pred parse_dcg_goal_2(string, list(term), term__context, varset, int, var,
				goal, varset, int, var).
:- mode parse_dcg_goal_2(in, in, in, in, in, in, out, out, out, out)
				is semidet.

	% The following is a temporary and gross hack to strip out
	% calls to `io__gc_call', since the mode checker can't handle
	% them yet.
parse_dcg_goal_2("io__gc_call", [Goal0],
		_, VarSet0, N0, Var0, Goal, VarSet, N, Var) :-
	parse_dcg_goal(Goal0, VarSet0, N0, Var0, Goal, VarSet, N, Var).

	% Ordinary goal inside { curly braces }.
parse_dcg_goal_2("{}", [G], _, VarSet0, N, Var,
		Goal, VarSet, N, Var) :-
	parse_goal(G, VarSet0, Goal, VarSet).

	% Empty list - just unify the input and output DCG args.
parse_dcg_goal_2("[]", [], Context, VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	new_dcg_var(VarSet0, N0, VarSet, N, Var),
	Goal = unify(term__variable(Var0), term__variable(Var)) - Context.

	% Non-empty list of terminals.  Append the DCG output arg
	% as the new tail of the list, and unify the result with
	% the DCG input arg.
parse_dcg_goal_2(".", [X, Xs], Context, VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	new_dcg_var(VarSet0, N0, VarSet, N, Var),
	term_list_append_term(term__functor(term__atom("."), [X, Xs], Context),
			term__variable(Var), Term), 
	Goal = unify(term__variable(Var0), Term) - Context.

	% Call to '='/1 - unify argument with DCG input arg.
parse_dcg_goal_2("=", [A], Context, VarSet, N, Var,
		Goal, VarSet, N, Var) :-
	Goal = unify(A, term__variable(Var)) - Context.

	% If-then (Prolog syntax).
	% We need to add an else part to unify the DCG args.

/******
	Since (A -> B) has different semantics in standard Prolog
	(A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
	for the moment we'll just disallow it.
parse_dcg_goal_2("->", [Cond0, Then0], Context, VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	parse_dcg_if_then(Cond0, Then0, Context, VarSet0, N0, Var0,
		SomeVars, Cond, Then, VarSet, N, Var),
	( Var = Var0 ->
		Goal = if_then(SomeVars, Cond, Then) - Context
	;
		Unify = unify(term__variable(Var), term__variable(Var0)),
		Goal = if_then_else(SomeVars, Cond, Then, Unify - Context)
			- Context
	).
******/

	% If-then (NU-Prolog syntax).
parse_dcg_goal_2("if", [
			term__functor(term__atom("then"), [Cond0, Then0], _)
		], Context, VarSet0, N0, Var0, Goal, VarSet, N, Var) :-
	parse_dcg_if_then(Cond0, Then0, Context, VarSet0, N0, Var0,
		SomeVars, Cond, Then, VarSet, N, Var),
	( Var = Var0 ->
		Goal = if_then(SomeVars, Cond, Then) - Context
	;
		Unify = unify(term__variable(Var), term__variable(Var0)),
		Goal = if_then_else(SomeVars, Cond, Then, Unify - Context)
			- Context
	).

	% Conjunction.
parse_dcg_goal_2(",", [A0, B0], Context, VarSet0, N0, Var0,
		(A, B) - Context, VarSet, N, Var) :-
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet1, N1, Var1),
	parse_dcg_goal(B0, VarSet1, N1, Var1, B, VarSet, N, Var).

	% Disjunction or if-then-else (Prolog syntax).
parse_dcg_goal_2(";", [A0, B0], Context, VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	(
		A0 = term__functor(term__atom("->"), [Cond0, Then0], _Context)
	->
		parse_dcg_if_then_else(Cond0, Then0, B0, Context,
			VarSet0, N0, Var0, Goal, VarSet, N, Var)
	;
		parse_dcg_goal(A0, VarSet0, N0, Var0, A1, VarSet1, N1, VarA),
		parse_dcg_goal(B0, VarSet1, N1, Var0, B1, VarSet, N, VarB),
		( VarA = Var0, VarB = Var0 ->
			Var = Var0,
			Goal = (A1 ; B1) - Context
		; VarA = Var0 ->
			Var = VarB,
			Unify = unify(term__variable(Var),
				term__variable(VarA)),
			append_to_disjunct(A1, Unify, Context, A2),
			Goal = (A2 ; B1) - Context
		; VarB = Var0 ->
			Var = VarA,
			Unify = unify(term__variable(Var),
				term__variable(VarB)),
			append_to_disjunct(B1, Unify, Context, B2),
			Goal = (A1 ; B2) - Context
		;
			Var = VarB,
			prog_util__rename_in_goal(A1, VarA, VarB, A2),
			Goal = (A2 ; B1) - Context
		)
	).

	% If-then-else (NU-Prolog syntax).
parse_dcg_goal_2( "else", [
		    term__functor(term__atom("if"), [
			term__functor(term__atom("then"), [Cond0, Then0], _)
		    ], Context),
		    Else0
		], _, VarSet0, N0, Var0, Goal, VarSet, N, Var) :-
	parse_dcg_if_then_else(Cond0, Then0, Else0, Context,
		VarSet0, N0, Var0, Goal, VarSet, N, Var).

	% Negation (NU-Prolog syntax).
parse_dcg_goal_2( "not", [A0], Context, VarSet0, N0, Var0,
		not(A) - Context, VarSet, N, Var ) :-
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, _),
	Var = Var0.

	% Negation (Prolog syntax).
parse_dcg_goal_2( "\\+", [A0], Context, VarSet0, N0, Var0,
		not(A) - Context, VarSet, N, Var ) :-
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, _),
	Var = Var0.

	% Universal quantification.
parse_dcg_goal_2("all", [Vars0, A0], Context,
		VarSet0, N0, Var0, all(Vars, A) - Context, VarSet, N, Var) :-
	term__vars(Vars0, Vars),
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, Var).

	% Existential quantification.
parse_dcg_goal_2("some", [Vars0, A0], Context,
		VarSet0, N0, Var0, some(Vars, A) - Context, VarSet, N, Var) :-
	term__vars(Vars0, Vars),
	parse_dcg_goal(A0, VarSet0, N0, Var0, A, VarSet, N, Var).

:- pred append_to_disjunct(goal, goal_expr, term__context, goal).
:- mode append_to_disjunct(in, in, in, out) is det.

append_to_disjunct(Disjunct0, Goal, Context, Disjunct) :-
	( Disjunct0 = (A0 ; B0) - Context2 ->
		append_to_disjunct(A0, Goal, Context, A),
		append_to_disjunct(B0, Goal, Context, B),
		Disjunct = (A ; B) - Context2
	;
		Disjunct = (Disjunct0, Goal - Context) - Context
	).

:- pred parse_some_vars_dcg_goal(term, vars, varset, int, var,
				goal, varset, int, var).
:- mode parse_some_vars_dcg_goal(in, out, in, in, in, out, out, out, out)
	is det.
parse_some_vars_dcg_goal(A0, SomeVars, VarSet0, N0, Var0, A, VarSet, N, Var) :-
	( A0 = term__functor(term__atom("some"), [SomeVars0, A1], _Context) ->
		term__vars(SomeVars0, SomeVars),
		A2 = A1
	;
		SomeVars = [],
		A2 = A0
	),
	parse_dcg_goal(A2, VarSet0, N0, Var0, A, VarSet, N, Var).

	% Parse the "if" and the "then" part of an if-then or an
	% if-then-else.
	% If the condition is a DCG goal, but then "then" part
	% is not, then we need to translate
	%	( a -> { b } ; c )
	% as
	%	( a(DCG_1, DCG_2) ->
	%		b,
	%		DCG_3 = DCG_2
	%	;
	%		c(DCG_1, DCG_3)
	%	)
	% rather than
	%	( a(DCG_1, DCG_2) ->
	%		b
	%	;
	%		c(DCG_1, DCG_2)
	%	)
	% so that the implicit quantification of DCG_2 is correct.

:- pred parse_dcg_if_then(term, term, term__context, varset, int, var,
		list(var), goal, goal, varset, int, var).
:- mode parse_dcg_if_then(in, in, in, in, in, in, out, out, out, out, out, out)
	is det.

parse_dcg_if_then(Cond0, Then0, Context, VarSet0, N0, Var0,
		SomeVars, Cond, Then, VarSet, N, Var) :-
	parse_some_vars_dcg_goal(Cond0, SomeVars, VarSet0, N0, Var0,
				Cond, VarSet1, N1, Var1),
	parse_dcg_goal(Then0, VarSet1, N1, Var1, Then1, VarSet2, N2, Var2),
	( Var0 \= Var1, Var1 = Var2 ->
		new_dcg_var(VarSet2, N2, VarSet, N, Var),
		Unify = unify(term__variable(Var), term__variable(Var2)),
		Then = (Then1, Unify - Context) - Context
	;
		Then = Then1,
		N = N2,
		Var = Var2,
		VarSet = VarSet2
	).

:- pred parse_dcg_if_then_else(term, term, term, term__context,
	varset, int, var, goal, varset, int, var).
:- mode parse_dcg_if_then_else(in, in, in, in, in, in, in,
	out, out, out, out) is det.

parse_dcg_if_then_else(Cond0, Then0, Else0, Context, VarSet0, N0, Var0,
		Goal, VarSet, N, Var) :-
	parse_dcg_if_then(Cond0, Then0, Context, VarSet0, N0, Var0,
		SomeVars, Cond, Then1, VarSet1, N1, VarThen),
	parse_dcg_goal(Else0, VarSet1, N1, Var0, Else1, VarSet, N, VarElse),
	( VarThen = Var0, VarElse = Var0 ->
		Var = Var0,
		Then = Then1,
		Else = Else1
	; VarThen = Var0 ->
		Var = VarElse,
		Unify = unify(term__variable(Var), term__variable(VarThen)),
		Then = (Then1, Unify - Context) - Context,
		Else = Else1
	; VarElse = Var0 ->
		Var = VarThen,
		Then = Then1,
		Unify = unify(term__variable(Var), term__variable(VarElse)),
		Else = (Else1, Unify - Context) - Context
	;
		% We prefer to substitute the then part since it is likely
		% to be smaller than the else part, since the else part may
		% have a deeply nested chain of if-then-elses.

		% parse_dcg_if_then guarantees that if VarThen \= Var0,
		% then the then part introduces a new DCG variable (i.e.
		% VarThen does not appear in the condition). We therefore
		% don't need to do the substitution in the condition.

		Var = VarElse,
		prog_util__rename_in_goal(Then1, VarThen, VarElse, Then),
		Else = Else1
	),
	Goal = if_then_else(SomeVars, Cond, Then, Else) - Context.

	% term_list_append_term(ListTerm, Term, Result):
	% 	if ListTerm is a term representing a proper list, 
	%	this predicate will append the term Term
	%	onto the end of the list

:- pred term_list_append_term(term, term, term).
:- mode term_list_append_term(in, in, out) is semidet.

term_list_append_term(List0, Term, List) :-
	( List0 = term__functor(term__atom("[]"), [], _Context) ->
		List = Term
	;
		List0 = term__functor(term__atom("."), [Head, Tail0], Context2),
		List = term__functor(term__atom("."), [Head, Tail], Context2),
		term_list_append_term(Tail0, Term, Tail)
	).

:- pred process_dcg_clause(maybe_functor, varset, var, var, goal, maybe1(item)).
:- mode process_dcg_clause(in, in, in, in, in, out) is det.
process_dcg_clause(ok(Name, Args0), VarSet, Var0, Var, Body,
		ok(pred_clause(VarSet, Name, Args, Body))) :-
	list__append(Args0, [term__variable(Var0), term__variable(Var)], Args).
process_dcg_clause(error(ErrMessage, Term), _, _, _, _,
		error(ErrMessage, Term)).

%-----------------------------------------------------------------------------%
	% parse a declaration

:- pred parse_decl(string, varset, term, maybe1(item)).
:- mode parse_decl(in, in, in, out) is det.
parse_decl(ModuleName, VarSet, F, Result) :-
	( 
		F = term__functor(term__atom(Atom), As, _Context)
	->
		(
			process_decl(ModuleName, VarSet, Atom, As, R)
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
:- pred process_decl(string, varset, string, list(term), maybe1(item)).
:- mode process_decl(in, in, in, in, out) is semidet.

process_decl(_ModuleName, VarSet, "type", [TypeDecl], Result) :-
	parse_type_decl(VarSet, TypeDecl, Result).

process_decl(ModuleName, VarSet, "pred", [PredDecl], Result) :-
	parse_type_decl_pred(ModuleName, VarSet, PredDecl, Result).

process_decl(ModuleName, VarSet, "func", [FuncDecl], Result) :-
	parse_type_decl_func(ModuleName, VarSet, FuncDecl, Result).

process_decl(ModuleName, VarSet, "mode", [ModeDecl], Result) :-
	parse_mode_decl(ModuleName, VarSet, ModeDecl, Result).

process_decl(_ModuleName, VarSet, "inst", [InstDecl], Result) :-
	parse_inst_decl(VarSet, InstDecl, Result).

process_decl(_ModuleName, VarSet, "import_module", [ModuleSpec], Result) :-
	parse_symlist_decl(parse_module_specifier, make_module, make_import,
		ModuleSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_module", [ModuleSpec], Result) :-
	parse_symlist_decl(parse_module_specifier, make_module, make_use,
		ModuleSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_module", [ModuleSpec], Result) :-
	parse_symlist_decl(parse_module_specifier, make_module, make_export,
		ModuleSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "import_sym", [SymSpec], Result) :-
	parse_symlist_decl(parse_symbol_specifier, make_sym, make_import,
		SymSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_sym", [SymSpec], Result) :-
	parse_symlist_decl(parse_symbol_specifier, make_sym, make_use,
		SymSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_sym", [SymSpec], Result) :-
	parse_symlist_decl(parse_symbol_specifier, make_sym, make_export,
		SymSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "import_pred", [PredSpec], Result) :-
	parse_symlist_decl(parse_predicate_specifier, make_pred, make_import,
		PredSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_pred", [PredSpec], Result) :-
	parse_symlist_decl(parse_predicate_specifier, make_pred, make_use,
		PredSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_pred", [PredSpec], Result) :-
	parse_symlist_decl(parse_predicate_specifier, make_pred, make_export,
		PredSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "import_func", [FuncSpec], Result) :-
	parse_symlist_decl(parse_function_specifier, make_func, make_import,
		FuncSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_func", [FuncSpec], Result) :-
	parse_symlist_decl(parse_function_specifier, make_func, make_use,
		FuncSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_func", [FuncSpec], Result) :-
	parse_symlist_decl(parse_function_specifier, make_func, make_export,
		FuncSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "import_cons", [ConsSpec], Result) :-
	parse_symlist_decl(parse_constructor_specifier, make_cons, make_import,
		ConsSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_cons", [ConsSpec], Result) :-
	parse_symlist_decl(parse_constructor_specifier, make_cons, make_use,
		ConsSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_cons", [ConsSpec], Result) :-
	parse_symlist_decl(parse_constructor_specifier, make_cons, make_export,
		ConsSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "import_type", [TypeSpec], Result) :-
	parse_symlist_decl(parse_type_specifier, make_type, make_import,
		TypeSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_type", [TypeSpec], Result) :-
	parse_symlist_decl(parse_type_specifier, make_type, make_use,
		TypeSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_type", [TypeSpec], Result) :-
	parse_symlist_decl(parse_type_specifier, make_type, make_export,
		TypeSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "import_adt", [ADT_Spec], Result) :-
	parse_symlist_decl(parse_adt_specifier, make_adt, make_import,
		ADT_Spec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_adt", [ADT_Spec], Result) :-
	parse_symlist_decl(parse_adt_specifier, make_adt, make_use,
		ADT_Spec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_adt", [ADT_Spec], Result) :-
	parse_symlist_decl(parse_adt_specifier, make_adt, make_export,
		ADT_Spec, VarSet, Result).

process_decl(_ModuleName, VarSet, "import_op", [OpSpec], Result) :-
	parse_symlist_decl(parse_op_specifier, make_op, make_import,
		OpSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "use_op", [OpSpec], Result) :-
	parse_symlist_decl(parse_op_specifier, make_op, make_use,
		OpSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "export_op", [OpSpec], Result) :-
	parse_symlist_decl(parse_op_specifier, make_op, make_export,
		OpSpec, VarSet, Result).

process_decl(_ModuleName, VarSet, "interface", [], 
				ok(module_defn(VarSet, interface))).
process_decl(_ModuleName, VarSet, "implementation", [],
				ok(module_defn(VarSet, implementation))).
process_decl(_ModuleName, VarSet, "external", [PredSpec], Result) :-
	parse_symbol_name_specifier(PredSpec, Result0),
	process_maybe1(make_external(VarSet), Result0, Result).

process_decl(_ModuleName0, VarSet, "module", [ModuleName], Result) :-
	(
		ModuleName = term__functor(term__atom(Module), [], _Context)
	->
		Result = ok(module_defn(VarSet, module(Module)))
	;
		ModuleName = term__variable(_)
	->
		dummy_term(ErrorContext),
		Result = error("Module names starting with capital letters must be quoted using single quotes (e.g. "":- module 'Foo'."")", ErrorContext)
	;
		Result = error("Module name expected", ModuleName)
	).

process_decl(_ModuleName0, VarSet, "end_module", [ModuleName], Result) :-
	(
		ModuleName = term__functor(term__atom(Module), [], _Context)
	->
		Result = ok(module_defn(VarSet, end_module(Module)))
	;
		Result = error("Module name expected", ModuleName)
	).

	% NU-Prolog `when' declarations are silently ignored for
	% backwards compatibility.
process_decl(_ModuleName, _VarSet, "when", [_Goal, _Cond], Result) :-
	Result = ok(nothing).

process_decl(_ModuleName, VarSet, "pragma", Pragma, Result):-
	parse_pragma(VarSet, Pragma, Result).

:- pred parse_type_decl(varset, term, maybe1(item)).
:- mode parse_type_decl(in, in, out) is det.
parse_type_decl(VarSet, TypeDecl, Result) :-
	( 
		TypeDecl = term__functor(term__atom(Name), Args, _),
		parse_type_decl_type(Name, Args, Cond, R) 
	->
		R1 = R,
		Cond1 = Cond
	;
		process_abstract_type(TypeDecl, R1),
		Cond1 = true
	),
	process_maybe1(make_type_defn(VarSet, Cond1), R1, Result).
		% we should check the condition for errs
		% (don't bother at the moment, since we ignore
		% conditions anyhow :-)

:- pred make_type_defn(varset, condition, type_defn, item).
:- mode make_type_defn(in, in, in, out) is det.
make_type_defn(VarSet, Cond, TypeDefn, type_defn(VarSet, TypeDefn, Cond)).

:- pred make_external(varset, sym_name_specifier, item).
:- mode make_external(in, in, out) is det.
make_external(VarSet, SymSpec, module_defn(VarSet, external(SymSpec))).

%-----------------------------------------------------------------------------%

	% add a warning message to the list of messages

:- pred add_warning(string, term, message_list, message_list).
:- mode add_warning(in, in, out, in) is det.
add_warning(Warning, Term, [Msg - Term | Msgs], Msgs) :-
	string__append("Warning: ", Warning, Msg).

	% add an error message to the list of messages

:- pred add_error(string, term, message_list, message_list).
:- mode add_error(in, in, in, out) is det.
add_error(Error, Term, Msgs, [Msg - Term | Msgs]) :-
	string__append("Error: ", Error, Msg).

%-----------------------------------------------------------------------------%
	% parse_type_decl_type(Term, Condition, Result) succeeds
	% if Term is a "type" type declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.

:- pred parse_type_decl_type(string, list(term), condition, maybe1(type_defn)).
:- mode parse_type_decl_type(in, in, out, out) is semidet.

:- parse_type_decl_type([A|B], _, _, _) when A and B.

parse_type_decl_type("--->", [H, B], Condition, R) :-
	get_condition(B, Body, Condition),
	process_du_type(H, Body, R).

parse_type_decl_type("=", [H, B], Condition, R) :-
	get_condition(B, Body, Condition),
	process_uu_type(H, Body, R).

parse_type_decl_type("==", [H, B], Condition, R) :-
	get_condition(B, Body, Condition),
	process_eqv_type(H, Body, R).

%-----------------------------------------------------------------------------%

	% parse_type_decl_pred(Pred, Condition, Result) succeeds
	% if Pred is a predicate type declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.
:- pred parse_type_decl_pred(string, varset, term, maybe1(item)).
:- mode parse_type_decl_pred(in, in, in, out) is det.

parse_type_decl_pred(ModuleName, VarSet, Pred, R) :-
	get_condition(Pred, Body, Condition),
	get_determinism(Body, Body2, MaybeDeterminism),
        process_type_decl_pred(ModuleName, MaybeDeterminism, VarSet, Body2,
                                Condition, R).

:- pred process_type_decl_pred(string, maybe1(maybe(determinism)), varset,
				term, condition, maybe1(item)).
:- mode process_type_decl_pred(in, in, in, in, in, out) is det.

process_type_decl_pred(_MNm, error(Term, Reason), _, _, _,
			error(Term, Reason)).
process_type_decl_pred(ModuleName, ok(MaybeDeterminism), VarSet, Body,
			Condition, R) :-
        process_pred(ModuleName, VarSet, Body, Condition, MaybeDeterminism, R).


%-----------------------------------------------------------------------------%

	% parse_type_decl_func(Func, Condition, Result) succeeds
	% if Func is a function type declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.
:- pred parse_type_decl_func(string, varset, term, maybe1(item)).
:- mode parse_type_decl_func(in, in, in, out) is det.

parse_type_decl_func(ModuleName, VarSet, Func, R) :-
	get_condition(Func, Body, Condition),
	get_determinism(Body, Body2, MaybeDeterminism),
        process_maybe1_to_t(process_func(ModuleName, VarSet, Body2, Condition),
				MaybeDeterminism, R).

%-----------------------------------------------------------------------------%

	% parse_mode_decl_pred(ModuleName, Pred, Condition, Result) succeeds
	% if Pred is a predicate mode declaration, and binds Condition
	% to the condition for that declaration (if any), and Result to
	% a representation of the declaration.
:- pred parse_mode_decl_pred(string, varset, term, maybe1(item)).
:- mode parse_mode_decl_pred(in, in, in, out) is det.

parse_mode_decl_pred(ModuleName, VarSet, Pred, Result) :-
	get_condition(Pred, Body, Condition),
	get_determinism(Body, Body2, MaybeDeterminism),
	process_maybe1_to_t(process_mode(ModuleName, VarSet, Body2, Condition),
			MaybeDeterminism, Result).

%-----------------------------------------------------------------------------%
	% parse the pragma declaration. 
:- pred parse_pragma(varset, list(term), maybe1(item)).
:- mode parse_pragma(in, in, out) is semidet.

parse_pragma(VarSet, PragmaTerms, Result) :-
	(
		% new syntax: `:- pragma foo(...).'
		PragmaTerms = [SinglePragmaTerm],
		SinglePragmaTerm = term__functor(term__atom(PragmaType), 
					PragmaArgs, _),
		parse_pragma_type(PragmaType, PragmaArgs, SinglePragmaTerm,
			VarSet, Result0)
	->
		Result = Result0
	;
		% old syntax: `:- pragma(foo, ...).'
		PragmaTerms = [PragmaTypeTerm | PragmaArgs2],
		PragmaTypeTerm = term__functor(term__atom(PragmaType), [], _),
		parse_pragma_type(PragmaType, PragmaArgs2, PragmaTypeTerm,
			VarSet, Result1)
	->
		Result = Result1
	;
		fail
	).

:- pred parse_pragma_type(string, list(term), term, varset, maybe1(item)).
:- mode parse_pragma_type(in, in, in, in, out) is semidet.

parse_pragma_type("c_header_code", PragmaTerms, ErrorTerm, _VarSet, Result) :-
    	(
       	    PragmaTerms = [HeaderTerm]
        ->
	    (
    	        HeaderTerm = term__functor(term__string(HeaderCode), [], _)
	    ->
	        Result = ok(pragma(c_header_code(HeaderCode)))
	    ;
		Result = error("Expected string for C header code", HeaderTerm)
	    )
	;
	    Result = error(
"Wrong number of arguments in `pragma c_header_code(...) declaration.", 
			    ErrorTerm)
        ).

parse_pragma_type("c_code", PragmaTerms, ErrorTerm, VarSet, Result) :-
	(
    	    PragmaTerms = [Just_C_Code_Term]
	->
	    (
		Just_C_Code_Term = term__functor(term__string(Just_C_Code), [],
			_)
	    ->
	        Result = ok(pragma(c_code(Just_C_Code)))
	    ;
		Result = error("Expected string for C code", Just_C_Code_Term)
	    )
	;
    	    PragmaTerms = [PredAndVarsTerm, C_CodeTerm]
	->
	    % XXX temporary for bootstrapping
	    Recursiveness = non_recursive,
	    /***
	    % XXX temporarily disabled for bootstrapping
	    % By default we assume that pragma c_codes may be recursive.
	    Recursiveness = recursive,
	    ***/
	    parse_pragma_c_code(Recursiveness, PredAndVarsTerm, C_CodeTerm,
			VarSet, Result)
	;
    	    PragmaTerms = [RecursivenessTerm, PredAndVarsTerm, C_CodeTerm]
	->
	    ( parse_c_code_recursiveness(RecursivenessTerm, Recursiveness) ->
	        parse_pragma_c_code(Recursiveness, PredAndVarsTerm, C_CodeTerm,
			VarSet, Result)
	    ;
		Result = error("invalid first argument in `:- pragma c_code(..., ..., ...)' declaration -- expecting either `recursive' or `non_recursive'",
			RecursivenessTerm)
	    )
	;
	    Result = error(
	    "wrong number of arguments in `:- pragma c_code' declaration.", 
		    ErrorTerm)
	).

parse_pragma_type("export", PragmaTerms, ErrorTerm, _VarSet, Result) :-
       (
	    PragmaTerms = [PredAndModesTerm, C_FunctionTerm]
       ->
	    (
		(
                PredAndModesTerm = term__functor(term__atom(PredName), 
	    		ModeTerms, _),
	        C_FunctionTerm = term__functor(term__string(C_Function), [], _)
		)
	    ->
		(
		    convert_mode_list(ModeTerms, Modes)
		->
		    Result = 
			ok(pragma(export(unqualified(PredName), Modes, 
				C_Function)))
		;
	    	    Result = error(
		    "Expected pragma(export, PredName(ModeList), C_Function).",
			PredAndModesTerm)
		)
	    ;
	    	Result = error(
		     "Expected pragma(export, PredName(ModeList), C_Function).",
		     PredAndModesTerm)
	    )
	;
	    Result = 
	    	error(
		"wrong number of arguments in pragma(export, ...) declaration.",
		ErrorTerm)
       ).

parse_pragma_type("inline", PragmaTerms, ErrorTerm, _VarSet, Result) :-
	parse_simple_pragma("inline",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = inline(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type("memo", PragmaTerms, ErrorTerm, _VarSet, Result) :-
	parse_simple_pragma("memo",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = memo(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type("obsolete", PragmaTerms, ErrorTerm, _VarSet, Result) :-
	parse_simple_pragma("obsolete",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = obsolete(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

:- pred parse_simple_pragma(string, pred(sym_name, int, pragma_type),
			list(term), term, maybe1(item)).
:- mode parse_simple_pragma(in, pred(in, in, out) is det, in, in, out) is det.

parse_simple_pragma(PragmaType, MakePragma, PragmaTerms, ErrorTerm, Result) :-
       (
            PragmaTerms = [PredAndArityTerm]
       ->
	    (
                PredAndArityTerm = term__functor(term__atom("/"), 
	    		[PredNameTerm, ArityTerm], _)
	    ->
		(
		    PredNameTerm = term__functor(term__atom(PredName), [], _),
		    ArityTerm = term__functor(term__integer(Arity), [], _)
		->
		    call(MakePragma, unqualified(PredName), Arity, Pragma),
		    Result = ok(pragma(Pragma))
		;
		    string__append_list(
			["Expected predname/arity for `pragma ",
			 PragmaType, "(...)' declaration"], ErrorMsg),
	    	    Result = error(ErrorMsg, PredAndArityTerm)
		)
	    ;
	        string__append_list(["Expected predname/arity for `pragma ",
			 PragmaType, "(...)' declaration"], ErrorMsg),
	        Result = error(ErrorMsg, PredAndArityTerm)
	    )
	;
	    string__append_list(["Wrong number of arguments in `pragma ",
		 PragmaType, "(...)' declaration"], ErrorMsg),
	    Result = error(ErrorMsg, ErrorTerm)
       ).

%-----------------------------------------------------------------------------%

% parse a term which is either the atom `recursive' or the atom `non_recursive'.
% if the term doesn't match either, then fail.

:- pred parse_c_code_recursiveness(term, c_is_recursive).
:- mode parse_c_code_recursiveness(in, out) is semidet.

parse_c_code_recursiveness(term__functor(term__atom("recursive"), [], _),
	recursive).
parse_c_code_recursiveness(term__functor(term__atom("non_recursive"), [], _),
	non_recursive).

% parse a pragma c_code declaration

:- pred parse_pragma_c_code(c_is_recursive, term, term, varset, maybe1(item)).
:- mode parse_pragma_c_code(in, in, in, in, out) is det.

parse_pragma_c_code(Recursiveness, PredAndVarsTerm, C_CodeTerm, VarSet,
			Result) :-
	(
    	        PredAndVarsTerm = term__functor(term__atom(PredName), VarList,
			_)
	->
		(
                	C_CodeTerm = term__functor(term__string(C_Code), [], _)
		->
	                parse_pragma_c_code_varlist(VarSet, 
				VarList, PragmaVars, Error),
		        (
				Error = no,
				Result = ok(pragma(c_code(Recursiveness,
					unqualified(PredName), 
					PragmaVars, VarSet, C_Code)))
		        ;
				Error = yes(ErrorMessage),
				Result = error(ErrorMessage, PredAndVarsTerm)
		        )
		;
		        Result = error("Expected string for C code", C_CodeTerm)
		)
	;
		Result = error("Term is not a predicate", PredAndVarsTerm)
	).

	% parse the variable list in the pragma c code declaration.
	% The final argument is 'no' for no error, or 'yes(ErrorMessage)'.
:- pred parse_pragma_c_code_varlist(varset, list(term), list(pragma_var), 
	maybe(string)).
:- mode parse_pragma_c_code_varlist(in, in, out, out) is det.

parse_pragma_c_code_varlist(_, [], [], no).
parse_pragma_c_code_varlist(VarSet, [V|Vars], PragmaVars, Error):-
	(
		V = term__functor(term__atom("::"), [VarTerm, ModeTerm], _),
		VarTerm = term__variable(Var)
	->
		(
			varset__search_name(VarSet, Var, VarName)
		->
			(
				convert_mode(ModeTerm, Mode)
			->
				P = (pragma_var(Var, VarName, Mode)),
				parse_pragma_c_code_varlist(VarSet, 
					Vars, PragmaVars0, Error),
				PragmaVars = [P|PragmaVars0]
			;
				PragmaVars = [],
				Error = yes("unknown mode in pragma(c_code, ...")
			)
		;
			% if the variable wasn't in the varset it must be an
			% underscore variable.
			PragmaVars = [],	% return any old junk for that.
			Error = yes(
"sorry, not implemented: anonymous `_' variable in pragma(c_code, ...)")
		)
	;
		PragmaVars = [],	% return any old junk in PragmaVars
		Error = yes("arguments not in form 'Var :: mode'")
	).
%-----------------------------------------------------------------------------%

	% get_determinism(Term0, Term, Determinism) binds Determinism
	% to a representation of the determinism condition of Term0, if any,
	% and binds Term to the other part of Term0. If Term0 does not
	% contain a determinism, then Determinism is bound to `unspecified'.

:- pred get_determinism(term, term, maybe1(maybe(determinism))).
:- mode get_determinism(in, out, out) is det.

get_determinism(B, Body, Determinism) :-
	( 
		B = term__functor(term__atom("is"), Args, _Context1),
		Args = [Body1, Determinism1]
	->
		Body = Body1,
		( 
		    (
			Determinism1 = term__functor(term__atom(Determinism2),
				[], _Context2),
			standard_det(Determinism2, Determinism3)
		    )
		->
			Determinism = ok(yes(Determinism3))
		;
			Determinism = error("invalid category", Determinism1)
		)
	;
		Body = B,
		Determinism = ok(no)
	).

:- pred standard_det(string, determinism).
:- mode standard_det(in, out) is semidet.

standard_det("det", det).
standard_det("cc_nondet", cc_nondet).
standard_det("cc_multi", cc_multidet).
standard_det("nondet", nondet).
standard_det("multi", multidet).
standard_det("multidet", multidet).
standard_det("semidet", semidet).
standard_det("erroneous", erroneous).
standard_det("failure", failure).

%-----------------------------------------------------------------------------%

	% get_condition(Term0, Term, Condition) binds Condition
	% to a representation of the 'where' condition of Term0, if any,
	% and binds Term to the other part of Term0. If Term0 does not
	% contain a condition, then Condition is bound to true.

:- pred get_condition(term, term, condition).
:- mode get_condition(in, out, out) is det.
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
:- pred process_uu_type(term, term, maybe1(type_defn)).
:- mode process_uu_type(in, in, out) is det.
process_uu_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_uu_type_2(Result0, Body, Result).

:- pred process_uu_type_2(maybe_functor, term, maybe1(type_defn)).
:- mode process_uu_type_2(in, in, out) is det.
process_uu_type_2(error(Error, Term), _, error(Error, Term)).
process_uu_type_2(ok(Name, Args), Body, ok(uu_type(Name, Args, List))) :-
		sum_to_list(Body, List).

%-----------------------------------------------------------------------------%

	% This is for "Head == Body" (equivalence) definitions.
:- pred process_eqv_type(term, term, maybe1(type_defn)).
:- mode process_eqv_type(in, in, out) is det.
process_eqv_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_eqv_type_2(Result0, Body, Result).

:- pred process_eqv_type_2(maybe_functor, term, maybe1(type_defn)).
:- mode process_eqv_type_2(in, in, out) is det.
process_eqv_type_2(error(Error, Term), _, error(Error, Term)).
process_eqv_type_2(ok(Name, Args), Body, ok(eqv_type(Name, Args, Body))).

%-----------------------------------------------------------------------------%

	% process_du_type(TypeHead, TypeBody, Result)
	% checks that its arguments are well formed, and if they are,
	% binds Result to a representation of the type information about the
	% TypeHead.
	% This is for "Head ---> Body" (constructor) definitions.
:- pred process_du_type(term, term, maybe1(type_defn)).
:- mode process_du_type(in, in, out) is det.
process_du_type(Head, Body, Result) :-
	check_for_errors(Head, Body, Result0),
	process_du_type_2(Result0, Body, Result).

:- pred process_du_type_2(maybe_functor, term, maybe1(type_defn)).
:- mode process_du_type_2(in, in, out) is det.
process_du_type_2(error(Error, Term), _, error(Error, Term)).
process_du_type_2(ok(Functor, Args), Body, Result) :-
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

:- pred process_abstract_type(term, maybe1(type_defn)).
:- mode process_abstract_type(in, out) is det.
process_abstract_type(Head, Result) :-
	dummy_term(Body),
	check_for_errors(Head, Body, Result0),
	process_abstract_type_2(Result0, Result).

:- pred process_abstract_type_2(maybe_functor, maybe1(type_defn)).
:- mode process_abstract_type_2(in, out) is det.
process_abstract_type_2(error(Error, Term), error(Error, Term)).
process_abstract_type_2(ok(Functor, Args), ok(abstract_type(Functor, Args))).

%-----------------------------------------------------------------------------%

	%  check a type definition for errors

:- pred check_for_errors(term, term, maybe_functor).
:- mode check_for_errors(in, in, out) is det.
check_for_errors(Head, Body, Result) :-
	( Head = term__variable(_) ->
		Result = error("Variable on LHS of type definition", Head)
	;
		parse_qualified_term(Head, "type definition", R),
		check_for_errors_2(R, Body, Head, Result)
	).

:- pred check_for_errors_2(maybe_functor, term, term, maybe_functor).
:- mode check_for_errors_2(in, in, in, out) is det.
check_for_errors_2(error(Msg, Term), _, _, error(Msg, Term)).
check_for_errors_2(ok(Name, Args), Body, Head, Result) :-
	check_for_errors_3(Name, Args, Body, Head, Result).

:- pred check_for_errors_3(sym_name, list(term), term, term, maybe_functor).
:- mode check_for_errors_3(in, in, in, in, out) is det.
check_for_errors_3(Name, Args, Body, Head, Result) :-
	% check that all the head args are variables
	( %%%	some [Arg]
		(
			list__member(Arg, Args),
			Arg \= term__variable(_)
		)
	->
		Result = error("Type parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
	  %%%	some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error("Repeated type parameters in LHS of type defn", Head)
	% check that all the variables in the body occur in the head
	; %%% some [Var2]
		(
			term__contains_var(Body, Var2),
			\+ term__contains_var_list(Args, Var2)
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
:- mode convert_constructors(in, out) is semidet.
convert_constructors(Body, Constrs) :-
	disjunction_to_list(Body, List),
	convert_constructors_2(List, Constrs).

	% true if input argument is a valid list of constructors

:- pred convert_constructors_2(list(term), list(constructor)).
:- mode convert_constructors_2(in, out) is semidet.
convert_constructors_2([], []).
convert_constructors_2([Term | Terms], [Constr | Constrs]) :-
	convert_constructor(Term, Constr),
	convert_constructors_2(Terms, Constrs).

	% true if input argument is a valid constructor.
	% Note that as a special case, one level of
	% curly braces around the constructor are ignored.
	% This is to allow you to define ';'/2 constructors.

:- pred convert_constructor(term, constructor).
:- mode convert_constructor(in, out) is semidet.
convert_constructor(Term, Result) :-
	( 
		Term = term__functor(term__atom("{}"), [Term1], _Context)
	->
		Term2 = Term1
	;
		Term2 = Term
	),
	parse_qualified_term(Term2, "convert_constructor/2", ok(F, As)),
	convert_type_list(As, ArgTypes),
	Result = F - ArgTypes.

%-----------------------------------------------------------------------------%

	% convert a "disjunction" (bunch of terms separated by ';'s) to a list

:- pred disjunction_to_list(term, list(term)).
:- mode disjunction_to_list(in, out) is det.
disjunction_to_list(Term, List) :-
	binop_term_to_list(";", Term, List).

	% convert a "conjunction" (bunch of terms separated by ','s) to a list

:- pred conjunction_to_list(term, list(term)).
:- mode conjunction_to_list(in, out) is det.
conjunction_to_list(Term, List) :-
	binop_term_to_list(",", Term, List).

	% convert a "sum" (bunch of terms separated by '+' operators) to a list

:- pred sum_to_list(term, list(term)).
:- mode sum_to_list(in, out) is det.
sum_to_list(Term, List) :-
	binop_term_to_list("+", Term, List).

	% general predicate to convert terms separated by any specified
	% operator into a list

:- pred binop_term_to_list(string, term, list(term)).
:- mode binop_term_to_list(in, in, out) is det.
binop_term_to_list(Op, Term, List) :-
	binop_term_to_list_2(Op, Term, [], List).

:- pred binop_term_to_list_2(string, term, list(term), list(term)).
:- mode binop_term_to_list_2(in, in, in, out) is det.
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

:- pred process_pred(string, varset, term, condition, maybe(determinism),
			maybe1(item)).
:- mode process_pred(in, in, in, in, in, out) is det.

process_pred(ModuleName, VarSet, PredType, Cond, MaybeDet, Result) :-
	parse_qualified_term(ModuleName, PredType, "`:- pred' declaration", R),
	process_pred_2(R, PredType, VarSet, MaybeDet, Cond, Result).

:- pred process_pred_2(maybe_functor, term, varset, maybe(determinism),
			condition, maybe1(item)).
:- mode process_pred_2(in, in, in, in, in, out) is det.
process_pred_2(ok(F, As0), PredType, VarSet, MaybeDet, Cond, Result) :-
	(
		convert_type_and_mode_list(As0, As)
	->
		Result = ok(pred(VarSet, F, As, MaybeDet, Cond))
	;
		Result = error("Syntax error in `:- pred' declaration",
				PredType)
	).
process_pred_2(error(M, T), _, _, _, _, error(M, T)).

%-----------------------------------------------------------------------------%

	% parse a `:- func p(...)' declaration

:- pred process_func(string, varset, term, condition, maybe(determinism),
			maybe1(item)).
:- mode process_func(in, in, in, in, in, out) is det.

process_func(ModuleName, VarSet, Term, Cond, MaybeDet, Result) :-
	(
		Term = term__functor(term__atom("="),
				[FuncTerm, ReturnTypeTerm], _Context)
	->
		parse_qualified_term(ModuleName, FuncTerm,
			"`:- func' declaration", R),
		process_func_2(R, FuncTerm, ReturnTypeTerm, VarSet, MaybeDet,
				Cond, Result)
	;
		Result = error("`=' expected in `:- func' declaration", Term)
	).

:- pred process_func_2(maybe_functor, term, term, varset, maybe(determinism),
			condition, maybe1(item)).
:- mode process_func_2(in, in, in, in, in, in, out) is det.
process_func_2(ok(F, As0), FuncTerm, ReturnTypeTerm, VarSet, MaybeDet, Cond,
		Result) :-
	( convert_type_and_mode_list(As0, As) ->
		( convert_type_and_mode(ReturnTypeTerm, ReturnType) ->
			Result = ok(func(VarSet, F, As, ReturnType, MaybeDet,
					Cond))
		;
			Result = error(
			"Syntax error in return type of `:- func' declaration",
					ReturnTypeTerm)
		)
	;
		Result = error(
			"Syntax error in arguments of `:- func' declaration",
					FuncTerm)
	).
process_func_2(error(M, T), _, _, _, _, _, error(M, T)).

%-----------------------------------------------------------------------------%

	% parse a `:- mode p(...)' declaration

:- pred process_mode(string, varset, term, condition, maybe(determinism),
		maybe1(item)).
:- mode process_mode(in, in, in, in, in, out) is det.

process_mode(ModuleName, VarSet, Term, Cond, MaybeDet, Result) :-
	(
		Term = term__functor(term__atom("="),
				[FuncTerm, ReturnTypeTerm], _Context)
	->
		parse_qualified_term(ModuleName, FuncTerm,
				"function `:- mode' declaration", R),
		process_func_mode(R, FuncTerm, ReturnTypeTerm, VarSet, MaybeDet,
				Cond, Result)
	;
		parse_qualified_term(ModuleName, Term,
				"predicate `:- mode' declaration", R),
		process_pred_mode(R, Term, VarSet, MaybeDet, Cond, Result)
	).

:- pred process_pred_mode(maybe_functor, term, varset, maybe(determinism),
			condition, maybe1(item)).
:- mode process_pred_mode(in, in, in, in, in, out) is det.

process_pred_mode(ok(F, As0), PredMode, VarSet, MaybeDet, Cond, Result) :-
	(
		convert_mode_list(As0, As)
	->
		Result = ok(pred_mode(VarSet, F, As, MaybeDet, Cond))
	;
		Result = error("Syntax error in predicate mode declaration",
				PredMode)
	).
process_pred_mode(error(M, T), _, _, _, _, error(M, T)).

:- pred process_func_mode(maybe_functor, term, term, varset, maybe(determinism),
			condition, maybe1(item)).
:- mode process_func_mode(in, in, in, in, in, in, out) is det.

process_func_mode(ok(F, As0), FuncMode, RetMode0, VarSet, MaybeDet, Cond,
		Result) :-
	(
		convert_mode_list(As0, As)
	->
		( convert_mode(RetMode0, RetMode) ->
			Result = ok(func_mode(VarSet, F, As, RetMode, MaybeDet,
					Cond))
		;
			Result = error(
		"Syntax error in return mode of function mode declaration",
					RetMode0)
		)
	;
		Result = error(
		"Syntax error in arguments of function mode declaration",
				FuncMode)
	).
process_func_mode(error(M, T), _, _, _, _, _, error(M, T)).

%-----------------------------------------------------------------------------%

	% parse a `:- inst foo = ...' definition

:- pred parse_inst_decl(varset, term, maybe1(item)).
:- mode parse_inst_decl(in, in, out) is det.
parse_inst_decl(VarSet, InstDefn, Result) :-
	(
		InstDefn = term__functor(term__atom(Op), [H, B], _Context),
		( Op = "=" ; Op = "==" )
	->
		get_condition(B, Body, Condition),
		convert_inst_defn(H, Body, R),
		process_maybe1(make_inst_defn(VarSet, Condition), R, Result)
	;
		% XXX this is for `abstract inst' declarations,
		% which are not really supported
		InstDefn = term__functor(term__atom("is"), [
				Head,
				term__functor(term__atom("private"), [], _)
			], _)
	->
		Condition = true,
		convert_abstract_inst_defn(Head, R),
		process_maybe1(make_inst_defn(VarSet, Condition), R, Result)
	;
		InstDefn = term__functor(term__atom("--->"), [H, B], Context)
	->
		get_condition(B, Body, Condition),
		Body1 = term__functor(term__atom("bound"), [Body], Context),
		convert_inst_defn(H, Body1, R),
		process_maybe1(make_inst_defn(VarSet, Condition), R, Result)
	;
		Result = error("`=' expected in `:- inst' definition", InstDefn)
	).
		% we should check the condition for errs
		% (don't bother at the moment, since we ignore
		% conditions anyhow :-)

:- pred convert_inst_defn(term, term, maybe1(inst_defn)).
:- mode convert_inst_defn(in, in, out) is det.
convert_inst_defn(Head, Body, Result) :-
	parse_qualified_term(Head, "inst definition", R),
	convert_inst_defn_2(R, Head, Body, Result).

:- pred convert_inst_defn_2(maybe_functor, term, term, maybe1(inst_defn)).
:- mode convert_inst_defn_2(in, in, in, out) is det.

convert_inst_defn_2(error(M, T), _, _, error(M, T)).
convert_inst_defn_2(ok(Name, Args), Head, Body, Result) :-
	% check that all the head args are variables
	( %%%	some [Arg]
		(
			list__member(Arg, Args),
			Arg \= term__variable(_)
		)
	->
		Result = error("Inst parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
	%%%	some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error("Repeated inst parameters in LHS of inst defn",
				Head)
	;
	% check that all the variables in the body occur in the head
	%%%	some [Var2]
		(
			term__contains_var(Body, Var2),
			\+ term__contains_var_list(Args, Var2)
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

:- pred convert_abstract_inst_defn(term, maybe1(inst_defn)).
:- mode convert_abstract_inst_defn(in, out) is det.
convert_abstract_inst_defn(Head, Result) :-
	parse_qualified_term(Head, "inst definition", R),
	convert_abstract_inst_defn_2(R, Head, Result).

:- pred convert_abstract_inst_defn_2(maybe_functor, term, maybe1(inst_defn)).
:- mode convert_abstract_inst_defn_2(in, in, out) is det.
convert_abstract_inst_defn_2(error(M, T), _, error(M, T)).
convert_abstract_inst_defn_2(ok(Name, Args), Head, Result) :-
	% check that all the head args are variables
	( %%%	some [Arg]
		(
			list__member(Arg, Args),
			Arg \= term__variable(_)
		)
	->
		Result = error("Inst parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
	%%%	some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error(
			"Repeated inst parameters in abstract inst definition",
				Head)
	;
		Result = ok(abstract_inst(Name, Args))
	).

:- pred convert_inst_list(list(term), list(inst)).
:- mode convert_inst_list(in, out) is semidet.
convert_inst_list([], []).
convert_inst_list([H0|T0], [H|T]) :-
	convert_inst(H0, H),
	convert_inst_list(T0, T).

:- pred convert_inst(term, inst).
:- mode convert_inst(in, out) is semidet.
convert_inst(term__variable(V), inst_var(V)).
convert_inst(term__functor(Name, Args0, Context), Result) :-
	% `free' insts
	( Name = term__atom("free"), Args0 = [] ->
		Result = free

	% `any' insts
	; Name = term__atom("any"), Args0 = [] ->
		Result = any(shared)
	; Name = term__atom("unique_any"), Args0 = [] ->
		Result = any(unique)
	; Name = term__atom("mostly_unique_any"), Args0 = [] ->
		Result = any(mostly_unique)
	; Name = term__atom("clobbered_any"), Args0 = [] ->
		Result = any(clobbered)
	; Name = term__atom("mostly_clobbered_any"), Args0 = [] ->
		Result = any(mostly_clobbered)

	% `ground' insts
	; Name = term__atom("ground"), Args0 = [] ->
		Result = ground(shared, no)
	; Name = term__atom("unique"), Args0 = [] ->
		Result = ground(unique, no)
	; Name = term__atom("mostly_unique"), Args0 = [] ->
		Result = ground(mostly_unique, no)
	; Name = term__atom("clobbered"), Args0 = [] ->
		Result = ground(clobbered, no)
	; Name = term__atom("mostly_clobbered"), Args0 = [] ->
		Result = ground(mostly_clobbered, no)
	;
		% The syntax for a higher-order pred inst is
		%
		%	pred(<Mode1>, <Mode2>, ...) is <Detism>
		%
		% where <Mode1>, <Mode2>, ... are a list of modes,
		% and <Detism> is a determinism.

		Name = term__atom("is"), Args0 = [PredTerm, DetTerm],
		PredTerm = term__functor(term__atom("pred"), ArgModesTerm, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(ArgModesTerm, ArgModes),
		PredInst = pred_inst_info(predicate, ArgModes, Detism),
		Result = ground(shared, yes(PredInst))
	;

		% The syntax for a higher-order func inst is
		%
		%	func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
		%
		% where <Mode1>, <Mode2>, ... are a list of modes,
		% <RetMode> is a mode, and <Detism> is a determinism.

		Name = term__atom("is"), Args0 = [EqTerm, DetTerm],
		EqTerm = term__functor(term__atom("="),
					[FuncTerm, RetModeTerm], _),
		FuncTerm = term__functor(term__atom("func"), ArgModesTerm, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(ArgModesTerm, ArgModes0),
		convert_mode(RetModeTerm, RetMode),
		list__append(ArgModes0, [RetMode], ArgModes),
		FuncInst = pred_inst_info(function, ArgModes, Detism),
		Result = ground(shared, yes(FuncInst))

	% `not_reached' inst
	; Name = term__atom("not_reached"), Args0 = [] ->
		Result = not_reached

	% `bound' insts
	; Name = term__atom("bound"), Args0 = [Disj] ->
		parse_bound_inst_list(Disj, shared, Result)
/* `bound_unique' is for backwards compatibility - use `unique' instead */
	; Name = term__atom("bound_unique"), Args0 = [Disj] ->
		parse_bound_inst_list(Disj, unique, Result)
	; Name = term__atom("unique"), Args0 = [Disj] ->
		parse_bound_inst_list(Disj, unique, Result)
	; Name = term__atom("mostly_unique"), Args0 = [Disj] ->
		parse_bound_inst_list(Disj, mostly_unique, Result)

	% anything else must be a user-defined inst
	;
		parse_qualified_term(term__functor(Name, Args0, Context),
			"", ok(QualifiedName, Args1)),
		convert_inst_list(Args1, Args),
		Result = defined_inst(user_inst(QualifiedName, Args))
	).

:- pred parse_bound_inst_list(term::in, uniqueness::in, (inst)::out) is semidet.
parse_bound_inst_list(Disj, Uniqueness, bound(Uniqueness, Functors)) :-
	disjunction_to_list(Disj, List),
	convert_bound_inst_list(List, Functors0),
	list__sort_and_remove_dups(Functors0, Functors).

:- pred convert_bound_inst_list(list(term), list(bound_inst)).
:- mode convert_bound_inst_list(in, out) is semidet.
convert_bound_inst_list([], []).
convert_bound_inst_list([H0|T0], [H|T]) :-
	convert_bound_inst(H0, H),
	convert_bound_inst_list(T0, T).

:- pred convert_bound_inst(term, bound_inst).
:- mode convert_bound_inst(in, out) is semidet.
convert_bound_inst(term__functor(Name0, Args0, _), functor(ConsId, Args)) :-
	list__length(Args0, Arity),
	make_functor_cons_id(Name0, Arity, ConsId),
	convert_inst_list(Args0, Args).

:- pred make_inst_defn(varset, condition, inst_defn, item).
:- mode make_inst_defn(in, in, in, out) is det.
make_inst_defn(VarSet, Cond, InstDefn, inst_defn(VarSet, InstDefn, Cond)).

%-----------------------------------------------------------------------------%

	% parse a `:- mode foo :: ...' or `:- mode foo = ...' definition.

:- pred parse_mode_decl(string, varset, term, maybe1(item)).
:- mode parse_mode_decl(in, in, in, out) is det.
parse_mode_decl(ModuleName, VarSet, ModeDefn, Result) :-
	( %%% some [H, B]
		mode_op(ModeDefn, H, B)
	->
		get_condition(B, Body, Condition),
		convert_mode_defn(H, Body, R),
		process_maybe1(make_mode_defn(VarSet, Condition), R, Result)
	;
		parse_mode_decl_pred(ModuleName, VarSet, ModeDefn, Result)
	).

:- pred mode_op(term, term, term).
:- mode mode_op(in, out, out) is semidet.
mode_op(term__functor(term__atom(Op), [H, B], _), H, B) :-
		% People never seem to remember what the right
		% operator to use in a `:- mode' declaration is,
		% so the syntax is forgiving.
		% We allow `::', the standard one which has the right
		% precedence, but we also allow `==' just to be nice.
	(	Op = "::"
	->	true
	;	Op = "=="
	).

:- pred convert_mode_defn(term, term, maybe1(mode_defn)).
:- mode convert_mode_defn(in, in, out) is det.
convert_mode_defn(Head, Body, Result) :-
	parse_qualified_term(Head, "mode definition", R),
	convert_mode_defn_2(R, Head, Body, Result).

:- pred convert_mode_defn_2(maybe_functor, term, term, maybe1(mode_defn)).
:- mode convert_mode_defn_2(in, in, in, out) is det.
convert_mode_defn_2(error(M, T), _, _, error(M, T)).
convert_mode_defn_2(ok(Name, Args), Head, Body, Result) :-
	% check that all the head args are variables
	( %%% some [Arg]
		(
			list__member(Arg, Args),
			Arg \= term__variable(_)
		)
	->
		Result = error("Mode parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
		%%% some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error("Repeated parameters in LHS of mode defn",
				Head)
	% check that all the variables in the body occur in the head
	; %%% some [Var2]
		(
			term__contains_var(Body, Var2),
			\+ term__contains_var_list(Args, Var2)
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
:- mode convert_type_and_mode_list(in, out) is semidet.
convert_type_and_mode_list([], []).
convert_type_and_mode_list([H0|T0], [H|T]) :-
	convert_type_and_mode(H0, H),
	convert_type_and_mode_list(T0, T).

:- pred convert_type_and_mode(term, type_and_mode).
:- mode convert_type_and_mode(in, out) is semidet.
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
:- mode convert_mode_list(in, out) is semidet.
convert_mode_list([], []).
convert_mode_list([H0|T0], [H|T]) :-
	convert_mode(H0, H),
	convert_mode_list(T0, T).

:- pred convert_mode(term, mode).
:- mode convert_mode(in, out) is semidet.
convert_mode(Term, Mode) :-
	(
		Term = term__functor(term__atom("->"), [InstA, InstB], _Context)
	->
		convert_inst(InstA, ConvertedInstA),
		convert_inst(InstB, ConvertedInstB),
		Mode = (ConvertedInstA -> ConvertedInstB)
	;
		% Handle higher-order predicate modes:
		% a mode of the form
		%	pred(<Mode1>, <Mode2>, ...) is <Det>
		% is an abbreviation for the inst mapping
		% 	(  pred(<Mode1>, <Mode2>, ...) is <Det>
		%	-> pred(<Mode1>, <Mode2>, ...) is <Det>
		%	)

		Term = term__functor(term__atom("is"), [PredTerm, DetTerm], _),
		PredTerm = term__functor(term__atom("pred"), ArgModesTerms, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(ArgModesTerms, ArgModes),
		PredInstInfo = pred_inst_info(predicate, ArgModes, Detism),
		Inst = ground(shared, yes(PredInstInfo)),
		Mode = (Inst -> Inst)
	;
		% Handle higher-order function modes:
		% a mode of the form
		%	func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
		% is an abbreviation for the inst mapping
		% 	(  func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
		%	-> func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
		%	)

		Term = term__functor(term__atom("is"), [EqTerm, DetTerm], _),
		EqTerm = term__functor(term__atom("="),
					[FuncTerm, RetModeTerm], _),
		FuncTerm = term__functor(term__atom("func"), ArgModesTerms, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(ArgModesTerms, ArgModes0),
		convert_mode(RetModeTerm, RetMode),
		list__append(ArgModes0, [RetMode], ArgModes),
		FuncInstInfo = pred_inst_info(function, ArgModes, Detism),
		Inst = ground(shared, yes(FuncInstInfo)),
		Mode = (Inst -> Inst)
	;
		parse_qualified_term(Term, "mode definition", R),
		R = ok(Name, Args),	% should improve error reporting
		convert_inst_list(Args, ConvertedArgs),
		Mode = user_defined_mode(Name, ConvertedArgs)
	).

:- pred make_mode_defn(varset, condition, mode_defn, item).
:- mode make_mode_defn(in, in, in, out) is det.
make_mode_defn(VarSet, Cond, ModeDefn, mode_defn(VarSet, ModeDefn, Cond)).

%-----------------------------------------------------------------------------%

:- type parser(T) == pred(term, maybe1(T)).
:- mode parser    :: pred(in, out) is det.

:- type maker(T1, T2) == pred(T1, T2).
:- mode maker         :: pred(in, out) is det.

:- pred parse_symlist_decl(parser(T), maker(list(T), sym_list),
			maker(sym_list, module_defn),
			term, varset, maybe1(item)).
:- mode parse_symlist_decl(parser, maker, maker, in, in, out) is det.

parse_symlist_decl(ParserPred, MakeSymListPred, MakeModuleDefnPred,
			Term, VarSet, Result) :-
	parse_list(ParserPred, Term, Result0),
	process_maybe1(make_module_defn(MakeSymListPred, MakeModuleDefnPred,
			VarSet), Result0, Result).

:- pred make_module_defn(maker(T, sym_list), maker(sym_list, module_defn),
			varset, T, item).
:- mode make_module_defn(maker, maker, in, in, out) is det.
make_module_defn(MakeSymListPred, MakeModuleDefnPred, VarSet, T,
		module_defn(VarSet, ModuleDefn)) :-
	call(MakeSymListPred, T, SymList),
	call(MakeModuleDefnPred, SymList, ModuleDefn).

%-----------------------------------------------------------------------------%

	% Parse a comma-separated list (misleading described as
	% a "conjunction") of things.

:- pred parse_list(parser(T), term, maybe1(list(T))).
:- mode parse_list(parser, in, out) is det.
parse_list(Parser, Term, Result) :-
	conjunction_to_list(Term, List),
	parse_list_2(List, Parser, Result).

:- pred parse_list_2(list(term), parser(T), maybe1(list(T))).
:- mode parse_list_2(in, parser, out) is det.
parse_list_2([], _, ok([])).
parse_list_2([X|Xs], Parser, Result) :-
	call(Parser, X, X_Result),
	parse_list_2(Xs, Parser, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

	% If a list of things contains multiple errors, then we only
	% report the first one.

:- pred combine_list_results(maybe1(T), maybe1(list(T)), maybe1(list(T))).
:- mode combine_list_results(in, in, out) is det.
combine_list_results(error(Msg, Term), _, error(Msg, Term)).
combine_list_results(ok(_), error(Msg, Term), error(Msg, Term)).
combine_list_results(ok(X), ok(Xs), ok([X|Xs])).

%-----------------------------------------------------------------------------%

:- pred process_maybe1(maker(T1, T2), maybe1(T1), maybe1(T2)).
:- mode process_maybe1(maker, in, out) is det.
process_maybe1(Maker, ok(X), ok(Y)) :- !, call(Maker, X, Y).
process_maybe1(_, error(M, T), error(M, T)).

:- pred process_maybe1_to_t(maker(T1, maybe1(T2)), maybe1(T1), maybe1(T2)).
:- mode process_maybe1_to_t(maker, in, out) is det.
process_maybe1_to_t(Maker, ok(X), Y) :- !, call(Maker, X, Y).
process_maybe1_to_t(_, error(M, T), error(M, T)).

%-----------------------------------------------------------------------------%

:- pred make_module(list(module_specifier)::in, sym_list::out) is det.
make_module(X, module(X)).

:- pred make_sym(list(sym_specifier)::in, sym_list::out) is det.
make_sym(X, sym(X)).

:- pred make_pred(list(pred_specifier)::in, sym_list::out) is det.
make_pred(X, pred(X)).

:- pred make_func(list(func_specifier)::in, sym_list::out) is det.
make_func(X, func(X)).

:- pred make_cons(list(cons_specifier)::in, sym_list::out) is det.
make_cons(X, cons(X)).

:- pred make_type(list(type_specifier)::in, sym_list::out) is det.
make_type(X, type(X)).

:- pred make_adt(list(adt_specifier)::in, sym_list::out) is det.
make_adt(X, adt(X)).

:- pred make_op(list(op_specifier)::in, sym_list::out) is det.
make_op(X, op(X)).

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

:- pred parse_symbol_specifier(term, maybe1(sym_specifier)).
:- mode parse_symbol_specifier(in, out) is det.

parse_symbol_specifier(MainTerm, Result) :-
	( MainTerm = term__functor(term__atom(Functor), [Term], _Context) ->
		( Functor = "cons" ->
			parse_constructor_specifier(Term, Result0),
			process_maybe1(make_cons_symbol_specifier, Result0,
				Result)
		; Functor = "pred" ->
			parse_predicate_specifier(Term, Result0),
			process_maybe1(make_pred_symbol_specifier, Result0,
				Result)
		; Functor = "func" ->
			parse_function_specifier(Term, Result0),
			process_maybe1(make_func_symbol_specifier, Result0,
				Result)
		; Functor = "type" ->
			parse_type_specifier(Term, Result0),
			process_maybe1(make_type_symbol_specifier, Result0,
				Result)
		; Functor = "adt" ->
			parse_adt_specifier(Term, Result0),
			process_maybe1(make_adt_symbol_specifier, Result0,
				Result)
		; Functor = "op" ->
			parse_op_specifier(Term, Result0),
			process_maybe1(make_op_symbol_specifier, Result0,
				Result)
		; Functor = "module" ->
			parse_module_specifier(Term, Result0),
			process_maybe1(make_module_symbol_specifier, Result0,
				Result)
		;
			parse_constructor_specifier(MainTerm, Result0),
			process_maybe1(make_cons_symbol_specifier, Result0,
				Result)
		)
	;
		parse_constructor_specifier(MainTerm, Result0),
		process_maybe1(make_cons_symbol_specifier, Result0, Result)
	).

% 	Once we've parsed the appropriate type of symbol specifier, we
%	need to convert it to a sym_specifier.

:- pred make_pred_symbol_specifier(pred_specifier::in, sym_specifier::out)
	is det.
make_pred_symbol_specifier(PredSpec, pred(PredSpec)).

:- pred make_func_symbol_specifier(func_specifier::in, sym_specifier::out)
	is det.
make_func_symbol_specifier(FuncSpec, func(FuncSpec)).

:- pred make_cons_symbol_specifier(cons_specifier::in, sym_specifier::out)
	is det.
make_cons_symbol_specifier(ConsSpec, cons(ConsSpec)).

:- pred make_type_symbol_specifier(type_specifier::in, sym_specifier::out)
	is det.
make_type_symbol_specifier(TypeSpec, type(TypeSpec)).

:- pred make_adt_symbol_specifier(adt_specifier::in, sym_specifier::out) is det.
make_adt_symbol_specifier(ADT_Spec, adt(ADT_Spec)).

:- pred make_op_symbol_specifier(op_specifier::in, sym_specifier::out) is det.
make_op_symbol_specifier(OpSpec, op(OpSpec)).

:- pred make_module_symbol_specifier(module_specifier::in, sym_specifier::out)
	is det.
make_module_symbol_specifier(ModuleSpec, module(ModuleSpec)).

:- pred cons_specifier_to_sym_specifier(cons_specifier, sym_specifier).
:- mode cons_specifier_to_sym_specifier(in, out) is det.

cons_specifier_to_sym_specifier(sym(SymSpec), sym(SymSpec)).
cons_specifier_to_sym_specifier(typed(SymSpec), typed_sym(SymSpec)).

%-----------------------------------------------------------------------------%

%	A ModuleSpecifier is just an identifier.

:- pred parse_module_specifier(term, maybe1(module_specifier)).
:- mode parse_module_specifier(in, out) is det.
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

:- pred parse_constructor_specifier(term, maybe1(cons_specifier)).
:- mode parse_constructor_specifier(in, out) is det.
parse_constructor_specifier(Term, Result) :-
    (
	Term = term__functor(term__atom("::"), [NameArgsTerm, TypeTerm],
		_Context)
    ->
	parse_arg_types_specifier(NameArgsTerm, NameArgsResult),
	parse_type(TypeTerm, TypeResult),
	process_typed_constructor_specifier(NameArgsResult, TypeResult, Result)
    ;
	parse_arg_types_specifier(Term, TermResult),
	process_maybe1(make_untyped_cons_spec, TermResult, Result)
    ).

%-----------------------------------------------------------------------------%

%	A PredicateSpecifier is one of
%		SymbolName(ArgType1, ..., ArgTypeN)
%			Matches only predicates with the specified argument
%			types.
%		SymbolNameSpecifier

:- pred parse_predicate_specifier(term, maybe1(pred_specifier)).
:- mode parse_predicate_specifier(in, out) is det.
parse_predicate_specifier(Term, Result) :-
    (
	Term = term__functor(term__atom("/"), [_,_], _Context)
    ->
	parse_symbol_name_specifier(Term, NameResult),
        process_maybe1(make_arity_predicate_specifier, NameResult, Result)
    ;
	parse_qualified_term(Term, "predicate specifier", TermResult),
	process_typed_predicate_specifier(TermResult, Result)
    ).

:- pred process_typed_predicate_specifier(maybe_functor, maybe1(pred_specifier)).
:- mode process_typed_predicate_specifier(in, out) is det.
process_typed_predicate_specifier(ok(Name, Args), ok(Result)) :-
    ( Args = [] ->
	Result = sym(name(Name))
    ;
	Result = name_args(Name, Args)
    ).
process_typed_predicate_specifier(error(Msg, Term), error(Msg, Term)).

:- pred make_arity_predicate_specifier(sym_name_specifier, pred_specifier).
:- mode make_arity_predicate_specifier(in, out) is det.
make_arity_predicate_specifier(Result, sym(Result)).

%-----------------------------------------------------------------------------%

% 	Parsing the name & argument types of a constructor specifier is
% 	exactly the same as parsing a predicate specifier...

:- pred parse_arg_types_specifier(term, maybe1(pred_specifier)).
:- mode parse_arg_types_specifier(in, out) is det.
parse_arg_types_specifier(Term, Result) :-
    (
	Term = term__functor(term__atom("/"), [_,_], _Context)
    ->
	parse_symbol_name_specifier(Term, NameResult),
        process_maybe1(make_arity_predicate_specifier, NameResult, Result)
    ;
	parse_qualified_term(Term, "constructor specifier", TermResult),
	process_typed_predicate_specifier(TermResult, Result)
    ).

% 	... but we have to convert the result back into the appropriate
% 	format.

:- pred process_typed_constructor_specifier(maybe1(pred_specifier),
		maybe1(type), maybe1(cons_specifier)).
:- mode process_typed_constructor_specifier(in, in, out) is det.
process_typed_constructor_specifier(error(Msg, Term), _, error(Msg, Term)).
process_typed_constructor_specifier(ok(_), error(Msg, Term), error(Msg, Term)).
process_typed_constructor_specifier(ok(NameArgs), ok(ResType), ok(Result)) :-
	process_typed_cons_spec_2(NameArgs, ResType, Result).

:- pred process_typed_cons_spec_2(pred_specifier, type, cons_specifier).
:- mode process_typed_cons_spec_2(in, in, out) is det.
process_typed_cons_spec_2(sym(Name), Res, typed(name_res(Name, Res))).
process_typed_cons_spec_2(name_args(Name, Args), Res,
			  typed(name_args_res(Name, Args, Res))).

:- pred make_untyped_cons_spec(pred_specifier::in, cons_specifier::out) is det.
make_untyped_cons_spec(sym(Name), sym(Name)).
make_untyped_cons_spec(name_args(Name, Args), typed(name_args(Name, Args))).

%-----------------------------------------------------------------------------%

%	A SymbolNameSpecifier is one of
%		SymbolName
%		SymbolName/Arity
%			Matches only symbols of the specified arity.
%	

:- pred parse_symbol_name_specifier(term, maybe1(sym_name_specifier)).
:- mode parse_symbol_name_specifier(in, out) is det.
parse_symbol_name_specifier(Term, Result) :-
    ( %%% some [NameTerm, ArityTerm, Context]
       	Term = term__functor(term__atom("/"), [NameTerm, ArityTerm], _Context)
    ->
        ( %%% some [Arity, Context2]
            ArityTerm = term__functor(term__integer(Arity), [], _Context2)
	->
            ( Arity >= 0 ->
		parse_symbol_name(NameTerm, NameResult),
		process_maybe1(make_name_arity_specifier(Arity), NameResult,
			Result)
	    ;
		Result = error("Arity in symbol name specifier must be a non-negative integer", Term)
	    )
        ;
	    Result = error("Arity in symbol name specifier must be an integer", Term)
        )
    ;
	parse_symbol_name(Term, SymbolNameResult),
	process_maybe1(make_name_specifier, SymbolNameResult, Result)
    ).

:- pred make_name_arity_specifier(arity, sym_name, sym_name_specifier).
:- mode make_name_arity_specifier(in, in, out) is det.
make_name_arity_specifier(Arity, Name, name_arity(Name, Arity)).

:- pred make_name_specifier(sym_name::in, sym_name_specifier::out) is det.
make_name_specifier(Name, name(Name)).

%-----------------------------------------------------------------------------%

%	A QualifiedTerm is one of
%		Name(Args)
%		Module:Name(Args)
%	(or if Args is empty, one of
%		Name
%		Module:Name)

:- pred parse_qualified_term(string, term, string, maybe_functor).
:- mode parse_qualified_term(in, in, in, out) is det.
parse_qualified_term(DefaultModName, Term, Msg, Result) :-
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
		(
		    Module = DefaultModName
		->
		    Result = ok(qualified(Module, Name), Args)
		;
		    Result = error("Module qualifier in predicate definition  does not match preceding `:- module' declaration", Term)
		)
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
	    (
		DefaultModName = ""
	    ->
            	Result = ok(unqualified(Name2), Args2)
	    ;
		Result = ok(qualified(DefaultModName, Name2), Args2)
	    )
        ;
	    string__append("atom expected in ", Msg, ErrorMsg),
            Result = error(ErrorMsg, Term)
        )
    ).

% parse_qualified_term/3 calls parse_qualified_term/4, and is used when
% no default module name exists.

:- pred parse_qualified_term(term, string, maybe_functor).
:- mode parse_qualified_term(in, in, out) is det.
parse_qualified_term(Term, Msg, Result) :-
	parse_qualified_term("", Term, Msg, Result).

%-----------------------------------------------------------------------------%

%	A SymbolName is one of
%		Name
%			Matches symbols with the specified name in the
%			current namespace.
%		Module:Name
%			Matches symbols with the specified name exported
%			by the specified module.

:- pred parse_symbol_name(string, term, maybe1(sym_name)).
:- mode parse_symbol_name(in, in, out) is det.
parse_symbol_name(DefaultModName, Term, Result) :-
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
	    (
		DefaultModName = ""
	    ->
		Result = ok(unqualified(Name2))
	    ;
		Result = ok(qualified(DefaultModName, Name2))
	    )
        ;
            Result = error("Symbol name specifier expected", Term)
        )
    ).


:- pred parse_symbol_name(term, maybe1(sym_name)).
:- mode parse_symbol_name(in, out) is det.
parse_symbol_name(Term, Result) :- parse_symbol_name("", Term, Result).

%-----------------------------------------------------------------------------%

% predicates used to convert a sym_list to a program item

:- pred make_use(sym_list::in, module_defn::out) is det.
make_use(Syms, use(Syms)).

:- pred make_import(sym_list::in, module_defn::out) is det.
make_import(Syms, import(Syms)).

:- pred make_export(sym_list::in, module_defn::out) is det.
make_export(Syms, export(Syms)).

%-----------------------------------------------------------------------------%

%	A FuncSpecifier is just a constructur name specifier.

:- pred parse_function_specifier(term, maybe1(func_specifier)).
:- mode parse_function_specifier(in, out) is det.
parse_function_specifier(Term, Result) :-
	parse_constructor_specifier(Term, Result).

%	A TypeSpecifier is just a symbol name specifier.

:- pred parse_type_specifier(term, maybe1(sym_name_specifier)).
:- mode parse_type_specifier(in, out) is det.
parse_type_specifier(Term, Result) :-
	parse_symbol_name_specifier(Term, Result).

%	An ADT_Specifier is just a symbol name specifier.

:- pred parse_adt_specifier(term, maybe1(sym_name_specifier)).
:- mode parse_adt_specifier(in, out) is det.
parse_adt_specifier(Term, Result) :-
	parse_symbol_name_specifier(Term, Result).

%-----------------------------------------------------------------------------%

%	For the moment, an OpSpecifier is just a symbol name specifier.
% 	XXX We should allow specifying the fixity of an operator

:- pred parse_op_specifier(term, maybe1(op_specifier)).
:- mode parse_op_specifier(in, out) is det.
parse_op_specifier(Term, Result) :-
	parse_symbol_name_specifier(Term, R),
	process_maybe1(make_op_specifier, R, Result).

:- pred make_op_specifier(sym_name_specifier::in, op_specifier::out) is det.
make_op_specifier(X, sym(X)).

%-----------------------------------------------------------------------------%

	% types are represented just as ordinary terms

:- pred parse_type(term, maybe1(type)).
:- mode parse_type(in, out) is det.
parse_type(T, ok(T)).

:- pred convert_type_list(list(term), list(type)).
:- mode convert_type_list(in, out) is det.
convert_type_list([], []).
convert_type_list([H0|T0], [H|T]) :-
	convert_type(H0, H),
	convert_type_list(T0, T).

:- pred convert_type(term, type).
:- mode convert_type(in, out) is det.
convert_type(T, T).

%-----------------------------------------------------------------------------%

report_warning(Message) -->
	io__stderr_stream(StdErr),
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
	( { HaltAtWarn = yes } ->
		io__set_exit_status(1)
	;
		[]
	),
	io__write_string(StdErr, Message).

report_warning(Stream, Message) -->
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
	( { HaltAtWarn = yes } ->
		io__set_exit_status(1)
	;
		[]
	),
	io__write_string(Stream, Message).

report_warning(ModuleName, LineNum, Message) -->
	{ string__format("%s.m:%3d: Warning: %s\n",
		[s(ModuleName), i(LineNum), s(Message)], FullMessage) },
	io__stderr_stream(StdErr),
	io__write_string(StdErr, FullMessage),
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
	( { HaltAtWarn = yes } ->
		io__set_exit_status(1)
	;
		[]
	).

%-----------------------------------------------------------------------------%
