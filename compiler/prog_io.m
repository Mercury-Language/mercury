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
:- import_module list, io.

%-----------------------------------------------------------------------------%

% This module (prog_io) exports the following predicates:

	% prog_io__read_module(FileName, ModuleName, Search, Error,
	%					Messages, Program)
	% Reads and parses the module 'ModuleName'.
	% If Search is yes, search directories given by the option
	% search_directories.
	% Error is `fatal' if the file coudn't be opened, `yes'
	% if a syntax error was detected, and `no' otherwise.
	% Messages is a list of warning/error messages.
	% Program is the parse tree.

:- type module_error
	--->	no	% no errors
	;	yes	% some syntax errors
	;	fatal.	% couldn't open the file

:- pred prog_io__read_module(string, string, bool, module_error,
	message_list, item_list, io__state, io__state).
:- mode prog_io__read_module(in, in, in, out, out, out, di, uo) is det.

	% Same as prog_io__read_module, but use intermod_directories
	% instead of search_directories when searching for the file.
:- pred prog_io__read_opt_file(string, string, bool, module_error,
	message_list, item_list, io__state, io__state).
:- mode prog_io__read_opt_file(in, in, in, out, out, out, di, uo) is det.

	% search_for_file(Dirs, FileName, Found, IO0, IO)
	%
	% Search Dirs for FileName, opening the file if it is found.
:- pred search_for_file(list(string), string, bool, io__state, io__state).
:- mode search_for_file(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_io_goal, prog_io_dcg, prog_io_pragma, prog_io_util.
:- import_module hlds_data, hlds_pred, prog_util, globals, options.
:- import_module bool, int, string, std_util, parser, term_io, dir, require.
:- import_module varset, term.

%-----------------------------------------------------------------------------%

prog_io__read_module(FileName, ModuleName, Search, Error, Messages, Items) -->
	prog_io__read_module_2(FileName, ModuleName, Search,
		search_directories, Error, Messages, Items).

prog_io__read_opt_file(FileName, ModuleName, Search, 
		Error, Messages, Items) -->
	prog_io__read_module_2(FileName, ModuleName, Search, 
		intermod_directories, Error, Messages, Items).
		
% This implementation uses io__read_term to read in the program
% term at a time, and then converts those terms into clauses and
% declarations, checking for errors as it goes.
% Note that rather than using difference lists, we just
% build up the lists of items and messages in reverse order
% and then reverse them afterwards.  (Using difference lists would require
% late-input modes.)

:- pred prog_io__read_module_2(string, string, bool, option, module_error,
	message_list, item_list, io__state, io__state).
:- mode prog_io__read_module_2(in, in, in, in, out, out, out, di, uo) is det.

prog_io__read_module_2(FileName, ModuleName, Search,
		SearchOpt, Error, Messages, Items) -->
	( 
		{ Search = yes }
	->
		globals__io_lookup_accumulating_option(SearchOpt, 
			Dirs)
	;
		{ dir__this_directory(CurrentDir) },
		{ Dirs = [CurrentDir] }
	),
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
        ThisError = "Warning: module should start with a `:- module' declaration"
		- Term2,
        Messages = [ThisError | Messages0],
	Items = Items0,
	Error = Error0
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
	io__input_stream(Stream),
	io__input_stream_name(Stream, SourceFileName),
	read_items_loop(ModuleName, SourceFileName, [], [], no,
			Messages, Items, Error).

%-----------------------------------------------------------------------------%

	% The code below was carefully optimized to run efficiently
	% in NU-Prolog.  We used to call read_item(MaybeItem) -
	% which does all the work for a single item -
	% via io__gc_call/1, which called the goal with garbage collection.
	% But optimizing for NU-Prolog is no longer a big priority...

:- pred read_items_loop(string, string, message_list, item_list, module_error, 
			message_list, item_list, module_error, 
			io__state, io__state).
:- mode read_items_loop(in, in, in, in, in, out, out, out, di, uo) is det.

read_items_loop(ModuleName, SourceFileName, Msgs1, Items1, Error1,
		Msgs, Items, Error) -->
	read_item(ModuleName, SourceFileName, MaybeItem),
 	read_items_loop_2(MaybeItem, ModuleName, SourceFileName,
			Msgs1, Items1, Error1, Msgs, Items, Error).

%-----------------------------------------------------------------------------%

:- pred read_items_loop_2(maybe_item_or_eof, string, string,
			message_list, item_list, module_error,
			message_list, item_list, module_error,
			io__state, io__state).
:- mode read_items_loop_2(in, in, in, in, in, in, out, out, out, di, uo) is det.

% do a switch on the type of the next item

read_items_loop_2(eof, _ModuleName, _SourceFileName, Msgs, Items, Error,
		Msgs, Items, Error) --> []. 
	% if the next item was end-of-file, then we're done.

read_items_loop_2(syntax_error(ErrorMsg, LineNumber), ModuleName,
		SourceFileName, Msgs0, Items0, _Error0, Msgs, Items, Error) -->
	% if the next item was a syntax error, then insert it in
	% the list of messages and continue looping
	{
	  term__context_init(SourceFileName, LineNumber, Context),
	  dummy_term_with_context(Context, Term),
	  ThisError = ErrorMsg - Term,
	  Msgs1 = [ThisError | Msgs0],
	  Items1 = Items0,
	  Error1 = yes
	},
	read_items_loop(ModuleName, SourceFileName, Msgs1, Items1, Error1,
		Msgs, Items, Error).

read_items_loop_2(error(M, T), ModuleName, SourceFileName,
		Msgs0, Items0, _Error0, Msgs, Items, Error) -->
	% if the next item was a semantic error, then insert it in
	% the list of messages and continue looping
	{
	  add_error(M, T, Msgs0, Msgs1),
	  Items1 = Items0,
	  Error1 = yes
	},
 	read_items_loop(ModuleName, SourceFileName, Msgs1, Items1, Error1,
			Msgs, Items, Error).

read_items_loop_2(ok(Item, Context), ModuleName, SourceFileName0,
			Msgs0, Items0, Error0, Msgs, Items, Error) -->
	% if the next item was a valid item, check whether it was
	% a `pragma source_file' declaration.  If so, set the new
	% source file name, and consume that item, otherwise insert
	% the item in the item list.  Then continue looping.
	{ Item = pragma(source_file(NewSourceFileName)) ->
		SourceFileName = NewSourceFileName,
		Items1 = Items0
	;
		SourceFileName = SourceFileName0,
		Items1 = [Item - Context | Items0]
	},
 	read_items_loop(ModuleName, SourceFileName, Msgs0, Items1, Error0,
			Msgs, Items, Error).

%-----------------------------------------------------------------------------%

	% read_item/1 reads a single item, and if it is a valid term
	% parses it.

:- type maybe_item_or_eof --->	eof
			;	syntax_error(string, int)
			;	error(string, term)
			;	ok(item, term__context).

:- pred read_item(string, string, maybe_item_or_eof, io__state, io__state).
:- mode read_item(in, in, out, di, uo) is det.

read_item(ModuleName, SourceFileName, MaybeItem) -->
	parser__read_term(SourceFileName, MaybeTerm),
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
			Result = error("unrecognized declaration", F)
		)
	;
		Result = error("atom expected after `:-'", F)
	).

	% process_decl(VarSet, Atom, Args, Result) succeeds if Atom(Args)
	% is a declaration and binds Result to a representation of that
	% declaration.
:- pred process_decl(string, varset, string, list(term), maybe1(item)).
:- mode process_decl(in, in, in, in, out) is semidet.

process_decl(ModuleName, VarSet, "type", [TypeDecl], Result) :-
	parse_type_decl(ModuleName, VarSet, TypeDecl, Result).

process_decl(ModuleName, VarSet, "pred", [PredDecl], Result) :-
	parse_type_decl_pred(ModuleName, VarSet, PredDecl, Result).

process_decl(ModuleName, VarSet, "func", [FuncDecl], Result) :-
	parse_type_decl_func(ModuleName, VarSet, FuncDecl, Result).

process_decl(ModuleName, VarSet, "mode", [ModeDecl], Result) :-
	parse_mode_decl(ModuleName, VarSet, ModeDecl, Result).

process_decl(ModuleName, VarSet, "inst", [InstDecl], Result) :-
	parse_inst_decl(ModuleName, VarSet, InstDecl, Result).

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
		Result = error("module names starting with capital letters must be quoted using single quotes (e.g. "":- module 'Foo'."")", ErrorContext)
	;
		Result = error("module name expected", ModuleName)
	).

process_decl(_ModuleName0, VarSet, "end_module", [ModuleName], Result) :-
	(
		ModuleName = term__functor(term__atom(Module), [], _Context)
	->
		Result = ok(module_defn(VarSet, end_module(Module)))
	;
		Result = error("module name expected", ModuleName)
	).

	% NU-Prolog `when' declarations are silently ignored for
	% backwards compatibility.
process_decl(_ModuleName, _VarSet, "when", [_Goal, _Cond], Result) :-
	Result = ok(nothing).

process_decl(ModuleName, VarSet, "pragma", Pragma, Result):-
	parse_pragma(ModuleName, VarSet, Pragma, Result).

:- pred parse_type_decl(string, varset, term, maybe1(item)).
:- mode parse_type_decl(in, in, in, out) is det.
parse_type_decl(ModuleName, VarSet, TypeDecl, Result) :-
	( 
		TypeDecl = term__functor(term__atom(Name), Args, _),
		parse_type_decl_type(ModuleName, Name, Args, Cond, R) 
	->
		R1 = R,
		Cond1 = Cond
	;
		process_abstract_type(ModuleName, TypeDecl, R1),
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

:- pred parse_type_decl_type(string, string, list(term),
				condition, maybe1(type_defn)).
:- mode parse_type_decl_type(in, in, in, out, out) is semidet.

:- parse_type_decl_type(_, [A|B], _, _, _) when A and B.

parse_type_decl_type(ModuleName, "--->", [H, B], Condition, R) :-
	get_condition(B, Body, Condition),
	process_du_type(ModuleName, H, Body, R).

parse_type_decl_type(ModuleName, "=", [H, B], Condition, R) :-
	get_condition(B, Body, Condition),
	process_uu_type(ModuleName, H, Body, R).

parse_type_decl_type(ModuleName, "==", [H, B], Condition, R) :-
	get_condition(B, Body, Condition),
	process_eqv_type(ModuleName, H, Body, R).

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
:- pred process_uu_type(string, term, term, maybe1(type_defn)).
:- mode process_uu_type(in, in, in, out) is det.
process_uu_type(ModuleName, Head, Body, Result) :-
	check_for_errors(ModuleName, Head, Body, Result0),
	process_uu_type_2(Result0, Body, Result).

:- pred process_uu_type_2(maybe_functor, term, maybe1(type_defn)).
:- mode process_uu_type_2(in, in, out) is det.
process_uu_type_2(error(Error, Term), _, error(Error, Term)).
process_uu_type_2(ok(Name, Args), Body, ok(uu_type(Name, Args, List))) :-
		sum_to_list(Body, List).

%-----------------------------------------------------------------------------%

	% This is for "Head == Body" (equivalence) definitions.
:- pred process_eqv_type(string, term, term, maybe1(type_defn)).
:- mode process_eqv_type(in, in, in, out) is det.
process_eqv_type(ModuleName, Head, Body, Result) :-
	check_for_errors(ModuleName, Head, Body, Result0),
	process_eqv_type_2(Result0, Body, Result).

:- pred process_eqv_type_2(maybe_functor, term, maybe1(type_defn)).
:- mode process_eqv_type_2(in, in, out) is det.
process_eqv_type_2(error(Error, Term), _, error(Error, Term)).
process_eqv_type_2(ok(Name, Args), Body, ok(eqv_type(Name, Args, Body))).

%-----------------------------------------------------------------------------%

	% process_du_type(ModuleName, TypeHead, TypeBody, Result)
	% checks that its arguments are well formed, and if they are,
	% binds Result to a representation of the type information about the
	% TypeHead.
	% This is for "Head ---> Body" (constructor) definitions.
:- pred process_du_type(string, term, term, maybe1(type_defn)).
:- mode process_du_type(in, in, in, out) is det.
process_du_type(ModuleName, Head, Body, Result) :-
	check_for_errors(ModuleName, Head, Body, Result0),
	process_du_type_2(ModuleName, Result0, Body, Result).

:- pred process_du_type_2(string, maybe_functor, term, maybe1(type_defn)).
:- mode process_du_type_2(in, in, in, out) is det.
process_du_type_2(_, error(Error, Term), _, error(Error, Term)).
process_du_type_2(ModuleName, ok(Functor, Args), Body, Result) :-
	% check that body is a disjunction of constructors
	( %%% some [Constrs] 
		convert_constructors(ModuleName, Body, Constrs)
	->
		Result = ok(du_type(Functor, Args, Constrs))
	;
		Result = error("invalid RHS of type definition", Body)
	).

%-----------------------------------------------------------------------------%

	% process_abstract_type(ModuleName, TypeHead, Result)
	% checks that its argument is well formed, and if it is,
	% binds Result to a representation of the type information about the
	% TypeHead.

:- pred process_abstract_type(string, term, maybe1(type_defn)).
:- mode process_abstract_type(in, in, out) is det.
process_abstract_type(ModuleName, Head, Result) :-
	dummy_term(Body),
	check_for_errors(ModuleName, Head, Body, Result0),
	process_abstract_type_2(Result0, Result).

:- pred process_abstract_type_2(maybe_functor, maybe1(type_defn)).
:- mode process_abstract_type_2(in, out) is det.
process_abstract_type_2(error(Error, Term), error(Error, Term)).
process_abstract_type_2(ok(Functor, Args), ok(abstract_type(Functor, Args))).

%-----------------------------------------------------------------------------%

	%  check a type definition for errors

:- pred check_for_errors(string, term, term, maybe_functor).
:- mode check_for_errors(in, in, in, out) is det.
check_for_errors(ModuleName, Head, Body, Result) :-
	( Head = term__variable(_) ->
		Result = error("variable on LHS of type definition", Head)
	;
		parse_qualified_term(ModuleName, Head, "type definition", R),
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
		Result = error("type parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
	  %%%	some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error("repeated type parameters in LHS of type defn", Head)
	% check that all the variables in the body occur in the head
	; %%% some [Var2]
		(
			term__contains_var(Body, Var2),
			\+ term__contains_var_list(Args, Var2)
		)
	->
		Result = error("free type parameter in RHS of type definition",
				Body)
	;
		Result = ok(Name, Args)
	).

%-----------------------------------------------------------------------------%

	% Convert a list of terms separated by semi-colons
	% (known as a "disjunction", even thought the terms aren't goals
	% in this case) into a list of constructors

:- pred convert_constructors(string, term, list(constructor)).
:- mode convert_constructors(in, in, out) is semidet.
convert_constructors(ModuleName, Body, Constrs) :-
	disjunction_to_list(Body, List),
	convert_constructors_2(ModuleName, List, Constrs).

	% true if input argument is a valid list of constructors

:- pred convert_constructors_2(string, list(term), list(constructor)).
:- mode convert_constructors_2(in, in, out) is semidet.
convert_constructors_2(_, [], []).
convert_constructors_2(ModuleName, [Term | Terms], [Constr | Constrs]) :-
	convert_constructor(ModuleName, Term, Constr),
	convert_constructors_2(ModuleName, Terms, Constrs).

	% true if input argument is a valid constructor.
	% Note that as a special case, one level of
	% curly braces around the constructor are ignored.
	% This is to allow you to define ';'/2 constructors.

:- pred convert_constructor(string, term, constructor).
:- mode convert_constructor(in, in, out) is semidet.
convert_constructor(ModuleName, Term, Result) :-
	( 
		Term = term__functor(term__atom("{}"), [Term1], _Context)
	->
		Term2 = Term1
	;
		Term2 = Term
	),
	parse_qualified_term(ModuleName, Term2,
		"convert_constructor/2", ok(F, As)),
	convert_constructor_arg_list(As, Args),
	Result = F - Args.

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
		(
			verify_type_and_mode_list(As)
		->
			Result = ok(pred(VarSet, F, As, MaybeDet, Cond))
		;
			Result = error("some but not all arguments have modes", PredType)
		)
	;
		Result = error("syntax error in `:- pred' declaration",
				PredType)
	).
process_pred_2(error(M, T), _, _, _, _, error(M, T)).

%-----------------------------------------------------------------------------%

	% Verify that among the arguments of a :- pred declaration,
	% either all arguments specify a mode or none of them do.

:- pred verify_type_and_mode_list(list(type_and_mode)).
:- mode verify_type_and_mode_list(in) is semidet.

verify_type_and_mode_list([]).
verify_type_and_mode_list([First | Rest]) :-
	verify_type_and_mode_list_2(Rest, First).

:- pred verify_type_and_mode_list_2(list(type_and_mode), type_and_mode).
:- mode verify_type_and_mode_list_2(in, in) is semidet.

verify_type_and_mode_list_2([], _).
verify_type_and_mode_list_2([Head | Tail], First) :-
	(
		Head = type_only(_),
		First = type_only(_)
	;
		Head = type_and_mode(_, _),
		First = type_and_mode(_, _)
	),
	verify_type_and_mode_list_2(Tail, First).

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
			"syntax error in return type of `:- func' declaration",
					ReturnTypeTerm)
		)
	;
		Result = error(
			"syntax error in arguments of `:- func' declaration",
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
		Result = error("syntax error in predicate mode declaration",
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
		"syntax error in return mode of function mode declaration",
					RetMode0)
		)
	;
		Result = error(
		"syntax error in arguments of function mode declaration",
				FuncMode)
	).
process_func_mode(error(M, T), _, _, _, _, _, error(M, T)).

%-----------------------------------------------------------------------------%

	% Parse a `:- inst <InstDefn>.' declaration.
	%
:- pred parse_inst_decl(string, varset, term, maybe1(item)).
:- mode parse_inst_decl(in, in, in, out) is det.
parse_inst_decl(ModuleName, VarSet, InstDefn, Result) :-
	(
		InstDefn = term__functor(term__atom(Op), [H, B], _Context),
		( Op = "=" ; Op = "==" )
	->
		get_condition(B, Body, Condition),
		convert_inst_defn(ModuleName, H, Body, R),
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
		convert_abstract_inst_defn(ModuleName, Head, R),
		process_maybe1(make_inst_defn(VarSet, Condition), R, Result)
	;
		InstDefn = term__functor(term__atom("--->"), [H, B], Context)
	->
		get_condition(B, Body, Condition),
		Body1 = term__functor(term__atom("bound"), [Body], Context),
		convert_inst_defn(ModuleName, H, Body1, R),
		process_maybe1(make_inst_defn(VarSet, Condition), R, Result)
	;
		Result = error("`=' expected in `:- inst' definition", InstDefn)
	).
		% we should check the condition for errs
		% (don't bother at the moment, since we ignore
		% conditions anyhow :-)

	% Parse a `:- inst <Head> ---> <Body>.' definition.
	%
:- pred convert_inst_defn(string, term, term, maybe1(inst_defn)).
:- mode convert_inst_defn(in, in, in, out) is det.
convert_inst_defn(ModuleName, Head, Body, Result) :-
	parse_qualified_term(ModuleName, Head, "inst definition", R),
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
		Result = error("inst parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
	%%%	some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error("repeated inst parameters in LHS of inst defn",
				Head)
	;
	% check that all the variables in the body occur in the head
	%%%	some [Var2]
		(
			term__contains_var(Body, Var2),
			\+ term__contains_var_list(Args, Var2)
		)
	->
		Result = error("free inst parameter in RHS of inst definition",
				Body)
	;
	% check that the inst is a valid user-defined inst, i.e. that
	% it does not have the form of one of the builtin insts
		\+ (
			convert_inst(Head, UserInst),
			UserInst = defined_inst(user_inst(_, _))
		)
	->
		Result = error("attempt to redefine builtin inst", Head)
	;
		% should improve the error message here

		( %%% some [ConvertedBody]
			convert_inst(Body, ConvertedBody)
		->
			Result = ok(eqv_inst(Name, Args, ConvertedBody))
		;
			Result = error("syntax error in inst body", Body)
		)
	).

:- pred convert_abstract_inst_defn(string, term, maybe1(inst_defn)).
:- mode convert_abstract_inst_defn(in, in, out) is det.
convert_abstract_inst_defn(ModuleName, Head, Result) :-
	parse_qualified_term(ModuleName, Head, "inst definition", R),
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
		Result = error("inst parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
	%%%	some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error(
			"repeated inst parameters in abstract inst definition",
				Head)
	;
		Result = ok(abstract_inst(Name, Args))
	).

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
		convert_mode_defn(ModuleName, H, Body, R),
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

:- pred convert_mode_defn(string, term, term, maybe1(mode_defn)).
:- mode convert_mode_defn(in, in, in, out) is det.
convert_mode_defn(ModuleName, Head, Body, Result) :-
	parse_qualified_term(ModuleName, Head, "mode definition", R),
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
		Result = error("mode parameters must be variables", Head)
	;
	% check that all the head arg variables are distinct
		%%% some [Arg2, OtherArgs]
		(
			list__member(Arg2, Args, [Arg2|OtherArgs]),
			list__member(Arg2, OtherArgs)
		)
	->
		Result = error("repeated parameters in LHS of mode defn",
				Head)
	% check that all the variables in the body occur in the head
	; %%% some [Var2]
		(
			term__contains_var(Body, Var2),
			\+ term__contains_var_list(Args, Var2)
		)
	->
		Result = error("free inst parameter in RHS of mode definition",
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
			Result = error("syntax error in mode definition body",
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
		Result = error("module specifier should be an identifier", Term)
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
		Result = error("arity in symbol name specifier must be a non-negative integer", Term)
	    )
        ;
	    Result = error("arity in symbol name specifier must be an integer", Term)
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

%	A SymbolName is one of
%		Name
%			Matches symbols with the specified name in the
%			current namespace.
%		Module:Name
%			Matches symbols with the specified name exported
%			by the specified module.
%
%	We [will one day] also allow the syntax `Module__Name'
%	as an alternative for `Module:Name'.

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
		Result = error("module name identifier expected before ':' in qualified symbol name", Term)
            )
        ;
            Result = error("identifier expected after ':' in qualified symbol name", Term)
	)
    ;
        ( 
            Term = term__functor(term__atom(Name), [], _Context3)
        ->
	    (
		string__sub_string_search(Name, "__", LeftLength),
		LeftLength > 0
	    ->
		string__left(Name, LeftLength, Module),
		string__length(Name, NameLength),
		RightLength is NameLength - LeftLength - 2,
		string__right(Name, RightLength, Name2),
		Result = ok(qualified(Module, Name2))
	    ;
	        (
		    DefaultModName = ""
	        ->
		    Result = ok(unqualified(Name))
	        ;
		    Result = ok(qualified(DefaultModName, Name))
	        )
	    )
        ;
            Result = error("symbol name specifier expected", Term)
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

:- pred convert_constructor_arg_list(list(term), list(constructor_arg)).
:- mode convert_constructor_arg_list(in, out) is det.

convert_constructor_arg_list([], []).
convert_constructor_arg_list([Term | Terms], [Arg | Args]) :-
	(
		Term = term__functor(term__atom("::"), [NameTerm, TypeTerm], _),
		NameTerm = term__functor(term__atom(Name), [], _)
	->
		convert_type(TypeTerm, Type),
		Arg = Name - Type
	;
		convert_type(Term, Type),
		Arg = "" - Type
	),
	convert_constructor_arg_list(Terms, Args).

:- pred convert_type(term, type).
:- mode convert_type(in, out) is det.
convert_type(T, T).

%-----------------------------------------------------------------------------%
