%---------------------------------------------------------------------------%
% Copyright (C) 1995-2001, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: parser.m.
% main author: fjh.
% stability: high.
%
% This file exports the predicate parser__read_term, which reads
% a term from the current input stream.
% The parser__read_term_from_string predicates are the same as the
% read_term predicates, except that the term is read from
% a string rather than from the current input stream.
% The parser__parse_token_list predicate is similar,
% but it takes a list of tokens rather than a string.
%
% The parser and lexer are intended to exactly follow ISO Prolog
% syntax, but there are some departures from that for three reasons:
%
%	(1) I wrote some of the code at home when the ISO Prolog draft
%	    was at uni - so in some places I just guessed.
%	(2) In some places the lexer reports an error when it shouldn't.
%	(3) There are a couple of hacks to make it compatible with NU-Prolog
%	    syntax.
%
% The parser is a relatively straight-forward top-down recursive descent
% parser, made somewhat complicated by the need to handle operator
% precedences.  It uses `lexer__get_token_list' to read a list of tokens.
% It uses the routines in module `ops' to look up operator precedences.
%
%-----------------------------------------------------------------------------%

:- module parser.
:- interface.
:- import_module io, ops, term_io, lexer.

%-----------------------------------------------------------------------------%
%
% parser__read_term/{3,4}:
%	Reads in (and parses) terms from the current input stream.

:- pred parser__read_term(read_term(T), io__state, io__state).
:- mode parser__read_term(out, di, uo) is det.
% parser__read_term(Result):
%	Reads a Mercury term from the current input stream.

:- pred parser__read_term_with_op_table(Ops, read_term(T),
		io__state, io__state) <= op_table(Ops).
:- mode parser__read_term_with_op_table(in, out, di, uo) is det.
% parser__read_term_with_op_table(Result):
%	Reads a term from the current input stream, using the
%	given op_table to interpret the operators.

:- pred parser__read_term(string, read_term(T), io__state, io__state).
:- mode parser__read_term(in, out, di, uo) is det.
% parser__read_term(FileName, Result):
%	Reads a term from the current input stream.
%	The string is the filename to use for the current input stream;
%	this is used in constructing the term__contexts in the read term.
%	This interface is used to support the `:- pragma source_file'
%	directive.

:- pred parser__read_term_with_op_table(Ops, string, read_term(T),
		io__state, io__state) <= op_table(Ops).
:- mode parser__read_term_with_op_table(in, in, out, di, uo) is det.
% parser__read_term_with_op_table(Ops, FileName, Result):
%	As above but using the given op_table.

%-----------------------------------------------------------------------------%
%
% parser__read_term_from_string/{4,6}:
%	Parses terms from a string.

	% The read_term_from_string predicates are the same as the
	% read_term predicates, except that the term is read from
	% a string rather than from the current input stream.
	% The returned value `EndPos' is the position one character
	% past the end of the term read.
	% The arguments `MaxOffset' and `StartPos' in the six-argument version
	% specify the length of the string and the position within the
	% string at which to start parsing.

:- pred parser__read_term_from_string(string, string, posn, read_term(T)).
:- mode parser__read_term_from_string(in, in, out, out) is det.
%	parser__read_term_from_string(FileName, String, EndPos, Term).

:- pred parser__read_term_from_string_with_op_table(Ops, string,
		string, posn, read_term(T)) <= op_table(Ops).
:- mode parser__read_term_from_string_with_op_table(in, in,
		in, out, out) is det.
%	parser__read_term_from_string_with_op_table(Ops, FileName,
%		String, EndPos, Term).

:- pred parser__read_term_from_string(string, string, int, posn, posn,
					read_term(T)).
:- mode parser__read_term_from_string(in, in, in, in, out, out) is det.
%	parser__read_term_from_string(FileName, String, MaxOffset, StartPos,
%					EndPos, Term).

:- pred parser__read_term_from_string_with_op_table(Ops, string, string,
		int, posn, posn, read_term(T)) <= op_table(Ops).
:- mode parser__read_term_from_string_with_op_table(in, in, in,
		in, in, out, out) is det.
%	parser__read_term_from_string_with_op_table(Ops, FileName, String,
%		MaxOffset, StartPos, EndPos, Term).

%-----------------------------------------------------------------------------%
%
% parser__parse_tokens/{3,4}:
%	Parses a list of tokens.

:- pred parser__parse_tokens(string, token_list, read_term(T)).
:- mode parser__parse_tokens(in, in, out) is det.
	% parser__parse_tokens(FileName, TokenList, Result):

:- pred parser__parse_tokens_with_op_table(Ops, string,
		token_list, read_term(T)) <= op_table(Ops).
:- mode parser__parse_tokens_with_op_table(in, in, in, out) is det.
	% parser__parse_tokens(FileName, TokenList, Result):

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string, char, int, float, bool, list, std_util, require.
:- import_module map, term, varset.
:- import_module lexer.

:- type parse(T)
	--->	ok(T)
	;	error(string, token_list).

	% Are we parsing an ordinary term, an argument or a list element?
:- type term_kind
	--->	ordinary_term
	;	argument
	;	list_elem.

%-----------------------------------------------------------------------------%

parser__read_term(Result) -->
	io__input_stream_name(FileName),
	parser__read_term_with_op_table(ops__init_mercury_op_table,
		FileName, Result).

parser__read_term_with_op_table(Ops, Result) -->
	io__input_stream_name(FileName),
	parser__read_term_with_op_table(Ops, FileName, Result).

parser__read_term(FileName, Result) -->
	parser__read_term_with_op_table(ops__init_mercury_op_table,
		FileName, Result).

parser__read_term_with_op_table(Ops, FileName, Result) -->
	lexer__get_token_list(Tokens),
	{ parser__parse_tokens_with_op_table(Ops, FileName, Tokens, Result) }.

parser__read_term_from_string(FileName, String, EndPos, Result) :-
	parser__read_term_from_string_with_op_table(ops__init_mercury_op_table,
		FileName, String, EndPos, Result).

parser__read_term_from_string_with_op_table(Ops, FileName, String,
		EndPos, Result) :-
	string__length(String, Len),
	StartPos = posn(1, 0, 0),
	parser__read_term_from_string_with_op_table(Ops, FileName, String, Len,
		StartPos, EndPos, Result).

parser__read_term_from_string(FileName, String, Len,
		StartPos, EndPos, Result) :-
	parser__read_term_from_string_with_op_table(ops__init_mercury_op_table,
		FileName, String, Len, StartPos, EndPos, Result).

parser__read_term_from_string_with_op_table(Ops, FileName, String, Len,
		StartPos, EndPos, Result) :-
	lexer__string_get_token_list(String, Len, Tokens, StartPos, EndPos),
	parser__parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

%-----------------------------------------------------------------------------%

parser__parse_tokens(FileName, Tokens, Result) :-
	parser__parse_tokens_with_op_table(ops__init_mercury_op_table,
		FileName, Tokens, Result).

parser__parse_tokens_with_op_table(Ops, FileName, Tokens, Result) :-
	( Tokens = token_nil ->
		Result = eof
	;
		parser__init_state(Ops, FileName, Tokens, ParserState0),
		parser__parse_whole_term(Term, ParserState0, ParserState),
		parser__final_state(ParserState, VarSet, LeftOverTokens),
		parser__check_for_errors(Term, VarSet,
			Tokens, LeftOverTokens, Result)
	).

:- pred parser__check_for_errors(parse(term(T)), varset(T),
		token_list, token_list, read_term(T)).
:- mode parser__check_for_errors(in, in, in, in, out) is det.

parser__check_for_errors(error(ErrorMessage, ErrorTokens), _VarSet, Tokens,
		_LeftOverTokens, Result) :-
	% check if the error was caused by a bad token
	(
		parser__check_for_bad_token(Tokens,
			BadTokenMessage, BadTokenLineNum)
	->
		Message = BadTokenMessage,
		LineNum = BadTokenLineNum
	;
		% find the token that caused the error
		(
			ErrorTokens = token_cons(ErrorTok, ErrorTokLineNum, _)
		->
			lexer__token_to_string(ErrorTok, TokString),
			string__append_list( ["Syntax error at ", TokString,
						": ", ErrorMessage], Message),
			LineNum = ErrorTokLineNum
		;
			(
				Tokens = token_cons(_, FirstTokLineNum, _)
			->
				LineNum = FirstTokLineNum
			;
				error("parser__check_for_errors")
			),
			string__append("Syntax error: ", ErrorMessage, Message)
		)
	),
	Result = error(Message, LineNum).

parser__check_for_errors(ok(Term), VarSet, Tokens, LeftOverTokens, Result) :-
	(
		parser__check_for_bad_token(Tokens, Message, LineNum)
	->
		Result = error(Message, LineNum)
	;
		LeftOverTokens = token_cons(Token, LineNum, _)
	->
		lexer__token_to_string(Token, TokString),
		string__append("Syntax error: unexpected ", TokString,
			Message),
		Result = error(Message, LineNum)
	;
		Result = term(VarSet, Term)
	).

:- pred parser__check_for_bad_token(token_list, string, int).
:- mode parser__check_for_bad_token(in, out, out) is semidet.

parser__check_for_bad_token(token_cons(Token, LineNum, Tokens),
		Message, LineNum) :-
	( Token = io_error(IO_Error) ->
		io__error_message(IO_Error, IO_ErrorMessage),
		string__append("I/O error: ", IO_ErrorMessage, Message)
	; Token = junk(Char) ->
		char__to_int(Char, Code),
		string__int_to_base_string(Code, 10, Decimal),
		string__int_to_base_string(Code, 16, Hex),
		string__append_list(["Syntax error: Illegal character 0x",
			Hex, " (", Decimal, ") in input"], Message)
	; Token = error(ErrorMessage) ->
		string__append("Syntax error: ", ErrorMessage, Message)
	;
		parser__check_for_bad_token(Tokens, Message, LineNum)
	).

:- pred parser__parse_whole_term(parse(term(T)), parser__state(Ops, T),
		parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_whole_term(out, in, out) is det.

parser__parse_whole_term(Term) -->
	parser__parse_term(Term0),
	( { Term0 = ok(_) } ->
		( parser__get_token(end) ->
			{ Term = Term0 }
		;
			parser__unexpected("operator or `.' expected", Term)
		)
	;
		% propagate error upwards
		{ Term = Term0 }
	).



:- pred parser__parse_term(parse(term(T)), parser__state(Ops, T),
		parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_term(out, in, out) is det.

parser__parse_term(Term) -->
	parser__get_ops_table(OpTable),
	parser__parse_term_2(ops__max_priority(OpTable) + 1, ordinary_term,
		Term).

:- pred parser__parse_arg(parse(term(T)), parser__state(Ops, T),
		parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_arg(out, in, out) is det.

parser__parse_arg(Term) -->
	parser__get_ops_table(OpTable),
	parser__parse_term_2(ops__arg_priority(OpTable), argument, Term).


:- pred parser__parse_list_elem(parse(term(T)), parser__state(Ops, T),
		parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_list_elem(out, in, out) is det.

parser__parse_list_elem(Term) -->
	parser__get_ops_table(OpTable),
	parser__parse_term_2(ops__arg_priority(OpTable), list_elem, Term).

:- pred parser__parse_term_2(int, term_kind, parse(term(T)),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_term_2(in, in, out, in, out) is det.

parser__parse_term_2(MaxPriority, TermKind, Term) -->
	parser__parse_left_term(MaxPriority, TermKind, LeftPriority, LeftTerm0),
	( { LeftTerm0 = ok(LeftTerm) } ->
		parser__parse_rest(MaxPriority, TermKind, LeftPriority,
			LeftTerm, Term)
	;
		% propagate error upwards
		{ Term = LeftTerm0 }
	).

:- pred parser__parse_left_term(int, term_kind, int, parse(term(T)),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_left_term(in, in, out, out, in, out) is det.

parser__parse_left_term(MaxPriority, TermKind, OpPriority, Term) -->
	( parser__get_token(Token, Context) ->
		(
			% check for unary minus of integer
			{ Token = name("-") },
			parser__get_token(integer(X), _IntContext)
		->
			parser__get_term_context(Context, TermContext),
			{ NegX = 0 - X },
			{ Term = ok(term__functor(term__integer(NegX), [],
						TermContext)) },
			{ OpPriority = 0 }
		;
			% check for unary minus of float
			{ Token = name("-") },
			parser__get_token(float(F), _FloatContext)
		->
			parser__get_term_context(Context, TermContext),
			{ NegF = 0.0 - F },
			{ Term = ok(term__functor(term__float(NegF), [],
				TermContext)) },
			{ OpPriority = 0 }
		;
			% check for binary prefix op
			{ Token = name(Op) },
			\+ parser__peek_token(open_ct),
			parser__get_ops_table(OpTable),
			{ ops__lookup_binary_prefix_op(OpTable, Op,
				BinOpPriority, RightAssoc, RightRightAssoc) },
			{ BinOpPriority =< MaxPriority },
			parser__peek_token(NextToken),
			{ parser__could_start_term(NextToken, yes) }
		->
			{ parser__adjust_priority(RightAssoc, BinOpPriority,
							RightPriority) },
			{ parser__adjust_priority(RightRightAssoc,
					BinOpPriority, RightRightPriority) },
			{ OpPriority = BinOpPriority },
			parser__parse_term_2(RightPriority, TermKind,
				RightResult),
			( { RightResult = ok(RightTerm) } ->
				parser__parse_term_2(RightRightPriority,
					TermKind, RightRightResult),
				( { RightRightResult = ok(RightRightTerm) } ->
					parser__get_term_context(Context,
						TermContext),
					{ Term = ok(term__functor(
						term__atom(Op),
						[RightTerm, RightRightTerm],
						TermContext)) }
				;
					% propagate error upwards
					{ Term = RightRightResult }
				)
			;
				% propagate error upwards
				{ Term = RightResult }
			)
		;
			% check for unary prefix op
			{ Token = name(Op) },
			\+ parser__peek_token(open_ct),
			parser__get_ops_table(OpTable),
			{ ops__lookup_prefix_op(OpTable, Op, UnOpPriority,
							RightAssoc) },
			{ UnOpPriority =< MaxPriority },
			parser__peek_token(NextToken),
			{ parser__could_start_term(NextToken, yes) }
		->
			{ parser__adjust_priority(RightAssoc, UnOpPriority,
							RightPriority) },
			parser__parse_term_2(RightPriority, TermKind,
				RightResult),
			{ OpPriority = UnOpPriority },
			( { RightResult = ok(RightTerm) } ->
				parser__get_term_context(Context, TermContext),
				{ Term = ok(term__functor(term__atom(Op),
						[RightTerm], TermContext)) }
			;
				% propagate error upwards
				{ Term = RightResult }
			)
		;
			parser__parse_simple_term(Token, Context, MaxPriority,
				Term),
			{ OpPriority = 0 }
		)
	;
		parser__error("unexpected end-of-file at start of sub-term",
			Term),
		{ OpPriority = 0 }
	).

:- pred parser__parse_rest(int, term_kind, int, term(T), parse(term(T)),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_rest(in, in, in, in, out, in, out) is det.

parser__parse_rest(MaxPriority, TermKind, LeftPriority, LeftTerm, Term) -->
	(
		% infix op
		parser__get_token(Token, Context),
		{
			Token = comma,
			TermKind = ordinary_term,
			Op0 = ","
		;
			Token = ht_sep,
			TermKind \= list_elem,
			Op0 = "|"
		;
			Token = name(Op0)
		},
		(
				% A token surrounded by backquotes is a
				% prefix token being using in an
				% infix manner.
			{ Op0 = "`" },
			parser__get_ops_table(OpTable),
			{ ops__lookup_operator_term(OpTable, OpPriority0,
				LeftAssoc0, RightAssoc0) }
		->
			{ OpPriority = OpPriority0 },
			{ LeftAssoc = LeftAssoc0 },
			{ RightAssoc = RightAssoc0 },
			parser__get_token(OpToken, _),
			(
				{ OpToken = name(NameOp) }
			->
				{ Op = NameOp },
				{ VariableTerm = [] }
			;
				{ OpToken = variable(VariableOp) },
				{ Op = "" },
				parser__add_var(VariableOp, Var),
				{ VariableTerm = [term__variable(Var)] }
			),
			parser__get_token(name("`"), _)
		;
			{ Op = Op0 },
			{ VariableTerm = [] },
			parser__get_ops_table(OpTable),
			{ ops__lookup_infix_op(OpTable, Op,
					OpPriority, LeftAssoc, RightAssoc) }
		),
		{ OpPriority =< MaxPriority },
		{ parser__check_priority(LeftAssoc, OpPriority, LeftPriority) }
	->
		{ parser__adjust_priority(RightAssoc, OpPriority,
					RightPriority) },
		parser__parse_term_2(RightPriority, TermKind, RightTerm0),
		( { RightTerm0 = ok(RightTerm) } ->
			parser__get_term_context(Context, TermContext),
			{ OpTerm = term__functor(term__atom(Op),
				list__append(VariableTerm,
					[LeftTerm, RightTerm]),
				TermContext) },
			parser__parse_rest(MaxPriority, TermKind, OpPriority,
				OpTerm, Term)
		;
			% propagate error upwards
			{ Term = RightTerm0 }
		)
	;
		% postfix op
		parser__get_token(name(Op), Context),
		parser__get_ops_table(OpTable),
		{ ops__lookup_postfix_op(OpTable, Op, OpPriority, LeftAssoc) },
		{ OpPriority =< MaxPriority },
		{ parser__check_priority(LeftAssoc, OpPriority, LeftPriority) }
	->
		parser__get_term_context(Context, TermContext),
		{ OpTerm = term__functor(term__atom(Op), [LeftTerm],
			TermContext) },
		parser__parse_rest(MaxPriority, TermKind, OpPriority, OpTerm,
			Term)
	;
		{ Term = ok(LeftTerm) }
	).

%-----------------------------------------------------------------------------%

:- pred parser__parse_simple_term(token, token_context, int, parse(term(T)),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_simple_term(in, in, in, out, in, out) is det.

parser__parse_simple_term(Token, Context, Priority, Term) -->
    ( parser__parse_simple_term_2(Token, Context, Priority, Term0) ->
	parser__check_for_higher_order_term(Term0, Context, Term)
    ;
	parser__unexpected_tok(Token, Context,
		"unexpected token at start of (sub)term", Term)
    ).

	% term --> integer		% priority 0
	% term --> float		% priority 0
	% term --> name("-") integer	% priority 0
	% term --> name("-") float	% priority 0
	% term --> atom(NonOp)		% priority 0
	% term --> atom(Op)		% priority `max_priority' + 1
	%	atom --> name
	%	atom --> open_list, close_list
	%	atom --> open_curly, close_curly
	% term --> variable		% priority 0
	% term --> atom, open_ct, arg_list, close
	%	arg_list --> arg
	%	arg_list --> arg, comma, arg_list
	% term --> open, term, close
	% term --> open_ct, term, close
	% term --> term, op, term	% with various conditions
	% term --> op, term		% with various conditions
	% term --> term, op		% with various conditions

:- pred parser__parse_simple_term_2(token, token_context, int, parse(term(T)),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_simple_term_2(in, in, in, out, in, out) is semidet.

parser__parse_simple_term_2(name(Atom), Context, Prec, Term) -->
	parser__get_term_context(Context, TermContext),
	( parser__get_token(open_ct) ->
		parser__parse_args(Args0),
		(	{ Args0 = ok(Args) },
			{ Term = ok(term__functor(term__atom(Atom), Args,
				TermContext)) }
		;
			% propagate error upwards
			{ Args0 = error(Message, Tokens) },
			{ Term = error(Message, Tokens) }
		)
	;
		parser__get_ops_table(OpTable),
		{ ops__lookup_op(OpTable, Atom) ->
			Prec > ops__max_priority(OpTable)
		;
			true
		},
		{ Term = ok(term__functor(term__atom(Atom), [], TermContext)) }
	).

parser__parse_simple_term_2(variable(VarName), _, _, Term) -->
	parser__add_var(VarName, Var),
	{ Term = ok(term__variable(Var)) }.

parser__parse_simple_term_2(integer(Int), Context, _, Term) -->
	parser__get_term_context(Context, TermContext),
	{ Term = ok(term__functor(term__integer(Int), [], TermContext)) }.

parser__parse_simple_term_2(float(Float), Context, _, Term) -->
	parser__get_term_context(Context, TermContext),
	{ Term = ok(term__functor(term__float(Float), [], TermContext)) }.

parser__parse_simple_term_2(string(String), Context, _, Term) -->
	parser__get_term_context(Context, TermContext),
	{ Term = ok(term__functor(term__string(String), [], TermContext)) }.

parser__parse_simple_term_2(open, _, _, Term) -->
	parser__parse_term(Term0),
	( { Term0 = ok(_) } ->
		( parser__get_token(close) ->
			{ Term = Term0 }
		;
			parser__unexpected("expecting `)' or operator", Term)
		)
	;
		% propagate error upwards
		{ Term = Term0 }
	).

parser__parse_simple_term_2(open_ct, Context, Prec, Term) -->
	parser__parse_simple_term_2(open, Context, Prec, Term).

parser__parse_simple_term_2(open_list, Context, _, Term) -->
	parser__get_term_context(Context, TermContext),
	( parser__get_token(close_list) ->
		parser__parse_special_atom("[]", TermContext, Term)
	;
		parser__parse_list(Term)
	).

parser__parse_simple_term_2(open_curly, Context, _, Term) -->
	parser__get_term_context(Context, TermContext),
	( parser__get_token(close_curly) ->
		parser__parse_special_atom("{}", TermContext, Term)
	;
		% This is a slight departure from ISO Prolog
		% syntax -- instead of parsing "{1,2,3}"
		% as "'{}'(','(1, ','(2, 3)))" we parse
		% it as "'{}'(1,2,3)". This makes the
		% structure of tuple functors the same
		% as other functors.
		parser__parse_term(SubTerm0),
		( { SubTerm0 = ok(SubTerm) } ->
			{ conjunction_to_list(SubTerm, ArgTerms) },
			( parser__get_token(close_curly) ->
				{ Term = ok(term__functor(term__atom("{}"), 
					ArgTerms, TermContext)) }
			;
				parser__unexpected("expecting `}' or operator",
					Term)
			)
		;
			% propagate error upwards
			{ Term = SubTerm0 }
		)
	).

:- pred parser__conjunction_to_list(term(T), list(term(T))).
:- mode parser__conjunction_to_list(in, out) is det.

parser__conjunction_to_list(Term, ArgTerms) :-
	( Term = term__functor(term__atom(","), [LeftTerm, RightTerm], _) ->
		parser__conjunction_to_list(RightTerm, ArgTerms0),
		ArgTerms = [LeftTerm | ArgTerms0]
	;
		ArgTerms = [Term]
	).

:- pred parser__check_for_higher_order_term(parse(term(T)), token_context,
	parse(term(T)), parser__state(Ops, T),
	parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__check_for_higher_order_term(in, in, out, in, out) is det.

parser__check_for_higher_order_term(Term0, Context, Term) -->
	%
	% As an extension to ISO Prolog syntax,
	% we check for the syntax "Term(Args)", and parse it
	% as the term ''(Term, Args).  The aim of this extension
	% is to provide a nicer syntax for higher-order stuff.
	%
	( { Term0 = ok(Term1) }, parser__get_token(open_ct) ->
		parser__get_term_context(Context, TermContext),
		parser__parse_args(Args0),
		(	{ Args0 = ok(Args) },
			{ Term2 = ok(term__functor(term__atom(""),
				[Term1 | Args],
				TermContext)) },
			parser__check_for_higher_order_term(Term2, Context, Term)
		;
			% propagate error upwards
			{ Args0 = error(Message, Tokens) },
			{ Term = error(Message, Tokens) }
		)
	;
		{ Term = Term0 }
	).

:- pred parser__parse_special_atom(string, term__context, parse(term(T)),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_special_atom(in, in, out, in, out) is det.

parser__parse_special_atom(Atom, TermContext, Term) -->
	( parser__get_token(open_ct) ->
		parser__parse_args(Args0),
		(	{ Args0 = ok(Args) },
			{ Term = ok(term__functor(term__atom(Atom),
				Args, TermContext)) }
		;
			% propagate error upwards
			{ Args0 = error(Message, Tokens) },
			{ Term = error(Message, Tokens) }
		)
	;
		{ Term = ok(term__functor(term__atom(Atom), [], TermContext)) }
	).

:- pred parser__parse_list(parse(term(T)), parser__state(Ops, T),
		parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_list(out, in, out) is det.

parser__parse_list(List) -->
	parser__parse_list_elem(Arg0),
	( { Arg0 = ok(Arg) } ->
	    ( parser__get_token(Token, Context) ->
		parser__get_term_context(Context, TermContext),
		( { Token = comma } ->
		    parser__parse_list(Tail0),
		    ( { Tail0 = ok(Tail) } ->
		        { List = ok(term__functor(term__atom("[|]"),
					[Arg, Tail], TermContext)) }
		    ;
			% propagate error
			{ List = Tail0 }
		    )
		; { Token = ht_sep } ->
		    parser__parse_arg(Tail0),
		    ( { Tail0 = ok(Tail) } ->
			( parser__get_token(close_list) ->
		            { List = ok(term__functor(term__atom("[|]"),
					[Arg, Tail], TermContext)) }
			;
			    parser__unexpected("expecting ']' or operator",
				List)
			)
		    ;
			% propagate error
			{ List = Tail0 }
		    )
		; { Token = close_list } ->
		    { Tail = term__functor(term__atom("[]"), [], TermContext) },
		    { List = ok(term__functor(term__atom("[|]"), [Arg, Tail],
				TermContext)) }
		;
		    parser__unexpected_tok(Token, Context,
			"expected comma, `|', `]', or operator", List)
		)
	    ;
		% XXX error message should state the line that the
		% list started on
		parser__error("unexpected end-of-file in list", List)
	    )
	;
	    % propagate error
	    { List = Arg0 }
	).

:- pred parser__parse_args(parse(list(term(T))),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__parse_args(out, in, out) is det.

parser__parse_args(List) -->
	parser__parse_arg(Arg0),
	(   { Arg0 = ok(Arg) },
	    ( parser__get_token(Token, Context) ->
		( { Token = comma } ->
		    parser__parse_args(Tail0),
		    ( { Tail0 = ok(Tail) } ->
			{ List = ok([Arg|Tail]) }
		    ;
			% propagate error upwards
		        { List = Tail0 }
		    )
		; { Token = close } ->
		    { List = ok([Arg]) }
		;
		    parser__unexpected_tok(Token, Context,
				"expected `,', `)', or operator", List)
		)
	    ;
		parser__error("unexpected end-of-file in argument list", List)
	    )
	;
	    { Arg0 = error(Message, Tokens) },
	    % propagate error upwards
	    { List = error(Message, Tokens) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Routines that manipulate the parser state.

:- type parser__state(Ops, T)	% <= op_table(Ops)
	--->	parser__state(
			stream_name	:: string,
					% the name of the stream being parsed
			ops_table	:: Ops,
					% the current set of operators
			varset		:: varset(T),
					% the names of the variables in the
					% term being parsed
			tokens_left	:: token_list,
					% the remaining tokens
			var_names	:: map(string, var(T))
					% a map from variable name to variable
					% so we know when to make a fresh var
		).

:- func parser_state_get_stream_name(parser__state(Ops, T)) = string.
:- func parser_state_get_ops_table(parser__state(Ops, T)) = Ops.
:- func parser_state_get_varset(parser__state(Ops, T)) = varset(T).
:- func parser_state_get_tokens_left(parser__state(Ops, T)) = token_list.
:- func parser_state_get_var_names(parser__state(Ops, T)) =
	map(string, var(T)).

:- func parser_state_set_varset(parser__state(Ops, T), varset(T))
	= parser__state(Ops, T).
:- func parser_state_set_tokens_left(parser__state(Ops, T), token_list)
	= parser__state(Ops, T).
:- func parser_state_set_var_names(parser__state(Ops, T), map(string, var(T)))
	= parser__state(Ops, T).

% If you want profiling to tell you the frequencies of these operations,
% change the inline pragmas to no_inline pragmas.

:- pragma inline(parser_state_get_stream_name/1).
:- pragma inline(parser_state_get_ops_table/1).
:- pragma inline(parser_state_get_varset/1).
:- pragma inline(parser_state_get_tokens_left/1).
:- pragma inline(parser_state_get_var_names/1).

:- pragma inline(parser_state_set_varset/2).
:- pragma inline(parser_state_set_tokens_left/2).
:- pragma inline(parser_state_set_var_names/2).

parser_state_get_stream_name(ParserState) = ParserState ^ stream_name.
parser_state_get_ops_table(ParserState) = ParserState ^ ops_table.
parser_state_get_varset(ParserState) = ParserState ^ varset.
parser_state_get_tokens_left(ParserState) = ParserState ^ tokens_left.
parser_state_get_var_names(ParserState) = ParserState ^ var_names.

parser_state_set_varset(ParserState0, VarSet) =
	ParserState0 ^ varset := VarSet.
parser_state_set_tokens_left(ParserState0, Tokens) =
	ParserState0 ^ tokens_left := Tokens.
parser_state_set_var_names(ParserState0, Names) =
	ParserState0 ^ var_names := Names.

%-----------------------------------------------------------------------------%

	% We encountered an error.  See if the next token
	% was an infix or postfix operator.  If so, it would
	% normally form part of the term, so the error must
	% have been an operator precedence error.  Otherwise,
	% it was some other sort of error, so issue the usual
	% error message.

:- pred parser__unexpected(string, parse(U),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__unexpected(in, out, in, out) is det.

parser__unexpected(UsualMessage, Error) -->
	( parser__get_token(Token, Context) ->
		parser__unexpected_tok(Token, Context, UsualMessage, Error)
	;
		parser__error(UsualMessage, Error)
	).

:- pred parser__unexpected_tok(token, token_context, string, parse(U),
	parser__state(Ops, T), parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__unexpected_tok(in, in, in, out, in, out) is det.

parser__unexpected_tok(Token, Context, UsualMessage, Error) -->
	% push the token back, so that the error message
	% points at it rather than at the following token
	parser__unget_token(Token, Context),
	(
		{ Token = name(Op)
		; Token = comma, Op = ","
		},
		parser__get_ops_table(OpTable),
		{ ops__lookup_infix_op(OpTable, Op, _, _, _)
		; ops__lookup_postfix_op(OpTable, Op, _, _)
		}
	->
		parser__error("operator precedence error", Error)
	;
		parser__error(UsualMessage, Error)
	).

%-----------------------------------------------------------------------------%

:- pred parser__error(string, parse(U), parser__state(Ops, T),
		parser__state(Ops, T)).
:- mode parser__error(in, out, in, out) is det.

parser__error(Message, error(Message, Tokens), ParserState, ParserState) :-
	Tokens = parser_state_get_tokens_left(ParserState).

%-----------------------------------------------------------------------------%

:- pred parser__could_start_term(token, bool).
:- mode parser__could_start_term(in, out) is det.

parser__could_start_term(name(_), yes).
parser__could_start_term(variable(_), yes).
parser__could_start_term(integer(_), yes).
parser__could_start_term(float(_), yes).
parser__could_start_term(string(_), yes).
parser__could_start_term(open, yes).
parser__could_start_term(open_ct, yes).
parser__could_start_term(close, no).
parser__could_start_term(open_list, yes).
parser__could_start_term(close_list, no).
parser__could_start_term(open_curly, yes).
parser__could_start_term(close_curly, no).
parser__could_start_term(ht_sep, no).
parser__could_start_term(comma, no).
parser__could_start_term(end, no).
parser__could_start_term(junk(_), no).
parser__could_start_term(error(_), no).
parser__could_start_term(io_error(_), no).
parser__could_start_term(eof, no).
parser__could_start_term(integer_dot(_), no).

%-----------------------------------------------------------------------------%

:- pred parser__init_state(Ops, string, token_list,
		parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__init_state(in, in, in, out) is det.

parser__init_state(Ops, FileName, Tokens, ParserState) :-
	varset__init(VarSet),
	map__init(Names),
	ParserState = parser__state(FileName, Ops, VarSet, Tokens, Names).

:- pred parser__final_state(parser__state(Ops, T), varset(T), token_list).
:- mode parser__final_state(in, out, out) is det.

parser__final_state(ParserState, VarSet, TokenList) :-
	VarSet = parser_state_get_varset(ParserState),
	TokenList = parser_state_get_tokens_left(ParserState).

%-----------------------------------------------------------------------------%

:- pred parser__get_token(token, parser__state(Ops, T), parser__state(Ops, T)).
:- mode parser__get_token(out, in, out) is semidet.

parser__get_token(Token) -->
	parser__get_token(Token, _Context).

:- pred parser__get_token(token, token_context,
		parser__state(Ops, T), parser__state(Ops, T)).
:- mode parser__get_token(out, out, in, out) is semidet.
% :- mode parser__get_token(in, in, out, in) is det.

parser__get_token(Token, Context, ParserState0, ParserState) :-
	Tokens0 = parser_state_get_tokens_left(ParserState0),
	Tokens0 = token_cons(Token, Context, Tokens),
	ParserState = parser_state_set_tokens_left(ParserState0, Tokens).

:- pred parser__unget_token(token, token_context,
		parser__state(Ops, T), parser__state(Ops, T)).
:- mode parser__unget_token(in, in, in, out) is det.
% :- mode parser__unget_token(out, out, out, in) is semidet.

parser__unget_token(Token, Context, ParserState0, ParserState) :-
	Tokens0 = parser_state_get_tokens_left(ParserState0),
	Tokens = token_cons(Token, Context, Tokens0),
	ParserState = parser_state_set_tokens_left(ParserState0, Tokens).

:- pred parser__peek_token(token, parser__state(Ops, T), parser__state(Ops, T)).
:- mode parser__peek_token(out, in, out) is semidet.

parser__peek_token(Token) -->
	parser__peek_token(Token, _Context).

:- pred parser__peek_token(token, token_context,
		parser__state(Ops, T), parser__state(Ops, T)).
:- mode parser__peek_token(out, out, in, out) is semidet.

parser__peek_token(Token, Context, ParserState, ParserState) :-
	Tokens = parser_state_get_tokens_left(ParserState),
	Tokens = token_cons(Token, Context, _).

%-----------------------------------------------------------------------------%

:- pred parser__add_var(string, var(T), parser__state(Ops, T),
		parser__state(Ops, T)).
:- mode parser__add_var(in, out, in, out) is det.

parser__add_var(VarName, Var, ParserState0, ParserState) :-
	( VarName = "_" ->
		VarSet0 = parser_state_get_varset(ParserState0),
		varset__new_var(VarSet0, Var, VarSet),
		ParserState = parser_state_set_varset(ParserState0, VarSet)
	;
		Names0 = parser_state_get_var_names(ParserState0),
		( map__search(Names0, VarName, Var0) ->
			Var = Var0,
			ParserState = ParserState0
		;
			VarSet0 = parser_state_get_varset(ParserState0),
			varset__new_named_var(VarSet0, VarName, Var, VarSet),
			map__det_insert(Names0, VarName, Var, Names),
			ParserState1 = parser_state_set_varset(ParserState0,
				VarSet),
			ParserState = parser_state_set_var_names(ParserState1,
				Names)
		)
	).

:- pred parser__get_ops_table(Ops, parser__state(Ops, T),
		parser__state(Ops, T)) <= op_table(Ops).
:- mode parser__get_ops_table(out, in, out) is det.

parser__get_ops_table(OpTable, ParserState, ParserState) :-
	OpTable = parser_state_get_ops_table(ParserState).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred parser__adjust_priority(ops__assoc, int, int).
:- mode parser__adjust_priority(in, in, out) is det.

parser__adjust_priority(y, Priority, Priority).
parser__adjust_priority(x, OldPriority, NewPriority) :-
	NewPriority = OldPriority - 1.

:- pred parser__check_priority(ops__assoc, int, int).
:- mode parser__check_priority(in, in, in) is semidet.

parser__check_priority(y, MaxPriority, Priority) :-
	Priority =< MaxPriority.
parser__check_priority(x, MaxPriority, Priority) :-
	Priority < MaxPriority.

:- pred parser__get_term_context(token_context, term__context,
	parser__state(Ops, T), parser__state(Ops, T)).
:- mode parser__get_term_context(in, out, in, out) is det.

parser__get_term_context(TokenContext, TermContext, ParserState, ParserState)
		:-
	FileName = parser_state_get_stream_name(ParserState),
	term__context_init(FileName, TokenContext, TermContext).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
