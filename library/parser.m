%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: parser.nl.
% main author: fjh.
%
% This file exports the predicate parser__read_term, which reads
% a term from the current input stream.  The parser and lexer are
% intended to exactly follow ISO Prolog syntax, but there are some
% departures from that for three reasons:
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
% Efficiency could be improved - the way prefix and binary prefix
% operators are handled could potentially cause parts of the input
% to be parsed many times over.
%
%-----------------------------------------------------------------------------%

:- module parser.
:- interface.
:- import_module io, term_io.

:- pred parser__read_term(read_term, io__state, io__state).
:- mode parser__read_term(out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, char, int, float, list, std_util, require.
:- import_module term, varset.
:- import_module lexer, ops.

:- type parse(T)
	--->	ok(T)
	;	error(string, token_list).

%-----------------------------------------------------------------------------%

parser__read_term(Result) -->
	lexer__get_token_list(Tokens),
	( { Tokens = [] } ->
		{ Result = eof }
	;
		parser__init_state(Tokens, ParserState0),
		{ parser__parse_whole_term(Term, ParserState0, ParserState) },
		{ parser__final_state(ParserState, VarSet, LeftOverTokens) },
		{ parser__check_for_errors(Term, VarSet,
			Tokens, LeftOverTokens, Result) }
	).

:- pred parser__check_for_errors(parse(term), varset, token_list, token_list,
				 read_term).
:- mode parser__check_for_errors(in, in, in, in, out) is det.

parser__check_for_errors(error(ErrorMessage, ErrorTokens), _VarSet, Tokens,
		_LeftOverTokens, Result) :-
	% determine the right error message
	(
		parser__check_for_bad_token(ErrorTokens, BadTokenMessage, _)
	->
		Message = BadTokenMessage
	;
		ErrorTokens = [FirstTok - _| _]
	->
		lexer__token_to_string(FirstTok, TokString),
		string__append_list(
			["Syntax error at ", TokString, ": ", ErrorMessage],
			Message)
	;
		string__append("Syntax error: ", ErrorMessage, Message)
	),
	% determine the appropriate line number
	(
		ErrorTokens = [_ - LineNum0 | _]
	->
		LineNum = LineNum0
	;
		Tokens = [_ - LineNum1 | _]
	->
		LineNum = LineNum1
	;
		error("parser__check_for_errors")
	),
	Result = error(Message, LineNum).

parser__check_for_errors(ok(Term), VarSet, _Tokens, LeftOverTokens, Result) :-
	(
		parser__check_for_bad_token(LeftOverTokens, Message, LineNum)
	->
		Result = error(Message, LineNum)
	;
		LeftOverTokens = [Token - LineNum | _]
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

parser__check_for_bad_token([Token - LineNum | _], Message, LineNum) :-
	(
		Token = io_error(IO_Error),
		io__error_message(IO_Error, IO_ErrorMessage),
		string__append("I/O error: ", IO_ErrorMessage, Message)
	;
		Token = junk(Char),
		char_to_int(Char, Code),
		string__int_to_base_string(Code, 10, Decimal),
		string__int_to_base_string(Code, 16, Hex),
		string__append_list(["Syntax error: Illegal character 0x",
			Hex, " (", Decimal, ") in input"], Message)
	;
		Token = error(ErrorMessage),
		string__append("Syntax error: ", ErrorMessage, Message)
	).

:- pred parser__parse_whole_term(parse(term), parser__state, parser__state).
:- mode parser__parse_whole_term(out, in, out) is det.

parser__parse_whole_term(Term) -->
	parser__parse_term(Term0),
	( { Term0 = ok(_) } ->
		( parser__get_token(end) ->
			{ Term = Term0 }
		;
			parser__error("operator or `.' expected", Term)
		)
	;
		% propagate error upwards
		{ Term = Term0 }
	).

:- pred parser__parse_term(parse(term), parser__state, parser__state).
:- mode parser__parse_term(out, in, out) is det.

parser__parse_term(Term) -->
	parser__parse_term_2(1201, no, Term).

:- pred parser__parse_arg(parse(term), parser__state, parser__state).
:- mode parser__parse_arg(out, in, out) is det.

parser__parse_arg(Term) -->
	parser__parse_term_2(1201, yes, Term).

	% XXX I think ISO prolog syntax would require us to
	% change that to  
	%	parser__parse_term(999, no, Term).
	% The above is because we need bug-for-bug compatibility
	% with the NU-Prolog parser in order to support e.g. `::' in args.

:- pred parser__parse_term_2(int, bool, parse(term),
				parser__state, parser__state).
:- mode parser__parse_term_2(in, in, out, in, out) is det.

parser__parse_term_2(MaxPriority, IsArg, Term) -->
	parser__parse_left_term(MaxPriority, IsArg, LeftPriority, LeftTerm0),
	( { LeftTerm0 = ok(LeftTerm) } ->
		parser__parse_rest(MaxPriority, IsArg, LeftPriority, LeftTerm,
			Term)
	;
		% propagate error upwards
		{ Term = LeftTerm0 }
	).

:- pred parser__parse_left_term(int, bool, int, parse(term),
				parser__state, parser__state).
:- mode parser__parse_left_term(in, in, out, out, in, out) is det.

parser__parse_left_term(MaxPriority, IsArg, OpPriority, Term) -->
	(
		% binary prefix op
		parser__get_token(name(Op), Context),
		\+ parser__get_token(open_ct),
		parser__get_ops_table(OpTable),
		{ ops__lookup_binary_prefix_op(OpTable, Op,
				BinOpPriority, RightAssoc, RightRightAssoc) },
		{ BinOpPriority =< MaxPriority },
		{ parser__adjust_priority(RightAssoc, BinOpPriority,
							RightPriority) },
		{ parser__adjust_priority(RightRightAssoc, BinOpPriority,
							RightRightPriority) },
		parser__parse_term_2(RightPriority, IsArg, ok(RightTerm)),
		parser__parse_term_2(RightRightPriority, IsArg,
						ok(RightRightTerm))
	->
		parser__get_term_context(Context, TermContext),
		{ OpPriority = BinOpPriority },
		{ Term = ok(term__functor(term__atom(Op),
			[RightTerm, RightRightTerm], TermContext)) }
	;
		% prefix op
		parser__get_token(name(Op), Context),
		\+ parser__get_token(open_ct),
		parser__get_ops_table(OpTable),
		{ ops__lookup_prefix_op(OpTable, Op, UnOpPriority,
						RightAssoc) },
		{ UnOpPriority =< MaxPriority },
		{ parser__adjust_priority(RightAssoc, UnOpPriority,
						RightPriority) },
		parser__parse_term_2(RightPriority, IsArg, ok(RightTerm))
	->
		parser__get_term_context(Context, TermContext),
		{ OpPriority = UnOpPriority },
		{
			Op = "-",
			RightTerm = term__functor(term__integer(X), [], _)
		->
			NegX is 0 - X,
			Term = ok(term__functor(term__integer(NegX), [],
						TermContext))
		; Op = "-", RightTerm = term__functor(term__float(F), [], _) ->
			builtin_float_minus(0.0, F, NegF),
			Term = ok(term__functor(term__float(NegF), [],
				TermContext))
		;
			Term = ok(term__functor(term__atom(Op), [RightTerm],
						TermContext))
		}
	;
		parser__parse_simple_term(MaxPriority, Term),
		{ OpPriority = 0 }
	).

:- pred parser__parse_rest(int, bool, int, term, parse(term),
				parser__state, parser__state).
:- mode parser__parse_rest(in, in, in, in, out, in, out) is det.

parser__parse_rest(MaxPriority, IsArg, LeftPriority, LeftTerm, Term) -->
	(
		% infix op
		parser__get_token(Token, Context),
		{ Token = comma, IsArg = no ->
			Op = ","
		;
			Token = name(Op)
		},
		\+ parser__get_token(open_ct),
		parser__get_ops_table(OpTable),
		{ ops__lookup_infix_op(OpTable, Op,
				OpPriority, LeftAssoc, RightAssoc) },
		{ OpPriority =< MaxPriority },
		{ parser__check_priority(LeftAssoc, OpPriority, LeftPriority) }
	->
		{ parser__adjust_priority(RightAssoc, OpPriority,
					RightPriority) },
		parser__parse_term_2(RightPriority, IsArg, RightTerm0),
		( { RightTerm0 = ok(RightTerm) } ->
			parser__get_term_context(Context, TermContext),
			{ OpTerm = term__functor(term__atom(Op),
				[LeftTerm, RightTerm], TermContext) },
			parser__parse_rest(MaxPriority, IsArg, OpPriority,
				OpTerm, Term)
		;
			% propagate error upwards
			{ Term = RightTerm0 }
		)
	;
		% postfix op
		parser__get_token(name(Op), Context),
		\+ parser__get_token(open_ct),
		parser__get_ops_table(OpTable),
		{ ops__lookup_postfix_op(OpTable, Op, OpPriority, LeftAssoc) },
		{ OpPriority =< MaxPriority },
		{ parser__check_priority(LeftAssoc, OpPriority, LeftPriority) }
	->
		parser__get_term_context(Context, TermContext),
		{ OpTerm = term__functor(term__atom(Op), [LeftTerm],
			TermContext) },
		parser__parse_rest(MaxPriority, IsArg, OpPriority, OpTerm, Term)
	;
		{ Term = ok(LeftTerm) }
	).

%-----------------------------------------------------------------------------%
	
:- pred parser__parse_simple_term(int, parse(term),
				parser__state, parser__state).
:- mode parser__parse_simple_term(in, out, in, out) is det.

parser__parse_simple_term(Priority, Term) -->
	( parser__get_token(Token, Context) ->
	    ( parser__parse_simple_term_2(Token, Context, Priority, Term0) ->
		{ Term = Term0 }
	    ;
		parser__error("unexpected token", Term)
	    )
	;
	    parser__error("unexpected end-of-file", Term)
	).

	% term --> integer		% priority 0
	% term --> float		% priority 0
	% term --> name("-") integer	% priority 0
	% term --> name("-") float	% priority 0
	% term --> atom(NonOp)		% priority 0
	% term --> atom(Op)		% priority 1201
	%	atom --> name
	%	atom --> open_list, close_list
	%	atom --> open_curly, close_curly
	% term --> variable		% priority 0
	% term -->
	
:- pred parser__parse_simple_term_2(token, token_context, int, parse(term),
				parser__state, parser__state).
:- mode parser__parse_simple_term_2(in, in, in, out, in, out) is semidet.

parser__parse_simple_term_2(name(Atom), Context, Prec, Term) -->
	parser__get_term_context(Context, TermContext),
	( parser__get_token(open_ct) ->
		parser__parse_args(Args0),
		(	{ Args0 = ok(Args) },
			{ Term = ok(term__functor(term__atom(Atom), Args,
				TermContext)) }
		;
			{ Args0 = error(Message, Tokens) },
			{ Term = error(Message, Tokens) }
		)
	;
		parser__get_ops_table(OpTable),
		{ ops__lookup_op(OpTable, Atom) ->
			Prec >= 1201
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
			parser__error("missing `)'", Term)
		)
	;
		% propagate error upwards
		{ Term = Term0 }
	).

parser__parse_simple_term_2(open_ct, _, _, Term) -->
	parser__parse_term(Term0),
	( { Term0 = ok(_) } ->
		( parser__get_token(close) ->
			{ Term = Term0 }
		;
			parser__error("missing `)'", Term)
		)
	;
		% propagate error upwards
		{ Term = Term0 }
	).

parser__parse_simple_term_2(open_list, Context, _, Term) -->
	( parser__get_token(close_list) ->
		parser__get_term_context(Context, TermContext),
		{ Term = ok(term__functor(term__atom("[]"), [], TermContext)) }
	;
		parser__parse_list(Term)
	).

parser__parse_simple_term_2(open_curly, Context, _, Term) -->
	parser__get_term_context(Context, TermContext),
	( parser__get_token(close_curly) ->
		{ Term = ok(term__functor(term__atom("{}"), [], TermContext)) }
	;
		parser__parse_term(SubTerm0),
		( { SubTerm0 = ok(SubTerm) } ->
			( parser__get_token(close_curly) ->
				{ Term = ok(term__functor(term__atom("{}"), 
					[SubTerm], TermContext)) }
			;
				parser__error("missing `}'", Term)
			)
		;
			% propagate error upwards
			{ Term = SubTerm0 }
		)
	).

:- pred parser__parse_list(parse(term), parser__state, parser__state).
:- mode parser__parse_list(out, in, out) is det.

parser__parse_list(List) -->
	parser__parse_arg(Arg0),
	( { Arg0 = ok(Arg) } ->
	    ( parser__get_token(Token, Context) ->
		parser__get_term_context(Context, TermContext),
		( { Token = comma } ->
		    parser__parse_list(Tail0),
		    ( { Tail0 = ok(Tail) } ->
		        { List = ok(term__functor(term__atom("."), [Arg, Tail],
						TermContext)) }
		    ;
			% propagate error
			{ List = Tail0 }
		    )
		; { Token = ht_sep } ->
		    parser__parse_arg(Tail0),
		    ( { Tail0 = ok(Tail) } ->
			( parser__get_token(close_list) ->
		            { List = ok(term__functor(term__atom("."),
					[Arg, Tail], TermContext)) }
			;
			    parser__error("missing ']'", List)
			)
		    ;
			% propagate error
			{ List = Tail0 }
		    )
		; { Token = close_list } ->
		    { List = ok(term__functor(term__atom("[]"), [Arg],
				TermContext)) }
		;
		    parser__error("expected comma, `|', or `]'", List)
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

:- pred parser__parse_args(parse(list(term)), parser__state, parser__state).
:- mode parser__parse_args(out, in, out) is det.

parser__parse_args(List) -->
	parser__parse_arg(Arg0),
	(   { Arg0 = ok(Arg) },
	    ( parser__get_token(Token) ->
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
		    parser__error("expected `,' or `)'", List)
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

:- type parser__state
	--->	parser__state(
			string,		% the name of the stream being parsed
			ops__table,	% the current set of operators
			varset,		% the names of the variables in the
					% term being parsed
			token_list	% the remaining tokens
		).


%-----------------------------------------------------------------------------%

:- pred parser__error(string, parse(T), parser__state, parser__state).
:- mode parser__error(in, out, di, uo) is det.

parser__error(Message, error(Message, Tokens), ParserState, ParserState) :-
	ParserState = parser__state(_, _, _, Tokens).

%-----------------------------------------------------------------------------%

:- pred parser__init_state(token_list, parser__state, io__state, io__state).
:- mode parser__init_state(in, out, di, uo) is det.

parser__init_state(Tokens, ParserState) -->
	{ ops__init_op_table(OpTable) },
	{ varset__init(VarSet) },
	io__input_stream_name(FileName),
	{ ParserState = parser__state(FileName, OpTable, VarSet, Tokens) }.

:- pred parser__final_state(parser__state, varset, token_list).
:- mode parser__final_state(in, out, out) is det.

parser__final_state(parser__state(_FileName, _OpTable, VarSet, TokenList),
			VarSet, TokenList).

%-----------------------------------------------------------------------------%

:- pred parser__get_token(token, parser__state, parser__state).
:- mode parser__get_token(out, in, out) is semidet.

parser__get_token(Token) -->
	parser__get_token(Token, _Context).

:- pred parser__get_token(token, token_context, parser__state, parser__state).
:- mode parser__get_token(out, out, in, out) is semidet.

parser__get_token(Token, Context,
			parser__state(FileName, OpTable, VarSet, Tokens0),
			parser__state(FileName, OpTable, VarSet, Tokens)) :-
	Tokens0 = [Token - Context | Tokens].

:- pred parser__peek_token(token, parser__state, parser__state).
:- mode parser__peek_token(out, in, out) is semidet.

parser__peek_token(Token) -->
	parser__peek_token(Token, _Context).

:- pred parser__peek_token(token, token_context, parser__state, parser__state).
:- mode parser__peek_token(out, out, in, out) is semidet.

parser__peek_token(Token, Context) -->
	=(parser__state(_, _, _, Tokens)),
	{ Tokens = [Token - Context | _] }.

%-----------------------------------------------------------------------------%

:- pred parser__add_var(string, var, parser__state, parser__state).
:- mode parser__add_var(in, out, in, out) is det.

parser__add_var(VarName, Var,
			parser__state(FileName, OpTable, VarSet0, Tokens),
			parser__state(FileName, OpTable, VarSet, Tokens)) :-
	( VarName = "_" ->
		varset__new_var(VarSet0, Var, VarSet)
	; varset__lookup_name(VarSet0, Var0, VarName) ->
		Var = Var0,
		VarSet = VarSet0
	;
		varset__new_var(VarSet0, Var, VarSet1),
		varset__name_var(VarSet1, Var, VarName, VarSet)
	).

:- pred parser__get_ops_table(ops__table, parser__state, parser__state).
:- mode parser__get_ops_table(out, in, out) is det.

parser__get_ops_table(OpTable) -->
	=(parser__state(_, OpTable, _, _)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred parser__adjust_priority(ops__assoc, int, int).
:- mode parser__adjust_priority(in, in, out) is det.

parser__adjust_priority(y, Priority, Priority).
parser__adjust_priority(x, OldPriority, NewPriority) :-
	NewPriority is OldPriority - 1.
		
:- pred parser__check_priority(ops__assoc, int, int).
:- mode parser__check_priority(in, in, in) is semidet.

parser__check_priority(y, MaxPriority, Priority) :-
	Priority =< MaxPriority.
parser__check_priority(x, MaxPriority, Priority) :-
	Priority < MaxPriority.

:- pred parser__get_term_context(token_context, term__context,
				parser__state, parser__state).
:- mode parser__get_term_context(in, out, in, out) is det.
		
parser__get_term_context(TokenContext, TermContext) -->
	=(parser__state(FileName, _Ops, _VarSet, _Tokens)),
	{ term__context_init(FileName, TokenContext, TermContext) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
