%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: term_io.m.
% Main author: fjh.
% Stability: medium to high.
%
% This file encapsulates all the term I/O.
% This exports predicates to read and write terms in the
% nice ground representation provided in term.m.
%
%-----------------------------------------------------------------------------%

:- module term_io.
:- interface.
:- import_module io, varset, term.

% External interface: exported predicates

/***** not yet implemented
:- type op_type ---> fx; fy; xf; yf; xfx; xfy; yfx; fxx; fxy; fyx; fyy.
:- pred term_io__op(int, op_type, string, io__state, io__state).
:- mode term_io__op(in, in, in, di, uo) is det.
%	term_io__op(Prec, Type, OpName, IOState0, IOState1).
%		Define an operator as per Prolog op/3 for future calls to
%		term_io__read_term.

:- type op_details ---> op(int, op_type, string).
:- pred term_io__current_ops(list(op_details), io__state, io__state).
:- mode term_io__current_ops(out, di, uo) is det.
%		Return a list containing all the current operator definitions.
%		Does not modify the io__state.
*****/

:- type read_term ---> eof ; error(string, int) ; term(varset, term).
:- pred term_io__read_term(read_term, io__state, io__state).
:- mode term_io__read_term(out, di, uo) is det.

%	term_io__read_term(Result, IO0, IO1).
%		Read a term from standard input. Similar to NU-Prolog
%		read_term/2, except that resulting term is in the ground
%		representation. Binds Result to either `eof',
%		`term(VarSet, Term)', or `error(Message, LineNumber)'.

:- pred term_io__write_term(varset, term, io__state, io__state).
:- mode term_io__write_term(in, in, di, uo) is det.
%		Writes a term to standard output.

:- pred term_io__write_term_nl(varset, term, io__state, io__state).
:- mode term_io__write_term_nl(in, in, di, uo) is det.
%		As above, except it appends a period and new-line.

:- pred term_io__write_constant(const, io__state, io__state).
:- mode term_io__write_constant(in, di, uo) is det.
%		Writes a constant (integer, float, or atom) to stdout.

:- pred term_io__write_variable(var, varset, io__state, io__state).
:- mode term_io__write_variable(in, in, di, uo) is det.
%		Writes a variable to stdout.

:- pred term_io__quote_string(string, io__state, io__state).
:- mode term_io__quote_string(in, di, uo) is det.
	% Given a string S, write S in double-quotes, with characters
	% escaped if necessary, to stdout.

:- pred term_io__quote_atom(string, io__state, io__state).
:- mode term_io__quote_atom(in, di, uo) is det.
	% Given an atom-name A, write A, enclosed in single-quotes if necessary,
	% with characters escaped if necessary, to stdout.

:- pred term_io__quote_char(char, io__state, io__state).
:- mode term_io__quote_char(in, di, uo) is det.
	% Given a character C, write C in single-quotes,
	% escaped if necessary, to stdout.

:- pred term_io__write_escaped_char(char, io__state, io__state).
:- mode term_io__write_escaped_char(in, di, uo) is det.
	% Given a character C, write C, escaped if necessary, to stdout.
	% The character is not enclosed in quotes.

:- pred term_io__write_escaped_string(string, io__state, io__state).
:- mode term_io__write_escaped_string(in, di, uo) is det.
	% Given a string S, write S, with characters
	% escaped if necessary, to stdout.
	% The string is not enclosed in quotes.

	% `term_io__quote_single_char' is the old (misleading) name for
	% `term_io__write_escaped_char'.  Use the latter instead.
:- pragma obsolete(term_io__quote_single_char/3).
:- pred term_io__quote_single_char(char, io__state, io__state).
:- mode term_io__quote_single_char(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, require, list, string, int, char.
:- import_module lexer, parser, ops.

term_io__read_term(Result) -->
	parser__read_term(Result).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% write a variable to standard output.
	% use the variable names specified by varset and write _N
	% for all unnamed variables with N starting at 0.

term_io__write_variable(Variable, VarSet) -->
	term_io__write_variable_2(Variable, VarSet, 0, _, _).

:- pred term_io__write_variable_2(var, varset, int, varset, int,
				io__state, io__state).
:- mode term_io__write_variable_2(in, in, in, out, out, di, uo) is det.

term_io__write_variable_2(Id, VarSet0, N0, VarSet, N) -->
	(
		{ varset__search_var(VarSet0, Id, Val) }
	->
		term_io__write_term_2(Val, VarSet0, N0, VarSet, N)
	;
		{ varset__search_name(VarSet0, Id, Name) }
	->
		{ N = N0 },
		{ VarSet = VarSet0 },
		io__write_string(Name)
	;
		% XXX problems with name clashes

		{ string__int_to_string(N0, Num) },
		{ string__append("_", Num, VarName) },
		{ varset__name_var(VarSet0, Id, VarName, VarSet) },
		{ N is N0 + 1 },
		io__write_string(VarName)
	).

%-----------------------------------------------------------------------------%

	% write a term to standard output.
	% use the variable names specified by varset and write _N
	% for all unnamed variables with N starting at 0.

term_io__write_term(VarSet, Term) -->
	term_io__write_term_2(Term, VarSet, 0, _, _).

:- pred term_io__write_term_2(term, varset, int, varset, int,
				io__state, io__state).
:- mode term_io__write_term_2(in, in, in, out, out, di, uo) is det.

term_io__write_term_2(Term, VarSet0, N0, VarSet, N) -->
	{ ops__max_priority(MaxPriority) },
	term_io__write_term_3(Term, MaxPriority + 1, VarSet0, N0, VarSet, N).

:- pred term_io__write_term_3(term, ops__priority, varset, int, varset, int,
				io__state, io__state).
:- mode term_io__write_term_3(in, in, in, in, out, out, di, uo) is det.

term_io__write_term_3(term__variable(Id), _, VarSet0, N0, VarSet, N) -->
	term_io__write_variable_2(Id, VarSet0, N0, VarSet, N).
term_io__write_term_3(term__functor(Functor, Args, _), Priority,
			VarSet0, N0, VarSet, N) -->
	io__get_op_table(OpTable),
	(
		{ Functor = term__atom(".") },
		{ Args = [ListHead, ListTail] }
	->
		io__write_char('['),
		term_io__write_term_2(ListHead, VarSet0, N0, VarSet1, N1),
		term_io__write_list_tail(ListTail, VarSet1, N1, VarSet, N),
		io__write_char(']')
	;
		{ Functor = term__atom("[]") },
		{ Args = [] }
	->
		io__write_string("[]"),
		{ N = N0 },
		{ VarSet = VarSet0 }
	;
		{ Functor = term__atom("{}") },
		{ Args = [BracedTerm] }
	->
		io__write_string("{ "),
		term_io__write_term_2(BracedTerm, VarSet0, N0, VarSet, N),
		io__write_string(" }")
	;
		% the empty functor '' is used for higher-order syntax:
		% Var(Arg, ...) gets parsed as ''(Var, Arg).  When writing
		% it out, we want to use the nice syntax.
		{ Functor = term__atom("") },
		{ Args = [term__variable(Var), FirstArg | OtherArgs] }
	->
		term_io__write_variable_2(Var, VarSet0, N0, VarSet1, N1),
		io__write_char('('),
		term_io__write_term_2(FirstArg, VarSet1, N1, VarSet2, N2),
		term_io__write_term_args(OtherArgs, VarSet2, N2, VarSet, N),
		io__write_char(')')
	;
		{ Args = [PrefixArg] },
		{ Functor = term__atom(OpName) },
		{ ops__lookup_prefix_op(OpTable, OpName,
			OpPriority, OpAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		term_io__write_constant(Functor),
		io__write_char(' '),
		{ adjust_priority(OpPriority, OpAssoc, NewPriority) },
		term_io__write_term_3(PrefixArg, NewPriority,
				VarSet0, N0, VarSet, N),
		maybe_write_char(')', Priority, OpPriority)
	;
		{ Args = [PostfixArg] },
		{ Functor = term__atom(OpName) },
		{ ops__lookup_postfix_op(OpTable, OpName,
			OpPriority, OpAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		{ adjust_priority(OpPriority, OpAssoc, NewPriority) },
		term_io__write_term_3(PostfixArg, NewPriority,
				VarSet0, N0, VarSet, N),
		io__write_char(' '),
		term_io__write_constant(Functor),
		maybe_write_char(')', Priority, OpPriority)
	;
		{ Args = [Arg1, Arg2] },
		{ Functor = term__atom(OpName) },
		{ ops__lookup_infix_op(OpTable, OpName,
			OpPriority, LeftAssoc, RightAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		{ adjust_priority(OpPriority, LeftAssoc, LeftPriority) },
		term_io__write_term_3(Arg1, LeftPriority,
				VarSet0, N0, VarSet1, N1),
		( { OpName = "," } ->
			io__write_string(", ")
		;
			io__write_char(' '),
			term_io__write_constant(Functor),
			io__write_char(' ')
		),
		{ adjust_priority(OpPriority, RightAssoc, RightPriority) },
		term_io__write_term_3(Arg2, RightPriority,
				VarSet1, N1, VarSet, N),
		maybe_write_char(')', Priority, OpPriority)
	;
		{ Args = [Arg1, Arg2] },
		{ Functor = term__atom(OpName) },
		{ ops__lookup_binary_prefix_op(OpTable, OpName,
			OpPriority, FirstAssoc, SecondAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		term_io__write_constant(Functor),
		io__write_char(' '),
		{ adjust_priority(OpPriority, FirstAssoc, FirstPriority) },
		term_io__write_term_3(Arg1, FirstPriority,
				VarSet0, N0, VarSet1, N1),
		io__write_char(' '),
		{ adjust_priority(OpPriority, SecondAssoc, SecondPriority) },
		term_io__write_term_3(Arg2, SecondPriority,
				VarSet1, N1, VarSet, N),
		maybe_write_char(')', Priority, OpPriority)
	;
		(
			{ Args = [] },
			{ Functor = term__atom(Op) },
			{ ops__lookup_op(OpTable, Op) },
			{ ops__max_priority(MaxPriority) },
			{ Priority =< MaxPriority }
		->
			io__write_char('('),
			term_io__write_constant(Functor),
			io__write_char(')')
		;
			term_io__write_constant(Functor)
		),
		(
			{ Args = [X|Xs] }
		->
			io__write_char('('),
			term_io__write_term_2(X, VarSet0, N0, VarSet1, N1),
			term_io__write_term_args(Xs, VarSet1, N1, VarSet, N),
			io__write_char(')')
		;
			{ N = N0,
			  VarSet = VarSet0 }
		)
	).

:- pred maybe_write_char(char, ops__priority, ops__priority,
		io__state, io__state).
:- mode maybe_write_char(in, in, in, di, uo) is det.

maybe_write_char(Char, Priority, OpPriority) -->
	( { OpPriority > Priority } ->
		io__write_char(Char)
	;
		[]
	).

:- pred adjust_priority(ops__priority, ops__assoc, ops__priority).
:- mode adjust_priority(in, in, out) is det.

adjust_priority(Priority, y, Priority).
adjust_priority(Priority, x, Priority - 1).

:- pred term_io__write_list_tail(term, varset, int, varset, int,
				io__state, io__state).
:- mode term_io__write_list_tail(in, in, in, out, out, di, uo) is det.

term_io__write_list_tail(Term, VarSet0, N0, VarSet, N) -->
	( 
		{ Term = term__variable(Id) },
		{ varset__search_var(VarSet0, Id, Val) }
	->
		term_io__write_list_tail(Val, VarSet0, N0, VarSet, N)
	;
		{ Term = term__functor(term__atom("."), [ListHead, ListTail], _) }
	->
		io__write_string(", "),
		term_io__write_term_2(ListHead, VarSet0, N0, VarSet1, N1),
		term_io__write_list_tail(ListTail, VarSet1, N1, VarSet, N)
	;
		{ Term = term__functor(term__atom("[]"), [], _) }
	->
		{ VarSet = VarSet0 },
		{ N = N0 }
	;
		io__write_string(" | "),
		term_io__write_term_2(Term, VarSet0, N0, VarSet, N)
	).

%-----------------------------------------------------------------------------%

:- pred term_io__write_term_args(list(term), varset, int, varset, int,
				io__state, io__state).
:- mode term_io__write_term_args(in, in, in, out, out, di, uo) is det.

	% write the remaining arguments
term_io__write_term_args([], VarSet, N, VarSet, N) --> [].
term_io__write_term_args([X|Xs], VarSet0, N0, VarSet, N) -->
	io__write_string(", "),
	term_io__write_term_2(X, VarSet0, N0, VarSet1, N1),
	term_io__write_term_args(Xs, VarSet1, N1, VarSet, N).

%-----------------------------------------------------------------------------%

	% write the functor
term_io__write_constant(term__integer(I)) -->
	io__write_int(I).
term_io__write_constant(term__float(F)) -->
	io__write_float(F).
term_io__write_constant(term__atom(A))  -->
	term_io__quote_atom(A).
term_io__write_constant(term__string(S)) -->
	term_io__quote_string(S).

%-----------------------------------------------------------------------------%

term_io__quote_char(C) -->
	io__write_char(''''),
	term_io__write_escaped_char(C),
	io__write_char('''').

term_io__quote_atom(S) -->
	(
		% I didn't make these rules up: see ISO Prolog 6.3.1.3
		% and 6.4.2.
		(
			% letter digit token (6.4.2)
			{ string__first_char(S, FirstChar, Rest) },
			{ char__is_lower(FirstChar) },
			{ string__is_alnum_or_underscore(Rest) }
		;
			% semicolon token (6.4.2)
			{ S = ";" }
		;
			% cut token (6.4.2)
			{ S = "!" }
		;
			% graphic token (6.4.2)
			{ string__to_char_list(S, Chars) },
			{ \+ (  list__member(Char, Chars),
				\+ lexer__graphic_token_char(Char)) },
			{ Chars \= [] }
		;
			% 6.3.1.3: atom = open list, close list ;
			{ S = "[]" }
		;
			% 6.3.1.3: atom = open curly, close curly ;
			{ S = "{}" }
		)
	->
		io__write_string(S)
	;
		% anything else must be output as a quoted token (6.4.2)
		io__write_char(''''),
		term_io__write_escaped_string(S),
		io__write_char('''')
	).

term_io__quote_string(S) -->
	io__write_char('"'),
	term_io__write_escaped_string(S),
	io__write_char('"').

term_io__write_escaped_string(String) -->
	string__foldl(term_io__write_escaped_char, String).

term_io__quote_single_char(Char) -->
	term_io__write_escaped_char(Char).

term_io__write_escaped_char(Char) -->
	( { mercury_escape_special_char(Char, QuoteChar) } ->
		io__write_char('\\'),
		io__write_char(QuoteChar)
	; { is_mercury_source_char(Char) } ->
		io__write_char(Char)
	;
		{ mercury_escape_char(Char, String) },
		io__write_string(String)
	).

:- pred mercury_escape_char(char, string).
:- mode mercury_escape_char(in, out) is det.

	% Convert a character to the corresponding octal escape code.

	% XXX Note that we use C-style octal escapes rather than ISO-Prolog
	% octal escapes.  This is for backwards compatibility with
	% NU-Prolog and (old versions of?) SICStus Prolog.
	% The Mercury lexer accepts either, so this should work
	% ok so long as you don't have two escaped characters
	% in a row :-(

mercury_escape_char(Char, EscapeCode) :-
	char__to_int(Char, Int),
	string__int_to_base_string(Int, 8, OctalString0),
	string__pad_left(OctalString0, '0', 3, OctalString),
	string__first_char(EscapeCode, '\\', OctalString).

:- pred is_mercury_source_char(char).
:- mode is_mercury_source_char(in) is semidet.

	% Succeed if Char is a character which is allowed in
	% Mercury string and character literals.

is_mercury_source_char(Char) :-
	( char__is_alnum(Char) ->
		true
	; is_mercury_punctuation_char(Char) ->
		true
	;
		fail
	).

	% Currently we only allow the following characters.
	% XXX should we just use is_printable(Char) instead?

:- pred is_mercury_punctuation_char(char).
:- mode is_mercury_punctuation_char(in) is semidet.

is_mercury_punctuation_char(' ').
is_mercury_punctuation_char('!').
is_mercury_punctuation_char('@').
is_mercury_punctuation_char('#').
is_mercury_punctuation_char('$').
is_mercury_punctuation_char('%').
is_mercury_punctuation_char('^').
is_mercury_punctuation_char('&').
is_mercury_punctuation_char('*').
is_mercury_punctuation_char('(').
is_mercury_punctuation_char(')').
is_mercury_punctuation_char('-').
is_mercury_punctuation_char('_').
is_mercury_punctuation_char('+').
is_mercury_punctuation_char('=').
is_mercury_punctuation_char('`').
is_mercury_punctuation_char('~').
is_mercury_punctuation_char('{').
is_mercury_punctuation_char('}').
is_mercury_punctuation_char('[').
is_mercury_punctuation_char(']').
is_mercury_punctuation_char(';').
is_mercury_punctuation_char(':').
is_mercury_punctuation_char('''').
is_mercury_punctuation_char('"').
is_mercury_punctuation_char('<').
is_mercury_punctuation_char('>').
is_mercury_punctuation_char('.').
is_mercury_punctuation_char(',').
is_mercury_punctuation_char('/').
is_mercury_punctuation_char('?').
is_mercury_punctuation_char('\\').
is_mercury_punctuation_char('|').

%-----------------------------------------------------------------------------%

	% mercury_escape_special_char(Char, EscapeChar)
	% is true iff Char is character for which there is a special
	% backslash-escape character EscapeChar that can be used
	% after a backslash in string literals or atoms to represent Char.

:- pred mercury_escape_special_char(char, char).
:- mode mercury_escape_special_char(in, out) is semidet.

mercury_escape_special_char('''', '''').
mercury_escape_special_char('"', '"').
mercury_escape_special_char('\\', '\\').
mercury_escape_special_char('\n', 'n').
mercury_escape_special_char('\t', 't').
mercury_escape_special_char('\b', 'b').

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

term_io__write_term_nl(VarSet, Term) -->
	term_io__write_term(VarSet, Term),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
