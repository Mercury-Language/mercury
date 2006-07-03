%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: token_io.m
% main author: philip

:- module ztoken_io.
:- interface.
:- import_module io, ztoken, word.

:- type ztoken_result ---> eof ; error(string) ; ok(ztoken_list).

:- pred readTokenList(operators, ztoken_result, io__state, io__state).
:- mode readTokenList(in, out, di, uo) is det.

:- pred writeTokenList(ztoken_list, io__state, io__state).
:- mode writeTokenList(in, di, uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.
:- import_module char, require, maybe, pair, int, list, string, higher_order.

% :- pred main(io__state::di, io__state::uo) is det.
% main --> readTokenList(TS), writeTokenList(TS).

% :- pred lerror(list(string), io__state, io__state).
% :- mode lerror(in, di, uo) is det.
% lerror(L) -->
% 	io__input_stream_name(F),
% 	io__get_line_number(LN),
% 	{string__int_to_string(LN, LNS)},
% 	io__write_strings([F, "(", LNS, ")", ":"| L]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Global state (and predicates for operating on it)
%

:- type lex_state ---> in_Z ; out_of_Z.

:- type globals --->
	globals(
		ztoken_list,
		maybe(pair(ztoken_list, lex_state)),
		operators
		).

:- pred get_globals(globals::out, io__state::di, io__state::uo) is det.
get_globals(G) -->
	io__get_globals(G0),
	{ univ_to_type(G0, G1) ->
		G = G1
	;	error("get_globals/3---univ_to_type failed.")
	}.

:- pred init_globals(operators, io__state, io__state).
:- mode init_globals(in, di, uo) is det.
init_globals(O) --> set_globals(globals([], no, O)).

:- pred set_globals(globals::in, io__state::di, io__state::uo) is det.
set_globals(G) -->
	{unsafe_promise_unique(G, G1),
	type_to_univ(G1, G2)},
	io__set_globals(G2).

% BUG: add_token uses the wrong line number (+1) if the token ends at
% end-of-line because the input is on the next line when add_token is called.
:- pred add_token(ztoken::in, io__state::di, io__state::uo) is det.
add_token(T) -->
	io__get_line_number(LN),
	get_globals(globals(TL, P, O)),
	set_globals(globals([T-LN|TL], P, O)).

:- pred get_operat_state(operators::out, io__state::di, io__state::uo) is det.
get_operat_state(O) --> get_globals(globals(_, _, O)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
readTokenList(Ops, Result) -->
	init_globals(Ops),
	io__read_char(C),
	ztokens(C, Result).
	% get_globals(G), io__write(G), io__write_string("\n"),

:- pred finished(char::in, ztoken_result::out,
					io__state::di, io__state::uo) is det.
finished(C, ok(TL)) -->
	io__putback_char(C),
	get_globals(globals(TL0, _, _)),
	{reverse_removing_soft_newlines(TL0, TL)}.

% The \verb and \verb* commands and the verbatim and verbatim* environments
% should be added.
% Lines with spaces only indicate end-of-paragraph

:- type tstate ---> in_block ; in_pragma.

:- pred ztokens(io__result(char), ztoken_result, io__state, io__state).
:- mode ztokens(in, out, di, uo) is det.
ztokens(eof, eof) -->
	[].
ztokens(error(M), error(S)) -->
	{io__error_message(M, S)}.
ztokens(ok(C), Result) -->
	( {C = '\n'} ->
		check_start_pragma(C1, YesOrNo),		% FIX THIS
		( {YesOrNo = yes},
			in_ztokens(in_pragma, C1, Result)
		; {YesOrNo = no},
			ztokens(C1, Result)
		)
	; {C = '%'} ->
		comment(C1), ztokens(C1, Result)
	; {C = ('\\')} ->
		next_lex("begin", F, C2),
		( {F = was_found},
			latex_arg("begin", C2, Arg0, C1),
			{string__from_char_list(Arg0, Arg)},
			( {start_of_Z(Arg, T)} ->
				add_token(T),
				in_ztokens(in_block, C1, Result)
			;	ztokens(C1, Result)
			)
		; {F = not_found},
			ztokens(C2, Result)
		)
	;	io__read_char(C1),
		ztokens(C1, Result)
	).

:- pred in_ztokens(tstate, io__result(char), ztoken_result,
							io__state, io__state).
:- mode in_ztokens(in, in, out, di, uo) is det.
in_ztokens(_, eof, error("End of file before end of paragraph")) -->
	[].
in_ztokens(_, error(M), error(S)) -->
	{io__error_message(M, S)}.
in_ztokens(State, ok(C), Result) -->
	( {C = '\n'} ->
		( {State = in_block},
			io__read_char(C1), in_ztokens(State, C1, Result)
		; {State = in_pragma},
			add_token(zEND),
			finished('\n', Result)
		)
	; {C = '%'} ->
		comment(C1), in_ztokens(State, C1, Result)
	; {ztoken_io__is_whitespace(C)} ->
		io__read_char(C1), in_ztokens(State, C1, Result)
	; {C = ('\\')} ->
		escape(MT, C1),
		( {MT = no},
			in_ztokens(State, C1, Result)
		; {MT = yes(T)},
			add_token(T),
			( {C1 = ok(Char1), T = zEND} ->
				finished(Char1, Result)
			;	in_ztokens(State, C1, Result)
			)
		)
	; {char__is_alpha(C)} ->
		word([C], T, C1),
		add_token(T),
		in_ztokens(State, C1, Result)
	; {char__is_digit(C)} ->
		digits(L, C1),
		{string__from_char_list([C|L], S)},
		add_token(number(S)),
		in_ztokens(State, C1, Result)
	; {is_predecoration(C)} ->
		decoration(C, C1, [], D),
		add_token(decoration(D)),
		in_ztokens(State, C1, Result)
	; {char_switch(C, T)} ->
		io__read_char(C1),
		add_token(T),
		in_ztokens(State, C1, Result)
	; {symbol(C)} ->
		symbol(L, C1),
		{string__from_char_list([C|L], S)},
		special_word(S, [], T),
		add_token(T),
		in_ztokens(State, C1, Result)
	;
		{char__to_int(C, Code),
		string__int_to_base_string(Code, 10, Decimal),
		string__int_to_base_string(Code, 16, Hex),
		string__append_list([
			"Illegal input character 0x", Hex, " (", Decimal, ")"],
			Error),
		Result = error(Error)}
	).

:- pred is_predecoration(char::in) is semidet.
is_predecoration('!').
is_predecoration('?').
is_predecoration('''').
is_predecoration('_').

:- pred decoration(io__result(char), io__result(char), list(stroke),
							io__state, io__state).
:- mode decoration(in, out, out, di, uo) is det.
decoration(C0, C, L) -->
	( {C0 = ok(DC), is_predecoration(DC)} ->
		decoration(DC, C, [], L)
	;	{C = C0, L = []}
	).

:- pred decoration(char, io__result(char), list(stroke), list(stroke),
							io__state, io__state).
:- mode decoration(in, out, in, out, di, uo) is det.
decoration(DC, C, L0, L) -->
	( {DC = '!'} ->
		{M = no, S = exclamation_mark}
	; {DC = '?'} ->
		{M = no, S = question_mark}
	; {DC = ''''} ->
		{M = no, S = prime}
	; {DC = '_'} ->
		io__read_char(C0),
		( {C0 = ok(C1)} -> %, char__is_alnum(C1)} ->
			{M = no, string__char_to_string(C1, S1)}
		;	{M = yes(C0), S1 = ""}
		),
		{S = subscript(S1)}
	;	{error("decoration/6---impossible decoration character")}
	),
	( {M = no}, io__read_char(C2);  {M = yes(C2)} ),
	{L1 = [S|L0]},
	( {C2 = ok(C3), is_predecoration(C3)} ->
		decoration(C3, C, L1, L)
	;	{C = C2, L = L1}
	).

:- pred is_whitespace(char::in) is semidet.
is_whitespace(' ').
is_whitespace('~').
is_whitespace('&').
is_whitespace('\t').
is_whitespace('\r').
is_whitespace('\f').
is_whitespace('\v').

:- type yes_or_no ---> yes ; no.

:- pred check_start_pragma(io__result(char), yes_or_no, io__state, io__state).
:- mode check_start_pragma(out, out, di, uo) is det.
check_start_pragma(C, Result) -->
	io__read_char(C0),
	( {C0 = ok('%')} ->
		io__read_char(C1),
		( {C1 = ok(Char1)} ->
			( {Char1 = '\n'} ->
				{C = C1, Result = no} % need to see newline
			; {Char1 = '%'} ->
				alpha(L, C2),
				% ( {L = [], C2 = ok(Char2)} ->
				% 	( {Char2 = ' ';Char2 = '\t'} ->
				% 		io__read_char(C),
				% 		{Result = yes}
				% 	;	comment(C),
				% 		{Result = no}
				% 	)
				( {string__from_char_list(L, S),pragma(S, P)} ->
					add_token(pragma(P)),
					{C = C2, Result = yes}
				;	comment(C), {Result = no}
				)
			;	comment(C), {Result = no}
			)
		;	{C = C1, Result = no}
		)
	;	{C = C0, Result = no}
	).

:- pred comment(io__result(char)::out, io__state::di, io__state::uo) is det.
comment(Char) -->
	io__read_line(L0),
	{ L0 = error(I), Char = error(I)
	; L0 = eof, Char = eof
	; L0 = ok(L1),
		( list__remove_suffix(L1, ['\n'], _) ->
			Char = ok('\n')
		;	Char = eof
		)
	}.

:- pred word(list(char), ztoken, io__result(char), io__state, io__state).
:- mode word(in, out, out, di, uo) is det.
word(L, T, R) -->
	io__read_char(R0),
	( {R0 = ok(C0), C0 = ('\\')} ->
		io__read_char(R1),
		( {R1 = ok(C1)} ->
			( {C1 = '_'} ->
				word(['_'|L], T, R)
			;	io__putback_char(C1),
				word_chars_to_token(L, [], T),
				{R = R0}
			)
		;	{T = newline, R = R1} % Arbit. token returned for error
		)
	; {R0 = ok(C0), char__is_alnum(C0)} ->
		word([C0|L], T, R)
	;	decoration(R0, R, D),
		word_chars_to_token(L, D, T)
	).

:- pred word_chars_to_token(list(char), decoration, ztoken,
							io__state, io__state).
:- mode word_chars_to_token(in, in, out, di, uo) is det.
word_chars_to_token(L, D, T) -->
	{list__reverse(L, L1),
	string__from_char_list(L1, S)},
	special_word(S, D, T).

:- pred digits(list(char), io__result(char), io__state, io__state).
:- mode digits(out, out, di, uo) is det.
digits(L, C1) -->
	io__read_char(C0),
	( {C0 = ok(C), char__is_digit(C)} ->
		{L = [C|L1]}, digits(L1, C1)
	;	{L = [], C1 = C0}
	).

:- pred special_word(string, decoration, ztoken, io__state, io__state).
:- mode special_word(in, in, out, di, uo) is det.
special_word(S, D, T) -->
	{I = id(no, S, D)},
	( {I = id(no, "\\also", [])} ->	% special case because
		{T = newline}		% \also is an alias for \\
	; {keyword(_, I, T0)} ->
		{T = T0}
	; 	get_operat_state(OM),
		{ search_operators(OM, I, Op) -> T = op(Op, I) ; T = name(I) }
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred escape(maybe(ztoken), io__result(char), io__state, io__state).
:- mode escape(out, out, di, uo) is det.

escape(T, C) -->
	io__read_char(C1),
	( {C1 = ok(Char1)} ->
		( {char__is_alpha(Char1)} ->
			alpha(L, C2),
			{string__from_char_list(['\\',Char1|L], S)},
			( {S = "\\end"} ->	% what if \end in pragma??
				% latex_arg(C2, L, C), %should check that this
				% {string__from_char_list(L, S)},%matches \begin
				{T = yes(zEND), C = C2}
			; {S = "\\quad"} ->	% em sized space
				{T = no, C = C2}
			; {S = "\\qquad"} ->	% 2 em sized space
				{T = no, C = C2}
			; {S = "\\t"} -> 	% \t is a tab which may be
				{T = no},	% followed by a number
				( {C2 = ok(C3), char__is_digit(C3)} ->
					digits(_, C)
				;	{C = C2}
				)
			;	% ( {S = "\\inrel"} ->
				%     	latex_arg("inrel", C2, Arg0, C),
				% 	{string__from_char_list(Arg0, Arg),
				% 	T1 = op(inrel, id(no, Arg, []))}
				% ;
					decoration(C2, C, D),
					special_word(S, D, T1),
				{T = yes(T1)}
				%)
			)
		;	io__read_char(C),
			{ escape_switch(Char1, T0) ->
				T = yes(T0)
			;	string__from_char_list(['\\', Char1], S),
				T = yes(name(id(no, S, [])))
			}
		)
	;	{T = no, C = C1}
	).

:- pred escape_switch(char::in, ztoken::out) is semidet.
escape_switch('\\', newline).
escape_switch('_', underscore).
escape_switch('{', zSETBRA).
escape_switch('}', zSETKET).

:- pred alpha(list(char), io__result(char), io__state, io__state).
:- mode alpha(out, out, di, uo) is det.
alpha(L, C1) -->
	io__read_char(C0),
	( {C0 = ok(C), char__is_alpha(C)} ->
		{L = [C|L1]}, alpha(L1, C1)
	;	{L = [], C1 = C0}
	).

:- pred symbol(list(char), io__result(char), io__state, io__state).
:- mode symbol(out, out, di, uo) is det.
symbol(L, C1) -->
	io__read_char(C0),
	( {C0 = ok(C), symbol(C)} ->
		{L = [C|L1]}, symbol(L1, C1)
	;	{L = [], C1 = C0}
	).

% :- pred alpha1(list(char), io__result(char), io__state, io__state).
% :- mode alpha1(out, out, di, uo) is det.
% alpha1(L, C1) --> lex_string(char__is_alpha, L, C1).
% 
% :- pred lex_string(pred(char), list(char), io__result(char),
% 							io__state, io__state).
% :- mode lex_string(pred(in) is semidet, out, out, di, uo) is det.
% lex_string(P, L, C1) -->
% 	io__read_char(C0),
% 	( {C0 = ok(C), call(P, C)} ->
% 		{L = [C|L1]}, lex_string(P, L1, C1)
% 	;	{L = [], C1 = C0}
% 	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred start_of_Z(string::in, ztoken::out) is semidet.
start_of_Z("zed", zBEGIN).
start_of_Z("syntax", zBEGIN).
start_of_Z("axdef", zAX).
start_of_Z("schema", zSCH).
start_of_Z("gendef", zGEN).

:- type found ---> was_found ; not_found.

:- pred next_lex(string, found, io__result(char), io__state, io__state).
:- mode next_lex(in, out, out, di, uo) is det.
next_lex(S, F, C) -->
	io__read_char(C0),
	( {string__first_char(S, H, T)} ->
		( {C0 = ok(H)} -> next_lex(T, F, C) ; {F = not_found, C = C0} )
	;	{F = was_found, C = C0}
	).

:- pred latex_arg(string, io__result(char), list(char), io__result(char),
							io__state, io__state).
:- mode latex_arg(in, in, out, out, di, uo) is det.
latex_arg(S, C0, L, C) -->
	{C1 = C0}, % eat_white_space(C0, C1),
	( {C1 = ok('{')} ->
		upto_close_brace(S, L, C)
	;	{L = [], C = C1}
	).

:- pred upto_close_brace(string, list(char), io__result(char),
							io__state, io__state).
:- mode upto_close_brace(in, out, out, di, uo) is det.
upto_close_brace(S, L, C1) -->
	io__read_char(C0),
	( {C0 = ok(C)} ->
		( {C = '}'} ->
			{L = []}, io__read_char(C1)
		;	{L = [C|L1]}, upto_close_brace(S, L1, C1)
		)
	;	{L = [], C1 = C0}
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred symbol(char::in) is semidet.
symbol(',').
symbol('.').
symbol('/').
symbol(':').
symbol(';').
symbol('=').
symbol('[').
symbol(']').
symbol('|').

symbol('`').
symbol('-').
symbol('*').
symbol('@').
symbol('+').
symbol('>').
symbol('<').

:- pred char_switch(char::in, ztoken::out) is semidet.
char_switch('(', zBRA).
char_switch(')', zKET).
char_switch('{', left_brace).
char_switch('}', right_brace).
char_switch('^', caret).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTPUT

writeTokenList(L) -->
	{SL = string_portray_list(ztokenToString, "[\n", ",\n", "\n]\n", L)},
	io__write_string(SL).

:- func ztokenToString(pair(ztoken, zcontext)) = string.
ztokenToString(T-_) = S :-
	( T = name(I) ->
		string__append_list(["name('", identPortray(I), "')"], S)
	; T = op(Op, I) ->
		S0 = op_to_string(Op),
		string__append_list(["op(",S0,", '",identPortray(I),"')"],S)
	; T = number(S0) -> string__append_list(["number(\"", S0, "\")"], S)
	; T = decoration(D) ->
		string__append_list(strokeLPortray(D), S0),
		string__append_list(["decoration(\"", S0, "\")"], S)
	; T = string(S0) -> string__append_list(["string(\"", S0, "\")"], S)
	; T = pragma(P) ->
		pragma(S0, P), string__append_list(["pragma(\"", S0, "\")"], S)
	; keyword(b, I, T) -> S = identPortray(I)
	; error("impossible token in ztokenToString")
	).
