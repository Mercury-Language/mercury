%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: c_util.m
% Main author: fjh.

% This module defines utility routines that are useful when
% generating and/or emitting C code.  Changes to this module may require
% changes to be made to java_util.m

%-----------------------------------------------------------------------------%

:- module backend_libs__c_util.
:- interface.
:- import_module io, char, string, int.
:- import_module backend_libs__builtin_ops.

%-----------------------------------------------------------------------------%

	% set_line_num(FileName, LineNum):
	%	emit a #line directive to set the specified filename & linenum
	%	so that C compiler error messages etc. will refer to the
	%	correct location in the original source file location.
:- pred c_util__set_line_num(string, int, io__state, io__state).
:- mode c_util__set_line_num(in, in, di, uo) is det.

	%	emit a #line directive to cancel the effect of any previous
	%	#line directives, so that C compiler error messages etc. will
	%	refer to the appropriate location in the generated .c file.
:- pred c_util__reset_line_num(io__state, io__state).
:- mode c_util__reset_line_num(di, uo) is det.

%-----------------------------------------------------------------------------%

	% Print out a string suitably escaped for use as a C string literal.
	% This doesn't actually print out the enclosing double quotes --
	% that is the caller's responsibility.
:- pred c_util__output_quoted_string(string, io__state, io__state).
:- mode c_util__output_quoted_string(in, di, uo) is det.

	% output_quoted_multi_string is like output_quoted_string
	% except that the string may contain embedded NUL characters
	% (i.e. '\0').  The int specifies the length of the string.
:- type multi_string == string.
:- pred c_util__output_quoted_multi_string(int, multi_string,
		io__state, io__state).
:- mode c_util__output_quoted_multi_string(in, in, di, uo) is det.

	% Convert a string to a form that is suitably escaped for use as a
	% C string literal.  This doesn't actually add the enclosing double
	% quotes -- that is the caller's responsibility.
:- pred c_util__quote_string(string, string).
:- mode c_util__quote_string(in, out) is det.

	% Convert a character to a form that is suitably escaped for use as a
	% C character literal.  This doesn't actually add the enclosing double
	% quotes -- that is the caller's responsibility.
:- pred c_util__quote_char(char, char).
:- mode c_util__quote_char(in, out) is semidet.

%-----------------------------------------------------------------------------%
%
% The following predicates all take as input an operator,
% check if it is an operator of the specified kind,
% and if so, return the name of the corresponding C operator
% that can be used to implement it.
%

	% The operator returned will be <, >, etc.;
	% it can be used in the form `strcmp(<Arg1>, <Arg2>) <Op> 0'.
	% 
:- pred c_util__string_compare_op(binary_op, string).
:- mode c_util__string_compare_op(in, out) is semidet.

	% The operator returned will be +, *, etc.;
	% the arguments should be floats and the result will be a float.
:- pred c_util__float_op(binary_op, string).
:- mode c_util__float_op(in, out) is semidet.

	% The operator returned will be <, >, etc.;
	% the arguments should be floats and the result will be a boolean.
:- pred c_util__float_compare_op(binary_op, string).
:- mode c_util__float_compare_op(in, out) is semidet.

	% The operator returned will be an infix operator.
	% The arguments should be cast to MR_Unsigned,
	% and the result will be a boolean.
:- pred c_util__unsigned_compare_op(binary_op, string).
:- mode c_util__unsigned_compare_op(in, out) is semidet.

	% The operator returned will be either a prefix operator
	% or a macro or function name.  The operand needs
	% to be placed in parentheses after the operator name.
:- pred c_util__unary_prefix_op(unary_op, string).
:- mode c_util__unary_prefix_op(in, out) is det.

	% The operator returned will be an infix operator.
	% The arguments should be integer or booleans
	% and the result will be an integer or a boolean.
:- pred c_util__binary_infix_op(binary_op, string).
:- mode c_util__binary_infix_op(in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module libs__globals, libs__options.
:- import_module list, bool.

%-----------------------------------------------------------------------------%

c_util__set_line_num(File, Line) -->
	globals__io_lookup_bool_option(line_numbers, LineNumbers),
	(
		{ Line > 0 },
		{ File \= "" },
		{ LineNumbers = yes }
	->
		io__write_string("#line "),
		io__write_int(Line),
		io__write_string(" """),
		c_util__output_quoted_string(File),
		io__write_string("""\n")
	;
		[]
	).

c_util__reset_line_num -->
	% We want to generate another #line directive to reset the C compiler's
	% idea of what it is processing back to the file we are generating.
	io__get_output_line_number(Line),
	io__output_stream_name(FileName),
	globals__io_lookup_bool_option(line_numbers, LineNumbers),
	(
		{ Line > 0 },
		{ FileName \= "" },
		{ LineNumbers = yes }
	->
		io__write_string("#line "),
		{ NextLine is Line + 1 },
		io__write_int(NextLine),
		io__write_string(" """),
		c_util__output_quoted_string(FileName),
		io__write_string("""\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

c_util__output_quoted_string(S0) -->
	c_util__output_quoted_multi_string(string__length(S0), S0).

c_util__output_quoted_multi_string(Len, S) -->
	c_util__output_quoted_multi_string_2(0, Len, S).

:- pred c_util__output_quoted_multi_string_2(int::in, int::in, string::in,
	io__state::di, io__state::uo) is det.

c_util__output_quoted_multi_string_2(Cur, Len, S) -->
	( { Cur < Len } ->
			% Avoid a limitation in the MSVC compiler where
			% string literals can be no longer then 2048
			% chars.  However if you output the string in
			% chunks, eg "part a" "part b" it will accept a
			% string longer then 2048 chars, go figure!
		( { Cur \= 0, Cur mod 512 = 0 } ->
			io__write_string("\" \"")
		;
			[]
		),

			% we must use unsafe index, because we want to be able
			% to access chars beyond the first NUL
		{ string__unsafe_index(S, Cur, Char) },
		( { char__to_int(Char, 0) } ->
			io__write_string("\\0")
		; { c_util__quote_char(Char, QuoteChar) } ->
			io__write_char('\\'),
			io__write_char(QuoteChar)
		;
			io__write_char(Char)
		),
		output_quoted_multi_string_2(Cur + 1, Len, S)
	;
		[]
	).

c_util__quote_string(String, QuotedString) :-
	QuoteOneChar = (pred(Char::in, RevChars0::in, RevChars::out) is det :-
		( c_util__quote_char(Char, QuoteChar) ->
			RevChars = [QuoteChar, '\\' | RevChars0]
		;
			RevChars = [Char | RevChars0]
		)),
	string__foldl(QuoteOneChar, String, [], RevQuotedChars),
	string__from_rev_char_list(RevQuotedChars, QuotedString).

c_util__quote_char('"', '"').
c_util__quote_char('\\', '\\').
c_util__quote_char('\n', 'n').
c_util__quote_char('\t', 't').
c_util__quote_char('\b', 'b').

%-----------------------------------------------------------------------------%

c_util__unary_prefix_op(mktag,			"MR_mktag").
c_util__unary_prefix_op(tag,			"MR_tag").
c_util__unary_prefix_op(unmktag,		"MR_unmktag").
c_util__unary_prefix_op(mkbody,			"MR_mkbody").
c_util__unary_prefix_op(unmkbody,		"MR_unmkbody").
c_util__unary_prefix_op(strip_tag,		"MR_strip_tag").
c_util__unary_prefix_op(hash_string,		"MR_hash_string").
c_util__unary_prefix_op(bitwise_complement,	"~").
c_util__unary_prefix_op(not,			"!").

c_util__string_compare_op(str_eq, "==").
c_util__string_compare_op(str_ne, "!=").
c_util__string_compare_op(str_le, "<=").
c_util__string_compare_op(str_ge, ">=").
c_util__string_compare_op(str_lt, "<").
c_util__string_compare_op(str_gt, ">").

c_util__unsigned_compare_op(unsigned_le, "<=").

c_util__float_op(float_plus, "+").
c_util__float_op(float_minus, "-").
c_util__float_op(float_times, "*").
c_util__float_op(float_divide, "/").

c_util__float_compare_op(float_eq, "==").
c_util__float_compare_op(float_ne, "!=").
c_util__float_compare_op(float_le, "<=").
c_util__float_compare_op(float_ge, ">=").
c_util__float_compare_op(float_lt, "<").
c_util__float_compare_op(float_gt, ">").

c_util__binary_infix_op(+, "+").
c_util__binary_infix_op(-, "-").
c_util__binary_infix_op(*, "*").
c_util__binary_infix_op(/, "/").
c_util__binary_infix_op(<<, "<<").
c_util__binary_infix_op(>>, ">>").
c_util__binary_infix_op(&, "&").
c_util__binary_infix_op('|', "|").
c_util__binary_infix_op(^, "^").
c_util__binary_infix_op(mod, "%").
c_util__binary_infix_op(eq, "==").
c_util__binary_infix_op(ne, "!=").
c_util__binary_infix_op(and, "&&").
c_util__binary_infix_op(or, "||").
c_util__binary_infix_op(<, "<").
c_util__binary_infix_op(>, ">").
c_util__binary_infix_op(<=, "<=").
c_util__binary_infix_op(>=, ">=").

%-----------------------------------------------------------------------------%
