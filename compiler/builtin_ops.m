%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% builtin_ops.m -- defines the builtin operator types.
% Main author: fjh.
%
% This module defines various types which enumerate the different builtin
% operators.  Several of the different back-ends -- the bytecode back-end,
% the LLDS, and the MLDS -- all use the same set of builtin operators.
% These operators are defined here.

%-----------------------------------------------------------------------------%

:- module builtin_ops.
:- interface.

:- type unary_op
	--->	mktag
	;	tag
	;	unmktag
	;	mkbody
	;	body
	;	unmkbody
	;	cast_to_unsigned
	;	hash_string
	;	bitwise_complement
	;	(not).

:- type binary_op
	--->	(+)	% integer arithmetic
	;	(-)
	;	(*)
	;	(/)	% integer division
			% assumed to truncate toward zero
	;	(mod)	% remainder (w.r.t. truncating integer division)
			% XXX `mod' should be renamed `rem'
	;	(<<)	% unchecked left shift
	;	(>>)	% unchecked right shift
	;	(&)	% bitwise and
	;	('|')	% bitwise or
	;	(^)	% bitwise xor
	;	(and)	% logical and
	;	(or)	% logical or
	;	eq	% ==
	;	ne	% !=
	;	array_index
	;	str_eq	% string comparisons
	;	str_ne
	;	str_lt
	;	str_gt
	;	str_le
	;	str_ge
	;	(<)	% integer comparions
	;	(>)
	;	(<=)
	;	(>=)
	;	float_plus
	;	float_minus
	;	float_times
	;	float_divide
	;	float_eq
	;	float_ne
	;	float_lt
	;	float_gt
	;	float_le
	;	float_ge.

%-----------------------------------------------------------------------------%
