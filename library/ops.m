%---------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: ops.m.
% main author: fjh.
% stability: low.
%
% This module exports a typeclass `ops__op_table' which is used to define
% operator precedence tables for use by `parser__read_term_with_op_table'
% and `term_io__write_term_with_op_table'.
%
% It also exports an instance `ops__mercury_op_table' which implements the
% Mercury operator table defined in the Mercury Language Reference Manual.
%
% See samples/calculator2.m for an example program.
%
% XXX In the current implementation the table of Mercury operators
% is fixed and cannot be modified at run-time.
%
%-----------------------------------------------------------------------------%

:- module ops.
:- interface.

:- typeclass ops__op_table(Table) where [

		% Check whether a string is the name of an infix operator,
		% and if it is, return its precedence and associativity.
	pred ops__lookup_infix_op(Table, string, ops__priority,
			ops__assoc, ops__assoc),
	mode ops__lookup_infix_op(in, in, out, out, out) is semidet,

		% Operator terms are terms of the form `X `Op` Y',
		% where `Op' is a variable or a name and `X' and `Y'
		% are terms. If operator terms are included in `Table',
		% return their precedence and associativity.
	pred ops__lookup_operator_term(Table, ops__priority,
			ops__assoc, ops__assoc),
	mode ops__lookup_operator_term(in, out, out, out) is semidet,

		% Check whether a string is the name of a prefix operator,
		% and if it is, return its precedence and associativity.
	pred ops__lookup_prefix_op(Table, string, ops__priority, ops__assoc),
	mode ops__lookup_prefix_op(in, in, out, out) is semidet,

		% Check whether a string is the name of a binary prefix
		% operator, and if it is, return its precedence and
		% associativity.
	pred ops__lookup_binary_prefix_op(Table, string,
			ops__priority, ops__assoc, ops__assoc),
	mode ops__lookup_binary_prefix_op(in, in, out, out, out) is semidet,
		
		% Check whether a string is the name of a postfix operator,
		% and if it is, return its precedence and associativity.
	pred ops__lookup_postfix_op(Table, string, ops__priority, ops__assoc),
	mode ops__lookup_postfix_op(in, in, out, out) is semidet,

		% Check whether a string is the name of an operator
	pred ops__lookup_op(Table, string),
	mode ops__lookup_op(in, in) is semidet,

		% Returns the highest priority number (the lowest is zero).
	func ops__max_priority(Table) = ops__priority,

		% The maximum priority of an operator appearing
		% as the top-level functor of an argument of a compound
		% term.
		%
		% This will generally be the precendence of `,/2' less one.
		% If `,/2' does not appear in the op_table,
		% `ops__max_priority' plus one may be a reasonable value.
	func ops__arg_priority(Table) = ops__priority
].

%-----------------------------------------------------------------------------%

	% The table of Mercury operators.
	% See the "Builtin Operators" section of the "Syntax" chapter
	% of the Mercury Language Reference Manual for details.
:- type ops__mercury_op_table.
:- instance ops__op_table(ops__mercury_op_table).

:- func ops__init_mercury_op_table = ops__mercury_op_table.
:- mode ops__init_mercury_op_table = uo is det.

%-----------------------------------------------------------------------------%

	% Operators with a low "priority" bind more tightly than those
	% with a high "priority". For example, given that `+' has
	% priority 500 and `*' has priority 400, the term `2 * X + Y'
	% would parse as `(2 * X) + Y'.
	%
	% The lowest priority is 0.
:- type ops__priority == int.

%-----------------------------------------------------------------------------%

	% An ops__specifier describes what structure terms
	% constructed with an operator are allowed to take.
	% `f' represents the operator and `x' and `y' represent arguments.
	% `x' represents an argument whose priority must be
	% strictly lower that that of the operator.
	% `y' represents an argument whose priority is
	% lower or equal to that of the operator.
	% For example, `yfx' indicates a left-associative infix operator,
	% while `xfy' indicates a right-associative infix operator.
:- type ops__specifier
	--->	fx ; fy ; xf ; yf ; xfx ; yfx ; xfy ; fxx ; fxy ; fyx.

:- type ops__assoc
	--->	x ; y.

:- type ops__class
	--->	infix(ops__assoc, ops__assoc)
	;	prefix(ops__assoc)
	;	binary_prefix(ops__assoc, ops__assoc)
	;	postfix(ops__assoc).

	% convert an ops__specifer (e.g. `xfy') to an ops__class
	% (e.g. `infix(x, y)').
:- pred ops__op_specifier_to_class(ops__specifier, ops__class).
:- mode ops__op_specifier_to_class(in, out) is det.
% :- mode ops__op_specifier_to_class(out, in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

% Anything below here is not documented in the library reference manual.

:- interface.

	% 
	% The Mercury operator table used to be the only one allowed.
	% The old names are no longer appropriate.
	%

:- type ops__table == ops__mercury_op_table.

	% create an op_table with the standard Mercury operators.
:- pred ops__init_op_table(ops__table).
:- mode ops__init_op_table(uo) is det.
:- pragma obsolete(ops__init_op_table/1).

:- func ops__init_op_table = ops__table.
:- pragma obsolete(ops__init_op_table/0).

%-----------------------------------------------------------------------------%

:- implementation.

ops__op_specifier_to_class(fx, prefix(x)).
ops__op_specifier_to_class(fy, prefix(y)).
ops__op_specifier_to_class(xf, postfix(x)).
ops__op_specifier_to_class(yf, postfix(y)).
ops__op_specifier_to_class(xfx, infix(x,x)).
ops__op_specifier_to_class(yfx, infix(y,x)).
ops__op_specifier_to_class(xfy, infix(x,y)).
ops__op_specifier_to_class(fxx, binary_prefix(x,x)).
ops__op_specifier_to_class(fyx, binary_prefix(y,x)).
ops__op_specifier_to_class(fxy, binary_prefix(x,y)).

:- type ops__mercury_op_table ---> ops__mercury_op_table.

	% ops__category is used to index the op_table so that
	% lookups are semidet rather than nondet.
	% Prefix and binary_prefix operators have ops__category `before'.
	% Infix and postfix operators have ops__category `after'.
:- type ops__category ---> before ; after.

ops__init_mercury_op_table = ops__mercury_op_table.

:- instance ops__op_table(ops__mercury_op_table) where [
	pred(ops__lookup_infix_op/5) is ops__lookup_mercury_infix_op,
	pred(ops__lookup_operator_term/4) is
			ops__lookup_mercury_operator_term,
	pred(ops__lookup_prefix_op/4) is ops__lookup_mercury_prefix_op,
	pred(ops__lookup_binary_prefix_op/5) is
			ops__lookup_mercury_binary_prefix_op,
	pred(ops__lookup_postfix_op/4) is ops__lookup_mercury_postfix_op,
	pred(ops__lookup_op/2) is ops__lookup_mercury_op,
	func(ops__max_priority/1) is ops__mercury_max_priority,
	func(ops__arg_priority/1) is ops__mercury_arg_priority
].

:- pred ops__lookup_mercury_infix_op(mercury_op_table, string, ops__priority,
		ops__assoc, ops__assoc).
:- mode ops__lookup_mercury_infix_op(in, in, out, out, out) is semidet.

ops__lookup_mercury_infix_op(_OpTable, Name, Priority,
			LeftAssoc, RightAssoc) :-
	ops__op_table(Name, after, Specifier, Priority),
	ops__op_specifier_to_class(Specifier, infix(LeftAssoc, RightAssoc)).

:- pred ops__lookup_mercury_operator_term(mercury_op_table, ops__priority,
		ops__assoc, ops__assoc).
:- mode ops__lookup_mercury_operator_term(in, out, out, out) is det.

	% Left associative, lower priority than everything
	% except record syntax.
ops__lookup_mercury_operator_term(_OpTable, 120, y, x).

:- pred ops__lookup_mercury_prefix_op(mercury_op_table,
		string, ops__priority, ops__assoc).
:- mode ops__lookup_mercury_prefix_op(in, in, out, out) is semidet.

ops__lookup_mercury_prefix_op(_OpTable, Name, Priority, LeftAssoc) :-
	ops__op_table(Name, before, Specifier, Priority),
	ops__op_specifier_to_class(Specifier, prefix(LeftAssoc)).

:- pred ops__lookup_mercury_binary_prefix_op(mercury_op_table, string,
		ops__priority, ops__assoc, ops__assoc).
:- mode ops__lookup_mercury_binary_prefix_op(in, in, out, out, out) is semidet.

ops__lookup_mercury_binary_prefix_op(_OpTable, Name, Priority, LeftAssoc,
			RightAssoc) :-
	ops__op_table(Name, before, Specifier, Priority),
	ops__op_specifier_to_class(Specifier,
		binary_prefix(LeftAssoc, RightAssoc)).

:- pred ops__lookup_mercury_postfix_op(mercury_op_table,
		string, ops__priority, ops__assoc).
:- mode ops__lookup_mercury_postfix_op(in, in, out, out) is semidet.

ops__lookup_mercury_postfix_op(_OpTable, Name, Priority, LeftAssoc) :-
	ops__op_table(Name, after, Specifier, Priority),
	ops__op_specifier_to_class(Specifier, postfix(LeftAssoc)).

:- pred ops__lookup_mercury_op(mercury_op_table, string).
:- mode ops__lookup_mercury_op(in, in) is semidet.

ops__lookup_mercury_op(_OpTable, Name) :-
	ops__op_table(Name, _, _, _).

:- func ops__mercury_max_priority(mercury_op_table) = ops__priority.

ops__mercury_max_priority(_Table) = 1200.

:- func ops__mercury_arg_priority(mercury_op_table) = ops__priority.
	
	% XXX ISO prolog syntax would require us to change this to 999,
	% but we need bug-for-bug compatibility with the NU-Prolog parser
	% in order to support e.g. `::' in args.
ops__mercury_arg_priority(_Table) = 1201.

	% Changes here may require changes to doc/reference_manual.texi.
:- pred ops__op_table(string, ops__category, ops__specifier, ops__priority).
:- mode ops__op_table(in, in, out, out) is semidet.
:- mode ops__op_table(in, out, out, out) is nondet.

ops__op_table("*", after, yfx, 400).		% standard ISO Prolog
ops__op_table("**", after, xfy, 200).		% standard ISO Prolog
ops__op_table("+", after, yfx, 500).		% standard ISO Prolog
ops__op_table("++", after, xfy, 500).		% Mercury extension
ops__op_table("+", before, fx, 500).		% traditional Prolog (not ISO)
ops__op_table(",", after, xfy, 1000).		% standard ISO Prolog
ops__op_table("&", after, xfy, 1025).		% Mercury extension
ops__op_table("-", after, yfx, 500).		% standard ISO Prolog
ops__op_table("--", after, yfx, 500).		% Mercury extension
ops__op_table("-", before, fx, 200).		% standard ISO Prolog
ops__op_table("--->", after, xfy, 1179).	% Mercury extension
ops__op_table("-->", after, xfx, 1200).		% standard ISO Prolog
ops__op_table("->", after, xfy, 1050).		% standard ISO Prolog
ops__op_table(".", after, xfy, 600).		% traditional Prolog (not ISO)
ops__op_table("/", after, yfx, 400).		% standard ISO Prolog
ops__op_table("//", after, yfx, 400).		% standard ISO Prolog
ops__op_table("/\\", after, yfx, 500).		% standard ISO Prolog
ops__op_table(":", after, yfx, 600).		% `xfy' in ISO Prolog
ops__op_table(":-", after, xfx, 1200).		% standard ISO Prolog
ops__op_table(":-", before, fx, 1200).		% standard ISO Prolog
ops__op_table("::", after, xfx, 1175).		% Mercury extension
ops__op_table(":=", after, xfx, 650).		% Mercury extension
ops__op_table(";", after, xfy, 1100).		% standard ISO Prolog
ops__op_table("<", after, xfx, 700).		% standard ISO Prolog
ops__op_table("<<", after, yfx, 400).		% standard ISO Prolog
ops__op_table("<=", after, xfy, 920).		% Mercury/NU-Prolog extension
ops__op_table("<=>", after, xfy, 920).		% Mercury/NU-Prolog extension
ops__op_table("=", after, xfx, 700).		% standard ISO Prolog
ops__op_table("=..", after, xfx, 700).		% standard ISO Prolog
ops__op_table("=:=", after, xfx, 700).		% standard ISO Prolog (*)
ops__op_table("=<", after, xfx, 700).		% standard ISO Prolog
ops__op_table("==", after, xfx, 700).		% standard ISO Prolog (*)
ops__op_table("==>", after, xfx, 1175).		% Mercury extension
ops__op_table("=>", after, xfy, 920).		% Mercury/NU-Prolog extension
ops__op_table("=\\=", after, xfx, 700).		% standard ISO Prolog (*)
ops__op_table("=^", after, xfx, 650).		% Mercury extension
ops__op_table(">", after, xfx, 700).		% standard ISO Prolog
ops__op_table(">=", after, xfx, 700).		% standard ISO Prolog
ops__op_table(">>", after, yfx, 400).		% standard ISO Prolog
ops__op_table("?-", before, fx, 1200).		% standard ISO Prolog (*)
ops__op_table("@", after, xfx, 90).		% Mercury extension
ops__op_table("@<", after, xfx, 700).		% standard ISO Prolog
ops__op_table("@=<", after, xfx, 700).		% standard ISO Prolog
ops__op_table("@>", after, xfx, 700).		% standard ISO Prolog
ops__op_table("@>=", after, xfx, 700).		% standard ISO Prolog
ops__op_table("\\", before, fx, 200).		% standard ISO Prolog
ops__op_table("\\+", before, fy, 900).		% standard ISO Prolog
ops__op_table("\\/", after, yfx, 500).		% standard ISO Prolog
ops__op_table("\\=", after, xfx, 700).		% standard ISO Prolog
ops__op_table("\\==", after, xfx, 700).		% standard ISO Prolog (*)
ops__op_table("^", after, xfy, 99).		% ISO Prolog (prec. 200,
						%	bitwise xor)
						% Mercury (record syntax)
ops__op_table("^", before, fx, 100).		% Mercury extension
						% (record syntax)
ops__op_table("aditi_bottom_up", before, fx, 500). % Mercury extension
ops__op_table("aditi_top_down", before, fx, 500). % Mercury extension
ops__op_table("all", before, fxy, 950).		% Mercury/NU-Prolog extension
ops__op_table("and", after, xfy, 720).		% NU-Prolog extension
ops__op_table("div", after, yfx, 400).		% standard ISO Prolog
ops__op_table("else", after, xfy, 1170).	% Mercury/NU-Prolog extension
ops__op_table("end_module", before, fx, 1199).	% Mercury extension
ops__op_table("func", before, fx, 800).		% Mercury extension
ops__op_table("if", before, fx, 1160).		% Mercury/NU-Prolog extension
ops__op_table("import_module", before, fx, 1199). % Mercury extension
ops__op_table("include_module", before, fx, 1199). % Mercury extension
ops__op_table("impure", before, fy, 800).	% Mercury extension
ops__op_table("inst", before, fx, 1199).	% Mercury extension
ops__op_table("instance", before, fx, 1199).	% Mercury extension
ops__op_table("is", after, xfx, 701).		% ISO Prolog says prec 700
ops__op_table("lambda", before, fxy, 950).	% Mercury extension
ops__op_table("mod", after, xfx, 400).		% Standard ISO Prolog
ops__op_table("mode", before, fx, 1199).	% Mercury extension
ops__op_table("module", before, fx, 1199).	% Mercury extension
ops__op_table("not", before, fy, 900).		% Mercury/NU-Prolog extension
ops__op_table("or", after, xfy, 740).		% NU-Prolog extension
ops__op_table("pragma", before, fx, 1199).	% Mercury extension
ops__op_table("pred", before, fx, 800).		% Mercury/NU-Prolog extension
ops__op_table("promise", before, fx, 1199).	% Mercury extension
ops__op_table("promise_exclusive", before, fy, 950). % Mercury extension 
ops__op_table("promise_exhaustive", before, fy, 950). % Mercury extension 
ops__op_table("promise_exclusive_exhaustive", before, fy, 950). 
						% Mercury extension 
ops__op_table("rem", after, xfx, 400).		% Standard ISO Prolog
ops__op_table("rule", before, fx, 1199).	% NU-Prolog extension
ops__op_table("semipure", before, fy, 800).	% Mercury extension
ops__op_table("some", before, fxy, 950).	% Mercury/NU-Prolog extension
ops__op_table("then", after, xfx, 1150).	% Mercury/NU-Prolog extension
ops__op_table("type", before, fx, 1180).	% Mercury extension
ops__op_table("typeclass", before, fx, 1199).	% Mercury extension
ops__op_table("use_module", before, fx, 1199).	% Mercury extension
ops__op_table("when", after, xfx, 900).		% NU-Prolog extension (*)
ops__op_table("where", after, xfx, 1175).	% NU-Prolog extension (*)
ops__op_table("~", before, fy, 900).		% Goedel (*)
ops__op_table("~=", after, xfx, 700).		% NU-Prolog (*)

% (*) means that the operator is not useful in Mercury
%     and is provided only for compatibility.

ops__init_op_table(ops__mercury_op_table).
ops__init_op_table = ops__mercury_op_table.

%-----------------------------------------------------------------------------%
