%---------------------------------------------------------------------------%
% Copyright (C) 1995-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: ops.m.
% main author: fjh.
% stability: low.
%
% Here's where we maintain the table of current operators.
%
% XXX In the current implementation the table is fixed and cannot be
% modified at run-time.
%
%-----------------------------------------------------------------------------%

:- module ops.
:- interface.

:- type ops__specifier
	--->	fx ; fy ; xf ; yf ; xfx ; yfx ; xfy ; fxx ; fxy ; fyx.

:- type ops__assoc
	--->	x ; y.

:- type ops__class
	--->	infix(ops__assoc, ops__assoc)
	;	prefix(ops__assoc)
	;	binary_prefix(ops__assoc, ops__assoc)
	;	postfix(ops__assoc).

:- type ops__table.

:- type ops__priority == int.

	% create an ops_table with the standard Mercury operators.
:- pred ops__init_op_table(ops__table).
:- mode ops__init_op_table(uo) is det.

:- func ops__init_op_table = ops__table.

	% check whether a string is the name of an infix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_infix_op(ops__table, string, int, ops__assoc, ops__assoc).
:- mode ops__lookup_infix_op(in, in, out, out, out) is semidet.

	% check whether a string is the name of a prefix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_prefix_op(ops__table, string, int, ops__assoc).
:- mode ops__lookup_prefix_op(in, in, out, out) is semidet.

	% check whether a string is the name of a binary prefix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_binary_prefix_op(ops__table, string,
					int, ops__assoc, ops__assoc).
:- mode ops__lookup_binary_prefix_op(in, in, out, out, out) is semidet.
		
	% check whether a string is the name of a postfix operator,
	% and if it is, return its precedence and associativity.
:- pred ops__lookup_postfix_op(ops__table, string, int, ops__assoc).
:- mode ops__lookup_postfix_op(in, in, out, out) is semidet.

	% check whether a string is the name of an operator
:- pred ops__lookup_op(ops__table, string).
:- mode ops__lookup_op(in, in) is semidet.

	% convert an ops__specifer (e.g. `xfy') to an ops__class
	% (e.g. `infix(x, y)').
:- pred ops__op_specifier_to_class(ops__specifier, ops__class).
:- mode ops__op_specifier_to_class(in, out) is det.
% :- mode ops__op_specifier_to_class(out, in) is semidet.

	% Returns the highest priority number (the lowest is zero).
	% Note that due to Prolog tradition, the priority numbers
	% are backwards: higher numbers mean lower priority
	% and lower numbers mean higher priority.  Sorry...
:- pred ops__max_priority(ops__priority).
:- mode ops__max_priority(out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type ops__table ---> ops__table.	% XXX

:- type ops__category ---> before ; after.

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

ops__lookup_infix_op(_OpTable, Name, Priority, LeftAssoc, RightAssoc) :-
	ops__op_table(Name, after, Specifier, Priority),
	ops__op_specifier_to_class(Specifier,
		infix(LeftAssoc, RightAssoc)).

ops__lookup_prefix_op(_OpTable, Name, Priority, LeftAssoc) :-
	ops__op_table(Name, before, Specifier, Priority),
	ops__op_specifier_to_class(Specifier, prefix(LeftAssoc)).

ops__lookup_binary_prefix_op(_OpTable, Name, Priority, LeftAssoc, RightAssoc) :-
	ops__op_table(Name, before, Specifier, Priority),
	ops__op_specifier_to_class(Specifier,
		binary_prefix(LeftAssoc, RightAssoc)).

ops__lookup_postfix_op(_OpTable, Name, Priority, LeftAssoc) :-
	ops__op_table(Name, after, Specifier, Priority),
	ops__op_specifier_to_class(Specifier, postfix(LeftAssoc)).

ops__lookup_op(_OpTable, Name) :-
	ops__op_table(Name, _, _, _).

	% Changes here may require changes to doc/transition_guide.texi
	% and doc/reference_manual.texi.
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
ops__op_table("export_adt", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("export_cons", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("export_module", before, fx, 1199). % Mercury extension (NYI)
ops__op_table("export_op", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("export_pred", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("export_sym", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("export_type", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("func", before, fx, 800).		% Mercury extension
ops__op_table("if", before, fx, 1160).		% Mercury/NU-Prolog extension
ops__op_table("import_adt", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("import_cons", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("import_module", before, fx, 1199). % Mercury extension
ops__op_table("include_module", before, fx, 1199). % Mercury extension
ops__op_table("import_op", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("import_pred", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("import_sym", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("import_type", before, fx, 1199).	% Mercury extension (NYI)
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
ops__op_table("rem", after, xfx, 400).		% Standard ISO Prolog
ops__op_table("rule", before, fx, 1199).	% NU-Prolog extension
ops__op_table("semipure", before, fy, 800).	% Mercury extension
ops__op_table("some", before, fxy, 950).	% Mercury/NU-Prolog extension
ops__op_table("then", after, xfx, 1150).	% Mercury/NU-Prolog extension
ops__op_table("type", before, fx, 1180).	% Mercury extension
ops__op_table("typeclass", before, fx, 1199).	% Mercury extension
ops__op_table("use_adt", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("use_cons", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("use_module", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("use_op", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("use_pred", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("use_sym", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("use_type", before, fx, 1199).	% Mercury extension (NYI)
ops__op_table("when", after, xfx, 900).		% NU-Prolog extension (*)
ops__op_table("where", after, xfx, 1175).	% NU-Prolog extension (*)
ops__op_table("~", before, fy, 900).		% Goedel (*)
ops__op_table("~=", after, xfx, 700).		% NU-Prolog (*)

% (*) means that the operator is not useful in Mercury
%     and is provided only for compatibility.
% (NYI) means that the operator is reserved for some Not Yet Implemented
%     future purpose

ops__init_op_table(ops__table).
ops__init_op_table = ops__table.

ops__max_priority(1200).

%-----------------------------------------------------------------------------%
