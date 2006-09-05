%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: ops.m.
% Main author: fjh.
% Stability: low.
% 
% This module exports a typeclass `ops.op_table' which is used to define
% operator precedence tables for use by `parser.read_term_with_op_table'
% and `term_io.write_term_with_op_table'.
%
% It also exports an instance `ops.mercury_op_table' that implements the
% Mercury operator table defined in the Mercury Language Reference Manual.
%
% See samples/calculator2.m for an example program.
%
% XXX In the current implementation the table of Mercury operators
% is fixed and cannot be modified at run-time.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module ops.
:- interface.

:- typeclass ops.op_table(Table) where [

        % Check whether a string is the name of an infix operator,
        % and if it is, return its precedence and associativity.
        %
    pred ops.lookup_infix_op(Table::in, string::in, ops.priority::out,
        ops.assoc::out, ops.assoc::out) is semidet,

        % Operator terms are terms of the form `X `Op` Y',
        % where `Op' is a variable or a name and `X' and `Y'
        % are terms. If operator terms are included in `Table',
        % return their precedence and associativity.
        %
    pred ops.lookup_operator_term(Table::in, ops.priority::out,
        ops.assoc::out, ops.assoc::out) is semidet,

        % Check whether a string is the name of a prefix operator,
        % and if it is, return its precedence and associativity.
        %
    pred ops.lookup_prefix_op(Table::in, string::in,
        ops.priority::out, ops.assoc::out) is semidet,

        % Check whether a string is the name of a binary prefix
        % operator, and if it is, return its precedence and
        % associativity.
        %
    pred ops.lookup_binary_prefix_op(Table::in, string::in,
        ops.priority::out, ops.assoc::out, ops.assoc::out)
        is semidet,

        % Check whether a string is the name of a postfix operator,
        % and if it is, return its precedence and associativity.
        %
    pred ops.lookup_postfix_op(Table::in, string::in, ops.priority::out,
        ops.assoc::out) is semidet,

        % Check whether a string is the name of an operator
        %
    pred ops.lookup_op(Table::in, string::in) is semidet,

        % Returns the highest priority number (the lowest is zero).
        %
    func ops.max_priority(Table) = ops.priority,

        % The maximum priority of an operator appearing
        % as the top-level functor of an argument of a compound
        % term.
        %
        % This will generally be the precedence of `,/2' less one.
        % If `,/2' does not appear in the op_table,
        % `ops.max_priority' plus one may be a reasonable value.
        %
    func ops.arg_priority(Table) = ops.priority
].

%-----------------------------------------------------------------------------%

    % The table of Mercury operators.
    % See the "Builtin Operators" section of the "Syntax" chapter
    % of the Mercury Language Reference Manual for details.
    %
:- type ops.mercury_op_table.
:- instance ops.op_table(ops.mercury_op_table).

:- func ops.init_mercury_op_table = (ops.mercury_op_table::uo) is det.

%-----------------------------------------------------------------------------%

    % Operators with a low "priority" bind more tightly than those
    % with a high "priority". For example, given that `+' has
    % priority 500 and `*' has priority 400, the term `2 * X + Y'
    % would parse as `(2 * X) + Y'.
    %
    % The lowest priority is 0.
    %
:- type ops.priority == int.

%-----------------------------------------------------------------------------%

    % An ops.specifier describes what structure terms
    % constructed with an operator are allowed to take.
    % `f' represents the operator and `x' and `y' represent arguments.
    % `x' represents an argument whose priority must be
    % strictly lower than that of the operator.
    % `y' represents an argument whose priority is
    % lower or equal to that of the operator.
    % For example, `yfx' indicates a left-associative infix operator,
    % while `xfy' indicates a right-associative infix operator.
    %
:- type ops.specifier
    --->    fx
    ;       fy
    ;       xf
    ;       yf
    ;       xfx
    ;       yfx
    ;       xfy
    ;       fxx
    ;       fxy
    ;       fyx.

:- type ops.assoc
    --->    x
    ;       y.

:- type ops.class
    --->    infix(ops.assoc, ops.assoc)
    ;       prefix(ops.assoc)
    ;       binary_prefix(ops.assoc, ops.assoc)
    ;       postfix(ops.assoc).

    % convert an ops.specifer (e.g. `xfy') to an ops.class
    % (e.g. `infix(x, y)').
    %
:- pred ops.op_specifier_to_class(ops.specifier::in, ops.class::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

% Anything below here is not documented in the library reference manual.

:- interface.

    % The Mercury operator table used to be the only one allowed.
    % The old names are no longer appropriate.
:- type ops.table == ops.mercury_op_table.

%-----------------------------------------------------------------------------%

:- implementation.

ops.op_specifier_to_class(fx, prefix(x)).
ops.op_specifier_to_class(fy, prefix(y)).
ops.op_specifier_to_class(xf, postfix(x)).
ops.op_specifier_to_class(yf, postfix(y)).
ops.op_specifier_to_class(xfx, infix(x,x)).
ops.op_specifier_to_class(yfx, infix(y,x)).
ops.op_specifier_to_class(xfy, infix(x,y)).
ops.op_specifier_to_class(fxx, binary_prefix(x,x)).
ops.op_specifier_to_class(fyx, binary_prefix(y,x)).
ops.op_specifier_to_class(fxy, binary_prefix(x,y)).

:- type ops.mercury_op_table
    --->    ops.mercury_op_table.

    % ops.category is used to index the op_table so that
    % lookups are semidet rather than nondet.
    % Prefix and binary_prefix operators have ops.category `before'.
    % Infix and postfix operators have ops.category `after'.
:- type ops.category
    --->    before
    ;       after.

ops.init_mercury_op_table = ops.mercury_op_table.

:- instance ops.op_table(ops.mercury_op_table) where [
    pred(ops.lookup_infix_op/5) is ops.lookup_mercury_infix_op,
    pred(ops.lookup_operator_term/4) is ops.lookup_mercury_operator_term,
    pred(ops.lookup_prefix_op/4) is ops.lookup_mercury_prefix_op,
    pred(ops.lookup_binary_prefix_op/5) is
        ops.lookup_mercury_binary_prefix_op,
    pred(ops.lookup_postfix_op/4) is ops.lookup_mercury_postfix_op,
    pred(ops.lookup_op/2) is ops.lookup_mercury_op,
    func(ops.max_priority/1) is ops.mercury_max_priority,
    func(ops.arg_priority/1) is ops.mercury_arg_priority
].

:- pred ops.lookup_mercury_infix_op(mercury_op_table::in, string::in,
    ops.priority::out, ops.assoc::out, ops.assoc::out) is semidet.

ops.lookup_mercury_infix_op(_OpTable, Name, Priority,
        LeftAssoc, RightAssoc) :-
    ops.op_table(Name, after, Specifier, Priority),
    ops.op_specifier_to_class(Specifier, infix(LeftAssoc, RightAssoc)).

:- pred ops.lookup_mercury_operator_term(mercury_op_table::in,
    ops.priority::out, ops.assoc::out, ops.assoc::out) is det.

    % Left associative, lower priority than everything
    % except record syntax.
ops.lookup_mercury_operator_term(_OpTable, 120, y, x).

:- pred ops.lookup_mercury_prefix_op(mercury_op_table::in,
    string::in, ops.priority::out, ops.assoc::out) is semidet.

ops.lookup_mercury_prefix_op(_OpTable, Name, Priority, LeftAssoc) :-
    ops.op_table(Name, before, Specifier, Priority),
    ops.op_specifier_to_class(Specifier, prefix(LeftAssoc)).

:- pred ops.lookup_mercury_binary_prefix_op(mercury_op_table::in, string::in,
    ops.priority::out, ops.assoc::out, ops.assoc::out) is semidet.

ops.lookup_mercury_binary_prefix_op(_OpTable, Name, Priority,
        LeftAssoc, RightAssoc) :-
    ops.op_table(Name, before, Specifier, Priority),
    ops.op_specifier_to_class(Specifier,
        binary_prefix(LeftAssoc, RightAssoc)).

:- pred ops.lookup_mercury_postfix_op(mercury_op_table::in,
    string::in, ops.priority::out, ops.assoc::out) is semidet.

ops.lookup_mercury_postfix_op(_OpTable, Name, Priority, LeftAssoc) :-
    ops.op_table(Name, after, Specifier, Priority),
    ops.op_specifier_to_class(Specifier, postfix(LeftAssoc)).

:- pred ops.lookup_mercury_op(mercury_op_table::in, string::in) is semidet.

ops.lookup_mercury_op(_OpTable, Name) :-
    ops.op_table(Name, _, _, _).

:- func ops.mercury_max_priority(mercury_op_table) = ops.priority.

ops.mercury_max_priority(_Table) = 1200.

:- func ops.mercury_arg_priority(mercury_op_table) = ops.priority.

    % This needs to be less than the priority of the ','/2 operator.
ops.mercury_arg_priority(_Table) = 999.

    % Changes here may require changes to doc/reference_manual.texi.
:- pred ops.op_table(string, ops.category, ops.specifier, ops.priority).
:- mode ops.op_table(in, in, out, out) is semidet.
:- mode ops.op_table(in, out, out, out) is nondet.

ops.op_table("..", after, xfx, 550).       % Mercury extension
ops.op_table("*", after, yfx, 400).        % standard ISO Prolog
ops.op_table("**", after, xfy, 200).       % standard ISO Prolog
ops.op_table("+", after, yfx, 500).        % standard ISO Prolog
ops.op_table("++", after, xfy, 500).       % Mercury extension
ops.op_table("+", before, fx, 500).        % traditional Prolog (not ISO)
ops.op_table(",", after, xfy, 1000).       % standard ISO Prolog
ops.op_table("&", after, xfy, 1025).       % Mercury extension
ops.op_table("-", after, yfx, 500).        % standard ISO Prolog
ops.op_table("--", after, yfx, 500).       % Mercury extension
ops.op_table("-", before, fx, 200).        % standard ISO Prolog
ops.op_table("--->", after, xfy, 1179).    % Mercury extension
ops.op_table("-->", after, xfx, 1200).     % standard ISO Prolog
ops.op_table("->", after, xfy, 1050).      % standard ISO Prolog
ops.op_table(".", after, yfx, 10).         % Mercury extension
ops.op_table("/", after, yfx, 400).        % standard ISO Prolog
ops.op_table("//", after, yfx, 400).       % standard ISO Prolog
ops.op_table("/\\", after, yfx, 500).      % standard ISO Prolog
ops.op_table(":", after, yfx, 120).        % Mercury extension
ops.op_table(":-", after, xfx, 1200).      % standard ISO Prolog
ops.op_table(":-", before, fx, 1200).      % standard ISO Prolog
ops.op_table("::", after, xfx, 1175).      % Mercury extension
ops.op_table(":=", after, xfx, 650).       % Mercury extension
ops.op_table(";", after, xfy, 1100).       % standard ISO Prolog
ops.op_table("<", after, xfx, 700).        % standard ISO Prolog
ops.op_table("<<", after, yfx, 400).       % standard ISO Prolog
ops.op_table("<=", after, xfy, 920).       % Mercury/NU-Prolog extension
ops.op_table("<=>", after, xfy, 920).      % Mercury/NU-Prolog extension
ops.op_table("=", after, xfx, 700).        % standard ISO Prolog
ops.op_table("=..", after, xfx, 700).      % standard ISO Prolog
ops.op_table("=:=", after, xfx, 700).      % standard ISO Prolog (*)
ops.op_table("=<", after, xfx, 700).       % standard ISO Prolog
ops.op_table("==", after, xfx, 700).       % standard ISO Prolog (*)
ops.op_table("==>", after, xfx, 1175).     % Mercury extension
ops.op_table("=>", after, xfy, 920).       % Mercury/NU-Prolog extension
ops.op_table("=\\=", after, xfx, 700).     % standard ISO Prolog (*)
ops.op_table("=^", after, xfx, 650).       % Mercury extension
ops.op_table(">", after, xfx, 700).        % standard ISO Prolog
ops.op_table(">=", after, xfx, 700).       % standard ISO Prolog
ops.op_table(">>", after, yfx, 400).       % standard ISO Prolog
ops.op_table("?-", before, fx, 1200).      % standard ISO Prolog (*)
ops.op_table("@", after, xfx, 90).         % Mercury extension
ops.op_table("@<", after, xfx, 700).       % standard ISO Prolog
ops.op_table("@=<", after, xfx, 700).      % standard ISO Prolog
ops.op_table("@>", after, xfx, 700).       % standard ISO Prolog
ops.op_table("@>=", after, xfx, 700).      % standard ISO Prolog
ops.op_table("\\", before, fx, 200).       % standard ISO Prolog
ops.op_table("\\+", before, fy, 900).      % standard ISO Prolog
ops.op_table("\\/", after, yfx, 500).      % standard ISO Prolog
ops.op_table("\\=", after, xfx, 700).      % standard ISO Prolog
ops.op_table("\\==", after, xfx, 700).     % standard ISO Prolog (*)
ops.op_table("^", after, xfy, 99).         % ISO Prolog (prec. 200,
                                            %   bitwise xor)
                                            % Mercury (record syntax)
ops.op_table("^", before, fx, 100).        % Mercury extension
                                            % (record syntax)
ops.op_table("all", before, fxy, 950).     % Mercury/NU-Prolog extension
ops.op_table("and", after, xfy, 720).      % NU-Prolog extension
ops.op_table("div", after, yfx, 400).      % standard ISO Prolog
ops.op_table("else", after, xfy, 1170).    % Mercury/NU-Prolog extension
ops.op_table("end_module", before, fx, 1199).  % Mercury extension
ops.op_table("func", before, fx, 800).     % Mercury extension
ops.op_table("if", before, fx, 1160).      % Mercury/NU-Prolog extension
ops.op_table("import_module", before, fx, 1199). % Mercury extension
ops.op_table("include_module", before, fx, 1199). % Mercury extension
ops.op_table("impure", before, fy, 800).   % Mercury extension
ops.op_table("initialise", before, fx, 1199).  % Mercury extension
ops.op_table("initialize", before, fx, 1199).  % Mercury extension
ops.op_table("finalise", before, fx, 1199).    % Mercury extension
ops.op_table("finalize", before, fx, 1199).    % Mercury extension
ops.op_table("inst", before, fx, 1199).    % Mercury extension
ops.op_table("instance", before, fx, 1199).    % Mercury extension
ops.op_table("is", after, xfx, 701).       % ISO Prolog says prec 700
ops.op_table("mod", after, xfx, 400).      % Standard ISO Prolog
ops.op_table("mode", before, fx, 1199).    % Mercury extension
ops.op_table("module", before, fx, 1199).  % Mercury extension
ops.op_table("not", before, fy, 900).      % Mercury/NU-Prolog extension
ops.op_table("or", after, xfy, 740).       % NU-Prolog extension
ops.op_table("pragma", before, fx, 1199).  % Mercury extension
ops.op_table("pred", before, fx, 800).     % Mercury/NU-Prolog extension
ops.op_table("promise", before, fx, 1199). % Mercury extension
ops.op_table("trace", before, fxy, 950).   % Mercury extension
ops.op_table("event", before, fx, 100).    % Mercury extension
ops.op_table("promise_exclusive", before, fy, 950). % Mercury extension
ops.op_table("promise_exhaustive", before, fy, 950). % Mercury extension
ops.op_table("promise_exclusive_exhaustive", before, fy, 950).
                                            % Mercury extension
ops.op_table("rem", after, xfx, 400).      % Standard ISO Prolog
ops.op_table("rule", before, fx, 1199).    % NU-Prolog extension
ops.op_table("semipure", before, fy, 800). % Mercury extension
ops.op_table("solver", before, fy, 1181).  % Mercury extension
ops.op_table("promise_pure", before, fx, 950). % Mercury extension
ops.op_table("promise_impure", before, fx, 950).   % Mercury extension
ops.op_table("promise_semipure", before, fx, 950). % Mercury extension
ops.op_table("promise_pure_implicit", before, fx, 950).    % Mercury extension
ops.op_table("promise_impure_implicit", before, fx, 950).  % Mercury extension
ops.op_table("promise_semipure_implicit", before, fx, 950).% Mercury extension
ops.op_table("promise_equivalent_solutions", before, fxy, 950).
                                           % Mercury extension
ops.op_table("promise_equivalent_solution_sets", before, fxy, 950).
                                           % Mercury extension
ops.op_table("arbitrary", before, fxy, 950).
                                           % Mercury extension
ops.op_table("some", before, fxy, 950).    % Mercury/NU-Prolog extension
ops.op_table("then", after, xfx, 1150).    % Mercury/NU-Prolog extension
ops.op_table("type", before, fx, 1180).    % Mercury extension
ops.op_table("typeclass", before, fx, 1199).   % Mercury extension
ops.op_table("use_module", before, fx, 1199).  % Mercury extension
ops.op_table("when", after, xfx, 900).     % NU-Prolog extension (*)
ops.op_table("where", after, xfx, 1175).   % NU-Prolog extension (*)
ops.op_table("~", before, fy, 900).        % Goedel (*)
ops.op_table("~=", after, xfx, 700).       % NU-Prolog (*)
ops.op_table("!", before, fx, 40).         % Mercury extension
ops.op_table("!.", before, fx, 40).        % Mercury extension
ops.op_table("!:", before, fx, 40).        % Mercury extension

% (*) means that the operator is not useful in Mercury
%     and is provided only for compatibility.

%-----------------------------------------------------------------------------%
