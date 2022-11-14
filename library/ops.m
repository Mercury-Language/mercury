%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2008, 2010, 2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: ops.m.
% Main author: fjh.
% Stability: low.
%
% This module exports a typeclass `ops.op_table' which is used to define
% operator precedence tables for use by
% mercury_term_parser.read_term_with_op_table and
% term_io.write_term_with_op_table.
%
% It also exports an instance `ops.mercury_op_table' that implements the
% Mercury operator table defined in the Mercury Language Reference Manual.
%
% See samples/calculator2.m for an example program.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ops.
:- interface.

%---------------------------------------------------------------------------%

    % An operator table maps strings (the operators themselves) to a value
    % of this type.
    %
    % If the string is an infix operator (term Op term), the info
    % about it is stored in the first field.
    %
    % If the string is a binary prefix operator (Op term term), the info
    % about it is stored in the second field.
    %
    % If the string is a prefix operator (Op term), the info
    % about it is stored in the third field.
    %
    % If the string is a postfix operator (term Op), the info
    % about it is stored in the fourth field.
    %
    % At least one of the fields should contain operator information.
:- type op_infos
    --->    op_infos(
                oi_infix            :: maybe_op_info_infix,
                oi_binary_prefix    :: maybe_op_info_binary_prefix,
                oi_prefix           :: maybe_op_info_prefix,
                oi_postfix          :: maybe_op_info_postfix
            ).

:- type maybe_op_info_infix
    --->    no_in
    ;       in(priority, arg_prio_gt_or_ge, arg_prio_gt_or_ge).

:- type maybe_op_info_binary_prefix
    --->    no_bin_pre
    ;       bin_pre(priority, arg_prio_gt_or_ge, arg_prio_gt_or_ge).

:- type maybe_op_info_prefix
    --->    no_pre
    ;       pre(priority, arg_prio_gt_or_ge).

:- type maybe_op_info_postfix
    --->    no_post
    ;       post(priority, arg_prio_gt_or_ge).

    % When a term appears as an argument of an operator, values of this type
    % specify the relationship that must hold between the priority of the
    % argument (which is the priority of its principal functor as an operator,
    % if it is an operator) and the priority of the operator.
:- type arg_prio_gt_or_ge
    --->    arg_gt
            % This represents an argument whose priority must bind
            % strictly tighter than the priority of the operator.
            % This means that the argument's priority must be strictly
            % greater than the operator's priority.
    ;       arg_ge.
            % This represents an argument whose priority must bind
            % at least as tightly as the priority of the operator.
            % This means that the argument's priority must be either
            % greater than, or equal to, the operator's priority.

    % Operators with a higher priority bind more tightly than those
    % with a low priority. For example, given that `+' has priority 1000
    % and `*' has priority 1100, the string "2 + X * Y" would parse as
    % `2 + (X * Y)'.
    %
    % The range of valid operator priorities is 1 to 1500, with 1 being
    % the loosest and 1500 being the tightest.
    %
    % The universal priority 0 describes contexts that accept terms
    % whose principal functor may be any operator.
    %
:- type priority
    --->    prio(uint).

    % min_priority_for_arg(OpPriority, GtOrGe) = MinArgPriority:
    %
    % Given the priority of an operator (OpPriority) and the required
    % relationship between this priority and the priority of a term
    % in given argument position (GtOrGe), return the minimum priority
    % of the term in that argument position (as MinArgPriority).
    %
:- func min_priority_for_arg(priority, arg_prio_gt_or_ge) = priority.

    % Return the priority that is one step looser than the given priority.
    %
:- func decrement_priority(priority) = priority.

    % Return the priority that is one step tighter than the given priority.
    %
:- func increment_priority(priority) = priority.

    % Tests whether the left priority is respectively
    %
    % - less than
    % - less than or equal to
    % - greater than
    % - greater than or equal to
    %
    % the right priority.
    %
:- pred priority_lt(priority::in, priority::in) is semidet.
:- pred priority_le(priority::in, priority::in) is semidet.
:- pred priority_gt(priority::in, priority::in) is semidet.
:- pred priority_ge(priority::in, priority::in) is semidet.

%---------------------------------------------------------------------------%

:- typeclass op_table(Table) where [

        % Check whether a string is the name of an infix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_infix_op(Table::in, string::in, priority::out,
        arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet,

        % Check whether a string is the name of a prefix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_prefix_op(Table::in, string::in,
        priority::out, arg_prio_gt_or_ge::out) is semidet,

        % Check whether a string is the name of a binary prefix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_binary_prefix_op(Table::in, string::in,
        priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out)
        is semidet,

        % Check whether a string is the name of a postfix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_postfix_op(Table::in, string::in, priority::out,
        arg_prio_gt_or_ge::out) is semidet,

        % Is the given string the name of an operator?
        %
    pred is_op(Table::in, string::in) is semidet,

        % Check whether a string is the name of an operator, and if it is,
        % return the op_infos describing that operator, in all its guises,
        % in the third argument.
        %
    pred lookup_op_infos(Table::in, string::in, op_infos::out) is semidet,

        % Operator terms are terms of the form `X `Op` Y', where `Op' is
        % a variable or a name and X and Y are terms. If operator terms
        % are included in Table, return their precedence and associativity.
        %
    pred lookup_operator_term(Table::in, priority::out,
        arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet,

        % Returns a priority that accepts even terms whose top functor
        % has the loosest op priority as arguments.
        %
    func universal_priority(Table) = priority,

        % Returns the loosest priority that an operator can have.
        %
    func loosest_op_priority(Table) = priority,

        % Returns the tightest priority that an operator can have.
        %
    func tightest_op_priority(Table) = priority,

        % Returns the priority of comma (',/2') as an operator,
        % if that operator exists in the table. If it does not,
        % it should return a priority one step looser than arg_priority.
        %
    func comma_priority(Table) = priority,

        % The minimum priority of an operator appearing as the top-level
        % functor of an argument of a compound term.
        %
        % This will generally be one step tighter than comma_priority.
        % If comma is not in the op table, then ops.universal_priority
        % may be a reasonable value.
        %
    func arg_priority(Table) = priority
].

%---------------------------------------------------------------------------%

    % The table of Mercury operators.
    % See the "Builtin Operators" section of the "Syntax" chapter
    % of the Mercury Language Reference Manual for details.
    %
:- type mercury_op_table.
:- instance ops.op_table(ops.mercury_op_table).

:- func init_mercury_op_table = (ops.mercury_op_table::uo) is det.

    % The implementations of the op_table type class for mercury_op_tables.
    % Each predicate or function here implements the method whose name
    % is the name of the predicate or function without the
    % "mercury_op_table" prefix, and (in some cases) with the "search"
    % replaced by "lookup". (Actually, all the methods that can fail
    % *should* have the "lookup" part of their name replaced by "search").
    % The Table argument is not needed by any of the predicates and functions,
    % since it is implicitly init_mercury_op_table.
    %
:- pred mercury_op_table_search_infix_op(string::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.
:- pred mercury_op_table_search_prefix_op(string::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.
:- pred mercury_op_table_search_binary_prefix_op(string::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.
:- pred mercury_op_table_search_postfix_op(string::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.
:- pred mercury_op_table_is_op(string::in) is semidet.
:- pred mercury_op_table_search_op_infos(string::in, op_infos::out) is semidet.
:- pred mercury_op_table_lookup_operator_term(priority::out,
    arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is det.
:- func mercury_op_table_universal_priority = priority.
:- func mercury_op_table_loosest_op_priority = priority.
:- func mercury_op_table_tightest_op_priority = priority.
:- func mercury_op_table_comma_priority = priority.
:- func mercury_op_table_arg_priority = priority.

    % These predicates do the same job as the corresponding
    % mercury_op_table_search_* predicates, but instead of looking up
    % the operator name in the Mercury op_table, they get it from
    % their callers, who presumably got them by calling
    % mercury_op_table_search_op_infos.
    %
    % This allows the cost of the table lookup to be paid just once
    % even if you are looking for more than one kind of op.
    %
:- pred op_infos_infix_op(op_infos::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.
:- pred op_infos_prefix_op(op_infos::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.
:- pred op_infos_binary_prefix_op(op_infos::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.
:- pred op_infos_postfix_op(op_infos::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Anything below here is not documented in the library reference manual.

:- interface.

    % We export this type synonym to string.m, for string_ops and
    % string_ops_noncanon). These both treat the argument whose type is
    % ops.table as being *any* op_table, i.e. as not necessarily being
    % the *Mercury* op table. Indeed, the notion of being able to pass
    % an arbitrary op table to string_ops and its noncanon version *depend*
    % on being able to pass op tables that differ from the standard Mercury
    % op table. XXX Yet we do not actually support this. Regardless
    % of what op table the user wants to pass to string_ops, the fact that
    % "table" is a synonym for "mercury_op_table" means that all operations
    % on the op table get mercury_op_table's instance of the op_table
    % typeclass, and the instance methods of this typeclass for
    % mercury_op_table, quite reasonably, all look up operators in the
    % *Mercury* table of operators. XXX There is also the minor issue
    % (compared to the issue above) that the references to ops.table in
    % string.m are dangling references from users' points of view,
    % since we deliberately prevent the definition of this type from appearing
    % the library manual.
    %
    % We could fix both issues by making the current users of this type
    % synonym all take arguments of any type that is an instance of the
    % op_table type class. That would be a breaking change, but the breakage
    % would be comparatively minor.
    % 
:- type table == ops.mercury_op_table.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module uint.

%---------------------------------------------------------------------------%

:- pragma inline(func(min_priority_for_arg/2)).

min_priority_for_arg(OpPriority, arg_ge) = OpPriority.
min_priority_for_arg(OpPriority, arg_gt) = increment_priority(OpPriority).

% NOTE This predicate is not used by the Mercury compiler.
decrement_priority(prio(P)) = prio(DecP) :-
    trace [compile_time(flag("enforce_ops_priority_bounds"))] (
        ( if prio(P) = mercury_op_table_loosest_op_priority then
            unexpected($pred, "decrementing loosest op priority")
        else
            true
        )
    ),
    ( if P = 0u then
        unexpected($pred, "decrementing 0")
    else
        DecP = P - 1u
    ).

increment_priority(prio(P)) = prio(IncP) :-
    trace [compile_time(flag("enforce_ops_priority_bounds"))] (
        ( if prio(P) = mercury_op_table_tightest_op_priority then
            unexpected($pred, "incrementing tightest op priority")
        else
            true
        )
    ),
    IncP = P + 1u.

priority_lt(prio(L), prio(R)) :-
    L < R.

priority_le(prio(L), prio(R)) :-
    ( L < R ; L = R ).

priority_gt(prio(L), prio(R)) :-
    L > R.

priority_ge(prio(L), prio(R)) :-
    ( L > R ; L = R ).

%---------------------------------------------------------------------------%

:- type mercury_op_table
    --->    mercury_op_table.

init_mercury_op_table = ops.mercury_op_table.

:- instance op_table(ops.mercury_op_table) where [
    pred(lookup_infix_op/5) is          lookup_mercury_infix_op,
    pred(lookup_prefix_op/4) is         lookup_mercury_prefix_op,
    pred(lookup_binary_prefix_op/5) is  lookup_mercury_binary_prefix_op,
    pred(lookup_postfix_op/4) is        lookup_mercury_postfix_op,
    pred(is_op/2) is                    is_mercury_op,
    pred(lookup_op_infos/3) is          lookup_mercury_op_infos,
    pred(lookup_operator_term/4) is     lookup_mercury_operator_term,
    func(universal_priority/1) is       mercury_universal_priority,
    func(loosest_op_priority/1) is      mercury_loosest_op_priority,
    func(tightest_op_priority/1) is     mercury_tightest_op_priority,
    func(comma_priority/1) is           mercury_comma_priority,
    func(arg_priority/1) is             mercury_arg_priority
].

:- pred lookup_mercury_infix_op(mercury_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

lookup_mercury_infix_op(_OpTable, Name, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    mercury_op_table_search_infix_op(Name, OpPriority,
        LeftGtOrGe, RightGtOrGe).

:- pred lookup_mercury_prefix_op(mercury_op_table::in,
    string::in, priority::out, arg_prio_gt_or_ge::out) is semidet.

lookup_mercury_prefix_op(_OpTable, Name, OpPriority, LeftGtOrGe) :-
    mercury_op_table_search_prefix_op(Name, OpPriority, LeftGtOrGe).

:- pred lookup_mercury_binary_prefix_op(mercury_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

lookup_mercury_binary_prefix_op(_OpTable, Name, OpPriority,
        LeftGtOrGe, RightGtOrGe) :-
    mercury_op_table_search_binary_prefix_op(Name, OpPriority,
        LeftGtOrGe, RightGtOrGe).

:- pred lookup_mercury_postfix_op(mercury_op_table::in,
    string::in, priority::out, arg_prio_gt_or_ge::out) is semidet.

lookup_mercury_postfix_op(_OpTable, Name, OpPriority, LeftGtOrGe) :-
    mercury_op_table_search_postfix_op(Name, OpPriority, LeftGtOrGe).

:- pred is_mercury_op(mercury_op_table::in, string::in) is semidet.

is_mercury_op(_OpTable, Name) :-
    mercury_op_table_is_op(Name).

:- pred lookup_mercury_op_infos(mercury_op_table::in, string::in,
    op_infos::out) is semidet.

lookup_mercury_op_infos(_OpTable, Name, OpInfos) :-
    mercury_op_table_search_op_infos(Name, OpInfos).

:- pred lookup_mercury_operator_term(mercury_op_table::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is det.

lookup_mercury_operator_term(_OpTable, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    mercury_op_table_lookup_operator_term(OpPriority, LeftGtOrGe, RightGtOrGe).

:- func mercury_universal_priority(mercury_op_table) = priority.

mercury_universal_priority(_Table) =
    mercury_op_table_universal_priority.

:- func mercury_loosest_op_priority(mercury_op_table) = priority.

mercury_loosest_op_priority(_Table) =
    mercury_op_table_loosest_op_priority.

:- func mercury_tightest_op_priority(mercury_op_table) = priority.

mercury_tightest_op_priority(_Table) =
    mercury_op_table_tightest_op_priority.

:- func mercury_comma_priority(mercury_op_table) = priority.

mercury_comma_priority(_Table) =
    mercury_op_table_comma_priority.

:- func mercury_arg_priority(mercury_op_table) = priority.

mercury_arg_priority(_Table) =
    mercury_op_table_arg_priority.

%---------------------------------------------------------------------------%

:- pragma inline(pred(mercury_op_table_search_infix_op/4)).
:- pragma inline(pred(mercury_op_table_search_prefix_op/3)).
:- pragma inline(pred(mercury_op_table_search_binary_prefix_op/4)).
:- pragma inline(pred(mercury_op_table_search_postfix_op/3)).
:- pragma inline(pred(mercury_op_table_is_op/1)).
:- pragma inline(pred(mercury_op_table_search_op_infos/2)).
:- pragma inline(pred(mercury_op_table_lookup_operator_term/3)).
:- pragma inline(func(mercury_op_table_universal_priority/0)).
:- pragma inline(func(mercury_op_table_loosest_op_priority/0)).
:- pragma inline(func(mercury_op_table_tightest_op_priority/0)).
:- pragma inline(func(mercury_op_table_comma_priority/0)).
:- pragma inline(func(mercury_op_table_arg_priority/0)).

mercury_op_table_search_infix_op(Name, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    ops.mercury_op_table(Name, OpInfos),
    op_infos_infix_op(OpInfos, OpPriority, LeftGtOrGe, RightGtOrGe).

mercury_op_table_search_prefix_op(Name, OpPriority, LeftGtOrGe) :-
    ops.mercury_op_table(Name, OpInfos),
    op_infos_prefix_op(OpInfos, OpPriority, LeftGtOrGe).

mercury_op_table_search_binary_prefix_op(Name, OpPriority,
        LeftGtOrGe, RightGtOrGe) :-
    ops.mercury_op_table(Name, OpInfos),
    op_infos_binary_prefix_op(OpInfos, OpPriority, LeftGtOrGe, RightGtOrGe).

mercury_op_table_search_postfix_op(Name, OpPriority, LeftGtOrGe) :-
    ops.mercury_op_table(Name, OpInfos),
    op_infos_postfix_op(OpInfos, OpPriority, LeftGtOrGe).

mercury_op_table_is_op(Name) :-
    ops.mercury_op_table(Name, _).

mercury_op_table_search_op_infos(Name, OpInfos) :-
    ops.mercury_op_table(Name, OpInfos).

mercury_op_table_lookup_operator_term(prio(1380u), arg_ge, arg_gt).
    % Left associative, lower priority than everything except record syntax.

mercury_op_table_universal_priority = prio(0u).
mercury_op_table_loosest_op_priority = prio(1u).
mercury_op_table_tightest_op_priority = prio(1500u).

mercury_op_table_comma_priority = prio(500u).
    % The priority of the ','/2 operator.

mercury_op_table_arg_priority = prio(501u).
    % This needs to bind tighter than mercury_op_table_comma_priority.

%---------------------------------------------------------------------------%

op_infos_infix_op(OpInfos, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    OpInfos = op_infos(MaybeInfix, _, _, _),
    MaybeInfix = in(OpPriority, LeftGtOrGe, RightGtOrGe).

op_infos_prefix_op(OpInfos, OpPriority, LeftGtOrGe) :-
    OpInfos = op_infos(_, _, MaybePrefix, _),
    MaybePrefix = pre(OpPriority, LeftGtOrGe).

op_infos_binary_prefix_op(OpInfos, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    OpInfos = op_infos(_, MaybeBinPrefix, _, _),
    MaybeBinPrefix = bin_pre(OpPriority, LeftGtOrGe, RightGtOrGe).

op_infos_postfix_op(OpInfos, OpPriority, LeftGtOrGe) :-
    OpInfos = op_infos(_, _, _, MaybePostfix),
    MaybePostfix = post(OpPriority, LeftGtOrGe).

%---------------------------------------------------------------------------%

:- pred mercury_op_table(string::in, op_infos::out) is semidet.

mercury_op_table(Op, OpInfos) :-
    % NOTE: Changes here may require changes to doc/reference_manual.texi.

    % The following operators are not useful in Mercury, and are provided
    % only for compatibility.
    %
    %   =:=
    %   =\\=
    %   ?-
    %   \\==
    %   ~
    %   ~=
    %   when

    % The priorities here are derived from the priorities in prolog_ops.m,
    % using the equation NP = 1500 - OP, where OP is the old priority in
    % extras/old_library_modules/old_ops.m, and NP is the new priority here.

    (
    % The following symbols represent more than one operator.

        Op = "+",
        OpInfos = op_infos(
            % standard ISO Prolog
            in(prio(1000u), arg_ge, arg_gt), no_bin_pre,
            % traditional Prolog (not ISO)
            pre(prio(1000u), arg_gt), no_post
        )
    ;
        Op = "-",
        OpInfos = op_infos(
            % standard ISO Prolog
            in(prio(1000u), arg_ge, arg_gt), no_bin_pre,
            % standard ISO Prolog
            pre(prio(1300u), arg_gt), no_post
        )
    ;
        Op = ":-",
        OpInfos = op_infos(
            % standard ISO Prolog
            in(prio(300u), arg_gt, arg_gt), no_bin_pre,
            % standard ISO Prolog
            pre(prio(300u), arg_gt), no_post
        )
    ;
        Op = "^",
        OpInfos = op_infos(
            % ISO Prolog (prec. prio(1300u), bitwise xor),
            % Mercury (record syntax)
            in(prio(1401u), arg_gt, arg_ge), no_bin_pre,
            % Mercury extension (record syntax)
            pre(prio(1400u), arg_gt), no_post
        )
    ;
    % The remaining symbols all represent just one operator.

        % First, the infix operators.

        % The following operators are standard ISO Prolog.
        ( Op = "*",         Infix = in(prio(1100u), arg_ge, arg_gt)
        ; Op = "**",        Infix = in(prio(1300u), arg_gt, arg_ge)
        ; Op = ",",         Infix = in(mercury_op_table_comma_priority,
                                        arg_gt, arg_ge)
        ; Op = "-->",       Infix = in(prio(300u),  arg_gt, arg_gt)
        ; Op = "->",        Infix = in(prio(450u),  arg_gt, arg_ge)
        ; Op = "/",         Infix = in(prio(1100u), arg_ge, arg_gt)
        ; Op = "//",        Infix = in(prio(1100u), arg_ge, arg_gt)
        ; Op = "/\\",       Infix = in(prio(1000u), arg_ge, arg_gt)
        ; Op = ";",         Infix = in(prio(400u),  arg_gt, arg_ge)
        ; Op = "<",         Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "<<",        Infix = in(prio(1100u), arg_ge, arg_gt)
        ; Op = "=",         Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "=..",       Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "=:=",       Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "=<",        Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "==",        Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "=\\=",      Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = ">",         Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = ">=",        Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = ">>",        Infix = in(prio(1100u), arg_ge, arg_gt)
        ; Op = "@<",        Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "@=<",       Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "@>",        Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "@>=",       Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "\\/",       Infix = in(prio(1000u), arg_ge, arg_gt)
        ; Op = "\\=",       Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "\\==",      Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "div",       Infix = in(prio(1100u), arg_ge, arg_gt)
        ; Op = "is",        Infix = in(prio(799u),  arg_gt, arg_gt) % ISO 800u
        ; Op = "mod",       Infix = in(prio(1100u), arg_gt, arg_gt)
        ; Op = "rem",       Infix = in(prio(1100u), arg_gt, arg_gt)
        % The following operators are NU-Prolog extensions.
        ; Op = "~=",        Infix = in(prio(800u),  arg_gt, arg_gt)
        ; Op = "and",       Infix = in(prio(780u),  arg_gt, arg_ge)
        ; Op = "or",        Infix = in(prio(760u),  arg_gt, arg_ge)
        ; Op = "when",      Infix = in(prio(600u),  arg_gt, arg_gt)
        ; Op = "where",     Infix = in(prio(325u),  arg_gt, arg_gt)
        % The following operators are Mercury/NU-Prolog extensions.
        ; Op = "<=",        Infix = in(prio(580u),  arg_gt, arg_ge)
        ; Op = "<=>",       Infix = in(prio(580u),  arg_gt, arg_ge)
        ; Op = "=>",        Infix = in(prio(580u),  arg_gt, arg_ge)
        ; Op = "then",      Infix = in(prio(350u),  arg_gt, arg_gt)
        ; Op = "else",      Infix = in(prio(330u),  arg_gt, arg_ge)
        ; Op = "catch",     Infix = in(prio(320u),  arg_gt, arg_ge)
        ; Op = "catch_any", Infix = in(prio(310u),  arg_gt, arg_ge)
        % The following operators are Mercury extensions.
        ; Op = "&",         Infix = in(prio(475u),  arg_gt, arg_ge)
        ; Op = "++",        Infix = in(prio(1000u), arg_gt, arg_ge)
        ; Op = "--",        Infix = in(prio(1000u), arg_ge, arg_gt)
        ; Op = "--->",      Infix = in(prio(321u),  arg_gt, arg_ge)
        ; Op = ".",         Infix = in(prio(1490u), arg_ge, arg_gt)
        ; Op = "..",        Infix = in(prio(950u),  arg_gt, arg_gt)
        ; Op = ":",         Infix = in(prio(1380u), arg_ge, arg_gt)
        ; Op = "::",        Infix = in(prio(325u),  arg_gt, arg_gt)
        ; Op = ":=",        Infix = in(prio(850u),  arg_gt, arg_gt)
        ; Op = "==>",       Infix = in(prio(325u),  arg_gt, arg_gt)
        ; Op = "=^",        Infix = in(prio(850u),  arg_gt, arg_gt)
        ; Op = "@",         Infix = in(prio(1410u), arg_gt, arg_gt)
        ; Op = "for",       Infix = in(prio(1000u), arg_gt, arg_gt)
        ; Op = "or_else",   Infix = in(prio(400u),  arg_gt, arg_ge)
        ),
        OpInfos = op_infos(Infix, no_bin_pre, no_pre, no_post)
    ;
        % Next, the binary prefix operators.

        % The following operators are Mercury/NU-Prolog extensions.
        ( Op = "all"
        ; Op = "some"
        % The following operators are Mercury extensions.
        ; Op = "arbitrary"
        ; Op = "disable_warning"
        ; Op = "disable_warnings"
        ; Op = "promise_equivalent_solutions"
        ; Op = "promise_equivalent_solution_sets"
        ; Op = "require_complete_switch"
        ; Op = "require_switch_arms_det"
        ; Op = "require_switch_arms_semidet"
        ; Op = "require_switch_arms_multi"
        ; Op = "require_switch_arms_nondet"
        ; Op = "require_switch_arms_cc_multi"
        ; Op = "require_switch_arms_cc_nondet"
        ; Op = "require_switch_arms_erroneous"
        ; Op = "require_switch_arms_failure"
        ; Op = "trace"
        ; Op = "atomic"
        ; Op = "try"
        ),
        BinPrefix = bin_pre(prio(550u), arg_gt, arg_ge),
        OpInfos = op_infos(no_in, BinPrefix, no_pre, no_post)
    ;
        % Next, the prefix operators.

        % The following operators are standard ISO Prolog.
        ( Op = "?-",                Prefix = pre(prio(300u),  arg_gt)
        ; Op = "\\",                Prefix = pre(prio(1300u), arg_gt)
        ; Op = "\\+",               Prefix = pre(prio(600u),  arg_ge)
        % The following operator is a Goedel extension.
        ; Op = "~",                 Prefix = pre(prio(600u),  arg_ge)
        % The following operator is a NU-Prolog extension.
        ; Op = "rule",              Prefix = pre(prio(301u),  arg_gt)
        % The following operators are Mercury/NU-Prolog extensions.
        ; Op = "if",                Prefix = pre(prio(340u),  arg_gt)
        ; Op = "not",               Prefix = pre(prio(600u),  arg_ge)
        ; Op = "pred",              Prefix = pre(prio(700u),  arg_gt)
        % The following operators are Mercury extensions.
        ; Op = "!",                 Prefix = pre(prio(1460u), arg_gt)
        ; Op = "!.",                Prefix = pre(prio(1460u), arg_gt)
        ; Op = "!:",                Prefix = pre(prio(1460u), arg_gt)
        ; Op = "end_module",        Prefix = pre(prio(301u),  arg_gt)
        ; Op = "event",             Prefix = pre(prio(1400u), arg_gt)
        ; Op = "finalise",          Prefix = pre(prio(301u),  arg_gt)
        ; Op = "finalize",          Prefix = pre(prio(301u),  arg_gt)
        ; Op = "func",              Prefix = pre(prio(700u),  arg_gt)
        ; Op = "import_module",     Prefix = pre(prio(301u),  arg_gt)
        ; Op = "impure",            Prefix = pre(prio(700u),  arg_ge)
        ; Op = "include_module",    Prefix = pre(prio(301u),  arg_gt)
        ; Op = "initialise",        Prefix = pre(prio(301u),  arg_gt)
        ; Op = "initialize",        Prefix = pre(prio(301u),  arg_gt)
        ; Op = "inst",              Prefix = pre(prio(301u),  arg_gt)
        ; Op = "instance",          Prefix = pre(prio(301u),  arg_gt)
        ; Op = "mode",              Prefix = pre(prio(301u),  arg_gt)
        ; Op = "module",            Prefix = pre(prio(301u),  arg_gt)
        ; Op = "pragma",            Prefix = pre(prio(301u),  arg_gt)
        ; Op = "promise",           Prefix = pre(prio(301u),  arg_gt)
        ; Op = "semipure",          Prefix = pre(prio(700u),  arg_ge)
        ; Op = "solver",            Prefix = pre(prio(319u),  arg_ge)
        ; Op = "type",              Prefix = pre(prio(320u),  arg_gt)
        ; Op = "typeclass",         Prefix = pre(prio(301u),  arg_gt)
        ; Op = "use_module",        Prefix = pre(prio(301u),  arg_gt)
        ),
        OpInfos = op_infos(no_in, no_bin_pre, Prefix, no_post)
    ;
        % The following operators are Mercury extensions.
        ( Op = "promise_exclusive"
        ; Op = "promise_exhaustive"
        ; Op = "promise_exclusive_exhaustive"
        ),
        Prefix = pre(prio(550u), arg_ge),
        OpInfos = op_infos(no_in, no_bin_pre, Prefix, no_post)
    ;
        % The following operators are Mercury extensions.
        ( Op = "promise_pure"
        ; Op = "promise_semipure"
        ; Op = "promise_impure"
        ; Op = "require_det"
        ; Op = "require_semidet"
        ; Op = "require_multi"
        ; Op = "require_nondet"
        ; Op = "require_cc_multi"
        ; Op = "require_cc_nondet"
        ; Op = "require_erroneous"
        ; Op = "require_failure"
        ),
        Prefix = pre(prio(550u), arg_gt),
        OpInfos = op_infos(no_in, no_bin_pre, Prefix, no_post)

        % Last, the postfix operators.
        % There are none.
    ).

%---------------------------------------------------------------------------%
:- end_module ops.
%---------------------------------------------------------------------------%
