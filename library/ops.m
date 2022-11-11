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

:- import_module list.

%---------------------------------------------------------------------------%

    % A class describes what structure terms constructed with an operator
    % of that class are allowed to take.
    %
:- type class
    --->    infix(arg_prio_gt_or_ge, arg_prio_gt_or_ge)          % term Op term
    ;       prefix(arg_prio_gt_or_ge)                            % Op term
    ;       binary_prefix(arg_prio_gt_or_ge, arg_prio_gt_or_ge)  % Op term term
    ;       postfix(arg_prio_gt_or_ge).                          % term Op

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

:- type op_info
    --->    op_info(
                class,
                priority
            ).

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

        % Check whether a string is the name of an operator.
        %
        % XXX OPS
    pred lookup_op(Table::in, string::in) is semidet,

        % Check whether a string is the name of an operator, and if it is,
        % return the op_info describing that operator in the third argument.
        % If the string is the name of more than one operator, return
        % information about its other guises in the last argument.
        %
    pred lookup_op_infos(Table::in, string::in,
        op_info::out, list(op_info)::out) is semidet,

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
        % This will generally be one step tigther than comma_priority.
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
:- pred mercury_op_table_search_op(string::in) is semidet.
:- pred mercury_op_table_search_op_infos(string::in,
    op_info::out, list(op_info)::out) is semidet.
:- pred mercury_op_table_lookup_operator_term(priority::out,
    arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is det.
:- func mercury_op_table_universal_priority = priority.
:- func mercury_op_table_loosest_op_priority = priority.
:- func mercury_op_table_tightest_op_priority = priority.
:- func mercury_op_table_comma_priority = priority.
:- func mercury_op_table_arg_priority = priority.

    % These predicates do the same job as the corresponding
    % mercury_op_table_search_* predicates, but instead of looking up
    % the operarator name in the Mercury op_table, they get it from
    % their callers, who presumably got them by calling
    % mercury_op_table_search_op_infos.
    %
    % This allows the cost of the table lookup to be paid just once
    % even if you are looking for more than one kind of op.
    %
:- pred op_infos_infix_op(op_info::in, list(op_info)::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.
:- pred op_infos_prefix_op(op_info::in, list(op_info)::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.
:- pred op_infos_binary_prefix_op(op_info::in, list(op_info)::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.
:- pred op_infos_postfix_op(op_info::in, list(op_info)::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Anything below here is not documented in the library reference manual.

:- interface.

    % We export this type synonym to io.m (for get_op_table/set_op_table)
    % and to string.m (for string_ops and string_ops_noncanon). These all
    % treat the argument whose type is ops.table as being *any* op_table,
    % i.e. as not necessarily being the *Mercury* op table. Indeed, the notion
    % of being able to set the op table in io.set_op_table, and of being able
    % to pass an arbitrary op table to string_ops and its noncanon version
    % *depend* on being able to pass op tables that differ from the standard
    % Mercury op table. XXX Yet we do not actually support this. Regardless
    % of what op table the user wants to pass to set_op_table or to string_ops,
    % the fact "table" is a synonym for "mercury_op_table" means that all
    % operations on the op table get mercury_op_table's instance of the
    % op_table typeclass, and the instance methods of this typeclass
    % for mercury_op_table, quite reasonably, all look up operators in the
    % *Mercury* table of operators. XXX There is also the minor issue
    % (compared to the issue above) that the references to ops.table in io.m
    % and string.m are dangling references from users' points of view,
    % since we deliberately prevent the definition of this type from appearing
    % the library manual.
    %
    % We could fix both issues by making the currrent users of this type
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
    pred(lookup_op/2) is                lookup_mercury_op,
    pred(lookup_op_infos/4) is          lookup_mercury_op_infos,
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

:- pred lookup_mercury_op(mercury_op_table::in, string::in) is semidet.

lookup_mercury_op(_OpTable, Name) :-
    mercury_op_table_search_op(Name).

:- pred lookup_mercury_op_infos(mercury_op_table::in, string::in,
    op_info::out, list(op_info)::out) is semidet.

lookup_mercury_op_infos(_OpTable, Name, Info, OtherInfos) :-
    mercury_op_table_search_op_infos(Name, Info, OtherInfos).

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
:- pragma inline(pred(mercury_op_table_search_op/1)).
:- pragma inline(pred(mercury_op_table_search_op_infos/3)).
:- pragma inline(pred(mercury_op_table_lookup_operator_term/3)).
:- pragma inline(func(mercury_op_table_universal_priority/0)).
:- pragma inline(func(mercury_op_table_loosest_op_priority/0)).
:- pragma inline(func(mercury_op_table_tightest_op_priority/0)).
:- pragma inline(func(mercury_op_table_comma_priority/0)).
:- pragma inline(func(mercury_op_table_arg_priority/0)).

mercury_op_table_search_infix_op(Name, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    op_infos_infix_op(Info, MaybeOtherInfo, OpPriority,
        LeftGtOrGe, RightGtOrGe).

mercury_op_table_search_prefix_op(Name, OpPriority, LeftGtOrGe) :-
    ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    op_infos_prefix_op(Info, MaybeOtherInfo, OpPriority, LeftGtOrGe).

mercury_op_table_search_binary_prefix_op(Name, OpPriority,
        LeftGtOrGe, RightGtOrGe) :-
    ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    op_infos_binary_prefix_op(Info, MaybeOtherInfo, OpPriority,
        LeftGtOrGe, RightGtOrGe).

mercury_op_table_search_postfix_op(Name, OpPriority, LeftGtOrGe) :-
    ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    op_infos_postfix_op(Info, MaybeOtherInfo, OpPriority, LeftGtOrGe).

mercury_op_table_search_op(Name) :-
    ops.mercury_op_table(Name, _, _).

mercury_op_table_search_op_infos(Name, Info, OtherInfos) :-
    ops.mercury_op_table(Name, Info, OtherInfos).

mercury_op_table_lookup_operator_term(prio(1380u), arg_ge, arg_gt).
    % Left associative, lower priority than everything except record syntax.

mercury_op_table_universal_priority = prio(0u).
mercury_op_table_loosest_op_priority = prio(1u).
mercury_op_table_tightest_op_priority = prio(1500u).

mercury_op_table_comma_priority = prio(500u).
    % The priority of the ','/2 operator.

mercury_op_table_arg_priority = prio(501u).
    % This needs to bind tighther than mercury_op_table_comma_priority.

%---------------------------------------------------------------------------%

op_infos_infix_op(Info, MaybeOtherInfo, OpPriority,
        LeftGtOrGe, RightGtOrGe) :-
    ( if
        Info = op_info(Class, OpPriorityPrime),
        Class = infix(LeftGtOrGePrime, RightGtOrGePrime)
    then
        LeftGtOrGe = LeftGtOrGePrime,
        RightGtOrGe = RightGtOrGePrime,
        OpPriority = OpPriorityPrime
    else if
        MaybeOtherInfo = [op_info(Class, OpPriorityPrime)],
        Class = infix(LeftGtOrGePrime, RightGtOrGePrime)
    then
        LeftGtOrGe = LeftGtOrGePrime,
        RightGtOrGe = RightGtOrGePrime,
        OpPriority = OpPriorityPrime
    else
        fail
    ).

op_infos_prefix_op(Info, MaybeOtherInfo, OpPriority, LeftGtOrGe) :-
    ( if
        Info = op_info(prefix(LeftGtOrGePrime), OpPriorityPrime)
    then
        LeftGtOrGe = LeftGtOrGePrime,
        OpPriority = OpPriorityPrime
    else if
        MaybeOtherInfo = [op_info(prefix(LeftGtOrGePrime), OpPriorityPrime)]
    then
        LeftGtOrGe = LeftGtOrGePrime,
        OpPriority = OpPriorityPrime
    else
        fail
    ).

op_infos_binary_prefix_op(Info, MaybeOtherInfo, OpPriority,
        LeftGtOrGe, RightGtOrGe) :-
    ( if
        Info = op_info(Class, OpPriorityPrime),
        Class = binary_prefix(LeftGtOrGePrime, RightGtOrGePrime)
    then
        LeftGtOrGe = LeftGtOrGePrime,
        RightGtOrGe = RightGtOrGePrime,
        OpPriority = OpPriorityPrime
    else if
        MaybeOtherInfo = [op_info(Class, OpPriorityPrime)],
        Class = binary_prefix(LeftGtOrGePrime, RightGtOrGePrime)
    then
        LeftGtOrGe = LeftGtOrGePrime,
        RightGtOrGe = RightGtOrGePrime,
        OpPriority = OpPriorityPrime
    else
        fail
    ).

op_infos_postfix_op(Info, MaybeOtherInfo, OpPriority, LeftGtOrGe) :-
    ( if
        Info = op_info(postfix(LeftGtOrGePrime), OpPriorityPrime)
    then
        LeftGtOrGe = LeftGtOrGePrime,
        OpPriority = OpPriorityPrime
    else if
        MaybeOtherInfo = [op_info(postfix(LeftGtOrGePrime), OpPriorityPrime)]
    then
        LeftGtOrGe = LeftGtOrGePrime,
        OpPriority = OpPriorityPrime
    else
        fail
    ).

%---------------------------------------------------------------------------%

:- pred mercury_op_table(string::in, op_info::out, list(op_info)::out)
    is semidet.

mercury_op_table(Op, Info, OtherInfos) :-
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
    % NOTE: The code of several other predicates above depends on the fact
    % that no symbol represents more than *two* operators, by assuming that
    % the length of OtherInfos cannot exceed one.

        Op = "+",
        Info = op_info(infix(arg_ge, arg_gt), prio(1000u)),
        % standard ISO Prolog
        OtherInfos = [op_info(prefix(arg_gt), prio(1000u))]
        % traditional Prolog (not ISO)
    ;
        Op = "-",
        Info = op_info(infix(arg_ge, arg_gt), prio(1000u)),
        % standard ISO Prolog
        OtherInfos = [op_info(prefix(arg_gt), prio(1300u))]
        % standard ISO Prolog
    ;
        Op = ":-",
        Info = op_info(infix(arg_gt, arg_gt), prio(300u)),
        % standard ISO Prolog
        OtherInfos = [op_info(prefix(arg_gt), prio(300u))]
        % standard ISO Prolog
    ;
        Op = "^",
        Info = op_info(infix(arg_gt, arg_ge), prio(1401u)),
        % ISO Prolog (prec. prio(1300u), bitwise xor), Mercury (record syntax)
        OtherInfos = [op_info(prefix(arg_gt), prio(1400u))]
        % Mercury extension (record syntax)
    ;
    % The remaining symbols all represent just one operator.

        % The following operators are standard ISO Prolog.
        ( Op = "*",     Info = op_info(infix(arg_ge, arg_gt),     prio(1100u))
        ; Op = "**",    Info = op_info(infix(arg_gt, arg_ge),     prio(1300u))
        ; Op = ",",     Info = op_info(infix(arg_gt, arg_ge),
                                            mercury_op_table_comma_priority)
        ; Op = "-->",   Info = op_info(infix(arg_gt, arg_gt),     prio(300u))
        ; Op = "->",    Info = op_info(infix(arg_gt, arg_ge),     prio(450u))
        ; Op = "/",     Info = op_info(infix(arg_ge, arg_gt),     prio(1100u))
        ; Op = "//",    Info = op_info(infix(arg_ge, arg_gt),     prio(1100u))
        ; Op = "/\\",   Info = op_info(infix(arg_ge, arg_gt),     prio(1000u))
        ; Op = ";",     Info = op_info(infix(arg_gt, arg_ge),     prio(400u))
        ; Op = "<",     Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "<<",    Info = op_info(infix(arg_ge, arg_gt),     prio(1100u))
        ; Op = "=",     Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "=..",   Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "=:=",   Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "=<",    Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "==",    Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "=\\=",  Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = ">",     Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = ">=",    Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = ">>",    Info = op_info(infix(arg_ge, arg_gt),     prio(1100u))
        ; Op = "?-",    Info = op_info(prefix(arg_gt),            prio(300u))
        ; Op = "@<",    Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "@=<",   Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "@>",    Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "@>=",   Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "\\",    Info = op_info(prefix(arg_gt),            prio(1300u))
        ; Op = "\\+",   Info = op_info(prefix(arg_ge),            prio(600u))
        ; Op = "\\/",   Info = op_info(infix(arg_ge, arg_gt),     prio(1000u))
        ; Op = "\\=",   Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "\\==",  Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "div",   Info = op_info(infix(arg_ge, arg_gt),     prio(1100u))
        ; Op = "is",    Info = op_info(infix(arg_gt, arg_gt),     prio(799u))
                                                                    % ISO 800u
        ; Op = "mod",   Info = op_info(infix(arg_gt, arg_gt),     prio(1100u))
        ; Op = "rem",   Info = op_info(infix(arg_gt, arg_gt),     prio(1100u))
        ),
        OtherInfos = []
    ;
        % The following operator is a Goedel extension.
        Op = "~",       Info = op_info(prefix(arg_ge),            prio(600u)),
        OtherInfos = []
    ;
        % The following operators are NU-Prolog extensions.
        ( Op = "~=",    Info = op_info(infix(arg_gt, arg_gt),     prio(800u))
        ; Op = "and",   Info = op_info(infix(arg_gt, arg_ge),     prio(780u))
        ; Op = "or",    Info = op_info(infix(arg_gt, arg_ge),     prio(760u))
        ; Op = "rule",  Info = op_info(prefix(arg_gt),            prio(301u))
        ; Op = "when",  Info = op_info(infix(arg_gt, arg_gt),     prio(600u))
        ; Op = "where", Info = op_info(infix(arg_gt, arg_gt),     prio(325u))
        ),
        OtherInfos = []
    ;
        % The following operators are Mercury/NU-Prolog extensions.
        ( Op = "<=",    Info = op_info(infix(arg_gt, arg_ge),     prio(580u))
        ; Op = "<=>",   Info = op_info(infix(arg_gt, arg_ge),     prio(580u))
        ; Op = "=>",    Info = op_info(infix(arg_gt, arg_ge),     prio(580u))
        ; Op = "all",   Info = op_info(binary_prefix(arg_gt, arg_ge),
                                                                  prio(550u))
        ; Op = "some",  Info = op_info(binary_prefix(arg_gt, arg_ge),
                                                                  prio(550u))
        ; Op = "if",    Info = op_info(prefix(arg_gt), prio(340u))
        ; Op = "then",  Info = op_info(infix(arg_gt, arg_gt),     prio(350u))
        ; Op = "else",  Info = op_info(infix(arg_gt, arg_ge),     prio(330u))
        ; Op = "catch", Info = op_info(infix(arg_gt, arg_ge),     prio(320u))
        ; Op = "catch_any", Info = op_info(infix(arg_gt, arg_ge), prio(310u))
        ; Op = "not",   Info = op_info(prefix(arg_ge),            prio(600u))
        ; Op = "pred",  Info = op_info(prefix(arg_gt),            prio(700u))
        ),
        OtherInfos = []
    ;
        % The following operators are Mercury extensions.
        ( Op = "!",         Info = op_info(prefix(arg_gt),        prio(1460u))
        ; Op = "!.",        Info = op_info(prefix(arg_gt),        prio(1460u))
        ; Op = "!:",        Info = op_info(prefix(arg_gt),        prio(1460u))
        ; Op = "&",         Info = op_info(infix(arg_gt, arg_ge), prio(475u))
        ; Op = "++",        Info = op_info(infix(arg_gt, arg_ge), prio(1000u))
        ; Op = "--",        Info = op_info(infix(arg_ge, arg_gt), prio(1000u))
        ; Op = "--->",      Info = op_info(infix(arg_gt, arg_ge), prio(321u))
        ; Op = ".",         Info = op_info(infix(arg_ge, arg_gt), prio(1490u))
        ; Op = "..",        Info = op_info(infix(arg_gt, arg_gt), prio(950u))
        ; Op = ":",         Info = op_info(infix(arg_ge, arg_gt), prio(1380u))
        ; Op = "::",        Info = op_info(infix(arg_gt, arg_gt), prio(325u))
        ; Op = ":=",        Info = op_info(infix(arg_gt, arg_gt), prio(850u))
        ; Op = "==>",       Info = op_info(infix(arg_gt, arg_gt), prio(325u))
        ; Op = "=^",        Info = op_info(infix(arg_gt, arg_gt), prio(850u))
        ; Op = "@",         Info = op_info(infix(arg_gt, arg_gt), prio(1410u))
        ; Op = "end_module", Info = op_info(prefix(arg_gt),       prio(301u))
        ; Op = "event",     Info = op_info(prefix(arg_gt),        prio(1400u))
        ; Op = "finalise",  Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "finalize",  Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "for",       Info = op_info(infix(arg_gt, arg_gt), prio(1000u))
        ; Op = "func",      Info = op_info(prefix(arg_gt),        prio(700u))
        ; Op = "import_module", Info = op_info(prefix(arg_gt),    prio(301u))
        ; Op = "impure",    Info = op_info(prefix(arg_ge),        prio(700u))
        ; Op = "include_module", Info = op_info(prefix(arg_gt),   prio(301u))
        ; Op = "initialise", Info = op_info(prefix(arg_gt),       prio(301u))
        ; Op = "initialize", Info = op_info(prefix(arg_gt),       prio(301u))
        ; Op = "inst",      Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "instance",  Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "mode",      Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "module",    Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "or_else",   Info = op_info(infix(arg_gt, arg_ge), prio(400u))
        ; Op = "pragma",    Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "promise",   Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "semipure",  Info = op_info(prefix(arg_ge),        prio(700u))
        ; Op = "solver",    Info = op_info(prefix(arg_ge),        prio(319u))
        ; Op = "type",      Info = op_info(prefix(arg_gt),        prio(320u))
        ; Op = "typeclass", Info = op_info(prefix(arg_gt),        prio(301u))
        ; Op = "use_module", Info = op_info(prefix(arg_gt),       prio(301u))
        ),
        OtherInfos = []
    ;
        ( Op = "arbitrary"
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
        Info = op_info(binary_prefix(arg_gt, arg_ge), prio(550u)),
        OtherInfos = []
    ;
        ( Op = "promise_exclusive"
        ; Op = "promise_exhaustive"
        ; Op = "promise_exclusive_exhaustive"
        ),
        Info = op_info(prefix(arg_ge), prio(550u)),
        OtherInfos = []
    ;
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
        Info = op_info(prefix(arg_gt), prio(550u)),
        OtherInfos = []
    ).

%---------------------------------------------------------------------------%
:- end_module ops.
%---------------------------------------------------------------------------%
