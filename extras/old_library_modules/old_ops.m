%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2008, 2010, 2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: old_ops.m.
% Main author: fjh.
% Stability: low.
%
% This module is the original version of the ops module, which was
% derived from the operator systems used by Prolog implementations.
% The current version of library/ops.m is non-backwards-compatible
% replacement for it.
%
% See samples/calculator2.m for an example program using this module.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module old_ops.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

    % An class describes what structure terms constructed with an operator
    % of that class are allowed to take.
:- type class
    --->    infix(assoc, assoc)                 % term Op term
    ;       prefix(assoc)                       % Op term
    ;       binary_prefix(assoc, assoc)         % Op term term
    ;       postfix(assoc).                     % term Op

    % `x' represents an argument whose priority must be
    % strictly lower than the priority of the operator.
    % `y' represents an argument whose priority must be
    % lower than or equal to the priority of the operator.
:- type assoc
    --->    x
    ;       y.

    % Operators with a low "priority" bind more tightly than those
    % with a high "priority". For example, given that `+' has
    % priority 500 and `*' has priority 400, the term `2 * X + Y'
    % would parse as `(2 * X) + Y'.
    %
    % The lowest priority is 0.
    %
:- type priority == int.

:- type op_info
    --->    op_info(
                class,
                priority
            ).

%---------------------------------------------------------------------------%

:- typeclass op_table(Table) where [

        % Check whether a string is the name of an infix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_infix_op(Table::in, string::in, priority::out,
        assoc::out, assoc::out) is semidet,

        % Check whether a string is the name of a prefix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_prefix_op(Table::in, string::in,
        priority::out, assoc::out) is semidet,

        % Check whether a string is the name of a binary prefix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_binary_prefix_op(Table::in, string::in,
        priority::out, assoc::out, assoc::out) is semidet,

        % Check whether a string is the name of a postfix operator,
        % and if it is, return its precedence and associativity.
        %
    pred lookup_postfix_op(Table::in, string::in, priority::out,
        assoc::out) is semidet,

        % Check whether a string is the name of an operator.
        %
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
        assoc::out, assoc::out) is semidet,

        % Returns the highest priority number (the lowest is zero).
        %
    func max_priority(Table) = priority,

        % The maximum priority of an operator appearing as the top-level
        % functor of an argument of a compound term.
        %
        % This will generally be the precedence of `,/2' less one.
        % If `,/2' does not appear in the op_table, then
        % old_ops.max_priority plus one may be a reasonable value.
        %
    func arg_priority(Table) = priority
].

%---------------------------------------------------------------------------%

    % The table of Mercury operators.
    % See the "Builtin Operators" section of the "Syntax" chapter
    % of the Mercury Language Reference Manual for details.
    %
:- type mercury_op_table.
:- instance old_ops.op_table(old_ops.mercury_op_table).

:- func init_mercury_op_table = (old_ops.mercury_op_table::uo) is det.

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
    priority::out, assoc::out, assoc::out) is semidet.
:- pred mercury_op_table_search_prefix_op(string::in,
    priority::out, assoc::out) is semidet.
:- pred mercury_op_table_search_binary_prefix_op(string::in,
    priority::out, assoc::out, assoc::out) is semidet.
:- pred mercury_op_table_search_postfix_op(string::in,
    priority::out, assoc::out) is semidet.
:- pred mercury_op_table_search_op(string::in) is semidet.
:- pred mercury_op_table_search_op_infos(string::in,
    op_info::out, list(op_info)::out) is semidet.
:- pred mercury_op_table_lookup_operator_term(priority::out,
    assoc::out, assoc::out) is det.
:- func mercury_op_table_max_priority = priority.
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
:- pred mercury_op_table_infix_op(op_info::in, list(op_info)::in,
    priority::out, assoc::out, assoc::out) is semidet.
:- pred mercury_op_table_prefix_op(op_info::in, list(op_info)::in,
    priority::out, assoc::out) is semidet.
:- pred mercury_op_table_binary_prefix_op(op_info::in, list(op_info)::in,
    priority::out, assoc::out, assoc::out) is semidet.
:- pred mercury_op_table_postfix_op(op_info::in, list(op_info)::in,
    priority::out, assoc::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Anything below here is not documented in the library reference manual.

:- interface.

    % The Mercury operator table used to be the only one allowed.
    % The old names are no longer appropriate.
:- type table == old_ops.mercury_op_table.

%
% For use by mercury_term_parser.m, term_io.m, stream.string_writer.m.
%

:- pred adjust_priority_for_assoc(priority::in, assoc::in,
    priority::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

:- type mercury_op_table
    --->    mercury_op_table.

init_mercury_op_table = old_ops.mercury_op_table.

:- instance op_table(old_ops.mercury_op_table) where [
    pred(lookup_infix_op/5) is lookup_mercury_infix_op,
    pred(lookup_prefix_op/4) is lookup_mercury_prefix_op,
    pred(lookup_binary_prefix_op/5) is lookup_mercury_binary_prefix_op,
    pred(lookup_postfix_op/4) is lookup_mercury_postfix_op,
    pred(lookup_op/2) is lookup_mercury_op,
    pred(lookup_op_infos/4) is lookup_mercury_op_infos,
    pred(lookup_operator_term/4) is lookup_mercury_operator_term,
    func(max_priority/1) is mercury_max_priority,
    func(arg_priority/1) is mercury_arg_priority
].

:- pred lookup_mercury_infix_op(mercury_op_table::in, string::in,
    priority::out, assoc::out, assoc::out) is semidet.

lookup_mercury_infix_op(_OpTable, Name, Priority, LeftAssoc, RightAssoc) :-
    mercury_op_table_search_infix_op(Name, Priority, LeftAssoc, RightAssoc).

:- pred lookup_mercury_prefix_op(mercury_op_table::in,
    string::in, priority::out, assoc::out) is semidet.

lookup_mercury_prefix_op(_OpTable, Name, Priority, LeftAssoc) :-
    mercury_op_table_search_prefix_op(Name, Priority, LeftAssoc).

:- pred lookup_mercury_binary_prefix_op(mercury_op_table::in, string::in,
    priority::out, assoc::out, assoc::out) is semidet.

lookup_mercury_binary_prefix_op(_OpTable, Name, Priority,
        LeftAssoc, RightAssoc) :-
    mercury_op_table_search_binary_prefix_op(Name, Priority,
        LeftAssoc, RightAssoc).

:- pred lookup_mercury_postfix_op(mercury_op_table::in,
    string::in, priority::out, assoc::out) is semidet.

lookup_mercury_postfix_op(_OpTable, Name, Priority, LeftAssoc) :-
    mercury_op_table_search_postfix_op(Name, Priority, LeftAssoc).

:- pred lookup_mercury_op(mercury_op_table::in, string::in) is semidet.

lookup_mercury_op(_OpTable, Name) :-
    mercury_op_table_search_op(Name).

:- pred lookup_mercury_op_infos(mercury_op_table::in, string::in,
    op_info::out, list(op_info)::out) is semidet.

lookup_mercury_op_infos(_OpTable, Name, Info, OtherInfos) :-
    mercury_op_table_search_op_infos(Name, Info, OtherInfos).

:- pred lookup_mercury_operator_term(mercury_op_table::in,
    priority::out, assoc::out, assoc::out) is det.

lookup_mercury_operator_term(_OpTable, Priority, LeftAssoc, RightAssoc) :-
    mercury_op_table_lookup_operator_term(Priority, LeftAssoc, RightAssoc).

:- func mercury_max_priority(mercury_op_table) = priority.

mercury_max_priority(_Table) =
    mercury_op_table_max_priority.

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
:- pragma inline(func(mercury_op_table_max_priority/0)).
:- pragma inline(func(mercury_op_table_arg_priority/0)).

mercury_op_table_search_infix_op(Name, Priority, LeftAssoc, RightAssoc) :-
    old_ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    mercury_op_table_infix_op(Info, MaybeOtherInfo, Priority,
        LeftAssoc, RightAssoc).

mercury_op_table_search_prefix_op(Name, Priority, LeftAssoc) :-
    old_ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    mercury_op_table_prefix_op(Info, MaybeOtherInfo, Priority, LeftAssoc).

mercury_op_table_search_binary_prefix_op(Name, Priority,
        LeftAssoc, RightAssoc) :-
    old_ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    mercury_op_table_binary_prefix_op(Info, MaybeOtherInfo, Priority,
        LeftAssoc, RightAssoc).

mercury_op_table_search_postfix_op(Name, Priority, LeftAssoc) :-
    old_ops.mercury_op_table(Name, Info, MaybeOtherInfo),
    mercury_op_table_postfix_op(Info, MaybeOtherInfo, Priority, LeftAssoc).

mercury_op_table_search_op(Name) :-
    old_ops.mercury_op_table(Name, _, _).

mercury_op_table_search_op_infos(Name, Info, OtherInfos) :-
    old_ops.mercury_op_table(Name, Info, OtherInfos).

mercury_op_table_lookup_operator_term(120, y, x).
    % Left associative, lower priority than everything except record syntax.

mercury_op_table_max_priority = 1200.

mercury_op_table_arg_priority = 999.
    % This needs to be less than the priority of the ','/2 operator.

%---------------------------------------------------------------------------%

mercury_op_table_infix_op(Info, MaybeOtherInfo, Priority,
        LeftAssoc, RightAssoc) :-
    ( if
        Info = op_info(Class, PriorityPrime),
        Class = infix(LeftAssocPrime, RightAssocPrime)
    then
        LeftAssoc = LeftAssocPrime,
        RightAssoc = RightAssocPrime,
        Priority = PriorityPrime
    else if
        MaybeOtherInfo = [op_info(Class, PriorityPrime)],
        Class = infix(LeftAssocPrime, RightAssocPrime)
    then
        LeftAssoc = LeftAssocPrime,
        RightAssoc = RightAssocPrime,
        Priority = PriorityPrime
    else
        fail
    ).

mercury_op_table_prefix_op(Info, MaybeOtherInfo, Priority, LeftAssoc) :-
    ( if
        Info = op_info(prefix(LeftAssocPrime), PriorityPrime)
    then
        LeftAssoc = LeftAssocPrime,
        Priority = PriorityPrime
    else if
        MaybeOtherInfo = [op_info(prefix(LeftAssocPrime), PriorityPrime)]
    then
        LeftAssoc = LeftAssocPrime,
        Priority = PriorityPrime
    else
        fail
    ).

mercury_op_table_binary_prefix_op(Info, MaybeOtherInfo, Priority,
        LeftAssoc, RightAssoc) :-
    ( if
        Info = op_info(Class, PriorityPrime),
        Class = binary_prefix(LeftAssocPrime, RightAssocPrime)
    then
        LeftAssoc = LeftAssocPrime,
        RightAssoc = RightAssocPrime,
        Priority = PriorityPrime
    else if
        MaybeOtherInfo = [op_info(Class, PriorityPrime)],
        Class = binary_prefix(LeftAssocPrime, RightAssocPrime)
    then
        LeftAssoc = LeftAssocPrime,
        RightAssoc = RightAssocPrime,
        Priority = PriorityPrime
    else
        fail
    ).

mercury_op_table_postfix_op(Info, MaybeOtherInfo, Priority, LeftAssoc) :-
    ( if
        Info = op_info(postfix(LeftAssocPrime), PriorityPrime)
    then
        LeftAssoc = LeftAssocPrime,
        Priority = PriorityPrime
    else if
        MaybeOtherInfo = [op_info(postfix(LeftAssocPrime), PriorityPrime)]
    then
        LeftAssoc = LeftAssocPrime,
        Priority = PriorityPrime
    else
        fail
    ).

%---------------------------------------------------------------------------%

:- pragma inline(pred(adjust_priority_for_assoc/3)).

adjust_priority_for_assoc(Priority, y, Priority).
adjust_priority_for_assoc(Priority, x, Priority - 1).

:- pred mercury_op_table(string::in, op_info::out, list(op_info)::out)
    is semidet.

mercury_op_table(Op, Info, OtherInfos) :-
    % NOTE: Changes here may require changes to doc/reference_manual.texi.

    % (*) means that the operator is not useful in Mercury
    % and is provided only for compatibility.

    (
    % The following symbols represent more than one operator.
    % NOTE: The code of several other predicates above depends on the fact
    % that no symbol represents more than *two* operators, by assuming that
    % the length of OtherInfos cannot exceed one.

        Op = "+",
        Info = op_info(infix(y, x), 500),
        % standard ISO Prolog
        OtherInfos = [op_info(prefix(x), 500)]
        % traditional Prolog (not ISO)
    ;
        Op = "-",
        Info = op_info(infix(y, x), 500),
        % standard ISO Prolog
        OtherInfos = [op_info(prefix(x), 200)]
        % standard ISO Prolog
    ;
        Op = ":-",
        Info = op_info(infix(x, x), 1200),
        % standard ISO Prolog
        OtherInfos = [op_info(prefix(x), 1200)]
        % standard ISO Prolog
    ;
        Op = "^",
        Info = op_info(infix(x, y), 99),
        % ISO Prolog (prec. 200, bitwise xor), Mercury (record syntax)
        OtherInfos = [op_info(prefix(x), 100)]
        % Mercury extension (record syntax)
    ;
    % The remaining symbols all represent just one operator.

        % The following operators are standard ISO Prolog.
        ( Op = "*",     Info = op_info(infix(y, x), 400)
        ; Op = "**",    Info = op_info(infix(x, y), 200)
        ; Op = ",",     Info = op_info(infix(x, y), 1000)
        ; Op = "-->",   Info = op_info(infix(x, x), 1200)
        ; Op = "->",    Info = op_info(infix(x, y), 1050)
        ; Op = "/",     Info = op_info(infix(y, x), 400)
        ; Op = "//",    Info = op_info(infix(y, x), 400)
        ; Op = "/\\",   Info = op_info(infix(y, x), 500)
        ; Op = ";",     Info = op_info(infix(x, y), 1100)
        ; Op = "<",     Info = op_info(infix(x, x), 700)
        ; Op = "<<",    Info = op_info(infix(y, x), 400)
        ; Op = "=",     Info = op_info(infix(x, x), 700)
        ; Op = "=..",   Info = op_info(infix(x, x), 700)
        ; Op = "=:=",   Info = op_info(infix(x, x), 700)    % (*)
        ; Op = "=<",    Info = op_info(infix(x, x), 700)
        ; Op = "==",    Info = op_info(infix(x, x), 700)    % (*)
        ; Op = "=\\=",  Info = op_info(infix(x, x), 700)    % (*)
        ; Op = ">",     Info = op_info(infix(x, x), 700)
        ; Op = ">=",    Info = op_info(infix(x, x), 700)
        ; Op = ">>",    Info = op_info(infix(y, x), 400)
        ; Op = "?-",    Info = op_info(prefix(x), 1200)     % (*)
        ; Op = "@<",    Info = op_info(infix(x, x), 700)
        ; Op = "@=<",   Info = op_info(infix(x, x), 700)
        ; Op = "@>",    Info = op_info(infix(x, x), 700)
        ; Op = "@>=",   Info = op_info(infix(x, x), 700)
        ; Op = "\\",    Info = op_info(prefix(x), 200)
        ; Op = "\\+",   Info = op_info(prefix(y), 900)
        ; Op = "\\/",   Info = op_info(infix(y, x), 500)
        ; Op = "\\=",   Info = op_info(infix(x, x), 700)
        ; Op = "\\==",  Info = op_info(infix(x, x), 700)    % (*)
        ; Op = "div",   Info = op_info(infix(y, x), 400)
        ; Op = "is",    Info = op_info(infix(x, x), 701)    % ISO: prec 700
        ; Op = "mod",   Info = op_info(infix(x, x), 400)
        ; Op = "rem",   Info = op_info(infix(x, x), 400)
        ),
        OtherInfos = []
    ;
        % The following operator is a Goedel extension.
        Op = "~",       Info = op_info(prefix(y), 900),     % (*)
        OtherInfos = []
    ;
        % The following operators are NU-Prolog extensions.
        ( Op = "~=",    Info = op_info(infix(x, x), 700)    % (*)
        ; Op = "and",   Info = op_info(infix(x, y), 720)
        ; Op = "or",    Info = op_info(infix(x, y), 740)
        ; Op = "rule",  Info = op_info(prefix(x), 1199)
        ; Op = "when",  Info = op_info(infix(x, x), 900)    % (*)
        ; Op = "where", Info = op_info(infix(x, x), 1175)   % (*)
        ),
        OtherInfos = []
    ;
        % The following operators are Mercury/NU-Prolog extensions.
        ( Op = "<=",    Info = op_info(infix(x, y), 920)
        ; Op = "<=>",   Info = op_info(infix(x, y), 920)
        ; Op = "=>",    Info = op_info(infix(x, y), 920)
        ; Op = "all",   Info = op_info(binary_prefix(x, y), 950)
        ; Op = "some",  Info = op_info(binary_prefix(x, y), 950)
        ; Op = "if",    Info = op_info(prefix(x), 1160)
        ; Op = "then",  Info = op_info(infix(x, x), 1150)
        ; Op = "else",  Info = op_info(infix(x, y), 1170)
        ; Op = "catch", Info = op_info(infix(x, y), 1180)
        ; Op = "catch_any", Info = op_info(infix(x, y), 1190)
        ; Op = "not",   Info = op_info(prefix(y), 900)
        ; Op = "pred",  Info = op_info(prefix(x), 800)
        ),
        OtherInfos = []
    ;
        % The following operators are Mercury extensions.
        ( Op = "!",                 Info = op_info(prefix(x), 40)
        ; Op = "!.",                Info = op_info(prefix(x), 40)
        ; Op = "!:",                Info = op_info(prefix(x), 40)
        ; Op = "&",                 Info = op_info(infix(x, y), 1025)
        ; Op = "++",                Info = op_info(infix(x, y), 500)
        ; Op = "--",                Info = op_info(infix(y, x), 500)
        ; Op = "--->",              Info = op_info(infix(x, y), 1179)
        ; Op = ".",                 Info = op_info(infix(y, x), 10)
        ; Op = "..",                Info = op_info(infix(x, x), 550)
        ; Op = ":",                 Info = op_info(infix(y, x), 120)
        ; Op = "::",                Info = op_info(infix(x, x), 1175)
        ; Op = ":=",                Info = op_info(infix(x, x), 650)
        ; Op = "==>",               Info = op_info(infix(x, x), 1175)
        ; Op = "=^",                Info = op_info(infix(x, x), 650)
        ; Op = "@",                 Info = op_info(infix(x, x), 90)
        ; Op = "end_module",        Info = op_info(prefix(x), 1199)
        ; Op = "event",             Info = op_info(prefix(x), 100)
        ; Op = "finalise",          Info = op_info(prefix(x), 1199)
        ; Op = "finalize",          Info = op_info(prefix(x), 1199)
        ; Op = "for",               Info = op_info(infix(x, x), 500)
        ; Op = "func",              Info = op_info(prefix(x), 800)
        ; Op = "import_module",     Info = op_info(prefix(x), 1199)
        ; Op = "impure",            Info = op_info(prefix(y), 800)
        ; Op = "include_module",    Info = op_info(prefix(x), 1199)
        ; Op = "initialise",        Info = op_info(prefix(x), 1199)
        ; Op = "initialize",        Info = op_info(prefix(x), 1199)
        ; Op = "inst",              Info = op_info(prefix(x), 1199)
        ; Op = "instance",          Info = op_info(prefix(x), 1199)
        ; Op = "mode",              Info = op_info(prefix(x), 1199)
        ; Op = "module",            Info = op_info(prefix(x), 1199)
        ; Op = "or_else",           Info = op_info(infix(x, y), 1100)
        ; Op = "pragma",            Info = op_info(prefix(x), 1199)
        ; Op = "promise",           Info = op_info(prefix(x), 1199)
        ; Op = "semipure",          Info = op_info(prefix(y), 800)
        ; Op = "solver",            Info = op_info(prefix(y), 1181)
        ; Op = "type",              Info = op_info(prefix(x), 1180)
        ; Op = "typeclass",         Info = op_info(prefix(x), 1199)
        ; Op = "use_module",        Info = op_info(prefix(x), 1199)
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
        Info = op_info(binary_prefix(x, y), 950),
        OtherInfos = []
    ;
        ( Op = "promise_exclusive"
        ; Op = "promise_exhaustive"
        ; Op = "promise_exclusive_exhaustive"
        ),
        Info = op_info(prefix(y), 950),
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
        Info = op_info(prefix(x), 950),
        OtherInfos = []
    ).

%---------------------------------------------------------------------------%
:- end_module old_ops.
%---------------------------------------------------------------------------%
