%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module bug383.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module mercury_term_parser.
:- import_module term_io.
:- import_module ops.
:- import_module pair.
:- import_module solutions.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    read_term_with_op_table(cadmium_op_table, Res : read_term, !IO),
    io.print_line(Res, !IO).

%---------------------------------------------------------------------------%

:- type cadmium_op_table
    --->    cadmium_op_table.

:- instance op_table(cadmium_op_table).

:- instance op_table(cadmium_op_table) where [
    pred(lookup_infix_op/5) is lookup_cadmium_infix_op,
    pred(lookup_prefix_op/4) is lookup_cadmium_prefix_op,
    pred(lookup_binary_prefix_op/5) is lookup_cadmium_binary_prefix_op,
    pred(lookup_postfix_op/4) is lookup_cadmium_postfix_op,
    pred(is_op/2) is is_cadmium_op,
    pred(lookup_op_infos/4) is lookup_cadmium_op_infos,
    pred(lookup_operator_term/4) is lookup_cadmium_operator_term,
    func(universal_priority/1) is cadmium_universal_priority,
    func(loosest_op_priority/1) is cadmium_loosest_op_priority,
    func(tightest_op_priority/1) is cadmium_tightest_op_priority,
    func(comma_priority/1) is cadmium_comma_priority,
    func(arg_priority/1) is cadmium_arg_priority
].

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_op_infos(cadmium_op_table::in, string::in,
    op_info::out, list(op_info)::out) is semidet.

lookup_cadmium_op_infos(_, Name, OpInfo, OpInfos) :-
    solutions(cadmium_op_info(Name), [OpInfo | OpInfos]).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_op_infos(cadmium_op_table::in, string::in,
    list(op_info)::out) is det.

lookup_cadmium_op_infos(_, Name, OpInfos) :-
    solutions(cadmium_op_info(Name), OpInfos).

%---------------------------------------------------------------------------%

:- pred cadmium_op_info(string::in, op_info::out) is nondet.

cadmium_op_info(Name, OpInfo) :-
    cadmium_op_table(Name, OpInfo).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_infix_op(cadmium_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_infix_op(_, Name, Priority, LeftAssoc, RightAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table, Name, OpInfos),
    find_first(is_infix_op, OpInfos, OpInfo),
    OpInfo = op_info(infix(LeftAssoc, RightAssoc), Priority).

%---------------------------------------------------------------------------%

:- pred is_infix_op(op_info::in) is semidet.

is_infix_op(op_info(infix(_, _), _)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_operator_term(cadmium_op_table::in, priority::out,
    arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_operator_term(_, prio(1400u), arg_ge, arg_gt) :-
    semidet_true.

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_prefix_op(cadmium_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_prefix_op(_, Name, Priority, LeftAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table, Name, OpInfos),
    find_first(is_prefix_op, OpInfos, OpInfo),
    OpInfo = op_info(prefix(LeftAssoc), Priority).

%---------------------------------------------------------------------------%

:- pred is_prefix_op(op_info::in) is semidet.

is_prefix_op(op_info(prefix(_), _)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_binary_prefix_op(cadmium_op_table, string, priority,
    arg_prio_gt_or_ge, arg_prio_gt_or_ge).
:- mode lookup_cadmium_binary_prefix_op(in, in, out, out, out) is semidet.

lookup_cadmium_binary_prefix_op(_, Name, Priority, LeftAssoc, RightAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table, Name, OpInfos),
    find_first(is_binary_prefix_op, OpInfos, OpInfo),
    OpInfo = op_info(binary_prefix(LeftAssoc, RightAssoc), Priority).

%---------------------------------------------------------------------------%

:- pred is_binary_prefix_op(op_info::in) is semidet.

is_binary_prefix_op(op_info(binary_prefix(_,_),_)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_postfix_op(cadmium_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_postfix_op(_, Name, Priority, LeftAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table, Name, OpInfos),
    find_first(is_postfix_op, OpInfos, OpInfo),
    OpInfo = op_info(postfix(LeftAssoc), Priority).

%---------------------------------------------------------------------------%

:- pred is_postfix_op(op_info::in) is semidet.

is_postfix_op(op_info(postfix(_),_)).

%---------------------------------------------------------------------------%

:- pred is_cadmium_op(cadmium_op_table::in, string::in) is semidet.

is_cadmium_op(_, Name) :-
    cadmium_op_table(Name, _).

%---------------------------------------------------------------------------%

:- func cadmium_universal_priority(cadmium_op_table) = priority.

cadmium_universal_priority(_) = prio(0u).

:- func cadmium_loosest_op_priority(cadmium_op_table) = priority.

cadmium_loosest_op_priority(_) = prio(1u).

:- func cadmium_tightest_op_priority(cadmium_op_table) = priority.

cadmium_tightest_op_priority(_) = prio(1500u).

:- func cadmium_comma_priority(cadmium_op_table) = priority.

cadmium_comma_priority(_) = prio(195u).

:- func cadmium_arg_priority(cadmium_op_table) = priority.

cadmium_arg_priority(_) = prio(196u).

%---------------------------------------------------------------------------%

:- pred find_first(pred(T)::(pred(in) is semidet), list(T)::in, T::out)
    is semidet.

find_first(Pred, [X | Xs], Y) :-
    ( if Pred(X) then
        Y = X
    else
        find_first(Pred, Xs, Y)
    ).

%---------------------------------------------------------------------------%

:- pred cadmium_op_table(string::in, op_info::out) is nondet.

cadmium_op_table("import",      op_info(prefix(arg_ge),        prio(100u))).
cadmium_op_table("ruleset",     op_info(prefix(arg_ge),        prio(100u))).
cadmium_op_table("transform",   op_info(prefix(arg_ge),        prio(100u))).
cadmium_op_table("<=>",         op_info(infix(arg_gt, arg_ge), prio(150u))).
cadmium_op_table("|",           op_info(infix(arg_gt, arg_ge), prio(190u))).
cadmium_op_table("\\",          op_info(infix(arg_ge, arg_gt), prio(190u))).
cadmium_op_table(",",           op_info(infix(arg_gt, arg_ge), prio(195u))).

cadmium_op_table("<->",         op_info(infix(arg_gt, arg_gt), prio(300u))).

cadmium_op_table("->",          op_info(infix(arg_gt, arg_ge), prio(400u))).
cadmium_op_table("<-",          op_info(infix(arg_gt, arg_ge), prio(400u))).

cadmium_op_table("\\/",         op_info(infix(arg_ge, arg_gt), prio(500u))).
cadmium_op_table("xor",         op_info(infix(arg_ge, arg_gt), prio(500u))).

cadmium_op_table("/\\",         op_info(infix(arg_ge, arg_gt), prio(600u))).

cadmium_op_table("<",           op_info(infix(arg_gt, arg_gt), prio(700u))).
cadmium_op_table(">",           op_info(infix(arg_gt, arg_gt), prio(700u))).
cadmium_op_table("<=",          op_info(infix(arg_gt, arg_ge), prio(700u))).
cadmium_op_table(">=",          op_info(infix(arg_gt, arg_gt), prio(700u))).
cadmium_op_table("=",           op_info(infix(arg_gt, arg_gt), prio(700u))).
cadmium_op_table("!=",          op_info(infix(arg_gt, arg_gt), prio(700u))).

cadmium_op_table("+",           op_info(infix(arg_ge, arg_gt), prio(1100u))).
cadmium_op_table("-",           op_info(infix(arg_ge, arg_gt), prio(1100u))).

cadmium_op_table("+",           op_info(prefix(arg_gt),        prio(1410u))).
cadmium_op_table("-",           op_info(prefix(arg_gt),        prio(1410u))).

cadmium_op_table(":=",          op_info(infix(arg_gt, arg_gt), prio(1430u))).
cadmium_op_table("@",           op_info(infix(arg_gt, arg_gt), prio(1430u))).
cadmium_op_table(".",           op_info(infix(arg_ge, arg_gt), prio(1490u))).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
