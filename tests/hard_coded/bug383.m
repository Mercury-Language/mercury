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
    pred(lookup_op_infos/3) is lookup_cadmium_op_infos,
    pred(lookup_operator_term/4) is lookup_cadmium_operator_term,
    func(universal_priority/1) is cadmium_universal_priority,
    func(loosest_op_priority/1) is cadmium_loosest_op_priority,
    func(tightest_op_priority/1) is cadmium_tightest_op_priority,
    func(comma_priority/1) is cadmium_comma_priority,
    func(arg_priority/1) is cadmium_arg_priority
].

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_op_infos(cadmium_op_table::in, string::in,
    op_infos::out) is semidet.

lookup_cadmium_op_infos(_, Name, OpInfos) :-
    cadmium_op_table(Name, OpInfos).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_infix_op(cadmium_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_infix_op(_, Name, Priority, GtOrGeA, GtOrGeB) :-
    cadmium_op_table(Name, OpInfos),
    OpInfos ^ oi_infix = in(Priority, GtOrGeA, GtOrGeB).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_prefix_op(cadmium_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_prefix_op(_, Name, Priority, GtOrGeA) :-
    cadmium_op_table(Name, OpInfos),
    OpInfos ^ oi_prefix = pre(Priority, GtOrGeA).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_binary_prefix_op(cadmium_op_table, string, priority,
    arg_prio_gt_or_ge, arg_prio_gt_or_ge).
:- mode lookup_cadmium_binary_prefix_op(in, in, out, out, out) is semidet.

lookup_cadmium_binary_prefix_op(_, Name, Priority, GtOrGeA, GtOrGeB) :-
    cadmium_op_table(Name, OpInfos),
    OpInfos ^ oi_binary_prefix = bin_pre(Priority, GtOrGeA, GtOrGeB).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_postfix_op(cadmium_op_table::in, string::in,
    priority::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_postfix_op(_, Name, Priority, GtOrGeA) :-
    cadmium_op_table(Name, OpInfos),
    OpInfos ^ oi_postfix = post(Priority, GtOrGeA).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_operator_term(cadmium_op_table::in, priority::out,
    arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

lookup_cadmium_operator_term(_, prio(1400u), arg_ge, arg_gt) :-
    semidet_true.

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

:- pred cadmium_op_table(string::in, op_infos::out) is semidet.

cadmium_op_table(Op, OpInfos) :-
    (
        ( Op = "+"
        ; Op = "-"
        ),
        Infix = in(prio(1100u), arg_ge, arg_gt),
        Prefix = pre(prio(1410u), arg_gt),
        OpInfos = op_infos(Infix, no_bin_pre, Prefix, no_post)
    ;
        ( Op = "import",    Prefix = pre(prio(100u), arg_ge)
        ; Op = "ruleset",   Prefix = pre(prio(100u), arg_ge)
        ; Op = "transform", Prefix = pre(prio(100u), arg_ge)
        ),
        OpInfos = op_infos(no_in, no_bin_pre, Prefix, no_post)
    ;
        ( Op = "<=>",       Infix = in(prio(150u),  arg_gt, arg_ge)
        ; Op = "|",         Infix = in(prio(190u),  arg_gt, arg_ge)
        ; Op = "\\",        Infix = in(prio(190u),  arg_ge, arg_gt)
        ; Op = ",",         Infix = in(prio(195u),  arg_gt, arg_ge)
        ; Op = "<->",       Infix = in(prio(300u),  arg_gt, arg_gt)
        ; Op = "->",        Infix = in(prio(400u),  arg_gt, arg_ge)
        ; Op = "<-",        Infix = in(prio(400u),  arg_gt, arg_ge)
        ; Op = "\\/",       Infix = in(prio(500u),  arg_ge, arg_gt)
        ; Op = "xor",       Infix = in(prio(500u),  arg_ge, arg_gt)
        ; Op = "/\\",       Infix = in(prio(600u),  arg_ge, arg_gt)
        ; Op = "<",         Infix = in(prio(700u),  arg_gt, arg_gt)
        ; Op = ">",         Infix = in(prio(700u),  arg_gt, arg_gt)
        ; Op = "<=",        Infix = in(prio(700u),  arg_gt, arg_ge)
        ; Op = ">=",        Infix = in(prio(700u),  arg_gt, arg_gt)
        ; Op = "=",         Infix = in(prio(700u),  arg_gt, arg_gt)
        ; Op = "!=",        Infix = in(prio(700u),  arg_gt, arg_gt)
        ; Op = ":=",        Infix = in(prio(1430u), arg_gt, arg_gt)
        ; Op = "@",         Infix = in(prio(1430u), arg_gt, arg_gt)
        ; Op = ".",         Infix = in(prio(1490u), arg_ge, arg_gt)
        ),
        OpInfos = op_infos(Infix, no_bin_pre, no_pre, no_post)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
