%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et tw=0
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
:- import_module term_io.
:- import_module ops.
:- import_module pair.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    term_io.read_term_with_op_table(cadmium_op_table, Res, !IO),
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
    pred(lookup_op/2) is lookup_cadmium_op,
    pred(lookup_op_infos/4) is lookup_cadmium_op_infos,
    pred(lookup_operator_term/4) is lookup_cadmium_operator_term,
    func(max_priority/1) is cadmium_max_priority,
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
    priority::out, assoc::out, assoc::out) is semidet.

lookup_cadmium_infix_op(_, Name, Priority, LeftAssoc, RightAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table, Name, OpInfos),
    find_first(is_infix_op, OpInfos, OpInfo),
    OpInfo = op_info(infix(LeftAssoc, RightAssoc), Priority).

%---------------------------------------------------------------------------%

:- pred is_infix_op(op_info::in) is semidet.

is_infix_op(op_info(infix(_, _), _)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_operator_term(cadmium_op_table::in, priority::out,
    assoc::out, assoc::out) is semidet.

lookup_cadmium_operator_term(_, 100, y, x) :-
    semidet_true.

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_prefix_op(cadmium_op_table::in, string::in,
    priority::out, assoc::out) is semidet.

lookup_cadmium_prefix_op(_, Name, Priority, LeftAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table, Name, OpInfos),
    find_first(is_prefix_op, OpInfos, OpInfo),
    OpInfo = op_info(prefix(LeftAssoc), Priority).

%---------------------------------------------------------------------------%

:- pred is_prefix_op(op_info::in) is semidet.

is_prefix_op(op_info(prefix(_), _)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_binary_prefix_op(cadmium_op_table, string, priority,
    assoc, assoc).
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
    priority::out, assoc::out) is semidet.

lookup_cadmium_postfix_op(_, Name, Priority, LeftAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table, Name, OpInfos),
    find_first(is_postfix_op, OpInfos, OpInfo),
    OpInfo = op_info(postfix(LeftAssoc), Priority).

%---------------------------------------------------------------------------%

:- pred is_postfix_op(op_info::in) is semidet.

is_postfix_op(op_info(postfix(_),_)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_op(cadmium_op_table::in, string::in) is semidet.

lookup_cadmium_op(_, Name) :-
    cadmium_op_table(Name, _).

%---------------------------------------------------------------------------%

:- func cadmium_max_priority(cadmium_op_table) = priority.

cadmium_max_priority(_) = 1400.

%---------------------------------------------------------------------------%

:- func cadmium_arg_priority(cadmium_op_table) = priority.

cadmium_arg_priority(_) = comma_priority - 1.
    % See ops.m docs for an explanation of this.

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

:- func comma_priority = int.

comma_priority = 1305.

:- pred cadmium_op_table(string::in, op_info::out) is nondet.

cadmium_op_table("import",      op_info(prefix(y),  1400)).
cadmium_op_table("ruleset",     op_info(prefix(y),  1400)).
cadmium_op_table("transform",   op_info(prefix(y),  1400)).
cadmium_op_table("<=>",         op_info(infix(x, y), 1350)).
cadmium_op_table("|",           op_info(infix(x, y), 1310)).
cadmium_op_table("\\",          op_info(infix(y, x), 1310)).
cadmium_op_table(",",           op_info(infix(x, y), comma_priority)).

cadmium_op_table("<->",         op_info(infix(x, x), 1200)).

cadmium_op_table("->",          op_info(infix(x, y), 1100)).
cadmium_op_table("<-",          op_info(infix(x, y), 1100)).

cadmium_op_table("\\/",         op_info(infix(y, x), 1000)).
cadmium_op_table("xor",         op_info(infix(y, x), 1000)).

cadmium_op_table("/\\",         op_info(infix(y, x), 900)).

cadmium_op_table("<",           op_info(infix(x, x), 800)).
cadmium_op_table(">",           op_info(infix(x, x), 800)).
cadmium_op_table("<=",          op_info(infix(x, y), 800)).
cadmium_op_table(">=",          op_info(infix(x, x), 800)).
cadmium_op_table("=",           op_info(infix(x, x), 800)).
cadmium_op_table("!=",          op_info(infix(x, x), 800)).

cadmium_op_table("+",           op_info(infix(y, x), 400)).
cadmium_op_table("-",           op_info(infix(y, x), 400)).

cadmium_op_table("+",           op_info(prefix(x), 90)).
cadmium_op_table("-",           op_info(prefix(x), 90)).

cadmium_op_table(":=",          op_info(infix(x, x), 70)).
cadmium_op_table("@",           op_info(infix(x, x), 70)).
cadmium_op_table(".",           op_info(infix(y, x), 10)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
