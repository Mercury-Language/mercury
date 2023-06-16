%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The following program used to expose a bug in the runtime's handling
% of deep profiled code. When compiled in a deep profiling grade at -O2
% or below it used to abort with a segmentation fault.
%
% The problem was caused by the code in builtin.{unify, compare} for du types
% having inappropriate references to the proc layout structures of the dummy
% unify and compare predicates for the dummy type builtin.user_by_rtti. If the
% first call to builtin.{unify, compare} at a given call site had the equality
% pretest of the two arguments succeed, this used to fill in the call site
% dynamic structure with a pointer to the proc dynamic structure of this dummy
% predicate, leading later calls through that call site to refer to the wrong
% data structure.

:- module profdeep_seg_fault.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- type t_type
    --->    t_bool
    ;       t_set(t_type)
    ;       t_list(t_type)
    ;       t_enum(string)
    ;       t_typevar(string).

:- type t_sig
    --->    t_sig(list(t_type), t_type).

:- type type_inst
    --->    ti_par_bool
    ;       ti_par_typevar(string).

:- type ti_sig
    --->    ti_sig(list(type_inst), type_inst).

main(!IO) :-
    A = [
        ti_sig([ti_par_bool], ti_par_bool),
        ti_sig([ti_par_bool], ti_par_bool)
    ],
    B = [
        ti_sig([ti_par_typevar("FOO")], ti_par_bool),
        ti_sig([ti_par_typevar("BAR")], ti_par_bool)
    ],
    AllBuiltinSigs = [A, B],
    add_sigs_to_sym(AllBuiltinSigs, [], S),
    io.write(S, !IO),
    io.nl(!IO).

:- pred add_sigs_to_sym(list(list(ti_sig))::in,
    list(t_sig)::in, list(t_sig)::out) is det.

add_sigs_to_sym([], S, S).
add_sigs_to_sym([TISigs | TISigs0], S0, S) :-
    ti_sigs_to_t_sigs(TISigs, TSigs0),
    insertion_sort(TSigs0, [], TSigs),
    append_type_sigs(TSigs, S0, S1),
    add_sigs_to_sym(TISigs0, S1, S).

:- pred ti_sigs_to_t_sigs(list(ti_sig)::in, list(t_sig)::out) is det.

ti_sigs_to_t_sigs([], []).
ti_sigs_to_t_sigs([X | Xs], [Y | Ys]) :-
    X = ti_sig(ArgTIs, RetTI),
    type_insts_to_t_types(ArgTIs, ArgTs),
    (
        RetTI = ti_par_bool,
        RetT = t_bool
    ;
        RetTI = ti_par_typevar(V),
        RetT = t_typevar(V)
    ),
    Y = t_sig(ArgTs, RetT),
    ti_sigs_to_t_sigs(Xs, Ys).

:- pred type_insts_to_t_types(list(type_inst)::in, list(t_type)::out) is det.

type_insts_to_t_types([], []).
type_insts_to_t_types([X | Xs], [Y | Ys]) :-
    (
        X = ti_par_bool,
        Y = t_bool
    ;
        X = ti_par_typevar(V),
        Y = t_typevar(V)
    ),
    type_insts_to_t_types(Xs, Ys).

:- pred append_type_sigs(list(t_sig)::in, list(t_sig)::in,
    list(t_sig)::out) is det.

append_type_sigs([], Zs, Zs).
append_type_sigs([X | Xs], Ys, [X | Zs]) :-
    append_type_sigs(Xs, Ys, Zs).

:- pred insertion_sort(list(t_sig)::in, list(t_sig)::in, list(t_sig)::out)
    is det.

insertion_sort([], Zs, Zs).
insertion_sort([X | Xs], Ys0, Zs) :-
    insert(X, Ys0, Ys),
    insertion_sort(Xs, Ys, Zs).

:- pred insert(t_sig::in, list(t_sig)::in, list(t_sig)::out) is det.

insert(X, [], [X]).
insert(X, [Y | Ys], Zs) :-
    ( if
        compare(Res, X, Y),
        Res = (>)
    then
        insert(X, Ys, Zs0),
        Zs = [Y | Zs0]
    else
        Zs = [X, Y | Ys]
    ).
