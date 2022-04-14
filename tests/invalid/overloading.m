%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module overloading.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module term.

main -->
    { q(A, B, C) },
    { p(A, B, C, T) },
    io.write(T),
    io.nl.

:- type var_save_map    ==  map(var, save_map).
:- type save_map        ==  map(goal_path, string).
:- type goal_path   --->    goal_path(string).
:- type anchor      --->    anchor(string).
:- type interval_id --->    interval_id(int).

:- type opt_info
    ---> opt_info(
            must_have_own_slot  ::  set(var),
            ever_on_stack       ::  set(var),
            var_save_map        ::  var_save_map,
            cur_interval        ::  interval_id,
            interval_counter    ::  counter,
            interval_start      ::  map(interval_id, anchor),
            interval_end        ::  map(interval_id, anchor),
            interval_vars       ::  map(interval_id, set(var)),
            interval_succ       ::  map(interval_id, list(interval_id))
        ).

:- pred p(set(var)::in, set(var)::in, set(var)::in, opt_info::out) is det.

p(MustHaveOwnSlot, EverOnStack, OutputVars, OptInfo) :-
    Counter0 = counter.init(1),
    counter.allocate(CurInterval, Counter0, Counter1),
    CurIntervalId = interval_id(CurInterval),
    EndMap0 = map.det_insert(map.init, CurIntervalId, anchor("end")),
    StartMap0 = map.init,
    VarsMap0 = map.det_insert(map.init, CurIntervalId, OutputVars),
    SuccMap0 = map.init,
    OptInfo = opt_info(MustHaveOwnSlot, EverOnStack,
        CurIntervalId, map.init, Counter1, StartMap0, EndMap0,
        VarsMap0, SuccMap0).

:- pred q(set(var)::out, set(var)::out, set(var)::out) is det.
:- pragma external_pred(q/3).
