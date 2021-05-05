%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test type specialization of higher-order terms (the map.lookup).

:- module type_spec_ho_term.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module map.
:- import_module term.
:- import_module varset.

main(!IO) :-
    varset.init(VarSet0 `with_type` varset),
    varset.new_vars(4, Vars, VarSet0, _VarSet),
    map.from_corresponding_lists(Vars, ["a", "b", "c", "d"], Map),
    lookup_list(Map,
        [list.det_index1(Vars, 1), list.det_index1(Vars, 3)],
        List),
    io.write_list(List, ", ", io.write_string, !IO),
    io.nl(!IO).

:- pred lookup_list(map(T, U)::in, list(T)::in, list(U)::out) is det.
:- pragma type_spec(pred(lookup_list/3), T = var).

lookup_list(Map, List0, List) :-
    list.map(map.lookup(Map), List0, List).
