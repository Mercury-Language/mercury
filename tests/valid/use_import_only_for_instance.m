%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% The bug that this test case tests for is that the compiler reports
% the import of the "int" module below as unused, even though without it,
% it generates an error saying
%
% In clause for predicate `build_zones_to_dup_vars'/4:
%   unsatisfiable typeclass constraints: `enum.enum(int)', `enum.enum(int)'.
%
% This constraint is imposed by the use of sparse_bitset(int).

:- module use_import_only_for_instance.
:- interface.

:- import_module list.
:- import_module map.
:- import_module sparse_bitset.

:- type pvar == int.
:- type zone == int.
:- type vars_to_zones == map(pvar, sparse_bitset(zone)).
:- type zones_to_dup_vars == map(zone, list(pvar)).

:- pred build_zones_to_dup_vars(pvar::in, sparse_bitset(zone)::in,
    zones_to_dup_vars::in, zones_to_dup_vars::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

build_zones_to_dup_vars(Var, Zones, !ZonesToDupVars) :-
    ZoneList = sparse_bitset.to_sorted_list(Zones),
    ( if
        ZoneList = [FirstZone, _SecondZone | _],
        FirstZone \= 0
    then
        list.foldl(add_var_to_zone(Var), ZoneList, !ZonesToDupVars)
    else
        true
    ).

:- pred add_var_to_zone(pvar::in, zone::in,
    zones_to_dup_vars::in, zones_to_dup_vars::out) is det.

add_var_to_zone(Var, Zone, !ZonesToDupVars) :-
    ( if map.search(!.ZonesToDupVars, Zone, DupVars0) then
        DupVars = [Var | DupVars0],
        map.det_update(Zone, DupVars, !ZonesToDupVars)
    else
        map.det_insert(Zone, [Var], !ZonesToDupVars)
    ).
