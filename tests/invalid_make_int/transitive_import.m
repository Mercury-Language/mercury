%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Up until May 2001, types, insts, modes and
% typeclasses from transitively imported modules (for which the `.int2' file
% is read) could be used if each reference was fully module qualified.

:- module transitive_import.

:- interface.

:- import_module assoc_list.
:- import_module int.

:- type enum
    --->    a
    ;       b
    ;       c.

:- func assoc_list_first(assoc_list(int, V)) = pair.pair(int, V) is semidet.

:- implementation.

:- import_module list.

:- instance enum.enum(enum) where [
    to_int(a) = 1,
    to_int(b) = 2,
    to_int(c) = 3,
    from_int(1) = a,
    from_int(2) = b,
    from_int(3) = c
].

assoc_list_first([H | _]) = H.
