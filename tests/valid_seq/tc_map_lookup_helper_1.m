%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tc_map_lookup_helper_1.
:- interface.

:- type t ---> t.

:- implementation.

:- import_module term_to_xml.

:- typeclass tc(T) <= xmlable(T) where [
    func f(T) = string
].

