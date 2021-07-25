%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. The 17-Nov-2002 version of the compiler got
% a runtime abort in the declarative debugger for the associated input script.
% The bug occurred during the conversion to typeinfos of the pseudotypeinfos
% describing xmap__init's two input arguments, TypeInfo_for_K and
% TypeInfo_for_V.
%
% The problem was caused by the type private_builtin.typeinfo being declared
% to have arity 1, when its true arity is variable, with the actual argument
% values being unused by the runtime system. At the call event of xmap_init,
% the types of the typeinfos include unbound type variables (K and V); the
% runtime system tried to follow a NULL pointer when attempting to look up
% the identities of the typeinfos bound to these type variables.

:- module mapinit.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module tree234.

:- type xmap(K, V) == tree234(K, V).

main(!IO) :-
    xmap_init(Init),
    xmap_set(Init, 0, "zero", Map),
    io.write_line(Map, !IO).

:- pred xmap_init(xmap(K, V)::out) is det.

xmap_init(Init) :-
    tree234.init(Init).

:- pred xmap_set(xmap(K, V)::in, K::in, V::in, xmap(K, V)::out) is det.

xmap_set(Map0, Key, Value, Map) :-
    tree234.set(Key, Value, Map0, Map).
