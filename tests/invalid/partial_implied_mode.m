%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test for partially implied modes where the overbound
% term is partially instantiated. This really shouldn't be an error,
% and won't be with the alias tracking mode checker. It is difficult
% to make this program legal with the current mode checker without
% disallowing construction of partially instantiated terms.
%
% The reason partial_implied_mode_helper_1.m has to be a separate module
% is because unification procedures for local types have unique mode
% analysis run on them (although it is really not necessary).
% For local types, the code generator abort did not happen
% because unique_modes reported a mode error.
%
% Mercury rotd-1999-05-08 aborted during code generation on this test case.

:- module partial_implied_mode.

:- interface.

:- import_module list.
:- import_module map.
:- import_module partial_implied_mode_helper_1.

:- type quantitiesdico == map(quantity_key, physic_quantity).

:- pred search_quantitykey_1pin(pin::in, list(quantity_key)::in,
    quantitiesdico::in, quantity_key::out) is det.

:- implementation.

:- import_module require.

search_quantitykey_1pin(PIN, [CUR_K | L], QTY_DICO, K) :-
    ( if
        map.lookup(QTY_DICO, CUR_K,
            physic_quantity(PIN, _SYN, absol(_MEAS, _TBS)))
    then
        K = CUR_K
    else
        search_quantitykey_1pin(PIN, L, QTY_DICO, K)
    ).
search_quantitykey_1pin(_, [], _, _) :-
    error("search_quantitykey_1pin : no such absolute quantity in the list").
