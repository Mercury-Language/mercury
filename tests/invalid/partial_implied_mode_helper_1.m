%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Type definitions for partial_implied_mode.m.

:- module partial_implied_mode_helper_1.

:- interface.

:- import_module list.
:- import_module maybe.
:- import_module pair.

:- type pin.
:- type quantity_key.
:- type measure == int.
:- type tbs == int.

:- type physic_quantity
    --->    physic_quantity(pin, list(quantity_key), physic_nature).

:- type physic_nature
    --->    direct
    ;       absol(maybe(measure), maybe(tbs))
    ;       diff(pin, maybe(pair(quantity_key, quantity_key)),
                maybe(measure), maybe(tbs)).

:- type port_name_ref == int.
:- type master_port
    --->    port_name_ref(port_name_ref)
    ;       anonymous_master_port(int).

:- type instance_path == list(int).
:- type component_key
    --->    component(instance_path).

:- type net_path == list(int).
:- type net_key
    --->    anonymous_net(int)
    ;       named_net(net_path).

:- type pin
    --->    net_pin(net_key)
    ;       component_pin(component_key, master_port).

:- type quantity_key == int.
