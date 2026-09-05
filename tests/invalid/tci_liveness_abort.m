%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tci_liveness_abort.

:- interface.

:- import_module list.

:- type object.

:- type value.

:- type kind
    --->    k1
    ;       k2
    ;       k3.

:- pred test1(object::in, kind::in, list(value)::in, list(value)::out) is det.

:- pred test2(object::in, kind::in, value::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type object
    --->    object(
                class :: object_class
            ).

:- type object_class
    --->    error
    ;       some [T] host(T) => host(T).

:- type value
    --->    null.

:- typeclass host(T) where [
    pred get_value(object, T, value),
    mode get_value(in, in, out) is det
].

test1(Ob, Kind, !List) :-
    (
        Kind = k1
    ;
        ( Kind = k2
        ; Kind = k3
        ),
        ( if Ob ^ class = host(HostOb) then
            get_value(Ob, HostOb, Value),
            (
                Kind = k2,
                Item = Value
            ;
                Kind = k3,
                Item = null
            ),
            cons(Item, !List)
        else
            true
        )
    ).

test2(Ob, Kind, Item) :-
    require_complete_switch [Kind]
    (
        Kind = k1,
        fail
    ;
        ( Kind = k2
        ; Kind = k3
        ),
        Ob ^ class = host(HostOb),
        get_value(Ob, HostOb, Value),
        (
            Kind = k2,
            Item = Value
        ;
            Kind = k3,
            Item = null
        )
    ).
