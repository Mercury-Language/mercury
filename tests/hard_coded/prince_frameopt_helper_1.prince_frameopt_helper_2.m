%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module prince_frameopt_helper_1.prince_frameopt_helper_2.

:- interface.

:- import_module list.

:- func new_prules = list(property).

:- type property
    --->    max_width(spec(max_width)).

:- type max_width
    --->    length(length)
    ;       percent(float).

:- type spec(T)
    --->    inherit
    ;       value(T).

:- implementation.

:- func get_max_width(value) = max_width is det.

get_max_width(V) =
    ( if get_percent(V) = W then
        percent(W)
    else
        length(get_length(V))
    ).

:- func spec((func(value) = T), value) = spec(T).
:- mode spec((func(in) = out is det), in) = out is det.

spec(F, V) =
    ( if V = ident("inherit") then
        inherit
    else
        value(F(V))
    ).

new_prules = [max_width(spec(get_max_width, percent(int(100))))].
