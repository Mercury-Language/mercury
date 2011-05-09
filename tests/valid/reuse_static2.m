:- module reuse_static2.
:- interface.

:- import_module maybe.

:- type struct
    --->    struct(int).

:- pred bad(maybe(struct)::in, maybe(int)::out) is det.

:- implementation.

bad(Maybe, Res) :-
    (
	Maybe = yes(This),
	( if p(This, yes(17)) then
	    Res = yes(1)
	else
	    Res = yes(1)
	)
    ;
	Maybe = no,
	Res = no
    ).

:- pred p(struct::in, maybe(int)::out) is det.

p(This, T) :-
    This = struct(T0),
    T = yes(T0).

