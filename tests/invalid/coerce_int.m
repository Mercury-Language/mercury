%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_int.
:- interface.

:- type wrap(T)
    --->    wrap(T).

:- inst wrap(I)
    --->    wrap(I).

:- implementation.

:- func ok(wrap(int)) = wrap(int).
:- mode ok(in(wrap(bound(1 ; 2 ; 3)))) = out(wrap(bound(3 ; 1 ; 2)))
    is det.

ok(X) = coerce(X).

:- func bad(wrap(int)) = wrap(int).
:- mode bad(in(wrap(bound(1 ; 2)))) = out(wrap(bound(1 ; 3)))
    is det.

bad(X) = coerce(X).

:- func bad_wrong_type(wrap(uint)) = wrap(uint).
:- mode bad_wrong_type(in(wrap(bound(1 ; 2)))) = out(wrap(bound(1 ; 2)))
    is det.

bad_wrong_type(X) = coerce(X).
