%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test handling of invalid module qualified cons_ids.
%

:- module qualified_cons_id.
:- interface.

:- import_module maybe.

:- type foo(T)
    --->    yes(T)
    ;       no.

:- inst yes
    --->    qualified_cons_id.yes(ground).

:- pred test(maybe(T), T).
:- mode test(in(bound(qualified_cons_id.yes(ground))), out) is det.
:- mode test(in(yes), out) is det.

:- implementation.

test(maybe.yes(T), T).
