%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case checks the handling of a situation in which a user uses
% = instead of == in a type equivalence, and thus unwittingly triggers code
% inside the compiler that considers this to be the introduction of an
% undiscriminated union type.
%
% This test case was contributed by Bart Demoen.

:- module uu_type.

:- interface.

:- type hiddenrenamedtype.

:- pred gen(hiddentype::out) is det.

:- implementation.

:- import_module int.

:- type hiddentype
    --->    f(int)
    ;       g(int).

:- type hiddenrenamedtype = hiddentype.

gen(f(4)).
