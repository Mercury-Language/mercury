:- module intermod_pragma_import2.

:- interface.

:- pred implemented_as_pragma_import(T::in, int::out) is det.

:- implementation.

:- pred p(T::in, int::out) is det.

p(_, 4).

:- pragma import(implemented_as_pragma_import(in, out), "imported").
:- pragma export(p(in, out), "imported").
