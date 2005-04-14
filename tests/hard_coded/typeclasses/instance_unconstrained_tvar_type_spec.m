:- module instance_unconstrained_tvar_type_spec.

:- interface.

:- import_module io, list.

:- pred main(io__state::di, io__state::uo) is det.

:- typeclass p(T) where [
        pred m(T, io__state, io__state),
        mode m(in, di, uo) is det
].

:- implementation.

:- instance p(list(T)) where [
        pred(m/3) is io__write
].

main -->
        call_m([1, 2, 3]),
        io__nl.

:- pred call_m(T::in, io__state::di, io__state::uo) is det <= p(T).
:- pragma type_spec(call_m/3, T = list(U)).
:- pragma no_inline(call_m/3).

call_m(T) -->
        m(T).

