%-----------------------------------------------------------------------------%
% abstract_typeclasses.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Oct 30 14:53:53 EST 2002
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module abstract_typeclasses.

:- interface.

:- import_module io, int.



:- typeclass tc(T).

:- instance tc(int).



:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string, list.



:- typeclass tc(T) where [ func double(T) = T ].

:- instance tc(int) where [ double(X) = X + X ].

%-----------------------------------------------------------------------------%

main(!IO) :-
    io__format("double(2) = %d\n", [i(double(2))], !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
