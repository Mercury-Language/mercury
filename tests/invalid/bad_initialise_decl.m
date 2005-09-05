%-----------------------------------------------------------------------------%
% bad_initialise_decl.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Aug 17 14:25:01 EST 2005
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Test error messages for the `:- initialise initpred' directive.
%
%-----------------------------------------------------------------------------%

:- module bad_initialise_decl.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- initialise i2.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- initialise i1.
:- initialise i3.

:- pred i1(T::di, T::uo) is det.
i1(X, X).

:- pred i2(io::in, io::out) is det.
i2(!IO).

main(!IO) :- io.print("This is main/2.\n", !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
