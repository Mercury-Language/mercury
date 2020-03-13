%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test error messages for the `:- initialise initpred' directive.

:- module bad_initialise_decl.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- initialise i2/2.

%---------------------------------------------------------------------------%

:- implementation.

:- initialise i1/2.
:- initialise i3/2.
:- initialise i4.
:- initialise i5/6.

:- pred i1(T::di, T::uo) is det.
i1(X, X).

:- pred i2(io::in, io::out) is det.
i2(!IO).

main(!IO) :-
    io.print("This is main/2.\n", !IO).

%---------------------------------------------------------------------------%
