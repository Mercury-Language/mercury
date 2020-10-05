%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% initialise_decl.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Aug 17 14:25:01 EST 2005
%
% Test the `:- initialise initpred' directive.
%
%---------------------------------------------------------------------------%

:- module initialise_decl.

:- interface.

:- import_module io.

:- pred main(io :: di, io :: uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- initialise i1/2.
:- initialize i2/2.

main(!IO) :-
    io.print("This is main/2.\n", !IO).

:- pred i1(io::di, io::uo) is det.
i1(!IO) :-
    io.print("This is the first initialise pred, i1/2.\n", !IO).

:- pred i2(io::di, io::uo) is det.
i2(!IO) :-
    io.print("This is the second initialise pred, i2/2.\n", !IO).

%---------------------------------------------------------------------------%
