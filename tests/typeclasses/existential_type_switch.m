%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test: rotd-1999-10-12 did not handle switches on existential
% data types.
%---------------------------------------------------------------------------%

:- module existential_type_switch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass tc(TC) where [
    pred r(TC, int),
    mode r(in, out) is semidet
].

:- type maybe
    --->    no
    ;       some [T] (yes(T) => tc(T)).

:- pred p(maybe).
:- mode p(out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

main(!IO) :-
    p(X),
    (
        X = yes(Y),
        ( if r(Y, Z) then
            io.print("r succeeded, Z = ", !IO),
            io.print_line(Z, !IO)
        else
            io.print_line("r failed", !IO)
        )
    ;
        X = no,
        io.print_line("no", !IO)
    ).

p(X) :-
    ( if q(1, Y) then
        X = 'new yes'(Y)
    else
        X = no
    ).

:- some [T2] pred q(int::in, T2::out) is semidet => tc(T2).

q(1, 2).

%---------------------------------------------------------------------------%

:- instance tc(int) where [
    pred(r/2) is s
].

:- pred s(int::in, int::out) is semidet.

s(1, 111).
s(2, 42).

%---------------------------------------------------------------------------%
