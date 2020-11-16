%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The code generated for this test case by the compiler
% of 13/1/2001 aborted with the following output:
%
%   yes(135251024)
%   yes("
%   *** Mercury runtime: caught segmentation violation ***
%
% The type-infos for T and U were being passed in the wrong order.

:- module typeclass_order_bug.

:- interface.

:- import_module io.
:- import_module maybe.

:- pred main(io::di, io::uo) is det.

:- typeclass p(T, U) where [
    pred m(U, T, io, io),
    mode m(in, in, di, uo) is det
].

:- implementation.

:- instance p(maybe(T), maybe(U)) where [
    pred(m/4) is write_maybe_pair
].

main(!IO) :-
    m(yes("ok"), yes(1), !IO),
    io.nl(!IO).

:- pred write_maybe_pair(maybe(T)::in, maybe(U)::in, io::di, io::uo) is det.

write_maybe_pair(MaybeT, MaybeU, !IO) :-
    io.write_line(MaybeT, !IO),
    io.write_line(MaybeU, !IO).
