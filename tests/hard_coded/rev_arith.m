%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test to see whether backwards arithmetic works.
%
% Note that currently only + and - can be run backwards;
% running * or // backwards would lead to multiple answers,
% and so we don't support that.
%

:- module rev_arith.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module std_util.

main(!IO) :-
    3 = 1 + A,
    3 = B + 2,
    C = 1 + 2,
%   10 = 2 * D,
%   10 = E * 5,
%   F = 2 * 5,
    20 = 30 - G,
    20 = H - 10,
    I = 30 - 10,
%   15 = 90 // J,
%   15 = K // 6,
%   L = 90 // 6

    io.write_int(A, !IO),
    io.write_string("\n", !IO),
    io.write_int(B, !IO),
    io.write_string("\n", !IO),
    io.write_int(C, !IO),
    io.write_string("\n", !IO),
%   io.write_int(D, !IO),
%   io.write_string("\n", !IO),
%   io.write_int(E, !IO),
%   io.write_string("\n", !IO),
%   io.write_int(F, !IO),
%   io.write_string("\n", !IO),
    io.write_int(G, !IO),
    io.write_string("\n", !IO),
    io.write_int(H, !IO),
    io.write_string("\n", !IO),
    io.write_int(I, !IO),
    io.write_string("\n", !IO).
%   io.write_int(J, !IO),
%   io.write_string("\n", !IO),
%   io.write_int(K, !IO),
%   io.write_string("\n", !IO),
%   io.write_int(L, !IO),
%   io.write_string("\n", !IO).
