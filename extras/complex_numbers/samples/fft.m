%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% File: fft.m.
% Main author: conway.
% Date: August 1996.
%
% This source file is hereby placed in the public domain.
%   -conway (the author).
%
% This module provides a predicate for performing the Fast Fourier Transform
% (fft) on a list of complex numbers. The list must be a power of 2 in length
% (ie 4, 8, 16, ... elements).
%
% The algorithm used here is derived from a few sources:
%   - "Numerical Recipes in C": Press, et al
%   - "Elements of Computer Music": Moore
%   - Lecture notes from 433-325 "Mathematical Software B": Dr Rex Harris
%
% It actually took the combination of these sources for me to uncover the
% algorithm for combining the component transforms. This code is not maximally
% efficient, but rather is intended as a clear presentation of the FFT
% algorithm.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module fft.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module complex_numbers.
:- import_module complex_numbers.complex.
:- import_module complex_numbers.float_imag.
:- import_module complex_numbers.imag.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module require.

%-----------------------------------------------------------------------------%

main(!IO) :-
    Zero = 0.0 + 0.0 * i,
    One  = 1.0 + 0.0 * i,
    fft([One, Zero, One, Zero], T),
    io.write_line(T, !IO).

:- pred fft(list(complex)::in, list(complex)::out) is det.

fft(Ins, Outs) :-
    % First put the list into bit-reversed order.
    bit_rev(Ins, Shuffle),
    list.length(Shuffle, NInt),
    int.log2(NInt, R),
    N = float(NInt),
    % Now recombine the component transforms.
    combine(N, 1.0, R, Shuffle, Outs).

:- pred bit_rev(list(T)::in, list(T)::out) is det.

bit_rev([], []).
bit_rev([X], [X]).
bit_rev([X1, X2 | Xs], List) :-
    split([X1, X2 | Xs], List1, List2),
    bit_rev(List1, List3),
    bit_rev(List2, List4),
    list.append(List3, List4, List).

:- pred split(list(T)::in, list(T)::out, list(T)::out) is det.

split([], [], []).
split([_], _, _) :-
    error("Input sequence not a power of two in length").
split([X1, X2|Xs], [X1|Ys], [X2|Zs]) :-
    split(Xs, Ys, Zs).

:- pred combine(float::in, float::in, int::in,
    list(complex)::in, list(complex)::out) is det.

combine(N, K, R, Ins, Outs) :-
    ( if
        R = 0
    then
        Outs = Ins
    else
        R1 = R - 1,
        L = 1 << R1,
        % Split the list in half.
        divide(L, Ins, Fs0, Ss0),
        K2 = K * 2.0,
        % Now combine the transforms in the respective halves.
        combine(N, K2, R1, Fs0, Fs),
        combine(N, K2, R1, Ss0, Ss),
        % Now perform the 'butterfly'.
        xform(Fs, Ss, complex(1.0), w(K, N), Rs0, Rs1),
        list.append(Rs0, Rs1, Outs)
    ).

:- pred xform(list(complex)::in, list(complex)::in, complex::in, complex::in,
    list(complex)::out, list(complex)::out) is det.

xform([], [_ | _], _WJN, _WKN, _, _) :-
    error("ran out of Di's").
xform([_ | _], [], _WJN, _WKN, _, _) :-
    error("ran out of Dj's").
xform([], [], _WJN, _WKN, [], []).
xform([Di0 | Dis0], [Dj0 | Djs0], WJN, WKN, [Di | Dis], [Dj | Djs]) :-
    Tmp = WJN * Dj0,
    Di = Di0 + Tmp,
    Dj = Di0 - Tmp,
    xform(Dis0, Djs0, WJN * WKN, WKN, Dis, Djs).

:- pred divide(int::in, list(T)::in, list(T)::out, list(T)::out) is det.

divide(R, Ins, Fs, Ss) :-
    ( if
        R =< 0
    then
        Fs = [],
        Ss = Ins
    else
        (
            Ins = [],
            error("divide error!")
        ;
            Ins = [F | Ins1],
            divide(R - 1, Ins1, Fs1, Ss),
            Fs = [F | Fs1]
        )
    ).

:- func w(float, float) = complex.

w(J, N) = cis(Theta) :-
    Theta = J * 2.0 * pi / N.

%-----------------------------------------------------------------------------%
:- end_module fft.
%-----------------------------------------------------------------------------%
