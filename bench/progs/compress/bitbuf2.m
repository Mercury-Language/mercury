% ---------------------------------------------------------------------------- %
% bitbuf2.m
% Ralph Becket <rbeck@microsoft.com>
% Fri Nov 10 18:03:25 GMT 2000
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% ---------------------------------------------------------------------------- %

:- module bitbuf2.

:- interface.

:- import_module io, bool, store, int.

:- type bitbuf(S).

:- some [S] pred new_bitbuf(bitbuf(S), store(S)).
:-          mode new_bitbuf(out, uo) is det.

:- pred read_byte(io__result(int), bitbuf(S), store(S), store(S),
            io__state, io__state).
:- mode read_byte(out, in, di, uo, di, uo) is det.

:- pred write_code(int, int, bitbuf(S), store(S), store(S),
            io__state, io__state).
:- mode write_code(in, in, in, di, uo, di, uo) is det.

:- pred compression_ratio_fallen(bitbuf(S), bool, store(S), store(S)).
:- mode compression_ratio_fallen(in, out, di, uo) is det.

:- pred reset_compression_ratio(bitbuf(S), store(S), store(S)).
:- mode reset_compression_ratio(in, di, uo) is det.

:- pred flush_buffer(bitbuf(S), store(S), store(S), io__state, io__state).
:- mode flush_buffer(in, di, uo, di, uo) is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module bmio, util.

% ---------------------------------------------------------------------------- %

:- type bitbuf(S)
    --->    bitbuf(
            bits_in     :: mutvar(int, S), % Number of bits read in.
            bits_out    :: mutvar(int, S), % Number of bits written out.
            ratio       :: mutvar(int, S), % Compression ratio guide.
            data        :: mutvar(int, S), % Buffer contents.
            size        :: mutvar(int, S)  % Number of bits in buffer.
        ).

% ---------------------------------------------------------------------------- %

new_bitbuf(bitbuf(A,B,C,D,E), S) :-
    store__new(                            S0),
    store__new_mutvar(0, A,                 S0, S1),
    store__new_mutvar(0, B,                 S1, S2),
    store__new_mutvar(0, C,                 S2, S3),
    store__new_mutvar(0, D,                 S3, S4),
    store__new_mutvar(0, E,                 S4, S).

% ---------------------------------------------------------------------------- %

read_byte(Result, Buf, S0, S) -->
    { store__get_mutvar(Buf ^ bits_in, BitsIn,      S0, S1) },
    bmio__read_byte(Result),
    { store__set_mutvar(Buf ^ bits_in, BitsIn + 8,  S1, S) }.

% ---------------------------------------------------------------------------- %

write_code(C, BPC, Buf, S0, S) -->
    { store__get_mutvar(Buf ^ bits_in, BitsIn,      S0, S1) },
    { store__get_mutvar(Buf ^ bits_out, BitsOut,    S1, S2) },
    { store__get_mutvar(Buf ^ data, Data0,          S2, S3) },
    { store__get_mutvar(Buf ^ size, Size0,          S3, S4) },

    write_full_bytes(Data0 \/ (C `lshift` Size0), Data, Size0 + BPC, Size),

    { store__set_mutvar(Buf ^ bits_out, BitsOut + BPC,          S4, S5) },
    { store__set_mutvar(Buf ^ ratio, ratio(BitsIn, BitsOut),    S5, S6) },
    { store__set_mutvar(Buf ^ data, Data,                       S6, S7) },
    { store__set_mutvar(Buf ^ size, Size,                       S7, S ) }.

% ---------------------------------------------------------------------------- %

:- pred write_full_bytes(int, int, int, int, io__state, io__state).
:- mode write_full_bytes(in, out, in, out, di, uo) is det.

write_full_bytes(Data0, Data, Size0, Size) -->
    ( if { Size0 >= 8 } then
        bmio__write_byte(Data0),
        write_full_bytes(Data0 `rshift` 8, Data, Size0 - 8, Size)
      else
        { Data = Data0 },
        { Size = Size0 }
    ).

% ---------------------------------------------------------------------------- %

compression_ratio_fallen(Buf, Result, S0, S) :-
    store__get_mutvar(Buf ^ bits_in, BitsIn,        S0, S1),
    store__get_mutvar(Buf ^ bits_out, BitsOut,      S1, S2),
    store__get_mutvar(Buf ^ ratio, Ratio,           S2, S ),
    Result = ( if ratio(BitsIn, BitsOut) < Ratio then yes else no ).

% ---------------------------------------------------------------------------- %

reset_compression_ratio(Buf, S0, S) :-
    store__set_mutvar(Buf ^ bits_in, 0,             S0, S1),
    store__set_mutvar(Buf ^ bits_out, 0,            S1, S2),
    store__set_mutvar(Buf ^ ratio, 0,               S2, S ).

% ---------------------------------------------------------------------------- %

flush_buffer(Buf, S0, S) -->
    { store__get_mutvar(Buf ^ data, Data,           S0, S ) },
    bmio__write_byte(Data).

% ---------------------------------------------------------------------------- %
