% ---------------------------------------------------------------------------- %
% compress5.m
% Ralph Becket <rbeck@microsoft.com>
% Thu Nov  9 16:52:58 GMT 2000
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% ---------------------------------------------------------------------------- %

:- module compress5.

:- interface.

:- import_module io.

:- pred go(io__state, io__state).
:- mode go(di, uo) is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module bitbuf2, code_table4, util, int, array, require, store, bool.

:- import_module bmio. % XXX to get better intermodule optimization

% ---------------------------------------------------------------------------- %

go -->
    { new_bitbuf(Buf, S) },
    start(Buf, S).

% ---------------------------------------------------------------------------- %

:- pragma inline(start/4).
:- pred start(bitbuf(T), store(T), io__state, io__state).
:- mode start(in, di, di, uo) is det.

start(Buf, S) -->

    read_byte(Result, Buf, S, S0),

    (   { Result = ok(B) },
        main_loop(B, first_new_code, new_code_table, initial_bpc, Buf, S0)

    ;   { Result = eof }

    ;   { Result = error(ErrNo) },
        { error(io__error_message(ErrNo)) }
    ).

% ---------------------------------------------------------------------------- %

:- pred main_loop(code, code, code_table, int, bitbuf(T), store(T),
            io__state, io__state).
:- mode main_loop(in, in, array_di, in, in, di, di, uo) is det.

main_loop(C, N, T, BPC, Buf, S) -->
    read_byte(Result, Buf, S, S0),
    (   { Result = ok(B) },
        { lookup(C, B, T, H, K, C0) },
        ( if { C0 \= empty_code } then
            main_loop(C0, N, T, BPC, Buf, S0)
          else if { N =< max_code } then
            write_code(C, BPC, Buf, S0, S1),
            main_loop(B, N + 1, set(T, H, K, N), update_bpc(N, BPC), Buf, S1)
          else
            { compression_ratio_fallen(Buf, Fallen, S0, S1) },
            ( if { Fallen = no } then
                write_code(C, BPC, Buf, S1, S2),
                main_loop(B, N, T, BPC, Buf, S2)
              else
                write_code(C, BPC,          Buf, S1, S2),
                write_code(B, BPC,          Buf, S2, S3),
                write_code(clear_code, BPC, Buf, S3, S4),
                { reset_compression_ratio(  Buf, S4, S5) },
                start(Buf, S5)
            )
        )

    ;   { Result = eof },
        write_code(C, BPC, Buf, S0, S1),
        flush_buffer(Buf, S1, _S2)

    ;   { Result = error(ErrNo) },
        { error(io__error_message(ErrNo)) }
    ).

% ---------------------------------------------------------------------------- %
