%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
% compress5.m
% Ralph Becket <rbeck@microsoft.com>
% Thu Nov  9 16:52:58 GMT 2000
%---------------------------------------------------------------------------%

:- module compress5.
:- interface.

:- import_module io.

:- pred go(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bmio. % XXX to get better intermodule optimization

:- import_module array.
:- import_module bitbuf2.
:- import_module bool.
:- import_module code_table4.
:- import_module int.
:- import_module require.
:- import_module store.
:- import_module util.

%---------------------------------------------------------------------------%

go(!IO) :-
    new_bitbuf(Buf, S),
    start(Buf, S, !IO).

%---------------------------------------------------------------------------%

:- pragma inline(start/4).
:- pred start(bitbuf(T)::in, T::di, io::di, io::uo) is det <= store(T).

start(Buf, !.S, !IO) :-
    read_byte(Result, Buf, !S, !IO),
    (
        Result = ok(B),
        main_loop(B, first_new_code, new_code_table, initial_bpc, Buf,
            !.S, !IO)
    ;
        Result = eof
    ;
        Result = error(ErrNo),
        error(io.error_message(ErrNo))
    ).

%---------------------------------------------------------------------------%

:- pred main_loop(code::in, code::in, code_table::array_di, int::in,
    bitbuf(T)::in, T::di, io::di, io::uo) is det <= store(T).

main_loop(C, N, T, BPC, Buf, !.S, !IO) :-
    read_byte(Result, Buf, !S, !IO),
    (
        Result = ok(B),
        lookup(C, B, T, H, K, C0),
        ( if C0 \= empty_code then
            main_loop(C0, N, T, BPC, Buf, !.S, !IO)
        else if N =< max_code then
            write_code(C, BPC, Buf, !S, !IO),
            main_loop(B, N + 1, set(T, H, K, N), update_bpc(N, BPC), Buf,
                !.S, !IO)
        else
            compression_ratio_fallen(Buf, Fallen, !S),
            ( if Fallen = no then
                write_code(C, BPC, Buf, !S, !IO),
                main_loop(B, N, T, BPC, Buf, !.S, !IO)
            else
                write_code(C, BPC, Buf, !S, !IO),
                write_code(B, BPC, Buf, !S, !IO),
                write_code(clear_code, BPC, Buf, !S, !IO),
                reset_compression_ratio(Buf, !S),
                start(Buf, !.S, !IO)
            )
        )

    ;   Result = eof,
        write_code(C, BPC, Buf, !S, !IO),
        flush_buffer(Buf, !.S, _, !IO)

    ;   Result = error(ErrNo),
        error(io.error_message(ErrNo))
    ).

%---------------------------------------------------------------------------%
:- end_module compress5.
%---------------------------------------------------------------------------%
