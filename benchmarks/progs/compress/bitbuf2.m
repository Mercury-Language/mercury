%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
% bitbuf2.m
% Ralph Becket <rbeck@microsoft.com>
% Fri Nov 10 18:03:25 GMT 2000
%---------------------------------------------------------------------------%

:- module bitbuf2.
:- interface.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module store.

%---------------------------------------------------------------------------%

:- type bitbuf(S).

:- some [S] pred new_bitbuf(bitbuf(S)::out, S::uo) is det => store(S).

:- pred read_byte(io.result(int)::out, bitbuf(S)::in, S::di, S::uo,
    io::di, io::uo) is det <= store(S).

:- pred write_code(int::in, int::in, bitbuf(S)::in, S::di, S::uo,
    io::di, io::uo) is det <= store(S).

:- pred compression_ratio_fallen(bitbuf(S)::in, bool::out, S::di, S::uo)
    is det <= store(S).

:- pred reset_compression_ratio(bitbuf(S)::in, S::di, S::uo)
    is det <= store(S).

:- pred flush_buffer(bitbuf(S)::in, S::di, S::uo, io::di, io::uo)
    is det <= store(S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bmio.
:- import_module util.

%---------------------------------------------------------------------------%

:- type bitbuf(S)
    --->    bitbuf(
                bits_in  :: generic_mutvar(int, S), % Number of bits read in.
                bits_out :: generic_mutvar(int, S), % Number of bits written out.
                ratio    :: generic_mutvar(int, S), % Compression ratio guide.
                data     :: generic_mutvar(int, S), % Buffer contents.
                size     :: generic_mutvar(int, S)  % Number of bits in buffer.
             ).

%---------------------------------------------------------------------------%

new_bitbuf(bitbuf(A,B,C,D,E), !:S) :-
    store.init(!:S),
    store.new_mutvar(0, A, !S),
    store.new_mutvar(0, B, !S),
    store.new_mutvar(0, C, !S),
    store.new_mutvar(0, D, !S),
    store.new_mutvar(0, E, !S).

%---------------------------------------------------------------------------%

read_byte(Result, Buf, !S, !IO) :-
    store.get_mutvar(Buf ^ bits_in, BitsIn, !S),
    bmio.read_byte(Result, !IO),
    store.set_mutvar(Buf ^ bits_in, BitsIn + 8, !S).

%---------------------------------------------------------------------------%

write_code(C, BPC, Buf, !S, !IO) :-
    store.get_mutvar(Buf ^ bits_in, BitsIn, !S),
    store.get_mutvar(Buf ^ bits_out, BitsOut, !S),
    store.get_mutvar(Buf ^ data, Data0, !S),
    store.get_mutvar(Buf ^ size, Size0, !S),

    write_full_bytes(Data0 \/ (C `lshift` Size0), Data,
        Size0 + BPC, Size, !IO),

    store.set_mutvar(Buf ^ bits_out, BitsOut + BPC, !S),
    store.set_mutvar(Buf ^ ratio, ratio(BitsIn, BitsOut), !S),
    store.set_mutvar(Buf ^ data, Data, !S),
    store.set_mutvar(Buf ^ size, Size, !S).

%---------------------------------------------------------------------------%

:- pred write_full_bytes(int::in, int::out, int::in, int::out,
    io::di, io::uo) is det.

write_full_bytes(!Data, !Size, !IO) :-
    ( if !.Size >= 8 then
        bmio.write_byte(!.Data, !IO),
        write_full_bytes(!.Data `rshift` 8, !:Data, !.Size - 8, !:Size, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

compression_ratio_fallen(Buf, Result, !S) :-
    store.get_mutvar(Buf ^ bits_in, BitsIn, !S),
    store.get_mutvar(Buf ^ bits_out, BitsOut, !S), 
    store.get_mutvar(Buf ^ ratio, Ratio, !S),
    Result = ( if ratio(BitsIn, BitsOut) < Ratio then yes else no ).

%---------------------------------------------------------------------------%

reset_compression_ratio(Buf, !S) :-
    store.set_mutvar(Buf ^ bits_in, 0, !S), 
    store.set_mutvar(Buf ^ bits_out, 0, !S),
    store.set_mutvar(Buf ^ ratio, 0, !S).

%---------------------------------------------------------------------------%

flush_buffer(Buf, !S, !IO) :-
    store.get_mutvar(Buf ^ data, Data, !S),
    bmio.write_byte(Data, !IO).

%---------------------------------------------------------------------------%
:- end_module bitbuf2.
%---------------------------------------------------------------------------%
