%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module rng3.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module rng.
:- import_module rng.binfile.
:- import_module string.

main(!IO) :-
    open("rng3.data", Res, !IO),
    (
        Res = ok(RPbin),
        test(20, RPbin, !IO),
        expect_eof(RPbin, !IO),
        close(RPbin, !IO)
    ;
        Res = error(E),
        io.progname($module, Name, !IO),
        io.format("%s: %s\n", [s(Name), s(error_message(E))], !IO)
    ).

:- pred test(int, binfile, io, io).
:- mode test(in, in, di, uo) is det.

test(Count, RP, !IO) :-
    ( if Count > 0 then
        rand(RP, N, !IO),
        io.write_uint64(N, !IO),
        io.nl(!IO),
        test(Count - 1, RP, !IO)
    else
        true
    ).

:- pred expect_eof(binfile, io, io).
:- mode expect_eof(in, di, uo) is cc_multi.

expect_eof(RP, !IO) :-
    ( try [io(!IO)]
        rand(RP, _, !IO)
    then
        io.write_string("EOF not found!\n", !IO)
    catch _ : software_error ->
        true
    ).

