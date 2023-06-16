%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that creation of a string containing a null character
% results in an exception being thrown.

:- module null_char.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module char.
:- import_module exception.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.write_string(
        "All tests should result in exceptions being thrown.\n", !IO),
    run_test(test1, !IO),
    run_test(test2, !IO),
    run_test(test3, !IO),
    run_test(test4, !IO),
    test_file(io.read_file_as_string, !IO),
    test_file(io.read_line_as_string, !IO).

:- pred test_file(pred(io.input_stream, T, io, io), io, io).
:- mode test_file((pred(in, out, di, uo) is det), di, uo) is det.

test_file(P, !IO) :-
    io.open_input("null_char.input_file", OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        P(Stream, Res, !IO),
        io.close_input(Stream, !IO),
        io.write(Res, !IO),
        io.nl(!IO)
    ;
        OpenRes = error(Err),
        io.write_string(io.error_message(Err), !IO),
        io.nl(!IO)
    ).

:- pred run_test(pred(T)::in(pred(out) is det), io::di, io::uo) is cc_multi.

run_test(P, !IO) :-
    try(P, Res),
    io.write_line(Res, !IO).

:- pred test1(string::out) is det.

test1(String) :-
    String = string.from_char_list(['1', 'a', nul, 'g']).

:- pred test2(string::out) is det.

test2(String) :-
    String = string.from_rev_char_list(['1', 'a', nul, 'g']).

:- pred test3(string::out) is det.

test3(String) :-
    String = string.det_set_char(nul, 2, "1234").

:- pred test4(string::out) is det.

test4(String) :-
    String = string.unsafe_set_char(nul, 2, "1234").

:- func nul = char.

nul = char.det_from_int(0).
