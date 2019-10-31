%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_append_pieces.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    foldl(test_append_string_pieces, test_cases, !IO).

:- func test_cases = list(list(string_piece)).

test_cases = [
    [],
    [string("")],
    [substring("", 0, 0)],
    [substring("ok", 2, 2)],
    [substring("axx", 0, 1), substring("xbx", 1, 2)],
    [
        string("c"),
        substring("whoops!", 2, 4),
        string("l!"),
        substring("😀😀😀", length("😀"), 2 * length("😀"))
    ],
    [substring("bad", -1, 0)],
    [substring("bad", 4, 3)],
    [substring("bad", 0, -1)],
    [substring("bad", 0, 4)],
    [substring("bad", 3, 2)]
].

:- pred test_append_string_pieces(list(string_piece)::in, io::di, io::uo)
    is cc_multi.

test_append_string_pieces(Pieces, !IO) :-
    ( try []
        append_string_pieces(Pieces, Str)
    then
        io.write_string("""", !IO),
        io.write_string(Str, !IO),
        io.write_string("""\n", !IO)
    catch_any Excp ->
        io.print_line(Excp, !IO)
    ).
