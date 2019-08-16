%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test array.fetch_items/4.
%

:- module array_fetch_items.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module exception.
:- import_module list.

main(!IO) :-
    test_fetch_items([1, 2, 3, 4, 5], 0, 4, !IO),
    test_fetch_items([1, 2, 3, 4, 5], 1, 3, !IO),
    test_fetch_items([1, 2, 3, 4, 5], 0, 0, !IO),
    test_fetch_items([1, 2, 3, 4, 5], -1, 0, !IO),
    test_fetch_items([1, 2, 3, 4, 5], 0, 6, !IO),
    test_fetch_items([1, 2, 3, 4, 5], 4, 2, !IO),
    test_fetch_items([1, 2, 3, 4, 5], 561, -1, !IO),
    test_fetch_items([1, 2, 3, 4, 5], 561, 561, !IO).

:- pred test_fetch_items(list(T)::in, int::in, int::in, io::di, io::uo)
    is cc_multi.

test_fetch_items(Elems, Lo, Hi, !IO) :-
    io.write_string("================\n", !IO),
    array.from_list(Elems, Array),
    io.write_string("Array = ", !IO),
    io.write_line(Array, !IO),
    io.write_string("Lo = ", !IO),
    io.write_line(Lo, !IO),
    io.write_string("Hi = ", !IO),
    io.write_line(Hi, !IO),
    ( try [] (
        array.fetch_items(Array, Lo, Hi, List)
    ) then
        io.write_string("List = ", !IO),
        io.write_line(List, !IO)
    catch index_out_of_bounds(S) ->
        io.write_string("EXCEPTION: ", !IO),
        io.write_line(S, !IO)
    ).
