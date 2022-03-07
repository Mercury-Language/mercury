%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module browser_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module io.file.
:- import_module list.

:- type big
    --->    big(big, int, big)
    ;       small
    ;       seq(int, list(big), int).

main(!IO) :-
    % In case we have these files lying around.
    io.file.remove_file("browser_test.save.1", _, !IO),
    io.file.remove_file("browser_test.save.2", _, !IO),
    big_data(Data),
    io.print(Data, !IO),
    io.write_string(".\n", !IO),
    list_data(List),
    io.print(List, !IO),
    io.write_string(".\n", !IO),
    print_file("browser_test.save.1", !IO),
    print_file("browser_test.save.2", !IO),
    % Clean up after the test.
    io.file.remove_file("browser_test.save.1", _, !IO),
    io.file.remove_file("browser_test.save.2", _, !IO),
    a_func(Data) = Data2,
    write(Data2, !IO),
    nl(!IO).

:- pred big_data(big::out) is det.

big_data(Data) :-
    Data = big(
        big(
            big(
                small,
                1,
                small
            ),
            2,
            small
        ),
        3,
        big(
            big(
                small,
                4,
                big(
                    small,
                    5,
                    small
                )
            ),
            6,
            small
        )
    ).

:- pred list_data(big::out) is det.

list_data(Data) :-
    Element1 = big(
            big(
                small,
                1,
                small
            ),
            2,
            small
        ),
    Element2 = small,
    Element3 = big(
            small,
            4,
            big(
                small,
                5,
                small
            )
        ),
    Data = seq(1, [Element1, Element2, Element3], 5).

:- pred print_file(string::in, io::di, io::uo) is det.

print_file(FileName, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        io.read_file_as_string(Stream, ReadRes, !IO),
        (
            ReadRes = ok(Contents),
            io.write_string(FileName, !IO),
            io.write_string(":\n", !IO),
            io.write_string(Contents, !IO),
            io.write_string("\n", !IO)
        ;
            ReadRes = error(_, _),
            io.write_string("read failed\n", !IO)
        )
    ;
        OpenRes = error(_),
        io.write_string("open failed\n", !IO)
    ).

:- func a_func(big) = big.

a_func(_) = Big :-
    big_data(Big).
