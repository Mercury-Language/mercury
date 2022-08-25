%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .exp file is for ???.
% The .exp2 file is for ???.
% The .exp3 file is for asm_fast.gc and asm_fast.gc.debug.

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
    remove_dump_files(!IO),
    big_data(Data),
    io.print(Data, !IO),
    io.write_string(".\n", !IO),
    list_data(List),
    io.print(List, !IO),
    io.write_string(".\n", !IO),
    print_dump_files(!IO),
    % Clean up after the test.
    remove_dump_files(!IO),
    a_func(Data) = Data2,
    io.write(Data2, !IO),
    io.nl(!IO).

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

:- pred remove_dump_files(io::di, io::uo) is det.

remove_dump_files(!IO) :-
    io.file.remove_file("browser_test.save.1", _, !IO),
    io.file.remove_file("browser_test.save.2", _, !IO),
    io.file.remove_file("browser_test.save.3", _, !IO),
    io.file.remove_file("browser_test.save.4", _, !IO).

:- pred print_dump_files(io::di, io::uo) is det.

print_dump_files(!IO) :-
    print_file("browser_test.save.1", !IO),
    print_file("browser_test.save.2", !IO),
    print_file("browser_test.save.3", !IO),
    print_file("browser_test.save.4", !IO).
