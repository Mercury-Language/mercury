%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_from_code_unit_list.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if length("😀") = 4 then
        Cases = test_cases_8
    else
        Cases = test_cases_16
    ),
    list.foldl(test_from_code_unit_list, Cases, !IO),
    io.nl(!IO),
    list.foldl(test_from_code_unit_list_allow_ill_formed, Cases, !IO).

:- func test_cases_8 = list(list(int)).

test_cases_8 = [
    [],
    [65, 0, 66],    % null char
    [0xf0, 0x9f, 0x98, 0x80],
    % Sign extend first byte: the old implementation used to silently ignore
    % higher bits so this case would succeed.
    [\0xff \/ 0xf0, 0x9f, 0x98, 0x80],
    [0x80, 0x98, 0x9f, 0xf0]
].

:- func test_cases_16 = list(list(int)).

test_cases_16 = [
    [],
    [65, 0, 66],
    [0xd83d, 0xde00],
    [\0xffff \/ 0xd83d, 0xde00],
    [0xde00, 0xd83d]
].

:- pred test_from_code_unit_list(list(int)::in, io::di, io::uo) is det.

test_from_code_unit_list(CodeList, !IO) :-
    io.write_string("from_code_unit_list(", !IO),
    io.write(CodeList, !IO),
    io.write_string(")", !IO),
    ( if string.from_code_unit_list(CodeList, String) then
        io.write_string(" = """, !IO),
        io.write_string(String, !IO),
        io.write_string("""\n", !IO)
    else
        io.write_string(" failed\n", !IO)
    ).

:- pred test_from_code_unit_list_allow_ill_formed(list(int)::in,
    io::di, io::uo) is det.

test_from_code_unit_list_allow_ill_formed(CodeList, !IO) :-
    io.write_string("from_code_unit_list_allow_ill_formed(", !IO),
    io.write(CodeList, !IO),
    io.write_string(")", !IO),
    ( if string.from_code_unit_list_allow_ill_formed(CodeList, String) then
        string.to_code_unit_list(String, CodeListB),
        io.write_string(" = ", !IO),
        io.write(CodeListB, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string(" failed\n", !IO)
    ).
