%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test list.split_upto/4 and list.take_upto/3.

:- module take_split_upto.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_take_upto_test, tests, !IO),
    io.nl(!IO),
    list.foldl(run_split_upto_test, tests, !IO).

:- pred run_take_upto_test(pair(int, list(int))::in, io::di, io::uo)
    is cc_multi.

run_take_upto_test(N - List, !IO) :-
    NStr = describe_int(N),
    io.format("take_upto(%s, %s) ===> ", [s(NStr), s(string(List))], !IO),
    (
        try [] (
            list.take_upto(N, List, Start)
        )
    then
        io.format("%s\n", [s(string(Start))], !IO)
    catch_any _ ->
        io.write_string("<<exception>>\n", !IO)
    ).

:- pred run_split_upto_test(pair(int, list(int))::in, io::di, io::uo)
    is cc_multi.

run_split_upto_test(N - List, !IO) :-
    NStr = describe_int(N),
    io.format("split_upto(%s, %s) ===> ", [s(NStr), s(string(List))], !IO),
    (
        try [] (
            list.split_upto(N, List, Start, End)
        )
    then
        io.format("(%s, %s)\n", [s(string(Start)), s(string(End))], !IO)
    catch_any _ ->
        io.write_string("<<exception>>\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- func tests = assoc_list(int, list(int)).

tests = [
    int.min_int - [],
    int.min_int - [111],
    int.min_int - [111, 222],
    -1 - [],
    -1 - [111],
    -1 - [111, 222],
    0 - [],
    0 - [111],
    0 - [111, 222],
    1 - [],
    1 - [111],
    1 - [111, 222],
    2 - [],
    2 - [111],
    2 - [111, 222],
    2 - [111, 222, 333],
    int.max_int - [],
    int.max_int - [111],
    int.max_int - [111, 222]
].

:- func describe_int(int) = string.

describe_int(N) =
    ( if N = int.min_int then
        "int.min_int"
    else if N = int.max_int then
        "int.max_int"
    else
        int_to_string(N)
    ).

%---------------------------------------------------------------------------%
:- end_module take_split_upto.
%---------------------------------------------------------------------------%
