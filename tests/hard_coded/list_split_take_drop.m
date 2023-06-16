%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module list_split_take_drop.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.

main(!IO) :-
    list.foldl(run_test, tests, !IO).

:- pred run_test(pair(int, list(int))::in, io::di, io::uo) is det.

run_test(N - List, !IO) :-
    NStr = describe_int(N),
    ListStr = string(List),

    % Test split_list/4.
    %
    io.format("split_list(%s, %s) ===> ", [s(NStr), s(ListStr)], !IO),
    ( if list.split_list(N, List, SplitStart, SplitEnd) then
        io.format("(%s, %s)\n",
            [s(string(SplitStart)), s(string(SplitEnd))], !IO),
        MaybeSplitStart = yes(SplitStart),
        MaybeSplitEnd = yes(SplitEnd)
    else
        write_false(!IO),
        MaybeSplitStart = no,
        MaybeSplitEnd = no
    ),

    % Test drop/3.
    %
    io.format("drop(%s, %s) ===> ", [s(NStr), s(ListStr)], !IO),
    ( if list.drop(N, List, DropEnd) then
        io.format("%s\n", [s(string(DropEnd))], !IO),
        MaybeDropEnd = yes(DropEnd)
    else
        write_false(!IO),
        MaybeDropEnd = no
    ),

    % Test take/3.
    %
    io.format("take(%s, %s) ===> ", [s(NStr), s(ListStr)], !IO),
    ( if list.take(N, List, TakeStart) then
        io.format("%s\n", [s(string(TakeStart))], !IO),
        MaybeTakeStart = yes(TakeStart)
    else
        write_false(!IO),
        MaybeTakeStart = no
    ),

    % Check that drop(N, List, End) <=> split_list(N, List, _, End).
    %
    ( if MaybeSplitEnd = MaybeDropEnd then
        true
    else
        io.format("ERROR: split_list/4 and drop/3 differ for (%s, %s)\n",
            [s(NStr), s(ListStr)], !IO)
    ),

    % Check that take(N, List, Start) <=> split_list(N, List, Start, _)
    %
    ( if MaybeSplitStart = MaybeTakeStart then
        true
    else
        io.format("ERROR: split_list/4 and take/3 differ for (%s, %s)\n",
            [s(NStr), s(ListStr)], !IO)
    ),
    io.nl(!IO).

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

:- pred write_false(io::di, io::uo) is det.

write_false(!IO) :-
    io.write_string("<<FALSE>>\n", !IO).

%---------------------------------------------------------------------------%
:- end_module list_split_take_drop.
%---------------------------------------------------------------------------%
