%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

% Some checks of array implementation.

:- module array_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module bt_array.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    test([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], !IO).

:- pred test(list(int)::in, io::di, io::uo) is det.

test(Xs, !IO) :-
    CmpFunc =
        ( func(X, Y) = Res :-
            compare(Res, X, Y)
        ),
    CmpPred =
        ( pred(X :: in, Y :: in, Res :: out) is det :-
            compare(Res, X, Y)
        ),
    array.from_list(Xs, A0),
    array.to_list(A0, As0),
    write_message_int_list("A0: ", As0, !IO),
    array.max(A0, AMax0),
    write_message_int("AMax0: ", AMax0, !IO),
    array.min(A0, AMin0),
    write_message_int("AMin0: ", AMin0, !IO),
    array.size(A0, ASize),
    write_message_int("ASize: ", ASize, !IO),
    array.bounds(A0, AMin1, AMax1),
    write_message_int("AMin1: ", AMin1, !IO),
    write_message_int("AMax1: ", AMax1, !IO),
    ( if array.binary_search(CmpFunc, A0, 4, ABsearch) then
        MaybeABsearch = yes(ABsearch)
    else
        MaybeABsearch = no
    ),
    write_message_maybe_int("ABsearch: ", MaybeABsearch, !IO),
    array.set(8, 100, A0, A1),
    array.to_list(A1, As1),
    write_message_int_list("A1: ", As1, !IO),
    array.resize(15, 1000, A1, A2),
    array.to_list(A2, As2),
    write_message_int_list("A2: ", As2, !IO),
    array.shrink(10, A2, A3),
    array.to_list(A3, As3),
    write_message_int_list("A3: ", As3, !IO),
    A4 = array.sort(array(1 `..` 10)),
    array.to_list(A4, As4),
    write_message_int_list("A4: ", As4, !IO),
    A5 = array.sort(array(list.reverse(1 `..` 10))),
    array.to_list(A5, As5),
    write_message_int_list("A5: ", As5, !IO),
    A6 = array.from_reverse_list(Xs),
    As6 = array.to_list(A6),
    write_message_int_list("A6: ", As6, !IO),

    bt_array.from_list(0, Xs, B0),
    bt_array.to_list(B0, Bs0),
    write_message_int_list("B0: ", Bs0, !IO),
    bt_array.max(B0, BMax0),
    write_message_int("BMax0: ", BMax0, !IO),
    bt_array.min(B0, BMin0),
    write_message_int("BMin0: ", BMin0, !IO),
    bt_array.size(B0, BSize),
    write_message_int("BSize: ", BSize, !IO),
    bt_array.bounds(B0, BMin1, BMax1),
    write_message_int("BMin1: ", BMin1, !IO),
    write_message_int("BMax1: ", BMax1, !IO),
    ( bt_array.bsearch(B0, 4, CmpPred, BBsearch) ->
        MaybeBBsearch = yes(BBsearch)
    ;
        MaybeBBsearch = no
    ),
    write_message_maybe_int("BBsearch: ", MaybeBBsearch, !IO),
    bt_array.set(B0, 8, 100, B1),
    bt_array.to_list(B1, Bs1),
    write_message_int_list("B1: ", Bs1, !IO),
    bt_array.resize(B1, 0, 14, 1000, B2),
    bt_array.to_list(B2, Bs2),
    write_message_int_list("B2: ", Bs2, !IO),
    bt_array.shrink(B2, 0, 9, B3),
    bt_array.to_list(B3, Bs3),
    write_message_int_list("B3: ", Bs3, !IO),

    % Finally, just in case, compare the two implementations.
    ( if
        As0 = Bs0,
        AMax0 = BMax1,
        AMin0 = BMin1,
        ASize = BSize,
        AMin1 = BMin1,
        AMax1 = BMax1,
        AMax0 = AMax1,  % Sanity check
        AMin0 = AMin1,  % Sanity check
        BMax0 = BMax1,  % Sanity check
        BMin0 = BMin1,  % Sanity check
        MaybeABsearch = MaybeBBsearch,
        As1 = Bs1,
        As2 = Bs2,
        As3 = Bs3,
        As1 = As3,  % Sanity check
        Bs1 = Bs3   % Sanity check
    then
        io.write_string("Results all match\n", !IO)
    else
        io.write_string("Results don't match\n", !IO)
    ).

:- pred write_message_int(string::in, int::in, io::di, io::uo) is det.

write_message_int(Msg, O, !IO) :-
    io.write_string(Msg, !IO),
    io.write_int(O, !IO),
    io.nl(!IO).

:- pred write_message_maybe_int(string::in, maybe(int)::in,
    io::di, io::uo) is det.

write_message_maybe_int(Msg, no, !IO) :-
    io.write_string(Msg, !IO),
    io.write_string("no", !IO),
    io.nl(!IO).
write_message_maybe_int(Msg, yes(I), !IO) :-
    io.write_string(Msg, !IO),
    io.write_string("yes(", !IO),
    io.write_int(I, !IO),
    io.write_string(")", !IO),
    io.nl(!IO).

:- pred write_message_int_list(string::in, list(int)::in,
    io::di, io::uo) is det.

write_message_int_list(Msg, List, !IO) :-
    io.write_string(Msg, !IO),
    (
        List = [],
        io.write_string("[]", !IO)
    ;
        List = [I | Is],
        io.write_char('[', !IO),
        io.write_int(I, !IO),
        write_int_list_rest(Is, !IO)
    ),
    io.nl(!IO).

:- pred write_int_list_rest(list(int)::in, io::di, io::uo) is det.

write_int_list_rest([], !IO) :-
    io.write_char(']', !IO).
write_int_list_rest([I | Is], !IO) :-
    io.write_string(", ", !IO),
    io.write_int(I, !IO),
    write_int_list_rest(Is, !IO).
