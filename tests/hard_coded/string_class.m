%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_class.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_is_all(is_all_alpha, "is_all_alpha", !IO),
    test_is_all(is_all_alpha_or_underscore, "is_all_alpha_or_underscore", !IO),
    test_is_all(is_all_alnum_or_underscore, "is_all_alnum_or_underscore", !IO),
    test_is_all(is_all_digits, "is_all_digits", !IO),
    test_is_all(all_match(nonascii), "all_match(nonascii) ", !IO).

:- pred test_is_all(pred(string)::in(pred(in) is semidet), string::in,
    io::di, io::uo) is det.

test_is_all(Pred, Name, !IO) :-
    test_is_all_2(Pred, Name, "", !IO),
    test_is_all_2(Pred, Name, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", !IO),
    test_is_all_2(Pred, Name, "abcdefghijklmnopqrstuvwxyz", !IO),
    test_is_all_2(Pred, Name, "0123456789", !IO),
    test_is_all_2(Pred, Name, "_", !IO),
    test_is_all_2(Pred, Name, "aßξ啕𐀀.", !IO),
    test_is_all_2(Pred, Name, "ßξ啕𐀀", !IO),
    io.nl(!IO).

:- pred test_is_all_2(pred(string)::in(pred(in) is semidet), string::in,
    string::in, io::di, io::uo) is det.

test_is_all_2(Pred, Name, Chars, !IO) :-
    ( if Pred(Chars) then
        io.format("%s(""%s"")\n", [s(Name), s(Chars)], !IO)
    else
        io.format("not %s(""%s"")\n", [s(Name), s(Chars)], !IO)
    ).

:- pred nonascii(char::in) is semidet.

nonascii(Char) :-
    char.to_int(Char, Int),
    Int > 0x7f.
