%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% Test the predicates for classifying various kinds of digits as well
% as conversion from digits to integers and back again.
%
%-----------------------------------------------------------------------------%

:- module test_char_digits.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    do_test("Testing binary digits", is_binary_digit, binary_digit_to_int,
        int_to_binary_digit, !IO),
    do_test("Testing octal digits", is_octal_digit, octal_digit_to_int,
        int_to_octal_digit, !IO),
    do_test("Testing decimal digits", is_decimal_digit, decimal_digit_to_int,
        int_to_decimal_digit, !IO),
    do_test("testing hex digits", is_hex_digit, hex_digit_to_int,
        int_to_hex_digit, !IO),
    int.fold_up(test_base, 2, 36, !IO).

:- pred do_test(
    string::in,
    pred(char)::in(pred(in) is semidet),
    pred(char, int)::in(pred(in, out) is semidet),
    pred(int, char)::in(pred(in, out) is semidet),
    io::di, io::uo) is det.

do_test(Header, IsDigit, ToInt, FromInt, !IO) :-
    io.format("==== %s ====\n", [s(Header)], !IO),
    list.foldl(do_test_char(IsDigit, ToInt, FromInt), digits, !IO).

:- pred do_test_char(
    pred(char)::in(pred(in) is semidet),
    pred(char, int)::in(pred(in, out) is semidet),
    pred(int, char)::in(pred(in, out) is semidet),
    char::in, io::di, io::uo) is det.

do_test_char(IsDigit, ToInt, FromInt, Char, !IO) :-
    io.format("'%c': ", [c(Char)], !IO),
    ( if IsDigit(Char)
    then io.write_string("yes : ", !IO)
    else io.write_string("no  : ", !IO)
    ),
    ( if ToInt(Char, Int) then
        io.format("%d : ", [i(Int)], !IO),
        ( if FromInt(Int, CharPrime) then
            io.format("'%c'", [c(CharPrime)], !IO)
        else
            io.write_string("*", !IO)
        )
    else
        io.write_string("*  : *", !IO)
    ),
    io.nl(!IO).

:- pred test_base(int::in, io::di, io::uo) is det.

test_base(Base, !IO) :-
    string.format("Testing base-%d digits", [i(Base)], Header),
    do_test(Header, is_base_digit(Base), base_digit_to_int(Base),
        base_int_to_digit(Base), !IO).

:- func digits = list(char).

digits = [
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
     'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
     'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
     'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
     'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
     'Y', 'Z', '!', '@', '?'].

%-----------------------------------------------------------------------------%
:- end_module test_char_digits.
%-----------------------------------------------------------------------------%
