%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

% Test the parsing of numeric literals from strings.

:- module parse_number_from_string.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module mercury_term_parser.
:- import_module term.
:- import_module term_io.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.print_line("Valid decimal literals:", !IO),
    list.foldl(run_test, valid_decimal_cases, !IO),
    io.print_line("\nInvalid decimal literals:", !IO),
    list.foldl(run_test, invalid_decimal_cases, !IO),
    io.print_line("\nValid binary literals:", !IO),
    list.foldl(run_test, valid_binary_cases, !IO),
    io.print_line("\nInvalid binary literals:", !IO),
    list.foldl(run_test, invalid_binary_cases, !IO),
    io.print_line("\nValid octal literals:", !IO),
    list.foldl(run_test, valid_octal_cases, !IO),
    io.print_line("\nInvalid octal literals:", !IO),
    list.foldl(run_test, invalid_octal_cases, !IO),
    io.print_line("\nValid hexadecimal literals:", !IO),
    list.foldl(run_test, valid_hex_cases, !IO),
    io.print_line("\nInvalid hexadecimal literals:", !IO),
    list.foldl(run_test, invalid_hex_cases, !IO),
    io.print_line("\nValid float literals:", !IO),
    list.foldl(run_test, valid_float_cases, !IO),
    io.print_line("\nInvalid float literals:", !IO),
    list.foldl(run_test, invalid_float_cases, !IO).

:- pred run_test(string::in, io::di, io::uo) is det.

run_test(String, !IO) :-
    io.format("read_term(\"%s\") = ", [s(String)], !IO),
    read_term_from_string("", String, _Posn, Result : read_term),
    (
        Result = eof,
        io.print_line("<<eof>>", !IO)
    ;
        Result = error(Msg, _),
        io.print_line(Msg, !IO)
    ;
        Result = term(_Varset, Term),
        io.print_line(Term, !IO)
    ).

%---------------------------------------------------------------------------%

:- func valid_decimal_cases = list(string).

valid_decimal_cases = [
    "0.",
    "-0.",
    "00.",
    "0_0.",
    "10.",
    "-10.",
    "1_0.",
    "-1_0.",
    "01.",
    "0_1.",
    "1_000_000_000_000_000_000_000.",
    "-1_000_000_000_000_000_000_000."
].

:- func invalid_decimal_cases = list(string).

invalid_decimal_cases = [
    "123_.",
    "-123_.",
    "-_123"
].

:- func valid_binary_cases = list(string).

valid_binary_cases = [
    "0b0.",
    "-0b0.",
    "0b_1.",
    "-0b_1.",
    "0b_1000_100.",
    "-0b_1000_100."
].

:- func invalid_binary_cases = list(string).

invalid_binary_cases =[
    "0b.",
    "-0b.",
    "0b_.",
    "-0b_.",
    "0b11_.",
    "-0b11_."
].

:- func valid_octal_cases = list(string).

valid_octal_cases = [
    "0o77.",
    "-0o77.",
    "0o_77.",
    "-0o_77.",
    "0o_7_7.",
    "-0o_7_7.",
    "0o_7__7___7.",
    "-0o_7__7___7."
].

:- func invalid_octal_cases = list(string).

invalid_octal_cases = [
    "0o.",
    "-0o",
    "0o_.",
    "-0o_.",
    "0o77_.",
    "-0o77_."
].

:- func valid_hex_cases = list(string).

valid_hex_cases = [
    "0xff.",
    "-0xff.",
    "0x_ff.",
    "-0x_ff.",
    "0xf_f.",
    "-0xf_f.",
    "0x_f_f__f.",
    "-0x_f_f__f.",
    "0xfffffffffffffffffffffffff.",
    "-0xfffffffffffffffffffffffff."
].

:- func invalid_hex_cases = list(string).

invalid_hex_cases = [
    "0x.",
    "-0x.",
    "0x_.",
    "-0x_.",
    "0xff_.",
    "-0xff_."
].

:- func valid_float_cases = list(string).

valid_float_cases = [
    "0.123.",
    "-0.123.",
    "0.1_2__3.",
    "-0.1_2__3.",
    "1.123.",
    "-1.123.",
    "1_2.123.",
    "-1_2.123.",
    "1__2.1_2__3.",
    "-1__2.1_2__3.",
    "1_2_3e1_1.",
    "1_2_3E1_1.",
    "1_2e+1_1.",
    "1_2E+1_1.",
    "1_2e-1_1.",
    "1_2E-1_1.",
    "00.0.",
    "0_0.0.",
    "01.0.",
    "0_1.0."
].

:- func invalid_float_cases = list(string).

invalid_float_cases = [
    "1_2_3.1_2_3_.",
    "1_2_3e1_2_3_.",
    "123_._123.",
    "123._123.",
    "123_.123.",
    "123_e12.",
    "123_E12.",
    "123e_12.",
    "123E_12.",
    "123e12_.",
    "123E12_.",
    "12_e11.",
    "12_E11.",
    "123.12e-_12.",
    "123.12e+_12.",
    "123.12e12_.",
    "123.12E12_."
].
