%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% The .exp file is for C grades that use double-precision floats.
% The .exp2 file is for the Java grades.
% The .exp3 file is for the C# grades.
% The .exp4 file is for C grades that use single-precision floats.
%
%---------------------------------------------------------------------------%


:- module parse_number_from_io.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.print_line("Decimal:", !IO),
    io.print_line(0, !IO),
    io.print_line(-0, !IO),
    io.print_line(00, !IO),
    io.print_line(0_0, !IO),
    io.print_line(10, !IO),
    io.print_line(-10, !IO),
    io.print_line(1_0, !IO),
    io.print_line(-1_0, !IO),
    io.print_line(01, !IO),
    io.print_line(0_1, !IO),
    io.print_line(-01, !IO),
    io.print_line(-0_1, !IO),
    io.nl(!IO),

    io.print_line("Binary:", !IO),
    io.print_line(0b0, !IO),
    io.print_line(-0b0, !IO),
    io.print_line(0b_1, !IO),
    io.print_line(-0b_1, !IO),
    io.print_line(0b_1000_100, !IO),
    io.print_line(-0b_1000_100, !IO),
    io.nl(!IO),

    io.print_line("Octal:", !IO),
    io.print_line(0o777, !IO),
    io.print_line(-0o777, !IO),
    io.print_line(0o_777, !IO),
    io.print_line(-0o_777, !IO),
    io.print_line(0o_7_7_7, !IO),
    io.print_line(-0o_7_7_7, !IO),
    io.print_line(0o_7__7___7, !IO),
    io.print_line(-0o_7__7___7, !IO),
    io.nl(!IO),

    io.print_line("Hexadecimal:", !IO),
    io.print_line(0xff, !IO),
    io.print_line(-0xff, !IO),
    io.print_line(0x_ff, !IO),
    io.print_line(-0x_ff, !IO),
    io.print_line(0xf_f, !IO),
    io.print_line(-0xf_f, !IO),
    io.print_line(0x_f_f__f, !IO),
    io.nl(!IO),

    io.print_line("Float:", !IO),
    io.print_line(0.123, !IO),
    io.print_line(-0.123, !IO),
    io.print_line(0.1_2__3, !IO),
    io.print_line(-0.1_2__3, !IO),
    io.print_line(1.123, !IO),
    io.print_line(-1.123, !IO),
    io.print_line(1_2.123, !IO),
    io.print_line(-1_2.123, !IO),
    io.print_line(1__2.1_2__3, !IO),
    io.print_line(-1__2.1_2__3, !IO),
    io.print_line(1_2_3e1_1, !IO),
    io.print_line(1_2_3E1_1, !IO),
    io.print_line(1_2e+1_1, !IO),
    io.print_line(1_2E+1_1, !IO),
    io.print_line(1_2e-1_1, !IO),
    io.print_line(1_2E-1_1, !IO),
    io.print_line(0_0.0, !IO),
    io.print_line(0_1.0_1, !IO).
