%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% File to test string.format predicate.
%
% The .exp file is for when int is 32-bit.
% The .exp2 file is for when int is 64-bit.
% The .exp3 file is for MSVC.
%
%---------------------------------------------------------------------------%

:- module string_format_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    Numba_0 = 32,
    Numba_1 = Numba_0 - 5,
    Numba_2 = Numba_0 + 5,
    Mg_poly = s("In the beginning there was the text, and the text was words, and yea, verily, I say unto you, there was too much of it and it had to be compressed."),

    Num_nr_1 = 9.9999,
%   builtin_float_times(2.0, Num_nr_1, Num_nr_2),
    Num_nr_2 = Num_nr_1 + Num_nr_1,

    string.format("First %#x characters of fig% 0.1f in MG are\n`%.*s'.\n",
        [i(Numba_0), f(1.4232), i(Numba_0), Mg_poly], String_0),
    string.format("First %#x characters of fig% 0.1f in MG are\n`%.*s'.\n",
        [i(Numba_1), f(1.4232), i(Numba_1), Mg_poly], String_1),
    string.format("First %#x characters of fig% 0.1f in MG are\n`%.*s'.\n",
        [i(Numba_2), f(1.4232), i(Numba_2), Mg_poly], String_2),

    string.format("Strangly, %5.2e%- 3c%-5.3f% -3c%-5.0E\n\n",
        [f(Num_nr_1), c('+'), f(Num_nr_1), c('='), f(Num_nr_2)], String_4),
    io.write_string(String_0, !IO),
    io.write_string(String_1, !IO),
    io.write_string(String_2, !IO),
    io.write_string("\nFollowing line should have 3, 4, 1 significant figures.\n", !IO),
    io.write_string(String_4, !IO),

    F_string0 = "!%0 10.5i!%0+10.5i!%0 -10.5i!%0+-10.5i!\n",
    F_string1 = "!% 10.5i!%+10.5i!% -10.5i!%+-10.5i!\n",
    F_string2 = "!% 10.5x!%+10.5x!% -10.5x!%+-10.5x!\n",
    F_string3="!%#10.x!%#+10.x!%#-10.x!%#+-10.x!\n",
    Num_1 = i(-31),
    Num_2 = i(31),
    string.format(F_string0, [Num_1, Num_1, Num_1, Num_1], String_5),
    string.format(F_string0, [Num_2, Num_2, Num_2, Num_2], String_6),
    string.format(F_string1, [Num_1, Num_1, Num_1, Num_1], String_7),
    string.format(F_string1, [Num_2, Num_2, Num_2, Num_2], String_8),
    string.format(F_string2, [Num_1, Num_1, Num_1, Num_1], String_9),
    string.format(F_string2, [Num_2, Num_2, Num_2, Num_2], String_10),
    string.format(F_string3, [Num_1, Num_1, Num_1, Num_1], String_11),
    string.format(F_string3, [Num_2, Num_2, Num_2, Num_2], String_12),

    io.nl(!IO),
    io.write_string(F_string0, !IO),
    io.write_string(F_string1, !IO),
    io.write_string(F_string2, !IO),
    io.write_string(F_string3, !IO),
    io.nl(!IO),
    io.write_string(String_5, !IO),
    io.write_string(String_6, !IO),
    io.write_string(String_7, !IO),
    io.write_string(String_8, !IO),
    io.write_string(String_9, !IO),
    io.write_string(String_10, !IO),
    io.write_string(String_11, !IO),
    io.write_string(String_12, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
