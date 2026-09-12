%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Benchmarking harness of string.base_string_to_int/3.
%
% TODO: add options for controlling leading zeros.
%
%---------------------------------------------------------------------------%

:- module benchmark_string_to_int.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module random.system_rng.
:- import_module random.sfc32.
:- import_module string.
:- import_module uint.
:- import_module uint32.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        short_option,
        long_option,
        option_default
    ),
    getopt.process_options(OptionOps, Args, NonOptionArgs, OptionResult),
    (
        OptionResult = ok(OptionTable),
        ( if getopt.lookup_bool_option(OptionTable, help, yes) then
            print_help_message(!IO)
        else
            (
                NonOptionArgs = [],
                handle_options_and_run_benchmark(OptionTable, !IO)
            ;
                NonOptionArgs = [_ | _],
                print_usage_error(!IO)
            )
        )
    ;
        OptionResult = error(Error),
        print_option_error(Error, !IO)
    ).

:- pred handle_options_and_run_benchmark(option_table(option)::in,
    io::di, io::uo) is cc_multi.

handle_options_and_run_benchmark(OptionTable, !IO) :-
    handle_options(OptionTable, SeedA, SeedB, SeedC, Base,
        MinDigits, MaxDigits, NumTestCases, Repeats, cord.init, Errors, !IO),
    ( if cord.is_empty(Errors) then
        run_benchmark(SeedA, SeedB, SeedC, Base, MinDigits, MaxDigits,
            NumTestCases, Repeats, !IO)
    else
        print_errors(Errors, !IO)
    ).

:- pred handle_options(option_table(option)::in,
    uint32::out, uint32::out, uint32::out, int::out, uint::out,
    uint::out, uint::out, int::out,
    cord(string)::in, cord(string)::out,
    io::di, io::uo) is det.

handle_options(OptionTable, SeedA, SeedB, SeedC, Base,
        MinDigits, MaxDigits, NumTestCases, Repeats, !Errors, !IO) :-
    getopt.lookup_int_option(OptionTable, base, OptBase),
    ( if OptBase >= 2, OptBase =< 36 then
        Base = OptBase
    else
        string.format("invalid value for option '--base': %d.",
            [i(OptBase)], InvalidBaseError),
        cord.snoc(InvalidBaseError, !Errors),
        Base = 0  % Dummy value.
    ),
    getopt.lookup_int_option(OptionTable, minimum_digits,
        OptMinDigits),
    ( if
        uint.from_int(OptMinDigits, MinDigits0),
        MinDigits0 > 0u
    then
        MinDigits = MinDigits0
    else
        InvalidMinDigitsError =
            "invalid value for option '--minimum-digits'.",
        cord.snoc(InvalidMinDigitsError, !Errors),
        MinDigits = 0u  % Dummy value.
    ),
    getopt.lookup_int_option(OptionTable, maximum_digits,
        OptMaxDigits),
    ( if
        uint.from_int(OptMaxDigits, MaxDigits0),
        MaxDigits0 > 0u
    then
        MaxDigits = MaxDigits0
    else
        InvalidMaxDigitsError =
            "invalid value for option '--maximum-digits'.",
        cord.snoc(InvalidMaxDigitsError, !Errors),
        MaxDigits = 0u  % Dummy value.
    ),
    ( if MinDigits > MaxDigits then
        string.format("--minimum-digits=%u > --maximum-digits=%u.",
            [u(MinDigits), u(MaxDigits)], MinMaxDigitsOverlapError),
        cord.snoc(MinMaxDigitsOverlapError, !Errors)
    else
        true
    ),
    getopt.lookup_int_option(OptionTable, num_test_cases, OptNumTestCases),
    ( if
        uint.from_int(OptNumTestCases, NumTestCases0),
        NumTestCases0 > 0u
    then
        NumTestCases = NumTestCases0
    else
        InvalidNumTestCasesError =
            "invalid value for option '--num-test-cases'.",
        cord.snoc(InvalidNumTestCasesError, !Errors),
        NumTestCases = 0u % Dummy value.
    ),
    getopt.lookup_int_option(OptionTable, num_repeats, OptRepeats),
    ( if
        OptRepeats < 1
    then
        InvalidRepeatsError =
            "value of option '--repeats' is less than one.",
        cord.snoc(InvalidRepeatsError, !Errors),
        Repeats = 0  % Dummy value.
    else
        Repeats = OptRepeats
    ),
    getopt.lookup_maybe_int_option(OptionTable, seed_a, MaybeSeedA),
    getopt.lookup_maybe_int_option(OptionTable, seed_b, MaybeSeedB),
    getopt.lookup_maybe_int_option(OptionTable, seed_c, MaybeSeedC),
    ( if
        MaybeSeedA = yes(OptSeedA),
        MaybeSeedB = yes(OptSeedB),
        MaybeSeedC = yes(OptSeedC)
    then
        ( if
            uint32.from_int(OptSeedA, SeedA0),
            uint32.from_int(OptSeedB, SeedB0),
            uint32.from_int(OptSeedC, SeedC0)
        then
            SeedA = SeedA0,
            SeedB = SeedB0,
            SeedC = SeedC0
        else
            InvalidSeedsError =
                "invalid value for one or more '--seed-' options.",
            cord.snoc(InvalidSeedsError, !Errors),
            SeedA = 0u32,
            SeedB = 0u32,
            SeedC = 0u32
        )
    else if
        MaybeSeedA = no,
        MaybeSeedB = no,
        MaybeSeedC = no
    then
        make_random_seeds(SeedA, SeedB, SeedC, !Errors, !IO)
    else
        MixedSeedSourceError = "Either all or none of " ++
            "'--seed-a', '--seed-b' and '--seed-c' must be specified.",
        cord.snoc(MixedSeedSourceError, !Errors),
        SeedA = 0u32,  % Dummy value.
        SeedB = 0u32,  % Dummy value.
        SeedC = 0u32   % Dummy value.
    ).

:- pred make_random_seeds(uint32::out, uint32::out, uint32::out,
    cord(string)::in, cord(string)::out, io::di, io::uo) is det.

make_random_seeds(SeedA, SeedB, SeedC, !Errors, !IO) :-
    system_rng.open_system_rng(MaybeRNG, !IO),
    (
        MaybeRNG = ok(RNG),
        system_rng.generate_uint32(RNG, SeedA, !IO),
        system_rng.generate_uint32(RNG, SeedB, !IO),
        system_rng.generate_uint32(RNG, SeedC, !IO),
        close_system_rng(RNG, !IO)
    ;
        MaybeRNG = error(Error),
        cord.snoc(Error, !Errors),
        SeedA = 0u32,  % Dummy value.
        SeedB = 0u32,  % Dummy value.
        SeedC = 0u32   % Dummy value.
    ).

:- pred run_benchmark(uint32::in, uint32::in, uint32::in, int::in,
    uint::in, uint::in, uint::in, int::in, io::di, io::uo) is cc_multi.

run_benchmark(SeedA, SeedB, SeedC, Base, MinDigits, MaxDigits, NumTestCases,
        Repeats, !IO) :-
    random.sfc32.seed(SeedA, SeedB, SeedC, RNG, State),
    make_io_urandom(RNG, State, IO_RNG, !IO),
    make_test_cases(IO_RNG, Base, NumTestCases, MinDigits, MaxDigits,
        TestCases, !IO),
    benchmark_det_io(test_pred(Base), TestCases, {Successes, Fails},
        !IO, Repeats, Time),
    OptionString = make_options_string(SeedA, SeedB, SeedC, Base, MinDigits,
        MaxDigits, NumTestCases, Repeats),
    io.format("Seed A: %u\n", [u32(SeedA)], !IO),
    io.format("Seed B: %u\n", [u32(SeedB)], !IO),
    io.format("Seed C: %u\n", [u32(SeedC)], !IO),
    io.format("Base: %d\n", [i(Base)], !IO),
    io.format("Min. Digits: %u\n", [u(MinDigits)], !IO),
    io.format("Max. Digits: %u\n", [u(MaxDigits)], !IO),
    io.format("Bits-per-int: %d\n", [i(bits_per_int)], !IO),
    io.format("Num. Tests: %u\n", [u(NumTestCases)], !IO),
    io.format("Repeats: %d\n", [i(Repeats)], !IO),
    io.format("Successes: %u\n", [u(Successes)], !IO),
    io.format("Failures: %u\n", [u(Fails)], !IO),
    io.format("Options: %s\n", [s(OptionString)], !IO),
    io.format("Total Time: %d ms\n", [i(Time)], !IO).

%---------------------------------------------------------------------------%

:- func make_options_string(uint32, uint32, uint32, int, uint, uint,
    uint, int) = string.

make_options_string(SeedA, SeedB, SeedC, Base, MinDigits, MaxDigits,
        NumTestCases, Repeats) = String :-
    string.format("-a %u -b %u -c %u -B %d -l %u -u %u -n %u -r %d",
        [u32(SeedA), u32(SeedB), u32(SeedC), i(Base), u(MinDigits),
         u(MaxDigits), u(NumTestCases), i(Repeats)], String).

%---------------------------------------------------------------------------%

:- type option
    --->    help

    ;       base
    ;       num_test_cases
    ;       num_repeats
    ;       minimum_digits
    ;       maximum_digits

    ;       seed_a
    ;       seed_b
    ;       seed_c.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('B', base).
short_option('n', num_test_cases).
short_option('r', num_repeats).
short_option('l', minimum_digits).
short_option('u', maximum_digits).
short_option('a', seed_a).
short_option('b', seed_b).
short_option('c', seed_c).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("base", base).
long_option("num-test-cases", num_test_cases).
long_option("num-repeats", num_repeats).
long_option("repeats", num_repeats).
long_option("min-digits", minimum_digits).
long_option("minimum-digits", minimum_digits).
long_option("max-digits", maximum_digits).
long_option("maximum-digits", maximum_digits).
long_option("seed-a", seed_a).
long_option("seed-b", seed_b).
long_option("seed-c", seed_c).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help, bool(no)).
option_default(base, int(10)).
option_default(num_test_cases, int(1_000_000)).
option_default(num_repeats, int(10)).
option_default(minimum_digits, int(1)).
option_default(maximum_digits, int(31)).
option_default(seed_a, maybe_int(no)).
option_default(seed_b, maybe_int(no)).
option_default(seed_c, maybe_int(no)).

%---------------------------------------------------------------------------%

:- pred test_pred(int::in, list(string)::in, {uint, uint}::out,
    io::di, io::uo) is det.

test_pred(Base, TestCases, {Successes, Fails}, !IO) :-
    list.foldl3(do_conversion(Base), TestCases, 0u, Successes, 0u, Fails, !IO).

:- pred do_conversion(int::in, string::in, uint::in, uint::out,
    uint::in, uint::out, io::di, io::uo) is det.

do_conversion(Base, String, !Successes, !Fails, !IO) :-
    ( if base_string_to_int(Base, String, Int) then
        !:Successes = !.Successes + 1u,
        consume_int(Int, !IO)
    else
        !:Fails = !.Fails + 1u
    ).

:- pragma no_inline(pred(consume_int/3)).
:- pred consume_int(int::in, io::di, io::uo) is det.

consume_int(_, !IO).

%---------------------------------------------------------------------------%

:- pred make_test_cases(RNG::in, int::in, uint::in, uint::in, uint::in,
    list(string)::out, State::di, State::uo) is det <= urandom(RNG, State).

make_test_cases(RNG, Base, N, MinDigits, MaxDigits, TestCases, !State) :-
    do_make_test_cases(RNG, Base, N, MinDigits, MaxDigits, [], TestCases,
        !State).

:- pred do_make_test_cases(RNG::in, int::in, uint::in, uint::in, uint::in,
    list(string)::in, list(string)::out, State::di, State::uo)
    is det <= urandom(RNG, State).

do_make_test_cases(RNG, Base, N, MinDigits, MaxDigits, !TestCases, !State) :-
    ( if N = 0u then
        true
    else
        uniform_uint_in_range(RNG, MinDigits, MaxDigits - MinDigits + 1u,
            NumDigits, !State),
        generate_n_random_digits(RNG, Base, NumDigits, [], Digits0, !State),
        uniform_int_in_range(RNG, 0, 3, PickSign, !State),
        ( if PickSign = 0 then
            Digits = Digits0 % No sign character.
        else if PickSign = 1 then
            Digits = ['+' | Digits0]
        else
            Digits = ['-' | Digits0]
        ),
        TestCase = from_char_list(Digits),
        !:TestCases = [TestCase | !.TestCases],
        do_make_test_cases(RNG, Base, N - 1u, MinDigits, MaxDigits, !TestCases,
            !State)
    ).

:- pred generate_n_random_digits(RNG::in, int::in, uint::in,
    list(char)::in, list(char)::out, State::di, State::uo)
    is det <= urandom(RNG, State).

generate_n_random_digits(RNG, Base, N, !Digits, !State) :-
    ( if N = 0u then
        true
    else
        uniform_int_in_range(RNG, 0, Base, Int, !State),
        Digit = det_base_int_to_digit(Base, Int),
        !:Digits = [Digit | !.Digits],
        generate_n_random_digits(RNG, Base, N - 1u, !Digits, !State)
    ).

%---------------------------------------------------------------------------%

:- pred print_help_message(io::di, io::uo) is det.

print_help_message(!IO) :-
    io.write_strings([
        "Name: benchmark_string_to_int\n",
        "\n",
        "Usage: benchmark_string_to_int [<options>]\n",
        "\n",
        "Description:\n"
    ], !IO),
    io.write_prefixed_lines("\t", [
        "Benchmark string-to-int conversion in the standard library.",
        "Specifically, benchmark string.base_string_to_int/3 on randomly",
        "generated test strings. The test strings have an equal probability",
        "of having a postive, negative or no sign character.",
        "Prints a summary to the standard output."
    ], !IO),
    io.nl(!IO),
    io.nl(!IO),
    io.write_string("Options:\n", !IO),
    io.write_prefixed_lines("\t", [
        "-h, --help",
        "\tPrint this information and exit.",

        "-B <n>, --base <n>",
        "\tSpecify the base of the digits in the strings. n must be between",
        "\t2 and 36 (inclusive). Default: 10.",

        "-n <n>, --num-test-cases <n>",
        "\tSpecify the number of test cases to generate. n must be greater",
        "\tthan zero. Default: 1_000_000.",

        "-r <n>, --repeats <n>, --num-repeats <n>",
        "\tSpecify the number of test repetitions: n must be greater than",
        "\tzero. Default: 10.",

        "-l <min>, --min-digits <min>, --minimum-digits <min>",
        "\tSpecify the minimum number of digits in the test cases.",
        "\tmin must be greater than zero and must be less than or equal to",
        "\tthe value of the '--maximum-digits' option.",
        "\tDefault: 1.",

        "-u <max>, --max-digits <max>, --maximum-digits <max>",
        "\tSpecify the maximum number of digits in the test cases.",
        "\tmax must be greater than zero and must be greater than or equal",
        "\tto the value of the 'minimum-digits' option.",
        "\tDefault: 31.",

        "-a <seed>, --seed-a <seed>",
        "-b <seed>, --seed-b <seed>",
        "-c <seed>, --seed-c <seed>",
        "\tSpecify the seeds of the SFC32 pseudo-random number generator used",
        "\tto generate the test cases. If no seed options are specified, then",
        "\tthe seeds will be randomly generated using the system RNG.",
        "\tIf seeds are specified, then all three seed options must occur on",
        "\tthe command line."
    ], !IO).

%---------------------------------------------------------------------------%

:- pred print_option_error(option_error(option)::in, io::di, io::uo) is det.

print_option_error(Error, !IO) :-
    Msg = option_error_to_string(Error),
    io.stderr_stream(Stderr, !IO),
    io.format(Stderr, "error: %s.\n", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

:- pred print_usage_error(io::di, io::uo) is det.

print_usage_error(!IO) :-
    io.stderr_stream(Stderr, !IO),
    io.print_line(Stderr, "Usage: benchmark_string_to_int [<options>]", !IO),
    io.set_exit_status(1, !IO).

:- pred print_errors(cord(string)::in, io::di, io::uo) is det.

print_errors(Errors, !IO) :-
    io.stderr_stream(Stderr, !IO),
    cord.foldl_pred(print_error(Stderr), Errors, !IO),
    io.set_exit_status(1, !IO).

:- pred print_error(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

print_error(Stream, Error, !IO) :-
    io.format(Stream, "Error: %s\n", [s(Error)], !IO).

%---------------------------------------------------------------------------%
:- end_module benchmark_string_to_int.
%---------------------------------------------------------------------------%
