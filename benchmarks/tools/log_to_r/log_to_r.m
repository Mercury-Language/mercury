
:- module log_to_r.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module parsing_utils.
:- import_module require.
:- import_module string.
:- import_module unit.

:- import_module csv.

main(!IO) :-
    command_line_arguments(Args, !IO),
    OptionOpts = option_ops_multi(short, long, defaults),
    getopt.process_options(OptionOpts, Args, InputFiles, MaybeOptions),
    (
        MaybeOptions = ok(Options),
        getopt.lookup_bool_option(Options, help_option, HelpOption),
        ( HelpOption = yes ->
            io.stdout_stream(Stdout, !IO),
            show_help(Stdout, !IO)
        ;
            getopt.lookup_string_option(Options, input_format_option, 
                InputFormatString),
            ( format_type_string(InputFormatString, InputFormat) ->
                (
                    InputFormat = ft_pybench,
                    map_foldl(parse_pybench_file, InputFiles, Outputs, !IO),
                    CSVHead = ["test", "run", "user", "sys", "wall", "cpu", 
                        "major_faults", "minor_faults", 
                        "volun_switches", "involun_switches"]
                ;
                    InputFormat = ft_speedtest,
                    map_foldl(parse_speedtest_file, InputFiles, Outputs, !IO),
                    CSVHead = ["test", "mcflags", "cflags", "mlflags", "grade", 
                        "user", "sys", "wall", "cpu", 
                        "major_faults", "minor_faults", "io_in", "io_out"]
                ),
                condense(Outputs, CSVRows),
                CSV = csv(CSVHead, CSVRows),
                write_csv("stats.csv", CSV, !IO)
            ;
                error("Incorrect input format: " ++ InputFormatString)
            )
        )
    ;
        MaybeOptions = error(Error),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, "Error passing command line options: " ++
            option_error_to_string(Error), !IO),
        show_help(Stderr, !IO),
        io.set_exit_status(1, !IO)
    ).

:- type option
    --->    input_format_option
    ;       help_option.

:- pred short(char::in, option::out) is semidet.

short('f', input_format_option).
short('h', help_option).

:- pred long(string::in, option::out) is semidet.

long("format",  input_format_option).
long("help",    help_option).

:- pred defaults(option::out, option_data::out) is multi.

defaults(input_format_option, string(String)) :-
    format_type_string(String, ft_speedtest).
defaults(help_option, bool(no)).

:- type format_type
    --->    ft_speedtest
                % The mercury tools/speedtest script's format.
    ;       ft_pybench.
                % My python test script's format.

:- pred format_type_string(string, format_type).
:- mode format_type_string(in, out) is semidet.
:- mode format_type_string(out, in) is det.

format_type_string("speedtest", ft_speedtest).
format_type_string("pybench", ft_pybench).

:- pred show_help(output_stream::in, io::di, io::uo) is det.

show_help(Stream, !IO) :-
    io.progname("log_to_r", ProgName, !IO),
    io.format(Stream, "%s [options] log_files...\n\tNormal execution\n",
        [s(ProgName)], !IO),
    io.format(Stream, "%s -h\n\tShow this help message\n", [s(ProgName)], !IO),

    io.write_string(Stream, "\nOptions:\n", !IO),
    io.write_string(Stream, "\t-f --format (speedtest|pybench)\n", !IO),
    io.write_string(Stream, "\t\tSet input log format\n", !IO).

%----------------------------------------------------------------------------%
%
% Parsing the pybench logfile. 
%

:- pred parse_pybench_file(string::in, list(csv_row)::out, io::di, io::uo) 
    is det.

parse_pybench_file(InputFile, Rows, !IO) :-
    open_input(InputFile, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        read_file_as_string(Stream, Result, !IO),
        (
            Result = ok(String)
        ;
            Result = error(_, Error),
            error(error_message(Error))
        ),
        close_input(Stream, !IO)
    ;
        OpenResult = error(Error),
        error(error_message(Error))
    ),
    new_src_and_ps(String, Src, PS0),
    ( parse_pybench_log(Src, RowStatusPairs, PS0, _PS) ->
        TestName = remove_suffix_if_present(".log", InputFile),
        filter_map(discard_failed_and_add_test_name(TestName), 
            RowStatusPairs, Rows)
    ;
        error("Parser error")
    ).

:- pred parse_pybench_log(src::in, list(pair(csv_row, int))::out, 
    ps::in, ps::out) is semidet.

parse_pybench_log(Src, RowStatusPairs, !PS) :-
    zero_or_more(parse_pybench_test, Src, RowStatusPairs, !PS),
    eof(Src, _, !PS).

:- pred parse_pybench_test(src::in, pair(csv_row, int)::out, ps::in, ps::out) 
    is semidet.

parse_pybench_test(Src, (Row - ExitStatus), !PS) :-
    whitespace(Src, _, !PS),
    
    % Rep
    pybench_field("Rep", Src, Rep, !PS),

    optional(pybench_exit_status_line, Src, MaybeValue, !PS),
    (
        MaybeValue = yes(ExitStatus)
    ;
        MaybeValue = no,
        ExitStatus = 0
    ),

    % Command line
    skip_to_eol(Src, _, !PS),

    % User time.
    pybench_field("User time (seconds)", Src, User, !PS),

    % System time.
    pybench_field("System time (seconds)", Src, Sys, !PS),

    % CPU utilization.
    pybench_field("Percent of CPU this job got", Src, CPU, !PS),

    % Wall time.
    pybench_field("Elapsed (wall clock) time (h:mm:ss or m:ss)", Src, Wall, 
        !PS),

    % Memory stats unfortunatly don't show up.
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),

    % Page faults, major then minor.
    pybench_field("Major (requiring I/O) page faults", Src, MajorFaults, !PS),
    pybench_field("Minor (reclaiming a frame) page faults", Src, MinorFaults, 
        !PS),

    % Context switches,  Voluntary then involuntary.
    pybench_field("Voluntary context switches", Src, VolunSwitches, !PS),
    pybench_field("Involuntary context switches", Src, InvolunSwitches, !PS),
    
    % Swaps
    skip_to_eol(Src, _, !PS),
        
    % IO Stats.
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),
    skip_to_eol(Src, _, !PS),
    
    % Signal stats.
    skip_to_eol(Src, _, !PS),

    % Page size
    skip_to_eol(Src, _, !PS),

    % Exit status.
    skip_to_eol(Src, _, !PS),

    Row = csv_row([Rep, User, Sys, Wall, CPU, MajorFaults, MinorFaults,
        VolunSwitches, InvolunSwitches]).

:- pred pybench_field(string::in, src::in, csv_data::out, ps::in, ps::out) 
    is semidet.

pybench_field(FieldName, Src, Value, !PS) :-
    field_header(FieldName, Src, _, !PS),
    colon(Src, _, !PS),
    field_value(Src, Value, !PS).

:- pred pybench_exit_status_line(src::in, int::out, ps::in, ps::out) 
    is semidet.

pybench_exit_status_line(Src, Value, !PS) :-
    field_header("Command exited with non-zero status", Src, _, !PS),
    whitespace(Src, _, !PS),
    int_literal(Src, Value, !PS). 

:- pred discard_failed_and_add_test_name(string::in, 
    pair(csv_row, int)::in, csv_row::out) is semidet.

discard_failed_and_add_test_name(TestName, (Row0 - Status), Row) :-
    Status = 0,
    Row0 = csv_row(Fields0),
    Fields = [csv_data_str(TestName) | Fields0],
    Row = csv_row(Fields).

%----------------------------------------------------------------------------%
%
% Speedtest file parsing.
%

:- pred parse_speedtest_file(string::in, list(csv_row)::out, io::di, io::uo) 
    is det.

parse_speedtest_file(InputFile, Rows, !IO) :-
    open_input(InputFile, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        read_file_as_string(Stream, Result, !IO),
        (
            Result = ok(String)
        ;
            Result = error(_, Error),
            error(error_message(Error))
        ),
        close_input(Stream, !IO)
    ;
        OpenResult = error(Error),
        error(error_message(Error))
    ),
    new_src_and_ps(String, Src, PS0),
    ( parse_speedtest_log(Src, RowsPrime, PS0, _PS) ->
        Rows = RowsPrime
    ;
        error("Parser error")
    ).

:- pred parse_speedtest_log(src::in, list(csv_row)::out, ps::in, ps::out) 
    is semidet.

parse_speedtest_log(Src, Rows, !PS) :-
    zero_or_more(parse_speedtest_batch, Src, Batches, !PS),
    condense(Batches, Rows),
    eof(Src, _, !PS).

:- pred parse_speedtest_batch(src::in, list(csv_row)::out, ps::in, ps::out) 
    is semidet.

parse_speedtest_batch(Src, Rows, !PS) :-
    parse_speedtest_field("EXTRA_MCFLAGS", Src, MCFlagsStr, !PS),
    optional(parse_speedtest_field("EXTRA_CFLAGS"), Src, MaybeCFlagsStr, !PS),
    optional(parse_speedtest_field("EXTRA_MLFLAGS"), Src, MaybeMLFlagsStr, !PS),
    parse_speedtest_field("GRADE", Src, GradeStr, !PS),

    string_to_csv_string(MCFlagsStr, MCFlags),
    string_to_csv_string(GradeStr, Grade),
    maybe_string_to_csv_string(MaybeCFlagsStr, CFlags),
    maybe_string_to_csv_string(MaybeMLFlagsStr, MLFlags),

    one_or_more(parse_speedtest_row, Src, Rows0, !PS),

    map((pred(csv_row(Fields0)::in, csv_row(Fields)::out) is det :-
            ( Fields0 = [ Test | Fields1 ] ->
                Fields = [ Test, MCFlags, CFlags, MLFlags, Grade | Fields1 ]
            ;
                error("Expected some content within the speedtest row")
            )
        ), Rows0, Rows). 

:- pred parse_speedtest_row(src::in, csv_row::out, ps::in, ps::out) is semidet.

parse_speedtest_row(Src, Row, !PS) :-
    whitespace(Src, _, !PS),
    get_until(whitespace, Src, TestStr, !PS),
    Test = csv_data_str(strip(TestStr)),
  
    whitespace(Src, _, !PS),
    float_literal(Src, UserFloat, !PS),
    next_char(Src, 'u', !PS),
    User = csv_data_float(UserFloat),

    whitespace(Src, _, !PS),
    float_literal(Src, SysFloat, !PS),
    next_char(Src, 's', !PS),
    Sys = csv_data_float(SysFloat),

    whitespace(Src, _, !PS),
    parse_time(Src, WallFloat, !PS),
    Wall = csv_data_float(WallFloat),

    whitespace(Src, _, !PS),
    float_literal(Src, CPUFloat, !PS),
    punct("%", Src, _, !PS),
    CPU = csv_data_float(CPUFloat / 100.0),

    whitespace(Src, _, !PS),
    % Skip over this field since linux doesn't provide it.
    get_until(whitespace, Src, _, !PS),

    whitespace(Src, _, !PS),
    int_literal(Src, IOInInt, !PS),
    IOIn = csv_data_int(IOInInt),
    punct("+", Src, _, !PS),
    int_literal(Src, IOOutInt, !PS),
    IOOut = csv_data_int(IOOutInt),
    punct("io", Src, _, !PS),

    whitespace(Src, _, !PS),
    int_literal(Src, MinorFaultsInt, !PS),
    MinorFaults = csv_data_int(MinorFaultsInt),
    punct("pf+", Src, _, !PS),
    int_literal(Src, MajorFaultsInt, !PS),
    MajorFaults = csv_data_int(MajorFaultsInt),
    punct("w", Src, _, !PS),

    Row = csv_row(
        [Test, User, Sys, Wall, CPU, MajorFaults, MinorFaults, IOIn, IOOut]). 

:- pred parse_speedtest_field(string::in, src::in, string::out, ps::in, ps::out)
    is semidet. 

parse_speedtest_field(FieldName, Src, Value, !PS) :-
    field_header(FieldName, Src, _, !PS),
    whitespace(Src, _, !PS),
    equals(Src, _, !PS),
    get_until_eol(Src, Value, !PS).

%----------------------------------------------------------------------------%
%
% Parsing utility functions.
%

:- pred get_until_eol(src::in, string::out, ps::in, ps::out) is semidet.

get_until_eol(Src, String, !PS) :-
    current_offset(Src, StartOffset, !PS),
    skip_to_eol(Src, _, !PS),
    current_offset(Src, EndOffset, !PS),
    input_substring(Src, StartOffset, EndOffset, String).

:- pred get_until(pred(char), src, string, ps, ps).
:- mode get_until(pred(in) is semidet, in, out, in, out) is semidet.

get_until(Pred, Src, String, !PS) :-
    current_offset(Src, StartOffset, !PS),
    skip_to(Pred, Src, _, !PS),
    current_offset(Src, EndOffset, !PS),
    input_substring(Src, StartOffset, EndOffset, String).

:- pred skip_to(pred(char), src, unit, ps, ps).
:- mode skip_to(pred(in) is semidet, in, out, in, out) is semidet.

skip_to(Pred, Src, unit, !PS) :-
    (
        next_char(Src, Char, !PS),
        not Pred(Char)
    ->
        skip_to(Pred, Src, unit, !PS)
    ;
        true
    ).

:- pred whitespace(char::in) is semidet.

whitespace(Char) :- 
    char.is_whitespace(Char).

:- pred field_header(string::in, src::in, unit::out, ps::in, ps::out) 
    is semidet.

field_header(String, Src, unit, !PS) :-
    whitespace(Src, _, !PS),
    punct(String, Src, _, !PS).

:- pred colon(src::in, unit::out, ps::in, ps::out) is semidet.

colon(Src, unit, !PS) :-
    punct(":", Src, _, !PS).

:- pred equals(src::in, unit::out, ps::in, ps::out) is semidet.

equals(Src, unit, !PS) :-
    next_char(Src, '=', !PS).

:- pred field_value(src::in, csv_data::out, ps::in, ps::out) is semidet.

field_value(Src, Data, !PS) :-
    whitespace(Src, _, !PS),
    ( parse_time(Src, Seconds, !PS) ->
        Data = csv_data_float(Seconds)
    ; parse_percent(Src, Percent, !PS) ->
        Data = csv_data_float(float(Percent) / 100.0)
    ; float_literal(Src, Float, !PS) ->
        Data = csv_data_float(Float)
    ; int_literal(Src, Int, !PS) ->
        Data = csv_data_int(Int)
    ;
        false 
    ).

:- pred parse_percent(src::in, int::out, ps::in, ps::out) is semidet.

parse_percent(Src, Percent, !PS) :-
    int_literal(Src, Percent, !PS),
    punct("%", Src, _, !PS).

:- pred parse_time(src::in, float::out, ps::in, ps::out) is semidet.

parse_time(Src, Time, !PS) :-
    int_literal(Src, Minutes, !PS),
    punct(":", Src, _, !PS),
    float_literal(Src, Seconds, !PS),
    Time = Seconds + float(60 * Minutes).

:- pred string_to_csv_string(string::in, csv_data::out) is det.

string_to_csv_string(String, csv_data_str(strip(String))).

:- pred maybe_string_to_csv_string(maybe(string)::in, csv_data::out) is det.

maybe_string_to_csv_string(no, csv_data_str("")).
maybe_string_to_csv_string(yes(String), CSVData) :-
    string_to_csv_string(String, CSVData).

