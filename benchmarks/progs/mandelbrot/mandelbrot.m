:- module mandelbrot.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module thread.
:- import_module thread.future.
:- import_module thread.mvar.

main(!IO) :-
    command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(short_options, long_options, default_options),
    getopt.process_options(OptionOps, Args, NonOptionsArgs, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        (
            NonOptionsArgs = [],
            getopt.lookup_bool_option(OptionTable, help, Help),
            (
                Help = yes,
                usage(!IO),
                Result = ok
            ;
                Help = no,
                process_options(OptionTable, MaybeOptions),
                (
                    MaybeOptions = ok(Options),
                    real_main(Options, !IO),
                    Result = ok
                ;
                    MaybeOptions = error(Error),
                    Result = error(format("Error processing options: %s\n",
                        [s(Error)]))
                )
            )
        ;
            NonOptionsArgs = [FirstArg | _],
            Result = error(format("Error processing argument: %s\n",
                [s(FirstArg)]))
        )
    ;
        GetoptResult = error(Error),
        Result = error(format("Error processing options: %s\n",
            [s(option_error_to_string(Error))]))
    ),

    (
        Result = ok
    ;
        Result = error(ErrorMessage),
        write_string(stderr_stream, ErrorMessage, !IO),
        usage(!IO),
        io.set_exit_status(1, !IO)
    ).

:- type option
    --->    help
    ;       dim_x
    ;       dim_y
    ;       dependent_conjunctions
    ;       parallel.

:- pred short_options(char::in, option::out) is semidet.

short_options('h', help).
short_options('?', help).
short_options('x', dim_x).
short_options('y', dim_y).
short_options('d', dependent_conjunctions).
short_options('p', parallel).

:- pred long_options(string::in, option::out) is semidet.

long_options("help", help).
long_options("dependent_conjunctions", dependent_conjunctions).
long_options("parallel", parallel).

:- pred default_options(option::out, option_data::out) is multi.

default_options(help,                   bool(no)).
default_options(dim_x,                  maybe_int(no)).
default_options(dim_y,                  maybe_int(no)).
default_options(dependent_conjunctions, bool(no)).
default_options(parallel,               string("no")).

:- type options
    --->    options(
                opts_dim_x              :: int,
                opts_dim_y              :: int,
                opts_use_dep_conjs      :: use_dependent_conjunctions,
                opts_parallel           :: parallel
            ).

:- type use_dependent_conjunctions
    --->    use_dependent_conjunctions
    ;       use_independent_conjunctions.

:- type parallel
    --->    parallel_conj
    ;       parallel_spawn
    ;       parallel_spawn_native
    ;       parallel_future
    ;       sequential.

:- pred process_options(option_table(option)::in, maybe_error(options)::out)
    is det.

process_options(Table, MaybeOptions) :-
    getopt.lookup_bool_option(Table, dependent_conjunctions, DepConjsBool),
    (
        DepConjsBool = yes,
        DepConjs = use_dependent_conjunctions
    ;
        DepConjsBool = no,
        DepConjs = use_independent_conjunctions
    ),
    getopt.lookup_string_option(Table, parallel, ParallelStr),
    ( if
        (
            ParallelStr = "no",
            Parallel0 = sequential
        ;
            ParallelStr = "conj",
            Parallel0 = parallel_conj
        ;
            ParallelStr = "spawn",
            Parallel0 = parallel_spawn
        ;
            ParallelStr = "spawn_native",
            Parallel0 = parallel_spawn_native
        ;
            ParallelStr = "future",
            Parallel0 = parallel_future
        )
    then
        MaybeParallel = ok(Parallel0)
    else
        MaybeParallel = error(
            "Parallel must be one of ""no"", ""conj"", ""spawn"", " ++
            """spawn_native"" or ""future""")
    ),

    getopt.lookup_maybe_int_option(Table, dim_x, MaybeX),
    getopt.lookup_maybe_int_option(Table, dim_y, MaybeY),
    (
        (
            MaybeX = yes(DimX0),
            MaybeY = yes(DimY0)
        ;
            MaybeX = no,
            MaybeY = no,
            dimension(DimX0, DimY0)
        ),
        MaybeDim = ok({DimX0, DimY0})
    ;
        (
            MaybeX = yes(_),
            MaybeY = no
        ;
            MaybeX = no,
            MaybeY = yes(_)
        ),
        MaybeDim = error("Specify both of -x and -y or neither of them")
    ),
    (
        MaybeDim = ok({DimX, DimY}),
        MaybeParallel = ok(Parallel),
        MaybeOptions = ok(options(DimX, DimY, DepConjs, Parallel))
    ;
        MaybeDim = ok(_),
        MaybeParallel = error(Error),
        MaybeOptions = error(Error)
    ;
        MaybeDim = error(Error),
        MaybeOptions = error(Error)
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname("mandelbrot", ProgName, !IO),
    format("Usage: %s <opts>\n", [s(ProgName)], !IO),
    write_string("<opts> may be one or more of:\n", !IO),
    write_string("\t-x X -y Y\n", !IO),
    write_string(
        "\t\tThe dimensions of the image, specify neither or both\n", !IO),
    write_string("\t-p <how> --parallel <how>\n", !IO),
    write_string(
        "\t\t<how> is one of ""no"", ""conj"", ""spawn"",\n", !IO),
    write_string(
        "\t\t""spawn_native"" or ""future"". These may be grade", !IO),
    write_string(
        "\t\tdependent.\n", !IO),
    write_string("\t-d --dependent-conjunctions\n", !IO),
    write_string(
        "\t\tUse an accumulator to represent the rows rendered so far\n", !IO).

:- pred real_main(options::in, io::di, io::uo) is det.

real_main(Options, !IO) :-
    Options = options(DimX, DimY, _, _),
    viewport(StartX, StartY, Length, Height),
    StepX = Length / float(DimX),
    StepY = Height / float(DimY),
    draw_image(Options, StartX, StartY, StepX, StepY, DimX, DimY, Image),
    write_ppm(Image, !IO).

:- pred draw_image(options::in, float::in, float::in, float::in, float::in,
    int::in, int::in, image::out) is det.

draw_image(Options, StartX, StartY, StepX, StepY, DimX, DimY,
        image(DimX, DimY, Rows)) :-
    draw_rows(Options, StartY, StepY, DimY, StartX, StepX, DimX, Rows).

:- pred draw_rows(options::in, float::in, float::in, int::in, float::in,
    float::in, int::in, cord(colour)::out) is det.

draw_rows(Options, StartY, StepY, DimY, StartX, StepX, DimX, Rows) :-
    pos_list(StartY, StepY, DimY, Ys),
    pos_list(StartX, StepX, DimX, Xs),
    DepConjs = Options ^ opts_use_dep_conjs,
    Parallel = Options ^ opts_parallel,
    (
        DepConjs = use_dependent_conjunctions,
        draw_rows_dep(Parallel, Xs, Ys, Rows)
    ;
        DepConjs = use_independent_conjunctions,
        draw_rows_indep(Parallel, Xs, Ys, Rows)
    ).

:- pred draw_rows_dep(parallel::in, list(float)::in, list(float)::in,
    cord(colour)::out) is det.

draw_rows_dep(Parallel, Xs, Ys, Rows) :-
    (
        Parallel = sequential,
        map_foldl(draw_row(Xs), append_row, Ys, empty, Rows)
    ;
        Parallel = parallel_conj,
        map_foldl_par_conj(draw_row(Xs), append_row, Ys, empty, Rows)
    ;
        ( Parallel = parallel_spawn
        ; Parallel = parallel_spawn_native
        ; Parallel = parallel_future
        ),
        sorry($file, $pred, string(Parallel))
    ).

:- pred draw_rows_indep(parallel::in, list(float)::in, list(float)::in,
    cord(colour)::out) is det.

draw_rows_indep(Parallel, Xs, Ys, Rows) :-
    (
        Parallel = sequential,
        my_map(draw_row(Xs), Ys, RowList)
    ;
        Parallel = parallel_conj,
        my_map_par_conj(draw_row(Xs), Ys, RowList)
    ;
        Parallel = parallel_spawn,
        promise_equivalent_solutions [RowList] (
            my_map_par_spawn(draw_row(Xs), Ys, RowList)
        )
    ;
        Parallel = parallel_spawn_native,
        promise_equivalent_solutions [RowList] (
            my_map_par_spawn_native(draw_row(Xs), Ys, RowList)
        )
    ;
        Parallel = parallel_future,
        my_map_par_future(draw_row(Xs), Ys, RowList)
    ),
    foldl(append_row, RowList, empty, Rows).

:- pred append_row(cord(X)::in, cord(X)::in, cord(X)::out) is det.

append_row(Row, !Rows) :-
    !:Rows = !.Rows ++ Row.

:- pred draw_row(list(float)::in, float::in, cord(colour)::out) is det.

draw_row(Xs, Y, Row) :-
    draw_row_2(Xs, Y, empty, Row).

:- pred draw_row_2(list(float)::in, float::in,
    cord(colour)::in, cord(colour)::out) is det.

draw_row_2([], _, !Row).
draw_row_2([X | Xs], Y, !Row) :-
    calc_pixel(X, Y, Colour),
    !:Row = snoc(!.Row, Colour),
    draw_row_2(Xs, Y, !Row).

:- pred calc_pixel(float::in, float::in, colour::out) is det.

calc_pixel(X, Y, Colour) :-
    max_iters(MaxIters),
    escape(0.0, 0.0, X, Y, MaxIters, 0, Iters),
    ( Iters > 0 ->
        colour_gradient(blue, yellow,
            sqrt(float(Iters)) / sqrt(float(MaxIters)),
            Colour)
    ;
        Colour = black
    ).

:- pred colour_gradient(colour::in, colour::in, float::in, colour::out) is det.

colour_gradient(Start, End, R, colour(Red, Green, Blue)) :-
    component_gradient(Start ^ red, End ^ red, R, Red),
    component_gradient(Start ^ green, End ^ green, R, Green),
    component_gradient(Start ^ blue, End ^ blue, R, Blue).

:- pred component_gradient(int::in, int::in, float::in, int::out) is det.

component_gradient(Start, End, V, Result) :-
    StartF = float(Start),
    EndF = float(End),
    Result = round_to_int(StartF + V * (EndF - StartF)).

:- pred escape(float::in, float::in, float::in, float::in, int::in,
    int::in, int::out) is det.

escape(Nr, Ni, Cr, Ci, MaxIters, !Iters) :-
    ( if MaxIters > 0 then
        N2r = Nr * Nr - Ni * Ni,
        N2i = 2.0 * Nr * Ni,
        Rr = N2r + Cr,
        Ri = N2i + Ci,
        ( if sqrt(Rr * Rr + Ri * Ri) > 2.0 then
            true
        else
            !:Iters = !.Iters + 1,
            escape(Rr, Ri, Cr, Ci, MaxIters - 1, !Iters)
        )
    else
        !:Iters = -1
    ).

%----------------------------------------------------------------------------%

:- pred pos_list(float::in, float::in, int::in, list(float)::out) is det.

pos_list(Cur, Step, Num, List) :-
    ( Num > 0 ->
        pos_list(Cur + Step, Step, Num - 1, Tail),
        List = [Cur | Tail]
    ;
        List = []
    ).

:- pred map_foldl(pred(X, Y), pred(Y, A, A), list(X), A, A).
:- mode map_foldl(pred(in, out) is det, pred(in, in, out) is det,
    in, in, out) is det.

map_foldl(_, _, [], !Acc).
map_foldl(M, F, [X | Xs], !Acc) :-
    M(X, Y),
    F(Y, !Acc),
    map_foldl(M, F, Xs, !Acc).

:- pred map_foldl_par_conj(pred(X, Y), pred(Y, A, A), list(X), A, A).
:- mode map_foldl_par_conj(pred(in, out) is det, pred(in, in, out) is det,
    in, in, out) is det.

map_foldl_par_conj(_, _, [], !Acc).
map_foldl_par_conj(M, F, [X | Xs], !Acc) :-
    (
        M(X, Y),
        F(Y, !Acc)
    &
        map_foldl_par_conj(M, F, Xs, !Acc)
    ).

:- pred my_map(pred(X, Y), list(X), list(Y)).
:- mode my_map(pred(in, out) is det, in, out) is det.

my_map(_, [], []).
my_map(M, [X | Xs], [Y | Ys]) :-
    M(X, Y),
    my_map(M, Xs, Ys).

:- pred my_map_par_conj(pred(X, Y), list(X), list(Y)).
:- mode my_map_par_conj(pred(in, out) is det, in, out) is det.

my_map_par_conj(_, [], []).
my_map_par_conj(M, [X | Xs], [Y | Ys]) :-
    M(X, Y) &
    my_map_par_conj(M, Xs, Ys).

:- pred my_map_par_future(pred(X, Y), list(X), list(Y)).
:- mode my_map_par_future(pred(in, out) is det, in, out) is det.

my_map_par_future(_, [], []).
my_map_par_future(M, [X | Xs], Ys) :-
    FutY = future((func) = Y0 :- M(X, Y0)),
    my_map_par_future(M, Xs, Ys0),
    Y = wait(FutY),
    Ys = [Y | Ys0].

:- pred my_map_par_spawn(pred(X, Y), list(X), list(Y)).
:- mode my_map_par_spawn(pred(in, out) is det, in, out) is cc_multi.

my_map_par_spawn(_, [], []).
my_map_par_spawn(M, [X | Xs], Ys) :-
    promise_pure (
        some [!IO] (
            impure make_io(!:IO),
            mvar.init(YMVar, !IO),
            spawn(
                ( pred(IO0::di, IO::uo) is cc_multi :-
                    M(X, Y0),
                    mvar.put(YMVar, Y0, IO0, IO)
                ), !IO),
            my_map_par_spawn(M, Xs, Ys0),
            mvar.take(YMVar, Y, !IO),
            Ys = [Y | Ys0],
            _ = !.IO
        )
    ).

:- pred my_map_par_spawn_native(pred(X, Y), list(X), list(Y)).
:- mode my_map_par_spawn_native(pred(in, out) is det, in, out) is cc_multi.

my_map_par_spawn_native(_, [], []).
my_map_par_spawn_native(M, [X | Xs], Ys) :-
    promise_pure (
        some [!IO] (
            impure make_io(!:IO),
            mvar.init(YMVar, !IO),
            spawn_native(
                ( pred(_::in, IO0::di, IO::uo) is cc_multi :-
                    M(X, Y0),
                    mvar.put(YMVar, Y0, IO0, IO)
                ), _, !IO),
            my_map_par_spawn_native(M, Xs, Ys0),
            mvar.take(YMVar, Y, !IO),
            Ys = [Y | Ys0],
            _ = !.IO
        )
    ).

:- impure pred make_io(io::uo) is det.

:- pragma foreign_proc("C",
    make_io(IO::uo),
    [will_not_call_mercury, thread_safe],
    "IO = 0;").

:- pragma foreign_proc("Java",
    make_io(IO::uo),
    [will_not_call_mercury, thread_safe],
    "IO = 0;").

%----------------------------------------------------------------------------%

:- type image
    --->    image(
                width           :: int,
                heignt          :: int,
                pixels          :: cord(colour)
            ).

:- type colour
    --->    colour(
                red             :: int,
                green           :: int,
                blue            :: int
            ).

:- pred write_ppm(image::in, io::di, io::uo) is det.

write_ppm(image(Width, Height, Rows), !IO) :-
    io.open_binary_output(filename, Result, !IO),
    (
        Result = ok(Stream),
        format("P6 %d %d 255\n", [i(Width), i(Height)], Header),
        foldl(
            ( pred(C::in, IO0::di, IO::uo) is det :-
                write_byte(Stream, char.to_int(C), IO0, IO)
            ), Header, !IO),
        foldl_pred(write_colour(Stream), Rows, !IO),
        io.close_binary_output(Stream, !IO)
    ;
        Result = error(Error),
        error(format("%s: %s", [s(filename), s(error_message(Error))]))
    ).

:- pred write_colour(binary_output_stream::in, colour::in, io::di, io::uo)
    is det.

write_colour(Stream, colour(R, G, B), !IO) :-
    write_byte(Stream, R, !IO),
    write_byte(Stream, G, !IO),
    write_byte(Stream, B, !IO).

%----------------------------------------------------------------------------%

:- pred dimension(int::out, int::out) is det.

dimension(1024, 1024).

    % viewport(X, Y, Length, Height),
    %
:- pred viewport(float::out, float::out, float::out, float::out) is det.

viewport(-0.75, -0.75, 0.5, 0.5).

:- pred max_iters(int::out) is det.

max_iters(5000).

:- func filename = string.

filename = "mandelbrot.ppm".

:- func blue = colour.

blue = colour(0, 0, 255).

:- func yellow = colour.

yellow = colour(255, 255, 0).

:- func black = colour.

black = colour(0, 0, 0).
