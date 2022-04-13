%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%-----------------------------------------------------------------------------%
%
% A nibbles clone by Peter Wang.
% This source file is hereby placed in the public domain.
%
%-----------------------------------------------------------------------------%

:- module nibbles.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module curs.
:- use_module sleep.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module random.sfc32.
:- import_module random.system_rng.
:- import_module require.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type world
    --->    world(
            cols        :: int,
            rows        :: int,
            snake       :: snake,
            next_apple_num :: int,
            apple       :: apple,
            score       :: int
        ).

:- type snake
    --->    snake(
            direction   :: direction,
            head        :: segment,
            tail        :: list(segment),
            growth      :: int
        ).

:- type direction
    --->    up
    ;       down
    ;       left
    ;       right.

:- type segment == {int, int}.

:- type apple
    --->    no_apple
    ;       apple(
                x    :: int,
                y    :: int,
                repr :: int
            ).

%-----------------------------------------------------------------------------%

main(!IO) :-
    open_system_rng(MaybeSystemRNG, !IO),
    (
        MaybeSystemRNG = ok(SystemRNG),
        system_rng.generate_uint32(SystemRNG, SeedA, !IO),
        system_rng.generate_uint32(SystemRNG, SeedB, !IO),
        system_rng.generate_uint32(SystemRNG, SeedC, !IO),
        close_system_rng(SystemRNG, !IO),

        sfc32.seed(SeedA, SeedB, SeedC, RNG0, RS),
        make_io_urandom(RNG0, RS, RNG, !IO),

        curs.start(!IO),
        curs.nodelay(yes, !IO),
        curs.rows_cols(Rows, Cols, !IO),
        curs.flushinp(!IO),
        play_game(RNG, Cols, Rows, !IO),
        curs.stop(!IO)
    ;
        MaybeSystemRNG = error(ErrMsg),
        io.stderr_stream(Stderr, !IO),
        io.format(Stderr, "Error %s.\n", [s(ErrMsg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred play_game(io_urandom(P, S)::in, int::in, int::in, io::di, io::uo) is det
    <= urandom(P, S).

play_game(RNG, Cols, Rows, !IO) :-
    Snake = snake(right, {Cols / 2, Rows / 2}, [], 10),
    World = world(Cols, Rows, Snake, 1, no_apple, 0),
    game_loop(RNG, World, !IO).

:- pred game_loop(io_urandom(P, S)::in, world::in, io::di, io::uo) is det
    <= urandom(P, S).

game_loop(RNG, !.World, !IO) :-
    handle_input(!World, !IO, Quit),
    (
        Quit = no,
        move_snake(!World),
        maybe_eat_apple(!World),
        draw_world(!.World, !IO),
        ( if snake_is_dead(!.World) then
            show_game_over(!IO)
        else
            sleep.usleep(50000, !IO),
            maybe_replenish_apple(RNG, !World, !IO),
            game_loop(RNG, !.World, !IO)
        )
    ;
        Quit = yes
    ).

%-----------------------------------------------------------------------------%

:- pred handle_input(world::in, world::out, io::di, io::uo, bool::out) is det.

handle_input(!World, !IO, Quit) :-
    curs.getch(Key, !IO),
    ( if quit_key(Key) then
        Quit = yes
    else
        Quit = no,
        ( if direction_key(Key, Dir) then
            change_snake_direction(Dir, !World)
        else
            true
        )
    ).

:- pred quit_key(int::in) is semidet.

quit_key(char.to_int('q')).
quit_key(27).   % escape

:- pred direction_key(int::in, direction::out) is semidet.

direction_key(Key, Direction) :-
    promise_equivalent_solutions [Direction] (
        direction_key_2(Key, Direction)
    ).

:- pred direction_key_2(int::in, direction::out) is cc_nondet.

direction_key_2(curs.key_up, up).
direction_key_2(curs.key_down, down).
direction_key_2(curs.key_left, left).
direction_key_2(curs.key_right, right).

:- pred change_snake_direction(direction::in, world::in, world::out) is det.

change_snake_direction(NewDir, World0, World) :-
    ( if valid_direction_change(World0 ^ snake ^ direction, NewDir) then
        World = World0 ^ snake ^ direction := NewDir
    else
        World = World0
    ).

:- pred valid_direction_change(direction::in, direction::in) is semidet.

valid_direction_change(up, left).
valid_direction_change(up, right).
valid_direction_change(down, left).
valid_direction_change(down, right).
valid_direction_change(left, up).
valid_direction_change(left, down).
valid_direction_change(right, up).
valid_direction_change(right, down).

%-----------------------------------------------------------------------------%

:- pred move_snake(world::in, world::out) is det.

move_snake(World0, World) :-
    World0 ^ snake = snake(Dir, Head @ {HeadX, HeadY}, Tail, Growth),
    ( Dir = up, NewHead = {HeadX, HeadY-1}
    ; Dir = down,   NewHead = {HeadX, HeadY+1}
    ; Dir = left,   NewHead = {HeadX-1, HeadY}
    ; Dir = right,  NewHead = {HeadX+1, HeadY}
    ),
    Result = ordering(Growth, 0),
    (
        Result = (>),
        World = World0 ^ snake :=
            snake(Dir, NewHead, [Head | Tail], Growth-1)
    ;
        Result = (=),
        NewTail = list.take_upto(length(Tail)-1, Tail),
        World = World0 ^ snake :=
            snake(Dir, NewHead, [Head | NewTail], Growth)
    ;
        Result = (<),
        error("move_snake/2: Growth should be >= 0")
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_eat_apple(world::in, world::out) is det.

maybe_eat_apple(World0, World) :-
    (
        World0 ^ apple = no_apple,
        World = World0
    ;
        World0 ^ apple = apple(X, Y, _),
        (if World0 ^ snake ^ head = {X, Y} then
            World = (((World0
                 ^ apple := no_apple)
                 ^ snake ^ growth := inc_growth(World0))
                 ^ score := World0 ^ score + 10)
        else
            World = World0
        )
    ).

:- func inc_growth(world) = int.

inc_growth(World) = NewGrowth :-
    Area = (World ^ cols-2) * (World ^ rows-2),
    Limit = Area/4,
    Snake = World ^ snake,
    CurrLength = length(Snake ^ tail) + Snake ^ growth,
    NewLength = CurrLength + 5,
    NewGrowth = (if NewLength > Limit
            then max(0, Limit - CurrLength)
            else NewLength - CurrLength).

%-----------------------------------------------------------------------------%

:- pred snake_is_dead(world::in) is semidet.

snake_is_dead(World) :-
    Head @ {HeadX, HeadY} = World ^ snake ^ head,
    ( HeadX = 0
    ; HeadY = 0
    ; HeadX = World ^ cols-1
    ; HeadY = World ^ rows-1
    ; Head `member` World ^ snake ^ tail
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_replenish_apple(io_urandom(P, S)::in, world::in, world::out,
    io::di, io::uo) is det <= urandom(P, S).

maybe_replenish_apple(RNG, World0, World, !IO) :-
    ( if World0 ^ apple = no_apple then
        new_apple(RNG, World0, NewApple, !IO),
        NextAppleNum = inc_apple_num(World0 ^ next_apple_num),
        World = ((World0
            ^ apple := NewApple)
            ^ next_apple_num := NextAppleNum)
    else
        World = World0
    ).

:- pred new_apple(io_urandom(P, S)::in, world::in, apple::out, io::di, io::uo)
    is det <= urandom(P, S).

new_apple(RNG, World, Apple, !IO) :-
    uniform_int_in_range(RNG, 1, World ^ cols - 2, X, !IO),
    uniform_int_in_range(RNG, 1, World ^ rows - 2, Y, !IO),
    ( if touches_snake(X, Y, World) then
        new_apple(RNG, World, Apple, !IO)
    else
        Apple = apple(X, Y, apple_char(World ^ next_apple_num))
    ).

:- pred touches_snake(int::in, int::in, world::in) is semidet.

touches_snake(X, Y, World) :- {X, Y} = World ^ snake ^ head.
touches_snake(X, Y, World) :- {X, Y} `member` World ^ snake ^ tail.

:- func inc_apple_num(int) = int.

inc_apple_num(N) = (if N < 9 then N + 1 else 1).

:- func apple_char(int) = int.

apple_char(N) = char.to_int('0') + N.

%-----------------------------------------------------------------------------%

:- func wall_char = int.

wall_char = char.to_int('+').

:- func head_char = int.

head_char = char.to_int('0').

:- func tail_char = int.

tail_char = char.to_int('O').

:- pred draw_world(world::in, io::di, io::uo) is det.

draw_world(World, !IO) :-
    curs.clear(!IO),
    draw_walls(World, !IO),
    draw_score(World ^ score, !IO),
    draw_snake(World ^ snake, !IO),
    draw_apple(World ^ apple, !IO).

:- pred draw_walls(world::in, io::di, io::uo) is det.

draw_walls(World, !IO) :-
    Cols = World ^ cols,
    Rows = World ^ rows,
    rect(0, 0, Cols - 1, Rows - 1, '+', !IO).

:- pred draw_score(int::in, io::di, io::uo) is det.

draw_score(Score, !IO) :-
    curs.move(0, 5, !IO),
    curs.addstr(curs.normal, String, !IO),
    String = string.format(" Score: %d ", [i(Score)]).

:- pred draw_snake(snake::in, io::di, io::uo) is det.

draw_snake(Snake, !IO) :-
    list.foldl(draw_snake_segment, Snake ^ tail, !IO),
    Snake ^ head = {HeadX, HeadY},
    curs.move(HeadY, HeadX, !IO),
    curs.addch(curs.bold, head_char, !IO).

:- pred draw_snake_segment(segment::in, io::di, io::uo) is det.

draw_snake_segment({X,Y}, !IO) :-
    curs.move(Y, X, !IO),
    curs.addch(curs.normal, tail_char, !IO).

:- pred draw_apple(apple::in, io::di, io::uo) is det.

draw_apple(no_apple, !IO).
draw_apple(apple(X, Y, Char), !IO) :-
    curs.move(Y, X, !IO),
    curs.addch(curs.standout, Char, !IO).

:- pred rect(int::in, int::in, int::in, int::in, char::in, io::di, io::uo)
    is det.

rect(X1,Y1, X2,Y2, Char, !IO) :-
    char.to_int(Char, C),
    curs.move(Y1, X1, !IO), curs.hline(C, X2-X1, !IO),
    curs.move(Y2, X1, !IO), curs.hline(C, X2-X1, !IO),
    curs.move(Y1, X1, !IO), curs.vline(C, Y2-Y1, !IO),
    curs.move(Y1, X2, !IO), curs.vline(C, Y2-Y1, !IO).

%-----------------------------------------------------------------------------%

:- pred show_game_over(io::di, io::uo) is det.

show_game_over(!IO) :-
    Message = " You died, press a key... ",
    curs.rows_cols(Rows, Cols, !IO),
    curs.move(Rows/2, (Cols/2) - string.length(Message) / 2, !IO),
    curs.addstr(curs.normal, Message, !IO),
    curs.refresh(!IO),
    sleep.usleep(500000, !IO),
    curs.nodelay(no, !IO),
    curs.flushinp(!IO),
    curs.getch(_, !IO),
    curs.nodelay(yes, !IO).

%-----------------------------------------------------------------------------%
:- end_module nibbles.
%-----------------------------------------------------------------------------%
