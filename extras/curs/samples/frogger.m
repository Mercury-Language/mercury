%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%-----------------------------------------------------------------------------%
%
% A frogger clone, by Peter Wang.
% This source file is hereby placed in the public domain.
%
% Missing features: colour, fly, turtles don't submerge, timeout.
%
%-----------------------------------------------------------------------------%

:- module frogger.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module curs.
:- use_module sleep.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type world
    --->    world(
                lives       :: int,
                remaining_goals :: int,
                frog        :: frog,
                level       :: level
            ).

:- type frog
    --->    frog(
                frog_x  :: int,
                frog_y  :: int
            ).

:- type level == list(row).

:- type row
    --->    row(
                scroll  :: scroll,
                str     :: string
            ).

:- type scroll
    --->    stationary
    ;       leftwards(int, leftwards_counter::int, bool)
    ;       rightwards(int, rightwards_counter::int, bool).

:- func initial_world = world.

initial_world = world(
    3,  % lives
    5,  % remaining_goals
    initial_frog,
    initial_level
).

:- func width = int.

width = string.length(list.det_head(initial_level) ^ str).

:- func height = int.

height = list.length(initial_level).

:- func initial_frog = frog.

initial_frog = frog(width / 2, height - 1).

:- func initial_level = level.

initial_level = [
    row(stationary,         "............................"),
    row(stationary,         ":gg::::gg::::gg::::gg::::gg:"),
    row(rightwards(5, 0, yes),  "~LLLLLL~~~~LLLLLLL~~~LLLLLL~"),
    row(leftwards(4, 0, yes),   "~~~TtTt~~~~TtTt~~TtTt~~~TtTt"),
    row(rightwards(6, 0, yes),  "L~~~~~LLLLLLLLLL~~~~~~LLLLLL"),
    row(leftwards(7, 0, yes),   "TtTt~~~TtTt~~~TtTt~~~~TtTt~~"),
    row(stationary,         "============================"),
    row(leftwards(5, 0, no),    "         Cccc   Cccc"),
    row(rightwards(1, 0, no),   "           cC       "),
    row(leftwards(7, 0, no),    "   Cc      Cc         Cc"),
    row(rightwards(6, 0, no),   " cC      cC         cC  "),
    row(leftwards(8, 0, no),    "Cc  Cc    Cc        "),
    row(stationary,         "============================")
].

:- pred goal_char(char::in) is semidet.

goal_char('g').

    % Frog cannot touch even a single one of these chars.
    %
:- pred frog_cant_touch_1(char::in) is semidet.

frog_cant_touch_1('<').
frog_cant_touch_1('>').
frog_cant_touch_1('.').
frog_cant_touch_1(':').
frog_cant_touch_1('C').
frog_cant_touch_1('c').

    % Frog can touch at most one of these chars.
    %
:- pred frog_cant_touch_2(char::in) is semidet.

frog_cant_touch_2('~').

:- func game_loop_rate = int.

game_loop_rate = 1000000 / 40.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

main(!IO) :-
    curs.start(!IO),
    curs.nodelay(yes, !IO),
    curs.flushinp(!IO),
    game_loop(initial_world, !IO),
    curs.stop(!IO).

:- pred game_loop(world::in, io::di, io::uo) is det.

game_loop(!.World, !IO) :-
    (if !.World ^ lives < 1 then
        end_game(" G A M E   O V E R ", !IO)
    else if !.World ^ remaining_goals < 1 then
        end_game(" Y O U   W O N ! ", !IO)
    else
        handle_input(!World, !IO, Quit),
        (
            Quit = no,
            handle_logic(!World),
            draw_world(!.World, !IO),
            sleep.usleep(game_loop_rate, !IO),
            move_world(!World),
            game_loop(!.World, !IO)
        ;
            Quit = yes
        )
    ).

%-----------------------------------------------------------------------------%

:- pred draw_world(world::in, io::di, io::uo) is det.

draw_world(World, !IO) :-
    curs.clear(!IO),
    draw_level(World ^ level, !IO),
    draw_frog(World ^ frog, !IO),
    draw_status(World ^ lives, !IO).

:- pred draw_level(level::in, io::di, io::uo) is det.

draw_level(Level, !IO) :-
    draw_level_2(0, Level, !IO).

:- pred draw_level_2(int::in, level::in, io::di, io::uo) is det.

draw_level_2(_RowNumber, [], !_IO).
draw_level_2(RowNumber, [Row | Rows], !IO) :-
    curs.move(RowNumber, 0, !IO),
    draw_row(0, Row ^ str, !IO),
    draw_level_2(RowNumber + 1, Rows, !IO).

:- pred draw_row(int::in, string::in, io::di, io::uo) is det.

draw_row(N, Str, !IO) :-
    ( if string.index(Str, N, C) then
        curs.addch(curs.normal, char.to_int(visualise(C)), !IO),
        draw_row(N + 1, Str, !IO)
    else
        true
    ).

:- pred draw_frog(frog::in, io::di, io::uo) is det.

draw_frog(frog(X, Y), !IO) :-
    curs.move(Y, X, !IO),
    curs.addstr(curs.standout, "<>", !IO).

:- pred draw_status(int::in, io::di, io::uo) is det.

draw_status(Lives, !IO) :-
    curs.move(height, 0, !IO),
    curs.addstr(curs.normal, String, !IO),
    String = string.format(" Lives: %d ", [i(Lives)]).

    % On screen the 'g' goal tiles are drawn as blanks.
    %
:- func visualise(char) = char.

visualise(Char) = (if Char = 'g' then ' ' else Char).

%-----------------------------------------------------------------------------%

:- pred end_game(string::in, io::di, io::uo) is det.

end_game(Message, !IO) :-
    curs.rows_cols(Rows, Cols, !IO),
    curs.move(Rows / 2, (Cols / 2) - string.length(Message) / 2, !IO),
    curs.addstr(curs.normal, Message, !IO),
    curs.refresh(!IO),
    sleep.usleep(1000000, !IO).

%-----------------------------------------------------------------------------%

:- pred handle_input(world::in, world::out, io::di, io::uo, bool::out) is det.

handle_input(!World, !IO, Quit) :-
    curs.getch(K, !IO),
    ( if is_quit(K) then
        Quit = yes
    else
        Quit = no,
        ( if K = curs.key_left then
            move_frog_left(!World)
        else if K = curs.key_right then
            move_frog_right(!World)
        else if K = curs.key_up then
            move_frog_up(!World)
        else if K = curs.key_down then
            move_frog_down(!World)
        else
            true
        )
    ).

:- pred is_quit(int::in) is semidet.

is_quit(char.to_int('q')).
is_quit(27).  % escape

:- pred move_frog_left(world::in, world::out) is det.

move_frog_left(World0, World) :-
    World0 ^ frog = frog(X, Y),
    World = World0 ^ frog := frog(max(0, X - 1), Y).

:- pred move_frog_right(world::in, world::out) is det.

move_frog_right(World0, World) :-
    World0 ^ frog = frog(X, Y),
    World = World0 ^ frog := frog(min(width - 2, X + 1), Y).

:- pred move_frog_up(world::in, world::out) is det.

move_frog_up(World0, World) :-
    World0 ^ frog = frog(X, Y),
    World = World0 ^ frog := frog(X, max(0, Y - 1)).

:- pred move_frog_down(world::in, world::out) is det.

move_frog_down(World0, World) :-
    World0 ^ frog = frog(X, Y),
    World = World0 ^ frog := frog(X, min(height - 1, Y + 1)).

%-----------------------------------------------------------------------------%

:- pred move_world(world::in, world::out) is det.

move_world(World0, World) :-
    move_world_2(0, World0 ^ level, Level, World0 ^ frog, Frog),
    World = ((World0 ^ level := Level)
             ^ frog := Frog).

:- pred move_world_2(int::in, level::in, level::out, frog::in, frog::out)
    is det.

move_world_2(_, [], [], Frog, Frog).
move_world_2(RowNumber, [Row0 | Rows0], [Row | Rows], Frog0, Frog) :-
    move_row(RowNumber, Row0, Row, Frog0, Frog1),
    move_world_2(RowNumber+1, Rows0, Rows, Frog1, Frog).

:- pred move_row(int::in, row::in, row::out, frog::in, frog::out) is det.

move_row(_RowNumber, Row @ row(stationary, _String), Row, Frog, Frog).
move_row(RowNumber, row(leftwards(Speed, Counter, DragFrog), String), Row,
        Frog0 @ frog(FrogX, FrogY), Frog) :-
    ( if Counter = Speed then
        string.split(String, 1, Prefix, Suffix),
        Row = row(leftwards(Speed, 0, DragFrog), Suffix ++ Prefix),
        (if DragFrog = yes,
            RowNumber = FrogY
        then
            Frog = frog(max(0, FrogX-1), FrogY)
        else
            Frog = Frog0
        )
    else
        Row = row(leftwards(Speed, Counter+1, DragFrog), String),
        Frog = Frog0
    ).

move_row(RowNumber, row(rightwards(Speed, Counter, DragFrog), String), Row,
        Frog0 @ frog(FrogX, FrogY), Frog) :-
    ( if Counter = Speed then
        string.split(String, width - 1, Prefix, Suffix),
        Row = row(rightwards(Speed, 0, DragFrog), Suffix ++ Prefix),
        ( if DragFrog = yes,
            RowNumber = FrogY
        then
            Frog = frog(min(width-2, FrogX+1), FrogY)
        else
            Frog = Frog0
        )
    else
        Row = row(rightwards(Speed, Counter+1, DragFrog), String),
        Frog = Frog0
    ).

%-----------------------------------------------------------------------------%

:- pred handle_logic(world::in, world::out) is det.

handle_logic(!World) :-
    ( if check_frog_in_goal(!World) then
        true
    else if check_frog_went_splat(!World) then
        true
    else
        true
    ).

:- pred check_frog_in_goal(world::in, world::out) is semidet.

check_frog_in_goal(World0, World) :-
    chars_at_frog(World0, C1, C2),
    goal_char(C1),
    goal_char(C2),
    stamp_frog_in_goal(World0, World1),
    World = ((World1 ^ remaining_goals := World0 ^ remaining_goals - 1)
             ^ frog := initial_frog).

:- pred check_frog_went_splat(world::in, world::out) is semidet.

check_frog_went_splat(World0, World) :-
    chars_at_frog(World0, C1, C2),
    ( frog_cant_touch_1(C1)
    ; frog_cant_touch_1(C2)
    ; frog_cant_touch_2(C1), frog_cant_touch_2(C2)
    ),
    World = ((World0 ^ lives := World0 ^ lives - 1)
             ^ frog := initial_frog).

:- pred chars_at_frog(world::in, char::out, char::out) is det.

chars_at_frog(World, C1, C2) :-
    frog(X, Y) = World ^ frog,
    Row = list.det_index0(World ^ level, Y),
    C1 = string.det_index(Row ^ str, X),
    C2 = string.det_index(Row ^ str, X + 1).

:- pred stamp_frog_in_goal(world::in, world::out) is det.

stamp_frog_in_goal(World0, World) :-
    frog(X, Y) = World0 ^ frog,
    Level = World0 ^ level,
    Row = list.det_index0(Level, Y),
    NewStr = string.det_set_char('<', X,
        string.det_set_char('>', X + 1, Row ^ str)),
    NewRow = Row ^ str := NewStr,
    NewLevel = list.det_replace_nth(Level, Y + 1, NewRow),
    World = World0 ^ level := NewLevel.

%-----------------------------------------------------------------------------%
:- end_module frogger.
%-----------------------------------------------------------------------------%
