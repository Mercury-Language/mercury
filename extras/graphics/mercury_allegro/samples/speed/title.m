%-----------------------------------------------------------------------------%

:- module title.
:- interface.

:- import_module io.

:- pred title_screen(what_to_do::out, io::di, io::uo) is det.

:- type what_to_do
    --->    play_game
    ;       quit.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.blit.
:- import_module allegro.color.
:- import_module allegro.keyboard.
:- import_module allegro.prim.
:- import_module allegro.text.
:- import_module allegro.timer.

:- import_module bool.
:- import_module int.

%-----------------------------------------------------------------------------%

title_screen(WhatToDo, !IO) :-
    det_screen(_, ScreenW, ScreenH, !IO),
    det_create_bitmap(ScreenW, ScreenH, Bmp, !IO),

    bitmap_color_depth(Bmp, BPP, !IO),
    (if BPP > 8 then
%         int.fold_up(draw_gradient(Bmp), 0 ScreenH/2, !IO)
        true
    else
        makecol(0, 0, 128, Bg, !IO),
        clear_to_color(Bmp, Bg, !IO)
    ),

    det_create_bitmap(40, 8, B, !IO),
    bitmap_mask_color(B, MaskColor, !IO),
    clear_to_color(B, MaskColor, !IO),

    makecol(0, 0, 0, Black, !IO),
    makecol(0, 0, 64, Blue, !IO),
    makecol(255, 255, 255, White, !IO),
    font(Font, !IO),

    textout_ex(B, Font, "SPEED", 0, 0, Black, -1, !IO),
    stretch_sprite(Bmp, B, ScreenW/128+8, ScreenH/24+8, ScreenW, ScreenH, !IO),

    textout_ex(B, Font, "SPEED", 0, 0, Blue, -1, !IO),
    stretch_sprite(Bmp, B, ScreenW/128, ScreenH/24, ScreenW, ScreenH, !IO),

    destroy_bitmap(B, !IO),

    textout_shadow(Bmp, "Simultaneous Projections",
        ScreenW/2, ScreenH/2-80, White, !IO),
    textout_shadow(Bmp, "Employing an Ensemble of Displays",
        ScreenW/2, ScreenH/2-64, White, !IO),

    textout_shadow(Bmp, "Or alternatively: Stupid Pointless",
        ScreenW/2, ScreenH/2-32, White, !IO),
    textout_shadow(Bmp, "Effort at Establishing a Dumb Acronym",
        ScreenW/2, ScreenH/2-16, White, !IO),

    textout_shadow(Bmp, "By Shawn Hargreaves, 1999",
        ScreenW/2, ScreenH/2+16, White, !IO),
    textout_shadow(Bmp, "Written for the Allegro",
        ScreenW/2, ScreenH/2+48, White, !IO),
    textout_shadow(Bmp, "SpeedHack competition",
        ScreenW/2, ScreenH/2+64, White, !IO),

    unshade_effect(Bmp, !IO),

    destroy_bitmap(Bmp, !IO),

    wait_for_action(WhatToDo, !IO).

:- pred unshade_effect(bitmap::in, io::di, io::uo) is det.
:- pred unshade_effect_1(bitmap::in, bitmap::in, int::in, int::in, int::in,
    int::in, io::di, io::uo) is det.
:- pred unshade_effect_2(bitmap::in, bitmap::in, int::in, int::in, int::in,
    int::in, io::di, io::uo) is det.

unshade_effect(Bmp, !IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    retrace_count(C, !IO),
    int.fold_up(unshade_effect_1(Bmp, Screen, ScreenW, ScreenH, C),
        0, (ScreenH/16)+1, !IO).

unshade_effect_1(Bmp, Screen, ScreenW, ScreenH, C, I, !IO) :-
    acquire_screen(!IO),
    int.fold_up(unshade_effect_2(Bmp, Screen, ScreenW, ScreenH, I),
        0, 16+1, !IO),
    release_screen(!IO),
    spin_until_retrace_count(C + I*256/ScreenH, !IO).

unshade_effect_2(Bmp, Screen, ScreenW, ScreenH, I, J, !IO) :-
    Y = J*(ScreenH/16) + I,
    blit(Bmp, Screen, 0, Y, 0, Y, ScreenW, 1, !IO).

:- pred spin_until_retrace_count(int::in, io::di, io::uo) is det.

spin_until_retrace_count(Limit, !IO) :-
    retrace_count(RetraceCount, !IO),
    (if RetraceCount < Limit then
        rest(1, !IO),
        spin_until_retrace_count(Limit, !IO)
    else
        true
    ).

:- pred wait_for_action(what_to_do::out, io::di, io::uo) is det.
:- pred wait_for_action_workaround_compiler_bug(what_to_do::out,
    io::di, io::uo) is det.

wait_for_action(WhatToDo, !IO) :-
    key(key_space, KeySpace, !IO),
    key(key_enter, KeyEnter, !IO),
    key(key_esc, KeyEsc, !IO),
    ( KeyEsc = yes ->
        WhatToDo = quit
    ; KeyEnter = yes ->
        WhatToDo = play_game
    ;
        KeySpace = yes ->
        WhatToDo = play_game
    ; 
        wait_for_action_workaround_compiler_bug(WhatToDo, !IO)
    ).

wait_for_action_workaround_compiler_bug(WhatToDo, !IO) :-
    wait_for_action(WhatToDo, !IO).

:- pred textout_shadow(bitmap::in, string::in, int::in, int::in, color::in,
        io::di, io::uo) is det.

textout_shadow(Bmp, Message, X, Y, Color, !IO) :-
    font(Font, !IO),
    makecol(0, 0, 0, Black, !IO),
    textout_centre_ex(Bmp, Font, Message, X+1, Y+1, Black, -1, !IO),
    textout_centre_ex(Bmp, Font, Message, X, Y, Color, -1, !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
