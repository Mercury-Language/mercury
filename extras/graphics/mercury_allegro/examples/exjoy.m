%-----------------------------------------------------------------------------%
%
% Example program for the Allegro library, by Grzegorz Adam Hankiewicz
% Mercury port by Peter Wang.
%
% This program uses the Allegro library to detect and read the value
% of a joystick. The output of the program is a small target sight
% on the screen which you can move. At the same time the program will
% tell you what you are doing with the joystick (moving or firing).
%
%-----------------------------------------------------------------------------%

:- module exjoy.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.blit.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.joystick.
:- import_module allegro.keyboard.
:- import_module allegro.palette.
:- import_module allegro.prim.
:- import_module allegro.text.
:- import_module allegro.transparency.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        install_keyboard(_, !IO),
        set_gfx_mode(gfx_autodetect, 320, 200, 0, 0, GfxOk, !IO),
        (
            GfxOk = yes,
            main_1(!IO)
        ;
            GfxOk = no,
            allegro_message("Unable to set any graphic mode", !IO)
        )
    ;
        Ok = no,
        io.write_string("Error initialising Allegro.", !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_1(io::di, io::uo) is det.

main_1(!IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    font(Font, !IO),
    set_palette(default_palette, !IO),
    Black = 0,
    White = 255,
    clear_bitmap(Screen, !IO),
    textout_centre_ex(Screen, Font, "Please centre the joystick",
        ScreenW/2, ScreenH/2 - 36, White, Black, !IO),
    textout_centre_ex(Screen, Font, "and press a key.",
        ScreenW/2, ScreenH/2 - 20, White, Black, !IO),
    readkey(K, !IO),
    (if (K /\ 0xff) = 27 then
        true
    else
        install_joystick(joy_type_autodetect, JoyOk, !IO),
        (
            JoyOk = yes,
            main_2(!IO)
        ;
            JoyOk = no,
            allegro_error(Msg, !IO),
            set_text_mode(!IO),
            allegro_message("Error initialising joystick\n" ++
                Msg ++ "\n", !IO)
        )
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    num_joysticks(Num, !IO),
    (if Num = 0 then
        set_text_mode(!IO),
        allegro_message("Error: joystick not found\n", !IO)
    else
        % no joystick calibration in this version
        joy_stick(0, 0, Stick),
        (if stick_flags(Stick) /\ joyflag_analogue \= 0 then
            select_analogue_digital(AnalogueMode, !IO)
        else
            AnalogueMode = no
        ),
        drawing_mode(xor, !IO),
        clear_keybuf(!IO),
        det_create_bitmap(320, 200, Bmp, !IO),
        main_3(AnalogueMode, Bmp, 160, 100, !IO),
        destroy_bitmap(Bmp, !IO)
    ).

:- pred select_analogue_digital(bool::out, io::di, io::uo) is det.
:- pred select_analogue_digital_2(bool::out, io::di, io::uo) is det.

select_analogue_digital(AnalogueMode, !IO) :-
    det_screen(Screen, ScreenW, _ScreenH, !IO),
    font(Font, !IO),
    clear_bitmap(Screen, !IO),
    textout_centre_ex(Screen, Font, "Now press 'D' to use a digital",
        ScreenW/2, 64, 255, 0, !IO),
    textout_centre_ex(Screen, Font, "joystick or 'A' for analogue mode.",
        ScreenW/2, 80, 255, 0, !IO),
    select_analogue_digital_2(AnalogueMode, !IO).

select_analogue_digital_2(AnalogueMode, !IO) :-
    readkey(C0, !IO),
    C = char.det_from_int(C0 /\ 0xff),
    (if C = 'd' ; C = 'D' then
        AnalogueMode = no
    else if C = 'a' ; C = 'A' then
        AnalogueMode = yes
    else
        select_analogue_digital_2(AnalogueMode, !IO)
    ).

:- pred main_3(bool::in, bitmap::in, int::in, int::in, io::di, io::uo) is det.

main_3(AnalogueMode, Bmp, X0, Y0, !IO) :-
    poll_joystick(_Ok, !IO),
    clear_bitmap(Bmp, !IO),
    font(Font, !IO),
    textout_centre_ex(Bmp, Font, "Joystick", 160, 150, 255, 0, !IO),
    (
        AnalogueMode = yes,
        AnalogueModeMsg = "Analogue mode selected"
    ;
        AnalogueMode = no,
        AnalogueModeMsg = "Digital mode selected"
    ),
    textout_centre_ex(Bmp, Font,
        AnalogueModeMsg, 160, 160, 255, 0, !IO),
    textout_centre_ex(Bmp, Font,
        "Move the joystick all around", 160, 170, 255, 0, !IO),
    textout_centre_ex(Bmp, Font,
        "Press any key to exit", 160, 180, 255, 0, !IO),
    textout_centre_ex(Bmp, Font,
        "Made by Grzegorz Adam Hankiewicz", 160, 190, 255, 0, !IO),
    joy_num_buttons(0, NumButtons, !IO),
    int.fold_up(print_button_message(Bmp, Font), 0, NumButtons-1, !IO),
    (
        AnalogueMode = yes,
        joy_stick(0, 0, Stick),
        stick_axis(Stick, 0, XAxis),
        stick_axis(Stick, 1, YAxis),
        axis_pos(XAxis, XPos, !IO),
        axis_pos(YAxis, YPos, !IO),
        X1 = X0 + XPos/40,
        Y1 = Y0 + YPos/40
    ;
        AnalogueMode = no,
        joy_stick(0, 0, Stick),
        stick_axis(Stick, 0, XAxis),
        stick_axis(Stick, 1, YAxis),
        axis_d(XAxis, XD1, XD2, !IO),
        axis_d(YAxis, YD1, YD2, !IO),
        some [!X, !Y] (
            !:X = X0,
            !:Y = Y0,
            ( XD1 = yes,
                add(-1, !X),
                textout_centre_ex(Bmp, Font, "Left", 120, 100, 255, 0, !IO)
            ; XD1 = no
            ),
            ( XD2 = yes,
                add(+1, !X),
                textout_centre_ex(Bmp, Font, "Right", 200, 100, 255, 0, !IO)
            ; XD2 = no
            ),
            ( YD1 = yes,
                add(-1, !Y),
                textout_centre_ex(Bmp, Font, "Up", 160, 70, 255, 0, !IO)
            ; YD1 = no
            ),
            ( YD2 = yes,
                add(+1, !Y),
                textout_centre_ex(Bmp, Font, "Down", 160, 130, 255, 0, !IO)
            ; YD2 = no
            ),
            X1 = !.X,
            Y1 = !.Y
        )
    ),
    X = clamp(0, X1, 319),
    Y = clamp(0, Y1, 199),
    circle(Bmp, X, Y, 5, 255, !IO),
    putpixel(Bmp, X, Y, 255, !IO),
    putpixel(Bmp, X+1, Y, 255, !IO),
    putpixel(Bmp, X, Y+1, 255, !IO),
    putpixel(Bmp, X-1, Y, 255, !IO),
    putpixel(Bmp, X, Y-1, 255, !IO),
    putpixel(Bmp, X+5, Y, 255, !IO),
    putpixel(Bmp, X, Y+5, 255, !IO),
    putpixel(Bmp, X-5, Y, 255, !IO),
    putpixel(Bmp, X, Y-5, 255, !IO),
    det_screen(Screen, ScreenW, ScreenH, !IO),
    blit(Bmp, Screen, 0, 0, ScreenW/2 - 160, ScreenH/2 - 100, 320, 200, !IO),
    keypressed(KeyPressed, !IO),
    (
        KeyPressed = yes
    ;
        KeyPressed = no,
        main_3(AnalogueMode, Bmp, X, Y, !IO)
    ).

:- pred print_button_message(bitmap::in, font::in, int::in, io::di, io::uo)
        is det.

print_button_message(Bmp, Font, Button, !IO) :-
    joy_button_b(0, Button, B, !IO),
    (
        B = yes,
        joy_button_name(0, Button, Name, !IO),
        Str = Name ++ " pressed",
        textout_centre_ex(Bmp, Font, Str, 160, Button*10, 15, 0, !IO)
    ;
        B = no
    ).

:- pred add(int::in, int::in, int::out) is det.
add(D, X, X+D).

:- func clamp(int, int, int) = int.
clamp(Lo, Val, Hi) = max(Lo, min(Val, Hi)).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
