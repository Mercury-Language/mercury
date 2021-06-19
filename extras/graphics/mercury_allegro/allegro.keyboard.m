%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.keyboard.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.keyboard.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type scancode == int.
:- type key_shifts == int.
:- type readkey_format == int.
:- type unicode == int.
:- type leds == int.

:- pred install_keyboard(bool::out, io::di, io::uo) is det.
:- pred remove_keyboard(io::di, io::uo) is det.
% install_keyboard_hooks
% poll_keyboard
% keyboard_needs_poll
:- pred key(scancode::in, bool::out, io::di, io::uo) is det.
:- pred key_shifts(key_shifts::out, io::di, io::uo) is det.
:- pred keypressed(bool::out, io::di, io::uo) is det.
:- pred readkey(readkey_format::out, io::di, io::uo) is det.
:- pred ureadkey(unicode::out, scancode::out, io::di, io::uo) is det.
:- pred scancode_to_ascii(scancode::in, char::out, io::di, io::uo) is det.
:- pred scancode_to_name(scancode::in, string::out, io::di, io::uo) is det.
:- pred simulate_keypress(readkey_format::in, io::di, io::uo) is det.
:- pred simulate_ukeypress(unicode::in, scancode::in, io::di, io::uo) is det.
% keyboard_callback
% keyboard_ucallback
% keyboard_lowlevel_callback
:- pred set_leds(leds::in, io::di, io::uo) is det.
:- pred set_keyboard_rate(int::in, int::in, io::di, io::uo) is det.
:- pred clear_keybuf(io::di, io::uo) is det.
:- pred set_three_finger_flag(bool::in, io::di, io::uo) is det.
:- pred three_finger_flag(bool::out, io::di, io::uo) is det.
:- pred set_key_led_flag(bool::in, io::di, io::uo) is det.
:- pred key_led_flag(bool::out, io::di, io::uo) is det.

:- func kb_shift_flag = int.
:- func kb_ctrl_flag = int.
:- func kb_alt_flag = int.
% KB_LWIN_FLAG
% KB_RWIN_FLAG
% KB_MENU_FLAG
% KB_COMMAND_FLAG
:- func kb_scrolock_flag = int.
:- func kb_numlock_flag = int.
:- func kb_capslock_flag = int.
% KB_INALTSEQ_FLAG
% KB_ACCENT1_FLAG
% KB_ACCENT2_FLAG
% KB_ACCENT3_FLAG
% KB_ACCENT4_FLAG
 
    % XXX might make the key constants into a type later
    %
:- func key_a = int.
:- func key_b = int.
:- func key_c = int.
:- func key_d = int.
:- func key_e = int.
:- func key_f = int.
:- func key_g = int.
:- func key_h = int.
:- func key_i = int.
:- func key_j = int.
:- func key_k = int.
:- func key_l = int.
:- func key_m = int.
:- func key_n = int.
:- func key_o = int.
:- func key_p = int.
:- func key_q = int.
:- func key_r = int.
:- func key_s = int.
:- func key_t = int.
:- func key_u = int.
:- func key_v = int.
:- func key_w = int.
:- func key_x = int.
:- func key_y = int.
:- func key_z = int.
:- func key_0 = int.
:- func key_1 = int.
:- func key_2 = int.
:- func key_3 = int.
:- func key_4 = int.
:- func key_5 = int.
:- func key_6 = int.
:- func key_7 = int.
:- func key_8 = int.
:- func key_9 = int.
:- func key_0_pad = int.
:- func key_1_pad = int.
:- func key_2_pad = int.
:- func key_3_pad = int.
:- func key_4_pad = int.
:- func key_5_pad = int.
:- func key_6_pad = int.
:- func key_7_pad = int.
:- func key_8_pad = int.
:- func key_9_pad = int.
:- func key_f1 = int.
:- func key_f2 = int.
:- func key_f3 = int.
:- func key_f4 = int.
:- func key_f5 = int.
:- func key_f6 = int.
:- func key_f7 = int.
:- func key_f8 = int.
:- func key_f9 = int.
:- func key_f10 = int.
:- func key_f11 = int.
:- func key_f12 = int.
:- func key_esc = int.
:- func key_tilde = int.
:- func key_minus = int.
:- func key_equals = int.
:- func key_backspace = int.
:- func key_tab = int.
:- func key_openbrace = int.
:- func key_closebrace = int.
:- func key_enter = int.
:- func key_colon = int.
:- func key_quote = int.
:- func key_backslash = int.
:- func key_backslash2 = int.
:- func key_comma = int.
:- func key_stop = int.
:- func key_slash = int.
:- func key_space = int.
:- func key_insert = int.
:- func key_del = int.
:- func key_home = int.
:- func key_end = int.
:- func key_pgup = int.
:- func key_pgdn = int.
:- func key_left = int.
:- func key_right = int.
:- func key_up = int.
:- func key_down = int.
:- func key_slash_pad = int.
:- func key_asterisk = int.
:- func key_minus_pad = int.
:- func key_plus_pad = int.
:- func key_del_pad = int.
:- func key_enter_pad = int.
:- func key_prtscr = int.
:- func key_pause = int.
:- func key_abnt_c1 = int.
:- func key_yen = int.
:- func key_kana = int.
:- func key_convert = int.
:- func key_noconvert = int.
:- func key_at = int.
:- func key_circumflex = int.
:- func key_colon2 = int.
:- func key_kanji = int.
:- func key_equals_pad = int.
:- func key_backquote = int.
:- func key_semicolon = int.
:- func key_command = int.
:- func key_unknown1 = int.
:- func key_unknown2 = int.
:- func key_unknown3 = int.
:- func key_unknown4 = int.
:- func key_unknown5 = int.
:- func key_unknown6 = int.
:- func key_unknown7 = int.
:- func key_unknown8 = int.
:- func key_modifiers = int.
:- func key_lshift = int.
:- func key_rshift = int.
:- func key_lcontrol = int.
:- func key_rcontrol = int.
:- func key_alt = int.
:- func key_altgr = int.
:- func key_lwin = int.
:- func key_rwin = int.
:- func key_menu = int.
:- func key_scrlock = int.
:- func key_numlock = int.
:- func key_capslock = int.
:- func key_max = int.

    % Additions to C API.
    %
:- pred readkey_decode(char::out, scancode::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #include <allegro.h>
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    install_keyboard(Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == install_keyboard()) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    remove_keyboard(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    remove_keyboard();
    IO = IO0;
").

    % Don't let this function be inlined, due to problems with intermodule
    % optimisation.  In some versions of Allegro, `key' is #defined to an
    % internal symbol[1], but the Mercury headers use the word `key' as well.
    % To avoid problems, in every module except this one, we #undef key right
    % after #include <allegro.h>.  If this function is inlined into a user
    % module, then we must have the "#define key" still around for it to
    % work.  Most likely it won't be the case, since the user probably will
    % include other allegro.* modules, which would cause `key' to be
    % undefined.
    %
    % [1] to avoid a conflict with the curses API.
    %
:- pragma no_inline(pred(key/4)).
:- pragma foreign_proc("C",
    key(Scancode::in, IsPressed::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ASSERT(Scancode > 0 && Scancode < KEY_MAX);
    IsPressed = key[Scancode] ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    key_shifts(KeyShifts::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    KeyShifts = key_shifts;
    IO = IO0;
").

:- pragma foreign_proc("C",
    keypressed(KeyPressed::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    KeyPressed = keypressed() ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    readkey(R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = readkey();
    IO = IO0;
").

:- pragma foreign_proc("C",
    ureadkey(Unichar::out, Scancode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    int Scancode0;
    Unichar = ureadkey(&Scancode0);
    Scancode = Scancode0;
    IO = IO0;
").

:- pragma foreign_proc("C",
    scancode_to_ascii(Scancode::in, Ascii::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Ascii = scancode_to_ascii(Scancode);
    IO = IO0;
").

:- pragma foreign_proc("C",
    scancode_to_name(Scancode::in, Name::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Name = (char *) scancode_to_name(Scancode);
    IO = IO0;
").

:- pragma foreign_proc("C",
    simulate_keypress(X::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    simulate_keypress(X);
    IO = IO0;
").

:- pragma foreign_proc("C",
    simulate_ukeypress(Unichar::in, Scancode::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    simulate_ukeypress(Unichar, Scancode);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_leds(Setting::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_leds(Setting);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_keyboard_rate(Delay::in, Repeat::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_keyboard_rate(Delay, Repeat);
    IO = IO0;
").

:- pragma foreign_proc("C",
    clear_keybuf(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    clear_keybuf();
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_three_finger_flag(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    three_finger_flag = Set ? TRUE : FALSE;
    IO = IO0;
").

:- pragma foreign_proc("C",
    three_finger_flag(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Get = three_finger_flag ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_key_led_flag(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    key_led_flag = Set ? TRUE : FALSE;
    IO = IO0;
").

:- pragma foreign_proc("C",
    key_led_flag(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Get = key_led_flag ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C", kb_shift_flag = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = KB_SHIFT_FLAG;").
:- pragma foreign_proc("C", kb_ctrl_flag = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = KB_CTRL_FLAG;").
:- pragma foreign_proc("C", kb_alt_flag = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = KB_ALT_FLAG;").
:- pragma foreign_proc("C", kb_scrolock_flag = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = KB_SCROLOCK_FLAG;").
:- pragma foreign_proc("C", kb_numlock_flag = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = KB_NUMLOCK_FLAG;").
:- pragma foreign_proc("C", kb_capslock_flag = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = KB_CAPSLOCK_FLAG;").

%-----------------------------------------------------------------------------%

key_a            = 1.
key_b            = 2.
key_c            = 3.
key_d            = 4.
key_e            = 5.
key_f            = 6.
key_g            = 7.
key_h            = 8.
key_i            = 9.
key_j            = 10.
key_k            = 11.
key_l            = 12.
key_m            = 13.
key_n            = 14.
key_o            = 15.
key_p            = 16.
key_q            = 17.
key_r            = 18.
key_s            = 19.
key_t            = 20.
key_u            = 21.
key_v            = 22.
key_w            = 23.
key_x            = 24.
key_y            = 25.
key_z            = 26.
key_0            = 27.
key_1            = 28.
key_2            = 29.
key_3            = 30.
key_4            = 31.
key_5            = 32.
key_6            = 33.
key_7            = 34.
key_8            = 35.
key_9            = 36.
key_0_pad        = 37.
key_1_pad        = 38.
key_2_pad        = 39.
key_3_pad        = 40.
key_4_pad        = 41.
key_5_pad        = 42.
key_6_pad        = 43.
key_7_pad        = 44.
key_8_pad        = 45.
key_9_pad        = 46.
key_f1           = 47.
key_f2           = 48.
key_f3           = 49.
key_f4           = 50.
key_f5           = 51.
key_f6           = 52.
key_f7           = 53.
key_f8           = 54.
key_f9           = 55.
key_f10          = 56.
key_f11          = 57.
key_f12          = 58.
key_esc          = 59.
key_tilde        = 60.
key_minus        = 61.
key_equals       = 62.
key_backspace    = 63.
key_tab          = 64.
key_openbrace    = 65.
key_closebrace   = 66.
key_enter        = 67.
key_colon        = 68.
key_quote        = 69.
key_backslash    = 70.
key_backslash2   = 71.
key_comma        = 72.
key_stop         = 73.
key_slash        = 74.
key_space        = 75.
key_insert       = 76.
key_del          = 77.
key_home         = 78.
key_end          = 79.
key_pgup         = 80.
key_pgdn         = 81.
key_left         = 82.
key_right        = 83.
key_up           = 84.
key_down         = 85.
key_slash_pad    = 86.
key_asterisk     = 87.
key_minus_pad    = 88.
key_plus_pad     = 89.
key_del_pad      = 90.
key_enter_pad    = 91.
key_prtscr       = 92.
key_pause        = 93.
key_abnt_c1      = 94.
key_yen          = 95.
key_kana         = 96.
key_convert      = 97.
key_noconvert    = 98.
key_at           = 99.
key_circumflex   = 100.
key_colon2       = 101.
key_kanji        = 102.
key_equals_pad   = 103.
key_backquote    = 104.
key_semicolon    = 105.
key_command      = 106.
key_unknown1     = 107.
key_unknown2     = 108.
key_unknown3     = 109.
key_unknown4     = 110.
key_unknown5     = 111.
key_unknown6     = 112.
key_unknown7     = 113.
key_unknown8     = 114.

key_modifiers    = 115.

key_lshift       = 115.
key_rshift       = 116.
key_lcontrol     = 117.
key_rcontrol     = 118.
key_alt          = 119.
key_altgr        = 120.
key_lwin         = 121.
key_rwin         = 122.
key_menu         = 123.
key_scrlock      = 124.
key_numlock      = 125.
key_capslock     = 126.

key_max          = 127.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    readkey_decode(Chr::out, Scancode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    int R = readkey();
    Chr = R & 0xff;
    Scancode = R >> 8;
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
