%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.joystick.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.joystick.
:- interface.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type joy_type
    --->    joy_type_autodetect.

:- type flags == int.

:- type stick_info.
:- type stick_flags == int.

:- type axis_info.

:- pred install_joystick(joy_type::in, bool::out, io::di, io::uo) is det.
:- pred remove_joystick(io::di, io::uo) is det.
:- pred poll_joystick(bool::out, io::di, io::uo) is det.
:- pred num_joysticks(int::out, io::di, io::uo) is det.

    % Additions to C API.
    %
:- pred joy_flags(int::in, flags::out, io::di, io::uo) is det.
:- pred joy_num_sticks(int::in, int::out, io::di, io::uo) is det.
:- pred joy_num_buttons(int::in, int::out, io::di, io::uo) is det.
:- pred joy_button_b(int::in, int::in, bool::out, io::di, io::uo) is det.
:- pred joy_button_name(int::in, int::in, string::out, io::di, io::uo) is det.
:- pred joy_stick(int::in, int::in, stick_info::out) is det.
:- func stick_flags(stick_info) = stick_flags.
:- func stick_num_axes(stick_info) = int.
:- pred stick_axis(stick_info::in, int::in, axis_info::out) is det.
:- func stick_name(stick_info) = string.
:- pred axis_pos(axis_info::in, int::out, io::di, io::uo) is det.
:- pred axis_d(axis_info::in, bool::out, bool::out, io::di, io::uo) is det.
:- func axis_name(axis_info) = string.

:- func joyflag_digital = int.
:- func joyflag_analogue = int.
:- func joyflag_calib_digital = int.
:- func joyflag_calib_analogue = int.
:- func joyflag_calibrate = int.
:- func joyflag_signed = int.
:- func joyflag_unsigned = int.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", stick_info, "JOYSTICK_STICK_INFO *",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("C", axis_info, "JOYSTICK_AXIS_INFO *",
    [can_pass_as_mercury_type]).

install_joystick(_Type, Success, !IO) :-
    install_joystick_2(Success, !IO).

:- pred install_joystick_2(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    install_joystick_2(Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == install_joystick(JOY_TYPE_AUTODETECT)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    remove_joystick(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    remove_joystick();
    IO = IO0;
").

:- pragma foreign_proc("C",
    poll_joystick(Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == poll_joystick()) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    num_joysticks(Num::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Num = num_joysticks;
    IO = IO0;
").

:- pragma foreign_proc("C",
    joy_flags(N::in, Flags::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Flags = joy[N].flags;
    IO = IO0;
").

:- pragma foreign_proc("C",
    joy_num_sticks(N::in, NumSticks::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NumSticks = joy[N].num_sticks;
    IO = IO0;
").

:- pragma foreign_proc("C",
    joy_num_buttons(N::in, NumButtons::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NumButtons = joy[N].num_buttons;
    IO = IO0;
").

:- pragma foreign_proc("C",
    joy_button_b(N::in, Button::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = joy[N].button[Button].b ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    joy_button_name(N::in, Button::in, Name::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const char *Name0 = joy[N].button[Button].name;
    Name = (char *)Name0;
    IO = IO0;
").

:- pragma foreign_proc("C",
    joy_stick(N::in, Stick::in, StickInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    StickInfo = &joy[N].stick[Stick];
").

:- pragma foreign_proc("C",
    stick_flags(Stick::in) = (Flags::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Flags = Stick->flags;
").

:- pragma foreign_proc("C",
    stick_num_axes(Stick::in) = (Num::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Num = Stick->num_axis;
").

:- pragma foreign_proc("C",
    stick_axis(Stick::in, Axis::in, AxisInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    AxisInfo = &Stick->axis[Axis];
").

:- pragma foreign_proc("C",
    stick_name(Stick::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const char *Name0 = Stick->name;
    Name = (char *)Name0;
").

:- pragma foreign_proc("C",
    axis_pos(AxisInfo::in, Pos::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pos = AxisInfo->pos;
    IO = IO0;
").

:- pragma foreign_proc("C",
    axis_d(AxisInfo::in, D1::out, D2::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D1 = AxisInfo->d1 ? MR_YES : MR_NO;
    D2 = AxisInfo->d2 ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    axis_name(AxisInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const char *Name0 = AxisInfo->name;
    Name = (char *)Name0;
").

:- pragma foreign_proc("C",
    joyflag_digital = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Flag = JOYFLAG_DIGITAL;").
:- pragma foreign_proc("C",
    joyflag_analogue = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Flag = JOYFLAG_ANALOGUE;").
:- pragma foreign_proc("C",
    joyflag_calib_digital = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Flag = JOYFLAG_CALIB_DIGITAL;").
:- pragma foreign_proc("C",
    joyflag_calib_analogue = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Flag = JOYFLAG_CALIB_ANALOGUE;").
:- pragma foreign_proc("C",
    joyflag_calibrate = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Flag = JOYFLAG_CALIBRATE;").
:- pragma foreign_proc("C",
    joyflag_signed = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Flag = JOYFLAG_SIGNED;").
:- pragma foreign_proc("C",
    joyflag_unsigned = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Flag = JOYFLAG_UNSIGNED;").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
