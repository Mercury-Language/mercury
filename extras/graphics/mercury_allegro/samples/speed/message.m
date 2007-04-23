%-----------------------------------------------------------------------------%

:- module message.
:- interface.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type messages.

:- func init_messages = messages.
:- pred add_message(string::in, messages::in, messages::out) is det.
:- pred update_messages(messages::in, messages::out, io::di, io::uo) is det.
:- pred draw_messages(bitmap::in, messages::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.color.
:- import_module allegro.text.

:- import_module float.
:- import_module int.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type messages == list(message).

:- type message
    --->    message(
                text    :: string,
                length  :: int,
                time    :: int,
                x       :: float,
                y       :: float
            ).

init_messages = [].

add_message(Text, Messages, [Message | Messages]) :-
    Message = message(Text, -1, 0, 0.0, 0.0).

draw_messages(Bitmap, Messages, !IO) :-
    font(Font, !IO),
    makecol(255, 255, 255, White, !IO),
    list.foldl(draw_message(Bitmap, Font, White), Messages, !IO).

update_messages(!Messages, !IO) :-
    det_screen(_, ScreenW, ScreenH, !IO),
    update_messages_2(ScreenW, !Messages, ScreenH/2, _, !IO).

:- pred update_messages_2(int::in, list(message)::in, list(message)::out,
        int::in, int::out, io::di, io::uo) is det.

update_messages_2(_ScreenW, [], [], Y, Y, !IO).
update_messages_2(ScreenW, [Msg0 | Msgs0], Msgs, Y0, Y, !IO) :-
    (if update_message(ScreenW, Msg0, Msg1, Y0, Y1) then
        update_messages_2(ScreenW, Msgs0, Msgs1, Y1, Y, !IO),
        % Hack to avoid having to thread the IO state through add_message.
        (if Msgs1 ^ length = -1 then
            font(Font, !IO),
            text_length(Font, Msg1 ^ text, Length, !IO),
            Msg = Msg1 ^ length := Length
        else
            Msg = Msg1
        ),
        Msgs = [Msg | Msgs1]
    else
        update_messages_2(ScreenW, Msgs0, Msgs, Y0, Y, !IO)
    ).

:- pred update_message(int::in, message::in, message::out, int::in, int::out)
        is semidet.

update_message(ScreenW,
        message(Text, Length, Time0, MX0, MY0),
        message(Text, Length, Time, MX, MY),
        Y, Y+16) :-
    (if Time0 < 100 then
        MX1 = MX0 * 0.9,
        MX = MX1 + float(ScreenW) * 0.05
    else
        MX = MX0 + float(Time0 - 100)
    ),
    MY = (MY0 * 0.9) + float(Y) * 0.1,
    Time = Time0 + 1,
    MX =< float(ScreenW + Length/4).

:- pred draw_message(bitmap::in, font::in, color::in, message::in,
        io::di, io::uo) is det.

draw_message(Bitmap, Font, White, message(Text, _Length, _Time, X, Y), !IO) :-
    IX = truncate_to_int(X),
    IY = truncate_to_int(Y),
    textout_centre_ex(Bitmap, Font, Text, IX, IY, White, -1, !IO).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
