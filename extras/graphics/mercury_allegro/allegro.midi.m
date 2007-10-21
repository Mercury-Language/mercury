%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.midi.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.midi.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type midi.

:- pred load_midi(string::in, maybe(midi)::out, io::di, io::uo) is det.
:- pred destroy_midi(midi::in, io::di, io::uo) is det.
% lock_midi
:- pred play_midi(midi::in, bool::in, io::di, io::uo) is det.
:- pred play_looped_midi(midi::in, int::in, int::in, io::di, io::uo) is det.
:- pred stop_midi(io::di, io::uo) is det.
:- pred midi_pause(io::di, io::uo) is det.
:- pred midi_resume(io::di, io::uo) is det.
:- pred midi_seek(int::in, io::di, io::uo) is det.
:- pred get_midi_length(midi::in, int::out, io::di, io::uo) is det.
% midi_out
:- pred load_midi_patches(bool::out, io::di, io::uo) is det.
:- pred midi_pos(maybe(int)::out, io::di, io::uo) is det.
:- pred midi_time(int::out, io::di, io::uo) is det.
:- pred midi_loop_start(int::out, io::di, io::uo) is det.
:- pred set_midi_loop_start(int::in, io::di, io::uo) is det.
:- pred midi_loop_end(int::out, io::di, io::uo) is det.
:- pred set_midi_loop_end(int::in, io::di, io::uo) is det.
% midi_msg_callback
% midi_meta_callback
% midi_sysex_callback
% load_ibk

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

:- pragma foreign_import_module("C", allegro.util).

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", midi, "MIDI *", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    load_midi(Filename::in, MaybeMidi::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    MIDI *Midi = load_midi(Filename);
    if (Midi) {
        MaybeMidi = _mal_make_yes_midi(Midi);
    } else {
        MaybeMidi = _mal_make_no_midi();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    destroy_midi(Midi::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    destroy_midi(Midi);
    IO = IO0;
").

:- pragma foreign_proc("C",
    play_midi(Midi::in, Loop::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    play_midi(Midi, Loop);
    IO = IO0;
").

:- pragma foreign_proc("C",
    play_looped_midi(Midi::in, LoopStart::in, LoopEnd::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    play_looped_midi(Midi, LoopStart, LoopEnd);
    IO = IO0;
").

:- pragma foreign_proc("C",
    stop_midi(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    stop_midi();
    IO = IO0;
").

:- pragma foreign_proc("C",
    midi_pause(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    midi_pause();
    IO = IO0;
").

:- pragma foreign_proc("C",
    midi_resume(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    midi_resume();
    IO = IO0;
").

:- pragma foreign_proc("C",
    midi_seek(Target::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    midi_seek(Target);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_midi_length(Midi::in, Time::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Time = get_midi_length(Midi);
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_midi_patches(Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = load_midi_patches();
    IO = IO0;
").

:- pragma foreign_proc("C",
    midi_pos(MaybePos::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    int Pos = midi_pos;
    if (Pos >= 0) {
        MaybePos = _mal_make_yes_int(Pos);
    } else {
        MaybePos = _mal_make_no_int();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    midi_time(Time::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Time = midi_time;
    IO = IO0;
").

:- pragma foreign_proc("C",
    midi_loop_start(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Get = midi_loop_start;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_midi_loop_start(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    midi_loop_start = Set;
    IO = IO0;
").

:- pragma foreign_proc("C",
    midi_loop_end(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Get = midi_loop_end;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_midi_loop_end(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    midi_loop_end = Set;
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func make_yes_midi(midi) = maybe(midi).
:- func make_no_midi = maybe(midi).

:- pragma foreign_export("C", make_yes_midi(in) = out, "_mal_make_yes_midi").
:- pragma foreign_export("C", make_no_midi = out, "_mal_make_no_midi").

make_yes_midi(X) = yes(X).
make_no_midi = no.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
