%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.sound.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.sound.
:- interface.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type digi_driver
    --->    digi_autodetect
    ;       digi_none.

:- type midi_driver
    --->    midi_autodetect
    ;       midi_none.

:- pred detect_digi_driver(digi_driver::in, int::out, io::di, io::uo) is det.
:- pred detect_midi_driver(midi_driver::in, int::out, io::di, io::uo) is det.
:- pred reserve_voices(int::in, int::in, io::di, io::uo) is det.
:- pred set_volume_per_voice(int::in, io::di, io::uo) is det.
:- pred install_sound(digi_driver::in, midi_driver::in, bool::out,
    io::di, io::uo) is det.
:- pred remove_sound(io::di, io::uo) is det.
:- pred set_volume(int::in, int::in, io::di, io::uo) is det.
:- pred set_hardware_volume(int::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

detect_digi_driver(Driver, Voices, !IO) :-
    detect_digi_driver_2(digi_driver_to_int(Driver), Voices, !IO).

:- pred detect_digi_driver_2(int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    detect_digi_driver_2(Driver::in, Voices::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Voices = detect_digi_driver(Driver);
    IO = IO0;
").

detect_midi_driver(Driver, Voices, !IO) :-
    detect_midi_driver_2(midi_driver_to_int(Driver), Voices, !IO).

:- pred detect_midi_driver_2(int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    detect_midi_driver_2(Driver::in, Voices::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Voices = detect_midi_driver(Driver);
    IO = IO0;
").

:- pragma foreign_proc("C",
    reserve_voices(DigiVoices::in, MidiVoices::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    reserve_voices(DigiVoices, MidiVoices);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_volume_per_voice(Scale::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_volume_per_voice(Scale);
    IO = IO0;
").

install_sound(Digi, Midi, Success, !IO) :-
    install_sound_2(digi_driver_to_int(Digi), midi_driver_to_int(Midi),
        Success, !IO).

:- pred install_sound_2(int::in, int::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    install_sound_2(Digi::in, Midi::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == install_sound(Digi, Midi, NULL)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    remove_sound(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    remove_sound();
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_volume(DigiVolume::in, MidiVolume::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_volume(DigiVolume, MidiVolume);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_hardware_volume(DigiVolume::in, MidiVolume::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_hardware_volume(DigiVolume, MidiVolume);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func digi_driver_to_int(digi_driver) = int.

digi_driver_to_int(digi_autodetect) = digi_autodetect_int.
digi_driver_to_int(digi_none) = digi_none_int.

:- func digi_autodetect_int = int.
:- func digi_none_int = int.

:- pragma foreign_proc("C", digi_autodetect_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = DIGI_AUTODETECT;").
:- pragma foreign_proc("C", digi_none_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = DIGI_NONE;").

%-----------------------------------------------------------------------------%

:- func midi_driver_to_int(midi_driver) = int.

midi_driver_to_int(midi_autodetect) = midi_autodetect_int.
midi_driver_to_int(midi_none) = midi_none_int.

:- func midi_autodetect_int = int.
:- func midi_none_int = int.

:- pragma foreign_proc("C", midi_autodetect_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MIDI_AUTODETECT;").
:- pragma foreign_proc("C", midi_none_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MIDI_NONE;").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
