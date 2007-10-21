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

:- pragma foreign_proc("C",
    detect_digi_driver(Driver::in, Voices::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Voices = detect_digi_driver(Driver);
    IO = IO0;
").

:- pragma foreign_proc("C",
    detect_midi_driver(Driver::in, Voices::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Voices = detect_midi_driver(Driver);
    IO = IO0;
").

:- pragma foreign_proc("C",
    reserve_voices(DigiVoices::in, MidiVoices::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    reserve_voices(DigiVoices, MidiVoices);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_volume_per_voice(Scale::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_volume_per_voice(Scale);
    IO = IO0;
").

:- pragma foreign_proc("C",
    install_sound(Digi::in, Midi::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == install_sound(Digi, Midi, NULL)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    remove_sound(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    remove_sound();
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_volume(DigiVolume::in, MidiVolume::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_volume(DigiVolume, MidiVolume);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_hardware_volume(DigiVolume::in, MidiVolume::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_hardware_volume(DigiVolume, MidiVolume);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", digi_driver/0, [
    digi_autodetect - "DIGI_AUTODETECT",
    digi_none       - "DIGI_NONE"
]).

:- pragma foreign_enum("C", midi_driver/0, [
    midi_autodetect - "MIDI_AUTODETECT",
    midi_none       - "MIDI_NONE"
]).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
