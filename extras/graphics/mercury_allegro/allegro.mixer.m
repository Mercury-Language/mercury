%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.mixer.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.mixer.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred set_mixer_quality(int::in, io::di, io::uo) is det.
:- pred get_mixer_quality(int::out, io::di, io::uo) is det.
:- pred get_mixer_frequency(int::out, io::di, io::uo) is det.
:- pred get_mixer_bits(int::out, io::di, io::uo) is det.
:- pred get_mixer_channels(int::out, io::di, io::uo) is det.
:- pred get_mixer_voices(int::out, io::di, io::uo) is det.
:- pred get_mixer_buffer_length(int::out, io::di, io::uo) is det.

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
    set_mixer_quality(Quality::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_mixer_quality(Quality);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_mixer_quality(Quality::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Quality = get_mixer_quality();
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_mixer_frequency(Frequency::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Frequency = get_mixer_frequency();
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_mixer_bits(Bits::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Bits = get_mixer_bits();
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_mixer_channels(Channels::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Channels = get_mixer_channels();
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_mixer_voices(Voices::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Voices = get_mixer_voices();
    IO = IO0;
").

:- pragma foreign_proc("c",
    get_mixer_buffer_length(BufferLength::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    BufferLength = get_mixer_buffer_length();
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
