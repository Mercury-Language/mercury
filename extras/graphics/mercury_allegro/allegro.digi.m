%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.digi.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.digi.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type sample.
:- type voice.

:- pred load_sample(string::in, maybe(sample)::out, io::di, io::uo) is det.
:- pred load_wav(string::in, maybe(sample)::out, io::di, io::uo) is det.
:- pred load_voc(string::in, maybe(sample)::out, io::di, io::uo) is det.
:- pred save_sample(string::in, sample::in, bool::out, io::di, io::uo) is det.
:- pred create_sample(bits::in, channels::in, freq::in, len::in, maybe(sample)::out, io::di, io::uo) is det.
:- pred destroy_sample(sample::in, io::di, io::uo) is det.
% lock_sample
% register_sample_file_type
:- pred play_sample(sample::in, vol::in, pan::in, freq::in, loop::in, io::di, io::uo) is det.
:- pred adjust_sample(sample::in, vol::in, pan::in, freq::in, loop::in, io::di, io::uo) is det.
:- pred stop_sample(sample::in, io::di, io::uo) is det.
:- pred allocate_voice(sample::in, maybe(voice)::out, io::di, io::uo) is det.
:- pred deallocate_voice(voice::in, io::di, io::uo) is det.
:- pred reallocate_voice(voice::in, sample::in, io::di, io::uo) is det.
:- pred release_voice(voice::in, io::di, io::uo) is det.
:- pred voice_start(voice::in, io::di, io::uo) is det.
:- pred voice_stop(voice::in, io::di, io::uo) is det.
:- pred voice_set_priority(voice::in, priority::in, io::di, io::uo) is det.
:- pred voice_check(voice::in, maybe(sample)::out, io::di, io::uo) is det.
:- pred voice_get_position(voice::in, maybe(pos)::out, io::di, io::uo) is det.
:- pred voice_set_position(voice::in, pos::in, io::di, io::uo) is det.
:- pred voice_set_playmode(voice::in, playmode::in, io::di, io::uo) is det.
:- pred voice_get_volume(voice::in, maybe(vol)::out, io::di, io::uo) is det.
:- pred voice_set_volume(voice::in, vol::in, io::di, io::uo) is det.
:- pred voice_ramp_volume(voice::in, vol::in, vol::in, io::di, io::uo) is det.
:- pred voice_stop_volumeramp(voice::in, io::di, io::uo) is det.
:- pred voice_get_frequency(voice::in, freq::out, io::di, io::uo) is det.
:- pred voice_set_frequency(voice::in, freq::in, io::di, io::uo) is det.
:- pred voice_sweep_frequency(voice::in, freq::in, freq::in, io::di, io::uo) is det.
:- pred voice_stop_frequency_sweep(voice::in, io::di, io::uo) is det.
:- pred voice_get_pan(voice::in, pan::out, io::di, io::uo) is det.
:- pred voice_set_pan(voice::in, pan::in, io::di, io::uo) is det.
:- pred voice_sweep_pan(voice::in, pan::in, pan::in, io::di, io::uo) is det.
:- pred voice_stop_pan_sweep(voice::in, io::di, io::uo) is det.
% voice_set_echo
% voice_set_tremolo
% voice_set_vibrato

:- type bits
    --->    bits_8
    ;       bits_16.

:- type channels
    --->    mono
    ;       stereo.

:- type vol == int.

:- type pan == int.

:- type freq == int.

:- type loop
    --->    loop
    ;       no_loop.

:- type len == int.

:- type priority == int.

:- type pos == int.

:- type playmode
    --->    playmode_play
    ;       playmode_loop
    ;       playmode_forward
    ;       playmode_backward
    ;       playmode_bidir.

    % Additions to C API
:- func sample ^ bits = bits.
:- func sample ^ channels = channels.
:- func sample ^ freq = freq.
:- func sample ^ priority = priority.
:- func sample ^ len = len.
:- pred get_sample(sample::in, pos::in, int::out, io::di, io::uo) is det.
:- pred set_sample(sample::in, pos::in, int::in, io::di, io::uo) is det.

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

:- pragma foreign_type("C", sample, "SAMPLE *").

:- pragma foreign_proc("C",
    load_sample(Filename::in, MaybeSample::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    SAMPLE *Sample = load_sample(Filename);
    if (Sample) {
        MaybeSample = _mal_make_yes_sample(Sample);
    } else {
        MaybeSample = _mal_make_no_sample();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_wav(Filename::in, MaybeSample::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    SAMPLE *Sample = load_wav(Filename);
    if (Sample) {
        MaybeSample = _mal_make_yes_sample(Sample);
    } else {
        MaybeSample = _mal_make_no_sample();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_voc(Filename::in, MaybeSample::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    SAMPLE *Sample = load_voc(Filename);
    if (Sample) {
        MaybeSample = _mal_make_yes_sample(Sample);
    } else {
        MaybeSample = _mal_make_no_sample();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    save_sample(Filename::in, Sample::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == save_sample(Filename, Sample)) ? MR_YES : MR_NO;
    IO = IO0;
").

create_sample(Bits, Channels, Freq, Len, MaybeSample, !IO) :-
    bits(Bits, BitsInt),
    channels_is_stereo_int(Channels, IsStereo),
    create_sample_2(BitsInt, IsStereo, Freq, Len, MaybeSample, !IO).

:- pred create_sample_2(int::in, int::in, int::in, int::in,
    maybe(sample)::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_sample_2(Bits::in, IsStereo::in, Freq::in, Len::in,
        MaybeSample::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    SAMPLE *Sample = create_sample(Bits, IsStereo, Freq, Len);
    if (Sample) {
        MaybeSample = _mal_make_yes_sample(Sample);
    } else {
        MaybeSample = _mal_make_no_sample();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    destroy_sample(Sample::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    destroy_sample(Sample);
    IO = IO0;
").

play_sample(Sample, Vol, Pan, Freq, Loop, !IO) :-
    loop(Loop, LoopInt),
    play_sample_2(Sample, Vol, Pan, Freq, LoopInt, !IO).

:- pred play_sample_2(sample::in, int::in, int::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    play_sample_2(Sample::in, Vol::in, Pan::in, Freq::in, Loop::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    play_sample(Sample, Vol, Pan, Freq, Loop);
    IO = IO0;
").

adjust_sample(Sample, Vol, Pan, Freq, Loop, !IO) :-
    loop(Loop, LoopInt),
    adjust_sample_2(Sample, Vol, Pan, Freq, LoopInt, !IO).

:- pred adjust_sample_2(sample::in, int::in, int::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    adjust_sample_2(Sample::in, Vol::in, Pan::in, Freq::in, Loop::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    adjust_sample(Sample, Vol, Pan, Freq, Loop);
    IO = IO0;
").

:- pragma foreign_proc("C",
    stop_sample(Sample::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    stop_sample(Sample);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- type voice == int.

:- pragma foreign_proc("C",
    allocate_voice(Sample::in, MaybeVoice::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    int Voice = allocate_voice(Sample);
    if (Voice == -1) {
        MaybeVoice = _mal_make_no_int();
    } else {
        MaybeVoice = _mal_make_yes_int(Voice);
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    deallocate_voice(Voice::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    deallocate_voice(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    reallocate_voice(Voice::in, Sample::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    reallocate_voice(Voice, Sample);
    IO = IO0;
").

:- pragma foreign_proc("C",
    release_voice(Voice::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    release_voice(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_start(Voice::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_start(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_stop(Voice::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_stop(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_set_priority(Voice::in, Priority::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_set_priority(Voice, Priority);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_check(Voice::in, MaybeSample::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    SAMPLE *Sample = voice_check(Voice);
    if (Sample) {
        MaybeSample = _mal_make_yes_sample(Sample);
    } else {
        MaybeSample = _mal_make_no_sample();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_get_position(Voice::in, MaybePos::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    int Pos = voice_get_position(Voice);
    if (Pos == -1) {
        MaybePos = _mal_make_yes_int(Pos);
    } else {
        MaybePos = _mal_make_no_int();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_set_position(Voice::in, Pos::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_set_position(Voice, Pos);
    IO = IO0;
").

voice_set_playmode(Voice, Playmode, !IO) :-
    voice_set_playmode_2(Voice, playmode_to_int(Playmode), !IO).

:- pred voice_set_playmode_2(voice::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    voice_set_playmode_2(Voice::in, Playmode::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_set_playmode(Voice, Playmode);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_get_volume(Voice::in, MaybeVolume::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    int Volume = voice_get_volume(Voice);
    if (Volume == -1) {
        MaybeVolume = _mal_make_yes_int(Volume);
    } else {
        MaybeVolume = _mal_make_no_int();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_set_volume(Voice::in, Volume::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_set_volume(Voice, Volume);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_ramp_volume(Voice::in, Time::in, Endvol::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_ramp_volume(Voice, Time, Endvol);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_stop_volumeramp(Voice::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_stop_volumeramp(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_get_frequency(Voice::in, Freq::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Freq = voice_get_frequency(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_set_frequency(Voice::in, Freq::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_set_frequency(Voice, Freq);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_sweep_frequency(Voice::in, Time::in, Endfreq::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_sweep_frequency(Voice, Time, Endfreq);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_stop_frequency_sweep(Voice::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_stop_frequency_sweep(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_get_pan(Voice::in, Pan::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Pan = voice_get_pan(Voice);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_set_pan(Voice::in, Pan::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_set_pan(Voice, Pan);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_sweep_pan(Voice::in, Time::in, Endpan::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_sweep_pan(Voice, Time, Endpan);
    IO = IO0;
").

:- pragma foreign_proc("C",
    voice_stop_pan_sweep(Voice::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    voice_stop_pan_sweep(Voice);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pred bits(bits, int).
:- mode bits(in, out) is det.
:- mode bits(out, in(bound(8 ; 16))) is det.

bits(bits_8, 8).
bits(bits_16, 16).

:- pred channels_is_stereo_int(channels, int).
:- mode channels_is_stereo_int(in, out) is det.
:- mode channels_is_stereo_int(out, in(bound(0 ; 1))) is det.

channels_is_stereo_int(mono, 0).
channels_is_stereo_int(stereo, 1).

:- pred loop(loop, int).
:- mode loop(in, out) is det.
:- mode loop(out, in(bound(0 ; 1))) is det.

loop(no_loop, 0).
loop(loop, 1).

:- func playmode_to_int(playmode) = int.

playmode_to_int(playmode_play) = playmode_play_int.
playmode_to_int(playmode_loop) = playmode_loop_int.
playmode_to_int(playmode_forward) = playmode_forward_int.
playmode_to_int(playmode_backward) = playmode_backward_int.
playmode_to_int(playmode_bidir) = playmode_bidir_int.

:- func playmode_play_int = int.
:- func playmode_loop_int = int.
:- func playmode_forward_int = int.
:- func playmode_backward_int = int.
:- func playmode_bidir_int = int.

:- pragma foreign_proc("C", playmode_play_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = PLAYMODE_PLAY;").
:- pragma foreign_proc("C", playmode_loop_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = PLAYMODE_LOOP;").
:- pragma foreign_proc("C", playmode_forward_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = PLAYMODE_FORWARD;").
:- pragma foreign_proc("C", playmode_backward_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = PLAYMODE_BACKWARD;").
:- pragma foreign_proc("C", playmode_bidir_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = PLAYMODE_BIDIR;").

%-----------------------------------------------------------------------------%

Sample ^ bits = Bits :-
    sample_bits_int(Sample, BitsInt),
    bits(Bits, BitsInt).

:- pred sample_bits_int(sample::in, int::out(bound(8 ; 16))) is det.

:- pragma foreign_proc("C",
    sample_bits_int(Sample::in, BitsInt::out(bound(8 ; 16))),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BitsInt = Sample->bits;
").

Sample ^ channels = Channels :-
    sample_is_stereo_int(Sample, IsStereoInt),
    channels_is_stereo_int(Channels, IsStereoInt).

:- pred sample_is_stereo_int(sample::in, int::out(bound(0 ; 1))) is det.

:- pragma foreign_proc("C",
    sample_is_stereo_int(Sample::in, IsStereoInt::out(bound(0 ; 1))),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    IsStereoInt = Sample->stereo ? 1 : 0;
").

Sample ^ freq = Freq :-
    sample_freq(Sample, Freq).

:- pred sample_freq(sample::in, freq::out) is det.

:- pragma foreign_proc("C",
    sample_freq(Sample::in, Freq::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Freq = Sample->freq;
").

Sample ^ priority = Priority :-
    sample_priority(Sample, Priority).

:- pred sample_priority(sample::in, priority::out) is det.

:- pragma foreign_proc("C",
    sample_priority(Sample::in, Priority::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Priority = Sample->priority;
").

Sample ^ len = Len :-
    sample_len(Sample, Len).

:- pred sample_len(sample::in, len::out) is det.

:- pragma foreign_proc("C",
    sample_len(Sample::in, Len::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Len = Sample->len;
").

:- pragma foreign_proc("C",
    get_sample(Sample::in, Pos::in, Value::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ASSERT(Pos < Sample->len);
    if (Sample->bits == 8) {
        int8_t *p = Sample->data;
        Value = p[Pos];
    } else {
        int16_t *p = Sample->data;
        Value = p[Pos];
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_sample(Sample::in, Pos::in, Value::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ASSERT(Pos < Sample->len);
    if (Sample->bits == 8) {
        int8_t *p = Sample->data;
        p[Pos] = Value;
    } else {
        int16_t *p = Sample->data;
        p[Pos] = Value;
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func make_yes_sample(sample) = maybe(sample).
:- func make_no_sample = maybe(sample).

:- pragma export(make_yes_sample(in) = out, "_mal_make_yes_sample").
:- pragma export(make_no_sample = out, "_mal_make_no_sample").

make_yes_sample(X) = yes(X).
make_no_sample = no.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
