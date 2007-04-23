%-----------------------------------------------------------------------------%

:- module sound.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type sounds.

:- pred generate_samples(io::di, io::uo) is det.
:- pred destroy_samples(io::di, io::uo) is det.
:- func init_sounds = sounds.
:- pred stop_sounds(sounds::in, io::di, io::uo) is det.
:- pred sfx_shoot(sounds::in, sounds::out) is det.
:- pred sfx_explode_alien(sounds::in, sounds::out) is det.
:- pred sfx_explode_block(sounds::in, sounds::out) is det.
:- pred sfx_explode_player(sounds::in, sounds::out) is det.
:- pred sfx_ping(int::in, sounds::in, sounds::out) is det.
:- pred play_queued_sfx(sounds::in, sounds::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.digi.
:- import_module allegro.timer.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Pregenerated samples
    %
:- type pregen
    --->    pregen(
                zap     :: maybe(sample),
                bang    :: maybe(sample),
                bigbang :: maybe(sample),
                ping    :: maybe(sample)
            ).

    % List of queued sound effects to play, plus the state of the ping sounds.
    %
:- type sounds
    --->    sounds(
                list(queued_sound),
                ping_state
            ).

:- type queued_sound
    --->    sfx(sfx_type, vol, pan, freq)
    ;       ping(int).

:- type sfx_type
    --->    zap
    ;       bang
    ;       bigbang.

:- type ping_state
    --->    not_pinging
    ;       pinging(ticker, vol, freq, count :: int).

%-----------------------------------------------------------------------------%

generate_samples(!IO) :-
    create_zap(Zap, !IO),
    create_bang(Bang, !IO),
    create_big_bang(BigBang, !IO),
    create_ping(Ping, !IO),
    Pregen = pregen(Zap, Bang, BigBang, Ping),
    set_pregen(Pregen, !IO).

destroy_samples(!IO) :-
    get_pregen(Pregen, !IO),
    Pregen = pregen(Zap, Bang, BigBang, Ping),
    maybe_destroy_sample(Zap, !IO),
    maybe_destroy_sample(Bang, !IO),
    maybe_destroy_sample(BigBang, !IO),
    maybe_destroy_sample(Ping, !IO),
    set_pregen(pregen(no, no, no, no), !IO).

:- pred create_zap(maybe(sample)::out, io::di, io::uo) is det.
:- pred create_zap_2(sample::in, int::in, len::in,
        {float,float,float,float}::in,
        {float,float,float,float}::out, io::di, io::uo) is det.

create_zap(MaybeZap, !IO) :-
    Len = 8192,
    create_sample(bits_8, mono, 22050, Len, MaybeZap, !IO),
    (
        MaybeZap = yes(Zap),
        int.fold_up2(create_zap_2(Zap, Len), 0, Len-1,
            {0.0, 0.0, 0.02, 0.025}, _, !IO)
    ;
        MaybeZap = no
    ).

    % zap (firing sound) consists of multiple falling saw waves
create_zap_2(Zap, Len, I, {OscA, OscB, FreqA, FreqB},
        {OscA+FreqA, OscB+FreqB, FreqA-0.000001, FreqB-0.00000125}, !IO):-
    Vol = 127.0 * float(Len - I) / float(Len),
    P = 128.0 + (fmod(OscA, 1.0) + fmod(OscB, 1.0) - 1.0) * Vol,
    set_sample(Zap, I, truncate_to_int(P), !IO).

:- pred create_bang(maybe(sample)::out, io::di, io::uo) is det.
:- pred create_bang_2(sample::in, len::in, pos::in, float::in, float::out,
        io::di, io::uo) is det.

create_bang(MaybeBang, !IO) :-
    Len = 8192,
    create_sample(bits_8, mono, 22050, Len, MaybeBang, !IO),
    (
        MaybeBang = yes(Bang),
        int.fold_up2(create_bang_2(Bang, Len), 0, Len-1, 0.0, _Val, !IO)
    ;
        MaybeBang = no
    ).

    % bang (explosion) consists of filtered noise
create_bang_2(Bang, Len, I, Val0, Val, !IO) :-
    randf(Rand, !IO),
    Vol = 255.0 * (float(Len - I) / float(Len)),
    Val = (Val0 * 0.75) + (Rand * 0.25),
    P = 128.0 + Val * Vol,
    set_sample(Bang, I, truncate_to_int(P), !IO).

:- pred create_big_bang(maybe(sample)::out, io::di, io::uo) is det.
:- pred create_big_bang_2(sample::in, len::in, pos::in, float::in, float::out,
        io::di, io::uo) is det.

create_big_bang(MaybeBigBang, !IO) :-
    Len = 24576,
    create_sample(bits_8, mono, 11025, Len, MaybeBigBang, !IO),
    (
        MaybeBigBang = yes(BigBang),
        int.fold_up2(create_big_bang_2(BigBang, Len), 0, Len-1,
            0.0, _Val, !IO)
    ;
        MaybeBigBang = no
    ).

    % big bang (explosion) consists of noise plus rumble
create_big_bang_2(BigBang, Len, I, Val0, Val, !IO) :-
    randf(Rand, !IO),
    Vol  = 128.0 * float(Len - I) / float(Len),
    F    = 0.5 + 0.4 * (float(I) / float(Len)),
    Val  = (Val0 * F) + (Rand * (1.0 - F)),
    Osc1 = float(I) * 0.03,
    Osc2 = float(I) * 0.04,
    P    = 128.0 + (Val + (sin(Osc1) + sin(Osc2)) / 4.0) * Vol,
    set_sample(BigBang, I, truncate_to_int(P), !IO).

:- pred create_ping(maybe(sample)::out, io::di, io::uo) is det.
:- pred create_ping_2(sample::in, len::in, pos::in, io::di, io::uo) is det.

create_ping(MaybePing, !IO) :-
    Len = 8192,
    create_sample(bits_8, mono, 22050, Len, MaybePing, !IO),
    (
        MaybePing = yes(Ping),
        int.fold_up(create_ping_2(Ping, Len), 0, Len-1, !IO)
    ;
        MaybePing = no
    ).

    % ping consists of two sine waves
create_ping_2(Ping, Len, I, !IO) :-
    Vol  = 31.0 * float(Len - I) / float(Len),
    Osc1 = float(I) * 0.2,
    Osc2 = float(I) * 0.3,
    P    = 128.0 + (sin(Osc1) + sin(Osc2) - 1.0) * Vol,
    set_sample(Ping, I, truncate_to_int(P), !IO).

:- pred randf(float::out, io::di, io::uo) is det.
% randf(F, !IO) :-
%     rand.random(I, !IO),
%     F = (float(I /\ 255) / 255.0) - 0.5.

    % It just doesn't sound the same using the `random' RNG.
    %
:- pragma foreign_proc("C",
    randf(F::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    F = ((float)(rand() & 255) / 255.0) - 0.5;
    IO = IO0;
").

:- func fmod(float, float) = float.

fmod(X, Y) = X - float(N) * Y :-
    N = floor_to_int(X / Y).

%-----------------------------------------------------------------------------%

:- mutable(pregen, pregen, pregen(no, no, no, no), ground,
    [attach_to_io_state, untrailed]).

%-----------------------------------------------------------------------------%

init_sounds = sounds([], not_pinging).

stop_sounds(sounds(_, Pings), !IO) :-
    stop_ping_state(Pings, !IO).

sfx_shoot(Sounds, sched(sfx(zap, 64, 128, 1000), Sounds)).
sfx_explode_alien(Sounds, sched(sfx(bang, 192, 128, 1000), Sounds)).
sfx_explode_block(Sounds, sched(sfx(bang, 224, 128, 400), Sounds)).
sfx_explode_player(Sounds, sched(sfx(bigbang, 255, 128, 1000), Sounds)).
sfx_ping(Times, Sounds, sched(ping(Times), Sounds)).

:- func sched(queued_sound, sounds) = sounds.
sched(Sfx, sounds(Queue, Pings)) = sounds([Sfx | Queue], Pings).

play_queued_sfx(sounds(Queue, Pings0), sounds([], Pings), !IO) :-
    list.foldl2(play_queued_sfx_2, Queue, Pings0, Pings1, !IO),
    update_ping_state(Pings1, Pings, !IO).

:- pred play_queued_sfx_2(queued_sound::in, ping_state::in, ping_state::out,
        io::di, io::uo) is det.

play_queued_sfx_2(sfx(Type, Vol, Pan, Freq), !Pings, !IO) :-
    get_pregen(Pregen, !IO),
    (
        Type = zap,
        MaybeSample = Pregen ^ zap
    ;
        Type = bang,
        MaybeSample = Pregen ^ bang
    ;
        Type = bigbang,
        MaybeSample = Pregen ^ bigbang
    ),
    (
        MaybeSample = yes(Sample),
        play_sample(Sample, Vol, Pan, Freq, no_loop, !IO)
    ;
        MaybeSample = no
    ).

play_queued_sfx_2(ping(Times), Pings0, Pings, !IO) :-
    get_pregen(Pregen, !IO),
    MaybeSample = Pregen ^ ping,
    (if Times = 0 then
        maybe_play_sample(MaybeSample, 255, 128, 500, no_loop, !IO),
        Pings = Pings0
    else 
        stop_ping_state(Pings0, !IO),
        (if Times > 1 then
            Vol = 255,
            Freq = 500
        else
            Vol = 128,
            Freq = 1000
        ),
        install_int(300, MaybeTicker, !IO),
        (
            MaybeTicker = yes(Ticker),
            maybe_play_sample(MaybeSample, Vol, 128, Freq, no_loop, !IO),
            Pings = pinging(Ticker, Vol, Freq, Times)
        ;
            MaybeTicker = no,
            Pings = not_pinging
        )
    ).

:- pred update_ping_state(ping_state::in, ping_state::out, io::di, io::uo)
        is det.

update_ping_state(not_pinging, not_pinging, !IO).
update_ping_state(Pings0, Pings, !IO) :-
    Pings0 = pinging(Ticker, Vol, Freq0, Times0),
    get_ticker(Ticker, Ticks, !IO),
    (if Ticks = 0 then
        Pings = Pings0
    else
        Freq = Freq0 * 4/3,
        get_pregen(Pregen, !IO),
        MaybeSample = Pregen ^ ping,
        maybe_play_sample(MaybeSample, Vol, 128, Freq, no_loop, !IO),
        Times = Times0 - 1,
        (if Times > 0 then
            set_ticker(Ticker, 0, !IO),
            Pings = pinging(Ticker, Vol, Freq, Times)
        else
            remove_int(Ticker, !IO),
            Pings = not_pinging
        )
    ).

:- pred stop_ping_state(ping_state::in, io::di, io::uo) is det.

stop_ping_state(not_pinging, !IO).
stop_ping_state(pinging(Ticker, _Vol, _Freq, _Times), !IO) :-
    remove_int(Ticker, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_play_sample(maybe(sample)::in, vol::in, pan::in, freq::in,
        loop::in, io::di, io::uo) is det.

maybe_play_sample(no, _, _, _, _, !IO).
maybe_play_sample(yes(Sample), Vol, Pan, Freq, Loop, !IO) :-
    play_sample(Sample, Vol, Pan, Freq, Loop, !IO).

:- pred maybe_destroy_sample(maybe(sample)::in, io::di, io::uo) is det.

maybe_destroy_sample(no, !IO).
maybe_destroy_sample(yes(Sample), !IO) :-
    destroy_sample(Sample, !IO).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
