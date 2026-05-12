%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test calendar.init_duration/7 and calendar.unpack_duration/8.
%
% The tests in this module check that:
%
% 1. init_duration/7 accepts inputs with consistent signs (all non-zero
%    components positive, or all non-zero components negative, or all
%    components zero).
%
% 2. init_duration/7 throws an exception for inputs with mixed signs.
%
% 3. unpack_duration/8 returns the components after normalisation.
%
% 4. The documented normalisation examples produce the documented
%    results (e.g. init_duration(1, 18, 0, 0, 0, 0, 0) yields two years
%    and 6 months).
%
% 5. zero_duration/0 round-trips through unpack_duration/8 as
%    all-zero components.
%
% 6. negate/1 applied twice is the identity (negate(negate(D)) = D)
%    and zero_duration is fixed under negation.
%
%---------------------------------------------------------------------------%

:- module calendar_init_duration.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module calendar.
:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_already_normalised(!IO),
    test_normalisation(!IO),
    test_zero_duration(!IO),
    test_mixed_signs(!IO),
    test_negate(!IO).

%---------------------------------------------------------------------------%
%
% A "normalisation case" describes both the inputs to init_duration/7
% and the expected outputs of unpack_duration/8. For inputs that are
% already in normal form, the two sets of values are equal.

:- type norm_case
    --->    norm_case(
                description     :: string,

                % Inputs to init_duration/7.
                in_years        :: int,
                in_months       :: int,
                in_days         :: int,
                in_hours        :: int,
                in_minutes      :: int,
                in_seconds      :: int,
                in_microseconds :: int,

                % Expected outputs of unpack_duration/8.
                out_years        :: int,
                out_months       :: int,
                out_days         :: int,
                out_hours        :: int,
                out_minutes      :: int,
                out_seconds      :: int,
                out_microseconds :: int
            ).

%---------------------------------------------------------------------------%
%
% Cases where the inputs are already in normal form, so the unpacked
% components equal the input components. These check that:
% - small, well-formed positive and negative inputs construct without
%   error;
% - unpack_duration/8 returns the canonical representation;
% - the per-component invariants from the duration/0 type comment hold.

:- pred test_already_normalised(io::di, io::uo) is det.

test_already_normalised(!IO) :-
    io.write_string(
        "=== Test init_duration/7 with already-normalised inputs ===\n\n",
        !IO),
    list.foldl(do_test_norm_case, already_normalised_cases, !IO),
    io.nl(!IO).

:- func already_normalised_cases = list(norm_case).

already_normalised_cases = [
    % All-zero is the simplest case.
    norm_case("all zero",
        0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0),

    % Single non-zero positive components.
    norm_case("years only",
        1, 0, 0, 0, 0, 0, 0,
        1, 0, 0, 0, 0, 0, 0),
    norm_case("months only",
        0, 1, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0),
    norm_case("days only",
        0, 0, 1, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 0),
    norm_case("hours only",
        0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 0),
    norm_case("minutes only",
        0, 0, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 1, 0, 0),
    norm_case("seconds only",
        0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 1, 0),
    norm_case("microseconds only",
        0, 0, 0, 0, 0, 0, 1,
        0, 0, 0, 0, 0, 0, 1),

    % Single non-zero negative components.
    norm_case("negative years only",
        -1, 0, 0, 0, 0, 0, 0,
        -1, 0, 0, 0, 0, 0, 0),
    norm_case("negative months only",
        0, -1, 0, 0, 0, 0, 0,
        0, -1, 0, 0, 0, 0, 0),
    norm_case("negative days only",
        0, 0, -1, 0, 0, 0, 0,
        0, 0, -1, 0, 0, 0, 0),
    norm_case("negative microseconds only",
        0, 0, 0, 0, 0, 0, -1,
        0, 0, 0, 0, 0, 0, -1),

    % Maximum per-component values for a positive duration that is
    % already in normal form.
    norm_case("max positive components in normal form",
        1, 11, 365, 23, 59, 59, 999_999,
        1, 11, 365, 23, 59, 59, 999_999),

    % Maximum per-component values for a negative duration in normal form.
    norm_case("max negative components in normal form",
        -1, -11, -365, -23, -59, -59, -999_999,
        -1, -11, -365, -23, -59, -59, -999_999),

    % Mixed positive components (all positive, none negative).
    norm_case("all positive components in normal form",
        1, 2, 3, 4, 5, 6, 7,
        1, 2, 3, 4, 5, 6, 7),

    % Mixed negative components (all negative, none positive).
    norm_case("all negative components in normal form",
        -1, -2, -3, -4, -5, -6, -7,
        -1, -2, -3, -4, -5, -6, -7)
].

%---------------------------------------------------------------------------%
%
% Cases where normalisation is required. Each case states what
% unpack_duration/8 should return after the input has been
% normalised according to the rules in the documentation for the
% duration/0 type.
%

:- pred test_normalisation(io::di, io::uo) is det.

test_normalisation(!IO) :-
    io.write_string(
        "=== Test init_duration/7 normalisation ===\n\n", !IO),
    list.foldl(do_test_norm_case, normalisation_cases, !IO),
    io.nl(!IO).

:- func normalisation_cases = list(norm_case).

normalisation_cases = [
    % Documented examples from the duration/0 type comment.
    %   init_duration(1, 18, 0, 0, 0, 0, 0) => years = 2, months = 6
    norm_case("doc example: 1Y 18M -> 2Y 6M",
        1, 18, 0, 0, 0, 0, 0,
        2, 6, 0, 0, 0, 0, 0),

    %   init_duration(0, 0, 0, 25, 0, 0, 0) => days = 1, hours = 1
    norm_case("doc example: 25H -> 1D 1H",
        0, 0, 0, 25, 0, 0, 0,
        0, 0, 1, 1, 0, 0, 0),

    % Months overflow into years.
    norm_case("12 months -> 1 year",
        0, 12, 0, 0, 0, 0, 0,
        1, 0, 0, 0, 0, 0, 0),
    norm_case("13 months -> 1 year 1 month",
        0, 13, 0, 0, 0, 0, 0,
        1, 1, 0, 0, 0, 0, 0),
    norm_case("years and months combine",
        2, 6, 0, 0, 0, 0, 0,
        2, 6, 0, 0, 0, 0, 0),
    norm_case("years and months combine and overflow",
        2, 18, 0, 0, 0, 0, 0,
        3, 6, 0, 0, 0, 0, 0),

    % Seconds overflow into minutes, hours, days.
    norm_case("60 seconds -> 1 minute",
        0, 0, 0, 0, 0, 60, 0,
        0, 0, 0, 0, 1, 0, 0),
    norm_case("3600 seconds -> 1 hour",
        0, 0, 0, 0, 0, 3600, 0,
        0, 0, 0, 1, 0, 0, 0),
    norm_case("86400 seconds -> 1 day",
        0, 0, 0, 0, 0, 86_400, 0,
        0, 0, 1, 0, 0, 0, 0),
    norm_case("60 minutes -> 1 hour",
        0, 0, 0, 0, 60, 0, 0,
        0, 0, 0, 1, 0, 0, 0),
    norm_case("24 hours -> 1 day",
        0, 0, 0, 24, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 0),

    % Microseconds carry into seconds.
    norm_case("1_000_000 microseconds -> 1 second",
        0, 0, 0, 0, 0, 0, 1_000_000,
        0, 0, 0, 0, 0, 1, 0),
    norm_case("1_500_000 microseconds -> 1 second 500_000 us",
        0, 0, 0, 0, 0, 0, 1_500_000,
        0, 0, 0, 0, 0, 1, 500_000),

    % Microseconds carry that crosses multiple boundaries: 1_000_000 us
    % + 59 seconds = 60 seconds = 1 minute.
    norm_case("microseconds carry into minute",
        0, 0, 0, 0, 0, 59, 1_000_000,
        0, 0, 0, 0, 1, 0, 0),

    % Days are NOT folded into months.
    norm_case("31 days stay as days",
        0, 0, 31, 0, 0, 0, 0,
        0, 0, 31, 0, 0, 0, 0),
    norm_case("365 days stay as days",
        0, 0, 365, 0, 0, 0, 0,
        0, 0, 365, 0, 0, 0, 0),

    % Days from the days component plus days carried from seconds combine.
    norm_case("1 day + 24 hours -> 2 days",
        0, 0, 1, 24, 0, 0, 0,
        0, 0, 2, 0, 0, 0, 0),

    % Cascading carry across every time boundary.
    norm_case("cascading carry: 23H 59M 59S 1_000_000us -> 1 day",
        0, 0, 0, 23, 59, 59, 1_000_000,
        0, 0, 1, 0, 0, 0, 0),

    % Negative-duration versions of the above.
    norm_case("negative doc example: -1Y -18M -> -2Y -6M",
        -1, -18, 0, 0, 0, 0, 0,
        -2, -6, 0, 0, 0, 0, 0),
    norm_case("negative doc example: -25H -> -1D -1H",
        0, 0, 0, -25, 0, 0, 0,
        0, 0, -1, -1, 0, 0, 0),
    norm_case("negative 12 months -> -1 year",
        0, -12, 0, 0, 0, 0, 0,
        -1, 0, 0, 0, 0, 0, 0),
    norm_case("negative 60 seconds -> -1 minute",
        0, 0, 0, 0, 0, -60, 0,
        0, 0, 0, 0, -1, 0, 0),
    norm_case("negative 1_000_000 us -> -1 second",
        0, 0, 0, 0, 0, 0, -1_000_000,
        0, 0, 0, 0, 0, -1, 0),

    % Overflow for negative durations.
    norm_case("negative 1_500_000 us -> -1 second -500_000 us",
        0, 0, 0, 0, 0, 0, -1_500_000,
        0, 0, 0, 0, 0, -1, -500_000),
    norm_case("negative microseconds carry into minute",
        0, 0, 0, 0, 0, -59, -1_000_000,
        0, 0, 0, 0, -1, 0, 0),
    norm_case("negative 60 minutes -> -1 hour",
        0, 0, 0, 0, -60, 0, 0,
        0, 0, 0, -1, 0, 0, 0),
    norm_case("negative 3600 seconds -> -1 hour",
        0, 0, 0, 0, 0, -3600, 0,
        0, 0, 0, -1, 0, 0, 0),
    norm_case("negative 24 hours -> -1 day",
        0, 0, 0, -24, 0, 0, 0,
        0, 0, -1, 0, 0, 0, 0),
    norm_case("negative 86400 seconds -> -1 day",
        0, 0, 0, 0, 0, -86_400, 0,
        0, 0, -1, 0, 0, 0, 0),
    norm_case("negative 1 day + -24 hours -> -2 days",
        0, 0, -1, -24, 0, 0, 0,
        0, 0, -2, 0, 0, 0, 0),
    norm_case("negative 13 months -> -1 year -1 month",
        0, -13, 0, 0, 0, 0, 0,
        -1, -1, 0, 0, 0, 0, 0),
    norm_case("negative years and months combine and overflow",
        -2, -18, 0, 0, 0, 0, 0,
        -3, -6, 0, 0, 0, 0, 0),

    norm_case("negative cascading carry: -23H -59M -59S -1_000_000us -> -1 day",
        0, 0, 0, -23, -59, -59, -1_000_000,
        0, 0, -1, 0, 0, 0, 0),

    % Example from documentation for duration_from_string/2.
    norm_case(
        "doc example: P1Y18M100DT10H15M90.0003S -> P2Y6M100DT10H16M30.0003S",
        1, 18, 100, 10, 15, 90, 300,
        2, 6, 100, 10, 16, 30, 300)
].

%---------------------------------------------------------------------------%

:- pred do_test_norm_case(norm_case::in, io::di, io::uo) is det.

do_test_norm_case(Case, !IO) :-
    Case = norm_case(Desc,
        InY, InMo, InD, InH, InMi, InS, InUs,
        ExpY, ExpMo, ExpD, ExpH, ExpMi, ExpS, ExpUs),
    Duration = init_duration(InY, InMo, InD, InH, InMi, InS, InUs),
    unpack_duration(Duration, ActY, ActMo, ActD, ActH, ActMi, ActS, ActUs),
    ( if
        ActY = ExpY, ActMo = ExpMo, ActD = ExpD,
        ActH = ExpH, ActMi = ExpMi, ActS = ExpS, ActUs = ExpUs
    then
        io.format("PASS: %s\n", [s(Desc)], !IO)
    else
        io.format("FAIL: %s\n", [s(Desc)], !IO),
        io.format("    input:    Y=%d Mo=%d D=%d H=%d Mi=%d S=%d Us=%d\n",
            [i(InY), i(InMo), i(InD), i(InH), i(InMi), i(InS), i(InUs)],
            !IO),
        io.format("    expected: Y=%d Mo=%d D=%d H=%d Mi=%d S=%d Us=%d\n",
            [i(ExpY), i(ExpMo), i(ExpD), i(ExpH), i(ExpMi), i(ExpS), i(ExpUs)],
            !IO),
        io.format("    actual:   Y=%d Mo=%d D=%d H=%d Mi=%d S=%d Us=%d\n",
            [i(ActY), i(ActMo), i(ActD), i(ActH), i(ActMi), i(ActS), i(ActUs)],
            !IO)
    ).

%---------------------------------------------------------------------------%
%
% zero_duration/0 should unpack as all-zero components, and should be equal
% (after unpacking) to the result of init_duration(0, 0, 0, 0, 0, 0, 0).
%

:- pred test_zero_duration(io::di, io::uo) is det.

test_zero_duration(!IO) :-
    io.write_string("=== Test zero_duration/0 ===\n\n", !IO),
    Zero = zero_duration,
    unpack_duration(Zero, Y, Mo, D, H, Mi, S, Us),
    ( if Y = 0, Mo = 0, D = 0, H = 0, Mi = 0, S = 0, Us = 0 then
        io.write_string("PASS: zero_duration unpacks to all zeros\n", !IO)
    else
        io.format("FAIL: zero_duration unpacks to (%d,%d,%d,%d,%d,%d,%d)\n",
            [i(Y), i(Mo), i(D), i(H), i(Mi), i(S), i(Us)], !IO)
    ),

    % Should be indistinguishable from init_duration(0, 0, 0, 0, 0, 0, 0).
    ZeroFromInit = init_duration(0, 0, 0, 0, 0, 0, 0),
    ( if Zero = ZeroFromInit then
        io.write_string(
            "PASS: zero_duration = init_duration(0, 0, 0, 0, 0, 0, 0)\n",
            !IO)
    else
        io.write_string(
            "FAIL: zero_duration \\= init_duration(0, 0, 0, 0, 0, 0, 0)\n",
            !IO)
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%
%
% init_duration/7 should throw an exception when non-zero components have mixed
% signs. These cases include:
% - the two examples from the documentation comment for the duration/0 type;
% - one mixed-sign pair for each pair of components.

:- type mixed_sign_case
    --->    mixed_sign_case(
                ms_description  :: string,
                ms_years        :: int,
                ms_months       :: int,
                ms_days         :: int,
                ms_hours        :: int,
                ms_minutes      :: int,
                ms_seconds      :: int,
                ms_microseconds :: int
            ).

:- pred test_mixed_signs(io::di, io::uo) is cc_multi.

test_mixed_signs(!IO) :-
    io.write_string(
        "=== Test init_duration/7 with mixed signs ===\n\n", !IO),
    list.foldl(do_test_mixed_sign, mixed_sign_cases, !IO),
    io.nl(!IO).

:- pred do_test_mixed_sign(mixed_sign_case::in, io::di, io::uo) is cc_multi.

do_test_mixed_sign(Case, !IO) :-
    Case = mixed_sign_case(Desc, Y, Mo, D, H, Mi, S, Us),
    ( try []
        Duration = init_duration(Y, Mo, D, H, Mi, S, Us)
    then
        use_duration(Duration, !IO),
        io.format("FAIL: init_duration did not throw for (%s)\n",
            [s(Desc)], !IO)
    catch_any _ ->
        io.format("PASS: init_duration threw exception for (%s)\n",
            [s(Desc)], !IO)
    ).

:- pragma no_inline(pred(use_duration/3)).
:- pred use_duration(duration::in, io::di, io::uo) is det.

use_duration(_, !IO).

:- func mixed_sign_cases = list(mixed_sign_case).

mixed_sign_cases = [
    % Documented examples from the duration/0 type comment.
    mixed_sign_case("doc example: 1 month, -5 days",
        0, 1, -5, 0, 0, 0, 0),
    mixed_sign_case("doc example: 2 hours, -30 minutes",
        0, 0, 0, 2, -30, 0, 0),

    % Each adjacent pair of components disagrees on sign.
    mixed_sign_case("positive years, negative months",
        1, -1, 0, 0, 0, 0, 0),
    mixed_sign_case("negative years, positive months",
        -1, 1, 0, 0, 0, 0, 0),
    mixed_sign_case("positive months, negative days",
        0, 1, -1, 0, 0, 0, 0),
    mixed_sign_case("positive days, negative hours",
        0, 0, 1, -1, 0, 0, 0),
    mixed_sign_case("positive hours, negative minutes",
        0, 0, 0, 1, -1, 0, 0),
    mixed_sign_case("positive minutes, negative seconds",
        0, 0, 0, 0, 1, -1, 0),
    mixed_sign_case("positive seconds, negative microseconds",
        0, 0, 0, 0, 0, 1, -1),
    mixed_sign_case("negative seconds, positive microseconds",
        0, 0, 0, 0, 0, -1, 1),

    % Non-adjacent pairs.
    mixed_sign_case("positive years, negative microseconds",
        1, 0, 0, 0, 0, 0, -1),
    mixed_sign_case("negative years, positive microseconds",
        -1, 0, 0, 0, 0, 0, 1),
    mixed_sign_case("positive years, negative seconds",
        1, 0, 0, 0, 0, -1, 0),

    % Mostly-zero with a single mixed pair.
    mixed_sign_case("one positive, one negative, rest zero",
        0, 0, 1, 0, 0, 0, -1)
].

%---------------------------------------------------------------------------%

:- pred test_negate(io::di, io::uo) is det.

test_negate(!IO) :-
    io.write_string("=== Test negate/1 ===\n\n", !IO),

    % negate(zero_duration) = zero_duration.
    NegZero = negate(zero_duration),
    ( if NegZero = zero_duration then
        io.write_string("PASS: negate(zero_duration) = zero_duration\n", !IO)
    else
        io.format("FAIL: negate(zero_duration) = %s\n", [s(string(NegZero))],
            !IO)
    ),

    % negate(negate(D)) = D.
    list.foldl(do_test_double_negate, double_negate_cases, !IO),
    io.nl(!IO).

:- pred do_test_double_negate(norm_case::in, io::di, io::uo) is det.

do_test_double_negate(Case, !IO) :-
    Case = norm_case(_Desc, InY, InMo, InD, InH, InMi, InS, InUs,
        _, _, _, _, _, _, _),
    D = init_duration(InY, InMo, InD, InH, InMi, InS, InUs),
    DoubleNegated = negate(negate(D)),
    ( if D = DoubleNegated then
        io.format("PASS: negate(negate(%s)) = %s\n",
            [s(string(D)), s(string(DoubleNegated))], !IO)
    else
        io.format("FAIL: negate(negate(%s)) = %s\n",
            [s(string(D)), s(string(DoubleNegated))], !IO)
    ).

:- func double_negate_cases = list(norm_case).

double_negate_cases = [
    norm_case("zero", 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0),
    norm_case("positive small", 1, 2, 3, 4, 5, 6, 7,  0, 0, 0, 0, 0, 0, 0),
    norm_case("negative small",
        -1, -2, -3, -4, -5, -6, -7,  0, 0, 0, 0, 0, 0, 0),
    norm_case("positive needing normalisation",
        1, 18, 100, 10, 15, 90, 300,  0, 0, 0, 0, 0, 0, 0),
    norm_case("negative needing normalisation",
        -1, -18, -100, -10, -15, -90, -300,  0, 0, 0, 0, 0, 0, 0)
].

%---------------------------------------------------------------------------%
:- end_module calendar_init_duration.
%---------------------------------------------------------------------------%
