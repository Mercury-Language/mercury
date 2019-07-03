%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: measurement_units.m.
% Author: pbone.
%
% This module contains abstract data types for representing memory, time
% and percentages, and predicates and functions for operating on them.
%
%---------------------------------------------------------------------------%

:- module measurement_units.

:- interface.

%---------------------------------------------------------------------------%
%
% Memory
%

    % Units for measuring memory.
    %
:- type memory_units
    --->    units_words
    ;       units_bytes.

    % Memory abstract data type.
    %
    % This type represents an amount of computer memory while abstracting away
    % how the memory is measured.
    %
:- type memory.

    % memory_words(Words, BytesPerWord) = Memory
    %
    % Convert number of words to memory type.
    %
:- func memory_words(int, int) = memory.

    % Division for memory units. Use of this function may return fractions of
    % units.
    %
:- func (memory) / (int) = (memory) is det.

    % Format a memory value using the given units.
    %
    % The third argument is the number of decimal places to show.
    %
:- func format_memory(memory, memory_units, int) = string.

:- pred compare_memory(memory::in, memory::in, comparison_result::out) is det.

%---------------------------------------------------------------------------%
%
% Percent
%

    % Percent abstract data type.
    %
:- type percent.

    % Convert from float between 0.0 and 1.0 (inclusive) and percent type.
    % Input of values outside the range above will throw exceptions.
    %
:- func percent(float) = percent.

    % Format a percentage. Prints the percentage with one decimal place and a
    % '%' symbol.
    %
:- func format_percent(percent) = string.

:- pred percent_at_or_above_threshold(int::in, percent::in) is semidet.

%---------------------------------------------------------------------------%
%
% Time
%

    % Time abstract data type.
    %
:- type time.

    % ticks_to_time(Ticks, TicksPerSec) = Time
    %
    % Converts profiler ticks to time,
    %
:- func ticks_to_time(int, int) = time.

    % time_percall(Time, Calls) = TimePercall
    %
    % Time / Calls = TimePerCall.
    %
:- func time_percall(time, int) = time.

    % Format a time, this prints the time in the most readable units for its
    % magnitude.  One or two letters follow the time to describe the units.
    %
    % Currently supported units are seconds; milli, micro, nano and pico
    % seconds.  For micro seconds a letter u is used rather than the greek
    % letter mu.
    %
:- func format_time(time) = string.

%---------------------------------------------------------------------------%
%
% Probability
%

:- type probability.

    % A certain thing,  A probability of 1.0.
    %
:- func certain = probability.

    % An impossible thing.  A probability of 0.0.
    %
:- func impossible = probability.

    % Any probability.  Note that the float augment must be between 0.0 and 1.0
    % inclusive.
    %
:- func probable(float) = probability.

    % Convert a probability value into a floating point value.
    %
:- func probability_to_float(probability) = float.

    % Combine probabilities.
    %
:- func or(probability, probability) = probability.
:- func and(probability, probability) = probability.

    % The probability of the given probability not occurring.
    %
:- func not_probability(probability) = probability.

%---------------------------------------------------------------------------%
%
% Code for formatting numbers.
%

    % Format an integer and place commas between groups of three digits.
    %
:- func commas(int) = string.

    % Format a floating point number, placing commas between groups of three
    % digits in the integer part. The first argument is a format string.
    %
:- func decimal_fraction(string, float) = string.

:- func one_decimal_fraction(float) = string.
:- func two_decimal_fraction(float) = string.
:- func four_decimal_fraction(float) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%
% Memory.
%

:- type memory
    --->    memory(
                words       :: float,
                word_size   :: int
            ).

memory_words(WordsI, BytesPerWord) = memory(WordsF, BytesPerWord) :-
    WordsF = float(WordsI).

memory(Nom, BPW) / Denom =
    ( if Denom = 0 then
        memory(0.0, BPW)
    else
        memory(Nom / float(Denom), BPW)
    ).

format_memory(memory(Words, BPW), units_bytes, Decimals) =
    format_number(Decimals, Words * float(BPW)).
format_memory(memory(Words, _), units_words, Decimals) =
    format_number(Decimals, Words).

compare_memory(MemoryA, MemoryB, Result) :-
    MemoryA = memory(WordsA, WordSizeA),
    MemoryB = memory(WordsB, WordSizeB),
    require(unify(WordSizeA, WordSizeB),
        "compare_memory: word size mismatch"),
    compare(Result, WordsA, WordsB).

%---------------------------------------------------------------------------%
%
% Percentages.
%

:- type percent
    --->    percent_float(float).

percent(P) = percent_float(P).

format_percent(percent_float(P)) = String :-
    string.format("%.2f", [f(P * 100.0)], String).

percent_at_or_above_threshold(Threshold, percent_float(P)) :-
    (P * float(100)) >= float(Threshold).

%---------------------------------------------------------------------------%
%
% Time.
%

    % Time is stored in seconds using a float.
    %
:- type time
    --->    time_sec(float).

ticks_to_time(Ticks, TicksPerSec) = Time :-
    % TicksPerSec will not be zero.
    SecPerTick = 1.0 / float(TicksPerSec),
    Time = time_sec(float(Ticks) * SecPerTick).

time_percall(time_sec(Time), Calls) = time_sec(TimePerCall) :-
    ( if Calls = 0 then
        TimePerCall = 0.0
    else
        TimePerCall = Time / float(Calls)
    ).

:- func milli = float.
milli = 0.001.

:- func micro = float.
micro = 0.000001.

:- func nano = float.
nano = 0.000000001.

:- func pico = float.
pico = 0.000000000001.

% TODO: When there is no resolution beyond 10ms since there is a clock tick
% every 10ms, the decimal points on some of these numbers should not be shown.
% However it's probably useful to show at least 2 decimal points when the value
% is within the range 1-10 seconds.
%
% TODO: If the display system supports printing the greek letter mu, then it
% should be used rather than the latin letter u.

format_time(time_sec(F)) = String :-
    ( if F < nano then
        % Print in ps.
        string.format("%.1fps", [f(F / pico)], String)
    else if F < micro then
        % Print in ns.
        string.format("%.1fns", [f(F / nano)], String)
    else if F < milli then
        % Print in us.
        string.format("%.1fus", [f(F / micro)], String)
    else if F < 1.0 then
        % Print in ms.
        string.format("%.1fms", [f(F / milli)], String)
    else
        % Print in seconds.
        string.format("%.1fs", [f(F)], String)
    ).

%---------------------------------------------------------------------------%
%
% Probabilities.
%

:- type probability == float.

certain = 1.0.

impossible = 0.0.

probable(Prob) = Prob :-
    ( if
        Prob =< 1.0,
        Prob >= 0.0
    then
        true
    else
        error(format("Probability %f out of range 0.0 to 1.0 inclusive",
            [f(Prob)]))
    ).

probability_to_float(Prob) = Prob.

    % Combine disjunct probabilities by the negation of the conjunction of
    % their negations.
    %
    % A V B = ~(~A ^ ~B)
    %
or(A, B) = not_probability(and(not_probability(A), not_probability(B))).

    % Combine conjoint probabilities with multiplication.
and(A, B) = A * B.

not_probability(X) = 1.0 - X.

%---------------------------------------------------------------------------%
%
% Code for formatting numbers.
%

commas(Num) = Str :-
    string.int_to_string(Num, Str0),
    add_commas_intstr(Str0, Str).

decimal_fraction(Format, Measure) = Representation :-
    string.format(Format, [f(Measure)], Str0),
    string.split_at_char('.', Str0) = SubStrings,
    ( if
        SubStrings = [WholeString0, FractionString]
    then
        add_commas_intstr(WholeString0, WholeString),
        Representation = WholeString ++ "." ++ FractionString
    else if
        % If there are no decimal symbols in the number, try to work with it
        % as an integer.
        SubStrings = [WholeString]
    then
        add_commas_intstr(WholeString, Representation)
    else
        unexpected($pred, "didn't split on decimal point properly")
    ).

one_decimal_fraction(Measure) = decimal_fraction("%.1f", Measure).

two_decimal_fraction(Measure) = decimal_fraction("%.2f", Measure).

four_decimal_fraction(Measure) = decimal_fraction("%.4f", Measure).

%---------------------------------------------------------------------------%

:- pred add_commas_intstr(string::in, string::out) is det.

add_commas_intstr(Str0, Str) :-
    string.to_char_list(Str0, Chars0),
    list.reverse(Chars0, RevChars0),
    string.from_char_list(reverse(add_commas(RevChars0)), Str).

:- func add_commas(list(char)) = list(char).

add_commas([]) = [].
add_commas([C]) = [C].
add_commas([C, D]) = [C, D].
add_commas([C, D, E]) = [C, D, E].
add_commas([C, D, E, F | R]) = [C, D, E, (',') | add_commas([F | R])].

%---------------------------------------------------------------------------%

:- func format_number(int, float) = string.

format_number(Decimals, Num) = String :-
    Format = "%." ++ string(Decimals) ++ "f",
    decimal_fraction(Format, Num) = String.

%---------------------------------------------------------------------------%
:- end_module measurement_units.
%---------------------------------------------------------------------------%
