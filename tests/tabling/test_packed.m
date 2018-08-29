%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case to exercise the code for tabling terms that have arguments
% packed next to both local and remote secondary tags.
%

:- module test_packed.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module string.

:- pragma require_feature_set([memo]).

    % An enum type whose representation takes 1 bit.
:- type flag
    --->    flag_clear
    ;       flag_set.

    % An enum type whose representation takes 2 bits.
:- type color
    --->    red
    ;       green
    ;       blue.

    % An enum type whose representation takes 3 bits.
:- type fruit
    --->    apple
    ;       pear
    ;       peach
    ;       orange
    ;       banana.

    % Every function symbol fN* should be allocated primary tag N.
    % The function symbols whose names end in _[abc] share their
    % primary tags. being distinguished by a local (f0) or remote (f7)
    % secondary tag. These all have an initial sequence of subword-sized
    % arguments that the compiler should pack next to the secondary tag.
:- type t
    --->    f0_a
    ;       f0_b(f0_b1 :: flag, f0_b2 :: color, f0_b3 :: fruit)
    ;       f0_c(f0_c1 :: color, f0_c2 :: fruit, f0_c3 :: flag)
    ;       f1(int)
    ;       f2(int)
    ;       f3(int)
    ;       f4(int)
    ;       f5(int)
    ;       f6(int)
    ;       f7_a(f7_a1 :: int)
    ;       f7_b(f7_b1 :: flag, f7_b2 :: color, f7_b3 :: fruit)
    ;       f7_c(f7_c1 :: fruit, f7_c2 :: flag, f7_c3 :: color, f7_c4 :: int).

main(!IO) :-
    LocalA = f0_a,
    LocalB = f0_b(flag_set, blue, peach),
    LocalC = f0_c(green, orange, flag_clear),
    RemoteA = f7_a(41),
    RemoteB = f7_b(flag_clear, red, peach),
    RemoteC = f7_c(pear, flag_set, green, 43),
    TermsToTable = [
        "local_a" -  LocalA,
        "local_b" -  LocalB,
        "local_c" -  LocalC,
        "remote_a" - RemoteA,
        "remote_b" - RemoteB,
        "remote_c" - RemoteC
    ],
    list.foldl(perform_trial, TermsToTable, !IO).

:- pred perform_trial(pair(string, t)::in, io::di, io::uo) is det.

perform_trial(Desc - T, !IO) :-
    io.nl(!IO),
    io.write_string(Desc, !IO),
    io.nl(!IO),
    test(T, !IO),
    test(T, !IO).

:- pred test(t::in, io::di, io::uo) is det.

test(In, !IO) :-
    io.write_string("In:  ", !IO),
    io.write_line(In, !IO),
    maybe_assign(In, Out),
    io.write_string("Out: ", !IO),
    io.write_line(Out, !IO).

:- pred maybe_assign(t::in, t::out) is det.

:- pragma no_inline(maybe_assign/2).
:- pragma memo(maybe_assign/2).

maybe_assign(In, Out) :-
    assign(In, Out).

:- pred assign(t::in, t::out) is det.

:- pragma foreign_proc("C",
    assign(In::in, Out::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    printf(""Out = In\\n"");
    Out = In;
").
