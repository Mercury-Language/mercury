%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case to test the debugger's handling of sub-word-sized arguments,
% especially when packed next to local or remote secondary tags.
%
%---------------------------------------------------------------------------%

:- module browse_packed.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

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
    ;       f1(f1_1 :: int, f1_2 :: flag, f1_3 :: fruit, f1_4 :: color)
    ;       f2(int)
    ;       f3(int)
    ;       f4(int)
    ;       f5(int)
    ;       f6(int)
    ;       f7_a(f7_a1 :: int)
    ;       f7_b(f7_b1 :: flag, f7_b2 :: color, f7_b3 :: fruit, f7_b4 :: float)
    ;       f7_c(f7_c1 :: fruit, f7_c2 :: flag, f7_c3 :: color, f7_c4 :: int).

main(!IO) :-
    F0_B = f0_b(flag_set, blue, peach),
    F0_C = f0_c(green, orange, flag_clear),
    F1   = f1(707, flag_clear, orange, red),
    F7_A = f7_a(727),
    F7_B = f7_b(flag_clear, red, peach, 98.7),
    F7_C = f7_c(pear, flag_set, green, 737),
    smallest_t(F0_B, F0_C, F1, F7_A, F7_B, F7_C, Smallest),
    io.write_line(Smallest, !IO).

:- pred smallest_t(t::in, t::in, t::in, t::in, t::in, t::in, t::out) is det.

smallest_t(F0_B, F0_C, F1, F7_A, F7_B, F7_C, Smallest) :-
    List = [F0_B, F0_C, F1, F7_A, F7_B, F7_C],
    list.sort(List, SortedList),
    Smallest = list.det_head(SortedList).
