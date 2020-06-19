%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The bug that this test case is testing for is that as of 2020 jun 19,
% the compiler aborts when trying to compile this module. It works correctly
% if the should-be-redundant import of sparse_bitset.m (now commented out)
% is included.
%
%---------------------------------------------------------------------------%

:- module bug510.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bug510a.

% :- import_module sparse_bitset.

main(!IO) :-
    init_bar_set(42, Set),
    bar_set_to_list(Set, List),
    io.write_line(List, !IO).
