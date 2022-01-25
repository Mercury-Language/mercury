%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The bug that this test case is testing for is that as of 2020 jun 19,
% the compiler aborts when trying to compile this module in MLDS grades.
% It works correctly if the should-be-redundant import of sparse_bitset.m
% (now commented out) is included.
%
% The cause of the problem is as follows.
%
% - bug510a.m defines the type bar_set, which is the type of Set below,
%   as being equivalent to foo_set. The definition is in the interface section
%   of bug510a.
%
%   This fact is recorded in the interface section of bug510a.int as
%
%   :- use_module bug510b.
%   :- type bar_set == bug510b.foo_set.
%
% - bug510b.m defines the type foo_set as being equivalent to
%   sparse_bitset(int). The definition is in the implementation section
%   of bug510b.m.   
%
%   This fact is recorded in the implementation section of bug510b.int as
%
%   :- use_module sparse_bitset.
%   :- type foo_set == sparse_bitset.sparse_bitset(int).
%
% However, since bug510.m does not import bug510b.m directly, it does not
% read bug510b.int; it reads bug510b.int2. And that file is missing the
% use_module declaration for sparse_bitset.
%
% The crash happens when the MLDS generator tries to figure out what the
% target language type of the MLDS variable representing Set should be.
% Expanding type equivalences recursively leads to Set being ultimately
% of type sparse_bitset(int), but, in the absence of the use_module for
% the sparse_bitset module, the compiler gets a map.lookup failure when
% trying to access to the definition of this type.
%
% The reason why the bug does not occur in LLDS grades is that those grades,
% we store the values of Mercury variables in abstract machine registers
% and stack slots. Since these are always of type MR_Word, we don't need
% the lookup that crashes in MLDS grades.
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
