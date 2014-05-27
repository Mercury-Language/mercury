%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% XXX
%
%---------------------------------------------------------------------------%

:- module no_refs_to_deleted_labels.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    L0 = make_list,
    list_reverse(L0, [], L),
    io.write(L, !IO),
    io.nl(!IO).

:- func make_list = list(int).

make_list = [1, 3, 2, 4].

    % list_reverse(A, L0, L):
    %
    % L is the reverse list of items in A appended in front of L0.
    %
:- pred list_reverse(list(A)::in, list(A)::in, list(A)::out) is det.

list_reverse([], L, L).
list_reverse([X | Xs], L0, L) :-
    list_reverse(Xs, [X | L0], L).
