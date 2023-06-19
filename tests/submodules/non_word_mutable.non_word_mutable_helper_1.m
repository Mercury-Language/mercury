%---------------------------------------------------------------------------%
% vim: ft=mercury sw=4 ts=4 expandtab
%---------------------------------------------------------------------------%
%
% This regression test is an expanded version of the program files by Greg Duck
% in a bug report on Feb 22 2006.
%

:- module non_word_mutable.non_word_mutable_helper_1.

:- interface.

:- impure   pred exported_set_gv1(float::in) is det.
:- semipure pred exported_get_gv1(float::out) is det.
:- impure   pred exported_set_gv2(float::in) is det.
:- semipure pred exported_get_gv2(float::out) is det.
:- impure   pred exported_set_gv3(string::in) is det.
:- semipure pred exported_get_gv3(string::out) is det.
:- impure   pred exported_set_gv4(string::in) is det.
:- semipure pred exported_get_gv4(string::out) is det.
:- impure   pred exported_set_gv5(coord::in) is det.
:- semipure pred exported_get_gv5(coord::out) is det.
:- impure   pred exported_set_gv6(coord::in) is det.
:- semipure pred exported_get_gv6(coord::out) is det.

:- implementation.

:- mutable(gv1, float, 0.0, ground, [untrailed]).
:- mutable(gv2, float, 2.3, ground, [untrailed]).
:- mutable(gv3, string, "", ground, [untrailed]).
:- mutable(gv4, string, "def", ground, [untrailed]).
:- mutable(gv5, coord, new_coord(0, 0), ground, [untrailed]).
:- mutable(gv6, coord, new_coord(2, 3), ground, [untrailed]).

:- pragma inline(exported_set_gv1/1).
:- pragma inline(exported_get_gv1/1).
:- pragma inline(exported_set_gv2/1).
:- pragma inline(exported_get_gv2/1).
:- pragma inline(exported_set_gv3/1).
:- pragma inline(exported_get_gv3/1).
:- pragma inline(exported_set_gv4/1).
:- pragma inline(exported_get_gv4/1).
:- pragma inline(exported_set_gv5/1).
:- pragma inline(exported_get_gv5/1).
:- pragma inline(exported_set_gv6/1).
:- pragma inline(exported_get_gv6/1).

exported_set_gv1(X) :- impure   set_gv1(X).
exported_get_gv1(X) :- semipure get_gv1(X).
exported_set_gv2(X) :- impure   set_gv2(X).
exported_get_gv2(X) :- semipure get_gv2(X).
exported_set_gv3(X) :- impure   set_gv3(X).
exported_get_gv3(X) :- semipure get_gv3(X).
exported_set_gv4(X) :- impure   set_gv4(X).
exported_get_gv4(X) :- semipure get_gv4(X).
exported_set_gv5(X) :- impure   set_gv5(X).
exported_get_gv5(X) :- semipure get_gv5(X).
exported_set_gv6(X) :- impure   set_gv6(X).
exported_get_gv6(X) :- semipure get_gv6(X).
