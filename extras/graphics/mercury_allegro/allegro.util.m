%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.util.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.util.
:- interface.

% This module only exports C functions.
% This module is private and should not be imported by user programs.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.

%-----------------------------------------------------------------------------%

:- func make_yes_int(int) = maybe(int).
:- func make_no_int = maybe(int).

:- pragma export(make_yes_int(in) = out, "_mal_make_yes_int").
:- pragma export(make_no_int = out, "_mal_make_no_int").

make_yes_int(X) = yes(X).
make_no_int = no.

%-----------------------------------------------------------------------------%

:- func make_yes_int_int(int, int) = maybe({int, int}).
:- func make_no_int_int = maybe({int, int}).

:- pragma export(make_yes_int_int(in, in) = out, "_mal_make_yes_int_int").
:- pragma export(make_no_int_int = out, "_mal_make_no_int_int").

make_yes_int_int(X, Y) = yes({X, Y}).
make_no_int_int = no.

%-----------------------------------------------------------------------------%

:- func make_yes_string(string) = maybe(string).
:- func make_no_string = maybe(string).

:- pragma export(make_yes_string(in) = out, "_mal_make_yes_string").
:- pragma export(make_no_string = out, "_mal_make_no_string").

make_yes_string(X) = yes(X).
make_no_string = no.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
