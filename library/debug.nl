%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Linking with this file ensures that main/1 will call the NU-Prolog
% command loop rather than calling main_predicate.
% The program can then be run manually using run/1, eg.
% 	run([progname, '-vVdg', 'foo.nl']).
% Files such as error.nl can be loaded in manually.

:- dynamic io__inhibit_user_main/0.
io__inhibit_user_main.

:- trace.

%-----------------------------------------------------------------------------%
