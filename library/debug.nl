%-----------------------------------------------------------------------------%

% Linking with this file ensures that main/1 will call the NU-Prolog
% command loop rather than calling main_predicate.
% The program can then be run manually using run/1, eg.
% 	run([progname, '-vVdg', 'foo.nl']).
% Files such as error.nl can be loaded in manually.

:- dynamic io__inhibit_user_main_predicate/0.
io__inhibit_user_main_predicate.

:- trace.

%-----------------------------------------------------------------------------%
