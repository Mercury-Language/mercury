%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.
% 
% Author : Erwan Jahier <jahier@irisa.fr>
%
% This is the first file to be loaded when Opium-M is run.
% It is called from the Opium-M script.


%------------------------------------------------------------------------------%
% re-definition of opium_answer/2
% defined in ~/sepia/workdir/sepia/pl/boot_bips.pl

opium_answer(_, yes).
opium_answer(_, no) :-
        write(toplevel_output, 'no.\n').
opium_answer(_, no_answer) :-
        write(toplevel_output, 'no (more) solution.\n').
opium_answer(_, last_yes).
opium_answer(_, last_answer) :-
        write(toplevel_output, '\n').
opium_answer(_, more_answers) :-
        write(toplevel_output, '     More? (;) '),
        flush(toplevel_output),
        tyi(toplevel_input, C),
	( C == 59 ->
		write(toplevel_output, '\n'),
	        flush(toplevel_output),
		fail
	;
		write(toplevel_output, '\n'),
		flush(toplevel_output)
	).

:- set_error_handler(156, opium_answer/2).


%------------------------------------------------------------------------------%
% Load Opium-M.
:- getenv('MERCURY_OPIUM_DIR', Dir),
	append_strings(Dir, "/source/load_scenario.pl", MakeFile),
	compile(MakeFile).

% Initialise the Opium-M session
:- init_opium_session.

:- set_flag(toplevel_module, 'Opium-M').

%------------------------------------------------------------------------------%

