%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA de Rennes.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file License in the Morphine distribution.
% 
% Author : Erwan Jahier <jahier@irisa.fr>
%
% This is the first file to be loaded when Morphine is run.
% It is called from the morphine script.


%------------------------------------------------------------------------------%
% re-definition of morphine_answer/2
% defined in ~/sepia/workdir/sepia/pl/boot_bips.pl

morphine_answer(_, yes).
morphine_answer(_, no) :-
        write(toplevel_output, 'no.\n').
morphine_answer(_, no_answer) :-
        write(toplevel_output, 'no (more) solution.\n').
morphine_answer(_, last_yes).
morphine_answer(_, last_answer) :-
        write(toplevel_output, '\n').
morphine_answer(_, more_answers) :-
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

:- set_error_handler(156, morphine_answer/2).


%------------------------------------------------------------------------------%
% Load Morphine.
:- getenv('MERCURY_MORPHINE_DIR', Dir),
	append_strings(Dir, "/source/load_scenario.pl", MakeFile),
	compile(MakeFile).

% Initialise the Morphine session
:- init_morphine_session.

:- set_flag(toplevel_module, morphine).

%------------------------------------------------------------------------------%

