%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA de Rennes.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file License in the Morphine distribution.
% 
% Author : Erwan Jahier <jahier@irisa.fr>
%
:- compile("./load_morphine.pl").


% Remove the Eclipse banner (for non-regression test)

opium_banner(_, _Sepiabanner) :-
        write(toplevel_output, ""),
        flush(toplevel_output).

:- set_error_handler(164, opium_banner/2).
