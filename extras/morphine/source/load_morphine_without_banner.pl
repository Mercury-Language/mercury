%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.
% 
% Author : Erwan Jahier <jahier@irisa.fr>
%
:- compile("./load_opium.pl").


% Remove the Eclipse banner (for non-regression test)

opium_banner(_, _Sepiabanner) :-
        write(toplevel_output, ""),
        flush(toplevel_output).

:- set_error_handler(164, opium_banner/2).
