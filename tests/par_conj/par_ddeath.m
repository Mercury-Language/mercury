% There was a problem with this file when --trace deep was used,
% due to a bug in the --delay-death pass.
% I'd made the delay death pass treat parallel conjunctions in
% the same way as plain conjunctions, but that is all wrong
% and causes this exception in the code generator.
% 
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%         Key Type: term.var(parse_tree.prog_data.prog_var_type)
%         Key Value: var(6)
%         Value Type: ll_backend.var_locn.var_state

:- module par_ddeath.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(IO0, IO) :-
    (
        A = 100,
        (
            C = A
        &
	    true
        )
    &
        true
    ),
    io.write_int(C, IO0, IO1),
    io.nl(IO1, IO).
