%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%   Key Type: term.var(parse_tree.prog_data.prog_var_type)
%   Key Value: var(9)
%   Value Type: ll_backend.var_locn.var_state

:- module dep_par_24.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    (
        nop(!IO)
    &
        io.open_input("no such file", Res, !IO)
    &
        (
            Res = ok(_)
        ;
            Res = error(_),
            (
                nop(!IO)
            &
                nop(!IO)
            )
        )
    ),
    io.write_string("ok\n", !IO).

:- pred nop(io::di, io::uo) is det.
:- pragma no_inline(nop/2).

nop(!IO).
