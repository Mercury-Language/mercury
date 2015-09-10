%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module missing_interface_import2.

:- interface.

:- pred write_key(partition_id::in, io.state::di, io.state::uo) is det.

:- implementation.

:- import_module io.                        % defines type "state"
:- import_module eqvclass.                  % defines type "partition_id"
:- import_module missing_interface_import3. % defines type "partition_id"

write_key(_, !IO) :-
    io.write_string("ok\n", !IO).
