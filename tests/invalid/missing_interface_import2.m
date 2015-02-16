%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module missing_interface_import2.

:- import_module io.
:- import_module eqvclass.
:- import_module missing_interface_import3.

:- interface.

:- pred write_key(partition_id::in, io__state::di, io__state::uo) is det.

:- implementation.

write_key(_) -->
    io__write_string("ok\n").
