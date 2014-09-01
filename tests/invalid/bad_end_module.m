%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module bad_end_module.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_line("hello world", !IO).

%---------------------------------------------------------------------------%
:- end_module bad_end_module_different_name.
%---------------------------------------------------------------------------%
