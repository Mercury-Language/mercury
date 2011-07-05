%-----------------------------------------------------------------------------%

:- module pack_args_intermod1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pack_args_intermod2.

%-----------------------------------------------------------------------------%

main(!IO) :-
    % Construct here.
    Dynamic = struct(
        ani(70), ani(60), 1000, "string",
        ani(50), ani(40), ani(30), ani(20)
    ),
    % Deconstruct there.
    pack_args_intermod2.write_struct(Dynamic, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
