%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for ???
% The .exp2 file is for ???
% The .exp3 file is for systems without the readline library.
%

:- module completion.

:- interface.

:- import_module io.

:- include_module completion.completion_helper_1.
:- include_module completion.completion_helper_2.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("ok\n", !IO).

:- func z = int.
z = 0.

:- func zz = int.
zz = 0.
