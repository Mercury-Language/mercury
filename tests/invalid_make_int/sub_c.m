%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file is for --no-use-subdirs.
% The .err_exp2 file is for --use-subdirs.
%
%---------------------------------------------------------------------------%

:- module sub_c.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module sub_c_helper_1.sub1.

main(!IO) :-
    io.write_string("Hello.\n", !IO).
