%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called intermod_nested_module_bug.
%

:- module mode_from_int0_opt.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.
:- import_module mode_from_int0_opt_helper_1.
:- import_module mode_from_int0_opt_helper_1.mode_from_int0_opt_helper_2.

main(!IO) :-
    get_request(Res0, !IO),
    (
        Res0 = ok(CGI),
        read_post(CGI, Form, !IO),
        io.write_line(Form, !IO)
    ;
        Res0 = error(Error),
        io.write_string(Error, !IO)
    ).
