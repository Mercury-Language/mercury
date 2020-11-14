%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_nested_module_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.
:- import_module intermod_nested_module_bug2.
:- import_module intermod_nested_module_bug2.sub.

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
