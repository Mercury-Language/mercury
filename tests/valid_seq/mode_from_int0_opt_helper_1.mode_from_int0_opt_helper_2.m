%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mode_from_int0_opt_helper_1.mode_from_int0_opt_helper_2.

:- interface.

:- import_module assoc_list.
:- import_module io.

:- type post == assoc_list(string).

:- pred read_post(cgi::in, maybe_error(post)::out, io::di, io::uo) is det.

:- implementation.

read_post(_CGI, error("foo"), !IO).

:- end_module mode_from_int0_opt_helper_1.mode_from_int0_opt_helper_2.
