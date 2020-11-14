%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_nested_module_bug2.sub.

:- interface.

:- type post == assoc_list(string).

:- pred read_post(cgi::in, maybe_error(post)::out, io::di, io::uo) is det.

:- implementation.

read_post(_CGI, error("foo"), !IO).

:- end_module intermod_nested_module_bug2.sub.
