
:- module intermod_nested_module_bug2__sub.

:- interface.

:- type post == assoc_list(string).

:- pred read_post(cgi, maybe_error(post), io, io).
:- mode read_post(in, out, di, uo) is det.

:- implementation.

read_post(_CGI, error("foo")) --> [].

:- end_module intermod_nested_module_bug2__sub.
