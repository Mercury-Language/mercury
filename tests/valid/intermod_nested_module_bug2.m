:- module intermod_nested_module_bug2.

:- interface.

:- include_module intermod_nested_module_bug2__sub.
:- import_module io, string, list, assoc_list, int, std_util.

:- type cgi
    --->    cgi(
		content_length :: maybe(int)
	    ).

:- pred get_request(maybe_error(cgi), io, io).
:- mode get_request(out, di, uo) is det.

:- implementation.

get_request(Res) --> { Res = promise_only_solution(get_request0) }.

:- pred get_request0(maybe_error(cgi)).
:- mode get_request0(out) is cc_multi.

get_request0(error("foo")).
