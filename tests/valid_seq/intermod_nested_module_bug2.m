%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_nested_module_bug2.

:- interface.

:- include_module intermod_nested_module_bug2.sub.
:- import_module int.
:- import_module io.
:- import_module maybe.

:- type cgi
    --->    cgi(
                content_length :: maybe(int)
            ).

:- pred get_request(maybe_error(cgi)::out, io::di, io::uo) is det.

:- implementation.

get_request(Res, !IO) :-
    promise_equivalent_solutions [Res] (
        get_request0(Res)
    ).

:- pred get_request0(maybe_error(cgi)::out) is cc_multi.

get_request0(error("foo")).
