:- module intermod_nested_module_bug.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module std_util.
:- import_module intermod_nested_module_bug2, intermod_nested_module_bug2__sub.

main -->
    get_request(Res0),
    (
	{ Res0 = ok(CGI) },
	read_post(CGI, Form),
	write(Form), nl
    ;
	{ Res0 = error(Error) },
	write_string(Error)
    ).

