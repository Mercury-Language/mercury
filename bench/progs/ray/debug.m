:- module debug.

:- interface.

:- import_module string, list.

:- pred dump(string, list(string__poly_type)).
:- mode dump(in, in) is det.

:- implementation.

dump(Fmt, Args) :-
	string__format(Fmt, Args, Str),
	dump_str(Str).

:- pred dump_str(string::in) is det.

:- pragma(c_code, dump_str(S::in), "
	fprintf(stderr, \"%s\", S);
").
