:- module debug.

:- interface.

:- import_module string, list.

:- impure pred dump(string, list(string__poly_type)).
:- mode dump(in, in) is det.

:- implementation.

dump(Fmt, Args) :-
	string__format(Fmt, Args, Str),
	impure dump_str(Str).

:- impure pred dump_str(string::in) is det.

:- pragma foreign_proc("C", 
	dump_str(S::in), 
	[will_not_call_mercury, thread_safe],
"
	fprintf(stderr, \"%s\", S);
").
