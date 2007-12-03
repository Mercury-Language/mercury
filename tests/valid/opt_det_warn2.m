:- module opt_det_warn2.
:- interface.
:- import_module io.
:- pred foo(io::di, io::uo) is det.
:- implementation.
:- import_module exception.
foo(_, _) :-
	throw("help!").
