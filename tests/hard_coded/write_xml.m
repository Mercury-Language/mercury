:- module write_xml.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module term_to_xml, bool, list, float, string, int, char, array, map.
:- import_module std_util.

:- type mytype 
	--->	hello(field1::string, 'Field<2>'::int, char, 
			'another field'::float, bool)
	;	'List'(list(listPart))
	;	'Tag-'(int)
	;	'String'(string)
	;	a_tuple({string, int, {char, float}})
	;	a_map(map(int, string))
	;	a_pred(pred(int))
	;	'a <!@#$%^&*()> functor name!!!'(int)
	;	t(type_desc)
	;	ctor(type_ctor_desc)
	;	pointer(c_pointer)
	;	foreign(ftype).

:- type listPart ---> listPart(int) ; nothing.

:- type a == array(mytype).

:- type ftype.

:- pred make_ftype(ftype::out) is det.

:- pragma foreign_type("C", ftype, "int").

:- pragma foreign_proc("C", make_ftype(F::out), 
	[will_not_call_mercury, thread_safe, promise_pure],
"
	F = 1;
").

:- pred make_pointer(c_pointer::out) is det.

:- pragma foreign_proc("C", make_pointer(P::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	P = (MR_Word) NULL;
").

:- pred p(int::in, int::in, mytype::in, int::out) is det.

p(_, _, _, 1).

:- type wrap(T) ---> wrap(T).

:- pred p1(type_desc::in, maybe_functor_info::in, string::out, 
	list(attribute)::out) is det.

p1(_, _, "X", []).

:- type ext ---> some [T] ext(ext_field_1::T).

main(!IO) :-
	some [!M] (
	map.init(!:M),
	map.set(!.M, 1, "hello", !:M),
	map.set(!.M, 2, "hello1", !:M),
	map.set(!.M, 3, "hello2", !:M),
	map.set(!.M, 4, "hello3", !:M),
	map.set(!.M, 5, "hello4", !:M),
	map.set(!.M, 6, "hello5", !:M),
	map.set(!.M, 7, "hello6", !:M),
	map.set(!.M, 8, "hello7", !:M),
	make_ftype(F),
	make_pointer(P),
	X = [
		'Tag-'(44),
		'String'("a string"),
		hello("this \n\nis a <string>&", -123, '<', 1.123, yes),
		a_tuple({"some more stuf", 123456,
			{a, 123.55322522e-99}}),
		'List'([listPart(1), 
			listPart(2), 
			nothing, 
			listPart(4), 
			nothing, 
			listPart(6), 
			listPart(7), 
			listPart(8), 
			nothing]), 
		a_map(!.M), 'a <!@#$%^&*()> functor name!!!'(999),
		a_pred(p(1, 2, hello("a string", 1, 'c', -0.00001e-10, yes))),
		t(type_of(!.M)),
		ctor(type_ctor(type_of(!.M))),
		foreign(F),
		pointer(P)],
	Map = !.M
	),
	array.from_list(X, A),
	write_xml_doc_cc(A, unique, with_stylesheet("text/css", 
		"http://www.cs.mu.oz.au/a_css.css"), embed, Result1, !IO),
	write_string("Result 1:\n", !IO),
	write(Result1, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc_cc(A, unique, with_stylesheet("text/css", 
		"http://www.cs.mu.oz.au/a_css.css"), no_dtd, Result2, !IO),
	write_string("Result 2:\n", !IO),
	write(Result2, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc_cc(wrap(Map), unique, with_stylesheet("text/css", 
		"http://www.cs.mu.oz.au/a_css.css"), embed, Result3, !IO),
	write_string("Result 3:\n", !IO),
	write(Result3, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc_cc(wrap(Map), simple, with_stylesheet("text/css", 
		"http://www.cs.mu.oz.au/a_css.css"), embed, Result3_1, !IO),
	write_string("Result 3_1:\n", !IO),
	write(Result3_1, !IO),
	nl(!IO),
	nl(!IO),
	array.from_list([listPart(1),listPart(2),
		nothing,
		listPart(4),
		nothing,
		listPart(6),
		listPart(7),
		listPart(8),
		nothing], A2),
	write_xml_doc(A2, unique, with_stylesheet("text/css", 
		"http://www.cs.mu.oz.au/a_css.css"), embed, Result4, !IO),
	write_string("Result 4:\n", !IO),
	write(Result4, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc(X, simple, no_stylesheet, 
		external(public("test", "test.dtd")), Result5, !IO),
	write_string("Result 5:\n", !IO),
	write(Result5, !IO),
	nl(!IO),
	nl(!IO),
	Simple = listPart(666),
	write_xml_doc(Simple, custom(p1), no_stylesheet, external(
		system("test")), Result6, !IO),
	write_string("Result 6:\n", !IO),
	write(Result6, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc(wrap(Simple), custom(p1), no_stylesheet, embed, 
		Result7, !IO),
	write_string("Result 7:\n", !IO),
	write(Result7, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc(yes, unique, no_stylesheet, embed, Result8, !IO),
	write_string("Result 8:\n", !IO),
	write(Result8, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc('new ext'(1), unique, no_stylesheet, no_dtd, 
		Result9, !IO),
	write_string("Result 9:\n", !IO),
	write(Result9, !IO),
	nl(!IO),
	nl(!IO),
	write_xml_doc('new ext'(1), unique, no_stylesheet, embed, 
		Result10, !IO),
	write_string("Result 10:\n", !IO),
	write(Result10, !IO),
	nl(!IO).
