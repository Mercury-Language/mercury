% Some checks of array implementation.

:- module array_test.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module int, string, list, array, bt_array, std_util.

main -->
	test([1,2,3,4,5,6,7,8,9,10]).

:- pred test(list(int), io__state, io__state).
:- mode test(in, di, uo) is det.

test(Xs) -->
	{
		Cmp = lambda([X :: in, Y :: in, Res :: out] is det,
			compare(Res, X, Y)),
		array__from_list(Xs, A0)
	},
	{ array__to_list(A0, As0) },
	write_message_int_list("A0: ", As0),
	{ array__max(A0, AMax0) },
	write_message_int("AMax0: ", AMax0),
	{ array__min(A0, AMin0) },
	write_message_int("AMin0: ", AMin0),
	{ array__size(A0, ASize) },
	write_message_int("ASize: ", ASize),
	{ array__bounds(A0, AMin1, AMax1) },
	write_message_int("AMin1: ", AMin1),
	write_message_int("AMax1: ", AMax1),
	{ array__bsearch(A0, 4, Cmp, ABsearch) },
	write_message_maybe_int("ABsearch: ", ABsearch),
	{ array__set(A0, 8, 100, A1) },
	{ array__to_list(A1, As1) },
	write_message_int_list("A1: ", As1),
	{ array__resize(A1, 15, 1000, A2) },
	{ array__to_list(A2, As2) },
	write_message_int_list("A2: ", As2),
	{ array__shrink(A2, 10, A3) },
	{ array__to_list(A3, As3) },
	write_message_int_list("A3: ", As3),
	{ A4 = array__sort(array(1 `..` 10)) },
	{ array__to_list(A4, As4) },
	write_message_int_list("A4: ", As4),
	{ A5 = array__sort(array(list__reverse(1 `..` 10))) },
	{ array__to_list(A5, As5) },
	write_message_int_list("A5: ", As5),

	{ bt_array__from_list(0, Xs, B0) },
	{ bt_array__to_list(B0, Bs0) },
	write_message_int_list("B0: ", Bs0),
	{ bt_array__max(B0, BMax0) },
	write_message_int("BMax0: ", BMax0),
	{ bt_array__min(B0, BMin0) },
	write_message_int("BMin0: ", BMin0),
	{ bt_array__size(B0, BSize) },
	write_message_int("BSize: ", BSize),
	{ bt_array__bounds(B0, BMin1, BMax1) },
	write_message_int("BMin1: ", BMin1),
	write_message_int("BMax1: ", BMax1),
	{ ( bt_array__bsearch(B0, 4, Cmp, BBsearch0) ->
		BBsearch = yes(BBsearch0)
	;
		BBsearch = no
	) },
	write_message_maybe_int("BBsearch: ", BBsearch),
	{ bt_array__set(B0, 8, 100, B1) },
	{ bt_array__to_list(B1, Bs1) },
	write_message_int_list("B1: ", Bs1),
	{ bt_array__resize(B1, 0, 14, 1000, B2) },
	{ bt_array__to_list(B2, Bs2) },
	write_message_int_list("B2: ", Bs2),
	{ bt_array__shrink(B2, 0, 9, B3) },
	{ bt_array__to_list(B3, Bs3) },
	write_message_int_list("B3: ", Bs3),

	% Finally, just in case, compare the two implementations.
	(
		{
			As0 = Bs0,
			AMax0 = BMax1,
			AMin0 = BMin1,
			ASize = BSize,
			AMin1 = BMin1,
			AMax1 = BMax1,
			AMax0 = AMax1,	% Sanity check
			AMin0 = AMin1,	% Sanity check
			BMax0 = BMax1,	% Sanity check
			BMin0 = BMin1,	% Sanity check
			ABsearch = BBsearch,
			As1 = Bs1,
			As2 = Bs2,
			As3 = Bs3,
			As1 = As3,	% Sanity check
			Bs1 = Bs3	% Sanity check
		}
	->
		io__write_string("Results all match\n")
	;
		io__write_string("Results don't match\n")
	).

:- pred write_message_int(string, int, io__state, io__state).
:- mode write_message_int(in, in, di, uo) is det.
write_message_int(Msg, O) -->
	io__write_string(Msg),
	io__write_int(O),
	io__nl.

:- pred write_message_maybe_int(string, maybe(int), io__state, io__state).
:- mode write_message_maybe_int(in, in, di, uo) is det.

write_message_maybe_int(Msg, no) -->
	io__write_string(Msg),
	io__write_string("no"),
	io__nl.
write_message_maybe_int(Msg, yes(I)) -->
	io__write_string(Msg),
	io__write_string("yes("),
	io__write_int(I),
	io__write_string(")"),
	io__nl.

:- pred write_message_int_list(string, list(int), io__state, io__state).
:- mode write_message_int_list(in, in, di, uo) is det.

write_message_int_list(Msg, List) -->
	io__write_string(Msg),
	( { List = [] },
		io__write_string("[]")
	; { List = [I | Is] },
		io__write_char('['),
		io__write_int(I),
		write_int_list_rest(Is)
	),
	io__nl.

:- pred write_int_list_rest(list(int), io__state, io__state).
:- mode write_int_list_rest(in, di, uo) is det.
write_int_list_rest([]) -->
	io__write_char(']').
write_int_list_rest([I | Is]) -->
	io__write_string(", "),
	io__write_int(I),
	write_int_list_rest(Is).

