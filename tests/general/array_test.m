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
		A0 = array(Xs),
		array__to_list(A0, As0),
		array__max(A0, AMax0),
		array__min(A0, AMin0),
		array__size(A0, ASize),
		array__bounds(A0, AMin1, AMax1),
		array__bsearch(A0, 4, Cmp, ABsearch),
		array__set(A0, 8, 100, A1),
		array__to_list(A1, As1),
		array__resize(A1, 15, 1000, A2),
		array__to_list(A2, As2),
		array__shrink(A2, 10, A3),
		array__to_list(A3, As3)
	},
	write_message("A0: ", As0),
	write_message("AMax0: ", AMax0),
	write_message("AMin0: ", AMin0),
	write_message("ASize: ", ASize),
	write_message("AMin1: ", AMin1),
	write_message("AMax1: ", AMax1),
	write_message("ABsearch: ", ABsearch),
	write_message("A1: ", As1),
	write_message("A2: ", As2),
	write_message("A3: ", As3),
	{
		bt_array__from_list(0, Xs, B0),
		bt_array__to_list(B0, Bs0),
		bt_array__max(B0, BMax0),
		bt_array__min(B0, BMin0),
		bt_array__size(B0, BSize),
		bt_array__bounds(B0, BMin1, BMax1),
		( bt_array__bsearch(B0, 4, Cmp, BBsearch0) ->
			BBsearch = yes(BBsearch0)
		;
			BBsearch = no
		),
		bt_array__set(B0, 8, 100, B1),
		bt_array__to_list(B1, Bs1),
		bt_array__resize(B1, 0, 14, 1000, B2),
		bt_array__to_list(B2, Bs2),
		bt_array__shrink(B2, 0, 9, B3),
		bt_array__to_list(B3, Bs3)
	},
	write_message("B0: ", Bs0),
	write_message("BMax0: ", BMax0),
	write_message("BMin0: ", BMin0),
	write_message("BSize: ", BSize),
	write_message("BMin1: ", BMin1),
	write_message("BMax1: ", BMax1),
	write_message("BBsearch: ", BBsearch),
	write_message("B1: ", Bs1),
	write_message("B2: ", Bs2),
	write_message("B3: ", Bs3),
	
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

:- pred write_message(string, T, io__state, io__state).
:- mode write_message(in, array_ui, di, uo) is det.
:- mode write_message(in, in, di, uo) is det.

write_message(Msg, O) -->
	io__write_string(Msg),
	io__write(O),
	io__nl.

