:- module dice.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module library_forwarding, string, list, exception.

main(!IO) :-
	io.command_line_arguments(Args, !IO),
	(
		list.map(string.to_int, Args, Ints)
	->
		merge_sort(Ints, Sorted),
		io.write(Sorted, !IO),
		io.nl(!IO)
	;
		io.write_string("usage error\n", !IO)
	).

:- pred merge_sort(list(int)::in, list(int)::out) is det.

merge_sort(Us, Ss) :-
	N = list.length(Us),
	msort_n(N, Us, Ss, _).

:- pred msort_n(int::in, list(int)::in, list(int)::out, list(int)::out) is det.

msort_n(N, Unsorted, SortedPart, Rest) :-
	(
		N =< 0
	->
		SortedPart = [],
		Rest = Unsorted
	;
		N = 1
	->
		(
			Unsorted = [U | Us],
			SortedPart = [U],
			Rest = Us
		;
			Unsorted = [],
			throw("Unsorted = [] and N = 0")
		)
	;
		N1 = N // 2,
		dice.msort_n(N1, Unsorted, Ss1, Us2),
		N2 = N - N1,
		msort_n(N2, Us2, Ss2, Rest),
		dice.merge(Ss1, Ss2, SortedPart)
	).

:- pred merge(list(int)::in, list(int)::in, list(int)::out) is det.

merge([], [], []).
merge([S | Ss], [], [S | Ss]).
merge([], [S | Ss], [S | Ss]).
merge([A | As], [B | Bs], [C | Cs]) :-
	(
		A =< B
	->
		dice.merge(As, [B | Bs], Cs),
		C = A
	;
		dice.merge(As, [B | Bs], Cs), % BUG
		C = B
	).
