%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- module string_format_lib.

:- interface.

:- import_module io, list, string.

%-----------------------------------------------------------------------------%

	% Given a specifier return all possible format strings for that
	% specifier.
:- func format_strings(string) = list(string).

	% Output each of the polytypes with the format string supplied.
:- pred output_list(list(string__poly_type)::in, string::in,
        io::di, io::uo) is det.


:- func standard_floats = list(string__poly_type).
:- func trailing_zero_floats = list(string__poly_type).
:- func rounding_floats = list(string__poly_type).
:- func extreme_floats = list(string__poly_type).
:- func denormal_floats = list(string__poly_type).
:- func infinite_floats = list(string__poly_type).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float, pair, maybe, solutions.

%-----------------------------------------------------------------------------%

standard_floats = [f(0.0), f(1.0), f(-1.0), f(10.0),
				f(-10.0), f(100.0), f(-100.0)].
trailing_zero_floats = [f(100000000000000000.0), f(-100000000000000000.0)].
rounding_floats = [f(55.555555555), f(-55.555555555)].
extreme_floats	= [f(-max), f(-min), f(min), f(max)].
denormal_floats	= [f(min / 2.0), f(-min / 2.0)].
infinite_floats = [f(-(max+max)), f(max+max)].

%-----------------------------------------------------------------------------%

output_list(PolyTypes, FormatStr) -->
	list__foldl(output_format(FormatStr), PolyTypes).

:- pred output_format(string::in, string__poly_type::in, io::di, io::uo) is det.

output_format(FormatStr, PolyType) -->
	io__format("%10s:'" ++ FormatStr ++ "'", [s(FormatStr), PolyType]),
	io__nl.

%-----------------------------------------------------------------------------%

format_strings(Specifier) = FormatStrings :-
	solutions(format_string(Specifier), FormatStrings).

%-----------------------------------------------------------------------------%

:- pred format_string(string::in, string::out) is nondet.

format_string(Specifier, FormatStr) :-
	flags_combinations(Specifier, Flags),
	width_and_prec(Specifier, WidthAndPrec),
	FormatStr = format_string(Flags, WidthAndPrec, Specifier).

:- func format_string(list(string), pair(maybe(string)), string) = string.
		
format_string(Flags, Width - Prec, Specifier) = Str :-
	FlagsStr = string__append_list(Flags),
	( Width = yes(WidthStr) ->
		Str0 = WidthStr
	;
		Str0 = ""
	),
	( Prec = yes(PrecStr) ->
		Str1 = Str0 ++ "." ++ PrecStr ++ Specifier
	;
		Str1 = Str0 ++ Specifier
	),
	Str = "%" ++ FlagsStr ++ Str1.

%-----------------------------------------------------------------------------%

:- pred flags_combinations(string::in, list(string)::out) is multi.

flags_combinations(Specifier, X) :-
	all_combinations(flags(Specifier), X).

:- func flags(string) = list(string).

flags(Specifier) = Flags :-
	Flags0 = ["-", "+", " "],
	( member(Specifier, ["o", "x", "X", "e", "E", "f", "F", "g", "G"]) ->
		Flags1 = ["#" | Flags0]
	;
		Flags1 = Flags0
	),
	( ( member(Specifier, ["d", "i", "u"]) ; Flags1 = ["#" | _] ) ->
		Flags = ["0" | Flags1]
	;
		Flags = Flags1
	).

:- pred all_combinations(list(T)::in, list(T)::out) is multi.

all_combinations(List, List).
all_combinations(List, Combination) :-
	list__delete(List, _, SubList),
	all_combinations(SubList, Combination).

:- pred width_and_prec(string::in, pair(maybe(string))::out) is nondet.

width_and_prec(Specifier, Width - Prec) :-
	maybe_num(Width),
	maybe_num(Prec),
	( Prec = yes(_) ->
		member(Specifier, ["d", "i", "o", "u", "x", "X",
				"e", "E", "f", "F", "g", "G"])
	;
		true
	).

:- pred maybe_num(maybe(string)::out) is multi.

maybe_num(no).
maybe_num(yes("0")).
maybe_num(yes("1")).
maybe_num(yes("2")).
maybe_num(yes("5")).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
