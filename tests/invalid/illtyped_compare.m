:- module illtyped_compare.

:- interface.

:- type bar.

:- implementation.
:- type bar ---> bar(bar_rep).

% This comparison predicate is ill-typed.
:- type bar_rep ---> bar_rep(int)
		where comparison is compare_bar_rep.
:- pragma foreign_type("C", bar_rep, "long") 
		where comparison is compare_bar.
:- pragma foreign_type("IL", bar_rep, "valuetype [mscorlib]System.Double")
		where comparison is compare_bar.
:- pragma foreign_type("Java", bar_rep, "long")
		where comparison is compare_bar.

:- pred compare_bar(comparison_result::uo, bar::in, bar::in) is det.

compare_bar((=), _, _).

:- pred compare_bar_rep(comparison_result::uo,
		bar_rep::in, bar_rep::in) is det.

compare_bar_rep((=), _, _).
