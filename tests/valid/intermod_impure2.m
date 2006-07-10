% See intermod_impure.m.
:- module intermod_impure2.

:- interface.

:- impure pred intermod_impure(int::out) is det.

:- implementation.

intermod_impure(Int) :-
	impure intermod_impure_2(Int).

:- impure pred intermod_impure_2(int::out) is det.

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pragma foreign_proc("C",
	intermod_impure_2(Int::out), 
	[will_not_call_mercury],
"
	printf(""Output from impure predicate\\n"");
	Int = 2;
").
:- pragma foreign_proc(il, intermod_impure_2(Int::out),
		[will_not_call_mercury, max_stack_size(1)],
"
	ldstr ""Output from impure predicate\\n""
	call void class [mscorlib]System.Console.Write(string)
	ldc.i4 2
	stloc Int
").
