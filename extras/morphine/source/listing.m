%------------------------------------------------------------------------------%
% Copyright (C) 1999 IRISA/INRIA.
%
% Auteur : Erwan Jahier <jahier@irisa.fr>
%
%
%   This module defines the program listing which takes a mercury module 
%   and a predicate name, a function name or a type name and outputs the 
%   corresponding source code or hlds code in the file listing_output. 
%   It is intended to be used within the Mercury debuggers.
%
%   Usage: listing <module_name> <pred_name>[.hlds_dump.*] [<arity>]
%   If arity is not typed in, listing will display all the predicates
%   pred_name/n. If the ".hlds_dump.*" extension is typed in, it will
%   display the hlds code of the file <pred_name>.hlds_dump.*.
%

:- module listing.

:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module string, int, list, term, term_io, varset, std_util, require.

:- type pair_of_lines 
	--->	pair_of_lines(int, int).


:- type type_of_file
	--->	hlds	% if it is a hlds file.
	;	source.	% if is is a source Mercury file.

%------------------------------------------------------------------------------%
main --> 
	io__command_line_arguments(Args),
	( 
		{ Args = [ModuleNameStr, ProcNameStr, ProcArStr] },
		{ string__to_int(ProcArStr, ProcAr) },
		{ string__right(ModuleNameStr, 19, EndName) }
	->
		( 
			{ EndName = ".hlds_dump.99-final" } 
		->
			open_file_and_start(ModuleNameStr, 
				ProcNameStr, yes(ProcAr), hlds)
		;
			{ string__append(ModuleNameStr, ".m", FileName) },
			open_file_and_start(FileName, 
				ProcNameStr, yes(ProcAr), source )
		)
	;
		{ Args = [ModuleNameStr, ProcNameStr] },
		{ string__right(ModuleNameStr, 19, EndName) }
	->
		( 
			{ EndName = ".hlds_dump.99-final"} 
		->
			open_file_and_start(ModuleNameStr, 
				ProcNameStr, no, hlds)
		;
			{ string__append(ModuleNameStr, ".m", FileName) },
			open_file_and_start(FileName, 
				ProcNameStr, no, source)
		)
	;
		io__write_string(" Usage: listing <module_name>[.hlds_dump.*] "),
		io__write_string("<pred_name> [<arity>]"),
		nl
        ).


:- pred open_file_and_start(string, string, maybe(int), type_of_file,
	io__state, io__state).
:- mode open_file_and_start(in, in, in, in, di, uo) is det.
	% First we read the file and collect a list of pair_of_lines(Begin, End)
	% where Begin (resp End) is the line number of the beginning (resp the 
	% end) of a part of the file where the requested predicate is defined.
	% Then we re-read the file and print those lines.
open_file_and_start(ModuleNameStr, ProcNameStr, ProcAr, FileType) -->
	( { FileType = hlds } ->
	% hlds files may contain the character "$" which provokes a syntax
	% error. So we remove all the occurrences of $ before collecting the 
	% interesting lines.
		{ append(ModuleNameStr, "copy", ModuleNameStr2) },
		remove_bad_characters(ModuleNameStr, ModuleNameStr2)
	;
		{ ModuleNameStr2 = ModuleNameStr }
	),
	io__see(ModuleNameStr2, Result1),
	( 
		{ Result1 = error(CodedMessage1) },
		{ io__error_message(CodedMessage1, Message1) },
		io__write(Message1)
	; 
		{ Result1 = ok},
		collect_interesting_lines(ProcNameStr, ProcAr, FileType,
			List_pair_of_lines),
		nl,
		io__seen,
		( { FileType = hlds } ->
			{ append("rm -f ", ModuleNameStr2, Command) },
			io__call_system(Command, _)
		;
			{ true }
		),
		io__see(ModuleNameStr, Result2),
		io__tell("listing_output", Result3),
		( 
			{ Result2 = error(CodedMessage2) },
			{ Result3 = ok },
			{ io__error_message(CodedMessage2, Message2) },
			io__write(Message2)
		; 
			{ Result2 = ok },
			{ Result3 = error(CodedMessage3) },
			{ io__error_message(CodedMessage3, Message3) },
			io__write(Message3)
		; 
			{ Result2 = error(CodedMessage2) },
			{ Result3 = error(CodedMessage3) },
			{ io__error_message(CodedMessage3, Message3) },
			{ io__error_message(CodedMessage2, Message2) },
			io__write(Message2),
			io__write(Message3)
		; 
			{ Result2 = ok },
			{ Result3 = ok },
			display_source_code(List_pair_of_lines, 1),
			nl,
			io__seen,
			io__told
		)
	).

		
:- pred remove_bad_characters(string, string, io__state, io__state).
:- mode remove_bad_characters(in, in, di, uo) is det.
remove_bad_characters(ModuleNameStr, ModuleNameStr2) -->
	{ append_list(["cat ", ModuleNameStr, "| sed s/\\$//g > ", 
		ModuleNameStr2], Call) },
	io__call_system(Call, _Result).


:- pred collect_interesting_lines(string, maybe(int), type_of_file, 
	list(pair_of_lines), io__state, io__state).
:- mode collect_interesting_lines(in, in, in, out, di, uo) is det.

collect_interesting_lines(ProcNameStr, ProcAr, FileType, ListOut) -->
	term_io__read_term(Result),
	get_line_number(LN),
	( 
		{ Result = eof },
		{ ListOut = [] }
	;
		{ Result = error(String, _) },
		io__write(String),
		{ ListOut = [] }
	;
		{ Result = term(_Varset, Term) },
		( 
			{ is_a_function_declaration(Term, ProcAr, 
				LineNumberStart, ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_predicate_and_mode_declaration(Term, ProcAr, 
				LineNumberStart, ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_function_and_mode_declaration(Term, ProcAr, 
				LineNumberStart, ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_predicate_declaration(Term, ProcAr, 
				LineNumberStart, ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_predicate_mode_declaration(Term, ProcAr, 
				LineNumberStart, ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_function_mode_declaration(Term, ProcAr, 
				LineNumberStart, ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_pragma_c_code_declaration(Term, ProcAr, 
				LineNumberStart, ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_type_declaration(Term, ProcAr, LineNumberStart, 
				ProcTerm) },
			{ ProcTerm = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_predicate(Term, ProcAr, FileType, LineNumberStart, 
				Functor) },
			{  Functor = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ is_a_function(Term, ProcAr, FileType, LineNumberStart, 
				Functor) },
			{  Functor = ProcNameStr }
		->
			{ List1 = [pair_of_lines(LineNumberStart, LN)] }
		;
			{ List1 = [] }
		),
		collect_interesting_lines(ProcNameStr, ProcAr, FileType, List2),
		{ append(List1, List2, ListOut) }
	).

%------------------------------------------------------------------------------%
:- pred is_a_predicate_declaration(term, maybe(int), int, string).
:- mode is_a_predicate_declaration(in, in, out, out) is semidet.
	% :- pred test(term, string).
is_a_predicate_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	Term = term__functor(term__atom(":-"), [T1 | _], 
		context(_, LineNumber)),
	T1 = term__functor(term__atom("pred"), [T2 | _], _),
	T2 = term__functor(term__atom(ProcTerm), ListArg, _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).


:- pred is_a_predicate_mode_declaration(term, maybe(int), int, string).
:- mode is_a_predicate_mode_declaration(in, in, out, out) is semidet.
	% :- mode test(in, out) is det.
is_a_predicate_mode_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	Term = term__functor(term__atom(":-"), [T1 | _],  
		context(_, LineNumber)),
	T1 = term__functor(term__atom("mode"), [T2 | _], _),
	T2 = term__functor(term__atom("is") , [T3 | _], _),
	T3 = term__functor(term__atom(ProcTerm) , ListArg, _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).


%------------------------------------------------------------------------------%
:- pred is_a_function_declaration(term, maybe(int), int, string).
:- mode is_a_function_declaration(in, in, out, out) is semidet.
	% :- func test(string) = int.
is_a_function_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	Term = term__functor(term__atom(":-"), [T1 | _], 
		context(_, LineNumber)),
	T1 = term__functor(term__atom("func"), [T2 | _], _),
	T2 = term__functor(term__atom("="), [T3 | _], _),
	T3 = term__functor(term__atom(ProcTerm),ListArg , _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).


:- pred is_a_function_mode_declaration(term, maybe(int), int, string).
:- mode is_a_function_mode_declaration(in, in, out, out) is semidet.
	% :- mode test(out) = in det .
is_a_function_mode_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	Term = term__functor(term__atom(":-"), [T1 | _], 
		context(_, LineNumber)),
	T1 = term__functor(term__atom("mode"), [T2 | _], _),
	T2 = term__functor(term__atom("is"), [T3 | _], _),
	T3 = term__functor(term__atom("="), [T4 | _], _),
	T4 = term__functor(term__atom(ProcTerm) , ListArg, _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).


%------------------------------------------------------------------------------%
:- pred is_a_predicate_and_mode_declaration(term, maybe(int), int, string).
:- mode is_a_predicate_and_mode_declaration(in, in, out, out) is semidet.
is_a_predicate_and_mode_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	%:- pred test(term::in, string::out) is det.
	Term = term__functor(term__atom(":-"), [T1 | _], 
		context(_, LineNumber)),
	T1 = term__functor(term__atom("pred"), [T2 | _], _),
	T2 = term__functor(term__atom("is"), [T3 | _], _),
	T3 = term__functor(term__atom(ProcTerm), ListArg, _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).

:- pred is_a_function_and_mode_declaration(term, maybe(int), int, string).
:- mode is_a_function_and_mode_declaration(in, in, out, out) is semidet.
	% :- func test(string::in) = int::out is det.
is_a_function_and_mode_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	Term = term__functor(term__atom(":-"), [T1 | _], 
		context(_, LineNumber)),
	T1 = term__functor(term__atom("::"), [T2 | _], _),
	T2 = term__functor(term__atom("func"), [T3 | _], _),
	T3 = term__functor(term__atom("=") , [T4 | _], _),
	T4 = term__functor(term__atom(ProcTerm), ListArg, _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).


%------------------------------------------------------------------------------%
:- pred is_a_pragma_c_code_declaration(term, maybe(int), int, string).
:- mode is_a_pragma_c_code_declaration(in, in, out, out) is semidet.
	% :- pragma c_code(test(S1::in, S2::in, S3::in), [...]
is_a_pragma_c_code_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	Term = term__functor(term__atom(":-"), [T1 | _], 
		context(_, LineNumber)),
	T1 = term__functor(term__atom("pragma"), [T2 | _], _),
	T2 = term__functor(term__atom("c_code"), [T3 | _], _),
	T3 = term__functor(term__atom(ProcTerm) , ListArg, _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).


%------------------------------------------------------------------------------%
:- pred is_a_type_declaration(term, maybe(int), int, string).
:- mode is_a_type_declaration(in, in, out, out) is semidet.
	% :- type maybe(T) ---> no ; yes(T)
is_a_type_declaration(Term, ProcAr, LineNumber, ProcTerm) :-
	Term = term__functor(term__atom(":-"), [T1 | _], 
		context(_, LineNumber)),
	T1 = term__functor(term__atom("type"), [T2 | _], _),
	T2 = term__functor(term__atom("--->"), [T3, _], _),
	T3 = term__functor(term__atom(ProcTerm) , ListArg, _),
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, L),
		L = Arity
	;
		ProcAr = no
	).


%------------------------------------------------------------------------------%
:- pred is_a_predicate(term, maybe(int), type_of_file, int, string).
:- mode is_a_predicate(in, in, in, out, out) is semidet.

is_a_predicate(Term, ProcAr, hlds, LineNumber, Functor) :-
	Term = functor(atom(":-"), L1, context(_, LineNumber)),
	L1 = [functor(atom(":"), [_, T1 | _], _) | _],
	T1 = functor(atom(String2), ListArg, _),
	Functor = String2,
	Functor \= "=",
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, Arity)
	;
		ProcAr = no
	).

is_a_predicate(Term, ProcAr, source, LineNumber, Functor) :-
	Term = functor(atom(String1), T1, context(_, LineNumber1)),
	( 
		( String1 = ":-" 
		; String1 = "-->") 
	->
		% The term is a clause with a body.
		T1 = [Head | _],
		Head = functor(atom(String2), ListArg, 
			context(_, LineNumber)),
		( 
			ProcAr = yes(Arity),
			list__length(ListArg, L),
			( String1 = ":-" ->
				L = Arity
			;
				% String1 = "-->"
				LL is L + 2,
				LL = Arity
			)
		;
			ProcAr = no
		),
		% We need to check if the term Term is not a declaration:
		% (From language reference manual, "Declarations" section)
		not( list__member(String2, [
				"type", "pred", "func", "inst", "mode", 
				"typeclass", "typeclass", "instance", "pragma", 
				"module", "interface", "implementation", 
				"import_module", "use_module", "include_module",
				"end_module"] )),
		Functor = String2,
		Functor \= "="	% It is the case if the proc correspond 
				% to a function.
	;
		% The term is a clause without body.
		Functor = String1,
		LineNumber = LineNumber1,
		Functor \= "=",	% It is the case if the proc correspond 
				% to a function.
		( 
			ProcAr = yes(Arity),
			list__length(T1, L),
			L = Arity
		;
			ProcAr = no
		)
	).
 
%------------------------------------------------------------------------------%
:- pred is_a_function(term, maybe(int), type_of_file, int, string).
:- mode is_a_function(in, in, in, out, out) is semidet.
is_a_function(Term, ProcAr, hlds, LineNumber, Functor) :-
	Term = functor(atom(":-"), L1, context(_, LineNumber)),
	L1 = [functor(atom("="), 
		[functor(atom(":"),[_, T1 | _], _) | _], _) | _],
	T1 = functor(atom(String2), ListArg, _),
	Functor = String2,
	Functor \= "=",
	( 
		ProcAr = yes(Arity),
		list__length(ListArg, Arity - 1)
	;
		ProcAr = no
	).

is_a_function(Term, ProcAr, source, LineNumber, Functor) :-
	Term = functor(atom(String1), T1, _),
	( 
		( String1 = ":-" 
		; String1 = "-->") 
	->
		% The term is a clause with a body.
		T1 = [Head | _],
		Head = functor(atom("="), T2, _),
		T2 = [Head2 | _],
		Head2 = functor(atom(String2), ListArg, context(_, LineNumber)),
		( 
			ProcAr = yes(Arity),
			list__length(ListArg, L),
			( String1 = ":-" ->
				LL is L + 1,
				LL = Arity
			;
				% String1 = "-->"
				LL is L + 3,
				LL = Arity
			)
		;
			ProcAr = no
		),
		% We need to check if the term Term is not a declaration:
		% (From language reference manual, "Declarations" section)
		not( list__member(String2, [
				"type", "pred", "func", "inst", "mode", 
				"typeclass", "typeclass", "instance", "pragma", 
				"module", "interface", "implementation", 
				"import_module", "use_module", "include_module",
				"end_module"] )),
		% List taken from the language reference manual, 
		% "Declarations" section.
		Functor = String2
	;
		% The term is a clause without body.
		T1 = [Head | _],
		Head = functor(atom(String2), _, context(_, LineNumber)),
		Functor = String2,
		( 
			ProcAr = yes(Arity),
			list__length(T1, L),
			L = Arity
		;
			ProcAr = no
		)
	).
	
%--------------------------------------------------------------------------%
:- pred display_source_code(list(pair_of_lines), int, io__state, io__state).
:- mode display_source_code(in, in, di, uo) is det.
display_source_code([], _, Io, Io).
display_source_code([pair_of_lines(L1, L2) | Tail], CurrentLine) -->
	{ N is L1 - CurrentLine },
	skip_n_lines(N),
	{ M is L2 - L1 + 1 },
	read_and_print_n_lines(M),	
	display_source_code(Tail, L2 + 1).


:- pred skip_n_lines(int, io__state, io__state).
:- mode skip_n_lines(in, di, uo) is det.
skip_n_lines(N) -->
	(
		{ N = 0 }
	->
		[]
	;
		io__read_line_as_string(Result),
		(
			{ Result = ok(_) },
			skip_n_lines(N - 1)
		;
			% Should never occur
			{ Result = eof },
			write_string("error in listing.m: end of file "),
			write_string("should not be reached"), nl
		;
			% Should never occur
			{ Result = error(CodedMessage) },
			{ io__error_message(CodedMessage, Message) },
			write(Message)
		)
	).


:- pred read_and_print_n_lines(int, io__state, io__state).
:- mode read_and_print_n_lines(in, di, uo) is det.
read_and_print_n_lines(N) -->
	(
		{ N = 0 }
	->
		[]
	;
		io__read_line_as_string(Result),
		(
			{ Result = ok(LineStr) },
			write_string(LineStr),
			read_and_print_n_lines(N - 1)
		;
			% Should never occur
			{ Result = eof },
			write_string("error in listing.m: end of file"),
			write_string("should not be reached"), nl
		;
			% Should never occur
			{ Result = error(CodedMessage) },
			{ io__error_message(CodedMessage, Message) },
			write(Message)
		)
	).


:- end_module listing.
%------------------------------------------------------------------------------%

