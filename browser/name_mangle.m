%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: name_mangle.m.
% Purpose: name mangling support.
% Main author: fjh.
% Stability: low.

% This module provides the proc_name_mangle/1 function, which takes a value of
% type `mercury_proc' and returns a string which is the symbol name for
% specified procedure, and which is suitable for use in a call to dl__sym.
%
% The details of name mangling are implementation-dependent, so unfortunately
% the values stored in the `mercury_proc' type might be subject to change
% in different Mercury implementations.  Any code which creates or
% examines values of that type should be carefully isolated so that
% it can be easily changed if the representation of `mercury_proc' changes.

%-----------------------------------------------------------------------------%
:- module mdb__name_mangle.
:- interface.

	% Given a mercury_proc specifying the module name,
	% predicate or function indicator, predicate name, arity,
	% and mode number of a Mercury procedure,
	% return the label name of that procedure.
	% The label name returned is suitable for passing to dl__sym.
	%
:- func proc_name_mangle(mercury_proc) = string.

:- type mercury_proc --->
	mercury_proc(is_pred_or_func, module_name, pred_name, arity, mode_num).

:- type is_pred_or_func
	--->	predicate
	;	function.

:- type module_name == sym_name.

:- type sym_name
	--->	qualified(sym_name, string)
	;	unqualified(string).

:- type pred_name == string.

:- type arity == int.		% note that for functions that arity here
				% does *not* include the function result
				% e.g. int:'*' has arity 2, not 3.

:- type mode_num == int.	% mode numbers start from zero

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, char, int, list.

% XXX most of the code below is very similar to the code in
% compiler/llds_out.m.  Any changes there may require changes here
% and vice versa.

%-----------------------------------------------------------------------------%

proc_name_mangle(MercuryProc) =
	( high_level_code ->
		mlds_proc_name_mangle(MercuryProc)
	;
		llds_proc_name_mangle(MercuryProc)
	).
		
:- func llds_proc_name_mangle(mercury_proc) = string.
llds_proc_name_mangle(MercuryProc) = LabelName :-
	MercuryProc = mercury_proc(PredOrFunc, Module, Name0, Arity, ModeNum),
	sym_name_mangle(Module, ModuleName),
	(
		( 
			Module = unqualified("builtin")
		;
			Name0 = "main",
			Arity = 2
		)
		% The conditions above define which labels are printed without
		% module qualification.
	->
		LabelName0 = Name0
	;
		qualify_name(ModuleName, Name0, LabelName0)
	),
	name_mangle(LabelName0, LabelName1),
	string__int_to_string(Arity, ArityString),
	string__int_to_string(ModeNum, ModeNumString),
	string__append_list([LabelName1, "_", ArityString, "_", ModeNumString],
		LabelName2),
	(
		PredOrFunc = function,
		string__append("fn__", LabelName2, LabelName3)
	;
		PredOrFunc = predicate,
		LabelName3 = LabelName2
	),
	string__append("mercury__", LabelName3, LabelName4),
	( use_asm_labels ->
		string__append("_entry_", LabelName4, LabelName)
	;
		LabelName = LabelName4
	).

:- func mlds_proc_name_mangle(mercury_proc) = string.
mlds_proc_name_mangle(MercuryProc) = LabelName :-
	MercuryProc = mercury_proc(PredOrFunc, Module, Name0, Arity, ModeNum),
	sym_name_mangle(Module, ModuleName),
	(
		PredOrFunc = predicate,
		Name0 = "main",
		Arity = 2
		% The conditions above define which labels are printed without
		% module qualification.
	->
		LabelName0 = Name0
	;
		qualify_name(ModuleName, Name0, LabelName0)
	),
	name_mangle(LabelName0, LabelName1),
	(
		PredOrFunc = predicate,
		PredOrFuncString = "p",
		ArityAsPred = Arity
	;
		PredOrFunc = function,
		PredOrFuncString = "f",
		ArityAsPred = Arity + 1
	),
	string__int_to_string(ArityAsPred, ArityString),
	string__int_to_string(ModeNum, ModeNumString),
	string__append_list([LabelName1, "_", ArityString, "_",
		PredOrFuncString, "_", ModeNumString],
		LabelName).

:- pred sym_name_mangle(sym_name, string).
:- mode sym_name_mangle(in, out) is det.

sym_name_mangle(unqualified(Name), MangledName) :-
	name_mangle(Name, MangledName).
sym_name_mangle(qualified(ModuleName, PlainName), MangledName) :-
	sym_name_mangle(ModuleName, MangledModuleName),
	name_mangle(PlainName, MangledPlainName),
	qualify_name(MangledModuleName, MangledPlainName,
			MangledName).
	
	% Convert a Mercury predicate name into something that can form
	% part of a C identifier.  This predicate is necessary because
	% quoted names such as 'name with embedded spaces' are valid
	% predicate names in Mercury.

:- pred name_mangle(string, string).
:- mode name_mangle(in, out) is det.

name_mangle(Name, MangledName) :-
	(
		string__is_alnum_or_underscore(Name)
	->
		% any names that start with `f_' are changed so that
		% they start with `f__', so that we can use names starting
		% with `f_' (followed by anything except an underscore)
		% without fear of name collisions
		(
			string__append("f_", Suffix, Name)
		->
			string__append("f__", Suffix, MangledName)
		;
			MangledName = Name
		)
	;
		convert_to_valid_c_identifier(Name, MangledName)
	).

:- pred convert_to_valid_c_identifier(string, string).
:- mode convert_to_valid_c_identifier(in, out) is det.

convert_to_valid_c_identifier(String, Name) :-	
	(
		name_conversion_table(String, Name0)
	->
		Name = Name0
	;
		convert_to_valid_c_identifier_2(String, Name0),
		string__append("f", Name0, Name)
	).

:- pred qualify_name(string, string, string).
:- mode qualify_name(in, in, out) is det.

qualify_name(Module0, Name0, Name) :-
	string__append_list([Module0, "__", Name0], Name).

	% A table used to convert Mercury functors into
	% C identifiers.  Feel free to add any new translations you want.
	% The C identifiers should start with "f_",
	% to avoid introducing name clashes.
	% If the functor name is not found in the table, then
	% we use a fall-back method which produces ugly names.

:- pred name_conversion_table(string, string).
:- mode name_conversion_table(in, out) is semidet.

name_conversion_table("\\=", "f_not_equal").
name_conversion_table(">=", "f_greater_or_equal").
name_conversion_table("=<", "f_less_or_equal").
name_conversion_table("=", "f_equal").
name_conversion_table("<", "f_less_than").
name_conversion_table(">", "f_greater_than").
name_conversion_table("-", "f_minus").
name_conversion_table("+", "f_plus").
name_conversion_table("*", "f_times").
name_conversion_table("/", "f_slash").
name_conversion_table(",", "f_comma").
name_conversion_table(";", "f_semicolon").
name_conversion_table("!", "f_cut").

	% This is the fall-back method.
	% Given a string, produce a C identifier
	% for that string by concatenating the decimal
	% expansions of the character codes in the string,
	% separated by underlines.
	% The C identifier will start with "f_"; this predicate
	% constructs everything except the initial "f".
	%
	% For example, given the input "\n\t" we return "_10_8".

:- pred convert_to_valid_c_identifier_2(string, string).
:- mode convert_to_valid_c_identifier_2(in, out) is det.

convert_to_valid_c_identifier_2(String, Name) :-	
	(
		string__first_char(String, Char, Rest)
	->
		char__to_int(Char, Code),
		string__int_to_string(Code, CodeString),
		string__append("_", CodeString, ThisCharString),
		convert_to_valid_c_identifier_2(Rest, Name0),
		string__append(ThisCharString, Name0, Name)
	;
		% String is the empty string
		Name = String
	).

:- pred use_asm_labels is semidet.
:- pragma foreign_proc("C", use_asm_labels,
	[will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_USE_ASM_LABELS
	SUCCESS_INDICATOR = MR_TRUE;
#else
	SUCCESS_INDICATOR = MR_FALSE;
#endif
").

:- pred high_level_code is semidet.
:- pragma foreign_proc("C", high_level_code,
	[will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_HIGHLEVEL_CODE
	SUCCESS_INDICATOR = MR_TRUE;
#else
	SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%-----------------------------------------------------------------------------%
