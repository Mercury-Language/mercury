%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prim_data.m.
% Main authors: fjh, zs.
%
% This module contains some types and predicates that are, or are planned to 
% be, shared between the compiler and the debugger.

%-----------------------------------------------------------------------------%

:- module mdbcomp.prim_data.

:- interface.

% was in browser/util.m and compiler/prog_data.m

	% This enumeration must be EXACTLY the same as the MR_PredFunc enum
	% in runtime/mercury_stack_layout.h, and in the same order, since the
	% code (in browser) assumes the representation is the same.

:- type pred_or_func
	--->	predicate
	;	function.

% was in browser/util.m and compiler/trace_params.m

	% The kinds of events with which MR_trace may be called, either
	% by compiler-generated code, or by code in the standard library
	% referring to compiler-generated data structures.
	%
	% This enumeration must be EXACTLY the same as the MR_trace_port enum
	% in runtime/mercury_trace_base.h, and in the same order, since the
	% code (in browser) assumes the representation is the same.

:- type trace_port
	--->	call
	;	exit
	;	redo
	;	fail
	;	exception
	;	ite_cond
	;	ite_then
	;	ite_else
	;	neg_enter
	;	neg_success
	;	neg_failure
	;	disj
	;	switch
	;	nondet_pragma_first
	;	nondet_pragma_later
	.

% was in compiler/prog_data.m

	% The order that the sym_name function symbols appear in is
	% significant for module dependency ordering.

:- type sym_name
	--->	unqualified(string)
	;	qualified(sym_name, string).

:- type module_name == sym_name.

% was in compiler/proc_label.m

	% A proc_label is a data structure a backend can use to as the basis
	% of the label used as the entry point of a procedure.
	%
	% The defining module is the module that provides the code for the
	% predicate, the declaring module contains the `:- pred' declaration.
	% When these are different, as for specialised versions of predicates
	% from `.opt' files, the defining module's name may need to be added
	% as a qualifier to the label.

:- type proc_label
	--->	proc(
			module_name,	% defining module
			pred_or_func,
			module_name,	% declaring module
			string,		% name
			int,		% arity
			int		% mode number
		)
	;	special_proc(
			module_name,	% defining module
			special_pred_id,% indirectly defines pred name
			module_name,	% type module
			string,		% type name
			int,		% type arity
			int		% mode number
		).

% was in compiler/special_pred.m

:- type special_pred_id
	--->	unify
	;	index
	;	compare
	;	initialise.

% was in compiler/prog_util.m

	% string_to_sym_name(String, Separator, SymName):
	%	Convert a string, possibly prefixed with
	%	module qualifiers (separated by Separator),
	%	into a symbol name.
	%
:- pred string_to_sym_name(string::in, string::in, sym_name::out) is det.

	% insert_module_qualifier(ModuleName, SymName0, SymName):
	%	prepend the specified ModuleName onto the module
	%	qualifiers in SymName0, giving SymName.
:- pred insert_module_qualifier(string::in, sym_name::in, sym_name::out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, string. 

% This would be simpler if we had a string__rev_sub_string_search/3 pred.
% With that, we could search for underscores right-to-left,
% and construct the resulting symbol directly.
% Instead, we search for them left-to-right, and then call
% insert_module_qualifier to fix things up.

string_to_sym_name(String, ModuleSeparator, Result) :-
	(
		string__sub_string_search(String, ModuleSeparator, LeftLength),
		LeftLength > 0
	->
		string__left(String, LeftLength, ModuleName),
		string__length(String, StringLength),
		string__length(ModuleSeparator, SeparatorLength),
		RightLength = StringLength - LeftLength - SeparatorLength,
		string__right(String, RightLength, Name),
		string_to_sym_name(Name, ModuleSeparator, NameSym),
		insert_module_qualifier(ModuleName, NameSym, Result)
	;
		Result = unqualified(String)
	).

insert_module_qualifier(ModuleName, unqualified(PlainName),
		qualified(unqualified(ModuleName), PlainName)).
insert_module_qualifier(ModuleName, qualified(ModuleQual0, PlainName),
		qualified(ModuleQual, PlainName)) :-
	insert_module_qualifier(ModuleName, ModuleQual0, ModuleQual).
