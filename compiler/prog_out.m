%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module parse_tree__prog_out.

% Main author: fjh.

% This module defines some predicates which output various parts
% of the parse tree created by prog_io.

% WARNING - this module is mostly junk at the moment!
% Only the first hundred lines or so are meaningful.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module std_util.

:- pred maybe_report_stats(bool::in, io::di, io::uo) is det.
:- pred maybe_write_string(bool::in, string::in, io::di, io::uo) is det.
:- pred maybe_flush_output(bool::in, io::di, io::uo) is det.

:- pred report_error(string::in, io::di, io::uo) is det.
:- pred report_error(io__output_stream::in, string::in, io::di, io::uo) is det.

    % Write out the list of error/warning messages which is returned
    % when a module is parsed.
    %
:- pred write_messages(message_list::in, io::di, io::uo) is det.

    % Write out the information in term context (at the moment, just
    % the line number) in a form suitable for the beginning of an
    % error message.
    %
:- pred write_context(prog_context::in, io::di, io::uo) is det.

    % Write to a string the information in term context (at the moment,
    % just the line number) in a form suitable for the beginning of an
    % error message.
    %
:- pred context_to_string(prog_context::in, string::out) is det.

    % Write out a symbol name, with special characters escaped,
    % but without any quotes.  This is suitable for use in
    % error messages, where the caller should print out an
    % enclosing forward/backward-quote pair (`...').
    %
:- pred write_sym_name(sym_name::in, io::di, io::uo) is det.
:- func sym_name_to_escaped_string(sym_name) = string.

:- pred write_sym_name_and_arity(sym_name_and_arity::in, io::di, io::uo)
    is det.

    % Write out a symbol name, enclosed in single forward quotes ('...')
    % if necessary, and with any special characters escaped.
    % The output should be a syntactically valid Mercury term.
    %
:- pred write_quoted_sym_name(sym_name::in, io::di, io::uo) is det.

    % sym_name_and_arity_to_string(SymName, String):
    %
    % Convert a symbol name and arity to a "<Name>/<Arity>" string,
    % with module qualifiers separated by the standard Mercury module
    % qualifier operator.
    %
:- pred sym_name_and_arity_to_string(sym_name_and_arity::in, string::out)
    is det.
:- func sym_name_and_arity_to_string(sym_name_and_arity) = string.

:- pred write_simple_call_id(simple_call_id::in, io::di, io::uo) is det.
:- func simple_call_id_to_string(simple_call_id) = string.

:- pred write_simple_call_id(pred_or_func::in, sym_name_and_arity::in,
	io::di, io::uo) is det.
:- func simple_call_id_to_string(pred_or_func, sym_name_and_arity) = string.

:- pred write_simple_call_id(pred_or_func::in, sym_name::in, arity::in,
	io::di, io::uo) is det.
:- func simple_call_id_to_string(pred_or_func, sym_name, arity) = string.

:- pred simple_call_id_to_sym_name_and_arity(simple_call_id::in,
	sym_name_and_arity::out) is det.

    % Write out a module specifier.
    %
:- pred write_module_spec(module_specifier::in, io::di, io::uo) is det.
:- func module_spec_to_escaped_string(module_specifier) = string.

:- pred write_module_list(list(module_name)::in, io::di, io::uo) is det.

:- pred write_list(list(T)::in, pred(T, io, io)::in(pred(in, di, uo) is det),
    io::di, io::uo) is det.

:- pred write_string_list(list(string)::in, io::di, io::uo) is det.

:- pred write_promise_type(promise_type::in, io::di, io::uo) is det.

:- func promise_to_string(promise_type) = string.
:- mode promise_to_string(in) = out is det.
:- mode promise_to_string(out) = in is semidet.
:- mode promise_to_string(out) = out is multi.

    % Print "predicate" or "function" depending on the given value.
    %
:- pred write_pred_or_func(pred_or_func::in, io::di, io::uo) is det.

    % Return "predicate" or "function" depending on the given value.
    %
:- func pred_or_func_to_full_str(pred_or_func) = string.

    % Return "pred" or "func" depending on the given value.
    %
:- func pred_or_func_to_str(pred_or_func) = string.

    % Print out a purity name.
    %
:- pred write_purity(purity::in, io::di, io::uo) is det.

    % Get a purity name as a string.
    %
:- pred purity_name(purity, string).
:- mode purity_name(in, out) is det.
:- mode purity_name(out, in) is semidet.

    % Print out a purity prefix.
    % This works under the assumptions that all purity names but `pure'
    % are operators, and that we never need `pure' indicators/declarations.
    %
:- pred write_purity_prefix(purity::in, io::di, io::uo) is det.
:- func purity_prefix_to_string(purity) = string.

    % Convert an eval_method to a string giving the name of the pragma,
    % and if the eval_method specifies tabling methods for individual
    % arguments, a description of those argument tabling methods.
    %
:- func eval_method_to_string(eval_method) = pair(string, maybe(string)).

    % Convert an eval_method to a single string. This is suitable for use
    % in error messages, but not for generating valid Mercury code.
    %
:- func eval_method_to_one_string(eval_method) = string.

:- func maybe_arg_tabling_method_to_string(maybe(arg_tabling_method)) = string.

:- func arg_tabling_method_to_string(arg_tabling_method) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_util.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

maybe_report_stats(yes, !IO) :-
    io__report_stats(!IO).
maybe_report_stats(no, !IO).

maybe_write_string(yes, String, !IO) :-
    io__write_string(String, !IO).
maybe_write_string(no, _, !IO).

maybe_flush_output(yes, !IO) :-
    io__flush_output(!IO).
maybe_flush_output(no, !IO).

report_error(ErrorMessage, !IO) :-
    io__write_string("Error: ", !IO),
    io__write_string(ErrorMessage, !IO),
    io__write_string("\n", !IO),
    io__set_exit_status(1, !IO).

report_error(Stream, ErrorMessage, !IO) :-
    io__set_output_stream(Stream, OldStream, !IO),
    report_error(ErrorMessage, !IO),
    io__set_output_stream(OldStream, _, !IO).

write_messages([], !IO).
write_messages([Message | Messages], !IO) :-
    write_message(Message, !IO),
    write_messages(Messages, !IO).

:- pred write_message(pair(string, term)::in,
    io::di, io::uo) is det.

write_message(Msg - Term, !IO) :-
    ( Term = term__functor(_Functor, _Args, Context0) ->
        Context0 = term__context(File, Line),
        Context = term__context(File, Line),
        write_context(Context, !IO)
    ;
        true
    ),
    io__write_string(Msg, !IO),
    ( Term = term__functor(term__atom(""), [], _Context2) ->
        io__write_string(".\n", !IO)
    ;
        io__write_string(": ", !IO),
        varset__init(VarSet),
            % XXX variable names in error messages
        term_io__write_term_nl(VarSet, Term, !IO)
    ).

%-----------------------------------------------------------------------------%

write_context(Context, !IO) :-
    context_to_string(Context, ContextMessage),
    io__write_string(ContextMessage, !IO).

context_to_string(Context, ContextMessage) :-
    term__context_file(Context, FileName),
    term__context_line(Context, LineNumber),
    ( FileName = "" ->
        ContextMessage = ""
    ;
        string__format("%s:%03d: ", [s(FileName), i(LineNumber)],
            ContextMessage)
    ).

%-----------------------------------------------------------------------------%

write_sym_name(qualified(ModuleSpec, Name), !IO) :-
    write_module_spec(ModuleSpec, !IO),
    io__write_string(".", !IO),
    term_io__write_escaped_string(Name, !IO).
write_sym_name(unqualified(Name), !IO) :-
    term_io__write_escaped_string(Name, !IO).

sym_name_to_escaped_string(qualified(ModuleSpec, Name)) =
    module_spec_to_escaped_string(ModuleSpec)
    ++ "."
    ++ term_io__escaped_string(Name).
sym_name_to_escaped_string(unqualified(Name)) =
    term_io__escaped_string(Name).

write_sym_name_and_arity(Name / Arity, !IO) :-
    write_sym_name(Name, !IO),
    io__write_string("/", !IO),
    io__write_int(Arity, !IO).

write_quoted_sym_name(SymName, !IO) :-
    io__write_string("'", !IO),
    write_sym_name(SymName, !IO),
    io__write_string("'", !IO).

sym_name_and_arity_to_string(SymName/Arity, String) :-
    mdbcomp__prim_data__sym_name_to_string(SymName, SymNameString),
    string__int_to_string(Arity, ArityString),
    string__append_list([SymNameString, "/", ArityString], String).

sym_name_and_arity_to_string(SymName/Arity) = String :-
    sym_name_and_arity_to_string(SymName/Arity, String).

write_simple_call_id(PredOrFunc - Name/Arity, !IO) :-
	Str = simple_call_id_to_string(PredOrFunc, Name, Arity),
	io__write_string(Str, !IO).

write_simple_call_id(PredOrFunc, Name/Arity, !IO) :-
	Str = simple_call_id_to_string(PredOrFunc, Name, Arity),
	io__write_string(Str, !IO).

write_simple_call_id(PredOrFunc, Name, Arity, !IO) :-
	Str = simple_call_id_to_string(PredOrFunc, Name, Arity),
	io__write_string(Str, !IO).

simple_call_id_to_string(PredOrFunc - Name/Arity) =
	simple_call_id_to_string(PredOrFunc, Name, Arity).

simple_call_id_to_string(PredOrFunc, Name/Arity) =
	simple_call_id_to_string(PredOrFunc, Name, Arity).

simple_call_id_to_string(PredOrFunc, Name, Arity) = Str :-
    % XXX When printed, promises are differentiated from predicates or
    % functions by module name, so the module names `promise',
    % `promise_exclusive', etc. should be reserved, and their dummy
    % predicates should have more unusual module names.
	(
		Name = unqualified(StrName)
	;
		Name = qualified(_, StrName)
	),
    % Is it really a promise?
	( string__prefix(StrName, "promise__") ->
		MaybePromise = yes(true)
	; string__prefix(StrName, "promise_exclusive__") ->
		MaybePromise = yes(exclusive)
	; string__prefix(StrName, "promise_exhaustive__") ->
		MaybePromise = yes(exhaustive)
	; string__prefix(StrName, "promise_exclusive_exhaustive__") ->
		MaybePromise = yes(exclusive_exhaustive)
	;
		MaybePromise = no	% No, it is really a pred or func.
	),
	(
		MaybePromise = yes(PromiseType),
		PromiseStr = promise_to_string(PromiseType),
		Str = "`" ++ PromiseStr ++ "' declaration"
	;
		MaybePromise = no,
		PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
		simple_call_id_to_sym_name_and_arity(PredOrFunc - Name/Arity,
            SymArity),
		SymArityStr = sym_name_and_arity_to_string(SymArity),
		Str = PredOrFuncStr ++ " `" ++ SymArityStr ++ "'"
	).

simple_call_id_to_sym_name_and_arity(PredOrFunc - SymName/Arity,
		SymName/OrigArity) :-
	adjust_func_arity(PredOrFunc, OrigArity, Arity).

write_module_spec(ModuleSpec, !IO) :-
    write_sym_name(ModuleSpec, !IO).

module_spec_to_escaped_string(ModuleSpec) =
    sym_name_to_escaped_string(ModuleSpec).

%-----------------------------------------------------------------------------%

write_module_list(Modules, !IO) :-
    write_list(Modules, write_module, !IO).

:- pred write_module(module_name::in, io::di, io::uo) is det.

write_module(Module, !IO) :-
    io__write_string("`", !IO),
    write_sym_name(Module, !IO),
    io__write_string("'", !IO).

write_list([Import1, Import2, Import3 | Imports], Writer, !IO) :-
    call(Writer, Import1, !IO),
    io__write_string(", ", !IO),
    write_list([Import2, Import3 | Imports], Writer, !IO).
write_list([Import1, Import2], Writer, !IO) :-
    call(Writer, Import1, !IO),
    io__write_string(" and ", !IO),
    call(Writer, Import2, !IO).
write_list([Import], Writer, !IO) :-
    call(Writer, Import, !IO).
write_list([], _, !IO) :-
    error("write_module_list").

write_string_list([], !IO).
write_string_list([Name], !IO) :-
    io__write_string(Name, !IO).
write_string_list([Name1, Name2 | Names], !IO) :-
    io__write_string(Name1, !IO),
    io__write_string(", ", !IO),
    write_string_list([Name2 | Names], !IO).

promise_to_string(true) = "promise".
promise_to_string(exclusive) = "promise_exclusive".
promise_to_string(exhaustive) =  "promise_exhaustive".
promise_to_string(exclusive_exhaustive) =
        "promise_exclusive_exhaustive".

write_promise_type(PromiseType, !IO) :-
    io__write_string(promise_to_string(PromiseType), !IO).

write_pred_or_func(PorF, !IO) :-
    io__write_string(pred_or_func_to_full_str(PorF), !IO).

pred_or_func_to_full_str(predicate) = "predicate".
pred_or_func_to_full_str(function) = "function".

pred_or_func_to_str(predicate) = "pred".
pred_or_func_to_str(function) = "func".

write_purity_prefix(Purity, !IO) :-
    ( Purity = pure ->
        true
    ;
        write_purity(Purity, !IO),
        io__write_string(" ", !IO)
    ).

purity_prefix_to_string(Purity) = String :-
    ( Purity = pure ->
        String = ""
    ;
        purity_name(Purity, PurityName),
        String = string__append(PurityName, " ")
    ).

write_purity(Purity, !IO) :-
    purity_name(Purity, String),
    io__write_string(String, !IO).

purity_name(pure, "pure").
purity_name((semipure), "semipure").
purity_name((impure), "impure").

eval_method_to_one_string(EvalMethod) = Str :-
    BaseStr - MaybeArgsStr = eval_method_to_string(EvalMethod),
    (
        MaybeArgsStr = yes(ArgsStr),
        Str = BaseStr ++ "(" ++ ArgsStr ++ ")"
    ;
        MaybeArgsStr = no,
        Str = BaseStr
    ).

eval_method_to_string(eval_normal) = "normal" - no.
eval_method_to_string(eval_loop_check) = "loop_check" - no.
eval_method_to_string(eval_memo(all_strict)) =  "memo" - no.
eval_method_to_string(eval_memo(all_fast_loose)) = "fast_loose_memo" - no.
eval_method_to_string(eval_memo(specified(Args))) = "memo" - yes(ArgsStr) :-
    ArgStrs = list__map(maybe_arg_tabling_method_to_string, Args),
    ArgsStr = "[" ++ string__join_list(", ", ArgStrs) ++ "]".
eval_method_to_string(eval_minimal(MinimalMethod)) = Str - no :-
    (
        MinimalMethod = own_stacks,
        Str = "minimal_model_own_stacks"
    ;
        MinimalMethod = stack_copy,
        Str = "minimal_model_stack_copy"
    ).
eval_method_to_string(eval_table_io(IsDecl, IsUnitize)) = Str - no :-
    (
        IsDecl = table_io_decl,
        DeclStr = "decl, "
    ;
        IsDecl = table_io_proc,
        DeclStr = "proc, "
    ),
    (
        IsUnitize = table_io_unitize,
        UnitizeStr = "unitize"
    ;
        IsUnitize = table_io_alone,
        UnitizeStr = "alone"
    ),
    Str = "table_io(" ++ DeclStr ++ UnitizeStr ++ ")".

maybe_arg_tabling_method_to_string(yes(ArgTablingMethod)) =
    arg_tabling_method_to_string(ArgTablingMethod).
maybe_arg_tabling_method_to_string(no) = "output".

arg_tabling_method_to_string(arg_value) = "value".
arg_tabling_method_to_string(arg_addr) = "addr".
arg_tabling_method_to_string(arg_promise_implied) = "promise_implied".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
