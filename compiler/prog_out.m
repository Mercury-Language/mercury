%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prog_out.m.
% Main author: fjh.

% This module defines some predicates which output various parts
% of the parse tree created by prog_io.

% WARNING - this module is mostly junk at the moment!
% Only the first hundred lines or so are meaningful.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_out.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- pred maybe_report_stats(bool::in, io::di, io::uo) is det.
:- pred maybe_write_string(bool::in, string::in, io::di, io::uo) is det.
:- pred maybe_flush_output(bool::in, io::di, io::uo) is det.

:- pred report_error(string::in, io::di, io::uo) is det.
:- pred report_error(io.output_stream::in, string::in, io::di, io::uo) is det.

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

:- pred write_string_list(list(string)::in, io::di, io::uo) is det.

:- pred write_promise_type(promise_type::in, io::di, io::uo) is det.

:- func promise_to_string(promise_type) = string.
:- mode promise_to_string(in) = out is det.
:- mode promise_to_string(out) = in is semidet.
:- mode promise_to_string(out) = out is multi.

:- pred builtin_type_to_string(builtin_type, string).
:- mode builtin_type_to_string(in, out) is det.
:- mode builtin_type_to_string(out, in) is semidet.

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

    % Convert an eval_method to a string giving the name of the pragma.
    %
:- func eval_method_to_string(eval_method) = string.

:- func maybe_arg_tabling_method_to_string(maybe(arg_tabling_method)) = string.

:- func arg_tabling_method_to_string(arg_tabling_method) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

maybe_report_stats(yes, !IO) :-
    io.report_stats(!IO).
maybe_report_stats(no, !IO).

maybe_write_string(yes, String, !IO) :-
    io.write_string(String, !IO).
maybe_write_string(no, _, !IO).

maybe_flush_output(yes, !IO) :-
    io.flush_output(!IO).
maybe_flush_output(no, !IO).

report_error(ErrorMessage, !IO) :-
    io.write_string("Error: ", !IO),
    io.write_string(ErrorMessage, !IO),
    io.write_string("\n", !IO),
    io.set_exit_status(1, !IO).

report_error(Stream, ErrorMessage, !IO) :-
    io.set_output_stream(Stream, OldStream, !IO),
    report_error(ErrorMessage, !IO),
    io.set_output_stream(OldStream, _, !IO).

write_messages([], !IO).
write_messages([Message | Messages], !IO) :-
    write_message(Message, !IO),
    write_messages(Messages, !IO).

:- pred write_message(pair(string, term)::in, io::di, io::uo) is det.

write_message(Msg - Term, !IO) :-
    ( Term = term.functor(_Functor, _Args, Context0) ->
        Context0 = term.context(File, Line),
        Context = term.context(File, Line),
        write_context(Context, !IO)
    ;
        true
    ),
    io.write_string(Msg, !IO),
    ( Term = term.functor(term.atom(""), [], _Context2) ->
        io.write_string(".\n", !IO)
    ;
        io.write_string(": ", !IO),
        varset.init(VarSet),
            % XXX variable names in error messages
        term_io.write_term_nl(VarSet, Term, !IO)
    ).

%-----------------------------------------------------------------------------%

write_context(Context, !IO) :-
    context_to_string(Context, ContextMessage),
    io.write_string(ContextMessage, !IO).

context_to_string(Context, ContextMessage) :-
    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    ( FileName = "" ->
        ContextMessage = ""
    ;
        string.format("%s:%03d: ", [s(FileName), i(LineNumber)],
            ContextMessage)
    ).

%-----------------------------------------------------------------------------%

write_sym_name(qualified(ModuleSpec, Name), !IO) :-
    write_module_spec(ModuleSpec, !IO),
    io.write_string(".", !IO),
    term_io.write_escaped_string(Name, !IO).
write_sym_name(unqualified(Name), !IO) :-
    term_io.write_escaped_string(Name, !IO).

sym_name_to_escaped_string(qualified(ModuleSpec, Name)) =
    module_spec_to_escaped_string(ModuleSpec)
    ++ "."
    ++ term_io.escaped_string(Name).
sym_name_to_escaped_string(unqualified(Name)) =
    term_io.escaped_string(Name).

write_sym_name_and_arity(Name / Arity, !IO) :-
    write_sym_name(Name, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO).

write_quoted_sym_name(SymName, !IO) :-
    io.write_string("'", !IO),
    write_sym_name(SymName, !IO),
    io.write_string("'", !IO).

sym_name_and_arity_to_string(SymName/Arity) = String :-
    SymNameString = sym_name_to_string(SymName),
    string.int_to_string(Arity, ArityString),
    string.append_list([SymNameString, "/", ArityString], String).

write_simple_call_id(simple_call_id(PredOrFunc, Name, Arity), !IO) :-
    Str = simple_call_id_to_string(PredOrFunc, Name, Arity),
    io.write_string(Str, !IO).

write_simple_call_id(PredOrFunc, Name/Arity, !IO) :-
    Str = simple_call_id_to_string(PredOrFunc, Name, Arity),
    io.write_string(Str, !IO).

write_simple_call_id(PredOrFunc, SymName, Arity, !IO) :-
    Str = simple_call_id_to_string(PredOrFunc, SymName, Arity),
    io.write_string(Str, !IO).

simple_call_id_to_string(simple_call_id(PredOrFunc, SymName, Arity)) =
    simple_call_id_to_string(PredOrFunc, SymName, Arity).

simple_call_id_to_string(PredOrFunc, SymName/Arity) =
    simple_call_id_to_string(PredOrFunc, SymName, Arity).

simple_call_id_to_string(PredOrFunc, SymName, Arity) = Str :-
    % XXX When printed, promises are differentiated from predicates or
    % functions by module name, so the module names `promise',
    % `promise_exclusive', etc. should be reserved, and their dummy
    % predicates should have more unusual module names.
    Name = unqualify_name(SymName),
    % Is it really a promise?
    ( string.prefix(Name, "promise__") ->
        MaybePromise = yes(promise_type_true)
    ; string.prefix(Name, "promise_exclusive__") ->
        MaybePromise = yes(promise_type_exclusive)
    ; string.prefix(Name, "promise_exhaustive__") ->
        MaybePromise = yes(promise_type_exhaustive)
    ; string.prefix(Name, "promise_exclusive_exhaustive__") ->
        MaybePromise = yes(promise_type_exclusive_exhaustive)
    ;
        MaybePromise = no   % No, it is really a pred or func.
    ),
    (
        MaybePromise = yes(PromiseType),
        Pieces = [quote(promise_to_string(PromiseType)), words("declaration")]
    ;
        MaybePromise = no,
        SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
        simple_call_id_to_sym_name_and_arity(SimpleCallId,
            AdjustedSymNameAndArity),
        Pieces = [p_or_f(PredOrFunc),
            sym_name_and_arity(AdjustedSymNameAndArity)]
    ),
    Str = error_pieces_to_string(Pieces).

simple_call_id_to_sym_name_and_arity(SimpleCallId, SymName / OrigArity) :-
    SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity).

write_module_spec(ModuleSpec, !IO) :-
    write_sym_name(ModuleSpec, !IO).

module_spec_to_escaped_string(ModuleSpec) =
    sym_name_to_escaped_string(ModuleSpec).

%-----------------------------------------------------------------------------%

:- pred write_list(list(T)::in, pred(T, io, io)::in(pred(in, di, uo) is det),
    io::di, io::uo) is det.

write_list([Import1, Import2, Import3 | Imports], Writer, !IO) :-
    call(Writer, Import1, !IO),
    io.write_string(", ", !IO),
    write_list([Import2, Import3 | Imports], Writer, !IO).
write_list([Import1, Import2], Writer, !IO) :-
    call(Writer, Import1, !IO),
    io.write_string(" and ", !IO),
    call(Writer, Import2, !IO).
write_list([Import], Writer, !IO) :-
    call(Writer, Import, !IO).
write_list([], _, !IO) :-
    unexpected(this_file, "write_list: empty list").

write_string_list([], !IO).
write_string_list([Name], !IO) :-
    io.write_string(Name, !IO).
write_string_list([Name1, Name2 | Names], !IO) :-
    io.write_string(Name1, !IO),
    io.write_string(", ", !IO),
    write_string_list([Name2 | Names], !IO).

promise_to_string(promise_type_true) = "promise".
promise_to_string(promise_type_exclusive) = "promise_exclusive".
promise_to_string(promise_type_exhaustive) =  "promise_exhaustive".
promise_to_string(promise_type_exclusive_exhaustive) =
    "promise_exclusive_exhaustive".

builtin_type_to_string(builtin_type_int, "int").
builtin_type_to_string(builtin_type_float, "float").
builtin_type_to_string(builtin_type_string, "string").
builtin_type_to_string(builtin_type_character, "character").

write_promise_type(PromiseType, !IO) :-
    io.write_string(promise_to_string(PromiseType), !IO).

write_pred_or_func(PorF, !IO) :-
    io.write_string(pred_or_func_to_full_str(PorF), !IO).

pred_or_func_to_full_str(predicate) = "predicate".
pred_or_func_to_full_str(function) = "function".

pred_or_func_to_str(predicate) = "pred".
pred_or_func_to_str(function) = "func".

write_purity_prefix(Purity, !IO) :-
    ( Purity = purity_pure ->
        true
    ;
        write_purity(Purity, !IO),
        io.write_string(" ", !IO)
    ).

purity_prefix_to_string(Purity) = String :-
    ( Purity = purity_pure ->
        String = ""
    ;
        purity_name(Purity, PurityName),
        String = string.append(PurityName, " ")
    ).

write_purity(Purity, !IO) :-
    purity_name(Purity, String),
    io.write_string(String, !IO).

purity_name(purity_pure, "pure").
purity_name(purity_semipure, "semipure").
purity_name(purity_impure, "impure").

eval_method_to_string(eval_normal) = "normal".
eval_method_to_string(eval_loop_check) = "loop_check".
eval_method_to_string(eval_memo) =  "memo".
eval_method_to_string(eval_minimal(MinimalMethod)) = Str :-
    (
        MinimalMethod = own_stacks,
        % The fact that this is not the name of the corresponding pragma
        % won't matter until this becomes the default way of doing minimal
        % model tabling, at which time we will return "minimal_model" here
        % and "minimal_model_stack_copy" in the other arm of the switch.
        Str = "minimal_model_own_stacks"
    ;
        MinimalMethod = stack_copy,
        Str = "minimal_model"
    ).
eval_method_to_string(eval_table_io(IsDecl, IsUnitize)) = Str :-
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

:- func this_file = string.

this_file = "prog_out.m".

%-----------------------------------------------------------------------------%
