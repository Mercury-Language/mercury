%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_out.m.
% Main author: fjh.
%
% This module defines some predicates which output various parts
% of the parse tree created by prog_io.
%
% WARNING - this module is mostly junk at the moment!
% Only the first hundred lines or so are meaningful.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_out.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module maybe.

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

    % Write out a symbol name, enclosed in single forward quotes ('...'),
    % and with any special characters escaped.
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

:- pred write_type_name(type_ctor::in, io::di, io::uo) is det.

:- func type_name_to_string(type_ctor) = string.

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

    % Convert an eval_method of a pragma to a string giving the name
    % of the pragma.
    %
:- func eval_method_to_pragma_name(eval_method) = string.

    % Convert an eval_method to a string description.
    %
:- func eval_method_to_string(eval_method) = string.
:- pred write_eval_method(eval_method::in, io::di, io::uo) is det.

:- func maybe_arg_tabling_method_to_string(maybe(arg_tabling_method)) = string.

:- func arg_tabling_method_to_string(arg_tabling_method) = string.

:- func determinism_to_string(determinism) = string.

:- func can_fail_to_string(can_fail) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_util.

:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_io.

%-----------------------------------------------------------------------------%
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

write_type_name(type_ctor(Name, _Arity), !IO) :-
    prog_out.write_sym_name(Name, !IO).

type_name_to_string(type_ctor(Name, _Arity)) =
    sym_name_to_escaped_string(Name).

builtin_type_to_string(builtin_type_int, "int").
builtin_type_to_string(builtin_type_float, "float").
builtin_type_to_string(builtin_type_string, "string").
builtin_type_to_string(builtin_type_char, "character").

write_promise_type(PromiseType, !IO) :-
    io.write_string(promise_to_string(PromiseType), !IO).

write_pred_or_func(PorF, !IO) :-
    io.write_string(pred_or_func_to_full_str(PorF), !IO).

pred_or_func_to_full_str(pf_predicate) = "predicate".
pred_or_func_to_full_str(pf_function) = "function".

pred_or_func_to_str(pf_predicate) = "pred".
pred_or_func_to_str(pf_function) = "func".

write_purity_prefix(Purity, !IO) :-
    (
        Purity = purity_pure
    ;
        ( Purity = purity_impure
        ; Purity = purity_semipure
        ),
        write_purity(Purity, !IO),
        io.write_string(" ", !IO)
    ).

purity_prefix_to_string(Purity) = String :-
    (
        Purity = purity_pure,
        String = ""
    ;
        ( Purity = purity_impure
        ; Purity = purity_semipure
        ),
        purity_name(Purity, PurityName),
        String = string.append(PurityName, " ")
    ).

write_purity(Purity, !IO) :-
    purity_name(Purity, String),
    io.write_string(String, !IO).

purity_name(purity_pure, "pure").
purity_name(purity_semipure, "semipure").
purity_name(purity_impure, "impure").

eval_method_to_pragma_name(eval_normal) = _ :-
    unexpected($module, $pred, "normal").
eval_method_to_pragma_name(eval_loop_check) = "loop_check".
eval_method_to_pragma_name(eval_memo) =  "memo".
eval_method_to_pragma_name(eval_minimal(MinimalMethod)) = Str :-
    (
        MinimalMethod = own_stacks_consumer,
        % The fact that this is not the name of the corresponding pragma
        % won't matter until this becomes the default way of doing minimal
        % model tabling, at which time we will return "minimal_model" here
        % and "minimal_model_stack_copy" in the other arm of the switch.
        Str = "minimal_model_own_stacks"
    ;
        MinimalMethod = own_stacks_generator,
        Str = "minimal_model_own_stacks_generator"
    ;
        MinimalMethod = stack_copy,
        Str = "minimal_model"
    ).
eval_method_to_pragma_name(eval_table_io(_EntryKind, _IsUnitize)) = _ :-
    unexpected($module, $pred, "io").

eval_method_to_string(eval_normal) = "normal".
eval_method_to_string(eval_loop_check) = "loop_check".
eval_method_to_string(eval_memo) =  "memo".
eval_method_to_string(eval_minimal(MinimalMethod)) = Str :-
    (
        MinimalMethod = own_stacks_consumer,
        Str = "minimal_model_own_stacks_consumer"
    ;
        MinimalMethod = own_stacks_generator,
        Str = "minimal_model_own_stacks_generator"
    ;
        MinimalMethod = stack_copy,
        Str = "minimal_model_stack_copy"
    ).
eval_method_to_string(eval_table_io(EntryKind, IsUnitize)) = Str :-
    (
        EntryKind = entry_stores_outputs,
        EntryKindStr = "entry_stores_outputs, "
    ;
        EntryKind = entry_stores_procid_outputs,
        EntryKindStr = "entry_stores_procid_outputs, "
    ;
        EntryKind = entry_stores_procid_inputs_outputs,
        EntryKindStr = "entry_stores_procid_inputs_outputs, "
    ),
    (
        IsUnitize = table_io_unitize,
        UnitizeStr = "unitize"
    ;
        IsUnitize = table_io_alone,
        UnitizeStr = "alone"
    ),
    Str = "table_io(" ++ EntryKindStr ++ UnitizeStr ++ ")".

write_eval_method(EvalMethod, !IO) :-
    io.write_string(eval_method_to_string(EvalMethod), !IO).

maybe_arg_tabling_method_to_string(yes(ArgTablingMethod)) =
    arg_tabling_method_to_string(ArgTablingMethod).
maybe_arg_tabling_method_to_string(no) = "output".

arg_tabling_method_to_string(arg_value) = "value".
arg_tabling_method_to_string(arg_addr) = "addr".
arg_tabling_method_to_string(arg_promise_implied) = "promise_implied".

determinism_to_string(detism_det) = "det".
determinism_to_string(detism_semi) = "semidet".
determinism_to_string(detism_non) = "nondet".
determinism_to_string(detism_multi) = "multi".
determinism_to_string(detism_cc_non) = "cc_nondet".
determinism_to_string(detism_cc_multi) = "cc_multi".
determinism_to_string(detism_erroneous) = "erroneous".
determinism_to_string(detism_failure) = "failure".

can_fail_to_string(can_fail) = "can_fail".
can_fail_to_string(cannot_fail) = "cannot_fail".

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_out.
%-----------------------------------------------------------------------------%
