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
% of the parse tree created by the parser.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_out.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module io.
:- import_module list.
:- import_module maybe.

    % Write out the information in term context (at the moment, just the
    % line number) in a form suitable for the beginning of an error message.
    %
:- pred write_context(prog_context::in, io::di, io::uo) is det.
:- pred write_context(io.text_output_stream::in, prog_context::in,
    io::di, io::uo) is det.

    % Write to a string the information in term context (at the moment,
    % just the line number) in a form suitable for the beginning of an
    % error message.
    %
:- pred context_to_string(prog_context::in, string::out) is det.

    % Write out a symbol name, with special characters escaped, but without
    % any quotes. This is suitable for use in error messages, where the
    % caller should print out an enclosing forward/backward-quote pair (`...').
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

    % Write out a module name.
    %
:- pred write_module_name(module_name::in, io::di, io::uo) is det.
:- func module_name_to_escaped_string(module_name) = string.

:- pred write_type_ctor(type_ctor::in, io::di, io::uo) is det.
:- func type_ctor_to_string(type_ctor) = string.

:- pred write_class_id(class_id::in, io::di, io::uo) is det.

:- func maybe_quoted_cons_id_and_arity_to_string(cons_id) = string.
:- func cons_id_and_arity_to_string(cons_id) = string.

:- pred write_string_list(list(string)::in, io::di, io::uo) is det.

:- pred write_promise_type(promise_type::in, io::di, io::uo) is det.

:- func promise_to_string(promise_type) = string.
:- mode promise_to_string(in) = out is det.
:- mode promise_to_string(out) = in is semidet.
:- mode promise_to_string(out) = out is multi.

:- pred write_type_name(type_ctor::in, io::di, io::uo) is det.

:- func type_name_to_string(type_ctor) = string.

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

:- func goal_warning_to_string(goal_warning) = string.

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
    io.output_stream(Stream, !IO),
    write_context(Stream, Context, !IO).

write_context(Stream, Context, !IO) :-
    context_to_string(Context, ContextMessage),
    io.write_string(Stream, ContextMessage, !IO).

context_to_string(Context, ContextMessage) :-
    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    ( if FileName = "" then
        ContextMessage = ""
    else
        string.format("%s:%03d: ", [s(FileName), i(LineNumber)],
            ContextMessage)
    ).

%-----------------------------------------------------------------------------%

write_sym_name(qualified(ModuleSpec, Name), !IO) :-
    write_module_name(ModuleSpec, !IO),
    io.write_string(".", !IO),
    term_io.write_escaped_string(Name, !IO).
write_sym_name(unqualified(Name), !IO) :-
    term_io.write_escaped_string(Name, !IO).

sym_name_to_escaped_string(qualified(ModuleSpec, Name)) =
    module_name_to_escaped_string(ModuleSpec)
    ++ "."
    ++ term_io.escaped_string(Name).
sym_name_to_escaped_string(unqualified(Name)) =
    term_io.escaped_string(Name).

write_sym_name_and_arity(sym_name_arity(Name, Arity), !IO) :-
    write_sym_name(Name, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO).

write_quoted_sym_name(SymName, !IO) :-
    io.write_string("'", !IO),
    write_sym_name(SymName, !IO),
    io.write_string("'", !IO).

sym_name_and_arity_to_string(sym_name_arity(SymName, Arity)) = String :-
    SymNameString = sym_name_to_string(SymName),
    string.int_to_string(Arity, ArityString),
    string.append_list([SymNameString, "/", ArityString], String).

write_simple_call_id(simple_call_id(PredOrFunc, Name, Arity), !IO) :-
    Str = simple_call_id_to_string(PredOrFunc, Name, Arity),
    io.write_string(Str, !IO).

write_simple_call_id(PredOrFunc, sym_name_arity(Name, Arity), !IO) :-
    Str = simple_call_id_to_string(PredOrFunc, Name, Arity),
    io.write_string(Str, !IO).

write_simple_call_id(PredOrFunc, SymName, Arity, !IO) :-
    Str = simple_call_id_to_string(PredOrFunc, SymName, Arity),
    io.write_string(Str, !IO).

simple_call_id_to_string(simple_call_id(PredOrFunc, SymName, Arity)) =
    simple_call_id_to_string(PredOrFunc, SymName, Arity).

simple_call_id_to_string(PredOrFunc, sym_name_arity(SymName, Arity)) =
    simple_call_id_to_string(PredOrFunc, SymName, Arity).

simple_call_id_to_string(PredOrFunc, SymName, Arity) = Str :-
    % XXX When printed, promises are differentiated from predicates or
    % functions by module name, so the module names `promise',
    % `promise_exclusive', etc. should be reserved, and their dummy
    % predicates should have more unusual module names.
    Name = unqualify_name(SymName),
    % Is it really a promise?
    ( if string.prefix(Name, "promise__") then
        MaybePromise = yes(promise_type_true)
    else if string.prefix(Name, "promise_exclusive__") then
        MaybePromise = yes(promise_type_exclusive)
    else if string.prefix(Name, "promise_exhaustive__") then
        MaybePromise = yes(promise_type_exhaustive)
    else if string.prefix(Name, "promise_exclusive_exhaustive__") then
        MaybePromise = yes(promise_type_exclusive_exhaustive)
    else
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
            qual_sym_name_and_arity(AdjustedSymNameAndArity)]
    ),
    Str = error_pieces_to_string(Pieces).

simple_call_id_to_sym_name_and_arity(SimpleCallId, SNA) :-
    SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    SNA = sym_name_arity(SymName, OrigArity).

write_module_name(ModuleName, !IO) :-
    write_sym_name(ModuleName, !IO).

module_name_to_escaped_string(ModuleName) =
    sym_name_to_escaped_string(ModuleName).

%-----------------------------------------------------------------------------%

write_type_ctor(type_ctor(Name, Arity), !IO) :-
    prog_out.write_sym_name_and_arity(sym_name_arity(Name, Arity), !IO).

type_ctor_to_string(type_ctor(Name, Arity)) =
    prog_out.sym_name_and_arity_to_string(sym_name_arity(Name, Arity)).

write_class_id(class_id(Name, Arity), !IO) :-
    prog_out.write_sym_name_and_arity(sym_name_arity(Name, Arity), !IO).

%-----------------------------------------------------------------------------%

maybe_quoted_cons_id_and_arity_to_string(ConsId) =
    cons_id_and_arity_to_string_maybe_quoted(quote_cons, ConsId).

cons_id_and_arity_to_string(ConsId) =
    cons_id_and_arity_to_string_maybe_quoted(dont_quote_cons, ConsId).

:- type maybe_quote_cons
    --->    dont_quote_cons
    ;       quote_cons.

:- func cons_id_and_arity_to_string_maybe_quoted(maybe_quote_cons, cons_id)
    = string.

cons_id_and_arity_to_string_maybe_quoted(QuoteCons, ConsId) = String :-
    (
        ConsId = cons(SymName, Arity, _TypeCtor),
        SymNameString0 = sym_name_to_string(SymName),
        ( if string.contains_char(SymNameString0, '*') then
            % We need to protect against the * appearing next to a /.
            Stuff = (pred(Char::in, Str0::in, Str::out) is det :-
                ( if Char = ('*') then
                    string.append(Str0, "star", Str)
                else
                    string.char_to_string(Char, CharStr),
                    string.append(Str0, CharStr, Str)
                )
            ),
            string.foldl(Stuff, SymNameString0, "", SymNameString1)
        else
            SymNameString1 = SymNameString0
        ),
        SymNameString = term_io.escaped_string(SymNameString1),
        string.int_to_string(Arity, ArityString),
        (
            QuoteCons = dont_quote_cons,
            String = SymNameString ++ "/" ++ ArityString
        ;
            QuoteCons = quote_cons,
            String = "`" ++ SymNameString ++ "'/" ++ ArityString
        )
    ;
        ConsId = tuple_cons(Arity),
        String = "{}/" ++ string.int_to_string(Arity)
    ;
        ConsId = int_const(Int),
        string.int_to_string(Int, String)
    ;
        ConsId = uint_const(UInt),
        String = uint_to_string(UInt)
    ;
        ConsId = float_const(Float),
        String = float_to_string(Float)
    ;
        ConsId = char_const(CharConst),
        String = term_io.quoted_char(CharConst)
    ;
        ConsId = string_const(StringConst),
        String = term_io.quoted_string(StringConst)
    ;
        ConsId = impl_defined_const(Name),
        (
            QuoteCons = dont_quote_cons,
            String = "$" ++ Name
        ;
            QuoteCons = quote_cons,
            String = "`$" ++ Name ++ "'"
        )
    ;
        ConsId = closure_cons(PredProcId, _),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<pred " ++ int_to_string(PredId) ++
            " proc " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = type_ctor_info_const(Module, Ctor, Arity),
        String =
            "<type_ctor_info " ++ sym_name_to_string(Module) ++ "." ++
            Ctor ++ "/" ++ int_to_string(Arity) ++ ">"
    ;
        ConsId = base_typeclass_info_const(_, _, _, _),
        String = "<base_typeclass_info>"
    ;
        ConsId = type_info_cell_constructor(_),
        String = "<type_info_cell_constructor>"
    ;
        ConsId = typeclass_info_cell_constructor,
        String = "<typeclass_info_cell_constructor>"
    ;
        ConsId = type_info_const(_),
        String = "<type_info_const>"
    ;
        ConsId = typeclass_info_const(_),
        String = "<typeclass_info_const>"
    ;
        ConsId = ground_term_const(_, _),
        String = "<ground_term_const>"
    ;
        ConsId = tabling_info_const(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<tabling_info " ++ int_to_string(PredId) ++
            ", " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = table_io_entry_desc(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<table_io_entry_desc " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ;
        ConsId = deep_profiling_proc_layout(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<deep_profiling_proc_layout " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ).

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

goal_warning_to_string(Warning) = Str :-
    (
        Warning = goal_warning_non_tail_recursive_calls,
        Str = "non_tail_recursive_calls"
    ;
        Warning = goal_warning_singleton_vars,
        Str = "singleton_vars"
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_out.
%-----------------------------------------------------------------------------%
