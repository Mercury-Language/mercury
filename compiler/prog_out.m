%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2011 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
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

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module io.
:- import_module list.
:- import_module maybe.

    % Write to a string the information in term context (at the moment,
    % just the line number) in a form suitable for the beginning of an
    % error message.
    %
:- pred context_to_string(prog_context::in, string::out) is det.

    % Write out the information in term context (at the moment, just the
    % line number) in a form suitable for the beginning of an error message.
    %
:- pred write_context_to_cur_stream(prog_context::in, io::di, io::uo) is det.
:- pred write_context(io.text_output_stream::in, prog_context::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func sym_name_to_escaped_string(sym_name) = string.

    % Write out a symbol name, with special characters escaped, but without
    % any quotes. This is suitable for use in error messages, where the
    % caller should print out an enclosing forward/backward-quote pair (`...').
    %
:- pred write_sym_name_to_cur_stream(sym_name::in, io::di, io::uo) is det.
:- pred write_sym_name(io.text_output_stream::in, sym_name::in,
    io::di, io::uo) is det.

    % Write out a symbol name, enclosed in single forward quotes ('...'),
    % and with any special characters escaped.
    % The output should be a syntactically valid Mercury term.
    %
:- pred write_quoted_sym_name_to_cur_stream(sym_name::in,
    io::di, io::uo) is det.

    % sym_name_arity_to_string(SymName, String):
    %
    % Convert a symbol name and arity to a "<Name>/<Arity>" string,
    % with module qualifiers separated by the standard Mercury module
    % qualifier operator.
    %
:- func sym_name_arity_to_string(sym_name_arity) = string.
:- pred write_sym_name_arity_to_cur_stream(sym_name_arity::in,
    io::di, io::uo) is det.
:- pred write_sym_name_arity(io.text_output_stream::in, sym_name_arity::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Write out a module name.
    %
:- func module_name_to_escaped_string(module_name) = string.

%-----------------------------------------------------------------------------%

:- func pf_sym_name_orig_arity_to_string(pf_sym_name_arity) = string.
:- func pf_sym_name_orig_arity_to_string(pred_or_func, sym_name_arity)
    = string.
:- func pf_sym_name_orig_arity_to_string(pred_or_func, sym_name, arity)
    = string.

%-----------------------------------------------------------------------------%

:- func type_ctor_to_string(type_ctor) = string.

:- func type_name_to_string(type_ctor) = string.
:- pred write_type_name(io.text_output_stream::in, type_ctor::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred write_class_id(io.text_output_stream::in, class_id::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Convert a cons_id to a string.
    %
    % The maybe_quoted_cons_id_and_arity_to_string version is for use
    % in error messages, while the cons_id_and_arity_to_string version
    % is for use when generating target language code. The differences are
    % that
    %
    % - the former puts quotation marks around user-defined cons_ids
    %   (i.e. those that are represented by cons/3), as opposed to
    %   builtin cons_ids such as integers, while the latter does not, and
    %
    % - the latter mangles user-defined cons_ids to ensure that they
    %   are acceptable in our target languages e.g. in comments,
    %   while the former does no mangling.
    %
    % The difference in the names refers to the first distinction above.
    %
:- func maybe_quoted_cons_id_and_arity_to_string(cons_id) = string.
:- func cons_id_and_arity_to_string(cons_id) = string.

%-----------------------------------------------------------------------------%

:- func promise_to_string(promise_type) = string.
:- mode promise_to_string(in) = out is det.
:- mode promise_to_string(out) = in is semidet.
:- mode promise_to_string(out) = out is multi.

    % Return "predicate" or "function" depending on the given value.
    %
:- func pred_or_func_to_full_str(pred_or_func) = string.

    % Return "pred" or "func" depending on the given value.
    %
:- func pred_or_func_to_str(pred_or_func) = string.

    % Print "predicate" or "function" depending on the given value.
    %
:- pred write_pred_or_func_to_cur_stream(pred_or_func::in,
    io::di, io::uo) is det.

    % Get a purity name as a string.
    %
:- pred purity_name(purity, string).
:- mode purity_name(in, out) is det.
:- mode purity_name(out, in) is semidet.

    % Print out a purity prefix.
    % This works under the assumptions that all purity names but `pure'
    % are operators, and that we never need `pure' indicators/declarations.
    %
:- func purity_prefix_to_string(purity) = string.

    % Convert an eval_method of a pragma to a string giving the name
    % of the pragma.
    %
:- func eval_method_to_pragma_name(eval_method) = string.

    % Convert an eval_method to a string description.
    %
:- func eval_method_to_string(eval_method) = string.

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

context_to_string(Context, ContextMessage) :-
    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    ( if FileName = "" then
        ContextMessage = ""
    else
        string.format("%s:%03d: ", [s(FileName), i(LineNumber)],
            ContextMessage)
    ).

write_context_to_cur_stream(Context, !IO) :-
    io.output_stream(Stream, !IO),
    write_context(Stream, Context, !IO).

write_context(Stream, Context, !IO) :-
    context_to_string(Context, ContextMessage),
    io.write_string(Stream, ContextMessage, !IO).

%-----------------------------------------------------------------------------%

sym_name_to_escaped_string(qualified(Module, Name)) =
    module_name_to_escaped_string(Module)
    ++ "."
    ++ term_io.escaped_string(Name).
sym_name_to_escaped_string(unqualified(Name)) =
    term_io.escaped_string(Name).

write_sym_name_to_cur_stream(SymName, !IO) :-
    io.output_stream(Stream, !IO),
    write_sym_name(Stream, SymName, !IO).

write_sym_name(Stream, qualified(Module, Name), !IO) :-
    write_sym_name(Stream, Module, !IO),
    io.write_string(Stream, ".", !IO),
    term_io.write_escaped_string(Stream, Name, !IO).
write_sym_name(Stream, unqualified(Name), !IO) :-
    term_io.write_escaped_string(Stream, Name, !IO).

write_quoted_sym_name_to_cur_stream(SymName, !IO) :-
    io.write_string("'", !IO),
    write_sym_name_to_cur_stream(SymName, !IO),
    io.write_string("'", !IO).

sym_name_arity_to_string(sym_name_arity(SymName, Arity)) = String :-
    SymNameString = sym_name_to_string(SymName),
    string.int_to_string(Arity, ArityString),
    string.append_list([SymNameString, "/", ArityString], String).

write_sym_name_arity_to_cur_stream(SNA, !IO) :-
    io.output_stream(Stream, !IO),
    write_sym_name_arity(Stream, SNA, !IO).

write_sym_name_arity(Stream, sym_name_arity(Name, Arity), !IO) :-
    write_sym_name(Stream, Name, !IO),
    io.write_string(Stream, "/", !IO),
    io.write_int(Stream, Arity, !IO).

%-----------------------------------------------------------------------------%

module_name_to_escaped_string(ModuleName) =
    sym_name_to_escaped_string(ModuleName).

%-----------------------------------------------------------------------------%

pf_sym_name_orig_arity_to_string(PFSymNameArity) = Str :-
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, SymName, Arity),
    Str = pf_sym_name_orig_arity_to_string(PredOrFunc, SymName, Arity).

pf_sym_name_orig_arity_to_string(PredOrFunc, SNA) = Str :-
    SNA = sym_name_arity(SymName, Arity),
    Str = pf_sym_name_orig_arity_to_string(PredOrFunc, SymName, Arity).

pf_sym_name_orig_arity_to_string(PredOrFunc, SymName, Arity) = Str :-
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    Str = pred_or_func_to_string(PredOrFunc) ++ " " ++
        add_quotes(sym_name_to_string(SymName)) ++ "/" ++
        string.int_to_string(OrigArity).

%-----------------------------------------------------------------------------%

type_ctor_to_string(type_ctor(Name, Arity)) =
    prog_out.sym_name_arity_to_string(sym_name_arity(Name, Arity)).

type_name_to_string(type_ctor(Name, _Arity)) =
    sym_name_to_escaped_string(Name).

write_type_name(Stream, type_ctor(Name, _Arity), !IO) :-
    prog_out.write_sym_name(Stream, Name, !IO).

%-----------------------------------------------------------------------------%

write_class_id(Stream, class_id(Name, Arity), !IO) :-
    prog_out.write_sym_name_arity(Stream, sym_name_arity(Name, Arity), !IO).

%-----------------------------------------------------------------------------%

maybe_quoted_cons_id_and_arity_to_string(ConsId) =
    cons_id_and_arity_to_string_maybe_quoted(dont_mangle_cons, quote_cons,
        ConsId).

cons_id_and_arity_to_string(ConsId) =
    cons_id_and_arity_to_string_maybe_quoted(mangle_cons, dont_quote_cons,
        ConsId).

:- type maybe_quote_cons
    --->    dont_quote_cons
    ;       quote_cons.

:- type maybe_mangle_cons
    --->    dont_mangle_cons
    ;       mangle_cons.

:- func cons_id_and_arity_to_string_maybe_quoted(maybe_mangle_cons,
    maybe_quote_cons, cons_id) = string.

cons_id_and_arity_to_string_maybe_quoted(MangleCons, QuoteCons, ConsId)
        = String :-
    (
        ConsId = cons(SymName, Arity, _TypeCtor),
        SymNameString0 = sym_name_to_string(SymName),
        (
            MangleCons = dont_mangle_cons,
            SymNameString = SymNameString0
        ;
            MangleCons = mangle_cons,
            ( if string.contains_char(SymNameString0, '*') then
                % We need to protect against the * appearing next to a /.
                Stuff =
                    ( pred(Char::in, Str0::in, Str::out) is det :-
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
            SymNameString = term_io.escaped_string(SymNameString1)
        ),
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
        ConsId = int8_const(Int8),
        String = string.int8_to_string(Int8)
    ;
        ConsId = uint8_const(UInt8),
        String = string.uint8_to_string(UInt8)
    ;
        ConsId = int16_const(Int16),
        String = string.int16_to_string(Int16)
    ;
        ConsId = uint16_const(UInt16),
        String = string.uint16_to_string(UInt16)
    ;
        ConsId = int32_const(Int32),
        String = string.int32_to_string(Int32)
    ;
        ConsId = uint32_const(UInt32),
        String = string.uint32_to_string(UInt32)
    ;
        ConsId = int64_const(Int64),
        String = string.int64_to_string(Int64)
    ;
        ConsId = uint64_const(UInt64),
        String = string.uint64_to_string(UInt64)
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
            "closure_cons<pred " ++ int_to_string(PredId) ++
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

promise_to_string(promise_type_true) = "promise".
promise_to_string(promise_type_exclusive) = "promise_exclusive".
promise_to_string(promise_type_exhaustive) =  "promise_exhaustive".
promise_to_string(promise_type_exclusive_exhaustive) =
    "promise_exclusive_exhaustive".

pred_or_func_to_full_str(pf_predicate) = "predicate".
pred_or_func_to_full_str(pf_function) = "function".

pred_or_func_to_str(pf_predicate) = "pred".
pred_or_func_to_str(pf_function) = "func".

write_pred_or_func_to_cur_stream(PorF, !IO) :-
    io.write_string(pred_or_func_to_full_str(PorF), !IO).

purity_name(purity_pure, "pure").
purity_name(purity_semipure, "semipure").
purity_name(purity_impure, "impure").

purity_prefix_to_string(Purity) = String :-
    (
        Purity = purity_pure,
        String = ""
    ;
        ( Purity = purity_impure
        ; Purity = purity_semipure
        ),
        purity_name(Purity, PurityName),
        String = PurityName ++ " "
    ).

eval_method_to_pragma_name(eval_normal) = _ :-
    unexpected($pred, "normal").
eval_method_to_pragma_name(eval_loop_check) = "loop_check".
eval_method_to_pragma_name(eval_memo(_)) =  "memo".
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
eval_method_to_pragma_name(eval_table_io(_, _)) = _ :-
    unexpected($pred, "io").

eval_method_to_string(eval_normal) = "normal".
eval_method_to_string(eval_loop_check) = "loop_check".
eval_method_to_string(eval_memo(_)) =  "memo".
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
        Warning = goal_warning_occurs_check,
        Str = "suspected_occurs_check_failure"
    ;
        Warning = goal_warning_singleton_vars,
        Str = "singleton_vars"
    ;
        Warning = goal_warning_suspicious_recursion,
        Str = "suspicious_recursion"
    ;
        Warning = goal_warning_no_solution_disjunct,
        Str = "no_solution_disjunct"
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_out.
%-----------------------------------------------------------------------------%
