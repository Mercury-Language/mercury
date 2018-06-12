%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rtti_access.m.
% Main authors: zs, maclarty
%
% This module contains an interface to the label_layout and proc_layout
% types which are used in the C backend of the debugger.
%
%---------------------------------------------------------------------------%

:- module mdbcomp.rtti_access.
:- interface.

:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.trace_counts.

:- import_module list.
:- import_module maybe.

:- type label_layout.

:- func get_proc_layout_from_label_layout(label_layout) = proc_layout.

:- func get_goal_path_from_label_layout(label_layout) = goal_path_string.

:- func get_goal_path_from_maybe_label(maybe(label_layout)) = goal_path_string.

:- func get_port_from_label_layout(label_layout) = trace_port.

:- func get_path_port_from_label_layout(label_layout) = path_port.

:- pred get_context_from_label_layout(label_layout::in, string::out, int::out)
    is semidet.

%---------------------------------------------------------------------------%

:- type proc_layout.

:- func get_proc_label_from_layout(proc_layout) = proc_label.

:- func get_proc_name(proc_label) = string.

    % find_initial_version_arg_num(Proc, OutputArgNum, InputArgNum):
    %
    % Given a procedure and an output argument number of that procedure,
    % find an input argument which has the same name as the output argument,
    % expect for a numerical suffix and possibly an underscore. The output
    % argument name needn't have a numerical suffix, but if it does, then the
    % input argument's numerical suffix should be less that the numerical
    % suffix of the output argument. This procedure is used as a heuristic to
    % determine when it is worth checking if a subterm appearing in the output
    % argument also appears in the same position in the input argument.
    % The heuristic is used by the subterm dependency tracking algorithm
    % to help speed up the search.
    % Argument numbers start at one.
    % This procedure is implemented in C to avoid having to allocate memory
    % to import non-word-aligned strings into Mercury code.
    %
:- pred find_initial_version_arg_num(proc_layout::in, int::in, int::out)
    is semidet.

:- func get_all_modes_for_layout(proc_layout) = list(proc_layout).

:- func containing_proc_layout(label_layout) = proc_layout.

:- func proc_bytecode_bytes(proc_layout) = bytecode_bytes.

%---------------------------------------------------------------------------%

:- type string_table
    --->    string_table(
                string_table_chars,
                % The characters of the string table, which may include
                % null characters.

                int
                % The number of characters in the string table.
            ).

:- type module_layout.
:- type string_table_chars.

:- pred containing_module_layout(proc_layout::in, module_layout::out)
    is semidet.

:- func module_string_table(module_layout) = string_table.

:- func lookup_string_table(string_table, int) = string.

%---------------------------------------------------------------------------%

:- type bytecode
    --->    bytecode(
                bytecode_bytes,     % The bytes of the bytecode.
                int                 % The number of bytes in the bytecode.
            ).

:- type bytecode_bytes
    --->    dummy_bytecode_bytes.

:- pragma foreign_type("C", bytecode_bytes, "const MR_uint_least8_t *",
    [can_pass_as_mercury_type, stable]).
    % The following definitions are only stubs.
:- pragma foreign_type("C#", bytecode_bytes, "object", []).
:- pragma foreign_type("Java", bytecode_bytes, "java.lang.Object", []).
:- pragma foreign_type("Erlang", bytecode_bytes, "").

    % read_byte(ByteCode, Byte, !Pos):
    %
    % Read a single byte.
    %
:- pred read_byte(bytecode::in, int::out, int::in, int::out) is semidet.

    % read_short(ByteCode, Short, !Pos):
    %
    % Read a short that is represented by two bytes.
    %
:- pred read_short(bytecode::in, int::out, int::in, int::out) is semidet.

    % read_int32(ByteCode, Int, !Pos):
    %
    % Read four byte integer.
    %
:- pred read_int32(bytecode::in, int::out, int::in, int::out) is semidet.

    % read_num(ByteCode, Num, !Pos):
    %
    % Read an integer encoded using the deep profiler's variable length
    % encoding scheme.
    %
:- pred read_num(bytecode::in, int::out, int::in, int::out) is semidet.

    % read_string_via_offset(ByteCode, StringTable, String, !Pos):
    %
    % Read a string represented as a four-byte integer giving an offset
    % in the string table.
    %
:- pred read_string_via_offset(bytecode::in, string_table::in, string::out,
    int::in, int::out) is semidet.

    % read_line(ByteCode, Line, !Pos):
    %
    % Read a sequence of characters ending in a newline.
    %
:- pred read_line(bytecode::in, string::out, int::in, int::out) is semidet.

    % read_len_string(ByteCode, String, !Pos):
    %
    % Read a string represented as a <length, characters> sequence, in which
    % the length is encoded using the deep profiler's variable length
    % encoding scheme.
    %
:- pred read_len_string(bytecode::in, string::out, int::in, int::out)
    is semidet.

    % read_string_table(ByteCode, StringTable, !Pos):
    %
    % Given that ByteCode contains a string table starting at the position
    % given by !.Pos, return that string table and set !:Pos to point to
    % the first byte after it.
    %
:- pred read_string_table(bytecode::in, string_table::out,
    int::in, int::out) is semidet.

%---------------------------------------------------------------------------%

:- pred encode_byte(int::in, list(int)::out) is semidet.
:- pred encode_byte_det(int::in, list(int)::out) is det.
:- func encode_byte_func(int) = list(int).

:- pred encode_short(int::in, list(int)::out) is semidet.
:- pred encode_short_det(int::in, list(int)::out) is det.
:- func encode_short_func(int) = list(int).

:- pred encode_int32(int::in, list(int)::out) is semidet.
:- pred encode_int32_det(int::in, list(int)::out) is det.
:- func encode_int32_func(int) = list(int).

:- pred encode_num(int::in, list(int)::out) is semidet.
:- pred encode_num_det(int::in, list(int)::out) is det.
:- func encode_num_func(int) = list(int).

:- pred encode_len_string(string::in, list(int)::out) is det.
:- func encode_len_string_func(string) = list(int).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.sym_name.

:- import_module char.
:- import_module int.
:- import_module require.
:- import_module string.

:- pragma foreign_type("C", label_layout, "const MR_LabelLayout *",
    [can_pass_as_mercury_type, stable]).
    % The following definitions are only stubs.
:- pragma foreign_type("C#", label_layout, "object", []).
:- pragma foreign_type("Java", label_layout, "java.lang.Object", []).
:- pragma foreign_type("Erlang", label_layout, "").

:- pragma foreign_proc("C",
    get_proc_layout_from_label_layout(Label::in) = (ProcLayout::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ProcLayout = Label->MR_sll_entry;
").

:- pragma foreign_proc("C",
    get_goal_path_from_label_layout(Label::in) = (GoalPath::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    GoalPath = (MR_String) MR_label_goal_path(Label);
").

get_goal_path_from_maybe_label(yes(Label))
    = get_goal_path_from_label_layout(Label).
get_goal_path_from_maybe_label(no) = "".

:- pragma foreign_proc("C",
    get_context_from_label_layout(Label::in, FileName::out, LineNo::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    const char  *filename;
    int         line_no;

    SUCCESS_INDICATOR = MR_find_context(Label, &filename, &line_no);
    LineNo = (MR_Integer) line_no;
    MR_TRACE_USE_HP(
        MR_make_aligned_string(FileName, (MR_String) filename);
    );
").

:- pragma foreign_proc("C",
    get_port_from_label_layout(Label::in) = (Port::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Port = Label->MR_sll_port;
").

get_path_port_from_label_layout(Label) = PathPort :-
    Port = get_port_from_label_layout(Label),
    GoalPathStr = get_goal_path_from_label_layout(Label),
    rev_goal_path_from_string_det(GoalPathStr, GoalPath),
    PathPort = make_path_port(GoalPath, Port).

%---------------------------------------------------------------------------%

:- pragma foreign_type("C", proc_layout, "const MR_ProcLayout *",
    [can_pass_as_mercury_type, stable]).
    % The following definitions are only stubs.
:- pragma foreign_type("C#", proc_layout, "object", []).
:- pragma foreign_type("Java", proc_layout, "java.lang.Object", []).
:- pragma foreign_type("Erlang", proc_layout, "").

get_proc_label_from_layout(Layout) = ProcLabel :-
    ( if proc_layout_is_uci(Layout) then
        proc_layout_get_uci_fields(Layout, TypeName, TypeModule,
            DefModule, PredName, TypeArity, ModeNum),
        ( special_pred_name_arity(SpecialIdPrime, _, PredName, _) ->
            SpecialId = SpecialIdPrime
        ;
            unexpected($pred, "bad special_pred_id")
        ),
        SymDefModule = string_to_sym_name(DefModule),
        SymTypeModule = string_to_sym_name(TypeModule),
        ProcLabel = special_proc_label(SymDefModule, SpecialId,
            SymTypeModule, TypeName, TypeArity, ModeNum)
    else
        proc_layout_get_non_uci_fields(Layout, PredOrFunc,
            DeclModule, DefModule, PredName, Arity, ModeNum),
        SymDefModule = string_to_sym_name(DefModule),
        SymDeclModule = string_to_sym_name(DeclModule),
        ProcLabel = ordinary_proc_label(SymDefModule, PredOrFunc,
            SymDeclModule, PredName, Arity, ModeNum)
    ).

get_proc_name(ordinary_proc_label(_, _, _, ProcName, _, _)) = ProcName.
get_proc_name(special_proc_label(_, _, _, ProcName , _, _)) = ProcName.

:- pred proc_layout_is_uci(proc_layout::in) is semidet.

:- pragma foreign_proc("C",
    proc_layout_is_uci(Layout::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    if (MR_PROC_ID_IS_UCI(Layout->MR_sle_proc_id)) {
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

:- pred proc_layout_get_uci_fields(proc_layout::in, string::out,
    string::out, string::out, string::out, int::out, int::out) is det.

:- pragma foreign_proc("C",
    proc_layout_get_uci_fields(Layout::in, TypeName::out, TypeModule::out,
        DefModule::out, PredName::out, TypeArity::out, ModeNum::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    const MR_UCIProcId  *proc_id;

    proc_id = &Layout->MR_sle_uci;

    /* The casts are there to cast away const without warnings */
    TypeName   = (MR_String) (MR_Integer) proc_id->MR_uci_type_name;
    TypeModule = (MR_String) (MR_Integer) proc_id->MR_uci_type_module;
    DefModule  = (MR_String) (MR_Integer) proc_id->MR_uci_def_module;
    PredName   = (MR_String) (MR_Integer) proc_id->MR_uci_pred_name;
    TypeArity  = proc_id->MR_uci_type_arity;
    ModeNum    = proc_id->MR_uci_mode;
").

:- pred proc_layout_get_non_uci_fields(proc_layout::in, pred_or_func::out,
    string::out, string::out, string::out, int::out, int::out) is det.

:- pragma foreign_proc("C",
    proc_layout_get_non_uci_fields(Layout::in, PredOrFunc::out,
        DeclModule::out, DefModule::out, PredName::out,
        Arity::out, ModeNum::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    const MR_UserProcId *proc_id;

    proc_id = &Layout->MR_sle_user;

    /* The casts are there to cast away const without warnings */
    PredOrFunc = proc_id->MR_user_pred_or_func;
    DeclModule = (MR_String) (MR_Integer) proc_id->MR_user_decl_module;
    DefModule  = (MR_String) (MR_Integer) proc_id->MR_user_def_module;
    PredName   = (MR_String) (MR_Integer) proc_id->MR_user_name;
    Arity      = proc_id->MR_user_arity;
    ModeNum    = proc_id->MR_user_mode;
").

:- pragma foreign_proc("C",
    find_initial_version_arg_num(Layout::in, OutArgNum::in, InArgNum::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    const MR_ProcLayout     *proc;
    int                     out_hlds_num;
    const char              *out_name;
    int                     should_copy_out;

    proc = Layout;

    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc)) {
        MR_fatal_error(""find_initial_version_arg_num: proc"");
    }

    out_hlds_num = proc->MR_sle_head_var_nums[OutArgNum - 1];
    out_name = MR_hlds_var_name(proc, out_hlds_num, &should_copy_out);
    if (out_name == NULL || MR_streq(out_name, """")) {
        /* out_hlds_num was not named by the user */
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        char                    out_name_buf[MR_MAX_VARNAME_SIZE];
        int                     out_base_name_len;
        int                     out_numerical_suffix;
        int                     num_matches;
        int                     in_hlds_num;
        int                     in_arg_num;
        const char              *in_name;
        int                     start_of_num;
        int                     in_numerical_suffix;
        int                     head_var_num;
        int                     call_var_num;
        int                     call_num_vars;
        const MR_LabelLayout    *call_label;
        MR_bool                 found;

        if (should_copy_out) {
            strncpy(out_name_buf, out_name, sizeof(out_name_buf) - 1);
            out_name_buf[sizeof(out_name_buf) - 1] = '\\0';
            out_name = (const char *) out_name_buf;
        }

        start_of_num = MR_find_start_of_num_suffix(out_name);
        if (start_of_num < 0) {
            out_base_name_len = strlen(out_name);
            out_numerical_suffix = -1;
        } else {
            out_base_name_len = start_of_num;
            out_numerical_suffix = atoi(out_name + start_of_num);
        }

        num_matches = 0;
        in_arg_num = -1;

        for (head_var_num = 0; head_var_num < proc->MR_sle_num_head_vars;
            head_var_num++)
        {
            in_hlds_num = proc->MR_sle_head_var_nums[head_var_num];
            in_name = MR_hlds_var_name(proc, in_hlds_num, NULL);
            if (in_name == NULL || MR_streq(in_name, """")) {
                continue;
            }

            start_of_num = MR_find_start_of_num_suffix(in_name);
            if (start_of_num < 0) {
                continue;
            }

            if (! (
                    (
                        /*
                        ** The names are exactly the same except
                        ** for the numerical suffix.
                        */
                        start_of_num == out_base_name_len &&
                        MR_strneq(out_name, in_name, start_of_num)
                    )
                ||
                    (
                        /*
                        ** The names are exactly the same except
                        ** for an underscore and the numerical suffix
                        ** (as is the case with state variable notation).
                        */
                        start_of_num == out_base_name_len + 1 &&
                        start_of_num > 0 &&
                        in_name[start_of_num - 1] == '_' &&
                        MR_strneq(out_name, in_name, start_of_num - 1)
                    )
                ))
            {
                continue;
            }

            in_numerical_suffix = atoi(in_name + start_of_num);
            if (! ((in_numerical_suffix >= out_numerical_suffix)
                || (out_numerical_suffix < 0)))
            {
                continue;
            }

            call_label = proc->MR_sle_call_label;
            if (! MR_has_valid_var_count(call_label)) {
                    continue;
            }

            if (! MR_has_valid_var_info(call_label)) {
                continue;
            }

            /*
            ** The in_hlds_num has the same prefix as the output variable.
            ** Check if in_hlds_num is an input argument.
            */
            call_num_vars = MR_all_desc_var_count(call_label);
            found = MR_FALSE;
            for (call_var_num = 0 ; call_var_num < call_num_vars;
                    call_var_num++)
            {
                if (call_label->MR_sll_var_nums[call_var_num] == in_hlds_num) {
                    found = MR_TRUE;
                    break;
                }
            }

            if (! found) {
                continue;
            }

            num_matches++;
            in_arg_num = head_var_num;
        }

        if (num_matches == 1) {
            InArgNum = in_arg_num + 1;
            SUCCESS_INDICATOR = MR_TRUE;
        } else {
            SUCCESS_INDICATOR = MR_FALSE;
        }
    }
").

:- pragma foreign_proc("C",
    get_all_modes_for_layout(Layout::in) = (Layouts::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    const MR_ModuleLayout   *module;
    const MR_ProcLayout     *proc;
    int                     i;
    MR_Word                 list;
    MR_bool                 match;
    const MR_ProcLayout     *selected_proc;

    selected_proc = Layout;

    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(selected_proc)) {
        MR_fatal_error(""get_all_modes_for_layout: selected_proc"");
    }

    module = selected_proc->MR_sle_module_layout;
    list = MR_list_empty();
    for (i = 0; i < module->MR_ml_proc_count; i++) {
        proc = module->MR_ml_procs[i];
        if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(selected_proc)) {
            MR_fatal_error(""get_all_modes_for_layout: proc"");
        }

        if (MR_PROC_LAYOUT_IS_UCI(selected_proc)
            && MR_PROC_LAYOUT_IS_UCI(proc))
        {
            const MR_UCIProcId  *proc_id;
            const MR_UCIProcId  *selected_proc_id;

            proc_id = &proc->MR_sle_uci;
            selected_proc_id = &selected_proc->MR_sle_uci;

            if (MR_streq(proc_id->MR_uci_type_name,
                selected_proc_id->MR_uci_type_name)
            && MR_streq(proc_id->MR_uci_type_module,
                selected_proc_id->MR_uci_type_module)
            && MR_streq(proc_id->MR_uci_pred_name,
                selected_proc_id->MR_uci_pred_name)
            && (proc_id->MR_uci_type_arity ==
                selected_proc_id->MR_uci_type_arity))
            {
                match = MR_TRUE;
            } else {
                match = MR_FALSE;
            }
        } else if (!MR_PROC_LAYOUT_IS_UCI(selected_proc)
            && !MR_PROC_LAYOUT_IS_UCI(proc))
        {
            const MR_UserProcId *proc_id;
            const MR_UserProcId *selected_proc_id;

            proc_id = &proc->MR_sle_user;
            selected_proc_id = &selected_proc->MR_sle_user;

            if ((proc_id->MR_user_pred_or_func ==
                selected_proc_id->MR_user_pred_or_func)
            && MR_streq(proc_id->MR_user_decl_module,
                selected_proc_id->MR_user_decl_module)
            && MR_streq(proc_id->MR_user_name,
                selected_proc_id->MR_user_name)
            && (proc_id->MR_user_arity ==
                selected_proc_id->MR_user_arity))
            {
                match = MR_TRUE;
            } else {
                match = MR_FALSE;
            }
        } else {
            match = MR_FALSE;
        }

        if (match) {
            list = MR_int_list_cons((MR_Integer) proc, list);
        }
    }

    Layouts = list;
").

:- pragma foreign_proc("C",
    containing_proc_layout(LabelLayout::in) = (ProcLayout::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ProcLayout = LabelLayout->MR_sll_entry;
").

:- pragma foreign_proc("C",
    proc_bytecode_bytes(ProcLayout::in) = (ByteCodeBytes::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ByteCodeBytes = ProcLayout->MR_sle_body_bytes;
#ifdef MR_DEBUG_PROC_REP
    printf(""lookup_proc_bytecode: %p %p\\n"", ProcLayout, ByteCodeBytes);
#endif
").

:- pragma foreign_proc("C#",
    proc_bytecode_bytes(_ProcLayout::in) = (_ByteCodeBytes::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    throw new System.Exception(\"not supported in C# grade\");
").

:- pragma foreign_proc("Java",
    proc_bytecode_bytes(_ProcLayout::in) = (_ByteCodeBytes::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    if (1 == 1) throw new Error(\"not supported in java grade\");
").

:- pragma foreign_proc("Erlang",
    proc_bytecode_bytes(_ProcLayout::in) = (ByteCodeBytes::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ByteCodeBytes = 0,  % Avoid a warning.
    throw({""not supported in erlang grade""})
").

    % Default version for non-C backends.
proc_bytecode_bytes(_) = dummy_bytecode_bytes.

%---------------------------------------------------------------------------%

:- pragma foreign_type("C", module_layout, "const MR_ModuleLayout *",
    [can_pass_as_mercury_type, stable]).
    % The following definitions are only stubs.
:- pragma foreign_type("C#", module_layout, "object", []).
:- pragma foreign_type("Java", module_layout, "java.lang.Object", []).
:- pragma foreign_type("Erlang", module_layout, "").

:- pragma foreign_type("C", string_table_chars, "MR_ConstString",
    [can_pass_as_mercury_type, stable]).
    % The following definitions are only stubs.
:- pragma foreign_type("C#", string_table_chars, "object", []).
:- pragma foreign_type("Java", string_table_chars, "java.lang.Object", []).
:- pragma foreign_type("Erlang", string_table_chars, "").

:- pragma foreign_proc("C",
    containing_module_layout(ProcLayout::in, ModuleLayout::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    if (MR_PROC_LAYOUT_HAS_THIRD_GROUP(ProcLayout)) {
        ModuleLayout = ProcLayout->MR_sle_module_layout;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

module_string_table(ModuleLayout) = StringTable :-
    module_string_table_components(ModuleLayout, StringTableChars, Size),
    StringTable = string_table(StringTableChars, Size).

:- pred module_string_table_components(module_layout::in,
    string_table_chars::out, int::out) is det.

:- pragma foreign_proc("C",
    module_string_table_components(ModuleLayout::in,
        StringTableChars::out, Size::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    StringTableChars = ModuleLayout->MR_ml_string_table;
    Size = ModuleLayout->MR_ml_string_table_size;
").

lookup_string_table(StringTable, NameCode) = Str :-
    StringTable = string_table(StringTableChars, Size),
    Str = lookup_string_table_2(StringTableChars, Size, NameCode).

:- func lookup_string_table_2(string_table_chars, int, int) = string.

:- pragma foreign_proc("C",
    lookup_string_table_2(StringTableChars::in, StringTableSize::in,
        NameCode::in) = (Str::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    MR_ConstString  str0;
    int             should_copy;

    str0 = MR_name_in_string_table(StringTableChars, StringTableSize,
        (MR_uint_least32_t)NameCode, &should_copy);
    if (should_copy) {
        MR_make_aligned_string(Str, str0);
    } else {
        MR_make_aligned_string_copy(Str, str0);
    }
").

%---------------------------------------------------------------------------%

read_byte(ByteCode, Value, !Pos) :-
    ByteCode = bytecode(Bytes, Size),
    !.Pos + 1 =< Size,
    read_byte_2(Bytes, Value, !Pos).

:- pred read_byte_2(bytecode_bytes::in, int::out, int::in, int::out) is det.

:- pragma foreign_proc("C",
    read_byte_2(ByteCode::in, Value::out, Pos0::in, Pos::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Value = ByteCode[Pos0];
    Pos = Pos0 + 1;
").

read_short(ByteCode, Value, !Pos) :-
    ByteCode = bytecode(Bytes, Size),
    !.Pos + 2 =< Size,
    read_short_2(Bytes, Value, !Pos).

:- pred read_short_2(bytecode_bytes::in, int::out, int::in, int::out) is det.

:- pragma foreign_proc("C",
    read_short_2(ByteCode::in, Value::out, Pos0::in, Pos::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Value = (ByteCode[Pos0] << 8) + ByteCode[Pos0+1];
    Pos = Pos0 + 2;
").

read_int32(ByteCode, Value, !Pos) :-
    ByteCode = bytecode(Bytes, Size),
    !.Pos + 4 =< Size,
    read_int32_2(Bytes, Value, !Pos).

:- pred read_int32_2(bytecode_bytes::in, int::out, int::in, int::out) is det.

:- pragma foreign_proc("C",
    read_int32_2(ByteCode::in, Value::out, Pos0::in, Pos::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Value = (ByteCode[Pos0] << 24) + (ByteCode[Pos0+1] << 16) +
        (ByteCode[Pos0+2] << 8) + ByteCode[Pos0+3];
    Pos = Pos0 + 4;
").

read_num(ByteCode, Num, !Pos) :-
    read_num_2(ByteCode, 0, Num, !Pos).

:- pred read_num_2(bytecode::in, int::in, int::out, int::in, int::out)
    is semidet.

read_num_2(ByteCode, Num0, Num, !Pos) :-
    read_byte(ByteCode, Byte, !Pos),
    Num1 = (Num0 << 7) \/ (Byte /\ 0x7F),
    ( if Byte /\ 0x80 = 0 then
        Num = Num1
    else
        read_num_2(ByteCode, Num1, Num, !Pos)
    ).

read_string_via_offset(ByteCode, StringTable, String, !Pos) :-
    read_int32(ByteCode, Offset, !Pos),
    String = lookup_string_table(StringTable, Offset).

read_line(ByteCode, Line, !Pos) :-
    read_line_2(ByteCode, [], RevChars, !Pos),
    string.from_rev_char_list(RevChars, Line).

:- pred read_line_2(bytecode::in, list(char)::in, list(char)::out,
    int::in, int::out) is semidet.

read_line_2(ByteCode, !RevChars, !Pos) :-
    read_byte(ByteCode, Byte, !Pos),
    char.from_int(Byte, Char),
    ( if Char = '\n' then
        !:RevChars = [Char | !.RevChars]
    else
        !:RevChars = [Char | !.RevChars],
        read_line_2(ByteCode, !RevChars, !Pos)
    ).

read_len_string(ByteCode, String, !Pos) :-
    read_num(ByteCode, Length, !Pos),
    read_len_string_2(ByteCode, Length, [], RevChars, !Pos),
    string.from_rev_char_list(RevChars, String).

:- pred read_len_string_2(bytecode::in, int::in,
    list(char)::in, list(char)::out, int::in, int::out) is semidet.

read_len_string_2(ByteCode, N, !RevChars, !Pos) :-
    ( if N =< 0 then
        true
    else
        read_byte(ByteCode, Byte, !Pos),
        char.from_int(Byte, Char),
        !:RevChars = [Char | !.RevChars],
        read_len_string_2(ByteCode, N - 1, !RevChars, !Pos)
    ).

read_string_table(ByteCode, StringTable, !Pos) :-
    read_num(ByteCode, Size, !Pos),
    ByteCode = bytecode(Bytes, NumBytes),
    !.Pos + Size =< NumBytes,
    bytecode_string_table_2(Bytes, !.Pos, Size, StringTableChars),
    !:Pos = !.Pos + Size,
    StringTable = string_table(StringTableChars, Size).

:- pred bytecode_string_table_2(bytecode_bytes::in, Offset::in, Size::in,
    string_table_chars::out) is det.

:- pragma foreign_proc("C",
    bytecode_string_table_2(Bytes::in, Offset::in, Size::in,
        StringTableChars::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    char        *buf;
    char        *table;
    MR_Unsigned i;

    MR_allocate_aligned_string_msg(buf, Size, MR_ALLOC_ID);
    table = ((char *) Bytes) + Offset;
    for (i = 0; i < Size; i++) {
        buf[i] = table[i];
    }

    StringTableChars = (MR_ConstString) buf;
").

%---------------------------------------------------------------------------%

encode_byte(Byte, [Byte]) :-
    Byte >= 0,
    Byte < 128.

encode_byte_det(Byte, Bytes) :-
    ( if encode_byte(Byte, BytesPrime) then
        Bytes = BytesPrime
    else
        unexpected($pred, "encode_byte failed")
    ).

encode_byte_func(Byte) = Bytes :-
    encode_byte_det(Byte, Bytes).

encode_short(Short, [Byte1, Byte2]) :-
    Short >= 0,
    Byte2 = Short /\ 255,
    Byte1 = Short / 256,
    Byte1 < 128.

encode_short_det(Short, Bytes) :-
    ( if encode_short(Short, BytesPrime)then
        Bytes = BytesPrime
    else
        unexpected($pred, "encode_short failed")
    ).

encode_short_func(Short) = Bytes :-
    encode_short_det(Short, Bytes).

encode_int32(Int32, [Byte1, Byte2, Byte3, Byte4]) :-
    Int32 >= 0,
    Byte4 = Int32 /\ 255,
    Bytes123 = Int32 / 256,
    Byte3 = Bytes123 /\ 255,
    Bytes12 = Bytes123 / 256,
    Byte2 = Bytes12 /\ 255,
    Byte1 = Bytes12 / 256,
    Byte1 < 128.

encode_int32_det(Int32, Bytes) :-
    ( if encode_int32(Int32, BytesPrime) then
        Bytes = BytesPrime
    else
        unexpected($pred, "encode_int32 failed")
    ).

encode_int32_func(Int32) = Bytes :-
    encode_int32_det(Int32, Bytes).

encode_num(Num, Bytes) :-
    Num >= 0,
    LastByte = Num /\ 127,
    NextNum = Num / 128,
    encode_num_2(NextNum, [LastByte], Bytes).

:- pred encode_num_2(int::in, list(int)::in, list(int)::out) is det.

encode_num_2(Num, RestBytes, Bytes) :-
    ( if Num = 0 then
        Bytes = RestBytes
    else
        CurByte = (Num /\ 127) \/ 128,
        NextNum = Num / 128,
        encode_num_2(NextNum, [CurByte | RestBytes], Bytes)
    ).

encode_num_det(Num, Bytes) :-
    ( if encode_num(Num, BytesPrime)then
        Bytes = BytesPrime
    else
        unexpected($pred, "encode_num failed")
    ).

encode_num_func(Num) = Bytes :-
    encode_num_det(Num, Bytes).

encode_len_string(String, Bytes) :-
    string.length(String, Length),
    encode_num_det(Length, LengthBytes),
    string.to_char_list(String, Chars),
    CharBytes = list.map(char.to_int, Chars),
    Bytes = LengthBytes ++ CharBytes.

encode_len_string_func(String) = Bytes :-
    encode_len_string(String, Bytes).

%---------------------------------------------------------------------------%
:- end_module mdbcomp.rtti_access.
%---------------------------------------------------------------------------%
