%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rtti_access.m.
% Main authors: zs, maclarty
%
% This module contains an interface to the label_layout and proc_layout
% types which are used in the C backend of the debugger.

:- module mdbcomp.rtti_access.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
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

:- type proc_layout. 

:- func get_proc_label_from_layout(proc_layout) = proc_label.

:- func get_proc_name(proc_label) = string.

    % find_initial_version_arg_num(Proc, OutputArgNum, InputArgNum).
    % Given a procedure and an output argument number of that procedure,
    % find an input argument which has the same name as the output argument,
    % expect for a numerical suffix and possibly an underscore.  The output
    % argument name needn't have a numerical suffix, but if it does, then the
    % input argument's numerical suffix should be less that the numerical
    % suffix of the output argument.  This procedure is used as a heuristic to
    % determine when it is worth checking if a subterm appearing in the output
    % argument also appears in the same position in the input argument.  The
    % heuristic is used by the subterm dependency tracking algorithm to help
    % speed up the search.
    % Argument numbers start at one.
    % This procedure is implemented in C to avoid having to allocate
    % memory to import non word-aligned strings into Mercury code.
    %
:- pred find_initial_version_arg_num(proc_layout::in, int::in, int::out)
    is semidet.

:- func get_all_modes_for_layout(proc_layout) = list(proc_layout).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

:- pragma foreign_type("C", label_layout, "const MR_Label_Layout *",
    [can_pass_as_mercury_type, stable]).

    % stub only
:- pragma foreign_type("Java", label_layout, "java.lang.Object", []). 

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
    ( path_from_string(GoalPathStr, ValidGoalPath) ->
        GoalPath = ValidGoalPath
    ;
        error("get_path_port_from_label_layout: invalid goal path")
    ),
    PathPort = make_path_port(GoalPath, Port).

:- pragma foreign_type("C", proc_layout, "const MR_Proc_Layout *",
    [can_pass_as_mercury_type, stable]).
:- pragma foreign_type("Java", proc_layout, "java.lang.Object", []). %stub only

get_proc_label_from_layout(Layout) = ProcLabel :-
    ( proc_layout_is_uci(Layout) ->
        proc_layout_get_uci_fields(Layout, TypeName, TypeModule,
            DefModule, PredName, TypeArity, ModeNum),
        ( special_pred_name_arity(SpecialIdPrime, _, PredName, _) ->
            SpecialId = SpecialIdPrime
        ;
            error("get_proc_label_from_layout: bad special_pred_id")
        ),
        string_to_sym_name(DefModule, ".", SymDefModule),
        string_to_sym_name(TypeModule, ".", SymTypeModule),
        ProcLabel = special_proc_label(SymDefModule, SpecialId, 
            SymTypeModule, TypeName, TypeArity, ModeNum)
    ;
        proc_layout_get_non_uci_fields(Layout, PredOrFunc,
            DeclModule, DefModule, PredName, Arity, ModeNum),
        string_to_sym_name(DefModule, ".", SymDefModule),
        string_to_sym_name(DeclModule, ".", SymDeclModule),
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
    const MR_UCI_Proc_Id    *proc_id;

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
    const MR_User_Proc_Id   *proc_id;

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
    const MR_Proc_Layout    *proc;
    int         out_hlds_num;
    const char      *out_name;

    proc = Layout;

    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc)) {
        MR_fatal_error(""find_initial_version_arg_num: proc"");
    }

    out_hlds_num = proc->MR_sle_head_var_nums[OutArgNum - 1];
    out_name = MR_hlds_var_name(proc, out_hlds_num);
    if (out_name == NULL || MR_streq(out_name, """")) {
        /* out_hlds_num was not named by the user */
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
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
        const MR_Label_Layout   *call_label;
        MR_bool                 found;

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
            in_name = MR_hlds_var_name(proc, in_hlds_num);
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
                        strneq(out_name, in_name, start_of_num)
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
                        strneq(out_name, in_name, start_of_num - 1)
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
    const MR_Module_Layout  *module;
    const MR_Proc_Layout    *proc;
    int                     i;
    MR_Word                 list;
    MR_bool                 match;
    const MR_Proc_Layout    *selected_proc;

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
            const MR_UCI_Proc_Id    *proc_id;
            const MR_UCI_Proc_Id    *selected_proc_id;

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
            const MR_User_Proc_Id   *proc_id;
            const MR_User_Proc_Id   *selected_proc_id;

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

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.rtti_access.
%-----------------------------------------------------------------------------%
