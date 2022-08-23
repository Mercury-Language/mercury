%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Utility services required by the other mlds_to_c*.m modules.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_util.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.

    % This type concentrates the values of all the options that this module
    % needs, in a form that can be looked up much more quickly than by calling
    % lookup_bool_option.
    %
    % The first batch of fields contains all the non-enum fields, to allow
    % all the enum fields to be stored in a single word. (We need the whole
    % globals as a field, despite storing the option values we want separately,
    % because module_name_to_file_name takes Globals as an argument.)
    %
    % Each field in the second batch of fields is named after the option
    % whose value it holds, though the value of the m2co_line_numbers field
    % is overridden by the value of the line_numbers_for_c_headers when
    % generating C header files.
    %
    % m2co_need_to_init is set to `yes' if any of the profile_calls,
    % profile_memory and profile_time fields is `yes'. This is because
    % we need to output calls to MR_init_entry if any form of profiling
    % is enabled. (It would be OK to output the calls regardless,
    % since they will macro-expand to nothing if profiling is not enabled,
    % but for readability of the generated code we prefer not to.)
    %
    % m2co_target_or_dump says whether we are generating target cod
    % or an MLDS dump. We permit nested functions in MLDS code only
    % when printing MLDS dumps, not when generating target code.
    %
    % m2co_std_func_decl is `yes' if want to use standard argument names
    % in function declarations.
    %
    % We use m2co_break_context to check whether goto_break_{switch,loop}s
    % are used in the intended contexts.
    %
:- type mlds_to_c_opts
    --->    mlds_to_c_opts(
                m2co_all_globals            :: globals,
                m2co_source_filename        :: string,

                m2co_line_numbers           :: bool,
                m2co_foreign_line_numbers   :: bool,
                m2co_auto_comments          :: bool,
                m2co_single_prec_float      :: bool,
                m2co_profile_calls          :: bool,
                m2co_profile_memory         :: bool,
                m2co_profile_time           :: bool,
                m2co_need_to_init           :: bool,
                m2co_target                 :: compilation_target,
                m2co_gc_method              :: gc_method,

                m2co_target_or_dump         :: target_or_dump,

                m2co_std_func_decl          :: bool,

                m2co_break_context          :: break_context
            ).

:- type target_or_dump
    --->    tod_target
    ;       tod_dump.

:- func init_mlds_to_c_opts(globals, string, target_or_dump) = mlds_to_c_opts.

%---------------------------------------------------------------------------%

:- pred c_output_stmt_context(io.text_output_stream::in, bool::in,
    mlds_stmt::in, io::di, io::uo) is det.
:- pred c_output_context(io.text_output_stream::in, bool::in,
    prog_context::in, io::di, io::uo) is det.
:- pred c_output_file_line(io.text_output_stream::in, bool::in,
    string::in, int::in, io::di, io::uo) is det.
:- pred c_reset_context(io.text_output_stream::in, bool::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type decl_or_defn
    --->    forward_decl
    ;       definition.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module libs.options.
:- import_module ml_backend.ml_util.

:- import_module maybe.
:- import_module term_context.

%---------------------------------------------------------------------------%

init_mlds_to_c_opts(Globals, SourceFileName, TargetOrDump) = Opts :-
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, line_numbers_around_foreign_code,
        ForeignLineNumbers),
    globals.lookup_bool_option(Globals, auto_comments, Comments),
    globals.lookup_bool_option(Globals, single_prec_float, SinglePrecFloat),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_memory, ProfileMemory),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    ( if
        ( ProfileCalls = yes
        ; ProfileMemory = yes
        ; ProfileTime = yes
        )
    then
        NeedToInit = yes
    else
        NeedToInit = no
    ),
    globals.get_target(Globals, Target),
    globals.get_gc_method(Globals, GCMethod),
    StdFuncDecls = no,
    BreakContext = bc_none,
    Opts = mlds_to_c_opts(Globals, SourceFileName,
        LineNumbers, ForeignLineNumbers, Comments,
        SinglePrecFloat, ProfileCalls, ProfileMemory, ProfileTime, NeedToInit,
        Target, GCMethod, TargetOrDump, StdFuncDecls, BreakContext).

%---------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations (#line directives).
%

c_output_stmt_context(Stream, OutputLineNumbers, Stmt, !IO) :-
    (
        OutputLineNumbers = yes,
        Context = get_mlds_stmt_context(Stmt),
        FileName = term_context.context_file(Context),
        LineNumber = term_context.context_line(Context),
        c_util.always_set_line_num(Stream, FileName, LineNumber, !IO)
    ;
        OutputLineNumbers = no
    ).

c_output_context(Stream, OutputLineNumbers, Context, !IO) :-
    (
        OutputLineNumbers = yes,
        FileName = term_context.context_file(Context),
        LineNumber = term_context.context_line(Context),
        c_util.always_set_line_num(Stream, FileName, LineNumber, !IO)
    ;
        OutputLineNumbers = no
    ).

c_output_file_line(Stream, OutputLineNumbers, FileName, LineNumber, !IO) :-
    (
        OutputLineNumbers = yes,
        c_util.always_set_line_num(Stream, FileName, LineNumber, !IO)
    ;
        OutputLineNumbers = no
    ).

c_reset_context(Stream, OutputLineNumbers, !IO) :-
    (
        OutputLineNumbers = yes,
        c_util.always_reset_line_num(Stream, no, !IO)
    ;
        OutputLineNumbers = no
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_util.
%---------------------------------------------------------------------------%
