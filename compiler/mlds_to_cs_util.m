%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Utility services required by the other mlds_to_cs_*.m modules.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

    % Keep the enum fields together, so they can be packed into one word.
:- type csharp_out_info
    --->    csharp_out_info(
                % These are static.
                csoi_module_info            :: module_info,
                csoi_auto_comments          :: bool,
                csoi_line_numbers           :: bool,
                csoi_foreign_line_numbers   :: bool,
                csoi_module_name            :: mlds_module_name,
                csoi_source_filename        :: string,
                csoi_code_addrs             :: map(mlds_code_addr, string),

                % These are dynamic.
                csoi_output_generics        :: output_generics,
                csoi_break_context          :: break_context,
                csoi_univ_tvars             :: list(tvar)
            ).

:- func init_csharp_out_info(module_info, string, map(mlds_code_addr, string))
    = csharp_out_info.

%---------------------------------------------------------------------------%

:- pred cs_output_context(io.text_output_stream::in, bool::in,
    prog_context::in, io::di, io::uo) is det.

:- pred cs_output_default_context(io.text_output_stream::in, bool::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred maybe_output_inline_comment_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_pragma_warning_disable(io.text_output_stream::in,
    io::di, io::uo) is det.

:- pred output_pragma_warning_restore(io.text_output_stream::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

init_csharp_out_info(ModuleInfo, SourceFileName, CodeAddrs) = Info :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, line_numbers_around_foreign_code,
        ForeignLineNumbers),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    Info = csharp_out_info(ModuleInfo, AutoComments,
        LineNumbers, ForeignLineNumbers, MLDS_ModuleName, SourceFileName,
        CodeAddrs, do_not_output_generics,  bc_none, []).

%---------------------------------------------------------------------------%

cs_output_context(Stream, OutputLineNumbers, Context, !IO) :-
    (
        OutputLineNumbers = yes,
        ( if is_dummy_context(Context) then
            true
        else
            Context = term_context.context(File, Line),
            io.format(Stream, "#line %d ""%s""\n", [i(Line), s(File)], !IO)
        )
    ;
        OutputLineNumbers = no
    ).

cs_output_default_context(Stream, OutputLineNumbers, !IO) :-
    (
        OutputLineNumbers = yes,
        io.write_string(Stream, "#line default\n", !IO)
    ;
        OutputLineNumbers = no
    ).

%---------------------------------------------------------------------------%

maybe_output_inline_comment_for_csharp(Info, Stream, Comment, !IO) :-
    AutoComments = Info ^ csoi_auto_comments,
    (
        AutoComments = yes,
        io.format(Stream, "/* %s */ ", [s(Comment)], !IO)
    ;
        AutoComments = no
    ).

%---------------------------------------------------------------------------%

output_pragma_warning_disable(Stream, !IO) :-
    % CS0162: Unreachable code detected.
    % CS0168: The variable `foo' is declared but never used.
    % CS0169: The private method `foo' is never used.
    % CS0219: The variable `foo' is assigned but its value is never used.
    % CS1717: Assignment made to same variable.
    io.write_string(Stream,
        "#pragma warning disable 162, 168, 169, 219, 1717\n", !IO).

output_pragma_warning_restore(Stream, !IO) :-
    io.write_string(Stream, "#pragma warning restore\n", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_util.
%---------------------------------------------------------------------------%
