%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Utility services required by the other mlds_to_java*.m modules.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_util.
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
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type code_addr_wrapper
    --->    code_addr_wrapper(
                caw_class           :: string,
                caw_ptr_num         :: maybe(int)
            ).

    % Keep the enum fields together, so they can be packed into one word.
:- type java_out_info
    --->    java_out_info(
                % These are static.
                joi_module_info     :: module_info,
                joi_auto_comments   :: bool,
                joi_line_numbers    :: bool,
                joi_foreign_line_numbers :: bool,
                joi_module_name     :: mlds_module_name,
                joi_source_filename :: string,
                joi_addrof_map      :: map(mlds_code_addr, code_addr_wrapper),

                % These are dynamic.
                joi_output_generics :: output_generics,
                joi_break_context   :: break_context,
                joi_univ_tvars      :: list(tvar)
            ).

:- func init_java_out_info(module_info, string,
    map(mlds_code_addr, code_addr_wrapper)) = java_out_info.

:- func get_debug_class_init(java_out_info) = bool.

%---------------------------------------------------------------------------%

:- type context_marker
    --->    marker_begin_block
            % The beginning of some Java foreign code whose errors
            % should be reported with Mercury line numbers.

    ;       marker_end_block
            % The end of such a block.

    ;       marker_comment.
            % This marks Mercury generated code for which Java's line numbers
            % should be used, it is just a comment for the Mercury developers.

:- pred output_context_for_java(bool::in, context_marker::in,
    prog_context::in, io::di, io::uo) is det.

:- pred indent_line_after_context(bool::in, context_marker::in,
    prog_context::in, indent::in, io::di, io::uo) is det.

:- pred write_string_with_context_block(java_out_info::in, indent::in,
    string::in, prog_context::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred maybe_output_comment_for_java(java_out_info::in, string::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred is_specialised_method_ptr_arity(int::in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

init_java_out_info(ModuleInfo, SourceFileName, AddrOfMap) = Info :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, line_numbers_around_foreign_code,
        ForeignLineNumbers),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    Info = java_out_info(ModuleInfo, AutoComments,
        LineNumbers, ForeignLineNumbers, MLDS_ModuleName, SourceFileName,
        AddrOfMap, do_not_output_generics, bc_none, []).

get_debug_class_init(Info) = DebugClassInit :-
    % It is not worth having an extra field in Info for this option,
    % since we only look it up twice per module.
    ModuleInfo = Info ^ joi_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    lookup_bool_option(Globals, debug_class_init, DebugClassInit).

%---------------------------------------------------------------------------%

:- mutable(last_context, prog_context, context_init, ground,
    [untrailed, attach_to_io_state]).

output_context_for_java(OutputLineNumbers, Marker, ProgContext, !IO) :-
    (
        OutputLineNumbers = yes,
        get_last_context(LastContext, !IO),
        term.context_file(ProgContext, File),
        term.context_line(ProgContext, Line),
        ( if
            % It is safe to ignore marker comments when the comment isn't
            % useful. All other marker types must be emitted in all cases.
            (
                Marker = marker_comment
            =>
                (
                    ProgContext \= LastContext,
                    Line > 0,
                    File \= ""
                )
            )
        then
            % Java doesn't have an equivalent of #line directives.
            % We use the token MER_LINE to allow us to filter these lines out
            % of the file when mangling javac's output.
            % \u is treated as a Unicode escape even with comments.
            string.replace_all(File, "\\u", "\\\\u", SafePath),
            % Do not modify this format string without modifying
            % mfilterjavac/mfilterjavac.m.
            io.format("// %s %s:%d\n",
                [s(marker_string(Marker)), s(SafePath), i(Line)], !IO),
            set_last_context(ProgContext, !IO)
        else
            true
        )
    ;
        OutputLineNumbers = no
    ).

    % Do not modify these strings without modifying util/mfilterjavac.m.
    %
:- func marker_string(context_marker) = string.

marker_string(marker_begin_block) = "MER_FOREIGN_BEGIN".
marker_string(marker_end_block) = "MER_FOREIGN_END".
marker_string(marker_comment) = "".

%---------------------------------------------------------------------------%

indent_line_after_context(OutputLineNumbers, Marker, Context, N, !IO) :-
    output_context_for_java(OutputLineNumbers, Marker, Context, !IO),
    output_n_indents(N, !IO).

write_string_with_context_block(Info, Indent, Code, Context, !IO) :-
    indent_line_after_context(Info ^ joi_foreign_line_numbers,
        marker_begin_block, Context, Indent, !IO),
    io.write_string(Code, !IO),
    io.nl(!IO),
    % The num_lines(Code) call is supposed to count the number of lines
    % occupied by Code in the source file. The result will be incorrect if
    % there were any escape sequences representing CR or LF characters --
    % they are expanded out in Code.
    Context = context(File, Lines0),
    ContextEnd = context(File, Lines0 + num_lines(Code)),
    indent_line_after_context(Info ^ joi_foreign_line_numbers,
        marker_end_block, ContextEnd, Indent, !IO).

:- func num_lines(string) = int.

num_lines(String) = Num :-
    % The initial "previous" character may be anything other than \r.
    string.foldl2(count_new_lines, String, 1, Num, 'x', _).

    % Increment the line count !N whenever we see CR or LF or CRLF,
    % ensuring that the latter counts as only ONE newline.
    %
:- pred count_new_lines(char::in, int::in, int::out, char::in, char::out)
    is det.

count_new_lines(C, !N, Prev, C) :-
    ( if
        (
            C = '\r'
        ;
            (
                C = '\n',
                Prev \= '\r'
            )
        )
    then
        !:N = !.N + 1
    else
        true
    ).

%---------------------------------------------------------------------------%

maybe_output_comment_for_java(Info, Comment, !IO) :-
    AutoComments = Info ^ joi_auto_comments,
    (
        AutoComments = yes,
        io.write_string("/* ", !IO),
        io.write_string(Comment, !IO),
        io.write_string(" */", !IO)
    ;
        AutoComments = no
    ).

%---------------------------------------------------------------------------%

is_specialised_method_ptr_arity(Arity) :-
    Arity > 0,  % No specialised method ptr for arity zero predicates.
    Arity =< max_specialised_method_ptr_arity.

    % The highest arity for which there is a specialised MethodPtr<n>
    % interface.
    %
:- func max_specialised_method_ptr_arity = int.

max_specialised_method_ptr_arity = 15.

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_util.
%---------------------------------------------------------------------------%
