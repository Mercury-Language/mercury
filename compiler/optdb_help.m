%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: optdb_help.m.
% Main author: zs.

:- module libs.optdb_help.
:- interface.

:- import_module char.
:- import_module list.

:- type help
    --->    no_help
    ;       gen_help(
                % Every character in this field is a short name of the option.
                % The order of these short options here will be preserved
                % in the output.
                gh_short_names          :: list(char),

                % The long name of the option. This will not contain
                % - the "--" prefix,
                % - any "no-" prefix even if the option is negateable
                %   and the default is negated, or
                % - the name of an argument.
                gh_long_name            :: string,

                % Any alternate long names of the option.
                %
                % We have many options that have both British and American
                % spelling of the option name, and some have both
                % short and long versions.
                % XXX We should decide on a consistent order:
                % - should British or American spelling come first?
                % - should we go from long versions to short, or vice versa?
                %
                % The order of these alt options here will be preserved
                % in the output. They will all follow gh_long_name.
                gh_alt_long_names       :: list(string),

                % Is the option's documentation printed for users, or not?
                gh_public_or_private    :: help_public_or_private,

                % The lines describing the effect of the option.
                gh_description          :: help_text
            )

    % The following function symbols (whose list may grow)
    % mostly contain a subset of the fields of gen_help.
    % The main exceptions are the function symbols with "align"
    % in the their names. These are intended for the documentation of
    % grade options, which have the usual option description on the left,
    % and a list of the relevant base grades or grade modifiers
    % in an aligned block on the right.
    %
    % All such aligned text has two separate versions, with the first
    % being intended for help text and the second intended for texinfo.
    % This is simpler than trying to come up with algorithm for e.g.
    % automatically deriving the help text version from the texinfo version.
    %
    % The fields that the function symbols below contain that do occur
    % in gen_help, have the same semantics as in gen_help.
    % The fields they do not contain implicitly default to "[]" for
    % alternate names, and will get their public vs private status
    % from the function symbol name.
    %
    % This design minimizes clutter in lists of help structures.

    ;       help(
                h_long_name             :: string,
                h_description           :: help_text
            )
    ;       arg_help(
                hr_long_name            :: string,
                hr_arg_name             :: string,
                hr_description          :: help_text
            )
    ;       priv_help(
                ph_long_name            :: string,
                ph_description          :: help_text
            )
    ;       priv_arg_help(
                prh_long_name           :: string,
                prh_arg_name            :: string,
                prh_description         :: help_text
            )
    ;       alt_help(
                ah_long_name            :: string,
                ah_alt_long_names       :: list(string),
                ah_description          :: help_text
            )

    ;       alt_arg_align_help(
                arlh_long_name          :: string,
                arlh_arg_aligns         :: list(arg_align),
                arlh_description        :: help_text
            )
    ;       alt_align_help(
                alh_long_name           :: string,
                alh_alt_long_names      :: list(string),
                alh_aligned_help_text   :: string,
                alh_aligned_texinfo     :: string,
                alh_description         :: help_text
            )
    ;       no_align_help(
                nlh_long_name           :: string,
                nlh_aligned_help_text   :: string,
                alh_no_aligned_help_text :: string, % for --no-long-name
                nlh_aligned_texinfo     :: string,
                alh_no_aligned_texinfo  :: string,  % for --no-long-name
                nlh_description         :: help_text
            )
    ;       priv_alt_align_help(
                palh_long_name          :: string,
                palh_alt_long_names     :: list(string),
                palh_aligned_help_text  :: string,
                palh_aligned_texinfo    :: string,
                palh_description        :: help_text
            )
    ;       short_alt_align_help(
                salh_short_name         :: char,
                salh_long_name          :: string,
                salh_alt_long_names     :: list(string),
                salh_aligned_help_text  :: string,
                salh_aligned_texinfo    :: string,
                salh_description        :: help_text
            )
    ;       alt_arg_help(
                aah_long_name           :: string,
                aah_alt_long_names      :: list(string),
                aah_arg_name            :: string,
                aah_description         :: help_text
            )
    ;       priv_alt_help(
                pah_long_name           :: string,
                pah_alt_long_names      :: list(string),
                pah_description         :: help_text
            )
    ;       priv_alt_arg_help(
                paah_long_name          :: string,
                paah_alt_long_names     :: list(string),
                paah_arg_name           :: string,
                paah_description        :: help_text
            )
    ;       short_help(
                sh_short_name           :: char,
                sh_long_name            :: string,
                sh_alt_long_names       :: list(string),
                sh_description          :: help_text
            )
    ;       priv_short_help(
                psh_short_name          :: char,
                psh_long_name           :: string,
                psh_alt_long_names      :: list(string),
                psh_description         :: help_text
            )
    ;       short_arg_help(
                sah_short_name          :: char,
                sah_long_name           :: string,
                sah_alt_long_names      :: list(string),
                sah_arg_name            :: string,
                sah_description         :: help_text
            )
    ;       priv_short_arg_help(
                psah_short_name         :: char,
                psah_long_name          :: string,
                psah_alt_long_names     :: list(string),
                psah_arg_name           :: string,
                psah_description        :: help_text
            )
    ;       unnamed_help(
                % Help for an internal-use-only option that has no visible
                % name, and therefore cannot be given on the command line
                % even by developers.
                uh_description          :: help_text
            ).

:- type help_public_or_private
    --->    help_public
    ;       help_private.

:- type arg_align
    --->    arg_align(
                aa_arg_name             :: string,
                aa_aligned_help_text    :: string,
                aa_aligned_texinfo      :: string
            ).

:- type help_text == list(help_piece).
:- type help_piece
    --->    w(string)                           % words
    ;       fixed(string)                       % as in the format_piece
    ;       opt(string)                         %
    ;       opt(string, string)                 %
    ;       arg(string)                         % ?
    ;       arg(string, string)                 % ?
    ;       bare_arg(string)                         % ?
    ;       bare_arg(string, string)                 % ?
    ;       opt_arg(string, string)             % ?
    ;       opt_arg(string, string, string)     % ?
    ;       quote(string)                       % ``str1''
    ;       quote(string, string)               % ``str1''str2
    ;       ref(string, string, string)         % @ref{str2}
    ;       ref(string, string, string, string) % @ref{str2}str4
    ;       xref(string)                        % @xref{str1}      texinfo only
    ;       xref(string, string)                % @xref{str1}str2  texinfo only
    ;       samp(string)                        % @samp{str1}
    ;       samp(string, string)                % @samp{str1}str2
    ;       emph(string)                        % @emph{str1}
    ;       emph(string, string)                % @emph{str1}str2
    ;       env(string)                         % @env{str1}
    ;       env(string, string)                 % @env{str1}str2
    ;       code(string)                        % @code{str1}
    ;       code(string, string)                % @code{str1}str2
    ;       file(string)                        % @file{str1}
    ;       file(string, string)                % @file{str1}str2
    ;       var(string)                         % @var{str1}
    ;       var(string, string)                 % @var{str1}str2
    ;       file_var(string, string)            % @file{@var{str1}.str2}
    ;       file_var(string, string, string)    % @file{@var{str1}.str2}str3
    ;       blank_line
    ;       help_text_only(list(help_piece))    %
    ;       texinfo_only(list(help_piece))      %
    ;       help_text_texinfo(list(help_piece), list(help_piece))
    ;       cindex(string)                      % @cindex{str}, out-of-line
    ;       findex(string).                     % @findex{str}, out-of-line

%---------------------------------------------------------------------------%
:- end_module libs.optdb_help.
%---------------------------------------------------------------------------%
