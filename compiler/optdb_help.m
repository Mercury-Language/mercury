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

    % XXX Most comments here all come print_help_old.m.
    % Once this module is complete, they should be updated to refer
    % to the final form of this data structure.
:- type help
    --->    no_help
    ;       gen_help(
                % Every character in this field is a short name of the option.
                % The order of these short options here will be preserved
                % in the output.
                %
                % We use a char because all options using gen_help
                % have short names that have no argument.
                % (Some options using the other function symbols below
                % *do* have short names that take arguments, and for these,
                % we use strings, the first character of which is the short
                % name, and the rest is the argument description suffix.
                gh_short_names          :: list(char),

                % The name of the option, minus the "--" prefix, but
                % including any "no-" prefix. If the option takes an argument,
                % then the option name should be followed by " <argdesc>".
                %
                % The reason for including the "no-" prefix is that
                % the description string has to be written differently
                % depending on whether the option is default-on or default-off,
                % and so requiring the documentation writer to add the prefix
                % is a trivially small extra burden.
                %
                % The reason for including the " <argdesc>" suffix if the
                % option has an argument is that for accumulating options,
                % only the positive version takes an argument, but we must
                % document the negative version as well. This makes we cannot
                % add the argdesc suffix to all alternate names willy-nilly,
                % and any automatic addition that is smart enough to always
                % do the right thing would come with an unjustifiable amount
                % of complexity.
                %
                % (Both "no-" prefixes and " <...>" suffixes will need to be
                % be stripped off for any bijection check.)
                gh_long_name            :: string,

                % Any alternate names of the option.
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
    % all have a subset of the fields of gen_help.
    %
    % The fields they contain have the same semantics as in gen_help.
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
                % We will need to handle arg names of the form "{...}"
                % and of the form "-..." specially, in that
                % we don't want to put "<>" around them.
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
                alh_aligned_text        :: string,
                alh_description         :: help_text
            )
    ;       no_align_help(
                nlh_long_name           :: string,
                nlh_aligned_text        :: string,
                alh_no_aligned_text     :: string,  % for --no-long-name
                nlh_description         :: help_text
            )
    ;       priv_alt_align_help(
                palh_long_name          :: string,
                palh_alt_long_names     :: list(string),
                palh_aligned_text       :: string,
                palh_description        :: help_text
            )
    ;       short_alt_align_help(
                salh_short_name         :: char,
                salh_long_name          :: string,
                salh_alt_long_names     :: list(string),
                salh_aligned_text       :: string,
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
                aa_aligned_text         :: string
            ).

:- type help_text == list(help_piece).
:- type help_piece
    --->    w(string)                           % words
    ;       opt(string)                         %
    ;       opt(string, string)                 %
    ;       samp(string)                        % @samp{str1}
    ;       samp(string, string)                % @samp{str1}str2
    ;       emph(string)                        % @emph{str1}
    ;       emph(string, string)                % @emph{str1}str2
    ;       var(string)                         % @var{str1}
    ;       var(string, string)                 % @var{str1}str2
    ;       file(string)                        % @file{str1}
    ;       file(string, string)                % @file{str1}str2
    ;       code(string)                        % @code{str1}
    ;       code(string, string)                % @code{str1}str2
    ;       file_var(string, string)            % @file{@var{str1}.str2}
    ;       file_var(string, string, string)    % @file{@var{str1}.str2}str3
    ;       texinfo_only(list(help_piece)).     %

%---------------------------------------------------------------------------%
:- end_module libs.optdb_help.
%---------------------------------------------------------------------------%
