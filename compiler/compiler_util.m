%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006, 2009-2010 The University of Melbourne.
% Copyright (C) 2014-2015, 2018, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: compiler_util.
% Main author: zs.
%
% This module contains code that can be helpful in any compiler module.
%
%-----------------------------------------------------------------------------%

:- module libs.compiler_util.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred maybe_is_error(maybe_error::in, string::out) is semidet.

%-----------------------------------------------------------------------------%

    % This type is useful when defining options and behaviours that may
    % raise either an error or a warning.  See
    % pragma_require_tail_recursion.
    %
:- type warning_or_error
    --->    we_warning
    ;       we_error.

    % warning_or_error_string(we_warning, "warn").
    % warning_or_error_string(we_error, "error").
    %
:- pred warning_or_error_string(warning_or_error, string).
:- mode warning_or_error_string(in, out) is det.
:- mode warning_or_error_string(out, in) is semidet.

%-----------------------------------------------------------------------------%

:- pred add_error(error_phase::in, list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_warning(error_phase::in, option::in, list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

    % Record the fact that a warning has been issued; set the exit status
    % to error if the `--halt-at-warn' option is set.
    %
:- pred record_warning(globals::in, io::di, io::uo) is det.
:- pred record_warning_opt_table(option_table::in, io::di, io::uo) is det.

    % Report a warning to the specified stream, and set the exit status
    % to error if the --halt-at-warn option is set.
    %
:- pred report_warning(io.text_output_stream::in, globals::in, string::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module getopt.

%-----------------------------------------------------------------------------%

maybe_is_error(error(Error), Error).

%-----------------------------------------------------------------------------%

warning_or_error_string(we_warning, "warn").
warning_or_error_string(we_error, "error").

%-----------------------------------------------------------------------------%

add_error(Phase, Pieces, !Specs) :-
    Spec = no_ctxt_spec($pred, severity_error, Phase, Pieces),
    !:Specs = [Spec | !.Specs].

add_warning(Phase, Option, Pieces, !Specs) :-
    Spec = no_ctxt_spec($pred, severity_warning(Option), Phase, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

record_warning(Globals, !IO) :-
    globals.lookup_bool_option(Globals, halt_at_warn, HaltAtWarn),
    (
        HaltAtWarn = yes,
        io.set_exit_status(1, !IO)
    ;
        HaltAtWarn = no
    ).

record_warning_opt_table(OptionTable, !IO) :-
    getopt.lookup_bool_option(OptionTable, halt_at_warn, HaltAtWarn),
    (
        HaltAtWarn = yes,
        io.set_exit_status(1, !IO)
    ;
        HaltAtWarn = no
    ).

report_warning(Stream, Globals, Message, !IO) :-
    record_warning(Globals, !IO),
    io.write_string(Stream, Message, !IO).

%-----------------------------------------------------------------------------%
:- end_module libs.compiler_util.
%-----------------------------------------------------------------------------%
