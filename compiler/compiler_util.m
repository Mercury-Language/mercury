%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006, 2009-2010 The University of Melbourne.
% Copyright (C) 2014-2015, 2018, 2025-2026 The Mercury team.
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

:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred maybe_is_error(maybe_error::in, string::out) is semidet.

%-----------------------------------------------------------------------------%

    % This type is useful when defining options and behaviours that may
    % raise either an error or a warning. See require_tail_recursion pragmas.
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

:- pred add_error(spec_phase::in, list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_warning(spec_phase::in, option::in, list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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
:- end_module libs.compiler_util.
%-----------------------------------------------------------------------------%
