%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2006 The University of Melbourne.
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module mdb.
:- interface.

:- pred mdb.version(string::out) is det.

    % These interface modules are used directly by the test programs
    % or the libmer_trace library.
:- include_module browse.
:- include_module browser_info.
:- include_module browser_term.
:- include_module collect_lib.
:- include_module cterm.
:- include_module debugger_interface.
:- include_module declarative_debugger.
:- include_module declarative_execution.
:- include_module diff.
:- include_module help.
:- include_module interactive_query.
:- include_module io_action.
:- include_module listing.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module declarative_analyser.
:- include_module declarative_edt.
:- include_module declarative_oracle.
:- include_module declarative_tree.
:- include_module declarative_user.
:- include_module frame.
:- include_module parse.
:- include_module sized_pretty.
:- include_module term_rep.
:- include_module term_to_html.
:- include_module util.

    % XXX these modules are more generally useful, but the
    % dynamic linking library is not yet installed anywhere.
:- include_module dl.
:- include_module name_mangle.

%---------------------------------------------------------------------------%

% See library/library.m for why we implement this predicate this way.

:- pragma foreign_proc("C",
    mdb.version(Version::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_ConstString version_string =
        MR_VERSION "", configured for "" MR_FULLARCH;
    /*
    ** Cast away const needed here, because Mercury declares Version
    ** with type MR_String rather than MR_ConstString.
    */
    Version = (MR_String) (MR_Word) version_string;
").

mdb.version("unknown version").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
