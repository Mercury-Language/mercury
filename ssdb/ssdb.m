%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ssdb.m.
% Author: oannet.
%
% This module is automatically imported into every module that is compiled
% using --source-to-source-debug.
%
% It provides the primitives which are needed by this source-to-source
% transformation to allow debugging.
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- module ssdb.
:- interface.


:- type ssdb_event_type
    --->    ssdb_call
    ;       ssdb_exit
    ;       ssdb_redo
    ;       ssdb_fail
    .

    %
    % This routine is called at each event that occurs
    %
:- impure pred handle_event(ssdb_event_type::in) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module io.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

    %
    % For the moment we just write the event out.
    % Later this will be extended.
    %
handle_event(Event) :-
    promise_impure (
    trace [io(!IO)] (
        io.write(Event, !IO),
        io.nl(!IO)
    )
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
