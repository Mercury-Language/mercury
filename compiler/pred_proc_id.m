%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pred_proc_id.m.
%
% This module defines the types that identify pred_infos and proc_infos.
%
%---------------------------------------------------------------------------%

:- module hlds.pred_proc_id.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module set.

%---------------------------------------------------------------------------%

:- type pred_proc_id
    --->    proc(pred_id, proc_id).

    % A proc_id is the name of a mode within a particular predicate -
    % not to be confused with a mode_id, which is the name of a
    % user-defined mode.

:- type pred_id.
:- type proc_id.

:- func pred_proc_id_project_pred_id(pred_proc_id) = pred_id.
:- func pred_proc_id_project_proc_id(pred_proc_id) = proc_id.

    % Predicate and procedure ids are abstract data types. One important
    % advantage of this arrangement is to make it harder to accidentally
    % confuse them for each other, or to use an integer in their place.
    % However, you can convert between integers and pred_ids/proc_ids
    % with the following predicates and functions.
    %
:- func shroud_pred_id(pred_id) = shrouded_pred_id.
:- func shroud_proc_id(proc_id) = shrouded_proc_id.
:- func shroud_pred_proc_id(pred_proc_id) = shrouded_pred_proc_id.

:- func unshroud_pred_id(shrouded_pred_id) = pred_id.
:- func unshroud_proc_id(shrouded_proc_id) = proc_id.
:- func unshroud_pred_proc_id(shrouded_pred_proc_id) = pred_proc_id.

:- pred pred_id_to_int(pred_id, int).
:- mode pred_id_to_int(in, out) is det.
:- mode pred_id_to_int(out, in) is det.
:- func pred_id_to_int(pred_id) = int.

:- pred proc_id_to_int(proc_id, int).
:- mode proc_id_to_int(in, out) is det.
:- mode proc_id_to_int(out, in) is det.
:- func proc_id_to_int(proc_id) = int.

    % Return the id of the first predicate in a module, and of the first
    % procedure in a predicate.
    %
:- func initial_pred_id = pred_id.
:- func initial_proc_id = proc_id.

    % Return an invalid predicate or procedure id. These are intended to be
    % used to initialize the relevant fields in call(...) goals before
    % we do type- and mode-checks, or when those checks find that there was
    % no predicate matching the call.
    %
:- func invalid_pred_id = pred_id.
:- func invalid_proc_id = proc_id.

:- pred next_pred_id(pred_id::in, pred_id::out) is det.

    % For semidet complicated unifications with mode (in, in), these are
    % defined to have the same proc_id (0). This returns that proc_id.
    %
:- pred in_in_unification_proc_id(proc_id::out) is det.

    % Several passes operate on the module one SCC at a time. An SCC is
    % a strongly connected component of the call graph, i.e. a group of
    % procedures that all recursively call each other, directly or indirectly,
    % which aren't mutually recursive with any procedure outside the SCC.
:- type scc == set(pred_proc_id).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

:- type pred_id
    --->    pred_id(int).

:- type proc_id == int.

pred_proc_id_project_pred_id(proc(PredId, _ProcId)) = PredId.
pred_proc_id_project_proc_id(proc(_PredId, ProcId)) = ProcId.

shroud_pred_id(pred_id(PredId)) = shrouded_pred_id(PredId).
shroud_proc_id(ProcId) = shrouded_proc_id(ProcId).
shroud_pred_proc_id(proc(pred_id(PredId), ProcId)) =
    shrouded_pred_proc_id(PredId, ProcId).

unshroud_pred_id(shrouded_pred_id(PredId)) = pred_id(PredId).
unshroud_proc_id(shrouded_proc_id(ProcId)) = ProcId.
unshroud_pred_proc_id(shrouded_pred_proc_id(PredId, ProcId)) =
    proc(pred_id(PredId), ProcId).

pred_id_to_int(pred_id(PredId), PredId).
pred_id_to_int(pred_id(PredId)) = PredId.

proc_id_to_int(ProcId, ProcId).
proc_id_to_int(ProcId) = ProcId.

initial_pred_id = pred_id(0).
initial_proc_id = 0.

invalid_pred_id = pred_id(-1).
invalid_proc_id = -1.

next_pred_id(pred_id(PredId), pred_id(NextPredId)) :-
    NextPredId = PredId + 1.

in_in_unification_proc_id(0).

%---------------------------------------------------------------------------%
:- end_module hlds.pred_proc_id.
%---------------------------------------------------------------------------%
