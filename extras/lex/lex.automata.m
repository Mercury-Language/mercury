%-----------------------------------------------------------------------------%
% lex.automata.m
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Copyright (C) 2002 The University of Melbourne
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%
% Fri Aug 18 15:48:09 BST 2000
% 
% Basic types and insts etc. for DFAs and NFAs over chars.
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%-----------------------------------------------------------------------------%

:- module lex__automata.

:- interface.

:- import_module set, list, char.



    % States are labelled with non-negative integers.
    %
:- type state_no
    ==      int.

:- type state_mc
    --->    state_mc(
                smc_start_state         :: state_no,
                smc_stop_states         :: set(state_no),
                smc_state_transitions   :: list(transition)
            ).

:- inst null_transition_free_state_mc
    ==      bound(state_mc(ground, ground, atom_transitions)).

:- type transitions
    ==      list(transition).

:- inst atom_transitions == list_skel(atom_transition).
:- inst null_transitions == list_skel(null_transition).

:- type transition
    --->    null(state_no, state_no)
    ;       trans(state_no, char, state_no).

:- inst atom_transition == bound(trans(ground, ground, ground)).
:- inst null_transition == bound(null(ground, ground)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
