%----------------------------------------------------------------------------
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% lex.lexeme.m
% Sat Aug 19 08:22:32 BST 2000
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Copyright (C) 2001 The Rationalizer Intelligent Software AG.
%   The changes made by Rationalizer are contributed under the terms
%   of the GNU Lesser General Public License, see the file COPYING.LGPL
%   in this directory.
% Copyright (C) 2002, 2010-2011 The University of Melbourne.
% Copyright (C) 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%
% A lexeme combines a token with a regexp.  The lexer compiles
% lexemes and returns the longest successful parse in the input
% stream or an error if no match occurs.
%
%-----------------------------------------------------------------------------%

:- module lex.lexeme.
:- interface.

:- import_module array.
:- import_module bool.
:- import_module bitmap.
:- import_module char.

%-----------------------------------------------------------------------------%

:- type compiled_lexeme(T)
    --->    compiled_lexeme(
                token              :: token_creator(T),
                state              :: state_no,
                transition_map     :: transition_map
            ).

:- inst compiled_lexeme
    --->    compiled_lexeme(token_creator, ground, ground).

:- type transition_map
    --->    transition_map(
                accepting_states    :: bitmap,
                rows                :: array(row)
            ).

    % A transition row is an array of packed_transitions.
    %
:- type row == array(packed_transition).

    % A packed_transition combines a target state_no
    % and the transition char codepoint for which the
    % transition is valid.
    %
:- type packed_transition
     ---> packed_transition(btr_state :: state_no, char :: char).

:- type packed_transitions
    == list(packed_transition).

:- func compile_lexeme(lexeme(T)) = compiled_lexeme(T).

    % next_state(CLXM, CurrentState, Char, NextState, IsAccepting)
    % succeeds iff there is a transition in CLXM from CurrentState
    % to NextState via Char; IsAccepting is `yes' iff NextState is
    % an accepting state_no.
    %
:- pred next_state(compiled_lexeme(T), state_no, char, state_no, bool).
:- mode next_state(in(compiled_lexeme), in, in, out, out) is semidet.

    % Succeeds iff a compiled_lexeme is in an accepting state_no.
    %
:- pred in_accepting_state(compiled_lexeme(T)).
:- mode in_accepting_state(in(compiled_lexeme)) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lex.automata.
:- import_module lex.regexp.
:- import_module lex.convert_NFA_to_DFA.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

compile_lexeme(Lexeme) = CompiledLexeme :-
    Lexeme         = (RegExp - TokenCreator),
    NFA            = remove_null_transitions(regexp_to_NFA(RegExp)),
    DFA            = convert_NFA_to_DFA(NFA),
    StartState     = DFA ^ smc_start_state,
    StopStates     = DFA ^ smc_stop_states,
    Transitions    = DFA ^ smc_state_transitions,
    N              = 1 + find_top_state(Transitions),
    Accepting      = set_accepting_states(StopStates, bitmap.init(N, no)),
    Rows           = array(set_up_rows(0, N, Transitions)),
    TransitionMap  = transition_map(Accepting, Rows),
    CompiledLexeme = compiled_lexeme(TokenCreator, StartState, TransitionMap).

%-----------------------------------------------------------------------------%

:- func find_top_state(transitions) = int.
:- mode find_top_state(in(atom_transitions)) = out is det.

find_top_state([])                    = 0.
find_top_state([trans(X, _, Y) | Ts]) = max(X, max(Y, find_top_state(Ts))).

%-----------------------------------------------------------------------------%

:- func set_accepting_states(set(state_no), bitmap) = bitmap.
:- mode set_accepting_states(in, bitmap_di) = bitmap_uo is det.

set_accepting_states(States, Bitmap0) =
    set_accepting_states_0(set.to_sorted_list(States), Bitmap0).

:- func set_accepting_states_0(list(state_no), bitmap) = bitmap.
:- mode set_accepting_states_0(in, bitmap_di) = bitmap_uo is det.

set_accepting_states_0([], Bitmap) = Bitmap.

set_accepting_states_0([St | States], Bitmap) =
    set_accepting_states_0(States, bitmap.set(Bitmap, St)).

%-----------------------------------------------------------------------------%

:- func set_up_rows(int, int, transitions) = list(row).
:- mode set_up_rows(in, in, in(atom_transitions)) = out is det.

set_up_rows(I, N, Transitions) = Rows :-
    ( if I >= N
      then Rows = []
      else Rows = [compile_transitions_for_state(I, [], Transitions) |
                   set_up_rows(I + 1, N, Transitions)]
    ).

%-----------------------------------------------------------------------------%

:- func compile_transitions_for_state(int, packed_transitions, transitions) =
            row.
:- mode compile_transitions_for_state(in, in, in(atom_transitions)) =
            array_uo is det.

compile_transitions_for_state(_, IBTs, []) = array(IBTs).

compile_transitions_for_state(I, IBTs, [T | Ts]) =
    compile_transitions_for_state(
        I,
        ( if T = trans(I, Charset, Y)
          then sparse_bitset.foldl(
             func(Char, Tx) = [packed_transition(Y, Char) | Tx],
             Charset,
             IBTs)
          else IBTs
        ),
        Ts
    ).

%-----------------------------------------------------------------------------%

next_state(CLXM, CurrentState, Char, NextState, IsAccepting) :-
    Rows            = CLXM ^ transition_map ^ rows,
    AcceptingStates = CLXM ^ transition_map ^ accepting_states,
    find_next_state(Char, Rows ^ elem(CurrentState), NextState),
    IsAccepting = AcceptingStates ^ bit(NextState).

%-----------------------------------------------------------------------------%

:- pred find_next_state(char, array(packed_transition), state_no).
:- mode find_next_state(in, in, out) is semidet.

find_next_state(Char, PackedTransitions, State) :-
    Lo = array.min(PackedTransitions),
    Hi = array.max(PackedTransitions),
    find_next_state_0(Lo, Hi, Char, PackedTransitions, State).

:- pred find_next_state_0(int, int, char, array(packed_transition), state_no).
:- mode find_next_state_0(in, in, in, in, out) is semidet.

find_next_state_0(Lo, Hi, Char, PackedTransitions, State) :-
    Lo =< Hi,
    PackedTransition = PackedTransitions ^ elem(Lo),
    ( if PackedTransition ^ char = Char
      then State = PackedTransition ^ btr_state
      else find_next_state_0(Lo + 1, Hi, Char, PackedTransitions, State)
    ).

%-----------------------------------------------------------------------------%

in_accepting_state(CLXM) :-
    bitmap.is_set(
        CLXM ^ transition_map ^ accepting_states, CLXM ^ state
    ).

%-----------------------------------------------------------------------------%
:- end_module lex.lexeme.
%-----------------------------------------------------------------------------%
