%-----------------------------------------------------------------------------
%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% lex.lexeme.m
% Sat Aug 19 08:22:32 BST 2000
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
% Thu Jul 26 07:45:47 UTC 2001
% Copyright (C) 2001 The Rationalizer Intelligent Software AG
%   The changes made by Rationalizer are contributed under the terms 
%   of the GNU Lesser General Public License, see the file COPYING.LGPL
%   in this directory.
%
% A lexeme combines a token with a regexp.  The lexer compiles
% lexemes and returns the longest successul parse in the input
% stream or an error if no match occurs.
%
%
%------------------------------------------------------------------------------%

:- module lex__lexeme.

:- interface.

:- import_module bool, char, array, bitmap.

:- type compiled_lexeme(T)
    --->    compiled_lexeme(
                token              :: token_creator(T),
                state              :: state_no,
                transition_map     :: transition_map
            ).
:- inst compiled_lexeme(Inst)
    --->    compiled_lexeme(Inst, ground, ground).

:- type transition_map
    --->    transition_map(
                accepting_states    :: bitmap,
                rows                :: array(row)
            ).

    % A transition row is an array of byte_transitions.
    %
:- type row
    ==      array(byte_transition).

    % A byte_transition encodes a target state_no no. in its upper bits
    % and the char byte value in its lower eight bits for which the
    % transition is valid.
    %
:- type byte_transition
    ==      int.

:- func byte_transition(int, state_no) = byte_transition.
:- func btr_byte(byte_transition) = int.
:- func btr_state(byte_transition) = state_no.



:- func compile_lexeme(lexeme(T)) = compiled_lexeme(T).

    % next_state(CLXM, CurrentState, Char, NextState, IsAccepting)
    % succeeds iff there is a transition in CLXM from CurrentState
    % to NextState via Char; IsAccepting is `yes' iff NextState is
    % an accepting state_no.
    %
:- pred next_state(compiled_lexeme(T), state_no, char, state_no, bool).
:- mode next_state(in(live_lexeme), in, in, out, out) is semidet.

    % Succeeds iff a compiled_lexeme is in an accepting state_no.
    %
:- pred in_accepting_state(live_lexeme(T)).
:- mode in_accepting_state(in(live_lexeme)) is semidet.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module list, set.
:- import_module lex__automata, lex__convert_NFA_to_DFA, lex__regexp.

%------------------------------------------------------------------------------%

compile_lexeme(Lexeme) = CompiledLexeme :-
    Lexeme         = (RegExp - TokenCreator),
    NFA            = remove_null_transitions(regexp_to_NFA(RegExp)),
    DFA            = convert_NFA_to_DFA(NFA),
    StartState     = DFA ^ smc_start_state,
    StopStates     = DFA ^ smc_stop_states,
    Transitions    = DFA ^ smc_state_transitions,
    N              = 1 + find_top_state(Transitions),
    Accepting      = set_accepting_states(StopStates, bitmap__new(N, no)),
    Rows           = array(set_up_rows(0, N, Transitions)),
    TransitionMap  = transition_map(Accepting, Rows),
    CompiledLexeme = compiled_lexeme(TokenCreator, StartState, TransitionMap).

%------------------------------------------------------------------------------%

:- func find_top_state(transitions) = int.
:- mode find_top_state(in(atom_transitions)) = out is det.

find_top_state([])                    = 0.
find_top_state([trans(X, _, Y) | Ts]) = max(X, max(Y, find_top_state(Ts))).

%------------------------------------------------------------------------------%

:- func set_accepting_states(set(state_no), bitmap) = bitmap.
:- mode set_accepting_states(in, bitmap_di) = bitmap_uo is det.

set_accepting_states(States, Bitmap0) =
    set_accepting_states_0(set__to_sorted_list(States), Bitmap0).



:- func set_accepting_states_0(list(state_no), bitmap) = bitmap.
:- mode set_accepting_states_0(in, bitmap_di) = bitmap_uo is det.

set_accepting_states_0([], Bitmap) = Bitmap.

set_accepting_states_0([St | States], Bitmap) =
    set_accepting_states_0(States, bitmap__set(Bitmap, St)).

%------------------------------------------------------------------------------%

:- func set_up_rows(int, int, transitions) = list(row).
:- mode set_up_rows(in, in, in(atom_transitions)) = out is det.

set_up_rows(I, N, Transitions) = Rows :-
    ( if I >= N
      then Rows = []
      else Rows = [compile_transitions_for_state(I, [], Transitions) |
                   set_up_rows(I + 1, N, Transitions)]
    ).

%------------------------------------------------------------------------------%

:- func compile_transitions_for_state(int, list(byte_transition), transitions) =
            row.
:- mode compile_transitions_for_state(in, in, in(atom_transitions)) =
            array_uo is det.

compile_transitions_for_state(_, IBTs, []) = array(IBTs).

compile_transitions_for_state(I, IBTs, [T | Ts]) =
    compile_transitions_for_state(
        I,
        ( if T = trans(I, C, Y)
          then [byte_transition(char__to_int(C), Y) | IBTs]
          else IBTs
        ),
        Ts
    ).

%------------------------------------------------------------------------------%

byte_transition(Byte, State) = (State << 8) \/ Byte.

btr_byte(BT) = BT /\ 0xff.

btr_state(BT) = BT `unchecked_right_shift` 8.

%------------------------------------------------------------------------------%

next_state(CLXM, CurrentState, Char, NextState, IsAccepting) :-
    Rows            = CLXM ^ transition_map ^ rows,
    AcceptingStates = CLXM ^ transition_map ^ accepting_states,
    find_next_state(char__to_int(Char), Rows ^ elem(CurrentState), NextState),
    IsAccepting     = bitmap__get(AcceptingStates, NextState).

%------------------------------------------------------------------------------%

:- pred find_next_state(int, array(byte_transition), state_no).
:- mode find_next_state(in, in, out) is semidet.

find_next_state(Byte, ByteTransitions, State) :-
    Lo = array__min(ByteTransitions),
    Hi = array__max(ByteTransitions),
    find_next_state_0(Lo, Hi, Byte, ByteTransitions, State).



:- pred find_next_state_0(int, int, int, array(byte_transition), state_no).
:- mode find_next_state_0(in, in, in, in, out) is semidet.

find_next_state_0(Lo, Hi, Byte, ByteTransitions, State) :-
    Lo =< Hi,
    ByteTransition = ByteTransitions ^ elem(Lo),
    ( if ByteTransition ^ btr_byte = Byte
      then State = ByteTransition ^ btr_state
      else find_next_state_0(Lo + 1, Hi, Byte, ByteTransitions, State)
    ).

%------------------------------------------------------------------------------%

in_accepting_state(CLXM) :-
    bitmap__is_set(
        CLXM ^ transition_map ^ accepting_states, CLXM ^ state
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
