%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Fix a bug where cse_detection wasn't hoisting the common deconstruction
% for a one-arm switch on argument 2 of display_diff_side_by_side_2.
% The symptom was a determinism error.

:- module switch_detection_bug2.

:- interface.
:- import_module bool.
:- import_module io.
:- import_module int.
:- import_module list.
:- import_module pair.

:- type pos == int.

:- type segment == pair(pos, pos).

:- type edit
    --->    add(pos, segment)
    ;       delete(segment, pos)
    ;       change(segment, segment).

:- type diff == list(edit).

    % Parameters to pass around.
:- type side_by_side_info
    --->    side_by_side_info(
                int,        % Half width
                int,        % Column 2 offset
                bool,       % Left column only
                bool,       % Suppress common lines
                bool        % Help sdiff
            ).

:- pred display_diff_side_by_side_2(int::in, side_by_side_info::in, diff::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module require.
:- import_module std_util.
:- import_module string.

display_diff_side_by_side_2(_Prev, SBS, [], !IO) :-
    SBS = side_by_side_info(_, _, _, Suppress, _),
    ( if Suppress = no then
        true
    else
        true
    ).
display_diff_side_by_side_2(Prev, SBS, [Edit | Diff], !IO) :-
    first_mentioned_positions(Edit, _, _),
    SBS = side_by_side_info(_, _, _, Suppress, _),
    ( if Suppress = no then
        true
    else
        true
    ),
    display_diff_side_by_side_2(Prev, SBS, Diff, !IO).

:- pred first_mentioned_positions(edit :: in, pos :: out, pos :: out) is det.
:- pragma no_inline(first_mentioned_positions/3).

first_mentioned_positions(_, 42, 42).

%---------------------------------------------------------------------------%
