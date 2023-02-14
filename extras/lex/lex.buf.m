%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%-----------------------------------------------------------------------------%
%
% lex.buf.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Copyright (C) 2002, 2010 The University of Melbourne.
% Copyright (C) 2017-2019, 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%
% Sat Aug 19 16:56:30 BST 2000
%
% This module implements the rolling char buffer. The char buffer
% is optimised for efficiency.
%
% The buffer stores chars read from an input source (e.g. IO or string).
% Because the lexer can want to `unread' chars (when a long candidate lexeme
% fails), the buffer may contain `read ahead' chars. The structure of the
% buffer is as follows:
%
%    buf[0]                                       buf[len]
%    |                  len = end - start                |
%    v                                                   v
%   +---------------------------------------------------+
%   |.|.|.|.|.|a|b|c|d|e|f|g|h|i|j|k|l| | | | | | | | | |
%   +---------------------------------------------------+
%    ^         ^           ^           ^                 ^
%    |         |           |           |                 |
%    origin    start       cursor      end        terminus
%
% origin, start etc. are all recorded in terms of offsets (number of chars)
% from the start of the input stream, counting the first char read as
% being at offset 1. Hence, the char at the cursor is at buf[cursor - origin].
%
% READING CHARS
%
% * In the diagram, `g' is the next char that will be read.
%
% Thu cursor marks the point of the next char to be read in.
%
% If the cursor advances to the end, then a new char is read from the input
% and inserted into the buffer at the end and the end marker is incremented.
%
% If the end marker advances to the terminus, then the buffer is extended
% and the terminus adjusted appropriately. The buffer may take this opportunity
% to garbage collect the inaccessible chars between the origin and
% the start marker.
%
% EOF
%
% * In the diagram, if EOF had been detected then the end marker
% would give the offset at which it occurred.
%
% When EOF is read from the input stream, a special eof flag is set
% (and the end marker, of course, will give its offset). Any attempt to read
% at or past this point will cause the buffer to return the EOF signal.
%
% REWINDING
%
% * In the diagram, the cursor may be rewound to any point
% between the start marker and itself, inclusive.
%
% At any point, the cursor may be reset to any point between
% itself and the start marker inclusive.
%
% At any point, the user may ask for the offset of the cursor.
%
% STRING EXTRACTION
%
% * In the diagram, the string read in so far is "abcdef".
%
% The buffer provides a facility to return the substring consisting of
% the chars between the start marker and up to, but not including,
% that under the cursor.
%
% COMMITS
%
% * In the diagram, a commit will move the start marker to be the same
% as the cursor.
%
% The user can issue a commit order to the buffer which moves the start pointer
% to where the cursor is, preventing rewinds back past this point.
% This is important since it means that the region prior to the cursor
% in the buffer is now available for garbage collection.
%
%-----------------------------------------------------------------------------%

:- module lex.buf.
:- interface.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module string.

%-----------------------------------------------------------------------------%

    % XXX We need a char and/or byte array datatype;
    % array(char) uses one word for each char, which is rather wasteful.
    %
:- type buf == array(char).

    % T is the type of the input source (typically io.state or string);
    % the user must initialise the buffer by specifying an appropriate
    % read predicate.
    %
:- type buf_state(T)
    --->    buf_state(
                buf_origin                  :: offset,
                buf_start                   :: offset,
                buf_cursor                  :: offset,
                buf_end                     :: offset,
                buf_terminus                :: offset,

                % If `yes', then buf_end has the offset.
                buf_eof_seen                :: bool,
                buf_read_pred               :: read_pred(T)
            ).

:- inst buf_state for buf_state/1
    --->    buf_state(
                ground,
                ground,
                ground,
                ground,
                ground,
                ground,
                read_pred
            ).

    % Returns an empty buffer and an initialised buf_state.
    %
:- pred init(read_pred(T)::in(read_pred), buf_state(T)::out(buf_state),
    buf::array_uo) is det.

    % Reads the next char and advances the cursor.
    % Updates the buf_state, the buf and the input.
    %
:- pred read(read_result::out,
    buf_state(T)::in(buf_state), buf_state(T)::out(buf_state),
    buf::array_di, buf::array_uo, T::di, T::uo) is det.

    % Returns the offset of the start marker.
    %
:- func start_offset(buf_state(T)) = offset.
:- mode start_offset(in(buf_state)) = out is det.

    % Returns the offset of the cursor.
    %
:- func cursor_offset(buf_state(T)) = offset.
:- mode cursor_offset(in(buf_state)) = out is det.

    % Rewinds the buffer. An exception is raised if the offset provided
    % is not legitimate.
    %
:- func rewind_cursor(offset, buf_state(T)) = buf_state(T).
:- mode rewind_cursor(in, in(buf_state)) = out(buf_state) is det.

    % Extracts the string of chars between the start and cursor.
    %
:- func string_to_cursor(buf_state(T), buf) = string.
:- mode string_to_cursor(in(buf_state), array_ui) = out is det.

    % Advances the start marker to the cursor. Rewinds past the cursor
    % will therefore no longer be possible.
    %
:- func commit(buf_state(T)) = buf_state(T).
:- mode commit(in(buf_state)) = out(buf_state) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

    % The amount the buffer is grown by if (a) more space is required
    % and (b) the available space is smaller than this amount.
    %
:- func low_water_mark = int.
low_water_mark = 256.

:- func initial_buf_size = int.
initial_buf_size = 1024.

    % XXX Debugging values.
    % %
% :- func low_water_mark = int.
% low_water_mark = 16.
%
% :- func initial_buf_size = int.
% initial_buf_size = 32.

%-----------------------------------------------------------------------------%

init(BufReadPred, BufState, Buf) :-
    BufState = buf_state(0, 0, 0, 0, initial_buf_size, no, BufReadPred),
    Buf      = array.init(initial_buf_size, ('@')).

%-----------------------------------------------------------------------------%

read(Result, BufState0, BufState, !Buf, !Src) :-
    Origin   = BufState0 ^ buf_origin,
    Start    = BufState0 ^ buf_start,
    Cursor   = BufState0 ^ buf_cursor,
    End      = BufState0 ^ buf_end,
    Terminus = BufState0 ^ buf_terminus,
    EOFSeen  = BufState0 ^ buf_eof_seen,
    ReadP    = BufState0 ^ buf_read_pred,

    ( if Cursor < End then
        Result   = ok(array.lookup(!.Buf, Cursor - Origin)),
        BufState = ( BufState0 ^ buf_cursor := Cursor + 1 )
    else /* Cursor = End */ if EOFSeen = yes then
        Result   = eof,
        BufState = BufState0
    else if End < Terminus then
        ReadP(Cursor, Result, !Src),
        ( if Result = ok(Char) then
            array.set(End - Origin, Char, !Buf),
            BufState = (( BufState0
                                ^ buf_cursor := Cursor + 1 )
                                ^ buf_end    := End + 1 )
        else
            BufState = BufState0
        )
    else
        % Need to GC and/or extend the buffer.
        GarbageLength = Start - Origin,
        adjust_buf(GarbageLength, ExtraLength, !Buf),
        NewOrigin     = Origin + GarbageLength,
        NewTerminus   = Terminus + GarbageLength + ExtraLength,
        BufState1     = (( BufState0
                                ^ buf_origin   := NewOrigin )
                                ^ buf_terminus := NewTerminus ),
        read(Result, BufState1, BufState, !Buf, !Src)
    ).

%-----------------------------------------------------------------------------%

    % Garbage collects the chars between the origin and start and
    % extends the buffer if the remaining space is below the low
    % water mark.
    %
:- pred adjust_buf(int::in, int::out, buf::array_di, buf::array_uo) is det.

adjust_buf(GarbageLength, ExtraLength, Buf0, Buf) :-
    Size0 = array.size(Buf0),
    ( if GarbageLength < low_water_mark then
        % We need to grow the buffer.
        array.init(Size0 + low_water_mark, ('@'), Buf1),
        ExtraLength = low_water_mark
      else
        Buf1 = Buf0,
        ExtraLength = 0
    ),
    Buf = shift_buf(0, Size0 - GarbageLength, GarbageLength, Buf0, Buf1).

%-----------------------------------------------------------------------------%

:- func shift_buf(int, int, int, buf, buf) = buf.
:- mode shift_buf(in, in, in, array_ui, array_di) = array_uo is det.

shift_buf(I, Hi, Disp, Src, Tgt) =
    ( if I < Hi then
        shift_buf(I + 1, Hi, Disp, Src,
            array.set(Tgt, I, array.lookup(Src, I + Disp)))
      else
        Tgt
    ).

%-----------------------------------------------------------------------------%

start_offset(BufState) = BufState ^ buf_start.

%-----------------------------------------------------------------------------%

cursor_offset(BufState) = BufState ^ buf_cursor.

%-----------------------------------------------------------------------------%

rewind_cursor(Offset, BufState) =
    ( if
        ( Offset < BufState ^ buf_start
        ; BufState ^ buf_cursor < Offset
        )
    then
        throw("buf: rewind/2: offset arg outside valid range")
    else
        BufState ^ buf_cursor := Offset
    ).

%-----------------------------------------------------------------------------%

string_to_cursor(BufState, Buf) = String :-
    From   = BufState ^ buf_start - BufState ^ buf_origin,
    Length = (BufState ^ buf_cursor - 1 - BufState ^ buf_start),
    To     = From + Length,
    String = string.from_char_list(array.fetch_items(Buf, From, To)).

%-----------------------------------------------------------------------------%

commit(BufState) = ( BufState ^ buf_start := BufState ^ buf_cursor ).

%-----------------------------------------------------------------------------%
:- end_module lex.buf.
%-----------------------------------------------------------------------------%
