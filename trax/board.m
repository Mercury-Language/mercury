% ---------------------------------------------------------------------------- %
% board.m
% Ralph Becket <rbeck@microsoft.com>
% Wed Aug 23 11:15:37  2000
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% Implementation of the Trax board.
% ---------------------------------------------------------------------------- %

:- module board.

:- interface.

:- import_module int, list, array, std_util, bool, string.



:- type board.                          % The board representation.
:- mode board_ui :: array_ui.
:- mode board_di :: array_di.
:- mode board_uo :: array_uo.

:- type undos.                          % The list of changes to be made (in
                                        % order) to return a board to its
                                        % previous state after a move.)

:- type tile.                           % Tile representation.

:- type move_result
    --->    ok                          % The move was legal.
    ;       illegal(string)             % Explanation for an illegal move.
    ;       win(colour).                % Somebody won!

:- type colour
    --->    black
    ;       white.



:- func board_size = int.               % Number of squares along each side.

:- func black_ns = tile.                % Tiles are described using the
:- func black_ne = tile.                % black connecting edges.
:- func black_nw = tile.
:- func black_se = tile.
:- func black_sw = tile.
:- func black_ew = tile.

:- pred try_move(
            tile,                       % Tile to place.
            int, int,                   % X, Y coords for tile.
            move_result,                % The outcome of the move.
            undos,                      % How to undo changes to the board.
            board, board                % Before and after states of the board.
        ).
:- mode try_move(in, in, in, out, out, board_di, board_uo) is det.

:- pred undo_move(undos, board, board).
:- mode undo_move(in, board_di, board_uo) is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module .

    % NOTES ON REPRESENTATION, VARIOUS ASSUMPTIONS ETC.
    %
    % All games start in the centre of the board.
    %
    % The board is square and of fixed size.
    %
    % We assume that the board is so large that no game ever extends
    % to placing a tile in the outer edge (in fact, we make such moves
    % illegal - note that the game can always be recentred on the board
    % to maximise spreading potential).
    %
    % Therefore loc 0 (or, indeed, any of the corner squares) cannot
    % abut anything.  We use 0 as a sentinel value in various places.
    %
    % A stable board is a legal board position in which there are no
    % forced moves to be played.
    %
    % A legal board position requires that
    % (1) the set of tiles on the board forms a single orthogonally
    % connected set and
    % (2) every pair of abutting tiles connects black to black or
    % white to white.
    %
    % New pieces may only be placed such that
    % (1) they are placed in a void cell that
    % (2) abuts at least one tile.
    %
    % A forced move may arise after a piece has been played and
    % occurs when a void is created that abuts two tiles with
    % incoming connections of the same colour.
    %
    % Each cell on a stable board, therefore, is either empty
    % (abutting only other empty or void cells), a void (abutting
    % one or two tiles; abutting three tiles would necessitate a
    % forced move) or a tile.
    %
    % An empty cell holds the value 0.
    %
    % A cell with a tile holds -T where T identifies one of the
    % six possible tiles.
    %
    % Voids have an interesting representation.  A void is a pair
    % of connection records [connection A][connection B] where a
    % connection record is [connected loc][via direction].
    %
    % (A connection denotes the void end-points at either end of
    % a connected line.  The game is won by either forming a closed
    % loop or a line that spans the entire width of the board -
    % left-to-right or top-to-bottom where the span is 8 or more
    % squares in length.)
    %
    % Since we use one bit each of a nybble to indicate n, s, e,
    % or w, the [via direction] field cannot be zero except when
    % one of the connection records is empty, in which case the
    % bit pattern for the connection record is all zeros.
    %
    % Since we assume 32 bit ints, we have the following constraints:
    %
    % * The MSB is reserved to distinguish tiles from voids and
    %   empty cells (by indicating negation).
    %
    % * A direction field occupies 4 bits.
    %
    % * Therefore we have 32 - 1 - 8 = 23 bits left to share out
    %   between two possible connection locs => a connection loc
    %   can occupy no more than 11 bits.
    %
    % * However, since the board is square, we have to have an
    %   even number of bits for a loc => a connection loc can
    %   occupy no more than 10 bits (max. loc = (board size)^2).
    %
    % * Therefore the board size (the number of squares along each
    %   edge) can occupy no more than 5 bits.
    %
    % * Therefore the largest board size that can be accommodated
    %   on a system with 32 bits words is 32 x 32.  This should be
    %   more than adequate!
    %
    % So, a tile is represented by four bits of which exactly two
    % are set, denoting the connections for black; a tile cell is
    % negated to differentiate it from a non-tile cell.
    %
    % An empty cell contains 0, 1, or 2 connection records, where
    % a connection record is [loc][dir].



:- func bits_per_side = int.
:- func bits_per_loc = int.
:- func bits_per_dir = int.             % Exactly one bit set.
:- func bits_per_dirpair = int.         % Exactly two bits set.
:- func bits_per_tile = int.
:- func bits_per_conn_record = int.

    % Used to mask out various objects.

:- func loc_mask = int.
:- func dir_mask = int.
:- func dirpair_mask = int.
:- func tile_mask = int.



:- type board                           % The board is represented as a linear
    ==      array(cell).                % array of cells indexed by loc.

:- type undos                           % The list of changes to be made (in
    ==      list(pair(loc, cell)).      % order) to return a board to its
                                        % previous state after a move.)

:- type loc                             % The cell at (x, y) has loc
    ==      int.                        % x + (size * y).

:- type cell                            % A cell is either a tile or a void.
    ==      int.                        % If it's a tile, the value is negated.

:- type tile                            % A tile describes a playing piece.
    ==      dirpair.

:- type void                            % A void is either empty or abuts
    ==      int.                        % one or two tiles on a stable board.

:- type dir                             % The cardinal directions.
    ==      int.

:- type dirpair                         % The possible pairwise connections.
    ==      int.



    % Turn a coordinate pair into a loc and vice versa.

:- func loc(int, int) = loc.
:- func coords(loc) = pair(int).

:- func n = dir.
:- func s = dir.
:- func e = dir.
:- func w = dir.

    % Calculate the loc-delta for a given direction.

:- func delta(dir) = int.

:- func ns = dirpair.
:- func ne = dirpair.
:- func nw = dirpair.
:- func se = dirpair.
:- func sw = dirpair.
:- func ew = dirpair.

:- func flip_dirpair(dirpair) = dirpair.

:- pred dirpair_connects_to_dir(dirpair, dir).
:- mode dirpair_connects_to_dir(in, in) is semidet.

    % Turn a `black' tile representation into a `white' one and vice versa.

:- func flip_tile_colour(dirpair) = dirpair.

    % Return the dirpair connected by a tile (the black edges if black, etc.)

:- func dirpair(tile) = dirpair.

:- pred tile_connects_to_dir(dirpair, dir).
:- mode tile_connects_to_dir(in, in) is semidet.

% ---------------------------------------------------------------------------- %

board_size = 1 << bits_per_loc.

bits_per_side = 5.

bits_per_loc = 2 * bits_per_side.

bits_per_dir = 4.

bits_per_dirpair = 4.

bits_per_tile = 4.

bits_per_conn_record = bits_per_loc + bits_per_dir.

loc_mask = (1 << bits_per_loc) - 1.

dir_mask = (1 << bits_per_dir) - 1.

dirpair_mask = (1 << bits_per_dirpair) - 1.

tile_mask = (1 << bits_per_tile) - 1.

% ---------------------------------------------------------------------------- %

    % One low-order bit per direction sitting in a nybble.

n = 2'1000.
s = 2'0100.
e = 2'0010.
w = 2'0001.

% ---------------------------------------------------------------------------- %

delta(D) =
    (      if D = n then Delta = -board_size
    ; else if D = s then Delta =  board_size
    ; else if D = e then Delta =  1
    ; else if D = w then Delta = -1
    ; else               throw("board: delta/1: argument is not a dir")
    ).

% ---------------------------------------------------------------------------- %

ns = n \/ s.
ne = n \/ e.
nw = n \/ w.
se = s \/ e.
sw = s \/ w.
ew = e \/ w.

% ---------------------------------------------------------------------------- %

flip_dirpair(DP) = DP `xor` nybble_mask.

% ---------------------------------------------------------------------------- %

dirpair_connects_to_dir(DP, D) :-
    DP /\ D \= 0.

% ---------------------------------------------------------------------------- %

    % Tiles and dirpairs are identical.  It's just nice to have
    % different names for them.

black_ns = ns.
black_ne = ne.
black_nw = nw.
black_se = se.
black_sw = sw.
black_ew = ew.

% ---------------------------------------------------------------------------- %

flip_tile_colour(Tile) = flip_dirpair(Tile).

% ---------------------------------------------------------------------------- %

dirpair(Tile) = DP :-
    DP = Tile.                          % They are, indeed, one and the same.

% ---------------------------------------------------------------------------- %

tile_connects_to_dir(Tile, D) :-
    dirpair_connects_to_dir(Tile, D).

% ---------------------------------------------------------------------------- %

loc(X, Y) = X + (board_size * Y).

% ---------------------------------------------------------------------------- %

coords(Loc) = X - Y :-
    X = Loc `rem` board_size,
    Y = Loc // board_size.

% ---------------------------------------------------------------------------- %

try_move(Tile, X, Y, Result, Undos, Board0, Board) :-
    ( if
        ((X =< 0) ; (board_size - 1 < X) ; (Y =< 0) ; (board_size - 1 < Y))
      then
        Result = illegal("move not 



% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
