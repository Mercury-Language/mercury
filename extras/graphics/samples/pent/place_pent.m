%------------------------------------------------------------------------------%
% file: place_pent.m
% author: Tyson Dowd, August 1997 
%
% Place pentominoes on the board.
%
% This source file is hereby placed in the public domain. -Tyson Dowd
% (the author).
%
%------------------------------------------------------------------------------%

:- module place_pent.

:- interface.

:- import_module list, std_util, io.

%--------------------------------------------------------------------------
	% The pieces - 'e' is for empty.
:- type piece --->	i ; l ; y ; n ; t ; v ; f ; w ; z ; p ; u ; x ; e.

:- type piece_descriptor ==	list(square).

:- type square == int.
			
:- type board == list(pair(square, piece)).

:- type squares == list(square).
%--------------------------------------------------------------------------

:- pred fill_board(int::in, int::in, squares::in, list(piece)::in,
		board::in, board::out) is nondet.

:- pred initial_board(board::out) is det.

:- pred initial_sqs(int::in, int::in, squares::out) is det.

:- pred initial_pieces(list(piece)::out) is det.

:- pred write_board(int::in, int::in, board::in, io__state::di, 
	io__state::uo) is det.

:- func make_sq(int::in, int::in) = (square::out) is det.
:- func sq_x(square::in) = (int::out) is det.
:- func sq_y(square::in) = (int::out) is det.

%--------------------------------------------------------------------------

:- implementation.

:- import_module std_util, list, int, assoc_list, require.
:- import_module shapes.

fill_board(MaxX, MaxY, EmptySqs0, Pieces, Board0, Board) :-
	fill_board_2(MaxX, MaxY, EmptySqs0, Pieces, Board0, Board).

:- pred fill_board_2(int::in, int::in, squares::in,
		list(piece)::in, board::in, board::out) is nondet.

fill_board_2(_, _, _, _, Board, Board).	% Comment out this to just see
					% solutions.
fill_board_2(MaxX, MaxY, [Sq0 | EmptySqs0], Pieces0, Board0, Board) :- 
	place_piece(Sq0, MaxX, MaxY, [Sq0 | EmptySqs0], EmptySqs1, 
		Pieces0, Pieces1, Board0, 
		Board1),
	fill_board_2(MaxX, MaxY, EmptySqs1, Pieces1, Board1, Board).


	% Since we only use the uppermost, leftmost point, the
	% Y value will always be zero, so you don't have to
	% translate Y as much.
:- pred place_piece(int::in, int::in, int::in, squares::in, squares::out,
	list(piece)::in, list(piece)::out, board::in, board::out) is nondet.

place_piece(Sq0, MaxX, MaxY, EmptySqs0, EmptySqs, Pieces0, Pieces, 
		Board0, Board) :-
	list__delete(Pieces0, Piece, Pieces),
	get_piece(Piece, PieceDesc),
	PieceDesc = [ PointSq | _ ],
	Sq1 = translate_sq(Sq0, 0 -(sq_x(PointSq)), 0),
	place(Piece, PieceDesc, MaxX, MaxY, Sq1, EmptySqs0, 
		EmptySqs, Board0, Board).

:- pred place(piece::in, piece_descriptor::in, int::in, int::in, square::in, 
	squares::in, squares::out, board::in, board::out) is semidet.

place(_Piece, [], _, _, _, EmptySqs, EmptySqs, Board, Board).
place(Piece, [Sq1 | Rest], MaxX, MaxY, Sq0, EmptySqs0,
	EmptySqs, Board0, Board) :-

	Sq = translate_sq(Sq0, Sq1),

	sq_x(Sq) >= 0,
%	sq_y(Sq) >= 0, % (don't need to check this, actually)
	sq_x(Sq) =< MaxX,
	sq_y(Sq) =< MaxY,
	Board1 = [Sq - Piece | Board0],
	list__delete_first(EmptySqs0, Sq, EmptySqs1),
	place(Piece, Rest, MaxX, MaxY, Sq0, EmptySqs1, EmptySqs,
		Board1, Board).

make_sq(X, Y) = Sq :-
	ShiftY = Y << 16,
	Sq = ShiftY + X.

sq_x(Sq) = Sq /\ ((1 << 16) - 1).

sq_y(Sq) = Sq >> 16.

:- func translate_sq(square::in, int::in, int::in) = (square::out) is det.
translate_sq(Sq0, X, Y) = Sq :-
	ShiftY = Y << 16,
	Sq = ShiftY + Sq0 + X.

:- func translate_sq(square::in, square::in) = (square::out) is det.
translate_sq(Sq0, Sq1) = Sq0 + Sq1.

initial_pieces([f,i,l,n,p,t,u,v,w,x,y,z]).

initial_board([]).

initial_sqs(MaxX, MaxY, Sqs) :-
	Generator = lambda([Sq::out] is nondet, (
		between(0, MaxX, X),
		between(0, MaxY, Y),
		Sq = make_sq(X, Y))),
	solutions(Generator, Sqs).

:- pred between(int, int, int).
:- mode between(in, in, out) is nondet.

between(Min, Max, I) :-
        Min =< Max,
        (
                I = Min
        ;
                Min1 is Min + 1,
                between(Min1, Max, I)
        ).

	

	% The Board must be sorted in advance.
write_board(MaxX, MaxY, Board) -->
	{ list__sort(Board, SortedBoard) },
	write_board_rows(0, MaxX, MaxY, SortedBoard, _).

:- pred write_board_rows(int::in, int::in, int::in, board::in, board::out,
	io__state::di, io__state::uo) is det.
write_board_rows(Y, MaxX, MaxY, Board0, Board) -->
	( 
		{ Y =< MaxY }
	->
		write_board_row(0, Y, MaxX, Board0, Board1),
		io__write_string("\n"),
		write_board_rows(Y + 1, MaxX, MaxY, Board1, Board)
	;
		{ Board = Board0 },
		io__write_string("\n")
	).


:- pred write_board_row(int::in, int::in, int::in, board::in, board::out,
	io__state::di, io__state::uo) is det.
write_board_row(X, Y, MaxX, Board0, Board) -->
	(
		{ X =< MaxX }
	->
		{
			Board0 = [make_sq(X, Y) - Elem0 | Board1]
		->
			Elem = Elem0,
			Board2 = Board1
		;
			Elem = e,
			Board2 = Board0
		},
		io__write(Elem),
		write_board_row(X + 1, Y, MaxX, Board2, Board)
	;
		{ Board = Board0 }
	).

