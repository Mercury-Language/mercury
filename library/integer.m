%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: integer.m
% authors: aet Mar 1998. 
% 
% Implements an arbitrary precision integer type and basic 
% operations on it. (An arbitrary precision integer may have
% any number of digits, unlike an int, which is limited to the
% precision of the machine's int type, which is typically 32 bits.)
%
% Note that all operators behave as the equivalent operators on ints do.
% This includes the division operators: / // rem div mod.
%
% Possible improvements:
%
%	1) allow negative digits (-base+1 .. base-1) in lists of
%	  digits and normalise only when printing. This would
%	  probably simplify the division algorithm, also.
%	
%	2) alternatively, instead of using base=10000, use *all* the
%	  bits in an int and make use of the properties of machine
%	  arithmetic. Base 10000 doesn't use even half the bits
%	  in an int, which is inefficient. (Base 2^14 would be
%	  a little better but would require a slightly more
%	  complex case conversion on reading and printing.)
%
%	3) Use an O(n^(3/2)) algorithm for multiplying large
%	  integers, rather than the current O(n^2) method.
%	  There's an obvious divide-and-conquer technique,
%	  Karatsuba multiplication.
%
%	4) We could overload operators so that we can have mixed operations
%	  on ints and integers. For example, "integer(1)+3". This
%	  would obviate most calls of integer().
%
%	5) Use double-ended lists rather than simple lists. This
%	  would improve the efficiency of the division algorithm,
%	  which reverse lists.
%
%	6) Add bit operations (XOR, AND, OR, etc). We would treat
%	  the integers as having a 2's complement bit representation.
%	  This is easier to do if we use base 2^14 as mentioned above.
%
%	7) The implementation of `div' is slower than it need be.
%
%	8) Fourier methods such as Schoenhage-Strassen and
%	  multiplication via modular arithmetic are left as
%	  exercises to the reader. 8^)
%
%
%	Of the above, 1) would have the best bang-for-buck, 5) would
%	benefit division and remainder operations quite a lot, and 3)
%	would benefit large multiplications (thousands of digits)
%	and is straightforward to implement.

:- module integer.

:- interface.

:- import_module string.

:- type integer.

:- pred integer:'<'(integer, integer).
:- mode integer:'<'(in, in) is semidet.

:- pred integer:'>'(integer, integer).
:- mode integer:'>'(in, in) is semidet.

:- pred integer:'=<'(integer, integer).
:- mode integer:'=<'(in, in) is semidet.

:- pred integer:'>='(integer, integer).
:- mode integer:'>='(in, in) is semidet.

:- pred integer:'='(integer, integer).
:- mode integer:'='(in, in) is semidet.

:- func integer(int) = integer.

:- func integer__to_string(integer) = string.

:- func integer__from_string(string) = integer.
:- mode integer__from_string(in) = out is semidet.

:- func integer:'+'(integer) = integer.

:- func integer:'-'(integer) = integer.

:- func integer:'+'(integer, integer) = integer.

:- func integer:'-'(integer, integer) = integer.

:- func integer:'*'(integer, integer) = integer.

:- func integer:'//'(integer, integer) = integer.

:- func integer:'div'(integer, integer) = integer.

:- func integer:'rem'(integer, integer) = integer.

:- func integer:'mod'(integer, integer) = integer.

:- func integer__abs(integer) = integer.

:- pred integer__pow(integer, integer, integer).
:- mode integer__pow(in, in, out) is det.

% :- func integer__float(integer) = float.

:- implementation.

:- import_module require, list, char, std_util, int.

:- type sign == int.	% -1, 0, +1
:- type digit == int.	% base 10000 digit

:- type comparison
	--->	lessthan
	;	equal
	;	greaterthan.

	% Note: the list of digits is stored in reverse order.
	% That is, little end first.
:- type integer
	--->	i(sign, list(digit)).

	% We choose base=10000 since 10000^2+10000 < maxint.
	% XXX: We should check this. 
:- func base = int.
base = 10000.

:- func log10base = int.
log10base = 4.

integer:'<'(X1, X2) :- 
	big_cmp(X1, X2) = C,
	C = lessthan.

integer:'>'(X1, X2) :-
	big_cmp(X1, X2) = C,
	C = greaterthan.

integer:'=<'(X1, X2) :-
	big_cmp(X1, X2) = C,
	( C = lessthan ; C = equal).

integer:'>='(X1, X2) :-
	big_cmp(X1, X2) = C,
	( C = greaterthan ; C = equal).

integer:'='(X1, X2) :-
	big_cmp(X1, X2) = C,
	C = equal.

:- func one = integer.
one = integer(1).

:- func zero = integer.
zero = integer(0).

integer:'+'(X1) =
	X1.

integer:'-'(N) =
	big_neg(N).

integer:'+'(X1, X2) =
	big_plus(X1, X2).

integer:'-'(X1, X2) = 
	big_plus(X1, big_neg(X2)).

integer:'*'(X1, X2) =
	big_mul(X1, X2).

integer:'div'(X1, X2) =
	big_div(X1, X2).

integer:'//'(X1, X2) =
	big_quot(X1, X2).

integer:'rem'(X1, X2) =
	big_rem(X1, X2).

integer:'mod'(X1, X2) =
	big_mod(X1, X2).

integer__abs(N) = Abs :-
	( N < integer(0) ->
		Abs = -N
	;
		Abs = N
	).

:- func big_neg(integer) = integer.
big_neg(i(S, Ds)) =
	i(-S, Ds).

:- func big_mul(integer, integer) = integer.
big_mul(i(S1, Ds1), i(S2, Ds2)) = i(S, Ds) :-
	S = S1 * S2,
	Ds = pos_mul(Ds1, Ds2).


:- func big_quot(integer, integer) = integer.
big_quot(X1, X2) = Q :-
	big_quot_rem(X1, X2, Q, _R).

:- func big_rem(integer, integer) = integer.
big_rem(X1, X2) = R :-
	big_quot_rem(X1, X2, _Q, R).

:- func big_div(integer, integer) = integer.
big_div(X, Y) = Div :-
	big_quot_rem(X, Y, Trunc, Rem),
	(
		( X >= zero, Y >= zero
		; X < zero, Y < zero
		; Rem = zero
		)
	->
		Div = Trunc
	;
		Div = Trunc - one
	).


	% XXX: This is dog-slow.
:- func big_mod(integer, integer) = integer.
big_mod(X, Y) = X - (X div Y) * Y.

	% Compare two integers.
:- func big_cmp(integer, integer) = comparison.
big_cmp(i(S1, D1), i(S2, D2)) = 
	( S1 < S2 ->
		lessthan
	; S1 > S2 ->
		greaterthan
	; (S1=0, S2=0) ->
		equal
	; S1=1 ->
		pos_cmp(D1, D2)
	;
		pos_cmp(D2, D1)
	).

:- func pos_cmp(list(digit), list(digit)) = comparison.
pos_cmp(Xs, Ys) = pos_cmp_2(Xs1, Ys1) :-
	Xs1 = norm(Xs),
	Ys1 = norm(Ys).

:- func pos_cmp_2(list(digit), list(digit)) = comparison.
pos_cmp_2([], []) = equal.
pos_cmp_2([_X|_Xs], []) = greaterthan.
pos_cmp_2([], [_Y|_Ys]) = lessthan.
pos_cmp_2([X|Xs], [Y|Ys]) = Cmp :-
	Res = pos_cmp_2(Xs, Ys),
	( (Res = lessthan ; Res = greaterthan) ->
		Cmp = Res
	; X = Y ->
		Cmp = equal
	; X < Y ->
		Cmp = lessthan
	;
		Cmp = greaterthan
	).

:- func big_plus(integer, integer) = integer.
big_plus(i(S1, Ds1), i(S2, Ds2)) = Sum :-
	( S1 = S2 ->
		Sum = i(S1, pos_plus(Ds1, Ds2))
	; S1 = 1 ->
		C = pos_cmp(Ds1, Ds2),
		( C = lessthan ->
			Sum = i(-1, pos_sub(Ds2, Ds1))
		; C = greaterthan ->
			Sum = i(1, pos_sub(Ds1, Ds2))
		;
			Sum = zero
		)
	;
		C = pos_cmp(Ds1, Ds2),
		(
			C = lessthan ->
				Sum = i(1, pos_sub(Ds2, Ds1))
			; C = greaterthan ->
				Sum = i(-1, pos_sub(Ds1, Ds2))
			;
				Sum = zero
		)
	).

integer__from_string(S) = Big :-
	string__to_char_list(S, Cs),
	string_to_integer(Cs) = Big.

:- func string_to_integer(list(char)) = integer.
:- mode string_to_integer(in) = out is semidet.
string_to_integer(CCs) = Result :-
	( CCs = [],
		fail
	; CCs = [C|Cs],
		% Note:
		%	- '-' must be in parentheses.
		%	- There can be only one minus sign in a valid string.
		( C = ('-') ->
			Result = i(Sign, Digs),
			Digs = string_to_integer_acc(Cs, []),
			pos_cmp(Digs, []) = Cmp,
			(Cmp = equal ->
				Sign = 0
			;
				Sign = -1
			)
		; char__is_digit(C) ->
			Result = i(Sign, Digs),
			Digs = string_to_integer_acc(CCs, []),
			pos_cmp(Digs, []) = Cmp,
			(Cmp = equal ->
				Sign = 0
			;
				Sign = 1
			)
		;
			fail
		)
	).


:- func string_to_integer_acc(list(char), list(digit)) = list(digit).
:- mode string_to_integer_acc(in, in) = out is semidet.
string_to_integer_acc([], Acc) = Acc.
string_to_integer_acc([C|Cs], Acc) = Result :-
	( char__is_digit(C) ->
		char__to_int(C, D1),
		char__to_int('0', Z),
		Dig = pos_int_to_digits(D1 - Z),
		NewAcc = pos_plus(Dig, mul_by_digit(10, Acc)),
		Result = string_to_integer_acc(Cs, NewAcc)
	;
		fail
	).

integer(N) =
	int_to_integer(N).

	% Note: Since most machines use 2's complement arithmetic,
	% INT_MIN is usually -INT_MAX-1, hence -INT_MIN will
	% cause int overflow. We handle overflow below.
	% We don't check for a negative result from abs(), which
	% would indicate overflow, since we may trap int overflow
	% instead.
	%
	% XXX: What about machines that aren't 2's complement?
:- func int_to_integer(int) = integer.
int_to_integer(D) = Int :-
	( int__min_int(D) ->
		% were we to call int__abs, int overflow might occur.
		Int = integer(D + 1) - one
	;
		int__abs(D, AD),
		Int = i(signum(D), pos_int_to_digits(AD))
	).

:- func int_max = int.
int_max = Maxint :-
	int__max_int(Maxint).

:- func signum(int) = int.
signum(N) = SN :-
	( N < 0 ->
		SN = -1
	; N = 0 ->
		SN = 0
	;
		SN = 1
	).

:- func pos_int_to_digits(int) = list(digit).
pos_int_to_digits(D) = Result :-
	( D = 0 ->
		Result = []
	;
		Result = [ S1 | pos_int_to_digits(C1) ],
		chop(D, S1, C1)
	).

	% Multiply a list of digits by the base.
:- func mul_base(list(digit)) = list(digit).
mul_base(Xs) =
	[0|Xs].

:- func mul_by_digit(digit, list(digit)) = list(digit).
mul_by_digit(D, Xs) = Norm :-
	Norm = norm(DXs),
	DXs = mul_by_digit_2(D, Xs).

:- func mul_by_digit_2(digit, list(digit)) = list(digit).
mul_by_digit_2(_D, []) = [].
mul_by_digit_2(D, [X|Xs]) = [ D*X | mul_by_digit_2(D, Xs) ].

	% Normalise a list of ints so that each element of the list
	% is a base 10000 digit and there are no extraneous zeros
	% at the big end. (Note: the big end (most significant
	% digit) is at the end of the list.)
:- func norm(list(int)) = list(digit).
norm(Xs) =
	nuke_zeros(norm_2(Xs, 0)).

:- func nuke_zeros(list(digit)) = list(digit).
nuke_zeros(Xs) = Zs :-
	list__reverse(Xs, RXs),
	RZs = drop_while(equals_zero, RXs),
	list__reverse(RZs, Zs).

:- func norm_2(list(int), digit) = list(digit).
norm_2([], C) = Xs :-
	( C = 0 ->
		Xs = []
	;
		Xs = [C]
	).
norm_2([X|Xs], C) = [S1 | norm_2(Xs, C1)] :-
	XC = X + C,
	chop(XC, S1, C1).

	% Chop an integer into the first two digits of its
	% base 10000 representation.
:- pred chop(int, digit, digit).
:- mode chop(in, out, out) is det.
chop(N, Dig, Carry) :-
	Dig = N mod base,
	Carry = N div base.


:- pred equals_zero(int).
:- mode equals_zero(in) is semidet.
equals_zero(X) :-
	X = 0.

:- func drop_while(pred(T), list(T)) = list(T).
:- mode drop_while(pred(in) is semidet, in) = out is det.
drop_while(_F, []) = [].
drop_while(F, [X|Xs]) =
	( call(F,X) ->
		drop_while(F, Xs)
	;
		[X|Xs]
	).

:- func pos_plus(list(digit), list(digit)) = list(digit).
pos_plus(Xs, Ys) = Norm :-
	Norm = norm(Sums),
	Sums = add_pairs(Xs, Ys).

:- func pos_sub(list(digit), list(digit)) = list(digit).
pos_sub(Xs, Ys) = Norm :-
	Norm = norm(Diffs),
	Diffs = diff_pairs(Xs, Ys).

:- func add_pairs(list(int), list(int)) = list(int).
add_pairs(XXs, YYs) = XYs :-
	( XXs = [],
		XYs = YYs
	; YYs = [], XXs = [_|_],
		XYs = XXs
	; XXs = [X|Xs], YYs = [Y|Ys],
		XYs = [ X+Y | add_pairs(Xs, Ys) ]
	).

:- func diff_pairs(list(int), list(int)) = list(int).
diff_pairs(XXs, YYs) = XYs :-
	( XXs = [],
		list__map(int_negate, YYs, XYs)
	; YYs = [], XXs = [_|_],
		XYs = XXs
	; XXs = [X|Xs], YYs = [Y|Ys],
		XYs = [ X-Y | diff_pairs(Xs, Ys) ]
	).

:- pred int_negate(int, int).
:- mode int_negate(in, out) is det.
int_negate(M, NegM) :-
	NegM = -M.

:- func pos_mul(list(digit), list(digit)) = list(digit).
pos_mul([], _Ys) = [].
pos_mul([X|Xs], Ys) = Sum :-
	mul_by_digit(X, Ys) = XYs,
	pos_mul(Xs, Ys) = XsYs,
	mul_base(XsYs) = TenXsYs,
	Sum = pos_plus(XYs, TenXsYs).

integer__to_string(N) = S :-
	integer_to_string_2(N) = S.

:- func integer_to_string_2(integer) = string.
integer_to_string_2(i(S, Ds)) = Str :-
	string__append(Sgn, digits_to_string(Ds), Str),
	( S = (-1) ->
		Sgn = "-"
	;
		Sgn = ""
	).

:- func digits_to_string(list(digit)) = string.
digits_to_string(DDs) = Str :-
	list__reverse(DDs, Rev),
	( Rev = [],
		Str = "0"
	; Rev = [R|Rs],
		string__int_to_string(R, S),
		list__map(digit_to_string, Rs, Ss),
		string__append_list([S|Ss], Str)
	).

:- pred digit_to_string(digit, string).
:- mode digit_to_string(in, out) is det.
digit_to_string(D, S) :-
	string__int_to_string(D, S1),
	Width = log10base,
	string__pad_left(S1, '0', Width, S).


:- pred big_quot_rem(integer, integer, integer, integer).
:- mode big_quot_rem(in, in, out, out) is det.
big_quot_rem(N1, N2, Qt, Rm) :-
	( N2 = zero ->
		error("integer__big_quot_rem: division by zero")
	; N1 = zero ->
		Qt = zero,
		Rm = N2
	; 
		N1 = i(S1, D1),
		N2 = i(S2, D2),
		Qt = i(SQ, Q),
		Rm = i(SR, R),
		( pos_is_zero(R) ->
			SR = 0
		;
			SR = S1
		),
		( pos_is_zero(Q) ->
			SQ = 0
		;
			SQ = S1 * S2
		),
		Q = norm(QRR),
		R = norm(RRR),
		list__reverse(RR, RRR),
		list__reverse(QR, QRR),
		list__reverse(D1, D1R),
		list__reverse(D2, D2R),
		quot_rem_rev([], D1R, D2R, QR, RR)
	).

	% Algorithm: We take digits from the start of U (call them Ur)
	% and divide by V to get a digit Q of the ratio.
	% Essentially the usual long division algorithm.
	% Qhat is an approximation to Q. It may be at most 2 too big.
	%
	% If the first digit of V is less than base/2, then
	% we scale both the numerator and denominator. This
	% way, we can use Knuth's[*] nifty trick for finding
	% an accurate approximation to Q. That's all we use from
	% Knuth; his MIX algorithm is fugly.
	%
	% [*] Knuth, Semi-numerical algorithms.
	%
:- pred quot_rem_rev(list(digit), list(digit), list(digit), list(digit),
	list(digit)).
:- mode quot_rem_rev(in, in, in, out, out) is det.
quot_rem_rev(Ur, U, V, Qt, Rm) :-
	( V = [V0|_] ->
		BaseDiv2 = base div 2,
		( V0 < BaseDiv2 ->
			quot_rem_rev_2(mul_by_digit_rev(M, Ur),
				mul_by_digit_rev(M, U),
				mul_by_digit_rev(M, V), Q, R),
			Qt = Q,
			Rm = div_by_digit_rev(M, R),
			M = base div (V0 + 1)
		;
			quot_rem_rev_2(Ur, U, V, Qt, Rm)
		)
	;
		error("integer__quot_rem_rev: software error")
	).

:- pred quot_rem_rev_2(list(digit), list(digit), list(digit), list(digit),
	list(digit)).
:- mode quot_rem_rev_2(in, in, in, out, out) is det.
quot_rem_rev_2(Ur, U, V, Qt, Rm) :-
	( pos_lt_rev(Ur, V) ->
		( U = [],
			Qt = [0],
			Rm = Ur
		; U = [Ua|Uas],
			quot_rem_rev_2(UrUa, Uas, V, Quot, Rem),
			Qt = [0|Quot],
			Rm = Rem,
			list__append(Ur, [Ua], UrUa)
		)
	;
		( U = [],
			Qt = [Q],
			Rm = NewUr
		; U = [Ua|Uas],
			quot_rem_rev_2(NewUrUa, Uas, V, Quot, Rem),
			Qt = [Q|Quot],
			Rm = Rem,
			list__append(NewUr, [Ua], NewUrUa)
		),
		NewUr = pos_sub_rev(Ur, mul_by_digit_rev(Q, V)),
		( pos_geq_rev(Ur, mul_by_digit_rev(Qhat, V)) ->
			Q = Qhat
		; pos_geq_rev(Ur, mul_by_digit_rev(Qhat - 1, V)) ->
			Q = Qhat - 1
		;
			Q = Qhat - 2
		),
		V0 = head(V),
		U0 = head(Ur),
		LengthUr = length(Ur),
		LengthV = length(V),
		( LengthUr > LengthV ->
			Qhat = (U0*B+U1) div V0,
			U1 = head(tail(Ur))
		;
			Qhat = U0 div V0
		),
		B = base
	).

:- func length(list(T)) = int.
length([]) = 0.
length([_|Xs]) = 1 + length(Xs).

:- func head(list(T)) = T.
head(HT) = H :-
	( HT = [Hd|_T] ->
		H = Hd
	;
		error("integer__head: []")
	).
		
:- func tail(list(T)) = list(T).
tail(HT) = T :-
	( HT = [_H|Tl] ->
		T = Tl
	;
		error("integer__tail: []")
	).


	% Multiply a *reverse* list of digits (big end first)
	% by a digit. 
	%
	% Note: All functions whose name has the suffix "_rev"
	% operate on such reverse lists of digits.
:- func mul_by_digit_rev(digit, list(digit)) = list(digit).
mul_by_digit_rev(D, Xs) = Rev :-
	list__reverse(Xs, RXs),
	Mul = mul_by_digit(D, RXs),
	list__reverse(Mul, Rev).

:- func div_by_digit_rev(digit, list(digit)) = list(digit).
div_by_digit_rev(_D, []) = [].
div_by_digit_rev(D, [X|Xs]) = div_by_digit_rev_2(X, Xs, D).

:- func div_by_digit_rev_2(digit, list(digit), digit) = list(digit).
div_by_digit_rev_2(X, Xs, D) = [Q|Rest] :-
	Q = X div D,
	( Xs = [],
		Rest = []
	; Xs = [H|T],
		Rest = div_by_digit_rev_2(R*base + H, T, D),
		R = X rem D
	).

:- func pos_sub_rev(list(digit), list(digit)) = list(digit).
pos_sub_rev(Xs, Ys) = Rev :-
	list__reverse(Xs, RXs),
	list__reverse(Ys, RYs),
	Sum = pos_sub(RXs, RYs),
	list__reverse(Sum, Rev).

:- pred pos_lt_rev(list(digit), list(digit)).
:- mode pos_lt_rev(in, in) is semidet.
pos_lt_rev(Xs, Ys) :-
	list__reverse(Xs, RXs),
	list__reverse(Ys, RYs),
	C = big_cmp(i(1, RXs), i(1, RYs)),
	C = lessthan.

:- pred pos_geq_rev(list(digit), list(digit)).
:- mode pos_geq_rev(in, in) is semidet.
pos_geq_rev(Xs, Ys) :-
	list__reverse(Xs, RXs),
	list__reverse(Ys, RYs),
	C = big_cmp(i(1, RXs), i(1, RYs)),
	( C = greaterthan ; C = equal ).

:- pred pos_is_zero(list(digit)).
:- mode pos_is_zero(in) is semidet.
pos_is_zero(Ds) :-
	NDs = nuke_zeros(Ds),
	NDs = [].

integer__pow(A, N, P) :-
	Zero = zero,
	( N < Zero ->
		error("integer__pow: negative exponent")
	;
		P = big_pow(A, N)
	).

:- func big_pow(integer, integer) = integer.
big_pow(A, N) = P :-
	( N = zero ->
		P = one
	; big_odd(N) ->
		P = A * big_pow(A, N-one)
	; % even
		P = big_sqr(big_pow(A, N//two))
	).

:- func two = integer.
two = integer(2).
	
:- func big_sqr(integer) = integer.
big_sqr(A) = A * A.

:- pred big_odd(integer).
:- mode big_odd(in) is semidet.
big_odd(N) :-
	N = i(_S, [D|_Ds]),
	Dmod2 is D mod 2,
	Dmod2 = 1.

