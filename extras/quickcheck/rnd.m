%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2006 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% file: rnd.m
% main author: conway.
%
% This pseudo-random number generator is derived from C source due to
% George Marsaglia geo@stat.fsu.edu.
%
% The original source is included at the end of the module as a reference.
%
%---------------------------------------------------------------------------%

:- module rnd.

:- interface.

:- import_module float, int, list, pair.

:- type rnd.

	% initialize a random number supply
:- pred rnd__init(int, rnd).
:- mode rnd__init(in, out) is det.

	% get a random float on the range [0, 1)
:- pred rnd(float, rnd, rnd).
:- mode rnd(out, in, out) is det.

	% get a random int on the range [Lower, Upper]
:- pred irange(int, int, int, rnd, rnd).
:- mode irange(in, in, out, in, out) is det.

	% get a random float on the range [Lower, Upper)
:- pred frange(float, float, float, rnd, rnd).
:- mode frange(in, in, out, in, out) is det.

	% generate a random permutation of a list
:- pred shuffle(list(T), list(T), rnd, rnd).
:- mode shuffle(in, out, in, out) is det.

	% get a random element of a list.
:- pred oneof(list(T), T, rnd, rnd).
:- mode oneof(in, out, in, out) is det.

	% get a random element of a weighted list.
:- pred wghtd_oneof(list(pair(int, T)), T, rnd, rnd).
:- mode wghtd_oneof(in, out, in, out) is det.

	% gaussian(X, Y, Rnd0, Rnd)
	% generate a pair of gaussian deviates `X' and `Y'.
:- pred gaussian(float, float, rnd, rnd).
:- mode gaussian(out, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module math, require.

irange(Min, Max, Val, R0, R) :-
	frange(rfloat(Min), rfloat(Max+1), FVal, R0, R),
	Val = rint(FVal).

frange(Min, Max, Val, R0, R) :-
	rnd(J, R0, R),
	Val = J*(Max - Min)+Min.

shuffle(Ins, Outs, R0, R) :-
	list__length(Ins, N),
	shuffle2(N, Ins, [], T0, R0, R1),
	shuffle2(N, T0, [], T1, R1, R2),
	shuffle2(N, T1, [], T2, R2, R3),
	shuffle2(N, T2, [], T3, R3, R4),
	shuffle2(N, T3, [], U, R4, R5),
	shuffle2(N, U, [], Outs, R5, R).

:- pred shuffle2(int, list(T), list(T), list(T), rnd, rnd).
:- mode shuffle2(in, in, in, out, in, out) is det.

shuffle2(N, Ins, Acc0, Acc, R0, R) :-
	( N > 0 ->
		irange(0, N-1, J, R0, R1),
		delnth(Ins, J, Rest, T),
		shuffle2(N-1, Rest, [T|Acc0], Acc, R1, R)
	;
		Acc = Acc0,
		R = R0
	).

:- pred delnth(list(T), int, list(T), T).
:- mode delnth(in, in, out, out) is det.

delnth([], _, _, _) :-
	error("delnth: no enough elems!").
delnth([X|Xs], N, Zs, Z) :-
	( N =< 0 ->
		Z = X,
		Zs = Xs
	;
		Zs = [X|Ys],
		delnth(Xs, N-1, Ys, Z)
	).

oneof(Things, Thing, R0, R) :-
	list__length(Things, Num),
	irange(0, Num-1, X, R0, R),
	list__index0_det(Things, X, Thing).

wghtd_oneof(WghtdThings, Thing, R0, R) :-
	cumu(WghtdThings, 0, Sum),
	irange(0, Sum, X, R0, R),
	pick(WghtdThings, X, Thing).

:- pred cumu(list(pair(int, T)), int, int).
:- mode cumu(in, in, out) is det.

cumu([], Sum, Sum).
cumu([X - _T|Rest0], Sum, Sum1) :-
	cumu(Rest0, X+Sum, Sum1).

:- pred pick(list(pair(int, T)), int, T).
:- mode pick(in, in, out) is det.

pick([], _, _) :-
	error("pick: no things to pick from!").
pick([N - T|Rest], P, Thing) :-
	( N >= P ->
		Thing = T
	;
		pick(Rest, P - N, Thing)
	).

gaussian(X, Y, Rnd0, Rnd) :-
	frange(-1.0, 1.0, V1, Rnd0, Rnd1),
	frange(-1.0, 1.0, V2, Rnd1, Rnd2),
	R = V1*V1 + V2*V2,
	( R >= 1.0, R \= 0.0  ->
		gaussian(X, Y, Rnd2, Rnd)
	;
		Fac = sqrt(-2.0 * ln(R) / R),
		X = V1 * Fac,
		Y = V2 * Fac,
		Rnd = Rnd2
	).

%---------------------------------------------------------------------------%

:- type vec
	---> vec(int, int, int, int, int, int, int, int, int, int).

:- type rnd
	---> rnd(
		vec,
		vec,
		int
	).

rnd__init(Seed, rnd(M1, M2, Seed)) :-
	SN = Seed /\ ((1 << 15) - 1),
	N  = Seed /\ ((1 << 30) - 1),
	M1a = vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	M2a = vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	seed1(17, SN, N, M1a, M2a, M1b, M2b),
	set(M1b, 0, (M1b ** 0) /\ ((1 << 15) - 1), M1),
	set(M2b, 0, (M2b ** 0) /\ ((1 << 15) - 1), M2).

:- pred seed1(int, int, int, vec, vec, vec, vec).
:- mode seed1(in, in, in, in, in, out, out) is det.

seed1(N, SNum0, Num0, M1a, M2a, M1, M2) :-
	(N > 0 ->
		Num1 = 30903 * SNum0 + (Num0 >> 15),
		SNum1 = Num1 /\ ((1 << 15) - 1),
		( N >= 9 ->
			M2b = M2a,
			set(M1a, 17 - N, SNum1, M1b)
		;
			M1b = M1a,
			set(M2a, 8 - N, SNum1, M2b)
		),
		seed1(N-1, SNum1, Num1, M1b, M2b, M1, M2)
	;
		M1 = M1a,
		M2 = M2a
	).

rnd(Res, rnd(M1a, M2a, _Seed0), rnd(M1d, M2d, Seed1)) :-
	shift(M1a, M1b),
	shift(M2a, M2b),
	N1a = (M1b ** 0),
	N2a = (M2b ** 0),

	N1b = N1a + 1941 * (M1b ** 2) + 1860 * (M1b ** 3) + 1812 * (M1b ** 4)
		+ 1776 * (M1b ** 5) + 1492 * (M1b ** 6) + 1215 * (M1b ** 7)
		+ 1066 * (M1b ** 8) + 12013 * (M1b ** 9),

	N2b = N2a + 1111 * (M2b ** 2) + 2222 * (M2b ** 3) + 3333 * (M2b ** 4)
		+ 4444 * (M2b ** 5) + 5555 * (M2b ** 6) + 6666 * (M2b ** 7)
		+ 7777 * (M2b ** 8) + 9272 * (M2b ** 9),

	set(M1b, 0, (N1b >> 15) /\ ((1 << 15) - 1), M1c),
	set(M2b, 0, (N2b >> 15) /\ ((1 << 15) - 1), M2c),

	set(M1c, 1, N1b /\ ((1 << 15) - 1), M1d),
	set(M2c, 1, N2b /\ ((1 << 15) - 1), M2d),

	Seed1 = ((M1d ** 1) << 15) + (M2d ** 1),

	Res = rfloat(Seed1)/rfloat((1 << 30) - 1).

:- pred shift(vec, vec).
:- mode shift(in, out) is det.

shift(Vec0, Vec1) :-
	Vec0 = vec(A, B, C, D, E, F, G, H, I, _),
	Vec1 = vec(A, B, B, C, D, E, F, G, H, I).

%---------------------------------------------------------------------------%

:- func (vec ** int) = int.
:- mode ((in ** in) = out) is det.
:- mode ((in ** in(bound(0))) = out) is det.
:- mode ((in ** in(bound(1))) = out) is det.
:- mode ((in ** in(bound(2))) = out) is det.
:- mode ((in ** in(bound(3))) = out) is det.
:- mode ((in ** in(bound(4))) = out) is det.
:- mode ((in ** in(bound(5))) = out) is det.
:- mode ((in ** in(bound(6))) = out) is det.
:- mode ((in ** in(bound(7))) = out) is det.
:- mode ((in ** in(bound(8))) = out) is det.
:- mode ((in ** in(bound(9))) = out) is det.

( Vec ** Ind ) = Res :-
	Vec = vec(A, B, C, D, E, F, G, H, I, J),
	(
		( Ind = 0, Res0 = A
		; Ind = 1, Res0 = B
		; Ind = 2, Res0 = C
		; Ind = 3, Res0 = D
		; Ind = 4, Res0 = E
		; Ind = 5, Res0 = F
		; Ind = 6, Res0 = G
		; Ind = 7, Res0 = H
		; Ind = 8, Res0 = I
		; Ind = 9, Res0 = J
		)
	->
		Res = Res0
	;
		error("**: out of range")
	).

:- pred set(vec, int, int, vec).
:- mode set(in, in, in, out) is det.
:- mode set(in, in(bound(0)), in, out) is det.
:- mode set(in, in(bound(1)), in, out) is det.
:- mode set(in, in(bound(2)), in, out) is det.
:- mode set(in, in(bound(3)), in, out) is det.
:- mode set(in, in(bound(4)), in, out) is det.
:- mode set(in, in(bound(5)), in, out) is det.
:- mode set(in, in(bound(6)), in, out) is det.
:- mode set(in, in(bound(7)), in, out) is det.
:- mode set(in, in(bound(8)), in, out) is det.
:- mode set(in, in(bound(9)), in, out) is det.

set(Vec0, Ind, V, Vec) :-
	Vec0 = vec(A, B, C, D, E, F, G, H, I, J),
	(
		( Ind = 0, Vec1 = vec(V, B, C, D, E, F, G, H, I, J)
		; Ind = 1, Vec1 = vec(A, V, C, D, E, F, G, H, I, J)
		; Ind = 2, Vec1 = vec(A, B, V, D, E, F, G, H, I, J)
		; Ind = 3, Vec1 = vec(A, B, C, V, E, F, G, H, I, J)
		; Ind = 4, Vec1 = vec(A, B, C, D, V, F, G, H, I, J)
		; Ind = 5, Vec1 = vec(A, B, C, D, E, V, G, H, I, J)
		; Ind = 6, Vec1 = vec(A, B, C, D, E, F, V, H, I, J)
		; Ind = 7, Vec1 = vec(A, B, C, D, E, F, G, V, I, J)
		; Ind = 8, Vec1 = vec(A, B, C, D, E, F, G, H, V, J)
		; Ind = 9, Vec1 = vec(A, B, C, D, E, F, G, H, I, V)
		)
	->
		Vec = Vec1
	;
		error("set: out of range")
	).

%---------------------------------------------------------------------------%

:- func rfloat(int) = float.
:- pragma foreign_proc("C",
	rfloat(I::in) = (F::out),
	[promise_pure, will_not_call_mercury],	
"
	F = I;
").

:- func rint(float) = int.
:- pragma foreign_proc("C",
	rint(F::in) = (I::out),
	[promise_pure, will_not_call_mercury],	
"
	I = F;
").

%---------------------------------------------------------------------------%
%
%/*
%
%Article: 16024 of sci.math.num-analysis
%Xref: taurus.cs.nps.navy.mil sci.stat.consult:7790 sci.math.num-analysis:16024
%Path: taurus.cs.nps.navy.mil!lll-winken.llnl.gov!uwm.edu!news.alpha.net!news.mathworks.com!udel!ssnet.com!usenet
%From: Bob Wheeler <bwheeler@ssnet.com>
%Newsgroups: sci.stat.consult,sci.math.num-analysis
%Subject: Marsaglia's Mother of all RNG's (Long?)
%Date: Fri, 28 Oct 94 19:32:08 EDT
%Organization: SSNet -- Public Internet Access in Delaware!
%Lines: 285
%Distribution: inet
%Message-ID: <38s2p1$qaf@marlin.ssnet.com>
%NNTP-Posting-Host: echip.com
%Mime-Version: 1.0
%Content-Type: TEXT/PLAIN; charset=US-ASCII
%X-Newsreader: NEWTNews & Chameleon -- TCP/IP for MS Windows from NetManage
%
%
%Several people have asked me to post this:
%First the C program, then George Marsaliga's post with details
%about the RNG.  He claims a period of about 2^250 for this and
%that it passes all of the usual tests.  I've tried it enough
%to be sure his claim is reasonable.
%
%The program:
%
%*/
%
%
%#include <string.h>
%
%static short mother1[10];
%static short mother2[10];
%static short mStart=1;
%
%#define m16Long 65536L				/* 2^16 */
%#define m16Mask 0xFFFF          /* mask for lower 16 bits */
%#define m15Mask 0x7FFF			/* mask for lower 15 bits */
%#define m31Mask 0x7FFFFFFF     /* mask for 31 bits */
%#define m32Double  4294967295.0  /* 2^32-1 */
%
%/* Mother **************************************************************
%|	George Marsaglia's The mother of all random number generators
%|		producing uniformly distributed pseudo random 32 bit values 
%with
%|		period about 2^250.
%|	The text of Marsaglia's posting is appended at the end of the function.
%|
%|	The arrays mother1 and mother2 store carry values in their
%|		first element, and random 16 bit numbers in elements 1 to 8.
%|		These random numbers are moved to elements 2 to 9 and a new
%|		carry and number are generated and placed in elements 0 and 1.
%|	The arrays mother1 and mother2 are filled with random 16 bit values
%|		on first call of Mother by another generator.  mStart is the 
%switch.
%|
%|	Returns:
%|	A 32 bit random number is obtained by combining the output of the
%|		two generators and returned in *pSeed.  It is also scaled by
%|		2^32-1 and returned as a double between 0 and 1
%|
%|	SEED:
%|	The inital value of *pSeed may be any long value
%|
%|	Bob Wheeler 8/8/94
%*/
%
%
%double Mother(unsigned long *pSeed)
%{
%	unsigned long  number,
%						number1,
%						number2;
%	short n,
%			*p;
%	unsigned short sNumber;
%
%		/* Initialize motheri with 9 random values the first time */
%	if (mStart) {
%		sNumber= *pSeed&m16Mask;   /* The low 16 bits */
%		number= *pSeed&m31Mask;   /* Only want 31 bits */
%
%		p=mother1;
%		for (n=18;n--;) {
%			number=30903*sNumber+(number>>16);   /* One line 
%multiply-with-cary */
%			*p++=sNumber=number&m16Mask;
%			if (n==9)
%				p=mother2;
%		}
%		/* make cary 15 bits */
%		mother1[0]&=m15Mask;
%		mother2[0]&=m15Mask;
%		mStart=0;
%	}
%
%		/* Move elements 1 to 8 to 2 to 9 */
%	memmove(mother1+2,mother1+1,8*sizeof(short));
%	memmove(mother2+2,mother2+1,8*sizeof(short));
%
%		/* Put the carry values in numberi */
%	number1=mother1[0];
%	number2=mother2[0];
%
%		/* Form the linear combinations */
%	
%number1+=1941*mother1[2]+1860*mother1[3]+1812*mother1[4]+1776*mother1[5]+
%				
%1492*mother1[6]+1215*mother1[7]+1066*mother1[8]+12013*mother1[9];
%	
%number2+=1111*mother2[2]+2222*mother2[3]+3333*mother2[4]+4444*mother2[5]+
%				
%5555*mother2[6]+6666*mother2[7]+7777*mother2[8]+9272*mother2[9];
%
%		/* Save the high bits of numberi as the new carry */
%	mother1[0]=number1/m16Long;
%	mother2[0]=number2/m16Long;
%		/* Put the low bits of numberi into motheri[1] */
%	mother1[1]=m16Mask&number1;
%	mother2[1]=m16Mask&number2;
%
%		/* Combine the two 16 bit random numbers into one 32 bit */
%	*pSeed=(((long)mother1[1])<<16)+(long)mother2[1];
%
%		/* Return a double value between 0 and 1 */
%	return ((double)*pSeed)/m32Double;
%}
%
%
%
%/*
%
%*********************
%Marsaglia's comments
%
%		 Yet another RNG
%Random number generators are frequently posted on
%the network; my colleagues and I posted ULTRA in
%1992 and, from the number of requests for releases
%to use it in software packages, it seems to be
%widely used.
%
%I have long been interested in RNG's and several
%of my early ones are used as system generators or
%in statistical packages.
%
%So why another one?  And why here?
%
%Because I want to describe a generator, or
%rather, a class of generators, so promising
%I am inclined to call it
%
%	The Mother of All Random Number Generators
%
%and because the generator seems promising enough
%to justify shortcutting the many months, even
%years, before new developments are widely
%known through publication in a journal.
%
%This new class leads to simple, fast programs that
%produce sequences with very long periods.  They
%use multiplication, which experience has shown
%does a better job of mixing bits than do +,- or
%exclusive-or, and they do it with easily-
%implemented arithmetic modulo a power of 2, unlike
%arithmetic modulo a prime.  The latter, while
%satisfactory, is difficult to implement.  But the
%arithmetic here modulo 2^16 or 2^32 does not suffer
%the flaws of ordinary congruential generators for
%those moduli: trailing bits too regular.  On the
%contrary, all bits of the integers produced by
%this new method, whether leading or trailing, have
%passed extensive tests of randomness.
%
%Here is an idea of how it works, using, say, integers
%of six decimal digits from which we return random 3-
%digit integers.  Start with n=123456, the seed.
%
%Then form a new n=672*456+123=306555 and return 555.
%Then form a new n=672*555+306=373266 and return 266.
%Then form a new n=672*266+373=179125 and return 125,
%
%and so on.  Got it?  This is a multiply-with-carry
%sequence x(n)=672*x(n-1)+ carry mod b=1000, where
%the carry is the number of b's dropped in the
%modular reduction. The resulting sequence of 3-
%digit x's has period 335,999.  Try it.
%
%No big deal, but that's just an example to give
%the idea. Now consider the sequence of 16-bit
%integers produced by the two C statements:
%
%k=30903*(k&65535)+(k>>16); return(k&65535);
%
%Notice that it is doing just what we did in the
%example: multiply the bottom half (by 30903,
%carefully chosen), add the top half and return the
%new bottom.
%
%That will produce a sequence of 16-bit integers
%with period > 2^29, and if we concatenate two
%such:
%	  k=30903*(k&65535)+(k>>16);
%	  j=18000*(j&65535)+(j>>16);
%	  return((k<<16)+j);
%we get a sequence of more than 2^59 32-bit integers
%before cycling.
%
%The following segment in a (properly initialized)
%C procedure will generate more than 2^118
%32-bit random integers from six random seed values
%i,j,k,l,m,n:
%		  k=30903*(k&65535)+(k>>16);
%		  j=18000*(j&65535)+(j>>16);
%		  i=29013*(i&65535)+(i>>16);
%		  l=30345*(l&65535)+(l>>16);
%		  m=30903*(m&65535)+(m>>16);
%		  n=31083*(n&65535)+(n>>16);
%		  return((k+i+m)>>16)+j+l+n);
%
%And it will do it much faster than any of several
%widely used generators designed to use 16-bit
%integer arithmetic, such as that of Wichman-Hill
%that combines congruential sequences for three
%15-bit primes (Applied Statistics, v31, p188-190,
%1982), period about 2^42.
%
%I call these multiply-with-carry generators. Here
%is an extravagant 16-bit example that is easily
%implemented in C or Fortran. It does such a
%thorough job of mixing the bits of the previous
%eight values that it is difficult to imagine a
%test of randomness it could not pass:
%
%x[n]=12013x[n-8]+1066x[n-7]+1215x[n-6]+1492x[n-5]+1776x[n-4]
% +1812x[n-3]+1860x[n-2]+1941x[n-1]+carry mod 2^16.
%
%The linear combination occupies at most 31 bits of
%a 32-bit integer. The bottom 16 is the output, the
%top 15 the next carry. It is probably best to
%implement with 8 case segments. It takes 8
%microseconds on my PC. Of course it just provides
%16-bit random integers, but awfully good ones. For
%32 bits you would have to combine it with another,
%such as
%
%x[n]=9272x[n-8]+7777x[n-7]+6666x[n-6]+5555x[n-5]+4444x[n-4]
%	 +3333x[n-3]+2222x[n-2]+1111x[n-1]+carry mod 2^16.
%
%Concatenating those two gives a sequence of 32-bit
%random integers (from 16 random 16-bit seeds),
%period about 2^250. It is so awesome it may merit
%the Mother of All RNG's title.
%
%The coefficients in those two linear combinations
%suggest that it is easy to get long-period
%sequences, and that is true.  The result is due to
%Cemal Kac, who extended the theory we gave for
%add-with-carry sequences: Choose a base b and give
%r seed values x[1],...,x[r] and an initial 'carry'
%c. Then the multiply-with-carry sequence
%
% x[n]=a1*x[n-1]+a2*x[n-2]+...+ar*x[n-r]+carry mod b,
%
%where the new carry is the number of b's dropped
%in the modular reduction, will have period the
%order of b in the group of residues relatively
%prime to m=ar*b^r+...+a1b^1-1.  Furthermore, the
%x's are, in reverse order, the digits in the
%expansion of k/m to the base b, for some 0<k<m.
%
%In practice b=2^16 or b=2^32 allows the new
%integer and the new carry to be the bottom and top
%half of a 32- or 64-bit linear combination of  16-
%or 32-bit integers.  And it is easy to find
%suitable m's if you have a primality test:  just
%search through candidate coefficients until you
%get an m that is a safeprime---both m and (m-1)/2
%are prime.  Then the period of the multiply-with-
%carry sequence will be the prime (m-1)/2. (It
%can't be m-1 because b=2^16 or 2^32 is a square.)
%
%Here is an interesting simple MWC generator with
%period> 2^92, for 32-bit arithmetic:
%
%x[n]=1111111464*(x[n-1]+x[n-2]) + carry mod 2^32.
%
%Suppose you have functions, say top() and bot(),
%that give the top and bottom halves of a 64-bit
%result.  Then, with initial 32-bit x, y and carry
%c,  simple statements such as
%	  y=bot(1111111464*(x+y)+c)
%	  x=y
%	  c=top(y)
%will, repeated, give over 2^92 random 32-bit y's.
%
%Not many machines have 64 bit integers yet.  But
%most assemblers for modern CPU's permit access to
%the top and bottom halves of a 64-bit product.
%
%I don't know how to readily access the top half of
%a 64-bit product in C.  Can anyone suggest how it
%might be done? (in integer arithmetic)
%
%George Marsaglia geo@stat.fsu.edu
%
%*/
%
%
%
%
%
