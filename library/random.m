%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% rand.m
%
% main author: conway
%
% Define a set of random number generator predicates. This implementation
% uses a threaded random-number supply. It could be made non-unique, but
% since each thread returns the same list of random numbers, in the interests
% of safety, it is declared with unique modes.
%
%---------------------------------------------------------------------------%

:- module random.

:- interface.

:- import_module int, list.

:- type random__supply.

:- pred random__init(int, random__supply).
:- mode random__init(in, uo) is det.

:- pred random__random(int, random__supply, random__supply).
:- mode random__random(out, di, uo) is det.

:- pred random__randmax(int, random__supply, random__supply).
:- mode random__randmax(out, di, uo) is det.

:- pred random__test(int, int, list(int), int).
:- mode random__test(in, in, out, out) is det.

:- pred random__bit_reverse(int::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module require.

:- type random__supply		==	int.	% I(j)

:- pred random__params(int, int, int).	% a, c, m
:- mode random__params(out, out, out) is det.

random__params(2416, 374441, 1771875).

random__init(I0, RS) :-
	RS = I0.

/**** temp hack - we use the version below
random__random(I, RS0, RS) :-
	RS0 = I0,
	random__params(A, C, M),
	I1 is I0 * A,
	I2 is I1 + C,
	I is I2 mod M,
	RS = I.
*****/

random__randmax(M1, Rs0, Rs) :-
	Rs0 = I,
	random__params(_A, _C, M),
	M1 is M - 1,
	Rs = I.

/*** start of temp hack ****/
% we use this code since the above doesn't work with sicstus
random__random(R, X, X1) :-
	X1 is X + 1,
	random__bit_reverse(X1, R).


random__bit_reverse(A, B) :-
	A0 is A /\ 255,
	A8 is (A >> 8),
	A1 is A8 /\ 255,
	A16 is (A >> 16),
	A2 is A16 /\ 255,
	( 
		random__bit_rev(A0, B2),
		random__bit_rev(A1, B1),
		random__bit_rev(A2, B0)
	->
		B2_16 is B2 << 16,
		B1_8 is B1 << 8,
		B10 is B1_8 + B0,
		B is B2_16 + B10
	;
		error("bit_reverse")
	).

% random__bit_rev(A, B) :-
% 	A0 is A /\ 1,
% 	A1 is (A >> 1) /\ 1,
% 	A2 is (A >> 2) /\ 1,
% 	A3 is (A >> 3) /\ 1,
% 	A4 is (A >> 4) /\ 1,
% 	A5 is (A >> 5) /\ 1,
% 	A6 is (A >> 6) /\ 1,
% 	A7 is (A >> 7) /\ 1,
% 	B is (A0 << 7) + (A1 << 6) + (A2 << 5) + (A3 << 4) + (A4 << 3) +
% 		(A5 << 2) + (A6 << 1) + A7.

:- pred random__bit_rev(int::in, int::out) is semidet.

random__bit_rev(0, 0).
random__bit_rev(1, 128).
random__bit_rev(2, 64).
random__bit_rev(3, 192).
random__bit_rev(4, 32).
random__bit_rev(5, 160).
random__bit_rev(6, 96).
random__bit_rev(7, 224).
random__bit_rev(8, 16).
random__bit_rev(9, 144).
random__bit_rev(10, 80).
random__bit_rev(11, 208).
random__bit_rev(12, 48).
random__bit_rev(13, 176).
random__bit_rev(14, 112).
random__bit_rev(15, 240).
random__bit_rev(16, 8).
random__bit_rev(17, 136).
random__bit_rev(18, 72).
random__bit_rev(19, 200).
random__bit_rev(20, 40).
random__bit_rev(21, 168).
random__bit_rev(22, 104).
random__bit_rev(23, 232).
random__bit_rev(24, 24).
random__bit_rev(25, 152).
random__bit_rev(26, 88).
random__bit_rev(27, 216).
random__bit_rev(28, 56).
random__bit_rev(29, 184).
random__bit_rev(30, 120).
random__bit_rev(31, 248).
random__bit_rev(32, 4).
random__bit_rev(33, 132).
random__bit_rev(34, 68).
random__bit_rev(35, 196).
random__bit_rev(36, 36).
random__bit_rev(37, 164).
random__bit_rev(38, 100).
random__bit_rev(39, 228).
random__bit_rev(40, 20).
random__bit_rev(41, 148).
random__bit_rev(42, 84).
random__bit_rev(43, 212).
random__bit_rev(44, 52).
random__bit_rev(45, 180).
random__bit_rev(46, 116).
random__bit_rev(47, 244).
random__bit_rev(48, 12).
random__bit_rev(49, 140).
random__bit_rev(50, 76).
random__bit_rev(51, 204).
random__bit_rev(52, 44).
random__bit_rev(53, 172).
random__bit_rev(54, 108).
random__bit_rev(55, 236).
random__bit_rev(56, 28).
random__bit_rev(57, 156).
random__bit_rev(58, 92).
random__bit_rev(59, 220).
random__bit_rev(60, 60).
random__bit_rev(61, 188).
random__bit_rev(62, 124).
random__bit_rev(63, 252).
random__bit_rev(64, 2).
random__bit_rev(65, 130).
random__bit_rev(66, 66).
random__bit_rev(67, 194).
random__bit_rev(68, 34).
random__bit_rev(69, 162).
random__bit_rev(70, 98).
random__bit_rev(71, 226).
random__bit_rev(72, 18).
random__bit_rev(73, 146).
random__bit_rev(74, 82).
random__bit_rev(75, 210).
random__bit_rev(76, 50).
random__bit_rev(77, 178).
random__bit_rev(78, 114).
random__bit_rev(79, 242).
random__bit_rev(80, 10).
random__bit_rev(81, 138).
random__bit_rev(82, 74).
random__bit_rev(83, 202).
random__bit_rev(84, 42).
random__bit_rev(85, 170).
random__bit_rev(86, 106).
random__bit_rev(87, 234).
random__bit_rev(88, 26).
random__bit_rev(89, 154).
random__bit_rev(90, 90).
random__bit_rev(91, 218).
random__bit_rev(92, 58).
random__bit_rev(93, 186).
random__bit_rev(94, 122).
random__bit_rev(95, 250).
random__bit_rev(96, 6).
random__bit_rev(97, 134).
random__bit_rev(98, 70).
random__bit_rev(99, 198).
random__bit_rev(100, 38).
random__bit_rev(101, 166).
random__bit_rev(102, 102).
random__bit_rev(103, 230).
random__bit_rev(104, 22).
random__bit_rev(105, 150).
random__bit_rev(106, 86).
random__bit_rev(107, 214).
random__bit_rev(108, 54).
random__bit_rev(109, 182).
random__bit_rev(110, 118).
random__bit_rev(111, 246).
random__bit_rev(112, 14).
random__bit_rev(113, 142).
random__bit_rev(114, 78).
random__bit_rev(115, 206).
random__bit_rev(116, 46).
random__bit_rev(117, 174).
random__bit_rev(118, 110).
random__bit_rev(119, 238).
random__bit_rev(120, 30).
random__bit_rev(121, 158).
random__bit_rev(122, 94).
random__bit_rev(123, 222).
random__bit_rev(124, 62).
random__bit_rev(125, 190).
random__bit_rev(126, 126).
random__bit_rev(127, 254).
random__bit_rev(128, 1).
random__bit_rev(129, 129).
random__bit_rev(130, 65).
random__bit_rev(131, 193).
random__bit_rev(132, 33).
random__bit_rev(133, 161).
random__bit_rev(134, 97).
random__bit_rev(135, 225).
random__bit_rev(136, 17).
random__bit_rev(137, 145).
random__bit_rev(138, 81).
random__bit_rev(139, 209).
random__bit_rev(140, 49).
random__bit_rev(141, 177).
random__bit_rev(142, 113).
random__bit_rev(143, 241).
random__bit_rev(144, 9).
random__bit_rev(145, 137).
random__bit_rev(146, 73).
random__bit_rev(147, 201).
random__bit_rev(148, 41).
random__bit_rev(149, 169).
random__bit_rev(150, 105).
random__bit_rev(151, 233).
random__bit_rev(152, 25).
random__bit_rev(153, 153).
random__bit_rev(154, 89).
random__bit_rev(155, 217).
random__bit_rev(156, 57).
random__bit_rev(157, 185).
random__bit_rev(158, 121).
random__bit_rev(159, 249).
random__bit_rev(160, 5).
random__bit_rev(161, 133).
random__bit_rev(162, 69).
random__bit_rev(163, 197).
random__bit_rev(164, 37).
random__bit_rev(165, 165).
random__bit_rev(166, 101).
random__bit_rev(167, 229).
random__bit_rev(168, 21).
random__bit_rev(169, 149).
random__bit_rev(170, 85).
random__bit_rev(171, 213).
random__bit_rev(172, 53).
random__bit_rev(173, 181).
random__bit_rev(174, 117).
random__bit_rev(175, 245).
random__bit_rev(176, 13).
random__bit_rev(177, 141).
random__bit_rev(178, 77).
random__bit_rev(179, 205).
random__bit_rev(180, 45).
random__bit_rev(181, 173).
random__bit_rev(182, 109).
random__bit_rev(183, 237).
random__bit_rev(184, 29).
random__bit_rev(185, 157).
random__bit_rev(186, 93).
random__bit_rev(187, 221).
random__bit_rev(188, 61).
random__bit_rev(189, 189).
random__bit_rev(190, 125).
random__bit_rev(191, 253).
random__bit_rev(192, 3).
random__bit_rev(193, 131).
random__bit_rev(194, 67).
random__bit_rev(195, 195).
random__bit_rev(196, 35).
random__bit_rev(197, 163).
random__bit_rev(198, 99).
random__bit_rev(199, 227).
random__bit_rev(200, 19).
random__bit_rev(201, 147).
random__bit_rev(202, 83).
random__bit_rev(203, 211).
random__bit_rev(204, 51).
random__bit_rev(205, 179).
random__bit_rev(206, 115).
random__bit_rev(207, 243).
random__bit_rev(208, 11).
random__bit_rev(209, 139).
random__bit_rev(210, 75).
random__bit_rev(211, 203).
random__bit_rev(212, 43).
random__bit_rev(213, 171).
random__bit_rev(214, 107).
random__bit_rev(215, 235).
random__bit_rev(216, 27).
random__bit_rev(217, 155).
random__bit_rev(218, 91).
random__bit_rev(219, 219).
random__bit_rev(220, 59).
random__bit_rev(221, 187).
random__bit_rev(222, 123).
random__bit_rev(223, 251).
random__bit_rev(224, 7).
random__bit_rev(225, 135).
random__bit_rev(226, 71).
random__bit_rev(227, 199).
random__bit_rev(228, 39).
random__bit_rev(229, 167).
random__bit_rev(230, 103).
random__bit_rev(231, 231).
random__bit_rev(232, 23).
random__bit_rev(233, 151).
random__bit_rev(234, 87).
random__bit_rev(235, 215).
random__bit_rev(236, 55).
random__bit_rev(237, 183).
random__bit_rev(238, 119).
random__bit_rev(239, 247).
random__bit_rev(240, 15).
random__bit_rev(241, 143).
random__bit_rev(242, 79).
random__bit_rev(243, 207).
random__bit_rev(244, 47).
random__bit_rev(245, 175).
random__bit_rev(246, 111).
random__bit_rev(247, 239).
random__bit_rev(248, 31).
random__bit_rev(249, 159).
random__bit_rev(250, 95).
random__bit_rev(251, 223).
random__bit_rev(252, 63).
random__bit_rev(253, 191).
random__bit_rev(254, 127).
random__bit_rev(255, 255).

/**** end of temporary hack ********/
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

random__test(Seed, N, Nums, Max) :-
	random__init(Seed, RS),
	random__randmax(Max, RS, RS1),
	random__test_2(N, Nums, RS1, _RS2).

:- pred random__test_2(int, list(int), random__supply, random__supply).
:- mode random__test_2(in, out, in, out) is det.

random__test_2(N, Is, RS0, RS) :-
	(
		N > 0
	->
		N1 is N - 1,
		random__random(I, RS0, RS1),
		random__test_2(N1, Is0, RS1, RS),
		Is = [I|Is0]
	;
		Is = [],
		RS = RS0
	).

%---------------------------------------------------------------------------%
