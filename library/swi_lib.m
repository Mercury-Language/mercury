%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Various predicates for SWI-Prolog compatibility.

%-----------------------------------------------------------------------------%

nuprolog :-
	fail.

compound(X) :-
	functor(X, _, _).

putprop(Atom, Key, Property) :-
	retractall(property(Atom, Key, _)),
	assert(property(Atom, Key, Property)).
getprop(Atom, Key, Property) :-
	property(Atom, Key, Property).
remprop(Atom, Key) :-
	retractall(property(Atom, Key, _Property)).

currentInput(X) :-
	current_input(X).

currentOutput(X) :-
	current_output(X).

flushOutput(X) :-
	flush_output(X).

setInput(X) :-
	set_input(X).

setOutput(X) :-
	set_output(X).

lineCount(X,Y) :-
	line_count(X,Y).

eof(end_of_file).

member(Element, List, SubList) :-
	SubList = [Element | _],
	append(_, SubList, List).

	% define =/3 for DCGs
=(A, A, A).

system(Command, Status) :-
	name(Com, Command),
	shell(Com, Status).

%-----------------------------------------------------------------------------%

% Various hacks to get things to work

random__random(R, X, X1) :-
	X1 is X + 1,
	bit_reverse(X1, R).

bit_reverse(A, B) :-
	A0 is A /\ 255,
	A1 is (A >> 8) /\ 255,
	A2 is (A >> 16) /\ 255,
	bit_rev(A0, B2),
	bit_rev(A1, B1),
	bit_rev(A2, B0),
	B is (B2 << 16) + (B1 << 8) + B0.

/*
bit_rev(A, B) :-
	A0 is A /\ 1,
	A1 is (A >> 1) /\ 1,
	A2 is (A >> 2) /\ 1,
	A3 is (A >> 3) /\ 1,
	A4 is (A >> 4) /\ 1,
	A5 is (A >> 5) /\ 1,
	A6 is (A >> 6) /\ 1,
	A7 is (A >> 7) /\ 1,
	B is (A0 << 7) + (A1 << 6) + (A2 << 5) + (A3 << 4) + (A4 << 3) +
		(A5 << 2) + (A6 << 1) + A7.
*/
bit_rev(0, 0).
bit_rev(1, 128).
bit_rev(2, 64).
bit_rev(3, 192).
bit_rev(4, 32).
bit_rev(5, 160).
bit_rev(6, 96).
bit_rev(7, 224).
bit_rev(8, 16).
bit_rev(9, 144).
bit_rev(10, 80).
bit_rev(11, 208).
bit_rev(12, 48).
bit_rev(13, 176).
bit_rev(14, 112).
bit_rev(15, 240).
bit_rev(16, 8).
bit_rev(17, 136).
bit_rev(18, 72).
bit_rev(19, 200).
bit_rev(20, 40).
bit_rev(21, 168).
bit_rev(22, 104).
bit_rev(23, 232).
bit_rev(24, 24).
bit_rev(25, 152).
bit_rev(26, 88).
bit_rev(27, 216).
bit_rev(28, 56).
bit_rev(29, 184).
bit_rev(30, 120).
bit_rev(31, 248).
bit_rev(32, 4).
bit_rev(33, 132).
bit_rev(34, 68).
bit_rev(35, 196).
bit_rev(36, 36).
bit_rev(37, 164).
bit_rev(38, 100).
bit_rev(39, 228).
bit_rev(40, 20).
bit_rev(41, 148).
bit_rev(42, 84).
bit_rev(43, 212).
bit_rev(44, 52).
bit_rev(45, 180).
bit_rev(46, 116).
bit_rev(47, 244).
bit_rev(48, 12).
bit_rev(49, 140).
bit_rev(50, 76).
bit_rev(51, 204).
bit_rev(52, 44).
bit_rev(53, 172).
bit_rev(54, 108).
bit_rev(55, 236).
bit_rev(56, 28).
bit_rev(57, 156).
bit_rev(58, 92).
bit_rev(59, 220).
bit_rev(60, 60).
bit_rev(61, 188).
bit_rev(62, 124).
bit_rev(63, 252).
bit_rev(64, 2).
bit_rev(65, 130).
bit_rev(66, 66).
bit_rev(67, 194).
bit_rev(68, 34).
bit_rev(69, 162).
bit_rev(70, 98).
bit_rev(71, 226).
bit_rev(72, 18).
bit_rev(73, 146).
bit_rev(74, 82).
bit_rev(75, 210).
bit_rev(76, 50).
bit_rev(77, 178).
bit_rev(78, 114).
bit_rev(79, 242).
bit_rev(80, 10).
bit_rev(81, 138).
bit_rev(82, 74).
bit_rev(83, 202).
bit_rev(84, 42).
bit_rev(85, 170).
bit_rev(86, 106).
bit_rev(87, 234).
bit_rev(88, 26).
bit_rev(89, 154).
bit_rev(90, 90).
bit_rev(91, 218).
bit_rev(92, 58).
bit_rev(93, 186).
bit_rev(94, 122).
bit_rev(95, 250).
bit_rev(96, 6).
bit_rev(97, 134).
bit_rev(98, 70).
bit_rev(99, 198).
bit_rev(100, 38).
bit_rev(101, 166).
bit_rev(102, 102).
bit_rev(103, 230).
bit_rev(104, 22).
bit_rev(105, 150).
bit_rev(106, 86).
bit_rev(107, 214).
bit_rev(108, 54).
bit_rev(109, 182).
bit_rev(110, 118).
bit_rev(111, 246).
bit_rev(112, 14).
bit_rev(113, 142).
bit_rev(114, 78).
bit_rev(115, 206).
bit_rev(116, 46).
bit_rev(117, 174).
bit_rev(118, 110).
bit_rev(119, 238).
bit_rev(120, 30).
bit_rev(121, 158).
bit_rev(122, 94).
bit_rev(123, 222).
bit_rev(124, 62).
bit_rev(125, 190).
bit_rev(126, 126).
bit_rev(127, 254).
bit_rev(128, 1).
bit_rev(129, 129).
bit_rev(130, 65).
bit_rev(131, 193).
bit_rev(132, 33).
bit_rev(133, 161).
bit_rev(134, 97).
bit_rev(135, 225).
bit_rev(136, 17).
bit_rev(137, 145).
bit_rev(138, 81).
bit_rev(139, 209).
bit_rev(140, 49).
bit_rev(141, 177).
bit_rev(142, 113).
bit_rev(143, 241).
bit_rev(144, 9).
bit_rev(145, 137).
bit_rev(146, 73).
bit_rev(147, 201).
bit_rev(148, 41).
bit_rev(149, 169).
bit_rev(150, 105).
bit_rev(151, 233).
bit_rev(152, 25).
bit_rev(153, 153).
bit_rev(154, 89).
bit_rev(155, 217).
bit_rev(156, 57).
bit_rev(157, 185).
bit_rev(158, 121).
bit_rev(159, 249).
bit_rev(160, 5).
bit_rev(161, 133).
bit_rev(162, 69).
bit_rev(163, 197).
bit_rev(164, 37).
bit_rev(165, 165).
bit_rev(166, 101).
bit_rev(167, 229).
bit_rev(168, 21).
bit_rev(169, 149).
bit_rev(170, 85).
bit_rev(171, 213).
bit_rev(172, 53).
bit_rev(173, 181).
bit_rev(174, 117).
bit_rev(175, 245).
bit_rev(176, 13).
bit_rev(177, 141).
bit_rev(178, 77).
bit_rev(179, 205).
bit_rev(180, 45).
bit_rev(181, 173).
bit_rev(182, 109).
bit_rev(183, 237).
bit_rev(184, 29).
bit_rev(185, 157).
bit_rev(186, 93).
bit_rev(187, 221).
bit_rev(188, 61).
bit_rev(189, 189).
bit_rev(190, 125).
bit_rev(191, 253).
bit_rev(192, 3).
bit_rev(193, 131).
bit_rev(194, 67).
bit_rev(195, 195).
bit_rev(196, 35).
bit_rev(197, 163).
bit_rev(198, 99).
bit_rev(199, 227).
bit_rev(200, 19).
bit_rev(201, 147).
bit_rev(202, 83).
bit_rev(203, 211).
bit_rev(204, 51).
bit_rev(205, 179).
bit_rev(206, 115).
bit_rev(207, 243).
bit_rev(208, 11).
bit_rev(209, 139).
bit_rev(210, 75).
bit_rev(211, 203).
bit_rev(212, 43).
bit_rev(213, 171).
bit_rev(214, 107).
bit_rev(215, 235).
bit_rev(216, 27).
bit_rev(217, 155).
bit_rev(218, 91).
bit_rev(219, 219).
bit_rev(220, 59).
bit_rev(221, 187).
bit_rev(222, 123).
bit_rev(223, 251).
bit_rev(224, 7).
bit_rev(225, 135).
bit_rev(226, 71).
bit_rev(227, 199).
bit_rev(228, 39).
bit_rev(229, 167).
bit_rev(230, 103).
bit_rev(231, 231).
bit_rev(232, 23).
bit_rev(233, 151).
bit_rev(234, 87).
bit_rev(235, 215).
bit_rev(236, 55).
bit_rev(237, 183).
bit_rev(238, 119).
bit_rev(239, 247).
bit_rev(240, 15).
bit_rev(241, 143).
bit_rev(242, 79).
bit_rev(243, 207).
bit_rev(244, 47).
bit_rev(245, 175).
bit_rev(246, 111).
bit_rev(247, 239).
bit_rev(248, 31).
bit_rev(249, 159).
bit_rev(250, 95).
bit_rev(251, 223).
bit_rev(252, 63).
bit_rev(253, 191).
bit_rev(254, 127).
bit_rev(255, 255).

bimap__search(bimap(O, C), K, V) :-
	( nonvar(K) ->
		map__search(O, K, V),
		map__search(C, V, K)
	; nonvar(V) ->
		map__search(C, V, K),
		map__search(O, K, V)
	;
		error("bimap__search")
	).

portray(Stream, Term) :-
	currentOutput(S),
	setOutput(Stream),
	( portray(Term) -> true ; print(Term) ),
	setOutput(S).

code_info__current_store_map(Map) -->
        code_info__get_store_map(Maps0),
        { stack__top(Maps0, Map0) },
        !,
        { Map = Map0 }.
code_info__current_store_map(_) -->
        { error("No store map on stack") }.

%-----------------------------------------------------------------------------%
