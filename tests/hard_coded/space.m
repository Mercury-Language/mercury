% A regression test: the Mercury compiler dated May 20 1997 
% generated incorrect code for this program.

%------------------------------------------------------------------------------%
%
% space : an ambient music generator
% usage: space	[-h|--help] \
%		[-k|--key {a-g}[#][M]]	(default "c")
%		[-l|--length n]		(default 16)
%		[-s|--seed n]		(default 0)
%
%------------------------------------------------------------------------------%

:- module space.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module char, string, int, list, solutions, require.
:- import_module getopt, bool.

:- type option
	--->	help
	;	key
	;	length
	;	seed
	.

:- type note	--->	note(rank, modifier, octave).

:- type rank
	--->	c
	;	d
	;	e
	;	f
	;	g
	;	a
	;	b
	.

:- type modifier
	--->	natural
	;	sharp
	;	flat
	.

:- type octave == int.

:- type interval
	--->	i
	;	ii
	;	iii
	;	iv
	;	v
	;	vi
	;	vii
	.

:- type qualifier
	--->	maj
	;	min
	.

:- type chord	--->	chord(interval, kind, inversion).

:- type kind
	--->	maj
	;	min
	;	open
	;	seven
	;	maj_seven
	;	ninth
	;	eleventh
	;	dim
	.

:- type inversion
	--->	o
	;	up(degree)
	;	down(degree)
	.

:- type degree
	--->	i
	;	ii
	;	iii
	.

%------------------------------------------------------------------------------%

main -->
	% io__command_line_arguments(Args0),
	{ Args0 = ["-k", "dM", "-l", "30", "-s", "1"] },

	{ getopt__process_options(
		option_ops(short, long, defaults), Args0, _Args, MOptTable) },
	(
		{ MOptTable = error(String) },
		{ string__format("bad option: %s\n", [s(String)], Msg) },
		io__stderr_stream(Stderr),
		io__write_string(Stderr, Msg)
	;
		{ MOptTable = ok(Opts) },
		(
			{ getopt__lookup_bool_option(Opts, help, yes) }
		->
			help
		;
			{ getopt__lookup_string_option(Opts, key, KeyStr) },
			{ figure_key(KeyStr, Note, Qual, Kind) }
		->
			{ getopt__lookup_int_option(Opts, length, Len) },
			{ getopt__lookup_int_option(Opts, seed, Seed) },
			{ random_init(Seed, Rnd) },
			{ Chord0 = chord(i, Kind, up(ii)) },
			doit(Len, Note, Qual, Chord0, Rnd)
		;
			{ getopt__lookup_string_option(Opts, key, KeyStr) },
			{ string__format("illegal key: `%s'\n",
				[s(KeyStr)], Msg) },
			io__stderr_stream(Stderr),
			io__write_string(Stderr, Msg)
		)
	).

:- pred figure_key(string, note, qualifier, kind).
:- mode figure_key(in, out, out, out) is semidet.

figure_key("c", note(c, natural, 2), maj, maj).
figure_key("cM", note(c, natural, 2), min, min).
figure_key("c#", note(c, sharp, 2), maj, maj).
figure_key("c#M", note(c, sharp, 2), min, min).
figure_key("d", note(d, natural, 2), maj, maj).
figure_key("dM", note(d, natural, 2), min, min).
figure_key("d#", note(d, sharp, 2), maj, maj).
figure_key("d#M", note(d, sharp, 2), min, min).
figure_key("e", note(e, natural, 2), maj, maj).
figure_key("eM", note(e, natural, 2), min, min).
figure_key("f", note(f, natural, 2), maj, maj).
figure_key("fM", note(f, natural, 2), min, min).
figure_key("f#", note(f, sharp, 2), maj, maj).
figure_key("f#M", note(f, sharp, 2), min, min).
figure_key("g", note(g, natural, 2), maj, maj).
figure_key("gM", note(g, natural, 2), min, min).
figure_key("g#", note(g, sharp, 2), maj, maj).
figure_key("g#M", note(g, sharp, 2), min, min).
figure_key("a", note(a, natural, 2), maj, maj).
figure_key("aM", note(a, natural, 2), min, min).
figure_key("a#", note(a, sharp, 2), maj, maj).
figure_key("a#M", note(a, sharp, 2), min, min).
figure_key("b", note(b, natural, 2), maj, maj).
figure_key("bM", note(b, natural, 2), min, min).

:- pred doit(int, note, qualifier, chord, random_supply, io__state, io__state).
:- mode doit(in, in, in, in, mdi, di, uo) is det.

doit(N, Trans, Qual, Chord0, Rnd0) -->
	(
		{ N =< 0 }
	->
		[]
	;
		{ chord_notes(Chord0, Qual, Notes0) },
		{ list__map(trans(Trans), Notes0, Notes) },
		write_chord(Notes),
		(
			{ random_random(I, Rnd0, Rnd) },
			{ next_chord(Chord0, Qual, I, Chord1) }
		->
			{ N1 = N - 1 },
			doit(N1, Trans, Qual, Chord1, Rnd)
		;
			io__write_string("next_chord failed\n")
		)
	).

:- pred write_chord(list(note), io__state, io__state).
:- mode write_chord(in, di, uo) is det.

write_chord([]) --> io__nl.
write_chord([Note]) -->
	{ Note = note(Rank, Mod, Oct) },
	write_note(Rank, Mod, Oct),
	io__nl.
write_chord([Note|Notes]) -->
	{ Notes = [_|_] },
	{ Note = note(Rank, Mod, Oct) },
	write_note(Rank, Mod, Oct),
	io__write_string(" "),
	write_chord(Notes).

:- pred write_note(rank, modifier, octave, io__state, io__state).
:- mode write_note(in, in, in, di, uo) is det.

write_note(c, flat, Oct) -->
	{ Oct1 = Oct - 1 },
	{ string__format("b%d", [i(Oct1)], Str) },
	io__write_string(Str).
write_note(c, natural, Oct) -->
	{ string__format("c%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(c, sharp, Oct) -->
	{ string__format("c#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(d, flat, Oct) -->
	{ string__format("c#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(d, natural, Oct) -->
	{ string__format("d%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(d, sharp, Oct) -->
	{ string__format("d#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(e, flat, Oct) -->
	{ string__format("d#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(e, natural, Oct) -->
	{ string__format("e%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(e, sharp, Oct) -->
	{ string__format("f%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(f, flat, Oct) -->
	{ string__format("e%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(f, natural, Oct) -->
	{ string__format("f%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(f, sharp, Oct) -->
	{ string__format("f#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(g, flat, Oct) -->
	{ string__format("f#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(g, natural, Oct) -->
	{ string__format("g%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(g, sharp, Oct) -->
	{ string__format("g#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(a, flat, Oct) -->
	{ string__format("g#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(a, natural, Oct) -->
	{ string__format("a%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(a, sharp, Oct) -->
	{ string__format("a#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(b, flat, Oct) -->
	{ string__format("a#%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(b, natural, Oct) -->
	{ string__format("b%d", [i(Oct)], Str) },
	io__write_string(Str).
write_note(b, sharp, Oct) -->
	{ Oct1 = Oct + 1 },
	{ string__format("c%d", [i(Oct1)], Str) },
	io__write_string(Str).

%------------------------------------------------------------------------------%

:- pred next_chord(chord, qualifier, int, chord).
:- mode next_chord(in, in, in, out) is semidet.

next_chord(Chord0, Qual, Pr, Chord) :-
	chord_notes(Chord0, Qual, Notes0),
	last(Notes0, TopNote0, _),
	Chord0 = chord(Int0, _, _),
/*
	Lambda = lambda([Ch::out] is nondet, (
		next_interval(Int0, Qual, Int, Kind),
		next_inversion(Inv),
		Ch = chord(Int, Kind, Inv),
		chord_notes(Ch, Qual, Notes),
		last(Notes, TopNote, _),
		next_topnote(TopNote0, Qual, TopNote)
	)),
	solutions(Lambda, List),
*/
	solutions(try_next_chord(Qual, Int0, TopNote0), List),
	list__length(List, Len),
	Len > 0,
	Ind = Pr mod Len,
	list__index0(List, Ind, Chord).

:- pred try_next_chord(qualifier::in, interval::in, note::in, chord::out)
	is nondet.
try_next_chord(Qual, Int0, TopNote0, Ch) :-
	next_interval(Int0, Qual, Int, Kind),
	next_inversion(Inv),
	Ch = chord(Int, Kind, Inv),
	chord_notes(Ch, Qual, Notes),
	last(Notes, TopNote, _),
	next_topnote(TopNote0, Qual, TopNote).

:- pred rotate(int, list(T), list(T)).
:- mode rotate(in, in, out) is det.

rotate(_, [], []).
rotate(I, [X|Xs], Zs) :-
	(
		I > 0
	->
		list__append(Xs, [X], Ys),
		I1 = I - 1,
		rotate(I1, Ys, Zs)
	;
		I < 0
	->
		list__append(Xs, [X], Ys),
		I1 = I + 1,
		rotate(I1, Ys, Zs)
	;
		Zs = [X|Xs]
	).

:- pred next_interval(interval, qualifier, interval, kind).
:- mode next_interval(in, in, out, out) is nondet.

next_interval(i, maj, iv, maj).
next_interval(i, maj, v, maj).
next_interval(i, maj, vi, min).

next_interval(ii, maj, iv, maj).
next_interval(ii, maj, v, maj).
next_interval(ii, maj, i, maj).

next_interval(iii, maj, i, maj).
next_interval(iii, maj, v, maj).
next_interval(iii, maj, iv, maj).

next_interval(iv, maj, i, maj).
next_interval(iv, maj, ii, min).
next_interval(iv, maj, v, maj).

next_interval(v, maj, i, maj).
next_interval(v, maj, iv, maj).

next_interval(vi, maj, v, maj).
next_interval(vi, maj, i, maj).

next_interval(i, min, iii, maj).
next_interval(i, min, iv, min).
next_interval(i, min, v, min).
next_interval(i, min, vi, maj).
next_interval(i, min, vii, maj).

next_interval(iii, min, i, min).
next_interval(iii, min, iv, min).
next_interval(iii, min, v, min).
next_interval(iii, min, vi, maj).
next_interval(iii, min, vii, maj).

next_interval(iv, min, i, min).
next_interval(iv, min, iii, maj).
next_interval(iv, min, v, min).
next_interval(iv, min, vi, maj).
next_interval(iv, min, vii, maj).

next_interval(v, min, i, min).
next_interval(v, min, iii, min).
next_interval(v, min, iv, min).
next_interval(v, min, vi, maj).
next_interval(v, min, vii, maj).

next_interval(vi, min, i, min).
next_interval(vi, min, iii, maj).
next_interval(vi, min, iv, min).
next_interval(vi, min, v, min).
next_interval(vi, min, vii, maj).

next_interval(vii, min, i, min).
next_interval(vii, min, iii, maj).
next_interval(vii, min, iv, min).
next_interval(vii, min, v, min).
next_interval(vii, min, vi, maj).

:- pred next_inversion(inversion::out) is multi.

next_inversion(o).
next_inversion(up(i)).
next_inversion(up(ii)).
next_inversion(up(iii)).
next_inversion(down(i)).
next_inversion(down(ii)).
next_inversion(down(iii)).

:- pred next_topnote(note, qualifier, note).
:- mode next_topnote(in, in, out) is nondet.

next_topnote(Note0, Qual, Note) :-
	note_to_interval(Note0, Qual, Int0, Oct0),
	adj_interval(Int0, Oct0, Int, Oct),
	note_to_interval(Note, Qual, Int, Oct).

:- pred note_to_interval(note, qualifier, interval, octave).
:- mode note_to_interval(in, in, out, out) is semidet.
:- mode note_to_interval(out, in, in, in) is multi.

note_to_interval(note(c, natural, Oct), _, i, Oct).
note_to_interval(note(d, natural, Oct), _, ii, Oct).
note_to_interval(note(d, sharp, Oct), min, iii, Oct).
note_to_interval(note(e, flat, Oct), min, iii, Oct).
note_to_interval(note(e, natural, Oct), maj, iii, Oct).
note_to_interval(note(f, natural, Oct), _, iv, Oct).
note_to_interval(note(g, natural, Oct), _, v, Oct).
note_to_interval(note(g, sharp, Oct), min, vi, Oct).
note_to_interval(note(a, flat, Oct), min, vi, Oct).
note_to_interval(note(a, natural, Oct), maj, vi, Oct).
note_to_interval(note(a, sharp, Oct), min, vii, Oct).
note_to_interval(note(b, flat, Oct), min, vii, Oct).
note_to_interval(note(b, natural, Oct), maj, vii, Oct).

:- pred adj_interval(interval, octave, interval, octave).
:- mode adj_interval(in, in, out, out) is multi.

adj_interval(i, Oct, vii, Oct1) :-
	Oct1 = Oct - 1.
adj_interval(i, Oct, ii, Oct).
adj_interval(ii, Oct, i, Oct).
adj_interval(ii, Oct, iii, Oct).
adj_interval(iii, Oct, ii, Oct).
adj_interval(iii, Oct, iv, Oct).
adj_interval(iv, Oct, iii, Oct).
adj_interval(iv, Oct, v, Oct).
adj_interval(v, Oct, iv, Oct).
adj_interval(v, Oct, vi, Oct).
adj_interval(vi, Oct, v, Oct).
adj_interval(vi, Oct, vii, Oct).
adj_interval(vii, Oct, vi, Oct).
adj_interval(vii, Oct, i, Oct1) :-
	Oct1 = Oct + 1.

%------------------------------------------------------------------------------%

:- pred chord_notes(chord, qualifier, list(note)).
:- mode chord_notes(in, in, out) is det.

chord_notes(chord(Interval, Kind, Inversion), Qual, Notes) :-
	base_notes(Kind, Notes0),
	list__map(transpose(Interval, Qual), Notes0, Notes1),
	invert(Notes1, Inversion, Notes).

%------------------------------------------------------------------------------%

:- pred base_notes(kind, list(note)).
:- mode base_notes(in, out) is det.

base_notes(maj, Notes) :-
	Notes = [ note(c, natural, 0),
		note(e, natural, 0),
		note(g, natural, 0)
	].

base_notes(min, Notes) :-
	Notes = [
		note(c, natural, 0),
		note(e, flat, 0),
		note(g, natural, 0)
	].

base_notes(open, Notes) :-
	Notes = [
		note(c, natural, 0),
		note(g, natural, 0),
		note(c, natural, 1)
	].

base_notes(seven, Notes) :-
	Notes = [
		note(c, natural, 0),
		note(e, natural, 0),
		note(g, natural, 0),
		note(b, flat, 0)
	].

base_notes(maj_seven, Notes) :-
	Notes = [
		note(c, natural, 0),
		note(e, natural, 0),
		note(g, natural, 0),
		note(b, natural, 0)
	].

base_notes(ninth, Notes) :-
	Notes = [
		note(c, natural, 0),
		note(d, natural, 0),
		note(e, natural, 0),
		note(g, natural, 0)
	].

base_notes(eleventh, Notes) :-
	Notes = [
		note(c, natural, 0),
		note(e, natural, 0),
		note(g, natural, 0),
		note(a, natural, 0)
	].

base_notes(dim, Notes) :-
	Notes = [
		note(c, natural, 0),
		note(e, flat, 0),
		note(g, flat, 0),
		note(a, natural, 0)
	].

%------------------------------------------------------------------------------%

:- pred transpose(interval, qualifier, note, note).
:- mode transpose(in, in, in, out) is det.

transpose(Trans, Qual, Note0, Note) :-
	interval_to_int(Trans, Qual, TNum),
	note_to_int(Note0, NNum0),
	NNum = TNum + NNum0,
	int_to_note(NNum, Note).

:- pred trans(note, note, note).
:- mode trans(in, in, out) is det.

trans(Trans, Note0, Note) :-
	note_to_int(Trans, TNum),
	note_to_int(Note0, NNum0),
	NNum = TNum + NNum0,
	int_to_note(NNum, Note).

%------------------------------------------------------------------------------%

:- type direction ---> up ; down .

:- pred invert(list(note), inversion, list(note)).
:- mode invert(in, in, out) is det.

invert(Notes, o, Notes).
invert(Notes0, up(Degree), Notes) :-
	degree_to_int(Degree, N),
	invert_list(Notes0, up, N, Notes).
invert(Notes0, down(Degree), Notes) :-
	degree_to_int(Degree, N),
	invert_list(Notes0, down, -N, Notes).

:- pred invert_list(list(note), direction, int, list(note)).
:- mode invert_list(in, in, in, out) is det.

invert_list(Notes0, Dir, N, Notes) :-
	(
		N > 0,
		Notes0 = [Note0|Notes1]
	->
		shift(Dir, Note0, Note),
		list__append(Notes1, [Note], Notes2),
		N1 = N - 1,
		invert_list(Notes2, Dir, N1, Notes)
	;
		N < 0,
		last(Notes0, Note0, Notes1)
	->
		shift(Dir, Note0, Note),
		N1 = N + 1,
		invert_list([Note|Notes1], Dir, N1, Notes)
	;
		Notes = Notes0
	).

:- pred shift(direction, note, note).
:- mode shift(in, in, out) is det.

shift(up, note(Rank, Mod, Oct), note(Rank, Mod, Oct1)) :-
	Oct1 = Oct + 1.
shift(down, note(Rank, Mod, Oct), note(Rank, Mod, Oct1)) :-
	Oct1 = Oct - 1.

:- pred last(list(T), T, list(T)).
:- mode last(in, out, out) is semidet.

last([X], X, []).
last([X|Xs], Z, [X|Ys]) :-
	Xs = [_|_],
	last(Xs, Z, Ys).

%------------------------------------------------------------------------------%

:- pred inversion_to_int(inversion, int).
:- mode inversion_to_int(in, out) is det.

inversion_to_int(o, 0).
inversion_to_int(up(Deg), Int) :- degree_to_int(Deg, Int).
inversion_to_int(down(Deg), -Int) :- degree_to_int(Deg, Int).

:- pred degree_to_int(degree, int).
:- mode degree_to_int(in, out) is det.

degree_to_int(i,	1).
degree_to_int(ii,	2).
degree_to_int(iii,	3).

:- pred interval_to_int(interval, qualifier, int).
:- mode interval_to_int(in, in, out) is det.

interval_to_int(i, _, 0).
interval_to_int(ii, _, 2).
interval_to_int(iii, min, 3).
interval_to_int(iii, maj, 4).
interval_to_int(iv, _, 5).
interval_to_int(v, _, 7).
interval_to_int(vi, min, 8).
interval_to_int(vi, maj, 9).
interval_to_int(vii, min, 10).
interval_to_int(vii, maj, 11).

:- pred note_to_int(note, int).
:- mode note_to_int(in, out) is det.

note_to_int(note(c, flat, Oct),		I) :-
	I = -1 + 12 * Oct.
note_to_int(note(c, natural, Oct),	I) :-
	I = 0 + 12 * Oct.
note_to_int(note(c, sharp, Oct),	I) :-
	I = 1 + 12 * Oct.
note_to_int(note(d, flat, Oct),		I) :-
	I = 1 + 12 * Oct.
note_to_int(note(d, natural, Oct),	I) :-
	I = 2 + 12 * Oct.
note_to_int(note(d, sharp, Oct),	I) :-
	I = 3 + 12 * Oct.
note_to_int(note(e, flat, Oct),		I) :-
	I = 3 + 12 * Oct.
note_to_int(note(e, natural, Oct),	I) :-
	I = 4 + 12 * Oct.
note_to_int(note(f, flat, Oct),		I) :-
	I = 4 + 12 * Oct.
note_to_int(note(e, sharp, Oct),	I) :-
	I = 5 + 12 * Oct.
note_to_int(note(f, natural, Oct),	I) :-
	I = 5 + 12 * Oct.
note_to_int(note(f, sharp, Oct),	I) :-
	I = 6 + 12 * Oct.
note_to_int(note(g, flat, Oct),		I) :-
	I = 6 + 12 * Oct.
note_to_int(note(g, natural, Oct),	I) :-
	I = 7 + 12 * Oct.
note_to_int(note(g, sharp, Oct),	I) :-
	I = 8 + 12 * Oct.
note_to_int(note(a, flat, Oct),		I) :-
	I = 8 + 12 * Oct.
note_to_int(note(a, natural, Oct),	I) :-
	I = 9 + 12 * Oct.
note_to_int(note(a, sharp, Oct),	I) :-
	I = 10 + 12 * Oct.
note_to_int(note(b, flat, Oct),		I) :-
	I = 10 + 12 * Oct.
note_to_int(note(b, natural, Oct),	I) :-
	I = 11 + 12 * Oct.
note_to_int(note(b, sharp, Oct),	I) :-
	I = 12 + 12 * Oct.

:- pred int_to_note(int, note).
:- mode int_to_note(in, out) is det.

int_to_note(Num, note(Rank, Mod, Oct)) :-
	Oct = Num // 12,
	Off = Num mod 12,
	(
		(
			Off = 0, Rank0 = c, Mod0 = natural
		;
			Off = 1, Rank0 = c, Mod0 = sharp
		;
			Off = 2, Rank0 = d, Mod0 = natural
		;
			Off = 3, Rank0 = d, Mod0 = sharp
		;
			Off = 4, Rank0 = e, Mod0 = natural
		;
			Off = 5, Rank0 = f, Mod0 = natural
		;
			Off = 6, Rank0 = f, Mod0 = sharp
		;
			Off = 7, Rank0 = g, Mod0 = natural
		;
			Off = 8, Rank0 = g, Mod0 = sharp
		;
			Off = 9, Rank0 = a, Mod0 = natural
		;
			Off = 10, Rank0 = a, Mod0 = sharp
		;
			Off = 11, Rank0 = b, Mod0 = natural
		)
	->
		Rank = Rank0,
		Mod = Mod0
	;
		error("Num mod 12 gave an illegal result!")
	).

%------------------------------------------------------------------------------%

:- pred short(char, option).
:- mode short(in, out) is semidet.

short('h', help).
short('k', key).
short('l', length).
short('s', seed).

:- pred long(string, option).
:- mode long(in, out) is semidet.

long("help", help).
long("key", key).
long("length", length).
long("seed", seed).

:- pred defaults(option, option_data).
:- mode defaults(out, out) is nondet.

defaults(Opt, Data) :-
	(
		semidet_succeed
	->
		defaults0(Opt, Data)
	;
		fail
	).

:- pred defaults0(option, option_data).
:- mode defaults0(out, out) is multi.

defaults0(help, bool(no)).
defaults0(key, string("c")).
defaults0(length, int(16)).
defaults0(seed, int(0)).

:- pred help(io__state::di, io__state::uo) is det.

help -->
	io__stderr_stream(StdErr),
	io__write_string(StdErr,
"usage: space [-h|--help] [-k|--key key[M]] [-l|--length n] [-s|--seed n]\n"
).

%------------------------------------------------------------------------------%

% This is a very bad number generator. Its only virtue is that
% unlike random.m, its behavior does not depend on word size.

:- type random_supply == int.

:- pred random_init(int::in, random_supply::muo) is det.

random_init(Seed, Supply) :-
	copy(Seed, Supply).

:- pred random_random(int::out, random_supply::mdi, random_supply::muo) is det.

random_random(Value, Supply0, Supply) :-
	Value = Supply0,
	( Supply0 < 100 ->
		Supply = Supply0 + 1
	;
		Supply = 0
	).

%------------------------------------------------------------------------------%
