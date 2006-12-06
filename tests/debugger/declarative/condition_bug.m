:- module condition_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, string, map, svmap, term_to_xml, assoc_list,
	pair.

main(!IO) :-
	some [!Map] (
		map.init(!:Map),
		svmap.set("mission", "missie", !Map),
		svmap.set("critical", "kritiek", !Map),
		FinalMap = !.Map
	),
	write_xml_doc(translation(FinalMap), !IO),
	io.nl(!IO).

:- type translation ---> translation(map(string, string)).

:- instance xmlable(translation) where [
	func(to_xml/1) is translation_to_xml
].

:- func translation_to_xml(translation::in) = (xml::out(xml_doc)).

translation_to_xml(translation(TranslationMap)) =
	elem("translations", [],
		translation_pairs_to_xml(map.to_assoc_list(TranslationMap))).

:- func translation_pairs_to_xml(assoc_list(string, string)) = list(xml).

translation_pairs_to_xml([]) = [].
translation_pairs_to_xml([English - Dutch | Rest]) = 
	[elem("word", [], [
		elem("english", [], [data(English)]),
	 	elem("dutch", [], [data(Dutch)])])
	| translation_pairs_to_xml(Rest)].

% This test case currently does not work. The symptom is an abort from the
% declarative debugger with a message about not being to perform a retry
% because the value of an input argument is missing. This arises at a call
% to list/foldl/4 with mode di/uo for the accumulator pair. The debugger
% erroneously believes the accumulator at that call to be a string, when in
% fact it is an I/O state.
%
% The problem arises at the call to write_xml_doc/4 in write_xml_doc/3.
% The arity 3 version of this predicate has an I/O state pair as its last two
% arguments, whereas the arity 4 has the signature
%
% :- pred write_xml_doc(Stream::in, T::in, State::di, State::uo)
%   is det <= (xmlable(T), stream.writer(Stream, string, State)).
%
% It seems the debugger is picking the typeinfo for string out of the second
% typeclass_info instead of the typeinfo for State, and thus it shows the type
% of the third argument of write_xml_doc/4 as string, not I/O state.
