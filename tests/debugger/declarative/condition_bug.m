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
