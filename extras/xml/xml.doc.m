%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2011 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%
:- module xml.doc.

:- interface.

:- import_module array, list, map.

:- type document
	--->	doc(
		    prestuff	:: list(ref(content)),
		    root	:: ref(content),
		    poststuff	:: list(ref(content)),
		    content	:: array(content)
		).

:- type content
	--->	element(element)
	;	pi(string, string)
	;	comment(string)
	;	data(string)
	.

:- type contentStore
	--->	content(
			eNext	:: ref(content),
			eMap	:: map(ref(content), content)
		).

:- type element
	--->	element(
		    eName	:: string,
		    eAttrs	:: list(attribute),
		    eContent	:: list(ref(content))
		).

:- type attribute
	--->	attribute(
		    aName	:: string,
		    aValue	:: string
		).

:- type ref(T)	== int.

:- func ref(contentStore, ref(content)) = content.

:- pred add(content, ref(content), contentStore, contentStore).
:- mode add(in, out, in, out) is det.

:- implementation.

:- import_module int.

ref(Elems, Ref) = Elem :-
	lookup(Elems^eMap, Ref, Elem).

add(Elem, Ref, Elems0, Elems) :-
	Ref = Elems0^eNext,
	Elems1 = Elems0^eNext := Ref + 1,
	map.set(Ref, Elem, Elems1^eMap, Map),
	Elems = Elems1^eMap := Map.

