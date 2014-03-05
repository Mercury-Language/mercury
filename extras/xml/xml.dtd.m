%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%
:- module xml.dtd.

:- interface.

:- import_module list, map.

:- type (A -> B) == map(A, B).

:- type name == string.

:- type token == string.

:- type [] ---> [].

:- type [T1|T2] == list(T1).

:- type dtd
	--->	dtd(
		root		:: name,
		elements	:: (name -> element),
		entities	:: (name -> entityDef),
		pentities	:: (name -> entityDef)
	).

:- type element
	--->	element(
		    eName	:: name,
		    eAttrs	:: (name -> attribute),
		    eContent	:: content
		).

:- type content
	--->	empty
	;	any
	;	children(contentParticle)
	;	mixed(mixed).

:- type contentParticle
	--->	(children - multiplicity).

:- type children
	--->	seq(list(contentParticle))
	;	alt(list(contentParticle))
	;	element(name)
	.

:- type mixed
	--->	mixed(list(name)).

:- type multiplicity
 --->		one
	;	('*')
	;	('+')
	;	('?')
	.

:- type attribute
	--->	attribute(
		    aName	:: name,
		    aType	:: (type),
		    aDefault	:: default
		).

:- type (type)
	--->	cdata
	;	id
	;	idref
	;	idrefs
	;	entity
	;	entities
	;	nmtoken
	;	nmtokens
	;	notation(list(token))
	;	enum(list(token))
	.

:- type default
	--->	required
	;	implied
	;	defaulted(string)
	;	fixed(string)
	.

:- type entityDef
	--->	internal(entity)
	;	external(externalId)
	.

:- type entity == string.

:- type externalId
	--->	system(string)
	;	public(string, string)
	.

