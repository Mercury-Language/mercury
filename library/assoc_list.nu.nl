%---------------------------------------------------------------------------%
% Copyright (C) 1997 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: assoc_list.nu.nl.
% Main author: bromage.
%
% This file provides an implementation of assoc_list__from_corresponding_lists,
% for which the Mercury % version calls type_of/1 if the lookup fails, but
% type_of/1 is not available from Prolog.
%
%-----------------------------------------------------------------------------%

assoc_list__from_corresponding_lists(Ks, Vs, KVs) :-
	( assoc_list__from_corresponding_2(Ks, Vs, KVs0) ->
		KVs = KVs0
	;
		list__length(Ks, KeyLength),
		string__int_to_string(KeyLength, KeyLengthString),
		list__length(Vs, ValueLength),
		string__int_to_string(ValueLength, ValueLengthString),
		string__append_list(
			["assoc_list__from_corresponding_lists: lists have different lengths.\n",
			"\tKey list length: ",
			KeyLengthString,
			"\n\tValue list length: ",
			ValueLengthString
			],
			ErrorString),
		error(ErrorString)
	).


