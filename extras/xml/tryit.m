%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%
:- module tryit.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module parsing, xml, xml.cat, xml.encoding.
:- import_module xml.parse, xml.ns.
:- import_module char, list, map, maybe, pair, string.

main -->
    io.command_line_arguments(Args),
    main(Args).

:- pred main(list(string), io, io).
:- mode main(in, di, uo) is det.

main([]) --> [].
main([File|Files]) -->
    see(File, Res0),		
    ( { Res0 = ok } ->
	io.read_file_as_string(TextResult),
	(
	    { TextResult = error(_, TextErr) },
	    stderr_stream(StdErr0),
	    format(StdErr0, "error reading file `%s': %s\n",
	    	[s(File), s(io.error_message(TextErr))])
	;
    	    { TextResult = ok(Text) },
	    pstate(mkEntity(Text), mkEncoding(utf8), init),
	    io((pred(Dirs0::out, di, uo) is det -->
		get_environment_var("XML_DIRS", MStr),
		(
		    { MStr = no },
		    { Str = "." }
		;
		    { MStr = yes(Str) }
		),
		{ split((':'), Str, Dirs0) }
	    ), Dirs),
	    set(gDirs, dirs(Dirs)),
	    io((pred(Cat0::out, di, uo) is det -->
		load("catalog", Dirs, Res1),
		(
		    { Res1 = ok(Cat0) }
		;
		    { Res1 = error(Err0) },
		    stderr_stream(StdErr0),
		    format(StdErr0, "error reading catalog: %s\n", [s(Err0)]),
		    { init(Catalog0) },
		    { Cat0 = catalog(Catalog0) }
		)
	    ), Cat),
	    set(gCatalog, Cat),
	    { map.from_assoc_list([
		"ASCII"		- mkEncoding(ascii7),
		"ascii"		- mkEncoding(ascii7),
		"Latin-1"	- mkEncoding(latin1),
		"Latin1"	- mkEncoding(latin1),
		"UTF-8"		- mkEncoding(utf8),
		"utf-8"		- mkEncoding(utf8)
	    ], Encodings) },
	    set(gEncodings, encodings(Encodings)),
	    document,
	    finish(Res),
	    (
	      	{ Res = ok((DTD, Doc)) },
	    	{ nsTranslate(Doc, NsDoc) }, 
		{ New = cat.ok((DTD, NsDoc)) },
		write(New)
	    	% if don't want to turn the doc to namespace awared, 
		% change the above three lines to  
	  	% write(Res) 
	    ;
	      	{ Res = error(Err) },
 	    	stderr_stream(StdErr),
	    	format(StdErr, "%s: %s\n", [s(File), s(Err)]),
	        write(Res)
	    ),
	    nl,
	    []
        )
    ;
	[]
    ),
    main(Files).

:- pred split(char, string, list(string)).
:- mode split(in, in, out) is det.

split(C, Str0, Strs) :-
    string.to_char_list(Str0, Chars),
    split1(C, [], Strs0, Chars, _),
    reverse(Strs0, Strs).

:- pred split1(char, list(string), list(string), list(char), list(char)).
:- mode split1(in, in, out, in, out) is det.

split1(_C, Strs, Strs, [], []).
split1(C, Strs0, Strs) -->
	=([_|_]),
	split2(C, [], Cs0),
	{ reverse(Cs0, Cs) },
	( { Cs \= [] } ->
	    { string.from_char_list(Cs, Str) },
	    { Strs1 = [Str|Strs0] }
	;
	    { Strs1 = Strs0 }
	),
	split1(C, Strs1, Strs).

:- pred split2(char, list(char), list(char), list(char), list(char)).
:- mode split2(in, in, out, in, out) is det.

split2(_C, Cs, Cs, [], []).
split2(C, Cs0, Cs) -->
    [C0],
    ( { C = C0 } ->
    	{ Cs = Cs0 }
    ;
    	split2(C, [C0|Cs0], Cs)
    ).

