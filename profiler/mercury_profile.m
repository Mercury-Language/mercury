%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Mercury profiler
% Main author: petdr.
%
% Notes:
%	Process's the two text files "addrpair.out" and "addrdecl.out" to
%	output a gprof similar output.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_profiler.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, string, map.
:- import_module getopt, options, globals.
:- import_module std_util, require.


	% addrdecl = map(label_address, label_name)
:- type addrdecl	==	map(string, string).

%-----------------------------------------------------------------------------%


main -->
	io__command_line_arguments(Args0),
	{ getopt__process_options(Args0, Args, Result0) },
	postprocess_options(Result0, Result),
	main_2(Result, Args).


	% XXX Need's to be expanded so as to process all the options.
:- pred postprocess_options(maybe_option_table, maybe(string),
	io__state, io__state).
:- mode postprocess_options(in, out, di, uo) is det.

postprocess_options(error(ErrorMessage), yes(ErrorMessage)) --> [].
postprocess_options(ok(OptionTable0), no) --> [].


        % Display error message and then usage message
:- pred usage_error(string::in, io__state::di, io__state::uo) is det.
usage_error(ErrorMessage) -->
        io__progname_base("mercury_compile", ProgName),
        io__stderr_stream(StdErr),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, ": "),
        io__write_string(StdErr, ErrorMessage),
        io__write_string(StdErr, "\n"),
        io__set_exit_status(1),
        usage.


        % Display usage message
:- pred usage(io__state::di, io__state::uo) is det.
usage -->
        io__progname_base("mprof", ProgName),
        io__stderr_stream(StdErr),
        io__write_string(StdErr, "Mercury profiler version 0.1\n"),
        io__write_string(StdErr, "Usage: "),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " [<options>]\n"),
        io__write_string(StdErr, "Use `"),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " --help' for more information.\n").

:- pred long_usage(io__state::di, io__state::uo) is det.
long_usage -->
        io__progname_base("mprof", ProgName),
        io__write_string("Mercury profiler version 0.1\n"),
        io__write_string("Usage: "),
        io__write_string(ProgName),
        io__write_string(" [<options>]\n"),
        io__write_string("Options:\n"),
        options_help.


:- pred main_2(maybe(string), list(string), io__state, io__state).
:- mode main_2(in, in, di, uo) is det.

main_2(yes(ErrorMessage), _) -->
	usage_error(ErrorMessage).
main_2(no, Args) -->
	process_addr_decl(AddrDeclMap),
	io__stdout_stream(StdOut),
	io__write_string(StdOut, "Nothing implemented yet\n").


:- pred process_addr_decl(addrdecl, io__state, io__state).
:- mode process_addr_decl(out, di, uo) is det.

process_addr_decl(AddrDeclMap) -->
	io__see("addrdecl.out", Result),
	(
		{ Result = ok }
	->
		{ map__init(AddrDeclMap0) },
		process_addr_decl_2(AddrDeclMap0, AddrDeclMap)
	;
		{ error("get_addr_decl: Couldn't open 'addrdecl.out'\n") }
	).


:- pred process_addr_decl_2(addrdecl, addrdecl, io__state, io__state).
:- mode process_addr_decl_2(in, out, di, uo) is det.

process_addr_decl_2(AddrDecl0, AddrDecl) -->
	read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(LabelAddr) },
		read_label_name(LabelName),
		{ map__det_insert(AddrDecl0, LabelAddr, LabelName, AddrDecl1) },
		process_addr_decl_2(AddrDecl1, AddrDecl)
	;
		{ MaybeLabelAddr = no },
		{ AddrDecl = AddrDecl0 }
	).
		

:- pred read_label_addr(maybe(string), io__state, io__state).
:- mode	read_label_addr(out, di, uo) is det.

read_label_addr(MaybeLabelAddr) -->
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, LabelAddr) },
		{ MaybeLabelAddr = yes(LabelAddr) }
	;
		{ WordResult = eof },
		{ MaybeLabelAddr = no }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("get_addr_decl_2: ", ErrorStr, Str) },
		{ error(Str) }
	).
		

:- pred read_label_name(string, io__state, io__state).
:- mode	read_label_name(out, di, uo) is det.

read_label_name(LabelName) -->
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, LabelName) }
	;
		{ WordResult = eof },
		{ error("read_label_name: EOF reached") }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("get_addr_decl_2: ", ErrorStr, Str) },
		{ error(Str) }
	).
