%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2019-2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: source_file_map.m.
% Author: stayl.
%
% Maintain a mapping from module name to source file name.
%
% The reason why this module is in the parse_tree package is that discovering
% what module is stored in a file requires reading the ":- module" declaration
% in that file.
%
%---------------------------------------------------------------------------%

:- module parse_tree.source_file_map.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%
%
% Part 1: file name operations that do not depend on Mercury.modules files.
%

    % Return the default fully qualified source file name.
    %
:- func default_source_file_name(module_name) = file_name.

%---------------------------------------------------------------------------%
%
% Part 2: constructing Mercury.modules files.
%

    % write_source_file_map(ErrorStream, Globals, FileNames, !IO):
    %
    % Given a list of file names, produce the Mercury.modules file.
    %
:- pred write_source_file_map(io.text_output_stream::in,
    globals::in, list(string)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Part 3: testing for the presence of a Mercury.modules file.
%

    % Return `found' if there is a valid Mercury.modules file.
    %
:- pred have_source_file_map(maybe_found::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Part 4: lookups that use the info in a Mercury.modules file, if there is one.
%

    % lookup_module_source_file(ModuleName, MaybeFileName, !IO):
    %
    % Return `yes(FileName)' if FileName is the source file for ModuleName,
    % getting it from Mercury.modules if that file exists *and* has an entry
    % for ModuleName, and otherwise by computing it using the rule for
    % a module's default file name. Return `no' if no source file is available
    % for ModuleName because the default file name for ModuleName is
    % mapped to another module.
    %
:- pred lookup_module_source_file(module_name::in, file_name::out,
    io::di, io::uo) is det.

    % lookup_source_file_module(FileName, MaybeModuleName, !IO):
    %
    % Return `yes(ModuleName)' if FileName is the source file for ModuleName,
    % either through the source file map, or by default. Return `no' if no
    % module name is available for FileName because the default module name
    % for FileName is stored in another file.
    %
:- pred lookup_source_file_maybe_module(file_name::in, maybe(module_name)::out,
    io::di, io::uo) is det.
:- pred lookup_source_file_module(file_name::in, module_name::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.parse_module.   % for peek_at_file
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.write_error_spec.

:- import_module bimap.
:- import_module cord.
:- import_module dir.
:- import_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Part 1.
%

default_source_file_name(ModuleName) = sym_name_to_string(ModuleName) ++ ".m".

    % If the file name ends in ".m", return the module name whose
    % default file name that would be.
    %
:- pred default_module_name_for_file(file_name::in, module_name::out)
    is semidet.

default_module_name_for_file(FileName, DefaultModuleName) :-
    string.remove_suffix(FileName, ".m", FileNameBeforeDotM),
    file_name_to_module_name(FileNameBeforeDotM, DefaultModuleName).

%---------------------------------------------------------------------------%
%
% Part 2.
%

write_source_file_map(ErrorStream, Globals, FileNames, !IO) :-
    list.foldl4(acc_source_file_map_line, FileNames,
        bimap.init, _, cord.init, MapFileLineCord, [], Specs, !IO),
    (
        Specs = [],
        ModulesFileName = modules_file_name,
        io.open_output(ModulesFileName, ModulesFileResult, !IO),
        (
            ModulesFileResult = ok(ModulesFileStream),
            MapFileLines = cord.list(MapFileLineCord),
            io.write_strings(ModulesFileStream, MapFileLines, !IO),
            io.close_output(ModulesFileStream, !IO)
        ;
            ModulesFileResult = error(Error),
            ErrorMsg = io.error_message(Error),
            io.progname_base("mercury_compile", Progname, !IO),
            Pieces = [fixed(Progname), suffix(":"), words("error opening"),
                quote(ModulesFileName), words("for output:"),
                words(ErrorMsg), suffix("."), nl],
            Spec = no_ctxt_spec($pred, severity_error, phase_read_files,
                Pieces),
            write_error_spec(ErrorStream, Globals, Spec, !IO)
        )
    ;
        Specs = [_ | _],
        write_error_specs(ErrorStream, Globals, Specs, !IO)
    ).

:- pred acc_source_file_map_line(file_name::in,
    source_file_map::in, source_file_map::out,
    cord(string)::in, cord(string)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

acc_source_file_map_line(FileName, Mn2FnMap0, Mn2FnMap,
        !MapFileLineCord, !Specs, !IO) :-
    ( if bimap.reverse_search(Mn2FnMap0, _, FileName) then
        % We have already processed FileName.
        % We could report an error here, but there is not much point;
        % ignoring the issue here gets the same result as the the user
        % invoking "mmc -f" again with a file name list from which
        % any duplicates have been deleted.
        Mn2FnMap = Mn2FnMap0
    else
        find_name_of_module_in_file(FileName, MaybeModuleName, !IO),
        (
            MaybeModuleName = ok1(ModuleName),
            ( if bimap.search(Mn2FnMap0, ModuleName, PrevFileName) then
                Pieces = [words("mercury_compile: the files named"),
                    fixed(PrevFileName), words("and"), fixed(FileName),
                    words("both contain the same module,"),
                    qual_sym_name(ModuleName), suffix("."), nl],
                Spec = no_ctxt_spec($pred, severity_error, phase_read_files,
                    Pieces),
                !:Specs = [Spec | !.Specs],
                Mn2FnMap = Mn2FnMap0
            else
                % We have checked that nether ModuleName nor FileName
                % appears in Mn2FnMap0.
                bimap.det_insert(ModuleName, FileName, Mn2FnMap0, Mn2FnMap)
            ),
            ( if string.remove_suffix(FileName, ".m", PartialFileName0) then
                PartialFileName = PartialFileName0
            else
                PartialFileName = FileName
            ),
            file_name_to_module_name(dir.det_basename(PartialFileName),
                DefaultModuleName),
            ( if
                % Only include a module in the mapping if the name
                % does not match the default.
                %
                % XXX This keeps the file size down, but I (zs)
                % am far from sure that this saving is worthwhile.
                dir.dirname(PartialFileName) = dir.this_directory : string,
                ModuleName = DefaultModuleName
            then
                true
            else
                string.format("%s\t%s\n",
                    [s(escaped_sym_name_to_string(ModuleName)), s(FileName)],
                    MapFileLine),
                cord.snoc(MapFileLine, !MapFileLineCord)
            )
        ;
            MaybeModuleName = error1(MnSpecs),
            Mn2FnMap = Mn2FnMap0,
            !:Specs = !.Specs ++ MnSpecs
        )
    ).

    % find_name_of_module_in_file(FileName, MaybeModuleName, !IO):
    %
    % Read the first item from the given file to find the module name.
    %
:- pred find_name_of_module_in_file(file_name::in, maybe1(module_name)::out,
    io::di, io::uo) is det.

find_name_of_module_in_file(FileName, MaybeModuleName, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(FileStream),
        MaybeDefaultModuleName = no,
        peek_at_file(FileStream, FileName, MaybeDefaultModuleName,
            MaybeModuleName, !IO),
        io.close_input(FileStream, !IO)
    ;
        OpenRes = error(Error),
        ErrorMsg = io.error_message(Error),
        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words("error opening"),
            quote(FileName), suffix(":"), words(ErrorMsg), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_read_files, Pieces),
        MaybeModuleName = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Part 3.
%

have_source_file_map(HaveMap, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.is_empty(SourceFileMap) then
        HaveMap = not_found
    else
        HaveMap = found
    ).

%---------------------------------------------------------------------------%
%
% Part 4.
%

lookup_module_source_file(ModuleName, FileName, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.search(SourceFileMap, ModuleName, FileNamePrime) then
        FileName = FileNamePrime
    else
        DefaultFileName = default_source_file_name(ModuleName),
        ( if bimap.reverse_search(SourceFileMap, _, DefaultFileName) then
            io.progname_base("mercury_compile", Progname, !IO),
            Pieces = [fixed(Progname), suffix(":"),
                words("cannot find out which file contains"),
                words("module"), qual_sym_name(ModuleName), suffix(","),
                words("because its name does not appear in Mercury.modules,"),
                words("and the file whose name is the default file name"),
                words("for this module name, i.e."), fixed(FileName),
                suffix(","), words("is recorded in Mercury.options"),
                words("as containing a different module."), nl],
            ErrorLines = error_pieces_to_std_lines(Pieces),
            ErrorStr = error_lines_to_multi_line_string("", ErrorLines),
            io.stderr_stream(StdErr, !IO),
            io.write_string(StdErr, ErrorStr, !IO),
            unexpected($pred, "cannot continue")
        else
            FileName = DefaultFileName
        )
    ).

lookup_source_file_maybe_module(FileName, MaybeModuleName, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.reverse_search(SourceFileMap, ModuleName, FileName) then
        MaybeModuleName = yes(ModuleName)
    else
        ( if default_module_name_for_file(FileName, DefaultModuleName) then
            ( if bimap.search(SourceFileMap, DefaultModuleName, _) then
                MaybeModuleName = no
            else
                MaybeModuleName = yes(DefaultModuleName)
            )
        else
            MaybeModuleName = no
        )
    ).

lookup_source_file_module(FileName, ModuleName, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.reverse_search(SourceFileMap, ModuleNamePrime, FileName) then
        ModuleName = ModuleNamePrime
    else
        ( if default_module_name_for_file(FileName, DefaultModuleName) then
            ( if bimap.search(SourceFileMap, DefaultModuleName, _) then
                io.progname_base("mercury_compile", Progname, !IO),
                Pieces = [fixed(Progname), suffix(":"),
                    words("cannot find out which module is contained in"),
                    words("file"), fixed(FileName), suffix(","),
                    words("because its name does not appear"),
                    words("in Mercury.modules, and the module"),
                    words("whose name is the file name minus the"),
                    quote(".m"), words("suffix is recorded as"),
                    words("being in a different file."), nl],
                ErrorLines = error_pieces_to_std_lines(Pieces),
                ErrorStr = error_lines_to_multi_line_string("", ErrorLines),
                io.stderr_stream(StdErr, !IO),
                io.write_string(StdErr, ErrorStr, !IO),
                io.set_exit_status(1, !IO),
                unexpected($pred, "cannot continue")
            else
                ModuleName = DefaultModuleName
            )
        else
            io.progname_base("mercury_compile", Progname, !IO),
            Pieces = [fixed(Progname), suffix(":"),
                words("cannot find out which module is contained in"),
                words("file"), fixed(FileName), suffix(","),
                words("because its name does not appear in Mercury.modules,"),
                words("and the file name does it end in"),
                quote(".m"), suffix("."), nl],
            % This is the only situation in which default_module_name_for_file
            % fails.
            ErrorLines = error_pieces_to_std_lines(Pieces),
            ErrorStr = error_lines_to_multi_line_string("", ErrorLines),
            io.stderr_stream(StdErr, !IO),
            io.write_string(StdErr, ErrorStr, !IO),
            io.set_exit_status(1, !IO),
            unexpected($pred, "cannot continue")
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Code common to parts 1, 2 and 3.
%

    % Bidirectional map between module names and file names.
    %
    % The only module names in this map will be modules that are
    % the top module in the file that contains them. Any submodules nested
    % within them will NOT appear in this bimap. Without this restruction,
    % this map could not be a bijection.
    %
    % Both the code that constructs source_file_maps for Mercury.modules files,
    % and the code that reads in Mercury.modules files, ensure that this map
    % is a bijection. They do this by treating any possible deviation from
    % being a bijection as an error to be reported.
    %
:- type source_file_map == bimap(module_name, file_name).

%---------------------%

:- mutable(maybe_source_file_map, maybe(source_file_map), no, ground,
    [untrailed, attach_to_io_state]).

%---------------------%

:- func modules_file_name = string.

modules_file_name = "Mercury.modules".

%---------------------%

    % Read the Mercury.modules file (if it exists, and if we have not
    % read and parsed it before) to find and return the mapping
    % from module names to file names, and vice versa.
    %
:- pred get_source_file_map(source_file_map::out, io::di, io::uo) is det.

get_source_file_map(SourceFileMap, !IO) :-
    get_maybe_source_file_map(MaybeSourceFileMap0, !IO),
    (
        MaybeSourceFileMap0 = yes(SourceFileMap0),
        SourceFileMap = SourceFileMap0
    ;
        MaybeSourceFileMap0 = no,
        ModulesFileName = modules_file_name,
        io.read_named_file_as_lines(ModulesFileName, ReadResult, !IO),
        (
            ReadResult = ok(FileLines),
            bimap.init(SourceFileMap0),
            parse_source_file_map(FileLines, ModulesFileName, 1,
                cord.init, ErrorMsgCord, SourceFileMap0, SourceFileMap1),
            ErrorMsgs = cord.list(ErrorMsgCord),
            (
                ErrorMsgs = [],
                SourceFileMap = SourceFileMap1
            ;
                ErrorMsgs = [_ | _],
                % If the file does exist but is malformed, then
                % we *should* print ErrorMsgs, but before 2025 jul 16,
                % we did not do so. Granted, corrupted Mercury.modules files
                % happen rarely, but precisely because of that, if it
                % does happen, users probably won't connect the strange
                % error messages that result from us returning an empty
                % SourceFileMap here to such corruption in the absence of
                % this kind of diagnostic.
                %
                % It would be nice if our callers told us the stream
                % to which this error should be reported, but for many of them,
                % this would require a nontrivial amount of complication
                % that this very rare error probably does not deserve.
                bimap.init(SourceFileMap),
                io.stderr_stream(StdErr, !IO),
                io.write_strings(StdErr, ErrorMsgs, !IO),
                io.write_string(StdErr,
                    "You need to rebuild Mercury.modules.\n", !IO)
            )
        ;
            ReadResult = error(_),
            % If the file doesn't exist, then the mapping is empty.
            % XXX ReadResult can be error/1 even when the file *does* exist.
            % For example, the open could fail due to a permission problem.
            bimap.init(SourceFileMap)
        ),
        % Set the mutable even in the presence of failures. If one attempt
        % at reading Mercury.modules has failed, that is no point in trying
        % again. In the usual case, every later attempt would fail the same
        % way, wasting time, which is bad. However, due to changes by other
        % processes, a later attempt could succeed. This would be worse,
        % because the module name / file name lookups done before and after
        % that later success would have almost certainly reported different
        % results, and those inconsistencies could result in some very weird
        % errors.
        set_maybe_source_file_map(yes(SourceFileMap), !IO)
    ).

:- pred parse_source_file_map(list(string)::in, string::in, int::in,
    cord(string)::in, cord(string)::out,
    source_file_map::in, source_file_map::out) is det.

parse_source_file_map(Lines, ModulesFileName, CurLineNumber, !ErrorMsgCord,
        !SourceFileMap) :-
    (
        Lines = [HeadLine | TailLines],
        ( if string.sub_string_search(HeadLine, "\t", TabIndex) then
            string.length(HeadLine, LineLength),
            string.unsafe_between(HeadLine, 0, TabIndex, ModuleNameStr),
            string.unsafe_between(HeadLine, TabIndex + 1, LineLength,
                FileName),
            ModuleName = string_to_sym_name(ModuleNameStr),
            ( if bimap.insert(ModuleName, FileName, !SourceFileMap) then
                true
            else
                ( if bimap.search(!.SourceFileMap, ModuleName, _) then
                    string.format("line %d of %s duplicates" ++
                        " an existing module name\n",
                        [i(CurLineNumber), s(ModulesFileName)], ErrorMsg)
                else
                    string.format("line %d of %s duplicates" ++
                        " an existing file name\n",
                        [i(CurLineNumber), s(ModulesFileName)], ErrorMsg)
                ),
                cord.snoc(ErrorMsg, !ErrorMsgCord)
            ),
            parse_source_file_map(TailLines, ModulesFileName,
                CurLineNumber + 1, !ErrorMsgCord, !SourceFileMap)
        else
            string.format("line %d of %s is missing a tab character\n",
                [i(CurLineNumber), s(ModulesFileName)], ErrorMsg),
            cord.snoc(ErrorMsg, !ErrorMsgCord)
        )
    ;
        Lines = []
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.source_file_map.
%---------------------------------------------------------------------------%
