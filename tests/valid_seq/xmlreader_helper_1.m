%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Binding to xmlReader by Daniel Veillard.
%

:- module xmlreader_helper_1.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.
:- import_module string.

:- type xmlreader.

:- pred open_file(string::in, maybe(xmlreader)::uo, io::di, io::uo) is det.

:- pred close_reader(xmlreader::di, io::di, io::uo) is det.

:- type evt
    --->    node(
                depth       :: int,
                nodetype    :: int,
                name        :: string,
                is_empty    :: bool,
                value       :: maybe(string)
            )
    ;       error(int)
    ;       eof.

:- pred read(evt::out, xmlreader::di, xmlreader::uo) is det.

:- implementation.

:- pragma foreign_decl(c, "
/*
#include <stdio.h>
#include <libxml/xmlreader.h>
*/
").

% The original code used this, but it breaks compiling this test
% with --intermodule-optimization since xmlTextReaderPtr is undefined.
%:- pragma foreign_type("C", xmlreader, "xmlTextReaderPtr",
%    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", xmlreader, "MR_Word",
    [can_pass_as_mercury_type]).

:- initialise c_init_xml_reader/2.

:- pred c_init_xml_reader(io::di, io::uo) is det.

:- pragma foreign_proc(c,
    c_init_xml_reader(IIO::di, OIO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /*
    ** this initialize the library and check potential ABI mismatches
    ** between the version it was compiled for and the actual shared
    ** library used.
    LIBXML_TEST_VERSION
    */
    LIBXML_TEST_VERSION = 1;

    MR_update_io(IIO, OIO);
").

open_file(FN, MayReader, !IO) :-
    c_open_file(FN, OK, Rdr, !IO),
    (
        OK = yes,
        MayReader = unsafe_promise_unique(yes(Rdr))
    ;
        OK = no,
        MayReader = unsafe_promise_unique(no)
    ).

:- pred c_open_file(string::in, bool::out, xmlreader::out,
    io::di, io::uo) is det.

:- pragma foreign_proc(c,
    c_open_file(FN::in, OK::out, Rdr::out, IIO::di, OIO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /*
    Rdr = xmlReaderForFile(FN, NULL, 0);
    */
    if (Rdr == NULL) {
        OK = 0;
    } else {
        OK = 1;
    }

    MR_update_io(IIO, OIO);
").

:- pragma foreign_proc(c,
    close_reader(Rdr::di, IIO::di, OIO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /*
    xmlFreeTextReader(Rdr);
    */
    MR_update_io(IIO, OIO);
").

read(Evt, !Rdr) :-
    c_read(Ret, !Rdr),
    ( if Ret = 1 then
        c_get(Depth, NodeType, Name, Empty, GotVal, Val, !Rdr),
        ( if GotVal = yes then
            MayVal = yes(Val)
        else
            MayVal = no
        ),
        Evt = node(Depth, NodeType, Name, Empty, MayVal)
    else if Ret = 0 then
        Evt = eof
    else
        Evt = error(Ret)
    ).

:- pred c_read(int::out, xmlreader::di, xmlreader::uo) is det.

:- pragma foreign_proc(c,
    c_read(Ret::out, IRdr::di, ORdr::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /*
    Ret = xmlTextReaderRead(IRdr);
    */
    ORdr = IRdr;
").

:- pred c_get(int::out, int::out, string::out, bool::out,
    bool::out, string::out, xmlreader::di, xmlreader::uo) is det.

:- pragma foreign_proc(c,
    c_get(Depth::out, NodeType::out, Name::out, Empty::out, GotVal::out,
        Val::out, IRdr::di, ORdr::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /*
    Name = xmlTextReaderConstName(IRdr);
    if (Name == NULL)
        Name = BAD_CAST ""--"";
    Val = xmlTextReaderConstValue(IRdr);
    Depth = xmlTextReaderDepth(IRdr);
    NodeType = xmlTextReaderNodeType(IRdr);
    Empty = xmlTextReaderIsEmptyElement(IRdr);
    GotVal = xmlTextReaderHasValue(IRdr);
    ORdr = IRdr;
    */
").
