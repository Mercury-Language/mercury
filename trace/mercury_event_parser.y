%{
// vim: ts=4 sw=4 expandtab ft=yacc

// Copyright (C) 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Grammar for event set specifications.

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "mercury_trace_term.h"             // for MR_CTerm and MR_FlatTerm
#include "mercury_memory.h"                 // for MR_NEW

#include "mercury_event_spec.h"             // for MR_EventSpecs etc
#include "mercury_event_spec_missing.h"     // for mercury_event_text etc
// #include "mercury_event_parser.h"
// The content of this header is in bison's .c output anyway;
// including it again chokes GCC in some configurations.
#include "mercury_event_scanner.h"          // for mercury_event_lex etc

MR_EventSet         mercury_event_parsetree;
static  unsigned    mercury_event_next_num = 0;
static  void        mercury_event_error(const char *s);
%}

%start      file

%union
{
    int                 Uline;
    char                *Uid;
    MR_EventSet         Ufile;
    MR_EventSpecs       Uevents;
    MR_EventSpec        Uevent;
    MR_EventAttrs       Uattrs;
    MR_EventAttr        Uattr;
    MR_CTerm            Uterm;
    MR_CArgs            Uargs;
    MR_FlatTerm         Uflatterm;
    MR_FlatArgs         Uflatargs;
    MR_EventAttrType    Utype;
}

%token  <Uline>     TOKEN_EVENT
%token              TOKEN_SET
%token              TOKEN_IMPURE
%token              TOKEN_FUNCTION
%token              TOKEN_SYNTHESIZED
%token              TOKEN_BY

%token              TOKEN_LPAREN
%token              TOKEN_RPAREN
%token  <Uline>     TOKEN_COLON
%token              TOKEN_COMMA

%token  <Uid>       TOKEN_ID
%token  <Uid>       TOKEN_SYM

%token              GARBAGE

%type   <Ufile>     file
%type   <Uevents>   events
%type   <Uevent>    event
%type   <Uattrs>    maybe_attrs
%type   <Uattrs>    attrs
%type   <Uattr>     attr
%type   <Utype>     type
%type   <Uterm>     term
%type   <Uargs>     terms
%type   <Uflatterm> flat_term
%type   <Uflatargs> ids
%type   <Uid>       sym

%%

////////////////////////////////////////////////////////////////////////////

file        :   TOKEN_EVENT TOKEN_SET TOKEN_ID events
                {
                    $$ = MR_NEW(struct MR_EventSet_Struct);
                    $$->MR_event_set_name = $3;
                    $$->MR_event_set_spec_list = $4;
                    // The following fields are filled in later.
                    $$->MR_event_set_specs = NULL;
                    $$->MR_event_set_num_events = 0;
                    mercury_event_parsetree = $$;
                }
            ;

events      :   event events
                {
                    $$ = MR_NEW(struct MR_EventSpecs_Struct);
                    $$->MR_events_head = $1;
                    $$->MR_events_tail = $2;
                }
            |   /* empty */
                {
                    $$ = NULL;
                }
            ;

event       :   TOKEN_EVENT TOKEN_ID TOKEN_LPAREN maybe_attrs TOKEN_RPAREN
                {
                    $$ = MR_NEW(struct MR_EventSpec_Struct);
                    $$->MR_event_num = mercury_event_next_num;
                    $$->MR_event_linenumber = $1;
                    $$->MR_event_name = $2;
                    $$->MR_event_attributes = $4;
                    mercury_event_next_num++;
                }
            ;

maybe_attrs :   attrs
                { $$ = $1; }
            |   /* empty */
                { $$ = NULL; }
            ;

attrs       :   attr TOKEN_COMMA attrs
                {
                    $$ = MR_NEW(struct MR_EventAttrs_Struct);
                    $$->MR_attrs_head = $1;
                    $$->MR_attrs_tail = $3;
                }
            |   attr
                {
                    $$ = MR_NEW(struct MR_EventAttrs_Struct);
                    $$->MR_attrs_head = $1;
                    $$->MR_attrs_tail = NULL;
                }
            ;

attr        :   TOKEN_ID TOKEN_COLON type
                {
                    $$ = MR_NEW(struct MR_EventAttr_Struct);
                    $$->MR_attr_name = $1;
                    $$->MR_attr_linenumber = $2;
                    $$->MR_attr_type = $3;
                }
            ;

type        :   term
                {
                    $$ = MR_NEW(struct MR_EventAttrType_Struct);
                    $$->MR_type_kind = MR_EVENT_ATTR_ORDINARY;
                    $$->MR_type_term = $1;
                    $$->MR_type_synth_call = NULL;
                }
            |   TOKEN_FUNCTION
                {
                    $$ = MR_NEW(struct MR_EventAttrType_Struct);
                    $$->MR_type_kind = MR_EVENT_ATTR_PURE_FUNCTION;
                    $$->MR_type_term = NULL;
                    $$->MR_type_synth_call = NULL;
                }
            |   TOKEN_IMPURE TOKEN_FUNCTION
                {
                    $$ = MR_NEW(struct MR_EventAttrType_Struct);
                    $$->MR_type_kind = MR_EVENT_ATTR_IMPURE_FUNCTION;
                    $$->MR_type_term = NULL;
                    $$->MR_type_synth_call = NULL;
                }
            |   term TOKEN_SYNTHESIZED TOKEN_BY flat_term
                {
                    $$ = MR_NEW(struct MR_EventAttrType_Struct);
                    $$->MR_type_kind = MR_EVENT_ATTR_SYNTHESIZED;
                    $$->MR_type_term = $1;
                    $$->MR_type_synth_call = $4;
                }
            ;

term        :   sym
                {
                    $$ = MR_NEW(struct MR_CTerm_Struct);
                    $$->MR_term_functor = $1;
                    $$->MR_term_args = NULL;
                }
            |   sym TOKEN_LPAREN terms TOKEN_RPAREN
                {
                    $$ = MR_NEW(struct MR_CTerm_Struct);
                    $$->MR_term_functor = $1;
                    $$->MR_term_args = $3;
                }
            ;

terms       :   term TOKEN_COMMA terms
                {
                    $$ = MR_NEW(struct MR_CArgs_Struct);
                    $$->MR_args_head = $1;
                    $$->MR_args_tail = $3;
                }
            |   term
                {
                    $$ = MR_NEW(struct MR_CArgs_Struct);
                    $$->MR_args_head = $1;
                    $$->MR_args_tail = NULL;
                }
            ;

flat_term   :   TOKEN_ID
                {
                    $$ = MR_NEW(struct MR_FlatTerm_Struct);
                    $$->MR_flat_term_functor = $1;
                    $$->MR_flat_term_args = NULL;
                }
            |   TOKEN_ID TOKEN_LPAREN ids TOKEN_RPAREN
                {
                    $$ = MR_NEW(struct MR_FlatTerm_Struct);
                    $$->MR_flat_term_functor = $1;
                    $$->MR_flat_term_args = $3;
                }
            ;

ids         :   TOKEN_ID TOKEN_COMMA ids
                {
                    $$ = MR_NEW(struct MR_FlatArgs_Struct);
                    $$->MR_flat_args_head = $1;
                    $$->MR_flat_args_tail = $3;
                }
            |   TOKEN_ID
                {
                    $$ = MR_NEW(struct MR_FlatArgs_Struct);
                    $$->MR_flat_args_head = $1;
                    $$->MR_flat_args_tail = NULL;
                }
            ;

sym         :   TOKEN_ID
                {
                    $$ = $1;
                }
            |   TOKEN_SYM
                {
                    $$ = $1;
                }
            ;

%%

static void
mercury_event_error(const char *s)
{
    char        buf[8192];

    if (mercury_event_char <= 0) {
        sprintf(buf, "premature EOF");
        mercury_event_linenum--;
    } else if (mercury_event_text[0] == '\n' || mercury_event_text[0] == '\f') {
        sprintf(buf, "%s at end of line", s);
    } else if (isprint((int)mercury_event_text[0])) {
        sprintf(buf, "%s at symbol `%s'", s, mercury_event_text);
    } else {
        sprintf(buf, "%s at \\%o", s, mercury_event_text[0]);
    }

    printf("%s:%d: %s\n", mercury_event_filename, mercury_event_linenum, buf);
}
