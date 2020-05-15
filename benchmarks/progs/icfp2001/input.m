%------------------------------------------------------------------------------%
% input.m
% Ralph Becket <rbeck@microsoft.com>
% Thu Jul 26 19:31:15 BST 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
% Reads in the source SML/NG from stdin, checks it for validity
% (no overlapping/unclosed tags), suppresses extraneous spaces
% in the input, and generates an unoptimized span representation
% of the tags.
%
%------------------------------------------------------------------------------%

:- module input.

:- interface.

:- import_module io, list, array, char.
:- import_module attrs.



:- type tag
    --->    b ; em ; i ; pl ; s ; tt ; u(int) ; size(int) ; colour(colour).

:- type tags      == list(tag).

:- type attrss    == array(attrs).

:- type extents   == array(int).

:- type token
    --->    open(tag)
    ;       close(tag)
    ;       spc(char)
    ;       text(string).

:- type tokens    == list(token).



    % read_smlng(FirstFile, DoneFile, Str, Attrss, Extents)
    %
    % Read the source SML/NG from stdin, check it for validity and
    % purge it of extraneous spaces.  This version is output to
    % FirstFile to ensure that we have something to offer that is
    % at least as small as the input.  Once FirstFile has been closed,
    % DoneFile is created to indicate the fact.
    %
    % The plaintext of the input is returned in Str
    % and corresponding members of Attrss and Extents give the
    % extents of successive attribute records over the plaintext.
    %
:- pred read_smlng(string,string,string,attrss,extents,io__state,io__state).
:- mode read_smlng(in, in, out, out, out, di, uo) is det.

    % file_names(Base, N, SMLNG, Done).
    %
    % Construct the output and done filenames.  The format is
    % Base-N.smlng for SMLNG and
    % Base-N.done  for Done.
    %
:- pred file_names(string::in, int::in, string::out, string::out) is det.

    % Write out a sequence of tokens.
    %
:- pred write_tokens(tokens, io__state, io__state).
:- mode write_tokens(in, di, uo) is det.

    % Effect the change of opening a particular tag on an attribute record.
    %
:- func apply_tag(tag, attrs) = attrs.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int, exception, string, bool.
:- import_module maybe.
:- import_module pair.

%------------------------------------------------------------------------------%

:- type st
    --->    reading_open_tag(list(char))
    ;       reading_close_tag(list(char))
    ;       reading_text(list(char)).

    % Read the source SML/NG and turn it into a list of tokens.
    %
:- pred tokenize_input(tokens, io__state, io__state).
:- mode tokenize_input(out, di, uo) is det.

tokenize_input(list__reverse(Toks)) -->
    parse_chars(reading_text([]), [], [], Toks).

%------------------------------------------------------------------------------%

:- pred parse_chars(st, tags, tokens, tokens, io__state, io__state).
:- mode parse_chars(in, in, in, out, di, uo) is det.

parse_chars(ST, OpenTags, Toks0, Toks) -->
    io__read_char(Result),
    (   { Result = error(_) }, { throw(Result) }
    ;   { Result = eof },      { Toks = finish_tokenizing(ST, OpenTags, Toks0) }
    ;   { Result = ok(Char) }, parse_char(Char, ST, OpenTags, Toks0, Toks)
    ).



    % Memo to self: this approach sucks...  More beer.
    % Next time use a table.
    %
:- pred parse_char(char, st, tags, tokens, tokens, io__state, io__state).
:- mode parse_char(in, in, in, in, out, di, uo) is det.

parse_char(C, reading_open_tag(Cs), OpenTags, Toks0, Toks) -->
    ( if { C = (/), Cs = [] } then
        parse_chars(reading_close_tag(Cs), OpenTags, Toks0, Toks)
      else if { C = (>) } then
        { Tag = string_to_tag(string__from_rev_char_list(Cs)) },
        { Tok = open(Tag) },
        parse_chars(reading_text([]), [Tag | OpenTags], [Tok | Toks0], Toks)
      else if { C = (<) } then
        { throw("`<' in open tag name") }
      else
        parse_chars(reading_open_tag([C | Cs]), OpenTags, Toks0, Toks)
    ).

parse_char(C, reading_close_tag(Cs), OpenTags0, Toks0, Toks) -->
    ( if { C = (>) } then
        { Tag = string_to_tag(string__from_rev_char_list(Cs)) },
        { Tok = close(Tag) },
        ( if { OpenTags0 = [Tag | OpenTags] } then
            parse_chars(reading_text([]), OpenTags, [Tok | Toks0], Toks)
          else
            { throw("unexpected close tag") }
        )
      else if { C = (<) } then
        { throw("`<' in close tag name") }
      else
        parse_chars(reading_close_tag([C | Cs]), OpenTags0, Toks0, Toks)
    ).

parse_char(C, reading_text(Cs), OpenTags, Toks0, Toks) -->
    ( if { is_wspc(C) } then
        ( if { Cs = [] } then
            parse_chars(reading_text(Cs), OpenTags, [spc(C) | Toks0], Toks)
          else
            { Tok = text(string__from_rev_char_list(Cs)) },
            parse_chars(reading_text([]), OpenTags, [spc(C), Tok | Toks0], Toks)
        )
      else if { C = (>) } then
        { throw("unexpected '>'") }
      else if { C = (<) } then
        ( if { Cs = [] } then
            parse_chars(reading_open_tag(Cs), OpenTags, Toks0, Toks)
          else
            { Tok = text(string__from_rev_char_list(Cs)) },
            parse_chars(reading_open_tag([]), OpenTags, [Tok | Toks0], Toks)
        )
      else
        parse_chars(reading_text([C | Cs]), OpenTags, Toks0, Toks)
    ).



:- func string_to_tag(string) = tag.

string_to_tag(S) = ( if st(S, T) then T else throw("unkown tag name: " ++ S) ).

:- func tag_to_string(tag) = string.

tag_to_string(T) = ( if st(S, T) then S else throw("input__tag_to_string: !") ).

:- pred st(string, tag).
:- mode st(in, out) is semidet.
:- mode st(out, in) is semidet.

st("B",  b).           st("EM", em).          st("I",  i).
st("PL", pl).          st("S",  s).           st("TT", tt).
st("U",  u(0)).

st("0", size(0)).
st("1", size(1)).      st("2", size(2)).      st("3", size(3)).
st("4", size(4)).      st("5", size(5)).      st("6", size(6)).
st("7", size(7)).      st("8", size(8)).      st("9", size(9)).

st("r", colour(r)).    st("g", colour(g)).    st("b", colour(b)).
st("c", colour(c)).    st("m", colour(m)).    st("y", colour(y)).
st("k", colour(k)).    st("w", colour(w)).  



:- pred is_wspc(char).
:- mode is_wspc(in) is semidet.

is_wspc(C) :-
    char__to_int(C, N),
    ( N = 0x20 ; N = 0x0D ; N = 0x0A ; N = 0x09 ).

%------------------------------------------------------------------------------%

:- func finish_tokenizing(st, tags, tokens) = tokens.

finish_tokenizing(reading_open_tag(_), _, _) =
    throw("unfinished open tag in input").

finish_tokenizing(reading_close_tag(_), _, _) =
    throw("unfinished close tag in input").

finish_tokenizing(reading_text(Cs), [], Toks) =
    ( if Cs = [] then Toks
                 else [text(string__from_rev_char_list(Cs)) | Toks]
    ).

finish_tokenizing(reading_text(_), [_|_], _) =
    throw("EOF with unclosed tags").

%------------------------------------------------------------------------------%

:- func purge_dead_spaces(tokens) = tokens.

purge_dead_spaces(Toks) =
    list__reverse(pds(Toks, root_attrs, [], no, [])).



:- func pds(tokens, attrs, list(attrs), maybe(attrs), tokens) = tokens.

pds([], _Attrs, _Attrss, _SpcAttrs, Toks) = Toks.

pds([text(T) | Toks0], Attrs, Attrss, _SpcAttrs, Toks) =
    pds(Toks0, Attrs, Attrss, no, [text(T) | Toks]).

pds([spc(S) | Toks0], Attrs, Attrss, no, Toks) =
    pds(Toks0, Attrs, Attrss, yes(Attrs), [spc(S) | Toks]).

pds([spc(S) | Toks0], Attrs, Attrss, yes(SpcAttrs), Toks) =
    ( if   equivalent_for_space(Attrs, SpcAttrs)
      then pds(Toks0, Attrs, Attrss, yes(SpcAttrs), Toks)
      else pds(Toks0, Attrs, Attrss, yes(/*Spc*/Attrs), [spc(S) | Toks])
    ).

pds([open(Tag) | Toks0], Attrs, Attrss, MaybeSpcAttrs, Toks) =
    pds(Toks0, apply_tag(Tag, Attrs), [Attrs | Attrss],
            MaybeSpcAttrs, [open(Tag) | Toks]).

pds([close(Tag) | Toks0], _Attrs, Attrss0, MaybeSpcAttrs, Toks) =
    pds(Toks0, Attrs, Attrss, MaybeSpcAttrs, [close(Tag) | Toks])
 :-
    hd_tl(Attrss0, Attrs, Attrss).



:- pred hd_tl(list(T), T, list(T)).
:- mode hd_tl(in, out, out) is det.

hd_tl([],       _, _ ) :- throw("input__hd_tl: aiiiieeeeeeee!").
hd_tl([X | Xs], X, Xs).

%------------------------------------------------------------------------------%

apply_tag(b,         Attrs) = ( Attrs ^ b      := yes             ).
apply_tag(i,         Attrs) = ( Attrs ^ i      := yes             ).
apply_tag(tt,        Attrs) = ( Attrs ^ tt     := yes             ).
apply_tag(u(_),      Attrs) = ( Attrs ^ u      := Attrs ^ u + 1   ).
apply_tag(size(S),   Attrs) = ( Attrs ^ size   := S               ).
apply_tag(colour(C), Attrs) = ( Attrs ^ colour := C               ).
apply_tag(s,         Attrs) = ( Attrs ^ s      := yes             ).
apply_tag(em,        Attrs) = ( Attrs ^ em     := not(Attrs ^ em) ).
apply_tag(pl,        Attrs) =
    (((((( Attrs
                    ^ u     := 0 )
                    ^ b     := no )
                    ^ em    := no )
                    ^ i     := no )
                    ^ s     := no )
                    ^ tt    := no ).

%------------------------------------------------------------------------------%

    % Extract the raw plain-text from the token list into a string.
    %
:- func plain_text(tokens) = string.

plain_text(Toks) =
    string__append_list(list__filter_map(extract_text, Toks)).



:- func extract_text(token) = string is semidet.

extract_text(text(T)) = T.
extract_text(spc(S))  = string__char_to_string(S).

%------------------------------------------------------------------------------%

read_smlng(FirstFile, FirstDone, Str, Attrss, Extents) -->
    tokenize_input(Toks0),
    { Toks     = purge_dead_spaces(Toks0) },
    { Str      = plain_text(Toks) },
    { attrss_extents(Toks, Attrss, Extents) },

 % io__print("attribute records:\n"), io__print(Attrss), io__nl,
 % io__print("extents:\n"), io__print(Extents), io__nl,

        % NOTE At this point we write out Toks to FirstFile so
        % we can guarantee having something that is no larger
        % than the input!  The follow-on optimizations *may*
        % actually make things worse.
        %
    write_tokens_to_file(FirstFile, Toks),

    io__tell(FirstDone, Result),
    ( if { Result = error(_) } then { throw(Result) } ),
    io__write_string("\n"),
    io__told.
    
%------------------------------------------------------------------------------%
    
    % Write out a sequence of tokens.
    %
:- pred write_tokens_to_file(string, tokens, io__state, io__state).
:- mode write_tokens_to_file(in, in, di, uo) is det.

write_tokens_to_file(File, Toks) -->
    io__tell(File, Result),
    ( if { Result = error(_) } then { throw(Result) } ),
    write_tokens(Toks),
    io__told.



write_tokens(Toks) -->
    io__write_list(Toks, "", write_token).



:- pred write_token(token, io__state, io__state).
:- mode write_token(in, di, uo) is det.

write_token(open(T)) -->    io__format("<%s>", [s(tag_to_string(T))]).
write_token(close(T)) -->   io__format("</%s>", [s(tag_to_string(T))]).
write_token(spc(S)) -->     io__write_char(S).
write_token(text(T)) -->    io__write_string(T).

%------------------------------------------------------------------------------%

:- type ae == pair(attrs, int).

:- pred attrss_extents(tokens, attrss, extents).
:- mode attrss_extents(in, out, out) is det.

attrss_extents(Toks, Attrss, Extents) :-
    AEs     = aes(Toks, root_attrs, 0, [], []),
    Attrss  = array([root_attrs | list__map(fst, AEs)]),
    Extents = array([0 | list__map(snd, AEs)]).



:- func aes(tokens, attrs, int, list(attrs), list(ae)) = list(ae).

aes([], As, E, _, AEs) =
    finish_aes([As - E | AEs], []).

aes([spc(_) | Toks], As, E, Stk, AEs) =
    aes(Toks, As, E + 1, Stk, AEs).

aes([text(T) | Toks], As, E, Stk, AEs) =
    aes(Toks, As, E + string__length(T), Stk, AEs).

aes([open(Tag) | Toks], As, E, Stk, AEs) =
    aes(Toks, apply_tag(Tag, As), 0, [As | Stk], [As - E | AEs]).

aes([close(_) | Toks], As0, E0, Stk0, AEs) =
    ( if   Stk0 = [As | Stk]
      then aes(Toks, As, 0, Stk, [As0 - E0 | AEs])
      else throw("input__aes: glark!")
    ).



:- func finish_aes(list(ae), list(ae)) = list(ae).

finish_aes([],                          AEs) = AEs.

finish_aes([As - E],                    AEs) =
    ( if E = 0 then AEs else [As - E | AEs] ).

finish_aes([As0 - E0, As1 - E1 | AEs0], AEs) =
    ( if      E0 = 0
      then    finish_aes([As1 - E1 | AEs0], AEs)
      else if equivalent(As0, As1)
      then    finish_aes([As1 - (E0 + E1) | AEs0], AEs)
      else    finish_aes([As1 - E1 | AEs0], [As0 - E0 | AEs])
    ).

%------------------------------------------------------------------------------%

opt_suffix = ".smlng".

done_suffix = ".done".

    % The suffix for optimized output files: ".smlng"
    %
:- func opt_suffix = string.

    % The suffix for `done' files (written when the corresponding
    % optimized output file has been closed): ".done"
    %
:- func done_suffix = string.

file_names(BaseName, N,
    string__format("%s-%d%s", [s(BaseName), i(N), s(opt_suffix)]),
    string__format("%s-%d%s", [s(BaseName), i(N), s(done_suffix)])
).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
