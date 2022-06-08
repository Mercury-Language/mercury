%------------------------------------------------------------------------------%
% optimize.m
% Ralph Becket <rbeck@microsoft.com>
% Fri Jul 27 01:58:09 BST 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
%------------------------------------------------------------------------------%

    %--------------------------------------------------------------------------%
    % [FIRST CUT]
    %
    % The optimizer works by computing the shortest tag sequence for
    % each consecutive N-sequence of attribute records and appending
    % them.  In the case of a draw the choice is made arbitrarily.
    % N is a search procedure parameter.
    %
    % The sequence generator takes the following arguments:
    % - the current attribute record;
    % - the index of the document attribute record we need to change to;
    % - the end-of-sequence index;
    % - the stack of currently open tags and the attribute records they
    % were applied to;
    % - the sequence of tokens to this point.
    %  
    % The non-determimistic generation relation works as follows:
    % - if a match has been achieved then
    %   - if we're reached the end of the sequence then return the current
    %   token sequence;
    %   - else recurse for the next index in the sequence
    % - else we still have some matching work to do:
    %   - if there are any *necessary closures*, namely
    %     * a size change to the root_size or
    %     * a colour change to the root_colour
    %     then
    %     - pop the stack (extending the tag list) until the requisite
    %     closures have been made and recurse
    %   - else
    %     - close a tag that needs closing and recurse - or
    %     - add a tag to set an attribute that needs setting - or
    %     - add a PL to close one or more tags that need closing and
    %     recurse.
    %     * If EM is in force and needs deleting then either close it
    %     or open a new EM.
    %
    % * Micro-optimization: after the document has been optmized by
    % the above procedure, we can change the innermost EM blocks that
    % do set the EM attribute to just S which saves a couple of bytes
    % in the output.
    %
    % * Once the generator has pushed a tag, there's no point in
    % it popping afterwards (and it would lead to unbounded recursion).
    %
    % * Iterative deepening is the right strategy here, otherwise we're
    % going to have an even more enormous search space.
    %--------------------------------------------------------------------------%

:- module optimize.

:- interface.

:- import_module io.
:- import_module input.



    % write_optimized_files(File, Str, Attrss, Exts, Num)
    %
    % Write out successive files with increasing optimization windows.
    % The files have names of the form File-NNN.smlng where NNN is the
    % optimization window size (starts off at Num and increases on
    % iteration; Num *must* be positive).
    %
    % When a particular file has been closed a corresponding file
    % named File-NNN.done is created.
    %
:- pred write_optimized_files(string, string, attrss, extents, int,
            io__state, io__state).
:- mode write_optimized_files(in, in, in, in, in, di, uo) is cc_multi.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module string, list, exception, int, array, bool, prolog.
:- import_module attrs.



:- type tas == list({tag, attrs}).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

write_optimized_files(BaseName, Str, Attrss, Exts, Num) -->

 % io__stderr_stream(StdErr),
 % io__format(StdErr, "optimizing file using window size %d\n", [i(Num)]),

    { file_names(BaseName, Num, SMLNG, Done) },

    prolog__tell(SMLNG, Res1),
    ( if { Res1 = error(_) } then { throw(Res1) } ),
    write_optimized(Str, Attrss, Exts, Num),
    prolog__told,

    prolog__tell(Done, Res2),
    ( if { Res2 = error(_) } then { throw(Res2) } ),
    io__write_string("\n"),
    prolog__told.



    % For each N-stretch, generate the shortest stretch of tokens
    % and output them interspersed with the intervening plaintext.
    % [There's no point in working out the whole file beforehand.]
    %
:- pred write_optimized(string, attrss, extents, int, io__state, io__state).
:- mode write_optimized(in, in, in, in, di, uo) is cc_multi.

write_optimized(Str, Attrss, Exts, Num) -->
    write_optimized_2(Str, Attrss, Exts, Num, 1, array__max(Attrss), 0, []).



:- pred write_optimized_2(string, attrss, extents, int, int, int, int, tas,
            io__state, io__state).
:- mode write_optimized_2(in, in, in, in, in, in, in, in, di, uo) is cc_multi.

write_optimized_2(Str, Attrss, Exts, Num, I0, End, Off0, Stk0) -->
    ( if { I0 > End } then
        write_close_stack_tokens(Stk0)
      else
        { As = Attrss ^ elem(I0 - 1) },
        { I  = min(I0 + Num - 1, End) },
        { gen_n_cc(0, I0, I, As, Attrss, Stk0, Stk, [], Tss) },
        write_tokenss(Str, Attrss, Exts, I0, Off0, Off, Tss),
        write_optimized_2(Str, Attrss, Exts, Num, I + 1, End, Off, Stk)
    ).



:- pred write_close_stack_tokens(tas, io__state, io__state).
:- mode write_close_stack_tokens(in, di, uo) is det.

write_close_stack_tokens(Stk) -->
    write_tokens(list__map(func({Tag, _As}) = close(Tag), Stk)).



    % Write out each sequence of tokens followed by the plaintext
    % over which the attribute record it creates ranges.
    %
:- pred write_tokenss(string, attrss, extents, int, int, int, list(tokens),
            io__state, io__state).
:- mode write_tokenss(in, in, in, in, in, out, in, di, uo) is det.

write_tokenss(_Str, _Attrss, _Exts, _I, Off, Off, []) -->
    [].

write_tokenss(Str, Attrss, Exts, I, Off0, Off, [Ts | Tss]) -->

 % io__stderr_stream(StdErr),
 % io__print(StdErr, Ts),
 % io__nl(StdErr),

    write_tokens(Ts),
    { Ext0 = Exts ^ elem(I) },
    io__write_string(string__between(Str, Off0, Off0 + Ext0)),

 % io__print(StdErr, string__substring(Str, Off0, Ext0) `with_type` string),
 % io__nl(StdErr),

    write_tokenss(Str, Attrss, Exts, I + 1, Off0 + Ext0, Off, Tss).



    % Find the smallest solution for the next few transformations.
    %
:- pred gen_n_cc(int, int, int, attrs, attrss, tas, tas,
            list(tokens), list(tokens)).
:- mode gen_n_cc(in, in, in, in, in, in, out, in, out) is cc_multi.

gen_n_cc(N, I, End, As, Attrss, Stk0, Stk, Tss0, Tss) :-
    ( if   gen_n(I, End, As, Attrss, Stk0, Stk1, N, _, Tss0, Tss1)
      then Stk = Stk1, Tss = list__reverse(Tss1)
      else gen_n_cc(N + 1, I, End, As, Attrss, Stk0, Stk, Tss0, Tss)
    ).



    % Find a solution transforming As into Attrss elems I, I + 1, ..., End.
    %
:- pred gen_n(int, int, attrs, attrss, tas, tas, int, int,
            list(tokens), list(tokens)).
:- mode gen_n(in, in, in, in, in, out, in, out, in, out) is nondet.

gen_n(I, End, As, Attrss, Stk0, Stk, N0, N, Tss0, Tss) :-
    ( if I > End then
        Stk = Stk0,
        N   = N0,
        Tss = Tss0
      else
        AsI = Attrss ^ elem(I),
        gen(As, AsI, Stk0, Stk1, N0, N1, [], Ts0),
        Ts  = list__reverse(Ts0),
        gen_n(I + 1, End, AsI, Attrss, Stk1, Stk, N1, N, [Ts | Tss0], Tss)
    ).



    % gen(As0, As, Stk0, Stk, N0, N, Ts0, Ts)
    %
    % Work out how to change As0 to As starting with open tag-attrs stack
    % Stk0 adding no more than N0 tags to Ts0.
    %
:- pred gen(attrs, attrs, tas, tas, int, int, tokens, tokens).
:- mode gen(in, in, in, out, in, out, in, out) is nondet.

gen(As, AsI, Stk0, Stk, N0, N, Ts0, Ts) :-

    ( if equivalent(As, AsI) then

        Stk = Stk0,
        N   = N0,
        Ts  = Ts0

      else if closing_necessary(As, AsI) then

        close_tag(Stk0, Stk1, N0, N1, Ts0, Ts1, As1),
        gen(As1, AsI, Stk1, Stk, N1, N, Ts1, Ts)

      else

        (
            close_tag(Stk0, Stk1, N0, N1, Ts0, Ts1, As1),
            gen(As1, AsI, Stk1, Stk, N1, N, Ts1, Ts)
        ;
            gen_open(As, AsI, Stk0, Stk, N0, N, Ts0, Ts)
        )
    ).



:- pred closing_necessary(attrs, attrs).
:- mode closing_necessary(in, in) is semidet.

closing_necessary(As, AsI) :-
    (
        As ^ colour \= root_colour,     AsI ^ colour = root_colour
    ;
        As ^ size \= root_size,         AsI ^ size = root_size
    ).



    % Try to close the top tag on the stack.
    %
:- pred close_tag(tas, tas, int, int, tokens, tokens, attrs).
:- mode close_tag(in, out, in, out, in, out, out) is semidet.

close_tag([{Tag, As} | Stk], Stk, N0, N, Ts, [close(Tag) | Ts], As) :-
    N = N0 - tag_cost(Tag),
    N >= 0.



    % Open a new tag.
    %
:- pred open_tag(tag,attrs,attrs,tas,tas,int,int,tokens,tokens).
:- mode open_tag(in, in, out, in, out, in, out, in, out) is semidet.

open_tag(Tag, As0, As, Stk, [{Tag,As0} | Stk], N0, N, Ts, [open(Tag) | Ts]) :-
    N = N0 - (1 + tag_cost(Tag)),
    N >= 0,
    As = apply_tag(Tag, As0).



:- func tag_cost(tag) = int.

tag_cost(T) = ( if (T = em ; T = pl) then 2 else 1 ).



    % Find the means to change one attribute record into another by
    % only opening new tags.
    %
:- pred gen_open(attrs, attrs, tas, tas, int, int, tokens, tokens).
:- mode gen_open(in, in, in, out, in, out, in, out) is nondet.

gen_open(As, AsI, Stk0, Stk, N0, N, Ts0, Ts) :-

    ( if equivalent(As, AsI) then

        Stk = Stk0,
        N   = N0,
        Ts  = Ts0

      else

        (
            As ^ colour \= AsI ^ colour,    Tag = colour(AsI ^ colour)
        ;
            As ^ b  = no, AsI ^ b  = yes,   Tag = b
        ;                          
            As ^ i  = no, AsI ^ i  = yes,   Tag = i
        ;                          
            As ^ tt = no, AsI ^ tt = yes,   Tag = tt
        ;                          
            As ^ em \= AsI ^ em,            Tag = em
        ;                          
            As ^ s  = no, AsI ^ s  = yes,   Tag = s
        ;                          
            As ^ u < AsI ^ u, As ^ u < 3,   Tag = u(0)
        ;
            As ^ size \= AsI ^ size,        Tag = size(AsI ^ size)
        ;
            (   As ^ b  = yes, AsI ^ b  = no
            ;   As ^ i  = yes, AsI ^ i  = no
            ;   As ^ tt = yes, AsI ^ tt = no
            ;   As ^ em = yes, AsI ^ tt = no
            ;   As ^ u > AsI ^ u, AsI ^ u < 3
            ),
            Tag = pl
        ),
        open_tag(Tag, As, As1, Stk0, Stk1, N0, N1, Ts0, Ts1),
        gen_open(As1, AsI, Stk1, Stk, N1, N, Ts1, Ts)

    ).


