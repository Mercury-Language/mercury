%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module deeply_nested.
:- interface.
  :- import_module io.

  :- module a.
  :- interface.
    :- type a ---> a.
    :- module b.
    :- interface.
      :- type b ---> b.
      :- module c.
      :- interface.
        :- type c ---> c.
        :- module d.
        :- interface.
          :- type d ---> d.
          :- module e.
          :- interface.
            :- type f ---> f.
          :- end_module e.
        :- end_module d.
      :- end_module c.
    :- end_module b.
  :- end_module a.

  :- pred main(io::di, io::uo) is det.

:- implementation.

  :- import_module deeply_nested.a.
  :- import_module deeply_nested.a.b.
  :- use_module    deeply_nested.a.b.c.
  :- import_module deeply_nested.a.b.c.d.
  :- use_module    deeply_nested.a.b.c.d.e.

  :- type t0 == deeply_nested.a.b.c.d.e.f.
% These were allowed until 2016 january.
%   :- type t1 == a.b.c.d.e.f.
%   :- type t2 == b.c.d.e.f.
%   :- type t3 == c.d.e.f.
% %  :- type t4 == d.e.f.     % error, "d" must be referred to as "c.d"
%   :- type t5 == e.f.
% %  :- type t6 == f.     % error, "f" must be referred to as "e.f"

main -->
    print(deeply_nested.a.b.c.d.e.f), nl,
    print(a.b.c.d.e.f), nl,
    print(b.c.d.e.f), nl,
    print(c.d.e.f), nl,
%   print(d.e.f), nl,   % error, "d" must be referred to as "c.d"
    print(e.f), nl,
%   print(f), nl,       % error, "f" must be referred to as "e.f"
    [].

:- end_module deeply_nested.
