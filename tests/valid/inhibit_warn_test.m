% missing `:- module' declaration
:- pred p(int).
:- mode p(out) is semidet. % determinism could be tighter
p(1).
