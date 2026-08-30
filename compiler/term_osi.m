%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains our first termination analysis system.
%
% This termination analysis is based on the algorithm given by Gerhard Groeger
% and Lutz Plumer in their paper "Handling of Mutual Recursion in Automatic
% Termination Proofs for Logic Programs"  which was printed in JICSLP '92
% (the proceedings of the Joint International Conference and Symposium on
% Logic Programming in 1992), pages 336 - 350.
%
% Details about this implementation are covered in:
% Chris Speirs, Zoltan Somogyi, and Harald Sondergaard: Termination
% analysis for Mercury. In P. Van Hentenryck, editor, Static Analysis:
% Proceedings of the 4th International Symposium, Lecture Notes in Computer
% Science, Springer, 1997. A more detailed version is available for download
% from the Mercury project's papers page.
%
%-----------------------------------------------------------------------------%

:- module termination.term_osi.
:- interface.

:- include_module term_osi_main.
:- include_module term_osi_util.

:- implementation.

:- include_module term_osi_pass1.
:- include_module term_osi_pass2.
:- include_module term_osi_traversal.
:- include_module term_osi_errors.

%-----------------------------------------------------------------------------%
:- end_module termination.term_osi.
%-----------------------------------------------------------------------------%
