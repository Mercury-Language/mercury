#-----------------------------------------------------------------------------#
# Copyright (C) 1994-1995, 1997 The University of Melbourne. 
# This file may only be copied under the terms of the GNU General
# Public Licence - see the file COPYING in the Mercury distribution.
#-----------------------------------------------------------------------------#
#
#  Usage: awk -f source_stats.awk *.m
#
#  This computes some simple statistics about Mercury source code.

{ lines++; }

/^[ 	]*$/		{ blank++; next; }
/^[ 	]*%[ 	%-]*$/	{ blank++; next; }
/^[ 	]*%/		{ comments++; next; }

/is[ 	]*det/		{ det_preds++; }
/is[ 	]*semidet/	{ semidet_preds++; }
/is[ 	]*nondet/	{ nondet_preds++; }
/is[ 	]*multi/	{ multidet_preds++; }
/is[ 	]*cc_nondet/	{ cc_nondet_preds++; }
/is[ 	]*cc_multi/	{ cc_multidet_preds++; }
/is[ 	]*erroneous/	{ erroneous_preds++; }
/is[ 	]*failure/	{ failure_preds++; }

/^:-[ 	]*func/		{ func_count++; in_func = 1; }
/^:-[ 	]*func.*::/	{ funcmode_count++; in_func = 1; }
/^:-[ 	]*pred/		{ pred_count++; in_pred = 1; }
/^:-[ 	]*pred.*::/	{ predmode_count++; in_pred = 1; }
/^:-[ 	]*mode/		{ mode_count++; in_mode = 1; }
/^:-[ 	]*type/		{ type_count++; in_type = 1; }
/^:-[ 	]*inst/		{ inst_count++; in_inst = 1; }
/^:-/			{ in_decl = 1; }
{
	if (in_func) funcs++;
	if (in_pred) preds++;
	if (in_mode) modes++;
	if (in_type) types++;
	if (in_inst) insts++;
	if (in_decl) decls++;
}
/\.[ 	]*$/ { in_pred = in_func = in_mode = in_type = in_inst = in_decl = 0; }
END {
	total_mode_count = mode_count + predmode_count + funcmode_count;
	printf("Number of types:                %6d\n", type_count);
	printf("Number of insts:                %6d\n", inst_count);
	printf("Number of predicates:           %6d\n", pred_count);
	printf("Number of functions:            %6d\n", func_count);
	printf("\n");
	printf("Number of predmodes:            %6d\n", predmode_count);
	printf("Number of funcmodes:            %6d\n", funcmode_count);
	printf("Number of separate modes:       %6d\n", mode_count);
	printf("Number of implicit function modes:    ?\n");
	printf("Total number of modes:       >= %6d\n", total_mode_count);
	printf("                             =< %6d\n",
					func_count + total_mode_count);
	printf("        - det:                  %6d (%6.2f%%)\n",				det_preds, 100 * det_preds / total_mode_count);
	printf("        - semidet:              %6d (%6.2f%%)\n",				semidet_preds, 100 * semidet_preds / total_mode_count);
	printf("        - nondet:               %6d (%6.2f%%)\n",				nondet_preds, 100 * nondet_preds / total_mode_count);
	printf("        - multi:                %6d (%6.2f%%)\n",				multidet_preds, 100 * multidet_preds / total_mode_count);
	printf("        - cc_nondet:            %6d (%6.2f%%)\n",				cc_nondet_preds, 100 * cc_nondet_preds / total_mode_count);
	printf("        - cc_multi:             %6d (%6.2f%%)\n",				cc_multidet_preds, 100 * cc_multidet_preds / total_mode_count);
	printf("        - erroneous:            %6d (%6.2f%%)\n",				erroneous_preds, 100 * erroneous_preds / total_mode_count);
	printf("        - failure:              %6d (%6.2f%%)\n",				failure_preds, 100 * failure_preds / total_mode_count);
	printf("Average modes per predicate: >= %6.3f\n",					(predmode_count + mode_count) / pred_count);
	printf("                             =< %6.3f\n",					total_mode_count / pred_count);
	printf("\n");
	printf("Blank lines:                    %6d (%6.2f%%)\n", 				blank, 100 * blank / lines);
	printf("Comment lines:                  %6d (%6.2f%%)\n",				comments, 100 * comments / lines);
	whitespace = blank + comments;
	printf("Total whitespace/comment lines: %6d (%6.2f%%)\n",				whitespace, 100 * whitespace / lines);
	printf("\n");
	printf("Function declaration lines:     %6d (%6.2f%%)\n",				funcs, 100 * funcs / lines);
	printf("Predicate declaration lines:    %6d (%6.2f%%)\n",				preds, 100 * preds / lines);
	printf("Mode declaration lines:         %6d (%6.2f%%)\n",				modes, 100 * modes / lines);
	printf("Type declaration lines:         %6d (%6.2f%%)\n",				types, 100 * types / lines);
	printf("Inst declaration lines:         %6d (%6.2f%%)\n",				insts, 100 * insts / lines);
	other_decls = decls - preds - funcs - modes - types - insts;
	printf("Other declaration lines:        %6d (%6.2f%%)\n",				other_decls, 100 * other_decls / lines);
	printf("Total declaration lines:        %6d (%6.2f%%)\n",				decls, 100 * decls / lines);
	printf("\n");
	code = lines - whitespace - decls;
	printf("Code lines:                     %6d (%6.2f%%)\n",				code, 100 * code / lines);
	printf("\n");
	printf("Total number of lines:          %6d (%6.2f%%)\n", lines, 100);
}
