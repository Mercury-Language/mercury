#include <stdio.h>

/* Globals */
extern lprec   *Lp; /* extern pointer to active problem */
extern int     Rows;
extern int     Columns;
extern int     Sum;
extern int     Non_zeros;
extern int     Level;
extern matrec  *Mat;
extern int     *Col_no;
extern int     *Col_end;
extern int     *Row_end;
extern REAL    *Orig_rh;
extern REAL    *Rh;
extern REAL    *Rhs;
extern short   *Must_be_int;
extern REAL    *Orig_upbo;
extern REAL    *Orig_lowbo;
extern REAL    *Upbo;
extern REAL    *Lowbo;
extern int     *Bas;
extern short   *Basis;
extern short   *Lower;
extern int     Eta_alloc; 
extern int     Eta_size;           
extern int     Num_inv;
extern REAL    *Eta_value;
extern int     *Eta_row_nr;
extern int     *Eta_col_end;
extern REAL    *Solution;
extern REAL    *Best_solution;
extern REAL    Infinite;
extern REAL    Epsilon;
extern REAL    Epsb;
extern REAL    Epsd;
extern REAL    Epsel;

extern REAL    TREJ;
extern REAL    TINV;

extern short   Maximise;
extern short   Floor_first;
extern REAL    Extrad;

extern int     Warn_count;

extern short just_inverted;
extern short status;
extern short do_iter;
extern short do_invert;


/* Globals for parser */
extern FILE       *yyin;
extern FILE       *lpfilename;
extern short      Molve_dual;
extern short      Maximise;
extern short      *relat;
extern int        Verbose;
extern int        yylineno;
extern int        yyleng;
extern int        Lin_term_count;
extern int        Sign;
extern constraint_name *First_constraint_name;
/* I hate #ifdefs, but there seems to be no "standard" way to do this */
#if defined(__hpux) || defined(__apollo) || defined(_AIX) || defined(_OSF_SOURCE)
/* for HP/UX, Apollo, AIX, DEC OSF  */
extern unsigned char       yytext[];
#else
/* For other computers */
extern char    yytext[];
#endif

extern hashelem   *Hash_tab[];
extern rside      *First_rside;
extern short      Ignore_decl;

extern tmp_store_struct tmp_store;
