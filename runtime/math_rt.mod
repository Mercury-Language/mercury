/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**  File: runtime/math_rt.mod.
**  Main author: bromage.
** 
**  This file implements the higher mathematical operations for
**  Mercury defined in `float.m' and `math.m'.
**
**  XXX This is a temporary fix only.  When the better C interface has
**      been written, this should be rewritten to use that.
*/

#include <math.h>
#include <float.h>
#include "imp.h"

/*
** System constants.  These are defined in float.h.
*/

#if defined USE_SINGLE_PREC_FLOAT
#define	MERCURY_FLOAT_MAX	FLT_MAX
#define	MERCURY_FLOAT_MIN	FLT_MIN
#define	MERCURY_FLOAT_EPSILON	FLT_EPSILON
#else
#define	MERCURY_FLOAT_MAX	DBL_MAX
#define	MERCURY_FLOAT_MIN	DBL_MIN
#define	MERCURY_FLOAT_EPSILON	DBL_EPSILON
#endif

/*
** Mathematical constants.
*/

#define	MERCURY_FLOAT__E		2.7182818284590452354
#define	MERCURY_FLOAT__PI		3.1415926535897932384
#define	MERCURY_FLOAT__LN2		0.69314718055994530941

/*
** Handle domain errors.
*/
static void
domain_error(const char *where)
{
	fflush(stdout);
	fprintf(stderr, "Software error: Domain error in call to `%s'\n",
		where);
	exit(1);
}

BEGIN_MODULE(math_rt)
BEGIN_CODE

/*
** System constants from float.m
*/

	/* Maximum floating-point number */
mercury__float__max_1_0:
	r1 = float_to_word(MERCURY_FLOAT_MAX);
	proceed();

	/* Minimum normalised floating-point number */
mercury__float__min_1_0:
	r1 = float_to_word(MERCURY_FLOAT_MIN);
	proceed();

	/* Smallest x such that x \= 1.0 + x */
mercury__float__epsilon_1_0:
	r1 = float_to_word(MERCURY_FLOAT_EPSILON);
	proceed();

/*
** Mathematical constants from math.m
*/

	/* Pythagoras' number */
mercury__math__pi_1_0:
	r1 = float_to_word(MERCURY_FLOAT__PI);
	proceed();

	/* Base of natural logarithms */
mercury__math__e_1_0:
	r1 = float_to_word(MERCURY_FLOAT__E);
	proceed();

/*
** math__ceiling(X, Ceil) is true if Ceil is the smallest integer
** not less than X.
*/
mercury__math__ceiling_2_0:
	r2 = float_to_word((Float)ceil((double)word_to_float(r1)));
	proceed();

/*
** math__floor(X, Floor) is true if Floor is the largest integer
** not greater than X.
*/
mercury__math__floor_2_0:
	r2 = float_to_word((Float)floor((double)word_to_float(r1)));
	proceed();

/*
** math__round(X, Round) is true if Round is the integer
** closest to X.  If X has a fractional component of 0.5,
** it is rounded up.
*/
mercury__math__round_2_0:
	r2 = float_to_word((Float)floor(0.5+(double)word_to_float(r1)));
	proceed();

/*
** math__truncate(X, Trunc) is true if Trunc is the integer
** closest to X such that |Trunc| =< |X|.
*/
mercury__math__truncate_2_0:
{
	double	x = (double)word_to_float(r1), y;
	if (x < 0.0)
	    y = ceil(x);
	else
	    y = floor(x);
	r2 = float_to_word((Float)y);
}
	proceed();

/*
** math__sqrt(X, Sqrt) is true if Sqrt is the positive square
** root of X.  
**
** Domain restrictions:
**		X >= 0
*/
mercury__math__sqrt_2_0:
{
	double	x = (double)word_to_float(r1);
	if (x < 0.0)
	    domain_error("math__sqrt");
	r2 = float_to_word((Float)sqrt(x));
}
	proceed();

/*
** math__pow(X, Y, Res) is true if Res is X raised to the
** power of Y.
**
** Domain restrictions:
**		X >= 0
**		X = 0 implies Y > 0
*/
mercury__math__pow_3_0:
{
	double	x = (double)word_to_float(r1),
		y = (double)word_to_float(r2),
		res;
	if (x < 0.0)
	    domain_error("math__pow");
	if (x == 0.0)
	{
	    if (y <= 0.0)
		domain_error("math__pow");
	    res = 0.0;
	}
	else
	    res = pow(x,y);
	r3 = float_to_word((Float)res);
}
	proceed();

/*
** math__exp(X, Exp) is true if Exp is X raised to the
** power of e.
*/
mercury__math__exp_2_0:
	r2 = float_to_word((Float)exp((double)word_to_float(r1)));
	proceed();

/*
** math__ln(X, Log) is true if Log is the natural logarithm
** of X.
**
** Domain restrictions:
**		X > 0
*/
mercury__math__ln_2_0:
{
	double	x = (double)word_to_float(r1);

	if (x <= 0.0)
	    domain_error("math__ln");
	r2 = float_to_word((Float)log(x));
}
	proceed();

/*
** math__log10(X, Log) is true if Log is the logarithm to
** base 10 of X.
**
** Domain restrictions:
**		X > 0
*/
mercury__math__log10_2_0:
{
	double	x = (double)word_to_float(r1);

	if (x <= 0.0)
	    domain_error("math__log10");
	r2 = float_to_word((Float)log10(x));
}
	proceed();

/*
** math__log2(X, Log) is true if Log is the logarithm to
** base 2 of X.
**
** Domain restrictions:
**		X > 0
*/
mercury__math__log2_2_0:
{
	double	x = (double)word_to_float(r1);

	if (x <= 0.0)
	    domain_error("math__log2");
	r2 = float_to_word((Float)(log(x)/MERCURY_FLOAT__LN2));
}
	proceed();

/*
** math__log(B, X, Log) is true if Log is the logarithm to
** base B of X.
**
** Domain restrictions:
**		X > 0
**		B > 0
**		B \= 1
*/
mercury__math__log_3_0:
{
	double	b = (double)word_to_float(r1),
		x = (double)word_to_float(r2);

	if (x <= 0.0 || b <= 0.0)
	    domain_error("math__log");
	if (b == 1.0)
	    domain_error("math__log");
	r3 = float_to_word((Float)(log(x)/log(b)));
}
	proceed();

/*
** math__sin(X, Sin) is true if Sin is the sine of X.
*/
mercury__math__sin_2_0:
	r2 = float_to_word((Float)sin((double)word_to_float(r1)));
	proceed();

/*
** math__cos(X, Sin) is true if Cos is the cosine of X.
*/
mercury__math__cos_2_0:
	r2 = float_to_word((Float)cos((double)word_to_float(r1)));
	proceed();

/*
** math__tan(X, Tan) is true if Tan is the tangent of X.
*/
mercury__math__tan_2_0:
	r2 = float_to_word((Float)tan((double)word_to_float(r1)));
	proceed();

/*
** math__asin(X, ASin) is true if ASin is the inverse
** sine of X, where ASin is in the range [-pi/2,pi/2].
**
** Domain restrictions:
**		X must be in the range [-1,1]
*/
mercury__math__asin_2_0:
{
	double	x = (double)word_to_float(r1);

	if (x < -1.0 || x > 1.0)
	    domain_error("math__asin");
	r2 = float_to_word((Float)asin(x));
}
	proceed();

/*
** math__acos(X, ACos) is true if ACos is the inverse
** cosine of X, where ACos is in the range [0, pi].
**
** Domain restrictions:
**		X must be in the range [-1,1]
*/
mercury__math__acos_2_0:
{
	double	x = (double)word_to_float(r1);

	if (x < -1.0 || x > 1.0)
	    domain_error("math__acos");
	r2 = float_to_word((Float)acos(x));
}
	proceed();

/*
** math__atan(X, ATan) is true if ATan is the inverse
** tangent of X, where ATan is in the range [-pi/2,pi/2].
*/
mercury__math__atan_2_0:
	r2 = float_to_word((Float)atan((double)word_to_float(r1)));
	proceed();

/*
** math__atan2(Y, X, ATan) is true if ATan is the inverse
** tangent of Y/X, where ATan is in the range [-pi,pi].
*/
mercury__math__atan2_3_0:
	r3 = float_to_word((Float)atan2((double)word_to_float(r1),
	                                (double)word_to_float(r2)));
	proceed();

/*
** math__sinh(X, Sinh) is true if Sinh is the hyperbolic
** sine of X.
*/
mercury__math__sinh_2_0:
	r2 = float_to_word((Float)sinh((double)word_to_float(r1)));
	proceed();

/*
** math__cosh(X, Cosh) is true if Cosh is the hyperbolic
** cosine of X.
*/
mercury__math__cosh_2_0:
	r2 = float_to_word((Float)cosh((double)word_to_float(r1)));
	proceed();

/*
** math__tanh(X, Tanh) is true if Tanh is the hyperbolic
** tangent of X.
*/
mercury__math__tanh_2_0:
	r2 = float_to_word((Float)tanh((double)word_to_float(r1)));
	proceed();

END_MODULE
