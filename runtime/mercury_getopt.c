extern	char	*getenv(const char *);
/* Getopt for GNU.
   NOTE: getopt is now part of the C library, so if you don't know what
   "Keep this file name-space clean" means, talk to roland@gnu.ai.mit.edu
   before changing it!

   Copyright (C) 1987, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97
   	Free Software Foundation, Inc.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* This tells Alpha OSF/1 not to define a getopt prototype in <stdio.h>.
   Ditto for AIX 3.2 and <stdlib.h>.  */
#ifndef _NO_PROTO
#define _NO_PROTO
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if !defined (__STDC__) || !__STDC__
/* This is a separate conditional since some stdc systems
   reject `defined (const)'.  */
#ifndef const
#define const
#endif
#endif

#include <stdio.h>

/* Comment out all this code if we are using the GNU C Library, and are not
   actually compiling the library itself.  This code is part of the GNU C
   Library, but also included in many other GNU distributions.  Compiling
   and linking in this code is a waste when using the GNU C library
   (especially if it is a shared library).  Rather than having every GNU
   program understand `configure --with-gnu-libc' and omit the object files,
   it is simpler to just do this in the source for each such file.  */

#define GETOPT_INTERFACE_VERSION 2
#if !defined (_LIBC) && defined (__GLIBC__) && __GLIBC__ >= 2
#include <gnu-versions.h>
#if _GNU_GETOPT_INTERFACE_VERSION == GETOPT_INTERFACE_VERSION
#define ELIDE_CODE
#endif
#endif

#ifndef XXXELIDE_CODEXXX


/* This needs to come after some library #include
   to get __GNU_LIBRARY__ defined.  */
#ifdef	__GNU_LIBRARY__
/* Don't include stdlib.h for non-GNU C libraries because some of them
   contain conflicting prototypes for getopt.  */
#include <stdlib.h>
#include <unistd.h>
#endif	/* GNU C library.  */

#ifdef VMS
#include <unixlib.h>
#if HAVE_STRING_H - 0
#include <string.h>
#endif
#endif

#if defined (WIN32) && !defined (__CYGWIN32__)
/* It's not Unix, really.  See?  Capital letters.  */
#include <windows.h>
#define getpid() GetCurrentProcessId()
#endif

#ifndef _
/* This is for other GNU distributions with internationalized messages.
   When compiling libc, the _ macro is predefined.  */
#ifdef HAVE_LIBINTL_H
# include <libintl.h>
# define _(msgid)	gettext (msgid)
#else
# define _(msgid)	(msgid)
#endif
#endif

/* This version of `MR_getopt' appears to the caller like standard Unix `getopt'
   but it behaves differently for the user, since it allows the user
   to intersperse the options with the other arguments.

   As `MR_getopt' works, it permutes the elements of ARGV so that,
   when it is done, all the options precede everything else.  Thus
   all application programs are extended to handle flexible argument order.

   Setting the environment variable POSIXLY_CORRECT disables permutation.
   Then the behavior is completely standard.

   GNU application programs can use a third alternative mode in which
   they can distinguish the relative order of options and other arguments.  */

#include "mercury_getopt.h"

/* For communication from `MR_getopt' to the caller.
   When `MR_getopt' finds an option that takes an argument,
   the argument value is returned here.
   Also, when `MR_ordering' is RETURN_IN_ORDER,
   each non-option ARGV-element is returned here.  */

char *MR_optarg = NULL;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `MR_getopt'.

   On entry to `MR_getopt', zero means this is the first call; initialize.

   When `MR_getopt' returns -1, this is the index of the first of the
   non-option elements that the caller should itself scan.

   Otherwise, `MR_optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

/* 1003.2 says this must be 1 before any call.  */
int MR_optind = 1;

/* Formerly, initialization of MR_getopt depended on MR_optind==0, which
   causes problems with re-calling MR_getopt as programs generally don't
   know that. */

int MR____getopt_initialized = 0;

/* The next char to be scanned in the MR_option-element
   in which the last option character we returned was found.
   This allows us to pick up the scan where we left off.

   If this is zero, or a null string, it means resume the scan
   by advancing to the next ARGV-element.  */

static char *MR_nextchar;

/* Callers store zero here to inhibit the error message
   for unrecognized options.  */

int MR_opterr = 1;

/* Set to an option character which was unrecognized.
   This must be initialized on some systems to avoid linking in the
   system's own MR_getopt implementation.  */

int MR_optopt = '?';

/* Describe how to deal with options that follow non-option ARGV-elements.

   If the caller did not specify anything,
   the default is REQUIRE_ORDER if the environment variable
   POSIXLY_CORRECT is defined, PERMUTE otherwise.

   REQUIRE_ORDER means don't recognize them as options;
   stop option processing when the first non-option is seen.
   This is what Unix does.
   This mode of operation is selected by either setting the environment
   variable POSIXLY_CORRECT, or using `+' as the first character
   of the list of option characters.

   PERMUTE is the default.  We permute the contents of ARGV as we scan,
   so that eventually all the non-options are at the end.  This allows options
   to be given in any order, even with programs that were not written to
   expect this.

   RETURN_IN_ORDER is an option available to programs that were written
   to expect options and other ARGV-elements in any order and that care about
   the ordering of the two.  We describe each non-option ARGV-element
   as if it were the argument of an option with character code 1.
   Using `-' as the first character of the list of option characters
   selects this mode of operation.

   The special argument `--' forces an end of option-scanning regardless
   of the value of `MR_ordering'.  In the case of RETURN_IN_ORDER, only
   `--' can cause `MR_getopt' to return -1 with `MR_optind' != ARGC.  */

static enum
{
  REQUIRE_ORDER, PERMUTE, RETURN_IN_ORDER
} MR_ordering;

/* Value of POSIXLY_CORRECT environment variable.  */
static char *MR_posixly_correct;

#ifdef	__GNU_LIBRARY__
/* We want to avoid inclusion of string.h with non-GNU libraries
   because there are many ways it can cause trouble.
   On some systems, it contains special magic macros that don't work
   in GCC.  */
#include <string.h>
#define	my_index	strchr
#else

/* Avoid depending on library functions or files
   whose names are inconsistent.  */

char *getenv ();

static char *
my_index (str, chr)
     const char *str;
     int chr;
{
  while (*str)
    {
      if (*str == chr)
	return (char *) str;
      str++;
    }
  return 0;
}

/* If using GCC, we can safely declare strlen this way.
   If not using GCC, it is ok not to declare it.  */
#ifdef __GNUC__
/* Note that Motorola Delta 68k R3V7 comes with GCC but not stddef.h.
   That was relevant to code that was here before.  */
#if !defined (__STDC__) || !__STDC__
/* gcc with -traditional declares the built-in strlen to return int,
   and has done so at least since version 2.4.5. -- rms.  */
extern int strlen (const char *);
#endif /* not __STDC__ */
#endif /* __GNUC__ */

#endif /* not __GNU_LIBRARY__ */

/* Handle permutation of arguments.  */

/* Describe the part of ARGV that contains non-options that have
   been skipped.  `MR_first_nonopt' is the index in ARGV of the first of them;
   `MR_last_nonopt' is the index after the last of them.  */

static int MR_first_nonopt;
static int MR_last_nonopt;

#ifdef _LIBC
/* Bash 2.0 gives us an environment variable containing flags
   indicating ARGV elements that should not be considered arguments.  */

/* Defined in MR_getopt_init.c  */
extern char *__getopt_nonoption_flags;

static int nonoption_flags_max_len;
static int nonoption_flags_len;

static int original_argc;
static char *const *original_argv;

extern pid_t __libc_pid;

/* Make sure the environment variable bash 2.0 puts in the environment
   is valid for the MR_getopt call we must make sure that the ARGV passed
   to MR_getopt is that one passed to the process.  */
static void
__attribute__ ((unused))
store_args_and_env (int argc, char *const *argv)
{
  /* XXX This is no good solution.  We should rather copy the args so
     that we can compare them later.  But we must not use malloc(3).  */
  original_argc = argc;
  original_argv = argv;
}
text_set_element (__libc_subinit, store_args_and_env);

# define SWAP_FLAGS(ch1, ch2) \
  if (nonoption_flags_len > 0)						      \
    {									      \
      char __tmp = __getopt_nonoption_flags[ch1];			      \
      __getopt_nonoption_flags[ch1] = __getopt_nonoption_flags[ch2];	      \
      __getopt_nonoption_flags[ch2] = __tmp;				      \
    }
#else	/* !_LIBC */
# define SWAP_FLAGS(ch1, ch2)
#endif	/* _LIBC */

/* Exchange two adjacent subsequences of ARGV.
   One subsequence is elements [MR_first_nonopt,MR_last_nonopt)
   which contains all the non-options that have been skipped so far.
   The other is elements [MR_last_nonopt,MR_optind), which contains all
   the options processed since those non-options were skipped.

   `MR_first_nonopt' and `MR_last_nonopt' are relocated so that they describe
   the new indices of the non-options in ARGV after they are moved.  */

#if defined (__STDC__) && __STDC__
static void exchange (char **);
#endif

static void
exchange (argv)
     char **argv;
{
  int bottom = MR_first_nonopt;
  int middle = MR_last_nonopt;
  int top = MR_optind;
  char *tem;

  /* Exchange the shorter segment with the far end of the longer segment.
     That puts the shorter segment into the right place.
     It leaves the longer segment in the right place overall,
     but it consists of two parts that need to be swapped next.  */

#ifdef _LIBC
  /* First make sure the handling of the `__getopt_nonoption_flags'
     string can work normally.  Our top argument must be in the range
     of the string.  */
  if (nonoption_flags_len > 0 && top >= nonoption_flags_max_len)
    {
      /* We must extend the array.  The user plays games with us and
	 presents new arguments.  */
      char *new_str = malloc (top + 1);
      if (new_str == NULL)
	nonoption_flags_len = nonoption_flags_max_len = 0;
      else
	{
	  memcpy (new_str, __getopt_nonoption_flags, nonoption_flags_max_len);
	  memset (&new_str[nonoption_flags_max_len], '\0',
		  top + 1 - nonoption_flags_max_len);
	  nonoption_flags_max_len = top + 1;
	  __getopt_nonoption_flags = new_str;
	}
    }
#endif

  while (top > middle && middle > bottom)
    {
      if (top - middle > middle - bottom)
	{
	  /* Bottom segment is the short one.  */
	  int len = middle - bottom;
	  register int i;

	  /* Swap it with the top part of the top segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[top - (middle - bottom) + i];
	      argv[top - (middle - bottom) + i] = tem;
	      SWAP_FLAGS (bottom + i, top - (middle - bottom) + i);
	    }
	  /* Exclude the moved bottom segment from further swapping.  */
	  top -= len;
	}
      else
	{
	  /* Top segment is the short one.  */
	  int len = top - middle;
	  register int i;

	  /* Swap it with the bottom part of the bottom segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[middle + i];
	      argv[middle + i] = tem;
	      SWAP_FLAGS (bottom + i, middle + i);
	    }
	  /* Exclude the moved top segment from further swapping.  */
	  bottom += len;
	}
    }

  /* Update records for the slots the non-options now occupy.  */

  MR_first_nonopt += (MR_optind - MR_last_nonopt);
  MR_last_nonopt = MR_optind;
}

/* Initialize the internal data when the first call is made.  */

#if defined (__STDC__) && __STDC__
static const char *MR__getopt_initialize (int, char *const *, const char *);
#endif
static const char *
MR__getopt_initialize (argc, argv, optstring)
     int argc;
     char *const *argv;
     const char *optstring;
{
  /* Start processing options with ARGV-element 1 (since ARGV-element 0
     is the program name); the sequence of previously skipped
     non-option ARGV-elements is empty.  */

  MR_first_nonopt = MR_last_nonopt = MR_optind;

  MR_nextchar = NULL;

  MR_posixly_correct = getenv ("POSIXLY_CORRECT");

  /* Determine how to handle the ordering of options and nonoptions.  */

  if (optstring[0] == '-')
    {
      MR_ordering = RETURN_IN_ORDER;
      ++optstring;
    }
  else if (optstring[0] == '+')
    {
      MR_ordering = REQUIRE_ORDER;
      ++optstring;
    }
  else if (MR_posixly_correct != NULL)
    MR_ordering = REQUIRE_ORDER;
  else
    MR_ordering = PERMUTE;

#ifdef _LIBC
  if (MR_posixly_correct == NULL
      && argc == original_argc && argv == original_argv)
    {
      if (nonoption_flags_max_len == 0)
	{
	  if (__getopt_nonoption_flags == NULL
	      || __getopt_nonoption_flags[0] == '\0')
	    nonoption_flags_max_len = -1;
	  else
	    {
	      const char *orig_str = __getopt_nonoption_flags;
	      int len = nonoption_flags_max_len = strlen (orig_str);
	      if (nonoption_flags_max_len < argc)
		nonoption_flags_max_len = argc;
	      __getopt_nonoption_flags =
		(char *) malloc (nonoption_flags_max_len);
	      if (__getopt_nonoption_flags == NULL)
		nonoption_flags_max_len = -1;
	      else
		{
		  memcpy (__getopt_nonoption_flags, orig_str, len);
		  memset (&__getopt_nonoption_flags[len], '\0',
			  nonoption_flags_max_len - len);
		}
	    }
	}
      nonoption_flags_len = nonoption_flags_max_len;
    }
  else
    nonoption_flags_len = 0;
#endif

  return optstring;
}

/* Scan elements of ARGV (whose length is ARGC) for option characters
   given in OPTSTRING.

   If an element of ARGV starts with '-', and is not exactly "-" or "--",
   then it is an option element.  The characters of this element
   (aside from the initial '-') are option characters.  If `MR_getopt'
   is called repeatedly, it returns successively each of the option characters
   from each of the option elements.

   If `MR_getopt' finds another option character, it returns that character,
   updating `MR_optind' and `MR_nextchar' so that the next call to `MR_getopt' can
   resume the scan with the following option character or ARGV-element.

   If there are no more option characters, `MR_getopt' returns -1.
   Then `MR_optind' is the index in ARGV of the first ARGV-element
   that is not an option.  (The ARGV-elements have been permuted
   so that those that are not MR_options now come last.)

   OPTSTRING is a string containing the legitimate option characters.
   If an option character is seen that is not listed in OPTSTRING,
   return '?' after printing an error message.  If you set `MR_opterr' to
   zero, the error message is suppressed but we still return '?'.

   If a char in OPTSTRING is followed by a colon, that means it wants an arg,
   so the following text in the same ARGV-element, or the text of the following
   ARGV-element, is returned in `MR_optarg'.  Two colons mean an option that
   wants an optional arg; if there is text in the current ARGV-element,
   it is returned in `MR_optarg', otherwise `MR_optarg' is set to zero.

   If OPTSTRING starts with `-' or `+', it requests different methods of
   handling the non-option ARGV-elements.
   See the comments about RETURN_IN_ORDER and REQUIRE_ORDER, above.

   Long-named options begin with `--' instead of `-'.
   Their names may be abbreviated as long as the abbreviation is unique
   or is an exact match for some defined option.  If they have an
   argument, it follows the option name in the same ARGV-element, separated
   from the option name by a `=', or else the in next ARGV-element.
   When `MR_getopt' finds a long-named option, it returns 0 if that option's
   `flag' field is nonzero, the value of the option's `val' field
   if the `flag' field is zero.

   The elements of ARGV aren't really const, because we permute them.
   But we pretend they're const in the prototype to be compatible
   with other systems.

   LONGOPTS is a vector of `struct MR_option' terminated by an
   element containing a name which is zero.

   LONGIND returns the index in LONGOPT of the long-named option found.
   It is only valid when a long-named option has been found by the most
   recent call.

   If LONG_ONLY is nonzero, '-' as well as '--' can introduce
   long-named options.  */

int
MR__getopt_internal (argc, argv, optstring, longopts, longind, long_only)
     int argc;
     char *const *argv;
     const char *optstring;
     const struct MR_option *longopts;
     int *longind;
     int long_only;
{
  MR_optarg = NULL;

  if (MR_optind == 0 || !MR____getopt_initialized)
    {
      if (MR_optind == 0)
	MR_optind = 1;	/* Don't scan ARGV[0], the program name.  */
      optstring = MR__getopt_initialize (argc, argv, optstring);
      MR____getopt_initialized = 1;
    }

  /* Test whether ARGV[MR_optind] points to a non-option argument.
     Either it does not have option syntax, or there is an environment flag
     from the shell indicating it is not an option.  The later information
     is only used when the used in the GNU libc.  */
#ifdef _LIBC
#define NONOPTION_P (argv[MR_optind][0] != '-' || argv[MR_optind][1] == '\0'	      \
		     || (MR_optind < nonoption_flags_len			      \
			 && __getopt_nonoption_flags[MR_optind] == '1'))
#else
#define NONOPTION_P (argv[MR_optind][0] != '-' || argv[MR_optind][1] == '\0')
#endif

  if (MR_nextchar == NULL || *MR_nextchar == '\0')
    {
      /* Advance to the next ARGV-element.  */

      /* Give FIRST_NONOPT & LAST_NONOPT rational values if OPTIND has been
	 moved back by the user (who may also have changed the arguments).  */
      if (MR_last_nonopt > MR_optind)
	MR_last_nonopt = MR_optind;
      if (MR_first_nonopt > MR_optind)
	MR_first_nonopt = MR_optind;

      if (MR_ordering == PERMUTE)
	{
	  /* If we have just processed some options following some non-options,
	     exchange them so that the options come first.  */

	  if (MR_first_nonopt != MR_last_nonopt && MR_last_nonopt != MR_optind)
	    exchange ((char **) argv);
	  else if (MR_last_nonopt != MR_optind)
	    MR_first_nonopt = MR_optind;

	  /* Skip any additional non-options
	     and extend the range of non-options previously skipped.  */

	  while (MR_optind < argc && NONOPTION_P)
	    MR_optind++;
	  MR_last_nonopt = MR_optind;
	}

      /* The special ARGV-element `--' means premature end of options.
	 Skip it like a null option,
	 then exchange with previous non-options as if it were an option,
	 then skip everything else like a non-option.  */

      if (MR_optind != argc && !strcmp (argv[MR_optind], "--"))
	{
	  MR_optind++;

	  if (MR_first_nonopt != MR_last_nonopt && MR_last_nonopt != MR_optind)
	    exchange ((char **) argv);
	  else if (MR_first_nonopt == MR_last_nonopt)
	    MR_first_nonopt = MR_optind;
	  MR_last_nonopt = argc;

	  MR_optind = argc;
	}

      /* If we have done all the ARGV-elements, stop the scan
	 and back over any non-options that we skipped and permuted.  */

      if (MR_optind == argc)
	{
	  /* Set the next-arg-index to point at the non-options
	     that we previously skipped, so the caller will digest them.  */
	  if (MR_first_nonopt != MR_last_nonopt)
	    MR_optind = MR_first_nonopt;
	  return -1;
	}

      /* If we have come to a non-option and did not permute it,
	 either stop the scan or describe it to the caller and pass it by.  */

      if (NONOPTION_P)
	{
	  if (MR_ordering == REQUIRE_ORDER)
	    return -1;
	  MR_optarg = argv[MR_optind++];
	  return 1;
	}

      /* We have found another option-ARGV-element.
	 Skip the initial punctuation.  */

      MR_nextchar = (argv[MR_optind] + 1
		  + (longopts != NULL && argv[MR_optind][1] == '-'));
    }

  /* Decode the current option-ARGV-element.  */

  /* Check whether the ARGV-element is a long option.

     If long_only and the ARGV-element has the form "-f", where f is
     a valid short option, don't consider it an abbreviated form of
     a long option that starts with f.  Otherwise there would be no
     way to give the -f short option.

     On the other hand, if there's a long option "fubar" and
     the ARGV-element is "-fu", do consider that an abbreviation of
     the long option, just like "--fu", and not "-f" with arg "u".

     This distinction seems to be the most useful approach.  */

  if (longopts != NULL
      && (argv[MR_optind][1] == '-'
	  || (long_only && (argv[MR_optind][2] || !my_index (optstring, argv[MR_optind][1])))))
    {
      char *nameend;
      const struct MR_option *p;
      const struct MR_option *pfound = NULL;
      int exact = 0;
      int ambig = 0;
      int indfound = -1;
      int MR_option_index;

      for (nameend = MR_nextchar; *nameend && *nameend != '='; nameend++)
	/* Do nothing.  */ ;

      /* Test all long options for either exact match
	 or abbreviated matches.  */
      for (p = longopts, MR_option_index = 0; p->name; p++, MR_option_index++)
	if (!strncmp (p->name, MR_nextchar, nameend - MR_nextchar))
	  {
	    if ((unsigned int) (nameend - MR_nextchar)
		== (unsigned int) strlen (p->name))
	      {
		/* Exact match found.  */
		pfound = p;
		indfound = MR_option_index;
		exact = 1;
		break;
	      }
	    else if (pfound == NULL)
	      {
		/* First nonexact match found.  */
		pfound = p;
		indfound = MR_option_index;
	      }
	    else
	      /* Second or later nonexact match found.  */
	      ambig = 1;
	  }

      if (ambig && !exact)
	{
	  if (MR_opterr)
	    fprintf (stderr, _("%s: option `%s' is ambiguous\n"),
		     argv[0], argv[MR_optind]);
	  MR_nextchar += strlen (MR_nextchar);
	  MR_optind++;
	  MR_optopt = 0;
	  return '?';
	}

      if (pfound != NULL)
	{
	  MR_option_index = indfound;
	  MR_optind++;
	  if (*nameend)
	    {
	      /* Don't test has_arg with >, because some C compilers don't
		 allow it to be used on enums.  */
	      if (pfound->has_arg)
		MR_optarg = nameend + 1;
	      else
		{
		  if (MR_opterr) {
		   if (argv[MR_optind - 1][1] == '-')
		    /* --option */
		    fprintf (stderr,
		     _("%s: option `--%s' doesn't allow an argument\n"),
		     argv[0], pfound->name);
		   else
		    /* +option or -option */
		    fprintf (stderr,
		     _("%s: option `%c%s' doesn't allow an argument\n"),
		     argv[0], argv[MR_optind - 1][0], pfound->name);
		  }

		  MR_nextchar += strlen (MR_nextchar);

		  MR_optopt = pfound->val;
		  return '?';
		}
	    }
	  else if (pfound->has_arg == 1)
	    {
	      if (MR_optind < argc)
		MR_optarg = argv[MR_optind++];
	      else
		{
		  if (MR_opterr)
		    fprintf (stderr,
			   _("%s: option `%s' requires an argument\n"),
			   argv[0], argv[MR_optind - 1]);
		  MR_nextchar += strlen (MR_nextchar);
		  MR_optopt = pfound->val;
		  return optstring[0] == ':' ? ':' : '?';
		}
	    }
	  MR_nextchar += strlen (MR_nextchar);
	  if (longind != NULL)
	    *longind = MR_option_index;
	  if (pfound->flag)
	    {
	      *(pfound->flag) = pfound->val;
	      return 0;
	    }
	  return pfound->val;
	}

      /* Can't find it as a long option.  If this is not MR_getopt_long_only,
	 or the option starts with '--' or is not a valid short
	 option, then it's an error.
	 Otherwise interpret it as a short option.  */
      if (!long_only || argv[MR_optind][1] == '-'
	  || my_index (optstring, *MR_nextchar) == NULL)
	{
	  if (MR_opterr)
	    {
	      if (argv[MR_optind][1] == '-')
		/* --option */
		fprintf (stderr, _("%s: unrecognized option `--%s'\n"),
			 argv[0], MR_nextchar);
	      else
		/* +option or -option */
		fprintf (stderr, _("%s: unrecognized option `%c%s'\n"),
			 argv[0], argv[MR_optind][0], MR_nextchar);
	    }
	  MR_nextchar = (char *) "";
	  MR_optind++;
	  MR_optopt = 0;
	  return '?';
	}
    }

  /* Look at and handle the next short option-character.  */

  {
    char c = *MR_nextchar++;
    char *temp = my_index (optstring, c);

    /* Increment `MR_optind' when we start to process its last character.  */
    if (*MR_nextchar == '\0')
      ++MR_optind;

    if (temp == NULL || c == ':')
      {
	if (MR_opterr)
	  {
	    if (MR_posixly_correct)
	      /* 1003.2 specifies the format of this message.  */
	      fprintf (stderr, _("%s: illegal option -- %c\n"),
		       argv[0], c);
	    else
	      fprintf (stderr, _("%s: invalid option -- %c\n"),
		       argv[0], c);
	  }
	MR_optopt = c;
	return '?';
      }
    /* Convenience. Treat POSIX -W foo same as long MR_option --foo */
    if (temp[0] == 'W' && temp[1] == ';')
      {
	char *nameend;
	const struct MR_option *p;
	const struct MR_option *pfound = NULL;
	int exact = 0;
	int ambig = 0;
	int indfound = 0;
	int MR_option_index;

	/* This is an MR_option that requires an argument.  */
	if (*MR_nextchar != '\0')
	  {
	    MR_optarg = MR_nextchar;
	    /* If we end this ARGV-element by taking the rest as an arg,
	       we must advance to the next element now.  */
	    MR_optind++;
	  }
	else if (MR_optind == argc)
	  {
	    if (MR_opterr)
	      {
		/* 1003.2 specifies the format of this message.  */
		fprintf (stderr, _("%s: option requires an argument -- %c\n"),
			 argv[0], c);
	      }
	    MR_optopt = c;
	    if (optstring[0] == ':')
	      c = ':';
	    else
	      c = '?';
	    return c;
	  }
	else
	  /* We already incremented `MR_optind' once;
	     increment it again when taking next ARGV-elt as argument.  */
	  MR_optarg = argv[MR_optind++];

	/* MR_optarg is now the argument, see if it's in the
	   table of longopts.  */

	for (MR_nextchar = nameend = MR_optarg; *nameend && *nameend != '='; nameend++)
	  /* Do nothing.  */ ;

	/* Test all long options for either exact match
	   or abbreviated matches.  */
	for (p = longopts, MR_option_index = 0; p->name; p++, MR_option_index++)
	  if (!strncmp (p->name, MR_nextchar, nameend - MR_nextchar))
	    {
	      if ((unsigned int) (nameend - MR_nextchar) == strlen (p->name))
		{
		  /* Exact match found.  */
		  pfound = p;
		  indfound = MR_option_index;
		  exact = 1;
		  break;
		}
	      else if (pfound == NULL)
		{
		  /* First nonexact match found.  */
		  pfound = p;
		  indfound = MR_option_index;
		}
	      else
		/* Second or later nonexact match found.  */
		ambig = 1;
	    }
	if (ambig && !exact)
	  {
	    if (MR_opterr)
	      fprintf (stderr, _("%s: option `-W %s' is ambiguous\n"),
		       argv[0], argv[MR_optind]);
	    MR_nextchar += strlen (MR_nextchar);
	    MR_optind++;
	    return '?';
	  }
	if (pfound != NULL)
	  {
	    MR_option_index = indfound;
	    if (*nameend)
	      {
		/* Don't test has_arg with >, because some C compilers don't
		   allow it to be used on enums.  */
		if (pfound->has_arg)
		  MR_optarg = nameend + 1;
		else
		  {
		    if (MR_opterr)
		      fprintf (stderr, _("\
%s: option `-W %s' doesn't allow an argument\n"),
			       argv[0], pfound->name);

		    MR_nextchar += strlen (MR_nextchar);
		    return '?';
		  }
	      }
	    else if (pfound->has_arg == 1)
	      {
		if (MR_optind < argc)
		  MR_optarg = argv[MR_optind++];
		else
		  {
		    if (MR_opterr)
		      fprintf (stderr,
			       _("%s: option `%s' requires an argument\n"),
			       argv[0], argv[MR_optind - 1]);
		    MR_nextchar += strlen (MR_nextchar);
		    return optstring[0] == ':' ? ':' : '?';
		  }
	      }
	    MR_nextchar += strlen (MR_nextchar);
	    if (longind != NULL)
	      *longind = MR_option_index;
	    if (pfound->flag)
	      {
		*(pfound->flag) = pfound->val;
		return 0;
	      }
	    return pfound->val;
	  }
	  MR_nextchar = NULL;
	  return 'W';	/* Let the application handle it.   */
      }
    if (temp[1] == ':')
      {
	if (temp[2] == ':')
	  {
	    /* This is an option that accepts an argument optionally.  */
	    if (*MR_nextchar != '\0')
	      {
		MR_optarg = MR_nextchar;
		MR_optind++;
	      }
	    else
	      MR_optarg = NULL;
	    MR_nextchar = NULL;
	  }
	else
	  {
	    /* This is an option that requires an argument.  */
	    if (*MR_nextchar != '\0')
	      {
		MR_optarg = MR_nextchar;
		/* If we end this ARGV-element by taking the rest as an arg,
		   we must advance to the next element now.  */
		MR_optind++;
	      }
	    else if (MR_optind == argc)
	      {
		if (MR_opterr)
		  {
		    /* 1003.2 specifies the format of this message.  */
		    fprintf (stderr,
			   _("%s: option requires an argument -- %c\n"),
			   argv[0], c);
		  }
		MR_optopt = c;
		if (optstring[0] == ':')
		  c = ':';
		else
		  c = '?';
	      }
	    else
	      /* We already incremented `MR_optind' once;
		 increment it again when taking next ARGV-elt as argument.  */
	      MR_optarg = argv[MR_optind++];
	    MR_nextchar = NULL;
	  }
      }
    return c;
  }
}

int
MR_getopt (argc, argv, optstring)
     int argc;
     char *const *argv;
     const char *optstring;
{
  return MR__getopt_internal (argc, argv, optstring,
			   (const struct MR_option *) 0,
			   (int *) 0,
			   0);
}

#endif	/* Not ELIDE_CODE.  */

#ifdef TEST

/* Compile with -DTEST to make an executable for use in testing
   the above definition of `MR_getopt'.  */

int
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int digit_optind = 0;

  while (1)
    {
      int this_option_optind = MR_optind ? MR_optind : 1;

      c = MR_getopt (argc, argv, "abc:d:0123456789");
      if (c == -1)
	break;

      switch (c)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  if (digit_optind != 0 && digit_optind != this_option_optind)
	    printf ("digits occur in two different argv-elements.\n");
	  digit_optind = this_option_optind;
	  printf ("MR_option %c\n", c);
	  break;

	case 'a':
	  printf ("MR_option a\n");
	  break;

	case 'b':
	  printf ("MR_option b\n");
	  break;

	case 'c':
	  printf ("MR_option c with value `%s'\n", MR_optarg);
	  break;

	case '?':
	  break;

	default:
	  printf ("?? MR_getopt returned character code 0%o ??\n", c);
	}
    }

  if (MR_optind < argc)
    {
      printf ("non-MR_option ARGV-elements: ");
      while (MR_optind < argc)
	printf ("%s ", argv[MR_optind++]);
      printf ("\n");
    }

  exit (0);
}

#endif /* TEST */
