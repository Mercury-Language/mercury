;;; gud.el --- Grand Unified Debugger mode for gdb, sdb, dbx, or xdb under Emacs
;; Modified to also support Mercury debugging using mdb.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: FSF
;; Keywords: unix, tools

;; Copyright (C) 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
;; Copyright (C) 1998-2000 Fergus Henderson.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The ancestral gdb.el was by W. Schelter <wfs@rascal.ics.utexas.edu>
;; It was later rewritten by rms.  Some ideas were due to Masanobu. 
;; Grand Unification (sdb/dbx support) by Eric S. Raymond <esr@thyrsus.com>
;; The overloading code was then rewritten by Barry Warsaw <bwarsaw@cen.com>,
;; who also hacked the mode to use comint.el.  Shane Hartman <shane@spr.com>
;; added support for xdb (HPUX debugger).  Rick Sladkey <jrs@world.std.com>
;; wrote the GDB command completion code.  Dave Love <d.love@dl.ac.uk>
;; added the IRIX kluge, re-implemented the Mips-ish variant and added
;; a menu. Brian D. Carlstrom <bdc@ai.mit.edu> combined the IRIX kluge with 
;; the gud-xdb-directories hack producing gud-dbx-directories.
;; Fergus Henderson <fjh@cs.mu.oz.au> added support for mdb
;; (the Mercury debugger).

;;; Code:

(require 'comint)
(require 'etags)

;; ======================================================================
;; GUD commands must be visible in C buffers visited by GUD

(defvar gud-key-prefix "\C-x\C-a"
  "Prefix of all GUD commands valid in C buffers.")

(global-set-key (concat gud-key-prefix "\C-l") 'gud-refresh)
(define-key ctl-x-map " " 'gud-break)	;; backward compatibility hack

(defvar gud-marker-filter nil)
(put 'gud-marker-filter 'permanent-local t)
(defvar gud-find-file nil)
(put 'gud-find-file 'permanent-local t)

(defvar gud-show-commands nil
  "Non-nil to show the debugger commands.")

(defvar gud-redirect-io nil
  "Non-nil if the debugger supports I/O redirection via `--tty FILENAME'.")

(defun gud-marker-filter (&rest args)
  (apply gud-marker-filter args))

(defun gud-find-file (file)
  ;;; The commented out code below is a bad idea, because
  ;;; (1) double-slashes are meaningful on some systems, e.g. cygwin
  ;;; (2) the argument FILE might not be a string
  ;
  ;; Don't get confused by double slashes in the name that comes from GDB.
  ;  (while (string-match "//+" file)
  ;  (setq file (replace-match "/" t t file)))
  ;
  (funcall gud-find-file file))

(defvar gud-menu-name "Gud"
  "Name of the top-level menu entry for the debugger.")

(defvar gud-buffer-name-prefix "gud"
  "Prefix used for the name of debugger buffers.")

;; Keymap definitions for menu bar entries common to all debuggers and
;; slots for debugger-dependent ones in sensible places.  (Defined here
;; before use.)
(defvar gud-menu-map (make-sparse-keymap gud-menu-name) nil)
(define-key gud-menu-map [refresh] '("Refresh" . gud-refresh))
(define-key gud-menu-map [remove] '("Remove Breakpoint" . gud-remove))
(define-key gud-menu-map [tbreak] nil)	; gdb, sdb and xdb
(define-key gud-menu-map [break] '("Set Breakpoint" . gud-break))
(define-key gud-menu-map [up] nil)	; gdb, dbx, and xdb
(define-key gud-menu-map [down] nil)	; gdb, dbx, and xdb
(define-key gud-menu-map [print] '("Print Expression" . gud-print))
(define-key gud-menu-map [finish] nil)	; gdb or xdb
(define-key gud-menu-map [stepi] '("Step Instruction" . gud-stepi))
(define-key gud-menu-map [step] '("Step Line" . gud-step))
(define-key gud-menu-map [next] '("Next Line" . gud-next))
(define-key gud-menu-map [cont] '("Continue" . gud-cont))

;; ======================================================================
;; command definition

;; This macro is used below to define some basic debugger interface commands.
;; Of course you may use `gud-def' with any other debugger command, including
;; user defined ones.

;; A macro call like (gud-def FUNC NAME KEY DOC) expands to a form
;; which defines FUNC to send the command NAME to the debugger, gives
;; it the docstring DOC, and binds that function to KEY in the GUD
;; major mode.  The function is also bound in the global keymap with the
;; GUD prefix.

(defmacro gud-def (func cmd key &optional doc)
  "Define FUNC to be a command sending STR and bound to KEY, with
optional doc string DOC.  Certain %-escapes in the string arguments
are interpreted specially if present.  These are:

  %f	name (without directory) of current source file. 
  %b	base name (without directory or extension) of current source file. 
  %d	directory of current source file. 
  %l	number of current source line
  %e	text of the C lvalue or function-call expression surrounding point.
  %i	identifier (i.e. [a-zA-Z0-9_:/-]*) surrounding point.
  %a	text of the hexadecimal address surrounding point
  %p	prefix argument to the command (if any) as a number

  The `current' source file is the file of the current buffer (if
we're in a C file) or the source file current at the last break or
step (if we're in the GUD buffer).
  The `current' line is that of the current buffer (if we're in a
source file) or the source line number at the last break or step (if
we're in the GUD buffer)."
  (list 'progn
	(list 'defun func '(arg)
	      (or doc "")
	      '(interactive "p")
	      (list 'gud-call cmd 'arg))
	(if key
	    (list 'define-key
		  '(current-local-map)
		  (concat "\C-c" key)
		  (list 'quote func)))
	(if key
	    (list 'global-set-key
		  (list 'concat 'gud-key-prefix key)
		  (list 'quote func)))))

;; Where gud-display-frame should put the debugging arrow.  This is
;; set by the marker-filter, which scans the debugger's output for
;; indications of the current program counter.
(defvar gud-last-frame nil)

;; Used by gud-refresh, which should cause gud-display-frame to redisplay
;; the last frame, even if it's been called before and gud-last-frame has
;; been set to nil.
(defvar gud-last-last-frame nil)

;; All debugger-specific information is collected here.
;; Here's how it works, in case you ever need to add a debugger to the mode.
;;
;; Each entry must define the following at startup:
;;
;;<name>
;; comint-prompt-regexp
;; gud-<name>-massage-args
;; gud-<name>-marker-filter
;; gud-<name>-find-file
;;
;; The job of the massage-args method is to modify the given list of
;; debugger arguments before running the debugger.
;;
;; The job of the marker-filter method is to detect file/line markers in
;; strings and set the global gud-last-frame to indicate what display
;; action (if any) should be triggered by the marker.  Note that only
;; whatever the method *returns* is displayed in the buffer; thus, you
;; can filter the debugger's output, interpreting some and passing on
;; the rest.
;;
;; The job of the find-file method is to visit and return the buffer indicated
;; by the car of gud-last-frame.  This may be a file name, a tag name, or
;; something else.  It would be good if it also copied the Gud menubar entry.

;; ======================================================================
;; gdb functions

;;; History of argument lists passed to gdb.
(defvar gud-gdb-history nil)

(defun gud-gdb-massage-args (file args)
  (cons "-fullname" args))

(defvar gud-gdb-marker-regexp
  (concat "\032\032\\([^" path-separator "\n]*\\)" path-separator
	  "\\([0-9]*\\)" path-separator ".*\n"))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar gud-marker-acc "")
(make-variable-buffer-local 'gud-marker-acc)

(defun gud-gdb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match gud-gdb-marker-regexp gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-marker-acc (match-beginning 1) (match-end 1))
	     (string-to-number (substring gud-marker-acc
				       (match-beginning 2)
				       (match-end 2))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" gud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring gud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq gud-marker-acc
		(substring gud-marker-acc (match-beginning 0))))

      (setq output (concat output gud-marker-acc)
	    gud-marker-acc ""))

    output))

(defun gud-new-keymap (map)
  "Return a new keymap which inherits from MAP and has name `Gud'."
  (nconc (make-sparse-keymap gud-menu-name) map))

(defun gud-make-debug-menu ()
  "Make sure the current local map has a [menu-bar debug] submap.
If it doesn't, replace it with a new map that inherits it,
and create such a submap in that new map."
  (if (and (current-local-map)
	   (lookup-key (current-local-map) [menu-bar debug]))
      nil
    (use-local-map (gud-new-keymap (current-local-map)))
    (define-key (current-local-map) [menu-bar debug]
      (cons gud-menu-name (gud-new-keymap gud-menu-map)))))

(defun gud-gdb-find-file (f)
  (save-excursion
    (let ((buf (find-file-noselect f)))
      (set-buffer buf)
      (gud-make-debug-menu)
      (local-set-key [menu-bar debug tbreak]
		     '("Temporary Breakpoint" . gud-tbreak))
      (local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
      (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
      (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
      buf)))

(defvar gdb-minibuffer-local-map nil
  "Keymap for minibuffer prompting of gdb startup command.")
(if gdb-minibuffer-local-map
    ()
  (setq gdb-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    gdb-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))

;;;###autoload
(defun gdb (command-line)
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run gdb (like this): "
			       (if (consp gud-gdb-history)
				   (car gud-gdb-history)
				 "gdb ")
			       gdb-minibuffer-local-map nil
			       '(gud-gdb-history . 1))))

  (gud-common-init command-line 'gud-gdb-massage-args
		   'gud-gdb-marker-filter 'gud-gdb-find-file)

  (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-remove "clear %f:%l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step %p"      "\C-s" "Step one source line with display.")
  (gud-def gud-stepi  "stepi %p"     "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "next %p"      "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "cont"         "\C-r" "Continue with display.")
  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
  (gud-def gud-up     "up %p"        "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"     "\C-p" "Evaluate C expression at point.")

  (local-set-key "\C-i" 'gud-gdb-complete-command)
  (local-set-key [menu-bar debug tbreak] '("Temporary Breakpoint" . gud-tbreak))
  (local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
  (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  (setq comint-prompt-regexp "^(.*gdb[+]?) *")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'gdb-mode-hook)
  )

;; One of the nice features of GDB is its impressive support for
;; context-sensitive command completion.  We preserve that feature
;; in the GUD buffer by using a GDB command designed just for Emacs.

;; The completion process filter indicates when it is finished.
(defvar gud-gdb-complete-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar gud-gdb-complete-string)

;; We need to know how much of the completion to chop off.
(defvar gud-gdb-complete-break)

;; The completion list is constructed by the process filter.
(defvar gud-gdb-complete-list)

;; The buffer for the debugger
(defvar gud-comint-buffer nil)

;; The buffer for the program being debugged
;; (if different from gud-comint-buffer)
(defvar gud-prog-buffer nil)

(defun gud-gdb-complete-command ()
  "Perform completion on the GDB command preceding point.
This is implemented using the GDB `complete' command which isn't
available with older versions of GDB."
  (interactive)
  (let* ((end (point))
	 (command (save-excursion
		    (beginning-of-line)
		    (and (looking-at comint-prompt-regexp)
			 (goto-char (match-end 0)))
		    (buffer-substring (point) end)))
	 command-word)
    ;; Find the word break.  This match will always succeed.
    (string-match "\\(\\`\\| \\)\\([^ ]*\\)\\'" command)
    (setq gud-gdb-complete-break (match-beginning 2)
	  command-word (substring command gud-gdb-complete-break))
    ;; Temporarily install our filter function.
    (let ((gud-marker-filter 'gud-gdb-complete-filter))
      ;; Issue the command to GDB.
      (gud-basic-call (concat "complete " command))
      (setq gud-gdb-complete-in-progress t
	    gud-gdb-complete-string nil
	    gud-gdb-complete-list nil)
      ;; Slurp the output.
      (while gud-gdb-complete-in-progress
	(accept-process-output (get-buffer-process gud-comint-buffer))))
    ;; Protect against old versions of GDB.
    (and gud-gdb-complete-list
	 (string-match "^Undefined command: \"complete\""
		       (car gud-gdb-complete-list))
	 (error "This version of GDB doesn't support the `complete' command."))
    ;; Sort the list like readline.
    (setq gud-gdb-complete-list
	  (sort gud-gdb-complete-list (function string-lessp)))
    ;; Remove duplicates.
    (let ((first gud-gdb-complete-list)
	  (second (cdr gud-gdb-complete-list)))
      (while second
	(if (string-equal (car first) (car second))
	    (setcdr first (setq second (cdr second)))
	  (setq first second
		second (cdr second)))))
    ;; Add a trailing single quote if there is a unique completion
    ;; and it contains an odd number of unquoted single quotes.
    (and (= (length gud-gdb-complete-list) 1)
	 (let ((str (car gud-gdb-complete-list))
	       (pos 0)
	       (count 0))
	   (while (string-match "\\([^'\\]\\|\\\\'\\)*'" str pos)
	     (setq count (1+ count)
		   pos (match-end 0)))
	   (and (= (mod count 2) 1)
		(setq gud-gdb-complete-list (list (concat str "'"))))))
    ;; Let comint handle the rest.
    (comint-dynamic-simple-complete command-word gud-gdb-complete-list)))
    
;; The completion process filter is installed temporarily to slurp the
;; output of GDB up to the next prompt and build the completion list.
(defun gud-gdb-complete-filter (string)
  (setq string (concat gud-gdb-complete-string string))
  (while (string-match "\n" string)
    (setq gud-gdb-complete-list
	  (cons (substring string gud-gdb-complete-break (match-beginning 0))
		gud-gdb-complete-list))
    (setq string (substring string (match-end 0))))
  (if (string-match comint-prompt-regexp string)
      (progn
	(setq gud-gdb-complete-in-progress nil)
	string)
    (progn
      (setq gud-gdb-complete-string string)
      "")))


;; ======================================================================
;; sdb functions

;;; History of argument lists passed to sdb.
(defvar gud-sdb-history nil)

(defvar gud-sdb-needs-tags (not (file-exists-p "/var"))
  "If nil, we're on a System V Release 4 and don't need the tags hack.")

(defvar gud-sdb-lastfile nil)

(defun gud-sdb-massage-args (file args) args)

(defun gud-sdb-marker-filter (string)
  (setq gud-marker-acc
	(if gud-marker-acc (concat gud-marker-acc string) string))
  (let (start)
    ;; Process all complete markers in this chunk
    (while 
	(cond 
	 ;; System V Release 3.2 uses this format
	 ((string-match "\\(^\\|\n\\)\\*?\\(0x\\w* in \\)?\\([^:\n]*\\):\\([0-9]*\\):.*\n"
			gud-marker-acc start)
	  (setq gud-last-frame
		(cons
		 (substring gud-marker-acc (match-beginning 3) (match-end 3))
		 (string-to-number 
		  (substring gud-marker-acc (match-beginning 4) (match-end 4))))))
	 ;; System V Release 4.0 quite often clumps two lines together
	 ((string-match "^\\(BREAKPOINT\\|STEPPED\\) process [0-9]+ function [^ ]+ in \\(.+\\)\n\\([0-9]+\\):" 
			gud-marker-acc start)
	  (setq gud-sdb-lastfile
		(substring gud-marker-acc (match-beginning 2) (match-end 2)))
	  (setq gud-last-frame
		(cons
		 gud-sdb-lastfile
		 (string-to-number 
		  (substring gud-marker-acc (match-beginning 3) (match-end 3))))))
	 ;; System V Release 4.0 
	 ((string-match "^\\(BREAKPOINT\\|STEPPED\\) process [0-9]+ function [^ ]+ in \\(.+\\)\n"
			gud-marker-acc start)
	  (setq gud-sdb-lastfile
		(substring gud-marker-acc (match-beginning 2) (match-end 2))))
	 ((and gud-sdb-lastfile (string-match "^\\([0-9]+\\):"
					      gud-marker-acc start))
	       (setq gud-last-frame
		     (cons
		      gud-sdb-lastfile
		      (string-to-number 
		       (substring gud-marker-acc (match-beginning 1) (match-end 1))))))
	 (t 
	  (setq gud-sdb-lastfile nil)))
      (setq start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" gud-marker-acc start)
      (setq start (match-end 0)))

    ;; If we have an incomplete line, store it in gud-marker-acc.
    (setq gud-marker-acc (substring gud-marker-acc (or start 0))))
  string)

(defun gud-sdb-find-file (f)
  (save-excursion
    (let ((buf (if gud-sdb-needs-tags
		   (find-tag-noselect f)
		 (find-file-noselect f))))
      (set-buffer buf)
      (gud-make-debug-menu)
      (local-set-key [menu-bar debug tbreak] '("Temporary Breakpoint" . gud-tbreak))
      buf)))

;;;###autoload
(defun sdb (command-line)
  "Run sdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run sdb (like this): "
			       (if (consp gud-sdb-history)
				   (car gud-sdb-history)
				 "sdb ")
			       nil nil
			       '(gud-sdb-history . 1))))
  (if (and gud-sdb-needs-tags
	   (not (and (boundp 'tags-file-name)
		     (stringp tags-file-name)
		     (file-exists-p tags-file-name))))
      (error "The sdb support requires a valid tags table to work."))

  (gud-common-init command-line 'gud-sdb-massage-args
		   'gud-sdb-marker-filter 'gud-sdb-find-file)

  (gud-def gud-break  "%l b" "\C-b"   "Set breakpoint at current line.")
  (gud-def gud-tbreak "%l c" "\C-t"   "Set temporary breakpoint at current line.")
  (gud-def gud-remove "%l d" "\C-d"   "Remove breakpoint at current line")
  (gud-def gud-step   "s %p" "\C-s"   "Step one source line with display.")
  (gud-def gud-stepi  "i %p" "\C-i"   "Step one instruction with display.")
  (gud-def gud-next   "S %p" "\C-n"   "Step one line (skip functions).")
  (gud-def gud-cont   "c"    "\C-r"   "Continue with display.")
  (gud-def gud-print  "%e/"  "\C-p"   "Evaluate C expression at point.")

  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (setq paragraph-start comint-prompt-regexp)
  (local-set-key [menu-bar debug tbreak]
    '("Temporary Breakpoint" . gud-tbreak))
  (run-hooks 'sdb-mode-hook)
  )

;; ======================================================================
;; dbx functions

;;; History of argument lists passed to dbx.
(defvar gud-dbx-history nil)

(defvar gud-dbx-directories nil
  "*A list of directories that dbx should search for source code.
If nil, only source files in the program directory
will be known to dbx.

The file names should be absolute, or relative to the directory
containing the executable being debugged.")

(defun gud-dbx-massage-args (file args)
  (nconc (let ((directories gud-dbx-directories)
	       (result nil))
	   (while directories
	     (setq result (cons (car directories) (cons "-I" result)))
	     (setq directories (cdr directories)))
	   (nreverse result))
	 args))

(defun gud-dbx-file-name (f)
  "Transform a relative file name to an absolute file name, for dbx."
  (let ((result nil))
    (if (file-exists-p f)
        (setq result (expand-file-name f))
      (let ((directories gud-dbx-directories))
        (while directories
          (let ((path (concat (car directories) "/" f)))
            (if (file-exists-p path)
                (setq result (expand-file-name path)
                      directories nil)))
          (setq directories (cdr directories)))))
    result))

(defun gud-dbx-marker-filter (string)
  (setq gud-marker-acc (if gud-marker-acc (concat gud-marker-acc string) string))

  (let (start)
    ;; Process all complete markers in this chunk.
    (while (or (string-match
		"stopped in .* at line \\([0-9]*\\) in file \"\\([^\"]*\\)\""
		gud-marker-acc start)
	       (string-match
		"signal .* in .* at line \\([0-9]*\\) in file \"\\([^\"]*\\)\""
		gud-marker-acc start))
      (setq gud-last-frame
	    (cons
	     (substring gud-marker-acc (match-beginning 2) (match-end 2))
	     (string-to-number 
	      (substring gud-marker-acc (match-beginning 1) (match-end 1))))
	    start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" gud-marker-acc start)
      (setq start (match-end 0)))

    ;; If the incomplete line APPEARS to begin with another marker, keep it
    ;; in the accumulator.  Otherwise, clear the accumulator to avoid an
    ;; unnecessary concat during the next call.
    (setq gud-marker-acc 
	  (if (string-match "\\(stopped\\|signal\\)" gud-marker-acc start)
	      (substring gud-marker-acc (match-beginning 0))
	    nil)))
  string)

;; Functions for Mips-style dbx.  Given the option `-emacs', documented in
;; OSF1, not necessarily elsewhere, it produces markers similar to gdb's.
(defvar gud-mips-p
  (or (string-match "^mips-[^-]*-ultrix" system-configuration)
      ;; We haven't tested gud on this system:
      (string-match "^mips-[^-]*-riscos" system-configuration)
      ;; It's documented on OSF/1.3
      (string-match "^mips-[^-]*-osf1" system-configuration)
      (string-match "^alpha-[^-]*-osf" system-configuration))
  "Non-nil to assume the MIPS/OSF dbx conventions (argument `-emacs').")

(defun gud-mipsdbx-massage-args (file args)
  (cons "-emacs" args))

;; This is just like the gdb one except for the regexps since we need to cope
;; with an optional breakpoint number in [] before the ^Z^Z
(defun gud-mipsdbx-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match
	    ;; This is like th gdb marker but with an optional
	    ;; leading break point number like `[1] '
	    "[][ 0-9]*\032\032\\([^:\n]*\\):\\([0-9]*\\):.*\n"
	    gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-marker-acc (match-beginning 1) (match-end 1))
	     (string-to-number (substring gud-marker-acc
				       (match-beginning 2)
				       (match-end 2))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "[][ 0-9]*\032.*\\'" gud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring gud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq gud-marker-acc
		(substring gud-marker-acc (match-beginning 0))))

      (setq output (concat output gud-marker-acc)
	    gud-marker-acc ""))

    output))

;; The dbx in IRIX is a pain.  It doesn't print the file name when
;; stopping at a breakpoint (but you do get it from the `up' and
;; `down' commands...).  The only way to extract the information seems
;; to be with a `file' command, although the current line number is
;; available in $curline.  Thus we have to look for output which
;; appears to indicate a breakpoint.  Then we prod the dbx sub-process
;; to output the information we want with a combination of the
;; `printf' and `file' commands as a pseudo marker which we can
;; recognise next time through the marker-filter.  This would be like
;; the gdb marker but you can't get the file name without a newline...
;; Note that gud-remove won't work since Irix dbx expects a breakpoint
;; number rather than a line number etc.  Maybe this could be made to
;; work by listing all the breakpoints and picking the one(s) with the
;; correct line number, but life's too short.
;;   d.love@dl.ac.uk (Dave Love) can be blamed for this

(defvar gud-irix-p
  (and (string-match "^mips-[^-]*-irix" system-configuration)
       (not (string-match "irix[6-9]\\.[1-9]" system-configuration)))
  "Non-nil to assume the interface appropriate for IRIX dbx.
This works in IRIX 4, 5 and 6, but `gud-dbx-use-stopformat-p' provides
a better solution in 6.1 upwards.")
(defvar gud-dbx-use-stopformat-p
  (string-match "irix[6-9]\\.[1-9]" system-configuration)
  "Non-nil to use the dbx feature present at least from Irix 6.1
  whereby $stopformat=1 produces an output format compatiable with
  `gud-dbx-marker-filter'.")
;; [Irix dbx seems to be a moving target.  The dbx output changed
;; subtly sometime between OS v4.0.5 and v5.2 so that, for instance,
;; the output from `up' is no longer spotted by gud (and it's probably
;; not distinctive enough to try to match it -- use C-<, C->
;; exclusively) .  For 5.3 and 6.0, the $curline variable changed to
;; `long long'(why?!), so the printf stuff needed changing.  The line
;; number was cast to `long' as a compromise between the new `long
;; long' and the original `int'.  This is reported not to work in 6.2,
;; so it's changed back to int -- don't make your sources too long.
;; From Irix6.1 (but not 6.0?) dbx supports an undocumented feature
;; whereby `set $stopformat=1' reportedly produces output compatible
;; with `gud-dbx-marker-filter', which we prefer.

;; The process filter is also somewhat
;; unreliable, sometimes not spotting the markers; I don't know
;; whether there's anything that can be done about that.  It would be
;; much better if SGI could be persuaded to (re?)instate the MIPS
;; -emacs flag for gdb-like output (which ought to be possible as most
;; of the communication I've had over it has been from sgi.com).]

;; this filter is influenced by the xdb one rather than the gdb one
(defun gud-irixdbx-marker-filter (string)
  (let (result (case-fold-search nil))
    (if (or (string-match comint-prompt-regexp string)
	    (string-match ".*\012" string))
	(setq result (concat gud-marker-acc string)
	      gud-marker-acc "")
      (setq gud-marker-acc (concat gud-marker-acc string)))
    (if result
	(cond
	 ;; look for breakpoint or signal indication e.g.:
	 ;; [2] Process  1267 (pplot) stopped at [params:338 ,0x400ec0]
	 ;; Process  1281 (pplot) stopped at [params:339 ,0x400ec8]
	 ;; Process  1270 (pplot) Floating point exception [._read._read:16 ,0x452188]
	 ((string-match
	   "^\\(\\[[0-9]+] \\)?Process +[0-9]+ ([^)]*) [^[]+\\[[^]\n]*]\n" 
	   result)
	  ;; prod dbx into printing out the line number and file
	  ;; name in a form we can grok as below
	  (process-send-string (get-buffer-process gud-comint-buffer)
			       "printf \"\032\032%1d:\",(int)$curline;file\n"))
	 ;; look for result of, say, "up" e.g.:
	 ;; .pplot.pplot(0x800) ["src/pplot.f":261, 0x400c7c]
	 ;; (this will also catch one of the lines printed by "where")
	 ((string-match
	   "^[^ ][^[]*\\[\"\\([^\"]+\\)\":\\([0-9]+\\), [^]]+]\n"
	   result)
	  (let ((file (substring result (match-beginning 1)
				 (match-end 1))))
	    (if (file-exists-p file)
		(setq gud-last-frame
		      (cons
		       (substring
			result (match-beginning 1) (match-end 1))
		       (string-to-number 
			(substring
			 result (match-beginning 2) (match-end 2)))))))
	  result)
	 ((string-match			; kluged-up marker as above
	   "\032\032\\([0-9]*\\):\\(.*\\)\n" result)
	  (let ((file (gud-dbx-file-name
		       (substring result (match-beginning 2) (match-end 2)))))
	    (if (and file (file-exists-p file))
		(setq gud-last-frame
		      (cons
		       file
		       (string-to-number 
			(substring
			 result (match-beginning 1) (match-end 1)))))))
	  (setq result (substring result 0 (match-beginning 0))))))
    (or result "")))

(defun gud-dbx-find-file (f)
  (save-excursion
    (let ((realf (gud-dbx-file-name f)))
      (if realf
	  (let ((buf (find-file-noselect realf)))
	    (set-buffer buf)
	    (gud-make-debug-menu)
	    (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
	    (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
	    buf)
	nil))))

;;;###autoload
(defun dbx (command-line)
  "Run dbx on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run dbx (like this): "
			       (if (consp gud-dbx-history)
				   (car gud-dbx-history)
				 "dbx ")
			       nil nil
			       '(gud-dbx-history . 1))))

  (cond
   (gud-mips-p
    (gud-common-init command-line 'gud-mipsdbx-massage-args
		     'gud-mipsdbx-marker-filter 'gud-dbx-find-file))
   (gud-irix-p
    (gud-common-init command-line 'gud-dbx-massage-args
		     'gud-irixdbx-marker-filter 'gud-dbx-find-file))
   (t
    (gud-common-init command-line 'gud-dbx-massage-args
		     'gud-dbx-marker-filter 'gud-dbx-find-file)))

  (cond
   (gud-mips-p
    (gud-def gud-up     "up %p"         "<" "Up (numeric arg) stack frames.")
    (gud-def gud-down   "down %p" ">" "Down (numeric arg) stack frames.")
    (gud-def gud-break "stop at \"%f\":%l"
				  "\C-b" "Set breakpoint at current line.")
    (gud-def gud-finish "return"  "\C-f" "Finish executing current function."))
   (gud-irix-p
    (gud-def gud-break "stop at \"%d%f\":%l"
				  "\C-b" "Set breakpoint at current line.")
    (gud-def gud-finish "return"  "\C-f" "Finish executing current function.")
    (gud-def gud-up     "up %p; printf \"\032\032%1d:\",(int)$curline;file\n"
	     "<" "Up (numeric arg) stack frames.")
    (gud-def gud-down "down %p; printf \"\032\032%1d:\",(int)$curline;file\n"
	     ">" "Down (numeric arg) stack frames.")
    ;; Make dbx give out the source location info that we need.
    (process-send-string (get-buffer-process gud-comint-buffer)
			 "printf \"\032\032%1d:\",(int)$curline;file\n"))
   (gud-dbx-use-stopformat-p
    (process-send-string (get-buffer-process gud-comint-buffer) 
			 "set $stopformat=1\n"))
   (t
    (gud-def gud-up     "up %p"         "<" "Up (numeric arg) stack frames.")
    (gud-def gud-down   "down %p" ">" "Down (numeric arg) stack frames.")
    (gud-def gud-break "file \"%d%f\"\nstop at %l"
				  "\C-b" "Set breakpoint at current line.")))

  (gud-def gud-remove "clear %l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step %p"	  "\C-s" "Step one line with display.")
  (gud-def gud-stepi  "stepi %p"  "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "next %p"	  "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "cont"	  "\C-r" "Continue with display.")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp  "^[^)\n]*dbx) *")
  (setq paragraph-start comint-prompt-regexp)
  (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  (run-hooks 'dbx-mode-hook)
  )

;; ======================================================================
;; xdb (HP PARISC debugger) functions

;;; History of argument lists passed to xdb.
(defvar gud-xdb-history nil)

(defvar gud-xdb-directories nil
  "*A list of directories that xdb should search for source code.
If nil, only source files in the program directory
will be known to xdb.

The file names should be absolute, or relative to the directory
containing the executable being debugged.")

(defun gud-xdb-massage-args (file args)
  (nconc (let ((directories gud-xdb-directories)
	       (result nil))
	   (while directories
	     (setq result (cons (car directories) (cons "-d" result)))
	     (setq directories (cdr directories)))
	   (nreverse result))
	 args))

(defun gud-xdb-file-name (f)
  "Transform a relative pathname to a full pathname in xdb mode"
  (let ((result nil))
    (if (file-exists-p f)
        (setq result (expand-file-name f))
      (let ((directories gud-xdb-directories))
        (while directories
          (let ((path (concat (car directories) "/" f)))
            (if (file-exists-p path)
                (setq result (expand-file-name path)
                      directories nil)))
          (setq directories (cdr directories)))))
    result))

;; xdb does not print the lines all at once, so we have to accumulate them
(defun gud-xdb-marker-filter (string)
  (let (result)
    (if (or (string-match comint-prompt-regexp string)
            (string-match ".*\012" string))
        (setq result (concat gud-marker-acc string)
              gud-marker-acc "")
      (setq gud-marker-acc (concat gud-marker-acc string)))
    (if result
        (if (or (string-match "\\([^\n \t:]+\\): [^:]+: \\([0-9]+\\)[: ]"
			      result)
                (string-match "[^: \t]+:[ \t]+\\([^:]+\\): [^:]+: \\([0-9]+\\):"
                              result))
            (let ((line (string-to-number
                         (substring result (match-beginning 2) (match-end 2))))
                  (file (gud-xdb-file-name
                         (substring result (match-beginning 1) (match-end 1)))))
              (if file
                  (setq gud-last-frame (cons file line))))))
    (or result "")))    
               
(defun gud-xdb-find-file (f)
  (save-excursion
    (let ((realf (gud-xdb-file-name f)))
      (if realf
	  (let ((buf (find-file-noselect realf)))
	    (set-buffer buf)
	    (gud-make-debug-menu)
	    (local-set-key [menu-bar debug tbreak]
			   '("Temporary Breakpoint" . gud-tbreak))
	    (local-set-key [menu-bar debug finish]
			   '("Finish Function" . gud-finish))
	    (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
	    (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
	    buf)
	nil))))

;;;###autoload
(defun xdb (command-line)
  "Run xdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable 'gud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory."
  (interactive
   (list (read-from-minibuffer "Run xdb (like this): "
			       (if (consp gud-xdb-history)
				   (car gud-xdb-history)
				 "xdb ")
			       nil nil
			       '(gud-xdb-history . 1))))

  (gud-common-init command-line 'gud-xdb-massage-args
		   'gud-xdb-marker-filter 'gud-xdb-find-file)

  (gud-def gud-break  "b %f:%l"    "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "b %f:%l\\t" "\C-t"
           "Set temporary breakpoint at current line.")
  (gud-def gud-remove "db"         "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "s %p"	   "\C-s" "Step one line with display.")
  (gud-def gud-next   "S %p"	   "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "c"	   "\C-r" "Continue with display.")
  (gud-def gud-up     "up %p"	   "<"    "Up (numeric arg) stack frames.")
  (gud-def gud-down   "down %p"	   ">"    "Down (numeric arg) stack frames.")
  (gud-def gud-finish "bu\\t"      "\C-f" "Finish executing current function.")
  (gud-def gud-print  "p %e"       "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp  "^>")
  (setq paragraph-start comint-prompt-regexp)
  (local-set-key [menu-bar debug tbreak] '("Temporary Breakpoint" . gud-tbreak))
  (local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
  (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  (run-hooks 'xdb-mode-hook))

;; ======================================================================
;; perldb functions

;;; History of argument lists passed to perldb.
(defvar gud-perldb-history nil)

(defun gud-perldb-massage-args (file args)
  (cons "-d" (cons (car args) (cons "-emacs" (cdr args)))))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar gud-perldb-marker-acc "")

(defun gud-perldb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match "\032\032\\([^:\n]*\\):\\([0-9]*\\):.*\n"
			 gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-marker-acc (match-beginning 1) (match-end 1))
	     (string-to-number (substring gud-marker-acc
				       (match-beginning 2)
				       (match-end 2))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" gud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring gud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq gud-marker-acc
		(substring gud-marker-acc (match-beginning 0))))

      (setq output (concat output gud-marker-acc)
	    gud-marker-acc ""))

    output))

(defun gud-perldb-find-file (f)
  (save-excursion
    (let ((buf (find-file-noselect f)))
      (set-buffer buf)
      (gud-make-debug-menu)
      buf)))

(defvar perldb-command-name "perl"
  "File name for executing Perl.")

;;;###autoload
(defun perldb (command-line)
  "Run perldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run perldb (like this): "
			       (if (consp gud-perldb-history)
				   (car gud-perldb-history)
				 (concat perldb-command-name " "))
			       nil nil
			       '(gud-perldb-history . 1))))

  (gud-common-init command-line 'gud-perldb-massage-args
		   'gud-perldb-marker-filter 'gud-perldb-find-file)

  (gud-def gud-break  "b %l"         "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "d %l"         "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "s"            "\C-s" "Step one source line with display.")
  (gud-def gud-next   "n"            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "c"            "\C-r" "Continue with display.")
;  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
;  (gud-def gud-up     "up %p"        "<" "Up N stack frames (numeric arg).")
;  (gud-def gud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "%e"           "\C-p" "Evaluate perl expression at point.")

  (setq comint-prompt-regexp "^  DB<[0-9]+> ")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'perldb-mode-hook)
  )


;; ======================================================================
;; mdb (Mercury debugger) functions

;;; History of argument lists passed to mdb.
(defvar gud-mdb-history nil)

(defun gud-mdb-massage-args (file args)
  args)

(defvar gud-mdb-directories nil
  "*A list of directories that mdb should search for source code.
If nil, only source files in the program directory
will be known to mdb.

The file names should be absolute, or relative to the directory
containing the executable being debugged.")

(defun gud-mdb-file-name (f)
  "Transform a relative pathname to a full pathname in mdb mode"
  (let ((result nil))
    (if (file-exists-p f)
        (setq result (expand-file-name f))
      (let ((directories gud-mdb-directories))
        (while directories
          (let ((path (concat (car directories) "/" f)))
            (if (file-exists-p path)
                (setq result (expand-file-name path)
                      directories nil)))
          (setq directories (cdr directories)))))
    result))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar gud-mdb-marker-acc "")

;; mdb does not print the lines all at once, so we have to accumulate them
(defun gud-mdb-marker-filter (string)
  (let ((result))
    (if (or (string-match comint-prompt-regexp string)
            (string-match "^browser> " string)
            (string-match "^cat> " string)
            (string-match "^--more-- " string)
            (string-match "^mdb: are you sure you want to quit? " string)
            (string-match ".*\012" string))
        (setq result (concat gud-marker-acc string)
              gud-marker-acc "")
      (setq gud-marker-acc (concat gud-marker-acc string)))
    (if result
;; mdb's status line looks like so:
;;        2:      2  2 CALL pred io:read_line/3-0 (det) c5;t; io.m:123 (foo.m:456)
;; We extract the file name and line number from this.
    	(let (
	       ;; Regexp to match "  <seq num>:  <event num>  <depth> "
	       (nums-regexp " *[0-9]+: *[0-9]+ *[0-9]+ ")

	       ;; Regexp to match "<event> "
	       (event-regexp "[A-Z]+ ")

	       ;; Regexp to match a procedure specification,
	       ;; i.e. "<module>.<sub-module>.<name>/<arity>-<mode> "
	       (proc-regexp "\\(?:[^\\.]+\\.\\)+[^\\./]+/[0-9]+-[0-9]+ ")
	       
	       ;; Regexp to match "(<detism>) "
	       (detism-regexp "([a-z]+) ")

	       ;; Regexp to match an optional goal path
	       (maybe-path-regexp "\\(.*; \\)?")

	       ;; Regexp to match a source context, i.e. "<filename>:<line number>"
	       ;; We use \(...\) here to allow the filename and line number that we
	       ;; matched with to be retrieved if a match is found.
	       (context-regexp "\\([^:]+\\):\\([0-9]+\\)")

	       ;; Regexp to match anything except newline
	       (junk-regexp "[^\012]*"))
        (if (or (string-match
		  ;; match pred with caller context
	          (concat nums-regexp "\\(" event-regexp "pred" proc-regexp
		          detism-regexp maybe-path-regexp "\\)" context-regexp
			  " (" context-regexp junk-regexp ")")
		  result)
                (string-match
		  ;; match func with caller context
		  (concat nums-regexp "\\(" event-regexp "func" proc-regexp
		  	  detism-regexp maybe-path-regexp "\\)" context-regexp
			  " (" context-regexp junk-regexp ")")
		  result))
	    (let
		((msg (substring result (match-beginning 1) (match-end 1)))
		 ;; match 2 is for the optional path
		 (file
		   (substring result (match-beginning 3) (match-end 3)))
		 (line
		   (substring result (match-beginning 4) (match-end 4)))
		 (caller-file (substring result (match-beginning 5) (match-end 5)))
		 (caller-line (substring result (match-beginning 6) (match-end 6))))
	      (message "%s" msg)
              (save-excursion
                (let ((abs-caller-file (gud-mdb-file-name caller-file))
                      (abs-file (gud-mdb-file-name file)))
                  (cond
                    (abs-caller-file
	              (setq gud-last-frame
                        (cons (find-file-noselect abs-caller-file)
                               (string-to-number caller-line))))
                    (abs-file
	              (setq gud-last-frame
                        (cons (find-file-noselect abs-file)
                               (string-to-number line))))))))
	  (if (or (string-match
		    ;; match pred without caller context
	            (concat nums-regexp "\\(" event-regexp "pred" proc-regexp
		    	    detism-regexp maybe-path-regexp "\\)" context-regexp
			    junk-regexp)
		    result)
		  ;; match func without caller context
		  (string-match
		    (concat nums-regexp "\\(" event-regexp "func" proc-regexp
		    	    detism-regexp maybe-path-regexp "\\)" context-regexp
			    junk-regexp)
		    result))
	      (let
		  ((msg (substring result (match-beginning 1) (match-end 1)))
		   ;; match 2 is for the optional path
		   (file (substring result (match-beginning 3) (match-end 3)))
		   (line (substring result (match-beginning 4) (match-end 4))))
		(message "%s" msg)
		(save-excursion
		  (let ((abs-file (gud-mdb-file-name file)))
		    (if abs-file
			(setq gud-last-frame
			      (cons (find-file-noselect abs-file)
				    (string-to-number line)))))))))))
    string))

(defun gud-mdb-new-keymap (map name)
  "Return a new keymap which inherits from MAP and has name MDB NAME'."
  (nconc (make-sparse-keymap (concat "MDB " name)) map))

(defvar gud-mdb-menu-map (make-sparse-keymap "MDB") nil)

(defun gud-mdb-define-menu-entries ()
  ;; queries
  (gud-def gud-mdb-query "query %b" nil "Find all solutions to query.")
  (gud-def gud-mdb-cc-query "cc_query %b" nil "Find single solution to query.")
  (gud-def gud-mdb-io-query "io_query %b" nil "Run I/O command.")

  ;; forward movement
  (gud-def gud-mdb-step   "step"      "\C-s" "Step to next trace event.")
  (gud-def gud-mdb-cont   "continue"  "\C-r" "Continue execution.")
  (gud-def gud-mdb-finish "finish"    "\C-f" "Finish executing current function.")
  (gud-def gud-mdb-forward "forward"  nil    "Forward (skip backtracking).")
  (gud-def gud-mdb-return "return"    nil    "Return (skip exits).")
  ;; NYI: goto

  ;; backward movement
  (gud-def gud-mdb-retry  "retry"    nil     "Retry")

  ;; data browsing
  (gud-def gud-mdb-stack  "stack"     "\C-d" "Show stack.")
  (gud-def gud-mdb-up     "up %p"     "<"    "Up N stack frames (numeric arg).")
  (gud-def gud-mdb-down   "down %p"   ">"    "Down N stack frames (numeric arg).")
  (gud-def gud-mdb-vars   "vars"      "\C-v" "Show variables.")
  (gud-def gud-mdb-print  "print %i"  "\C-p" "Print Mercury variable at point.")
  (gud-def gud-mdb-browse "browse %i" "\C-i" "Browse Mercury variable at point.")
  (gud-def gud-mdb-modules "modules"  nil    "List debuggable modules.")
  (gud-def gud-mdb-procs "procedures %i" nil "List procedures in a module.")
  ;; NYI: level, current

  ;; breakpoints
  (gud-def gud-mdb-break  "break %i" "\C-p" "Set breakpoint on procedure.")
  (gud-def gud-mdb-break-line  "break %f:%l" "\C-l" "Set breakpoint on line.")
  (gud-def gud-mdb-breakpoints  "break info" nil "Show current breakpoints.")

  ;; option settings
  (gud-def gud-mdb-aliases "alias" nil "Show aliases.")
  (gud-def gud-mdb-printlevel-none "printlevel none" nil "Set printlevel none.")
  (gud-def gud-mdb-printlevel-some "printlevel some" nil "Set printlevel some.")
  (gud-def gud-mdb-printlevel-all "printlevel all" nil "Set printlevel all.")
  (gud-def gud-mdb-printlevel "printlevel" nil "Show printlevel.")
  (gud-def gud-mdb-echo-on "echo on" nil "Set echo on.")
  (gud-def gud-mdb-echo-off "echo off" nil "Set echo off.")
  (gud-def gud-mdb-echo "echo" nil "Show echoing.")
  ;; NYI: scroll

  ;; NYI: help category
  ;; NYI: experimental category
  ;; NYI: developer commands category

  ;; misc
  ;; NYI: source
  ;; (gud-def gud-mdb-source "source <NYI>" nil "Source file")
  (gud-def gud-mdb-quit "quit" nil "Quit")
)

(defun gud-mdb-make-debug-menu (name)
  (if (and (current-local-map)
	   (lookup-key (current-local-map) [menu-bar debug (name)]))
      nil
    (use-local-map (gud-mdb-new-keymap (current-local-map) name))
    (define-key (current-local-map) [menu-bar debug (arg)]
      (cons (concat "MDB " name) (gud-mdb-new-keymap gud-menu-map name)))))

(defun gud-mdb-add-menu (path name)
  (if (and (current-local-map)
	   (lookup-key (current-local-map) path))
      nil
    (use-local-map (gud-mdb-new-keymap (current-local-map) name))
    (define-key (current-local-map) path
      (cons name (gud-mdb-new-keymap gud-mdb-menu-map name)))))

(defun gud-mdb-add-menu-entries ()
  ;; menu entries, in reverse order

  ;; top-level menu entry
  (gud-mdb-add-menu [menu-bar debug] "MDB")

  ;; misc
  (gud-mdb-add-menu [menu-bar debug misc] "Miscellaneous Commands")
  (local-set-key [menu-bar debug misc quit] '("Quit" . gud-mdb-quit))
  ;; The following settings commands are not yet implemented because
  ;; they take parameters: source

  ;; settings
  (gud-mdb-add-menu [menu-bar debug settings] "Option Settings")
  (local-set-key [menu-bar debug settings aliases]
		 '("Show Aliases" . gud-mdb-aliases))
  (local-set-key [menu-bar debug settings printlevel]
		 '("Show Printlevel" . gud-mdb-printlevel))
  (local-set-key [menu-bar debug settings printlevel-none]
		 '("Set Printlevel None" . gud-mdb-printlevel-all))
  (local-set-key [menu-bar debug settings printlevel-some]
		 '("Set Printlevel Some" . gud-mdb-printlevel-some))
  (local-set-key [menu-bar debug settings printlevel-all]
		 '("Set Printlevel All" . gud-mdb-printlevel-all))
  ;; The following settings commands are not yet implemented because
  ;; they take parameters: alias, unalias, mmc_options
  ;; The following settings commands are not implemented because
  ;; they are not useful in an Emacs environment: echo, scroll

  ;; breakpoints
  (gud-mdb-add-menu [menu-bar debug breakpoints] "Breakpoint Commands")
  (local-set-key [menu-bar debug breakpoints break-procedure]
		 '("Set breakpoint on procedure" . gud-mdb-break))
  (local-set-key [menu-bar debug breakpoints break-line]
		 '("Set breakpoint on line" . gud-mdb-break-line))
  (local-set-key [menu-bar debug breakpoints breakpoints]
		 '("Show current breakpoints" . gud-mdb-breakpoints))

  ;; browsing
  (gud-mdb-add-menu [menu-bar debug browsing] "Data Browsing Commands")
  (local-set-key [menu-bar debug browsing browse]
		 '("Browse Variable" . gud-mdb-browse))
  (local-set-key [menu-bar debug browsing print]
		 '("Print Variable" . gud-mdb-print))
  (local-set-key [menu-bar debug browsing vars] '("Variables" . gud-mdb-vars))
  (local-set-key [menu-bar debug browsing down] '("Down Stack" . gud-mdb-down))
  (local-set-key [menu-bar debug browsing up] '("Up Stack" . gud-mdb-up))
  (local-set-key [menu-bar debug browsing stack] '("Show Stack" . gud-mdb-stack))

  ;; backward
  (gud-mdb-add-menu [menu-bar debug backward] "Backward Movement Commands")
  (local-set-key [menu-bar debug backward retry] '("Retry" . gud-mdb-retry))

  ;; forward
  (gud-mdb-add-menu [menu-bar debug forward] "Forward Movement Commands")
  (local-set-key [menu-bar debug forward return]
		 '("Return (skip exits)" . gud-mdb-return))
  (local-set-key [menu-bar debug forward forward]
		 '("Forward (skip backtracking)" . gud-mdb-forward))
  (local-set-key [menu-bar debug forward finish]
		 '("Finish procedure" . gud-mdb-finish))
  (local-set-key [menu-bar debug forward continue]
		 '("Continue execution" . gud-mdb-cont))
  (local-set-key [menu-bar debug forward step]
		 '("Step to next trace event" . gud-mdb-step))
  ;; The following commands are not yet implemented because
  ;; they take parameters: mindepth, maxdepth, goto
  
  ;; queries
  (gud-mdb-add-menu [menu-bar debug query] "Interactive Query Commands")
  (local-set-key [menu-bar debug query query]
		 '("Find all solutions to query" . gud-mdb-query))
  (local-set-key [menu-bar debug query cc-query]
		 '("Find single solution to query" . gud-mdb-cc-query))
  (local-set-key [menu-bar debug query io-query]
		 '("Run I/O command" . gud-mdb-io-query))

  )

(defun gud-mdb-find-file (buf)
  (save-excursion
    (set-buffer buf)
    (display-buffer buf)
    (gud-mdb-add-menu-entries)
    buf))

(defvar mdb-command-name "mdb"
  "Command name for executing mdb.")

;;;###autoload
(defun mdb (command-line)
  "Run mdb (the Mercury debugger) on program FILE
with mdb's I/O in buffer *mdb-FILE*,
and with the program's I/O in buffer *FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run mdb (like this): "
			       (if (consp gud-mdb-history)
				   (car gud-mdb-history)
				 (concat mdb-command-name " "))
			       nil nil
			       '(gud-mdb-history . 1))))

  (setq gud-menu-name "MDB")
  (setq gud-buffer-name-prefix "mdb")
  (setq gud-show-commands t)
  ;; Setting gud-redirect-io is broken in recent versions of
  ;; GNU Emacs (e.g. 20.4.1), since Emacs apparently now uses
  ;; pipes rather than ttys.  So we don't enable this by
  ;; default anymore.
  ;; (setq gud-redirect-io t)

  (gud-common-init command-line 'gud-mdb-massage-args
		   'gud-mdb-marker-filter 'gud-mdb-find-file)

  (gud-mdb-define-menu-entries)
  (gud-mdb-add-menu-entries)
  (if gud-prog-buffer
      (save-excursion
        (set-buffer gud-prog-buffer)
        (display-buffer gud-prog-buffer)
        (gud-mdb-add-menu-entries)))

  (setq comint-prompt-regexp "^mdb> ")
  (setq paragraph-start comint-prompt-regexp)

  (run-hooks 'mdb-mode-hook)
  )

;;
;; End of debugger-specific information
;;


;;; When we send a command to the debugger via gud-call, it's annoying
;;; to see the command and the new prompt inserted into the debugger's
;;; buffer; we have other ways of knowing the command has completed.
;;;
;;; If the buffer looks like this:
;;; --------------------
;;; (gdb) set args foo bar
;;; (gdb) -!-
;;; --------------------
;;; (the -!- marks the location of point), and we type `C-x SPC' in a
;;; source file to set a breakpoint, we want the buffer to end up like
;;; this:
;;; --------------------
;;; (gdb) set args foo bar
;;; Breakpoint 1 at 0x92: file make-docfile.c, line 49.
;;; (gdb) -!-
;;; --------------------
;;; Essentially, the old prompt is deleted, and the command's output
;;; and the new prompt take its place.
;;;
;;; Not echoing the command is easy enough; you send it directly using
;;; process-send-string, and it never enters the buffer.  However,
;;; getting rid of the old prompt is trickier; you don't want to do it
;;; when you send the command, since that will result in an annoying
;;; flicker as the prompt is deleted, redisplay occurs while Emacs
;;; waits for a response from the debugger, and the new prompt is
;;; inserted.  Instead, we'll wait until we actually get some output
;;; from the subprocess before we delete the prompt.  If the command
;;; produced no output other than a new prompt, that prompt will most
;;; likely be in the first chunk of output received, so we will delete
;;; the prompt and then replace it with an identical one.  If the
;;; command produces output, the prompt is moving anyway, so the
;;; flicker won't be annoying.
;;;
;;; So - when we want to delete the prompt upon receipt of the next
;;; chunk of debugger output, we position gud-delete-prompt-marker at
;;; the start of the prompt; the process filter will notice this, and
;;; delete all text between it and the process output marker.  If
;;; gud-delete-prompt-marker points nowhere, we leave the current
;;; prompt alone.
(defvar gud-delete-prompt-marker nil)


(defun gud-mode ()
  "Major mode for interacting with an inferior debugger process.

   You start it up with one of the commands M-x gdb, M-x sdb, M-x dbx,
M-x mdb, M-x perldb, or M-x xdb.  Each entry point finishes by executing a
hook; `gdb-mode-hook', `sdb-mode-hook', `dbx-mode-hook',
`mdb-mode-hook', `perldb-mode-hook', or `xdb-mode-hook' respectively.

After startup, the following commands are available in both the GUD
interaction buffer and any source buffer GUD visits due to a breakpoint stop
or step operation:

\\[gud-break] sets a breakpoint at the current file and line.  In the
GUD buffer, the current file and line are those of the last breakpoint or
step.  In a source buffer, they are the buffer's file and current line.

\\[gud-remove] removes breakpoints on the current file and line.

\\[gud-refresh] displays in the source window the last line referred to
in the gud buffer.

\\[gud-step], \\[gud-next], and \\[gud-stepi] do a step-one-line,
step-one-line (not entering function calls), and step-one-instruction
and then update the source window with the current file and position.
\\[gud-cont] continues execution.

\\[gud-print] tries to find the largest C lvalue or function-call expression
around point, and sends it to the debugger for value display.

The above commands are common to all supported debuggers except xdb which
does not support stepping instructions.

Under gdb, sdb and xdb, \\[gud-tbreak] behaves exactly like \\[gud-break],
except that the breakpoint is temporary; that is, it is removed when
execution stops on it.

Under gdb, dbx, and xdb, \\[gud-up] pops up through an enclosing stack
frame.  \\[gud-down] drops back down through one.

If you are using gdb or xdb, \\[gud-finish] runs execution to the return from
the current function and stops.

All the keystrokes above are accessible in the GUD buffer
with the prefix C-c, and in all buffers through the prefix C-x C-a.

All pre-defined functions for which the concept make sense repeat
themselves the appropriate number of times if you give a prefix
argument.

You may use the `gud-def' macro in the initialization hook to define other
commands.

Other commands for interacting with the debugger process are inherited from
comint mode, which see."
  (interactive)
  (comint-mode)
  (setq major-mode 'gud-mode)
  (setq mode-name "Debugger")
  (setq mode-line-process '(":%s"))
  (use-local-map comint-mode-map)
  (gud-make-debug-menu)
  (define-key (current-local-map) "\C-c\C-l" 'gud-refresh)
  (make-local-variable 'gud-last-frame)
  (setq gud-last-frame nil)
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'gud-delete-prompt-marker)
  (setq gud-delete-prompt-marker (make-marker))
  (run-hooks 'gud-mode-hook))

;; Chop STRING into words separated by SPC or TAB and return a list of them.
(defun gud-chop-words (string)
  (let ((i 0) (beg 0)
	(len (length string))
	(words nil))
    (while (< i len)
      (if (memq (aref string i) '(?\t ? ))
	  (progn
	    (setq words (cons (substring string beg i) words)
		  beg (1+ i))
	    (while (and (< beg len) (memq (aref string beg) '(?\t ? )))
	      (setq beg (1+ beg)))
	    (setq i (1+ beg)))
	(setq i (1+ i))))
    (if (< beg len)
	(setq words (cons (substring string beg) words)))
    (nreverse words)))

;; Perform initializations common to all debuggers.
;; The first arg is the specified command line,
;; which starts with the program to debug.
;; The other three args specify the values to use
;; for local variables in the debugger buffer.
(defun gud-common-init (command-line massage-args marker-filter find-file)
  (let* ((words (gud-chop-words command-line))
	 (program (car words))
	 ;; Extract the file name from WORDS
	 ;; and put t in its place.
	 ;; Later on we will put the modified file name arg back there.
	 (file-word (let ((w (cdr words)))
		      (while (and w (= ?- (aref (car w) 0)))
			(setq w (cdr w)))
		      (and w
			   (prog1 (car w)
			     (setcar w t)))))
	 (file-subst
	  (and file-word (substitute-in-file-name file-word)))
	 (args (cdr words))
	 ;; If a directory was specified, expand the file name.
	 ;; Otherwise, don't expand it, so GDB can use the PATH.
	 ;; A file name without directory is literally valid
	 ;; only if the file exists in ., and in that case,
	 ;; omitting the expansion here has no visible effect.
	 (file (and file-word
		    (if (file-name-directory file-subst)
			(expand-file-name file-subst)
		      file-subst)))
	 (base-file (file-name-nondirectory file))
	 (filepart (and file-word (concat "-" base-file)))
	 (prog-buf-name (concat gud-buffer-name-prefix filepart))
	 (debugger-buf nil))
      
    (switch-to-buffer (concat "*" gud-buffer-name-prefix filepart "*"))
    ;; Set default-directory to the file's directory.
    (and file-word
	 ;; Don't set default-directory if no directory was specified.
	 ;; In that case, either the file is found in the current directory,
	 ;; in which case this setq is a no-op,
	 ;; or it is found by searching PATH,
	 ;; in which case we don't know what directory it was found in.
	 (file-name-directory file)
	 (setq default-directory (file-name-directory file)))
    (or (bolp) (newline))

    (if (not gud-redirect-io)
    	(insert "Current directory is " default-directory "\n"))
    ;; Put the substituted and expanded file name back in its place.
    (let ((w args))
      (while (and w (not (eq (car w) t)))
	(setq w (cdr w)))
      (if w
	  (setcar w file)))
    (if gud-redirect-io
	;; Create window for I/O, and insert "--tty" option in args
	(let* ((buf-tty (gud-start-io-window prog-buf-name))
	       (buf (car buf-tty))
	       (tty (car (cdr buf-tty))))
	  (setq prog-buf-name base-file)
	  (setq debugger-buf buf)
	  (setq args (cons "--tty" (cons tty args)))))
    (apply 'make-comint prog-buf-name program nil
	   (funcall massage-args file args))
    (if gud-redirect-io
        (progn
	  (setq gud-prog-buffer (concat "*" prog-buf-name "*"))
	  (set-buffer debugger-buf))))

  ;; Since comint clobbered the mode, we don't set it until now.
  (gud-mode)
  (make-local-variable 'gud-marker-filter)
  (setq gud-marker-filter marker-filter)
  (make-local-variable 'gud-find-file)
  (setq gud-find-file find-file)

  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
  (gud-set-buffer)
)

;;
;; Emacs doesn't let a single process have two different I/O buffers.
;; So we need to to create a dummy process for the second buffer.
;; This dummy process just prints out its tty, so that we can
;; redirect the I/O from the real process to that tty,
;; and then loops forever.
;;
(defun gud-start-io-window (name)
  (let (buf tty)
    (setq buf (make-comint name "/bin/sh" nil
			  "-c" "tty; while : ; do sleep 32767; done"))
    (save-excursion
      (set-buffer buf)
      (display-buffer buf)
      (sleep-for 1)
      (if (string-match "\\(/dev/[^\012]*\\)\012" (buffer-string))
	  (setq tty
		(substring (buffer-string) (match-beginning 1) (match-end 1)))
	(error "Hmm, can't figure out the tty...")))
    (list buf tty)))

(defun gud-set-buffer ()
  (cond ((eq major-mode 'gud-mode)
	(setq gud-comint-buffer (current-buffer)))))

(defvar gud-filter-defer-flag nil
  "Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")

(defvar gud-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in `gud-filter'.")

;; These functions are responsible for inserting output from your debugger
;; into the buffer.  The hard work is done by the method that is
;; the value of gud-marker-filter.

(defun gud-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if gud-filter-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later.
	    (setq gud-filter-pending-text
		  (concat (or gud-filter-pending-text "") string))

	  ;; If we have to ask a question during the processing,
	  ;; defer any additional text that comes from the debugger
	  ;; during that time.
	  (let ((gud-filter-defer-flag t))
	    ;; Process now any text we previously saved up.
	    (if gud-filter-pending-text
		(setq string (concat gud-filter-pending-text string)
		      gud-filter-pending-text nil))
	    (save-excursion
	      (set-buffer (process-buffer proc))
	      ;; If we have been so requested, delete the debugger prompt.
	      (if (marker-buffer gud-delete-prompt-marker)
		  (progn
		    (delete-region (process-mark proc) gud-delete-prompt-marker)
		    (set-marker gud-delete-prompt-marker nil)))
	      ;; Save the process output, checking for source file markers.
	      (setq output (gud-marker-filter string))
	      ;; Check for a filename-and-line number.
	      ;; Don't display the specified file
	      ;; unless (1) point is at or after the position where output appears
	      ;; and (2) this buffer is on the screen.
	      (setq process-window
		    (and gud-last-frame
			 (>= (point) (process-mark proc))
			 (get-buffer-window (current-buffer))))

	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
	      (comint-output-filter proc output)))

	  ;; Put the arrow on the source line.
	  ;; This must be outside of the save-excursion
	  ;; in case the source file is our current buffer.
	  (if process-window
	      (save-selected-window
		(select-window process-window)
		(gud-display-frame))
	    ;; We have to be in the proper buffer, (process-buffer proc),
	    ;; but not in a save-excursion, because that would restore point.
	    (let ((old-buf (current-buffer)))
	      (set-buffer (process-buffer proc))
	      (unwind-protect
		  (gud-display-frame)
		(set-buffer old-buf))))

	  ;; If we deferred text that arrived during this processing,
	  ;; handle it now.
	  (if gud-filter-pending-text
	      (gud-filter proc ""))))))

(defun gud-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ":"
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 (force-mode-line-update)
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gud buffer.
	     (set-buffer obuf))))))

(defun gud-display-frame ()
  "Find and obey the last filename-and-line marker from the debugger.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (if gud-last-frame
   (progn
     (gud-set-buffer)
     (gud-display-line (car gud-last-frame) (cdr gud-last-frame))
     (setq gud-last-last-frame gud-last-frame
	   gud-last-frame nil))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.
;; Most of the trickiness in here comes from wanting to preserve the current
;; region-restriction if that's possible.  We use an explicit display-buffer
;; to get around the fact that this is called inside a save-excursion.

(defun gud-display-line (true-file line)
  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
	 (buffer
	  (save-excursion
	    (or (eq (current-buffer) gud-comint-buffer)
		(set-buffer gud-comint-buffer))
	    (gud-find-file true-file)))
	 (window (and buffer (display-buffer buffer)))
	 (pos))
    (if buffer
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (setq pos (point))
	      (setq overlay-arrow-string "=>")
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))

;;; The gud-call function must do the right thing whether its invoking
;;; keystroke is from the GUD buffer itself (via major-mode binding)
;;; or a C buffer.  In the former case, we want to supply data from
;;; gud-last-frame.  Here's how we do it:

(defun gud-format-command (str arg)
  (let ((insource (not (eq (current-buffer) gud-comint-buffer)))
	(frame (or gud-last-frame gud-last-last-frame))
	result match2)
    (while (and str (string-match "\\([^%]*\\)%\\([abdefilp]\\)" str))
      (let* ((key (string-to-char (substring str (match-beginning 2))))
	     (match1 (substring str (match-beginning 1) (match-end 1)))
	     subst)
	(setq match2 (substring str (match-end 2)))
	(cond
	 ((eq key ?f)
	  (setq subst (file-name-nondirectory (if insource
						  (buffer-file-name)
						(save-excursion
						  (set-buffer (car frame))
						  (buffer-file-name))))))
	 ((eq key ?b)
	  (let ((filename (file-name-nondirectory (if insource
						      (buffer-file-name)
						    (save-excursion
						      (set-buffer (car frame))
						      (buffer-file-name))))))
	    ;; (message "Filename: %s" filename)
	    (if (string-match "\\(.*\\)\\..*" filename)
	      (setq subst (substring filename (match-beginning 1) (match-end 1)))
	      (setq subst filename))))
	 ((eq key ?d)
	  (setq subst (file-name-directory (if insource
					       (buffer-file-name)
					     (save-excursion
					       (set-buffer (car frame))
					       (buffer-file-name))))))
	 ((eq key ?l)
	  (setq subst (if insource
			  (save-excursion
			    (beginning-of-line)
			    (save-restriction (widen) 
					      (1+ (count-lines 1 (point)))))
			(cdr frame))))
	 ((eq key ?e)
	  (setq subst (find-c-expr)))
	 ((eq key ?a)
	  (setq subst (gud-read-address)))
	 ((eq key ?i)
	  (setq subst (gud-read-identifier)))
	 ((eq key ?p)
	  (setq subst (if arg (int-to-string arg) ""))))
	(setq result (concat result match1 subst)))
      (setq str match2))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun gud-read-address ()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
    (let ((pt (point)) found begin)
      (setq found (if (search-backward "0x" (- pt 7) t) (point)))
      (cond
       (found (forward-char 2)
	      (buffer-substring found
				(progn (re-search-forward "[^0-9a-f]")
				       (forward-char -1)
				       (point))))
       (t (setq begin (progn (re-search-backward "[^0-9]") 
			     (forward-char 1)
			     (point)))
	  (forward-char 1)
	  (re-search-forward "[^0-9]")
	  (forward-char -1)
	  (buffer-substring begin (point)))))))

(defun gud-read-identifier ()
  "Return a string containing an identifier found in the buffer at point."
  (save-excursion
    (let (begin end)
      (re-search-backward "[^0-9a-zA-Z_:/-]")
      (forward-char 1)
      (setq begin (point))
      (re-search-forward "[^0-9a-zA-Z_:/-]")
      (backward-char 1)
      (setq end (point))
      (buffer-substring begin end))))

(defun gud-call (fmt &optional arg)
  (let ((msg (gud-format-command fmt arg)))
    (message "Command: %s" msg)
    (sit-for 0)
    (gud-basic-call msg)))

(defun gud-basic-call (command)
  "Invoke the debugger COMMAND displaying source in other window."
  (interactive)
  (gud-set-buffer)
  (let ((command (concat command "\n"))
	(proc (get-buffer-process gud-comint-buffer)))
    (or proc (error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (if gud-show-commands
	(comint-output-filter proc command)
      (save-excursion
	(set-buffer gud-comint-buffer)
	(goto-char (process-mark proc))
	(beginning-of-line)
	(if (looking-at comint-prompt-regexp)
	    (set-marker gud-delete-prompt-marker (point)))))
    (process-send-string proc command)))

(defun gud-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive "P")
  (recenter arg)
  (or gud-last-frame (setq gud-last-frame gud-last-last-frame))
  (gud-display-frame))

;;; Code for parsing expressions out of C code.  The single entry point is
;;; find-c-expr, which tries to return an lvalue expression from around point.
;;;
;;; The rest of this file is a hacked version of gdbsrc.el by
;;; Debby Ayers <ayers@asc.slb.com>,
;;; Rich Schaefer <schaefer@asc.slb.com> Schlumberger, Austin, Tx.

(defun find-c-expr ()
  "Returns the C expr that surrounds point."
  (interactive)
  (save-excursion
    (let ((p) (expr) (test-expr))
      (setq p (point))
      (setq expr (expr-cur))
      (setq test-expr (expr-prev))
      (while (expr-compound test-expr expr)
	(setq expr (cons (car test-expr) (cdr expr)))
	(goto-char (car expr))
	(setq test-expr (expr-prev)))
      (goto-char p)
      (setq test-expr (expr-next))
      (while (expr-compound expr test-expr)
	(setq expr (cons (car expr) (cdr test-expr)))
	(setq test-expr (expr-next))
	)
      (buffer-substring (car expr) (cdr expr)))))

(defun expr-cur ()
  "Returns the expr that point is in; point is set to beginning of expr.
The expr is represented as a cons cell, where the car specifies the point in
the current buffer that marks the beginning of the expr and the cdr specifies 
the character after the end of the expr."
  (let ((p (point)) (begin) (end))
    (expr-backward-sexp)
    (setq begin (point))
    (expr-forward-sexp)
    (setq end (point))
    (if (>= p end) 
	(progn
	 (setq begin p)
	 (goto-char p)
	 (expr-forward-sexp)
	 (setq end (point))
	 )
      )
    (goto-char begin)
    (cons begin end)))

(defun expr-backward-sexp ()
  "Version of `backward-sexp' that catches errors."
  (condition-case nil
      (backward-sexp)
    (error t)))

(defun expr-forward-sexp ()
  "Version of `forward-sexp' that catches errors."
  (condition-case nil
     (forward-sexp)
    (error t)))

(defun expr-prev ()
  "Returns the previous expr, point is set to beginning of that expr.
The expr is represented as a cons cell, where the car specifies the point in
the current buffer that marks the beginning of the expr and the cdr specifies 
the character after the end of the expr"
  (let ((begin) (end))
    (expr-backward-sexp)
    (setq begin (point))
    (expr-forward-sexp)
    (setq end (point))
    (goto-char begin)
    (cons begin end)))

(defun expr-next ()
  "Returns the following expr, point is set to beginning of that expr.
The expr is represented as a cons cell, where the car specifies the point in
the current buffer that marks the beginning of the expr and the cdr specifies 
the character after the end of the expr."
  (let ((begin) (end))
    (expr-forward-sexp)
    (expr-forward-sexp)
    (setq end (point))
    (expr-backward-sexp)
    (setq begin (point))
    (cons begin end)))

(defun expr-compound-sep (span-start span-end)
  "Returns '.' for '->' & '.', returns ' ' for white space,
returns '?' for other punctuation."
  (let ((result ? )
	(syntax))
    (while (< span-start span-end)
      (setq syntax (char-syntax (char-after span-start)))
      (cond
       ((= syntax ? ) t)
       ((= syntax ?.) (setq syntax (char-after span-start))
	(cond 
	 ((= syntax ?.) (setq result ?.))
	 ((and (= syntax ?-) (= (char-after (+ span-start 1)) ?>))
	  (setq result ?.)
	  (setq span-start (+ span-start 1)))
	 (t (setq span-start span-end)
	    (setq result ??)))))
      (setq span-start (+ span-start 1)))
    result))

(defun expr-compound (first second)
  "Non-nil if concatenating FIRST and SECOND makes a single C token.
The two exprs are represented as a cons cells, where the car 
specifies the point in the current buffer that marks the beginning of the 
expr and the cdr specifies the character after the end of the expr.
Link exprs of the form:
      Expr -> Expr
      Expr . Expr
      Expr (Expr)
      Expr [Expr]
      (Expr) Expr
      [Expr] Expr"
  (let ((span-start (cdr first))
	(span-end (car second))
	(syntax))
    (setq syntax (expr-compound-sep span-start span-end))
    (cond
     ((= (car first) (car second)) nil)
     ((= (cdr first) (cdr second)) nil)
     ((= syntax ?.) t)
     ((= syntax ? )
	 (setq span-start (char-after (- span-start 1)))
	 (setq span-end (char-after span-end))
	 (cond
	  ((= span-start ?) ) t )
	  ((= span-start ?] ) t )
          ((= span-end ?( ) t )
	  ((= span-end ?[ ) t )
	  (t nil))
	 )
     (t nil))))

(defun gud-linenum (buf)
  "Return the current line number of BUF"
  (save-excursion
    (set-buffer buf)
    (beginning-of-line)
    (save-restriction
      (widen) 
      (1+ (count-lines 1 (point))))))

(provide 'gud)

;;; gud.el ends here
