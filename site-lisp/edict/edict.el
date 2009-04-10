;;; edict.el --- Word lookup (with deinflection) in EDICT

;; Copyright (C) 1991, 1992 Per Hammarlund (perham@nada.kth.se)

;; Author:      Per Hammarlund <perham@nada.kth.se>
;; Keywords:    mule, edict, dictionary
;; Version:     0.9.7
;; Adapted-by:  Stephen J. Turnbull <turnbull@sk.tsukuba.ac.jp> for XEmacs
;; Maintainer:  Stephen J. Turnbull <turnbull@sk.tsukuba.ac.jp>

;;   This file is part of XEmacs.

;;   XEmacs is free software; you can redistribute it and/or modify it
;;   under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2, or (at your
;;   option) any later version.

;;   XEmacs is distributed in the hope that it will be useful, but
;;   WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;   General Public License for more details.
;; 
;;   You should have received a copy of the GNU General Public License
;;   along with XEmacs; if not, write to the Free Software Foundation,
;;   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Some code that looks for translations of english and japanese using the
;; EDICTJ Public Domain japanese/english dictionary.

;; Written by Per Hammarlund <perham@nada.kth.se>
;; Morphology and private dictionary handling/editing by Bob Kerns
;; <rwk@crl.dec.com>
;; Helpful remarks from Ken-Ichi Handa <handa@etl.go.jp>.
;; The EDICTJ PD dictionary is maintained by Jim Breen
;; <jwb@monu6.cc.monash.edu.au>

;; Short getting started guide, this assumes that you have not used
;; the install script and that you understand the "technical" words
;; used, if you don't, please read the documentation in edict.doc:

;; 1. Make sure that you have placed edict.el in a directory that is
;;    included in the nemacs's search path, look at the variable
;;    "load-path" to make sure that the directory is in that list.

;; 2. Add something like this to your .emacs (or .nemacs) file:
;;	(autoload 'edict-search-english "edict"
;;		  "Search for a translation of an English word")
;;	(global-set-key "\e*" 'edict-search-english)
;;	(autoload 'edict-search-kanji "edict"
;;		  "Search for a translation of a Kanji sequence")
;;	(global-set-key "\e_" 'edict-search-kanji)
;;	(autoload 'edict-insert "edict" "Insert the last translation")
;;     (global-set-key "\e+" 'edict-insert)
;; Note that you can change the key binding to whatever you like,
;; these are only "examples".

;; 3. The variable *edict-files* should be a list of filenames of
;;    edict dictionary files that you want edict to load and search
;;    in.  The real dictionary EDICTJ should be one of these files,
;;    you may also have have some local file(s) there.  Something 
;;    like this *may* be appropriate to:
;;	(setq *edict-files*  '("edictj"
;;			      "~my-friend-the-user/.edict"
;;			      "~my-other-friend-the-user/.edict"))
;;    By default, it searches the load path (the same directories that
;;    are searched when you do m-X load-file<return>edict<return>),
;;    for a file named "edictj".

;; 4. Set the name of your *own* local edictj file.  (Note that this
;;    file should not be included in the list above!)  Edict will
;;    include the additions that you do in this file.  The variable
;;    *edict-private-file* defaults to "~/.edict", if you want
;;    something else do a:
;;	  (setq *edict-private-file* "~/somewhere/somethingelse/")

;; (Don't forget to submit your useful words to Jim Breen once in a
;; while! His address is jwb@monu6.cc.monash.edu.au)

;; You are done.  Report errors and comments to perham@nada.kth.se.

;;; To do:

;; From: "Stephen J. Turnbull" <turnbull@sk.tsukuba.ac.jp>
;; To: SL Baur <steve@xemacs.org>
;; Message-ID: <13580.37185.529017.717779@tanko.sk.tsukuba.ac.jp>
;; X-Mailer: VM 6.43 under 20.5 "Moxoto" XEmacs  Lucid (beta30)
;; Subject: Re: Porting edict.el
;; Date: Mon, 16 Mar 1998 12:51:36 +0900 (JST)
;;
;; My current state is I have a compiling and working version using the
;; category/range variables introduced in that file.  (Attached below, FYI.)
;;
;; Unless you have reason to want to keep your hands directly on this
;; package now, I think I can take care of it for the next few steps, now
;; that it compiles and runs.  The failure of half-a-dozen productions in
;; edict-tests.el are Japanese grammar problems, not a LISP problem
;; (including the final "unexpected expansion", which is an English
;; syntax problem).
;;
;; Here's what I have in mind (not necessarily in this order):
;;
;; 1.  [OK] Keep the range variables already introduced, and document
;;     them and how to byte-compile with them.  (Fix the package Makefile.)
;; 2.  [OK] Make the ranges correct (first try; until I really understand the
;;     implementation of the grammar, I can't say they are truly correct, 
;;     it's not quite as simple as A-B-C).
;; 3.  [OK] Make the file comply with lisp-mnt.el.
;; 4.  [OK] Reorganize and document the file by functionality, possibly
;;     separating  them into different files.  [SB: check with Per H. first.]
;; 5.  [OK] Punt on the coding-system of EDICT problem, since recognizing
;;     which of a half-dozen coding systems is in use is hard, and it is
;;     highly likely that different systems will translate the file.
;;     I guess rather than simply punt-by-documenting I will add an
;;     option variable to specify a coding system, and default it to
;;     auto-recognition.
;;     [SJT: *edict-files* is a _list_.  Drop back 15....  The quick fix is
;;	to put it in file-coding-system-alist.  "Fixed-by-documentation."]
;; 6.  [OK] Find an appropriate binding for the minor mode map.  My instant
;;     reaction was "C-C ?", but that's taken.  So I will look around
;;     (check ispell and the like, maybe one of those can be
;;     appropriately augmented---this would have to be coordinated with
;;     the appropriate maintainer, right?), but I'm leaning toward one of
;;     the quote marks (" ' `) if I don't find something else.
;;     [SB:  before doing anything permanent, discuss on XEmacs Beta.]
;;     [SJT:  OK, ispell uses M-$, and LEIM and Wnn both use C-\.  I see
;;	all three processes (spell-check, localized input methods, and
;;	dictionary lookup) as being aspects of high-level dictionary-
;;	based natural language input.  I would like to overload the same
;;	mode-toggle for all of them.  I see IMs as being primary (analogous
;;	to a minor mode), while the checking functions are secondary and/or
;;	transient.  Unfortunately, both ispell and LEIM use prefix args to
;;	modify the toggle's behavior.  But all of "C-$", "C-c $", "C-c \",
;;	and "C-c \" are undefined.
;;	  I see the interface as follows.
;;	The main-entry point is the the transient function (ispell-word,
;;	edict-lookup-*), bound to the un-prefixed key.
;;	The prefixed key would give a short choice menu, in the echo area.
;;	A short-list of defaults would be alternative transient functions,
;;	plus the choices to add or delete from the menu, or to do more
;;	complicated maintenance (eg, customize, once we have an interface.)
;; 7.  Maybe add a customize interface (the only thing that might want
;;     customized that I know of is the proposed edict-coding-system
;;     variable, but there it would be definitely useful since Custom can 
;;     be set up to automatically reread EDICT, unlike setq).
;;     [SJT: also *edict-files*.]
;; 8.  [OK] Autoload cookies as appropriate.
;; 9.  Fix style of docstrings and move comments into docstrings where
;;     appropriate.
;; 10. [OK] Fix grammar errors reported by edict-test.el.
;;
;; Some other stuff I definitely need advice or help on:
;;
;; 1.  The nomenclature is non-standard (use of `*' in variable names).
;;     Change these to the `edict-...' style?  Only with permission of
;;     Per Hammarlund?
;; 2.  [OK] I'm willing to do maintenance of this package.  Obviously that
;;     requires an attempt to contact Per.  I think he would be amenable
;;     from our last conversation, when he said he didn't have time for it.
;; 3.  FSF compatibility.  This shouldn't be too hard, and I intend to do 
;;     it, but is it a priority (I would need to install FSF Emacs)?
;; 4.  As far as XEmacs is concerned, I can say anything pre-20.0 is
;;     unsupported, obviously.  But it probably should be tested for
;;     backward compatibility with 20.[02].
;;
;; Projects:
;;
;; 1.  A "report private dictionary to Jim Breen" function.
;; 2.  A "parse-word-at-point" function.  (I was wrong, it isn't in 
;;     the current version.  Hard; you can ask for it, but I don't know
;;     that I'll be able to produce a good one quickly :-)
;; 3.  Info documentation.
;; 4.  Generalization (eg for Korean/Chinese, or for Spanish/German for
;;     that matter).
;;
;; Any comments or requests gratefully accepted.
;;
;; Ah yes, deadline?  I guess since it's a package, none in particular,
;; but I oughtta ask.

;; More to do:
;;
;; 1.  [OK] Should not require the IMs in Makefile.  How to avoid?
;;     [SJT: we go through the generic Mule/LEIM interface.]
;; 2.  [OK] Quick-fix the window configuration (window-edges) stuff.
;; 3.  [OK -> More projects #5] Figure out the morphology rewriting 
;;     system and comment it up.
;; 4.  [OK] Change *edict-files* to allow cons'es of files and coding
;;     systems; default is autoconversion, of course(?)
;; 5.  Error handling on file I/O etc is rude.  Especially fix the
;;     `with-output-to-temp-buffer' stuff.
;; 6.  Why are there so many `edict-add-*' and `edict-insert-*' commands?
;; 7.  Move the docstring for `edict-init' back into the function.
;; 8.  [OK] Fix the bogus "found it" reports.
;; 9.  Need to handle JIS X 0201 (and so on) in the dictionary as well as
;;     in the key.
;; 10. [OK] Do something more user-friendly if there's no region for
;;     edict-search-kanji.
;; 11. The timing for missing/unreadable files and preregistered methods
;;     is bogus.  Think and fix.

;; More projects:
;;
;; 1.  "Upstream" and "external" package tracking.  Things like
;;     dictionaries should be tracked at their sources for updates.
;;     If you're going to have that capability, then why not the
;;     capability to do this for arbitrary packages?
;; 2.  If you're going to be automatically running off to do external
;;     package tracking, then automate sending the local dictionary to
;;     Jim Breen.  
;; 3.  Use VM's tapestry.el to track window configuration, or borrow
;;     from view-process-mode.el.
;; 4.  Bug report/feature request function.  (Use reporter.el, see
;;     view-process-mode.el for how.)
;; 5.  Make the morphology rewrite system make sense, and be consistent
;;     with the docs.
;; 6.  kanjidic support

;; Testing

;; edict.el commands

;; global-map
;  dui-invoke-search-method  "\C-c $ s"

;; via "\C-u\C-c$"
;  ispell-word (external)
;  edict-search-english
;  edict-search-kanji
;  edict-insert
;
;; edict-edit-mode-map
;  edict-standin              default
;  edict-exit                 "\C-c \C-c", "\C-x \C-s"
;  edict-tab                  "\t"
;  edict-new-entry            "\r"
;  edict-beginning-of-line    "\C-a"
;  edict-end-of-line          "\C-e"
;  edict-open-bracket         "["
;  edict-close-bracket        "]"
;  edict-slash                "/"

;; not bound
;  edict-version
;  edict-force-init
;  edict-insert
;  edict-insert-english
;  edict-insert-$BF|K\8l(B
;  edict-delete-matches-window
;  edict-edit-mode
;  edict-add-word
;  edict-add-english
;  edict-add-kanji

;; not fully implemented
;  edict-decircularize-rules
;  edict-circularize-rules

;;; Changelog:
;;      0.9.7          XEmacs-beta beta release
;;	0.9.6-sjt-0.1  Modifications provided by Steven Baur and Olivier
;;		       Galibert to get it to compile; the character
;;		       categories for Japanese are not implemented in XEmacs
;;		       so they are emulated via ranges in variables.
;;		       Some lisp-mnt.el compatiblity.
;;		       Changes in spacing, typos, etc, but not major
;;		       formatting.
;;		       Change format to comply with lisp-mnt.el
;;	0.9.6	       See ChangeLog.perham for history to this point.

;;; Code:

;; Require standard XEmacs packages.

(require 'cl)

;; Require edict support files

(require 'dui)				; method registry and history
(require 'edict-edit)			; edict-add-$language functions
(require 'edict-morphology)

;;; Variables:

(defvar edict-user-dictionary "~/.edict"
  "*This is the edict dictionary where the user's entries will be added.

May be a string (filename), or a cons of a filename and a symbol
(coding system).  Will be searched first in dictionary lookup.")

(defvar edict-dictionary-path
  (let (path)
    (dolist (dir (reverse package-path) path)
      (if (and dir			; handle nil components of package-path
	       (eq (car (file-attributes dir)) t))
	  (progn
	    ;; don't put the package root on this path
	    ;; (setq path (cons dir path))
	    (let ((file (expand-file-name "etc" dir)))
	      (if (eq (car (file-attributes file)) t)
		  (progn
		    (setq path (cons file path))
		    (let ((file (expand-file-name "etc/edict" dir)))
		      (if (eq (car (file-attributes file)) t)
			  (setq path (cons file path)))))))))))
  "Search path for edict dictionaries.

The default value is depth-first search into `etc/edict' for each
directory on the global `package-path'.")

(defvar edict-dictionaries '("edict")
  "*List of edict dictionary specifications.

A dictionary specification is either a string (file name), or a cons
of a file name and a symbol (coding system).  Relative paths are
searched for in each directory in edict-dictionary-path.

All dictionaries found are loaded into edict-buffer for searching.
Usually at least one of them should be the main edict file.  It is
redundant to include your private dictionary.

The auxiliary dictionaries enamdict (proper names) and kanjidic (kanji
database) may be used.  kanjidic is not supported and is probably not
useful.")

(defvar edict-buffer nil
  "The buffer containing the concatenated dictionaries.")

(defvar edict-buffer-name "*edict*"
  "The name of `edict-buffer'.")

;;The edict matches buffer and the name of it
(defvar edict-match-buffer-name "*edict matches*")
(defvar edict-match-buffer nil)

(defvar edict-version-date "980406 [$BJ?@.#1#0G/#47n#7F|(B($BLZ(B)]"
  "The variable edict-version-date contains a string with the
date when this version was released.  In both Swedish and Japanese
standards")

(defvar edict-version "0.9.7"
 "The variable edict-version contains a string that describes
 what version of the edict software that you are running.")

;; #### is this appropriate?
;;;###autoload
(defun edict-version ()
  "The function edict-version simply displays (as a message in the
mini-buffer) the version of the edict software that you are running
at the moment.  The same string is also returned from the function."
   (interactive)
   (message (concat "Edict version " edict-version  " of " edict-version-date)))

;; Marker so we can find the individual files in the buffer.
(defvar *edict-file-begin-marker* "<<<<<<<<<<<<<<<<")
(defvar *edict-file-end-marker* ">>>>>>>>>>>>>>>>")

;; This is the set of characters to be ignored in the middle of kanji
;; words being looked up.
;; The $B!:(B below should be $B!{(B, but there seems to be an off-by-one error
;; in the regexp code.
;; #### The comment above about "off-by-one" may be bogus as there are
;;	no less than three large circles in ku 1 and 2 of JIS X 0208.
;; #### The logic seems incorrect.  It is certainly an error to ignore the
;;	kanji and kana repetition marks (ku 1, ten 19-22,25; ## check if
;;	these are all!), probably wrong to ignore most punctuation,
;;	possibly wrong to ignore parentheses and quotation marks (these
;;      should mark word boundaries.
;; #### Probably this should be made conditional on a prefix arg,
;;	possibly with a customizable option to reverse the sense of
;;	the arg.

(defvar *edict-kanji-whitespace* "$B!!(B-$B!:!=(B-$B"`(B \n\t>;!:#?,.\"/@$B(!(B-$B(@(B")

;; This is the set of characters to be ignored in the middle of english
;; words being looked up.
;; #### That comment is misleading, since spaces should indicate word breaks.
;; The $B!:(B below should be $B!{(B, but there seems to be an off-by-one error
;; in the regexp code.
;; #### Maybe it's better to filter for `not-eigo'?  Check the code.

(defvar *edict-eigo-whitespace* "$B!!(B-$B!:!=(B-$B"`(B \n\t>;!:#?,.\"/@$B(!(B-$B(@(B")

;; #### This possibly is not correct as it will miss hyphenated words.
;; #### Can we just steal from ispell?
(defvar *edict-eigo-characters* "[A-Za-z$B#A(B-$B#Z#a(B-$B#z(B]"
  "These are the characters that eigo is made up of.")

;; #### These errors should be warnings.
(defvar *edict-unreadable-error*
  "Edict file \"%s\": doesn't exist or isn't readable!")

;(defvar *edict-non-existent-error*
;  "While loading edict files: \"%s\" doesn't exist!")

(defvar edict-default-coding-system 'automatic-conversion
  "Default coding system for reading dictionary files.

You probably shouldn't change this, but rather specify a coding system
in `edict-dictionaries' or file-coding-system-alist or use the Mule
`prefer-coding-system' function.")

(defconst edict-bad-dict-spec-cons
  "In edict-dictionaries: %s - car not string or cdr not coding-system.")

(defconst edict-bad-dict-spec
  "In edict-dictionaries: %s - not string or cons.")

(defvar edict-warn-missing-dictionaries-p t
  "Warn about dictionaries specified in edict-dictionaries but not found.")

(defvar edict-missing-dictionaries nil
  "List of dictionaries not found at initialization.")

(defvar edict-unreadable-files nil
  "List of dictionaries found at initialization but unreadable.")

(defun edict-regularize-file-argument (dict-spec)
  "Return dictionary specification in the form (FILE . CODING-SYSTEM).

Argument can be a file name (string) or a cons of a string and a coding
system.

Check for existence and readability of the file specified by the
string component of DICT-SPEC.  Return 'nil if not found and readable."
  (let (filename coding-system)
    (cond ((stringp dict-spec)
	   (setq filename dict-spec
		 coding-system edict-default-coding-system))
	  ((consp dict-spec)
	   (if (not (and (stringp (setq filename (car dict-spec)))
			 (coding-system-p
			  (setq coding-system
				(find-coding-system (cdr dict-spec))))))
	       ;; Just because one spec is in error doesn't mean they
	       ;; all are.  Tough.
	       ;; I'm too lazy to be user-friendly here.
	       (error edict-bad-dict-spec-cons dict-spec)))
	  (t (error edict-bad-dict-spec dict-spec)))
    (catch 'found
      (dolist (dir edict-dictionary-path nil)
	(let ((file (expand-file-name filename dir)))
	  (if (file-exists-p file)
	      (if (file-readable-p file)
		  (throw 'found (cons file coding-system))
		(setq edict-unreadable-files
		      (concat edict-unreadable-files filename "\n"))))))
      (setq edict-missing-dictionaries
	    (concat edict-missing-dictionaries filename "\n"))
      nil)))

(defvar edict-dictionaries-loaded nil
  "List of dictionaries loaded into the edict-buffer.")

;;Reads the edict files (the ones in the list edict-dictionaries) into a buffer
;; called what the string edict-buffer-name is set to.
;; #### I don't understand this function.
;  "Read the edict file into a buffer.

;The buffer's name is the value of *edict*.  The buffer itself is the
;value of edict-buffer."

;Normally initialization is done lazily, and only once.  Use the
;command edict-force-init to reread the edict files.  It is possible
;that Mule will incorrectly recognize the coding system in one or more
;dictionary files.  Customize the variable `file-coding-system-alist'
;(q.v.).  An entry of the form (FILE-REGEXP . CODING-SYS) is needed for
;each troublesome file.  For the main dictionary `edict' in EUC-JP
;format fresh from the Monash repository:  `(\"^edict$\" . euc-jp)'."
(defun edict-init ()

  ;;create a match buffer.
  (if (not (get-buffer edict-match-buffer-name))
    (setq edict-match-buffer (get-buffer-create
				edict-match-buffer-name)))

  ;;Check that we have a list, we will check that they are readable below.
  (if (not (listp edict-dictionaries))
      (error "The variable edict-dictionaries should be a list!"))

  ;;Create and read the edict files.
  (if (not (get-buffer edict-buffer-name))
    (progn
      (save-window-excursion
	;;First create the buffer and make it the current one
	(setq edict-buffer (get-buffer-create edict-buffer-name))
	(set-buffer edict-buffer)

	;;Read in the files from the list.
	(message "Reading the dictionaries.  This may take a while...")
	(mapcar (function
		 (lambda (arg)
		   (let* ((arg (edict-regularize-file-argument arg))
			  (filename (car arg))
			  (coding-system (cdr arg)))
                     (edict-add-file filename coding-system))))
		(if edict-user-dictionary
		    (cons edict-user-dictionary edict-dictionaries)
		  edict-dictionaries))
	;;If none of the files were readable, puke.
	(if (null edict-dictionaries-loaded)
	    (progn
	      (kill-buffer edict-buffer)
	      (error "No edict files found! Check value of edict-dictionaries.")))
	(message "Reading the dictionaries...done."))))
  t)

;;
;;
;;
;;;###autoload
(defun edict-force-init ()
  "Reread the edict files even if edict-buffer exists.

Useful when you have updated the edict-dictionaries variable or corrupted
the edict buffer."
  (interactive)
  (setq edict-dictionaries-loaded nil)
  (kill-buffer edict-buffer)
  (edict-init))

;;
;; Add file filename to the current buffer with the begin end markers around that file...
;;
(defun edict-add-file (filename coding-system)
  "Add FILENAME to the current buffer using CODING-SYSTEM.
*edict-file-begin-marker* and *edict-file-end-marker* are placed around
the file contents.

If FILENAME is nil, do nothing (cf. edict-regularize-file-argument)."
  (if (not filename)
      nil
    (goto-char (point-max))
    (insert (format "%s %s\n" *edict-file-begin-marker* filename))
    (let ((pos (point)))
      (let ((coding-system-for-read coding-system))
	(insert-file-contents filename))
      (goto-char (point-max))
      (insert (format "%s %s\n" *edict-file-end-marker* filename))
      (goto-char pos)
      ;; #### Huh?  Unprintable characters in dictionary names?  and
      ;;	    why not allow Japanese?  Ask Jim Breen.
      (when (looking-at "$B!)!)!)!)(B /\\([ -.0-\177]+\\)/")
	(message "Loaded dictionary %s."
		 (buffer-substring (match-beginning 1) (match-end 1))))
      (goto-char (point-max))
      (setq edict-dictionaries-loaded
	    (append edict-dictionaries-loaded (list filename))))))

;; Remove any leading, trailing, or embedded whitespace or other noise
;; characters (such as the inserted ">" etc. used to denote inserted
;; quotations in mail and news)
;; #### Supercite will hose that last!	Can we borrow from filladapt?

(defun edict-clean-up-kanji (key)
  (let ((start 0)
	(loc 0)
	(end (length key))
	(result "")
	(pattern (concat "[" *edict-kanji-whitespace* "]+")))
    (while (and (< start end) (setq start (string-match pattern key start)))
      (setq result (concat result (substring key loc start)))
      (setq loc (setq start (match-end 0))))
    (concat result (substring key loc))))

;; #### Why isn't this a defconst?  Why strings and not characters?
(defvar *edict-romaji-remaps* nil)
(setq *edict-romaji-remaps* 
      '(("$B#a(B" . "a") ("$B#b(B" . "b") ("$B#c(B" . "c") ("$B#d(B" . "d") ("$B#e(B" . "e") ("$B#f(B" . "f") ("$B#g(B" . "g")
	("$B#h(B" . "h") ("$B#i(B" . "i") ("$B#j(B" . "j") ("$B#k(B" . "k") ("$B#l(B" . "l") ("$B#m(B" . "m")
	("$B#n(B" . "n") ("$B#o(B" . "o") ("$B#p(B" . "p") ("$B#q(B" . "q") ("$B#r(B" . "r") ("$B#s(B" . "s") ("$B#t(B" . "t")
	("$B#u(B" . "u") ("$B#v(B" . "v") ("$B#w(B" . "w") ("$B#x(B" . "x") ("$B#y(B" . "y") ("$B#z(B" . "z")
	("$B#A(B" . "A") ("$B#B(B" . "B") ("$B#C(B" . "C") ("$B#D(B" . "D") ("$B#E(B" . "E") ("$B#F(B" . "F") ("$B#G(B" . "G")
	("$B#H(B" . "H") ("$B#I(B" . "I") ("$B#J(B" . "J") ("$B#K(B" . "K") ("$B#L(B" . "L") ("$B#M(B" . "M")
	("$B#N(B" . "N") ("$B#O(B" . "O") ("$B#P(B" . "P") ("$B#Q(B" . "Q") ("$B#R(B" . "R") ("$B#S(B" . "S") ("$B#T(B" . "T")
	("$B#U(B" . "U") ("$B#V(B" . "V") ("$B#W(B" . "W") ("$B#X(B" . "X") ("$B#Y(B" . "Y") ("$B#Z(B" . "Z")))

;;
;; Lookup a mapping for zenkaku roman characters to ASCII.
;; #### Wouldn't this be better done with assoc, if necessary with some
;;	type-checking on the args?
;;
(defun edict-in-remap-list (item list)
  "Return first link in LIST whose car is `equal' to ITEM."
  (let ((ptr list)
	(done nil)
	(result '()))
    (while (not (or done (endp ptr)))
      (cond ((string= item (car (car ptr)))
	     (setq done t)
	     (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

;;
;; Remap zenkaku roman characters to ASCII.
;;
(defun edict-remap-romaji (eigo-string)
  (let ((stop (length eigo-string))
	(current 0)
	(match nil)
	(result ""))
    (while (< current stop)
      (if (<  (+ 1 current) stop)
	(setq match (edict-in-remap-list (substring eigo-string current (+ 2 current)) *edict-romaji-remaps*))
	(setq match nil))
      (if match
	(progn
	  (setq result (concat result (cdr (car match))))
	  (setq current (+ 2 current)))
	(progn
	  (setq result (concat result (substring eigo-string current (1+ current))))
	  (setq current (1+ current)))))
    result))

;;
;;  Eliminate extra whitespace, newlines, punctuation, etc. which would
;;  interfere with our dictionary lookup.
;;
(defun edict-clean-up-eigo (key)
  (let ((start 0)
	(loc 0)
	(end (length key))
	(result "")
	(pattern (concat "[" *edict-eigo-whitespace* "]+")))
    (while (and (< start end)
		(setq start (string-match pattern key start)))
      (setq result (concat result (substring key loc start) " "))
      (setq loc	 (setq start (match-end 0))))

    (setf result (concat result (substring key loc)))

    (edict-remap-romaji result)))

;;
;;  slightly specialized function to be changed when the real backward
;;  word things are included.
;;
(defun edict-eigo-one-word (direction)
  "The function edict-eigo-one-word goes one word forward (direction > 0)
or backward (direction <= 0).  It assumes that it is looking at a word
when invoked.  It returns the point either at the beginning of a word or
at the whitespace after a word."
  (let ((stop-point (point))
	(stop nil))
    (if (> direction 0)
      ;;forward
      (progn
	(while (not stop)
	  (setq stop-point (point))
	  (if (< (point) (point-max))
	    (if (looking-at *edict-eigo-characters*)
	      (forward-char 1)
	      (setq stop t))
	    (setq stop t))))
      ;;backward
      (progn
	(while (not stop)
	  (setq stop-point (point))
	  (if (> (point) (point-min))
	    (if (looking-at *edict-eigo-characters*)
	      (backward-char 1)
	      (progn
		(setq stop t)
		(forward-char 1)
		(setq stop-point (point))))
	    (setq stop t )))))
    stop-point))
    

;;
;; perham
;;
(defun edict-find-word-at-point ()
  "Find an English word close to or behind point.

If it does not find any word it reports an error."
  (let (start end)

    ;; Move backward for word if not already on one.
    (if (not (looking-at *edict-eigo-characters*))
      (re-search-backward *edict-eigo-characters* (point-min) 'stay))

    (if (looking-at *edict-eigo-characters*)
      (progn
	(setq start (edict-eigo-one-word -1))
	(setq end   (edict-eigo-one-word 1))
	
	(edict-clean-up-eigo (buffer-substring start end)))
      (error "Can't find English word!")
      )))

;;
;;
;;
;;;###autoload
(defun edict-search-english (arg)
  "Attempts to translate the english word we are looking at. Picks the word 
in the same way as ispell, ie backs up from whitespace, and then expands.

Result is presented in a window that is not selected. Clear the window by
using a negative prefix argument.

If given an argument, adds an english word to the private dictionary."

  (interactive "P")
  (if arg
      (if (< (prefix-numeric-value arg) 0)
	  (edict-restore-display)
	(edict-add-english))
    (let ((word (edict-get-english-word)))
      ;;Search if there is a word.
      (when word
	(edict-search-and-display word 'english)))))

;; Return the english word, or nil
(defun edict-get-english-word ()
  (let (word real-word)

    ;;Find the word
    (setq word (edict-find-word-at-point))

    ;;ask the user if this is really the word that is interesting.
    (setq real-word (read-string
		     (format "Translate word (default \"%s\"): "
			     word)))
    (setq real-word (edict-clean-up-eigo real-word))
    (if (equal real-word "")
	(if (equal word "")
	    nil
	  word)
      real-word)))

;;
;;
;;
;;;###autoload
(defun edict-search-kanji (arg min max)
  "Attempts to translate the Kanji sequence between mark and point.

Result is presented in a window that is not selected. Clear the window
with for instance C-X 1

Given a numeric argument, this adds the Kanji sequence to the user's
private dictionary.

If all searches fail, initialization may be bogus.  See the documentation
for `edict-init'."

  ;;Interactive, with a region as argument
  (interactive "P
r")

  ;;make sure that the dictionary is read
  (edict-init)

  (if arg
      (if (< (prefix-numeric-value arg) 0)
	  (edict-restore-display)
	(edict-add-kanji min max))
    (let ((word (edict-clean-up-kanji (buffer-substring min max))))
      (if (equal word "")
	  (error "No word to search for!")
	(edict-search-and-display word '$BF|K\8l(B))))
  t)

;;
;;
;;
(defun edict-copy-of-current-line ()
  "Copy-of-current-line creates and returns a copy of the line
where point is. It does not affect the buffer it is working on,
except for moving the point around.

It leaves the point at the end of the line, which is fine for this
application."

  ;;Find the start and end of the current line
  (let ((line-start (progn (beginning-of-line) (point)))
	(line-end   (progn (end-of-line) (point))))

    ;;return a copy of his line, perham, is there something that
    ;; should be tested here?
    (buffer-substring line-start line-end)))


;;
;;
;;
(defun edict-search (key buffer)
  "Searches the edict-buffer and returns a list of strings that are
the matches.

If there are no matches this string will be nil."

  ;;perham, should this really go here? Or what should we have? Look
  ;;at ispell.el...
  (save-window-excursion
    (message (format "Searching for word \"%s\"..." key))
    (let ((match-list nil))
      ;;select the database and goto to the first char
      (set-buffer buffer)
      (goto-char (point-min))
      ;;Search for lines that match the key and copy the over to the
      ;; match buffer.
      (while (edict-search-key key)
	(setq match-list (union match-list (list (edict-copy-of-current-line)))))
      match-list)))

(defun edict-search-key (key)
  (search-forward			;Ken-ichi says that one cannot
					;use the re-search-forward
					;function with actually having
					;some reg exp in the target string.
					;(concat "[\[/ 
					;]" key "[\]/ ]")
   key nil t))

;;
;;
;;

(defvar *edict-previous-configuration* nil)

(defun edict-note-windows ()
  (or *edict-previous-configuration*
      (setq *edict-previous-configuration* (current-window-configuration))))

;; This doesn't work yet; leave it set to 'top!
(defvar *edict-window-location* 'top
  "*Location to place edict matches window.  top or bottom.
Doesn't work yet.")

(defun edict-display (key-list match-list)
  "Edict-display displayes the strings in a separate window that is
not selected."
  (let* ((text-window (get-buffer-window (current-buffer)))
	 (edict-window (get-buffer-window edict-match-buffer))
	 ;; We have available some of this window's height plus any we've already
	 ;; already gotten.
	 (avail-height (+ (window-height text-window)
			  (if edict-window
			      (window-height edict-window)
			    0)))
	 ;; We limit the height to half of what's available, but no more than we need,
	 ;; and no less than window-min-height.  We must remember to include 1 line for
	 ;; the mode-line in our minimum figure.
	 (height (min (max window-min-height (+ (length match-list) 1))
		      (/ avail-height 2))))
    (if (not edict-window)
	(progn
	  ;; We don't have a window, so remember our existing configuration,
	  ;; and either find an acceptable window to split, or use the current
	  ;; window.
	  (edict-note-windows)
	  (let ((use-window (edict-find-acceptable-window text-window)))
	    (if use-window
		(setq edict-window use-window
		      text-window (split-window text-window height))
	      (setq edict-window text-window))))
      ;; We have a window already.  Just adjust its size appropriately.
      (unless (equal height (window-height edict-window))
	(let ((selected (selected-window)))
	  (select-window edict-window)
	  (enlarge-window (- height (window-height edict-window)))
	  (select-window selected))))
    (set-buffer edict-match-buffer)
    (let ((min (point-min)))
      ;; Replace everything.
      (erase-buffer)
      (mapcar (function (lambda (string-item)
			  (insert string-item)
			  (newline)))
	      match-list)
      (when (eq *edict-window-location* 'bottom)
	(let ((w text-window))
	  (setq text-window edict-window
		edict-window w)))
      ;; OK, now let's move the exact matches to the top.
      (goto-char min)
      ;; Be careful to preserve the order.
      ;; An exact match is any of "^key ", "[key]", "/key/", or "/to key/".
      (dolist (key (reverse key-list))
	(let* ((pattern (concat "^" key " \\|\\[" key "\\]\\|\\/" key
				"\\/\\|\\/to " key "\\/" ))
	       (top-lines nil))
	  ;; First pull them out of the buffer into a list (top-lines).
	  ;; Then re-insert them at the top.
	  (while (re-search-forward pattern nil t)
	    (forward-line 0)
	    (let ((p (point)))
	      (forward-line 1)
	      (push (buffer-substring p (point)) top-lines)
	      (delete-region p (point))))
	  (goto-char min)
	  (mapcar 'insert top-lines)))
      ;; OK, display it all.
      (select-window text-window)
      (set-window-buffer edict-window edict-match-buffer)
      (set-window-start edict-window min)))
  t)

;; Find a window which is of acceptable size to split.
;; It must be at least twice window-min-height.
(defun edict-find-acceptable-window (window)
  (catch 'no-window
    (let ((new-window window))
      (while (< (window-height new-window) (* 2 window-min-height))
	(setq new-window (next-window new-window))
	(when (eq new-window window)
	  (throw 'no-window nil)))
      new-window)))

;; Try to put the display back the way it was before showing matches.
(defun edict-restore-display ()
  "Remove the edict windows."
  (when *edict-previous-configuration*
    (set-window-configuration *edict-previous-configuration*))
  (setq *edict-previous-configuration* nil)
  t)

;; Variables to remember the last insertion of a match into our
;; buffer, for later replacement.

(defvar edict-last-language nil)
(defvar edict-insert-last-start)
(defvar edict-insert-last-end)

;;
;;
;;
(defun edict-search-and-display (key &optional from-language)
  "Edict-search-and-display searches for matches to the argument key.
If there are any matches these are displayed in a window that is not
selected. This window can be removed with C-X 1."
  (edict-init)
  ;; Remember the last language looked up, so edict-insert can pick the
  ;; right one.
  (setq edict-last-language from-language)
  (save-excursion
    (let ((match-list nil)
	  (one-char-keys nil)
	  (key-list (edict-expand-string key () () (or from-language '$BF|K\8l(B))))
      ;; Sort them into the order we'd like exact matches to appear.
      (setq key-list (sort key-list (function (lambda (x y)
						(let ((lx (length x))
						      (ly (length y)))
						  (if (= lx ly)
						      (string-lessp x y)
						    (> lx ly)))))))
      ;; For all the possibilities
      (dolist (key key-list)
	;; Search for matches.  We exlude any one-character keys on
	;; the theory that they're likely to be uninteresting
	;; fragments.
	;; #### This is a strange way to do this test.  What
	;;	are we thinking?
	(if (string-match "^[$B!"(B-$Bt$(B]$" key) ;1 char
	    (push key one-char-keys)
	  (setq match-list (union match-list (edict-search key edict-buffer)))))
      ;; If we didn't get anything, we can try including the one-char keys.
      (or match-list
	  (dolist (key one-char-keys)
	    (setq match-list (union match-list
				    (edict-search key edict-buffer)))))
      ;; #### I don't understand the logic of this whole function.
      (if (not match-list)
	  (progn
	    (edict-delete-matches-window)
	    ;; This probably didn't need to be an error....
	    (message "No matches for key \"%s\"." key))
	(edict-display key-list match-list)
	(message "Found it!")))))

(defun edict-insert (arg)
  "Insert the last value looked up at the current position.  If repeated,
replace with the next possibility.  If given an argument N, use the
Nth possibility.  Inserts in the opposite language from what was looked up,
unless the argument is negative."
  (interactive "P")
  ;; If we were given a negative argument, we need to switch languages.
  (cond ((null arg))
	((> (prefix-numeric-value arg) 0))
	(t (case arg
	     (- (setq arg nil))
	     (otherwise (setq arg (list (- (prefix-numeric-value arg))))))
	   (setq edict-last-language
		 (ecase edict-last-language
		   (english '$BF|K\8l(B)
		   ($BF|K\8l(B 'english)))))
  (ecase edict-last-language
    (english (edict-insert-$BF|K\8l(B arg))
    ($BF|K\8l(B (edict-insert-english arg))))

(defun edict-insert-english (arg)
  "Insert the last english word looked up at the current position.
If repeated, replace with the next possibility.  If given an argument N,
use the Nth possibility."
  (interactive "P")
  (or edict-match-buffer
      (error "You must first look up a word."))
  (let ((value nil))
    (save-excursion
      (set-buffer edict-match-buffer)
      ;; If we're going to a specific one, always count from the beginning.
      (when arg
	(goto-char (point-min)))
      ;; If the last command was this, then we're going on to the next possibility.
      ;; Otherwise, start at the beginning.
      (case last-command
	(edict-insert-english)
	(t (goto-char (point-min))))
      ;; Seach forward for /<definitition>/  If we don't find one, start over from the
      ;; beginning.
      (unless (re-search-forward "/\\([^/\n]+\\)/" (point-max) t (prefix-numeric-value arg))
	(goto-char (point-min))
	(unless (or arg
		    (re-search-forward "/\\([^/\n]+\\)/" (point-max) t))
	  (error "No match numbered %d found." (prefix-numeric-value arg))))
      ;; Extract the match.  Leave ourselves just before the final /,
      ;; so if it starts a new definition, we'll find it.
      (goto-char (match-end 1))
      (setq value (buffer-substring (match-beginning 1) (match-end 1))))
    ;; If we inserted one of our languages, then we should delete the old
    ;; one first.
    (case last-command
      ((edict-insert-english edict-insert-$BF|K\8l(B)
       (delete-region edict-insert-last-start edict-insert-last-end)))
    ;; Insert, remembering where we did it, so it can be replaced if we
    ;; repeat the command.
    (setq edict-insert-last-start (point-marker))
    (insert value)
    (setq edict-insert-last-end (point-marker)))
  ;; Remember this as the last command, not edict-insert.
  (setq this-command 'edict-insert-english)
  t)

(defun edict-insert-$BF|K\8l(B (arg)
  "Insert the last $BF|K\8l(B word looked up at the current position.
If repeated, replace with the next possibility.  If given an argument N,
use the Nth possibility."
  (interactive "P")
  (or edict-match-buffer
      (error "You must first look up a word."))
  (let ((value nil))
    (save-excursion
      (set-buffer edict-match-buffer)
      ;; If we're going to a specific one, always count from the beginning.
      (when arg
	(goto-char (point-min)))
      ;; If the last command was this, then we're going on to the next possibility.
      ;; Otherwise, start at the beginning.
      (case last-command
	(edict-insert-$BF|K\8l(B)
	(t (goto-char (point-min))))
      ;; Seach forward for a word at the start of a line.  If we don't find one,
      ;; start over from the beginning.
      (unless (re-search-forward edict-dictionary-entry-start-regexp
				 (point-max) t (prefix-numeric-value arg))
	(goto-char (point-min))
	(unless (or arg
		    (re-search-forward edict-dictionary-entry-start-regexp
				       (point-max) t))
	  (error "No match numbered %d found." (prefix-numeric-value arg))))
      (goto-char (match-end 1))
      (setq value (buffer-substring (match-beginning 1) (match-end 1))))
    ;; If we inserted one of our languages, then we should delete the old
    ;; one first.
    (case last-command
      ((edict-insert-$BF|K\8l(B edict-insert-english)
       (delete-region edict-insert-last-start edict-insert-last-end)))
    ;; Insert, remembering where we did it, so it can be replaced if we
    ;; repeat the command.
    (setq edict-insert-last-start (point-marker))
    (insert value)
    (setq edict-insert-last-end (point-marker)))
  ;; Remember this as the last command, not edict-insert.
  (setq this-command 'edict-insert-$BF|K\8l(B)
  t)

;; Remove the matches window from the screen.
;; This is harder than you'd think.
;; (SJT - if you try to be overly intelligent about it....)
(defun edict-delete-matches-window ()
  (interactive)
  (let ((window (get-buffer-window edict-match-buffer)))
    (when window
      ;; SJT: `window-edges' doesn't seem to exist under XEmacs.  In
      ;; any case, I don't particularly see why it makes sense to
      ;; split the space among several windows.
      (if (featurep 'xemacs)
	  (delete-window window)
	;; #### The following code is _not_ known to work in recent FSF Emacs :-(
	(let* ((selected (selected-window))
	       (next (previous-window window))
	       (height (window-height window))
	       (nedges (window-edges next))
	       (tedges (window-edges window)))
	  (delete-window window)
	  ;; The following is sheer magic.  Deleting a window is not
	  ;; an inverse to splitting a window.  The space is returned
	  ;; not to the window below, OR to the window above, but
	  ;; rather is divided between them.
	  (when (and (equal (car nedges) (car tedges))
		     (< (car (cdr nedges)) (car (cdr tedges))))
	    (select-window next)
	    (shrink-window (/ (- height 1) 2))
	    (select-window selected)))))))

;; #### This can't possibly work, since dictionary loading does not
;;      take place at library load time.  Move the relevant clauses to
;;      `edict-init'.
(if (or edict-unreadable-files
	edict-missing-dictionaries)
    (with-output-to-temp-buffer "*edict load warnings*"
      (if edict-unreadable-files
	  (progn
	    (princ "The following files were found but are unreadable.
This is probably an error.
")
	    (princ edict-unreadable-files)
	    (setq edict-unreadable-files nil)))
      (if (and edict-warn-missing-dictionaries-p
	       edict-missing-dictionaries)
	  (progn
	    (princ "The following dictionaries were not found on the search path.
")
	    (princ edict-missing-dictionaries)
	    (setq edict-missing-dictionaries nil)))))

;; Load morphology rewrite engine and grammar rules
;; This can be done a lot more lazily
(require 'edict-english)
(require 'edict-japanese)

(provide 'edict)

;;; edict.el ends here
