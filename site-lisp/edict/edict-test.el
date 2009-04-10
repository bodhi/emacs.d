;;;;;; Copyright (C) 1992 Bob Kerns <rwk@crl.dec.com>
;;;
;;;
;;;   This program is free software; you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation; either version 1, or (at your option)
;;;   any later version.  ;;; 
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.  ;;; 
;;;   You should have received a copy of the GNU General Public License
;;;   along with this program; if not, write to the Free Software
;;;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  ;;; 

;;; Test suite for morphology rules for edict.el.
;;; To run the tests, load this file, and do m-X edict-perform-tests.
;;; This will create an *EDICT-TESTS* buffer with the results.


(require 'cl)

;;; This should exist, but doesn't.  See edict.install for the
;;; compiler half of this.  You should be sure to load the same
;;; hacks into your compiler if you compile this by hand, or you
;;; won't get it byte compiled.

;(defmacro eval-when (when &rest forms)
;  (and (or (member 'eval when)
;	    (member ':execute when))
;       (mapcar (function eval) forms))
;  (and (or (member 'load when)
;	   (member ':load-toplevel when))
;       (cons 'progn forms)))

;;; first, a couple simple tests.

(defun edict-test-string (flag string)
  "Show what strings will be searched for a test string.  If given a prefix arg,
traces step by step; type Return for each new step."
  (interactive "P
sTest string: ")
  (let ((*edict-expand-string-trace* flag))
    (message (format "%s" (edict-expand-string string)))))

(defun edict-test-rule (rule-name string)
  (interactive "SRule name: 
sTest string: ")
  (let ((rule (edict-get-rule rule-name)))
    (unless rule (error "There is no rule named '%s'" rule-name))
    (unless (string-match (edict-rule-pattern rule) string)
      (error "The rule %s does not match '%s'." rule-name string))
    (apply (edict-rule-function rule) string
	   (edict-rule-additional-args rule))))

(eval-when (eval load compile)
(defstruct edict-test
  word					; Word to be tested.
  should-have				; Expansions that should be found
  should-not-have			; Expansions that should not be found.
  from-syntax-types
  to-syntax-types)
)

(defvar *edict-tests* nil)

(defun remove-edict-test (name)
  (let ((test (get-edict-test name)))
    (setq *edict-tests* (delq test *edict-tests*))))

(defun add-edict-test (test)
  ;; Preserve the order of the tests.
  (let* ((name (edict-test-word test))
	 (old (get-edict-test name)))
    (if old
	(setf (edict-test-should-have old) (edict-test-should-have test)
	      (edict-test-should-not-have old) (edict-test-should-not-have test)
	      (edict-test-from-syntax-types old)
	      (edict-test-from-syntax-types test)
	      (edict-test-to-syntax-types old)
	      (edict-test-to-syntax-types test))
      (setq *edict-tests* (append *edict-tests* (list test))))))

(defun get-edict-test (name)
  (if (symbolp name)
      (setq name (symbol-name name)))
  (catch 'found-it
    (dolist (test *edict-tests*)
      (if (equal (edict-test-word test) name)
	  (throw 'found-it test)))))

(defmacro deftest (case &optional fromto should-have should-not-have not-self)
  (` (define-edict-test '(, case) '(, (first fromto)) '(, (second fromto))
       '(, should-have) '(, should-not-have) '(, not-self))))

(defun define-edict-test (name from to should-have should-not-have
			       &optional not-self)
  (if (symbolp name)
      (setq name (symbol-name name)))
  (unless (listp from)
    (setq from (list from)))
  (unless (listp to)
    (setq to (list to)))
  (unless from
    (setq from '($BF|K\8l(B)))
  (let ((f (function (lambda (x)
		       (if (symbolp x)
			   (symbol-name x)
			 x)))))
    (setq should-have (mapcar f should-have))
    (setq should-not-have (mapcar f should-not-have))
    (or not-self (member name should-have)
	(push name should-have))
    (add-edict-test (make-edict-test :word name
				     :should-have should-have
				     :should-not-have should-not-have
				     :from-syntax-types from
				     :to-syntax-types to)))
  name)

;;; This should be in emacs, but it isn't.
;;; (Borrowed from ilisp.el, where I inherited it accidentally).

(defun edict-del (item list &optional test)
  "Delete ITEM from LIST using TEST comparison and return the result.
Default test is equal."
  (let ((test (or test (function equal)))
	(element list)
	(prev nil)
	(done nil))
    (while (and element (not done))
      (if (funcall test item (car element))
	  (progn
	    (setq done t)
	    (if prev
		(rplacd prev (cdr element))
		(setq list (cdr list))))
	  (setq prev element
		element (cdr element))))
    list))


(defun edict-test (test)
  (if (or (symbolp test) (stringp test))
      (setq test (get-edict-test test)))
  ;; Cleaning up the kanji shouldn't break anything;
  ;; give it a chance to do so if it's buggy.
  (let* ((name (edict-test-word test))
	 (word (edict-clean-up-kanji name))
	 (from-syntax-types (edict-test-from-syntax-types test))
	 (to-syntax-types (edict-test-to-syntax-types test))
	 (should-have (edict-test-should-have test))
	 (should-not-have (edict-test-should-not-have test)))
    (let* ((expansion (edict-expand-string-syntaxes word () () from-syntax-types))
	   (save-expansion expansion)
	   (failed nil))
      (dolist (sh should-have)
	(if (member sh expansion)
	    (setq expansion (edict-del sh expansion (function equal)))
	  (progn 
	    (princ (format ";%s: did not produce %s - %S\n" name sh save-expansion))
	    (setq failed t))))
      (dolist (case should-not-have)
	(and (member case expansion)
	     (progn
	       (princ (format ";%s: Should not have %s as expansion.\n"
			      name case))
	       (setq failed t)
	       (setq expansion (edict-del sh expansion (function equal))))))
      (dolist (bad expansion)
	(princ (format ";%s: Unexpected expansion: %s\n" name bad))
	(setq failed t))
      (or failed
	  (princ (format ";%s: OK\n" name)))
      (not failed))))

(defun edict-perform-tests ()
  (interactive)
  (let ((test-buffer (get-buffer-create "*EDICT-TESTS*"))
	(failures 0)
	(first-failure nil))
    (set-buffer test-buffer)
    (set-window-buffer (selected-window) test-buffer)
    (delete-region (point-min) (point-max))
    (let ((standard-output test-buffer))
      (dolist (test *edict-tests*)
	(let ((msg-point (point)))
	  (cond ((not (edict-test test))
		 (incf failures)
		 (or first-failure (setq first-failure msg-point))))
	  (sit-for 0))))
    (cond ((= failures 0)
	   (message "Done.  All Tests OK."))
	  ((= failures 1)
	   (message "1 test failed."))
	  (t (message (format "%d tests failed." failures))))
    (goto-char (or first-failure (point-min)))))

(defun edict-run-test (arg)
  "Execute the test that point is in or before.
Print value in minibuffer.
With argument, insert value in current buffer after the defun.
With argument >= 16 (i.e. c-U c-U), single-step through the expansion process."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point))
	  (*edict-expand-string-trace* (and arg (> (prefix-numeric-value arg) 4))))
      (beginning-of-defun)
      (let* ((test-form (read (current-buffer)))
	     (test-name (second test-form))
	     (test))
	(eval test-form)
	(setq test (get-edict-test test-name))
	(forward-line 1)
	(while (looking-at (concat ";" (symbol-name test-name)
				   ": \\(Unexpected expansion: \\|did not produce \\|OK$\\)"))
	  (let ((start (point)))
	    (forward-line 1)
	    (delete-region start (point))))
	(let ((standard-output (if arg (current-buffer) standard-output)))
	  (edict-test test)))))
  t)

;(global-set-key "\e_" 'edict-run-test)

;;; **** NOTE WELL ****
;;; The proper test results here are not necessarily valid words.
;;; These are words which are MORPHOLOGICALLY correct.  That is,
;;; this reverse-chains on the possible rules to produce a given
;;; word, generally only one or two of which would actually be
;;; correct.

;;; Also note that these are regression tests.  No distinction is being
;;; made between results which are "correct" and results which are
;;; "acceptable".  In general, we accept spurious expansions if they
;;; lead to including desirable results in other cases.  Modifying the
;;; rule set may either result in eliminating spurious expansions (resulting
;;; in missing expansions from the tests) or adding new spurious expansions.
;;; In case of problems from these tests, the offending test should be single-stepped
;;; (with c-u c-u m-X edict-run-test), and the reasons for the expansion should be
;;; evaluated.  If, after careful consideration, the modified result is regarded
;;; as correct, the test should be modified accordingly.  Otherwise, the bug should
;;; be fixed.

;;; Be careful.  Regression tests are good for considering all the effects of
;;; a change, but they do not themselves determine the correctness of a change.
;;; When the regression tests determine that something has changed, it is up
;;; to YOU to be careful and determine the correct result.

(deftest "$BGc$&(B "
  ()
  ($BGc$&(B)
  ()
  :not-self)

(deftest "
$B!d!!Gc!t(B#>!$B!*!'(B:$B$&(B	 "
  ()
  ($BGc$&(B)
  ()
  :not-self)

;;; The basics: $B8^CJF0;l(B
(deftest $BGc$&(B
  ())
(deftest $B9T$/(B
  ()
  ($B9T$$(B))				;Looks like it could be an adverb
;$B9T$/(B: OK
(deftest $B1K$0(B
  ())
(deftest $BOC$9(B
  ())
(deftest $BBT$D(B
  ())
(deftest $B;`$L(B
  ())
(deftest $B8F$V(B
  ())
(deftest $BFI$`(B
  ())
(deftest $BJ,$+$k(B
  ())
(deftest $B@.$k(B
  ())
;;; $B0lCJF0;l(B
(deftest $B@8$-$k(B
  ())
(deftest $B8+$k(B
  ())

;;; Distal style
;;; These all produce the improbable but possible result of removing only the
;;; masu and adding $B$k(B as if it were a $B0lCJF0;l(B, since the result of that situation
;;; would look the same.

(deftest $BGc$$$^$9(B
  ()
  ($BGc$&(B $BGc$$$k(B))
(deftest $BCV$-$^$9(B
  ()
  ($BCV$/(B $BCV$-$k(B))
;$BCV$-$^$9(B: OK
(deftest $B1K$.$^$9(B
  ()
  ($B1K$0(B $B1K$.$k(B))
(deftest $BOC$7$^$9(B
  ()
  ($BOC$9(B $BOC$7$k(B $BOC$9$k(B $BOC(B))
(deftest $B;}$A$^$9(B
  ()
  ($B;}$D(B $B;}$A$k(B))
(deftest $B;`$K$^$9(B
  ()
  ($B;`$L(B $B;`$K$k(B))
(deftest $B8F$S$^$9(B
  ()
  ($B8F$V(B $B8F$S$k(B))
(deftest $BFI$_$^$9(B
  ()
  ($BFI$`(B $BFI$_$k(B))
(deftest $BJ,$+$j$^$9(B
  ()
  ($BJ,$+$k(B $BJ,$+$j$k(B))
(deftest $B@.$j$^$9(B
  ()
  ($B@.$k(B $B@.$j$k(B))
(deftest $B@8$-$^$9(B
  ()
  ($B@8$-$k(B $B@8$/(B))
;$B@8$-$^$9(B: OK
(deftest $B8+$^$9(B
  ()
  ($B8+$k(B))


;;; Irregulars

(deftest $BMh$^$9(B
  ()
  ($BMh$k(B))
(deftest $B$-$^$9(B
  ()
  ($B$/$k(B $B$-$k(B))
(deftest $B$7$^$9(B
  ()
  ($B$9$k(B $B$7$k(B))

(deftest $BGc$$$^$;$s(B
  ()
  ($BGc$&(B $BGc$$$k(B))
(deftest $BCV$-$^$;$s(B
  ()
  ($BCV$/(B $BCV$-$k(B))
;$BCV$-$^$;$s(B: OK
(deftest $B1K$.$^$;$s(B
  ()
  ($B1K$0(B $B1K$.$k(B))
(deftest $BOC$7$^$;$s(B
  ()
  ($BOC$9(B $BOC$7$k(B $BOC$9$k(B $BOC(B))
(deftest $B;}$A$^$;$s(B
  ()
  ($B;}$D(B $B;}$A$k(B))
(deftest $B;`$K$^$;$s(B
  ()
  ($B;`$L(B $B;`$K$k(B))
(deftest $B8F$S$^$;$s(B
  ()
  ($B8F$V(B $B8F$S$k(B))
(deftest $BFI$_$^$;$s(B
  ()
  ($BFI$`(B $BFI$_$k(B))
(deftest $BJ,$+$j$^$;$s(B
  ()
  ($BJ,$+$k(B $BJ,$+$j$k(B))
(deftest $B@.$j$^$;$s(B
  ()
  ($B@.$k(B $B@.$j$k(B))
(deftest $B@8$-$^$;$s(B
  ()
  ($B@8$-$k(B $B@8$/(B))
;$B@8$-$^$;$s(B: OK
(deftest $B8+$^$;$s(B
  ()
  ($B8+$k(B))


;;; Irregulars

(deftest $BMh$^$;$s(B
  ()
  ($BMh$k(B))
(deftest $B$-$^$;$s(B
  ()
  ($B$/$k(B $B$-$k(B))
(deftest $B$7$^$;$s(B
  ()
  ($B$9$k(B $B$7$k(B))


;;; Past tense

(deftest $BGc$C$?(B
  ()
  ($BGc$&(B $BGc$D(B $BGc$k(B))
(deftest $BCV$$$?(B
  ()
  ($BCV$/(B $BCV$$$k(B))
;$BCV$$$?(B: OK
(deftest $B9T$C$?(B
					();iku is irregular It looks like a $B$k(B/$B$D(B/$B$&(B.
  ($B9T$/(B $B9T$$(B $B9T$&(B $B9T$D(B $B9T$k(B))
;$B9T$C$?(B: OK
(deftest $BOC$7$?(B
  ()
  ($BOC$9(B $BOC$7$k(B $BOC$9$k(B $BOC(B))
;$BOC$7$?(B: OK
(deftest $B;}$C$?(B
  ()
  ($B;}$D(B $B;}$&(B $B;}$k(B))
(deftest $B;`$s$?(B
					();Don't mis-interpret
  ()
  ($B;`$L(B))
(deftest $B;`$s$@(B
  ()
  ($B;`$L(B $B;`$V(B $B;`$`(B $B;`$s(B))
;$B;`$s$@(B: OK
(deftest $B8F$s$@(B
  ()
  ($B8F$V(B $B8F$`(B $B8F$L(B $B8F$s(B))
;$B8F$s$@(B: OK
(deftest $BFI$s$@(B
  ()
  ($BFI$`(B $BFI$L(B $BFI$V(B $BFI$s(B))
;$BFI$s$@(B: OK
(deftest $BJ,$+$C$?(B
  ()
  ($BJ,$+$k(B $BJ,$$(B $BJ,$+$&(B $BJ,$+$D(B))
;$BJ,$+$C$?(B: OK
(deftest $B@.$C$?(B
  ()
  ($B@.$k(B $B@.$&(B $B@.$D(B))
;;; $B0lCJF0;l(B
(deftest $B@8$-$?(B
  ()
  ($B@8$-$k(B $B@8$/$k(B))
;$B@8$-$?(B: OK
(deftest $B8+$?(B
  ()
  ($B8+$k(B))

;;; Gerund
;;; These all also map to $B$D(B, because of the plan imperative form.
;;; This seems surprising, if you're not thinking about it.

(deftest $BGc$C$F(B
  ()
  ($BGc$&(B $BGc$D(B $BGc$k(B $BGc$C$D(B $BGc$C$F$k(B))
;$BGc$C$F(B: OK
(deftest $BCV$$$F(B
  ()
  ($BCV$/(B $BCV$$$k(B $BCV$$$D(B $BCV$$$F$k(B))
;$BCV$$$F(B: OK
(deftest $B9T$C$F(B
					();iku is irregular It looks like a $B$k(B/$B$D(B/$B$&(B.
  ($B9T$/(B $B9T$$(B $B9T$&(B $B9T$D(B $B9T$k(B $B9T$C$D(B $B9T$C$F$k(B))
;$B9T$C$F(B: OK
(deftest $BOC$7$F(B
  ()
  ($BOC$9(B $BOC$7$k(B $BOC$7$D(B $BOC$9$k(B $BOC(B $BOC$7$F$k(B))
;$BOC$7$F(B: OK
(deftest $B;}$C$F(B
  ()
  ($B;}$D(B $B;}$&(B $B;}$k(B $B;}$C$D(B $B;}$C$F$k(B))
;$B;}$C$F(B: OK
(deftest $B;`$s$F(B
					();Don't mis-interpret
  ($B;`$s$D(B $B;`$s$F$k(B)
  ($B;`$L(B))
;$B;`$s$F(B: OK
(deftest $B;`$s$G(B
  ()
  ($B;`$L(B $B;`$V(B $B;`$`(B))
;$B;`$s$G(B: OK
(deftest $B8F$s$G(B
  ()
  ($B8F$V(B $B8F$`(B $B8F$L(B))
;$B8F$s$G(B: OK
(deftest $BFI$s$G(B
  ()
  ($BFI$`(B $BFI$L(B $BFI$V(B))
(deftest $BJ,$+$C$F(B
  ()
  ($BJ,$+$k(B $BJ,$+$&(B $BJ,$+$D(B $BJ,$+$C$D(B $BJ,$+$C$F$k(B))
;$BJ,$+$C$F(B: OK
(deftest $B@.$C$F(B
  ()
  ($B@.$k(B $B@.$&(B $B@.$D(B $B@.$C$D(B $B@.$C$F$k(B))
;$B@.$C$F(B: OK
;;; $B0lCJF0;l(B
(deftest $B@8$-$F(B
  ()
  ($B@8$-$k(B $B@8$-$D(B $B@8$/$k(B $B@8$-$F$k(B))
;$B@8$-$F(B: OK
(deftest $B8+$F(B
  ()
  ($B8+$k(B $B8+$D(B $B8+$F$k(B))
;$B8+$F(B: OK

;;; Potential

(deftest $BGc$($k(B
  ()
  ($BGc$&(B))
;$BGc$($k(B: OK

(deftest $B?)$Y$i$l$k(B
  ()
  ($B?)$Y$k(B $B?)$Y$i$k(B $B?)$V(B))
;$B?)$Y$i$l$k(B: OK

(deftest $B8F$Y$k(B
  ()
  ($B8F$V(B))
;$B8F$Y$k(B: OK

;;; Passive
;;; These also look like they could be $B0lCJ$I$&$7(B potentials.

(deftest $BGc$o$l$k(B
  ()
  ($BGc$&(B $BGc$o$k(B))
;$BGc$o$l$k(B: OK

(deftest $BCV$+$l$k(B
  ()
  ($BCV$/(B $BCV$+$k(B))
;$BCV$+$l$k(B: OK

(deftest $B1K$,$l$k(B
  ()
  ($B1K$0(B $B1K$,$k(B))
(deftest $BOC$5$l$k(B
  ()
  ($BOC$9(B $BOC$9$k(B $BOC$5$k(B $BOC(B))		;Because of irregular $B$9$k(B
(deftest $BBT$?$l$k(B
  ()
  ($BBT$D(B $BBT$?$k(B))
(deftest $B;`$J$l$k(B
  ()
  ($B;`$L(B $B;`$J$k(B))
(deftest $BFI$^$l$k(B
  ()
  ($BFI$`(B $BFI$^$k(B))
;$BFI$^$l$k(B: OK
(deftest $B8F$P$l$k(B
  ()
  ($B8F$V(B $B8F$P$k(B))
(deftest $B8+$i$l$k(B
  ()
  ($B8+$k(B $B8+$i$k(B))

;;; Irregulars
(deftest $BMh$i$l$k(B
  ()
  ($BMh$k(B $BMh$i$k(B))
(deftest $B$5$l$k(B
  ()
  ($B$9$k(B $B$5$k(B $B$9(B))			;$B$9(B because of the regular rule.

;;; Causitive

(deftest $BGc$o$;$k(B
  ()
  ($BGc$&(B $BGc$o$9(B))
;$BGc$o$;$k(B: OK
(deftest $BCV$+$;$k(B
  ()
  ($BCV$/(B $BCV$+$9(B))
;$BCV$+$;$k(B: OK
(deftest $B1K$,$;$k(B
  ()
  ($B1K$0(B $B1K$,$9(B))
;$B1K$,$;$k(B: OK
(deftest $BOC$5$;$k(B
  ()
  ($BOC$k(B $BOC$9(B $BOC$9$k(B $BOC$5$9(B $BOC(B))		;Because of irregular $B$9$k(B
;$BOC$5$;$k(B: OK
(deftest $BBT$?$;$k(B
  ()
  ($BBT$D(B $BBT$?$9(B))
;$BBT$?$;$k(B: OK
(deftest $B;`$J$;$k(B
  ()
  ($B;`$L(B $B;`$J$9(B))
;$B;`$J$;$k(B: OK
(deftest $BFI$^$;$k(B
  ()
  ($BFI$`(B $BFI$^$9(B))
;$BFI$^$;$k(B: OK
(deftest $B8F$P$;$k(B
  ()
  ($B8F$V(B $B8F$P$9(B))
;$B8F$P$;$k(B: OK
(deftest $B8+$5$;$k(B
  ()
  ($B8+$k(B $B8+$9(B $B8+$9$k(B $B8+$5$9(B $B8+(B))		;Because of regular & irregular rules
;$B8+$5$;$k(B: OK

;;; Irregulars
(deftest $BMh$5$;$k(B
  ()
  ($BMh$k(B $BMh$9(B $BMh$9$k(B $BMh$5$9(B $BMh(B))		;because of regular & irregular rules.
;$BMh$5$;$k(B: OK
(deftest $B$5$;$k(B
  ()
  ($B$9$k(B $B$5$9(B $B$9(B))			;$B$9(B because of the regular rule.
;$B$5$;$k(B: OK

;;; Conditional

(deftest $BGc$($P(B
  ()
  ($BGc$&(B))
(deftest $BCV$1$P(B
  ()
  ($BCV$/(B))
(deftest $B1K$2$P(B
  ()
  ($B1K$0(B))
(deftest $BOC$;$P(B
  ()
  ($BOC$9(B))
(deftest $BBT$F$P(B
  ()
  ($BBT$D(B))
(deftest $B;`$M$P(B
  ()
  ($B;`$L(B))
(deftest $BFI$a$P(B
  ()
  ($BFI$`(B))
(deftest $B8F$Y$P(B
  ()
  ($B8F$V(B))
(deftest $B8+$l$P(B
  ()
  ($B8+$k(B))

;;; $B$?$i(B conditional form

(deftest $BGc$C$?$i(B
  ()
  ($BGc$&(B $BGc$D(B $BGc$k(B))
(deftest $BCV$$$?$i(B
  ()
  ($BCV$/(B $BCV$$$k(B))
(deftest $B9T$C$?$i(B
					();iku is irregular It looks like a $B$k(B/$B$D(B/$B$&(B.
  ($B9T$/(B $B9T$$(B $B9T$&(B $B9T$D(B $B9T$k(B))
(deftest $BOC$7$?$i(B
  ()
  ($BOC$9(B $BOC$7$k(B $BOC$9$k(B $BOC(B))
;$BOC$7$?$i(B: OK
(deftest $B;}$C$?$i(B
  ()
  ($B;}$D(B $B;}$&(B $B;}$k(B))
(deftest $B;`$s$?$i(B
					();Don't mis-interpret
  ()
  ($B;`$L(B))
(deftest $B;`$s$@$i(B
  ()
  ($B;`$L(B $B;`$V(B $B;`$`(B))
(deftest $B8F$s$@$i(B
  ()
  ($B8F$V(B $B8F$`(B $B8F$L(B))
(deftest $BFI$s$@$i(B
  ()
  ($BFI$`(B $BFI$L(B $BFI$V(B))
(deftest $BJ,$+$C$?$i(B
  ()
  ($BJ,$+$k(B $BJ,$+$&(B $BJ,$+$D(B))
(deftest $B@.$C$?$i(B
  ()
  ($B@.$k(B $B@.$&(B $B@.$D(B))
;;; $B0lCJF0;l(B
(deftest $B@8$-$?$i(B
  ()
  ($B@8$-$k(B $B@8$/$k(B))
;$B@8$-$?$i(B: OK
(deftest $B8+$?$i(B
  ()
  ($B8+$k(B))

;;; Plain negative

(deftest $BGc$o$J$$(B
  ()
  ($BGc$&(B $BGc$o$J$$(B $BGc$o$J$&(B $BGc$o$J$$$k(B))
;$BGc$o$J$$(B: OK
(deftest $BCV$+$J$$(B
  ()
  ($BCV$/(B $BCV$+$J$$(B $BCV$+$J$&(B $BCV$+$J$$$k(B))
;$BCV$+$J$$(B: OK
(deftest $B1K$,$J$$(B
  ()
  ($B1K$0(B $B1K$,$J$$$k(B $B1K$,$J$&(B))
;$B1K$,$J$$(B: OK
(deftest $BOC$5$J$$(B
  ()
  ($BOC$9(B $BOC$5$J$$$k(B $BOC$5$J$&(B))
;$BOC$5$J$$(B: OK
(deftest $BBT$?$J$$(B
  ()
  ($BBT$D(B $BBT$?$J$$$k(B $BBT$?$J$&(B))
;$BBT$?$J$$(B: OK
(deftest $B;`$J$J$$(B
  ()
  ($B;`$L(B $B;`$J$J$$$k(B $B;`$J$J$&(B))
;$B;`$J$J$$(B: OK
(deftest $BFI$^$J$$(B
  ()
  ($BFI$`(B $BFI$^$J$$$k(B $BFI$^$J$&(B))
;$BFI$^$J$$(B: OK
(deftest $B8F$P$J$$(B
  ()
  ($B8F$V(B $B8F$P$J$$$k(B $B8F$P$J$&(B))
;$B8F$P$J$$(B: OK
(deftest $B8+$J$$(B
  ()
  ($B8+$k(B $B8+$J$$$k(B $B8+$J$&(B))
;$B8+$J$$(B: OK

;;; Irregulars
(deftest $BMh$J$$(B
  ()
  ($BMh$k(B $BMh$J$$$k(B $BMh$J$&(B))
;$BMh$J$$(B: OK
(deftest $B$7$J$$(B
  ()
  ($B$9$k(B $B$7$k(B $B$7$J$$$k(B $B$7$J$&(B))		;$B$7$k(B because of regular rules.
;$B$7$J$$(B: OK
(deftest $B$J$$(B
  ()
  ($B$"$k(B $B$J$$$k(B $B$J$&(B))
;$B$J$$(B: OK

;;; $B$:(B negatives

(deftest $BGc$o$:(B
  ()
  ($BGc$&(B))
;$BGc$o$:(B: OK
(deftest $BCV$+$:(B
  ()
  ($BCV$/(B))
;$BCV$+$:(B: OK
(deftest $B1K$,$:(B
  ()
  ($B1K$0(B))
;$B1K$,$:(B: OK
(deftest $BOC$5$:(B
  ()
  ($BOC$9(B))
;$BOC$5$:(B: OK
(deftest $BBT$?$:(B
  ()
  ($BBT$D(B))
;$BBT$?$:(B: OK
(deftest $B;`$J$:(B
  ()
  ($B;`$L(B))
;$B;`$J$:(B: OK
(deftest $BFI$^$:(B
  ()
  ($BFI$`(B))
;$BFI$^$:(B: OK
(deftest $B8F$P$:(B
  ()
  ($B8F$V(B))
;$B8F$P$:(B: OK
(deftest $B8+$:(B
  ()
  ($B8+$k(B))
;$B8+$:(B: OK

;;; Irregulars
(deftest $BMh$:(B
  ()
  ($BMh$k(B))
;$BMh$:(B: OK
(deftest $B$;$:(B
  ()
  ($B$9$k(B $B$;$k(B))				;$B$;$k(B because of regular rules.
;$B$;$:(B: OK


;;; Plain command form

(deftest $BGc$((B
  ()
  ($BGc$&(B $BGc$($k(B))

(deftest $BCV$1(B
  ()
  ($BCV$/(B $BCV$1$k(B))
;$BCV$1(B: OK
(deftest $B1K$2(B
  ()
  ($B1K$0(B $B1K$2$k(B))
(deftest $BOC$;(B
  ()
  ($BOC$9(B $BOC$;$k(B))
(deftest $BBT$F(B
  ()
  ($BBT$D(B $BBT$F(B $BBT$k(B $BBT$F$k(B))
;$BBT$F(B: OK
(deftest $B;`$M(B
  ()
  ($B;`$L(B $B;`$M$k(B))
(deftest $BFI$a(B
  ()
  ($BFI$`(B $BFI$a$k(B))
(deftest $B8F$Y(B
  ()
  ($B8F$V(B $B8F$Y$k(B))
(deftest $B8+$m(B
  ()
  ($B8+$k(B))

;;; Irregulars
(deftest $BMh$$(B
  ()
  ($BMh$k(B $BMh$$$k(B $BMh$&(B))
;$BMh$$(B: OK
(deftest $B$3$$(B
  ()
  ($B$/$k(B $B$3$$$k(B $B$3$&(B))
;$B$3$$(B: OK
(deftest $B$7$m(B
  ()
  ($B$9$k(B $B$7$k(B))				;$B$7$k(B because of regular rules.

;;; The plain desideratives

(deftest $BGc$$$?$$(B
  ()
  ($BGc$&(B $BGc$$$k(B $BGc$$$?$$$k(B $BGc$$$?$&(B))
;$BGc$$$?$$(B: OK
(deftest $BCV$-$?$$(B
  ()
  ($BCV$/(B $BCV$-$k(B $BCV$-$?$$$k(B $BCV$-$?$&(B))
;$BCV$-$?$$(B: OK
(deftest $B1K$.$?$$(B
  ()
  ($B1K$0(B $B1K$.$k(B $B1K$.$?$$$k(B $B1K$.$?$&(B))
;$B1K$.$?$$(B: OK
(deftest $BOC$7$?$$(B
  ()
  ($BOC$9(B $BOC$7$k(B $BOC$7$?$$$k(B $BOC$7$?$&(B))
;$BOC$7$?$$(B: OK
(deftest $B;}$A$?$$(B
  ()
  ($B;}$D(B $B;}$A$k(B $B;}$A$?$$$k(B $B;}$A$?$&(B))
;$B;}$A$?$$(B: OK
(deftest $B;`$K$?$$(B
  ()
  ($B;`$L(B $B;`$K$k(B $B;`$K$?$$$k(B $B;`$K$?$&(B))
;$B;`$K$?$$(B: OK
(deftest $B8F$S$?$$(B
  ()
  ($B8F$V(B $B8F$S$k(B $B8F$S$?$$$k(B $B8F$S$?$&(B))
;$B8F$S$?$$(B: OK
(deftest $BFI$_$?$$(B
  ()
  ($BFI$`(B $BFI$_$k(B $BFI$_$?$$$k(B $BFI$_$?$&(B))
;$BFI$_$?$$(B: OK
(deftest $BJ,$+$j$?$$(B
  ()
  ($BJ,$+$k(B $BJ,$+$j$k(B $BJ,$+$j$?$$$k(B $BJ,$+$j$?$&(B))
;$BJ,$+$j$?$$(B: OK
(deftest $B@.$j$?$$(B
  ()
  ($B@.$k(B $B@.$j$k(B $B@.$j$?$$$k(B $B@.$j$?$&(B))
;$B@.$j$?$$(B: OK
(deftest $B@8$-$?$$(B
  ()
  ($B@8$-$k(B $B@8$/(B $B@8$-$?$$$k(B $B@8$-$?$&(B))
;$B@8$-$?$$(B: OK
(deftest $B8+$?$$(B
  ()
  ($B8+$k(B $B8+$?$$$k(B $B8+$?$&(B))
;$B8+$?$$(B: OK


;;; Irregulars

(deftest $BMh$?$$(B
  ()
  ($BMh$k(B $BMh$?$$$k(B $BMh$?$&(B))
;$BMh$?$$(B: OK
(deftest $B$-$?$$(B
  ()
  ($B$/$k(B $B$-$k(B $B$-$?$$$k(B $B$-$?$&(B))
;$B$-$?$$(B: OK
(deftest $B$7$?$$(B
  ()
  ($B$9$k(B $B$7$k(B $B$7$?$$$k(B $B$7$?$&(B))
;$B$7$?$$(B: OK

(deftest $BGc$$$?$,$k(B
  ()
  ($BGc$&(B $BGc$$$k(B))
(deftest $BCV$-$?$,$k(B
  ()
  ($BCV$/(B $BCV$$(B $BCV$-$k(B))
(deftest $B1K$.$?$,$k(B
  ()
  ($B1K$0(B $B1K$.$k(B))
(deftest $BOC$7$?$,$k(B
  ()
  ($BOC$9(B $BOC$7$k(B))
(deftest $B;}$A$?$,$k(B
  ()
  ($B;}$D(B $B;}$A$k(B))
(deftest $B;`$K$?$,$k(B
  ()
  ($B;`$L(B $B;`$K$k(B))
(deftest $B8F$S$?$,$k(B
  ()
  ($B8F$V(B $B8F$S$k(B))
(deftest $BFI$_$?$,$k(B
  ()
  ($BFI$`(B $BFI$_$k(B))
(deftest $BJ,$+$j$?$,$k(B
  ()
  ($BJ,$+$k(B $BJ,$+$j$k(B))
(deftest $B@.$j$?$,$k(B
  ()
  ($B@.$k(B $B@.$j$k(B))
(deftest $B@8$-$?$,$k(B
  ()
  ($B@8$-$k(B $B@8$/(B $B@8$$(B))			; Could be an adverb or adjective.
(deftest $B8+$?$,$k(B
  ()
  ($B8+$k(B))


;;; Irregulars

(deftest $BMh$?$,$k(B
  ()
  ($BMh$k(B))
(deftest $B$-$?$,$k(B
  ()
  ($B$/$k(B $B$-$k(B))
(deftest $B$7$?$,$k(B
  ()
  ($B$9$k(B $B$7$k(B))


;;; Here's a compound test.

(deftest $B9T$-$?$,$C$F$$$^$9(B
  ()
  ($B9T$/(B $B9T$-$?$,$C$F(B $B9T$-$?$,$k(B $B9T$-$?$,$&(B $B9T$-$?$,$D(B
   $B9T$-$?$,$C$D(B $B9T$-$?$,$C$F$$$k(B $B9T$-$?$,$C$F$&(B $B9T$$(B
   $B9T$-$k(B))
;$B9T$-$?$,$C$F$$$^$9(B: OK

(deftest $BFI$s$G$$$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$G$$$k(B: OK
(deftest $BGc$C$F$$$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$F$$$k(B: OK

(deftest $BFI$s$G$$$?(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$$$k(B $BFI$s$G$/(B))
;$BFI$s$G$$$?(B: OK
(deftest $BGc$C$F$$$?(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$$$k(B $BGc$C$F$/(B))
;$BGc$C$F$$$?(B: OK

(deftest $BFI$s$G$$$^$9(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$$$k(B $BFI$s$G$&(B))
;$BFI$s$G$$$^$9(B: OK
(deftest $BGc$C$F$$$^$9(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$$$k(B $BGc$C$F$&(B))
;$BGc$C$F$$$^$9(B: OK

(deftest $BFI$s$G$"$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$G$"$k(B: OK
(deftest $BGc$C$F$"$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$F$"$k(B: OK

(deftest $BFI$s$G$*$/(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$*$$(B))
;$BFI$s$G$*$/(B: OK
(deftest $BGc$C$F$*$/(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$*$$(B))
;$BGc$C$F$*$/(B: OK

(deftest $BFI$s$G$_$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$G$_$k(B: OK
(deftest $BGc$C$F$_$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$F$_$k(B: OK

(deftest $BFI$s$G$7$^$&(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$G$7$^$&(B: OK
(deftest $BGc$C$F$7$^$&(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$F$7$^$&(B: OK

(deftest $BFI$s$G$/$@$5$$(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$/$@$5$$$k(B $BFI$s$G$/$@$5$&(B))
;$BFI$s$G$/$@$5$$(B: OK
(deftest $BGc$C$F$/$@$5$$(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$/$@$5$$$k(B $BGc$C$F$/$@$5$&(B))
;$BGc$C$F$/$@$5$$(B: OK

(deftest $BFI$s$G2<$5$$(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G2<$5$$$k(B $BFI$s$G2<$5$&(B))
;$BFI$s$G2<$5$$(B: OK
(deftest $BGc$C$F2<$5$$(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F2<$5$$$k(B $BGc$C$F2<$5$&(B))
;$BGc$C$F2<$5$$(B: OK

(deftest $BFI$s$G$J$5$$(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$J$5$$$k(B $BFI$s$G$J$5$&(B))
;$BFI$s$G$J$5$$(B: OK
(deftest $BGc$C$F$J$5$$(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$J$5$$$k(B $BGc$C$F$J$5$&(B))
;$BGc$C$F$J$5$$(B: OK

(deftest $BFI$s$G$$$/(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$$$$(B))
;$BFI$s$G$$$/(B: OK
(deftest $BGc$C$F$$$/(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$$$$(B))
;$BGc$C$F$$$/(B: OK

(deftest $BFI$s$G$/$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$G$/$k(B: OK
(deftest $BGc$C$F$/$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$F$/$k(B: OK

(deftest $BFI$s$G$"$2$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$"$0(B))
;$BFI$s$G$"$2$k(B: OK
(deftest $BGc$C$F$"$2$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$"$0(B))
;$BGc$C$F$"$2$k(B: OK

(deftest $BFI$s$G$d$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$G$d$k(B: OK
(deftest $BGc$C$F$d$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$F$d$k(B: OK

(deftest $BFI$s$G$b$i$&(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$G$b$i$&(B: OK
(deftest $BGc$C$F$b$i$&(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$F$b$i$&(B: OK

(deftest $BFI$s$G$$$?$@$/(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$$$?$@$$(B))
;$BFI$s$G$$$?$@$/(B: OK
(deftest $BGc$C$F$$$?$@$/(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$$$?$@$$(B))
;$BGc$C$F$$$?$@$/(B: OK

(deftest $BFI$s$G$/$l$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$/$k(B))
;$BFI$s$G$/$l$k(B: OK
(deftest $BGc$C$F$/$l$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$/$k(B))
;$BGc$C$F$/$l$k(B: OK

(deftest $BFI$s$G$$$?$@$-$^$9(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$$$?$@$/(B $BFI$s$G$$$?$@$-$k(B))
;$BFI$s$G$$$?$@$-$^$9(B: OK
(deftest $BGc$C$F$$$?$@$-$^$9(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$$$?$@$/(B $BGc$C$F$$$?$@$-$k(B))
;$BGc$C$F$$$?$@$-$^$9(B: OK

(deftest $BGc$C$FD:$-$^$9(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$FD:$/(B $BGc$C$FD:$-$k(B))
;$BGc$C$FD:$-$^$9(B: OK

(deftest $BFI$s$G$/$@$5$$(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G$/$@$5$&(B $BFI$s$G$/$@$5$$$k(B))
;$BFI$s$G$/$@$5$$(B: OK
(deftest $BGc$C$F$/$@$5$$(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$/$@$5$&(B $BGc$C$F$/$@$5$$$k(B))
;$BGc$C$F$/$@$5$$(B: OK

(deftest $BFI$s$G>e$2$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G>e$0(B))
;$BFI$s$G>e$2$k(B: OK
(deftest $BGc$C$F$"$2$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$"$0(B))
;$BGc$C$F$"$2$k(B: OK
(deftest $BFI$s$G:9$7>e$2$k(B
  ()
  ($BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B $BFI$s$G:9$7>e$0(B))
;$BFI$s$G:9$7>e$2$k(B: OK
(deftest $BGc$C$F:9$7>e$2$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F:9$7>e$0(B))
;$BGc$C$F:9$7>e$2$k(B: OK

(deftest $BGc$C$F:9$7$"$2$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F:9$7$"$0(B))
;$BGc$C$F:9$7$"$2$k(B: OK
(deftest $BGc$C$F$5$7$"$2$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$5$7$"$0(B))
;$BGc$C$F$5$7$"$2$k(B: OK
(deftest $BGc$C$F$5$7>e$2$k(B
  ()
  ($BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B $BGc$C$F$5$7>e$0(B))
;$BGc$C$F$5$7>e$2$k(B: OK

(deftest $BFI$`$i$7$$(B
  ()
  ($BFI$`(B $BFI$`$i$7$&(B $BFI$`$i$7$$$k(B))
;$BFI$`$i$7$$(B: OK

(deftest $BFI$`$=$&(B
  ()
  ($BFI$`(B $BFI$`$9(B))
;$BFI$`$=$&(B: OK

(deftest $BFI$`$h$&(B
  ()
  ($BFI$`(B))
;$BFI$`$h$&(B: OK

(deftest $BFI$`$h$&$@(B
  ()
  ($BFI$`(B $BFI$`$h$&(B))
;$BFI$`$h$&$@(B: OK

(deftest $BGc$*$&(B
  ()
  ($BGc$&(B))
;$BGc$*$&(B: OK
(deftest $BCV$3$&(B
  ()
  ($BCV$/(B))
;$BCV$3$&(B: OK
(deftest $B1K$4$&(B
  ()
  ($B1K$0(B))
;$B1K$4$&(B: OK
(deftest $BOC$=$&(B
  ()
  ($BOC$9(B))
;$BOC$=$&(B: OK
(deftest $BBT$H$&(B
  ()
  ($BBT$D(B))
;$BBT$H$&(B: OK
(deftest $B;`$N$&(B
  ()
  ($B;`$L(B))
;$B;`$N$&(B: OK
(deftest $BFI$b$&(B
  ()
  ($BFI$`(B))
;$BFI$b$&(B: OK
(deftest $B8F$\$&(B
  ()
  ($B8F$V(B))
;$B8F$\$&(B: OK
(deftest $B8+$h$&(B
  ()
  ($B8+$k(B))
;$B8+$h$&(B: OK

;;; Irregulars
(deftest $BMh$h$&(B
  ()
  ($BMh$k(B))
;$BMh$h$&(B: OK
(deftest $B$3$h$&(B
  ()
  ($B$/$k(B))
;$B$3$h$&(B: OK
(deftest $B$7$h$&(B
  ()
  ($B$9$k(B $B$7$k(B))				;$B$7$k(B due to the regular rules.
;$B$7$h$&(B: OK

(deftest $BFI$s$A$c$&(B
  ()
  ($BFI$s$G$7$^$&(B $BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B))
;$BFI$s$A$c$&(B: OK
(deftest $BGc$C$A$c$&(B
  ()
  ($BGc$C$F$7$^$&(B $BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B))
;$BGc$C$A$c$&(B: OK

(deftest $BFI$s$A$c$C$?(B
  ()
  ($BFI$s$G$7$^$&(B $BFI$s$G(B $BFI$`(B $BFI$L(B $BFI$V(B
   $BFI$s$A$c$&(B $BFI$s$A$c$k(B $BFI$s$A$c$D(B))
;$BFI$s$A$c$C$?(B: OK

(deftest $BGc$C$A$c$C$?(B
  ()
  ($BGc$C$F$7$^$&(B $BGc$C$F(B $BGc$&(B $BGc$C$D(B $BGc$D(B $BGc$k(B
   $BGc$C$A$c$&(B $BGc$C$A$c$k(B $BGc$C$A$c$D(B))
;$BGc$C$A$c$C$?(B: OK

(deftest $B:o=|$9$k(B
  ()
  ($B:o=|(B))
;$B:o=|$9$k(B: OK

;;; Honorific prefixes

(deftest $B$*?e(B
  ()
  ($B?e(B))
;$B$*?e(B: OK

(deftest $B$4HS(B
  ()
  ($BHS(B))
;$B$4HS(B: OK

(deftest $B8fHS(B
  ()
  ($BHS(B))
;$B8fHS(B: OK

;;; Adjectives

(deftest $B?7$7$/(B
  ()
  ($B?7$7$$(B))
;$B?7$7$/(B: OK

(deftest $B?7$7$/$F(B
  ()
  ($B?7$7$$(B $B?7$7$/$D(B $B?7$7$/$F$k(B))
;$B?7$7$/$F(B: OK

(deftest $B?7$7$+$C$?(B
  ()
  ($B?7$7$$(B $B?7$7$+$&(B $B?7$7$+$D(B $B?7$7$+$k(B))
;$B?7$7$+$C$?(B: OK

(deftest $B855$$G$O$"$j$^$;$s(B
  ()
  ($B855$(B $B855$$G$O$"$k(B $B855$$G$O$"$j$k(B $B855$$@(B $B855$$G$9(B))
;$B855$$G$O$"$j$^$;$s(B: OK

(deftest $B855$$G$O$J$$(B
  ()
  ($B855$(B $B855$$G$O$J$$$k(B $B855$$G$O$J$&(B $B855$$@(B))
;$B855$$G$O$J$$(B: OK

(deftest $B855$$8$c$"$j$^$;$s(B
  ()
  ($B855$(B $B855$$8$c$"$k(B $B855$$8$c$"$j$k(B $B855$$@(B $B855$$G$9(B))
;$B855$$8$c$"$j$^$;$s(B: OK

(deftest $B855$$8$c$J$$(B
  ()
  ($B855$(B $B855$$8$c$J$$$k(B $B855$$8$c$J$&(B $B855$$@(B))
;$B855$$8$c$J$$(B: OK

(deftest $B?7$7$/$J$/$F(B
  ()
  ($B?7$7$$(B $B?7$7$/$J$$(B $B?7$7$/$J$/$D(B $B?7$7$/$J$/$F$k(B))
;$B?7$7$/$J$/$F(B: OK

(deftest $B?7$7$1$l$P(B
  ()
  ($B?7$7$$(B $B?7$7$/(B $B?7$7$1$k(B))
;$B?7$7$1$l$P(B: OK

(deftest $B?7$7$/$J$$(B
  ()
  ($B?7$7$$(B $B?7$7$/$J$&(B $B?7$7$/$J$$$k(B))
;$B?7$7$/$J$$(B: OK

(deftest $BJY6/Cf(B
  ()
  ($BJY6/(B))
;$BJY6/Cf(B: OK

(deftest $B7k:'<0(B
  ()
  ($B7k:'(B))
;$B7k:'<0(B: OK

(deftest $BK:$l$b$N(B
  ()
  ($BK:$l(B $BK:$l$k(B $BK:$k(B))
;$BK:$l$b$N(B: OK

(deftest $BK:$lJ*(B
  ()
  ($BK:$l(B $BK:$l$k(B $BK:$k(B))
;$BK:$lJ*(B: OK

(deftest $BN99T<T(B
  ()
  ($BN99T(B))
;$BN99T<T(B: OK

(deftest $B4[0w(B
  ()
  ($B4[(B))
;$B4[0w(B: OK

(deftest $B:rF|(B
  ()
  ($BF|(B))
;$B:rF|(B: OK

(deftest $BMhG/(B
  ()
  ($BG/(B))
;$BMhG/(B: OK

(deftest $BA49q(B
  ()
  ($B9q(B))
;$BA49q(B: OK

;;; Humble

(deftest $B$*0{$_$K$J$j$^$9(B
  ()
  ($B0{$`(B $B$*0{$_$K$J$k(B $B$*0{$_$K$J$j$k(B $B$*0{$_$K$J$j$^$9(B 
   $B0{$_$K$J$j$^$9(B $B0{$_$K$J$k(B $B0{$_$K$J$j$k(B))
;$B$*0{$_$K$J$j$^$9(B: OK

(deftest $B$*0{$_$K@.$j$^$9(B
  ()
  ($B0{$`(B $B$*0{$_$K@.$k(B $B$*0{$_$K@.$j$k(B 
   $B0{$_$K@.$j$^$9(B $B0{$_$K@.$k(B $B0{$_$K@.$j$k(B))
;$B$*0{$_$K@.$j$^$9(B: OK

(deftest $B9T$C$F$-$^$9(B
  ()
  ($B9T$/(B $B9T$C$F(B $B9T$C$F$/$k(B $B9T$C$F$/(B $B9T$C$F$-$k(B
   $B9T$&(B $B9T$D(B $B9T$k(B $B9T$$(B $B9T$C$D(B))
;$B9T$C$F$-$^$9(B: OK

(deftest $BJ,3d$7$J$$$h$&(B
  ()
  ($BJ,3d(B $BJ,3d$9$k(B $BJ,3d$7$k(B $BJ,3d$7$J$$(B $BJ,3d$7$J$$$k(B $BJ,3d$7$J$&(B))
;$BJ,3d$7$J$$$h$&(B: OK

(deftest $BBZ:_$7$F$$$k(B
  ()
  ($BBZ:_(B $BBZ:_$9$k(B $BBZ:_$7$F(B $BBZ:_$7$D(B $BBZ:_$9(B $BBZ:_$7$k(B))
;$BBZ:_$7$F$$$k(B: OK

(deftest $BD9$/$J$j$^$9(B
  ()
  ($BD9$/(B $BD9$$(B $BD9$/$J$k(B $BD9$/$J$j$k(B))
;$BD9$/$J$j$^$9(B: OK

;;; $B$3$l$OJ,$+$C$F$$$^$;$s!'(B
;;  >  |$B$3$l$G(Bxinfo$B$GF|K\8l$,I=<($G$-$^$9!%(BEmacs$B$N(Binfo$B$O;H$$$:$i$+$C$?$N$G!$(B
;;  >  |xinfo$B$NB8:_$O$H$F$b$"$j$,$?$$$H;W$$$^$9!%(B
;; 
;; $B!V;H$$$:$i$+$C$?!W$H$O2?$G$9$+!#(B
;; $B"*!V;H$$$:!W$O!"!V;H$o$:!W$G$9$+!#(B
;; $B"*!V;H$o$J$+$C$?$i!W$G$9$+!#(B
;; $B$G$O!"!V;H$$$:!W$H!V$i$+$C$?!W$H!V$:$i!W$r(Bedict $B$,$o$+$i$J$C$?!#(B
;; $B;d$N@h@8$K?R$M$h$&!#(B

(deftest $B>/$J$+$i$:(B
  ()
  ($B>/$J$$(B $B>/$J$+$k(B $B>/$k(B))
;$B>/$J$+$i$:(B: OK

;;; Test the various titles.

(deftest $Ba15H<B@h@8(B
  ()
  ($Ba15H<B(B))
;$Ba15H<B@h@8(B: OK

(deftest $BCfB<$5$s(B
  ()
  ($BCfB<(B))
;$BCfB<$5$s(B: OK

(deftest $B5\K\$A$c$s(B
  ()
  ($B5\K\(B))
;$B5\K\$A$c$s(B: OK

(deftest $BNS7/(B
  ()
  ($BNS(B))
;$BNS7/(B: OK

(deftest $B>.Bt$/$s(B
  ()
  ($B>.Bt(B))
;$B>.Bt$/$s(B: OK

(deftest $B@1LnMM(B
  ()
  ($B@1Ln(B))
;$B@1LnMM(B: OK

(deftest $B8E:d$5$^(B
  ()
  ($B8E:d(B))
;$B8E:d$5$^(B: OK

;;; Test the various number cases.

(deftest $BFs?M(B
  ()
  ($B0l?M(B $B?M(B))
;$BFs?M(B: OK

(deftest 17$B?M(B
  ()
  ($B0l?M(B $B?M(B))
;17$B?M(B: OK

(deftest $B#1#7?M(B
  ()
  ($B0l?M(B $B?M(B))
;$B#1#7?M(B: OK

;;; This one caused infinite recursion, due to a hole in the
;;; redundant-expansion checking (things didn't get checked for redundancy
;;; quite soon enough, so short cycles weren't detected).

(deftest $B=P$F(B
  ()
  ($B=P$k(B $B=P$F$k(B $B=P$D(B))
;$B=P$F(B: OK

;;; This one caused infinite recursion, due to failure to root certain
;;; patterns.  I've since added checks on the patterns to enforce rootedness.

(deftest $BDL$8$k(B
  ()
  ())
;$BDL$8$k(B: OK

(deftest $B#2<oN`(B
  ()
  ($B0l<oN`(B $B<oN`(B))
;$B#2<oN`(B: OK

(deftest $B$"$+$$$8$c$"$j$^$;$s(B
  ()
  ($B$"$+$$$G$9(B $B$"$+$$$8$c$"$k(B $B$"$+$$$8$c$"$j$k(B $B$"$+$$$@(B $B$"$+$0(B $B$"$+$&(B $B$"$+$$$k(B $B$"$+$$(B))
;$B$"$+$$$8$c$"$j$^$;$s(B: OK

(deftest $B1+$G$7$g$&(B
  ()
  ($B1+$G$9(B $B1+$@(B $B1+(B))
;$B1+$G$7$g$&(B: OK

(deftest $BG-(Bs
  ()
  ()
  ($BG-(B))
;$BG-(Bs: OK

(deftest $B7AMF;l!<(B
  ()
  ($B7AMF;l(B))

(deftest keys
  (english)
  (key))
;keys: OK

(deftest families
  (english)
  (family))
;families: OK

(provide 'edict-test)
