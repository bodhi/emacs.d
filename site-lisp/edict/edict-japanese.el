;;; edict-japanese.el --- Japanese morphology rules for edict.el

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

;; Japanese morphological rules

;;; To do:

;;; Changelog:

;; 1998-03-27  Stephen Turnbull  <turnbull@sk.tsukuba.ac.jp>
;;        (created):  broken out from monolithic edict.el

;;; Code:

(provide 'edict-japanese)

(require 'edict-morphology)

;; Strip "$B$$$^$9(B"
(define-edict-rule $B!V$$$^$9!W$r:o=|$9$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([$B$$$-$.$7$A$K$S$_$j(B]\\)\\($B$^(B\\($B$9(B\\|$B$;$s(B\\)\\)$")
  "$B$^$;$k(B$"
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (1 2)
  edict-ignore ())

(define-edict-rule $B!V$^$9!W$r:o=|$9$k(B
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\($B$^(B\\($B$9(B\\|$B$;$s(B\\)\\)$")
  "$B$^$;$k(B$"
  edict-subst-affix edict-identity "$B$k(B")

(define-edict-rule $B!VMh$^$9!W$NFCJL%k!<%k(B
  "\\($BMh$^(B\\($B$9(B\\|$B$;$s(B\\)\\)$"
  ()
  edict-subst-affix "$BMh$k(B")

(define-edict-rule $B!V$-$^$9!W$NFCJL%k!<%k(B
  "\\(^\\|$B$F(B\\|$B$s$G(B\\)\\($B$-$^(B\\($B$9(B\\|$B$;$s(B\\)\\)$"
  "$B$^$;$k(B$"
  edict-subst-modified-affix
  edict-identity ()
  edict-subst ("$B$/$k(B"))

(define-edict-rule $B!V$7$^$9!W$NFCJL%k!<%k(B
  "\\($B$7$^(B\\($B$9(B\\|$B$;$s(B\\)\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

;; The several cases of $B$F!?$C$F(B.
;;  Note either pattern may generate multiple possibilities.
;; Also, $B$?(B.
(define-edict-rule $B!V$F!?$?!W$+$i!V$&!W$^$GJQ49$9$k(B
  "\\($B$C(B\\($B$F(B\\|$B$?(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$&(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$D!W$^$GJQ49$9$k(B
  "\\($B$C(B\\($B$F(B\\|$B$?(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$D(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$k!W$^$GJQ49$9$k(B
  "\\($B$C(B\\($B$F(B\\|$B$?(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$k(B")

(define-edict-rule $B0lCJ$N!V$F!?$?!W$+$i!V$k!W$^$GJQ49$9$k(B
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\(\\($B$F(B\\|$B$?(B[$B$i(B]?\\)\\)$")
  ()
  edict-subst-affix edict-identity "$B$k(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$9!W$^$GJQ49$9$k(B
  "\\($B$7(B\\($B$F(B\\|$B$?(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$9(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$/!W$^$GJQ49$9$k(B
  "\\($B$$(B\\($B$F(B\\|$B$?(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$/(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$0!W$^$GJQ49$9$k(B
  "\\($B$$(B[$B$G$@(B]\\)$" 
  ()
  edict-subst-affix "$B$0(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$V!W$^$GJQ49$9$k(B
  "\\($B$s(B\\($B$G(B\\|$B$@(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$V(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$`!W$^$GJQ49$9$k(B
  "\\($B$s(B\\($B$G(B\\|$B$@(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$`(B")

(define-edict-rule $B!V$F!?$?!W$+$i!V$L!W$^$GJQ49$9$k(B
  "\\($B$s(B\\($B$G(B\\|$B$@(B[$B$i(B]?\\)\\)$" 
  ()
  edict-subst-affix "$B$L(B")

;; $B9T$/(B is an irregular verb.
(define-edict-rule $B9T$/$NFCJL%k!<%k(B
  "$B9T(B\\($B$C(B\\($B$F(B\\|$B$?(B[$B$i(B]?\\)\\)$"
  ()
  edict-subst-affix "$B$/(B")

(define-edict-rule $B!VMh$F!W$NFCJL%k!<%k(B
  "$BMh(B\\($B$F(B\\|$B$?(B[$B$i(B]?\\)$"
  ()
  edict-subst-affix "$BMh$k(B")

(define-edict-rule $B!V$-$F!W$NFCJL%k!<%k(B
  "\\($B$-$F(B\\|$B$-$?(B[$B$i(B]?\\)$"
  ()
  edict-subst-affix "$B$/$k(B")

(define-edict-rule $B!V$7$F!W$NFCJL%k!<%k(B
  "\\($B$7$F(B\\|$B$7$?(B[$B$i(B]?\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

;; Potential form.
;; The filters here are due to $B!V0lCJ$N!V$F!?$?!W$+$i!V$k!W$^$GJQ49$9$k!W(B
(define-edict-rule $B$l$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$l$k(B\\)$")
  "$B$l$F(B$"
  edict-subst-affix edict-identity "$B$k(B")

(define-edict-rule $B$1$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$1$k(B\\)$")
  "$B$1$F(B$"
  edict-subst-affix edict-identity "$B$/(B")

(define-edict-rule $B$;$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$;$k(B\\)$")
  "$B$;$F(B$"
  edict-subst-affix edict-identity "$B$9(B")

(define-edict-rule $B$F$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$F$k(B\\)$")
  "\\($B$F(B\\|$B$F$i$l$k(B\\)$"
  edict-subst-affix edict-identity "$B$D(B")

(define-edict-rule $B$M$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$M$k(B\\)$")
  "$B$M$F(B"
  edict-subst-affix edict-identity "$B$L(B")

(define-edict-rule $B$a$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$a$k(B\\)$")
  "$B$a$F(B"
  edict-subst-affix edict-identity "$B$`(B")

(define-edict-rule $B$((B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$($k(B\\)$")
  "$B$($F(B"
  edict-subst-affix edict-identity "$B$&(B")

(define-edict-rule $B$2$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$2$k(B\\)$")
  "$B$1$F(B"
  edict-subst-affix edict-identity "$B$0(B")

(define-edict-rule $B$Y$k(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$Y$k(B\\)$")
  "$B$Y$F(B"
  edict-subst-affix edict-identity "$B$V(B")

;; $B0lCJF0;l!#(B Also serves for the passive.
(define-edict-rule $B$i$l$k(B
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\($B$i$l$k(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$k(B")

;; Passive
(define-edict-rule $B8^CJF0;l$N!V$"$l$k!W$rJQ49$9$k(B 
  "\\([$B$o$+$,$5$?$J$^$P$i(B]\\)\\($B$l$k(B\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (0 2)
  edict-ignore ())

(define-edict-rule $BMh$i$l$k$N%k!<%k(B
  "$BMh(B\\($B$i$l$k(B\\)$"
  ()
  edict-subst-affix "$B$k(B")

(define-edict-rule $B$5$l$k$N%k!<%k(B
  "\\($B$5$l$k(B\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

;; Causative
(define-edict-rule $B8^CJF0;l$N!V$"$;$k!W$rJQ49$9$k(B 
  "\\([$B$o$+$,$5$?$J$^$P$i(B]\\)\\($B$;$k(B\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (0 2)
  edict-ignore ())

(define-edict-rule $B0lCJF0;l$N!V$"$;$k!W$rJQ49$9$k(B 
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\($B$5$;$k(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$k(B")

(define-edict-rule $B$5$;$k$N%k!<%k(B
  "\\($B$5$;$k(B\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

;; eba conditional form.
(define-edict-rule $B!V$($P!W$rJQ49$9$k(B "\\([$B$($1$2$;$F$M$Y$a$l(B]\\)\\($B$P(B\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (3 2)
  edict-ignore ())

;; tara conditional form is handled as part of the $B$F!?$?!?$?$i(B rules.

;; The informal negative form.
(define-edict-rule $B!V$J$$!W$rJQ49$9$k(B "\\([$B$o$+$,$5$?$J$^$P$i(B]\\)\\($B$J$$(B\\|$B$:(B\\)$"
  ()
  edict-subst-modified-affix
  edict-modify-verb (0 2)
  edict-ignore ())

(define-edict-rule $B0lCJ$N!V$J$$!W$rJQ49$9$k(B
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\($B$J$$(B\\|$B$:(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$k(B")

(define-edict-rule $B!V$7$J$$!W$NFCJL%k!<%k(B
  "\\($B$7$J$$(B\\|$B$;$:(B\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

(define-edict-rule $B!V$J$$!W$NFCJL%k!<%k(B
  "^\\($B$J$$(B\\)$"
  ()
  edict-subst-affix "$B$"$k(B")

;; Conjunctive form

(define-edict-rule $B0lCJ$N(Bconjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\(\\)$")
  "$B$/(B$\\|$B$+$C$?(B$\\|$B$/$k(B$\\|$B$/$l$k(B$\\|$B$/$@$5$$(B$\\|$B$"$2$k(B$\\|$B>e$2$k(B$\\|$B$7$^$&(B$\\|$B$/$F(B$\\|$B$/$J$$(B$\\|$B$1$l$P(B$\\|$B$$$k(B$\\|$B$+$i$:(B$\\|$B$$$^$9(B$\\|$B$"$k(B$\\|$B$_$k(B$\\|$B2<$5$$(B$\\|$B$J$5$$(B$\\|$B$d$k(B$\\|$B$b$i$&(B$"
  edict-subst-modified-affix
  edict-identity ()
  edict-subst ("$B$k(B"))

(define-edict-rule $B8^CJ$N(Bconjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([$B$$$-$.$7$A$K$S$_$j(B]\\)$")
  "$B$/(B$\\|$B$+$C$?(B$\\|$B$/$k(B$\\|$B$/$l$k(B$\\|$B$/$@$5$$(B$\\|$B$"$2$k(B$\\|$B>e$2$k(B$\\|$B$7$^$&(B$\\|$B$/$F(B$\\|$B$/$J$$(B$\\|$B$1$l$P(B$\\|$B$$$k(B$\\|$B$+$i$:(B$\\|$B$$$^$9(B$\\|$B$"$k(B$\\|$B$_$k(B$\\|$B2<$5$$(B$\\|$B$J$5$$(B$\\|$B$d$k(B$\\|$B$b$i$&(B$"
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (1 2))

(define-edict-rule $B!V$9$k!W$NFCJL(Bconjunctive
  (concat "\\(" edict-category-Japanese-word-constituent "\\)\\($B$7(B\\)$")
  "$B$9(B$"
  edict-subst-affix edict-identity "$B$9$k(B")

(define-edict-rule $B!V$8$k!W$NFCJL(Bconjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$8(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$8$k(B")

(define-edict-rule $B!V$:$k!W$NFCJL(Bconjunctive
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$8(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$:$k(B")

;; The informal imperative form, $B8^CJF0;l(B
(define-edict-rule $B!V$l!W$N8^CJF0;l$rJQ49$9$k(B 
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([$B$($1$2$;$F$M$Y$a$l(B]\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (3 2))

;; The informal imperative form, $B0lCJF0;l(B
(define-edict-rule $B!V$m!W$N0lCJF0;l$rJQ49$9$k(B
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\($B$m(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$k(B")

;; Irregulars
(define-edict-rule $B!VMh$$!W$NFCJL%k!<%k(B
  "^\\($BMh$$(B\\)$"
  ()
  edict-subst-affix "$BMh$k(B")

(define-edict-rule $B!V$3$$!W$NFCJL%k!<%k(B
  "^\\($B$3$$(B\\)$"
  "$B$/(B$"
  edict-subst-affix "$B$/$k(B")

(define-edict-rule $B!V$7$m!W$NFCJL%k!<%k(B
  "^\\($B$7$m(B\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

;; The plain desiderative
(define-edict-rule $B!V$?$$!W$r:o=|$9$k(B 
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([$B$$$-$.$7$A$K$S$_$j(B]\\)\\($B$?$$(B\\|$B$?$,$k(B\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (1 2)
  edict-ignore ())

(define-edict-rule $B0lCJ$N!V$?$$!W$r:o=|$9$k(B
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\($B$?$$(B\\|$B$?$,$k(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$k(B")

(define-edict-rule $B!V$7$?$$!W$NFCJL%k!<%k(B
  "^\\($B$7$?$$(B\\|$B$7$?$,$k(B\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

(define-edict-rule $B!VMh$?$$!W$NFCJL%k!<%k(B
  "^\\($BMh$?$$(B\\|$BMh$?$,$k(B\\)$"
  ()
  edict-subst-affix "$BMh$k(B")

(define-edict-rule $B!V$-$?$$!W$NFCJL%k!<%k(B
  "^\\($B$-$?$$(B\\|$B$-$?$,$k(B\\)$"
  ()
  edict-subst-affix "$B$/$k(B")

;; Flush auxilliary verbs after te form.
(define-edict-rule $B=uF0;l!<#1(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\($B$/(B\\|$B$F(B\\|$B$s$G(B\\)\\($B$$$k(B\\|$B$*$k(B\\|$B$$$^$9(B\\|$B$"$k(B\\|$B$*$/(B\\|$B$_$k(B\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule $B=uF0;l!<#1#a(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\($B$F(B\\|$B$s$G(B\\)\\($B$k(B\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule $B=uF0;l!<#2(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\($B$/(B\\|$B$F(B\\|$B$s$G(B\\)\\($B2<$5$$(B\\|$B$/$@$5$$(B\\|$B$J$5$$(B\\|$B$$$/(B\\|$B9T$/(B\\|$B$/$k(B\\|$BMh$k(B\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule $B=uF0;l!<#3(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\($B$/(B\\|$B$F(B\\|$B$s$G(B\\)\\(\\([$B$5:9(B]$B$7(B\\)?[$B$">e(B]$B$2$k(B\\|$B$d$k(B\\|$B$b$i$&(B\\|$B$$$?$@$/(B\\|$BD:$/(B\\|$B$/$l$k(B\\|$B$/$@$5$k(B\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule $B=uF0;l!<#4(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\($B$/(B\\|$B$F(B\\|$B$s$G(B\\)\\($B$9$k(B\\|$B@.$k(B\\|$B$J$k(B\\|$B$7$^$&(B\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-identity ()
  edict-ignore ())

(define-edict-rule modifiers
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)[$B$$$?$&$/$0$9$D$L$V$`$k(B]\\($B$i$7$$(B\\|$B$=$&(B\\|$B$h$&(B\\)$")
  ()
  edict-subst-affix edict-identity "")

(define-edict-rule humble
  (concat "\\($B$*(B\\)\\(" edict-category-c  "\\|" edict-category-h
	  "\\)+\\([$B$$$-$.$7$A$K$S$_$j(B]\\)\\($B$K@.$k(B\\|$B$K$J$k(B\\|$B$9$k(B\\|$B$$$?$9(B\\|$B?=$7>e$2$k(B\\|$B$b$&$7$"$2$k(B\\)$")
  ()
  edict-subst-modified-affix
  edict-ignore ()
  edict-identity ()
  edict-modify-verb (1 2)
  edict-ignore ())

;; Volitional
(define-edict-rule $B8^CJ$N!V$*$&!W(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\([$B$*$3$4$=$H$N$\$b$m(B]\\)\\($B$&(B\\)$")
  ()
  edict-subst-modified-affix
  edict-identity ()
  edict-modify-verb (4 2)
  edict-ignore ())

(define-edict-rule $B0lCJ$N!V$h$&!W(B
  (concat "\\(" edict-category-c
	  "\\|[$B$$$-$.$7$A$K$S$_$j$($1$2$;$F$M$Y$a$l(B]\\)\\($B$h$&(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$k(B")

(define-edict-rule $B!VMh$h$&!W$NFCJL%k!<%k(B
  "\\($BMh$h$&(B\\)$"
  ()
  edict-subst-affix "$BMh$k(B")

(define-edict-rule $B!V$3$h$&!W$NFCJL%k!<%k(B
  "\\($B$3$h$&(B\\)$"
  ()
  edict-subst-affix "$B$/$k(B")

(define-edict-rule $B!V$7$h$&!W$NFCJL%k!<%k(B
  "\\($B$7$h$&(B\\)$"
  ()
  edict-subst-affix "$B$9$k(B")

(define-edict-rule $B$F$7$^$&(B
  "[^$B$s(B]\\($B$A$c$&(B\\)$"
  ()
  edict-subst-affix "$B$F$7$^$&(B")

(define-edict-rule $B$G$7$^$&(B
  "$B$s(B\\($B$A$c$&(B\\)$"
  ()
  edict-subst-affix "$B$G$7$^$&(B")

;; Honorific prefixes
(define-edict-rule $B7I8l$N@\F,<-(B
  "^\\($B$*(B\\|$B8f(B\\|$B$4(B\\)"
  ()
  edict-subst-affix "")

;; Various forms of adjectives.
(define-edict-rule $B7AMF;l!<$/(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$/(B\\)$")
  "\\($B$+(B\\($B$l$k(B\\|$B$;$k(B\\|$B$J$$(B\\|$B$:(B\\)\\|$B$-(B\\($B$^$9(B\\|$B$^$;$s(B\\|$B$?$$(B\\|$B$J$+$i(B\\|$B$D$D(B\\|$B$d$5$$(B\\|$B$K$/$$(B\\|$B$=$&$J(B\\)\\|$B$1(B\\($B$P(B\\|\\|$B$k(B\\)\\|$B$3$&(B\\|$B$$(B\\($B$?(B\\|$B$?$i(B\\|$B$?$j(B\\|$B$?$m$&(B\\|$B$F(B\\|$B$F$$$k(B\\)\\)$"
  edict-subst-affix edict-identity "$B$$(B")

(define-edict-rule $B7AMF;l!<$/$F(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$/$F(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$$(B")

(define-edict-rule $B7AMF;l!<$/$J$$(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$/$J$$(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$$(B")

(define-edict-rule $B7AMF;l!<$+$i$:(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$+$i$:(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$$(B")

(define-edict-rule $B7AMF;l!<$+$C$?(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$+$C$?(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$$(B")

(define-edict-rule $B7AMF;l!<$J$$(B
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\(\\($B$8$c(B\\|$B$G$O(B\\)\\($B$J$$(B\\|$B$"$j$^$;$s(B\\)\\)$")
  ()
  edict-subst-affix edict-identity "")

(define-edict-rule $B7AMF;l!<$1$l$P(B
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$1$l$P(B\\)$")
  ()
  edict-subst-affix edict-identity "$B$$(B")

;; Other affixes

(define-edict-rule other-suffixes
  (concat "\\(" edict-category-c "\\|" edict-category-h
	  "\\)\\($BE*(B\\|$B$F$-(B\\|$B$b$N(B\\|$BJ*(B\\|$B<T(B\\|$B<0(B\\|$BCf(B\\|$B0w(B\\|$B$9$k(B\\|$B$5$s(B\\|$B@h@8(B\\|$BMM(B\\|$B$5$^(B\\|$B$A$c$s(B\\|$B7/(B\\|$B$/$s(B\\|$B20(B\\)$")
  ()
  edict-subst-affix edict-identity "")

(define-edict-rule other-prefixes
  (concat "^\\($B:r(B\\|$BMh(B\\|$BA4(B\\|$BH>(B\\|$BKh(B\\)" edict-category-c)
  ()
  edict-subst-affix "")

;; Canonicalize number expressions
(define-edict-rule numbers
  (concat "^\\([0-9$B#0(B-$B#90lFs;0;M8^O;<7H,6e==I4@iK|2/(B]+\\)\\("
	  edict-category-c "\\|" edict-category-h "\\)")
  ()
  edict-subst-affix "$B0l(B" edict-identity )

(define-edict-rule $B?t$J$7(B
  (concat "^\\([0-9$B#0(B-$B#90lFs;0;M8^O;<7H,6e==I4@iK|2/(B]+\\)\\("
	  edict-category-c "\\|" edict-category-h "\\)")
  ()
  edict-subst-affix edict-ignore edict-identity )

(define-edict-rule $B$@(B
  "\\($B$8$c$J$$(B\\|$B$G$O$J$$(B\\|$B$@$C$?(B\\|$B$@$m$&(B\\)$"
  ()
  edict-subst-affix "$B$@(B")

(define-edict-rule $B$G$9(B
  "\\($B$8$c$"$j$^$;$s(B\\|$B$G$O$"$j$^$;$s(B\\|$B$G$7$g$&(B\\)$"
  ()
  edict-subst-affix "$B$G$9(B")

(define-edict-rule $B$G$9(B/$B$@(B
  "\\($B$G$9(B\\)$"
  ()
  edict-subst-affix "$B$@(B")

(define-edict-rule copula
  (concat "\\(" edict-category-c "\\|" edict-category-h "\\)\\($B$@(B\\|$B$G$9(B\\)$")
  ()
  edict-subst-affix edict-identity edict-ignore)

;;; edict-japanese.el ends here
