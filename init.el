(prefer-coding-system 'utf-8)

;; Undo some changes in emacs 23.1
(setq inhibit-startup-screen t)
(transient-mark-mode -1)

(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (let ((kill-read-only-ok t)
	(buffer-read-only t))
    (kill-line arg)))

(global-set-key "\M-k" 'copy-line)


;; append some additional paths to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))

(setq imenu-auto-rescan t)       ; ensure function names auto-refresh

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; processing
(add-to-list 'auto-mode-alist '("\\.pde\\'" . java-mode))

(require 'markdown-mode)

(require 'font-lock)
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^[rR]akefile" . ruby-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)
	     ))


(defun ruby-test-file()
  (interactive)
  (let* ((filename (buffer-file-name))
         (pkg-root
          (cond
           ((string-match "^\\(.*\\)/app/models/\\(.*\\)\\.rb$" filename)
            (concat (match-string 1 filename) "/test/unit/"))
           ((string-match "^\\(.*\\)/app/controllers/\\(.*\\)\\.rb$" filename)
            (concat (match-string 1 filename) "/test/functional/"))))
         (unit-test (concat pkg-root (match-string 2 filename) "_test.rb")))
    (compile (if (file-readable-p unit-test)
        (concat "/opt/local/bin/ruby " unit-test)
    (concat "/opt/local/bin/ruby " filename)))))

;; run the current test function

(require 'which-func)
(defun ruby-test-function ()
  "Test the current ruby function (must be runable via ruby <buffer> --name <test>)."
  (interactive)
  (save-excursion (imenu "*Rescan*"))
  (let* ((funname (which-function))
     (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname))))
    (compile (concat "/opt/local/bin/ruby " (buffer-file-name) " --name " fn))))


(require 'compile)
(require 'inf-ruby)
(add-to-list 'compilation-error-regexp-alist
    '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:"
     1 2))
(add-to-list 'compilation-error-regexp-alist
             '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))


(define-key ruby-mode-map (kbd "C-c C-t") 'ruby-test-function)
(define-key ruby-mode-map (kbd "C-c C-f") 'ruby-test-file)

(defun compilation-mode-switch-to-inferior-ruby ()
  "switch buffer to inferior-ruby-mode and make the buffer writeable"
  (interactive)
  (toggle-read-only -1)
   (inferior-ruby-mode))
(defun compilation-mode-switch-from-inferior-ruby ()
  "switch buffer to compilation-mode"
  (interactive)
  (toggle-read-only 1)
   (compilation-mode))
(define-key compilation-mode-map (kbd "C-c C-i") 'compilation-mode-switch-to-inferior-ruby)
(define-key inferior-ruby-mode-map (kbd "C-c C-i") 'compilation-mode-switch-from-inferior-ruby)

(setq mac-allow-anti-aliasing nil)

(require 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;(require 'yaml-mode)
;;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


(require 'psvn)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)


(global-hl-line-mode 1)
(set-face-background 'highlight "grey10")  ;; Emacs 22 Only

(setq mmm-mode-prefix-key [(control 99) 44])
(add-to-list 'load-path "~/.emacs.d/site-lisp/mmm-mode-0.4.8")
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(mmm-add-classes
 '((embedded-ruby
    :submode ruby-mode
    :front "<%[=#]?"
    :back "%>"
    :insert ((?r eruby-directive nil @ "<%" @ " " _ " " @ "%>" @)
             (?= eruby-directive-out nil @ "<%=" @ " " _ " " @ "%>" @)
	     (?e eruby-directive-end nil @ "<%" @ " end " @ "%>" _ @)))))
;; (mmm-add-classes
;;  '((embedded-html
;;     :submode php-mode
;;     :front "<\\?php"
;;     :back "\\?>"
;;     :insert ((?p php-directive nil @ "<?php" @ " " _ " " @ "?>" @)))))
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "style=\""
    :back "\"")))
;; (mmm-add-classes
;;  '((embedded-javascript
;;     :submode javascript-mode ;; javascript-generic-mode
;;     :face mmm-declaration-submode-face
;;     :front "<script\[^>\]*>"
;;     :back "</script>")))
;; (mmm-add-classes
;;  '((embedded-javascript-attribute
;;     :submode javascript-mode ;; javascript-generic-mode
;;     :face mmm-declaration-submode-face
;;     :front "\\bon\\w+=\\s-*\""
;;     :back "\"")))

;; What files to invoke the new html-mode for?
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; What features should be turned on in this html-mode?
(add-to-list 'mmm-mode-ext-classes-alist
         '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist
         '(html-mode nil embedded-ruby))
;; (add-to-list 'mmm-mode-ext-classes-alist
;;          '(html-mode nil embedded-javascript))
;; (add-to-list 'mmm-mode-ext-classes-alist
;;          '(html-mode nil embedded-javascript-attribute))
;; (add-to-list 'mmm-mode-ext-classes-alist
;;          '(php-mode nil embedded-html))


(global-set-key "\C-x\C-g" 'find-file-at-point)

(global-set-key (kbd "") `delete-char)
(global-set-key (kbd "\C-g") `goto-line)
(global-set-key '[C-tab] 'other-window)

(setq-default delete-key-deletes-forward t)

(fset 'reindent-newline-and-indent
   [tab ?\C-e return tab])

(global-set-key (kbd "C-<return>") 'reindent-newline-and-indent)


(setq-default indent-tabs-mode nil)
;; Set the tab width
(setq c-default-style "bsd")
(setq c-basic-offset 2)
;(setq default-tab-width 4)
(setq tab-width 2)
;(setq c-basic-indent 4)
;(setq-default php-indent 4)
;(setq c-basic-offset 4)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(fringe-mode (quote (5 . 5)) nil (fringe))
 '(global-font-lock-mode t nil (font-core))
 '(grep-find-ignored-directories (quote ("CVS" ".hg" "{arch}" ".svn")))
 '(indicate-empty-lines t)
 '(js2-idle-timer-delay 1.0)
 '(make-backup-files nil)
 '(midnight-mode t nil (midnight))
 '(parens-require-spaces nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tuareg-library-path "/opt/local/lib/ocaml/"))
 '(which-func-modes (quote (emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode ruby-mode)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "grey5" :foreground "grey80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :family "inconsolata"))))
 '(cursor ((t (:background "green"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "chocolate4"))))
 '(fringe ((((class color) (background dark)) (:background "grey5"))))
 '(mmm-declaration-submode-face ((t (:background "dark slate gray"))))
 '(mmm-default-submode-face ((t (:background "Grey25"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "grey50" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(region ((t (:background "blue3"))))
 '(variable-pitch ((t (:height 1.1 :family "baskerville")))))

;;(global-set-key (kbd "C-<tab>") 'next-window)

(blink-cursor-mode -1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(add-to-list 'load-path "~/.emacs.d/site-lisp/http-emacs")
(require 'simple-wiki-completion)

;; Mark diary entries in calendar

(setq diary-file "~/calendar")
(setq mark-diary-entries-in-calendar t)
(setq view-diary-entries-initially t)
(setq today-visible-calendar-hook 'calendar-mark-today)
(require 'calendar)
;;(calendar)
(european-calendar)


;; ============= MUSE ======================
(add-to-list 'load-path "~/.emacs.d/site-lisp/muse/lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/planner/")
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/remember/")

(require 'muse)
(require 'muse-html)

;; get CamelCase links for Muse
(require 'muse-wiki)
(setq muse-wiki-allow-nonexistent-wikiword t)

;; use footnotes in muse-mode
(add-hook 'muse-mode-hook 'footnote-mode)

;; Set muse project list
(setq muse-project-alist
      '(("Pages"
	 ("~/Pages" :default "index")
	 (:base "html" :path "~/Sites/Pages"))
	("b.5263"
	 ("~/Documents/b.5263" :default "index")
	 (:base "portfolio-html" :path "~/Sites/bodhi.5263.org"))
	("Masters"
	 ("~/Masters" :default "index")
	 (:base "html" :path "~/Sites/Masters"))))

(muse-derive-style "portfolio-html" "html"
		   :link-suffix ""
		   :header "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
    <head>
        <title><lisp>\n  (concat (muse-publishing-directive \"title\")\n          (let ((author (muse-publishing-directive \"author\")))\n            (if (not (string= author (user-full-name)))\n                (concat \" (by \" author \")\"))))</lisp></title>
        <meta name=\"generator\" content=\"muse.el\">
        <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"  content=\"<lisp>muse-html-meta-content-type</lisp>\">
        <lisp>(let ((maintainer (muse-style-element :maintainer)))\n        (when maintainer\n          (concat \"<link rev=\\\"made\\\" href=\\\"\" maintainer \"\\\">\")))\n    </lisp><lisp>\n      (muse-style-element :style-sheet muse-publishing-current-style)\n    </lisp>
    </head>
<body class=\"<lisp>(concat (muse-publishing-directive \"title\"))</lisp>\">
<div id=\"home-link\"><lisp>(if (not (string= (muse-publishing-directive \"title\") \"Bodhi.5263.org\")) (concat \"<a href=\\\"/\\\">Bodhi.5263.org</a> /\"))</lisp></div>
<h1><lisp>\n  (concat (muse-publishing-directive \"title\")\n          (let ((author (muse-publishing-directive \"author\")))\n            (if (not (string= author (user-full-name)))\n                (concat \" (by \" author \")\"))))</lisp></h1>
<!-- Page published by Emacs Muse begins here -->\n"
:style-sheet "<link rel=\"Stylesheet\" href=\"style.css\" type=\"text/css\">")



 (require 'planner)
;; (require 'planner-id)

;; (when (and (locate-library "planner")
;;        (locate-library "muse"))
;;   (setq planner-project "WikiPlans")
;;   (setq planner-directory "~/Plans")
;;   (setq planner-default-page "WelcomePage")
;;   ;; Tell muse about planner.  We use add-hook instead of
;;   ;; add-to-list because muse might not be loaded yet.
;;   (add-hook 'muse-project-alist
;;         (list planner-project
;;           (list planner-directory
;;             :default planner-default-page
;;             :major-mode 'planner-mode
;;             :visit-link 'planner-visit-link))))

;; (require 'remember)
;; (require 'remember-planner)
;; (setq remember-handler-functions '(remember-planner-append))
;; (setq remember-annotation-functions planner-annotation-functions)
;; (setq remember-append-to-planner-hook
;;       '(remember-planner-add-timestamp))

;;(require 'planner-diary)
;;(add-hook 'diary-display-hook 'fancy-diary-display)
;;(setq planner-diary-use-diary t)
;;(planner-diary-insinuate)

;; (setq planner-use-day-pages nil)


(setq user-mail-address "bodhi@5263.org")

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-w3m-1.4.4")
;;(require 'w3m-load)
;;(require 'mime-w3m)
;;(setq w3m-use-cookies t)

;;'(exec-path (quote ("/opt/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/X11R6/bin")))


(defun zoom-way-out() (interactive)
  (set-selective-display 0))
(defun zoom-way-in() (interactive)
  (set-selective-display 4))
(defun zoom-out() (interactive)
  (set-selective-display
   (if selective-display
       (if (or (= selective-display 0) (= selective-display 14))
	   0
	 (+ selective-display 2))
     0)))
(defun zoom-in() (interactive)
  (set-selective-display
   (if selective-display
       (if (= selective-display 0)
	   14
	 (if (= selective-display 4)
	     4
	   (- selective-display 2)))
     14)))

(defun maybe-load-file (name)
  "load ~/.emacs.d/<filename>.el if it exists"
  (let* ((name-var (cond ((symbolp name) (symbol-name name))
			 (t name)))
	 (safe-name-var (replace-regexp-in-string "/" "-" name-var))
	 (filename (concat "~/.emacs.d/" (downcase safe-name-var) ".el")))
    (if (file-exists-p filename)
      (load filename))))

(maybe-load-file system-type)
(maybe-load-file system-name)

(global-set-key [C-clear] 'calc)

(require 'actionscript-mode)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

(add-to-list 'load-path "~/.emacs.d/site-lisp/tuareg-mode-1.46.2")
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(fset 'comment-line
   [?\C-a ?\C-  ?\C-  down ?\M-\; up])
(global-set-key (kbd "C-M-;") 'comment-line)

(define-key cssm-mode-map (read-kbd-macro "C-'") 'cssm-complete-property)

(setq mouse-wheel-scroll-amount '(0.0001))

;; Macros
(fset 'dequotify-string
   [?\C-r ?\" ?\C-d ?\C-s ?\" left right backspace])

(global-set-key (kbd "C-'") 'dequotify-string)

;; Ruby macros
(fset 'rb-string-to-symbol
   [right ?\C-  left C-M-right backspace ?\C-w backspace ?: ?\C-y])

(fset 'rb-symbol-to-string
   [?\C-r ?: right ?\C-  C-M-right ?\C-w backspace ?\" ?\C-y])

(fset 'rb-extract-string
   [?\C-s ?\" ?\C-m left ?\C-  C-M-right ?\C-w ?\C-a ?\C-k ?\C-y ?\M-y])

(define-key ruby-mode-map (read-kbd-macro "C-\"") 'rb-symbol-to-string)
(define-key ruby-mode-map (read-kbd-macro "C-:") 'rb-string-to-symbol)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ri for ruby
(setq ri-ruby-script (expand-file-name "~/.emacs.d/site-lisp/ri-emacs.rb"))
(autoload 'ri (expand-file-name "~/.emacs.d/site-lisp/ri-ruby.el") nil t)

;; Cribbed from http://sami.samhuri.net/2007/6/23/emacs-for-textmate-junkies
(defun wrap-region (left right beg end)
  "Wrap the region in arbitrary text, LEFT goes to the left and RIGHT goes to the right."
  (interactive)
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)))

(defmacro wrap-region-with-function (left right)
  "Returns a function which, when called, will interactively `wrap-region-or-insert' using LEFT and RIGHT."
  `(lambda () (interactive)
     (wrap-region-or-insert ,left ,right)))

(defun wrap-region-with-tag-or-insert ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'wrap-region-with-tag)
    (insert "<")))

(defun wrap-region-with-tag (tag beg end)
  "Wrap the region in the given HTML/XML tag using `wrap-region'. If any
attributes are specified then they are only included in the opening tag."
  (interactive "*sTag (including attributes): \nr")
  (let* ((elems    (split-string tag " "))
         (tag-name (car elems))
         (right    (concat "</" tag-name ">")))
    (if (= 1 (length elems))
        (wrap-region (concat "<" tag-name ">") right beg end)
      (wrap-region (concat "<" tag ">") right beg end))))

(defun wrap-region-or-insert (left right)
  "Wrap the region with `wrap-region' if an active region is marked, otherwise insert LEFT at point."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (wrap-region left right (region-beginning) (region-end))
    (insert left)))

(global-set-key "'"  (wrap-region-with-function "'" "'"))
(global-set-key "\"" (wrap-region-with-function "\"" "\""))
(global-set-key "`"  (wrap-region-with-function "`" "`"))
(global-set-key "("  (wrap-region-with-function "(" ")"))
(global-set-key "["  (wrap-region-with-function "[" "]"))
(global-set-key "{"  (wrap-region-with-function "{" "}"))
(global-set-key "<"  'wrap-region-with-tag-or-insert) ;; I opted not to have a wrap-with-angle-brackets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Obj-c stuff from (http://hutley.net/brett/emacs/integrating-emacs-and-xcode/)

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))

(defun bh-compile ()
  (interactive)
  (let ((df (directory-files "."))
        (has-proj-file nil)
        )
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t))))
      (setq df (cdr df)))
    (if has-proj-file
        (compile "xcodebuild -activeconfiguration")
      (compile "make"))))

(defun bh-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        ;; OK, we got a .h file, if a .m file exists we'll assume it's
        ; an objective c file. Otherwise, we'll look for a .cpp file.
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file)
              (progn (objc-mode))
            (if (file-exists-p dot-cpp-file)
                (c++-mode)))))))

(add-hook 'find-file-hook 'bh-choose-header-mode)

(define-key objc-mode-map [f5] 'bh-compile)
(define-key objc-mode-map [f6] 'next-error)

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*")))
    (message "Compilation Finished"))

  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)


(setq slime-net-coding-system 'utf-8-unix)
(setq inferior-lisp-program "/opt/local/bin/sbcl --noinform")
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime-2009-06-15")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(slime-setup '(slime-repl))

;; igrep customisations
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)
(autoload 'grep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'egrep "igrep"
  "*Run `egrep`..." t)
(autoload 'fgrep "igrep"
  "*Run `fgrep`..." t)
(autoload 'agrep "igrep"
  "*Run `agrep`..." t)
(autoload 'grep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'egrep-find "igrep"
  "*Run `egrep` via `find`..." t)
(autoload 'fgrep-find "igrep"
  "*Run `fgrep` via `find`..." t)
(autoload 'agrep-find "igrep"
  "*Run `agrep` via `find`..." t)
;; To ignore case by default:
(setq igrep-case-fold-search t)
;; To search subdirectories by default:
(setq igrep-find t)

(setq igrep-find-prune-clause
      (format "-type d %s -name RCS -o -name CVS -o -name SCCS -o -name .svn %s"
	      (shell-quote-argument "(")
	      (shell-quote-argument ")")))

;; LaTeX stuff
(add-hook 'LaTeX-mode-hook 'reftex-mode) ;; with AucTeX
;;(add-hook 'latex-mode-hook 'turn-on-reftex) ;; Emacs LaTeX mode

(require 'osx-osascript)

(setf kill-whole-line t)
(setf show-trailing-whitespace t)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(setf comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
(setf comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
(setf comint-scroll-show-maximum-output t) ; scroll to show max possible output
(setf comint-completion-autolist t)        ; show completion list when ambiguous
(setf comint-input-ignoredups t)           ; no duplicates in command history
(setf comint-completion-addsuffix t)       ; insert space/slash after file completion

;; Inline-Image
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook 'turn-on-iimage-mode)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; Takes a multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(require 'egg)
(define-key egg-hide-show-map [tab] 'egg-section-cmd-toggle-hide-show)
(define-key egg-hide-show-map [backtab] 'egg-section-cmd-toggle-hide-show-children)

(add-hook 'csharp-mode-hook 'c-subword-mode)

;; Supercollider mode
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/supercollider/"))

(autoload 'longlines-mode "longlines.el" "Minor mode for editing long lines." t)
(add-hook 'text-mode-hook 'longlines-mode)
;;(setf longlines-wrap-follows-window-size t)

;; RCIRC config
;; Join these channels at startup.
(setq rcirc-server-alist '(("irc.freenode.net")))
(setq rcirc-default-nick "bodhi")

(global-set-key "\M-["  'blink-matching-open)