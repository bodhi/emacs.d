;; CYGWIN

(let* ((cygwin-root "c:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
  (setq exec-path (cons cygwin-bin exec-path)))

(setq shell-file-name "bash")
(setq explicit-shell-file-name "bash")

;; default face
(set-face-font 'default "-outline-Envy Code R-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")

(server-start)

;; C# mode
(autoload 'csharp-mode "csharp-mode.el" "Major mode for editing C# code." t)
(setq auto-mode-alist
   (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;  Use ctrl+function keys f9 through f12 as a 'zoom' control.
(global-set-key [C-f12] 'zoom-way-out)
(global-set-key [C-f11] 'zoom-out)
(global-set-key [C-f10] 'zoom-in)
(global-set-key [C-f9] 'zoom-way-in)

(global-set-key [C-pause] 'calc)

(setq default-frame-alist
      '((width . 130) (height . 45) (left . 30) (top . 30)))
