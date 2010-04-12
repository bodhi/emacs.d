;; CYGWIN
(let* ((cygwin-root "c:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
  (setq exec-path (cons cygwin-bin exec-path)))

(setq shell-file-name "bash")
(setq explicit-shell-file-name "bash")

(require 'cygwin-mount)
(cygwin-mount-activate)

(setenv "PATH" (concat "/cygdrive/c/Program Files/Git/bin" ";" (getenv "PATH")))

;; default face
(set-face-attribute 'default nil :height 110 :foreground "grey95" :background "grey5")
(set-face-attribute 'mode-line nil :background "grey75")
(set-face-attribute 'font-lock-comment-face nil :foreground "chocolate3")

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
      '((width . 40) (height . 45) (left . 30) (top . 30)))

(server-start)
