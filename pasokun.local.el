(setq default-frame-alist
      '((width . 120) (height . 48) (left . 80) (top . 80)))
;;  Use ctrl+function keys f10 through f13 as a 'zoom' control.
(global-set-key [C-f13] 'zoom-way-out)
(global-set-key [C-f12] 'zoom-out)
(global-set-key [C-f11] 'zoom-in)
(global-set-key [C-f10] 'zoom-way-in)

(global-set-key "\C-z" nil)
(global-set-key "\C-x\C-z" nil)
(global-set-key "\M-h" 'ns-do-hide-emacs)

(setq ns-command-modifier 'meta)
