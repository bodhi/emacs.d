(setq default-frame-alist
      '((width . 160) (height . 50) (left . 160) (top . 160)))
;;  Use ctrl+function keys f10 through f13 as a 'zoom' control.
(global-set-key [C-f13] 'zoom-way-out)
(global-set-key [C-f12] 'zoom-out)
(global-set-key [C-f11] 'zoom-in)
(global-set-key [C-f10] 'zoom-way-in)
