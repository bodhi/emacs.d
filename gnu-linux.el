(set-face-font 'default "Inconsolata-dz")
(set-face-attribute 'default nil :height 90)
(set-face-attribute 'default nil :foreground "grey95")

(server-start)

;;  Use ctrl+function keys f9 through f12 as a 'zoom' control.
(global-set-key [C-f12] 'zoom-way-out)
(global-set-key [C-f11] 'zoom-out)
(global-set-key [C-f10] 'zoom-in)
(global-set-key [C-f9] 'zoom-way-in)

(global-set-key [C-pause] 'calc)

(set-scroll-bar-mode 'right)
