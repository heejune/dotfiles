;;; package --- Summary

;;; Commentary:

;;; Code:

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
;; linum-mode doens't work as expected when increased text size
;; 2016-04-02 hekim
;;(global-linum-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; load theme
;;(load-theme 'monokai t)
(load-theme 'leuven t)                  ; For Emacs 24+.
;;(load-theme 'deeper-blue t)

;; hide toolbar
(if window-system
    (tool-bar-mode -1)
  )

;; nyan-mode
(require 'nyan-mode)
(nyan-mode 1)

(provide 'ui)
;;; ui.el ends here
