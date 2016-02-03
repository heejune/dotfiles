;;; package --- Summary

;;; Commentary:

;;; Code:

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; load theme
(load-theme 'monokai t)
;;(load-theme 'leuven t)                  ; For Emacs 24+.

(provide 'ui)
;;; ui.el ends here
