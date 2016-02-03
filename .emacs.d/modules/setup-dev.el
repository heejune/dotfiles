;;; package --- summary

;;; Commentary:
;;; Code


;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: projejctile
;;(require 'projectile)
                                        ;(projectile-global-mode)
;; (setq projectile-enable-caching t)

;; cmake-mode
(require 'cmake-mode)

;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;;(ac-set-trigger-key "TAB")
;;(ac-set-trigger-key "<tab>")

;;(require 'auto-complete-clang)

;;(global-set-key (kbd "C-c `") 'ac-complete-clang)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; prevent the error c-mode-map nil
(require 'cc-mode)

(load-library "setup-python")
;;(require 'setup-ggtags)
(require 'setup-helm)
(require 'setup-company)
(require 'setup-cedet)

(provide 'setup-dev)
