

;; function-args
(require 'function-args)
;(fa-config-default)
;(define-key c-mode-map  [(tab)] 'moo-complete)
;(define-key c++-mode-map  [(tab)] 'moo-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;(delete 'company-semantic company-backends)
;;(define-key c-mode-map  [(control tab)] 'company-complete)
;;(define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
;;(add-to-list 'company-backends 'company-c-headers)

;; Source code completion with Clang
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

(provide 'setup-company)
