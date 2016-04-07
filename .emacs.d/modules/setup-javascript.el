;; ref https://github.com/jfroffice/emacs.d/blob/master/js2-mode.el

;;; Code:

;(require 'js2-mode)
(require 'js2-highlight-vars)

(autoload 'js2-mode "js2" nil t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ref https://elpa.gnu.org/packages/js2-mode.html
;;   (add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(replace-auto-mode 'javascript-mode 'js2-mode)
(replace-auto-mode 'js-mode 'js2-mode)

(defun my-js2-mode-hook ()
  ;; js2 configuration
  (setq js2-basic-offset 4)
  (setq js2-mode-indent-ignore-first-tab t)
  (setq js2-highlight-external-variables nil)
  (setq js2-highlight-level 3)
  (setq js2-mirror-mode nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-bounce-indent-p nil)
  (setq js2-pretty-multiline-declarations t)
  (setq js2-use-font-lock-faces t)
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode)))

(defun my-javascript-mode-hook ()
  (js2-refactor-mode t)
  (tern-mode t)
  (add-to-list 'company-backends 'company-tern))

(add-hook 'js2-mode-hook
          (lambda ()
               (setq js2-basic-offset 4)
               (my-js2-mode-hook)
               ;; http://stackoverflow.com/questions/2370028/emacs-js2-mode-disable-indenting-completely
               ;(define-key js2-mode-map [tab] 'self-insert-command))
              ))
;;(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(require 'flymake-jshint)
(add-hook 'js2-mode-hook 'flymake-mode)
(setq jshint-configuration-path "~/.jshint.json")

(add-hook
 'js2-mode-hook
 'my-javascript-mode-hook)

(setq js2-basic-offset 4)

(eval-after-load
    'flycheck
  (lambda ()
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    ;; Disable jshint
    (setq-default
     flycheck-disabled-checkers
     (append flycheck-disabled-checkers
             '(javascript-jshint)))))

(provide 'setup-javascript)
