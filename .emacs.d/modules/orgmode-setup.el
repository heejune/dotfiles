

;;;(require 'org)

(eval-after-load 'org
  (lambda()
    (require 'ob-C)
    (require 'org-bullets)
    (require 'ox-taskjuggler)
    (require 'ox-latex)

    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (setq org-log-done t)

    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (add-to-list 'org-export-backends 'taskjuggler)

    ;;; http://orgmode.org/worg/org-dependencies.html
    ;;; setup source code syntax highlight
    (setq org-latex-listings t)
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color"))

;;; setup beamer
;;; http://orgmode.org/worg/exporters/beamer/ox-beamer.html
    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass\[presentation\]\{beamer\}"
                   ("\\section\{%s\}" . "\\section*\{%s\}")
                   ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                   ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


    ;; Fontify the whole line for headings (with a background color).
    (setq org-fontify-whole-heading-line t)

    ;; fontify code in code blocks
    (setq org-src-fontify-natively t)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '( (perl . t)
        (ruby . t)
        (sh . t)
        (python . t)
        (emacs-lisp . t)
        (matlab . t)
        (C . t)))

    ;; agenda setup
    (setq org-agenda-files '("~/Dropbox/private/orgs"))

    ;; override the default keyword
    (setq org-todo-keywords
          '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

    )
)


(provide 'orgmode-setup)
