;;; package --- summary

;;; Commentary:
;;; ref: http://pages.sachachua.com/.emacs.d/Sacha.html
;;; http://doc.norang.ca/org-mode.html
;;; Code


;; Package: org
(require 'org)

(eval-after-load 'org
  (lambda()
    (require 'ob-C)
    (require 'org-bullets)
    ;;    (require 'ox-taskjuggler) not use anymore...
    (require 'ox-latex)

    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (setq org-log-done t)

    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (add-to-list 'org-export-backends 'taskjuggler)

    (add-to-list 'org-src-lang-modes '("http" . ob-http))

    (defvar org-dir (expand-file-name  "private/orgs/" (file-name-as-directory dropbox-dir))
      "Org files home directory.") ;; D:\Dropbox\private\orgs in Windows-nt
    (defvar org-refile-filename (expand-file-name  "refile.org" org-dir)
      "Org files home directory.")

    ;;; http://orgmode.org/worg/org-dependencies.html
    ;;; setup source code syntax highlight
    (setq org-latex-listings t)
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color"))

    ;; Let the exporter use the -shell-escape option to let latex
    ;; execute external programs.
    ;; This obviously and can be dangerous to activate!
    ;; (setq org-latex-pdf-process
    ;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; (setq org-latex-to-pdf-process
    ;;       '("xelatex -interaction nonstopmode -output-directory %o %f"
    ;;         "xelatex -interaction nonstopmode -output-directory %o %f"
    ;;         "xelatex -interaction nonstopmode -output-directory %o %f"))

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
        (http . t)
        (C . t)))

    ;; agenda setup
    ;;(setq org-agenda-files (list org-dir))
    ;;(add-to-list 'org-agenda-files (expand-file-name "D:\storage\Dropbox\private\orgs"))
    (setq org-agenda-files '("D:\\Dropbox\\private\\orgs"))
;;------------------------------------------------------------------------------
;; Load org agenda files
;;------------------------------------------------------------------------------
;;(load-library "find-lisp")
;;(setq org-agenda-files (find-lisp-find-files org-dir "\.org$"))

    ;; override the default keyword
    ;; (setq org-todo-keywords
    ;;       '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

    ;; setup ref http://doc.norang.ca/org-mode.html
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "IN-PROGRESS(n)" "|" "DONE(d)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))

    (setq org-todo-keyword-faces
          (quote (("TODO" :foreground "red" :weight bold)
                  ("IN-PROGRESS" :foreground "blue" :weight bold)
                  ("DONE" :foreground "forest green" :weight bold)
                  ("WAITING" :foreground "orange" :weight bold)
                  ("HOLD" :foreground "magenta" :weight bold)
                  ("CANCELLED" :foreground "forest green" :weight bold)
                  ("MEETING" :foreground "forest green" :weight bold)  )))

    (setq org-use-fast-todo-selection t)

    (setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")

    (setq org-agenda-use-tag-inheritance t)

    (setq org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("HOLD" ("WAITING") ("HOLD" . t))
                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("IN-PROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

    (setq org-directory org-dir)
    (setq org-default-notes-file (expand-file-name "refile.org" org-dir))

    ;; I use C-c c to start capture mode
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Allow setting single tags without the menu
    (setq org-fast-tag-selection-single-key (quote expert))

    ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
    (setq org-capture-templates
          (quote (("t" "todo" entry (file org-default-notes-file)
                   "* TODO %?  :REFILE:\n%U\n%a\n" :clock-in t :clock-resume t)
                  ("r" "respond" entry (file org-default-notes-file)
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                  ("n" "note" entry (file org-default-notes-file)
                   "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                  ("j" "Journal" entry (file+datetree org-default-notes-file)
                   "* %?\n%U\n" :clock-in t :clock-resume t)
                  ("w" "org-protocol" entry (file org-default-notes-file)
                   "* TODO Review %c\n%U\n" :immediate-finish t)
                  )))

    (add-to-list 'org-capture-templates '("b" "Buy list" entry (file org-default-notes-file)
                                          "* %? :BUYLIST:\n%U\n%a\n" :clock-in t :clock-resume t) t)


    ;; ref https://www.reddit.com/r/emacs/comments/3tc4sv/what_ways_do_you_use_orgmode/cx5yiqy
    (setq org-agenda-custom-commands
      ;; The " " here is the shortcut for this agenda, so `C-c a SPC`
      '((" " "Agenda"
         ((agenda "" nil)
          ;; All items with the "REFILE" tag, everything in refile.org
          ;; automatically gets that applied
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          ;; All "INPROGRESS" todo items
          (todo "IN-PROGRESS"
                ((org-agenda-overriding-header "Current work")))
          ;; All headings with the "support" tag
          (tags "BUYLIST/!"
                ((org-agenda-overriding-header "Things to buy")))
          ;; All "NEESREVIEW" todo items
          (todo "NEEDSREVIEW"
                ((org-agenda-overriding-header "Waiting on reviews")))
          ;; All "WAITING" items without a "support" tag
          (tags "WAITING"
                ((org-agenda-overriding-header "Waiting for something")))
          ;; All TODO items
          (todo "TODO"
                ((org-agenda-overriding-header "Task list")
                 ;; sort by time, priority, and category
                 (org-agenda-sorting-strategy
                  '(time-up priority-down category-keep))))
          ;; Everything on hold
          (todo "HOLD"
                ((org-agenda-overriding-header "On-hold")))
          ;; All headings with the "recurring" tag
          (tags "recurring"
                ((org-agenda-overriding-header "Recurring"))
                ) )
         nil)))

    (setq org-startup-with-inline-images t)

    ;; setup tags
    ;; setting tags http://orgmode.org/manual/Setting-tags.html
    (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

    ;; refile setup http://doc.norang.ca/org-mode.html
    ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))

                                        ; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)

                                        ; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)

                                        ; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))

                                        ; Use IDO for both buffer and file completion and ido-everywhere to t
    (setq org-completion-use-ido t)
;;    (setq ido-everywhere t)
;;    (setq ido-max-directory-size 100000)
;;    (ido-mode (quote both))
                                        ; Use the current window when visiting files and buffers with ido
;;    (setq ido-default-file-method 'selected-window)
;;    (setq ido-default-buffer-method 'selected-window)
                                        ; Use the current window for indirect buffer display
    (setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
    ;; http://doc.rix.si/cce/cce-org.html
                                        ; Exclude DONE state tasks from refile targets
    (defun bh/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))

    (setq org-refile-target-verify-function 'bh/verify-refile-target)

    ;; http://superuser.com/questions/373617/re-file-outline-tree-into-new-org-mode-file
    (defun subtree-to-new-file ()
      (interactive)
      "sloppily assists in moving an org subtree to a new file"
      (org-copy-subtree nil t)
;;; This long setq statement gets the title of the first heading, to use as a default filename for the new .org file.
      (setq first-heading
            (with-temp-buffer
              (yank)
              (beginning-of-buffer)
              (search-forward " " nil nil 1)
              (setq title-start (point))
              (end-of-visual-line)
              (setq title-end (point))
              (setq first-heading (buffer-substring title-start title-end))
              ))
      (setq def-filename (concat first-heading ".org"))
      (let ((insert-default-directory t))
        (find-file-other-window
         (read-file-name "Move subtree to file:" def-filename)
         ))
      (org-paste-subtree)
;;; this final command adds the new .org file to the agenda
      (org-agenda-file-to-front)
      )

    ;; http://stackoverflow.com/questions/18168146/how-to-remove-just-some-tags-in-org-mode-using-a-custom-function
    (defun zin/org-remove-tag (tag)
    "Removes `TAG' from current list of tags if it's refiled."
    (org-toggle-tag tag 'off))

(defun zin/remove-refile-tag ()
   "Removes `:REFILE:' tag from list of tags when an item is refiled."
  (zin/org-remove-tag "REFILE"))

(add-hook 'org-after-refile-insert-hook 'zin/remove-refile-tag)

    ) ;; lambda
) ;; eval-after-load


(provide 'setup-orgmode)
