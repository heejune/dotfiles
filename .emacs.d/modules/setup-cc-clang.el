;;; Code:

;; refs: http://stackoverflow.com/questions/663588/emacs-c-mode-incorrect-indentation

;; prevent the error c-mode-map nil
(require 'cc-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Note. Uninstalled.
;; http://stackoverflow.com/questions/663588/emacs-c-mode-incorrect-indentation
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)  ; use spaces only if nil
  )

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; http://blog.binchen.org/posts/ccjava-code-indentation-in-emacs.html
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              ;; indent
              (fix-c-indent-offset-according-to-syntax-context 'substatement-open 0))
            ))

;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq-default c-basic-offset 4)

;; https://www.emacswiki.org/emacs/CPlusPlusMode
(require 'cl)

(defun file-in-directory-list-p (file dirlist)
  "Returns true if the file specified is contained within one of
the directories in the list. The directories must also exist."
  (let ((dirs (mapcar 'expand-file-name dirlist))
        (filedir (expand-file-name (file-name-directory file))))
    (and
     (file-directory-p filedir)
     (member-if (lambda (x) ; Check directory prefix matches
                  (string-match (substring x 0 (min(length filedir) (length x))) filedir))
                dirs))))

(defun buffer-standard-include-p ()
  "Returns true if the current buffer is contained within one of
the directories in the INCLUDE environment variable."
  (and (getenv "INCLUDE")
       (file-in-directory-list-p buffer-file-name (split-string (getenv "INCLUDE") path-separator))))

(add-to-list 'magic-fallback-mode-alist '(buffer-standard-include-p . c++-mode))

;; ---
                                        ; style I want to use in c++ mode
(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

;; Also uninstalled...
(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

;; disable hungry mode.. 2016-06-14 by hekim
;;(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;(define-key c++-mode-map "\C-ct" 'some-function-i-want-to-call)

;; http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore
;; Fix indent when tab pressed
;; (setq tab-always-indent 'complete)

(require 'company)
(setq w32-pipe-read-delay 0)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))


(provide 'setup-cc-clang)
