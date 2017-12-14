;;; package --- summary

;;; Commentary:
;;; Code

(require 'compile)
(require 'cl)
(require 'yasnippet)
(require 'projectile)
(require 'yasnippet)

(yas-global-mode 1)

(projectile-global-mode)
(setq projectile-enable-caching t)
(projectile-global-mode)

;; cmake-mode
(require 'cmake-mode)

; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(yas-global-mode 1)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

;; setup build-env
;; https://www.emacswiki.org/emacs/CompileCommand
;; https://stackoverflow.com/questions/5016440/can-i-code-and-compile-all-type-of-visual-studio-application-from-command-line/23199265#23199265
(add-to-list 'compilation-error-regexp-alist-alist '(msvc "^[ \t]\([A-Za-z0-9\.][^(]\.\(cpp\|c\|h\)\)(\([0-9]+\)) *: +\(error\|fatal error\|warning\) C[0-9]+:" 1 3))

(defun window-compile-hook()
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
    ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
           (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s -fms-compatibility-version=19.00"
                             (or (getenv "CC") "clang++")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-std=c++1z")
                             (or (getenv "CFLAGS") "-Wall -g")
                             file)))))

(setq compilation-read-command nil) ;; no prompt

;; https://stackoverflow.com/questions/9725015/how-do-i-make-the-compilation-window-in-emacs-to-always-be-a-certain-size
(setq compilation-window-height 10)

(defun compile-window-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))

(pushnew '("*compilation*"
           (minibuffer . nil)
           (unsplittable . t)
           (menu-bar-lines . 0))
         special-display-buffer-names)

(defun find-dedicated-frames (buf)
  (let (result)
    (dolist (window (get-buffer-window-list buf t) result)
      (let ((frame (window-frame window)))
        (when (frame-parameter frame 'unsplittable)
          (push frame result))))))

(defun qtmstr-setup-compile-mode ()
  ;; Support C++ better
  (modify-syntax-entry ?< "(")
  (modify-syntax-entry ?> ")")

  (dolist (frame (find-dedicated-frames (current-buffer)))
    (let ((orig (frame-parameter frame 'orig-background)))
      (when orig
        (modify-frame-parameters
         frame (list (cons 'background-color orig)))))))

(defun* qtmstr-compile-finish (buf status)
  (with-current-buffer buf
    (let* ((color (if (string-match "^finished\\b" status)
                      "#dfd"
                    "#fdd"))
           found)

      (dolist (frame (find-dedicated-frames buf))
        (setq found t)
        (modify-frame-parameters
         frame
         (list (cons 'background-color color)
               (cons 'orig-background
                     (frame-parameter frame 'background-color)))))

      (unless found
        (let ((overlay)
              (overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'face (list :background color))
          (overlay-put overlay 'evaporate t))))))

(add-hook 'c++-mode-hook 'window-compile-hook)
(add-hook 'compilation-mode-hook 'compile-window-hook)
(add-hook 'compilation-mode-hook #'qtmstr-setup-compile-mode)
(add-hook 'compilation-finish-functions #'qtmstr-compile-finish)

;;(load-library "setup-python")
(require 'setup-python)
(require 'setup-helm)
(require 'setup-company)
(require 'setup-cc-clang)
(require 'setup-cedet)
(require 'setup-javascript)

(provide 'setup-dev)
