;;; package --- summary

;;; Commentary:
;;; Code


;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: projejctile
(require 'projectile)
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

;; setup build-env
;; https://www.emacswiki.org/emacs/CompileCommand
;; https://stackoverflow.com/questions/5016440/can-i-code-and-compile-all-type-of-visual-studio-application-from-command-line/23199265#23199265
(add-to-list 'compilation-error-regexp-alist-alist '(msvc "^[ \t]\([A-Za-z0-9\.][^(]\.\(cpp\|c\|h\)\)(\([0-9]+\)) *: +\(error\|fatal error\|warning\) C[0-9]+:" 1 3))

(require 'compile)
(add-hook 'c++-mode-hook
          (lambda ()
        (unless (file-exists-p "Makefile")
          (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
           (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "clang++")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-std=c++17")
                             (or (getenv "CFLAGS") "-Wall -g")
                             file))))))

(setq compilation-read-command nil) ;; no prompt

(require 'cl)

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

(add-hook 'compilation-mode-hook #'qtmstr-setup-compile-mode)

(defun qtmstr-compile-finish (buf status)
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

(add-hook 'compilation-finish-functions 'qtmstr-compile-finish)

(defun my-select-bottom-window ()
  (let ((bottom-window (selected-window))
        window-below)
    (while (setq window-below (window-in-direction 'below bottom-window))
      (setq bottom-window window-below))
    (select-window bottom-window)))

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (my-select-bottom-window)
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compile5*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

;; https://www.emacswiki.org/emacs/CompilationMode#toc4
;; https://stackoverflow.com/questions/9725015/how-do-i-make-the-compilation-window-in-emacs-to-always-be-a-certain-size

;;(load-library "setup-python")
(require 'setup-python)
;;(require 'setup-ggtags)
(require 'setup-helm)
(require 'setup-company)
(require 'setup-cc-clang)
(require 'setup-cedet)
(require 'setup-javascript)

(provide 'setup-dev)
