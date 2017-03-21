


;; -------------------------------
;; Based on following references
;; http://tuhdo.github.io/c-ide.html
;; https://github.com/tmtxt/.emacs.d/blob/master/init.el
;; https://github.com/bbatsov/prelude
;; -------------------------------

;;; Code:

;; Always load newest byte code

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-prefer-newer t)

(defvar dot-emacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar core-emacs-dir (expand-file-name "core" dot-emacs-dir)
  "The home of Prelude's core functionality.")
(defvar modules-setup-dir (expand-file-name  "modules" dot-emacs-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar personal-dir (expand-file-name "personal" dot-emacs-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar personal-preload-dir (expand-file-name "preload" personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar vendor-dir (expand-file-name "vendor" dot-emacs-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar savefile-dir (expand-file-name "savefile" dot-emacs-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar modules-setup-file (expand-file-name "my-modules.el" dot-emacs-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path core-emacs-dir)
(add-to-list 'load-path modules-setup-dir)
(add-to-list 'load-path vendor-dir)
(prelude-add-subfolders-to-load-path vendor-dir)

;;; custom file => obsolete
;;(add-to-list 'load-path "~/.emacs.d/custom")
;;(setq custom-file "~/.emacs.d/custom.el")
;;(load custom-file)

;; Use a company-mode instead of auto-complete
;;(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")
(add-to-list 'load-path "~/.emacs.d/config")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `personal-preload-dir'
(when (file-exists-p personal-preload-dir)
  (message "Loading personal configuration files in %s..." personal-preload-dir)
  (mapc 'load (directory-files personal-preload-dir 't "^[^#].*el$")))

(message "Loading core...")

;; ref https://github.com/jfroffice/emacs.d/blob/master/init.el
;; This function replaces modes in some alist with another mode
;;
;; Some modes just insist on putting themselves into the
;; auto-mode-alist, this function helps me get rid of them
(defun replace-auto-mode (oldmode newmode)
  (dolist (aitem auto-mode-alist)
    (if (eq (cdr aitem) oldmode)
        (setcdr aitem newmode))))


;; the core stuff
(require 'packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'ui)
(require 'core)
(require 'editor)
(require 'keybinding)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'osx))

(when (eq system-type 'gnu/linux)
  (defvar dropbox-dir "~/Dropbox"
  "Dropbox home folder.")
  )

;; Windows-nt specific
(when (eq system-type 'windows-nt)
  (defvar dropbox-dir "D:/Dropbox"
    "Dropbox home folder.")
  (require 'windows)
  )

(message "Loading modules...")

;; the modules
(if (file-exists-p modules-setup-file)
    (load modules-setup-file)
  (message "Missing modules file %s" modules-setup-file)
  (message "You can get started by copying the bundled example file"))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p personal-dir)
  (message "Loading personal configuration files in %s..." personal-dir)
  (mapc 'load (directory-files personal-dir 't "^[^#].*el$")))

