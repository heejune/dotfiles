;; -------------------------------
;; Based on following references
;; http://tuhdo.github.io/c-ide.html
;; https://github.com/tmtxt/.emacs.d/blob/master/init.el
;;
;; heejune@gmail.com
;; -------------------------------

(require 'package) ;; You might already have this line
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

  ;;; Required packages
;;; everytime emacs starts, it will automatically check if those packages are
;;; missing, it will install them automatically
;;; hints from https://github.com/tmtxt/.emacs.d
  (when (not package-archive-contents)
    (package-refresh-contents))
  (defvar hekim/packages
    '(auto-complete
      helm
      magit
      paredit
      popup
      yasnippet
      autopair
      header2
      rainbow-mode
      rainbow-delimiters
      undo-tree
      htmlize
      exec-path-from-shell
      web-beautify
      nyan-mode

      ;; helm
      helm

      ;; javascript
      json-mode
      js2-mode

      ;; from demo-packages
      company
      helm
      helm-gtags
      helm-projectile
      helm-swoop
      function-args
      clean-aindent-mode
      comment-dwim-2
      dtrt-indent
      ws-butler
      iedit
      yasnippet
      smartparens
      projectile
      volatile-highlights
      undo-tree
      zygospore

      ;; theme

      monokai-theme))
  (dolist (p hekim/packages)
    (when (not (package-installed-p p))
      (package-install p)))

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(anzu
    company
    duplicate-thing
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    zygospore))

;; (defun install-packages ()
;;   "Install all required packages."
;;   (interactive)
;;   (unless package-archive-contents
;;     (package-refresh-contents))
;;   (dolist (package demo-packages)
;;     (unless (package-installed-p package)
;;       (package-install package))))

;; (install-packages)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")
(add-to-list 'load-path "~/.emacs.d/config")

;;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)

;; ---
(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;; ---


(windmove-default-keybindings)

;; function-args
(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(tab)] 'moo-complete)
(define-key c++-mode-map  [(tab)] 'moo-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(control tab)] 'company-complete)
(define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "ellemtel" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projejctile
;;(require 'projectile)
;(projectile-global-mode)
;; (setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Source code completion with Clang
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

;; compile
(global-set-key (kbd "<f5>") (lambda()
                                   (interactive)
                                   (setq-local compilation-read-command nil)
                                   (call-interactively 'compile)))
;; cmake-mode
(require 'cmake-mode)

;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(require 'auto-complete-clang)

(global-set-key (kbd "C-c `") 'ac-complete-clang)

;;; nyan-mode
(load "nyan-mode.el")
(nyan-mode 1)
(nyan-start-animation)

;;; window resize
;;; http://stackoverflow.com/questions/6315243/emacs-nw-mode-resize-split-window
(global-set-key (kbd "<A-up>") 'shrink-window)
(global-set-key (kbd "<A-down>") 'enlarge-window)
(global-set-key (kbd "<A-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<A-right>") 'enlarge-window-horizontally)

;; load theme
(load-theme 'monokai t)

(require 'undo-tree)

;; Gnus
(load "gnus-config.el")

;; jedi
;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Type:
;;     M-x el-get-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

;; Fix tramp slow issue on yosemite
;; http://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh
(setq projectile-mode-line "Projectile")
