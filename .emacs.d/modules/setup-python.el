

;; https://github.com/porterjamesj/virtualenvwrapper.el
;;; Code:

(require 'python)

;; Type:
;;     M-x el-get-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

;; jedi
;; Standard Jedi.el setting

;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;;(setq venv-location "/path/to/your/virtualenvs/")

;; Used by virtualenvwrapper.el
(setq venv-location (expand-file-name "~/.envs"))   ;; Change with the path to your virtualenvs
;; Used python-environment.el and by extend jedi.el
(setq python-environment-directory venv-location)

;;(setq python-shell-virtualenv-path venv-location)

;; virtualenv setting
;; http://stackoverflow.com/questions/21246218/how-can-i-make-emacs-jedi-use-project-specific-virtualenvs
(defun project-directory (buffer-name)
  "Returns the root directory of the project that contains the
given buffer. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun project-name (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (file-name-nondirectory
         (directory-file-name root-dir))
      nil)))

(defun jedi-setup-venv ()
  "Activates the virtualenv of the current buffer."
  (let ((project-name (project-name buffer-file-name)))
    (when project-name (venv-workon project-name))))


;; use company-jedi instead of jedi
(defun my-jedi-setup ()
  (setq python-python-command "python3")
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  (jedi:setup)
  (add-to-list 'company-backends 'company-jedi)
  (setq-default python-indent-offset 4)    )

;; Highlight character at "fill-column" position.
(require 'column-marker)
(set-face-background 'column-marker-1 "red")

(add-hook 'python-mode-hook 'my-jedi-setup)

(add-hook 'python-mode-hook
          (lambda () (interactive)
            (column-marker-1 fill-column)))

;(jedi:install-server)

(defun python-shell-parse-command ()
  "Return the string used to execute the inferior Python process."
  "/Library/Frameworks/Python.framework/Versions/3.4/bin/python3 -i"
  )

;; (setq jedi:environment-root "jedi")  ; or any other name you like
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/local/bin/python3")))

;; (setq jedi:environment-virtualenv
;;       (list "virtualenv3" "--system-site-packages"))

(provide 'setup-python)
