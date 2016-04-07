
;; org mode setup
(require 'setup-orgmode)

;; shortcut to connect
(defun connect-remote (host user)
  (interactive "sInput the host address to connect: \nsUser to login: ")
  (dired (format "/%s@%s:/" user host)))  ;;

;; load from ../modules
(require 'setup-editing)
(require 'setup-dev)

;;; nyan-mode
(load "nyan-mode.el")
(nyan-mode 1)
(nyan-start-animation)

;; elfeed
(setq elfeed-feeds
      '("http://www.reddit.com/r/emacs/.rss"
        "https://www.reddit.com/r/CS_Questions/.rss"
        "https://www.reddit.com/r/coding/.rss"
        "http://www.reddit.com/r/cpp/.rss"))

(setf url-queue-timeout 30)

;; Gnus
(load-library "gnus-config")

;; w3m
(setq w3m-default-display-inline-images t)
(setq w3m-display-inline-images t)

;; more w3m configuration from http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;quick access hacker news
(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

;;quick access reddit
(defun reddit (reddit)
  "Opens the REDDIT in w3m-new-session"
  (interactive (list
                (read-string "Enter the reddit (default: psycology): " nil nil "cpp" nil)))
  (browse-url (format "http://m.reddit.com/r/%s" reddit))
  )

;;i need this often
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
   )
  (browse-url
   (concat
    "http://en.m.wikipedia.org/w/index.php?search="
    search-term
    ))
  )

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))

;; flycheck installation
(add-hook 'after-init-hook #'global-flycheck-mode)

;; search key binding
(defun guess-where-keybinding-is-defined (key)
  "try to guess where a key binding might be defined"
  (interactive (list (read-key-sequence "Describe key: ")))
  (let ((bindings (minor-mode-key-binding key))
        found)
    (while (and bindings (not found))
      (if (setq found (caar bindings))
          (find-function (cdar bindings)))
      (setq bindings (cdr bindings)))))

;; settings from http://docs.astropy.org/en/stable/development/codeguide_emacs.html
;; Set the number to the number of columns to use.
(setq-default fill-column 79)

;; Add Autofill mode to mode hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Show line number in the mode line.
;; disabled lin min mode
(line-number-mode 1)

;; Show column number in the mode line.
(column-number-mode 1)

(global-font-lock-mode 1)

;; disable alarm
;; ref http://stackoverflow.com/questions/11679700/emacs-disable-beep-when-trying-to-move-beyond-the-end-of-the-document
(defun my-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)


(provide 'my-modules)
;;; my-modules.el ends here
