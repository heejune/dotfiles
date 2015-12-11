
;; gnus configuration

;; 1. ~/.profile
;; export EMAIL="<EMAIL_ADDRESS>"
;; export NAME="<FULL NAME>"
;; export SMTPSERVER="smtp.gmail.com"

;; 2. ~/.authinfo
;; machine imap.gmail.com login <USER> password <PASSWORD> port imaps
;; machine smtp.gmail.com login <USER> password <PASSWORD> port 587

;; reference
;; http://blog.binchen.org/posts/notes-on-using-gnus.html

(require 'nnir)

;;@see http://www.emacswiki.org/emacs/GnusGmail#toc1
(setq gnus-select-method '(nntp "news.gmane.org"))


(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
               (nnir-search-engine imap)
               (nnimap-server-port "imaps")
               (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
               (nnmail-expiry-wait 90)
               (nnimap-stream ssl)))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent ""
 gnus-sum-thread-tree-leaf-with-other "-> "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "|_ "
 gnus-sum-thread-tree-vertical "|")

(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)
        ))

;; Storing sent mail in the server
;; (setq gnus-posting-styles
;;       '((".*"
;;          ("CC" "heejune@gmail.com")
;;          (address "heejune@gmail.com"))))

                                        ; NO 'passive
(setq gnus-use-cache t)
(setq gnus-use-adaptive-scoring t)
(setq gnus-save-score t)
(add-hook 'mail-citation-hook 'sc-cite-original)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
                                        ; @see http://stackoverflow.com/questions/945419/how-dont-use-gnus-adaptive-scoring-in-some-newsgroups
(setq gnus-parameters
      '(("nnimap.*"
         (gnus-use-scoring nil))
        ))

(defvar gnus-default-adaptive-score-alist
  '((gnus-kill-file-mark (from -10))
    (gnus-unread-mark)
    (gnus-read-mark (from 10) (subject 30))
    (gnus-catchup-mark (subject -10))
    (gnus-killed-mark (from -1) (subject -30))
    (gnus-del-mark (from -2) (subject -15))
    (gnus-ticked-mark (from 10))
    (gnus-dormant-mark (from 5))))

(setq  gnus-score-find-score-files-function
       '(gnus-score-find-hierarchical gnus-score-find-bnews bbdb/gnus-score)
       )

;; gnus
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )
(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
          )

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)
