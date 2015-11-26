;;; TTRSS --- Reader for Gnus
;;; Commentary:
;;; Code:

(require 'ttrss)


(setq ttrss-address ""
      ttrss-user ""
      ttrss-password "")

(let ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password)))
  (if (ttrss-logged-in-p ttrss-address ttrss-sid)
      (with-current-buffer (switch-to-buffer "*ttrss reader*")
        (erase-buffer)
        (message "Logged in to %s as %s with password %s\n"
                 ttrss-address
                 ttrss-user
                 ttrss-password)
        (message "Server running version %s and API level %d\n"
                 (ttrss-get-version ttrss-address ttrss-sid)
                 (ttrss-get-api-level ttrss-address ttrss-sid))
        (message "There are %s unread articles in %d feeds"
                 (ttrss-get-unread ttrss-address ttrss-sid)
                 (length (ttrss-get-feeds ttrss-address ttrss-sid :unread_only t)))
        (insert "The grand <b>*ttrss* reader!</b>\n\n---\n\n")
        (mapcar (lambda (pl) (insert (format "%s\n%s\n\n%s\n---\n"
                                             (plist-get pl :title)
                                             (plist-get pl :link)
                                             (plist-get pl :content))))
                (ttrss-get-headlines ttrss-address ttrss-sid :feed_id -3 :limit 3 :show_content t))
        (html2text))
    (message "Login failed")))

(provide 'init-ttrss)
;;; init-ttrss.el ends here
