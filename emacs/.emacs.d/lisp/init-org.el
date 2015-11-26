(when (< emacs-major-version 24)
  (require-package 'org))
(require-package 'org-fstree)
(when *is-a-mac*
  (require-package 'org-mac-link)
  (autoload 'org-mac-grab-link "org-mac-link" nil t)
  (require-package 'org-mac-iCal))

(require 'org-install)
(package-initialize)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
;; Conflict with EDE
(define-key global-map (kbd "C-c C-.") 'org-time-stamp)

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; C-c a t only shows unscheduled items
(setq org-agenda-todo-ignore-scheduled nil)

(defvar dir-where-you-store-org-files "~/Org/")
(setq org-agenda-files
 (mapcar (lambda (x) (concat dir-where-you-store-org-files x))
         '("journal.org" "notes.org" "todo.org" "study.org")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "|" "CANCELLED(c@/!)")
        (sequence "MOVED(m)" "|")
        (sequence "PENDING(p@/!)" "|" )))
(setq org-todo-keyword-faces
      '(("CANCELLED"  . (:foreground "blue" :weight bold))
        ("MOVED" . (:foreground "dark green" :weight bold))
        ("PENDING"  . (:foreground "orange" :weight bold))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (setq org-clock-persist 'history)
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; ;; Change task state to STARTED when clocking in
;; (setq org-clock-in-switch-to-state "STARTED")
;; ;; Save clock data and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;; ;; Removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

;; ask the user if they wish to clock out before killing Emacs
(defun my/org-query-clock-out ()
  "Ask the user before clocking out.
    This is a useful function for adding to `kill-emacs-query-functions'."
  (if (and (featurep 'org-clock)
           (funcall 'org-clocking-p)
           (y-or-n-p "You are currently clocking time, clock out? "))
      (org-clock-out)
    t))                             ; only fails on keyboard quit or error

(add-hook 'kill-emacs-query-functions 'my/org-query-clock-out)

;; Moving tasks up and down the list
(defun org-move-subtree-among-superior(howmuch)
  "Moving subtree among superiors."
  (interactive)
  (save-excursion
    (org-copy-subtree)
    (move-beginning-of-line nil)
    (save-excursion
      (org-yank)
      (org-todo "MOVED"))
    (while (ignore-errors (org-move-subtree-down)))
    (org-promote-subtree)
    (if (minusp howmuch)
        (dotimes (var (abs howmuch)) (org-move-subtree-up))
      (dotimes (var howmuch) (org-move-subtree-down)))
    (org-demote-subtree)))
(defun org-move-subtree-to-previous-superior()
  "Moving subtree to previous superior."
  (interactive)
  (org-move-subtree-among-superior -1))
(defun org-move-subtree-to-next-superior()
  "Moving subtree to next superior."
  (interactive)
  (org-move-subtree-among-superior 1))
(global-set-key (kbd "C-M-<prior>") 'org-move-subtree-to-previous-superior)
(global-set-key (kbd "C-M-<next>") 'org-move-subtree-to-next-superior)

;; Clock mode
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; ask the user if they wish to clock out before killing Emacs
(defun my/org-query-clock-out ()
  "Ask the user before clocking out.
    This is a useful function for adding to `kill-emacs-query-functions'."
  (if (and (featurep 'org-clock)
           (funcall 'org-clocking-p)
           (y-or-n-p "You are currently clocking time, clock out? "))
      (org-clock-out)
    t))                             ; only fails on keyboard quit or error

(add-hook 'kill-emacs-query-functions 'my/org-query-clock-out)

(require-package 'org-pomodoro)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil))
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (sh . t)
     (sql . nil)
     (sqlite . t))))


(provide 'init-org)
