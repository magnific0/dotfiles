;; Special hook for Gnus
(add-to-list
 'command-switch-alist
 '("gnus" . (lambda (&rest ignore)
              ;; Start Gnus when Emacs starts
              (add-hook 'emacs-startup-hook 'gnus t)
              ;; Exit Emacs after quitting Gnus
              (add-hook 'gnus-after-exiting-gnus-hook
                        'save-buffers-kill-emacs))))

(setq mail-user-agent 'gnus-user-agent)

(provide 'init-gnus)
