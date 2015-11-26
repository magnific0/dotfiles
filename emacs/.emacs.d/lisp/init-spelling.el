(require 'ispell)

(when (executable-find ispell-program-name)
  (require 'init-flyspell))

(setq ispell-dictionary "british")

;; cycle between languages
(defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "dutch") "british" "dutch" "english")))
        (ispell-change-dictionary change)
        (message "Dictionary switched from %s to %s" dic change)
        ))

(global-set-key (kbd "<f8>")   'fd-switch-dictionary)

(provide 'init-spelling)
