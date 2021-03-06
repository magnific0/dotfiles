;;; init-themes.el --- Provide configuration for themes
;;; Commentary:
;;; Code:
(when (< emacs-major-version 24)
  (require-package 'color-theme))

(require-package 'zenburn-theme)
(require-package 'anti-zenburn-theme)

(load-theme 'zenburn t)

(set-default-font "Inconsolata-12")
(menu-bar-mode 0)
(tool-bar-mode 0)

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)

;; ;;------------------------------------------------------------------------------
;; ;; Old-style color theming support (via color-theme.el)
;; ;;------------------------------------------------------------------------------
;; (defcustom window-system-color-theme 'color-theme-zenburn
;;   "Color theme to use in window-system frames.
;; If Emacs' native theme support is available, this setting is
;; ignored: use `custom-enabled-themes' instead."
;;   :type 'symbol)

;; (defcustom tty-color-theme 'color-theme-terminal
;;   "Color theme to use in TTY frames.
;; If Emacs' native theme support is available, this setting is
;; ignored: use `custom-enabled-themes' instead."
;;   :type 'symbol)

;; (unless (boundp 'custom-enabled-themes)
;;   (defun color-theme-terminal ()
;;     (interactive)
;;     (color-theme-zenburn))

;;   (defun apply-best-color-theme-for-frame-type (frame)
;;     (with-selected-frame frame
;;       (funcall (if window-system
;;                    window-system-color-theme
;;                  tty-color-theme))))

;;   (defun reapply-color-themes ()
;;     (interactive)
;;     (mapcar 'apply-best-color-theme-for-frame-type (frame-list)))

;;   (set-variable 'color-theme-is-global nil)
;;   (add-hook 'after-make-frame-functions 'apply-best-color-theme-for-frame-type)
;;   (add-hook 'after-init-hook 'reapply-color-themes)
;;   (apply-best-color-theme-for-frame-type (selected-frame)))


;;------------------------------------------------------------------------------
;; New-style theme support, in which per-frame theming is not possible
;;------------------------------------------------------------------------------

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(zenburn))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; Fix the cursor colour for gui emacsclient
(setq default-frame-alist '((cursor-color . "#DCDCCC")))

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun loco ()
  "Activate a light color theme."
  (interactive)
  (load-theme 'zenburn t))

(defun hico ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'hc-zenburn t))

(provide 'init-themes)
