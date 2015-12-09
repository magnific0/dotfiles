;;; Init-C --- Suppl. init. for C/C++
;;; Copyright 2014 <Jacco>
;;; Commentary:
;;; Code:

;; YAS
(require-package 'yasnippet)
(yas-global-mode 1)

;; ECB
(require-package 'ecb)
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-compile-window-height 8)

;; IEDIT
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; FCI Column Rule
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-column 80)
(setq fci-rule-color "grey")
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)

;; Set the GCC version
(setq gcc_version "5.2.0")

;; Auto-Complete Headers
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories (concat "/usr/include/c++/1" gcc_version))
  (add-to-list 'achead:include-directories (concat "/usr/include/c++/" gcc_version "/x86_64-unknown-linux-gnu"))
  (add-to-list 'achead:include-directories (concat "/usr/include/c++/" gcc_version "/backward"))
  (add-to-list 'achead:include-directories (concat "/usr/lib/gcc/x86_64-unknown-linux-gnu/" gcc_version "/include"))
  (add-to-list 'achead:include-directories (concat "/usr/lib/gcc/x86_64-unknown-linux-gnu/" gcc_version "/include-fixed"))
  (add-to-list 'achead:include-directories '"/usr/include/boost")
  (add-to-list 'achead:include-directories '"/usr/include/eigen3"))
;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; Flymake CPP Lint
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/bin/cpplint"))
  (flymake-google-cpplint-load)
)
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

;; Set Google C Style
(require-package 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; My C Style
(defun my-c-mode-hook ()
  (c-set-offset 'innamespace '-))       ; ignore namespace indentations

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; (c-add-style "my-c-style" '((c-continued-statement-offset 4))) ; If a statement continues on the next line, indent the continuation by 4

;; (defun my-c-mode-hook ()
;;   (c-set-style "my-c-style")
;;   (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
;;   (c-set-offset 'inline-open '+)
;;   (c-set-offset 'block-open '+)
;;   (c-set-offset 'arglist-intro '+)
;;   (c-set-offset 'arglist-cont-nonempty 4)
;;   (c-set-offset 'arglist-close 0)
;;   (c-set-offset 'innamespace '-)       ; ignore the accolades of namespaces
;;   (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
;;   (c-set-offset 'case-label '+)        ; indent case labels by c-indent-level, too
;;   (setq c-basic-offset 4)
;;   (setq c-indent-tabs-mode nil)        ; Pressing TAB should cause indentation
;;   (setq c-indent-level 4)              ; A TAB is equivilent to four spaces
;;   (setq c-argdecl-indent 0)            ; Do not indent argument decl's extra
;;   (setq c-tab-always-indent t)
;;   (setq backward-delete-function nil))

;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;; (add-hook 'c++-mode-hook 'my-c-mode-hook)

(provide 'init-c)
;;; init-c.el ends here
