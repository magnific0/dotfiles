;; (require-package 'auctex)
;; (require-package 'latex-preview-pane)
(require-package 'auto-complete-auctex)
(require-package 'ac-math)

;; (latex-preview-pane-enable)

;; LuaLaTeX and BibLateX + Biber compatible
(setq LaTeX-command-style (quote (("" "lualatex --jobname=%s -shell-escape %S %(PDFout)")))
      TeX-command-BibTeX "Biber"
      TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeglossaries" "makeglossaries %s" TeX-run-command nil (latex-mode) :help "Create glossaries") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("Biber" "biber %s.bcf" TeX-run-BibTeX nil t) ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
      TeX-view-program-list nil
      TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "mupdf") (output-html "xdg-open"))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'Latex-mode-hook 'auto-complete-auctex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-view-program-list '(("mupdf" "mupdf %o %(outpage)") ("acroread" "acroread %o -start %(outpage)")))

(add-to-list 'ac-modes 'latex-mode)

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
               ac-sources))
)


(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
;;
;; LaTeX wordcount
;;
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "/usr/bin/texcount "
                         ; "uncomment then options go here "
                         "\""
                         (buffer-file-name)
                         "\"")))
(eval-after-load 'latex
                    '(define-key LaTeX-mode-map "\C-cw" 'latex-word-count))


(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;;
;; Auctex stuff
;;
;; Check buffer for custom macros
(setq-default TeX-auto-regexp-list 'LaTeX-auto-regexp-list)
(setq-default TeX-auto-parse-length 999999)

(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;; ;; Find tex-master file automagically
;; (defun guess-TeX-master (filename)
;;   "Guess the master file for FILENAME from currently open .tex files."
;;   (let ((candidate nil)
;;         (filename (file-name-nondirectory filename)))
;;     (save-excursion
;;       (dolist (buffer (buffer-list))
;;         (with-current-buffer buffer
;;           (let ((name (buffer-name))
;;                 (file buffer-file-name))
;;             (if (and file (string-match "\\.tex$" file))
;;                 (progn
;;                   (goto-char (point-min))
;;                   (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
;;                       (setq candidate file))
;;                   (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
;;                       (setq candidate file))))))))
;;     (if candidate
;;         (message "TeX master document: %s" (file-name-nondirectory candidate)))
;;     candidate))
;; (setq TeX-master (guess-TeX-master (buffer-file-name)))

(provide 'init-tex)
