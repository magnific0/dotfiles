


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq
 python-shell-interpreter "ipython2"
 python-shell-interpreter-args "--matplotlib"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(require-package 'pip-requirements)
(require-package 'elpy)
(elpy-enable)
;; Fixing a keybinding in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

(provide 'init-python-mode)
