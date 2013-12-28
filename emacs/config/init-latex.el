(require-package 'auctex)
;(require 'auctex-pkg)
(require 'auctex-autoloads)
(require 'init-ac-auctex)
(require 'reftex)

(add-to-list 'TeX-command-list
		'("PDFLaTeX" "pdflatex %s" TeX-run-command t t :help "To PDF") t)

(add-to-list 'TeX-command-list
		'("XeTex" "xelatex %s" TeX-run-command t t :help "Using XeTex") t)

(defun my-latex-mode-hook-funcs ()
  (flyspell-mode 1)
  (turn-on-reftex)
  (setq TeX-command-default "XeTex"
        TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook-funcs)


;; AUCTeX configuration
(setq-default TeX-master nil)

;(add-hook 'LaTeX-mode-hook
;       (lambda ()
;	   (set (make-local-variable 'compile-command
;		(concat "pdflatex " buffer-file-name)))))

(provide 'init-latex)
