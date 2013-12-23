(require-package 'auctex)
;(require 'auctex-pkg)
(require 'auctex-autoloads)
(require 'reftex)
(require 'init-ac-auctex)

(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-to-list 'TeX-command-list
		'("PDF" "pdflatex %s" TeX-run-command t t :help "To PDF") t)



(add-hook 'LaTeX-mode-hook
       (lambda ()
          (setq TeX-command-default "PDF")
		))

;(add-hook 'LaTeX-mode-hook
;       (lambda ()
;	   (set (make-local-variable 'compile-command
;		(concat "pdflatex " buffer-file-name)))))

(provide 'init-latex)
