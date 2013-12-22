(require-package 'auctex)
;(require 'auctex-pkg)
(require 'auctex-autoloads)
(require 'reftex)

(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(provide 'init-latex)
