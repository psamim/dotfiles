(require-package 'vimrc-mode)
(setq auto-mode-alist
      (cons '("\\.vim\\'" . vimrc-mode) auto-mode-alist))

(setq evil-search-module 'evil-search
     evil-want-C-u-scroll t
     evil-want-C-w-in-emacs-state t)

(provide 'init-vim)
