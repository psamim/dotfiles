(require-package 'auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)

;(global-auto-complete-mode t)

; This adds the dabbrev to ac, for autocompleting strings, but did not work as intended for me
;(require-package 'ac-dabbrev)
;(require 'ac-dabbrev)

(setq ac-auto-show-menu t)
(setq ac-auto-start t)
(setq ac-comphist-file (concat user-emacs-directory ".cache/ac-comphist.dat"))
(setq ac-quick-help-delay 0.3)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)

(dolist (mode '(vimrc-mode
                ;; shell-mode term-mode terminal-mode eshell-mode comint-mode skewer-repl-mode
                html-mode stylus-mode))
  (add-to-list 'ac-modes mode))

(ac-config-default)
(setq-default ac-sources
          '(
        ac-source-filename
        ;ac-source-abbrev
        ac-source-dictionary
        ;ac-source-dabbrev
        ac-source-words-in-buffer
        ac-source-words-in-all-buffer
        ;ac-source-yasnippet
        ac-source-words-in-same-mode-buffers))

; Latex Configs
;(defun my-ac-tex-setup()
 ; (setq ac-sources (append '(
  ;                           ) ac-sources)))
;(add-hook 'LaTeX-mode-hook 'my-ac-tex-setup)

(after 'linum
  (ac-linum-workaround))

;(defadvice ac-expand (before advice-for-ac-expand activate)
;  (when (yas-expand)
;   (ac-stop)))

; Auto-complete for everything even inside strings
(setq ac-disable-faces nil)

(provide 'init-auto-complete)
