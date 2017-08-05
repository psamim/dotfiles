(setq frame-title-format
      '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
                                                        (abbreviate-file-name (buffer-file-name))
                                                      "%b")) " [%*]"))
(setq-default
 ;; js2-mode
 js2-basic-offset 2
 js-indent-level 2
 ;; web-mode
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)

;; Different font for Farsi characters
(set-fontset-font
 "fontset-default"
 (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
 ;; "Vazir Code-13")
 "Shabnam-12")
;; "DejaVu Sans-12")

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Add icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Hide mode names
(spacemacs|diminish super-save-mode  nil)
(spacemacs|diminish prettier-js-mode  nil)
(spacemacs|diminish eslintd-fix-mode  nil)
(spacemacs|diminish centered-window-mode  nil)
(spacemacs|diminish all-the-icons-dired-mode  nil)

;; Configure org-journal
(custom-set-variables
 '(org-journal-date-format 'psamim-journal-prefix))

;(use-package spaceline-all-the-icons
;  :after spaceline
;  :config (spaceline-all-the-icons-theme))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (visual-line-mode)
;;             (spacemacs/toggle-vi-tilde-fringe-off)
;;             (spacemacs/toggle-line-numbers-off)
;;             (setq bidi-paragraph-direction t)))

;; (keychain-refresh-environment)
;; (super-save-mode +1)

;; http://emacs.stackexchange.com/a/21207
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

