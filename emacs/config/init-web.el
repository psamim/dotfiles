(require-package 'js2-mode)
(require 'js2-mode)
(setq js2-highlight-level 3)
(setq-default js2-basic-offset 2)

(require-package 'ac-js2)
(add-hook 'js2-mode-hook 'ac-js2-mode)


(after 'js2-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;(require-package 'js2-refactor)
;(require 'js2-refactor)
;(js2r-add-keybindings-with-prefix "C-c C-m")


;(require-package 'tern)
;(require-package 'tern-auto-complete)
;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;(after 'tern
  ;(after 'auto-complete
    ;(require 'tern-auto-complete)
    ;(tern-ac-setup)))


;(require-package 'coffee-mode)
;(require-package 'jade-mode)


;(require-package 'stylus-mode)

;(defvar my-stylus-command-args nil
  ;"Additional list of arguments to pass into the stylus command.")

;(defun my-stylus-compile (start end)
  ;(let ((buffer (get-buffer "*Stylus*")))
    ;(when buffer (with-current-buffer buffer (erase-buffer))))
  ;(apply 'call-process-region start end "stylus" nil (get-buffer-create "*Stylus*") nil
         ;my-stylus-command-args)
  ;(let ((buffer (get-buffer "*Stylus*")))
    ;(display-buffer buffer)
    ;(when buffer (with-current-buffer buffer (css-mode)))))

;(defun my-stylus-compile-region (start end)
  ;(interactive "r")
  ;(my-stylus-compile start end))

;(defun my-stylus-compile-buffer ()
  ;(interactive)
  ;(my-stylus-compile (point-min) (point-max)))


(require-package 'skewer-mode)
(skewer-setup)

(require-package 'php-mode)
(require 'php-mode)

(require-package 'php-eldoc)
(add-hook 'php-mode-hook 'php-eldoc-enable)
(add-hook 'php-mode-hook 'flycheck-mode nil)

(defun my-php-mode-hook ()
  (setq indent-tabs-mode t)
  (let ((my-tab-width 4))
    (setq tab-width my-tab-width)
    (setq c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence my-tab-width 200 my-tab-width))))
(add-hook 'php-mode-hook 'my-php-mode-hook)

(require-package 'rainbow-mode)
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook web-mode-hook))
    (add-hook hook 'rainbow-mode))

;;; Auto-complete CSS keywords
  (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
    (add-hook hook 'ac-css-mode-setup))


;;; Use eldoc for syntax hints
(require-package 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;(require-package 'emmet-mode)
;(add-hook 'css-mode-hook 'emmet-mode)
;(add-hook 'sgml-mode-hook 'emmet-mode)
;(add-hook 'web-mode-hook 'emmet-mode)


(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; indent after deleting a tag
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))


(provide 'init-web)
