(defconst aj-javascript-packages
  '(
    add-node-modules-path
    company-flow
    eslintd-fix
    flycheck
    prettier-js
    rjsx-mode))


(defun aj-javascript/init-eslintd-fix ()
  (use-package eslintd-fix
    :defer t
    :commands eslintd-fix-mode
    :init
    (progn
      (add-hook 'rjsx-mode-hook #'eslintd-fix-mode t))))

(defun aj-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
      (evil-define-key 'normal rjsx-mode-map
        (kbd "x") 'rjsx-delete-creates-full-tag)

      (setq
       js2-mode-show-strict-warnings nil
       js2-mode-show-parse-errors nil
       js-indent-level 2
       js2-basic-offset 2
       js2-strict-trailing-comma-warning nil
       js2-strict-missing-semi-warning nil)

      (advice-add #'js-jsx-indent-line
                  :after
                  #'aj-javascript/js-jsx-indent-line-align-closing-bracket)
      (add-hook 'rjsx-mode-hook #'aj-javascript/eslintd-set-flycheck-executable t))
    :config
    (modify-syntax-entry ?_ "w" js2-mode-syntax-table)))

(defun aj-javascript/post-init-add-node-modules-path ()
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

(defun aj-javascript/post-init-company-flow ()
  (spacemacs|add-company-backends
    :backends
    '((company-flow :with company-dabbrev-code)
      company-files)))

(defun aj-javascript/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))

  (spacemacs/enable-flycheck 'rjsx-mode))

(defun aj-javascript/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "p" 'prettier-js-mode)
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "e" 'eslintd-fix-mode)
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "r" 'rjsx-rename-tag-at-point)
      (add-hook 'rjsx-mode-hook 'prettier-js-mode)
      (setq
       prettier-js-command "prettier_d"
       prettier-js-args '(
                               "--trailing-comma" "es5"
                               "--single-quote")))))
