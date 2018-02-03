(defconst psamim-js-packages
  '(
    (eslintd-fix :location (recipe :fetcher github :repo "aaronjensen/eslintd-fix"))
    flycheck
    company-flow
    prettier-js
    ))

(defun aj-javascript/init-company-flow ()
  (use-package company-flow
    :defer t
    :init
    (progn
      (push 'company-flow company-backends-js2-mode)
      (push 'company-flow company-backends-rjsx-mode)
      (when (configuration-layer/package-usedp 'web-mode)
        (push 'company-flow company-backends-react-mode))
      )
    :config
    (when (configuration-layer/package-usedp 'web-mode)
      (push 'react-mode company-flow-modes))))

(defun psamim-js/init-eslintd-fix ()
  (use-package eslintd-fix
    :defer t
    :commands eslintd-fix-mode
    :init
    (progn
      (add-hook 'react-mode-hook #'eslintd-fix-mode t))))

(defun psamim-js/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))
  (spacemacs/enable-flycheck 'react-mode))

(defun psamim-js/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'react-mode "p" 'prettier-js-mode)
      (spacemacs/set-leader-keys-for-major-mode 'react-mode "e" 'eslintd-fix-mode)
      ;; (add-hook 'react-mode-hook 'prettier-js-mode)
      (setq
       prettier-js-command "prettier_d"
       prettier-js-args '(
                          "--trailing-comma" "es5"
                          "--single-quote")))))
