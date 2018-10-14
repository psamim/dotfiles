;;; packages.el --- react Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq psamim-react-packages
      '(
        evil-matchit
        flycheck
        js-doc
        smartparens
        web-beautify
        web-mode
        ))

(defun psamim-react/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'react-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump)))))

(defun psamim-react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'react-mode)))
  (spacemacs/enable-flycheck 'react-mode))

(defun psamim-react/post-init-js-doc ()
  (add-hook 'react-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'react-mode))

(defun psamim-react/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'react-mode-hook #'smartparens-strict-mode)
    (add-hook 'react-mode-hook #'smartparens-mode)))

(defun psamim-react/post-init-web-beautify ()
  (spacemacs/set-leader-keys-for-major-mode 'react-mode  "=" 'web-beautify-js))

(defun psamim-react/post-init-web-mode ()
  (define-derived-mode react-mode web-mode "react")
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . react-mode))
  (add-to-list 'magic-mode-alist '("/\\*\\* @jsx .*\\*/" . react-mode))
  (add-to-list 'magic-mode-alist '("import\s+[^\s]+\s+from\s+['\"]react['\"]" . react-mode))
  (add-hook 'react-mode-hook 'spacemacs//setup-react-mode))
