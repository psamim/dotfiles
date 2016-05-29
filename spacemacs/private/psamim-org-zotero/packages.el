;;; packages.el --- psamim-org-zotero layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Samim Pezeshki <psamim@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst psamim-org-zotero-packages
  '(zotxt
    zotelo))

(defun psamim-org-zotero/init-zotxt ()
  (spacemacs|diminish org-zotxt-mode " Ⓩ" " z")
  (spacemacs/declare-prefix-for-mode 'org-mode "mz"  "zotero")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "zi"    'org-zotxt-insert-reference-link
    "iz"    'org-zotxt-insert-reference-link
    "zo"    'org-zotxt-open-attachment)
  (add-hook 'org-mode-hook 'org-zotxt-mode))

;; (defun org-psamim/post-init-zotxt ()
  ;; This citation style should be installed in Zotero
  ;; http://www.mkbehr.com/files/mkbehr-short.csl
  ;; It's nothing special or needed, only a shorter text
  ;; (setq zotxt-default-bibliography-style "mkbehr-short"))

(defun psamim-org-zotero/init-zotelo ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "zu"    'zotelo-update-database
    "zs"    'zotelo-set-collection))
