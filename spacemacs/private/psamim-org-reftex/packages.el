;;; packages.el --- psamim-org-reftex layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Samim Pezeshki <psamim@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst psamim-org-reftex-packages
  '((reftex :location built-in)))

(defun psamim-org-reftex/init-reftex ()
  (add-hook 'org-mode-hook 'turn-on-reftex)
  (spacemacs|diminish reftex-mode " ⓡ" " r")
  (spacemacs/declare-prefix-for-mode 'org-mode "mr"  "reftex")
  (setq reftex-cite-format
        '((?\C-m . "\\cite[]{%l}")
          (?f . "\\footcite[][]{%l}")
          (?t . "\\textcite[]{%l}")
          (?p . "\\parencite[]{%l}")
          (?o . "\\citepr[]{%l}")
          (?n . "\\nocite{%l}")))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "rc"    'reftex-citation
    "ic"    'reftex-citation
    "rg"    'reftex-grep-document
    "ri"    'reftex-index-selection-or-word
    "rI"    'reftex-display-index
    "r TAB" 'reftex-index
    "rl"    'reftex-label
    "rp"    'reftex-index-phrase-selection-or-word
    "rP"    'reftex-index-visit-phrases-buffer
    "rr"    'reftex-reference
    "rs"    'reftex-search-document
    "rt"    'reftex-toc
    "rT"    'reftex-toc-recenter
    "rv"    'reftex-view-crossref))
