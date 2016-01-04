;;; packages.el --- Colemak HJKL Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Remap HJKL for Colemak layout.
;; (left . h), (down . j), (top . k)
;; becomes
;; (left . j), (down . k), (top . h)

;; Remap all HJKL bindings used as movement keys in packages loaded by Spacemacs
;; or by other layers. We want to execute our remapping after the configuration
;; from other layers, so we use `spacemacs|use-package-add-hook' on
;; `:post-config'. We add these hooks in the `pre-init' phase of package
;; loading, so that they are correctly installed /before/ the packages are
;; loaded via `use-package'.

;; Using `with-eval-after-load' is not sufficient, as we do not know if the body
;; will be executed after the `:config' phase of `use-package'.

(setq colemak-hjkl-packages
      '(evil
        helm
        company
        auto-complete
        org
        evil-org
        web-mode
        magit))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/distribution/spacemacs-base/packages.el

(defun colemak-hjkl/pre-init-evil ()
  (spacemacs|use-package-add-hook evil
    :post-config

    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; In elpa/evil/evil-maps.el

    ;; Window commands
    (define-key evil-window-map "j" 'evil-window-left)
    (define-key evil-window-map "J" 'evil-window-move-far-left)
    (define-key evil-window-map "k" 'evil-window-down)
    (define-key evil-window-map "K" 'evil-window-move-very-bottom)
    (define-key evil-window-map "h" 'evil-window-up)
    (define-key evil-window-map "H" 'evil-window-move-very-top)
    (define-key evil-window-map (kbd "C-S-j") 'evil-window-move-far-left)
    (define-key evil-window-map (kbd "C-S-k") 'evil-window-move-very-bottom)
    (define-key evil-window-map (kbd "C-S-h") 'evil-window-move-very-top)

    ;; Motion state
    (define-key evil-motion-state-map "j" 'evil-backward-char)
    (define-key evil-motion-state-map "k" 'evil-next-line)
    (define-key evil-motion-state-map "h" 'evil-previous-line)

    ;; Leave evil-window-top on H, but use K for evil-window-bottom. That leaves
    ;; L for evil-lookup.
    (define-key evil-motion-state-map "L" 'evil-lookup)
    (define-key evil-motion-state-map "K" 'evil-window-bottom)

    (define-key evil-motion-state-map "gk" 'evil-next-visual-line)
    (define-key evil-motion-state-map "gh" 'evil-previous-visual-line)
    (define-key evil-motion-state-map "gj" nil)

    (define-key evil-motion-state-map "zh" nil)
    (define-key evil-motion-state-map "zH" nil)
    (define-key evil-motion-state-map "zj" 'evil-scroll-column-left)
    (define-key evil-motion-state-map "zJ" 'evil-scroll-left)

    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; In elpa/evil/evil-integration.el

    (evil-add-hjkl-bindings Info-mode-map 'motion
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      (kbd "M-h") 'Info-help    ; "h"
      "\C-t" 'Info-history-back ; "l"
      "\C-o" 'Info-history-back
      "x" 'Info-scroll-up ; SPC is taken in Spacemacs
      "\C-]" 'Info-follow-nearest-node
      (kbd "DEL") 'Info-scroll-down)

    ;; TAB goes to next link in Help buffers.
    ;; FIXME: upstream this.
    (evil-define-key 'motion help-mode-map (kbd "TAB") 'forward-button)

    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; In layers/distribution/spacemacs-base/packages.el

    (define-key evil-normal-state-map "L" 'spacemacs/evil-smart-doc-lookup)
    (define-key evil-normal-state-map "K" nil)))

(defun colemak-hjkl/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    ;; need this otherwise helm-find-files-map is not defined
    (helm-mode +1)

    ;; helm navigation on hjkl
    (defun colemak-hjkl//helm-hjkl-navigation (&optional arg)
      "Set navigation in helm on `jklh'.
ARG non nil means that the editing style is `vim'."
      (cond
       (arg
        ;; better navigation on homerow
        ;; rebind `describe-key' for convenience
        (define-key helm-map (kbd "C-k") 'helm-next-line)
        (define-key helm-map (kbd "C-h") 'helm-previous-line)
        (define-key helm-map (kbd "C-j") 'helm-next-source)
        (define-key helm-map (kbd "C-S-h") 'describe-key)
        (dolist (keymap (list helm-find-files-map helm-read-file-map))
          (define-key keymap (kbd "C-j") 'helm-find-files-up-one-level)
          (define-key keymap (kbd "C-h") nil)
          (define-key keymap (kbd "C-S-h") 'describe-key)))
       (t
        (define-key helm-map (kbd "C-k") 'helm-execute-persistent-action)
        (define-key helm-map (kbd "C-h") 'helm-delete-minibuffer-contents)
        (define-key helm-map (kbd "C-j") nil))))
    (colemak-hjkl//helm-hjkl-navigation (member dotspacemacs-editing-style '(vim hybrid)))

    (defun colemak-hjkl//helm-navigation-ms-full-doc ()
      "Full documentation for helm navigation micro-state."
      "
  [?]          display this help
  [a]          toggle action selection page
  [e]          edit occurrences if supported
  [k] [j]      next/previous candidate
  [j] [l]      previous/next source
  [t]          toggle visible mark
  [T]          toggle all mark
  [v]          persistent action
  [q]          quit")

    (spacemacs|define-micro-state colemak-hjkl/helm-navigation
      :persistent t
      :disable-evil-leader t
      :define-key (helm-map . "M-SPC") (helm-map . "s-M-SPC")
      :on-enter (spacemacs//helm-navigation-ms-on-enter)
      :on-exit  (spacemacs//helm-navigation-ms-on-exit)
      :bindings
      ("<tab>" helm-select-action :exit t)
      ("C-i" helm-select-action :exit t)
      ("<RET>" helm-maybe-exit-minibuffer :exit t)
      ("?" nil :doc (colemak-hjkl//helm-navigation-ms-full-doc))
      ("a" helm-select-action :post (spacemacs//helm-navigation-ms-set-face))
      ("e" spacemacs/helm-edit)
      ("j" helm-previous-source)
      ("k" helm-next-line)
      ("h" helm-previous-line)
      ("l" helm-next-source)
      ("q" nil :exit t)
      ("t" helm-toggle-visible-mark)
      ("T" helm-toggle-all-marks)
      ("v" helm-execute-persistent-action))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/auto-completion/packages.el

(defun colemak-hjkl/pre-init-company ()
  (spacemacs|use-package-add-hook company
    :post-config
    (define-key company-active-map (kbd "C-k") 'company-select-next)
    (define-key company-active-map (kbd "C-h") 'company-select-previous)
    (define-key company-active-map (kbd "C-j") nil)))

(defun colemak-hjkl/pre-init-auto-complete ()
  (spacemacs|use-package-add-hook auto-complete
    :post-config
    (define-key ac-completing-map (kbd "C-k") 'ac-next)
    (define-key ac-completing-map (kbd "C-h") 'ac-previous)
    (define-key ac-completing-map (kbd "C-j") nil)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/org/packages.el

(defun colemak-hjkl/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (evil-leader/set-key-for-mode 'org-mode
      ;; More cycling options (timestamps, headlines, items, properties)
      "mJ" 'org-shiftleft
      "mK" 'org-shiftdown
      "mH" 'org-shiftup

      ;; Change between TODO sets
      "m C-S-j" 'org-shiftcontrolleft
      "m C-S-k" 'org-shiftcontroldown
      "m C-S-h" 'org-shiftcontrolup

      ;; Subtree editing
      "mSl" 'org-demote-subtree
      "mSj" 'org-promote-subtree
      "mSk" 'org-move-subtree-down
      "mSh" 'org-move-subtree-up

      ;; tables
      "mtj" 'org-table-previous-field
      "mtJ" 'org-table-move-column-left
      "mtk" 'org-table-next-row
      "mtK" 'org-table-move-row-down
      "mtH" 'org-table-move-row-up)

    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map "k" 'org-agenda-next-line)
      (define-key org-agenda-mode-map "h" 'org-agenda-previous-line)
      (define-key org-agenda-mode-map "j" nil))))


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override elpa/evil-org/evil.org.el

(defun colemak-hjkl/pre-init-evil-org ()
  (spacemacs|use-package-add-hook evil-org
    :post-config
    (evil-define-key 'normal evil-org-mode-map
      "gj" 'outline-up-heading
      "gk" 'org-forward-heading-same-level
      "gh" 'org-backward-heading-same-level
      (kbd "M-j") 'org-metaleft
      (kbd "M-h") 'org-metaup
      (kbd "M-k") 'org-metadown
      (kbd "M-J") 'org-shiftmetaleft
      (kbd "M-H") 'org-shiftmetaup
      (kbd "M-K") 'org-shiftmetadown)

    (evil-define-key 'insert evil-org-mode-map
      (kbd "M-j") 'org-metaleft
      (kbd "M-h") 'org-metaup
      (kbd "M-k") 'org-metadown
      (kbd "M-J") 'org-shiftmetaleft
      (kbd "M-H") 'org-shiftmetaup
      (kbd "M-K") 'org-shiftmetadown)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/+lang/html/packages.el

(defun colemak-hjkl/pre-init-web-mode ()
  (spacemacs|use-package-add-hook web-mode
    :post-config
    (defun colemak-hjkl//web-mode-ms-doc ()
      (if (equal 0 spacemacs--web-mode-ms-doc-toggle)
          "[?] for help"
        "
  [?] display this help
  [h] previous [k] next   [H] previous sibling [K] next sibling
  [h] parent   [l] child  [c] clone [d] delete [D] kill [r] rename
  [w] wrap     [p] xpath
  [q] quit"))

    (spacemacs|define-micro-state web-mode
      :doc (colemak-hjkl//web-mode-ms-doc)
      :persistent t
      :evil-leader-for-mode (web-mode . "m.")
      :bindings
      ("<escape>" nil :exit t)
      ("?" spacemacs//web-mode-ms-toggle-doc)
      ("c" web-mode-element-clone)
      ("d" web-mode-element-vanish)
      ("D" web-mode-element-kill)
      ("k" web-mode-element-next)
      ("K" web-mode-element-sibling-next)
      ("gk" web-mode-element-sibling-next)
      ("h" web-mode-element-previous)
      ("H" web-mode-element-sibling-previous)
      ("gh" web-mode-element-sibling-previous)
      ("j" web-mode-element-parent)
      ("l" web-mode-element-child)
      ("p" web-mode-dom-xpath)
      ("r" web-mode-element-rename :exit t)
      ("q" nil :exit t)
      ("w" web-mode-element-wrap))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/+source-control/git/packages.el

(defun colemak-hjkl/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (evil-define-key 'evilified git-rebase-mode-map "H" 'git-rebase-move-line-up)
    (evil-define-key 'evilified git-rebase-mode-map "K" 'git-rebase-move-line-down)
    (evil-define-key 'evilified git-rebase-mode-map "J" nil)))
