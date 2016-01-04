;;; keybindings.el --- Colemak HJKL Layer key bindings File
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

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/distribution/spacemacs-base/keybindings.el

(evil-leader/set-key
  "iK" 'spacemacs/insert-line-below-no-indent
  "iH" 'spacemacs/insert-line-above-no-indent
  "ih" 'spacemacs/evil-insert-line-above
  "ik" 'spacemacs/evil-insert-line-below)

;; evil-loader does not define an unset-key function.
(define-key evil-leader--default-map "iJ" nil)
(define-key evil-leader--default-map "ij" nil)

;; Overload this toggle to restore the right keybindings.
(spacemacs|add-toggle visual-line-navigation
  :status visual-line-mode
  :on (progn
        (visual-line-mode)
        (define-key evil-motion-state-map "k" 'evil-next-visual-line)
        (define-key evil-motion-state-map "h" 'evil-previous-visual-line)
        (when (bound-and-true-p evil-escape-mode)
          (evil-escape-mode -1)
          (setq evil-escape-motion-state-shadowed-func nil)
          (define-key evil-motion-state-map "k" 'evil-next-visual-line)
          (define-key evil-motion-state-map "h" 'evil-previous-visual-line)
          (evil-escape-mode)))
  :off (progn
         (visual-line-mode -1)
         (define-key evil-motion-state-map "k" 'evil-next-line)
         (define-key evil-motion-state-map "h" 'evil-previous-line)
         (when (bound-and-true-p evil-escape-mode)
           (evil-escape-mode -1)
           (setq evil-escape-motion-state-shadowed-func nil)
           (define-key evil-motion-state-map "k" 'evil-next-line)
           (define-key evil-motion-state-map "h" 'evil-previous-line)
           (evil-escape-mode)))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")

(evil-leader/set-key
  "wJ" 'evil-window-move-far-left
  "wj" 'evil-window-left
  "wK" 'evil-window-move-very-bottom
  "wk" 'evil-window-down
  "wH" 'evil-window-move-very-top
  "wh" 'evil-window-up
  "wL" 'evil-window-move-far-right
  "wl" 'evil-window-right)

(defun colemak-hjkl//window-manipulation-full-doc ()
  "Full documentation for window manipulation micro-state."
  "
    [?]                       display this help
    [0,9]                     go to numbered window
    [-] [/] [s] [v] [S] [V]   split windows bellow|right and focus
    [c] [C]                   close current|other windows
    [g]                       toggle golden-ratio
    [j] [k] [h] [l]           go to left|bottom|top|right
    [J] [K] [H] [L]           move windows to far/very left|bottom|top|right
    [[] []] [{] [}]           shrink/enlarge horizontaly and verticaly respectively
    [o] [w]                   other frame|window
    [R]                       rotate windows
    [u] [U]                   restore previous|next window layout")

(defun colemak-hjkl//window-manipulation-move-doc ()
  "Help string for moving between windows"
  (concat "[j] [k] [h] [l] to move focus, "
          "[J] [K] [H] [L] to move window, "
          "[R]otate windows, other [f]rame, other [w]indow"))

;; Have to redefine the whole macro. Not sure if we are leaving some things
;; dangling after that. Seems to work well.
(spacemacs|define-micro-state window-manipulation
  :doc "[?] for help"
  :evil-leader "w."
  :use-minibuffer t
  :bindings
  ("?" nil                                   :doc (colemak-hjkl//window-manipulation-full-doc))
  ("0" select-window-0                       :doc (spacemacs//window-manipulation-number-doc))
  ("1" select-window-1                       :doc (spacemacs//window-manipulation-number-doc))
  ("2" select-window-2                       :doc (spacemacs//window-manipulation-number-doc))
  ("3" select-window-3                       :doc (spacemacs//window-manipulation-number-doc))
  ("4" select-window-4                       :doc (spacemacs//window-manipulation-number-doc))
  ("5" select-window-5                       :doc (spacemacs//window-manipulation-number-doc))
  ("6" select-window-6                       :doc (spacemacs//window-manipulation-number-doc))
  ("7" select-window-7                       :doc (spacemacs//window-manipulation-number-doc))
  ("8" select-window-8                       :doc (spacemacs//window-manipulation-number-doc))
  ("9" select-window-9                       :doc (spacemacs//window-manipulation-number-doc))
  ("-" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("/" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("[" spacemacs/shrink-window-horizontally  :doc (spacemacs//window-manipulation-resize-doc))
  ("]" spacemacs/enlarge-window-horizontally :doc (spacemacs//window-manipulation-resize-doc))
  ("{" spacemacs/shrink-window               :doc (spacemacs//window-manipulation-resize-doc))
  ("}" spacemacs/enlarge-window              :doc (spacemacs//window-manipulation-resize-doc))
  ("c" delete-window                         :doc (spacemacs//window-manipulation-layout-doc))
  ("C" delete-other-windows                  :doc (spacemacs//window-manipulation-layout-doc))
  ("g" spacemacs/toggle-golden-ratio         :doc (spacemacs//window-manipulation-gratio-doc))
  ("j" evil-window-left                      :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<left>" evil-window-left                 :doc (colemak-hjkl//window-manipulation-move-doc))
  ("k" evil-window-down                      :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<down>" evil-window-down                 :doc (colemak-hjkl//window-manipulation-move-doc))
  ("h" evil-window-up                        :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<up>" evil-window-up                     :doc (colemak-hjkl//window-manipulation-move-doc))
  ("l" evil-window-right                     :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<right>" evil-window-right               :doc (colemak-hjkl//window-manipulation-move-doc))
  ("J" evil-window-move-far-left             :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<S-left>" evil-window-move-far-left      :doc (colemak-hjkl//window-manipulation-move-doc))
  ("K" evil-window-move-very-bottom          :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<S-down>" evil-window-move-very-bottom   :doc (colemak-hjkl//window-manipulation-move-doc))
  ("H" evil-window-move-very-top             :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<S-up>" evil-window-move-very-top        :doc (colemak-hjkl//window-manipulation-move-doc))
  ("L" evil-window-move-far-right            :doc (colemak-hjkl//window-manipulation-move-doc))
  ("<S-right>" evil-window-move-far-right    :doc (colemak-hjkl//window-manipulation-move-doc))
  ("o" other-frame                           :doc (colemak-hjkl//window-manipulation-move-doc))
  ("R" spacemacs/rotate-windows              :doc (colemak-hjkl//window-manipulation-move-doc))
  ("s" split-window-below                    :doc (spacemacs//window-manipulation-split-doc))
  ("S" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("u" winner-undo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("U" winner-redo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("v" split-window-right                    :doc (spacemacs//window-manipulation-split-doc))
  ("V" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("w" other-window                          :doc (colemak-hjkl//window-manipulation-move-doc)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override core/core-evilified-state.el

(with-eval-after-load 'core-evilified-state
  (define-key evil-evilified-state-map "j" 'evil-backward-char)
  (define-key evil-evilified-state-map "k" 'evil-next-visual-line)
  (define-key evil-evilified-state-map "h" 'evil-previous-visual-line))
