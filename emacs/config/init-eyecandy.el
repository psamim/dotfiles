;(require-package 'smartmode-line)
;(require 'smart-mode-line)
;(setq sml/theme 'dark)
;(sml/setup)

;(require-package 'pretty-mode)
;(setq pretty-default-groups '(:function))
;(require 'pretty-mode)
;(global-pretty-mode)

; Window Title
(setq frame-title-format '("%b - %F"))

(require-package 'diminish)
(after 'diminish-autoloads
  (diminish 'visual-line-mode)
  (after 'autopair (diminish 'autopair-mode))
  (after 'undo-tree (diminish 'undo-tree-mode))
  (after 'auto-complete (diminish 'auto-complete-mode))
  (after 'projectile (diminish 'projectile-mode))
  (after 'yasnippet (diminish 'yas-minor-mode))
  (after 'guide-key (diminish 'guide-key-mode))
  (after 'eldoc (diminish 'eldoc-mode))
  (after 'smartparens (diminish 'smartparens-mode))
  (after 'company (diminish 'company-mode))
  (after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
  (after 'git-gutter+ (diminish 'git-gutter+-mode)))


;(require 'linum)
;(setq-default linum-format "%4d ")
;(add-hook 'find-file-hook (lambda ()
                            ;(hl-line-mode)
                            ;(linum-mode)))

;(set-face-attribute 'default nil :font
;"DejaVu Sans Mono:pixelsize=15:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")


(set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
   "Dejavu Sans Mono-13")

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (menu-bar-mode t)
              (font . "Inconsolata-14")
              ))


(require-package 'solarized-theme)
;(require-package 'color-theme-sanityinc-tomorrow)
(load-theme 'solarized-dark)
;(load-theme 'sanityinc-tomorrow-night)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(secondary-selection ((t (:background "#002B36")))))
(require-package 'writeroom-mode)

;(require-package 'powerline)
;(require 'powerline)
;(powerline-center-evil-theme)
;(powerline-vim-theme)

;(menu-bar-mode -1)
;(toggle-scroll-bar -1)
(tool-bar-mode -1)
;(setq-default mode-line-format nil)

; Mouse moves only one line
(setq mouse-wheel-follow-mouse 't)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(setq org-startup-indented t)

(provide 'init-eyecandy)
