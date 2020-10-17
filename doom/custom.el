(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(custom-safe-themes
   '("37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "30095d77cf3b19b736e472f56899df6e072c9c3c404ea1e16dbcffb1544eaf08" "f908b0b99115953beaa28cafa4868a5ee2a723526bebababbb2f8caf18280b86" "3b116632d401aa4a6fce54b82d746b5441f543fa96619c1c79923e2f68ce1665" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" default))
 '(default-input-method "farsi-isiri-9147")
 '(doom-modeline-mode t)
 '(fci-rule-color "#D6D6D6")
 '(global-org-pretty-table-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(ledger-complete-in-steps t)
 '(mu4e-bookmarks
   '((:name "Inbox" :query "maildir:/INBOX" :key 100)
     (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
     (:name "Today's messages" :query "date:today..now" :key 116)
     (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
     (:name "Messages with images" :query "mime:image/*" :key 112)))
 '(mu4e-update-interval 300)
 '(objed-cursor-color "#dc322f")
 '(org-capture-templates
   '(("t" "todo" entry
      (file+headline "~/Notes/todo.org" "Inbox")
      "* %?
%a
" :clock-keep t)))
 '(package-selected-packages
   '(bbdb org-msg modus-vivendi-theme modus-operandi-theme zen-mode web-mode org-pretty-tags org-beautify-theme olivetti oauth2 oauth moe-theme lastfm emojify dired-quick-sort centered-window))
 '(pdf-view-midnight-colors (cons "#556b72" "#FDF6E3"))
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(safe-local-variable-values
   '((org-use-property-inheritance . t)
     (org-re-reveal-title-slide)))
 '(send-mail-function 'mailclient-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(vc-annotate-background "#FDF6E3")
 '(vc-annotate-color-map
   (list
    (cons 20 "#859900")
    (cons 40 "#959300")
    (cons 60 "#a58e00")
    (cons 80 "#b58900")
    (cons 100 "#bc7407")
    (cons 120 "#c35f0e")
    (cons 140 "#cb4b16")
    (cons 160 "#cd4439")
    (cons 180 "#d03d5d")
    (cons 200 "#d33682")
    (cons 220 "#d63466")
    (cons 240 "#d9334a")
    (cons 260 "#dc322f")
    (cons 280 "#dd5c56")
    (cons 300 "#de867e")
    (cons 320 "#dfb0a5")
    (cons 340 "#D6D6D6")
    (cons 360 "#D6D6D6")))
 '(vc-annotate-very-old-color nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-agenda-date ((t (:foreground "grey31"))))
;;  '(org-agenda-date-date ((t (:foreground "grey31"))))
;;  '(org-agenda-date-today ((t (:underline "on"))))
;;  '(org-agenda-date-weekend ((t (:foreground "grey31"))))
;;  '(org-agenda-structure ((t (:family "Pacifico" :height 200 :underline "on"))))
;;  '(org-drawer ((t (:foreground "dark gray"))))
;;  '(org-ellipsis ((t (:height 1.0))))
;;  '(org-headline-done ((t (:foreground "dark gray"))))
;;  '(org-level-1 ((t (:foreground "#bf360c" :weight normal :height 1.3 :inherit outline-1))))
;;  '(org-level-2 ((t (:weight normal :foreground "#424242" :inherit outline-2))))
;;  '(org-level-3 ((t (:weight normal :inherit outline-3))))
;;  '(org-link ((t (:weight normal :inherit link))))
;;  '(org-meta-line ((t (:foreground "dark gray"))))
;;  '(org-table ((t (:foreground "grey"))))
;;  '(org-tag ((t (:foreground "#fbf5e3")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date ((t (:box (:line-width 32 :color "Gray10")))))
 '(org-agenda-date-today ((t (:foreground "grey40"))))
 '(org-agenda-date-weekend ((t (:box (:line-width 32 :color "Gray10")))))
 '(org-agenda-structure ((t (:family "Iosevka Etoile" :height 240))))
 '(org-agerda-date-today ((t (:box (:line-width 32 :color "Gray10")))))
 '(org-drawer ((t (:foreground "dark gray"))))
 '(org-ellipsis ((t (:height 1.0))))
 '(org-headline-done ((t (:foreground "dark gray"))))
 '(org-level-1 ((t (:foreground "#bf360c" :weight normal :height 1.3 :inherit outline-1))))
 '(org-level-2 ((t (:weight normal :foreground "#424242" :inherit outline-2))))
 '(org-level-3 ((t (:weight normal :inherit outline-3))))
 '(org-link ((t (:weight normal :inherit link))))
 '(org-meta-line ((t (:foreground "dark gray"))))
 '(org-scheduled ((t (:foreground "grey"))))
 '(org-table ((t (:background "LightGoldenrodYellow"))))
 '(org-tag ((t (:foreground "#fbf5e3")))))
