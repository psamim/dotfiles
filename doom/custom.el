(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#FFFBEA" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(custom-safe-themes
   (quote
    ("5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" default)))
 '(fci-rule-color "#D6D6D6")
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(mu4e-bookmarks
   (quote
    ((:name "Inbox" :query "maildir:/INBOX" :key 100)
     (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
     (:name "Today's messages" :query "date:today..now" :key 116)
     (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
     (:name "Messages with images" :query "mime:image/*" :key 112))))
 '(mu4e-update-interval 300)
 '(objed-cursor-color "#dc322f")
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file+headline "~/Notes/todo.org" "Inbox")
      "* %?
%a
" :clock-keep t))))
 '(pdf-view-midnight-colors (cons "#556b72" "#FDF6E3"))
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(send-mail-function (quote mailclient-send-it))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-structure ((t (:family "Pacifico" :height 200 :underline "on"))))
 '(org-drawer ((t (:foreground "dark gray"))))
 '(org-ellipsis ((t (:height 1.0))))
 '(org-headline-done ((t (:foreground "dark gray"))))
 '(org-level-1 ((t (:foreground "#bf360c" :weight normal :height 1.3 :inherit outline-1))))
 '(org-level-2 ((t (:weight normal :foreground "#424242" :inherit outline-2))))
 '(org-level-3 ((t (:weight normal :inherit outline-3))))
 '(org-link ((t (:weight normal :inherit link))))
 '(org-meta-line ((t (:foreground "dark gray"))))
 '(org-tag ((t (:foreground "#fbf5e3")))))
