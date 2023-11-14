(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(emojify-display-style 'unicode)
 '(org-capture-templates
   '(("t" "todo" entry
      (file "~/Notes/projects/misc.org")
      "* TODO %?\12%a\12" :clock-keep t)
     ("e" "event" entry
      (file+headline "~/Notes/events.org" "Inbox")
      "* %?\12" :clock-keep t)
     ("s" "schedule" entry
      (file+headline "~/Notes/events.org" "Inbox")
      "* %?\12SCHEDULED: %t" :clock-keep t)))
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(safe-local-variable-values
   '((org-use-property-inheritance . reveal_)
     (org-re-reveal-title-slide)
     (org-archive-location . "~/Notes/archive/work.org.gpg::"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
