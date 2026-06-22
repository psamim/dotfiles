(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "ac18cc10455c6c26a98354ba8f9d338842d7ecc9ae3d28c205ed154ef20d74ce" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(emojify-display-style 'unicode)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("/home/samim/Notes/roam/20210507181408-people.gpg.org" "/home/samim/Notes/roam/20210625224916-areas.org" "/home/samim/Notes/calendar-inbox.org" "/home/samim/Notes/study.org" "/home/samim/Notes/events.org" "/home/samim/Notes/projects/misc.org" "/home/samim/Notes/projects/mobile-activities.org" "/home/samim/Notes/projects/projects.org" "/home/samim/Notes/journal/yearly/20240101.gpg"))
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
 '(package-selected-packages '(restclient))
 '(safe-local-variable-values
   '((org-tidy-properties-style quote inline)
     (org-tidy-properties-style . inline)
     (org-tidy-properties-style)
     (org-use-property-inheritance . reveal_)
     (org-re-reveal-title-slide)
     (org-archive-location . "~/Notes/archive/work.org.gpg::"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
