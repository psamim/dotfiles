(require 'org)
(setq org-default-notes-file "~/.orgs/notes.org"
      org-log-done t)

(setq org-agenda-files (list "~/.orgs/my.org.gpg"))

(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

;(require 'ob)
;(org-babel-do-load-languages
; 'org-babel-load-languages
; '((plantuml . t)))
;
;(setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))

(provide 'init-org)
