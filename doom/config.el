;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Samim Pezeshki"
      user-mail-address "psamim@gmail.com")

(setq doom-localleader-key ",")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 20)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile")
      doom-unicode-font (font-spec :family "Iosevka")
      ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
      )

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(custom-set-variables '(emojify-display-style 'image))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq
  org-clock-persist t
  ;; org-duration-format 'h:mm
  org-duration-format '((special . h:mm))
  org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
  org-export-with-section-numbers nil
  org-icalendar-timezone "Asia/Tehran"
  org-agenda-diary-file "~/Notes/diary.org"
  org-roam-directory "~/Notes/roam"
  ;; org-caldav-url 'google
  ;; org-caldav-calendar-id "X"
  ;; org-caldav-files '("~/Notes/appointments.org")
  ;; org-caldav-oauth2-client-id "X"
  ;; org-caldav-oauth2-client-secret "X"
  ;; org-caldav-inbox "~/Notes/calendar-inbox.org"
  ;; org-caldav-delete-org-entries 'always
  ;; org-caldav-sync-changes-to-org 'all
  ;; org-caldav-sync-direction 'cal->org
  plstore-cache-passphrase-for-symmetric-encryption t
  org-ellipsis "…"
   ;; ➡, ⚡, ▼, ↴, , ∞, ⬎, ⤷, ⤵
  org-agenda-files (quote ("~/Notes/projects"
                           "~/Notes/calendar-inbox.org"
                           "~/Notes/roam/20210625224916-areas.org"
                           "~/Notes/roam/20210507181408-people.org"
                           "~/Notes/events.org"))
  org-deadline-warning-days 7
  org-agenda-breadcrumbs-separator " ❱ "
  org-directory "~/Notes")

(setq org-gcal-client-id "279358326453-ar2bfnerndjnnie90e59i9otuif9ut84.apps.googleusercontent.com"
      org-gcal-file-alist '(("samim@globalworkandtravel.com" .  "~/Notes/calendar-inbox.org")))

 (setq mixed-pitch-fixed-pitch-faces
   (quote (line-number-current-line line-number font-lock-comment-face org-done org-todo org-todo-keyword-outd org-todo-keyword-kill org-todo-keyword-wait org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-todo org-tag org-ref-cite-face org-property-value org-special-keyword org-date diff-added org-drawer diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim)))

(defun add-property-with-date-captured ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property "CREATED" (format-time-string "%F")))

(add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

(customize-set-variable
 'org-capture-templates
 (quote (
         ("t" "todo" entry
          (file+headline "~/Notes/projects/projects.org" "Inbox") "* %?\n%a\n" :clock-keep t)
         ("s" "schedule" entry
          (file+headline "~/Notes/projects/projects.org" "Inbox") "* %?\nSCHEDULED: %t" :clock-keep t)
         )))

(setq-hook! org-mode
  org-log-done t
  org-log-reschedule 'time
  org-image-actual-width nil
  org-clock-into-drawer t
  org-clock-persist t
  org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM"
  org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                ("STYLE_ALL" . "habit")))
  ;; org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar")
  ;; org-export-babel-evaluate nil
  org-confirm-babel-evaluate nil
  org-archive-location "~/Notes/archive/todo.org.gpg::"
  bidi-paragraph-direction t
  org-hide-emphasis-markers t
  org-fontify-done-headline t
  org-fontify-whole-heading-line t
  org-fontify-quote-and-verse-blocks t
  )

(customize-set-value
    'org-agenda-category-icon-alist
    `(
      ("work" "~/.dotfiles/icons/work.svg" nil nil :ascent center :mask heuristic)
      ("chore" "~/.dotfiles/icons/chore.svg" nil nil :ascent center :mask heuristic)
      ("events" "~/.dotfiles/icons/events.svg" nil nil :ascent center :mask heuristic)
      ("inbox" "~/.dotfiles/icons/inbox.svg" nil nil :ascent center :mask heuristic)
      ("walk" "~/.dotfiles/icons/walk.svg" nil nil :ascent center :mask heuristic)
      ("solution" "~/.dotfiles/icons/solution.svg" nil nil :ascent center :mask heuristic)
      ("community" "~/.dotfiles/icons/community.svg" nil nil :ascent center :mask heuristic)
      ("idea" "~/.dotfiles/icons/idea.svg" nil nil :ascent center :mask heuristic)
      ("man" "~/.dotfiles/icons/man.svg" nil nil :ascent center :mask heuristic)
      ("scheduled" "~/.dotfiles/icons/scheduled.svg" nil nil :ascent center :mask heuristic)
      ("class" "~/.dotfiles/icons/class.svg" nil nil :ascent center :mask heuristic)
      ("plant" "~/.dotfiles/icons/plant.svg" nil nil :ascent center :mask heuristic)
      ("check" "~/.dotfiles/icons/check.svg" nil nil :ascent center :mask heuristic)
      ("search" "~/.dotfiles/icons/search.svg" nil nil :ascent center :mask heuristic)
      ("home" "~/.dotfiles/icons/home.svg" nil nil :ascent center :mask heuristic)
      ("book" "~/.dotfiles/icons/book.svg" nil nil :ascent center :mask heuristic)
      ("cook" "~/.dotfiles/icons/cook.svg" nil nil :ascent center :mask heuristic)
      ("buy" "~/.dotfiles/icons/buy.svg" nil nil :ascent center :mask heuristic)
      ("shower" "~/.dotfiles/icons/shower.svg" nil nil :ascent center :mask heuristic)
      ))

(setq org-agenda-hidden-separator "‌‌ ")
(defun agenda-color-char ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "⚡" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:height 220 :foreground "gold2" :bold t))))

  ;; (save-excursion
  ;;   (goto-char (point-min))
  ;;   (while (re-search-forward org-agenda-hidden-separator nil t)
  ;;     (put-text-property (match-beginning 0) (- (match-end 0) 20)
  ;;                        'face '(:foreground "red"))))
  )

(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)

(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date 1 nil))
         (day (cadr date))
         (persian (substring (calendar-persian-date-string date) 0 -6))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month 1))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
         (format " %-2s. %2d %s, %s"
            dayname day monthname persian)))

(setq org-agenda-block-separator nil)

(setq org-agenda-custom-commands
      '(("a" "My Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero nil)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 2)
                      (org-agenda-overriding-header "⚡ Calendar")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")
                      ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                       ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline: " "Deadline: "))
                      (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

          (todo "TODO" (
                      (org-agenda-overriding-header "\n⚡ To Do")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format "   %-2i %?b")
                      (org-agenda-todo-keyword-format "")))

         ;; (todo "NEXT" (
         ;;              (org-agenda-todo-ignore-scheduled 'all)
         ;;              (org-agenda-overriding-header "⚡ THIS WEEK")
         ;;              (org-agenda-remove-tags t)
         ;;              (org-agenda-prefix-format "   %-2i %?b")
         ;;              (org-agenda-todo-keyword-format "")))

         (tags "+project" (
                      (org-agenda-overriding-header "\n⚡ Projects")
                      (org-agenda-remove-tags t)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i %?b")
                      (org-agenda-todo-keyword-format "")))

         ;; (org-ql-block '(and
         ;;                 (tags "project")
         ;;                 )
         ;;               (
         ;;                (org-ql-block-header "⚡ Projects and Areas")
         ;;                ))
         ))
("mo" "My Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero nil)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 3)
                      (org-agenda-overriding-header "⚡ Calendar")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")
                      ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                       ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline: " "Deadline: "))
                      (org-agenda-time-grid nil)))

          (todo "TODO" (
                      (org-agenda-overriding-header "\n⚡ To Do")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format "   %-2i %?b")
                      (org-agenda-todo-keyword-format "")))

         ))))

(defun psamim-journal-prefix (time)
  (let*
      (
       (decodedTime (decode-time time))
       (now (list (nth 4 decodedTime) (nth 3 decodedTime) (nth 5 decodedTime))))
    (concat
     ;; (format-time-string "%B %e, %Y" time) "\n"
     ;; (format-time-string "%A" time)
     "# " (calendar-persian-date-string now) "\n"
     "# " (calendar-bahai-date-string now) "\n\n")
    ))

;; (customize-set-variable 'org-journal-file-type "daily")
(setq!
 org-journal-dir "~/Notes/journal/daily"
 org-journal-encrypt-journal t
 org-journal-file-header 'psamim-journal-prefix)

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))


;; (set-email-account! "psamim@gmail.com"
;;   '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
;;     (mu4e-drafts-folder     . "/[Gmail].Drafts")
;;     (mu4e-trash-folder      . "/[Gmail].Bin")
;;     (mu4e-refile-folder     . "/[Gmail].All Mail")
;;     (smtpmail-smtp-user     . "psamim@gmail.com")
;;     (smtpmail-default-smtp-server . "smtp.gmail.com")
;;     (smtpmail-smtp-server . "smtp.gmail.com")
;;     (smtpmail-smtp-service . 587)
;;     (mu4e-compose-signature . "---\nSamim Pezeshki"))
;;   t)


(add-hook! '(org-clock-out-hook org-clock-in-hook) #'org-save-all-org-buffers)
(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)


(defun export-clock ()
  (interactive)
  (org-clock-csv-to-file "~/clock.csv" "~/Notes/clock.org"))

(defun my-org-agenda ()
  (interactive)
    (org-agenda nil "a"))

(defun my-org-index ()
  (interactive)
    (find-file "~/Notes/roam/20210625224818-index.org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(defun set-farsi-font ()
  (interactive)
  (set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
   ;; "Vazir Code-13")
   ;; "Tanha-16"))
   "Mikhak-16"))

;; (after! org-mode
;;   (set-company-backend! 'company-dabbrev)
;;   )
;;
;; (after! company
;;   (setq company-idle-delay 0.3
;;         company-minimum-prefix-length 1
;;   company-dabbrev-code-everywhere t
;;   company-dabbrev-code-other-buffers 'all))

(add-hook 'text-mode-hook
          (lambda ()
            (set-farsi-font)
            (setq olivetti-body-width 0.91)
            (olivetti-mode)
            (setq header-line-format " ")
            (mixed-pitch-mode 1)))

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(92 92))
;; (add-to-list 'default-frame-alist '(alpha . (92 . 92)))

(defun my-org-mode-autosave-settings ()
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))

(add-hook 'org-agenda-finalize-hook #'set-window-clean)
(add-hook! 'org-mode-hook #'my-org-mode-autosave-settings)

(add-hook 'org-mode-local-vars-hook #'(lambda () (eldoc-mode -1)))

;; (add-hook! 'mu4e-view-mode-hook
;;            #'olivetti-mode)

;; (global-org-pretty-table-mode)

(defun set-window-clean ()
  (agenda-color-char)
  (setq mode-line-format nil)
  (set-frame-parameter nil 'font "Iosevka-18")
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :background "#00000000")
  (set-window-margins (frame-selected-window) 4))

;; (setq org-journal-enable-agenda-integration t)

;; (global-activity-watch-mode)

(defun string-to-int (s) (string-to-number s))

(defun what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
            (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))



;; (use-package! mu4e
;;   :config
;;   (setq mu4e-use-fancy-chars nil
;;         mu4e-update-interval 300
;;         mu4e-headers-draft-mark '("D" . "")
;;         mu4e-headers-flagged-mark '("F" . "")
;;         mu4e-headers-new-mark '("N" . "✱")
;;         mu4e-headers-passed-mark '("P" . "❯")
;;         mu4e-headers-replied-mark '("R" . "❮")
;;         mu4e-headers-seen-mark '("S" . "✔")
;;         mu4e-headers-trashed-mark '("T" . "")
;;         mu4e-headers-attach-mark '("a" . "")
;;         mu4e-headers-encrypted-mark '("x" . "")
;;         mu4e-headers-signed-mark '("s" . "☡")
;;         mu4e-headers-unread-mark '("u" . "⎕")))

(use-package! doom-modeline
  :init
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode org-mode org-journal-mode)))

;; Define your custom doom-modeline
(doom-modeline-def-modeline 'my-simple-line
  '(buffer-info selection-info)
  '(misc-info input-method process))

;; Add to `doom-modeline-mode-hook` or other hooks
(defun setup-custom-doom-modeline ()
   (doom-modeline-set-modeline 'my-simple-line 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)


;; (mu4e-alert-set-default-style 'libnotify)
;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

(use-package unidecode
  :defer t
  :commands unidecode-region unidecode-sanitize-region
  :config
  (evil-define-operator evil-unidecode (beg end type register)
    "Applies unidecode to text as an Evil operator"
    :move-point nil
    :keep-visual nil
    (interactive "<R><x>")
    (when (evil-visual-state-p)
      (let ((range (evil-expand beg end
                                (if (and evil-respect-visual-line-mode
                                         visual-line-mode)
                                    'screen-line
                                  'line))))
        (setq beg (evil-range-beginning range)
              end (evil-range-end range)
              type (evil-type range))))
    (unidecode-region beg end))
  :general
  (general-define-key
   :states '(normal visual)
   "gt" 'evil-unidecode))

(require 'notmuch)
(add-to-list 'auto-mode-alist '("psamim@gmail.com" . notmuch-message-mode))

(setq sendmail-program "gmi")
(setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/.mail/account.gmail"))

(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	;; org-msg-greeting-fmt "<div dir='rtl'>\n\n</div>"
	org-msg-greeting-name-limit 3
	org-msg-text-plain-alternative t
	org-msg-signature "

 Regards,

 #+begin_signature
 -- *Samim Pezeshki* \\\\
 #+end_signature")
(org-msg-mode)

;; (after! evil-org (progn
;;                    (map! :map evil-org-mode-map :n "M-l" #'centaur-tabs-forward)
;;                    (map! :map evil-org-mode-map :n "M-h" #'centaur-tabs-backward)))
;; (map! "M-l" #'centaur-tabs-forward)
;; (map! "M-h" #'centaur-tabs-backward)
(map! "M-q" #'kill-current-buffer)
(map! "M-n" #'vterm)

;; (add-hook 'org-agenda-mode-hook 'centaur-tabs-local-mode)

(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

;; (defun org-toggle-tag-visibility (state)
;;   "Run in `org-cycle-hook'."
;;   (message "%s" state)
;;   (cond
;;    ;; global cycling
;;    ((memq state '(overview contents showall))
;;     (org-map-entries
;;      (lambda ()
;;        (let ((tagstring (nth 5 (org-heading-components)))
;;          start end)
;;      (when tagstring
;;        (save-excursion
;;          (beginning-of-line)
;;          (re-search-forward tagstring)
;;          (setq start (match-beginning 0)
;;            end (match-end 0)))
;;        (cond
;;         ((memq state '(overview contents))
;;          (outline-flag-region start end t))
;;         (t
;;          (outline-flag-region start end nil))))))))
;;    ;; local cycling
;;    ((memq state '(folded children subtree))
;;     (save-restriction
;;       (org-narrow-to-subtree)
;;       (org-map-entries
;;        (lambda ()
;;      (let ((tagstring (nth 5 (org-heading-components)))
;;            start end)
;;        (when tagstring
;;          (save-excursion
;;            (beginning-of-line)
;;            (re-search-forward tagstring)
;;            (setq start (match-beginning 0)
;;              end (match-end 0)))
;;          (cond
;;           ((memq state '(folded children))
;;            (outline-flag-region start end t))
;;           (t
;;            (outline-flag-region start end nil)))))))))))

;; (add-hook 'org-cycle-hook 'org-toggle-tag-visibility)

(doom-modeline-mode 0)

(after! org
;; ⧗             ―               ﮸          λ ◁ ▷ ✧ ✦
  (appendq! +ligatures-extra-symbols
            `(:clock      "⧗ "
              :circle "⚫"
              :shogi "⛊"
              :white_shogi "☖"
              :black_shogi "☗"
              :two_lines "⚏"
              :tags "    ‌"
              :empty ""
              ))
  (set-ligatures! 'org-mode
    :merge t
    :clock ":LOGBOOK:"
    :quote         "#+begin_quote"
    :name "#+CAPTION:"
    :quote_end     "#+end_quote"
    :src_block         "#+begin_src"
    :src_block         "#+BEGIN_SRC"
    :src_block         "#+BEGIN:"
    :src_block_end     "#+end_src"
    :src_block_end     "#+END_SRC"
    :src_block_end     ":END:"
    :src_block_end     "#+END"
    :two_lines   ":PROPERTIES:"
    :black_shogi   "#+CATEGORY:"
    :black_shogi   "#+category:"
    ;; :two_lines   "#+startup:"
    ;; :two_lines   "#+STARTUP:"
    :empty "#+title:"
    :empty "#+TITLE:"
    :shogi "#+NAME:"
    :shogi "#+name:"
    ;; :tags "keywords:"
    :black_shogi "#+roam_tags:"
    ))

(use-package! org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚑" "⬆" "⬇")))

(setq org-superstar-headline-bullets-list '("◯" "∙" "∘" "∘" "∘" "∘" "∘" "∘"))

(setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "NEXT(n)"  ; This task going to be done thiw iteration (week)
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("NEXT" . +org-todo-project)
          ("PROJ" . +org-todo-project)))

(custom-theme-set-faces!
  'doom-solarized-light
  '((org-agenda-date org-agenda-date-weekend) :foreground "#586e75"
    :weight semibold :slant normal
    :box (:line-width 7 :color "#fffbea" :style nil))
  '((org-agenda-date-today) :foreground "#073642" :weight semibold :slant normal
    :box (:line-width 7 :color "#fffbea" :style nil)
    )
  '((org-agenda-calendar-event)  :weight light)

  '((org-agenda-done)
     :foreground "#91a6ad"
     :weight light
     :strike-through "dark gray")

  '((org-scheduled org-scheduled-today)  :foreground "#556b72" :weight light)
  '((org-agenda-structure) :family "pacifico" :height 220
    :box (:line-width 2 :color "#fffbea" :style nil))
  '((org-ellipsis) :height 1.0)
  '((org-level-1) :foreground "#bf360c" :weight normal :height 1.3 :inherit outline-1)
  '((org-level-2) :weight normal :foreground "#211221" :inherit outline-2)
  '((org-level-3) :weight normal :inherit outline-3 :foreground "#424242")
  '((org-level-4) :weight normal :inherit outline-4 :foreground "#616161")
  '((org-level-5) :weight normal :inherit outline-5 :foreground "#616161")
  '((org-level-6) :weight normal :inherit outline-6 :foreground "#616161")
  '((org-link) :weight normal :inherit link)
  '((org-document-title) :height 1.6)
  '(org-tag :foreground "#fbf5e3")
  '((org-drawer org-meta-line org-headline-done) :foreground "dark gray")
  '((org-block-begin-line org-block-end-line) :foreground "dark gray" :background "#f7edd0" :extend t)
  '((org-table) :background "#f7edd0"))

(map! :map magit-status-mode-map :n "<tab>" 'magit-section-toggle)

(setq ispell-dictionary "en")

;; (use-package! nroam
;;   :after org-roam
;;   :config
;;   (add-hook 'org-mode-hook #'nroam-setup-maybe))

(setq +org-roam-open-buffer-on-find-file nil)

(use-package! org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(defun load-secrets () (interactive) (load "~/.doom.d/secrets.el.gpg"))

(defun sync-calendars ()
  (interactive)
  (progn
    (load-secrets)
    (org-gcal-fetch)))

(run-with-timer 0 (* 18 60 60) 'sync-calendars)
(run-with-timer 0 (* 1 60 60) (lambda () (progn (sync-agenda-svg) (sleep-for 1) (my-org-agenda))))

;; https://orgmode.org/manual/Filtering_002flimiting-agenda-items.html
(defun my-auto-exclude-fn (tag)
  (when (member tag '("work" "global"))
    (concat "-" tag)))

(setq org-agenda-auto-exclude-function #'my-auto-exclude-fn)

(defun do-not-display-work ()
  (interactive)
  (org-agenda-filter-apply '("-work") 'tag))


(defun save-screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
  Saves to a chosen file and puts the filename in the kill ring."
  (interactive)
  (let* ((file-name (concat
                     (make-temp-name "Emacs-") ".svg"))
         (path "~/Notes/html/agenda/")
         (full-file-name (concat path file-name))
         (data (x-export-frames nil 'svg))
         (index-file-template "~/.dotfiles/doom/org-agenda.html.template")
         (index-file (concat path "index.html")))
    (dolist
        (var (directory-files path t "Emacs.*svg"))
      (delete-file var))
    (with-temp-file full-file-name
      (insert data))
    (with-temp-file index-file
      (progn
        (insert-file-contents index-file-template)
        (goto-char (point-min))
        (while (search-forward "{{FILENAME}}" nil t)
          (replace-match file-name t))))
    (message (concat "Saved screenshot to " file-name))))

(defun sync-agenda-svg ()
  "Save a screenshot of the current frame as an SVG image.
  Saves to a chosen file and puts the filename in the kill ring."
  (interactive)
  (progn
    (org-agenda nil "mo")
    (setq cursor-type nil)
    (save-screenshot-svg)
    (setq cursor-type 'box)))


(map! :localleader
      (:map org-mode-map
        "c e" #'export-clock))

(map! :localleader
      (:map ledger-mode-map
        "c" #'ledger-mode-clean-buffer))

(map! :localleader (:map org-agenda-mode-map "f p" #'do-not-display-work))
(map! :localleader (:map org-agenda-mode-map "o" #'org-agenda-set-property))
(map! :leader :desc "Org clock context" :nvg "n c" #'counsel-org-clock-context)
(map! :leader :desc "Dired" :nvg "d" #'dired-jump)
(map! :leader :desc "my-org-agenda" :nvg "na" 'my-org-agenda)
(map! :leader :desc "my-org-index" :nvg "ni" 'my-org-index)
(map! :leader :desc "sync-calendar" :nvg "rc" 'sync-calendars)
(map! :leader :desc "sync-agenda-svg" :nvg "ra" 'sync-agenda-svg)

(custom-theme-set-faces!
  'doom-one-light
  '((org-agenda-date org-agenda-date-weekend)
    ;; :foreground "#586e75"
    :weight semibold
    :slant normal
    :box (:line-width 7 :color "#fafafa" :style nil))
  '((org-agenda-date-today)
    ;; :foreground "#073642"
    :weight semibold
    :slant normal
    :box (:line-width 7 :color "#fafafa" :style nil))
  '((org-agenda-calendar-event
     org-scheduled
     org-scheduled-today
     org-agenda-calendar-sexp
     org-scheduled-previously)  :weight light)
  ;; '((org-agenda-done) :foreground "#91a6ad" :weight light :strike-through "dark gray")
  '((org-agenda-structure) :family "pacifico" :height 220
    :box (:line-width 2 :color "#fafafa" :style nil))
  '((org-document-title) :family "pacifico")
  '((org-ellipsis) :height 1.0)
  '((org-level-1) :weight normal :height 1.3 :inherit outline-1)
  '((org-level-2) :weight normal :foreground "#211221" :inherit outline-2)
  '((org-level-3) :weight normal :inherit outline-3 :foreground "#424242")
  '((org-level-4) :weight normal :inherit outline-4 :foreground "#616161")
  '((org-level-5) :weight normal :inherit outline-5 :foreground "#616161")
  '((org-level-6) :weight normal :inherit outline-6 :foreground "#616161")
  '((org-link) :weight normal :inherit link)
  '((org-document-title) :height 1.6)
  '(org-tag :foreground "#fafafa")
  '((org-drawer org-meta-line org-headline-done) :foreground "dark gray")
  '((org-block-begin-line org-block-end-line) :foreground "dark gray" :background "#f7edd0" :extend t)
  ;; '((org-table) :background "#f7edd0")
  )

;; (use-package! org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8181
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'."
  (let* ((remote? (file-remote-p file))
	 (file-or-data
	  (pcase org-display-remote-inline-images
	    ((guard (not remote?)) file)
	    (`download (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
	    (`cache (let ((revert-without-query '(".")))
		      (with-current-buffer (find-file-noselect file)
			(buffer-string))))
	    (`skip nil)
	    (other
	     (message "Invalid value of `org-display-remote-inline-images': %S"
		      other)
	     nil))))
    (when file-or-data
      (create-image file-or-data
		    (and (image-type-available-p 'imagemagick)
			 width
			 'imagemagick)
		    remote?
		    :width width :mask 'heuristic :ascent 'center))))

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

(setq ivy-use-selectable-prompt t)
