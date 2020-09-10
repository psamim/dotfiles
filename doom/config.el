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
(setq doom-font (font-spec :family "Iosevka" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile")
      doom-unicode-font (font-spec :family "Iosevka")
      ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
      )

;; (setq
;;  modus-operandi-theme-bold-constructs t
;;  modus-operandi-theme-slanted-constructs t
;;  modus-operandi-theme-org-blocks 'greyscale
;;  modus-operandi-theme-section-headings nil
;;  modus-operandi-theme-rainbow-headings nil
;;  modus-operandi-theme-scale-headings t
;;  modus-operandi-theme-variable-pitch-headings t
;;  modus-operandi-theme-3d-modeline nil)

;; (custom-theme-set-faces!
;;   'modus-operandi
;;   '((org-tag) :foreground "#f8f8f8")
;;   '((org-drawer org-meta-line org-headline-done) :foreground "grey58"))

  ;; invoke the above function when appropriate in order to override the
  ;; styles of the desired faces
  ;; (add-hook 'after-load-theme-hook 'customize-modus-operandi)

  ;; load the theme
  ;; (load-theme 'modus-operandi t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq
  org-clock-persist 'history
  org-icalendar-timezone "Asia/Tehran"
  ;; org-caldav-url 'google
  ;; org-caldav-calendar-id "samim@globalworkandtravel.com"
  ;; org-caldav-files '("~/Notes/appointments.org")
  ;; org-caldav-oauth2-client-id "702215651471-9g9vjg9hhnp7hathnteh4iajecp91atm.apps.googleusercontent.com"
  ;; org-caldav-oauth2-client-secret "tnc8k2D9w0RqPIRyEag4AqhQ"
  ;; org-caldav-inbox "~/Notes/calendar-inbox.org"
  ;; org-caldav-delete-org-entries 'always
  ;; org-caldav-sync-changes-to-org 'all
  ;; org-caldav-sync-direction 'cal->org
  plstore-cache-passphrase-for-symmetric-encryption t
  org-ellipsis "…"
   ;; ➡, ⚡, ▼, ↴, , ∞, ⬎, ⤷, ⤵
  org-agenda-files (quote ("~/Notes/todo.org" "~/Notes/someday.org" "~/Notes/calendar-inbox.org"))
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

(customize-set-variable 'org-capture-templates
      (quote (("t" "todo" entry (file+headline "~/Notes/todo.org" "Inbox")
               "* %?\n%a\n" :clock-keep t))))



(setq-hook! org-mode
  org-log-done t
  org-image-actual-width '(700)
  org-clock-into-drawer t
  org-clock-persist t
  org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM"
  org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                ("STYLE_ALL" . "habit")))
  ;; org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar")
  ;; org-export-babel-evaluate nil
  org-confirm-babel-evaluate nil
  ;; org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE"))
  org-archive-location "~/Notes/archive/todo.org.gpg::"
  org-duration-format '((special . h:mm))
  org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  bidi-paragraph-direction t
  org-hide-emphasis-markers t
  org-fontify-done-headline t
  org-fontify-whole-heading-line t
  org-fontify-quote-and-verse-blocks t
  )


(customize-set-value
    'org-agenda-category-icon-alist
    `(
      ("work" "~/.dotfiles/icons/money-bag.svg" nil nil :ascent center)
      ("chore" "~/.dotfiles/icons/loop.svg" nil nil :ascent center)
      ("events" "~/.dotfiles/icons/calendar.svg" nil nil :ascent center)
      ("todo" "~/.dotfiles/icons/checklist.svg" nil nil :ascent center)
      ("walk" "~/.dotfiles/icons/walk.svg" nil nil :ascent center)
      ("solution" "~/.dotfiles/icons/solution.svg" nil nil :ascent center)
      ))



(setq org-agenda-hidden-separator "‌‌ ")
(defun agenda-color-char ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "⚡" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:height 300 :foreground "gold2" :bold t))))

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

(setq org-agenda-custom-commands
      '(("o" "My Agenda"
         ((todo "TODO" (
                      (org-agenda-overriding-header "⚡ TO DO:\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "  %-2i %-13b")
                      (org-agenda-todo-keyword-format "")))
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 5)
                      (org-agenda-overriding-header "⚡ SCHEDULE:\n")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                       ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-time)
                      (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                      ;; (org-agenda-scheduled-leaders '("" ""))
                      ;; (org-agenda-deadline-leaders '("" ""))
                      (org-agenda-time-grid (quote ((require-timed remove-match) (0900 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))))

(defun psamim-journal-prefix (time)
  (let*
      (
       (decodedTime (decode-time time))
       (now (list (nth 4 decodedTime) (nth 3 decodedTime) (nth 5 decodedTime))))
    (concat
     ;; (format-time-string "%B %e, %Y" time) "\n"
     ;; (format-time-string "%A" time)
     "- امروز چی کارا کردی؟" "\n"
     "- چیز خاصی ازشون یادگرفتی که بخوای یادت بمونه؟" "\n"
     "- چه احساسی داری؟" "\n"
     "- چیزی هست که خیلی داری بش فکر می‌کنی؟ زودتر حلش کن." "\n"
     "- فردا چی کارا داری؟" "\n\n"
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

(setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))


(set-email-account! "psamim@gmail.com"
  '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
    (mu4e-drafts-folder     . "/[Gmail].Drafts")
    (mu4e-trash-folder      . "/[Gmail].Bin")
    (mu4e-refile-folder     . "/[Gmail].All Mail")
    (smtpmail-smtp-user     . "psamim@gmail.com")
    (smtpmail-default-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-service . 587)
    (mu4e-compose-signature . "---\nSamim Pezeshki"))
  t)


(add-hook! '(org-clock-out-hook org-clock-in-hook) #'org-save-all-org-buffers)
(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)


(defun export-clock ()
  (interactive)
  (org-clock-csv-to-file "~/clock.csv" "~/Notes/clock.org"))

(map! :localleader
      (:map org-mode-map
        "c e" #'export-clock))

(map! :localleader
      (:map ledger-mode-map
        "c" #'ledger-mode-clean-buffer))

(after! ledger-mode
  (set-company-backend! 'ledger-mode 'ledger-mode))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
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
(after! company
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
  company-dabbrev-code-everywhere t
  company-dabbrev-code-other-buffers 'all))

(add-hook 'text-mode-hook
           (lambda ()
            (mixed-pitch-mode 1)))

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(100 100))
;; (add-to-list 'default-frame-alist '(alpha . (92 . 92)))


(defun my-org-mode-autosave-settings ()
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))

(add-hook 'org-agenda-finalize-hook #'set-window-clean)
(add-hook! 'org-mode-hook
           #'set-farsi-font
           #'olivetti-mode
           #'mixed-pitch-mode
           #'my-org-mode-autosave-settings)

(add-hook 'org-mode-local-vars-hook #'(lambda () (eldoc-mode -1)))

(add-hook! 'mu4e-view-mode-hook
           #'olivetti-mode)

(global-org-pretty-table-mode)

(custom-set-faces!
  '((org-agerda-date-today
     org-agenda-date
     org-agenda-date-weekend)
    :box (:line-width 32 :color "Gray10"))
  '(org-scheduled
    :foreground "grey")
  '((org-drawer org-meta-line org-headline-done) :foreground "dark gray")
  '(org-tag :foreground "#fbf5e3"  )
  '(org-ellipsis :height 1.0)
  '(org-level-1 :foreground "#bf360c" :weight normal :height 1.3 :inherit outline-1)
  '(org-level-2 :weight normal :foreground "#424242" :inherit outline-2)
  '(org-level-3 :weight normal :inherit outline-3)
  '(org-link :weight normal :inherit link)
  '(org-table :background "LightGoldenrodYellow")
  '(org-agenda-date-today :foreground "grey40")
  '(org-agenda-date :foreground "grey40")
  '(org-agenda-date-weekend :foreground "grey40")
  '(org-agenda-structure
    :family "Iosevka Etoile"
    :height 240))

(defun set-window-clean ()
  (agenda-color-char)
  (setq mode-line-format nil)
  (set-frame-parameter nil 'font "Iosevka-18")
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :background "#00000000")
  (set-window-margins (frame-selected-window) 4))

(setq org-journal-enable-agenda-integration t)

;; (global-activity-watch-mode)

(defun string-to-int (s) (string-to-number s))

(defun what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
            (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun loadSecrets () (interactive) (load "~/.doom.d/secrets.el.gpg"))


(use-package! mu4e
  :config
  (setq mu4e-use-fancy-chars nil
        mu4e-update-interval 300
        mu4e-headers-draft-mark '("D" . "")
        mu4e-headers-flagged-mark '("F" . "")
        mu4e-headers-new-mark '("N" . "✱")
        mu4e-headers-passed-mark '("P" . "❯")
        mu4e-headers-replied-mark '("R" . "❮")
        mu4e-headers-seen-mark '("S" . "✔")
        mu4e-headers-trashed-mark '("T" . "")
        mu4e-headers-attach-mark '("a" . "")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark '("s" . "☡")
        mu4e-headers-unread-mark '("u" . "⎕")))

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


(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

