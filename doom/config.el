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
(setq doom-font (font-spec :family "Iosevka" :size 18))
(setq doom-variable-pitch-font (font-spec :family "Fira Sans" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq
  org-ellipsis "…"
   ;; ➡, ⚡, ▼, ↴, , ∞, ⬎, ⤷, ⤵
  org-agenda-files (quote ("~/Notes/todo.org" "~/Notes/appointments.org"))
  org-deadline-warning-days 7
  org-agenda-breadcrumbs-separator " ❱ "
  org-directory "~/Notes")


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
  org-icalendar-timezone "Asia/Tehran"
  ;; org-caldav-url 'google
  ;; org-caldav-calendar-id "ovuticv96133cisuc0pm8f7d6g@group.calendar.google.com"
  ;; org-caldav-files '("~/Notes/appointments.org")
  ;; org-caldav-oauth2-client-id "279358326453-ar2bfnerndjnnie90e59i9otuif9ut84.apps.googleusercontent.com"
  ;; org-caldav-oauth2-client-secret "SECRET"
  ;; org-caldav-inbox "~/Notes/calendar-inbox.org"
  org-hide-emphasis-markers t
  org-fontify-done-headline t
  org-fontify-whole-heading-line t
  org-fontify-quote-and-verse-blocks t
  )


(customize-set-value
    'org-agenda-category-icon-alist
    `(
      ("work" "~/Downloads/money-bag(1).svg" nil nil :ascent center)
      ("chore" "~/Downloads/loop(1).svg" nil nil :ascent center)
      ("events" "~/Downloads/calendar(1).svg" nil nil :ascent center)
      ("todo" "~/Downloads/checklist(1).svg" nil nil :ascent center)
      ("walk" "~/Downloads/walk(1).svg" nil nil :ascent center)
      ("solution" "~/Downloads/solution(1).svg" nil nil :ascent center)
      ))


;; (add-hook 'org-agenda-finalize-hook
;;           #'eh-org-agenda-change-breadcrumbs-color)

;; (defun eh-org-agenda-change-breadcrumbs-color ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward org-agenda-breadcrumbs-separator nil t)
;;       (put-text-property (match-beginning 0) (match-end 0)
;;                          'face '(:foreground "grey" :bold t)))))

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
         (format "%-2s. %2d %s, %s"
            dayname day monthname persian)))

(setq org-agenda-custom-commands
      '(("o" "My Agenda"
         ((todo "TODO" (
                      (org-agenda-overriding-header "\n⚡ Do Today:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format " %-2i %-15b")
                      (org-agenda-todo-keyword-format "")
                       ))
          (agenda "" (
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 5)
                      (org-agenda-overriding-header "⚡ Schedule:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format   "  %-3i  %-15b %t%s")
                      (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-time-grid (quote ((daily today remove-match)
                                                    (0900 1200 1500 1800 2100)
                                                    "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                       ))
          ))))

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


(add-hook! '(org-clock-out-hook org-clock-in-hook) #'org-save-all-org-buffers)
(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)
(defun my-org-mode-autosave-settings ()
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))
(add-hook 'org-mode-hook 'my-org-mode-autosave-settings)


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

(set-fontset-font
 "fontset-default"
 (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
 ;; "Vazir Code-13")
 "Shabnam-12")

;; (after! org-mode
;;   (set-company-backend! 'company-dabbrev)
;;   )
;;

;; (add-hook 'text-mode-hook
;;            (lambda ()
;;             (variable-pitch-mode 1)))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha . (97 . 95)))

(setq org-journal-enable-agenda-integration t)

(global-activity-watch-mode)
