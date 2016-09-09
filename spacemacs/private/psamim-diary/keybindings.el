(require 'parse-time)

(setq psamim-diary-diary-directory "/home/samim/Notes/daily/")

(defun psamim-diary-next-day-string (current-day diff)
  (format-time-string "%Y-%m-%d"
     (time-add
      (apply (function encode-time)
             `(0 0 0
                 ,(nth-value 3 (parse-time-string current-day))
                 ,(nth-value 4 (parse-time-string current-day))
                 ,(nth-value 5 (parse-time-string current-day))
                 0 t 0))
      (days-to-time diff))))

(defun psamim-diary-next-file (file-name diff)
  (concat
   psamim-diary-diary-directory
   (psamim-diary-next-day-string (file-name-base file-name) diff)
   ".org"))

(defun psamim-diary-open (file)
   (interactive)
   (if (file-exists-p file)
       (let ((prev-buffer (current-buffer)))
         (find-file file)
         (kill-buffer prev-buffer))
     (message "Not found")))

(defun psamim-diary-show-next ()
  (interactive)
  (let ((count 1)
        (next-file (psamim-diary-next-file (buffer-file-name) 1)))
  (while (and (< count 1000) (not (file-exists-p next-file)))
    (setq count (1+ count))
    (setq next-file (psamim-diary-next-file (buffer-file-name) count)))
  (psamim-diary-open next-file)))

(defun psamim-diary-show-prev ()
  (interactive)
  (let ((count -1)
        (next-file (psamim-diary-next-file (buffer-file-name) -1)))
    (while (and (> count -1000) (not (file-exists-p next-file)))
      (setq count (1- count))
      (setq next-file (psamim-diary-next-file (buffer-file-name) count)))
    (psamim-diary-open next-file)))

(spacemacs|define-micro-state psamim-diary
  :doc "[p] [N] previous [n] next [q] quit"
  :evil-leader "aa"
  :persistent t
  :bindings
  ("n" psamim-diary-show-next)
  ("p" psamim-diary-show-prev)
  ("q" nil :exit t))
