(defun psamim-write-room ()
  (interactive)
  (writeroom-mode)
  (visual-line-mode)
  (spacemacs/toggle-vi-tilde-fringe-off)
  (spacemacs/toggle-line-numbers-off))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun psamim-journal-prefix (time)
  (let*
      ((diary-in-a-while "5") ;; Fridays
       (diary-template "/home/samim/Notes/archive/template.org")
       (diary-template-in-a-while "/home/samim/Notes/archive/template-in-a-while.org")
       (decodedTime (decode-time time))
       (now (list (nth 4 decodedTime) (nth 3 decodedTime) (nth 5 decodedTime)))
       (template
        (if (string= (format-time-string "%w" time) diary-in-a-while)
            diary-template-in-a-while diary-template)))
    (concat
     "* "
     (format-time-string "%B %e, %Y" time) "\n"
     (format-time-string "%A" time) "\n"
     (calendar-persian-date-string now) "\n"
     (calendar-bahai-date-string now) "\n\n"
     (get-string-from-file template))))
