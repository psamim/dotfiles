;; Different font for Farsi characters
(set-fontset-font
 "fontset-default"
 (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
 "B Traffic-15")

(visual-line-mode t)
