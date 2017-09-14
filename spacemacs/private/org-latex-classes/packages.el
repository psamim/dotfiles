(setq org-latex-classes-packages
      '(
        ;; org is installed by `org-plus-contrib'
        (org :location built-in)
        (ox-latex :location local)
        ))

(defun org-latex-classes/init-ox-latex ()
  (use-package ox-latex
    :config
    (progn
      ;; Tell the latex export to use the minted package for source
      ;; code coloration.
      (add-to-list 'org-latex-packages-alist '("" "minted" nil))
      (setq org-latex-listings 'minted)

      ;; Let the exporter use the -shell-escape option to let latex
      ;; execute external programs.
      ;; This obviously and can be dangerous to activate!
      (setq org-latex-pdf-process
            (quote ("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "biber %b"
                    "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

      (setq org-latex-minted-options
            '(("frame" "leftline")
              ("fontsize" "\\scriptsize")
              ("bgcolor" "bg")
              ("stepnumber" "2")
              ("mathescape" "true")
              ("linenos" "true")))
      )
    ))

(defun org-latex-classes/post-init-org ()
  (progn
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))

    (add-to-list 'org-latex-classes
                 '("assignment"
                   "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{xcolor}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\BeforeBeginEnvironment{minted}{\\vspace{3mm}}
\\AfterEndEnvironment{minted}{\\vspace{3mm}}
\\hypersetup{pdfborder=0 0 0}
\\usepackage[labelsep=quad,labelformat=simple]{caption}
\\usepackage{xepersian}
\\settextfont{XB Yas}
\\setmainfont[Script=Arabic,Numbers=Lining]{XB Yas}
\\usepackage{eukdate}
\\renewcommand{\\mkbibbrackets}[1]{]#1[}
\\setLTR"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("article-fa"
                   "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{longtable}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{float}

\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{enumerate}
\\usepackage{xcolor}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\usepackage{xepersian}
\\BeforeBeginEnvironment{minted}{\\vspace{3mm}}
\\AfterEndEnvironment{minted}{\\vspace{3mm}}
\\linespread{1.4}
\\hypersetup{pdfborder=0 0 0}
\\settextfont{XB Yas}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("assignment-unno"
                   "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{xcolor}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\BeforeBeginEnvironment{minted}{\\vspace{3mm}}
\\AfterEndEnvironment{minted}{\\vspace{3mm}}
\\hypersetup{pdfborder=0 0 0}"
                   ("\\section*{%s}" . "\\section{%s}")
                   ("\\subsection*{%s}" . "\\subsection{%s}")
                   ("\\subsubsection*{%s}" . "\\subsubsection{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))
    ))
