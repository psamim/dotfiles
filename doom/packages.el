;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;; (unpin! t)

;; ...but to unpin a single package:
;; (unpin! pinned-package)
;; Use it to unpin multiple packages
;; (unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;; (package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; (package! org-clock-csv)
;; (package! org-ql)
;; (package! poet-theme)
;; (package! leuven-theme)
;; (package! activity-watch-mode)
;; (package! org-pretty-tags)
(package! olivetti)
;; (package! tea-time :recipe (:host github :repo "dakra/tea-timer.el"))
;; (package! tea-time :recipe (:host github :repo "konzeptual/tea-time"))
(package! org-caldav)
;; (package! org-gcal)
(package! oauth2)
;; (package! mu4e-alert)
;; (package! modus-themes)
;; (package! web-mode)
(package! unidecode)
;; (package! org-msg)
;; (package! nroam :recipe (:host github :branch "master" :repo "NicolasPetton/nroam"))
(package! literate-calc-mode)
;; (package! org-mru-clock)
;; (package! counsel-org-clock)
(package! graphviz-dot-mode)
;; (package! org-ql)
;; (package! org-super-agenda)
;; (package! org-mind-map)
(package! solaire-mode :disable t)
(package! websocket)
;; (package! org-tidy)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
;; (disable-packages! diff-hl)
;; (package! calibredb)
;; (package! org-analyzer)
;; (package! org-inline-pdf)
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
(package! jest-test-mode)

;; (package! org-modern :recipe (:host github :repo "minad/org-modern"))
;; (setq org-xournalpp-image-type 'png)
;; (package! org-xournalpp
;;   :recipe (:host gitlab
;;            :repo "vherrmann/org-xournalpp"
;;            :files ("resources" "*.el")))
;; (package! org-krita
;;    :recipe (:host github
;;            :repo "lepisma/org-krita"
;;            :files ("resources" "resources" "*.el" "*.el")))

;; (package! ob-ledger
;;   :recipe (:host github :repo "overtone/emacs-live"
;;            :files ("packs/stable/org-pack/lib/org-mode/lisp/ob-ledger.el")))
