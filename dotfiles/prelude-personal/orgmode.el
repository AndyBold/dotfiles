;; Org mode
;; Literate programming things
(setq org-confirm-babel-evaluate nil
  org-src-fontify-natively t
  org-src-tab-acts-natively t)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((shell         . t)
     (js         . t)
     (emacs-lisp . t)
     (perl       . t)
     (clojure    . t)
     (python     . t)
     (ruby       . t)
     (plantuml   . t)))

;; Templates

;; Day book

(setq org-capture-templates
  '(("d" "daybook" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "%<%Y%m%d>-daybook"
      :head "#+TITLE: Work log %Y-%m-%d\n"
      :unarrowed t)))

;; Journal
(setq org-journal-dir "~/Documents/org/launchdarkly")
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-date-format "%A, %d %B %Y")

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(setq org-journal-file-header 'org-journal-file-header-func)

(prelude-require-package 'org-journal)
