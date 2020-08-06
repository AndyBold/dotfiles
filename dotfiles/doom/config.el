;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Andy Bold"
      user-mail-address "abold@launchdarkly.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-font "Input Mono")
(setq doom-variable-pitch-font "Gill Sans")
(setq doom-theme 'doom-nord)

(exec-path-from-shell-copy-env "GPG_TTY")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/andyb/Google Drive File Stream/My Drive/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Load in some personal config before doing anything else
(defun ab-load-file-if-exists (&rest paths)
  "Load files when they exists."
  (dolist (file-path paths)
   (when (file-exists-p file-path)
    (load file-path))))

(ab-load-file-if-exists "~/.emacs-secrets.el")

;;
;; Generic Emacs things
;;
;; Keyboard shortcuts
(global-set-key (kbd "C-c =") 'er/expand-region)

;; Make sure Tempo templating is enabled.
(require 'org-tempo)
(tempo-define-template "plain-src"
                       '("#+begin_src ?\n\n" > "#+end_src")
                       "<s"
                       "Insert a generic source code template")

(tempo-define-template "lisp-block"
                       '("#+begin_src emacs-lisp :tangle yes\n?\n" > "#+end_src")
                       "<sl"
                       "Insert an embedded Lisp block for literate emacs.d")

(tempo-define-template "k8s"
                       '("#+name: k8s\n#+begin_src shell :tangle no :results output\n?\n" > "#+end_src")
                       "<sk"
                       "Insert a, non-tangled, k8s block")

(tempo-define-template "shell-code"
                       '("#+begin_src shell :tangle no :results value raw :results value drawer :session" > "\n\n#+end_src")
                       "<ss"
                       "Shell code block, with raw results")

(tempo-define-template "properties-block"
                       '(":PROPERTIES:\n" > ":END:")
                       "<PROP"
                       "Orgmode properties block")
(tempo-define-template "python-code"
                       '("#+begin_src python :tangle no :results value raw :results value drawer :session" > "\n\n#+end_src")
                       "<sp"
                       "Python code block, with raw results")
(tempo-define-template "plantuml"
                       '("#+begin_src plantuml :tangle no :results value raw :results value drawer :session" > "\n\n#+end_src")
                       "<puml"
                       "Plantuml")

;; Hook Ansi colour filtering into shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;
;; Orgmode configuration
;;

;; Add .org.gpg file suffix to the auto-mode list
(add-to-list 'auto-mode-alist '("\\.org\\.gpg" . org-mode))

;; Configure todo keywords

(setq org-todo-keywords
  '((sequence "TODO(t)" "PROJ(p)" "STRT(s!)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k!)")
    (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))

;; Keep a log of when a task was completed.
(setq org-log-into-drawer "LOGBOOK")

;; Configure fancy-priorities
(after! org-fancy-priorities
  (add-hook! org-mode . org-fancy-priorities-mode)
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

;; Refile configuration

;; Tell Agenda mode to include .org.gpg files
(unless (string-match-p "\\.gpg" org-agenda-file-regexp)
  (setq org-agenda-file-regexp
    (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
      org-agenda-file-regexp)))

;; Expand Agenda targets to include .org.gpg. Refile uses these to build it's list of places
(add-to-list 'org-agenda-files (expand-file-name "~/Documents/org/*\\.org\\.gpg"))

;; Targets include the current file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 4)
                                (org-agenda-files :maxlevel . 4)
                                )))

;; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; I haven't invested enough (any) time in properly getting to grips with
;; org-roam, so it's disabled for now.
;; org-roam
;;
;;(setq org-roam-directory "~/Documents/org/launchdarkly")
;;
;;(after! org-roam
;;        (map! :leader
;;            :prefix "n"
;;            :desc "org-roam" "l" #'org-roam
;;            :desc "org-roam-insert" "i" #'org-roam-insert
;;            :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
;;            :desc "org-roam-find-file" "f" #'org-roam-find-file
;;            :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;            :desc "org-roam-insert" "i" #'org-roam-insert
;;            :desc "org-roam-capture" "c" #'org-roam-capture))
;;
;;(require 'company-org-roam)
;;    (use-package company-org-roam
;;      :when (featurep! :completion company)
;;      :after org-roam
;;      :config
;;      (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
;;
;;(after! org-roam
;;  (setq org-roam-encrypt-files t)
;;  (setq org-roam-ref-capture-templates
;;    '(("r" "ref" plain (function org-roam-capture--get-point)
;;        "%?"
;;        :file-name "websites/${slug}.org.gpg"
;;        :head "-*- epa-file-encrypt-to: ("andy.bold@me.com") -*-
;;#+TITLE: ${title}
;;    #+ROAM_KEY: ${ref}
;;    - source :: ${ref}"
;;        :unnarrowed t)))
;;
;;  (setq org-roam-capture-templates
;;    '(("d" "default" plain (function org-roam--capture-get-point)
;;       "%?"
;;       :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
;;       :head "-*- epa-file-encrypt-to: (\"andy.bold@me.com\") -*-
;;
;;#+TITLE: ${title}\n"
;;       :unnarrowed t))))

(use-package org-journal
      :bind
      ("C-c j n" . org-journal-new-entry)
      ("C-c j <" . org-journal-open-previous-entry)
      ("C-c j >" . org-journal-open-next-entry)
  :custom

  ;; (org-journal-enable-encryption t)
  (org-journal-dir "/Users/andyb/Google Drive File Stream/My Drive/org/journal/")
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))
    (setq org-journal-enable-agenda-integration t)
    (setq org-journal-time-format "")

  (defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "-*- mode: org; fill-column: 78 -*-\n#+STARTUP: folded\n")
      (`weekly "-*- mode: org; fill-column: 78 -*-\n#+STARTUP: folded\n")
      (`monthly "-*- mode: org; fill-column: 78 -*-\n#+STARTUP: folded\n")
      (`yearly "-*- mode: org; fill-column: 78 -*-\n#+STARTUP: folded\n"))))

  (setq org-journal-file-header 'org-journal-file-header-func)

(add-hook 'dired-mode-hook 'org-download-enable)

(after! org-journal
  (org-download-enable)
  (setq org-download-method "directory"
    org-download-heading-lvl nil))

;;
;; Restclient babel for Orgmode
;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))


;;;
;;; Pretty Magit
;;;
(require 'dash)

(defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
  "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
  `(prog1
     (add-to-list 'pretty-magit-alist
                  (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                        ,ICON ',PROPS))
     (unless ,NO-PROMPT?
       (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

(setq pretty-magit-alist nil)
(setq pretty-magit-prompt nil)
(pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
(pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
(pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
(pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
(pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
(pretty-magit "WIP"    ?🚧 (:foreground "#3F681C" :height 1.2))
(pretty-magit "master"  ? (:box t :height 1.2) t)
(pretty-magit "origin"  ? (:box t :height 1.2) t)

(defun add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let (((rgx icon props) it))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (compose-region
             (match-beginning 1) (match-end 1) icon)
            (when props
              (add-face-text-property
               (match-beginning 1) (match-end 1) props))))))))

(advice-add 'magit-status :after 'add-magit-faces)
(advice-add 'magit-refresh-buffer :after 'add-magit-faces)

;;;
;;; Configure Ivy to prompt for a leader
;;;

(setq use-magit-commit-prompt-p nil)
(defun use-magit-commit-prompt (&rest args)
  (setq use-magit-commit-prompt-p t))

(defun magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."
  (interactive)
  (when use-magit-commit-prompt-p
    (setq use-magit-commit-prompt-p nil)
    (insert (ivy-read "Commit Type " pretty-magit-prompt
                      :require-match t :sort t :preselect "Add: "))
    ;; Or if you are using Helm...
    ;; (insert (helm :sources (helm-build-sync-source "Commit Type "
    ;;                          :candidates pretty-magit-prompt)
    ;;               :buffer "*magit cmt prompt*"))
    ;; I haven't tested this but should be simple to get the same behaior
    (add-magit-faces)
    (evil-insert 1)  ; If you use evil
    ))

(remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
(add-hook 'git-commit-setup-hook 'magit-commit-prompt)
(advice-add 'magit-commit :after 'use-magit-commit-prompt)

;;;
;;; END pretty magit
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(git-auto-commit-mode))
 '(paradox-github-token "{{@@ env['personal_github_token'] @@}}"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
