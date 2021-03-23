;;; My additions to Prelude

;; Hugo Exporter
(prelude-require-packages '(ox-hugo))

(with-eval-after-load 'ox
  (require 'ox-hugo)
  (add-hook 'org-mode-hook '(lambda()
                              (visual-line-mode)
                              (toggle-word-wrap)
                              )))

;; Disable right-opt modifications
(setq ns-right-option-modifier 'none)

;; Configure yasnippet snippets
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(prelude-require-package 'yasnippet-snippets)

;; K8s snippets
(prelude-require-package 'k8s-mode)

;; Ansible
(prelude-require-packages '(ansible ansible-vault))

;; Theming
(set-face-attribute 'default nil :family "Consolas for Powerline" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro for Powerline")
(set-face-attribute 'variable-pitch nil :family "Palatino Regular")

;; Make column highlighting wider
;; whitespace-mode
;; free of trailing whitespace and to use 80-column width, standard indentation
(setq whitespace-style '(face trailing lines space-before-tab
                          indentation space-after-tab)
  whitespace-line-column 120)

;; MacOS title bar
(add-to-list
  'default-frame-alist'(ns-transparent-titlebar . t))
(add-to-list
  'default-frame-alist'(ns-appearance . light))

;; (prelude-require-package 'poet-theme)
;; (load-theme 'poet-dark)

;; Reset line spacing - Poet makes things too wide
;; (setq line-spacing 0.1)
;; End Poet theme

;; Leuven
(prelude-require-package 'leuven-theme)

;; to disable headline scaling uncomment this
;; (setq leuven-scale-outline-headlines nil)
;; (setq leuven-scale-org-agenda-structure nil)

(load-theme 'leuven t)
;; End Leuven

;; Package management
(prelude-require-package 'paradox)

;; Install org-roam for Zettels
;; Ensure Org is up to date
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(prelude-require-package 'org-roam)

(setq org-roam-directory "~/Documents/org")
(add-hook 'after-init-hook 'org-roam-mode)

;; Snippets
(yas-global-mode 1)


;; (add-hook 'k8s-mode 'yas-minor-mode)
