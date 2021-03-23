;;; init.el --- Summary
;;; Commentary:
;; Vanilla Emacs init.el


;; Package management
(package-initialize)
(require 'package)

;; Configure MELPA archives
(setq package-archives '(
                         ("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("elpy"      . "https://jorgenschaefer.github.io/packages/")))

;; Configure use-package
(if (not (package-installed-p 'use-package))
(progn
  (package-refresh-contents)
  (package-install 'use-package)))

(require 'use-package)

;; Install 'straight'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;; Load orgmode formatted settings.
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))



;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
