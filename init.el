;;; init.el --- Emacs configuration

;;; Commentary:
;; This is the initial loader for the code found in README.org
;; It sets up org mode and babel, as well as some custom variables
;; and faces.  These variables and faces were added automatically by
;; packages, and are not meant to be directly edited.
;; It then loads the code in README.org

;;; Initial Install
;; This variable is used to determine if the user is installing Starmacs for the first time, which we use to dismiss the initial warnings
;; and force the display of the dashboard.
(setq starmacs/initial-install (not (file-exists-p (expand-file-name "straight/repos/straight.el" user-emacs-directory))))

;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Package Management
;; We assume that we will want packages from MELPA, the most popular emacs package registry, as well as ELPA, the official GNU repository, and MELPA Stable (which is exactly what it sounds like).
(require 'package)
(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 0)))

(add-to-list 'load-path "~/.emacs.d/modules") ; add local files

;;; Straight
;; =straight.el= is an Emacs package manager that allows users to easily and
;; efficiently install and manage Emacs packages. It provides a simple and
;; streamlined interface for managing packages, making it ideal for users who want
;; to quickly and easily add functionality to their Emacs setup. With straight.el,
;; users can easily install and update packages with just a few commands, and can
;; also configure package sources, dependencies, and version locking. This makes it
;; an essential tool for anyone who wants to customize their Emacs experience and
;; stay up-to-date with the latest packages and features. Whether you're a seasoned
;; Emacs user or just getting started, straight.el is a powerful and flexible
;; package manager that can help you get the most out of your Emacs setup.
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

;; Integrate with =use-package=
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; load org so that babel will work, custom org configuration is in README.org
(use-package org
  :ensure t)

(org-babel-load-file
 (expand-file-name "README.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "000ae191922c662e7f89eae84932415f9d7f5c3045b167b3375c2ad9b62a0c78" "4f7b78f1db71645999a8d632fe1d5cf863750a75b2153d429e59051827e439f2" "f40d0dc5fd64fef08959e2f5a35050baeb98faef572c233b7dcc3f89f0feed69" "574167ab321bb3041545e414a466cb30c48ec41d4ec27593c58be78d837575cc" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "0e83cec64ea5e9d63769fd8644936d367f624f83d7cd5310c949f74b8975d305" default))
 '(org-agenda-files nil)
 '(org-modern-block-fringe t)
 '(warning-suppress-types
   '(((defvaralias losing-value org-tab-first-hook))
	 (use-package)
	 (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t)))
 '(treemacs-fringe-indicator-face ((t (:inherit font-lock-doc-face))))
 '(treemacs-git-ignored-face ((t (:inherit (shadow)))))
 '(whitespace-newline ((t (:foreground "#525252"))))
 '(whitespace-tab ((t (:foreground "#525252")))))
(put 'downcase-region 'disabled nil)
(provide 'init)
;;; init.el ends here
