;;; init.el --- Emacs configuration

;;; Commentary:
;; This is the initial loader for the code found in README.org
;; It sets up org mode and babel, as well as some custom variables
;; and faces.  These variables and faces were added automatically by
;; packages, and are not meant to be directly edited.
;; It then loads the code in README.org

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
 '(connection-local-criteria-alist
   '(((:application eshell)
	  eshell-connection-default-profile)
	 ((:application tramp :machine "localhost")
	  tramp-connection-local-darwin-ps-profile)
	 ((:application tramp :machine "starlight.lan")
	  tramp-connection-local-darwin-ps-profile)
	 ((:application tramp)
	  tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
	  (eshell-path-env-list))
	 (tramp-connection-local-darwin-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (euid . number)
	   (user . string)
	   (egid . number)
	   (comm . 52)
	   (state . 5)
	   (ppid . number)
	   (pgrp . number)
	   (sess . number)
	   (ttname . string)
	   (tpgid . number)
	   (minflt . number)
	   (majflt . number)
	   (time . tramp-ps-time)
	   (pri . number)
	   (nice . number)
	   (vsize . number)
	   (rss . number)
	   (etime . tramp-ps-time)
	   (pcpu . number)
	   (pmem . number)
	   (args)))
	 (tramp-connection-local-busybox-ps-profile
	  (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (user . string)
	   (group . string)
	   (comm . 52)
	   (state . 5)
	   (ppid . number)
	   (pgrp . number)
	   (ttname . string)
	   (time . tramp-ps-time)
	   (nice . number)
	   (etime . tramp-ps-time)
	   (args)))
	 (tramp-connection-local-bsd-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (euid . number)
	   (user . string)
	   (egid . number)
	   (group . string)
	   (comm . 52)
	   (state . string)
	   (ppid . number)
	   (pgrp . number)
	   (sess . number)
	   (ttname . string)
	   (tpgid . number)
	   (minflt . number)
	   (majflt . number)
	   (time . tramp-ps-time)
	   (pri . number)
	   (nice . number)
	   (vsize . number)
	   (rss . number)
	   (etime . number)
	   (pcpu . number)
	   (pmem . number)
	   (args)))
	 (tramp-connection-local-default-shell-profile
	  (shell-file-name . "/bin/sh")
	  (shell-command-switch . "-c"))
	 (tramp-connection-local-default-system-profile
	  (path-separator . ":")
	  (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("4f7b78f1db71645999a8d632fe1d5cf863750a75b2153d429e59051827e439f2" "f40d0dc5fd64fef08959e2f5a35050baeb98faef572c233b7dcc3f89f0feed69" "574167ab321bb3041545e414a466cb30c48ec41d4ec27593c58be78d837575cc" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "0e83cec64ea5e9d63769fd8644936d367f624f83d7cd5310c949f74b8975d305" default))
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
