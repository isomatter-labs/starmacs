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

(require 'org)
(org-babel-load-file
 (expand-file-name "README.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(treemacs-git-ignored-face ((t (:inherit (shadow))))))
(put 'downcase-region 'disabled nil)
(provide 'init)
;;; init.el ends here
