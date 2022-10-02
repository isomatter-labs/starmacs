
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" default))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(why-this which-key visual-fill-column typescript-mode treemacs-projectile treemacs-magit tree-sitter-langs solaire-mode sbt-mode rainbow-delimiters quelpa-use-package py-autopep8 prettier-js org-bullets mmm-mode magithub lsp-ui lsp-metals lsp-julia lsp-ivy julia-snail ivy-rich helpful general forge flycheck doom-themes doom-modeline discover diff-hl dashboard company-box command-log-mode auto-package-update all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t)))
 '(treemacs-fringe-indicator-face ((t (:inherit font-lock-doc-face))))
 '(treemacs-git-ignored-face ((t (:inherit (shadow)))))
 '(whitespace-newline ((t (:foreground "#525252"))))
 '(whitespace-space ((t (:foreground "#525252"))))
 '(whitespace-tab ((t (:foreground "#E06C75")))))
(put 'downcase-region 'disabled nil)
(provide 'init)
;;; init.el ends here
