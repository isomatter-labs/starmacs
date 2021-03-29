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
 '(package-selected-packages
   '(auto-package-update org helpful ivy-prescient ivy-rich company-rtags irony-server irony-eldoc flycheck-irony company-irony irony flycheck py-autopep8 haskell-mode cider lispy slime eglot magithub magit counsel ivy company-quickhelp company dumb-jump projectile olivetti mmm-mode pretty-mode rainbow-delimiters dashboard doom-modeline org-bullets doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#5B6268" :weight light :slant italic :family "Iosevka"))))
 '(whitespace-newline ((t (:foreground "#525252"))))
 '(whitespace-space ((t (:foreground "#525252"))))
 '(whitespace-tab ((t (:foreground "#E06C75")))))
(put 'downcase-region 'disabled nil)
