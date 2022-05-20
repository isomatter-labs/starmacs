
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
 '(package-selected-packages '(use-package)))

(put 'downcase-region 'disabled nil)

(add-to-list 'default-frame-alist '(font . "Iosevka"))
(set-face-attribute 'default nil :font "Iosevka" :height 140)
(set-face-attribute 'mode-line nil
                    :weight 'extra-bold ;should be _very_ bold
                    :font "Iosevka")       ;should be a little larger than regular text
(custom-set-faces
  '(font-lock-comment-face ((t (:foreground "#5B6268" :weight light :slant italic :family "Iosevka")))))
