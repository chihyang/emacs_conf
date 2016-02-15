(server-start)
;;; load packages and customized functions
(load "~/.emacs.d/init-package-elpa.el")
(load "~/.emacs.d/init-night-mode.el")

;;; set parameters of built-in functions
(show-paren-mode 1)                     ; highlight paired brackets
(setq default-tab-width 4) ; set tab as 4 spaces
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)           ; substitue y/n for yes/no
(setq backup-by-copying nil)  ; do not copy
(setq backup-directory-alist `(("." . "~/.saves")))
(setq visible-bell 1)                   ; turn of audible belling
(global-linum-mode 1) ; enable linum-mode
(setq hs-allow-nesting t) ; hide-show
(desktop-save-mode 1)     ; save sessions
(setq url-proxy-services                ; set proxy
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "127.0.0.1:8118")
     ("https" . "127.0.0.1:8118")))
(setenv "GIT_ASKPASS" "git-gui--askpass")

;; change hot-key
(global-set-key (kbd "M-9") 'kill-whole-line) ; delete a whole line with M-9

;; custom parameter
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(markdown-command "pandoc -f markdown_github")
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
