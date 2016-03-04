;;; load packages and customized functions
(load "~/.emacs.d/init-package-elpa.el")
(load "~/.emacs.d/init-night-mode.el")

(server-start)
;;; set parameters of built-in functions
(show-paren-mode 1)                     ; highlight paired brackets
(setq default-tab-width 4) ; set tab as 4 spaces
(setq-default c-basic-offset 4)         ; set indentation for cc mode
(setq c-default-style "linux"
          c-basic-offset 4)             ; set tab width as four spaces
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)           ; substitue y/n for yes/no
(setq backup-by-copying nil)  ; do not copy
(setq backup-directory-alist `(("." . "~/.saves")))
(setq visible-bell 1)                   ; turn of audible belling
(global-linum-mode 1) ; enable linum-mode
(setq hs-allow-nesting t) ; hide-show
(desktop-save-mode 1)     ; save sessions
(setenv "GIT_ASKPASS" "git-gui--askpass") ; set git for pushing to github by https
(delete-selection-mode 1)                 ; delete selection mode
(setq column-number-mode t)               ; enable column-number-mode
(prefer-coding-system 'utf-8)             ; set default encoding as utf-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq-default fill-column 80)                  ;set auto-fill at 80

;; change hot-key
(global-set-key (kbd "M-9") 'kill-whole-line)  ; delete a whole line with M-9
(global-set-key (kbd "C-c q") 'auto-fill-mode) ; auto-fill mode by C-c q

;; flyspell mode for text mode
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))

;; custom parameter
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile-name "profile1" t)
 '(ecb-options-version "2.40")
 '(markdown-command "pandoc -f markdown_github")
 '(session-use-package t nil (session))
 '(speedbar-show-unknown-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
