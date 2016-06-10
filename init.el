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
(if (eq system-type 'windows-nt)
    (setq file-name-coding-system 'gbk))
(setq-default cursor-type 'bar)
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

(dolist (hook '(markdown-mode-hook
                text-mode-hook
                latex-mode-hook
                tex-mode-hook))
  (add-hook hook 'turn-on-auto-fill))

;; compile command
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "gcc " buffer-file-name " -Wall -g -o "
                         (file-name-directory buffer-file-name) "/obj/"
                         (file-name-sans-extension (file-name-nondirectory
                                                    buffer-file-name))
                         ".exe"))))
(defun execute-c-program ()
  (interactive)
  (defvar run)
  (setq run (concat (file-name-directory buffer-file-name) "obj/"
                    (file-name-sans-extension
                     (file-name-nondirectory buffer-file-name))
                    ".exe"))
  (shell-command run))
(add-hook 'java-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "javac "
                         buffer-file-name
                         " -d "
                         (file-name-directory buffer-file-name) "obj/"))))
(global-set-key [C-f5] 'compile)
(global-set-key [C-f1] 'execute-c-program)

;; CC-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq ac-sources (append '(ac-source-semantic) ac-sources))))
(setq gdb-show 1)

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  )
(global-set-key (kbd "C-q") 'comment-or-uncomment-line-or-region)

;; cedet implementation
(semantic-mode 1)
(global-ede-mode 1)                      ; Enable the Project management system
(ede-enable-generic-projects)
(global-semantic-idle-summary-mode 1)
(global-set-key [f12] 'semantic-ia-fast-jump)

;; recentf-mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; global key
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)

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
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-default-width 100)
 '(sr-speedbar-max-width 100))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
