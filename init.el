;;; package --- Summary
;;; Commentary:
;;; load packages and customized functions
;;; Code:

(load "~/.emacs.d/init-package-elpa.el")
(load "~/.emacs.d/init-night-mode.el")

(server-start)

;;; encoding system
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

;;; set parameters of built-in functions
(show-paren-mode 1)                       ; highlight paired brackets
(setq default-tab-width 4)                ; set tab as 4 spaces
(setq-default c-basic-offset 4)           ; set indentation for cc mode
(setq c-default-style "linux"
      c-basic-offset 4)                   ; set tab width as four spaces
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)             ; substitue y/n for yes/no
(setq backup-by-copying nil)              ; do not copy
(setq backup-directory-alist
      `(("." . "~/.saves")))              ; change backup directory
(setq visible-bell 1)                     ; turn of audible belling
(global-linum-mode 1)                     ; enable linum-mode
(setq hs-allow-nesting t)                 ; hide-show
(setenv "GIT_ASKPASS" "git-gui--askpass") ; set git for pushing to github by https
(delete-selection-mode 1)                 ; delete selection mode
(setq column-number-mode t)               ; enable column-number-mode
(setq compilation-scroll-output t)        ; auto-scroll the compilation buffer

(add-hook 'foo-mode-hook
          (lambda () (interactive) (column-marker-1 80))) ; Highlight column 80 in foo mode

;; change hot-key
(global-set-key (kbd "M-9") 'kill-whole-line)  ; delete a whole line with M-9

;; auto-fill
(global-set-key (kbd "C-c q") 'auto-fill-mode) ; auto-fill mode by C-c q
(setq-default fill-column 80)                  ;set auto-fill at 80
(setq comment-auto-fill-only-comments t)
(dolist (hook '(text-mode-hook
                prog-mode-hook))
  (add-hook hook 'turn-on-auto-fill))


;; compile command
(defun compile-program ()
  (interactive)
  (cond ((string-equal (file-name-extension buffer-file-name) "c")
         (set (make-local-variable 'compile-command)
                 (concat "gcc " buffer-file-name " -Wall -g -o "
                         (file-name-directory buffer-file-name) "/obj/"
                         (file-name-sans-extension (file-name-nondirectory
                                                    buffer-file-name))
                         ".exe")))
        ((string-equal (file-name-extension buffer-file-name) "cpp")
         (set (make-local-variable 'compile-command)
              (concat "g++ -std=c++11 " buffer-file-name " -Wall -g -o "
                      (file-name-directory buffer-file-name) "obj/"
                      (file-name-sans-extension (file-name-nondirectory
                                                 buffer-file-name))
                      ".exe")))
        ((string-equal (file-name-extension buffer-file-name) "java")
         (set (make-local-variable 'compile-command)
              (concat "javac " buffer-file-name " -d "
                      (file-name-directory buffer-file-name) "obj/"))))
  (compile compile-command))
;; execution command
(defun execute-program ()
  (interactive)
  (defvar run)
  (if (string-equal (file-name-extension buffer-file-name) "java")
      (setq run (concat "java -cp "
                        (file-name-directory buffer-file-name) "obj "
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
    (setq run (concat (file-name-directory buffer-file-name) "obj/"
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))
                      ".exe")))
  (shell-command run))
(defun bind-exercise-key ()
  (when (string-match "[Ee]xer*" buffer-file-name)
    (local-set-key [C-f5]  #'compile-program)
    (local-set-key [C-f1]  #'execute-program)))
;; C
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-hook
          (lambda ()
            (bind-exercise-key)))
;; C++
(add-hook 'c++-mode-hook
          (lambda ()
            (bind-exercise-key)))
;; Java
(add-to-list 'auto-mode-alist '(".\\(aidl\\)" . idl-mode))
(add-hook 'java-mode-hook
          (lambda ()
            (bind-exercise-key)))
;; CC-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq ac-sources (append '(ac-source-semantic) ac-sources))))
(setq gdb-show 1)

;; comment line or region with the shortcut `C-q'
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(defun comment-shortcut-for-coding-mode-hook ()
  (local-set-key (kbd "C-q") 'comment-or-uncomment-line-or-region))
(dolist (hook '(prog-mode-hook
                latex-mode-hook
                tex-mode-hook
                plain-TeX-mode-hook
                ))
  (add-hook hook 'comment-shortcut-for-coding-mode-hook))

;;; cedet implementation
;; semantic
(semantic-mode 1)
(global-semanticdb-minor-mode)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-set-key [f12] 'semantic-ia-fast-jump)

;; ede
(global-ede-mode 1)                      ; Enable the Project management system
(ede-enable-generic-projects)

;; recentf-mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; hl-line-mode
(global-hl-line-mode)
(if  (not (window-system))
    (set-face-background 'hl-line "brightblack"))

;; copy a word and a line with shortcut
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
         (lambda()
           (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))
    ))

;; copy a word
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

;; copy a line
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  ;; (paste-to-mark arg)
  )

;; Key binding
(global-set-key (kbd "C-c w")         (quote copy-word))
(global-set-key (kbd "C-c l")         (quote copy-line))

;; display-time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode t)

;; configuration mode
(add-to-list 'auto-mode-alist '("\\.rc\\'" . conf-mode))

;; bind `C-x n' to next-multiframe-window
(global-set-key (kbd "C-x n") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

;; tramp-mode
(require 'tramp)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

;; delete files by moving to system trash
(setq delete-by-moving-to-trash t)

;; hide mixed line ending
(defun hide-mixed-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
;; show mixed line ending
(defun show-mixed-eol ()
  "Show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table nil))

;; read-only-mode hot key
(global-set-key (kbd "C-c C-r") 'read-only-mode)

;; org-mode
(setq org-startup-indented t)           ; enable auto-indent
(setq org-src-fontify-natively t)       ; enable source code highlight

;; custom parameter
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile-name "profile1" t)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(ecb-options-version "2.40")
 '(markdown-command "pandoc -f markdown_github")
 '(scheme-program-name "petite")
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-default-width 100)
 '(sr-speedbar-max-width 100))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
