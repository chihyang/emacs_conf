;;; package --- Summary
;;; Commentary:
;;; load packages and customized functions
;;; Code:

;; load newer bytecodes
(setq load-prefer-newer t)
(require 'package)

(server-start)

;;; encoding system
(prefer-coding-system 'utf-8)           ; set default encoding as utf-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(if (eq system-type 'windows-nt)
    (setq file-name-coding-system 'gbk))
(if (boundp 'buffer-file-coding-system)             ; backward compatibility as
    (setq-default buffer-file-coding-system 'utf-8) ; default-buffer-file-coding-system
  (setq default-buffer-file-coding-system 'utf-8))  ; is deprecated in 23.2.
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq delete-by-moving-to-trash t)      ; delete files by moving to system trash
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)
(setq backup-by-copying nil)              ; do not copy
(setq backup-directory-alist `(("." . "~/.saves"))) ; change backup directory
(setq visible-bell 1)                     ; turn of audible belling
(fset 'yes-or-no-p 'y-or-n-p)             ; substitue y/n for yes/no

(setenv "GIT_ASKPASS" "git-gui--askpass") ; set git for pushing to github by https

(global-set-key (kbd "M-9") 'kill-whole-line)  ; delete a whole line with M-9
(global-set-key (kbd "C-x _") 'shrink-window)  ; shrink window vertically

;; bind `C-x n' to next-multiframe-window
(global-set-key (kbd "C-x n") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
;; rebind `C-x C-b' to buffer-menu
(global-unset-key "\C-x\C-b")
(global-set-key "\C-x\C-b" 'buffer-menu)

;; compile command
(defun check-or-create-obj-directory ()
  "Check if the folder `obj' exists.  If not, create it."
  (if (not (file-exists-p (concat (file-name-directory buffer-file-name) "obj")))
      (make-directory (concat (file-name-directory buffer-file-name) "obj"))))
(defun compile-single-c-program ()
  "Compile C file with `gcc'."
  (interactive)
  (check-or-create-obj-directory)
  (set (make-local-variable 'compile-command)
       (concat "gcc " buffer-file-name " -Wall -g -o "
               (file-name-directory buffer-file-name) "obj/"
               (file-name-sans-extension (file-name-nondirectory
                                          buffer-file-name))))
  (if (eq system-type 'windows-nt)
      (setq compile-command
            (concat compile-command ".exe")))
  (compile compile-command))
(defun compile-single-c++-program ()
  "Compile C++ file with `g++'."
  (interactive)
  (check-or-create-obj-directory)
  (set (make-local-variable 'compile-command)
       (concat "g++ -std=c++11 " buffer-file-name " -Wall -g -o "
               (file-name-directory buffer-file-name) "obj/"
               (file-name-sans-extension (file-name-nondirectory
                                          buffer-file-name))))
  (if (eq system-type 'windows-nt)
      (setq compile-command
            (concat compile-command ".exe")))
  (compile compile-command))
(defun compile-single-java-program ()
  "Compile Java file with `javac'."
  (interactive)
  (check-or-create-obj-directory)
  (set (make-local-variable 'compile-command)
       (concat "javac " buffer-file-name " -d "
               (file-name-directory buffer-file-name) "obj/"))
  (compile compile-command))

;; execution command
(defun execute-c-c++-program ()
  "Execute c/c++ program, just for practice purpose."
  (interactive)
  (defvar execute-c-c++-program-run)
  (setq execute-c-c++-program-run
        (concat (file-name-directory buffer-file-name) "obj/"
                (file-name-sans-extension
                 (file-name-nondirectory buffer-file-name))))
  (if (string-equal system-type "windows-nt")
      (setq execute-c-c++-program-run
            (concat execute-c-c++-program-run
                    ".exe &")))
  (shell-command execute-c-c++-program-run
                 "*Async Shell Command*")
  (next-multiframe-window)
  (switch-to-buffer "*Async Shell Command*"))
(defun execute-java-program ()
  "Execute Java class, just for practice purpose."
  (interactive)
  (set (make-local-variable 'execute-java-program-prefix)
       (concat (file-name-directory buffer-file-name) "obj/"))
  (defvar execute-java-program-run nil
    "Executable java class path.")
  (if (file-exists-p (concat execute-java-program-prefix "Main.class"))
      (setq execute-java-program-run
            (concat "java -cp " execute-java-program-prefix " Main"))
    (setq execute-java-program-run
          (concat "java -cp "
                  execute-java-program-prefix " "
                  (file-name-sans-extension
                   (file-name-nondirectory buffer-file-name)))))
  (shell-command execute-java-program-run))

;; key-binding
(defun bind-c-exercise-key ()
  "C mode compile key setting."
  (local-set-key [C-f5]  #'compile-single-c-program)
  (local-set-key [C-f1]  #'execute-c-c++-program))
(defun bind-c++-exercise-key ()
  "C++ mode compile key setting."
  (local-set-key [C-f5]  #'compile-single-c++-program)
  (local-set-key [C-f1]  #'execute-c-c++-program))
(defun bind-java-exercise-key ()
  "Java mode compile key setting."
  (local-set-key [C-f5]  #'compile-single-java-program)
  (local-set-key [C-f1]  #'execute-java-program))
;; C
(add-hook 'c-mode-hook
          (lambda ()
            (bind-c-exercise-key)))
;; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook
          (lambda ()
            (bind-c++-exercise-key)))
;; Java
(add-to-list 'auto-mode-alist '(".\\(aidl\\)" . idl-mode))
(add-hook 'java-mode-hook
          (lambda ()
            (bind-java-exercise-key)))

;; comment line or region with the shortcut `C-q'
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(defun comment-shortcut-for-coding-mode-hook ()
  "Comment/uncomment a line or a region hook."
  (local-set-key (kbd "C-q") 'comment-or-uncomment-line-or-region))
(dolist (hook '(prog-mode-hook
                latex-mode-hook
                tex-mode-hook
                plain-TeX-mode-hook
                ))
  (add-hook hook 'comment-shortcut-for-coding-mode-hook))

;;; cedet implementation
;; cedet
(require 'cedet)
(require 'semantic)
(add-to-list 'semantic-inhibit-functions
             (lambda () (not (member major-mode '(c-mode c++-mode)))))
(semantic-mode 1)

;; semantic/ia
(require 'semantic/ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-highlight-func-mode 1)
(global-set-key [f12] 'semantic-ia-fast-jump)

;; semantic/db-global
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; ede
(global-ede-mode 1)                      ; Enable the Project management system
(ede-enable-generic-projects)

(load "~/.emacs.d/init-package-builtin.el")
(load "~/.emacs.d/init-package-elpa.el")
(load "~/.emacs.d/easy-copy.el")
(load "~/.emacs.d/hide-show-mixed-eol.el")
(load "~/.emacs.d/night-mode.el")

;; custom parameter
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile-name "profile1" t)
 '(column-number-mode t)
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
