;;; package --- Summary
;;; Commentary:
;;; load packages and customized functions
;;; Code:
(setq gc-cons-threshold (* 50 1024 1024))

(defconst package-elpa-sources
  '((standard-dev . (("gnu"   . "http://elpa.gnu.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")))
    (standard-statble . (("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://stable.melpa.org/packages/")))
    (tsinghua . (("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                 ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                 ("emacswiki"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")))))
(defun package-available-elpa-sources ()
  (defun get-all-keys (alst)
    (if (null alst)
        '()
      (cons (caar alst)
            (get-all-keys (cdr alst)))))
  (get-all-keys package-elpa-sources))
(defun package-switch-elps-sources (elpa)
  "Switch between different `ELPA' sources."
  (interactive
   (list
    (intern (completing-read
             "Select from available sources: "
             (mapcar 'symbol-name
                     (package-available-elpa-sources))))))
  (setq package-archives (alist-get elpa package-elpa-sources)))

(fset 'yes-or-no-p 'y-or-n-p)             ; substitue y/n for yes/no
(require 'package)
;;; Standard package repositories
(setq package-archives (alist-get 'tsinghua package-elpa-sources))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq load-prefer-newer t)

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

(when (< emacs-major-version 26)
  ;; set git for pushing to github by https
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(when (> emacs-major-version 24)
  (setq jit-lock-defer-time 0)
  (setq fast-but-imprecise-scrolling t))

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(global-set-key (kbd "M-9") 'kill-whole-line)  ; delete a whole line with M-9
(global-set-key (kbd "C-x _") 'shrink-window)  ; shrink window vertically
(global-set-key (kbd "C-x n") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-unset-key "\C-x\C-b")           ; rebind `C-x C-b' to ibuffer
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-\M-^" 'scroll-other-window-down)

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

;;; Set highlight face for c/c++ printf format string
;;; from: https://gustafwaldemarson.com/posts/printf-format-highlighting-in-emacs/
(defface font-lock-format-specifier-face
  '((t . (:inherit font-lock-regexp-grouping-backslash
         :foreground "OrangeRed1")))
  "Font-lock face used to highlight printf format specifiers."
  :group 'font-lock-faces)

(defvar printf-fmt-regexp
  (concat "\\(%"
          "\\([[:digit:]]+\\$\\)?"   ; Posix argument position extension.
          "[-+' #0*]*"
          "\\(?:[[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)"
          "\\(?:\\.\\(?:[[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?"
          "\\(?:[hlLjzt]\\|ll\\|hh\\)?"
          "\\(?:[aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)")
  "Regular expression to capture all possible `printf' formats in C/C++.")

(defun printf-fmt-matcher (end)
  "Search for `printf' format specifiers within strings up to END."
  (let ((pos)
        (case-fold-search nil))
    (while (and (setq pos (re-search-forward printf-fmt-regexp end t))
                (null (nth 3 (syntax-ppss pos)))))
    pos))

(defun hightlight-c-format-string ()
  "Setup common utilities for all C-like modes."
  (font-lock-add-keywords
   nil
   '((printf-fmt-matcher (0 'font-lock-format-specifier-face prepend)))))

(add-hook 'c-mode-common-hook #'hightlight-c-format-string)

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

;; Set transparency of emacs
(defun set-transparency (value)
  "Set the transparency of the frame window to VALUE.
0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Open file's current directory in a platform-independent way
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

;;; tool for minify a JSON
;;; https://www.accidentalrebel.com/posts/minifying-buffer-contents-in-emacs.html
(defun minify-json()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

;;; load other configurations
(when (file-exists-p "~/.emacs.d/init-package-builtin.el")
  (load "~/.emacs.d/init-package-builtin.el"))
(when (file-exists-p "~/.emacs.d/init-package-elpa.el")
  (load "~/.emacs.d/init-package-elpa.el"))
(when (file-exists-p "~/.emacs.d/init-package-manual.el")
  (load "~/.emacs.d/init-package-manual.el"))
;;; load tool functions
(let ((tools (directory-files "~/.emacs.d/tools" t "\\.el$")))
  (dolist (tool tools)
    (load tool)))
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
(setq gc-cons-threshold (* 2 1024 1024))
