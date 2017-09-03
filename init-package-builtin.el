;;; init-package-builtin --- Configuration for built-in Emacs packages

;;; Commentary:

;;; Load built-in Emacs packages.  Configurations are sorted alphabetically.

;;; Code:

;; auto-fill
(global-set-key (kbd "C-c q") 'auto-fill-mode) ; auto-fill mode by C-c q
(setq-default fill-column 80)                  ;set auto-fill at 80
(setq comment-auto-fill-only-comments t)
(dolist (hook '(text-mode-hook
                prog-mode-hook))
  (add-hook hook 'turn-on-auto-fill))
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))
(global-set-key (kbd "C-c M-q") 'unfill-paragraph)

;; auto-revert-mode
(require 'autorevert)

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
(global-set-key [f12] 'semantic-ia-fast-jump)

;; semantic/db-global
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; cc-mode
(require 'cc-mode)
(setq-default c-basic-offset 4)         ; set indentation for cc mode
(setq-default tab-width 4)              ; set tab as 4 spaces
(setq c-default-style "bsd")
(setq c-basic-offset 4)
(c-set-offset 'case-label '+)
;;; Seems `c-toggle-auto-newline' takes more trouble than convenience, so it is
;;; disabled.
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (if (derived-mode-p 'c-mode 'c++-mode)
;;                 (c-toggle-auto-newline))
;;             (local-set-key (kbd "RET") 'newline-and-indent)))

;; column-number-mode
(column-number-mode t)                  ; enable column-number-mode

;; compile
(require 'compile)
(setq compilation-scroll-output t)      ; auto-scroll the compilation buffer

;; conf-mode
(add-to-list 'auto-mode-alist '("\\.rc\\'" . conf-mode))

;; delete-selection-mode
(delete-selection-mode 1)

;; desktop
(use-package desktop
  :defer t
  :init
  (setq desktop-load-locked-desktop t)
  (setq desktop-files-not-to-save "^$")     ; reload tramp path
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))
  :config
  (unless (daemonp)
    (eval-after-load "init.el"
      (desktop-save-mode))))

;; dried omit mode
(require 'dired-x)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; ede
(global-ede-mode 1)                      ; Enable the Project management system
(ede-enable-generic-projects)

;; ediff
(setq ediff-diff-options "-w")

;; electric-pair-mode
(if (not (version< emacs-version "24.4"))
    (electric-pair-mode 1))

;; hide-show-mode
(require 'hideshow)
(setq hs-allow-nesting t)
(dolist (hook '(prog-mode-hook))
  (add-hook hook 'hs-minor-mode))

;; hl-line-mode
(global-hl-line-mode)
(if  (not (window-system))
    (set-face-background 'hl-line "brightblack"))

;; html-mode
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))
(add-hook 'sgml-mode-hook
          (lambda ()
            ;; Default indentation to 4, but let SGML mode guess, too.
            (set (make-local-variable 'sgml-basic-offset) 4)
            (sgml-guess-indent)))

;; linum-mode
(require 'linum)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins
   win
   (ceiling (* (if (boundp 'text-scale-mode-step)
                   (expt text-scale-mode-step
                         text-scale-mode-amount)
                 1)
               (if (car (window-margins))
                   (car (window-margins)) 1)
               ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)
(defun linum-format-func (line)
  "Add padding for line number in terminal mode"
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(when (not (display-graphic-p))
  (setq linum-format 'linum-format-func))
(global-linum-mode 1)

;; Man-mode
(require 'man)
(add-hook 'Man-mode-hook 'visual-line-mode)
(when (eq (face-attribute 'Man-overstrike :foreground) 'unspecified)
    (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t))
(when (eq (face-attribute 'Man-overstrike :foreground) 'unspecified)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

;; octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; org-mode
(require 'org)
(setq org-startup-indented t)           ; enable auto-indent
(setq org-src-fontify-natively t)       ; enable source code highlight
(setq org-src-tab-acts-natively t)      ; enable tab indent
(setq org-src-preserve-indentation t)   ; preserve indentation, no padding
                                        ; blanks
(setq org-src-strip-leading-and-trailing-blank-lines t)
(setq org-src-window-setup 'current-window)
(setq org-catch-invisible-edits 'error)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "DOING(o@/!)" "|" "DONE(d@/!)" "CANCELED(c@/!)")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :background 'unspecified)
        ("WAIT" :foreground "orange" :background 'unspecified)
        ("DOING" :foreground "blue" :background 'unspecified)
        ("CANCELED" :foreground "gray50" :background 'unspecified)
        ("DONE" :foreground "green" :background 'unspecified)))
;; remove blanks between Chinese characters
(defadvice org-html-paragraph
    (before org-html-paragraph-advice
            (paragraph contents info) activate)
  "Exclude surplus whitespace when exporting to html.
Join consecutive Chinese lines into a single long line without
unwanted space when exporting `org-mode' to html."
  (let* ((origin-contents (ad-get-arg 1))
         (fix-regexp "[[:multibyte:]]")
         (fixed-contents
          (replace-regexp-in-string
           (concat
            "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
    (ad-set-arg 1 fixed-contents)))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-ct" 'org-time-stamp)))

(require 'ob-plantuml)
(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml.jar"))
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "plantuml")))      ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; read-only-mode
(global-set-key (kbd "C-c C-r") 'read-only-mode)

;; recentf-mode
(require 'recentf)
(setq recentf-max-menu-items 25)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; set program to run scheme
(require 'scheme)
(if (eq system-type 'windows-nt)
    (setq scheme-program-name "petite"))

;; show-paren-mode
(show-paren-mode 1)                       ; highlight paired brackets

;; time
(require 'time)
(setq display-time-format "%Y/%m/%d %H:%M")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode t)

;; tool-bar-mode
(tool-bar-mode 0)

;; tramp-mode
(require 'tramp)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

;; winner-mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; which-function-mode
(which-function-mode 1)

(provide 'init-package-builtin)
;;; init-package-builtin.el ends here
