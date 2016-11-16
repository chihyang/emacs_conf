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

;; cc-mode
(require 'cc-mode)
(setq-default c-basic-offset 4)         ; set indentation for cc mode
(setq-default tab-width 4)              ; set tab as 4 spaces
(setq c-default-style "bsd")
(setq c-basic-offset 4)
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (c-toggle-auto-newline))
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; column-number-mode
(setq column-number-mode t)             ; enable column-number-mode

;; compile
(require 'compile)
(setq compilation-scroll-output t)        ; auto-scroll the compilation buffer

;; conf-mode
(add-to-list 'auto-mode-alist '("\\.rc\\'" . conf-mode))

;; delete-selection-mode
(delete-selection-mode 1)

;; ediff
(setq ediff-diff-options "-w")

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
            ;; Default indentation to 2, but let SGML mode guess, too.
            (set (make-local-variable 'sgml-basic-offset) 4)
            (sgml-guess-indent)))

;; linum-mode
(global-linum-mode 1)
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

;; org-mode
(require 'org)
(setq org-startup-indented t)           ; enable auto-indent
(setq org-src-fontify-natively t)       ; enable source code highlight

;; read-only-mode
(global-set-key (kbd "C-c C-r") 'read-only-mode)

;; recentf-mode
(require 'recentf)
(setq recentf-max-menu-items 25)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; show-paren-mode
(show-paren-mode 1)                       ; highlight paired brackets

;; time
(require 'time)
(setq display-time-format "%Y/%m/%e %H:%M")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode t)

;; tramp-mode
(require 'tramp)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

;; winner-mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

(provide 'init-package-builtin)
;;; init-package-builtin.el ends here
