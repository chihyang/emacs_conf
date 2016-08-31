;; proxy settings, disable this if necessary
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "127.0.0.1:8118")
     ("https" . "127.0.0.1:8118")))

;; load newer bytecodes
(setq load-prefer-newer t)
(require 'package)

;;; Standard package repositories
;; melpa for most packages
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
;; org repository for completeness
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(defconst required-packages
  '(
    auctex
    auto-complete
    auto-highlight-symbol
    autopair
    blank-mode
    bm
    cal-china-x
    chinese-fonts-setup
    cmake-mode
    column-marker
    color-theme
    color-theme-solarized
    company
    company-shell
    company-c-headers
    company-shell
    cpputils-cmake
    csharp-mode
    cursor-chg
    fish-mode
    flycheck
    flyspell-popup
    haskell-mode
    icicles
    irony
    indent-guide
    lua-mode
    magit
    markdown-mode
    markdown-preview-mode
    minimap
    pandoc-mode
    powerline-evil
    protobuf-mode
    rainbow-mode
    session
    smart-mode-line
    smart-mode-line-powerline-theme
    sr-speedbar
    switch-window
    tabbar
    vimrc-mode
    websocket
    ))

(defun ensure-packages ()
  "install lost packages"
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package required-packages)
    (unless (package-installed-p package)
      (package-install package))))

(ensure-packages)

;; cedet
;; (load-file (concat user-emacs-directory "cedet/cedet-devel-load.el"))
;; (load-file (concat user-emacs-directory "cedet/contrib/cedet-contrib-load.el"))

;;; configuration of packages
;; flyspell-mode
(ispell-change-dictionary "american" t)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1)))) ;disable spell check for log mode

;; flyspell-popup
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

;; flycheck-mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; auto-complete
(ac-config-default)                             ; auto-complete
(ac-flyspell-workaround)                        ; fix collisions with flyspell
(ac-linum-workaround)                           ; fix collisions with linum
(global-auto-complete-mode t)                   ; enable auto-complete-mode  globally
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'text-mode-hook 'auto-complete-mode)

;; company
;; (add-hook 'after-init-hook 'global-company-mode)

;; auto-highlight-symbol-mode
(global-auto-highlight-symbol-mode)

;; autopair
(autopair-global-mode 1)                        ; enable autopair in all buffers

;; sr-speedbar
;; (sr-speedbar-open)
(custom-set-variables
 '(sr-speedbar-default-width 100)
 '(sr-speedbar-max-width 100))

;; tabbar-mode
(tabbar-mode)
(load "~/.emacs.d/init-tabbar-theme.el")
(global-set-key [C-tab] 'tabbar-forward-tab) ; tabbar mode
(global-set-key (kbd "C-c <C-tab>") 'tabbar-backward-tab) ; tabbar mode

;; irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(setq w32-pipe-read-delay 0)

;; switch-window
(global-set-key (kbd "C-x o") 'switch-window) ; rebind `C-x o' to switch-window

;; blank-mode
(global-set-key (kbd "C-c C-b") 'whitespace-mode) ; show whitespace

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\README\\'" . markdown-mode))

;; paredit
(add-to-list 'load-path "~/.emacs.d/elpa/paredit")
(require 'paredit)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                ielm-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;; indent-guide
(setq indent-guide-recursive t)

;; bm
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; lua-mode
(setq lua-indent-level 4)

;; vimrc-mode
(add-to-list 'auto-mode-alist '("vim\\(rc\\)?$" . vimrc-mode))

;; cpputils-cmake
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all))))

;; session restore
(add-hook 'after-init-hook 'session-initialize) ;restore session

;; smart-mode-line
(setq sml/no-confirm-load-theme t)
;; (setq powerline-arrow-shape 'curve)
;; (setq powerline-default-separator-dir '(right . left))
;; (setq sml/theme 'respectful)
;; (sml/setup)

;; chinese-fonts-setup
(require 'chinese-fonts-setup)
