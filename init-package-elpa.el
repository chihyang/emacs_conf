;; load newer bytecodes
(setq load-prefer-newer t)
(require 'package)
;;; Standard package repositories
;;; melpa for most packages
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
    column-marker
    color-theme
    color-theme-solarized
    csharp-mode
    cursor-chg
    ecb
    haskell-mode
    irony
    indent-guide
    lua-mode
    magit
    markdown-mode
    markdown-preview-mode
    minimap
    pandoc-mode
    session
    sr-speedbar
    switch-window
    tabbar
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

;; configuration of packages
(ac-config-default)                             ; auto-complete
(ac-flyspell-workaround)                        ; fix collisions with flyspell
(ac-linum-workaround)                   ;fix collisions with linum
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                c-mode-hook
                c++-mode
                CC-mode
                java-mode
                ruby-mode-hook
                yaml-mode
                python-mode-hook
                shell-mode-hook
                php-mode-hook
                css-mode-hook
                nxml-mode-hook
                crontab-mode-hook
                perl-mode-hook
                javascript-mode-hook
                latex-mode-hook
                tex-mode-hook))
  (add-hook hook 'flyspell-prog-mode))  ; enable comments spell check
(autopair-global-mode 1)                        ; enable autopair in all buffers
(auto-highlight-symbol-mode 1)                  ; auto highlight current symbol
;; sr-speedbar
(sr-speedbar-open)                              ; open
(custom-set-variables
 '(sr-speedbar-default-width 100)
 '(sr-speedbar-max-width 100))
(tabbar-mode)                                 ; tab-bar

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

;; enable some minor modes globally
(define-globalized-minor-mode my-global-minor-mode auto-highlight-symbol-mode
  (lambda () (auto-highlight-symbol-mode 1)))
(my-global-minor-mode 1)

(add-hook 'after-init-hook 'session-initialize) ;restore session
(add-hook 'foo-mode-hook
          (lambda () (interactive) (column-marker-1 80))) ; Highlight column 80 in foo mode

(global-set-key (kbd "C-x o") 'switch-window) ; rebind switch-window
(global-set-key (kbd "C-c C-b") 'blank-mode) ; show whitespace
(global-set-key [C-tab] 'tabbar-forward-tab) ; tabbar mode
(global-set-key (kbd "C-c <C-tab>") 'tabbar-backward-tab) ; tabbar mode

;; configuration of markdown-mode
;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; paredit, not available in melpa
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

(require 'chinese-fonts-setup)
