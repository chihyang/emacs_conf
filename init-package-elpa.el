;;; init-package-elpa --- Configuration for packages managed by elpa.

;;; Commentary:
;;; Load packages configuration managed by elpa.
;;; Generally speaking, everything in this configuration file is platform
;;; independent, i.e., once you download it from server, it will download
;;; necessary packages automatically.  You don't need to do anything but offer a
;;; accessible network.  Then get a cup of coffee to wait until the download
;;; completes.

;;; Code:

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
    anzu
    auctex
    auto-complete
    auto-complete-clang
    auto-complete-c-headers
    auto-highlight-symbol
    autopair
    blank-mode
    bookmark+
    cal-china-x
    chinese-fonts-setup
    cmake-mode
    column-marker
    color-theme
    color-theme-solarized
    company
    company-c-headers
    company-shell
    cpputils-cmake
    csharp-mode
    cursor-chg
    dired-single
    fish-mode
    flycheck
    flyspell-popup
    haskell-mode
    icicles
    irony
    indent-guide
    java-snippets
    logview
    lua-mode
    magit
    markdown-mode
    markdown-preview-mode
    minimap
    pandoc-mode
    paredit
    plantuml-mode
    powerline-evil
    protobuf-mode
    psvn
    rainbow-mode
    session
    smart-mode-line
    smart-mode-line-powerline-theme
    sr-speedbar
    swiper
    switch-window
    tabbar
    vimrc-mode
    websocket
    workgroups2
    yasnippet
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

;;; configuration of packages, ordered alphabetically

;; anzu
(global-anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; auto-complete
(ac-config-default)                             ; auto-complete
(ac-flyspell-workaround)                        ; fix collisions with flyspell
(ac-linum-workaround)                           ; fix collisions with linum
(global-auto-complete-mode t)                   ; enable auto-complete-mode  globally
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'text-mode-hook 'auto-complete-mode)

;; auto-highlight-symbol-mode
(global-auto-highlight-symbol-mode)

;; autopair
(autopair-global-mode 1)                        ; enable autopair in all buffers

;; blank-mode
(global-set-key (kbd "C-c C-b") 'whitespace-mode) ; show whitespace

;; bookmark+
; rebind bmkp prefix key to "C x /"
(setq bmkp-bookmark-map-prefix-keys (quote ("/")))
(setq bmkp-last-as-first-bookmark-file nil)

;; chinese-fonts-setup
(require 'chinese-fonts-setup)

;; color-theme-solarized
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'dark)
(if (window-system)
    (load-theme 'solarized t))

;; cpputils-cmake
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all))))

;; dired-single
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
            (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
            (define-key dired-mode-map (kbd "^")
              (lambda ()
                (interactive)
                (dired-single-buffer "..")))))

;; flycheck-mode
(global-flycheck-mode)

;; flyspell-mode
(ispell-change-dictionary "american" t)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1)))) ;disable spell check for log mode

;; flyspell-popup
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

;; icicle-mode
(icy-mode 1)

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

;; indent-guide
(setq indent-guide-recursive t)

;; logview-mode
(setq
 logview-additional-timestamp-formats
 (quote (("Android Logcat Time Format"
          (java-pattern . "MM-dd HH:mm:ss.SSS")
          (aliases "MM-dd HH:mm:ss.SSS")))))
(setq
 logview-additional-level-mappings
 (quote (("Logcat"
          (error "E")
          (warning "W")
          (information "I")
          (debug "D")
          (trace "V")
          (aliases "android")))))
(setq
 logview-additional-submodes
 (quote (("Android"
          (format . "TIMESTAMP THREAD NAME:")
          (levels . "Logcat")
          (timestamp "Android Logcat Time Format")
          (aliases "logcat")))))

;; lua-mode
(setq lua-indent-level 4)

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\README*" . markdown-mode))

;; paredit
(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                ielm-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;; plantuml-mode
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; smart-mode-line
(setq sml/no-confirm-load-theme t)

;; session restore
(setq desktop-files-not-to-save "^$")     ; reload tramp path
(desktop-save-mode 1)                     ; save session
;; (add-hook 'after-init-hook 'session-initialize) ; restore session

;; sr-speedbar
(custom-set-variables
 '(sr-speedbar-default-width 100)
 '(sr-speedbar-max-width 100))

;; switch-window
(global-set-key (kbd "C-x o") 'switch-window) ; rebind `C-x o' to switch-window

;; tabbar-mode
(tabbar-mode)
(load "~/.emacs.d/init-tabbar-theme.el") ; theme
(if (display-graphic-p)                  ; key-binding
    (progn
      (global-set-key [C-tab] 'tabbar-forward-tab)
      (global-set-key (kbd "C-c <C-tab>") 'tabbar-backward-tab))
  (progn
    (global-set-key (kbd "C-c t n") 'tabbar-forward-tab)
    (global-set-key (kbd "C-c t p") 'tabbar-backward-tab)))

;; vimrc-mode
(add-to-list 'auto-mode-alist '("vim\\(rc\\)?$" . vimrc-mode))

;; workgroups2
(setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
(setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
;; Mode Line changes
(setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
(setq wg-flag-modified t)                 ; Display modified flags as well
(setq wg-mode-line-decor-left-brace "["
      wg-mode-line-decor-right-brace "]"  ; how to surround it
      wg-mode-line-decor-divider ":")
(workgroups-mode 1)

;; yasnippet
(yas-global-mode 1)
(add-hook 'prog-mode-hook #'yas-minor-mode)
;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)
;; push yasnippet into auto complete sources
(setq ac-sources (append '(ac-source-yasnippet) ac-sources))

(provide 'init-package-elpa)
;;; init-package-elpa.el ends here
