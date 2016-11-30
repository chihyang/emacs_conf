;;; init-package-elpa --- Configuration for packages managed by elpa.

;;; Commentary:
;;; Load packages configuration managed by elpa.
;;; Generally speaking, everything in this configuration file is platform
;;; independent, i.e., once you download it from server, it will download
;;; necessary packages automatically.  You don't need to do anything but offer a
;;; accessible network.  Then get a cup of coffee to wait until the download
;;; completes.

;;; Code:

;;; Standard package repositories
;; melpa for most packages
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
;; org repository for completeness
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; marmalade packages
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(defconst required-packages
  '(
    ac-ispell
    adaptive-wrap
    anzu
    auctex
    auto-complete
    auto-complete-clang
    auto-complete-c-headers
    auto-highlight-symbol
    autopair
    avy
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
    dim
    dired+
    dired-single
    dired-sort-menu+
    everything
    fill-column-indicator
    fish-mode
    flycheck
    flyspell-popup
    haskell-mode
    hide-lines
    icicles
    iedit
    irony
    indent-guide
    java-snippets
    logview
    lua-mode
    magit
    markdown-mode
    markdown-preview-mode
    minimap
    multiple-cursors
    origami
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
    sos
    sr-speedbar
    swiper
    switch-window
    tabbar
    vimrc-mode
    websocket
    workgroups2
    yasnippet
    ztree
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

;; ac-ispell
;; Completion words longer than 4 characters
(custom-set-variables
 '(ac-ispell-requires 4)
 '(ac-ispell-fuzzy-limit 2))

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
(add-hook 'prog-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook 'ac-ispell-ac-setup)
(add-hook 'org-mode-hook 'ac-ispell-ac-setup)

;; adaptive-wrap
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;; anzu
(global-anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; auto-complete
(ac-config-default)                             ; auto-complete
(ac-flyspell-workaround)                        ; fix collisions with flyspell
(ac-linum-workaround)                           ; fix collisions with linum
(global-auto-complete-mode t)                   ; enable auto-complete-mode globally
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'text-mode-hook 'auto-complete-mode)
(add-hook 'org-mode-hook 'auto-complete-mode)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/c)
(load (expand-file-name "~/.emacs.d/auto-complete-clang-extension"))
(setq semantic-c-dependency-system-include-path
      ac-clang-extension-all-include-dirs)
(mapc (lambda (dir)
        (semantic-add-system-include dir 'c++-mode)
        (semantic-add-system-include dir 'c-mode))
      ac-clang-extension-all-include-dirs)
(if (file-exists-p "~/.emacs.d/init-package-manual.el")
    (load "~/.emacs.d/init-package-manual.el"))

;; auto-highlight-symbol-mode
(global-auto-highlight-symbol-mode)

;; autopair
(autopair-global-mode 1)                        ; enable autopair in all buffers

;; avy
(global-set-key (kbd "M-g f") 'avy-goto-line)
(avy-setup-default)

;; blank-mode
(global-set-key (kbd "C-c C-b") 'whitespace-mode) ; show whitespace

;; bookmark+
; rebind bmkp prefix key to "C x /"
(setq bmkp-bookmark-map-prefix-keys (quote ("/")))
(setq bmkp-last-as-first-bookmark-file nil)
; change annoying bookmark name face color in terminal mode
(require 'bookmark+-bmu)
(when (not (display-graphic-p))
  (set-face-attribute
   'bmkp-local-file-without-region nil
   :foreground "green"))
; automatically save change
(setq bookmark-save-flag 1)

;; chinese-fonts-setup
(require 'chinese-fonts-setup)
(chinese-fonts-setup-enable)

;; color-theme-solarized
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'dark)
(if (window-system)
    (load-theme 'solarized t))

;; column-marker
(column-marker-1 80)                    ; column marker width

;; cpputils-cmake
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all))))

;; dim
(defun simplify-mode-alias ()
  "Shorten mode line major/minor modes names."
  (dim-major-names
   '(
     (emacs-lisp-mode     "Eλ")
     (makefile-gmake-mode "GM")
     (scheme-mode         "λ")
     ))
  (dim-minor-names
   '(
     (auto-complete-mode         "")
     (auto-fill-function         " ↵")
     (auto-highlight-symbol-mode "")
     (auto-revert-mode           " ^")
     (autopair-mode              "")
     (anzu-mode                  "")
     (hs-minor-mode              "")
     (abbrev-mode                "")
     )))
(eval-after-load "~/.emacs.d/init.el" (simplify-mode-alias))
(add-hook 'workgroups-mode-hook
          (lambda ()
            (dim-minor-name 'workgroups-mode "")))
(add-hook 'yas-minor-mode-hook
          (lambda ()
            (dim-minor-name 'yas-minor-mode  " ->")))
(add-hook 'paredit-mode-hook
          (lambda ()
            (dim-minor-name 'paredit-mode    " ()")))
(add-hook 'flyspell-mode-hook
          (lambda ()
            (dim-minor-name 'flyspell-mode   " √")))

;; dired-single
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
            (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
            (define-key dired-mode-map (kbd "^")
              (lambda ()
                (interactive)
                (dired-single-buffer "..")))))

;; everything
(if (eq system-type 'windows-nt)
    (setq everything-cmd "D:/Program Files/Everything/es.exe"))

;; fill-column-indicator
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")

;; flycheck-mode
(global-flycheck-mode)

;; flyspell-mode
(require 'ispell)
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(if (eq system-type 'windows-nt)
    (setq ispell-alternate-dictionary "~/.emacs.d/windows/dict.txt"))
(ispell-change-dictionary "american" t)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1)))) ;disable spell check for log mode

;; flyspell-popup
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

;; icicle-mode
(icy-mode 1)

;; iedit-mode
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

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

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; origami
(dolist (hook '(
                prog-mode-hook
                ))
  (add-hook hook 'origami-mode))
(global-set-key (kbd "C-c C-f") 'origami-toggle-node)
(global-set-key (kbd "C-c M-f") 'origami-toggle-all-nodes)

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
