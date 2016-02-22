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
    auto-complete
    autopair
    cal-china-x
    chinese-fonts-setup
    column-marker
    csharp-mode
    ecb
    emms
    magit
    markdown-mode
    markdown-preview-mode
    markdown-preview-mode
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
(ac-config-default)                             ; autocomplete
(autopair-global-mode)                ; enable autopair in all buffers
(sr-speedbar-open)                            ; sr-speendbar
(tabbar-mode)                                 ; tab-bar

(add-hook 'after-init-hook 'session-initialize) ;restore session
(add-hook 'foo-mode-hook (lambda () (interactive) (column-marker-1 80))) ; Highlight column 80 in foo mode

(global-set-key (kbd "C-x o") 'switch-window) ; rebind switch-window

;; configuration of markdown-mode
;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'chinese-fonts-setup)
