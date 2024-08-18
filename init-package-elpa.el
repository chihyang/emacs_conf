;;; init-package-elpa --- Configuration for packages managed by elpa.

;;; Commentary:
;;; Load packages configuration managed by elpa.
;;; Generally speaking, everything in this configuration file is platform
;;; independent, i.e., once you download it from server, it will download
;;; necessary packages automatically.  You don't need to do anything but offer a
;;; accessible network.  Then get a cup of coffee to wait until the download
;;; completes.

;;; Code:

(defconst required-packages
  '(
    adaptive-wrap
    ample-theme
    anzu
    anaphora
    auctex
    auto-highlight-symbol
    avy
    ace-pinyin
    cal-china-x
    cnfonts
    cmake-font-lock
    cmake-mode
    cmake-project
    color-theme-modern
    company
    company-prescient
    company-quickhelp
    csharp-mode
    counsel-projectile
    dashboard
    dim
    dired-single
    dired-subtree
    edit-server
    elpy
    ethan-wspace
    fill-column-indicator
    fish-mode
    flycheck
    flyspell-popup
    geiser
    geiser-chez
    ;; geiser-racket
    git-gutter
    gmail-message-mode
    golden-ratio
    graphviz-dot-mode
    gruvbox-theme
    haskell-mode
    hide-lines
    highlight-escape-sequences
    hl-todo
    htmlize
    iedit
    indent-guide
    ivy
    ivy-prescient
    ivy-rich
    java-snippets
    json-mode
    langtool
    linum-relative
    logview
    lua-mode
    magit
    markdown-mode
    markdown-preview-mode
    minimap
    modern-cpp-font-lock
    multiple-cursors
    nlinum
    nov
    omni-scratch
    org-preview-html
    origami
    pandoc-mode
    paredit
    persistent-scratch
    plantuml-mode
    powerline-evil
    protobuf-mode
    projectile
    py-autopep8
    pyenv-mode
    racket-mode
    rainbow-delimiters
    rainbow-mode
    slime
    smart-mode-line
    smart-mode-line-powerline-theme
    solarized-theme
    separedit
    sx
    sr-speedbar
    swiper
    switch-window
    tabbar
    trashed
    undo-tree
    use-package
    vlf
    vimrc-mode
    visual-fill-column
    visual-regexp-steroids
    websocket
    which-key
    wttrin
    xcscope
    yasnippet
    yasnippet-snippets
    youdao-dictionary
    ztree
    ))

(defun ensure-packages ()
  "Install lost packages."
  (interactive)
  (let ((cache-refreshed? nil))
    (dolist (package required-packages)
      (unless (package-installed-p package)
        (unless (and package-archive-contents cache-refreshed?)
          (setq cache-refreshed? t)
          (message "Ready to refresh package contents")
          (package-refresh-contents))
        (package-install package)))))

(ensure-packages)

;;; configuration of packages, ordered alphabetically
;; adaptive-wrap
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)


;; auctex
(use-package latex
  :ensure auctex
  :config
  (define-key TeX-mode-map (kbd "C-x n n") #'next-multiframe-window)
  (define-key LaTeX-mode-map (kbd "C-x n n") #'next-multiframe-window))

;; anzu
(use-package anzu
  :config
  (if (display-graphic-p)
      (progn
        (global-set-key [remap query-replace] 'anzu-query-replace)
        (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))
    (progn
      (global-set-key [remap query-replace] 'anzu-query-replace-regexp)
      (global-set-key [remap query-replace-regexp] 'anzu-query-replace)))
  (setq anzu-search-threshold nil)
  (global-anzu-mode +1))

;; auto-highlight-symbol-mode
(use-package auto-highlight-symbol
  :config
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

;; avy
(use-package avy
  :config
  (global-set-key (kbd "M-g :") 'avy-goto-char)
  (global-set-key (kbd "M-g '") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (avy-setup-default)
  )

(use-package ace-pinyin
  :requires avy
  :config
  (setq ace-pinyin-treat-word-as-char nil)
  (ace-pinyin-global-mode +1)
  )

;; bookmark+
(use-package bookmark+
  :load-path "emacswiki/bookmark+/"
  :config
  (setq bmkp-bookmark-map-prefix-keys (quote ("/")))
  (setq bmkp-last-as-first-bookmark-file nil)
  (setq bmkp-auto-light-when-set 'any-bookmark)
  (setq bmkp-auto-light-when-jump 'any-bookmark)
  ;; change annoying bookmark name face
  (when (not (display-graphic-p))
    (set-face-attribute
     'bmkp-local-file-without-region nil
     :foreground "green"))
  ;; automatically save change
  (setq bookmark-save-flag 1))

;; cal-china-x
(use-package cal-china-x
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holiday
        cal-china-x-chinese-holidays)
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))

;; chinese-fonts-setup
(use-package cnfonts
  :init
  (defun my-set-symbol-fonts (fontsizes-list)
    (let* ((fontname "Segoe UI Symbol")
           (fontsize (nth 0 fontsizes-list))
           (fontspec (font-spec :name fontname
                                :size fontsize
                                :weight 'normal
                                :slant 'normal)))
      (if (cnfonts--fontspec-valid-p fontspec)
          (set-fontset-font "fontset-default" 'symbol fontspec nil 'append)
        (message "字体 %S 不存在！" fontname))))
  (add-hook 'cnfonts-set-font-finish-hook 'my-set-symbol-fonts)
  :config
  (cnfonts-enable))

;; theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)
  (enable-theme 'gruvbox-dark-medium))

(defun switch-theme (gui-theme terminal-theme)
  (interactive
    (list
     (intern (completing-read "Select custom GUI theme: "
                              (mapcar 'symbol-name
                                      (custom-available-themes))))
     (intern (completing-read "Select custom TERM theme: "
                               (mapcar 'symbol-name
                                       (custom-available-themes))))))
  (if (display-graphic-p)
      (progn
        (disable-theme terminal-theme)
        (set-frame-parameter nil 'background-mode 'dark)
        (set-terminal-parameter nil 'background-mode 'dark)
        (load-theme gui-theme t)
        (enable-theme gui-theme))
    (progn
      (disable-theme gui-theme)
      (set-frame-parameter nil 'background-mode 'dark)
      (set-terminal-parameter nil 'background-mode 'dark)
      (load-theme terminal-theme t t)
      (enable-theme terminal-theme))))

;; company
(use-package company
  :config
  (setq company-show-quick-access t)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (defun my-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (defun company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-require-match nil)
    (setq company-auto-commit #'my-company-visible-and-explicit-action-p)
    (setq company-frontends '(company-echo-metadata-frontend
                              company-pseudo-tooltip-unless-just-one-frontend-with-delay
                              company-preview-frontend))
    (define-key company-active-map [tab]
      'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "TAB")
      'company-select-next-if-tooltip-visible-or-complete-selection))

  (company-ac-setup)
  (add-hook 'after-init-hook 'global-company-mode))

;; company-prescient
(use-package company-prescient
  :config
  (company-prescient-mode 1))

;; company-quickhelp
(use-package company-quickhelp
  :requires company
  :config
  (company-quickhelp-mode 1))

;; cmake-font-lock
(use-package cmake-font-lock
  :requires cmake-mode
  :config
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

;; cmake-project
(use-package cmake-project
  :requires cmake-mode
  :config
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'cmake-mode-hook 'maybe-cmake-project-hook))

;; column-marker
(use-package column-marker
  :load-path "emacswiki/column-marker/"
  :config
  (column-marker-1 80))

;; dashboard
(use-package dashboard
  :disabled
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-navigator t)
  (setq dashboard-items
        '((projects . 5)
          (recents . 5)
          (bookmarks . 5)))
  ;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

;; dim
(use-package dim
  :config
  (defun simplify-mode-alias ()
    "Shorten mode line major/minor modes names."
    (dim-major-names
     '(
       (emacs-lisp-mode     "Eλ")
       (haskell-mode        "Hs")
       (makefile-gmake-mode "GM")
       (scheme-mode         "λ")
       (racket-mode         "Rλ")
       ))
    (dim-minor-names
     '(
       (abbrev-mode                "")
       (ace-pinyin-mode            "")
       (anzu-mode                  "")
       (auto-fill-function         "¶")
       (auto-highlight-symbol-mode "")
       (auto-revert-mode           "")
       (company-mode               "")
       (counsel-mode               "")
       (eldoc-mode                 "")
       (ethan-wspace-mode          "␣")
       (flycheck-mode              "!")
       (flyspell-mode              "√")
       (git-gutter-mode            "")
       (hs-minor-mode              "")
       (interactive-haskell-mode   "")
       (ivy-mode                   "")
       (modern-c++-font-lock-mode  " C++11 ")
       (paredit-mode               "()")
       (projectile-mode            "Pj")
       (subword-mode               "")
       (undo-tree-mode             "⇔")
       (workgroups-mode            "")
       (yas-minor-mode             "")
       )))
  (add-hook 'find-file-hook 'simplify-mode-alias))

;; dired+
(use-package dired+
  :load-path "emacswiki/dired+/"
  :config
  (unbind-key "M-O" dired-mode-map)
  (define-key dired-mode-map (kbd "y") 'dired-show-file-type)
  (define-key dired-mode-map (kbd "_") 'diredp-relsymlink-this-file))

;; dired-single
(use-package dired-single
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
              (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
              (define-key dired-mode-map (kbd "^")
                (lambda ()
                  (interactive)
                  (dired-single-buffer ".."))))))

;; dired-sort-menu+
(use-package dired-sort-menu
  :requires (dired+)
  :load-path "emacswiki/dired-sort-menu/")

;; dired-sort-menu+
(use-package dired-sort-menu+
  :requires (dired+ dired-sort-menu)
  :load-path "emacswiki/dired-sort-menu+/")

;; dired-subtree
(use-package dired-subtree
  :defer t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-cycle)))

;; elpy
(use-package elpy
  :ensure t
  :defer t
  ;; :config
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; ethan-wspace
(use-package ethan-wspace
  :config
  (add-hook
   'prog-mode-hook
   (lambda ()
     (setq mode-require-final-newline nil)
     (global-ethan-wspace-mode 1)
     (ethan-wspace-clean-no-nl-eof-mode 1)
     (ethan-wspace-highlight-tabs-mode 1)))
  (add-hook
   'text-mode-hook
   (lambda ()
     (ethan-wspace-mode 0)
     (setq mode-require-final-newline t))))

;; fill-column-indicator
(use-package fill-column-indicator
  :defer t
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "darkblue"))

;; flycheck-mode
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'tex-mode-hook 'flycheck-mode))

;; flyspell-mode
(use-package ispell
  :init
  (ispell-change-dictionary "american" t)
  (setq ispell-extra-args '("--lang=en_US"))
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (if (eq system-type 'windows-nt)
      (progn
        (setq ispell-program-name "aspell")
        (setq ispell-alternate-dictionary "~/.emacs.d/windows/dict.txt")))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  ;; flyspell-popup
  ;; (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
)

(use-package git-gutter
  :init
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  :config
  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
  ;; Stage current hunk
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  ;; Revert current hunk
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
  ;; Mark current hunk
  (global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk))

;;; geiser
(use-package geiser
  :requires paredit
  :config
  (add-hook 'geiser-repl-mode-hook 'paredit-mode))

(use-package highlight-escape-sequences
  :init
  (hes-mode 1)
  :config
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face))

;; hl-todo
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

;; ivy
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  (counsel-mode 1))

;; ivy-prescient
(use-package ivy-prescient
  :after
  (ivy)
  :config
  (ivy-prescient-mode))

;; ivy-rich
(use-package ivy-rich
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-parse-remote-file-path nil)
  (setq ivy-rich-path-style (quote full))
  (ivy-rich-mode 1))

;; iedit-mode
(use-package iedit
  :config
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
  (global-set-key (kbd "C-c ; s") 'iedit-dwim))

;; indent-guide
(setq indent-guide-recursive t)

;; langtool
(use-package langtool
  :config
  (when (file-exists-p "~/languagetool/languagetool-commandline.jar")
    (setq langtool-language-tool-jar
          "~/languagetool/languagetool-commandline.jar")
    (setq langtool-mother-tongue "en-US")
    (global-set-key "\C-x4w" 'langtool-check)
    (global-set-key "\C-x4W" 'langtool-check-done)
    (global-set-key "\C-x4l" 'langtool-switch-default-language)
    (global-set-key "\C-x44" 'langtool-show-message-at-point)
    (global-set-key "\C-x4c" 'langtool-correct-buffer))
  (defun langtool-autoshow-detail-popup (overlays)
    "Show langtool check result automatically."
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  (setq langtool-autoshow-message-function
        'langtool-autoshow-detail-popup))

;; logview-mode
(require 'logview)
(setq
 logview-additional-timestamp-formats
 (quote (("Android Logcat Time Format"
          (java-pattern . "MM-dd HH:mm:ss.SSS")
          (aliases "ant"))
         ("Log4cpp Time Format"
          (java-pattern . "yyyy-MM-dd HH:mm:ss,SSS")
          (aliases "l4t")))))
(setq
 logview-additional-level-mappings
 (quote (("Logcat"
          (error "E")
          (warning "W")
          (information "I")
          (debug "D")
          (trace "V")
          (aliases "anl"))
         ("Log4cpp"
          (error "ERROR")
          (warning "WARN")
          (information "NOTICE")
          (debug "INFO")
          (trace "DEBUG")
          (aliases "l4l")))))
(setq
 logview-additional-submodes
 (quote (("Android"
          (format . "TIMESTAMP LEVEL/NAME( THREAD):")
          (levels . "anl")
          (timestamp "ant")
          (aliases "an"))
         ("Android-nontime"
          (format . "LEVEL NAME THREAD:")
          (levels . "anl")
          (aliases "ann"))
         ("threadtime"
          (format . "TIMESTAMP THREAD THREAD LEVEL NAME:")
          (levels . "anl")
          (timestamp "ant")
          (aliases "tt"))
         ("thread-blank"
          (format . "LEVEL/NAME( THREAD):")
          (levels . "anl")
          (timestamp "ant")
          (aliases "tb"))
         ("thread-nonblank"
          (format . "LEVEL/NAME(THREAD):")
          (levels . "anl")
          (timestamp "ant")
          (aliases "tn"))
         ("android-colon"
          (format . "TIMESTAMP: LEVEL/NAME(THREAD):")
          (levels . "anl")
          (timestamp "ant")
          (aliases "ancolon"))
         ("log4cpp1"
          (format . "[TIMESTAMP] LEVEL:")
          (levels . "l4l")
          (timestamp "l4t")
          (aliases "l4g1"))
         ("log4cpp2"
          (format . "[TIMESTAMP] [LEVEL]:")
          (levels . "l4l")
          (timestamp "l4t")
          (aliases "l4g2"))
         ("log4cpp3"
          (format . "[TIMESTAMP]")
          (timestamp "ISO 8601 datetime")
          (aliases "l4g3")))))

;; lua-mode
(setq lua-indent-level 4)

;; magit
(use-package magit
  :defer t
  :init
  (setq magit-blame-heading-format "%-20a %H %C %s")
  (setq magit-diff-refine-hunk 'all))

;; markdown-mode
;; Note: \' matches the end of a string, while $ matches the empty string before
;; a newline. Thus, $ may lead to unexpected behavior when dealing with
;; filenames containing newlines. See the following link:
;; https://www.emacswiki.org/emacs/AutoModeAlist
(use-package markdown-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("[^Mm]\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("README.*" . gfm-mode))
  (define-key markdown-mode-map (kbd "C-x n n") #'next-multiframe-window)
  (setq markdown-command "pandoc -f markdown_github")
  (markdown-toggle-fontify-code-blocks-natively))

;; modern-c++-font-lock
(use-package modern-cpp-font-lock
  :config
  (add-hook 'c-mode-common-hook #'modern-c++-font-lock-mode))

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; nlinum
(use-package nlinum
  :defer t
  :init
  (setq nlinum-format "%d "))

(use-package nov
  :after
  (visual-fill-column)
  :init
  (setq nov-text-width 80)
  (setq nov-text-width t)
  (defun my-nov-mode-hook ()
    (setq buffer-display-table (make-display-table))
    (display-line-numbers-mode -1)
    (setq visual-fill-column-center-text t)
    (set-display-table-slot buffer-display-table 'wrap ?\ )
    (define-key (current-local-map) (kbd "j") #'next-line)
    (define-key (current-local-map) (kbd "k") #'previous-line))
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (add-hook 'nov-mode-hook 'my-nov-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; omni-scratch
(use-package omni-scratch
  :init
  (setq omni-scratch-default-mode 'emacs-lisp-mode)
  :bind (("M-s DEL" . omni-scratch-buffer)
         ("M-s <deletechar>" . omni-scratch-major-buffer)
         ("M-s $" . omni-scratch-goto-latest)
         ("M-s *" . omni-scratch-buffers)))

;; origami
(use-package origami
  :config
  (dolist (hook '(prog-mode-hook))
    (add-hook hook 'origami-mode))
  (global-set-key (kbd "C-c C-f") 'origami-toggle-node)
  (global-set-key (kbd "C-c M-f") 'origami-toggle-all-nodes))

;; paredit
(use-package paredit
  :after (racket-mode scribble-mode)
  :config
  ;; adapted from https://www.reddit.com/r/emacs/comments/101uwgd/comment/jjq0jen/
  (defun my/paredit-RET ()
    "Wraps `paredit-RET' to provide a sensible minibuffer experience."
    (interactive)
    (when (not (local-key-binding (kbd "<RET>")))
      (define-key (current-local-map) (kbd "<RET>") (local-key-binding (kbd "<RET>")))
      (define-key (current-local-map) (kbd "<RET>") #'paredit-RET))
    (enable-paredit-mode))
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map "\M-r" nil)
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  ielm-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  racket-mode-hook
                  racket-repl-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'my/paredit-RET))
  (defun paredit-replace-wrap (target)
    "Replace the pair of brackets around an s-exp with a pair of square brackets."
    (save-excursion
      (let ((paren (remove target '(?\{ ?\( ?\[)))
            (paredit-wrap-func
             (cond
              ((char-equal target ?\[) #'paredit-wrap-square)
              ((char-equal target ?\() #'paredit-wrap-round)
              ((char-equal target ?\{) #'paredit-wrap-curly)))
            (sexp-start
             (progn
               (paredit-move-forward)
               (paredit-move-backward)
               (point))))
        (cond
         ((memq (char-after sexp-start) paren)
          (funcall paredit-wrap-func)
          (goto-char (+ (point) 1))
          (paredit-splice-sexp))))))
  (defun paredit-replace-wrap-with-square ()
    "Replace the pair of brackets around an s-exp with a pair of square brackets."
    (interactive)
    (paredit-replace-wrap ?\[))
  (defun paredit-replace-wrap-with-round ()
    "Replace the pair of brackets around an s-exp with a pair of round brackets."
    (interactive)
    (paredit-replace-wrap ?\())
  (defun paredit-replace-wrap-with-curly ()
    "Replace the pair of brackets around an s-exp with a pair of curly brackets."
    (interactive)
    (paredit-replace-wrap ?\{))
  (dolist (mode-map (list
                     emacs-lisp-mode-map
                     lisp-mode-map
                     racket-mode-map
                     racket-repl-mode-map
                     scheme-mode-map
                     scribble-mode-map))
    (define-key mode-map (kbd "C-c [") #'paredit-replace-wrap-with-square)
    (define-key mode-map (kbd "C-c 9") #'paredit-replace-wrap-with-round))
  (define-key scribble-mode-map (kbd "C-c \\") #'paredit-replace-wrap-with-curly)
  ;; switch default buffer in REPL
  (defun racket-change-default-directory ()
    (let ((dir (file-name-directory (buffer-file-name (current-buffer)))))
      (with-current-buffer racket-repl-buffer-name
        (setq default-directory dir))))
  (add-hook 'racket-after-run-hook 'racket-change-default-directory))

;; plantuml-mode
(use-package plantuml-mode
  :config
  (when (file-exists-p plantuml-jar-path)
    (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))

;; projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p")
    'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))

(use-package counsel-projectile
  :requires projectile
  :config
  (counsel-projectile-mode 1))

;; pyenv-mode
(use-package pyenv-mode
  :config
  (when (and (eq system-type 'gnu/linux)
             (executable-find "pyenv" nil))
    (pyenv-mode 1)))

;; racket, use racket-mode instead of geiser
(use-package racket-mode
  :config
  (setq auto-mode-alist (assq-delete-all "\\.rkt\\'" auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (define-key racket-mode-map (kbd "C-c \\") #'racket-insert-lambda)
  (define-key racket-mode-map (kbd "C-c C-a") #'racket-run-and-switch-to-repl)
  (define-key racket-repl-mode-map (kbd "C-c M-o") #'racket-repl-clear-leaving-last-prompt)
  (define-key racket-repl-mode-map (kbd "C-c \\") #'racket-insert-lambda)
  (define-key racket-repl-mode-map (kbd "M-r") #'comint-history-isearch-backward-regexp))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  ielm-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  racket-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode)))

;; slime
(use-package slime
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (if (file-exists-p "~/quicklisp/slime-helper.el")
      (load (expand-file-name "~/quicklisp/slime-helper.el"))))

;; smart-mode-line
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 20)
  (setq eol-mnemonic-dos ":CRLF")
  (setq eol-mnemonic-mac ":CR")
  (setq eol-mnemonic-unix ":LF")
  (setq eol-mnemonic-undecided ":?")
  (setq sml/mule-info "%Z")
  (sml/setup))

;; modeline-posn
(use-package modeline-posn
  :load-path "emacswiki/modeline-posn/"
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1))

;; persistent-scratch
(use-package persistent-scratch
  :defer t)

(use-package scribble-mode
  :after
  (racket-mode)
  :config
  (define-key scribble-mode-map (kbd "C-c \\") #'racket-insert-lambda))

;; separedit
(use-package separedit
  :config
  (define-key prog-mode-map        (kbd "C-c '") #'separedit)
  ;; Default major-mode for editing buffer
  (setq separedit-default-mode 'text-mode)
  ;; Feature options
  (setq separedit-preserve-string-indentation t)
  (setq separedit-continue-fill-column t)
  (setq separedit-write-file-when-execute-save t)
  (setq separedit-remove-trailing-spaces-in-comment t)
  (add-hook 'separedit-buffer-creation-hook #'auto-fill-mode))

;; switch-window
(global-set-key (kbd "C-x o") 'switch-window) ; rebind `C-x o' to switch-window

;; tabbar-mode
(use-package tabbar
  :config
  ;; (load "~/.emacs.d/init-tabbar-theme.el") ; theme
  (if (display-graphic-p)               ; key-binding
      (progn
        (global-set-key [C-tab] 'tabbar-forward-tab)
        (global-set-key (kbd "C-c <C-tab>") 'tabbar-backward-tab))
    (progn
      (global-set-key (kbd "C-c t n") 'tabbar-forward-tab)
      (global-set-key (kbd "C-c t p") 'tabbar-backward-tab)
      (global-set-key (kbd "C-c t <up>") 'tabbar-backward-group)
      (global-set-key (kbd "C-c t <down>") 'tabbar-forward-group)
      (global-set-key (kbd "C-c t <home>") 'tabbar-press-home)))
  (tabbar-mode))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; vimrc-mode
(add-to-list 'auto-mode-alist '("vim\\(rc\\)?$" . vimrc-mode))

(use-package visual-fill-column)

;; vlf
(use-package vlf-setup
  :config
  (setq vlf-application 'dont-ask))

;; workgroups2
(use-package workgroups2
  :load-path "emacswiki/workgroups2/"
  :config
  (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
  (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
  ;; Mode Line changes
  (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
  (setq wg-flag-modified t)                 ; Display modified flags as well
  (setq wg-mode-line-decor-left-brace "["
        wg-mode-line-decor-right-brace "]"  ; how to surround it
        wg-mode-line-decor-divider ":")
  (add-hook 'after-init-hook (lambda ()  (workgroups-mode 1)))
  ;; (add-hook 'server-after-make-frame-hook (lambda () (wg-open-session (wg-get-session-file))))
  (add-to-list 'delete-frame-functions (lambda (frame) (wg-save-session))))

;; wttrin
(use-package wttrin
  :defer t
  :config
  (add-to-list 'wttrin-default-cities "Beijing")
  (add-to-list 'wttrin-default-cities "Shanghai"))

;; xcscope
(use-package xcscope
  :config
  (setq cscope-option-do-not-update-database t)
  (add-hook 'java-mode-hook #'cscope-minor-mode)
  (add-hook 'c-mode-common-hook #'cscope-minor-mode)
  (cscope-setup))

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand))

;; youdao-dictionary
(use-package youdao-dictionary
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
  (global-set-key (kbd "C-x y") 'youdao-dictionary-search-from-input))

(provide 'init-package-elpa)
;;; init-package-elpa.el ends here
