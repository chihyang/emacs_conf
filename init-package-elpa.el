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
    ac-ispell
    adaptive-wrap
    ample-theme
    anzu
    auctex
    auto-complete
    auto-complete-clang
    auto-complete-clang-async
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
    csharp-mode
    cursor-chg
    dim
    dired+
    dired-single
    dired-sort-menu+
    elpy
    ethan-wspace
    everything
    fill-column-indicator
    fish-mode
    flycheck
    flyspell-popup
    haskell-mode
    hide-lines
    htmlize
    iedit
    indent-guide
    ivy
    java-snippets
    json-mode
    linum-relative
    logview
    lua-mode
    magit
    markdown-mode
    markdown-preview-mode
    minimap
    modeline-posn
    modern-cpp-font-lock
    multiple-cursors
    origami
    pandoc-mode
    paredit
    plantuml-mode
    powerline-evil
    protobuf-mode
    psvn
    py-autopep8
    rainbow-mode
    smart-mode-line
    smart-mode-line-powerline-theme
    sos
    sr-speedbar
    swiper
    switch-window
    tabbar
    use-package
    vimrc-mode
    vlf
    websocket
    workgroups2
    xcscope
    yasnippet
    youdao-dictionary
    ztree
    ))

(defun ensure-packages ()
  "Install lost packages."
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
(require 'auto-complete-c-headers)
(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)))
(defun ac-cc-mode-setup ()
  "Set up auto-complete-clang."
  (if (eq system-type 'windows-nt)
      (progn
        (require 'auto-complete-clang)
        (add-to-list 'ac-sources 'ac-source-clang))
    (progn
      (require 'auto-complete-clang-async)
      (setq ac-clang-complete-executable "~/clang-complete")
      (add-to-list 'ac-sources 'ac-source-clang-async)
      (ac-clang-launch-completion-process))))
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(load (expand-file-name "~/.emacs.d/auto-complete-clang-extension"))
(add-hook 'c++-mode-hook
          (lambda ()
            (setq achead:include-directories
                  (append achead:include-directories
                          ac-clang-extension-all-include-dirs))
            (setq ac-clang-flags
                  (mapcar (lambda (item) (concat "-I" item))
                          ac-clang-extension-all-include-dirs))))

;; auto-highlight-symbol-mode
(require 'auto-highlight-symbol)
(add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)

;; autopair
(require 'autopair)
(if (version< emacs-version "24.4")
    (autopair-global-mode 1))

;; avy
(global-set-key (kbd "M-g :") 'avy-goto-char)
(global-set-key (kbd "M-g '") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
(avy-setup-default)

;; blank-mode
(global-set-key (kbd "C-c C-b") 'whitespace-mode) ; show whitespace

;; bookmark+
; rebind bmkp prefix key to "C x /"
(setq bmkp-bookmark-map-prefix-keys (quote ("/")))
(setq bmkp-last-as-first-bookmark-file nil)
(require 'bookmark+-bmu)
; change annoying bookmark name face
(when (not (display-graphic-p))
  (set-face-attribute
   'bmkp-local-file-without-region nil
   :foreground "green"))                ; color in terminal mode
(setq bookmark-save-flag 1)             ; automatically save change

;; cal-china-x
(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holiday
      cal-china-x-chinese-holidays)
(setq calendar-holidays
      (append cal-china-x-important-holidays
              cal-china-x-general-holidays
              holiday-other-holidays))

;; chinese-fonts-setup
(require 'chinese-fonts-setup)
(cnfonts-enable)
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

;; color-theme-solarized
(require 'color-theme)
(require 'color-theme-solarized)
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
(switch-theme 'solarized 'ample-flat)

;; column-marker
(column-marker-1 80)                    ; column marker width

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
     (auto-fill-function         " ¶")
     (auto-revert-mode           "")
     (auto-complete-mode         "")
     (auto-highlight-symbol-mode "")
     (autopair-mode              "")
     (anzu-mode                  "")
     (abbrev-mode                "")
     (hs-minor-mode              "")
     (ivy-mode                   "")
     )))
(eval-after-load "~/.emacs.d/init.el" (simplify-mode-alias))
(add-hook 'workgroups-mode-hook
          (lambda ()
            (dim-minor-name 'workgroups-mode "")))
(add-hook 'yas-minor-mode-hook
          (lambda ()
            (dim-minor-name 'yas-minor-mode  " →")))
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

;; elpy
(require 'elpy)
(elpy-enable)
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))

;; ethan-wspace
(require 'ethan-wspace)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq mode-require-final-newline nil)
            (setq require-final-newline nil)
            (ethan-wspace-mode 1)
            (ethan-wspace-clean-no-nl-eof-mode -1)
            (ethan-wspace-highlight-tabs-mode 1)))

;; everything
(if (eq system-type 'windows-nt)
    (setq everything-cmd "D:/Program Files/Everything/es.exe"))

;; fill-column-indicator
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")

;; flycheck-mode
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'tex-mode-hook 'flycheck-mode)

;; flyspell-mode
(require 'ispell)
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(if (eq system-type 'windows-nt)
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-alternate-dictionary "~/.emacs.d/windows/dict.txt")))
(ispell-change-dictionary "american" t)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1)))) ;disable spell check for log mode

;; flyspell-popup
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

(require 'ivy)
(setq ivy-use-virtual-buffers t)
(ivy-mode 1)

;; iedit-mode
(require 'iedit)
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
(global-set-key (kbd "C-c ; s") 'iedit-dwim)

;; indent-guide
(setq indent-guide-recursive t)

;; langtool
(require 'langtool)
(setq langtool-mother-tongue "en-US")
(setq langtool-language-tool-jar
      "~/languagetool/languagetool-commandline.jar")
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)
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
      'langtool-autoshow-detail-popup)

;; logview-mode
(require 'logview)
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
          (format . "TIMESTAMP LEVEL/NAME( THREAD):")
          (levels . "Logcat")
          (timestamp "Android Logcat Time Format")
          (aliases "la"))
         ("Android-nontime"
          (format . "LEVEL NAME THREAD:")
          (levels . "Logcat")
          (aliases "la-n"))
         ("threadtime"
          (format . "TIMESTAMP THREAD THREAD LEVEL NAME:")
          (levels . "Logcat")
          (timestamp "Android Logcat Time Format")
          (aliases "tt"))
         ("thread-blank"
          (format . "LEVEL/NAME( THREAD):")
          (levels . "Logcat")
          (timestamp "Android Logcat Time Format")
          (aliases "tb"))
         ("thread-nonblank"
          (format . "LEVEL/NAME(THREAD):")
          (levels . "Logcat")
          (timestamp "Android Logcat Time Format")
          (aliases "tn"))
         ("android-colon"
          (format . "TIMESTAMP: LEVEL/NAME(THREAD):")
          (levels . "Logcat")
          (timestamp "Android Logcat Time Format")
          (aliases "lac"))
         ("decoder"
          (format . "[TIMESTAMP]")
          (timestamp "ISO 8601 datetime")
          (aliases "de")))))

;; lua-mode
(setq lua-indent-level 4)

;; markdown-mode
;; Note: \' matches the end of a string, while $ matches the empty string before
;; a newline. Thus, $ may lead to unexpected behavior when dealing with
;; filenames containing newlines. See the following link:
;; https://www.emacswiki.org/emacs/AutoModeAlist
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("[^Mm]\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README.*" . gfm-mode))

;; modern-c++-font-lock
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

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
(require 'plantuml-mode)
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;; smart-mode-line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 20)
(sml/setup)
(require 'modeline-posn)                ; modeline-posn must be loaded after sml
                                        ; to take effect

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

;; vlf
(require 'vlf-setup)
(setq vlf-application 'dont-ask)

;; workgroups2
(require 'workgroups2)
(setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
(setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
;; Mode Line changes
(setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
(setq wg-flag-modified t)                 ; Display modified flags as well
(setq wg-mode-line-decor-left-brace "["
      wg-mode-line-decor-right-brace "]"  ; how to surround it
      wg-mode-line-decor-divider ":")
(add-hook 'after-init-hook (lambda ()  (workgroups-mode 1)))

;; xcscope
(require 'xcscope)
(setq cscope-option-do-not-update-database t)
(cscope-setup)
(add-hook 'java-mode-hook #'cscope-minor-mode)
(add-hook 'c-mode-common-hook #'cscope-minor-mode)

;; yasnippet
(require 'yasnippet)
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

;; youdao-dictionary
(require 'youdao-dictionary)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)

(provide 'init-package-elpa)
;;; init-package-elpa.el ends here
