;;; init-tabbar-theme.el --- Set tabbar theme

;;; Commentary:
;; Original url: https://gist.github.com/3demax/1264635
;; Tabbar settings

;;; Code:
;; If frame-parameter is dark, exchange foreground and background color
(if (not (eq (frame-parameter nil 'background-mode) 'dark))
    (progn 
      (set-face-attribute
       'tabbar-default nil
       :background "gray90"
       :foreground "gray90"
       :family "DejaVu Sans Mono"
       :height 110
       :box '(:line-width 1 :color "gray90" :style nil))
      (set-face-attribute
       'tabbar-unselected nil
       :background "gray80"
       :foreground "gray50"
       :box '(:line-width 5 :color "gray80" :style nil))
      (set-face-attribute
       'tabbar-selected nil
       :background "white"
       :foreground "black"
       :box '(:line-width 5 :color "white" :style nil))
      (set-face-attribute
       'tabbar-highlight nil
       :background "gray95"
       :foreground "black"
       :underline nil
       :box '(:line-width 5 :color "gray95" :style nil))
      (set-face-attribute
       'tabbar-modified nil
       :background "gray80"
       :foreground "blue"
       :underline nil
       :slant 'oblique
       :weight 'bold
       :box '(:line-width 5 :color "gray80" :style nil))
      (set-face-attribute
       'tabbar-button nil
       :background "gray95"
       :foreground "gray90"
       :box '(:line-width 1 :color "gray90" :style nil))
      (set-face-attribute
       'tabbar-separator nil
       :background "gray90"
       :height 0.6))
  (progn 
    (set-face-attribute
     'tabbar-default nil
     :background "gray90"
     :foreground "gray90"
     :family "DejaVu Sans Mono"
     :height 110
     :box '(:line-width 1 :color "gray90" :style nil))
    (set-face-attribute
     'tabbar-unselected nil
     :background "gray50"
     :foreground "gray80"
     :box '(:line-width 5 :color "gray80" :style nil))
    (set-face-attribute
     'tabbar-selected nil
     :background "black"
     :foreground "white"
     :box '(:line-width 5 :color "white" :style nil))
    (set-face-attribute
     'tabbar-highlight nil
     :background "black"
     :foreground "gray95"
     :underline nil
     :box '(:line-width 5 :color "gray95" :style nil))
    (set-face-attribute
     'tabbar-modified nil
     :background "blue"
     :foreground "gray80"
     :underline nil
     :slant 'oblique
     :weight 'bold
     :box '(:line-width 5 :color "gray80" :style nil))
    (set-face-attribute
     'tabbar-button nil
     :background "gray90"
     :foreground "gray95"
     :box '(:line-width 1 :color "gray90" :style nil))
    (set-face-attribute
     'tabbar-separator nil
     :foreground "gray90"
     :height 0.6)))

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted
;; tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))
