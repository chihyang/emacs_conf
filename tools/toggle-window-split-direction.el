;;; toggle-window-split-direction.el --- switch window split direction

;; Author: JeffDwork
;; Version: 1.0
;; Keywords: window, split, switch,
;; URL: https://www.emacswiki.org/emacs/ToggleWindowSplit

;;; Commentary:

;; This package switches windows split direction.

;;; Code:

(defun toggle-window-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split-direction)

(provide 'toggle-window-split-direction)
;;; toggle-window-split-direction.el ends here
