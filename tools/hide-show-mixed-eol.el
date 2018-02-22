;;; hide-show-mixed-eol --- Hide or show mixed End-of-line

;;; Commentary:

;;; Hide or show mixed end-of-file symbol in a buffer.

;;; Code:

;; hide mixed line ending
(defun hide-mixed-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; show mixed line ending
(defun show-mixed-eol ()
  "Show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table nil))

(provide 'hide-show-mixed-eol)
;;; hide-show-mixed-eol.el ends here
