;;; easy-copy --- Copy a line or word without selection

;;; Commentary:

;;; Copy the word or line where cursor locates without selection.

;;; Code:
(defun get-point (symbol &optional arg)
  "Get the point."
  (funcall symbol arg)
  (point))

(defun copy-thing (beg end &optional arg)
  "Copy thing between BEG & END into kill ring."
  (save-excursion
    (let ((beg (get-point beg 1))
          (end (get-point end arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (let ((pasteMe
         (lambda()
           (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))))

;; copy a word
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring."
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg))

;; copy a line
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line."
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg))

;; Key binding
(global-set-key (kbd "C-c w") 'copy-word)
(global-set-key (kbd "C-c l") 'copy-line)

(provide 'easy-copy)
;;; easy-copy.el ends here
