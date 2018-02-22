;;; update-load-path.el --- update Emacs load path

;; Version: 1.0

;;; Commentary:

;; Update load path after package is updated.

;;; Code:
(defun update-load-path ()
  "Clear invalid paths from `load-path'."
  (interactive)
  (defun update-path-list-iter (paths result)
    (cond ((null paths) result)
          ((file-directory-p (car paths))
           (update-path-list-iter (cdr paths) (append result (list (car paths)))))
          (t
           (update-path-list-iter (cdr paths) result))))
  (setq load-path
        (update-path-list-iter load-path '())))

(provide 'update-load-path)
;;; update-load-path.el ends here
