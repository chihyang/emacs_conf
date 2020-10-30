;;; night-mode.el --- keep silent at night

;; Author: PiotrMieszkowski
;; Version: 1.0
;; Keywords: ding, control
;; URL: http://www.emacswiki.org/emacs/AlarmBell

;;; Commentary:

;; This package forbids dings at night.
;; You can change the value of night-start and night-end to control the
;; period of the night mode.

;;; Code:

(defvar night-start 21
  "The hour that people go to sleep.")
(defvar night-end 8
  "The hour that people wake up.")
(defvar noon-start 11
  "The hour that people go to sleep.")
(defvar noon-end 16
  "The hour that people wake up.")

(defun night-mode-p ()
  "Check if it is night."
  (let ((hr (nth 2 (decode-time (current-time)))))
    (not (or (and (>= hr night-end)
                  (< hr noon-start))
             (and (>= hr noon-end)
                  (< hr night-start))))))
(defun night-mode-check-bell-time ()
  "Check if bell should be turned off."
  (if (display-graphic-p)
      (setq ring-bell-function
            (lambda ()
              (unless (night-mode-p)
                (ding))))
    (setq ring-bell-function 'ignore)))

(defvar night-mode-bell-timer (run-at-time "1 hour" 1 #'night-mode-check-bell-time)
  "Timer to run bell check every 1 hour.")

(provide 'night-mode)
;;; night-mode.el ends here
