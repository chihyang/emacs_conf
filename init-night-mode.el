;;; init-night-mode.el --- keep silent at night

;; Author: PiotrMieszkowski
;; Version: 1.0
;; Keywords: ding, control
;; URL: http://www.emacswiki.org/emacs/AlarmBell

;;; Commentary:

;; This package forbids dings at night.
;; You can change the value of night-start and night-end to control the 
;; period of the night mode.
(setq ring-bell-function 
      (lambda ()
        (unless (night-mode)
          (ding))))
(defvar night-start 21
  "The hour that people go to sleep.")
(defvar night-end 8
  "The hour that people wake up.")
(defvar noon-start 11
  "The hour that people go to sleep.")
(defvar noon-end 16
  "The hour that people wake up.")  
(defun night-mode ()
  "Check if it is night."
  (let ((hr (nth 2 (decode-time (current-time)))))
    (or (< hr night-end)
        (> hr night-start)
        (and (< hr noon-end) (> hr noon-start)))))
