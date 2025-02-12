(defvar justdoit-timer nil
  "Store the timer object.")

(defvar justdoit-start-time nil
  "Store the start time.")

(defvar justdoit-duration nil
  "Store the duration in minutes.")

(defvar justdoit-task-name nil
  "Store the task name.")

(defvar justdoit-buffer-name "*justdoit*"
  "Name of the timer buffer.")

(defun justdoit-get-progress-color (percent)
  "Get the color for progress bar based on PERCENT complete."
  (let ((remaining-percent (- 100 percent)))
    (cond
     ((>= remaining-percent 50) "#b4f9f8")  ; More than 50% remaining - cyan
     ((>= remaining-percent 20) "#e0af68")  ; Between 50% and 20% - orange
     (t "#f7768e"))))                       ; Less than 20% - red

(defun justdoit-create-progress-bar (percent width height)
  "Create SVG progress bar with PERCENT complete (0-100)."
  (let* ((svg (svg-create width height))
         (bar-width (* (/ (- 100 percent) 100.0) width)) ; Reverse for countdown
         (progress-color (justdoit-get-progress-color percent)))
    ;; Background
    (svg-rectangle svg 0 0 width height
                   :fill "#0c0a0d")
    ;; Progress bar
    (svg-rectangle svg 0 0 bar-width height
                   :fill progress-color)
    (svg-image svg :ascent 'center)))

(defun justdoit-format-time (seconds)
  "Format SECONDS into MM:SS string."
  (format "%02d:%02d"
          (floor (/ (ceiling seconds) 60))
          (floor (mod (ceiling seconds) 60))))

(defun justdoit-render-buffer ()
  "Update the timer display."
  (when (get-buffer justdoit-buffer-name)
    (with-current-buffer justdoit-buffer-name
      (let* ((elapsed (- (float-time) justdoit-start-time))
             (total-seconds (* justdoit-duration 60))
             (remaining (max 0 (- total-seconds elapsed)))
             (percent-complete (* (/ elapsed total-seconds) 100))
             (inhibit-read-only t))
        (erase-buffer)

        ;; Header
        (insert "\n    ")
        (let ((start-pos (point)))
          (insert "justdoit")
          (let ((end-pos (point)))
            (overlay-put (make-overlay start-pos end-pos)
                         'face '(:weight bold :slant italic))
            (put-text-property start-pos end-pos
                               'display '(height 3.2))))
        (insert "\n\n")

        ;; Task name and progress
        (insert (format "    %s\n    "
                        (downcase justdoit-task-name)))
        (insert-image (justdoit-create-progress-bar percent-complete 500 16))
        (insert (format "    %s\n"
                        (justdoit-format-time remaining)))


        ;; Check if timer is complete
        (when (<= remaining 0)
          (let ((timer justdoit-timer))  ; Store reference to timer
            (setq justdoit-timer nil)    ; Clear global timer first
            (when timer                  ; Only cancel if it exists
              (cancel-timer timer)))     ; Cancel using stored reference
          (save-excursion
            (goto-char (point-min))
            (when (search-forward justdoit-task-name nil t)
              (let ((inhibit-read-only t))
                (insert " [complete]"))))
          (when (fboundp 'helios/utility/notify)
            (helios/utility/notify (format "Task [%s] is over!" justdoit-task-name)))
          (message "Task [%s] is over!" justdoit-task-name))))))

(defun justdoit ()
  "Start a new timer with progress bar."
  (interactive)
  ;; Check if timer is already running
  (if justdoit-timer
      (progn
        (switch-to-buffer justdoit-buffer-name)
        (message "Task [%s] is still ongoing! Just do it." justdoit-task-name))
    (progn ; Only run this if no timer is active
      ;; Get task details
      (setq justdoit-task-name (read-string "Task name: ")
            justdoit-duration (read-number "Duration (minutes): ")
            justdoit-start-time (float-time))

      ;; Call heads-up UI executable if it exists
      (let ((executable-path (expand-file-name "~/justdoit")))
        (if (and (file-exists-p executable-path)
                 (file-executable-p executable-path))
            (start-process "test" nil executable-path justdoit-task-name (number-to-string justdoit-duration))))

      ;; Create and setup buffer
      (let ((buffer (get-buffer-create justdoit-buffer-name)))
        (with-current-buffer buffer
          (org-mode)
          (setq-local display-line-numbers nil)
          (read-only-mode 1)
          (face-remap-add-relative 'default :family "Exo")
          (dolist (face '(org-level-1 org-level-2 org-level-3))
            (face-remap-add-relative face :family "Exo"))
          (text-scale-set 1.3)
          (let ((inhibit-read-only t))
            (erase-buffer)))

        ;; Display buffer
        (switch-to-buffer buffer)

        ;; Start timer that updates every second
        (setq justdoit-timer
              (run-with-timer 0 1 'justdoit-render-buffer))

        ;; Initial display update
        (justdoit-render-buffer)))))

(provide 'justdoit)
