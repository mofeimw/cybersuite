(defun level-up-read-file ()
  "Read the level.up file from home directory."
  (let ((file-path (expand-file-name "level.up" "~")))
    (message "Reading from %s" file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

(defun level-up-parse-habit-line (line)
  "Parse a habit declaration line like 'run [5]' into (habit-name target)."
  (when (string-match "\\([^[]+\\)\\[\\([0-9]+\\)\\]" line)
    (let ((name (string-trim (match-string 1 line)))
          (target (string-to-number (match-string 2 line))))
      (message "Found habit: %s with target %d" name target)
      (list name target))))

(defun level-up-parse-date (date-str)
  "Parse a date string in format MM.DD.YYYY into a time value."
  (when (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" date-str)
    (encode-time 0 0 0
                 (string-to-number (match-string 2 date-str))
                 (string-to-number (match-string 1 date-str))
                 (string-to-number (match-string 3 date-str)))))

(defun level-up-get-week-key (time)
  "Get a string key for the week containing TIME."
  (format-time-string "%Y-W%V" time))

(defun level-up-group-dates-by-week (dates)
  "Group DATES by week, returning alist of (week-key . completion-count)."
  (let ((weeks (make-hash-table :test 'equal)))
    (dolist (date dates)
      (let ((week (level-up-get-week-key date)))
        (puthash week (1+ (gethash week weeks 0)) weeks)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) weeks)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun level-up-calculate-level (habit-data)
  "Calculate level for HABIT-DATA based on weekly completions."
  (let* ((dates (plist-get (cdr habit-data) :dates))
         (target (plist-get (cdr habit-data) :target))
         (level 0)
         (weeks (sort (level-up-group-dates-by-week dates)
                      (lambda (a b) (string< (car a) (car b)))))
         last-week)

    (message "\nCalculating level for %s" (car habit-data))
    (dolist (week weeks)
      (let* ((week-key (car week))
             (count (cdr week))
             (week-num (string-to-number (substring week-key 6))))

        ;; Check for gap with previous week
        (when (and last-week
                   (> (- week-num
                         (string-to-number (substring last-week 6)))
                      1))
          (message "Gap detected after week %s" last-week)
          (when (> level 0)
            (setq level (1- level))
            (message "Level down to %d due to gap" level)))

        ;; Process current week
        (message "Week %s: %d/%d completions" week-key count target)
        (if (>= count target)
            (progn
              (setq level (1+ level))
              (message "Level up! Now level %d" level))
          (when (> level 0)
            (setq level (1- level))
            (message "Level down to %d" level)))

        (setq last-week week-key)))
    level))

(defun level-up-parse-habits ()
  "Parse the level.up file and return an alist of habits and their data."
  (message "Starting habit parsing...")
  (let ((content (level-up-read-file))
        (habits '())
        current-habit)
    (dolist (line (split-string content "\n" t))
      (cond
       ((string-match "\\[" line)
        (let ((habit-data (level-up-parse-habit-line line)))
          (when habit-data
            (setq current-habit (car habit-data))
            (push (list current-habit
                        :target (cadr habit-data)
                        :dates nil)
                  habits))))
       ((and current-habit
             (string-match "[0-9]\\{1,2\\}\\.[0-9]\\{1,2\\}\\.[0-9]\\{4\\}" line))
        (let* ((date (level-up-parse-date (string-trim line)))
               (habit (assoc current-habit habits))
               (dates (plist-get (cdr habit) :dates)))
          (when date
            (setq dates (cons date dates))
            (setcdr habit (plist-put (cdr habit) :dates dates)))))))
    (nreverse habits)))

(defun level-up-get-week-progress (habit-data)
  "Calculate current week's progress for HABIT-DATA."
  (let* ((dates (plist-get (cdr habit-data) :dates))
         (target (plist-get (cdr habit-data) :target))
         (current-week (level-up-get-week-key (current-time)))
         (this-week-count
          (cdr (assoc current-week (level-up-group-dates-by-week dates)))))
    (list (or this-week-count 0) target)))

(defun level-up-make-progress-svg (current target)
  "Create an SVG progress bar showing CURRENT out of TARGET."
  (let* ((svg-width 500)
         (svg-height 16)
         (block-gap 4)
         (total-gaps (* block-gap (1- target)))
         (block-width (/ (- svg-width total-gaps) target))
         (y-padding 0)
         (block-height svg-height)
         (overflow (max 0 (- current target)))
         (overflow-gaps (* block-gap overflow))
         (total-width (+ svg-width
                         (* block-width overflow)
                         overflow-gaps))
         (completed-width (- (* target (+ block-width block-gap)) block-gap)))

    (let ((svg (svg-create total-width svg-height)))
      ;; Background
      (if (>= current target)
          (svg-rectangle svg 0 y-padding
                         completed-width
                         block-height
                         :fill "#edf2f7")
        (dotimes (i target)
          (let ((x (* i (+ block-width block-gap))))
            (svg-rectangle svg x y-padding block-width block-height
                           :fill "#edf2f7"))))

      ;; Progress
      (if (>= current target)
          (svg-rectangle svg 0 y-padding
                         completed-width
                         block-height
                         :fill "#32a860")
        (dotimes (i (min current target))
          (let ((x (* i (+ block-width block-gap))))
            (svg-rectangle svg x y-padding block-width block-height
                           :fill "#4299e1"))))

      ;; Overflow blocks
      (when (> current target)
        (dotimes (i overflow)
          (let ((x (+ completed-width block-gap
                      (* i (+ block-width block-gap)))))
            (svg-rectangle svg x y-padding block-width block-height
                           :fill "#9f7aea"))))

      (svg-image svg :ascent 'center))))

(defun level-up-render-buffer ()
  "Render the level-up buffer contents."
  (let ((habits (level-up-parse-habits))
        (inhibit-read-only t))
    (erase-buffer)
    (insert "\n  level_up\n\n")

    (dolist (habit habits)
      (let* ((name (car habit))
             (progress (level-up-get-week-progress habit))
             (current (car progress))
             (target (cadr progress))
             (level (level-up-calculate-level habit)))
        (insert (format "  %s [level %d]\n  "
                        (downcase name)
                        level))
        (insert-image (level-up-make-progress-svg current target))
        (insert (format "  %d/%d\n\n" current target))))))

(defun level-up ()
  "Open the Level Up habit tracker buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Level Up*")))
    (with-current-buffer buffer
      (org-mode)
      (setq-local display-line-numbers nil)
      (read-only-mode 1)
      (face-remap-add-relative 'default :family "Helvetica")
      (dolist (face '(org-level-1 org-level-2 org-level-3))
        (face-remap-add-relative face :family "Helvetica"))
      (level-up-render-buffer))
    (switch-to-buffer buffer)))

(provide 'level_up)
