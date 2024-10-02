;;; nvidia-monitor.el --- NVIDIA GPU Monitoring in Emacs  -*- lexical-binding: t; -*-

;; Author: Ben + o1
;; Version: 0.1
;; Keywords: tools, hardware
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; This package provides a command `nvidia-monitor-start` to monitor NVIDIA GPUs
;; in an Emacs buffer. It displays GPU usage and memory information, updating
;; every X seconds (configurable). Data is also saved to a CSV file.

;;; Code:

(defgroup nvidia-monitor nil
  "Monitor NVIDIA GPUs in an Emacs buffer."
  :group 'tools)

(defcustom nvidia-monitor-update-interval 1
  "Time interval in seconds between updates."
  :type 'number
  :group 'nvidia-monitor)

(defcustom nvidia-monitor-max-rows 20
  "Maximum number of rows to display in the table."
  :type 'number
  :group 'nvidia-monitor)

(defcustom nvidia-monitor-tabulate-path  "/usr/bin"
  "Path to the Python `tabulate` module."
  :type 'string
  :group 'nvidia-monitor)

(defcustom nvidia-monitor-tabulate-format "pipe"
  "Table format to use with `tabulate`."
  :type 'string
  :group 'nvidia-monitor)

(defcustom nvidia-monitor-csv-file
  (expand-file-name "nvidia-monitor.csv" (or (getenv "XDG_RUNTIME_DIR") "/tmp"))
  "Path to the CSV file where data is saved."
  :type 'string
  :group 'nvidia-monitor)

(defvar nvidia-monitor--timer nil
  "Timer object for the update function.")

(defvar nvidia-monitor--buffer "*NVIDIA Monitor*"
  "Name of the buffer to display the GPU monitoring.")

(defvar nvidia-monitor--data nil
  "List to hold the collected GPU data.")

(defvar nvidia-monitor--gpu-info nil
  "List of static GPU information.")

(defvar nvidia-monitor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-c C-c") 'nvidia-monitor-quit)
    map)
  "Keymap for `nvidia-monitor-mode`.")

(define-derived-mode nvidia-monitor-mode special-mode "NVIDIA-Monitor"
  "Major mode for NVIDIA GPU monitoring."
  :keymap nvidia-monitor-mode-map
  (setq buffer-read-only t))

;;;###autoload
(defun nvidia-monitor-start ()
  "Start monitoring NVIDIA GPUs."
  (interactive)
  ;; Open a new buffer in the current window
  (switch-to-buffer nvidia-monitor--buffer)
  (nvidia-monitor-mode)
  ;; Clear existing data
  (setq nvidia-monitor--data nil)
  ;; Get static GPU information
  (nvidia-monitor--get-gpu-info)
  ;; Initialize CSV file
  (nvidia-monitor--init-csv-file)
  ;; Add kill-buffer-hook to stop timer if buffer is killed manually
  (add-hook 'kill-buffer-hook #'nvidia-monitor--buffer-killed nil t)
  ;; Start the timer
  (setq nvidia-monitor--timer
        (run-at-time 0 nvidia-monitor-update-interval #'nvidia-monitor--update)))

(defun nvidia-monitor-quit ()
  "Quit NVIDIA GPU monitoring."
  (interactive)
  ;; Cancel the timer
  (when nvidia-monitor--timer
    (cancel-timer nvidia-monitor--timer)
    (setq nvidia-monitor--timer nil))
  ;; Close the buffer
  (when (get-buffer nvidia-monitor--buffer)
    (kill-buffer nvidia-monitor--buffer))
  ;; Message about CSV file
  (message "NVIDIA monitor results stored in CSV file: %s" nvidia-monitor-csv-file))

(defun nvidia-monitor--buffer-killed ()
  "Handle the buffer being killed."
  (when nvidia-monitor--timer
    (cancel-timer nvidia-monitor--timer)
    (setq nvidia-monitor--timer nil))
  ;; Message about CSV file
  (message "NVIDIA monitor stopped; results stored in CSV file: %s" nvidia-monitor-csv-file))

(defun nvidia-monitor--get-gpu-info ()
  "Retrieve static GPU information."
  (setq nvidia-monitor--gpu-info
        (split-string
         (nvidia-monitor--shell-command-to-string "nvidia-smi --query-gpu=name --format=csv,noheader")
         "\n" t)))

(defun nvidia-monitor--shell-command-to-string (cmd)
  "Execute CMD and return its output as a string."
  (with-temp-buffer
    (call-process-shell-command cmd nil t)
    (string-trim (buffer-string))))

(defun nvidia-monitor--update ()
  "Update the GPU monitoring data."
  (if (not (buffer-live-p (get-buffer nvidia-monitor--buffer)))
      ;; Buffer is gone; stop the timer
      (nvidia-monitor-quit)
    ;; Proceed with update
    (let* ((cmd "nvidia-smi --query-gpu=memory.used,utilization.gpu --format=csv,noheader,nounits")
           (output (nvidia-monitor--shell-command-to-string cmd))
           (lines (split-string output "\n" t))
           (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
           (data-row (list timestamp)))
      ;; Parse each line (one per GPU)
      (dolist (line lines)
        (let* ((values (split-string line ","))
               (mem-used (string-trim (nth 0 values)))
               (gpu-util (string-trim (nth 1 values))))
          ;; Append to data-row
          (setq data-row
                (append data-row (list (concat mem-used "MiB")
                                       (concat gpu-util "%"))))))
      ;; Prepend to data list
      (push data-row nvidia-monitor--data)
      ;; Trim data to max rows
      (when (> (length nvidia-monitor--data) nvidia-monitor-max-rows)
        (setq nvidia-monitor--data (cl-subseq nvidia-monitor--data 0 nvidia-monitor-max-rows)))
      ;; Update the display
      (nvidia-monitor--refresh-display)
      ;; Append data to CSV file
      (nvidia-monitor--append-to-csv data-row))))

(defun nvidia-monitor--refresh-display ()
  "Refresh the display in the buffer."
  (with-current-buffer nvidia-monitor--buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Insert static info
      (insert "NVIDIA GPU Monitor\n")
      (insert "------------------\n")
      (insert (substitute-command-keys
               (format "Press `%s' to quit. Data is being saved to: %s\n\n"
                       (key-description (where-is-internal 'nvidia-monitor-quit nil t))
                       nvidia-monitor-csv-file)))
      (insert "GPU Info:\n")
      (dolist (gpu-name nvidia-monitor--gpu-info)
        (insert (format " - %s\n" gpu-name)))
      (insert "\n")
      ;; Prepare data for tabulate
      (let* ((headers (nvidia-monitor--generate-headers))
             (rows (mapcar (lambda (row) (cdr row)) nvidia-monitor--data))
             (table (nvidia-monitor--format-table headers rows)))
        ;; Insert table
        (insert table)))))

(defun nvidia-monitor--generate-headers ()
  "Generate table headers based on number of GPUs."
  (let* ((num-gpus (length nvidia-monitor--gpu-info))
         (headers '()))
    (dotimes (i num-gpus)
      (setq headers
            (append headers
                    (list (format "id %d mem" i)
                          (format "id %d usage" i)))))
    headers))

(defun nvidia-monitor--format-table (headers rows)
  "Format the table using Python tabulate.
HEADERS is a list of column headers.
ROWS is a list of data rows."
  (let* ((tabulate-path nvidia-monitor-tabulate-path)
         (tabulate-import (if (string-empty-p tabulate-path)
                              "from tabulate import tabulate"
                            (format "import sys; sys.path.append('%s'); from tabulate import tabulate" tabulate-path)))
         (tabulate-cmd (format "python3 -c \"%s; import csv; data = list(csv.reader(sys.stdin)); print(tabulate(data[1:], headers=data[0], tablefmt='%s'))\""
                               tabulate-import
                               nvidia-monitor-tabulate-format))
         (input-data (concat (string-join headers ",") "\n"
                             (mapconcat (lambda (row)
                                          (string-join row ","))
                                        rows "\n")))
         (output (nvidia-monitor--call-process-region tabulate-cmd input-data)))
    output))

(defun nvidia-monitor--call-process-region (cmd input)
  "Call a process CMD with INPUT string as stdin, and return output."
  (with-temp-buffer
    (insert input)
    (let ((exit-code (call-process-region (point-min) (point-max)
                                          shell-file-name t t nil
                                          shell-command-switch cmd)))
      (if (eq exit-code 0)
          (buffer-string)
        (error "Error running command: %s" cmd)))))

(defun nvidia-monitor--init-csv-file ()
  "Initialize the CSV file by writing headers."
  (let* ((headers (cons "Timestamp" (nvidia-monitor--generate-csv-headers)))
         (csv-line (string-join headers ",")))
    (with-temp-file nvidia-monitor-csv-file
      (insert csv-line "\n"))))

(defun nvidia-monitor--append-to-csv (data-row)
  "Append DATA-ROW to the CSV file."
  (let ((csv-line (string-join data-row ",")))
    (with-temp-buffer
      (insert csv-line "\n")
      (append-to-file (point-min) (point-max) nvidia-monitor-csv-file))))

(defun nvidia-monitor--generate-csv-headers ()
  "Generate CSV headers including timestamp."
  (let* ((num-gpus (length nvidia-monitor--gpu-info))
         (headers '()))
    (dotimes (i num-gpus)
      (setq headers
            (append headers
                    (list (format "id %d mem used" i)
                          (format "id %d usage" i)))))
    headers))

(provide 'nvidia-monitor)

;;; nvidia-monitor.el ends here
