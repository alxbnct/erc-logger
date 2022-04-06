;;                     _                             
;;   ___ _ __ ___     | | ___   __ _  __ _  ___ _ __ 
;;  / _ \ '__/ __|____| |/ _ \ / _` |/ _` |/ _ \ '__|
;; |  __/ | | (_|_____| | (_) | (_| | (_| |  __/ |   
;;  \___|_|  \___|    |_|\___/ \__, |\__, |\___|_|   
;;                             |___/ |___/           

(require 'erc)
(require 'cl)

(defgroup erc-logger nil
  "erc-logger customizations."
  :group 'erc
  :package-version '(erc-logger . "0.3"))

(defcustom *erc-logger-log-directory*
  "~/erc/log"
  "This directory is used for storing messages of the current day,
   it is recommanded to be set on an SSD. "
  :type 'string
  :group 'erc-logger)

(defcustom *erc-logger-log-other-directory*
  "~/erc/otherlog"
  "This directory is used for storing all compressed messages organized by date,
   it is recommanded to be set on an HDD. "
  :type 'string
  :group 'erc-logger)

(defcustom *erc-logger-log-interval*
  10
  "The interval (second) to run `erc-log-buffers' repeatedly."
  :type 'float
  :group 'erc-logger)

(defvar *erc-logger-log-timer* nil)
(defvar *erc-logger-log-date* nil)
(defvar *erc-logger-log-todays-date* nil)
(defvar *erc-logger-irc-buffer-size-map*
  (make-hash-table :test 'equal))

(defun erc-logger-write-file-immut (filename)
  (let ((cur-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer cur-buffer)
      (unless inhibit-read-only
	(setq-local inhibit-read-only t)
	(remove-text-properties (point-min) (point-max) 'read-only))
      (goto-char (point-max))
      (search-backward "ERC> ")
      (delete-region (line-beginning-position) (line-end-position))
      (write-file filename))))

(defun erc-logger-end-of-messages ()
  (save-excursion
    (goto-char (point-max))
    (previous-line)
    (end-of-line)
    (end-of-line)
    (end-of-line)
    (point)))

(defun erc-logger-log-start ()
  (interactive)
  (if (and *erc-logger-log-directory* *erc-logger-log-other-directory*)
      (setq *erc-logger-log-directory* (file-name-as-directory *erc-logger-log-directory*)
	    *erc-logger-log-other-directory* (file-name-as-directory *erc-logger-log-other-directory*))
    (error "Invalid directory name, please set variable `*erc-logger-log-directory*' and `*erc-logger-log-other-directory*' properly."))
  (unless (and (file-exists-p *erc-logger-log-directory*)
	       (file-exists-p *erc-logger-log-other-directory*))
    (mkdir *erc-logger-log-directory* "-p")
    (mkdir *erc-logger-log-other-directory* "-p"))
  ;; initialize hash table
  (dolist (erc-buffer (erc-buffer-list))
    (save-excursion
      (switch-to-buffer erc-buffer)
      (setq-local inhibit-read-only t)
      (remove-text-properties (point-min) (point-max) 'read-only)
      (unless (gethash erc-buffer *erc-logger-irc-buffer-size-map*)
	(puthash erc-buffer (erc-logger-end-of-messages)
		 *erc-logger-irc-buffer-size-map*))))
  (setq *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d")
	*erc-logger-log-timer* (run-at-time "1 sec" *erc-logger-log-interval* #'erc-log-buffers)))

(defun erc-logger-log-stop ()
  (interactive)
  (when *erc-logger-log-timer*
    (cancel-timer *erc-logger-log-timer*)))

(defun erc-log-buffers ()
  (if (and *erc-logger-log-directory*
	   (file-directory-p *erc-logger-log-directory*))
      (progn (save-excursion
	       (dolist (erc-buffer (erc-buffer-list))
		 (switch-to-buffer erc-buffer)
		 (let* ((file-name (concat (buffer-name erc-buffer)
					   (datetime-format "_%Y-%m-%d.txt")))
			(file-full-path (concat *erc-logger-log-directory*
						"/" file-name))
			(current-message-point (gethash erc-buffer *erc-logger-irc-buffer-size-map*))
			(end-of-message-point (erc-logger-end-of-messages)))
		   (cl-flet ((save-buffer-graceful
			      nil (if (file-exists-p file-full-path)
				      (when (not (= end-of-message-point current-message-point))
					(append-to-file current-message-point end-of-message-point file-full-path))
				    (erc-logger-write-file-immut file-full-path))
			      (when (not (= end-of-message-point current-message-point))
				(puthash erc-buffer end-of-message-point *erc-logger-irc-buffer-size-map*)))
			     (buffer-read-write
			      ()
			      (setq-local inhibit-read-only t)
			      (remove-text-properties (point-min) (point-max) 'read-only))
			     (buffer-read-only
			      ()
			      (setq-local inhibit-read-only nil))
			     (clear-previous-days-messages
			      ()
			      (delete-region (point-min) end-of-message-point)))
		     (if (string= *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d"))
			 (save-buffer-graceful)

		       ;; compress log files and mv them to another directory on next day
		       ;; and clear the buffer, save to new files
		       (progn (save-buffer-graceful)
			      (buffer-read-write)
			      (clear-previous-days-messages)
			      (buffer-read-only)
			      (if (directory-name-p *erc-logger-log-other-directory*)
				  (let* ((dir-name (concat *erc-logger-log-other-directory*
							   (datetime-format "%Y-%m-%d") "/")))
				    (cl-flet ((transfer-file ()
							     (rename-file file-full-path dir-name t)
							     (unless
								 (= 0 (shell-command
								       (concat "gzip -9f "
									       dir-name
									       file-name)))
							       (message "failed to compress file!"))))
				      (if (file-exists-p dir-name)
					  (transfer-file)
					(progn (make-directory dir-name)
					       (when (file-exists-p dir-name)
						 (transfer-file))))))
				(error "Invalid directory name, please set variable `*erc-logger-log-other-directory*' properly.")))
		       )))))
	     (unless (string= *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d"))
	       (setq *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d"))))
    (error "Invalid directory name, please set variable `*erc-logger-log-directory*' properly.")))

(provide 'erc-logger)
