;;                     _                             
;;   ___ _ __ ___     | | ___   __ _  __ _  ___ _ __ 
;;  / _ \ '__/ __|____| |/ _ \ / _` |/ _` |/ _ \ '__|
;; |  __/ | | (_|_____| | (_) | (_| | (_| |  __/ |   
;;  \___|_|  \___|    |_|\___/ \__, |\__, |\___|_|   
;;                             |___/ |___/           

(require 'erc)
(require 'cl-lib)

(defgroup erc-logger nil
  "erc-logger customizations."
  :group 'erc
  :package-version '(erc-logger . "0.3"))

(defcustom erc-logger-log-directory
  "~/erc/log"
  "This directory is used for storing messages of the current day,
   it is recommanded to be set on an SSD. "
  :type 'string
  :group 'erc-logger)

(defcustom erc-logger-log-other-directory
  "~/erc/otherlog"
  "This directory is used for storing all compressed messages organized by date,
   it is recommanded to be set on an HDD. "
  :type 'string
  :group 'erc-logger)

(defcustom erc-logger-log-begin-time
  "30 sec"
  "Time when to run `erc-logger-log-start' at.
   To specify a relative time as a string, use numbers followed by units. For
   example:
   
   ‘1 min’
      
   denotes 1 minute from now.
      
   ‘1 min 5 sec’
      
   denotes 65 seconds from now.
      
   ‘1 min 2 sec 3 hour 4 day 5 week 6 fortnight 7 month 8 year’
       
   denotes exactly 103 months, 123 days, and 10862 seconds from now.

   For more information, please read the manual at https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html#index-run_002dat_002dtime"
  :type 'string
  :group 'erc-logger)

(defcustom erc-logger-log-interval
  10
  "The interval (second) to run `erc-log-buffers' repeatedly."
  :type 'float
  :group 'erc-logger)

(defvar erc-logger-log-timer nil)
(defvar erc-logger-log-date nil)
(defvar erc-logger-log-todays-date nil)
(defvar erc-logger-irc-buffer-size-map nil)

(defun erc-logger-write-file-immut (filename)
  (let ((cur-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer cur-buffer)
      (unless inhibit-read-only
	(setq-local inhibit-read-only t))
      (goto-char (point-max))
      (search-backward "ERC> ")
      (delete-region (line-beginning-position) (line-end-position))
      (write-file filename))))

(defun erc-logger-end-of-messages ()
  (save-excursion
    (goto-char (point-max))
    (search-backward-regexp "\nERC>[^z-a]*?")
    (point)))

(defun erc-logger-log-start ()
  (interactive)
  (if (and erc-logger-log-directory erc-logger-log-other-directory)
      (setq erc-logger-log-directory (file-name-as-directory erc-logger-log-directory)
	    erc-logger-log-other-directory (file-name-as-directory erc-logger-log-other-directory))
    (error "Invalid directory name, please set variable `erc-logger-log-directory' and `erc-logger-log-other-directory' properly."))
  (unless (and (file-exists-p erc-logger-log-directory)
	       (file-exists-p erc-logger-log-other-directory))
    (mkdir erc-logger-log-directory t)
    (mkdir erc-logger-log-other-directory t))
  ;; initialize hash table
  (setq erc-logger-irc-buffer-size-map (make-hash-table :test 'equal))
  (dolist (erc-buffer (erc-buffer-list))
    (save-excursion
      (with-current-buffer erc-buffer
	(unless (gethash erc-buffer erc-logger-irc-buffer-size-map)
	  (puthash erc-buffer (erc-logger-end-of-messages)
		   erc-logger-irc-buffer-size-map)))))
  (setq erc-logger-log-todays-date (format-time-string "%Y-%m-%d")
	erc-logger-log-timer (run-at-time erc-logger-log-begin-time erc-logger-log-interval #'erc-log-buffers)))

(defun erc-logger-log-stop ()
  (interactive)
  (when erc-logger-log-timer
    (cancel-timer erc-logger-log-timer)))

(defun erc-log-buffers ()
  (if (and erc-logger-log-directory
	   (file-directory-p erc-logger-log-directory))
      (progn (save-excursion
	       (dolist (erc-buffer (erc-buffer-list))
		 (with-current-buffer erc-buffer
		   (let* ((file-name (concat (buffer-name erc-buffer)
					     "_" erc-logger-log-todays-date ".txt"))
			  (file-full-path (concat erc-logger-log-directory
						  "/" file-name))
			  (current-message-point (gethash erc-buffer erc-logger-irc-buffer-size-map))
			  (end-of-message-point (erc-logger-end-of-messages)))
		     (cl-flet ((save-buffer-graceful
				nil (unless current-message-point
				      (let ((new-current-message-point (erc-logger-end-of-messages)))
					(puthash erc-buffer new-current-message-point
						 erc-logger-irc-buffer-size-map)
					(setq current-message-point new-current-message-point)))
				(if (file-exists-p file-full-path)
				    (when (not (= end-of-message-point current-message-point))
				      (append-to-file current-message-point end-of-message-point file-full-path))
				  (erc-logger-write-file-immut file-full-path))
				(when (not (= end-of-message-point current-message-point))
				  (puthash erc-buffer end-of-message-point erc-logger-irc-buffer-size-map)))
			       (buffer-read-write nil
						  (setq-local inhibit-read-only t))
			       (buffer-read-only nil
						 (setq-local inhibit-read-only nil))
			       (clear-previous-days-messages
				nil
				(delete-region (point-min) current-message-point)
				(setq end-of-message-point (erc-logger-end-of-messages)
				      current-message-point end-of-message-point)
				(puthash erc-buffer end-of-message-point erc-logger-irc-buffer-size-map)))
		       (if (string= erc-logger-log-todays-date (format-time-string "%Y-%m-%d"))
			   (save-buffer-graceful)

			 ;; compress log files and mv them to another directory on next day
			 ;; and clear the buffer, save to new files
			 (progn (save-buffer-graceful)
			   (if (directory-name-p erc-logger-log-other-directory)
			       (let* ((dir-name (file-name-as-directory (concat erc-logger-log-other-directory
										erc-logger-log-todays-date))))
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
			     (error "Invalid directory name, please set variable `erc-logger-log-other-directory' properly."))
			   (buffer-read-write)
			   (clear-previous-days-messages)
			   (buffer-read-only))
			 ))))))
	     (unless (string= erc-logger-log-todays-date (format-time-string "%Y-%m-%d"))
	       (setq erc-logger-log-todays-date (format-time-string "%Y-%m-%d"))))
    (error "Invalid directory name, please set variable `erc-logger-log-directory' properly.")))

(provide 'erc-logger)
