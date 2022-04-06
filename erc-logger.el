;;                     _                             
;;   ___ _ __ ___     | | ___   __ _  __ _  ___ _ __ 
;;  / _ \ '__/ __|____| |/ _ \ / _` |/ _` |/ _ \ '__|
;; |  __/ | | (_|_____| | (_) | (_| | (_| |  __/ |   
;;  \___|_|  \___|    |_|\___/ \__, |\__, |\___|_|   
;;                             |___/ |___/           

(defvar *erc-logger-log-directory* nil)
(defvar *erc-logger-log-other-directory* nil)
(defvar *erc-logger-log-timer* nil)
(defvar *erc-logger-log-date* nil)
(defvar *erc-logger-log-todays-date* nil)
(defvar *erc-logger-irc-buffer-size-map*
  (make-hash-table :test 'equal))

(defun write-file-immut (filename)
  (let ((cur-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer cur-buffer)
      (write-file filename))))

(defun erc-logger-log-start ()
  (interactive)
  (setq *erc-logger-log-directory* (file-name-as-directory *erc-logger-log-directory*)
	*erc-logger-log-other-directory* (file-name-as-directory *erc-logger-log-other-directory*))
  ;; initialize hash table
  (dolist (erc-buffer (erc-buffer-list))
    (with-current-buffer (current-buffer)
      (switch-to-buffer erc-buffer)
      (unless (gethash erc-buffer *erc-logger-irc-buffer-size-map*)
	(puthash erc-buffer (buffer-size) *erc-logger-irc-buffer-size-map*))))
  (setq *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d")
	*erc-logger-log-timer* (run-at-time "1 sec" 10 #'erc-log-buffers)))

(defun erc-logger-log-stop ()
  (interactive)
  (when *erc-logger-log-timer*
    (cancel-timer *erc-logger-log-timer*)))

(defun erc-log-buffers ()
  (if (and *erc-logger-log-directory*
	   (file-directory-p *erc-logger-log-directory*))
      (progn (with-current-buffer (current-buffer)
	       (dolist (erc-buffer (erc-buffer-list))
		 (switch-to-buffer erc-buffer)
		 (let* ((file-name (concat (buffer-name erc-buffer)
					   ;;(datetime-format "_%Y-%m-%d_%H:%M:%S.txt")
					   (datetime-format "_%Y-%m-%d.txt")))
			(file-full-path (concat *erc-logger-log-directory*
						"/" file-name))
			(buffer-size (buffer-size)))
		   (if (string= *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d"))
		       (progn
			 (when (not (= buffer-size (gethash erc-buffer *erc-logger-irc-buffer-size-map*)))
			   (write-file-immut file-full-path)
			   (puthash erc-buffer buffer-size *erc-logger-irc-buffer-size-map*)))

		     ;; compress log files and mv them to another directory on next day
		     ;; and clear the buffer, save to new files
		     (progn (write-file-immut file-full-path)
			    (if (directory-name-p *erc-logger-log-other-directory*)
				(let* ((dir-name (concat *erc-logger-log-other-directory*
							 (datetime-format "%Y-%m-%d") "/")))
				  (cl-flet ((transfer-file ()
							   (rename-file file-full-path dir-name t)
							   (unless
							       (= 0 (shell-command
								     (concat "gzip -9 -f "
									     dir-name
									     file-name)))
							     (message "failed to compress file!"))))
				    (if (file-exists-p dir-name)
					(transfer-file)
				      (progn (make-directory dir-name)
					     (when (file-exists-p dir-name)
					       (transfer-file))))))
			      (error "Invalid directory name, please set variable `*erc-logger-log-other-directory*' properly.")))
		     ))))
	     (unless (string= *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d"))
	       (setq *erc-logger-log-todays-date* (datetime-format "%Y-%m-%d"))))
    (error "Invalid directory name, please set variable `*erc-logger-log-directory*' properly.")))
