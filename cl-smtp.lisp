;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005/2006/2007 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: cl-smtp.lisp
;;; Description: main smtp client logic

(in-package :cl-smtp)

(defparameter *content-type* "text/plain; charset=ISO-8859-1")

(defparameter *x-mailer* (format nil "(~A ~A)" 
				 (lisp-implementation-type)
				 (lisp-implementation-version)))

(defun check-arg (arg name)
  (cond
   ((or (stringp arg)
        (pathnamep arg))
    (list arg))
   ((listp arg)
    arg)
   (t
    (error "the \"~A\" argument is not a string or cons" name))))

(defun mask-dot (str)
  "replace \r\n.\r\n with \r\n..\r\n"
  (let ((dotstr (format nil "~C~C.~C~C" #\Return #\NewLine
			#\Return #\NewLine))
	(maskdotsr (format nil "~C~C..~C~C" #\Return #\NewLine
			#\Return #\NewLine))
	(resultstr ""))
    (labels ((mask (tempstr)
	       (let ((n (search dotstr tempstr)))
		 (cond
		  (n
		   (setf resultstr (concatenate 'string resultstr 
						(subseq tempstr 0 n)
						maskdotsr))
		   (mask (subseq tempstr (+ n 5))))
		  (t
		   (setf resultstr (concatenate 'string resultstr 
						tempstr)))))))
      (mask str))
    resultstr))

(defun string-to-base64-string (str)
  (declare (ignorable str))
  #+allegro (excl:string-to-base64-string str)
  #-allegro (cl-base64:string-to-base64-string str))


(defun send-email (host from to subject message 
		   &key (port 25) cc bcc reply-to extra-headers
		   html-message display-name authentication
		   attachments (buffer-size 256) ssl)
  (send-smtp host from (check-arg to "to") subject (mask-dot message)
	     :port port :cc (check-arg cc "cc") :bcc (check-arg bcc "bcc")
	     :reply-to reply-to 
	     :extra-headers extra-headers
	     :html-message html-message
	     :display-name display-name
	     :authentication authentication
	     :attachments (check-arg attachments "attachments")
	     :buffer-size (if (numberp buffer-size) 
			      buffer-size
			      256)
	     :ssl ssl))


(defun send-smtp (host from to subject message 
		  &key (port 25) cc bcc reply-to extra-headers html-message 
		  display-name authentication attachments buffer-size ssl)
  (let* ((sock (usocket:socket-stream (usocket:socket-connect host port)))
	 (boundary (make-random-boundary))
	 (html-boundary (if (and attachments html-message)
			    (make-random-boundary)
			    boundary)))
    (unwind-protect
	 (let ((stream (open-smtp-connection sock 
					     :authentication authentication 
					     :ssl ssl)))
	   (send-smtp-headers stream :from from :to to :cc cc :bcc bcc 
			      :reply-to reply-to
			      :display-name display-name 
			      :extra-headers extra-headers :subject subject)
	   (when (or attachments html-message)
	     (send-multipart-headers 
	      stream :attachment-boundary (when attachments boundary) 
	      :html-boundary html-boundary))
	   ;;----------- Send  the body Message ---------------------------
	   ;;--- Send the proper headers depending on plain-text, 
	   ;;--- multi-part or html email 
	   (cond ((and attachments html-message)
		  ;; if both present, start attachment section, 
		  ;; then define alternative section, 
		  ;; then write alternative header
		  (progn 
		    (generate-message-header 
		     stream :boundary boundary :include-blank-line? nil)
		    (generate-multipart-header stream html-boundary 
					       :multipart-type "alternative")
		    (write-blank-line stream)
		    (generate-message-header 
		     stream :boundary html-boundary :content-type *content-type* 
		     :content-disposition "inline" :include-blank-line? nil)))
		 (attachments 
		  (generate-message-header 
		   stream :boundary boundary 
		   :content-type *content-type* :content-disposition "inline"
		   :include-blank-line? nil))
		 (html-message
		  (generate-message-header 
		   stream :boundary html-boundary :content-type *content-type* 
		   :content-disposition "inline"))
		 (t 
		  (generate-message-header stream :content-type *content-type*
					   :include-blank-line? nil)))
	   (write-blank-line stream)
	   (write-to-smtp stream message)
	   (write-blank-line stream)
	   ;;---------- Send  Html text if needed -------------------------
	   (when html-message
	     (generate-message-header 
	      stream :boundary html-boundary 
	      :content-type "text/html; charset=ISO-8859-1" 
	      :content-disposition "inline")
	     (write-to-smtp stream html-message)
	     (send-end-marker stream html-boundary))
	   ;;---------- Send Attachments -----------------------------------
	   (when attachments
	     (dolist (attachment attachments)
	       (send-attachment stream attachment boundary buffer-size))
	     (send-end-marker stream boundary))
	   (write-char #\. stream)
	   (write-blank-line stream)
	   (force-output stream)
	   (multiple-value-bind (code msgstr)
	       (read-from-smtp stream)
	     (when (/= code 250)
	       (error "Message send failed: ~A" msgstr)))
	   (write-to-smtp stream "QUIT")
	   (multiple-value-bind (code msgstr)
	       (read-from-smtp stream)
	     (when (/= code 221)
	       (error "in QUIT command:: ~A" msgstr))))      
      (close sock))))

(defun open-smtp-connection (stream &key authentication ssl)
  (multiple-value-bind (code msgstr)
      (read-from-smtp stream)
    (when (/= code 220)
      (error "wrong response from smtp server: ~A" msgstr)))
  (when ssl
    (write-to-smtp stream (format nil "EHLO ~A" 
				  (usocket::get-host-name)))
    (multiple-value-bind (code msgstr lines)
	(read-from-smtp stream)
      (when (/= code 250)
	(error "wrong response from smtp server: ~A" msgstr))
      (when ssl
	(cond
	  ((find "STARTTLS" lines :test #'equal)
	   (print-debug "this server supports TLS")
	   (write-to-smtp stream "STARTTLS")
	   (multiple-value-bind (code msgstr)
	       (read-from-smtp stream)
	     (when (/= code 220)
	       (error "Unable to start TLS: ~A" msgstr))
	     (setf stream 
		   #+allegro (socket:make-ssl-client-stream stream)
		   #-allegro
		   (let ((s stream))
		     (cl+ssl:make-ssl-client-stream 
		      (cl+ssl:stream-fd stream)
		      :close-callback (lambda () (close s)))))
	     #-allegro
	     (setf stream (flexi-streams:make-flexi-stream 
			   stream
			   :external-format 
			   (flexi-streams:make-external-format 
			    :latin-1 :eol-style :lf)))))
	  (t
	   (error "this server does not supports TLS"))))))
  (cond
    (authentication
     (write-to-smtp stream (format nil "EHLO ~A" 
				   (usocket::get-host-name)))
     (multiple-value-bind (code msgstr)
	 (read-from-smtp stream)
       (when (/= code 250)
	 (error "wrong response from smtp server: ~A" msgstr)))
     (cond
       ((eq (car authentication) :plain)
	(write-to-smtp stream (format nil "AUTH PLAIN ~A" 
				      (string-to-base64-string
				       (format nil "~A~C~A~C~A" 
					       (cadr authentication)
					       #\null (cadr authentication) 
					       #\null
					       (caddr authentication)))))
	(multiple-value-bind (code msgstr)
	    (read-from-smtp stream)
	  (when (/= code 235)
	    (error "plain authentication failed: ~A" msgstr))))
       ((eq (car authentication) :login)
	(write-to-smtp stream "AUTH LOGIN")
	(multiple-value-bind (code msgstr)
	    (read-from-smtp stream)
	  (when (/= code 334)
	    (error "login authentication failed: ~A" msgstr)))
	(write-to-smtp stream (string-to-base64-string (cadr authentication)))
	(multiple-value-bind (code msgstr)
	    (read-from-smtp stream)
	  (when (/= code 334)
	    (error "login authentication send username failed: ~A" msgstr)))
	(write-to-smtp stream (string-to-base64-string (caddr authentication)))
	(multiple-value-bind (code msgstr)
	    (read-from-smtp stream)
	  (when (/= code 235)
	    (error "login authentication send password failed: ~A" msgstr))))
       (t
	(error "authentication ~A is not supported in cl-smtp" 
	       (car authentication)))))
    (t
     (write-to-smtp stream (format nil "HELO ~A" (usocket::get-host-name)))
     (multiple-value-bind (code msgstr)
	 (read-from-smtp stream)
       (when (/= code 250)
	 (error "wrong response from smtp server: ~A" msgstr)))))
  stream)
  
(defun send-smtp-headers (stream 
			  &key from to  cc bcc reply-to 
			  extra-headers display-name subject)
  (write-to-smtp stream 
		 (format nil "MAIL FROM:~@[~A ~]<~A>" display-name from))
  (multiple-value-bind (code msgstr)
      (read-from-smtp stream)
    (when (/= code 250)
      (error "in MAIL FROM command: ~A" msgstr)))
  (compute-rcpt-command stream to)
  (compute-rcpt-command stream cc)
  (compute-rcpt-command stream bcc)
  (write-to-smtp stream "DATA")
  (multiple-value-bind (code msgstr)
      (read-from-smtp stream)
    (when (/= code 354)
      (error "in DATA command: ~A" msgstr)))
  (write-to-smtp stream (format nil "Date: ~A" (get-email-date-string)))
  (write-to-smtp stream (format nil "From: ~@[~A <~]~A~@[>~]" 
				display-name from display-name))
  (write-to-smtp stream (format nil "To: ~{ ~a~^,~}" to))
  (when cc
    (write-to-smtp stream (format nil "Cc: ~{ ~a~^,~}" cc)))
  (write-to-smtp stream (format nil "Subject: ~A" subject))
  (write-to-smtp stream (format nil "X-Mailer: cl-smtp ~A" 
				*x-mailer*))
  (when reply-to
    (write-to-smtp stream (format nil "Reply-To: ~A" reply-to)))
  (when (and extra-headers
	     (listp extra-headers))
    (dolist (l extra-headers)
      (write-to-smtp stream 
		     (format nil "~A: ~{~a~^,~}" (car l) (rest l)))))
  (write-to-smtp stream "Mime-Version: 1.0"))

(defun send-multipart-headers (stream &key attachment-boundary html-boundary)
  (cond (attachment-boundary
	 (generate-multipart-header stream attachment-boundary 
				    :multipart-type "mixed"))
	(html-boundary (generate-multipart-header 
			stream html-boundary 
			:multipart-type "alternative"))
	(t nil)))

(defun compute-rcpt-command (stream adresses)
  (dolist (to adresses)
    (write-to-smtp stream (format nil "RCPT TO:<~A>" to))
    (multiple-value-bind (code msgstr)
	(read-from-smtp stream)
      (when (/= code 250)	
	(error "in RCPT TO command: ~A" msgstr)))))

(defun write-to-smtp (stream command)
  (print-debug (format nil "to server: ~A" command)) 
  (write-string command stream)
  (write-char #\Return stream)
  (write-char #\NewLine stream)
  (force-output stream))

(defun write-blank-line (stream)
  (write-char #\Return stream)
  (write-char #\NewLine stream)
  (force-output stream))

(defun read-from-smtp (stream &optional lines)
  (let* ((line (read-line stream))
	 (response (string-trim '(#\Return #\NewLine) (subseq line 4)))
	 (response-code (parse-integer line :start 0 :junk-allowed t)))
    (print-debug (format nil "from server: ~A" line))
    (if (= (char-code (elt line 3)) (char-code #\-))
	(read-from-smtp stream (append lines (list response)))
	(values response-code line lines))))

(defun get-email-date-string ()
  (multiple-value-bind (sec min h d m y wd) (get-decoded-time)
    (let* ((month (elt '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (- m 1)))
	   (weekday (elt '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") wd))
	   (timezone (get-timezone-from-integer
		      (- (encode-universal-time sec min h d m y 0)
			 (get-universal-time)))))
      (format nil "~A, ~2,'0d ~A ~d ~2,'0d:~2,'0d:~2,'0d ~D" 
	      weekday d month y h min sec timezone))))

(defun get-timezone-from-integer (x)
  (let ((min (/ x 60))
	(hour (/ x 3600)))
    (if (integerp hour)
	 (cond
	  ((>= hour 0)
	   (format nil "+~2,'0d00" hour))
	  ((< hour 0)
	   (format nil "-~2,'0d00" (* -1 hour))))
      (multiple-value-bind (h m) (truncate min 60)
	(cond
	  ((>= hour 0)
	   (format nil "+~2,'0d~2,'0d" h (truncate m)))
	  ((< hour 0)
	   (format nil "-~2,'0d~2,'0d" (* -1 h) (* -1 (truncate m)))))))))
