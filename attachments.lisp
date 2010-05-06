;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-SMTP, the Lisp SMTP Client


;;; Copyright (C) 2004/2005/2006/2007/2008/2009/2010 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: attachments.lisp
;;; Description: encoding and transmitting login to include a mime attachment

;;;
;;; Contributed by Brian Sorg
;;;
;;; Thanks to David Cooper for make-random-boundary
;;;
(in-package :cl-smtp)

;;; Addition to allow for sending mime attachments along with the smtp message 

;;---- Initialize array of possible boundary characters to make start of attachments
(defparameter *boundary-chars*
  (let* ((chars (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m 
                      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M 
                      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (arr (make-array (length chars))))
    (dotimes (i (length chars) arr)
      (setf (aref arr i) (pop chars)))))

(defun make-random-boundary (&optional (length 30) 
			     (boundary-chars *boundary-chars*))
  (let ((boundary (make-string length))
	(prefix "_---------_")
        (chars-length (length boundary-chars)))
    (dotimes (i length (concatenate 'string prefix boundary))
      (setf (aref boundary i) 
	    (svref *boundary-chars* (random chars-length))))))

(defun generate-multipart-header (sock boundary &key (multipart-type "mixed"))
  (write-to-smtp sock 
                 (format nil "Content-type: multipart/~a;~%~tBoundary=\"~a\"" 
                         multipart-type boundary)))

(defun generate-message-header (sock 
				&key boundary ;; uniques string of character -- see make-random-boundary 
				content-type ;; "text/plain; charset=ISO-8859-1"
				content-disposition ;; inline attachment 
				content-transfer-encoding ;; 7 bit or 8 bit 
				(include-blank-line? t))
  (when boundary
    (write-to-smtp sock (format nil "--~a" boundary)))
  (when content-type 
    (write-to-smtp sock (format nil "Content-type: ~a" content-type)))
  (when content-disposition 
    (write-to-smtp sock (format nil "Content-Disposition: ~A" 
				content-disposition)))
  (when content-transfer-encoding 
    (write-to-smtp sock (format nil "Content-Transfer-Encoding: ~A" 
				content-transfer-encoding)))
  (when include-blank-line? (write-blank-line sock)))

(defun escape-rfc822-quoted-string (str)
  (with-output-to-string (s)
    (loop
       for c across str do
       (when (find (char-code c) '(10 13 92 34))
	 (write-char #\\ s))
       (write-char c s))))

(defun rfc2231-encode-string (string &key (external-format :utf-8))
  (with-output-to-string (s)
    (format s "~A''" (string-upcase (symbol-name external-format)))
    (loop for n across (string-to-octets string
                                         :external-format external-format)
          for c = (code-char n)
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        (find c "$-_.!*'()," :test #'char=))
                     (write-char c s))
                   ((char= c #\Space)
                     (write-char #\+ s))
                   (t (format s "%~2,'0x" (char-code c)))))))

(defun send-attachment-header (sock boundary attachment external-format)
  (let ((quoted-name
         (escape-rfc822-quoted-string 
          (rfc2045-q-encode-string (attachment-name attachment) 
                                   :external-format external-format)))
        (quoted-name*
	 (escape-rfc822-quoted-string 
          (rfc2231-encode-string (attachment-name attachment) 
                                 :external-format external-format))))
    (generate-message-header 
     sock 
     :boundary boundary
     :content-type (format nil "~A;~%~tname*=~A;~%~tname=~S"
			   (attachment-mime-type attachment)
			   quoted-name* quoted-name)
     :content-transfer-encoding "base64"
     :content-disposition (format nil "attachment; filename*=~A; filename=~S"
				  quoted-name* quoted-name))))

(defun send-end-marker (sock boundary)
  ;; Note the -- at beginning and end of boundary is required
  (write-to-smtp sock (format nil "~%--~a--~%" boundary)))

(defclass attachment ()
  ((name :initarg :name
	 :accessor attachment-name)
   (data-pathname :initarg :data-pathname
	 :accessor attachment-data-pathname)
   (mime-type :initarg :mime-type
	      :accessor attachment-mime-type)))

(defun make-attachment (data-pathname
			&key (name (file-namestring data-pathname))
			     (mime-type (lookup-mime-type name)))
  (make-instance 'attachment
		 :data-pathname data-pathname
		 :name name
		 :mime-type mime-type))

(defmethod attachment-name ((attachment pathname))
  (file-namestring attachment))

(defmethod attachment-data-pathname ((attachment pathname))
  attachment)

(defmethod attachment-mime-type ((attachment pathname))
  (lookup-mime-type (namestring attachment)))

(defmethod attachment-name ((attachment string))
  (file-namestring attachment))

(defmethod attachment-data-pathname ((attachment string))
  attachment)

(defmethod attachment-mime-type ((attachment string))
  (lookup-mime-type attachment))

(defun send-attachment (sock attachment boundary buffer-size external-format)
  (send-attachment-header sock boundary attachment external-format)
  (base64-encode-file (attachment-data-pathname attachment)
		      sock
		      :buffer-size buffer-size))

(defun base64-encode-file (file-in sock
                                   &key 
                                   (buffer-size 256) ;; in KB
                                   (wrap-at-column 70))
  "Encodes the file contents given by file-in, which can be of any form appropriate to with-open-file, and write the base-64 encoded version to sock, which is a socket.

Buffer-size, given in KB, controls how much of the file is processed and written to the socket at one time. A buffer-size of 0, processes the file all at once, regardless of its size. One will have to weigh the speed vs, memory consuption risks when chosing which way is best. 

Wrap-at-column controls where the encode string is divided for line breaks." 
    (when (probe-file file-in)
      ;;-- open filein ---------
      (with-open-file (strm-in file-in
                               :element-type '(unsigned-byte 8))
	(let* ((;; convert buffer size given to bytes 
                ;; or compute bytes based on file 
		max-buffer-size 
		(if (zerop buffer-size)
		    (file-length strm-in)
		    ;; Ensures 64 bit encoding is properly 
		    ;; divided so that filler 
		    ;; characters are not required between chunks
		    (* 24 (truncate (/ (* buffer-size 1024) 24)))))
	       (column-count 0)
	       (eof? nil)
	       (buffer (make-array max-buffer-size 
				   :element-type '(unsigned-byte 8))))
	  (loop 
	   (print-debug (format nil "~%Process attachment ~a~%" file-in))
	   (let* ((;; read a portion of the file into the buffer arrary and 
                   ;; returns the index where it stopped
		   byte-count (dotimes (i max-buffer-size max-buffer-size)
				(let ((bchar (read-byte strm-in nil 'EOF)))
				  (if (eql bchar 'EOF)
				      (progn 
					(setq eof? t)
					(return i))
				      (setf (aref buffer i) bchar))))))
		  (if (zerop buffer-size)
		      ;; send file all at once to socket. 
		      #+allegro
		      (write-string (excl:usb8-array-to-base64-string 
				     buffer wrap-at-column) sock)
		      #-allegro
		      (cl-base64:usb8-array-to-base64-stream 
		       buffer sock :columns wrap-at-column)
		      ;; otherwise process file in chunks. 
                    ;; The extra encoded-string, 
		      ;; and its subseq functions are brute force methods
                    ;; to properly handle the wrap-at-column feature 
                    ;; between buffers. 
                    ;; Not the most efficient way,
                    ;; but it works and uses existing functions 
                    ;; in the cl-base64 package.
		    (let* ((;; drops off extra elements that were not filled in in reading, this is important for lisp systems that default a value into
			    ;; the array when it is created. -- ie Lispworks, SBCL
			    trimmed-buffer (if eof?
					       (subseq buffer 0 byte-count)
					       buffer))
			   (encoded-string 
			    #+allegro
			     (excl:usb8-array-to-base64-string 
			      trimmed-buffer)
			    #-allegro
			    (cl-base64:usb8-array-to-base64-string 
			     trimmed-buffer)))
		      (loop for ch across encoded-string 
			    do (progn 
				 (write-char ch sock)
				 (incf column-count)
				 (when (= column-count wrap-at-column)
				   (setq column-count 0)
				   (write-char #\Newline sock))))))
		  (force-output sock)
		  (print-debug (format nil "~% Eof is ~a~%" eof?))
		  (when (or (zerop buffer-size)
			    eof?)
                    (write-blank-line sock)
		    (return))))))))
