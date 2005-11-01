;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser GNU General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU  General Public License for more details.

;;; File: sbcl.lisp
;;; Description: SBCL Socket Interface

(in-package :cl-smtp)

(defun make-smtp-socket (host port)
  (handler-case
      (let* ((protocol (sb-bsd-sockets:get-protocol-by-name "tcp"))
	     (socket (make-instance 'sb-bsd-sockets:inet-socket 
		       :type :stream 
		       :protocol protocol)))
	(sb-bsd-sockets:socket-connect 
	 socket 
	 (sb-bsd-sockets:host-ent-address 
	  (sb-bsd-sockets:get-host-by-name host))
	 port)
	socket)
    (serious-condition (e)
      (error "could not create client socket:~A" e))))

(defun socket-stream (socket)
  (sb-bsd-sockets:socket-make-stream
   socket
   :input t
   :output t))

(defun get-host-name ()
  (sb-unix:unix-gethostname))
