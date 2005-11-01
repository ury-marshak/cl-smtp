;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: openmcl.lisp
;;; Description: OpenMCL Socket Interface

(in-package :cl-smtp)

(defun make-smtp-socket (host port)
  (handler-case
      (ccl:make-socket :remote-host host :remote-port port)
    (serious-condition (e)
      (error "could not create client socket:~A" e))))

(defun socket-stream (socket)
  socket)

(defun get-host-name ()
  (machine-instance))
