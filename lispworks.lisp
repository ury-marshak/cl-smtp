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

;;; File: lispworks.lisp
;;; Description: Lispworks Socket Interface

(in-package :cl-smtp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defun make-smtp-socket (host port)
  (comm:open-tcp-stream host port))

(defun socket-stream (socket)
  socket)

(defun get-host-name ()
  (machine-instance))
 
