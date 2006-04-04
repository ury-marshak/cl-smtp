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

;;; File: cl-smtp.asd
;;; Description: cl-smtp ASDF system definition file

(defpackage :cl-smtp
   	(:use :cl :asdf)
	(:export :send-email))

#+sbcl (require :sb-bsd-sockets)


(in-package :cl-smtp)

(asdf:defsystem :cl-smtp
        :version "20060404.1"
	:depends-on
	        #-allegro (:cl-base64)
	        #+allegro ()
	:components 
		(#+sbcl(:file "sbcl")
		 #+allegro(:file "acl")
                 #+cmu(:file "cmucl")
                 #+clisp(:file "clisp")
		 #+openmcl(:file "openmcl")
		 #+lispworks(:file "lispworks")
		 (:file "cl-smtp" :depends-on #+sbcl("sbcl") 
					      #+allegro("acl")
                                              #+cmu("cmucl")
                                              #+clisp("clisp")
					      #+openmcl("openmcl")
					      #+lispworks("lispworks"))
		 (:file "attachments" :depends-on #+sbcl("sbcl") 
					      #+allegro("acl")
                                              #+cmu("cmucl")
                                              #+clisp("clisp")
					      #+openmcl("openmcl")
					      #+lispworks("lispworks"))))
