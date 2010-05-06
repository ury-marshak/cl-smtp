;; -*- mode: common-lisp; coding: utf-8 -*-
(in-package :cl-smtp)

(defparameter *cl-smtp-tests* (make-hash-table :test 'equal))

(defmacro define-cl-smtp-test (name (&rest args) &body body)
  (let ((tmpname (gensym (string name))))
    `(progn
       (defun ,tmpname (,@(mapcar #'car args))
         ,@body)
       (setf (gethash ,(string-downcase name) *cl-smtp-tests*)
             (list #',tmpname ,args)))))

(defun get-component-pathname ()
  (asdf:component-pathname (asdf:find-system "cl-smtp")))

(define-cl-smtp-test "rfc2045-q-encode-string-utf-8" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (rfc2045-q-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (string-equal 
             qstr "=?UTF-8?Q?=C3=B6=C3=BC=C3=A4=C3=96=C3=9C=C3=84=C3=9F?="))))

(define-cl-smtp-test "escape-rfc822-quoted-string" ()
  (assert (equal (escape-rfc822-quoted-string "test end") "test end"))
  (assert (equal (escape-rfc822-quoted-string "test\\end") 
                 "test\\\\end"))
  (assert (equal (escape-rfc822-quoted-string "test\"end")
                 "test\\\"end"))
  (assert (equal (escape-rfc822-quoted-string (format nil "test~%end")) 
                 (format nil "test\\~%end")))
  (assert (equal (escape-rfc822-quoted-string 
                  (format nil "test~cend" #\Return)) 
                 (format nil "test\\~cend" #\Return)))
  (assert (equal (escape-rfc822-quoted-string "test/end") "test/end"))
  (assert (equal (escape-rfc822-quoted-string "test end\\") 
                 "test end\\\\"))
  (assert (equal (escape-rfc822-quoted-string (format nil "~%test end\\")) 
                 (format nil "\\~%test end\\\\"))))

(define-cl-smtp-test "rfc2231-encode-string-utf-8" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (rfc2231-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (string-equal 
             qstr "UTF-8''%C3%B6%C3%BC%C3%A4%C3%96%C3%9C%C3%84%C3%9F"))
    ))

(define-cl-smtp-test "make-attachment-1" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p)))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "make-attachment-2" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment p))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "make-attachment-3" ()
  (let* ((p (namestring (merge-pathnames "tests.lisp" 
                                         (get-component-pathname))))
         (attachment p))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "send-attachment-header-1" ()
  (let* ((boundary (make-random-boundary))
         (p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p))
         (headerstr (with-output-to-string (s)
                      (send-attachment-header s boundary attachment :utf-8)))
         (returnnewline (format nil (format nil "~C~C" #\Return #\NewLine)))
         (tmpstr (format nil "--~A~AContent-type: text/plain;~% name*=UTF-8''tests.lisp;~% name=\"tests.lisp\"~AContent-Disposition: attachment; filename*=UTF-8''tests.lisp; filename=\"tests.lisp\"~AContent-Transfer-Encoding: base64~A~A" 
                         boundary returnnewline returnnewline returnnewline 
                         returnnewline returnnewline)))
    (assert (equal headerstr tmpstr))
    ))


(define-cl-smtp-test "mask-dot-1" ()
  (assert (equal (mask-dot (format nil "~C~C.~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C..~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C~C..~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C..~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C~C~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C.~C.~C.~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C.~C.~C.~C" #\Return #\NewLine
                         #\Return #\NewLine))))

(define-cl-smtp-test "substitute-return-newline" ()
  (assert (equal (substitute-return-newline 
                  (format nil "start~Aende" *return-newline*))
                 "start ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~Aweiter~Aende" 
                          *return-newline* *return-newline*))
                 "start weiter ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "~Astart~Aweiter~Aende~A" 
                          *return-newline* *return-newline* *return-newline*
                          *return-newline*))
                 " start weiter ende "))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~Aende" 
                          *return-newline* *return-newline*))
                 "start  ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~A~Aende" 
                          *return-newline* *return-newline* *return-newline*))
                 "start   ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~%~A~Aende" 
                          *return-newline* *return-newline* *return-newline*))
                 "start 
  ende"))
  )

(defun run-test (name)
  (handler-case
      (let ((test (gethash name *cl-smtp-tests*)))
        (format t "~%run test: ~S ~@[(~A)~]~%" name (cadr test))
        (apply (car test) (cadr test))
        (format t "pass~%")
        t)
    (simple-error (c)
      (format t "failed: ~A" c)
      nil)))

(defun run-tests ()
  (let ((n (hash-table-count *cl-smtp-tests*))
        (pass 0))
    (format t "~%run ~D cl-smtp-tests~%~%" (hash-table-count *cl-smtp-tests*))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (when (run-test k)
                   (incf pass)))
             *cl-smtp-tests*)
    (format t "~%pass: ~D | failed: ~D~%~%" pass (- n pass))))
