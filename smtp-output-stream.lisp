;; -*- Lisp -*-

;; smtp-output-stream.lisp - Perform protocol and mail specific
;; processing to convert an email message into an external format
;; proper to be transfered through SMTP.

;; This stream type is used to do two things:

;; In the message header, convert all non-ASCII characters to their
;; equivalent Q-encoded representation (RFC2047)

;; In the whole message, convert all line endings to CR+LF as required
;; by the SMTP protocol.

;; This stream class should also perform automatic dot masking, tbd.

(in-package :cl-smtp)

(defclass smtp-output-stream (trivial-gray-stream-mixin fundamental-character-output-stream)
  ((encapsulated-stream
    :initarg :encapsulated-stream
    :reader encapsulated-stream)
   (in-header
    :initform t
    :accessor in-header
    :documentation
    "Currently emitting the header of the message")
   (line-has-non-ascii
    :initform nil
    :accessor line-has-non-ascii
    :documentation
    "The current line has non ASCII characters in it")
   (previous-char
    :initform nil
    :accessor previous-char
    :documentation
    "Previous character sent to the stream, used to detect end of header")
   (external-format
    :initform (flex:make-external-format :iso-8859-15)
    :initarg :external-format
    :reader external-format)))

(defmethod stream-element-type ((stream smtp-output-stream))
  (stream-element-type (encapsulated-stream stream)))

(defmethod close ((stream smtp-output-stream) &key abort)
  (close (encapsulated-stream stream) :abort abort))

(defmethod stream-write-char ((stream smtp-output-stream) char)
  (with-accessors ((in-header in-header)
                   (line-has-non-ascii line-has-non-ascii)
                   (previous-char previous-char)
                   (external-format external-format)
                   (encapsulated-stream encapsulated-stream)) stream
    (when in-header
      (cond
        ;; Newline processing
        ((eql char #\Newline)
         ;; Finish quoting
         (when line-has-non-ascii
           (format encapsulated-stream "?=")
           (setf line-has-non-ascii nil))
         ;; Test for end of header
         (when (eql previous-char #\Newline)
           (setf in-header nil)))
        ((eql char #\Return)
         ;; CR is suppressed here and added before each #\Newline
         )
        ;; Handle non-ASCII characters
        ((< 127 (char-code char))
         (unless line-has-non-ascii
           (format encapsulated-stream "=?~A?Q?" (flex:external-format-name external-format))
           (setf line-has-non-ascii t))
         (loop for byte across (flex:string-to-octets (make-string 1 :initial-element char)
                                                      :external-format external-format)
            do (format encapsulated-stream "=~2,'0X" byte))))
      (setf previous-char char))
      #+nil(when (eql char #\Newline)
        (write-char #\Return encapsulated-stream))
      (unless (< 127 (char-code char))
        (write-char char encapsulated-stream))))

(defmethod stream-write-sequence ((stream smtp-output-stream) sequence start end &key)
  (if (in-header stream)
      (loop for i from start below end
           do (stream-write-char stream (elt sequence i)))
      (write-sequence sequence (encapsulated-stream stream) :start start :end end)))
