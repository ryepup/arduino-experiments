;;;; reader.lisp

(defpackage #:serial-reader
  (:use #:cl))

(in-package #:serial-reader)

(defun read-serial ()   
  (with-open-file (s "/dev/ttyUSB0"
		     :direction :io
		     :if-exists :overwrite)
    (read-line s)))

