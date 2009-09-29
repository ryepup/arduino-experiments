;;;; reader.lisp

(defpackage #:serial-reader
  (:use #:cl))

(in-package #:serial-reader)

(defvar *default-serial-stream* nil "special variable to hold the default stream")

(defvar *device-path* "/dev/ttyUSB0")

(defun initialize (speed)
  "configures the serial port to run at the given speed"
  (sb-ext:run-program
   "/bin/stty" (list "-F" *device-path* (princ-to-string speed)
		     "raw -parenb -parodd cs8 -hupcl -cstopb clocal")))

(defmacro with-serial-io ((stream) &body body)
  `(with-open-file (,stream *device-path*
		      :direction :io
		      :if-exists :overwrite
		      :external-format :ascii )
     (let ((*default-serial-stream* ,stream))
       ,@body)))

(defun read-serial ()
  (with-serial-io (s)
    (read-line s)))

(defun echoer (byte)
  (with-serial-io (s)
    (format s "~D~%" byte)))
