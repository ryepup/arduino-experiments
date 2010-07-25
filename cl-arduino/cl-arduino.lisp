;;;; cl-arduino.lisp

(defpackage #:cl-arduino
  (:use #:cl #:iterate))

(in-package #:cl-arduino)


(defvar *default-serial-stream* nil
  "special variable to hold the default stream")

(defvar *device-path* "/dev/ttyUSB0")

(defmacro with-serial-io ((stream) &body body)
  `(serial:with-serial-port (,stream *device-path*
				     :speed 9600
				     :timeout 1)
     (let ((*default-serial-stream* ,stream))
       ,@body)))

(defun read-serial ()
  (with-serial-io (s)
    (read-line s)))

(defun echoer (byte)
  (with-serial-io (s)
    (write-byte byte s)))

(defvar *opcodes* '((:analogRead . 200)
		    (:digitalWrite . 210)
		    (:analogWrite . 205)
		    (:irRead . 215)
		    (:shutdown . 220)))
(defun opcode (operation)
  (let ((code (assoc operation *opcodes*)))
    (assert code (code)
	    "Expected to find a valid code for ~a" operation)
  (cdr code)))

(defclass arduino ()
  ((pins :accessor pins
	 :initform (make-array 1 :adjustable t))
   (serial-stream :accessor serial-stream)))

(defmethod initialize-instance :after ((obj arduino) &key &allow-other-keys)
  (setf (serial-stream obj)
	(serial::open-serial-port-stream *device-path*))

  (serial::set-serial-port-raw-mode (serial-stream obj)
				    :speed 9600 :parity :n))

(defmethod disconnect ((ar arduino))
  (serial::close-serial-port-stream (serial-stream ar)))

(defmacro with-serial-ack ((stream-var arduino) &body body)
  `(let ((,stream-var (serial-stream ,arduino)))
     (prog1
	 (progn ,@body)
       (read-byte ,stream-var) ;;our ack
       )))

(defmethod analog ((ar arduino) pin-number)
  (assert (<= 0 pin-number 5) ()
	  "must be an analog in pin #")
  ;;ask arduino for that pin
 (with-serial-ack (s ar)
   (write-sequence (vector (opcode :analogRead) pin-number)
		   s)
   (read-byte s)))

(defmethod (setf analog) (new-value (ar arduino) pin)
  (assert (member pin '(3 5 6 9 10 11))
	  ()
	  "must be a pwm pin")
  (assert (<= 0 new-value 255) ()
	  "new value must be between 0 and 255")
  (with-serial-ack (s ar)
    (write-sequence
     (vector (opcode :analogWrite) pin new-value)
     s)))
    
(defmethod (setf digital) (new-value (ar arduino) pin-number)  
  (with-serial-ack (s ar)
    (write-sequence
     (vector (opcode :digitalWrite)
	     pin-number
	     (if new-value 255 0))
     s)))

(defmethod ir-read ((ar arduino) pin)
  (with-serial-ack (s ar)
    (write-sequence
     (vector (opcode :irRead) pin) s)
    (parse-integer
     (iter (for line = (ignore-errors (read-line s)))
	   (when line
	     (return line))))))

(defmethod shutdown ((ar arduino))
  (with-serial-ack (s ar)
    (write-sequence
     (vector (opcode :shutdown)) s))
  (disconnect ar))

(defmethod blink ((ar arduino) pin count &key (interval 0.5))
  (loop for i from 1 to count
	do
     (progn
       (setf (digital ar pin) nil)
       (sleep interval)
       (setf (digital ar pin) T)
       (sleep interval)))
  (setf (digital ar pin) nil))