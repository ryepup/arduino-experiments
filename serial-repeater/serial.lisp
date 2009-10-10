;;;; serial.lisp
(require 'alexandria)

(defpackage #:serial
  (:use #:cl)
  (:export with-serial-port
	    with-serial-ports*))

(in-package #:serial)

(defvar *open-tty-streams* (make-hash-table)
  "Hash table of open ttys for signal handlers")

(defun open-serial-port-stream (pathname &optional (timeout 0.1))
  "Open an sb-sys:fd-stream"
  (multiple-value-bind (fd errno)
      (sb-unix:unix-open pathname
			 (logior sb-posix::o-rdwr
				 sb-posix::o-noctty
				 sb-posix::o-nonblock)
			 0)
    (unless fd
      (error "Failed to open serial port ~a!~%~a~%"
	     pathname  (sb-int:strerror errno)))
 
    (sb-sys:make-fd-stream fd
			   :input t
			   :output t
			   :buffering :none
			   :timeout timeout
			   :pathname
			   (make-pathname :name pathname)
			   :element-type :default)))

(defun close-serial-port-stream (fd-stream)
  (let ((fd (sb-sys:fd-stream-fd fd-stream)))
    (sb-posix:fcntl fd sb-posix:F-SETFL 0)
    (sb-posix:close fd)))

(defun set-serial-port-raw-mode (serial-port-stream &key speed parity)
  "Example:
   ;;; (let ((tty (open-serial-port-stream \"/dev/ttyS0\")))       
   ;;;   (set-serial-port-raw-mode tty :speed 19200 :parity :n))
  "
  (let ((termios (sb-posix:tcgetattr (sb-sys:fd-stream-fd serial-port-stream)))
	(bspeed (symbol-value (find-symbol (format nil "B~d" speed)
					   :sb-posix))))
    (if (null bspeed) (error "~a baud - mode not supported" speed)
      (progn
	(sb-posix::cfsetispeed bspeed termios)
	(sb-posix::cfsetospeed bspeed termios)
	(mapcar #'(lambda (x)
		    (setf (sb-posix:termios-iflag termios)
			  (logandc2 (sb-posix:termios-iflag termios)
				    x)))
		(list sb-posix:ignbrk sb-posix:brkint sb-posix:parmrk
		      sb-posix:istrip sb-posix:inlcr sb-posix:igncr
		      sb-posix:icrnl sb-posix:ixon))
	(mapcar #'(lambda (x)
		    (setf (sb-posix:termios-oflag termios)
			  (logandc2 (sb-posix:termios-oflag termios)
				    x)))
		(list sb-posix:opost))
	(mapcar #'(lambda (x)
		    (setf (sb-posix:termios-lflag termios)
			  (logandc2 (sb-posix:termios-lflag termios)
				    x)))
		(list sb-posix:echo sb-posix:echonl sb-posix:icanon
		      sb-posix:isig sb-posix:iexten))
	(cond
	 ((eql parity :n)
	  (mapcar #'(lambda (x)
		      (setf (sb-posix:termios-cflag termios)
			    (logandc2 (sb-posix:termios-cflag termios)
				      x)))
		  (list sb-posix:csize sb-posix:parenb sb-posix:cstopb)))
	 ((eql parity :e) 
	  (mapcar #'(lambda (x)
		      (setf (sb-posix:termios-cflag termios)
			    (logandc2 (sb-posix:termios-cflag termios)
				      x)))
		  (list sb-posix:csize sb-posix:parodd sb-posix:cstopb))
	  (setf (sb-posix:termios-cflag termios)
		(logior (sb-posix:termios-cflag termios)
			sb-posix:parenb)))
	 (t (error "Usuported parity checking mode: ~a" parity)))
	(setf (sb-posix:termios-cflag termios)
	      (logior (sb-posix:termios-cflag termios)
		      sb-posix:cs8))
	(setf (sb-posix::termios-cflag termios)
	      (logior (sb-posix::termios-cflag termios)
		      sb-posix::clocal sb-posix::cread))
	(setf (sb-posix::termios-cflag termios)
	      (logior (sb-posix::termios-cflag termios)
		      sb-posix::clocal sb-posix::clocal))
	(sb-posix:tcsetattr (sb-sys:fd-stream-fd
			     serial-port-stream)
			    sb-posix:tcsanow
			    termios)))))


(defun tty-signal-handler ()
  "Can be used with signal handlers (such sigterm, sigint etc.)
   for restoring original serial device settings"
  (maphash #'(lambda (k h)
	       (declare (ignore k))
	       (if (cdr h) (sb-posix:tcsetattr (sb-sys:fd-stream-fd (car h))
					       sb-posix:tcsanow
					       (cdr h)))
	       (if (car h) (close-serial-port-stream (car h))))
	   *open-tty-streams*))

(defmacro with-serial-port ((stream filespec
				    &key
				    (speed 115200)
				    (parity :n)
				    (timeout 0.1))
			    &body body)
  "Example (checking with rx<->tx connected:
   ;;; (with-serial-port (tty \"/dev/ttyS1\" :speed 19200 :parity :e)
   ;;;   (let ((out (vector 1 2 3 4))                                 
   ;;;         (in  (make-array 4 :element-type '(unsigned-byte 8)))) 
   ;;;     (write-sequence out tty)                                
   ;;;     (read-sequence in tty)                                     
   ;;;     (every #'= in out)))                                        
  "
  (alexandria:with-gensyms (old hash)
    `(let ((,stream nil)
	   (,old nil))
       (unwind-protect
	    (progn
	      (setf ,stream (open-serial-port-stream ,filespec  ,timeout)
		    ,old    (sb-posix:tcgetattr (sb-sys:fd-stream-fd ,stream)))
	      (set-serial-port-raw-mode ,stream
					:speed ,speed
					:parity ,parity)
	      (setf (gethash ',hash *open-tty-streams*)
		    (cons ,stream ,old)) 
	      ,@body)
	 (progn 
	   (if ,old (sb-posix::tcsetattr (sb-sys:fd-stream-fd ,stream)
					 sb-posix:tcsanow
					 ,old))
	   (if ,stream (close-serial-port-stream ,stream))
	   (remhash ',hash *open-tty-streams*))))))

(defmacro with-serial-ports* (serial-binds &body body)
  "Example:
   ;;; (with-serial-ports* ((tty0 \"/dev/ttyS0\" :speed 19200 :parity :n)
   ;;;                      (tty1 \"/dev/ttyS1\" :speed 19200 :parity :n))
   ;;;   (let ((out (vector 1 2 3 4))                                        
   ;;;         (in  (make-array 4 :element-type '(unsigned-byte 8))))        
   ;;;     (write-sequence out tty0)                                         
   ;;;     (read-sequence in tty1)                                           
   ;;;     (every #'= in out)))                                              
  "
  (if (null serial-binds)
      `(progn ,@body)
      `(with-serial-port ,(car serial-binds)
	 (if ,(caar serial-binds)
	     (with-serial-ports* ,(cdr serial-binds) ,@body)))))

