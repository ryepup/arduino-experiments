(defpackage #:cl-arduino
  (:use #:cl #:iterate)
  (:export
   #:arduino #:disconnect #:analog #:digital
   #:ir-read #:shutdown #:blink))