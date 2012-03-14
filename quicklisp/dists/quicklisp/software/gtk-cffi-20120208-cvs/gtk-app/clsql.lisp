;; clisp UTF8 support for postgresql-socket

(in-package :postgresql-socket)

(fmakunbound 'read-socket-value-string)
(fmakunbound 'send-socket-value-string)
(fmakunbound 'read-socket-sequence)


(defun read-socket-value-string (socket)
  (declare (type stream socket))
  (let ((bytes (make-array 64
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (loop for code = (read-byte socket)
          until (zerop code)
          do (vector-push-extend code bytes))
    (babel:octets-to-string bytes :encoding :utf-8)))


(defun send-socket-value-string (socket value)
  (declare (type stream socket)
           (type string value))
  (write-sequence (babel:string-to-octets value :encoding :utf-8) socket)
  (write-byte 0 socket)
   nil)

(defun read-socket-sequence (stream length)
  (declare (stream stream))
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) bytes))    
    (read-sequence bytes stream) 
    (babel:octets-to-string bytes :encoding :utf-8)))