;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; array.lisp --- CFFI wrapper for arrays
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :g-lib-cffi)

(defvar *array-length* (foreign-alloc :uint))

;; TODO: add with-pointer-to-vector-data optimization
(define-foreign-type cffi-array (freeable)
  ((element-type :initarg :type :accessor element-type))
  (:actual-type :pointer))

(define-parse-method garray (type &key free)
  (make-instance 'cffi-array :type type :free free))

(defcfun g-free :void (var :pointer))

(defmethod free-ptr ((type cffi-array) ptr)
  (g-free ptr))

(defmethod translate-to-foreign (value (cffi-array cffi-array))
  (if (pointerp value)
      value
      (let* ((length (length value))
             (type (element-type cffi-array))
             (res (foreign-alloc type :count length)))
        (dotimes (i length (values res t))
          (setf (mem-aref res type i) (elt value i)))
        res)))

;(defmethod free-translated-object (ptr (cffi-array cffi-array) param)
;  (declare (ignore param))
;  (free-if-needed cffi-array ptr :free-func #'foreign-free))


(defmethod translate-from-foreign (ptr (cffi-array cffi-array))
  (let ((array-length (mem-ref *array-length* :uint)))
    (let* ((res (make-array array-length))
           (el-type (element-type cffi-array)))
      (iter
        (for i from 0 below array-length)
        (setf (aref res i)
              (mem-aref ptr el-type i)))
      res)))
