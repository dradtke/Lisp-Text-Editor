;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; string.lisp --- UTF8 string operations for CFFI
;;;    
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cffi-object)

(define-foreign-type gtk-string ()
  ()
  (:actual-type :pointer)
  (:simple-parser gtk-string))

(defun string->ptr (value)
  "string -> foreign pointer char*"
  (typecase value
    (null (null-pointer))
    (foreign-pointer value)
    (t (foreign-string-alloc (string value) :encoding :utf-8))))

(defmethod translate-to-foreign (value (type gtk-string))
  (string->ptr value))

(defmethod translate-from-foreign (ptr (name gtk-string))
  (foreign-string-to-lisp ptr :encoding :utf-8))

(defmethod free-translated-object (value (name gtk-string) param)
  (declare (ignore param))
  (foreign-string-free value))

(define-foreign-type gtk-dyn-string ()
  ()
  (:actual-type :pointer)
  (:simple-parser gtk-dyn-string)
  (:documentation "gtk-string without freeing after sending to C"))

(defmethod translate-to-foreign (value (type gtk-dyn-string))
  (string->ptr value))

(defmethod translate-from-foreign (ptr (name gtk-dyn-string))
  (foreign-string-to-lisp ptr :encoding :utf-8))

(define-foreign-type gtk-new-string ()
  ()
  (:actual-type :pointer)
  (:simple-parser gtk-new-string)
  (:documentation "gtk-string for returned new allocated strings"))

(defmethod translate-from-foreign (ptr (name gtk-new-string))
  (prog1
      (foreign-string-to-lisp ptr :encoding :utf-8)
    (foreign-free ptr)))

