;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; variant.lisp -- interface to GVariantType
;;;
;;; Copyright (C) 2012, Roman Klochkov <monk@slavsoft.surgut.ru>
;;;

(in-package #:g-lib-cffi)

(define-foreign-type variant-type (freeable)
  ((free :initform t))
  (:actual-type :pointer)
  (:simple-parser variant-type))

(defcfun g-variant-type-peek-string :pointer (ptr :pointer))
(defcfun g-variant-type-new :pointer (format :string))
(defcfun g-variant-type-free :void (ptr :pointer))
(defcfun g-variant-type-get-string-length gsize (ptr :pointer))

(defmethod free-ptr ((type variant-type) ptr)
  (g-variant-type-free ptr))

(defmethod translate-from-foreign (ptr (type variant-type))
  (declare (type foreign-pointer ptr))
  (when ptr
    (foreign-string-to-lisp 
     (g-variant-type-peek-string ptr)
     :count (g-variant-type-get-string-length ptr))))

(defmethod translate-to-foreign (str (type variant-type))
  (g-variant-type-new str))
