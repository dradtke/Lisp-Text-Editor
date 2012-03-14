;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; loadlib.lisp --- loading C library
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

;; ;; Added possibility of using list of enums as sum 
;; (defmethod cffi::translate-type-to-foreign (value (type cffi::foreign-enum))
;;   (cond
;;    ((keywordp value) (cffi::%foreign-enum-value type value))
;;    ((listp value)
;;     (apply '+ (mapcar
;;                (lambda (x) (cffi::translate-type-to-foreign x type))  value)))
;;    (t value)))


;(eval-when (:compile-toplevel :load-toplevel :execute)
(define-foreign-library :gtk
  (:unix "libgtk-3.so.0") ;libgtk-x11-2.0.so")
  (:windows "libgtk-win32-3-0.dll"))
  
(use-foreign-library :gtk)



