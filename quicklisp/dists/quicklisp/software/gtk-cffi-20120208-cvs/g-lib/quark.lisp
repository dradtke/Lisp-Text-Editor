;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; quark.lisp:
;;;
;;;     Quarks --- a 2-way association between a string
;;;                and a unique integer identifier       
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-lib-cffi)

(defctype g-quark :uint32)

(defcfun g-quark-to-string gtk-string (quark g-quark))

(defcfun g-intern-string :pointer (string gtk-string))

(defcfun g-intern-static-string :pointer (string gtk-dyn-string))

