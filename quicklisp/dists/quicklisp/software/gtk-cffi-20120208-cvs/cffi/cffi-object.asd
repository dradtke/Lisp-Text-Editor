;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-object.asd --- ASDF system definition for cffi-object
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:cffi-object-system
  (:use #:cl #:asdf))
(in-package #:cffi-object-system)

(defsystem cffi-object
  :description "CFFI utilities for object wrappers and Unicode"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.9"
  :license "BSD"
  :depends-on (cffi iterate gtk-cffi-utils trivial-garbage)
  :components
  ((:file package)
   (:file object :depends-on (package))
   (:file pfunction :depends-on (package))
   (:file string :depends-on (package))
   (:file struct :depends-on (package))))
