;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gtk-cffi.asd --- ASDF system definition for gtk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:schedule-system
  (:use #:cl #:asdf))
(in-package #:schedule-system)

(defsystem schedule
  :description "Schedule for SlavSoft"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi clsql)
  :components
  ((:file :package)
   (:file :database :depends-on (:package))
   (:file :ui :depends-on (:database))))