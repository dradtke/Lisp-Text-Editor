;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; учёт.asd --- Определение системы ASDF для модуля учёт
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:система-учёт
  (:use #:cl #:asdf))
(in-package #:система-учёт)



(defsystem учёт
  :description "Учётная система на основе cl-sql и gtk-cffi"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi clsql clsql-postgresql-socket babel) 
  :components
  ((:file :clsql)
   (:file :package :depends-on (:clsql))
   (:file :бд :depends-on (:package))
   (:file :справочник :depends-on (:бд))))