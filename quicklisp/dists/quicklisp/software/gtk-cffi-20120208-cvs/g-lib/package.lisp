;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for glib-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage #:g-lib-cffi
  (:nicknames #:g-lib #:glib)
  (:use #:common-lisp #:cffi #:cffi-object #:iterate #:alexandria)
  (:export
   ;; gerror macro
   #:with-g-error
   
   ;; types
   #:g-list
   #:g-slist
   #:g-quark
   #:string-list
   #:variant-type

   #:g-error
   #:get-error

   #:garray
   #:*array-length*

   #:timeout-add
   #:timeout-remove
   #:yield

   #:g-intern-static-string
   #:g-free
   
   #:g-file
   ))
