;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for cffi-object
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;
;;; This library contains set of utils to make CLOS wrappers around
;;;    GLib/GObject/GDK/GTK objects

(in-package #:cl-user)

(defpackage #:cffi-object
  (:use #:iterate #:common-lisp #:cffi #:gtk-cffi-utils #:alexandria)
  (:export

   #:gconstructor

   #:object
   #:object-by-id
   ;; slots
   #:pointer
   ;; methods
   #:free

   ;; types
   #:gtk-string ; deprecated
   #:gtk-dyn-string ; deprecated
   #:gtk-new-string ; deprecated
   #:pfunction
   #:cffi-object

   #:struct
   #:cffi-struct
   #:new-struct
   #:free-struct

   #:freeable
   #:freeable-base
   #:free-sent-if-needed
   #:free-returned-if-needed
   #:free-ptr
   
   #:defcstruct-accessors
   #:defcstruct*
   #:defbitaccessors

   #:with-foreign-out
   #:with-foreign-outs
   #:with-foreign-outs-list
   
   #:setf-init
   #:init-slots
   #:save-setter
   #:remove-setter
   #:clear-setters))
