;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for gobject-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage #:g-object-cffi
  (:use #:common-lisp #:cffi #:alexandria #:iterate
        #:cffi-object #:g-lib-cffi #:gtk-cffi-utils)
  (:import-from #:cffi-object *objects*)
  (:export

   #:g-object
   ;; slots
   #:signals
   #:property
   #:properties
   #:gsignal
   
   #:connect

   #:storage
   ;; slot
   #:data

   ;; macro
   #:with-object
   
   ;; type
   #:pobject
   #:pdata
   #:g-list-object

   #:with-g-value
   #:*g-value*
   #:g-value
   #:value
   #:unset
   #:init

;   #:g-type->name
   #:g-type->lisp
   #:keyword->g-type
   #:g-type

   #:g-type-name
   #:g-type-from-name
   #:g-type-from-instance
   #:g-type-info
   #:g-type-flags
   #:g-type-register-static
   #:g-type-register-static-simple
   #:g-interface-info
   #:g-type-add-interface-static
   #:g-type-interface
   #:g-type-class
   #:g-type-instance

   #:register-type
   #:register-package
   #:register-prefix

   #:ref
   #:unref

   #:find-property
   #:find-child-property

   #:g-object-class
   #:g-param-spec
   #:g-object-newv
   #:new
   #:make-closure

   ; utility functions
   #:defslot
   #:defgdkslot
   #:defgtkslot

   #:defslots
   #:defgtkslots
   #:defgdkslots

   #:defgetter
   #:defgtkgetter
   #:defgdkgetter

   #:defsetter
   #:defgtksetter
   #:defgdksetter

   #:deffun
   #:defgtkfun
   #:defgdkfun

   #:deffuns
   #:defgtkfuns
   #:defgdkfuns

   #:*callback*
   #:foreach
   #:make-foreach))
