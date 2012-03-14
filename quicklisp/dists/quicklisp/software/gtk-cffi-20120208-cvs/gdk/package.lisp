;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for gdk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage #:gdk-cffi
  (:use #:common-lisp #:alexandria
        #:cffi #:cffi-object #:g-lib-cffi #:g-object-cffi)
  (:import-from #:cl-cairo2 #:x #:y #:width #:height #:cairo_rectangle_t)
  (:export
   ; types
   #:event-mask
   #:extension-mode
   #:pcolor
   #:prgba
   #:color-struct
   #:event
   ;; methods of event
   #:get-slot
   #:event-type

   #:parse-event
   
   #:rectangle
   #:intersect
   #:union

   #:screen
   ;; slots of screen
   #:height
   #:width

   #:window
   #:modifier-type

   #:pixmap
   
   #:image
   
   #:pixbuf
   ;; slots of pixbuf
   #:width

   #:with-threads

   #:key
   #:unichar
   
   #:keymap
   #:keycode
   #:group
   #:level
   #:entries-for-keyval
   #:entries-for-keycode
   #:lookup-key
   #:direction

   #:keyval-name
   #:keyval-from-name
   #:keyval-to-unicode
   #:unicode-to-keyval
   #:keyval-to-upper
   #:keyval-to-lower

   #:gatom
   ))

(in-package #:gdk-cffi)
(register-package "Gdk" *package*)
(register-package *package* 'gdk)
