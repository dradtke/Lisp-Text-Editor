(defpackage #:gtk-application
  (:use #:common-lisp #:gtk-cffi)
  (:export #:run))

(in-package #:gtk-application)

(gtk-init)

(defclass application (window)
  ((menu :accessor menu :initform (make-instance 'menu-bar))
   (toolbar :accessor toolbar :initform (make-instance 'toolbar))
   (notebook :accessor notebook :initform (make-instance 'notebook))
   (statusbar :accessor statusbar :initform (make-instance 'statusbar))
   (textview :accessor textview :initform (make-instance 'text-view))))


(defmethod initialize-instance :after ((application application) 
                                       &key &allow-other-keys)
  (let ((v-box (make-instance 'v-box)))
    (add application v-box)
    (with-slots (menu toolbar notebook statusbar textview) application
      (pack* v-box
             :expand nil 
             menu toolbar 
             :expand t 
             notebook textview 
             :expand nil 
             statusbar)))
  (setf (gsignal application :destroy) :gtk-main-quit))

(let ((win (make-instance 'application :width 400)))

  (show win)

  (gtk-main))