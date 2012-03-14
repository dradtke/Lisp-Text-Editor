;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; database.lisp -- Records interface for Schedule
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defun records-view ()
  (or (gethash 'records-view *forms*)
      (let ((records-view (make-instance 'window :width 400 :height 280))
            (v-box (make-instance 'v-box))
            (com-box (make-instance 'h-button-box))
            (but-plus (make-instance 'button :text "+"))
            (but-minus (make-instance 'button :text "X")))
            
        (setf (gsignal window :destroy) :gtk-main-quit)
        (add records-view v-box)
        (pack v-box (make-instance 'label :text "Список работ"))
        (pack v-box com-box)
        (pack* com-box but-plus but-minus)
        
        (setf (gethash 'records-view *forms*) records-view)
        window)))

(gtk-init)
(show (records-view) :all t)
(gtk-main)

