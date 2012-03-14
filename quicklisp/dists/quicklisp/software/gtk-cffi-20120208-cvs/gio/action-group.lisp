;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; action-group.lisp --- GActionGroup
;;;
;;; Copyright (C) 2012, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gio-cffi)

(defclass action-group (object) ())

(deffuns action-group
  (has-action :boolean (action-name gtk-string))
  (list-actions (string-list :free t))
  (:get action-enabled :boolean (action-name gtk-string)))
;  (:get action-parameter-type variant-type (action-name gtk-string))
;  (:get action-state-type variant-type (action-name gtk-string)))
  
  