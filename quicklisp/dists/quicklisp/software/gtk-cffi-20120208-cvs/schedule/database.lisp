;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; database.lisp -- Database interface for Schedule
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :schedule)

(clsql:def-view-class сотрудники ()
                      ((id
                        :db-kind :key
                        :db-constraints :not-null
                        :type integer
                        :initarg :id)
                       (name
                        :accessor имя
                        :type (string 100)
                        :initarg :имя))
                      (:base-table employee))

(clsql:def-view-class клиенты ()
                      ((id
                        :db-kind :key
                        :db-constraints :not-null
                        :type integer
                        :initarg :id)
                       (name
                        :accessor имя
                        :type (string 100)
                        :initarg :имя))
                      (:base-table clients))


(clsql:def-view-class конфигурации ()
                      ((id
                        :db-kind :key
                        :db-constraints :not-null
                        :type integer
                        :initarg :id)
                       (name
                        :accessor имя
                        :type (string 100)
                        :initarg :имя)
                       (client
                        :accessor клиент
                        :db-type :join
                        :db-info (:join-class клиенты
                                              :home-key client
                                              :foreign-key id
                                              :set nil)))                      
                      (:base-table conf))

(clsql:def-view-class записи ()
  ((id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :id)
   (date
    :type date
    :accessor дата
    :initarg :дата)
   (employee
    :db-kind :join
    :accessor сотрудник
    :db-info (:join-class сотрудники
			  :home-key emplyee
			  :foreign-key id
			  :set nil)
    :initarg :сотрудник)
   (client-base
    :db-kind :join
    :accessor конфигурация-клиента
    :db-info (:join-class конфигурации
                          :home-key base
                          :foreign-key id
                          :set nil))
   (description
    :type string
    :accessor описание
    :initarg :описание)
   )
  (:base-table records))

(defun init-db ()
  (clsql:connect '("" "monk" "monk" "13b.kZ") :database-type :postgresql)
  (ignore-errors
    (clsql:create-view-from-class 'работники))
  (ignore-errors
    (clsql:create-view-from-class 'клиенты))
  (ignore-errors
    (clsql:create-view-from-class 'конфигурации))
  (ignore-errors
    (clsql:create-view-from-class 'записи)))