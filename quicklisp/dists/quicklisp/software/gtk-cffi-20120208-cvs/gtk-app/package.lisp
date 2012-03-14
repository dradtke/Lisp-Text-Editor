(in-package #:cl-user)

(defpackage #:бд 
  (:use #:common-lisp #:clsql #:gtk-cffi)
  (:export
   #:*поля*
   #:*формы*
   #:объект-бд
   #:поля-бд
   #:поле-бд
   #:имя-лисп
   #:тип-gtk
   #:обновить-модель))

(defpackage #:справочник
  (:use #:common-lisp #:clsql #:бд #:gtk-cffi)
  (:intern #:справочник))

(defpackage #:учёт
  (:use #:common-lisp #:gtk-cffi #:clsql)
  (:import-from #:справочник #:справочник)
  (:export
   #:форма-списка-по-объекту
   #:форма-списка
   #:справочник))