(in-package :справочник)


(def-view-class справочник (объект-бд)
  ((код :accessor код :type integer :column code)
   (наименование :accessor наименование :type (varchar 100) :column name))
  (:base-table catalog))


(defun реквизиты-справочника (поля)
  (cons '(код :целое)
        (cons '(наименование :строка 100)
              поля)))

(defmacro справочник (имя поля)
  `(prog1
       (def-view-class ,(имя-лисп имя :справочник) (справочник) ,(поля-бд поля)
         (:base-table ,(format nil "cat-~a" (поле-бд имя))))
     (setf (gethash (find-class ',(имя-лисп имя :справочник)) *поля*)
           ',(реквизиты-справочника поля))
     (export ',(имя-лисп имя :справочник) :справочник)))



(defun представление-поля (поле)
  (let ((str (format nil "~a" поле)))
    (concatenate 'string
                 (subseq str 0 1)
                 (string-downcase (subseq str 1)))))

(defmethod учёт:форма-списка-по-объекту ((справочник справочник))
  (обновить-модель справочник)
  (let ((key (cons (class-of справочник) "список")))
    (or (gethash key *формы*)
        (setf (gethash key *формы*)
              (gtk-model
               'window
               :width 800
               :height 600
               :title (format nil "Справочник ~a" (class-of справочник)) 
               :signals '(:destroy :gtk-main-quit)
               ('v-box
                ('h-button-box)
                ('scrolled-window
                 ('tree-view :model (model справочник)
                             :columns (mapcar (lambda (x)
                                                (представление-поля (car x)))
                                              (gethash (class-of справочник)
                                                       *поля*))
                             :id :tree-view))))))))

;(import 'справочник :учёт) 
;(export 'справочник :учёт)