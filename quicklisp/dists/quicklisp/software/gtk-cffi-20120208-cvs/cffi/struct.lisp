;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; struct.lisp --- CFFI wrapper for structs. We need to save on lisp
;;;                 side only values of struct field, not pointer on
;;;                 the struct to be able to garbage collect it
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :cffi-object)

(defclass struct (object)
  ((value :documentation "Assoc list (field-name . field-value)*"))
  (:documentation "If value bound, use it, else use pointer.
Struct may be used in OBJECT cffi-type or STRUCT cffi-type"))

(defmethod gconstructor ((struct struct) &key new-struct &allow-other-keys)
  (if new-struct 
      (new-struct (class-name (class-of struct)))
      (progn 
        (setf (slot-value struct 'value) nil)
        (null-pointer))))

(defmacro save-setter (class name)
  "Use this to register setters for SETF-INIT and INIT-SLOTS macro"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew ',name (get ',class 'slots))))

(defmacro remove-setter (class name)
  "Use this to unregister setters for SETF-INIT and INIT-SLOTS macro"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get ',class 'slots)
          (delete ',name (get ',class 'slots)))))

(defmacro clear-setters (class)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get ',class 'slots) nil)))

(defmacro setf-init (object &rest fields)
  "Should be used in constructors"
  `(progn
     ,@(mapcar (lambda (field-all)
                 (let ((field (if (consp field-all) 
                                  (first field-all) field-all))
                       (field-p (if (consp field-all)
                                    (third field-all) field-all)))
                   `(when ,field-p
                      (setf (,field ,object) ,field))))
               fields)))

(defmacro init-slots (class &optional add-keys &body body)
  "For SETF-INIT auto-constructor"
  (let ((slots (mapcar (lambda (x) (list x nil (symbolicate x '-p)))
                       (get class 'slots))))
    `(defmethod shared-initialize :after ((,class ,class) slot-names
                                          &key ,@slots ,@add-keys
                                          &allow-other-keys)
       (declare (ignore slot-names))
       (setf-init ,class ,@slots)
       ,@body)))


(defmacro defcstruct-accessors (class)
  "CLASS may be symbol = class-name = struct name,
or may be cons (class-name . struct-name)"
  (let ((class-name (if (consp class) (car class) class))
        (struct-name (if (consp class) (cdr class) class)))
    `(progn
       (clear-setters ,class-name)
       ;(eval-when (:compile-toplevel :load-toplevel :execute)
       ;  (setf (get ',class-name 'struct) ',struct-name))
       ,@(mapcar
          (lambda (x) 
           `(progn
              (unless (fboundp ',x)
                (defgeneric ,x (,class-name)))
              (defmethod ,x ((,class-name ,class-name))
                (if (slot-boundp ,class-name 'value)
                    (cdr (assoc ',x (slot-value ,class-name 'value)))
                    (foreign-slot-value (pointer ,class-name) 
                                        ',struct-name ',x)))
              (unless (fboundp '(setf ,x))
                (defgeneric (setf ,x) (val ,class-name)))
              (defmethod (setf ,x) (val (,class-name ,class-name))
                (if (slot-boundp ,class-name 'value)
                    (push val (slot-value ,class-name 'value))
                    (setf (foreign-slot-value (pointer ,class-name) 
                                              ',struct-name ',x) 
                          val)))
              (save-setter ,class-name ,x)))
          (foreign-slot-names `,struct-name)))))

(defmacro defbitaccessors (class slot &rest fields)
  (let ((pos 0))
    (flet ((build-field (field)
             (destructuring-bind (name type size) field
               (prog1 
                   `(progn
                      (unless (fboundp ',name)
                        (defgeneric ,name (,class)))
                      (defmethod ,name ((,class ,class))
                        (convert-from-foreign 
                         (ldb (byte ,size ,pos) (slot-value ,class ',slot))
                         ,type))
                      (unless (fboundp '(setf ,name))
                        (defgeneric (setf ,name) (value ,class)))
                      (defmethod (setf ,name) (value (,class ,class))
                        (setf (ldb (byte ,size ,pos) (slot-value ,class ',slot))
                              (convert-to-foreign value ,type))))
                 (incf pos size)))))
      (cons 'progn (mapcar #'build-field fields)))))


(defmacro defcstruct* (class &body body)
  `(progn
    (defcstruct ,class ,@body)
    (defcstruct-accessors ,class)))

(defgeneric new-struct (class)
  (:method (class)
    (foreign-alloc class)))        

(defgeneric free-struct (class value)
  (:method (class value)
    (declare (ignore class))
    (foreign-free value)))

(defun clos->new-struct (class object)
  (if (slot-boundp object 'value)
      (let ((res (new-struct class)))
;        (format t "Allocated ~a~%" res)
        (mapc (lambda (slot) 
                (let ((val (assoc slot (slot-value object 'value))))
                  (when (consp val)
                    (setf (foreign-slot-value res class slot) (cdr val)))))
              (foreign-slot-names class))
        res)
      (slot-value object 'pointer)))

(defun struct->clos (class struct &optional object)
  (let ((res (or object (make-instance class))))
    (setf (slot-value res 'value) nil)
    (mapc (lambda (slot)
            (push (cons slot (foreign-slot-value struct class slot))
                  (slot-value res 'value)))
          (foreign-slot-names class))
    res))

(define-foreign-type freeable-base ()
  ((free :accessor obj-free :initarg :free :initform :no-transfer
         :type (member :none :all :no-transfer :transfer)
         :documentation "Free returned or sent value")))

(defgeneric free-ptr (type ptr)
  (:method ((type freeable-base) ptr)
    (foreign-free ptr)))

(defgeneric free-sent-ptr (type ptr)
  (:method ((type freeable-base) ptr)
    (free-ptr type ptr)))

(defgeneric free-returned-ptr (type ptr)
  (:method ((type freeable-base) ptr)
    (free-ptr type ptr)))

(defun free-sent-if-needed (type ptr)
  (when (member (obj-free type) '(:all :no-transfer))
    (free-sent-ptr type ptr)))

(defun free-returned-if-needed (type ptr)
  (when (member (obj-free type) '(:all :transfer))
    (free-returned-ptr type ptr)))

(defclass freeable (freeable-base) ())

(defmethod free-translated-object :after (ptr (type freeable) param)
  (declare (ignore param))
  (free-sent-if-needed type ptr))

(defmethod translate-from-foreign :after (ptr (type freeable))
  (free-returned-if-needed type ptr))


(define-foreign-type cffi-struct (cffi-object freeable)
  ((out :accessor obj-out :initarg :out
        :documentation "This is out param (for fill in gtk side)"))
  (:actual-type :pointer))

(defmethod free-ptr ((type cffi-struct) ptr)
  (free-struct (obj-class type) ptr))

(defmethod foreign-type-size ((type cffi-struct))
  "Return the size in bytes of a foreign typedef."
  (foreign-type-size (obj-class type)))

;(defmethod cffi::aggregatep ((type cffi-struct)) t)

;(defmethod cffi::canonicalize ((type cffi-struct))
;  `(:struct ,(obj-class type)))

(define-parse-method struct (class &key free out)
  (make-instance 'cffi-struct
                 :class class :free free :out out))

(defun %class (type value)
  (or (obj-class type) (class-name (class-of value))))

(defmethod translate-to-foreign ((value struct) (type cffi-object))
  (values (clos->new-struct (%class type value) value) value))

(defmethod free-translated-object (value (type cffi-struct) (param struct))
  (when (and (slot-boundp param 'value)
             (obj-out type))
    (struct->clos (%class type param) value param)))

(defmethod translate-from-foreign (value (type cffi-struct))
  (let ((class (obj-class type)))
    (struct->clos class value)))

;;; for use with pobject designator
;; pobject == (struct nil :out t :free t)

(defmethod free-translated-object (value (type cffi-object) (param struct))
  (let ((class (%class type param)))
    (when (slot-boundp param 'value)
      (struct->clos class value param)
      (free-struct class value))))



(eval-when (:compile-toplevel :load-toplevel :execute) 
   (unless (get 'mem-ref 'struct)
     (let ((old (fdefinition 'mem-ref)))
       (fmakunbound 'mem-ref)
       (defun mem-ref (ptr type &optional (offset 0))
         (let ((ptype (cffi::parse-type type)))
           (if (subtypep (type-of ptype) 'cffi-struct)
               (translate-from-foreign (inc-pointer ptr offset) ptype)
               (funcall old ptr type offset)))))
     (setf (get 'mem-ref 'struct) t)))

(defun from-foreign (var type count)
  "VAR - symbol; type - symbol or list -- CFFI type; count -- integer"
  (if count
      (let ((res (make-array count)))
        (dotimes (i count)
          (setf (aref res i)
                (mem-aref var type i)))
        res)
      (mem-ref var type)))


(defmacro with-foreign-out ((var type &optional count) return-result &body body)
  "The same as WITH-FOREIGN-OBJECT, but returns value of object"
  (let ((value `(from-foreign ,var ,type ,count)))
  `(with-foreign-object (,var ,type ,@(when count (list count)))
     ,(if (eq return-result :ignore)
          `(progn ,@body ,value)
          `(let ((res ,@body))
             ,(ecase return-result
                     (:if-success `(when res ,value))
                     (:return `(values res ,value))))))))

(flet 
    ((make-with-foreign-outs (res-fun bindings return-result body)
       (let ((values-form (mapcar (lambda (x)
                                    (destructuring-bind 
                                          (var type &optional count) x
                                      `(from-foreign ,var ,type ,count)))
                                  bindings)))
         `(with-foreign-objects ,bindings
            ,(if (eq return-result :ignore)
                 `(progn ,@body (,res-fun ,@values-form))
                 `(let ((res ,@body))
                    ,(ecase return-result
                            (:if-success
                             `(when res (,res-fun ,@values-form)))
                            (:return
                              `(,res-fun res ,@values-form)))))))))
  
  (defmacro with-foreign-outs (bindings return-result &body body)
    "The same as WITH-FOREIGN-OBJECTS, but returns (values ...) 
of result and binded vars, RETURN-RESULT may be 
:RETURN - return result and values
:IF-SUCCESS - return values if result t
:IGNORE - discard result"
    (make-with-foreign-outs 'values bindings return-result body))

  (defmacro with-foreign-outs-list (bindings return-result &body body)
    "The same as WITH-FOREIGN-OBJECTS, but returns list"
    (make-with-foreign-outs 'list bindings return-result body)))
