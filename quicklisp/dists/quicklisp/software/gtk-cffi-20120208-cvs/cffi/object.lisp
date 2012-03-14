;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; object.lisp --- CFFI type OBJECT
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :cffi-object)

(defvar *objects* (make-hash-table)
  "Hash table: foreign-pointer address as integer -> lisp object")

(defvar *objects-ids* (make-hash-table)
  "Hash table: atom -> lisp object")

(defclass object ()
  ((pointer :accessor pointer :initarg :pointer 
            :initform (null-pointer) :type foreign-pointer)
   ;; by default object shouldn't be stored unless it is GtkObject
   (volatile :type boolean :accessor volatile 
             :initarg :volatile :initform t
             :documentation "Will not be saved in hash")
   (free-after :type boolean :initarg :free-after :initform t
               :documentation "Should be freed by finalizer")
   (id :type symbol :accessor id :initarg :id :initform nil))
  (:documentation "Lisp wrapper for any object. VOLATILE slot set when object
shouldn't be stored in *OBJECTS*. Stored pointer GC'ed, if VOLATILE."))

(defmethod (setf pointer) :after (value (object object))
  (declare (type foreign-pointer value))
  (tg:cancel-finalization object)
  (when (and (slot-value object 'free-after) (not (null-pointer-p value)))
    (tg:finalize object (lambda () 
                          (foreign-free value))))
  (unless (or (volatile object) (null-pointer-p value))
    (setf (gethash (pointer-address value) *objects*) object)
    (when (id object)
      (let ((cur-obj (gethash (id object) *objects-ids*)))
        (unless (or (null cur-obj) (eq cur-obj object))
          (warn "ID ~a for object ~a already set for ~a~%"
                (id object) object (gethash (id object) *objects-ids*)))
        (setf (gethash (id object) *objects-ids*) object)))))

(defgeneric gconstructor (object &rest initargs)
  (:documentation "Called during initialization of GTK class instance.
Should return a pointer to GTK instance, for example, by g_object_new."))

(defmethod gconstructor (something-bad &rest rest)
  (warn "No constructor for ~a ~a~%" something-bad rest))

(defmethod shared-initialize :after ((object object) slot-names 
                              &rest initargs
                              &key pointer &allow-other-keys)
  (unless pointer
    (setf (pointer object) (apply #'gconstructor object initargs))))

(defmethod pointer (something-bad)
  (declare (ignore something-bad))
  "Empty method to return null-pointer for non-objects"
  (null-pointer))

(defgeneric free (object)
  (:documentation "Removes object pointer from lisp hashes."))

(defmethod free ((object object))
  (unless (null-pointer-p (pointer object))
    (debug-out "Freeing ~a@~a~%" (type-of object) (pointer object)) 
    (remhash (pointer-address (pointer object)) *objects*)
    (remhash (id object) *objects-ids*)
    (setf (pointer object) (null-pointer)
          (id object) nil)))

(defun object (pointer &key class)
  "Returns lisp object for an Object pointer.
If not found or found with wrong class, create new one with given CLASS"
  (declare (type symbol class) (type foreign-pointer pointer))
  (unless (null-pointer-p pointer)
    (let  ((try-find (gethash (pointer-address pointer) *objects*)))
      (if class
          (progn
            (unless (or (null try-find)
                        (eq (class-of try-find) (find-class class)))
              (progn
                (free try-find)
                (setf try-find nil)))
            (or try-find (make-instance class :pointer pointer)))
        try-find))))

(defun object-by-id (id-key)
  (gethash id-key *objects-ids*))

;; Type OBJECT
;;   converts class object to pointer and vice versa

(define-foreign-type cffi-object ()
  ((class :initarg :class :accessor obj-class))
  (:actual-type :pointer))

(define-parse-method object (&optional class)
  (make-instance 'cffi-object :class class))

(defmethod translate-to-foreign ((value null) (type cffi-object))
  (null-pointer))

(defmethod translate-to-foreign ((value object) (type cffi-object))
  (pointer value))

;; Hack: redefine translater for :pointer to be able to use
;;       objects or nulls instead of pointer
(defmethod translate-to-foreign ((value object)
                                 (type cffi::foreign-pointer-type))
  (pointer value))

(defmethod translate-to-foreign ((value null)
                                 (type cffi::foreign-pointer-type))
  (null-pointer))

(defmethod translate-to-foreign (value (type cffi-object))
  (check-type value foreign-pointer)
  value)

(defmethod translate-from-foreign (ptr (cffi-object cffi-object))
  (object ptr :class (obj-class cffi-object)))