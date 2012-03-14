;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; defslots.lisp --- def*slot(s) macros for group binding setters and getters
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:g-object-cffi)

(defvar *gtk-prefixes* nil
  "Assoc: lisp package -> C function prefix")

(defun register-prefix (package prefix)
  (push (cons package prefix) *gtk-prefixes*))

(defun get-prefix ()
  (cdr (assoc *package* *gtk-prefixes*)))

(defun pair (maybe-pair)
  (if (consp maybe-pair) maybe-pair (cons maybe-pair maybe-pair)))

(defun expand-defslot (prefix current-class slot-name slot-type)
  (destructuring-bind (name-lisp . name-gtk) (pair slot-name)
    (let ((getter (symbolicate prefix '- current-class '-get- name-gtk))
          (setter (symbolicate prefix '- current-class '-set- name-gtk)))
      `(progn
         (save-setter ,current-class ,name-lisp)
         (defcfun ,getter ,slot-type (object pobject))
         (defcfun ,setter :void (widget pobject) (value ,slot-type))
         (unless (fboundp ',name-lisp)
           (defgeneric ,name-lisp (,current-class)))
         (unless (fboundp '(setf ,name-lisp))
           (defgeneric (setf ,name-lisp) (value ,current-class)))
         (defmethod ,name-lisp ((object ,current-class)) (,getter object))
         (defmethod (setf ,name-lisp) (value (object ,current-class))
           (,setter object value) value)))))

(template ((defgtkslot 'gtk)
           (defgdkslot 'gdk)
           (defslot (get-prefix)))
   (destructuring-bind (name prefix) param
         `(defmacro ,name (current-class slot-name slot-type)
            (expand-defslot ,prefix current-class slot-name slot-type))))

(defun expand-defslots (prefix current-class slots)
  `(progn
     (clear-setters ,current-class)
     ,@(iter
        (for x on slots by #'cddr) 
        (collect 
            (expand-defslot prefix current-class (first x) (second x))))))

(template ((defgtkslots 'gtk)
           (defgdkslots 'gdk)
           (defslots (get-prefix)))
   (destructuring-bind (name prefix) param
     `(defmacro ,name (current-class &rest slots)
        (expand-defslots ,prefix current-class slots))))
 
(defun expand-deffun (prefix name res-type class params &key get)
  (destructuring-bind (name-lisp . name-gtk) (pair name)
    (let ((fun-name (symbolicate prefix '- class (if get '-get- '-) name-gtk))
          (param-list (mapcar #'car params)))
      `(progn            
         (defcfun ,fun-name ,res-type (,class pobject) ,@params)
         (unless (fboundp ',name-lisp)
           (defgeneric ,name-lisp (,class ,@param-list)))
         (defmethod ,name-lisp ((,class ,class) ,@param-list)
           (,fun-name ,class ,@param-list))))))


(template ((defgtkfun 'gtk)
           (defgdkfun 'gdk)
           (deffun (get-prefix)))
   (destructuring-bind (name prefix) param        
     `(defmacro ,name (name res-type class &rest params)
        (expand-deffun ,prefix name res-type class params))))

(template ((defgtkgetter 'gtk)
           (defgdkgetter 'gdk)
           (defgetter (get-prefix)))
   (destructuring-bind (name prefix) param        
     `(defmacro ,name (name res-type class &rest params)
        (expand-deffun ,prefix name res-type class params :get t))))


(defun expand-defsetter (prefix name slot-type class params last)
  (destructuring-bind (name-lisp . name-gtk) (pair name)
    (let ((setter (symbolicate prefix '- class '-set- name-gtk))
          (param-list (mapcar #'car params)))
      `(progn
         ,(unless params `(save-setter ,class ,name-lisp))
         ,(if last
              `(defcfun ,setter :void (widget pobject) 
                        ,@params (value ,slot-type)) 
              `(defcfun ,setter :void (widget pobject) 
                        (value ,slot-type) ,@params))
         (unless (fboundp '(setf ,name-lisp))
           (defgeneric (setf ,name-lisp) (value ,class ,@param-list)))
         (defmethod (setf ,name-lisp) (value (object ,class) ,@param-list) 
           (,setter object value ,@param-list) value)))))

(template ((defgtksetter 'gtk)
           (defgdksetter 'gdk)
           (defsetter (get-prefix)))
  (destructuring-bind (name prefix) param        
    `(defmacro ,name (name slot-type class last &rest params)
       (expand-defsetter ,prefix name slot-type class params last))))

(defun expand-deffuns (prefix class funs)
  (cons 'progn
        (mapcar (lambda (fun)
                  (destructuring-bind (name slot-type &rest params) 
                      (if (keywordp (car fun)) (cdr fun) fun)
                    (case (car fun)
                      (:set (expand-defsetter prefix 
                                              name slot-type class params nil))
                      (:set-last (expand-defsetter prefix 
                                                   name slot-type class 
                                                   params t))
                      (:get (expand-deffun prefix 
                                           name slot-type class params :get t))
                      (t (expand-deffun prefix name slot-type class params)))))
                  funs)))

(template ((defgtkfuns 'gtk)
           (defgdkfuns 'gdk)
           (deffuns (get-prefix)))
  (destructuring-bind (name prefix) param        
    `(defmacro ,name (class &rest funs)
       (expand-deffuns ,prefix class funs))))

(defmacro with-object ((name &optional for-free) init &rest body)
  `(let ((,name ,init))
     (unwind-protect
         (progn
           ,@body)
       (free ,(or for-free name)))))

(defvar *callback* nil
  "Lisp callback for use in gtk methods, that need callback function")

(defgeneric foreach (class func &optional data)
  (:documentation "For each element in CLASS execute FUNC"))
(defmacro make-foreach (class &rest params)
  (let ((gtk-name (symbolicate 'gtk- class '-foreach))
        (cb-name (gensym)))
  `(progn
     (defcfun ,gtk-name :void
       (,class pobject) (func pfunction) (data (pdata :free t)))
     (defcallback ,cb-name :void ,params ;((tag pobject) (data pdata))
       (funcall *callback* ,@(mapcar #'car params)))
     (defmethod foreach ((,class ,class) func &optional data)
       (if (functionp func)
           (let ((*callback* func))
             (,gtk-name ,class (callback ,cb-name) data))
           (,gtk-name ,class func data))))))
