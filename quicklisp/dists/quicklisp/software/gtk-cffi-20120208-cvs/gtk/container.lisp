(in-package :gtk-cffi)

(defclass container (widget)
  ((%child-properties :accessor %child-properties 
                      :initform nil :allocation :class)))

(defcenum resize-mode
  :parent :queue :immediate)

(defcfun "gtk_container_add" :void (container pobject) (widget pobject))

(defgtkslots container
    border-width :uint
    resize-mode resize-mode
    focus-child pobject
    focus-vadjustment pobject
    focus-hadjustment pobject)

(defmethod add ((container container) (widget widget))
  (gtk-container-add container widget))

(defmethod pack ((container container) (widget widget) &rest rest)
  (declare (ignore rest))
  (add container widget))

(defmacro pack* (box &rest widgets)
  `(progn
     ,@(mapcar
        (lambda (widget) 
          `(pack ,box ,@(ensure-cons widget)))
        widgets)))

(defmethod (setf kids) (kids (container container))
  (mapc (lambda (x) (setf (kid container) x)) kids))

(defmethod (setf kid) (kid (container container))
  (pack container kid))

(defcfun "gtk_widget_reparent" :void (widget pobject) (parent pobject))

(defmethod reparent ((widget widget) (new-parent container))
  (gtk-widget-reparent widget new-parent))

(defmethod initialize-instance
  :after ((container container)
          &key kid kids &allow-other-keys)
  (setf-init container kid kids))

(defmacro pack-with-param (container token cur-param keyword-list)
  "Handle to let user set (pack* box widget1 :expand t widget2 widget3)
Here, widget2 and widget3 will be packed with expand."
  `(if (member ,token ,keyword-list) ;'(:pack-fill :padding :expand))
       (setf (slot-value ,container ',cur-param)
             (intern (string ,token) #.*package*))
       (let ((param (slot-value ,container ',cur-param)))
         (when param
           (setf (slot-value ,container param) ,token)))))

(defcfun gtk-container-child-get-property :void 
  (container pobject) (widget pobject) (name :string) (value pobject))

(defcfun gtk-container-child-set-property :void 
  (container pobject) (widget pobject) (name :string) (value pobject))

(defgeneric child-property-type (container key))

(defmethod child-property-type ((container container) (key symbol))
  (child-property-type container (string-downcase key)))

(defmethod child-property-type ((container container) (key string))
  "Should return GType of property KEY."
  (or (cdr (assoc key (%child-properties container)))
      (let* ((gclass (make-instance 'g-object-cffi:g-object-class 
                                    :object container))
             (prop (find-child-property gclass key)))
        (when prop
          (let ((g-type (g-type prop)))
            (setf (%child-properties container)
                  (acons key g-type (%child-properties container)))
            g-type)))
      (error "Incorrect child property name ~a" key)))

(defmethod child-property ((widget widget) (parent container) &rest keys)
  (funcall (lambda (x) (if (cdr x) x (car x)))
           (mapcar (lambda (key)
                     (let* ((skey (string-downcase key))
                            (g-type (child-property-type parent skey)))
                       (with-g-value
                        (:g-type g-type)
                         (gtk-container-child-get-property 
                          parent widget skey *g-value*))))
                   keys)))

(defmethod child-property ((widget widget) (parent null) &rest keys)
  (apply #'child-property `(,widget ,(parent widget) ,@keys)))

(defmethod (setf child-property) (values (widget widget) (parent container) 
                                  &rest keys)
  "
Usage: (setf (child-property object parent :property) value)
       (setf (child-property object parent :prop1 :prop2) 
             (list value1 value2))"
  (mapc (lambda (key value)
          (declare (type (or symbol string) key))
          (let ((skey (string-downcase key)))
            (with-g-value (:value value 
                           :g-type (child-property-type parent skey))
              (gtk-container-child-set-property parent widget 
                                                skey *g-value*))))
        keys (if (listp values) values (list values))))

(defmethod (setf child-property) (values (widget widget) (parent null)
                                  &rest keys)
  (apply #'(setf child-property) `(,values ,widget ,(parent widget) ,@keys)))

(defcfun "gtk_container_class_find_child_property" :pointer
  (obj-class pobject) (key :string))

(defmethod find-child-property ((container container) key)
  (let ((ptr (gtk-container-class-find-child-property container key)))
    (unless (null-pointer-p ptr)
      (make-instance 'g-object-cffi:g-param-spec :pointer ptr))))

(defcfun gtk-container-remove :void (container pobject) (widget pobject))

(defmethod container-remove ((container container) (widget widget))
  (gtk-container-remove container widget))

