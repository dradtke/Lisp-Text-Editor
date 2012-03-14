(in-package :gtk-cffi)

(defclass tree-selection (g-object)
  ((mode :accessor mode :initarg :mode :initform :single)))

(defcenum selection-mode
  :none :single :browse :multiple)

(defcfun "gtk_tree_selection_set_mode" :void
  (selection pobject) (selection-mode selection-mode))

(defmethod (setf mode) :after (mode (tree-selection tree-selection))
  (gtk-tree-selection-set-mode tree-selection mode))

(defcfun "gtk_tree_selection_get_selected" :boolean
  (selection :pointer) (model :pointer) (iter :pointer))

(defcfun "gtk_tree_selection_selected_foreach" :void
  (selection :pointer) (func :pointer) (data :pointer))

(defvar *tree-selection-foreach* nil)

(defcallback cb-tree-selection-foreach :boolean
  ((model :pointer) (path :pointer) (iter :pointer) (data :pointer))
  (when *tree-selection-foreach*
    (funcall *tree-selection-foreach*
             (object model)
             (make-instance 'tree-path :pointer path)
             (make-instance 'tree-iter :pointer iter)
             (object data))))

(defmethod tree-selection-foreach ((tree-selection tree-selection)
                               func &optional (data (null-pointer)))
  (let ((*tree-selection-foreach* func))
    (gtk-tree-selection-selected-foreach (pointer tree-selection)
                            (callback cb-tree-selection-foreach) data)))

(defvar *selected* nil)

(defmethod get-selected ((tree-selection tree-selection))
  "Returns list (model iter &optional iter2 iter3 ...)"
  (if (eq (mode tree-selection) :multiple)
      (progn
        (let ((*selected* nil))
          (tree-selection-foreach tree-selection
                                  (lambda (model path iter data)
                                    (declare (ignore data path))
                                    (unless *selected*
                                      (push model *selected*))
                                    (push (copy iter) *selected*)))
          (when *selected*
            (debug-out "selected: ~a~%" *selected*)
            (nreverse *selected*))))
        
    (let ((iter (make-instance 'tree-iter)))
      (with-foreign-object (model-ptr :pointer)
                           (when (gtk-tree-selection-get-selected
                                  (pointer tree-selection)
                                  model-ptr (pointer iter))
                             (list (object (mem-ref model-ptr :pointer))
                                   iter))))))

(defmacro with-selection (selection tree-selection &body body)
  `(let ((,selection (get-selected ,tree-selection)))
     (unwind-protect
         (progn ,@body)
       (mapc #'free (cdr ,selection)))))

(defmethod initialize-instance
  :after ((tree-selection tree-selection)
          &key (mode :single) &allow-other-keys)
;  (when pointer
;    (setf (pointer tree-selection) pointer)) ;; to save in *objects* 
  (unless (eq mode :single)
    (setf (mode tree-selection) mode)))




