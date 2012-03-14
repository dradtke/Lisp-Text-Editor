(in-package #:gtk-cffi)

(defclass tree-path (object)
  ())

(defcfun "gtk_tree_path_new" :pointer)

(defcfun "gtk_tree_path_free" :void (path pobject))

(defcfun "gtk_tree_path_new_from_string" :pointer (str gtk-string))

(defcfun "gtk_tree_path_new_from_indices" :pointer &rest)

(defcfun "gtk_tree_path_append_index" :void (path pobject) (index :int))

(defmethod gconstructor ((tree-path tree-path)
                         &key string indices &allow-other-keys)
  (cond
   (string (gtk-tree-path-new-from-string string))
   (indices (let ((ptr (gtk-tree-path-new)))
              (mapc (lambda (x)
                      (gtk-tree-path-append-index ptr x))
                    indices)
              ptr))
   (t (gtk-tree-path-new))))


(defmethod free :before ((tree-path tree-path))
  (gtk-tree-path-free tree-path))

(defcfun "gtk_tree_path_get_depth" :int (path pobject))

(defcfun "gtk_tree_path_get_indices" :pointer (path pobject))

(defmethod get-indices ((tree-path tree-path))
  (let* ((ptr (pointer tree-path))
         (depth (gtk-tree-path-get-depth ptr))
         (array (gtk-tree-path-get-indices ptr)))
    (loop :for i :below depth
          :collect (mem-aref array :int i))))

(defmethod get-index ((tree-path tree-path) &optional (pos 0))
  (mem-aref (gtk-tree-path-get-indices (pointer tree-path)) :int pos))

(defclass tree-row (object)
  ())

(defcfun "gtk_tree_row_reference_new"
  :pointer (model pobject) (path pobject))

(defcfun "gtk_tree_row_reference_free"
  :void (row pobject))

(defmethod gconstructor ((tree-row tree-row)
                         &key model path &allow-other-keys)
  (gtk-tree-row-reference-new model path))

(defmethod free :before ((tree-row tree-row))
  (gtk-tree-row-reference-free tree-row))

(defcfun "gtk_tree_row_reference_copy" (object tree-row) (row pobject))

(defmethod copy ((tree-row tree-row))
  (gtk-tree-row-reference-copy tree-row))

(defcstruct tree-iter-struct
    "GtkTreeIter"
  (stamp :int)
  (u1 :pointer)
  (u2 :pointer)
  (u3 :pointer))

(defclass tree-iter (object)
  ())

(defmethod gconstructor ((tree-iter tree-iter)
                         &key &allow-other-keys)
  (foreign-alloc 'tree-iter-struct))

(defmethod copy ((tree-iter tree-iter))
  (let* ((res (make-instance 'tree-iter))
         (ptr (pointer tree-iter))
         (new-ptr (pointer res)))
    (mapc (lambda (x)
              (setf (foreign-slot-value new-ptr 'tree-iter-struct x)
                    (foreign-slot-value ptr 'tree-iter-struct x)))
            (foreign-slot-names 'tree-iter-struct))
    res))

(defcfun "gtk_tree_iter_free" :void (iter pobject))

(defmethod free :before ((tree-iter tree-iter))
  (gtk-tree-iter-free tree-iter))

(defclass tree-model (object)
  ((columns :accessor columns :initarg :columns)
   (iter :accessor tree-iter :documentation "Current tree-iter")))

(defcstruct tree-model-iface
    "GtkTreeModelIface"
  (g-iface g-type-interface)
  (row-changed :pointer)
  (row-inserted :pointer)
  (has-child-toggled :pointer)
  (row-deleted :pointer)
  (row-reordered :pointer)

  ; virtual methods
  (get-flags :pointer)
  (get-n-columns :pointer)
  (get-column-type :pointer)
  (get-iter :pointer)
  (get-path :pointer)
  (get-value :pointer)
  (iter-next :pointer)
  (iter-previous :pointer)
  (iter-children :pointer)
  (iter-has-child :pointer)
  (iter-n-children :pointer)
  (iter-nth-child :pointer)
  (iter-parent :pointer)
  (ref-node :pointer)
  (unref-node :pointer))

(defmethod initialize-instance
  :after ((tree-model tree-model)
          &key &allow-other-keys)
  (setf (tree-iter tree-model) (make-instance 'tree-iter)))

(defmethod free :before ((tree-model tree-model))
  (free (tree-iter tree-model)))

(make-foreach tree-model 
              (path (object tree-path)) 
              (tree-iter (object tree-iter)))

(defcfun "gtk_tree_model_get_path" (object tree-path) 
  (model pobject) (iter pobject))

(defmethod iter->path ((tree-model tree-model) (tree-iter tree-iter))
  (warn "Dangerous method: don't forget to use free")
  (gtk-tree-model-get-path tree-model tree-iter))

(defcfun "gtk_tree_model_get_string_from_iter" :string
  (model pobject) (iter pobject))

(defmethod iter->string ((tree-model tree-model) (tree-iter tree-iter))
  (gtk-tree-model-get-string-from-iter tree-model tree-iter))

(defcfun "gtk_tree_model_get_value" :void (model pobject) (iter pobject)
  (column :int) (g-value pobject))

(defmethod model-values
  ((tree-model tree-model) &key
   (iter (tree-iter tree-model)) col (columns (ensure-list col)))
  "columns = num0 &optional num1 num2 ..."
  ;(format t "model-values: ~a ~a ~a~%" tree-model tree-iter cols)
  (mapcar
   (lambda (col) 
     (with-g-value ()  
       (gtk-tree-model-get-value tree-model
                                 iter col *g-value*)))
   columns))

(defcfun "gtk_tree_model_get_iter" :boolean
  (model pobject) (iter pobject) (path pobject))

(defmethod path->iter ((tree-model tree-model) (tree-path tree-path)
                       &optional (tree-iter (tree-iter tree-model)))
  (gtk-tree-model-get-iter tree-model tree-iter tree-path) 
  tree-iter)

(defcfun "gtk_tree_model_get_iter_from_string" :boolean
  (model pobject) (iter pobject) (path :string))

(defmethod path->iter ((tree-model tree-model) tree-path-string
                       &optional (tree-iter (tree-iter tree-model)))
  (gtk-tree-model-get-iter-from-string tree-model tree-iter tree-path-string)
  tree-iter)

(defmacro with-tree-iter (var &body body)
  `(with-object (,var) (make-instance 'tree-iter)
                ,@body))

(defcfun gtk-tree-model-get-n-columns :int (tree-model pobject))

(defmethod n-columns ((tree-model tree-model))
  (gtk-tree-model-get-n-columns tree-model))

(defcfun gtk-tree-model-get-column-type :int (tree-model pobject) (col :int))

(defmethod column-type ((tree-model tree-model) col)
  (gtk-tree-model-get-column-type tree-model col))



