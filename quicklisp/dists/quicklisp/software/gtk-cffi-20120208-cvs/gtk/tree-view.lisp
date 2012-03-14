(in-package :gtk-cffi)

(defclass tree-view (container)
  ())

(defcenum tree-view-grid-lines
  :none :horizontal :vertical :both) 

(defcfun "gtk_tree_view_new" :pointer)

(defcfun "gtk_tree_view_new_with_model" :pointer (model pobject))

(defmethod gconstructor ((tree-view tree-view)
                         &key model &allow-other-keys)
  (if model
      (gtk-tree-view-new-with-model model)
    (gtk-tree-view-new)))


(defmethod initialize-instance
  :after ((tree-view tree-view)
          &key columns on-select &allow-other-keys)
  (setf-init tree-view columns)
  (when on-select
    (setf (gsignal (get-selection tree-view) :changed)
          (lambda (selection)
            (let ((selected (get-selected selection)))
              (when (cdr selected)
                (apply on-select selected)))))))

(defmethod (setf columns) (columns (tree-view tree-view))
  (labels
      ((mk-column (column colnum)
                 (cond
                  ((stringp column)
                   (mk-column (list :title column
                                    :cell (make-instance 'cell-renderer-text)
                                    :text colnum) colnum))
                  ((consp column) (apply #'make-instance
                                         (cons 'tree-view-column column)))
                  (t column)))
                   
       (process (columns colnum)
                (let* ((col (car columns))
                       (col-obj (mk-column col colnum)))
                  (append-column tree-view col-obj))
                (when (cdr columns)
                  (process (cdr columns) (1+ colnum)))))
      
    (process columns 0)))

       

(defcfun "gtk_tree_view_append_column" :int
  (view pobject) (column pobject))

(defmethod append-column ((tree-view tree-view)
                          (tree-view-column tree-view-column))
  (gtk-tree-view-append-column tree-view tree-view-column))

(defcfun "gtk_tree_view_set_model" :void
  (view pobject) (model pobject))

(defmethod (setf model) ((tree-model tree-model) 
                         (tree-view tree-view))
  (gtk-tree-view-set-model tree-view tree-model))

(defmethod (setf model) ((tree-model null) 
                         (tree-view tree-view))
  (gtk-tree-view-set-model tree-view tree-model))


(defcfun "gtk_tree_view_get_model" pobject
  (view pobject))

(defmethod model ((tree-view tree-view))
  (gtk-tree-view-get-model tree-view))

(defcfun "gtk_tree_view_get_selection" :pointer (view pobject))

(defmethod get-selection ((tree-view tree-view))
  (make-instance 'tree-selection :pointer
                 (gtk-tree-view-get-selection tree-view)))

(defcfun "gtk_tree_view_get_path_at_pos" :boolean (view pobject)
  (x :int) (y :int) (path :pointer) (column :pointer)
  (cell-x :pointer) (cell-y :pointer))

(defmethod path-at-pos ((tree-view tree-view) x y)
  (with-foreign-objects
   ((path :pointer)
    (column :pointer)
    (cell-x :pointer)
    (cell-y :pointer))
   (when
       (gtk-tree-view-get-path-at-pos tree-view x y path column cell-x cell-y)
     (list
      (make-instance 'tree-path :pointer (mem-ref path :pointer))
      (mem-ref column 'pobject)
      (mem-ref cell-x :int) (mem-ref cell-y :int)))))

(defmacro with-path-at-pos (tree-view x y &rest body)
  `(with-object (%path (first %path)) (path-at-pos ,tree-view ,x ,y) ,@body))

(defcfun "gtk_tree_view_get_cursor" :void (view pobject)
  (path :pointer) (column :pointer))

(defmethod get-cursor ((tree-view tree-view))
  (with-foreign-objects
   ((path :pointer)
    (column :pointer))
   (gtk-tree-view-get-cursor tree-view path column)
   (list (make-instance 'tree-path :pointer (mem-ref path :pointer))
         (mem-ref column 'pobject))))

(defmacro with-get-cursor-path (tree-view &rest body)
  `(with-object (%path (first %path)) (get-cursor ,tree-view) ,@body))

(defcfun "gtk_tree_view_get_columns" g-list-object (tree-view pobject))

(defmethod columns ((tree-view tree-view))
   (gtk-tree-view-get-columns tree-view))

(defcfun "gtk_tree_view_get_column" pobject (tree-view pobject) (n :int))

(defmethod column ((tree-view tree-view) n)
  (gtk-tree-view-get-column tree-view n))

(defcfun "gtk_tree_view_set_search_column" :void (tree-view pobject) (n :int))

(defmethod (setf search-column) (n (tree-view tree-view))
  (gtk-tree-view-set-search-column tree-view n))

(defcfun "gtk_tree_view_get_search_column" :int (tree-view pobject))

(defmethod search-column ((tree-view tree-view))
  (gtk-tree-view-get-search-column tree-view))