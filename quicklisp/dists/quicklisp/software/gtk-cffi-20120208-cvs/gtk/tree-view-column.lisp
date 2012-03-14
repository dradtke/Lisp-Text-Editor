(in-package :gtk-cffi)

(defclass tree-view-column (cell-layout)
  ())

(defcfun "gtk_tree_view_column_new" :pointer)

(defmethod gconstructor ((tree-view-column tree-view-column)
                         &key &allow-other-keys)
  (gtk-tree-view-column-new))

(defmethod initialize-instance
  :after ((tree-view-column tree-view-column)
          &rest initargs &key title cell &allow-other-keys)
  "other attributes = (:text 2 :color 3)"
  (setf-init tree-view-column title)
  (when cell
    (pack tree-view-column cell)
    (labels ((process (x)
                      (when x
                        (let ((key (first x))
                              (val (second x)))
                          (when (and (keywordp key)
                                     (not (member key '(:title :cell))))
                            (add-attribute tree-view-column cell key val)))
                        (process (cddr x)))))
      (process initargs))))

(defcfun "gtk_tree_view_column_set_title" :void
  (column pobject) (title gtk-string))

(defcfun "gtk_tree_view_column_get_title" gtk-string (column pobject))
  
(defmethod (setf title) (value (tree-view-column tree-view-column))
  (gtk-tree-view-column-set-title tree-view-column value))

(defmethod title ((tree-view-column tree-view-column))
  (gtk-tree-view-column-get-title tree-view-column))

(defcfun "gtk_tree_view_column_set_sort_column_id" :void
  (column :pointer) (id :int))

(defmethod (setf sort-column-id) (id (tree-view-column tree-view-column))
  (gtk-tree-view-column-set-sort-column-id (pointer tree-view-column) id))

(defcfun "gtk_tree_view_column_set_alignment" :void
  (column :pointer) (xalign :float))

(defmethod (setf alignment) (xalign (tree-view-column tree-view-column))
  (gtk-tree-view-column-set-alignment (pointer tree-view-column)
                                      (float xalign)))

(defcfun "gtk_tree_view_column_set_reorderable" :void
  (column :pointer) (reorderable :boolean))

(defmethod (setf reorderable) (reorderable (tree-view-column tree-view-column))
  (gtk-tree-view-column-set-reorderable (pointer tree-view-column)
                                        reorderable))

(defcfun "gtk_tree_view_column_get_reorderable" :boolean
  (column :pointer))

(defmethod reorderable ((tree-view-column tree-view-column))
  (gtk-tree-view-column-get-reorderable (pointer tree-view-column)))

(defcfun "gtk_tree_view_column_set_cell_data_func" :void
  (column pobject) (renderer pobject) (func :pointer)
  (data pdata) (notify :pointer))

(defmethod (setf cell-data-func) (c-handler (tree-view-column tree-view-column)
                                            (cell-renderer cell-renderer)
                                            &optional
                                            (data (null-pointer))
                                            (destroy-notify (null-pointer)))
  (gtk-tree-view-column-set-cell-data-func tree-view-column cell-renderer
                                           c-handler data destroy-notify))

(defcfun "gtk_tree_view_column_set_widget" :void
  (column pobject) (widget pobject))

(defmethod (setf widget) ((widget widget)
                          (tree-view-column tree-view-column))
  (gtk-tree-view-column-set-widget tree-view-column widget))

(defcfun "gtk_tree_view_column_get_widget" pobject
  (column pobject))

(defmethod widget ((tree-view-column tree-view-column))
  (gtk-tree-view-column-get-widget tree-view-column))



(defcfun "gtk_tree_view_column_cell_get_position" :boolean
  (column pobject) (cell-renderer pobject)
  (start-pos :pointer) (width :pointer))

(defmethod cell-get-position ((tree-view-column tree-view-column)
                              (cell-renderer cell-renderer))
  (with-foreign-objects
   ((start-pos :int)
    (width :int))
   (gtk-tree-view-column-cell-get-position tree-view-column
                                           cell-renderer start-pos width)
   (list (mem-ref start-pos :int) (mem-ref width :int))))


(defmethod get-cell-at ((tree-view-column tree-view-column) x)
  (loop :for cell in (cell-renderers tree-view-column)
        :when (destructuring-bind (start-pos width)
                  (cell-get-position tree-view-column cell)
                (and (>= x start-pos) (>= (+ start-pos width) x)))
        :return cell))
