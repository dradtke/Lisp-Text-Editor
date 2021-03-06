(in-package :gtk-cffi)

(defclass cell-layout (g-object)
  ())


(defcfun "gtk_cell_layout_add_attribute" :void
  (layout pobject) (cell pobject) (attr :string) (column :int))

(defmethod add-attribute ((cell-layout cell-layout)
                          (cell-renderer cell-renderer) attr column)
  (gtk-cell-layout-add-attribute cell-layout cell-renderer
                                 (string-downcase attr) column))

(defcfun "gtk_cell_layout_pack_start" :void
  (cell-layout pobject) (renderer pobject) (expand :boolean))

(defcfun "gtk_cell_layout_pack_end" :void
  (cell-layout pobject) (renderer pobject) (expand :boolean))

(defmethod pack ((cell-layout cell-layout)
                 (cell-renderer cell-renderer)
                 &key end expand)
  (funcall (if end
               #'gtk-cell-layout-pack-end
             #'gtk-cell-layout-pack-start)
           cell-layout cell-renderer expand))

(defcfun "gtk_cell_layout_get_cells" g-list-object (cell-layout pobject))

(defmethod cell-renderers ((cell-layout cell-layout))
  (gtk-cell-layout-get-cells cell-layout))

(defcallback cb-cell-data-func :void
  ((cell-layout pobject) (cell-renderer pobject)
   (model pobject) (iter :pointer) (data pdata))
  (funcall data cell-layout cell-renderer model
           (make-instance 'tree-iter :pointer iter)))

(defcfun "gtk_cell_layout_set_cell_data_func" :void
  (cell-layout pobject) (renderer pobject) (func pfunction)
  (data pdata) (notify :pointer))

(defmethod (setf cell-data-func) (c-handler
                                  (cell-layout cell-layout)
                                  (cell-renderer cell-renderer)
                                  &optional data destroy-notify)
                                  
  (if (functionp c-handler)
      (gtk-cell-layout-set-cell-data-func
       cell-layout cell-renderer
       (callback cb-cell-data-func)
       (pointer (make-instance 'storage :data c-handler))
       (callback free-storage))
    (gtk-cell-layout-set-cell-data-func
     cell-layout cell-renderer
     c-handler
     data
     ;; destroy-notify
     (or destroy-notify
         (if (or (null data) (pointerp data) (typep data 'g-object))
             (null-pointer) (callback free-storage))))))

(defcfun "gtk_cell_layout_clear_attributes" :void
  (cell-layout pobject) (cell-renderer pobject))

(defmethod clear-attributes ((cell-layout cell-layout)
                             (cell-renderer cell-renderer))
  (gtk-cell-layout-clear-attributes cell-layout cell-renderer))

(defcfun "gtk_cell_layout_clear" :void (cell-layout pobject))

(defmethod clear ((cell-layout cell-layout))
  (gtk-cell-layout-clear cell-layout))