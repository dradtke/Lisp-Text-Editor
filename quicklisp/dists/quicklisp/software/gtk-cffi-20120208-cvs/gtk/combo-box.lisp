(in-package :gtk-cffi)

(defclass combo-box (bin)
  ((model :accessor model :initarg :model)))

(defcfun "gtk_combo_box_new_with_model" :pointer (model pobject)) 

(defcfun "gtk_combo_box_new" :pointer)

(defcfun "gtk_combo_box_new_text" :pointer)

(defmethod initialize-instance
  :after ((combo-box combo-box)
          &key model text-only &allow-other-keys)
  (setf (pointer combo-box)
        (cond
         (model (gtk-combo-box-new-with-model model))
         (text-only (gtk-combo-box-new-text))
         (t (gtk-combo-box-new)))))


(defcfun "gtk_combo_box_set_model" :void (combo-box pobject) (model pobject))

(defmethod (setf model) :after ((tree-model tree-model) (combo-box combo-box))
  (gtk-combo-box-set-model combo-box tree-model))

(defmethod (setf model) :after (badval (combo-box combo-box))
  (error "Should be tree-model in setf model "))

(defcfun "gtk_combo_box_append_text" :void
  (combo-box pobject) (text gtk-string))

(defmethod append-text ((combo-box combo-box) text)
  (gtk-combo-box-append-text combo-box text))

(defcfun "gtk_combo_box_prepend_text" :void
  (combo-box pobject) (text gtk-string))

(defmethod prepend-text ((combo-box combo-box) text)
  (gtk-combo-box-prepend-text combo-box text))

(defcfun "gtk_combo_box_insert_text" :void
  (combo-box pobject) (text gtk-string))

(defmethod insert-text ((combo-box combo-box) text)
  (gtk-combo-box-insert-text combo-box text))

(defcfun "gtk_combo_box_remove_text" :void
  (combo-box pobject) (pos :int))

(defmethod remove-text ((combo-box combo-box) pos)
  (gtk-combo-box-remove-text combo-box pos))

(defcfun "gtk_combo_box_get_active_text" gtk-string (combo-box pobject))

(defmethod active-text ((combo-box combo-box))
  (gtk-combo-box-get-active-text combo-box))
  
