(in-package :gtk-cffi)

(defclass entry (widget)
  ())

(defcstruct border
  ""
  (left :int)
  (right :int)
  (top :int)
  (bottom :int))

(defcfun "gtk_entry_new" :pointer)

;(defcfun "gtk_entry_new_with_max_length" :pointer (max :int))

(defmethod gconstructor ((entry entry)
                         &key &allow-other-keys)
  (gtk-entry-new))

(defcfun gtk-entry-get-text gtk-string (entry pobject))
(defcfun gtk-entry-set-text :void (entry pobject) (text gtk-string))

(defmethod text ((entry entry) &rest rest)
  (declare (ignore rest))
  (gtk-entry-get-text entry))

(defmethod (setf text) (value (entry entry) &rest rest)
  (declare (ignore rest))
  (gtk-entry-set-text entry value))

(defgtkslots entry
    visibility :boolean
    max-length :int
;    entry-buffer pobject
    activates-default :boolean
    has-frame :boolean
    inner-border border
    width-chars :int
    alignment :float
    overwrite-mode :boolean
    completion pobject
    cursor-hadjustment pobject
    progress-fraction :double
    progress-pulse-step :double)
    


