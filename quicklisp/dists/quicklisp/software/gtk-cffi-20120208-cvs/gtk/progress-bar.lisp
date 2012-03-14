(in-package :gtk-cffi)

(defclass progress-bar (widget)
  ())

(defcfun "gtk_progress_bar_new" :pointer)

(defmethod gconstructor ((progress-bar progress-bar)
                         &key &allow-other-keys)
  (gtk-progress-bar-new))

(defcfun "gtk_progress_bar_set_fraction" :void
  (bar pobject) (fraction :double))

(defmethod (setf fraction) (fraction (progress-bar progress-bar))
  (gtk-progress-bar-set-fraction progress-bar (coerce fraction 'double-float)))

(defcfun "gtk_progress_bar_get_fraction" :double (bar pobject))

(defmethod fraction ((progress-bar progress-bar))
  (gtk-progress-bar-get-fraction progress-bar))
