(in-package :gtk-cffi)

(defcenum window-type
  :top-level :popup)

(defclass window (bin)
  ())

(defmethod gconstructor ((window window)
                         &key (type :top-level) &allow-other-keys)
  (gtk-window-new type))

(defgtkslots window
    title gtk-string
    screen pobject
    transient-for pobject)

(defcfun "gtk_window_new" :pointer (type window-type))

(defcfun "gtk_window_set_default_size"
  :void (window pobject) (w :int) (h :int))

(defcfun "gtk_window_get_default_size"
  :void (window pobject) (w :pointer) (h :pointer))

(defmethod (setf default-size) (coords (window window))
  (let ((width (first coords))
        (height (second coords)))
    (gtk-window-set-default-size window (round width) (round height))))

(defmethod default-size ((window window))
  (with-foreign-objects
   ((width :int) (height :int))
   (gtk-window-get-default-size window width height)
   (list (mem-ref width :int) (mem-ref height :int))))

(defcenum position
  :none
  :center
  :mouse
  :center-always
  :center-on-parent)

(defcfun "gtk_window_set_position" :void (window pobject) (pos position))

(defmethod (setf window-position) (pos (window window))
  (gtk-window-set-position window pos))

(init-slots window ((width -1) (height -1) position)
  (when (or (/= width -1) (/= height -1))
    (gtk-window-set-default-size window width height))
  (when position (setf (window-position window) position)))

