(in-package :gtk-cffi)

(defclass misc (widget)
  ())

(defcfun "gtk_misc_set_alignment" :void (misc pobject) (x :float) (y :float))

(defmethod (setf alignment) (coords (misc misc))
  (gtk-misc-set-alignment misc
                          (float (first coords))
                          (float (second coords))))

(defcfun "gtk_misc_get_alignment" :void (misc pobject)
  (x :pointer) (y :pointer))

(defmethod alignment ((misc misc))
  (with-foreign-objects ((x :float) (y :float))
                        (gtk-misc-get-alignment misc x y)
                        (list (mem-ref x :float)
                              (mem-ref y :float))))

