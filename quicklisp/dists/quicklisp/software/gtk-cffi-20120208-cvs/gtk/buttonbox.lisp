(in-package :gtk-cffi)

(defclass button-box (box)
  ())

(defcfun "gtk_button_box_set_child_secondary" :void
  (button-box pobject) (widget pobject) (secondary :boolean))

(defcfun "gtk_button_box_get_child_secondary" :boolean
  (button-box pobject) (widget pobject))

(defmethod child-secondary ((button-box button-box) (widget widget))
  (gtk-button-box-get-child-secondary button-box widget))

(defmethod (setf child-secondary) (secondary 
                                   (button-box button-box) (widget widget))
  (gtk-button-box-set-child-secondary button-box widget secondary))
