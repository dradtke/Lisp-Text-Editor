(in-package :gtk-cffi)

(defclass image (misc)
  ())

(defcfun "gtk_image_new_from_file" :pointer (filename gtk-string))
;(defcenum "gtk_image_new_from_icon_set" :pointer 
;          (icon-set pobject) (icon-size icon-size))
(defcfun "gtk_image_new_from_pixbuf" :pointer (pixbuf pobject))
(defcfun "gtk_image_new_from_stock" :pointer 
  (stock-id gtk-string) (size icon-size))

(defmethod gconstructor ((image image) &key file pixbuf stock-id icon-size)
  (cond
    (file (gtk-image-new-from-file file))
    (pixbuf (gtk-image-new-from-pixbuf pixbuf))
    (stock-id (gtk-image-new-from-stock stock-id icon-size))))