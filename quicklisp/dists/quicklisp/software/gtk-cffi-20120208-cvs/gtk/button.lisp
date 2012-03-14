(in-package :gtk-cffi)

(defclass button (bin)
  ())

(defcfun "gtk_button_new" :pointer)

(defcfun "gtk_button_new_with_label" :pointer (label gtk-string))

(defcfun "gtk_button_new_with_mnemonic" :pointer (label gtk-string))

(defcfun "gtk_button_new_from_stock" :pointer (label gtk-string))

(defmethod gconstructor ((button button)
                         &key label type &allow-other-keys)
  "type can be :stock or :mnemonic, any other means button with label"
  (if label
      (let ((creator
             (case type
               (:stock #'gtk-button-new-from-stock)
               (:mnemonic #'gtk-button-new-with-mnemonic)
               (otherwise #'gtk-button-new-with-label))))
        (funcall creator label))
    (gtk-button-new)))

