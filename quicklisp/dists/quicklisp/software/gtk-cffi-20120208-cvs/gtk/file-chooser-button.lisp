(in-package :gtk-cffi)

(defclass file-chooser-button (h-box file-chooser)
  ())

(defcfun "gtk_file_chooser_button_new" :pointer
  (title gtk-string) (action file-chooser-action))

;(defcfun "gtk_file_chooser_button_new_with_backend" :pointer
;  (title gtk-string) (action file-chooser-action) (backend gtk-string))

(defmethod gconstructor ((file-chooser-button file-chooser-button)
                         &key title action &allow-other-keys)
  (gtk-file-chooser-button-new title action))
