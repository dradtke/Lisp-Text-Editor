(in-package :gtk-cffi)

(defclass file-chooser (object)
  ())

(defcenum file-chooser-action
  :open :save :select-folder :create-folder)

(defcfun "gtk_file_chooser_set_filename" :boolean
  (chooser pobject) (filename gtk-string))

(defmethod (setf filename) (filename (file-chooser file-chooser))
  (gtk-file-chooser-set-filename file-chooser filename))

(defcfun "gtk_file_chooser_get_filename" gtk-string (chooser pobject))

(defmethod filename ((file-chooser file-chooser))
  (gtk-file-chooser-get-filename file-chooser))