(in-package :gtk-cffi)

(defclass file-chooser-dialog (dialog file-chooser)
  ())

(defcfun "gtk_file_chooser_dialog_new" :pointer
  (title gtk-string) (parent pobject) (action file-chooser-action)
  (but1-text gtk-string) (but1-response dialog-response)
  (but2-text gtk-string) (but2-response dialog-response)
  (null :pointer))

;; (defcfun "gtk_file_chooser_dialog_new_with_backend" :pointer
;;   (title gtk-string) (parent pobject) (action file-chooser-action)
;;   (backend gtk-string)
;;   (but1-text gtk-string) (but1-response dialog-response)
;;   (but2-text gtk-string) (but2-response dialog-response)
;;   (null :pointer))


(defmethod gconstructor ((file-chooser-dialog file-chooser-dialog)
                         &key title parent action &allow-other-keys)
  (gtk-file-chooser-dialog-new
   title parent action 
   "gtk-cancel" :cancel
   (case action
     ((:open :select-folder) "gtk-open")
     ((:save :create-folder) "gtk-save")) :accept (null-pointer)))
