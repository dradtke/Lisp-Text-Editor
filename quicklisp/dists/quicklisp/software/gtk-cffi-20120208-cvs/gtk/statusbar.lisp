(in-package :gtk-cffi)

(defclass statusbar (h-box)
  ())

(defcfun "gtk_statusbar_new" :pointer)

(defmethod gconstructor ((statusbar statusbar) &key &allow-other-keys)
  (gtk-statusbar-new))

(defgtkfuns statusbar
  ((statusbar-push . push) :uint (context-id :uint) (text gtk-string))
  ((statusbar-pop . pop) :void (context-id :uint))
  (:get context-id :uint (context gtk-string))
  (:get message-area pobject))

(defcfun gtk-statusbar-remove :void
  (statusbar pobject) (context-id :uint) (message-id :uint))
(defcfun gtk-statusbar-remove-all :void
  (statusbar pobject) (context-id :uint))

(defmethod statusbar-remove ((statusbar statusbar) context-id 
                             &optional message-id)
  (if message-id
      (gtk-statusbar-remove statusbar context-id message-id)
      (gtk-statusbar-remove-all statusbar context-id)))
