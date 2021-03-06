(in-package :gtk-cffi)

(defcenum icon-size
  :invalid
  :menu
  :small-toolbar
  :large-toolbar
  :button
  :dnd
  :dialog)

(defcenum state 
  :normal :active :prelight :selected :insensitive :inconsistent :focused)

(defclass icon-source (object) ())

(defcfun "gtk_icon_source_new" :pointer)

(defmethod gconstructor ((icon-source icon-source) &rest rest)
  (declare (ignore icon-source rest))
  (gtk-icon-source-new))

(defgtkslots icon-source
    direction text-direction
    direction-wildcarded :boolean
    filename gtk-string
    pixbuf pobject
    icon-name gtk-string
    size icon-size
    size-wildcarded :boolean
    state state
    state-wildcarded :boolean)