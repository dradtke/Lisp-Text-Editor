(in-package :gdk-cffi)

(defcstruct color-struct
  "GdkColor"
  (pixel :int32)
  (red :int16)
  (green :int16)
  (blue :int16))

(defcfun "gdk_color_parse" :boolean (str gtk-string) (color color-struct))
(defcfun "gdk_color_to_string" gtk-string (color color-struct))
(defcfun gdk-color-free :void (color :pointer))

(define-foreign-type color-cffi (freeable)
  ()
  (:actual-type color-struct)
  (:simple-parser pcolor))

(defmethod free-ptr ((class color-cffi) ptr)
  (gdk-color-free ptr))

(defmethod translate-to-foreign (value (type color-cffi))
  (if (pointerp value) value
    (let ((color-st (foreign-alloc 'color-struct)))
      (gdk-color-parse (string value) color-st)
      color-st)))

(defmethod translate-from-foreign (ptr (type color-cffi))
  (gdk-color-to-string ptr))

(defcfun (color-equal "gdk_color_equal") :boolean 
  (color pcolor) (color2 pcolor))

(defcstruct rgba-struct
  "GdkRGBA"
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(define-foreign-type rgba-cffi (freeable)
  ()
  (:actual-type rgba-struct)
  (:simple-parser prgba))

(defcfun gdk-rgba-parse :boolean (color rgba-struct) (str :string))
(defcfun gdk-rgba-to-string :string (color rgba-struct))
(defcfun gdk-rgba-free :void (color :pointer))

(defmethod free-ptr ((class rgba-cffi) ptr)
  (gdk-rgba-free ptr))

(defmethod translate-to-foreign (value (type rgba-cffi))
  (if (pointerp value) value
    (let ((color-st (foreign-alloc 'rgba-struct)))
      (assert (gdk-rgba-parse color-st (string value)) (value) 
              "Bad RGBA color") 
      color-st)))

(defmethod translate-from-foreign (ptr (type rgba-cffi))
  (gdk-rgba-to-string ptr))
