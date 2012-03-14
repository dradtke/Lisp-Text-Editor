(in-package :cl-cairo2)


;; library functions to create a gdk-surface 
;; written by Peter Hildebrandt <peter.hildebrandt@washbear-network.de>

(defcfun ("gdk_cairo_create" gdk-cairo-create) :pointer (window :pointer))

(defclass gtk-context (context)
  ())
                          
(defun create-gtk-context (gdk-window)
  "creates an context to draw on a GTK widget, more precisely on the
associated gdk-window.  This should only be called from within the
expose event.  In cells-gtk, use (gtk-adds-widget-window gtk-pointer)
to obtain the gdk-window. 'gtk-pointer' is the pointer parameter
passed to the expose event handler."
  (make-instance 'gtk-context
                 :pointer (gdk-cairo-create gdk-window)))

(defmethod destroy ((self gtk-context))
  (cairo_destroy (slot-value self 'pointer)))

(defmacro with-gtk-context ((context gdk-window) &body body)
  "Executes body while context is bound to a valid cairo context for
gdk-window.  This should only be called from within an expose event
handler.  In cells-gtk, use (gtk-adds-widget-window gtk-pointer) to
obtain the gdk-window. 'gtk-pointer' is the pointer parameter passed
to the expose event handler."
  (with-gensyms (context-pointer)
    `(let ((,context (create-gtk-context ,gdk-window)))
       (with-context-pointer (,context ,context-pointer)
         ,@body)
       (destroy ,context))))

;; export manually
(export '(xlib-image-context create-xlib-image-context))
