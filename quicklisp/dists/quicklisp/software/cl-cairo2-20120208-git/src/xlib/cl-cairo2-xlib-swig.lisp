;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 2.0.4
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

(in-package :cl-cairo2)



(cl:defconstant CL_CAIRO2_USING_XLIB 1)

(cl:defconstant CAIRO_HAS_XLIB_XRENDER_SURFACE 1)

(cl:defconstant CAIRO_HAS_XLIB_SURFACE 1)

(cl:defconstant CAIRO_HAS_FT_FONT 1)

(cl:defconstant CAIRO_HAS_FC_FONT 1)

(cffi:defcfun ("cairo_xlib_surface_create_with_xrender_format" cairo_xlib_surface_create_with_xrender_format) :pointer
  (dpy :pointer)
  (drawable :pointer)
  (screen :pointer)
  (format :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_xlib_surface_get_xrender_format" cairo_xlib_surface_get_xrender_format) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_ft_font_face_create_for_ft_face" cairo_ft_font_face_create_for_ft_face) :pointer
  (face :pointer)
  (load_flags :int))

(cffi:defcfun ("cairo_ft_scaled_font_lock_face" cairo_ft_scaled_font_lock_face) :pointer
  (scaled_font :pointer))

(cffi:defcfun ("cairo_ft_scaled_font_unlock_face" cairo_ft_scaled_font_unlock_face) :void
  (scaled_font :pointer))

(cffi:defcfun ("cairo_ft_font_face_create_for_pattern" cairo_ft_font_face_create_for_pattern) :pointer
  (pattern :pointer))

(cffi:defcfun ("cairo_ft_font_options_substitute" cairo_ft_font_options_substitute) :void
  (options :pointer)
  (pattern :pointer))


