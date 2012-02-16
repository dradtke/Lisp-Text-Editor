; {{{ Load up GTK+
(LOAD '/opt/lisp/quicklisp/setup.lisp)
(ql:quickload "cffi")
(ql:quickload "trivial-garbage")
(ql:quickload "iterate")
(ql:quickload "bordeaux-threads")
(ql:quickload "closer-mop")
(ql:quickload "cl-opengl")
(ql:quickload "cl-cairo2")
(asdf:operate 'asdf:load-op :cl-gtk2-gtk)
; }}}

; display a window
(gtk:within-main-loop
  (let ((window (make-instance 'gtk:gtk-window :title "Hello, world!")))
	(gtk:widget-show window)))
