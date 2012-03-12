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
  ; Define the widgets
  (let ((window (make-instance 'gtk:gtk-window
							   :title "Notepad"
							   :default-width 400
							   :default-height 300
							   :type :toplevel))
		(textarea (make-instance 'gtk:text-view))
		(menubar (make-instance 'gtk:menu-bar))
		(vbox (make-instance 'gtk:v-box)))

	; Fill the window
	(gtk:box-pack-start vbox menubar :expand nil :fill nil :padding 0)
	(gtk:box-pack-start vbox textarea :expand t :fill t :padding 0)
	(gtk:container-add window vbox)

	; Connect signals
	(gobject:g-signal-connect window "delete-event" (lambda (window event)
											  (declare (ignore window event))
											  (gtk:gtk-main-quit)))
	
	; Finally, display everything
	(gtk:widget-show window)))
