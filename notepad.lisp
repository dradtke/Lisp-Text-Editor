; {{{ Load up GTK+
(LOAD '/home/damien/quicklisp/setup.lisp)
(ql:quickload "cffi")
(ql:quickload "trivial-garbage")
(ql:quickload "iterate")
(ql:quickload "bordeaux-threads")
(ql:quickload "closer-mop")
(ql:quickload "cl-opengl")
(ql:quickload "cl-cairo2")
(push "/opt/cl-gtk2/glib/" asdf:*central-registry*)
(push "/opt/cl-gtk2/pango/" asdf:*central-registry*)
(push "/opt/cl-gtk2/gdk/" asdf:*central-registry*)
(push "/opt/cl-gtk2/gtk/" asdf:*central-registry*)
(push "/opt/cl-gtk2/gtk-glext/" asdf:*central-registry*)
(push "/opt/cl-gtk2/cairo/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-gtk2-gtk)
; }}}

(defvar *dirty* nil)
(defvar *buffer-path* nil)

(defun modified (window)
  (setf (gtk:gtk-window-title window)
		(if *dirty*
		  (gtk:gtk-window-title window)
		  (concatenate 'string (gtk:gtk-window-title window) "*")))
  (setf *dirty* t))

(defun save (buffer window)
  (if (null *buffer-path*)
	(let ((dialog (make-instance 'gtk:file-chooser-dialog :title "Save" :action :save))
		  (text (gtk:text-buffer-text buffer)))
	  (gtk:dialog-add-button dialog "gtk-save" :accept)
	  (gtk:dialog-add-button dialog "gtk-cancel" :cancel)
	  (when (eq (gtk:dialog-run dialog) :accept)
		(setf *dirty* nil)
		(setf *buffer-path* (gtk:file-chooser-filename dialog))
		(with-open-file (stream *buffer-path*
								:direction :output
								:if-exists :supersede
								:if-does-not-exist :create)
		  (format stream text) *buffer-path*)
		(setf (gtk:gtk-window-title window) (concatenate 'string "Lispad - " *buffer-path*)))
	  (gtk:object-destroy dialog))
	
	(let (text (gtk:text-buffer-text buffer))
	  ; This is null here for some reason...WTF
	  (print text)
	  (with-open-file (stream *buffer-path*
							  :direction :output
							  :if-exists :supersede
							  :if-does-not-exist :create)
		(format stream text) *buffer-path*)
		(setf (gtk:gtk-window-title window) (concatenate 'string "Lispad - " *buffer-path*)))))

; display a window
(gtk:within-main-loop
  ; Define the widgets
  (let ((window (make-instance 'gtk:gtk-window
							   :title "Lispad"
							   :default-width 400
							   :default-height 300
							   :type :toplevel))
		(textarea (make-instance 'gtk:text-view))
		(menubar (make-instance 'gtk:menu-bar))
		(vbox (make-instance 'gtk:v-box))

		(filemenuitem (make-instance 'gtk:menu-item :label "File"))
		(editmenuitem (make-instance 'gtk:menu-item :label "Edit"))
		(formatmenuitem (make-instance 'gtk:menu-item :label "Format"))
		(viewmenuitem (make-instance 'gtk:menu-item :label "View"))
		(helpmenuitem (make-instance 'gtk:menu-item :label "Help"))

		(filemenu (make-instance 'gtk:menu))
		(editmenu (make-instance 'gtk:menu))
		(formatmenu (make-instance 'gtk:menu))
		(viewmenu (make-instance 'gtk:menu))
		(helpmenu (make-instance 'gtk:menu))

		; File
		(filemenu-new (make-instance 'gtk:menu-item :label "New"))
		(filemenu-edit (make-instance 'gtk:menu-item :label "Edit"))
		(filemenu-save (make-instance 'gtk:menu-item :label "Save")))

	; Create the menu
	(gtk:menu-shell-append menubar filemenuitem)
	(gtk:menu-shell-append menubar editmenuitem)
	(gtk:menu-shell-append menubar formatmenuitem)
	(gtk:menu-shell-append menubar viewmenuitem)
	(gtk:menu-shell-append menubar helpmenuitem)

	(setf (gtk:menu-item-submenu filemenuitem) filemenu)
	(setf (gtk:menu-item-submenu editmenuitem) editmenu)
	(setf (gtk:menu-item-submenu formatmenuitem) formatmenu)
	(setf (gtk:menu-item-submenu viewmenuitem) viewmenu)
	(setf (gtk:menu-item-submenu helpmenuitem) helpmenu)

	(gtk:menu-shell-append filemenu filemenu-new)
	(gtk:menu-shell-append filemenu filemenu-edit)
	(gtk:menu-shell-append filemenu filemenu-save)

	; Fill the window
	(gtk:box-pack-start vbox menubar :expand nil :fill nil :padding 0)
	(gtk:box-pack-start vbox textarea :expand t :fill t :padding 0)
	(gtk:container-add window vbox)

	; Connect signals
	(gobject:g-signal-connect window "delete-event" (lambda (window event)
													  (declare (ignore window event))
													  (gtk:gtk-main-quit)))

	(gobject:g-signal-connect (gtk:text-view-buffer textarea) "changed" (lambda (buffer)
																		  (modified window)))

	; Menu actions
	(gobject:g-signal-connect filemenu-save "activate" (lambda (menuitem)
														 (save (gtk:text-view-buffer textarea) window)))

	
	; Finally, display everything
	(gtk:widget-show window)))
