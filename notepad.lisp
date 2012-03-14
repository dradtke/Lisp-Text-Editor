; {{{ Load up GTK+
(LOAD 'quicklisp/setup.lisp)
(ql:quickload "cffi")
(ql:quickload "trivial-garbage")
(ql:quickload "iterate")
(ql:quickload "bordeaux-threads")
(ql:quickload "closer-mop")
(ql:quickload "cl-opengl")
(ql:quickload "cl-cairo2")
(push "cl-gtk2/glib/" asdf:*central-registry*)
(push "cl-gtk2/pango/" asdf:*central-registry*)
(push "cl-gtk2/gdk/" asdf:*central-registry*)
(push "cl-gtk2/gtk/" asdf:*central-registry*)
(push "cl-gtk2/gtk-glext/" asdf:*central-registry*)
(push "cl-gtk2/cairo/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-gtk2-gtk)
; }}}

(defvar *dirty* nil)
(defvar *buffer-path* nil)

; Signal handler.
; Called whenever changes are made.
(defun modified (window)
  (setf (gtk:gtk-window-title window)
        (if *dirty*
          (gtk:gtk-window-title window)
          (concatenate 'string (gtk:gtk-window-title window) "*")))
  (setf *dirty* t))

; Create a new buffer.
(defun new-buffer (buffer window)
  (when *dirty*
    (let ((dialog (make-instance 'gtk:message-dialog :title "New")))
      (setf (gtk:message-dialog-text dialog) "Discard current changes?")
      (setf (gtk:message-dialog-message-type dialog) :question)
      (gtk:dialog-add-button dialog "gtk-yes" :accept)
      (gtk:dialog-add-button dialog "gtk-no" :cancel)
      (when (eq (gtk:dialog-run dialog) :accept)
        (setf (gtk:text-buffer-text buffer) "")
        (setf (gtk:gtk-window-title window) "Lispad")
        (setf *dirty* nil)
        (setf *buffer-path* nil))
      (gtk:object-destroy dialog))))

; Save the current buffer,
; prompting for a path if one isn't set yet.
(defun save-buffer (buffer window)
  (when (null *buffer-path*)
    (let ((dialog (make-instance 'gtk:file-chooser-dialog :title "Save" :action :save)))
      (gtk:dialog-add-button dialog "gtk-save" :accept)
      (gtk:dialog-add-button dialog "gtk-cancel" :cancel)
      (when (eq (gtk:dialog-run dialog) :accept)
        (setf *dirty* nil)
        (setf *buffer-path* (gtk:file-chooser-filename dialog)))
      (gtk:object-destroy dialog)))
  (when (not (null *buffer-path*))
      (with-open-file (stream *buffer-path*
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream (gtk:text-buffer-text buffer) *buffer-path*)
        (setf (gtk:gtk-window-title window) (concatenate 'string "Lispad - " *buffer-path*)))
      (setf *dirty* nil)))

; Open a file into a new buffer.
(defun open-buffer (buffer window)
  (let ((dialog (make-instance 'gtk:file-chooser-dialog :title "Open" :action :open)))
    (gtk:dialog-add-button dialog "gtk-open" :accept)
    (gtk:dialog-add-button dialog "gtk-cancel" :cancel)
    (when (eq (gtk:dialog-run dialog) :accept)
      (setf *buffer-path* (gtk:file-chooser-filename dialog))
      (with-open-file (stream *buffer-path*)
        (let ((data (make-string (file-length stream))))
          (read-sequence data stream)
          (setf (gtk:text-buffer-text buffer) data)))
      (setf (gtk:gtk-window-title window) (concatenate 'string "Lispad - " *buffer-path*))
      (setf *dirty* nil))
    (gtk:object-destroy dialog)))

; Quit the program.
(defun quit-notepad (buffer window)
  (if *dirty*
    (let ((dialog (make-instance 'gtk:message-dialog :title "Quit")))
      (setf (gtk:message-dialog-text dialog) "Quit without saving?")
      (setf (gtk:message-dialog-message-type dialog) :question)
      (gtk:dialog-add-button dialog "gtk-yes" :accept)
      (gtk:dialog-add-button dialog "gtk-no" :cancel)
      (when (eq (gtk:dialog-run dialog) :accept)
        (gtk:gtk-main-quit))
      (gtk:object-destroy dialog)
      t)
    (gtk:gtk-main-quit)))

; Display an about dialog.
(defun about-notepad ()
  (let ((dialog (make-instance 'gtk:message-dialog :title "About")))
    (setf (gtk:message-dialog-text dialog)
          "Lispad is Damien Radtke's class project for CSC 358,
written in Common Lisp using cl-gtk2.")
    (gtk:dialog-add-button dialog "gtk-ok" :accept)
    (gtk:dialog-run dialog)
    (gtk:object-destroy dialog)))


; The main loop.
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
        (helpmenuitem (make-instance 'gtk:menu-item :label "Help"))

        (filemenu (make-instance 'gtk:menu))
        (helpmenu (make-instance 'gtk:menu))

        ; File
        (filemenu-new (make-instance 'gtk:menu-item :label "New"))
        (filemenu-open (make-instance 'gtk:menu-item :label "Open"))
        (filemenu-save (make-instance 'gtk:menu-item :label "Save"))
        (filemenu-quit (make-instance 'gtk:menu-item :label "Quit"))

        ; Help
        (helpmenu-about (make-instance 'gtk:menu-item :label "About")))

    ; Create the menu
    (gtk:menu-shell-append menubar filemenuitem)
    (gtk:menu-shell-append menubar helpmenuitem)

    (setf (gtk:menu-item-submenu filemenuitem) filemenu)
    (setf (gtk:menu-item-submenu helpmenuitem) helpmenu)

    (gtk:menu-shell-append filemenu filemenu-new)
    (gtk:menu-shell-append filemenu filemenu-open)
    (gtk:menu-shell-append filemenu filemenu-save)
    (gtk:menu-shell-append filemenu filemenu-quit)

    (gtk:menu-shell-append helpmenu helpmenu-about)

    ; Fill the window
    (gtk:box-pack-start vbox menubar :expand nil :fill nil :padding 0)
    (gtk:box-pack-start vbox textarea :expand t :fill t :padding 0)
    (gtk:container-add window vbox)

    ; Connect signals
    (gobject:g-signal-connect window "delete-event" (lambda (window event)
                                                      (declare (ignore window event))
                                                      (quit-notepad (gtk:text-view-buffer textarea) window)))

    (gobject:g-signal-connect (gtk:text-view-buffer textarea) "changed" (lambda (buffer)
                                                                          (modified window)))

    ; Menu actions
    (gobject:g-signal-connect filemenu-new "activate" (lambda (menuitem)
                                                         (new-buffer (gtk:text-view-buffer textarea) window)))
    (gobject:g-signal-connect filemenu-save "activate" (lambda (menuitem)
                                                         (save-buffer (gtk:text-view-buffer textarea) window)))
    (gobject:g-signal-connect filemenu-open "activate" (lambda (menuitem)
                                                         (open-buffer (gtk:text-view-buffer textarea) window)))
    (gobject:g-signal-connect filemenu-quit "activate" (lambda (menuitem)
                                                         (quit-notepad (gtk:text-view-buffer textarea) window)))
    (gobject:g-signal-connect helpmenu-about "activate" (lambda (menuitem)
                                                         (about-notepad)))

    
    ; Finally, display everything
    (gtk:widget-show window)))
