;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gtk-cffi.asd --- ASDF system definition for gtk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:gtk-cffi-system
  (:use #:cl #:asdf))
(in-package #:gtk-cffi-system)

(defsystem gtk-cffi-core
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.5"
  :license "GPL"
  :depends-on (gdk-cffi g-object-cffi g-lib-cffi gtk-cffi-utils gio-cffi)
  :components
  ((:file package)
   (:file enums :depends-on (package))
   (:file loadlib :depends-on (package))
   (:file generics :depends-on (package))
   (:file common :depends-on (loadlib generics))
   (:file accel-group :depends-on (loadlib))
   (:file style-context :depends-on (loadlib enums icon css-provider))
   (:file style-provider :depends-on (loadlib))
   (:file css-provider :depends-on (style-provider))
   (:file icon :depends-on (loadlib enums))))

(defsystem gtk-cffi-widget
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.99"
  :license "GPL"
  :depends-on (gtk-cffi-core)
  :components
  ((:file widget)))

(defsystem gtk-cffi-misc
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-widget)
  :components
  ((:file misc)))

(defsystem gtk-cffi-label
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-misc)
  :components
  ((:file label)))

(defsystem gtk-cffi-container
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-widget)
  :components
  ((:file container)))

(defsystem gtk-cffi-bin
  :description "Interface to GTK/Glib via CFFI: GtkBin"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.99"
  :license "GPL"
  :depends-on (gtk-cffi-container)
  :components
  ((:file bin)
   (:file expander :depends-on (bin))
   (:file menu-item :depends-on (bin))))

(defsystem gtk-cffi-window
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-bin)
  :components
  ((:file :window)))

(defsystem gtk-cffi-dialog
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.99"
  :license "GPL"
  :depends-on (gtk-cffi-window gtk-cffi-vbox gtk-cffi-hbuttonbox)
  :components
  ((:file :dialog)))

(defsystem gtk-cffi-entry
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-widget)
  :components
  ((:file :entry)))

(defsystem gtk-cffi-button
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-widget)
  :components
  ((:file :button)))

(defsystem gtk-cffi-box
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-container)
  :components
  ((:file :box)))

(defsystem gtk-cffi-hbox
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-box)
  :components
  ((:file :hbox)))

(defsystem gtk-cffi-vbox
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-box)
  :components
  ((:file :vbox)))

(defsystem gtk-cffi-buttonbox
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-box)
  :components
  ((:file :buttonbox)))

(defsystem gtk-cffi-hbuttonbox
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-buttonbox)
  :components
  ((:file :hbuttonbox)))

(defsystem gtk-cffi-eventbox
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-box)
  :components
  ((:file :eventbox)))

(defsystem gtk-cffi-cell-renderer
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-core)
  :components
  ((:file :cell-renderer)))

(defsystem gtk-cffi-cell-renderer-text
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-cell-renderer)
  :components
  ((:file :cell-renderer-text)))

(defsystem gtk-cffi-cell-renderer-pixbuf
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-cell-renderer)
  :components
  ((:file :cell-renderer-pixbuf)))

(defsystem gtk-cffi-cell-renderer-toggle
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-cell-renderer)
  :components
  ((:file :cell-renderer-toggle)))


(defsystem gtk-cffi-cell-layout
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "1.0"
  :license "GPL"
  :depends-on (gtk-cffi-core gtk-cffi-cell-renderer)
  :components
  ((:file :cell-layout)))

(defsystem gtk-cffi-paned
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-container)
  :components
  ((:file :paned)))

(defsystem gtk-cffi-frame
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-bin)
  :components
  ((:file :frame)))

(defsystem gtk-cffi-tree-model
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-core)
  :components
  ((:file :tree-model)))

(defsystem gtk-cffi-list-store
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-tree-model)
  :components
  ((:file :list-store)))

(defsystem gtk-cffi-tree-model-filter
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-tree-model)
  :components
  ((:file :tree-model-filter)))

(defsystem gtk-cffi-tree-selection
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-tree-model)
  :components
  ((:file :tree-selection)))

(defsystem gtk-cffi-tree-view-column
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-cell-layout gtk-cffi-cell-renderer gtk-cffi-widget)
  :components
  ((:file :tree-view-column)))

(defsystem gtk-cffi-tree-view
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-tree-selection gtk-cffi-tree-view-column)
  :components
  ((:file :tree-view)))

(defsystem gtk-cffi-scrolled-window
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "1.0"
  :license "GPL"
  :depends-on (gtk-cffi-bin)
  :components
  ((:file :scrolled-window)))

(defsystem gtk-cffi-text-buffer
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-core)
  :components
  ((:file text-tag)
   (:file text-buffer :depends-on (text-tag))))

(defsystem gtk-cffi-text-view
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-text-buffer)
  :components
  ((:file text-mark)
   (:file text-view)))

(defsystem gtk-cffi-combo-box
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-bin)
  :components
  ((:file :combo-box)))

(defsystem gtk-cffi-message-dialog
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-dialog)
  :components
  ((:file :message-dialog)))

(defsystem gtk-cffi-file-chooser
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-core)
  :components
  ((:file :file-chooser)))

(defsystem gtk-cffi-file-chooser-dialog
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-file-chooser gtk-cffi-dialog)
  :components
  ((:file :file-chooser-dialog)))

(defsystem gtk-cffi-file-chooser-button
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-file-chooser gtk-cffi-hbox)
  :components
  ((:file :file-chooser-button)))

(defsystem gtk-cffi-progress-bar
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-widget)
  :components
  ((:file :progress-bar)))

(defsystem gtk-cffi-table
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-container)
  :components
  ((:file :table)))

(defsystem gtk-cffi-menu-shell
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-container)
  :components
  ((:file :menu-shell)))

(defsystem gtk-cffi-menu
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-menu-shell)
  :components
  ((:file :menu)))

(defsystem gtk-cffi-menu-bar
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-menu-shell)
  :components
  ((:file :menu-bar)))

(defsystem gtk-cffi-tool-shell
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-container)
  :components
  ((:file :tool-shell)))

(defsystem gtk-cffi-toolbar
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-tool-shell)
  :components
  ((:file :toolbar)))

(defsystem gtk-cffi-notebook
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-container)
  :components
  ((:file :notebook)))

(defsystem gtk-cffi-statusbar
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-hbox)
  :components
  ((:file :statusbar)))

(defsystem gtk-cffi-image
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-misc)
  :components
  ((:file :image)))

(defsystem gtk-cffi
  :description "Interface to GTK/Glib via CFFI"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "GPL"
  :depends-on (gtk-cffi-message-dialog
               gtk-cffi-file-chooser-dialog
               gtk-cffi-file-chooser-button
               gtk-cffi-progress-bar
               gtk-cffi-entry
               gtk-cffi-button
               gtk-cffi-label
               gtk-cffi-paned
               gtk-cffi-frame
               gtk-cffi-eventbox
               gtk-cffi-list-store
               gtk-cffi-tree-model-filter
               gtk-cffi-tree-view
               gtk-cffi-scrolled-window
               gtk-cffi-cell-renderer-text
               gtk-cffi-cell-renderer-pixbuf
               gtk-cffi-cell-renderer-toggle
               gtk-cffi-table
               gtk-cffi-menu
               gtk-cffi-menu-bar
               gtk-cffi-toolbar
               gtk-cffi-statusbar
               gtk-cffi-notebook
               gtk-cffi-image
               gtk-cffi-text-view))

