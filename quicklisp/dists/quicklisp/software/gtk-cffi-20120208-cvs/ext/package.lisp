(in-package #:cl-user)

(defpackage gtk-cffi-ext
  (:use #:common-lisp #:cffi #:alexandria #:iterate
        #:cffi-object #:g-object-cffi #:g-lib-cffi #:gdk-cffi 
        #:gtk-cffi-utils #:gtk-cffi)
  (:shadowing-import-from #:gtk-cffi #:image #:window)
  (:import-from #:gtk-cffi 
                #:tree-iter-struct #:u1 #:stamp 
                #:tree-model-iface #:get-n-columns #:get-column-type
                #:get-iter #:get-path #:get-value #:iter-next #:iter-previous
                #:iter-children #:iter-has-child #:iter-n-children #:get-flags
                #:iter-nth-child #:iter-parent #:ref-node #:unref-node
                #:tree-path)
  (:export
   #:lisp-model
   #:implementation
   #:lisp-model-array
   #:lisp-model-tree-array
   #:larray))

