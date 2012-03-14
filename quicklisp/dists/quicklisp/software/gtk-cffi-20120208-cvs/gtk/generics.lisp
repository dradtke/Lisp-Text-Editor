(in-package :gtk-cffi)

;; (defgeneric destroy (gtk-object))
;; (defgeneric flags (gtk-object))

;; (defgeneric text (widget &rest flags))
;; (defgeneric (setf text) (text widget &rest rest))
;; (defgeneric (setf mnemonic-widget) (widget label))
;; (defgeneric mnemonic-widget (label))
;; (defgeneric activate (widget))
;; (defgeneric realize (widget))
;; (defgeneric size-request (widget))
;; (defgeneric (setf size-request) (size widget))
;; (defgeneric style-field (widget field &optional state type))
;; (defgeneric (setf style-field) (value widget field &optional state type))
;; (defgeneric color (widget &rest rest))
;; (defgeneric (setf color) (color widget &rest rest))
;; (defgeneric font (widget &rest rest))
;; (defgeneric (setf font) (font widget &rest rest))
;; (defgeneric bg-pixmap (widget &rest state))
;; (defgeneric (setf bg-pixmap) (pixmap widget &rest rest))
;; (defgeneric allocation (widget))
;; (defgeneric (setf allocation) (value widget))
;; (defgeneric show (widget &rest flags))
;; (defgeneric hide (widget))
;; (defgeneric gdk-window (widget))
;; (defgeneric (setf justify) (justify label))
;; (defgeneric justify (label))

;; (defgeneric child (bin))

;; (defgeneric (setf default-size) (coords window))
;; (defgeneric default-size (window))
;; (defgeneric (setf screen) (screen window))
;; (defgeneric screen (window))
;; (defgeneric transient-for (window))
;; (defgeneric (setf transient-for) (window parent))
;; (defgeneric (setf win-position) (pos window))

;; (defgeneric add (container widget))
;; (defgeneric border-width (container))
;; (defgeneric (setf border-width) (value container))
;; (defgeneric reparent (widget new-parent))
;; (defgeneric propagate-expose (container child event))

;; (defgeneric run (dialog &key keep-alive))
;; (defgeneric (setf has-separator) (has dialog))
;; (defgeneric has-separator (dialog))
;; (defgeneric add-button (dialog string response))

;; ;(defgeneric get-iter (text-buffer text-iter pos))
;; (defgeneric buffer (text-view))
;; (defgeneric (setf buffer) (buffer text-view))

;; (defgeneric add-attribute (cell-layout cell-renderer attr column))
;; (defgeneric (setf cell-data-func) (c-handler
;;                                   cell-layout cell-renderer
;;                                   &optional data destroy-notify))
;; (defgeneric clear-attributes (cell-layout cell-renderer))
;; (defgeneric clear (cell-layout))

;; (defgeneric (setf sort-column-id) (id tree-view-column))
;; (defgeneric (setf reorderable) (reorderable tree-view-column))
;; (defgeneric reorderable (tree-view-column))
;; (defgeneric (setf widget) (widget tree-view-column))
;; (defgeneric widget (tree-view-column))
;; (defgeneric pack (tree-view-column cell-renderer &rest flags))
;; (defgeneric cell-get-position (tree-view-column cell-renderer))
;; (defgeneric cell-renderers (tree-view-column))
;; (defgeneric get-cell-at (tree-view-column x)) 
;; (defgeneric (setf title) (title tree-view-column))
;; (defgeneric title (tree-view-column))

;; (defgeneric get-indices (tree-path))
;; (defgeneric get-index (tree-path &optional pos))
;; (defgeneric copy (struct-object))
;; (defgeneric foreach (tree-model func &optional data))
;; (defgeneric iter->path (tree-model tree-iter))
;; (defgeneric iter->string (tree-model tree-iter))
;; (defgeneric model-values (tree-model &key iter columns col))
;; (defgeneric path->iter (tree-model tree-path &optional tree-iter))
;; (defgeneric n-columns (tree-model))
;; (defgeneric column-type (tree-model col))


;; (defgeneric path-from-child (tree-model-filter tree-path))
;; (defgeneric iter-to-child (tree-model-filter tree-iter))
;; (defgeneric (setf model-values) (values tree-model-filter
;;                                         &key iter columns col))
;; (defgeneric (setf visible-column) (column tree-model-filter))

;; (defgeneric (setf shadow-type) (shadow-type frame))
;; (defgeneric shadow-type (frame))

;; (defgeneric (setf policy) (policy scrolled-window))

;; (defgeneric get-selection (tree-view))
;; (defgeneric path-at-pos (tree-view  x y))
;; (defgeneric get-cursor (tree-view))
;; (defgeneric column (tree-view n))
;; (defgeneric append-column (tree-view tree-view-column))
;; (defgeneric (setf search-column) (n tree-view))
;; (defgeneric search-column (tree-view))
;; (defgeneric model (tree-view))
;; (defgeneric (setf model) (model tree-view))

;; (defgeneric get-selected (tree-selection))
;; (defgeneric tree-selection-foreach (tree-selection func &optional data)) 

;; (defgeneric append-iter (list-store &optional tree-iter))
;; (defgeneric append-values (list-store values))

;; (defgeneric append-text (combo-box text))
;; (defgeneric prepend-text (combo-box text))
;; (defgeneric insert-text (combo-box text))
;; (defgeneric remove-text (combo-box pos))
;; (defgeneric active-text (combo-box))


;; (defgeneric fraction (progress-bar))
;; (defgeneric (setf fraction) (fraction progress-bar))

;; (defgeneric (setf kid) (kid container))
;; (defgeneric (setf kids) (kids container))

;; (defgeneric resize (table &key rows columns))

;; (defgeneric attach (table widget &key left right top bottom
;;                           xoptions yoptions xpadding ypadding))